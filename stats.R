#' Compute reliability
#' @description Compute variance and associated statistics for grades data
#' for subjects. 
#' @param my_grades A dataframe with columns Student, Subject, and Points. Rows
#' with missing data will be deleted.
#' @param filter_zero A boolean, if TRUE eliminates instances of zero points. This
#' is recommended, since course failure is often caused by reasons that
#' are extranous to relability.
#' @param n_range An array of integers to filter grade counts by. Defaults
#' to 2:50, meaning a student must have 2-50 grades in a subject to be
#' included in the statistics. This can be used to contrast service
#' @param simple A boolean, if TRUE returns only the essential columns.
#' @param random_effects A boolean, if TRUE adds a second estimate of ICC and bounds using heirarchical modeling.
#' @return A dataframe with statistics for each Subject, including Students (number of),
#' Mean (GPA), N (number of grades), iSS (within-student sum of squares), 
#' sSS (between student sum of squares), icc_est (the intra-class correlation estimate), icc_lower, icc_upper (approximately 95%
#' confidence interval). Adjusted values of these are included, to account for a small amount of 
#' bias and to make the confidence intervals more realistic.
#' @details Uses maximum likelihood solution (via lme4::lmer) to estimate variances for group effects,
#' which are individual students here, and residual variance. These are used to compute the ICC. 
#' @export
#' 
grade_stats <- function(my_grades, filter_zero = TRUE, n_range = 2:50, simple = TRUE, random_effects = FALSE){
  
  # compute ICC from F and k
  # take the S-F formula for ICC and divide top and bottom by MSE
  # Setting F0 = MSB/MSE gives the first formula
  ICC_F <- function(ICC, Fint, k){ 
    F0 =(ICC*(k-1)+1)/(1-ICC) 
    return( (F0*Fint - 1)/(F0*Fint + k - 1) )
  }
  
  my_grades <- my_grades %>% 
    ungroup() %>% 
    na.omit()  # remove rows with NA
  
  if(filter_zero == TRUE){
    my_grades <- my_grades %>% 
      filter(Points > 0)
  }
  
  grade_counts <- my_grades %>% 
    group_by(Student, Subject) %>% 
    summarize(N = n()) %>%  # individual student mean
    filter(N %in% n_range) %>% 
    select(-N)
  
  my_grades <- my_grades %>% 
    inner_join(grade_counts, by = c("Student","Subject")) # includes only data within specs
  
  ############################ Parameter estimates ################
  student_stats <- my_grades %>% 
    group_by(Subject, Student) %>%
    summarize(iMean = mean(Points), # subject avg for student
              iSS = sum((Points - iMean)^2), # sum of sqrs within
              iN    = n()) %>% # count of grades in subject
    group_by(Subject) %>% 
    summarize(Students = n(),
              Mean = weighted.mean(iMean,iN), # grand mean
              N    = sum(iN), # number of grades in subject
              iN_avg = mean(iN), # avg number of grades per student
              iSS  = sum(iSS), # individual sum of squares
              sSS  = sum(iN*(Mean - iMean)^2),
              DF_iSS = N - Students, # lose one DF for each student GPA estimate
              DF_sSS = Students - 1, # lose one DF for the grand mean GPA
              MiSS = iSS / DF_iSS,   # mean sum of squares for error (per DF)
              MsSS = sSS / DF_sSS,   # mean sum of squares for between-student variation (per DF)
              F_stat = MsSS/MiSS,    # F statistic and error bounds, F = MSB/MSE 
              F_upper = qf(p = .95, df1 = DF_sSS, df2 = DF_sSS),
              F_lower = qf(p = .05, df1 = DF_sSS, df2 = DF_sSS),
              icc_sf = ( MsSS - MiSS ) / ( MsSS + (iN_avg -1)*MiSS ), # Shrout-Fleiss formula
              icc_sf_lower = ICC_F(icc_sf,F_lower,iN_avg),
              icc_sf_upper = ICC_F(icc_sf,F_upper,iN_avg)
              ) %>% 
    select(-F_upper, -F_lower)
  
    if (simple == TRUE) {
      student_stats <- student_stats %>% 
        select(Subject, Students, Mean, N, icc_sf, icc_sf_lower, icc_sf_upper)
    }
   
    if(random_effects == TRUE){
      ICCs <- my_grades %>% 
        group_by(Subject) %>% 
        summarize(estimate_ICC(Student, Points)) 
      
      student_stats <- student_stats %>% 
        left_join(ICCs, by = "Subject")
    }
    
    return(student_stats)
}

#' Estimate the intra-class correlation with a random effects model
#' @description Uses a simple random effects model to estimate the ICC(1,1) from two 
#' vectors, the first identifying a grouping variable (like students) and the second
#' giving the measure (like grade points). Some of the implementation was cribbed from
#' the psych:ICC algorithm. 
#' @param group_var A grouping variable, numeric, factor, or character
#' @param measure A numeric vector giving the values to calculate variances from
#' @return A dataframe with the ICC estimate and 95% confidence interval.
#' @export
estimate_ICC <- function(group_var, measure){
  
  # center the data for random effects modeling
  measure <- scale(measure)
  
  N  <- length(measure)                # number of data points
  Ns <- length(unique(group_var))      # number of groups (e.g. students)
  Ng <- N/Ns                           # avg number of measurements per group
  
  df_between <- Ns-1                   # degrees of freedom for between-student
  df_error   <- Ns*(Ng-1)              # approx degrees of freedom for error
  
  m1_stats <- lme4::lmer(measure ~ 1 + (1|group_var)) %>% 
    broom::tidy()
  
  # get variance estimates from the model output
  v_b <- m1_stats$estimate[2]^2        # est. variance for between-student
  v_e <- m1_stats$estimate[3]^2        # est. variance for within-student "error"
  
  # compute ICC
  ICC <- v_b/(v_b + v_e)
  
  # Shrout & Fleiss (1979) "Intraclass Correlations : Uses in Assessing Rater Reliability"
  # since ICC = (MSB - MSE)/(MSB +(k-1)MSE), where k ~ avg # grades per student
  # and F0 = MSB/MSE
  # if we divide through by MSE we get 
  # ICC = (F0 - 1)/(F0+k-1), which we invert to get
  # (ICC(k-1)+1)/(1-ICC) = F0
  # function to adjust ICC for confidence interval
  ICC_F <- function(ICC, Fint, k){ 
     F0 =(ICC*(k-1)+1)/(1-ICC) 
     return( (F0*Fint - 1)/(F0*Fint + k - 1) )
  }
  
  # F distribution error bounds per Shrout & Fleiss pg 424
  F_bounds <- qf(p = c(.05,.95), df1 = df_between, df2 = df_error)  # F = MSB/MSE
  
  ICC_lower  <- ICC_F(ICC,F_bounds[1],Ng)
  ICC_upper  <- ICC_F(ICC,F_bounds[2],Ng)  # this is the reciprocal of the first Fint
  
  return(data.frame(icc_re = ICC, icc_re_lower = ICC_lower, icc_re_upper = ICC_upper))
} 


#' Simulate grades
#' @param m number of students
#' @param k average number of grades per student
#' @param mu the grand mean for grade points
#' @param s_b standard deviation between student means
#' @param s_e standard deviation within student grades
#' @return A dataframe with Student and Points, which are simulated from the parameters (floats)
#' @description Generates grade-like points as floats (not integers--round if you want) with a preset 
#' between-student variance and error variance. Each of the m students is assigned a number of grades
#' n_i between 2 and k uniformly. Grade points are generated with a normal distribution with fixed
#' grand mean of three.
#' @examples 
#' hist(simulated_grades()$Points)
#' @export
simulated_grades <- function(m = 100, k = 20, mu = 3, s_b = .5, s_e = .5){
  
  data.frame(Student = 1:m, 
             n_i     = ceiling(runif(m, min = 1.001, max = k)), # generate a random number of grades per student
             mu_i    = rnorm(m, 0, s_b),                        # choose a student mean
             Points  = NA) %>% 
    group_by(Student) %>%                                              # generate n_i random grades per student
    summarize(Points = rnorm(n_i, mu + mu_i ,s_e))                  # simulate grades
}

#' Simulate grades and compare actual ICC to estimated ICC
#' @param Nsim Number of simulations to run
#' @param min_m Minimum number of students in a run
#' @param max_m Maximum number of students in a run
#' @param k Maximum number of classes per student
#' @examples 
#' reliability_sim(Nsim = 50) %>% 
#' ggplot(aes(x = icc, y = icc_sf, ymin = icc_sf_lower, ymax = icc_sf_upper)) + 
#'  geom_point(size = 1, alpha = .3) +
#'  geom_errorbar(width = 0) +
#'  geom_abline(slope = 1, intercept = 0) +
#'  theme_bw()
#' @export
reliability_sim <- function(Nsim = 100, min_m = 100, max_m= 5000, k = 20){
  
  specs <- data.frame(run = 1:Nsim,                                        # simulation number
                      m   = round(runif(Nsim, min = min_m, max = max_m)),  # number of students
                      mu  = rnorm(Nsim, 3, .2),                            # overall GPA
                      s_b = abs(rnorm(Nsim, .5, .2)) + .05,                # between student sd
                      s_e = abs(rnorm(Nsim, .5, .2)) + .05) %>%            # error sd
    mutate(icc = s_b^2/(s_b^2 + s_e^2),
           s_t = sqrt(s_b^2 + s_e^2))    
  
  
  for(i in 1:Nsim){
    sim_grades <- simulated_grades(specs$m[i],k,specs$mu[i],specs$s_b[i],specs$s_e[i]) %>% 
      mutate(Subject = "Subj")
    
    icc_stats  <- grade_stats(sim_grades,filter_zero = FALSE, n_range = 2:50, simple = TRUE, random_effects = TRUE)
    
    # Shrout-Fleiss
    specs$icc_sf[i]       <- icc_stats$icc_sf
    specs$icc_sf_lower[i] <- icc_stats$icc_sf_lower
    specs$icc_sf_upper[i] <- icc_stats$icc_sf_upper
    
    # Random effects
    specs$icc_re[i]       <- icc_stats$icc_re
    specs$icc_re_lower[i] <- icc_stats$icc_re_lower
    specs$icc_re_upper[i] <- icc_stats$icc_re_upper
  }
  
  return(specs)
}
