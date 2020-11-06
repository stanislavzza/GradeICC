library(tidyverse)
source("code/stats.R")

grades <- read_csv("data/grades.csv") %>% 
  select(Student,Subject,Points) %>% 
  na.omit()

grade_stats(grades) %>% 
    write_csv("data/icc_all.csv")

grade_stats(grades, n_range = 2:3) %>% 
     write_csv("data/icc_service.csv")

grade_stats(grades, n_range = 4:50) %>%       
    write_csv("data/icc_major.csv")