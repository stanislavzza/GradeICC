# GradeICC
Computes intraclass correlations for course grades by subject

This project allows you to compute ICC(1,1) statistics as defined in Shrout and Fleiss (1979) "Intraclass Correlations : Uses in Assessing
Rater Reliability." It is designed to calculate ICC independently for each subject using registraton codes, like BIO for biology, but you can 
set the subject to anything you like, including setting all rows to the same value (like "all") to get overall reliability. Or you could use a
two digit CIP code to aggregate at that level. Or, if comparing institutions, you could use the subject field to specify the institution.

The folder structure expected by the code is: 

``` /Top folder
      get_icc.R
      /code
            stats.R
      /data
            grades.csv
```

Then just run get_icc.R (best way is to create a project in the top folder with Rstudio, open the file, highlight it all and run). 
The code requires that the tidyverse library be loaded with install.packages("tidyverse").

The grades.csv file can be any numeric data that you want to calculate ICC from (like rubric ratings), but the column names are suggestive for
grades. The file must be a comma-separated data file with three columns:

* Student: a unique identifier for each subject being rated, assumed to be a student for grades.
* Subject: a unique identifier for each group within the data that you want an independent ICC for.
* Points: a numerical rating or other comparative value. If grades, must be grade points, not letters. 

Student | Subject | Points
------- | --------|-------
12334   | BIO     | 4
12335   | BIO     | 3
12336   | MAT     | 3
12337   | ART     | 2


There are several options for running the ICCs, and the get_icc.R uses some of them. You can find out more by reading the included documentation. It's possible to create an R package using Roxygen for documentation.
