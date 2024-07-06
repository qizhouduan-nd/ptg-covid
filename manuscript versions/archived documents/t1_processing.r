setwd(dirname(rstudioapi::documentPath()))
library(readxl)
library(tidyverse)
library(ggplot2)
covid_t1 <- read_excel("Covid T1 Raw(3)-2.xlsx")
covid_t1_new <- read_excel("Covid T1 Raw(3)-4.xlsx")

ptg = covid_t1$`Assessment of Post-Traumatic Growth`

sum(str_detect(tolower(ptg), pattern = 'ptg')) ## 23 studies used some sort of ptg 

View(covid_t1[str_detect(tolower(ptg), pattern = 'ptg|post'), 2]) ## include all studies that contains ptg in any form

as.matrix(covid_t1[str_detect(tolower(ptg), pattern = 'ptg|post'), 2])

## studies excluded 
covid_t1[!str_detect(tolower(ptg), pattern = 'ptg|post'), 2]

## studies not to be included by AA 
covid_t1_new$A.A
covid_t1_new$`First Author, Year`[!str_detect(tolower(covid_t1_new$A.A), pattern = 'yes')]

covid_t1_new$`First Author, Year`[str_detect(tolower(covid_t1_new$A.A), pattern = 'yes')]

covid_t1_new$`First Author, Year`[str_detect(tolower(covid_t1_new$A.A), pattern = 'maybe')]

