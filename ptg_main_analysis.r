###############################
### ptg main analysis script
###############################
library(tidyverse)
library(readxl)
setwd(dirname(rstudioapi::documentPath()))

# ptg_effect_sizes <- read_excel("Desktop/ptg effect sizes.xlsx")
# View(ptg_effect_sizes)   
# 
# 
# ptg_effect_sizes %>% select(`scale type` )
# 
# ptgi_studies = ptg_effect_sizes %>% filter(`scale type` == 'PTGI')
# 
# as.numeric(ptgi_studies$`effect size`)
# 
# 
# 

##############################
# analysis for t1 raw
##############################
## load the data
## examining data
t1_raw <- read_excel("Covid T1 Raw.xlsx")
head(t1_raw)   
View(t1_raw)
glimpse(t1_raw)


## pulled sample variance for chen 2021 
n = 422
n1 = n * 10.7/100
n2 = n * 20.1/100
n3 = n * 42.2/100
n4 = n * 27/100

s1 = 12.25
s2 = 10.73
s3 = 11.28
s4 = 7.75

sp_num = (n1 - 1) * s1^2 + (n2 - 1) * s2^2 + (n3 - 1) * s3^2 + (n4 - 1) * s4^2
sp_denom = n1 + n2 + n3 + n4 - 4

sp = sp_num / sp_denom
sd_p = sqrt(sp)
sd_p






