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



















