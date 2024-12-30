###############################
### ptg main analysis script
###############################
library(tidyverse)
library(readxl)
library(metafor)
library(psych)
library(kableExtra)
library(modelsummary)
setwd(dirname(rstudioapi::documentPath()))

## load data
shortlist = read_excel('effect_sizes_and_moderators.xlsx')
## look at how many studies uses PTGI
shortlist %>% group_by(`scale type`) %>% count() # 20 PTGI and 10 PTGI-SF 

## we can do normalization or we can perform separate analysis for these two types of studies
## check the sample size
sum(shortlist$`sample size`) ## overall
PTGI_dat = shortlist %>% filter(`scale type` == 'PTGI')
# PTGISF_dat = shortlist %>% filter(`scale type` != 'PTGI')
# 
# ## use escalc function to get effect sizes
calculate_effect_size = escalc(measure = "SMD", 
                               m1i = PTGI_dat$`effect size`, 
                               sd1i = PTGI_dat$sd,
                               n1i = PTGI_dat$`sample size`,  # Use actual sample sizes
                               m2i = rnorm(nrow(PTGI_dat), mean = 45, sd = PTGI_dat$sd), 
                               sd2i = PTGI_dat$sd,
                               n2i = PTGI_dat$`sample size`,  # Use actual sample sizes
                               data = PTGI_dat)
calculate_effect_size ## noticed some quite high values; this indicates that maybe screening for outlier is needed

## we run intercept only model for main analysis (this would be random intercept model)
main_analysis_model_PTGI = rma(yi ~ 1, vi = vi, data = calculate_effect_size)
main_analysis_model_PTGI







