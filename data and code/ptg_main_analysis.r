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
shortlist %>% group_by(`scale type`) %>% count() # 57 PTGI and 18 PTGI-SF 

## we focus on PTGI for the moment
## we can do normalization or we can perform separate analysis for these two types of studies
## check the sample size
sum(shortlist$`sample size`) ## overall
PTGI_dat = shortlist %>% filter(`scale type` == 'PTGI')
PTGISF_dat = shortlist %>% filter(`scale type` != 'PTGI')

## check the sample size for studies with PTGI 
sum(PTGI_dat$`sample size`)

PTGI_dat$`effect size`
### calculate effect size
## try escalc here
escalc(measure = "SMD", m1i = PTGI_dat$`effect size`, m2i = rep(45, 57), 
       sd1i = PTGI_dat$sd,
       sd2i = PTGI_dat$sd, 
       n1i=57, n2i=57)

## we run intercept only model for main analysis (this would be random intercept model)
main_analysis_model_PTGI = rma(g ~ 1, vi = v_g, data = PTGI_g)
main_analysis_model_PTGI




