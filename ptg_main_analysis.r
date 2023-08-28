###############################
### ptg main analysis script
###############################
library(tidyverse)
library(readxl)
setwd(dirname(rstudioapi::documentPath()))

## load data
shortlist = read_excel('ptg_shortlist.xlsx')
t1_raw = read_excel('Covid T1 Raw.xlsx')

## select variables for main analysis
main_data = merge(shortlist, t1_raw,by = 'Source') %>% 
  select(Source, 'scale type', 'effect size', 'sd', 'sample size', 
         'Groups with PTG', 'Countries of Origin')

## look at how many studies uses PTGI
main_data %>% group_by(`scale type`) %>% count() # 13 effect sizes uses PTGI 6 uses PTGI-SF

## we can do normalization or we can perform separate analysis for these two types of studies
## check the sample size
sum(main_data$`sample size`) ## overall
PTGI_dat = main_data %>% filter(`scale type` == 'PTGI')
PTGISF_dat = main_data %>% filter(`scale type` != 'PTGI')
sum(PTGI_dat$`sample size`)
sum(PTGISF_dat$`sample size`)

## let's do meta analysis for the PTGI studies
























