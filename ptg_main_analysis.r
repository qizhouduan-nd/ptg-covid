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

View(main_data)



























