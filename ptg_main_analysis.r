###############################
### ptg main analysis script
###############################
library(tidyverse)
library(readxl)
library(metafor)
library(psych)
setwd(dirname(rstudioapi::documentPath()))

## load data
shortlist = read_excel('ptg_shortlist.xlsx')
## look at how many studies uses PTGI
shortlist %>% group_by(`scale type`) %>% count() # 20 PTGI and 10 PTGI-SF 

## we can do normalization or we can perform separate analysis for these two types of studies
## check the sample size
sum(shortlist$`sample size`) ## overall
PTGI_dat = shortlist %>% filter(`scale type` == 'PTGI')
PTGISF_dat = shortlist %>% filter(`scale type` != 'PTGI')

################################################################
################################################################
#### stretch PTGI-SF 
PTGISF_transformed = PTGISF_dat %>% mutate(`effect size` = `effect size` / 10 * 21) %>% 
  mutate(sd = sqrt(sd^2 * 2.1^2)) 
PTGISF_transformed
PTGI = rbind(PTGI_dat, PTGISF_transformed) %>% select(`effect size`, sd, `sample size`)

### start analysis with all the studies (PTGI gets successfully transformed)
PTGI_num = sum((PTGI_dat$`sample size` - 1) * PTGI_dat$sd^2)
PTGI_denom = sum(PTGI_dat$`sample size`) - length(PTGI_dat$`sample size`)
PTGI_sp = PTGI_num / PTGI_denom
PTGI_pooled_sd = sqrt(PTGI_sp)
PTGI_pooled_sd

cutoff = 45
PTGI_g = PTGI %>% 
  mutate(PTGI_num = (sum(`sample size`) - 1) * sd^2) %>% 
  mutate(PTGI_denom = sum(`sample size`) - length(`sample size`)) %>% 
  mutate(PTGI_pooled_sd = sqrt(PTGI_num / PTGI_denom)) %>% 
  mutate(g = (`effect size` - cutoff) / PTGI_pooled_sd) %>% 
  mutate(v_g = 2 * (1 - 0) / (`sample size`) + g^2 / (2 * (`sample size` - 1)))

## now we have the ingredient to perform meta analysis
PTGI_g[,c('g', 'v_g')]

## we run intercept only model for main analysis (this would be random intercept model)
main_analysis_model_PTGI = rma(g ~ 1, vi = v_g, data = PTGI_g)
main_analysis_model_PTGI

## check a few descriptives
describe(PTGI_g)
describe(PTGI_g$g)






