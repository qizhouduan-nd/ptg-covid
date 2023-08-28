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
head(PTGI_dat)
PTGI_num = sum((PTGI_dat$`sample size` - 1) * PTGI_dat$sd^2)
PTGI_denom = sum(PTGI_dat$`sample size`) - length(PTGI_dat$`sample size`)
PTGI_sp = PTGI_num / PTGI_denom
PTGI_pooled_sd = sqrt(PTGI_sp)
PTGI_pooled_sd
## based on literature 45 is a reasonable cutoff point, and we use it with the pooled sd
## we use hedges' g 
cutoff = 45
complete_PTGI_dat = PTGI_dat %>% 
  mutate(g = (`effect size` - cutoff) / PTGI_pooled_sd) %>% 
  mutate(v_g = 2 * (1 - 0) / (`sample size`) + g^2 / (2 * (`sample size` - 1)))

## now we have the ingredient to perform meta analysis
complete_PTGI_dat[,c('g', 'v_g')]

## we run intercept only model for main analysis (this would be random intercept model)
main_analysis_model_PTGI = rma(g ~ 1, vi = v_g, data = complete_PTGI_dat)
main_analysis_model_PTGI
## overall, high heterogeneity so we can be assured that the random intercept model is correct
## for more accurate indicator of heterogeneity, we consult tau square
## also high heterogeneity is an incentive to conduct subgroup analysis
## significant positive change

### now for PTGISF
head(PTGISF_dat)
PTGISF_num = sum((PTGISF_dat$`sample size` - 1) * PTGISF_dat$sd^2)
PTGISF_denom = sum(PTGISF_dat$`sample size`) - length(PTGISF_dat$`sample size`)
PTGISF_sp = PTGISF_num / PTGISF_denom
PTGISF_pooled_sd = sqrt(PTGISF_sp)
PTGISF_pooled_sd
## based on literature 15 is a reasonable cutoff point, and we use it with the pooled sd
## we use hedges' g 
cutoff = 2.15 * 10
complete_PTGISF_dat = PTGISF_dat %>% 
  mutate(g = (`effect size` - cutoff) / PTGISF_pooled_sd) %>% 
  mutate(v_g = 2 * (1 - 0) / (`sample size`) + g^2 / (2 * (`sample size` - 1))) # consider using escalc

## now we have the ingredient to perform meta analysis
complete_PTGISF_dat[,c('g', 'v_g')]

## we run intercept only model for main analysis (this would be random intercept model)
main_analysis_model_PTGISF = rma(g ~ 1, vi = v_g, data = complete_PTGISF_dat)
main_analysis_model_PTGISF

## get how many studies is from which country
main_data %>% group_by(`Countries of Origin`) %>% count() %>% arrange(desc(n))

## sample size ranges from 
describe(main_data$`sample size`)


