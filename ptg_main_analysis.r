###############################
### ptg main analysis script
###############################
library(tidyverse)
library(readxl)
library(metafor)
library(psych)
library(kableExtra)
setwd(dirname(rstudioapi::documentPath()))

## load data
shortlist = read_excel('ptg_shortlist.xlsx')
## look at how many studies uses PTGI
shortlist %>% group_by(`scale type`) %>% count() # 20 PTGI and 10 PTGI-SF 
table(shortlist$PTSD)
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
PTGI = rbind(PTGI_dat, PTGISF_transformed) %>% 
  select(`Source`,`effect size`, sd, `sample size`, 
         `PTSD`, Anxiety, Depression, `Social Support`,
         `Coping`, `Sprituality/Religion`)

PTGI = PTGI %>% mutate(Anxiety = ifelse(Anxiety == 'yes',1,0)) %>% 
  mutate(Depression = ifelse(Depression == 'yes',1,0)) %>% 
  mutate(`Social Support` = ifelse(`Social Support` == 'yes',1,0)) %>%
  mutate(`Coping` = ifelse(`Coping` == 'yes',1,0)) %>% 
  mutate(`Sprituality/Religion` = ifelse(`Sprituality/Religion` == 'yes',1,0)) %>% 
  mutate(PTSD = ifelse(PTSD == 'Yes',1,0)) 

View(PTGI)

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
  mutate(v_g = 2 * (1 - 0) / (`sample size`) + g^2 / (2 * (`sample size` - 1))) %>% 
  mutate(PTSD = ifelse(`PTSD` == 'Yes', 1,0 ))

## now we have the ingredient to perform meta analysis
PTGI_g[,c('g', 'v_g')]

## we run intercept only model for main analysis (this would be random intercept model)
main_analysis_model_PTGI = rma(g ~ 1, vi = v_g, data = PTGI_g)
main_analysis_model_PTGI


## check a few descriptives
describe(PTGI_g)
describe(PTGI_g$g)

## subgroup analysis for PTSD
PTGI_PTSD = rma(g ~ PTSD, vi = v_g, data = PTGI_g)
PTGI_PTSD

ptsd_sample = PTGI_g %>% filter(`PTSD` == 1) %>% select(`Source`, `sample size`)
sum(ptsd_sample$`sample size`)
## forest plot
forest(main_analysis_model_PTGI, 
       header="Author(s) and Year", mlab="", shade=TRUE,
       cex=0.75)

## escalc portion
SMD_es <- escalc(measure = "SMD",
                 m1i = `effect size`,
                 sd1i = `sd`,
                 n1i = `sample size`,
                 m2i = rep(45,30),
                 sd2i = `sd`,
                 n2i = `sample size`,
                 data = PTGI,
                 var.names = c("smd", "varsmd"))

res = rma(yi = smd, vi = varsmd, data = SMD_es)
forest(res, cex=0.7, 
       header="Author(s) and Year", mlab="", shade=TRUE, slab = SMD_es$Source)

## create tables for subgroup analysis












