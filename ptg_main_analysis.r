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
shortlist = read_excel('ptg_shortlist_new.xlsx')
## look at how many studies uses PTGI
shortlist %>% group_by(`scale type`) %>% count() # 20 PTGI and 10 PTGI-SF 
table(shortlist$PTSD)
## we can do normalization or we can perform separate analysis for these two types of studies
## check the sample size
sum(shortlist$`sample size`) ## overall
PTGI_dat = shortlist %>% filter(`scale type` == 'PTGI')
PTGISF_dat = shortlist %>% filter(`scale type` != 'PTGI')


new_dat = shortlist[!is.na(as.numeric(shortlist$`effect size`)),]
new_dat

### start analysis with all the studies (PTGI gets successfully transformed)
PTGI_num = sum((as.numeric( new_dat$`sample size`) - 1)) * (as.numeric( new_dat$`sample size`) - 1)
PTGI_denom = sum(as.numeric(new_dat$`sample size`)) - length(as.numeric(new_dat$`sample size`))
PTGI_sp = PTGI_num / PTGI_denom
PTGI_pooled_sd = sqrt(PTGI_sp)
PTGI_pooled_sd

cutoff = 45
PTGI_g = new_dat %>% 
  mutate(`sample size` = as.numeric(`sample size`)) %>% 
  mutate(sd = as.numeric(sd)) %>% 
  mutate(PTGI_num = (sum(`sample size`) - 1) * sd^2) %>% 
  mutate(PTGI_denom = sum(`sample size`) - length(`sample size`)) %>% 
  mutate(PTGI_pooled_sd = sqrt(PTGI_num / PTGI_denom)) %>% 
  mutate(g = (`effect size` - cutoff) / PTGI_pooled_sd) %>% 
  mutate(v_g = 2 * (1 - 0) / (`sample size`) + g^2 / (2 * (`sample size` - 1)))


test = new_dat %>% 
  mutate(`sample size` = as.numeric(`sample size`))%>% 
  mutate(sd = as.numeric(sd)) %>% 
  mutate(`effect size` = as.numeric(`effect size`)) %>% 
  mutate(PTGI_num = (sum(`sample size`) - 1) * sd^2) %>% 
  mutate(PTGI_denom = sum(`sample size`) - length(`sample size`)) %>% 
  mutate(PTGI_pooled_sd = sqrt(PTGI_num / PTGI_denom)) %>% 
  mutate(g = (`effect size` - cutoff) / PTGI_pooled_sd) %>% 
  mutate(v_g = 2 * (1 - 0) / (`sample size`) + g^2 / (2 * (`sample size` - 1)))
  
View(test)
test[,c('g', 'v_g')]
main_analysis_model_PTGI = rma(g ~ 1, vi = v_g, data = test)
main_analysis_model_PTGI
## now we have the ingredient to perform meta analysis
PTGI_g[,c('g', 'v_g')]

## we run intercept only model for main analysis (this would be random intercept model)
main_analysis_model_PTGI = rma(g ~ 1, vi = v_g, data = PTGI_g)
main_analysis_model_PTGI

################################################################
################################################################
#### stretch PTGI-SF 
PTGISF_transformed = PTGISF_dat %>% mutate(`effect size` = `effect size` / 10 * 21) %>% 
  mutate(sd = sqrt(sd^2 * 2.1^2)) 
PTGISF_transformed
PTGI = rbind(PTGI_dat, PTGISF_transformed) %>% 
  dplyr::select(`Source`, `Publication Year`, `Male%`, `Mean Age` ,`effect size`, sd, `sample size`, 
         `PTSD`, Anxiety, Depression, `Social Support`,
         `Coping`, `Sprituality/Religion`)

PTGI = PTGI %>% mutate(Anxiety = ifelse(Anxiety == 'yes',1,0)) %>% 
  mutate(Depression = ifelse(Depression == 'yes',1,0)) %>% 
  mutate(`Social Support` = ifelse(`Social Support` == 'yes',1,0)) %>%
  mutate(`Coping` = ifelse(`Coping` == 'yes',1,0)) %>% 
  mutate(`Sprituality/Religion` = ifelse(`Sprituality/Religion` == 'yes',1,0)) %>% 
  mutate(PTSD = ifelse(PTSD == 'Yes',1,0)) %>% 
  mutate(`Male%` = round(as.numeric(`Male%`),2)) %>% 
  mutate(`Mean Age` = round(as.numeric(`Mean Age`),2))

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
  mutate(v_g = 2 * (1 - 0) / (`sample size`) + g^2 / (2 * (`sample size` - 1)))

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

## create tables for subgroup analysis (table 3 in the manuscript)
# Source	Year	Sample size	Male,%	Age (mean)	End Point	Follow up, y	Determinant
PTGI_subgroup = PTGI %>% dplyr::select("Source", "Publication Year", "sample size",
                                       "Male%", "Mean Age","PTSD", "Anxiety", 
                                       "Depression", "Social Support", 
                                       "Coping","Sprituality/Religion")

## this is table 3
PTGI_subgroup  %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria")

## all the risk factors
study_names = PTGI_subgroup %>% filter(PTSD == 1 | Anxiety == 1 | Depression == 1) %>% 
  select(Source)


sort(substr(study_names$Source,1,1))
str_rank(substr(study_names$Source,1,1))
str_order(substr(study_names$Source,1,1))
## create tables for main analysis (table 1 in the manuscript)
PTGI_main_analysis_table = PTGI %>% select(Source, `Publication Year`, 
                                           `sample size`, `Male%`, `Mean Age`,
                                           `effect size`, sd) %>% 
  mutate(`effect size` = round(`effect size`,2)) %>% 
  mutate(sd = round(sd,2))

## this is table 1
PTGI_main_analysis_table %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria")


### more subgroup analysis
PTGI_Anxiety = rma(g ~ Anxiety, vi = v_g, data = PTGI_g)
PTGI_Anxiety

PTGI_Depression = rma(g ~ Depression, vi = v_g, data = PTGI_g)
PTGI_Depression

PTGI_Support = rma(g ~ `Social Support`, vi = v_g, data = PTGI_g)
PTGI_Support

PTGI_Coping = rma(g ~ `Coping`, vi = v_g, data = PTGI_g)
PTGI_Coping

PTGI_religion = rma(g ~ `Sprituality/Religion`, vi = v_g, data = PTGI_g)
PTGI_religion

## making table 4 
Correlate = c("Anxiety", "Depression", "PTSD", 
              "Social Support", "Coping", "Sprituality/Religion")

K = c(sum(PTGI_g$Anxiety),
sum(PTGI_g$Depression),
sum(PTGI_g$PTSD),
sum(PTGI_g$`Social Support`),
sum(PTGI_g$Coping),
sum(PTGI_g$`Sprituality/Religion`))

N = c(sum(PTGI_g %>% filter(Anxiety == 1) %>% select("sample size")),
sum(PTGI_g %>% filter(Depression == 1) %>% select("sample size")),
sum(PTGI_g %>% filter(PTSD == 1) %>% select("sample size")),
sum(PTGI_g %>% filter(`Social Support` == 1) %>% select("sample size")),
sum(PTGI_g %>% filter(Coping == 1) %>% select("sample size")),
sum(PTGI_g %>% filter(`Sprituality/Religion` == 1) %>% select("sample size")))

ES = round(c(PTGI_Anxiety$beta[2], PTGI_Depression$beta[2], PTGI_PTSD$beta[2],
       PTGI_Support$beta[2], PTGI_Coping$beta[2], PTGI_religion$beta[2]),2)


CI_lower = round(c(PTGI_Anxiety$ci.lb[2], PTGI_Depression$ci.lb[2], PTGI_PTSD$ci.lb[2],
             PTGI_Support$ci.lb[2], PTGI_Coping$ci.lb[2], PTGI_religion$ci.lb[2]),2)
CI_upper = round(c(PTGI_Anxiety$ci.ub[2], PTGI_Depression$ci.ub[2], PTGI_PTSD$ci.ub[2],
                   PTGI_Support$ci.ub[2], PTGI_Coping$ci.ub[2], PTGI_religion$ci.ub[2]),2)

I2 = round(c(PTGI_Anxiety$I2, PTGI_Depression$I2, PTGI_PTSD$I2,
             PTGI_Support$I2, PTGI_Coping$I2, PTGI_religion$I2),2)

table4_content = data.frame(Correlate, K, N, ES, CI_lower, CI_upper, I2)

table4_content %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria")

## number of cross-sectional studies
sum(shortlist$`cross-sectional`)


