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
shortlist = read_excel('effect_sizes_and_moderators.xlsx')
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
PTGI = rbind(PTGI_dat, PTGISF_transformed)

head(PTGI)

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

################################################################
## Subgroup analysis
################################################################
## PTSD 
PTGI_PTSD = rma(g ~ PTSD, vi = v_g, data = PTGI_g)
PTGI_PTSD

sum(PTGI_g$`sample size` * PTGI_g$PTSD)
PTGI_g$Source[ifelse(PTGI_g$PTSD == 1, TRUE, FALSE)]
### Anxiety Scales
PTGI_Anxiety = rma(g ~ Anxiety, vi = v_g, data = PTGI_g)
PTGI_Anxiety

sum(PTGI_g$Anxiety)
sum(PTGI_g$`sample size` * PTGI_g$Anxiety)
PTGI_g$Source[ifelse(PTGI_g$Anxiety == 1, TRUE, FALSE)]

### Depression Scales
PTGI_Depression = rma(g ~ Depression, vi = v_g, data = PTGI_g)
PTGI_Depression

sum(PTGI_g$Depression)
sum(PTGI_g$`sample size` * PTGI_g$Depression)
PTGI_g$Source[ifelse(PTGI_g$Depression == 1, TRUE, FALSE)]

### social support scales
PTGI_Support = rma(g ~ `Social Support`, vi = v_g, data = PTGI_g)
PTGI_Support

sum(PTGI_g$`Social Support`)
sum(PTGI_g$`sample size` * PTGI_g$`Social Support`)
PTGI_g$Source[ifelse(PTGI_g$`Social Support` == 1, TRUE, FALSE)]

## coping scales
PTGI_Coping = rma(g ~ `Coping`, vi = v_g, data = PTGI_g)
PTGI_Coping

sum(PTGI_g$`Coping`)
sum(PTGI_g$`sample size` * PTGI_g$`Coping`)
PTGI_g$Source[ifelse(PTGI_g$`Coping` == 1, TRUE, FALSE)]

### religion scales
PTGI_religion = rma(g ~ `Sprituality/Religion`, vi = v_g, data = PTGI_g)
PTGI_religion

sum(PTGI_g$`Sprituality/Religion`)
sum(PTGI_g$`sample size` * PTGI_g$`Sprituality/Religion`)
PTGI_g$Source[ifelse(PTGI_g$`Sprituality/Religion` == 1, TRUE, FALSE)]

### coping and anxiety are significant with coping clearly significant. 



################################################################
## Tables and Figures
################################################################
## forest plot
ptsd_sample = PTGI_g %>% filter(`PTSD` == 1) %>% dplyr::select(Source, `sample size`)
forest(main_analysis_model_PTGI, 
       header="Author(s) and Year", mlab="", shade=TRUE,
       cex=0.75)

## create tables for main analysis (table 1 in the manuscript)
PTGI_main_analysis_table = PTGI_g %>% dplyr::select(Source, `sample size`, 
                                           `effect size`, sd) %>% 
  mutate(`effect size` = round(`effect size`,2)) %>% 
  mutate(sd = round(sd,2))
## this is table 1
table_1 = PTGI_main_analysis_table %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria") 

table_1 %>% as_image(width = 4)

kable(PTGI_main_analysis_table, 'pdf') %>% as_image(width = 4)


library(knitr)

mykable <- kable(matrix(rep(letters,600*8),
                        nrow = 600,
                        ncol =8))

library(kableExtra)
kableExtra::save_kable(x = mykable,
                       file = "mykable.pdf")



## create tables for subgroup analysis (table 3 in the manuscript)
# Source	Year	Sample size	Male,%	Age (mean)	End Point	Follow up, y	Determinant
PTGI_subgroup = PTGI %>% dplyr::select("Source", "sample size","PTSD", "Anxiety", 
                                       "Depression", "Social Support", 
                                       "Coping","Sprituality/Religion")
## this is table 3
PTGI_subgroup  %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria")







