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
                                           `effect size`, sd, `Mean Age`, Countries, Groups) %>% 
  mutate(`effect size` = round(`effect size`,2)) %>% 
  mutate(`Mean Age` = round( as.numeric(`Mean Age`),2)) %>% 
  mutate(sd = round(sd,2))
## this is table 1
table_1 = PTGI_main_analysis_table %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria") 

# table_1 %>% as_image(width = 4)

PTGI_main_analysis_table[1:40,] %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria") 

PTGI_main_analysis_table[40:75,] %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria") 


## create tables for subgroup analysis (table 3 in the manuscript
PTGI_subgroup = PTGI %>% dplyr::select("Source","PTSD", "Anxiety", 
                                       "Depression", "Social Support", 
                                       "Coping","Sprituality/Religion") %>% 
  mutate(PTSD = ifelse(PTSD == 1, '✔', ' ')) %>% 
  mutate(Anxiety = ifelse(Anxiety == 1, '✔', ' ')) %>%  
  mutate(Depression = ifelse(Depression == 1, '✔', ' ')) %>% 
  mutate(`Social Support` = ifelse(`Social Support` == 1, '✔', ' ')) %>% 
  mutate(Coping = ifelse(Coping == 1, '✔', ' ')) %>% 
  mutate(`Sprituality/Religion` = ifelse(`Sprituality/Religion` == 1, '✔', ' '))
## this is table 3
PTGI_subgroup[1:40, ]  %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria")

PTGI_subgroup[41:75, ]  %>% 
  kbl() %>% 
  kable_classic(full_width = F, html_font = "Cambria")

## check age range
range(as.numeric(PTGI_g$`Mean Age`)[!is.na(as.numeric(PTGI_g$`Mean Age`))])

which(as.numeric(PTGI_g$`Mean Age`) == 16)
PTGI_g$Source


## table 2 

line1 = c(" ", "Estimate", "se", 'Z', 'p', 'CI LB', 'CI UP', ' ')
line2 = c('Intercept', '1.95', '0.62', '3.16', '0.002', '0.74', '3.16', ' ')
line3= c(' Heterogeneity Statistics', ' ', ' ', ' ', ' ', ' ', ' ', ' ')
line4 = c(" ", "Tau", "Tau^2", 'I^2', 'H^2', 'df', 'Q', 'p')
line5 = c(" ", "5.34", "28.47", '99.99%', '9695', '74', '15829', '< 0.001')

table2 = rbind(c('Random Effects Model (k=75)', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
               c(" ", "Estimate", "se", 'Z', 'p', 'CI LB', 'CI UP', ' '), 
               c('Intercept', '1.95', '0.62', '3.16', '0.002', '0.74', '3.16', ' '),
               c(' Heterogeneity Statistics', ' ', ' ', ' ', ' ', ' ', ' ', ' '),
               c(" ", "Tau", "Tau^2", 'I^2', 'H^2', 'df', 'Q', 'p'),
               c(" ", "5.34", "28.47", '99.99%', '9695', '74', '15829', '< 0.001'))

tibble(table2)

x = kbl(table2) %>% kable_classic(full_width = F, html_font = "Cambria") 

row_spec(x,1, hline_after = TRUE)

## table 4
Correlate = c('Anxiety', 'Depression', 'PTSD', 'Social Support', 'Coping', 'Spirituality')
K = c(28, 19, 20, 17, 38, 25)
N = c(19522, 21802, 24033, 20912, 23386, 15263)
CI_lower = c(-4.83, 0.15, -0.42, -1.61, 0.02, -0.93)
CI_upper = c(0.13, -4.93, 1.49, 4.24, 4.82, 4.24)
I2 = c(99.99, 99.99, 99.99, 99.99, 99.99, 99.99)

kbl(data.frame(Correlate, K,N,CI_lower, CI_upper, I2)) %>% kable_classic(full_width = F, html_font = "Cambria") 

