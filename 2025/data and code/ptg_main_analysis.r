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
                               data = PTGI_dat, 
                               append = TRUE)
calculate_effect_size ## noticed some quite high values; 
# this indicates that maybe screening for outlier is needed

## we run intercept only model for main analysis (this would be random intercept model)
main_analysis_model_PTGI = rma(yi, vi, data = calculate_effect_size)
main_analysis_model_PTGI


# forest plot
forest(main_analysis_model_PTGI, slab = `Source`, header = "Study")
# influence
influence(main_analysis_model_PTGI)


## moderators
mod.groups = rma(yi, vi, mods = ~ factor(Groups), data = calculate_effect_size)
mod.groups
##############################################
##############################################
## examine PTGI data
high_exposure_group = PTGI_dat$Groups == "Front Line worker" | 
  PTGI_dat$Groups == "Nurses" | 
  PTGI_dat$Groups == "Medical Doctors" | 
  PTGI_dat$Groups == "Health Care Workers" | 
  PTGI_dat$Groups == "Patients"


first.half <- data.frame(PTGI_dat[high_exposure_group, 
                    -which(names(PTGI_dat) %in% 
                             c("Groups", "Mean Age" ,
                               "scale type", 
                               "female proportion(Marg)"))])

second.half <- data.frame(PTGI_dat[!high_exposure_group, 
                                   -which(names(PTGI_dat) %in% 
                                            c("Groups", "Mean Age" ,
                                              "scale type", 
                                              "female proportion(Marg)"))])

new_dat <- cbind(rbind(first.half, second.half),
      exposure = ifelse(high_exposure_group == TRUE, 1, 0))

### in-balanced group
calculate_effect_size_2 <-escalc(measure = "SMD", 
                               m1i = first.half$effect.size, 
                               sd1i = first.half$sd,
                               n1i = first.half$sample.size, 
                               m2i = second.half$effect.size, 
                               sd2i = second.half$sd,
                               n2i = second.half$sample.size)












