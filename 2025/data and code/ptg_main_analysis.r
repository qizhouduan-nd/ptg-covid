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

## find out pattern depending on the regions of the world
length(unique(PTGI_dat$continents))

library(ggplot2)

ggplot(PTGI_dat, aes(x = `effect size`, fill = continents, color = continents)) +
  geom_histogram(position = "dodge", 
                 alpha = 0.6,
                 binwidth = 5) +
  geom_density(aes(y = after_stat(count) * 5), # Scale density to match histogram height
               alpha = 0.4,
               linewidth = 1) +  # Make density lines thicker
  theme_minimal() +
  labs(
    title = "Distribution by Continent",
    x = "Effect Size",
    y = "Count"
  ) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() # Match density line colors with fill colors

## there is only one oceania country
sum(PTGI_dat$continents == 'Oceania')
