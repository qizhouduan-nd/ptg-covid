###############################
### ptg main analysis script
###############################
library(tidyverse)
ptg_effect_sizes <- read_excel("Desktop/ptg effect sizes.xlsx")
View(ptg_effect_sizes)   


ptg_effect_sizes %>% select(`scale type` )

ptgi_studies = ptg_effect_sizes %>% filter(`scale type` == 'PTGI')

as.numeric(ptgi_studies$`effect size`)




