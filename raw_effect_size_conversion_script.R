### effect sizes calc script 


## pulled sample variance for chen 2021 
n = 422
n1 = n * 10.7/100
n2 = n * 20.1/100
n3 = n * 42.2/100
n4 = n * 27/100

s1 = 12.25
s2 = 10.73
s3 = 11.28
s4 = 7.75

sp_num = (n1 - 1) * s1^2 + (n2 - 1) * s2^2 + (n3 - 1) * s3^2 + (n4 - 1) * s4^2
sp_denom = n1 + n2 + n3 + n4 - 4

sp = sp_num / sp_denom
sd_p = sqrt(sp)
sd_p

## adj study male and female average
male_n = 226
female_n = 155

male_m = 21.68
female_m = 23.1

male_sd = 5.38
female_sd = 4.53

sp_num = (male_n - 1) * male_sd^2 + (female_n - 1) * female_sd^2 
sp_denom = male_n + female_n - 2

sp_adj = sp_num / sp_denom
sd_p_adj = sqrt(sp_adj)
sd_p_adj

adj_mean = (male_m * male_n + female_m * female_n) / (male_n + female_n)
adj_mean

round(c(adj_mean, sd_p_adj) ,3)

### lyu study mean and standard deviation 
## study 1 (they have a modified version with 20 items)
round(c(3.92 * 20, sqrt(0.70^2 * 20^2)), 2)

## study 2 
round(c(2.94 * 20, sqrt(0.74^2 * 20^2)), 2)

### lau study 
round(c(2.53 * 21, sqrt(0.82^2 * 21^2)), 2)

### Chasson 
jew_n = 517
arab_n = 399

jew_m = 2.46 * 21
arab_m = 2.91 * 21

jew_sd = sqrt( 1.09 ^2 * 21^2)
arab_sd = sqrt( 0.6 ^2 * 21^2)

joint_m = (jew_m * jew_n + arab_m * arab_n) / (jew_n + arab_n)
joint_m

sp_num = (jew_n - 1) * jew_sd^2 + (arab_n - 1) * arab_sd^2 
sp_denom = jew_n + arab_n- 2

sp_cha = sp_num / sp_denom
sd_p_cha = sqrt(sp_cha)
sd_p_cha

### ulset 
round(c(2.16 * 10, sqrt(0.68^2 * 10^2)), 2)

### arnout 
male_n = 116
female_n = 249

male_m = 60.2414
female_m = 67.5020

male_sd = 20.27018
female_sd = 16.75659

sp_num = (male_n - 1) * male_sd^2 + (female_n - 1) * female_sd^2 
sp_denom = male_n + female_n - 2

sp_arnout = sp_num / sp_denom
sd_p_arnout = sqrt(sp_adj)
sd_p_arnout

arnout_mean = (male_m * male_n + female_m * female_n) / (male_n + female_n)
arnout_mean

round(c(arnout_mean, sd_p_arnout) ,3)