#checks for installation and loads packages
pacman::p_load(tidyverse, 
               here,
               metafor,
               emmeans)

#loading data----
# TODO - Erin - name all files like as you name functions (good practice)
# TODO - Erin about the directions of effects and expectations
# TODO - tell Erin about why we you do not want to name things with function names
dat <- read_csv(here("Data","Pilot_data.csv"))

# Load custom function to extract data 
source(here("R/functions.R"))

# I do not seem to have this issue
# data1 <- data %>% 
#   rename(
#     Study_ID = ï..Study_ID
#   )
#checking for outliers/potential typos---
#note that it very hard to check for typos/incorrect entries when there is a range of traits etc that are not directly comparable. Will need to re-do when effect sizes are calculated but all data looks correct at this stage
# TODO ask Erin for the rationale for this - also the naming?? why EC and CS???? 

# par(mfrow = c(3,1))
# boxplot(CC_mean ~ Study_ID, data = data)
# boxplot(CC_SD ~ Study_ID, data = data)
# boxplot(CC_n ~ Study_ID, data = data)
# 
# boxplot(EC_mean ~ Study_ID, data = data) 
# boxplot(EC_SD ~ Study_ID, data = data)
# boxplot(EC_n ~ Study_ID, data = data)
# 
# boxplot(CS_mean ~ Study_ID, data = data) 
# boxplot(CS_SD ~ Study_ID, data = data1)
# boxplot(CS_n ~ Study_ID, data = data1)
# 
# boxplot(ES_mean ~ Study_ID, data = data1)  
# boxplot(ES_SD ~ Study_ID, data = data1)
# boxplot(ES_n ~ Study_ID, data = data1)

# this may not be that useful as means and SD are measured on the same scale - maybe looking at CV (or z value) is more useful (i.e. )

# alternative
# this looks good
# CV: CC
qplot(factor(Study_ID), CC_SD/CC_mean, geom = "boxplot", data = dat_full)
# funnel-like plots
qplot(CC_SD/CC_mean, CC_n, data = data)

qplot(factor(Study_ID), EC_SD/EC_mean, geom = "boxplot", data = dat_full)
qplot(EC_SD/CC_mean, EC_n, data = data)

# TODO Study 16 is a problem for us - look at this study 
qplot(factor(Study_ID), CS_SD/CS_mean, geom = "boxplot", data = dat_full)
qplot(CS_SD/CS_mean, CS_n, data = data)

qplot(factor(Study_ID), ES_SD/ES_mean, geom = "boxplot", data = dat_full)
qplot(ES_SD/ES_mean, ES_n, data = data)

# getting effect size

effect_size <- effect_set(CC_n = "CC_n", CC_mean = "CC_mean", CC_SD = "CC_SD",
                          EC_n = "EC_n", EC_mean = "EC_mean" , EC_SD ="EC_SD",
                          CS_n = "CS_n", CS_mean = "CS_mean", CS_SD = "CS_SD",
                          ES_n = "ES_n", ES_mean = "ES_mean", ES_SD = "ES_SD",
                          data = dat_full)

# which one has all the data avaiable
full_info <- which(complete.cases(effect_size) == TRUE)

dat_effect <- cbind(dat_full, effect_size)
names(dat_effect)

# delete all the ones without complete data sets 
# TODO - we need to sort this out a bit more later
dat <- dat_effect[full_info, ]

dim(dat_effect)
dimentions <- dim(dat) # 7 less

# TODO - NA for all at the moment  - FIX
dat$ES_ID <- 1:dimentions[1]


# TODO - need to think about VCV??
# TODO - need to do something about Strain
# TODO - talk Erin about the direction - this is super importnat (not quite sure - what to do with the interaction - think about this a bit later)
# modeling with lnRR
# enviroment
mod_E0 <- rma.mv(yi = lnRR_E, V = lnRRV_E, random = list(~1|Study_ID, 
                                                        # ~ 1|Strain, does not run as we have NA
                                                         ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_E0)

funnel(mod_E0)

# stress
mod_S0 <- rma.mv(yi = lnRR_S, V = lnRRV_S, random = list(~1|Study_ID, 
                                                         # ~ 1|Strain, does not run as we have NA
                                                         ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_S0)
funnel(mod_S0)

# interaction
mod_ES0 <- rma.mv(yi = lnRR_ES, V = lnRRV_ES, random = list(~1|Study_ID, 
                                                         # ~ 1|Strain, does not run as we have NA
                                                         ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_ES0)
funnel(mod_ES0)


# TODO Try SMD
# TODO Try key modeators (meta-regression)