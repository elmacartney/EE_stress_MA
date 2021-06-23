#checks for installation and loads packages
install.packages("devtools")
install.packages("tidyverse")
install.packages("metafor")
install.packages("patchwork")
install.packages("R.rsp")
install.packages("Rtools")

devtools::install_github("itchyshin/orchard_plot", subdir = "orchaRd", force = TRUE, build_vignettes = TRUE) #need this to get orchaRD package

pacman::p_load(tidyverse, 
               here,
               metafor,
               emmeans,
               orchaRd) #orchardRD is used to calculate I2 and R2 


#loading data----
# TODO - Erin - name all files like as you name functions (good practice)
# TODO - Erin about the directions of effects and expectations
# TODO - tell Erin about why we you do not want to name things with function names
dat <- read_csv(here("Data","Pilot_data.csv"))

# Load custom function to extract data 
source(here("R/functions.R")) # this has all the function used to calculate effect sizes

# I do not seem to have this issue
# data1 <- data %>% 
#   rename(
#     Study_ID = ï..Study_ID
#   )
#checking for outliers/potential typos----

# this looks good
# CV: CC
qplot(factor(Study_ID), CC_SD/CC_mean, geom = "boxplot", data = dat)
# funnel-like plots
qplot(CC_SD/CC_mean, CC_n, data = dat)

qplot(factor(Study_ID), EC_SD/EC_mean, geom = "boxplot", data = dat)
qplot(EC_SD/CC_mean, EC_n, data = dat)

# TODO Study 16 is a problem for us - look at this study 
qplot(factor(Study_ID), CS_SD/CS_mean, geom = "boxplot", data = dat)
qplot(CS_SD/CS_mean, CS_n, data = dat)

qplot(factor(Study_ID), ES_SD/ES_mean, geom = "boxplot", data = dat)
qplot(ES_SD/ES_mean, ES_n, data = dat)

# getting effect size----

effect_size <- effect_set(CC_n = "CC_n", CC_mean = "CC_mean", CC_SD = "CC_SD",
                          EC_n = "EC_n", EC_mean = "EC_mean" , EC_SD ="EC_SD",
                          CS_n = "CS_n", CS_mean = "CS_mean", CS_SD = "CS_SD",
                          ES_n = "ES_n", ES_mean = "ES_mean", ES_SD = "ES_SD",
                          data = dat) #this is running the function that we have already loaded

# which one has all the data avaiable
full_info <- which(complete.cases(effect_size) == TRUE) #removing missing effect sizes

dat_effect <- cbind(dat, effect_size)
names(dat_effect)

# TODO - we need to sort this out a bit more later
dat <- dat_effect[full_info, ]

dim(dat_effect)
dimentions <- dim(dat) # 7 less

# TODO - NA for all at the moment  - FIX
dat$ES_ID <- 1:dimentions[1]

# TODO - need to think about VCV??
# TODO - need to do something about Strain - probably include as a random effect

# flipping effect sizes ----
#flipping lnRR for values where higher = worse

dat$lnRR_Ea <- ifelse(dat$Response_direction == 2, dat$lnRR_E*-1,ifelse(is.na(dat$Response_direction) == TRUE, NA, dat$lnRR_E)) # currently NAswhich causes error
dat$lnRR_Sa  <- ifelse(dat$Response_direction == 2, dat$lnRR_S*-1,ifelse(is.na(dat$Response_direction) == TRUE, NA, dat$lnRR_S)) # currently NAswhich causes error
dat$lnRR_ESa <-  ifelse(dat$Response_direction == 2, dat$lnRR_ES*-1,ifelse(is.na(dat$Response_direction) == TRUE, NA, dat$lnRR_ES)) # currently NAswhich causes error

# modeling with lnRR
# environment

mod_E0 <- rma.mv(yi = lnRR_Ea, V = lnRRV_E, random = list(~1|Study_ID, 
                                                        # ~ 1|Strain, does not run as we have NA
                                                         ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_E0) #learning and memory significnatly better when enrichment

funnel(mod_E0)

#heterogeneity
i2_ml(mod_E0) #high hetero

#use r2_ml for meta-regression

# stress
mod_S0 <- rma.mv(yi = lnRR_Sa, V = lnRRV_S, random = list(~1|Study_ID, 
                                                         # ~ 1|Strain, does not run as we have NA
                                                         ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_S0) #learning and memory significantly worse when stressed
funnel(mod_S0)
i2_ml(mod_S0) #high hetero

# interaction
mod_ES0 <- rma.mv(yi = lnRR_ESa, V = lnRRV_ES, random = list(~1|Study_ID, 
                                                         # ~ 1|Strain, does not run as we have NA
                                                         ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_ES0) #significantly positive- seems like SE is better than just E by itself
funnel(mod_ES0)
i2_ml(mod_ES0) #high hetero

# TODO Try SMD
# TODO Try key moderators (meta-regression)