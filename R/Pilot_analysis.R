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

qplot(factor(Study_ID), CS_SD/CS_mean, geom = "boxplot", data = dat)
qplot(CS_SD/CS_mean, CS_n, data = dat)

qplot(factor(Study_ID), ES_SD/ES_mean, geom = "boxplot", data = dat)
qplot(ES_SD/ES_mean, ES_n, data = dat)

#fixing negative values in study 16
#modifying study 16 that has negative values: shifting everything up by the lowest value
#note, this results in inf lnRR - is it because of the zeros?
#dat1 <- dat %>%
 # rowwise() %>%
  #mutate(min.mean = min(CC_mean, EC_mean, CS_mean, ES_mean)) %>%
  #ungroup() %>%
  #mutate(CC_mean = case_when(First_author == "Wang" ~ CC_mean + abs(min.mean),
                            # TRUE ~ as.numeric(.$CC_mean))) %>%
  #mutate(EC_mean = case_when(First_author == "Wang" ~ EC_mean + abs(min.mean), 
                             #TRUE ~ as.numeric(.$EC_mean))) %>%
  #mutate(CS_mean = case_when(First_author == "Wang" ~ CS_mean + abs(min.mean),
                             #TRUE ~ as.numeric(.$CS_mean))) %>%
  #mutate(ES_mean = case_when(First_author == "Wang" ~ ES_mean + abs(min.mean),
                             #TRUE ~ as.numeric(.$ES_mean)))

# getting effect size----

effect_size <- effect_set(CC_n = "CC_n", CC_mean = "CC_mean", CC_SD = "CC_SD",
                          EC_n = "EC_n", EC_mean = "EC_mean" , EC_SD ="EC_SD",
                          CS_n = "CS_n", CS_mean = "CS_mean", CS_SD = "CS_SD",
                          ES_n = "ES_n", ES_mean = "ES_mean", ES_SD = "ES_SD",
                          data = dat) #this is running the function that we have already loaded

# which one has all the data available
full_info <- which(complete.cases(effect_size) == TRUE) #removing missing effect sizes

dat_effect <- cbind(dat, effect_size)
names(dat_effect)

# TODO - we need to sort this out a bit more later
dat <- dat_effect[full_info, ]

dim(dat_effect)
dimentions <- dim(dat) # 7 less

# TODO - NA for all at the moment  - FIX
#dat$ES_ID <- 1:dimentions[1]

# TODO - need to think about VCV?? 
# TODO - need to do something about Strain - probably include as a random effect

# flipping effect sizes ----
#flipping lnRR for values where higher = worse

dat$lnRR_Ea <- ifelse(dat$Response_direction == 2, dat$lnRR_E*-1,ifelse(is.na(dat$Response_direction) == TRUE, NA, dat$lnRR_E)) # currently NAswhich causes error
dat$lnRR_Sa  <- ifelse(dat$Response_direction == 2, dat$lnRR_S*-1,ifelse(is.na(dat$Response_direction) == TRUE, NA, dat$lnRR_S)) # currently NAswhich causes error
dat$lnRR_ESa <-  ifelse(dat$Response_direction == 2, dat$lnRR_ES*-1,ifelse(is.na(dat$Response_direction) == TRUE, NA, dat$lnRR_ES)) # currently NAswhich causes error

# modeling with lnRR----

# environment----

mod_E0 <- rma.mv(yi = lnRR_Ea, V = lnRRV_E, random = list(~1|Study_ID, 
                                                        # ~ 1|Strain, does not run as we have NA
                                                         ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_E0) #learning and memory significantly better when enrichment

funnel(mod_E0)

#heterogeneity
i2_ml(mod_E0) #high hetero

#potentially important moderators:all meta-regression excludes intercept
# Age exposures
# Type exposure: Social, exercise
# Type stressor
# Stress duration
# appetitve vs aversive
# learning vs memory
# Type learning

#type of learning
dat$Type_learning<-as.factor(dat$Type_learning)

mod_E1 <- rma.mv(yi = lnRR_Ea, V = lnRRV_E, mod = ~Type_learning, random = list(~1|Study_ID, 
                                                          # ~ 1|Strain, does not run as we have NA
                                                          ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_E1)

mod_E1a <- rma.mv(yi = lnRR_Ea, V = lnRRV_E, mod = ~Type_learning-1, random = list(~1|Study_ID, 
                                                                                # ~ 1|Strain, does not run as we have NA
                                                                                ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_E1a) #enriched animals do much better at conditioning 
r2_ml(mod_E1a) 

#learning vs memory
dat1<-dat %>% subset(Learning_vs_memory < 3) #remove 3 = unclear

dat1$Learning_vs_memory<-as.factor(dat1$Learning_vs_memory)

mod_E2 <-  rma.mv(yi = lnRR_Ea, V = lnRRV_E, mod = ~Learning_vs_memory-1, random = list(~1|Study_ID, 
                                                                                   # ~ 1|Strain, does not run as we have NA
                                                                                   ~1|ES_ID),
                  test = "t",
                  data = dat1)

summary(mod_E2) #learning and memory are important: need to remove category 3 = unclear
r2_ml(mod_E2) #marginal R2 is low

#appetitive_vs_aversive
dat2<-  dat %>% subset(Appetitive_vs_aversive < 3) #remove 3 = unclear

dat2$Appetitive_vs_aversive <- as.factor(dat2$Appetitive_vs_aversive)

mod_E3 <- rma.mv(yi = lnRR_Ea, V = lnRRV_E, mod = ~Appetitive_vs_aversive-1, random = list(~1|Study_ID, 
                                                                                       # ~ 1|Strain, does not run as we have NA
                                                                                       ~1|ES_ID),
                 test = "t",
                 data = dat2)

summary(mod_E3)
r2_ml(mod_E3) #marginal R2 is low

#social enrichment
dat3<-  dat %>% subset(EE_social < 3) #remove 3 = unclear
dat3$EE_social <- as.factor(dat3$EE_social)

summary(mod_E4)
r2_ml(mod_E4) #social enrichment does slightly better

#exercise enrichment
dat$EE_exercise<-as.factor(dat$EE_exercise)

mod_E5 <- rma.mv(yi = lnRR_Ea, V = lnRRV_E, mod = ~EE_exercise-1, random = list(~1|Study_ID, 
                                                                              # ~ 1|Strain, does not run as we have NA
                                                                              ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_E5) #not really any difference in exercise
r2_ml(mod_E5) #marginal R2 is very low

#age of enrichment
dat4<-  dat %>% subset(Age_EE_exposure < 4) #remove 3 = unclear
dat4$Age_EE_exposure <- as.factor(dat4$Age_EE_exposure)

mod_E6 <- rma.mv(yi = lnRR_Ea, V = lnRRV_E, mod = ~Age_EE_exposure-1, random = list(~1|Study_ID, 
                                                                                # ~ 1|Strain, does not run as we have NA
                                                                                ~1|ES_ID),
                 test = "t",
                 data = dat4)

summary(mod_E6) #adult but not juvenile age of enrichment increases learning and memory
r2_ml(mod_E6) #high R2
count(dat4, Age_EE_exposure) #9 studies of juvenile, 21 on adults

# stress----
mod_S <- rma.mv(yi = lnRR_Sa, V = lnRRV_S, random = list(~1|Study_ID, 
                                                         # ~ 1|Strain, does not run as we have NA
                                                         ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_S0) #learning and memory significantly worse when stressed
funnel(mod_S0)
i2_ml(mod_S0) #high hetero

#moderators

#type of learning
count(dat, Type_learning)
dat$Type_learning<-as.factor(dat$Type_learning)

mod_S1 <- rma.mv(yi = lnRR_Sa, V = lnRRV_S, mod = ~Type_learning-1, random = list(~1|Study_ID, 
                                                                                   # ~ 1|Strain, does not run as we have NA
                                                                                   ~1|ES_ID),
                  test = "t",
                  data = dat)

summary(mod_S1) #Habituation and condition most strongly affected by stress
r2_ml(mod_S1) 

#learning vs memory
dat1<-dat %>% subset(Learning_vs_memory < 3) #remove 3 = unclear

dat1$Learning_vs_memory<-as.factor(dat1$Learning_vs_memory)

mod_S2 <-  rma.mv(yi = lnRR_Sa, V = lnRRV_S, mod = ~Learning_vs_memory-1, random = list(~1|Study_ID, 
                                                                                        # ~ 1|Strain, does not run as we have NA
                                                                                        ~1|ES_ID),
                  test = "t",
                  data = dat1)

summary(mod_S2) #memory but nor learning affected
r2_ml(mod_S2) #marginal R2 is low

#appetitive_vs_aversive
dat2<-  dat %>% subset(Appetitive_vs_aversive < 3) #remove 3 = unclear

dat2$Appetitive_vs_aversive <- as.factor(dat2$Appetitive_vs_aversive)

mod_S3 <- rma.mv(yi = lnRR_Sa, V = lnRRV_S, mod = ~Appetitive_vs_aversive-1, random = list(~1|Study_ID, 
                                                                                           # ~ 1|Strain, does not run as we have NA
                                                                                           ~1|ES_ID),
                 test = "t",
                 data = dat2)

summary(mod_S3)
r2_ml(mod_S3) #marginal R2 is low

#type of stress
count(dat, Type_stress_exposure) #need to remove exposure 3 and 9
dat$Type_stress_exposure <- as.factor(dat$Type_stress_exposure)
dat5 <- filter(dat, Type_stress_exposure %in% c("5", "6", "8","10"))

mod_S4 <- rma.mv(yi = lnRR_Sa, V = lnRRV_S, mod = ~Type_stress_exposure-1, random = list(~1|Study_ID, 
                                                                                           # ~ 1|Strain, does not run as we have NA
                                                                                           ~1|ES_ID),
                 test = "t",
                 data = dat5)
summary(mod_S4) #restraint has highest effect on learning and memory
r2_ml(mod_S4)

#age of stress
count(dat, Age_stress_exposure) #need to sort out all the unclear ones
dat$Age_stress_exposure <-as.factor(dat$Age_stress_exposure)

mod_S5 <-rma.mv(yi = lnRR_Sa, V = lnRRV_S, mod = ~Age_stress_exposure-1, random = list(~1|Study_ID, 
                                                                                        # ~ 1|Strain, does not run as we have NA
                                                                                        ~1|ES_ID),
                test = "t",
                data = dat)
summary(mod_S5) #unclear is significant. Need to sort out all the ages
r2_ml(mod_S5) #marginal R2 is very high

# interaction----
mod_ES0 <- rma.mv(yi = lnRR_ESa, V = lnRRV_ES, random = list(~1|Study_ID, 
                                                         # ~ 1|Strain, does not run as we have NA
                                                         ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_ES0) #significantly positive- seems like SE is better than just E by itself
funnel(mod_ES0)
i2_ml(mod_ES0) #high hetero

#moderators
#TODO should we include multiple moderators (i.e., stress and enrichment) into the interaction model?
dat$Type_learning<-as.factor(dat$Type_learning)

mod_ES1 <- rma.mv(yi = lnRR_ESa, V = lnRRV_E, mod = ~Type_learning-1, random = list(~1|Study_ID, 
                                                                                   # ~ 1|Strain, does not run as we have NA
                                                                                   ~1|ES_ID),
                  test = "t",
                  data = dat)

summary(mod_ES1) #enriched animals do much better at conditioning 
r2_ml(mod_ES1) #conditioning significantly increases, same strength as habituation.

#learning vs memory
dat1<-dat %>% subset(Learning_vs_memory < 3) #remove 3 = unclear

dat1$Learning_vs_memory<-as.factor(dat1$Learning_vs_memory)

mod_ES2 <-  rma.mv(yi = lnRR_ESa, V = lnRRV_E, mod = ~Learning_vs_memory-1, random = list(~1|Study_ID, 
                                                                                        # ~ 1|Strain, does not run as we have NA
                                                                                        ~1|ES_ID),
                  test = "t",
                  data = dat1)

summary(mod_ES2) #learning and memory are important
r2_ml(mod_ES2) #marginal R2 is low

#appetitive_vs_aversive
dat2<-  dat %>% subset(Appetitive_vs_aversive < 3) #remove 3 = unclear

dat2$Appetitive_vs_aversive <- as.factor(dat2$Appetitive_vs_aversive)

mod_ES3 <- rma.mv(yi = lnRR_ESa, V = lnRRV_E, mod = ~Appetitive_vs_aversive-1, random = list(~1|Study_ID, 
                                                                                           # ~ 1|Strain, does not run as we have NA
                                                                                           ~1|ES_ID),
                 test = "t",
                 data = dat2)

summary(mod_ES3) #aversive but not appetitive is significantly increased
r2_ml(mod_ES3) #marginal R2 is low
count(dat2, Appetitive_vs_aversive)

#social enrichment
dat3<-  dat %>% subset(EE_social < 3) #remove 3 = unclear
dat3$EE_social <- as.factor(dat3$EE_social)

mod_ES4 <- rma.mv(yi = lnRR_ESa, V = lnRRV_E, mod = ~EE_social-1, random = list(~1|Study_ID, 
                                                                                # ~ 1|Strain, does not run as we have NA
                                                                                ~1|ES_ID),
                 test = "t",
                 data = dat3)

summary(mod_ES4) #social enrichment
Sr2_ml(mod_ES4) 

#exercise enrichment
dat$EE_exercise<-as.factor(dat$EE_exercise)

mod_ES5 <- rma.mv(yi = lnRR_ESa, V = lnRRV_E, mod = ~EE_exercise-1, random = list(~1|Study_ID, 
                                                                                # ~ 1|Strain, does not run as we have NA
                                                                                ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_ES5) #not really any difference in exercise
r2_ml(mod_ES5) #marginal R2 is very low

#age of enrichment
dat4<-  dat %>% subset(Age_EE_exposure < 4) #remove 3 = unclear
dat4$Age_EE_exposure <- as.factor(dat4$Age_EE_exposure)

mod_ES6 <- rma.mv(yi = lnRR_ESa, V = lnRRV_E, mod = ~Age_EE_exposure-1, random = list(~1|Study_ID, 
                                                                                    # ~ 1|Strain, does not run as we have NA
                                                                                    ~1|ES_ID),
                 test = "t",
                 data = dat4)

summary(mod_ES6) #no significance but adult exposure is stronger effect
r2_ml(mod_ES6) #high R2
count(dat4, Age_EE_exposure) #9 studies of juvenile, 21 on adults

#type of stress
count(dat, Type_stress_exposure) #need to remove exposure 3 and 9
dat$Type_stress_exposure <- as.factor(dat$Type_stress_exposure)
dat5 <- filter(dat, Type_stress_exposure %in% c("5", "6", "8","10"))

mod_ES7 <- rma.mv(yi = lnRR_ESa, V = lnRRV_S, mod = ~Type_stress_exposure-1, random = list(~1|Study_ID, 
                                                                                         # ~ 1|Strain, does not run as we have NA
                                                                                         ~1|ES_ID),
                 test = "t",
                 data = dat5)
summary(mod_ES7) #Maternal separation are the ones that do significantly better with enrichment
r2_ml(mod_ES7)

#age of stress
dat$Age_stress_exposure <-as.factor(dat$Age_stress_exposure)
mod_ES8 <-rma.mv(yi = lnRR_ESa, V = lnRRV_S, mod = ~Age_stress_exposure-1, random = list(~1|Study_ID, 
                                                                                       # ~ 1|Strain, does not run as we have NA
                                                                                       ~1|ES_ID),
                test = "t",
                data = dat)
summary(mod_ES8) #pre natal stress is the highest
r2_ml(mod_ES8) 
# TODO Try SMD