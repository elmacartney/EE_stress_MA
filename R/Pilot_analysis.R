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
# TODO - Erin about the directions of effects and expectations
# TODO - shifting up negative values of study 16 results in inf value of effect size because  cant divide my 0
# TODO - funnel plots look very strange with SMD
dat <- read_csv(here("Data","Pilot_data.csv"))
# TODO - what to do about outliers: one very high put low precision effect size

# Load custom function to extract data 
source(here("R/functions.R")) # this has all the function used to calculate effect sizes

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

#flipping SMD
dat$SMD_Ea <- ifelse(dat$Response_direction == 2, dat$SMD_E*-1,ifelse(is.na(dat$Response_direction) == TRUE, NA, dat$SMD_E)) # currently NAswhich causes error
dat$SMD_Sa  <- ifelse(dat$Response_direction == 2, dat$SMD_S*-1,ifelse(is.na(dat$Response_direction) == TRUE, NA, dat$SMD_S)) # currently NAswhich causes error
dat$SMD_ESa <-  ifelse(dat$Response_direction == 2, dat$SMD_ES*-1,ifelse(is.na(dat$Response_direction) == TRUE, NA, dat$SMD_ES)) # currently NAswhich causes error


# modeling with lnRR----

# environment----

mod_E0 <- rma.mv(yi = lnRR_Ea, V = lnRRV_E, random = list(~1|Study_ID, 
                                                        # ~ 1|Strain, does not run as we have NA
                                                         ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_E0) #learning and memory significantly better when enrichment

funnel(mod_E0)

#trying orchard plot

orchard_plot(mod_E0, mod = "Int", xlab = "lnRR", alpha=0.4) +  # Orchard plot 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5)+ # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2)+ # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_colour_manual(values = "darkorange")+ # change colours
  scale_fill_manual(values="darkorange")+ 
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

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

mod_E1 <- rma.mv(yi = lnRR_Ea, V = lnRRV_E, mod = ~Type_learning-1, random = list(~1|Study_ID, 
                                                                                # ~ 1|Strain, does not run as we have NA
                                                                                ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_E1) #enriched animals do much better at conditioning 
r2_ml(mod_E1) 

#orchard plot
orchard_plot(mod_E1, mod = "Type_learning", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

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

orchard_plot(mod_E2, mod = "Type_vs_memory", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

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

# Orchard plot 
orchard_plot(mod_E3, mod = "Appetitive_vs_aversive", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

#social enrichment
dat3<-  dat %>% subset(EE_social < 3) #remove 3 = unclear
dat3$EE_social <- as.factor(dat3$EE_social)

mod_E4<- rma.mv(yi = lnRR_Ea, V = lnRRV_E, mod = ~EE_social-1, random = list(~1|Study_ID, 
                                                                              # ~ 1|Strain, does not run as we have NA
                                                                              ~1|ES_ID),
                 test = "t",
                 data = dat3)

summary(mod_E4)
r2_ml(mod_E4) #social enrichment does slightly better

# Orchard plot 
orchard_plot(mod_E4, mod = "EE_social", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

#exercise enrichment
dat$EE_exercise<-as.factor(dat$EE_exercise)

mod_E5 <- rma.mv(yi = lnRR_Ea, V = lnRRV_E, mod = ~EE_exercise-1, random = list(~1|Study_ID, 
                                                                              # ~ 1|Strain, does not run as we have NA
                                                                              ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_E5) #not really any difference in exercise
r2_ml(mod_E5) #marginal R2 is very low

# Orchard plot 
orchard_plot(mod_E5, mod = "EE_exercise", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

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

# Orchard plot 
orchard_plot(mod_E6, mod = "Age_E_exposure", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

# stress----
mod_S0 <- rma.mv(yi = lnRR_Sa, V = lnRRV_S, random = list(~1|Study_ID, 
                                                         # ~ 1|Strain, does not run as we have NA
                                                         ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_S0) #learning and memory significantly worse when stressed
funnel(mod_S0)
i2_ml(mod_S0) #high hetero

# Orchard plot 
orchard_plot(mod_S0, mod = "Int", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

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

# Orchard plot 
orchard_plot(mod_S1, mod = "Type_learning", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

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

# Orchard plot 
orchard_plot(mod_S2, mod = "Learning_vs_memory", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

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

# Orchard plot 
orchard_plot(mod_S3, mod = "Appetitive_vs_aversive", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

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

# Orchard plot 
orchard_plot(mod_S4, mod = "Type_stress_exposure", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 15), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

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

# Orchard plot 
orchard_plot(mod_S5, mod = "Age_stress_exposure", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

# interaction----
mod_ES0 <- rma.mv(yi = lnRR_ESa, V = lnRRV_ES, random = list(~1|Study_ID, 
                                                             # ~ 1|Strain, does not run as we have NA
                                                             ~1|ES_ID),
                  test = "t",
                  data = dat)
summary(mod_ES0) #significantly positive- seems like SE is better than just E by itself
funnel(mod_ES0)
i2_ml(mod_ES0) #high hetero

# Orchard plot 
orchard_plot(mod_ES0, mod = "Int", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

#modelling with lnVRV----

#stress intercept model
VCV_Sb <- make_VCV_matrix(data = dat, V = "lnVRV_S", cluster = "Study_ID", obs = "ES_ID")

mod_Sb <- rma.mv(yi = lnVR_S, V = VCV_Sb, random = list(~1|Study_ID, 
                                                                                  ~ 1|Strain,
                                                                                  ~1|ES_ID),
                test = "t",
                data = dat)
summary(mod_Sb) #significantly positive- seems like SE is better than just E by itself
funnel(mod_Sb)
i2_ml(mod_Sb) 

#enrichment intercept model
VCV_Eb <- make_VCV_matrix(data = dat, V = "lnVRV_E", cluster = "Study_ID", obs = "ES_ID")

mod_Eb <- rma.mv(yi = lnVR_E, V = VCV_Eb, random = list(~1|Study_ID, 
                                                          ~ 1|Strain,
                                                          ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_Eb) #significantly positive- seems like SE is better than just E by itself
funnel(mod_Eb)
i2_ml(mod_Eb)

#interaction intercept model
VCV_ESb <- make_VCV_matrix(data = dat, V = "lnVRV_ES", cluster = "Study_ID", obs = "ES_ID")

mod_ESb <- rma.mv(yi = lnVR_ES, V = VCV_ESb, random = list(~1|Study_ID, 
                                                          ~ 1|Strain,
                                                          ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_ESb) 
funnel(mod_ESb)
i2_ml(mod_ESb)

#modelling with lnCVR----

#stress intercept model
VCV_Sc <- make_VCV_matrix(data = dat, V = "lnCVRV_S", cluster = "Study_ID", obs = "ES_ID")

mod_Sc <- rma.mv(yi = lnCVR_S, V = VCV_Sc, random = list(~1|Study_ID, 
                                                          ~ 1|Strain,
                                                          ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_Sc) #significantly positive- seems like SE is better than just E by itself
funnel(mod_Sc)
i2_ml(mod_Sc) 

#enrichment intercept model
VCV_Ec <- make_VCV_matrix(data = dat, V = "lnCVRV_E", cluster = "Study_ID", obs = "ES_ID")
mod_Ec <- rma.mv(yi = lnCVR_E, V = VCV_Ec, random = list(~1|Study_ID, 
                                                          ~ 1|Strain,
                                                          ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_Ec) 
funnel(mod_Ec)
i2_ml(mod_Ec)

#interaction intercept model
VCV_ESc <- make_VCV_matrix(data = dat, V = "lnCVRV_ES", cluster = "Study_ID", obs = "ES_ID")
mod_ESc <- rma.mv(yi = lnCVR_ES, V = VCV_ESc, random = list(~1|Study_ID, 
                                                            ~ 1|Strain,
                                                            ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_ESc) 
funnel(mod_ESc)
i2_ml(mod_ESc)

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

# Orchard plot 
orchard_plot(mod_ES1, mod = "Type_learning", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

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

# Orchard plot 
orchard_plot(mod_ES2, mod = "Learning_vs_memory", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

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

# Orchard plot 
orchard_plot(mod_ES3, mod = "Appetitive_vs_aversive", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 


#social enrichment
dat3<-  dat %>% subset(EE_social < 3) #remove 3 = unclear
dat3$EE_social <- as.factor(dat3$EE_social)

mod_ES4 <- rma.mv(yi = lnRR_ESa, V = lnRRV_E, mod = ~EE_social-1, random = list(~1|Study_ID, 
                                                                                # ~ 1|Strain, does not run as we have NA
                                                                                ~1|ES_ID),
                 test = "t",
                 data = dat3)

summary(mod_ES4) #social enrichment
r2_ml(mod_ES4) 

# Orchard plot 
orchard_plot(mod_ES4, mod = "EE_social", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

#exercise enrichment
dat$EE_exercise<-as.factor(dat$EE_exercise)

mod_ES5 <- rma.mv(yi = lnRR_ESa, V = lnRRV_E, mod = ~EE_exercise-1, random = list(~1|Study_ID, 
                                                                                # ~ 1|Strain, does not run as we have NA
                                                                                ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_ES5) #not really any difference in exercise
r2_ml(mod_ES5) #marginal R2 is very low

# Orchard plot 
orchard_plot(mod_ES5, mod = "EE_exercise", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

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

# Orchard plot 
orchard_plot(mod_ES6, mod = "Age_EE_exposure", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

#age stress exposure

count(dat, Age_stress_exposure) #need to sort out all the unclear ones
dat$Age_stress_exposure <-as.factor(dat$Age_stress_exposure)

mod_ES7 <-rma.mv(yi = lnRR_ESa, V = lnRRV_ES, mod = ~Age_stress_exposure-1, random = list(~1|Study_ID, 
                                                                                       # ~ 1|Strain, does not run as we have NA
                                                                                       ~1|ES_ID),
                test = "t",
                data = dat)
summary(mod_ES7) #unclear is significant. Need to sort out all the ages
r2_ml(mod_ES7) #marginal R2 is very high

# Orchard plot 
orchard_plot(mod_ES7, mod = "Age_stress_exposure", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

#type of stress
count(dat, Type_stress_exposure) #need to remove exposure 3 and 9
dat$Type_stress_exposure <- as.factor(dat$Type_stress_exposure)
dat5 <- filter(dat, Type_stress_exposure %in% c("5", "6", "8","10"))

mod_ES8 <- rma.mv(yi = lnRR_ESa, V = lnRRV_S, mod = ~Type_stress_exposure-1, random = list(~1|Study_ID, 
                                                                                         # ~ 1|Strain, does not run as we have NA
                                                                                         ~1|ES_ID),
                 test = "t",
                 data = dat5)
summary(mod_ES8) #Maternal separation are the ones that do significantly better with enrichment
r2_ml(mod_ES8)

# Orchard plot 
orchard_plot(mod_ES8, mod = "Type_stress_exposure", xlab = "lnRR", alpha=0.4) + 
  geom_errorbarh(aes(xmin = lowerPR, xmax = upperPR), height = 0, show.legend = FALSE, size = 1.1, alpha = 0.5) + # prediction intervals
  geom_errorbarh(aes(xmin = lowerCL, xmax = upperCL), height = 0.05, show.legend = FALSE, size = 2) + # confidence intervals
  geom_point(aes(fill = name),  size = 5, shape = 21)+ # mean estimate
  scale_size_continuous(range = c(1, 7))+ # change point scaling
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1.3), # border around the plot
        text = element_text(size = 24), # change font sizes
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13)) 

#modelling with SMD ----

# environment----

mod_E0a <- rma.mv(yi = SMD_Ea, V = SMDV_E, random = list(~1|Study_ID, 
                                                          # ~ 1|Strain, does not run as we have NA
                                                          ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_E0a) #learning and memory significantly better when enrichment

#TODO whats going on with the funnel plot?
funnel(mod_E0a) #this doesn't look right...?

#heterogeneity
i2_ml(mod_E0a) #I2 is extremely high: 99.7

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

mod_E1a <- rma.mv(yi = SMD_Ea, V = SMDV_E, mod = ~Type_learning-1, random = list(~1|Study_ID, 
                                                                                   # ~ 1|Strain, does not run as we have NA
                                                                                   ~1|ES_ID),
                  test = "t",
                  data = dat)

summary(mod_E1a) #enriched animals do much better at conditioning 
r2_ml(mod_E1a) 

#learning vs memory
dat1<-dat %>% subset(Learning_vs_memory < 3) #remove 3 = unclear

dat1$Learning_vs_memory<-as.factor(dat1$Learning_vs_memory)

mod_E2a <-  rma.mv(yi = SMD_Ea, V = SMDV_E, mod = ~Learning_vs_memory-1, random = list(~1|Study_ID, 
                                                                                        # ~ 1|Strain, does not run as we have NA
                                                                                        ~1|ES_ID),
                  test = "t",
                  data = dat1)

summary(mod_E2a)
r2_ml(mod_E2a) #marginal R2 is low

#appetitive_vs_aversive
dat2<-  dat %>% subset(Appetitive_vs_aversive < 3) #remove 3 = unclear

dat2$Appetitive_vs_aversive <- as.factor(dat2$Appetitive_vs_aversive)

mod_E3a <- rma.mv(yi = SMD_Ea, V = SMDV_E, mod = ~Appetitive_vs_aversive-1, random = list(~1|Study_ID, 
                                                                                           # ~ 1|Strain, does not run as we have NA
                                                                                           ~1|ES_ID),
                 test = "t",
                 data = dat2)

summary(mod_E3a) #Enriched animals do much better with aversive conditioning
r2_ml(mod_E3a) #marginal R2 is low

#social enrichment
dat3<-  dat %>% subset(EE_social < 3) #remove 3 = unclear
dat3$EE_social <- as.factor(dat3$EE_social)

mod_E4a<- rma.mv(yi = SMD_Ea, V = SMDV_E, mod = ~EE_social-1, random = list(~1|Study_ID, 
                                                                                # ~ 1|Strain, does not run as we have NA
                                                                                ~1|ES_ID),
                 test = "t",
                 data = dat3)

summary(mod_E4)
r2_ml(mod_E4) #social enrichment does slightly better

#exercise enrichment
dat$EE_exercise<-as.factor(dat$EE_exercise)

mod_E5a <- rma.mv(yi = SMD_Ea, V = SMDV_E, mod = ~EE_exercise-1, random = list(~1|Study_ID, 
                                                                                # ~ 1|Strain, does not run as we have NA
                                                                                ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_E5a) #Stronger difference with exercise
r2_ml(mod_E5a) 

#age of enrichment
dat4<-  dat %>% subset(Age_EE_exposure < 4) #remove 3 = unclear
dat4$Age_EE_exposure <- as.factor(dat4$Age_EE_exposure)

mod_E6a <- rma.mv(yi = SMD_Ea, V = SMDV_E, mod = ~Age_EE_exposure-1, random = list(~1|Study_ID, 
                                                                                    # ~ 1|Strain, does not run as we have NA
                                                                                    ~1|ES_ID),
                 test = "t",
                 data = dat4)

summary(mod_E6a) #much stronger effect of adult enrichment
r2_ml(mod_E6a) #high R2
count(dat4, Age_EE_exposure) #9 studies of juvenile, 21 on adults


# stress----
mod_S0a <- rma.mv(yi = SMD_Sa, V = SMDV_S, random = list(~1|Study_ID, 
                                                         # ~ 1|Strain, does not run as we have NA
                                                         ~1|ES_ID),
                test = "t",
                data = dat)
summary(mod_S0a) #learning and memory significantly worse when stressed
funnel(mod_S0a) #very strange funnel plot with very low ES
i2_ml(mod_S0a) #hetero = 99.8%

#moderators

#type of learning
count(dat, Type_learning)
dat$Type_learning<-as.factor(dat$Type_learning)

mod_S1a <- rma.mv(yi = SMD_Sa, V = SMDV_S, mod = ~Type_learning-1, random = list(~1|Study_ID, 
                                                                                  # ~ 1|Strain, does not run as we have NA
                                                                                  ~1|ES_ID),
                 test = "t",
                 data = dat)

summary(mod_S1a) #Habituation and condition most strongly affected by stress
r2_ml(mod_S1a) 

#learning vs memory
dat1<-dat %>% subset(Learning_vs_memory < 3) #remove 3 = unclear

dat1$Learning_vs_memory<-as.factor(dat1$Learning_vs_memory)

mod_S2a <-  rma.mv(yi = SMD_Sa, V = SMDV_S, mod = ~Learning_vs_memory-1, random = list(~1|Study_ID, 
                                                                                        # ~ 1|Strain, does not run as we have NA
                                                                                        ~1|ES_ID),
                  test = "t",
                  data = dat1)

summary(mod_S2a) #memory but not learning affected
r2_ml(mod_S2a) #marginal R2 is low

#appetitive_vs_aversive
dat2<-  dat %>% subset(Appetitive_vs_aversive < 3) #remove 3 = unclear

dat2$Appetitive_vs_aversive <- as.factor(dat2$Appetitive_vs_aversive)

mod_S3a <- rma.mv(yi = SMD_Sa, V = SMDV_S, mod = ~Appetitive_vs_aversive-1, random = list(~1|Study_ID, 
                                                                                           # ~ 1|Strain, does not run as we have NA
                                                                                           ~1|ES_ID),
                 test = "t",
                 data = dat2)

summary(mod_S3a)
r2_ml(mod_S3) #marginal R2 is low

#type of stress
count(dat, Type_stress_exposure) #need to remove exposure 3 and 9
dat$Type_stress_exposure <- as.factor(dat$Type_stress_exposure)
dat5 <- filter(dat, Type_stress_exposure %in% c("5", "6", "8","10"))

mod_S4a <- rma.mv(yi = SMD_Sa, V = SMDV_S, mod = ~Type_stress_exposure-1, random = list(~1|Study_ID, 
                                                                                         # ~ 1|Strain, does not run as we have NA
                                                                                         ~1|ES_ID),
                 test = "t",
                 data = dat5)
summary(mod_S4a) #restraint has highest effect on learning and memory
r2_ml(mod_S4a)

#age of stress
count(dat, Age_stress_exposure) #need to sort out all the unclear ones
dat$Age_stress_exposure <-as.factor(dat$Age_stress_exposure)

mod_S5a <-rma.mv(yi = SMD_Sa, V = SMDV_S, mod = ~Age_stress_exposure-1, random = list(~1|Study_ID, 
                                                                                       # ~ 1|Strain, does not run as we have NA
                                                                                       ~1|ES_ID),
                test = "t",
                data = dat)
summary(mod_S5a) #adult stress has a much greater effect. Followed by prenatal (although non-significant)
r2_ml(mod_S5a) #marginal R2 is very high

#note that the importance of adult could be that the time to the assay is less (most assay were done as adults) - not enough data to test age at the time of the assay

# interaction----
mod_ES0a <- rma.mv(yi = SMD_ESa, V = SMDV_ES, random = list(~1|Study_ID, 
                                                             # ~ 1|Strain, does not run as we have NA
                                                             ~1|ES_ID),
                  test = "t",
                  data = dat)
summary(mod_ES0a) #significantly positive- seems like SE is better than just E by itself
funnel(mod_ES0a) #funnel plot doesn't look right
i2_ml(mod_ES0a) #high hetero

#moderators
#TODO should we include multiple moderators (i.e., stress and enrichment) into the interaction model?
dat$Type_learning<-as.factor(dat$Type_learning)

mod_ES1a <- rma.mv(yi = SMD_ESa, V = SMDV_E, mod = ~Type_learning-1, random = list(~1|Study_ID, 
                                                                                    # ~ 1|Strain, does not run as we have NA
                                                                                    ~1|ES_ID),
                  test = "t",
                  data = dat)

summary(mod_ES1a) #enriched animals do much better at conditioning and habituation 
r2_ml(mod_ES1a) #conditioning significantly increases, same strength as habituation.

#learning vs memory
dat1<-dat %>% subset(Learning_vs_memory < 3) #remove 3 = unclear

dat1$Learning_vs_memory<-as.factor(dat1$Learning_vs_memory)

mod_ES2a <-  rma.mv(yi = SMD_ESa, V = SMDV_E, mod = ~Learning_vs_memory-1, random = list(~1|Study_ID, 
                                                                                          # ~ 1|Strain, does not run as we have NA
                                                                                          ~1|ES_ID),
                   test = "t",
                   data = dat1)

summary(mod_ES2a) #learning and memory are important but leanring is higher
r2_ml(mod_ES2a) #marginal R2 is low

#appetitive_vs_aversive
dat2<-  dat %>% subset(Appetitive_vs_aversive < 3) #remove 3 = unclear

dat2$Appetitive_vs_aversive <- as.factor(dat2$Appetitive_vs_aversive)

mod_ES3a <- rma.mv(yi = SMD_ESa, V = SMDV_E, mod = ~Appetitive_vs_aversive-1, random = list(~1|Study_ID, 
                                                                                             # ~ 1|Strain, does not run as we have NA
                                                                                             ~1|ES_ID),
                  test = "t",
                  data = dat2)

summary(mod_ES3a) #aversive but not appetitive is significantly increased
r2_ml(mod_ES3a) #marginal R2 is low
count(dat2, Appetitive_vs_aversive)

#social enrichment
dat3<-  dat %>% subset(EE_social < 3) #remove 3 = unclear
dat3$EE_social <- as.factor(dat3$EE_social)

mod_ES4a <- rma.mv(yi = SMD_ESa, V = SMDV_E, mod = ~EE_social-1, random = list(~1|Study_ID, 
                                                                                # ~ 1|Strain, does not run as we have NA
                                                                                ~1|ES_ID),
                  test = "t",
                  data = dat3)

summary(mod_ES4a) #social enrichment does substantially more better when improving learning and memory
r2_ml(mod_ES4a) 

#exercise enrichment
dat$EE_exercise<-as.factor(dat$EE_exercise)

mod_ES5a <- rma.mv(yi = SMD_ESa, V = SMDV_E, mod = ~EE_exercise-1, random = list(~1|Study_ID, 
                                                                                  # ~ 1|Strain, does not run as we have NA
                                                                                  ~1|ES_ID),
                  test = "t",
                  data = dat)

summary(mod_ES5a) #not really any difference in exercise
r2_ml(mod_ES5a) #marginal R2 is very low

#age of enrichment
dat4<-  dat %>% subset(Age_EE_exposure < 4) #remove 3 = unclear
dat4$Age_EE_exposure <- as.factor(dat4$Age_EE_exposure)

mod_ES6a <- rma.mv(yi = SMD_ESa, V = SMDV_E, mod = ~Age_EE_exposure-1, random = list(~1|Study_ID, 
                                                                                      # ~ 1|Strain, does not run as we have NA
                                                                                      ~1|ES_ID),
                  test = "t",
                  data = dat4)

summary(mod_ES6a) #no significance but adult exposure is stronger effect
r2_ml(mod_ES6a) #high R2
count(dat4, Age_EE_exposure) #9 studies of juvenile, 21 on adults

#type of stress
count(dat, Type_stress_exposure) #need to remove exposure 3 and 9
dat$Type_stress_exposure <- as.factor(dat$Type_stress_exposure)
dat5 <- filter(dat, Type_stress_exposure %in% c("5", "6", "8","10"))

mod_ES7a <- rma.mv(yi = SMD_ESa, V = SMDV_S, mod = ~Type_stress_exposure-1, random = list(~1|Study_ID, 
                                                                                           # ~ 1|Strain, does not run as we have NA
                                                                                           ~1|ES_ID),
                  test = "t",
                  data = dat5)
summary(mod_ES7a) #restrain and maternal separation are increased the most
r2_ml(mod_ES7a)

#age of stress
dat$Age_stress_exposure <-as.factor(dat$Age_stress_exposure)
mod_ES8a <-rma.mv(yi = SMD_ESa, V = SMDV_S, mod = ~Age_stress_exposure-1, random = list(~1|Study_ID, 
                                                                                         # ~ 1|Strain, does not run as we have NA
                                                                                         ~1|ES_ID),
                 test = "t",
                 data = dat)
summary(mod_ES8a) #pre natal and adult stress are recovered the most
r2_ml(mod_ES8a) #moderate R2
