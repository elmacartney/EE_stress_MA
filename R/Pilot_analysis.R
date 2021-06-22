#checks for installation and loads packages
pacman::p_load(tidyverse, 
               here)

#loading data----
# TODO - Erin - name all files like as you name functions (good practice)
# TODO - Erin about the directions of effects and expectations
data <- read_csv(here("Data","Pilot_data.csv"))

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
qplot(factor(Study_ID), CC_SD/CC_mean, geom = "boxplot", data = data)
# funnel-like plots
qplot(CC_SD/CC_mean, CC_n, data = data)

qplot(factor(Study_ID), EC_SD/EC_mean, geom = "boxplot", data = data)
qplot(EC_SD/CC_mean, EC_n, data = data)

# TODO Study 16 is a problem for us - look at this study 
qplot(factor(Study_ID), CS_SD/CS_mean, geom = "boxplot", data = data)
qplot(CS_SD/CS_mean, CS_n, data = data)

qplot(factor(Study_ID), ES_SD/ES_mean, geom = "boxplot", data = data)
qplot(ES_SD/ES_mean, ES_n, data = data)

#