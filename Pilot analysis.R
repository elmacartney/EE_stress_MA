library(dplyr)

#loading data----
data <- read.csv("Pilot data.csv")

data1 <- data %>% 
  rename(
    Study_ID = ï..Study_ID
  )
#checking for outliers/potential typos---
#note that it very hard to check for typos/incorrect entries when there is a range of traits etc that are not directly comparable. Will need to re-do when effect sizes are calculated but all data looks correct at this stage

boxplot(CC_mean ~ Study_ID, data = data1)
boxplot(CC_SD ~ Study_ID, data = data1)
boxplot(CC_n ~ Study_ID, data = data1)

boxplot(EC_mean ~ Study_ID, data = data1) 
boxplot(EC_SD ~ Study_ID, data = data1)
boxplot(EC_n ~ Study_ID, data = data1)

boxplot(CS_mean ~ Study_ID, data = data1) 
boxplot(CS_SD ~ Study_ID, data = data1)
boxplot(CS_n ~ Study_ID, data = data1)

boxplot(ES_mean ~ Study_ID, data = data1)  
boxplot(ES_SD ~ Study_ID, data = data1)
boxplot(ES_n ~ Study_ID, data = data1)

