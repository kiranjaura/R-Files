rm(list=ls())
getwd()
setwd('C:/Users/19728/Desktop/EPPS 7313')
library(dplyr)
library(tidyverse)
read.csv('crime2005.csv')
crime <- read.csv('crime2005.csv')
new_crime <- filter(crime,crime$STATE!= 'DC')
hist(new_crime$MU),
     main='2005 Histogram for Murder Rates',
     xlab = 'Murder Rate per 100,000 population',
     border = 'brown',
     col = 'gray',
     xlim = c(0,15),
     las=1,
     breaks=10)
murder_rates <- new_crime$MU
stem(murder_rates)

