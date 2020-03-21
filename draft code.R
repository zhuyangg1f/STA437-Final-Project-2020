# World Happiness report of 2017

# required package
library(dplyr)
library(ggplot2)

# set directory
setwd("~/Desktop/1EE/STA437 2020winter/Final Project STA437") # should be changed

# import data
happiness_dt <- read.csv('happiness2017.csv')

# delete variable 'country'
# happiness_dt <- happiness_dt %>% select(-c(country))

# Data manipulation and summary

## set seed for reproducibility
set.seed(1234) # should be changed

## random sampling 100 obs from original data
dt <- sample_n(happiness_dt, 100)
# dt <- dt[sample(nrow(happiness_dt),100),]

## Exploratory data analysis (EDA)

### distribution of happiness score (dependent variable)
ggplot(dt, aes(x = Ladder)) + geom_histogram() + labs(x = "Happiness Score", y = 'Count', title = 'Histogram: happiness score')

### distribution of log(GDP) (independent variable) with 5 NA's
# check country without GDP
dt$country[is.na(dt$LogGDP)]
# Impute NA's with median (should use sth else) make a resonable guess
dt$LogGDP[is.na(dt$LogGDP)] <- median(dt$LogGDP, na.rm = T)
# quantile(dt$LogGDP, 0.25, na.rm = T)
ggplot(dt, aes(x = LogGDP)) + geom_histogram() + labs(x = "log(GDP)", y = 'Count', title = 'Histogram: Log GDP')

### distribution of social score (independent variable) not normal
ggplot(dt, aes(x = Social)) + geom_histogram() + labs(x = "Social Score", y = 'Count', title = 'Histogram: Social Score')

### distribution of HLE (independent variable) bimodel with 1 NA
# check country without HLE
dt$country[is.na(dt$HLE)]
# Impute NA's with median (should use sth else) make a resonable guess
dt$HLE[is.na(dt$HLE)] <- median(dt$HLE, na.rm = T)
ggplot(dt, aes(x = HLE)) + geom_histogram() + labs(x = "HLE", y = 'Count', title = 'Histogram: HLE')

### distribution of Freedom (independent variable) not normal with 3 NA's
# check country without freedom
dt$country[is.na(dt$Freedom)]
# Impute NA's with median (should use sth else) make a resonable guess
dt$Freedom[is.na(dt$Freedom)] <- median(dt$Freedom, na.rm = T)
ggplot(dt, aes(x = Freedom)) + geom_histogram() + labs(x = "Freedom", y = 'Count', title = 'Histogram: Freedom')
# check possible outlier
dt$country[dt$Freedom == min(dt$Freedom)] # cannot be removed

### distribution of Generosity (independent variable) with 7 NA's
# check country without Generosity
dt$country[is.na(dt$Generosity)]
# Impute NA's with median (should use sth else) make a resonable guess
dt$Generosity[is.na(dt$Generosity)] <- median(dt$Generosity, na.rm = T)
ggplot(dt, aes(x = Generosity)) + geom_histogram() + labs(x = "Generosity", y = 'Count', title = 'Histogram: Generosity')
# check possible outlier
dt$country[dt$Generosity == max(dt$Generosity)] # cannot be removed???Indonesia

### distribution of Corruption (independent variable) left skewed with 9 NA's
# check country without Corruption
dt$country[is.na(dt$Corruption)]
# Impute NA's with median (should use sth else) make a resonable guess
dt$Corruption[is.na(dt$Corruption)] <- median(dt$Corruption, na.rm = T)
ggplot(dt, aes(x = Corruption)) + geom_histogram() + labs(x = "Corruption", y = 'Count', title = 'Histogram: Corruption')
# check possible outlier
dt$country[dt$Corruption == min(dt$Corruption)] # cannot be removed

### distribution of Positive Score (independent variable) bimodel
ggplot(dt, aes(x = Positive)) + geom_histogram() + labs(x = "Positive Score", y = 'Count', title = 'Histogram: Positive Score')

### distribution of Negative Score (independent variable) right skewed
ggplot(dt, aes(x = Negative)) + geom_histogram() + labs(x = "Negative Score", y = 'Count', title = 'Histogram: Negative Score')

### distribution of Gini score of income (independent variable) 2 NA's
# check country without gini
dt$country[is.na(dt$gini)]
# Impute NA's with median (should use sth else) make a resonable guess
dt$gini[is.na(dt$gini)] <- median(dt$gini, na.rm = T)
ggplot(dt, aes(x = gini)) + geom_histogram() + labs(x = "Gini of income", y = 'Count', title = 'Histogram: Gini of income')
