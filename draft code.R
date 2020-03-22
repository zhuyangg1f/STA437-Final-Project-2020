# World Happiness report of 2017

# required package
library(dplyr)
library(ggplot2)
library(car)
library(MVN)
library(RVAideMemoire)
library(MASS)
library(lmtest)

# set directory
setwd("~/Desktop/1EE/STA437 2020winter/Final Project STA437") # should be changed

# import data
happiness_dt <- read.csv('happiness2017.csv')

# delete variable 'country'
# happiness_dt <- happiness_dt %>% select(-c(country))

#' --------------------------------------------------------------------------------------------------------------------------

# Data manipulation and summary

## set seed for reproducibility
set.seed(1234) # should be changed

## random sampling 100 obs from original data
dt <- sample_n(happiness_dt, 100)
# dt <- dt[sample(nrow(happiness_dt),100),]

## Exploratory data analysis (EDA)

par(mfrow=c(1,2))

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

## Some useful code for manipulation
### delete the minimal/maximum (outlier) point
#1. make a copy of data
dt1 <- dt
#2. subsetting
dt1 <- dt1 %>% filter(Corruption != min(Corruption))

### make a transformation
#### log transformation
dt1$Ladder <- log(dt1$Ladder)
#### square root transformation
dt1$Ladder <- sqrt(dt1$Ladder)
#### power transformation 0.25
dt1$Ladder <- (dt1$Ladder^0.25-1)/0.25

#' --------------------------------------------------------------------------------------------------------------------------

# Multiple Linear Model using Original variables

## Check Normality
original_dt <- dt %>% select(-c(country))
linear_reg1 <- lm(Ladder ~ LogGDP + Social + HLE + Freedom + Generosity + Corruption + Positive + Negative + gini, 
                  data = original_dt)
summary(linear_reg1)
qqnorm(linear_reg1$residuals); qqline(linear_reg1$residuals)
shapiro.test(linear_reg1$residuals)

linear_reg2 <- lm(Ladder ~ LogGDP + Social + HLE + Freedom + Positive, 
                  data = original_dt)
summary(linear_reg2)
qqnorm(linear_reg2$residuals); qqline(linear_reg2$residuals)
shapiro.test(linear_reg2$residuals)

lrtest(linear_reg1, linear_reg2)
## !!! Please note that linear regression assumes residual errors \epsilon which represent variation in 
## which is not explained by the predictors that the outcome is normally distributed.

## reference: https://cran.r-project.org/web/packages/MVN/vignettes/MVN.pdf

indep_dt <- original_dt %>% select(-Ladder)
## Check normality by using 'MVN' package
norm_mardia <- mvn(data = indep_dt, mvnTest = "mardia",multivariatePlot = "qq"); norm_mardia
norm_hz <- mvn(data = indep_dt, mvnTest = "hz"); norm_hz

# Check normality by using 'RVAideMemoire' package
mshapiro.test(indep_dt)

# Box-Cox Transformation
# make all values to be positive
trans_dt <- original_dt
trans_dt[,2:10] <- trans_dt[,2:10] + 1
for (i in 2:10) {
  bc <- boxcox(trans_dt[,i] ~ 1)
  lambda <- bc$x[which.max(bc$y)]
  trans_dt[,i] <- (trans_dt[,i]^lambda-1)/lambda
}
mshapiro.test(trans_dt)

### Check Normality again
norm_mardia <- mvn(data = trans_dt, mvnTest = "mardia",multivariatePlot = "qq"); norm_mardia
### Still not normal

# Multiple Linear Model using transformed variables
linear_reg3 <- lm(Ladder ~ LogGDP + Social + HLE + Freedom + Generosity + Corruption + Positive + Negative + gini, 
                  data = trans_dt)
summary(linear_reg3)
qqnorm(linear_reg3$residuals); qqline(linear_reg3$residuals)
shapiro.test(linear_reg3$residuals) # Does not change the conclusion

## Check Multicollinearity
vif(linear_reg1)
cor_tbl <- cor(original_dt)
cor_tbl >= 0.8 ## No Multicollinearity

#' --------------------------------------------------------------------------------------------------------------------------

# Multiple Linear Model using Principal Components
n = 100
X= as.matrix(original_dt[2:10])
x.bar = apply(X,2,mean) # 2 indicates columns
x.bar
S = cov(X)
round(S, 3)
# to get the PCs, first find the eigenvalues and eigenvectors

Val = eigen(S)$values
Vec = eigen(S)$vectors

# Get PCs
W = X  # just to create a data matrix of the same size of X

# now fill in the entries by calculating sample PCs

for(i in 1:9){
  for(j in 1:100){
    W[j,i] = Vec[,i] %*% ( X[j,] -x.bar)  # centered PCs
  }}

colnames(W) = paste("W", 1:9, sep="")

# PCs plot
plot(Val, type="b")  # suggests keeping the first PC only!

# Proportion of variation explained by each PC:

round( Val/sum(Val),3)  # 99.1 % of the sample variation in X is explained by the first PC.

# Now let's run regression with the first PC as the explanatory variable:
##############################################################################

PC.model = lm(original_dt$Ladder ~ W[,1]) # first column = first pc
summary(PC.model)  # W1 is not found significant! adj.R^2 is too low (negative!)

# however, if we add W2 (or even more PCs) we would find them significant:

PC.model.2 = lm(original_dt$Ladder ~  W[,1] + W[,2] + W[,3]+ W[,4] + W[,5] + W[,6] + W[,7]+ W[,8] + W[,9])
summary(PC.model.2)  

lrtest(PC.model,PC.model.2)

# obtain the standardized variables:

Z = X
for(i in 1:9){
  Z[,i] = (X[,i]-x.bar[i])/sqrt(diag(S)[i])
}

# obtain correlation matrix
R = cor(X)
cov(Z)  # they should be the same!

# obtain eigenvalues and eigenvectors of R
Val.new = eigen(R)$values
round(Val.new ,2)

Vec.new = eigen(R)$vectors
rownames(Vec.new) = colnames(X)
colnames(Vec.new) = paste("Z", 1:9, sep="")
round(Vec.new ,2)


# Obtain sample PC values:

W.new = X  # just to create a data matrix of the same size of X
colnames(W.new) = paste("Z", 1:9, sep="")

# now fill in the entries by calculating sample PCs

for(i in 1:9){ # PC's
  for(j in 1:100){
    W.new[j,i] = Vec.new[,i] %*% Z[j,]   # no need to center when using normalized PCCs 
  }}

# How many components should we keep? 

plot(Val.new, type="b", pch=19, xlab="",ylab="Variances")  # suggests keeping the first 4 or 5 PCs.

# Proportion of variation explained by each PC:

round( Val.new/sum(Val.new),3)  


# Regression with all standardized PCs as the explanatory variables
##############################################################################

PC.model.new.1 = lm(original_dt$Ladder ~  W.new[,1] + W.new[,2] + W.new[,3] + W.new[,4] + W.new[,5] +
                      W.new[,6] + W.new[,7] + W.new[,8] + W.new[,9])
summary(PC.model.new.1)  # W2 and W6,7,8,9 seem not to be significant

# Let's remove W3.new and W5.new

PC.model.new.2 = lm(original_dt$Ladder ~  W.new[,1] + W.new[,3] + W.new[,4] + W.new[,5])
summary(PC.model.new.2) 

# the correlations between PCs and variables also indicate importance:

cor.WX = function(mat){
  vals= mat
  rownames(vals) = paste("X", 1:nrow(mat), sep="")
  colnames(vals) = paste("W", 1:nrow(mat), sep="")
  
  for(i in 1: ncol(vals)){
    val= t(eigen(mat)$vectors[,i])* sqrt( eigen(mat)$values[i])
    vals[,i] = val/sqrt(diag(mat))
  }
  return(vals)
  
}

cor.WX(R)
```