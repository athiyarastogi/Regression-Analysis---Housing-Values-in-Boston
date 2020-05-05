
library (tidyverse)  
library (ggplot2)  
library(corrplot)
library(MASS)
library(dplyr)
library(broom)
library (car)

housing_data = read.csv("housing.proper.csv") #loading the dataset
housing_data_df= data.frame(housing_data)
colnames(housing_data_df) <- c("X1","X2","X3","X4","X5","X6","X7","X8",
                            "X9","X10","X11","X12","X13", "Y") #transforming column names
lnY = log(housing_data_df$Y)


# Exploratory Data Analysis    

set.seed(1234) #set seed to ensure same random numbers are generated
require(caTools)
sample = sample.split(housing_data_df, SplitRatio = 0.7)
#Splits the dataset "housing_data" in the ratio mentioned. Marks rows as logical true and remaining as logical false.  
training_housing_data = subset(housing_data_df, sample == TRUE)  #creates training dataset with rows marked true
lny = log(training_housing_data$Y)
testing_housing_data = subset(housing_data_df, sample ==FALSE)
lny_testing = log(testing_housing_data$Y)
summary (housing_data_df) # to get statistic of each variable to detect outliers  

housing_data.cor = round(cor(housing_data_df), 3) #correlation between features and median housing values

heatmap(housing_data.cor, Rowv = NA, Colv = NA)

corrplot(cor(housing_data_df), method = "number", type = "upper", diag = FALSE)

housing_data.box = boxplot(housing_data_df, xlab= "Features", ylab ="Frequency") #boxplot to detect outliers 

full_housing_model =lm(Y~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 ,data=housing_data_df)
log_full_housing_model =lm(lnY~X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 + X11 + X12 + X13 ,data=housing_data_df)


summary(full_housing_model)
tidy(full_housing_model) %>% arrange(p.value)
plot(log_full_housing_model)
pairs(housing_data_df)


# Model Selection 
log_full_housing_model =lm(lny~X1 + X2 + X5 + X6 + X8 + X10 + X11 + X13 ,data=training_housing_data)
s <- summary(log_full_housing_model)$sigma
# Selection criteria for the full model
SSres <- sum(log_full_housing_model$residuals^2)
Rsq <- summary(log_full_housing_model$r.squared)
Rsq_adj <- summary(log_full_housing_model)$adj.r.squared
p_prime <- length(log_full_housing_model$coefficients)
n <- nrow(training_housing_data)
C <- SSres/s^2 + 2*p_prime - n
AIC <- n*log(SSres) - n*log(n) + 2*p_prime


# Function that returns the selection criteria
select_criteria = function(model, n, s)
{
  SSres <- sum(model$residuals^2)
  Rsq <- summary(model)$r.squared
  Rsq_adj <- summary(model)$adj.r.squared
  p_prime <- length(model$coefficients)
  C <- SSres/s^2 + 2*p_prime - n
  AIC <- n*log(SSres) - n*log(n) + 2*p_prime
  res <- c(SSres, Rsq, Rsq_adj, C, AIC)
  names(res) <- c("SSres", "Rsq", "Rsq_adj", "C", "AIC")
  return(res)
}

rbind(
  select_criteria(lm(lny~X1+X2, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X6, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X3+X6+X8, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X5+X6+X8, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X3+X6+X9, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X5+X6+X8+X10, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X5+X6+X8+X10+X11, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X5+X6+X8+X10+X11+X13, data = training_housing_data), n, s),
  select_criteria(lm(lny~X1+X2+X3+X5+X6+X8+X10+X11+X13, data = training_housing_data), n, s))


#Validation
sum((lny) - mean(lny)^2)
fit_validation <- lm(lny_testing~X1+X2+X5+X6+X8+X10+X11+X13, data = testing_housing_data) 
Y_pred <- predict(fit_validation)
Y_obs <- (testing_housing_data$Y)
n_star <- nrow(testing_housing_data)
MSPE <- sum( (Y_obs-Y_pred)^2/n_star ) #Mean Squared Prediction Error should be small
print(MSPE)
MS_res <- (summary(fit_validation)$sigma)^2
print(MS_res)

sum((training_housing_data$lny - predict(fit_validation))^2)


# Diagnostics


fit1 <- lm(Y ~ X1+X2+X5+X6+X8+X10+X11+X13, data = housing_data_df)
# Outlying Y observations

# Statistical test
outlierTest(fit1)
# Studentized deleted residuals
t <- rstudent(fit1)
alpha <- 0.05
n <- length(lnY)
p_prime = length(coef(fit1)) 
t_crit <- qt(1-alpha/(2*n),n-p_prime-1)
round(t,2)
t_crit
which(abs(t) > t_crit)

# Outlying X observations 
Pii <- hatvalues(fit1)
round(Pii, 2)
which(Pii > 2*p_prime/n)
which(Pii > 0.5)


# Influence 
influencePlot(fit1,	id.method="identify", 
              main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )
DFFITS <- dffits(fit1)
which(DFFITS > 1)
D <- cooks.distance(fit1)
which(D > qf(0.2, p_prime, n-p_prime))
DFBETAS <- dfbetas(fit1)
head(DFBETAS)
which(DFBETAS > 1)


# Matrix with Y, Yhat, P, e, t
s <- summary(fit1)$sigma
Y. <- lnY
Y.hat <- fitted(fit1)
P <- hatvalues(fit1)
e <- residuals(fit1)
t <- rstudent(fit1) 
DFFITS <- dffits(fit1)
D <- cooks.distance(fit1)
res <- cbind(Y., Y.hat, P, e, t, DFFITS, D)
colnames(res) <- c("Y.", "Y.hat", "P", "e", "t","DFFITS", "D")
res



  







  
 
  
