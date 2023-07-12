# Linear Regression for Prediction
# Data Science for Business

# install packages
install.packages("tidyverse")
install.packages("lm.beta") #standardize regression coefficients
install.packages("car") #calculate VIF
install.packages("olsrr") #best subsets procedure

#load libraries
library(tidyverse)
library(lm.beta)
library(car)
library(olsrr)

#set working directory
setwd("C:/Users/kspen/OneDrive/Documents")

#read dataset into R and view
creditdf <- read.csv("Credit.csv")
view(creditdf)

#recode categorical variables to 0,1
#student, gender, married already coded as 0 and 1

#convert categorical variables to factors with levels and labels
creditdf$Student <- factor(creditdf$Student, levels = c(0,1), labels = c("No", "Yes"))
creditdf$Gender <- factor(creditdf$Gender, levels = c(0,1), labels = c("Male", "Female"))
creditdf$Married <- factor(creditdf$Married, levels = c(0,1), labels = c("No", "Yes"))

#check for missing data
sum(is.na(creditdf))

#generate summary statitistics for all variables in df
summary(creditdf)

#set seed so the random sample is reproducible
set.seed(42)

#partition the data into a training set and a validation set
sample <- sample(c(TRUE, FALSE), nrow(creditdf), replace= TRUE, prob = c(0.7, 0.3))

#create a training dataframe
traincredit <- creditdf[sample,]

#create a validation dataframe
validatecredit <- creditdf[!sample,]

#create a correlation matrix with all quantitative variables in df
cor(traincredit[c(1, 2, 3, 4, 5, 9)])

#turn off scientific notation
options(scipen = 999)

#perform mr on all variables in trainset
traincredit_allMR <- lm(Balance ~ Income + Limit + Rating + Age + Education +
                          Student + Gender + Married, data = traincredit)
summary(traincredit_allMR)

#extract standardized regression coefficients from all variables
lm.beta(traincredit_allMR)

#STEPS TO CREATE A NEW SCATTERPLOT OF RESIDUALS VS PREDICTED VALUES WITH ALL VARIABLES

#1. create a vector of predicted values generated from the multiple regression above
balance_pred = predict(traincredit_allMR)

#2. Create a vector of residuals generated from the multiple regression above
balance_res = resid(traincredit_allMR)

#3. Create a data frame of the predicted values and residuals
balance_pred_res <- data.frame(balance_pred, balance_res)

#4. Create a scatterplot of the residuals vs the predicted values
ggplot(data = balance_pred_res, mapping = aes(x = balance_pred, y = balance_res)) +
  geom_point() +
  labs(title = "Plot of residuals vs predicted values", x = "Predicted values",
       y = "Residuals")

#STEPS TO CREATE A NORMAL PROBABIBLITY PLOT

#1. Create a vector of standardized residuals generated from the mult regression above 
credit_std.res = rstandard(traincredit_allMR)

#2. Produce normal scores for the standardized residuals and create qq plot
qqnorm(credit_std.res, ylab = "Standardized residuals", xlab = "Normal scores")

#3. Create normal probability histogram
ggplot(data = balance_pred_res, aes(x = balance_res)) +
  geom_histogram()


#calculate variance inflation factor for all variables to assess multicollinearity
vif(traincredit_allMR)

#perform multiple regression with Income, Limit, Age, Education, Student
traincredit_adjMR <- lm(Balance ~ Income + Limit + Age + Education + Student,
                        data = traincredit)
summary(traincredit_adjMR)


#run a multiple regression using the training df and all independent variables

#run best subsets procedure with multiple regression output from training df
bestsubset <- ols_step_all_possible(traincredit_allMR)
view(bestsubset)


#run a final multiple regression using the validation df and bestsubset results
validatecredit_bestMR <- lm(Balance ~ Income + Limit + Age + Education + Student,
                            data = validatecredit)
summary(validatecredit_bestMR)

#run a final multiple regression using the validation df and adjMR
validatecredit_adjMR <- lm(Balance ~ Income + Limit + Rating + Age + Education +Student,
                           data = validatecredit)
summary(validatecredit_adjMR)

#run a final multiple regression using only variables that will be in new customer data
validatecredit_4MR <- lm(Balance ~ Income + Rating + Age + Student, data = validatecredit)
summary(validatecredit_4MR)

#read new data into R and view
credit_card_prediction <- read.csv("credit_card_prediction.csv")
view(credit_card_prediction)

#convert categorical variables to factors with levels and labels
credit_card_prediction$Student <- factor(credit_card_prediction$Student,
                                         levels = c(0,1), labels = c("No", "Yes"))

#estimate predicted y values and prediction intervals for new data
predict(validatecredit_4MR, credit_card_prediction, 
        interval = "prediction", level = 0.95)

###############################################################################

# FOR SIMPLE LINEAR REGRESSION

#create a scatterplot showing the relationship between an x and y, add regression line

#calculate a correlation coefficient for the relationship between x and y

#perform a simple linear regression with x and y

#view the simple linear regression output

###################################################################################



