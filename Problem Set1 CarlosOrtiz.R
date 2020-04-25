## Problem Set 1
##Student 91351021 0

#Conceptual Questions
#1. For each of the following questions state:
##(1)Whether it is a regression question or a classification question
##(2)Whether we are interested in inference or prediction
####(a)1.Classification 2.Inference
####(b)1.Regression 2.Prediction
####(c)1.Classification 2.Prediction

#2. True or false?
##(a) False
##(b) True
##(c) False
##(d) False

#Data questions

install.packages("MASS")
library (MASS)
data("Boston")
summary(Boston)
attach(Boston)
dim(Boston)

#Code gives me a dimension of 506 rows times 14 columns

print(tail(Boston))

#The data set has 506 rows or 506 observations for each of the 14 variables
#1)This solves question 1, the data set has 506 observations

colnames(Boston)

#[1] "crim"    "zn"      "indus"   "chas"    "nox"     "rm"      "age"    
#[8] "dis"     "rad"     "tax"     "ptratio" "black"   "lstat"   "medv"   

#There are 14 variables in the dataset as shown in the number of colnames displayed.
#2)This solves question 2, there are 14 variables in the data set

sum(is.na(Boston))

#No observations with a value of NA (Not available)
#3)This solves question 3, there are no NA values in the data set

#observing the predictors in the Boston data set confirms there are categorical
#columns in this data set (e.g. rad & chas)
#4)This solves question 4, yes there are categorical columns in the data set

mean(crim)

#Mean suburb level crime rate per capita is 3.613524
#5)This solves question 5, mean suburb level crime rate per capita is 3.613524

?sd

sd(crim)

#Standard deviation for crime rate per capita is 8.601545
#6)This solves question 6, standard deviation for crime rate per capita is 8.601545

dim(Boston)

#create a test data matrix called test_data 
#including the last 100 rows of the Boston dataset

test_data <- Boston[407:506,]

#create a training data matrix called training_data
#including the all but the last 100 rows of the Boston dataset

training_data <- Boston[1:406,]

#Build my text vector that will be used as formula for linear regression 
#through lm() function

formulac <- paste(colnames(Boston[,-14]), collapse = " + ")
lmformulac <- paste("medv", formulac, sep = " ~ ")
 
print (lmformulac)

#use my text vector lmformulac as formula in a linear regression on training data

lregmode1 <- lm(as.formula(lmformulac), training_data)

summary (lregmode1)

#Install Metrics package to obtain an MSE function (that I ended up not using)

install.packages("Metrics")

library(Metrics)

#now let's look at the MSE of out prediction

pred <- predict(lregmode1,test_data[,-14])

print (pred)

mserr <- mean((pred-test_data[,14])^2)

print(mserr)

##7)This solves question No. 7, my mean square error is 33.54828 

plot(pred,test_data[,14])

cor(pred,test_data[,14])

#MSE for my linear function is 15.01016

LinMod2 <- lm(medv~crim+chas+tax, training_data)

summary(LinMod2)
summary(lregmode1)

#looking at the summaries I see that for my first regression, 
#chas had a value of 1.891684 and on the second regression (less predictors)
#chas went up to 5.35024 or a differential of 3.4585
#Therefore for question 8 I obtained the following:
#8.1) chas
#8.2) 1.891684
#8.3) 5.35024
#8.4) We are ussing less predictors and removing some predictors that had high influence
#in the first regression (e.g. dis, ptratio & lstat) this has the effect of increasing
#the influence that chas has as a predictor over the new regression model.

#install library FNN to use the function knn.reg

install.packages("FNN")

library(FNN)

#run knnn regression for training data column 14 (medv) with k value of 2

knnreg2 <- knn.reg(training_data[,-14],test_data[-14],training_data[,14],k=2)

#evaluate and print MSE of knnreg2 

mseknn2 <- mean((knnreg2$pred - test_data[,14])^2)

print(mseknn2)

#9)Mean square error for KNN regression with k=2 for training data column 14(medv)
#has a value of 29.14075

#run knnn regression for training data column 14 (medv) with k value of 10

knnreg10 <- knn.reg(training_data[,-14],test_data[-14],training_data[,14],k=10)

#evaluate and print MSE of knnreg10 

mseknn10 <- mean((knnreg10$pred - test_data[,14])^2)

print(mseknn10)

#10)Mean square error for KNN regression with k=10 for training data column 14(medv)
#has a value of 28.48396


