
#------------------------------Preparing the environment for MLRM---------------------------------------#

list.of.packages <- c("boot", "car","QuantPsyc","lmtest","sandwich","vars","nortest","MASS","caTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)

1#-----------------------------Setting Working Directory--------------------------------#
path1<-setwd("C:/Users/user/Downloads/Fn-UseC_-Marketing-Campaign-Eff-UseC_-FastF.csv")
getwd()

path1<-setwd("C:/Users/user/Downloads/Fn-UseC_-Marketing-Campaign-Eff-UseC_-FastF.csv")
getwd()

data=read.csv("Fn-UseC_-Marketing-Campaign-Eff-UseC_-FastF.csv")
data1=data
2#------------------------------Exploring Data------------------------------------------#
str(data1)
dim(data1)
summary(data1)


3#-----------------------------------Change the Dependent Variable Name------------------#
colnames(data1)[which(names(data)=="SalesInThousands")]="Sales"
data1


4#-----------------------------------Removing Outliers by Quantile Method-----------------#
quantile(data1$Sales,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
boxplot(data1$Sales,horizontal = T)

data2=data1[data1$Sales<63.61600,]
nrow(data1)
nrow(data2)
nrow(data1)-nrow(data2)

quantile(data2$Sales,c(0,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,0.99,0.995,1))
boxplot(data2$Sales,horizontal = T)


data3=data2[data2$Sales>20,]
quantile(data3$Sales,c(0,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.55,0.60,0.65,0.70,0.75,0.80,0.85,0.90,0.95,0.99,0.995,1))
boxplot(data3$Sales,horizontal = T)

5 #---------------------------------------Checking For Missing Value---------------------------------------#

as.data.frame(colSums(is.na(data3)))

#There are No missing Values in the dataset#.

5#--------------------------Splitting the data into Original and test data set------------------------#
set.seed(11234)

spl = sample.split(data3$Sales, 0.7)

original.data = subset(data3, spl == TRUE)
str(original.data)
dim(original.data)

test.data = subset(data3, spl == FALSE)
str(test.data)
dim(test.data)
6#-------------------------------Fitting the Model---------------------------------------------#
fit=lm(Sales~.,data=original.data)
summary(fit)

#Iteration 1
fit2=lm(Sales~MarketID+I(MarketSize=="Medium")+I(MarketSize=="Small")+LocationID+AgeOfStore+Promotion,data=original.data)
summary(fit2)


#Iteration 2
fit3=lm(Sales~MarketID+I(MarketSize=="Small")+LocationID+AgeOfStore+Promotion,data=original.data)
summary(fit3)

#Final Model
fit4=lm(Sales~I(MarketSize=="Small")+LocationID+AgeOfStore+Promotion,data=original.data)
summary(fit4)
?dwt
plot(fit4)
Anova(fit4)
7#Checking Multicollinearity in the model

## Get the predicted or fitted values
vif(fit4)

#Since Vif<2,so there is no multicollinearity#

## Get the predicted or fitted values
fitted(fit4)

par(mfrow=c(2,2))
plot(fit4)

8## MAPE
original.data$pred <- fitted(fit4)
write.csv(original.data,"mape1.csv")

#Calculating MAPE
attach(original.data)
(sum((abs(Sales-pred))/Sales))/nrow(original.data)

##################################### Checking of Assumption ############################################

9#Test for Autocorelation
durbinWatsonTest(fit4)
dwt(fit4)


10#Test for Heteroscedacity

# Breusch-Pagan test
bptest(fit4)

#Cook-Weisberg test
ncvTest(lm(Sales~I(MarketSize=="Small")+LocationID+AgeOfStore+Promotion,data=original.data))


#-------------since p value is more than 0.05 so there is presence of Heteroscedacity-------------#
11## Normality testing Null hypothesis is data is normal.

resids <- fit4$residuals


ad.test(resids) #get Anderson-Darling test for normality 
cvm.test(resids) #get Cramer-von Mises test for normaility 
lillie.test(resids) #get Lilliefors (Kolmogorov-Smirnov) test for normality 
pearson.test(resids) #get Pearson chi-square test for normaility 
sf.test(resids) #get Shapiro-Francia test for normaility 

qqnorm(resids)

#There is presence of Normality
#---------------------------------Testing of Data--------------------------------------#
#Iteration 1
fit5=lm(Sales~I(MarketSize=="Small")+LocationID+AgeOfStore+Promotion,data=test.data)
summary(fit5)

#Iteration 2
fit6=lm(Sales~I(MarketSize=="Small")+LocationID+AgeOfStore,data=test.data)
summary(fit6)

#Final Model
fit7=lm(Sales~I(MarketSize=="Small")+LocationID,data=test.data)
summary(fit7)

vif(fit7)

fitted(fit7)

par(mfrow=c(2,2))
plot(fit7)
8## MAPE
original.data$pred <- fitted(fit7)

test.data$pred <- fitted(fit7)

attach(test.data)
(sum((abs(Sales-pred))/Sales))/nrow(test.data)

##################################### Checking of Assumption ############################################

9#Test for Autocorelation

durbinWatsonTest(fit7)
dwt(fit7)

10# Checking multicollinearity
vif(fit7)

#Since vif<2,there is no multocllinearity

11#Test for Heteroscedasticity


# Breusch-Pagan test
bptest(fit7)  

#Cook-Weisberg test
ncvTest(lm(Sales~LocationID+I(MarketSize=="Small"), data=test.data))

#Since p-value is more than 0.05,so there is presence of Heteroscedacity

12## Normality testing Null hypothesis is data is normal.

resids1 <- fit7$residuals


ad.test(resids1) #get Anderson-Darling test for normality 
cvm.test(resids1) #get Cramer-von Mises test for normaility 
lillie.test(resids1) #get Lilliefors (Kolmogorov-Smirnov) test for normality 
pearson.test(resids1) #get Pearson chi-square test for normaility 
sf.test(resids1) #get Shapiro-Francia test for normaility 

qqnorm(resids1)

#Since p-value>0.05 and errors are normally distributed so there is presence of Normality

#---------------------------------------------End-------------------------------------------#