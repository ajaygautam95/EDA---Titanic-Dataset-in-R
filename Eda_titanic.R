#Exploratory Data Analysis on Titanic Data set from Kaggle

#Checking and setting the working directory
getwd()
setwd()

#Installing the required packages
#Importing the required libraries
library(mice)
library(dplyr)
library(caret)
#library(ROCR)
#library(Metrics)
#library(e1071)
#library(party)
#library(randomForest)
#library(grid)
#library(broom)
#library(scales)
#library(tidyr)
#library(data.table)
library(ggplot2)
library(ggthemr)
library(ggthemes)
library(reshape2)
library(corrplot)
#library(gridExtra)


#Importing the dataset
train = read.csv("train.csv", header=T, na.strings="")
View(train)
head(train)
tail(train)

#Viweing the summary and the structure of Dataset
dim(train)
str(train)
summary(train)

#Survived - is the dependant categorical variable
#PClass - is also a categorical variable
#Converting resp variables to catergorical from numeric
train$Pclass = factor(train$Pclass, levels = c(1,2,3), labels = c("1st", "2nd","3rd"))
train$Survived = factor(train$Survived, levels=c(0,1), labels = c("Not Survived", "Survived"))

#Checking how many passengers survived and not survived
table(train$Survived)
table(train$Pclass)
table(train$Sex)


#Histogram
barplot(table(train$Pclass), xlab="Passenger Class", ylab="Frequency", main="Histogram of Passenger Class", col = "lightblue")
barplot(table(train$Sex), xlab="Sex", ylab="Frequency", main="Histogram of Sex", col = "red")
barplot(table(train$Embarked), xlab="Port of Embarkment", ylab="Frequency", main="Histogram of Port of Embarkment", col = "green")

#Geenral Plot 
#ggplot(train, aes(x = Parch, y = SibSp)) + geom_point(aes(color = Survived))
#ggplot(train, aes(x =Fare, y = Age)) + geom_point(aes(color = Survived))


#Corrlation Matrix
cor_vars = train[,c("Age", "SibSp", "Fare", "Parch")]
cor(cor_vars)
corrplot(cor(cor_vars))

#Interaction plot
interaction.plot(train$Pclass, train$Sex, train$Survived, fun=mean, legend = TRUE, xlab="Passenger Class", ylab="Mean number of Survivors", main="Interaction Effect between Passenger Class and Sex")
interaction.plot(train$Pclass, train$Embarked, train$Survived, fun=mean, legend = TRUE, xlab="Passenger Class", ylab="Mean number of Survivors", main="Interaction Effect between Passenger Class and Embarkment")
#No of Survivors according to Sex
str(train)
sur_sex = c(0,0)
train$Sex = as.numeric(train$Sex)
train$Survived = as.numeric(train$Survived)
sur_sex[1] =filter(train, Survived ==1 & Sex =="1") %>% nrow  #Sex and Survived should be numeric
sur_sex[2] =filter(train, Survived ==1 & Sex =="2") %>% nrow

plot(sur_sex,type = "o", xlab = "Sex", ylab="No. of Survivors" , xaxt="n", main = "Effect of Sex Variable")
axis(1, at=c(1,2), labels=c("Female", "Male"))

#No.of Survivors according to Pclass
sur_cls = c(0,0,0)
sur_cls[1] =filter(train, Survived ==1 & Pclass ==1) %>% nrow  #Sex and Survived should be numeric
sur_cls[2] =filter(train, Survived ==1 & Pclass ==2) %>% nrow
sur_cls[3] =filter(train, Survived ==1 & Pclass ==3) %>% nrow

plot(sur_cls,type = "o", xlab = "Class", ylab="No. of Survivors" , xaxt="n", main= "Effect of Class Variable")
axis(1, at=c(1,2,3), labels=c("1st", "2nd","3rd"))

#No. of Survivors according to Port
sur_p = c(0,0,0)
sur_p[1] =filter(train, Survived ==1 & Embarked =="C") %>% nrow  #Sex and Survived should be numeric
sur_p[2] =filter(train, Survived ==1 & Embarked =="Q") %>% nrow
sur_p[3] =filter(train, Survived ==1 & Embarked =="S") %>% nrow

plot(sur_p,type = "o", xlab = "Ports", ylab="No. of Survivors" , xaxt="n", main = "Effect of Embarkment")
axis(1, at=c(1,2,3), labels=c("Cherbourg", "Queenstown", "Southampton"))


#Missing data
#Checkin the missing values in each column
sapply(train, function(x) sum(is.na(x)))

#Imputing the missing value for the Embarked column
levels(train$Embarked)
which(is.na(train$Embarked))
train[c(62,830),]
"S" -> train[c(62,830), "Embarked"]
train[c(62,830),]
#Using the Data from the Encyclopedia about the real survivors

#Imputing the missing valued for the Age column using MICE package
init = mice(train, maxit = 0)
init
predM = init$predictorMatrix
meth = init$method

#The not-significant variables to predict the Age are set to zero in predictor matrix
predM[, c("PassengerId", "Name","Ticket","Cabin")]=0    
meth[c("Cabin")]=""

imp = mice(train, m=5, predictorMatrix = predM, method = meth)
train = complete(imp)

#Checking whether the values are imputed
sapply(train, function(x) sum(is.na(x)))
View(train)




