## Exploratory Data Analysis
### Dataset - [Titanic Datset] from Kaggle (https://www.kaggle.com/c/titanic/data) 

Setting the working directory
```
setwd()
getwd()
```

Installing and Importing the required libraries
```
library(mice)
library(dplyr)
library(caret)
library(ggplot2)
library(ggthemr)
library(ggthemes)
library(reshape2)
library(corrplot)
```
Importing the titanic dataset
```
train = read.csv("train.csv", header=T, na.strings="")
View(train)
head(train)
tail(train)
```
Checking the structure of the dataset and its summary
```
> dim(train)
[1] 891  12

> str(train)
'data.frame':	891 obs. of  12 variables:
 $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
 $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
 $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
 $ Name       : Factor w/ 891 levels "Abbing, Mr. Anthony",..: 109 191 358 277 16 559 520 629 417 581 ...
 $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
 $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
 $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
 $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
 $ Ticket     : Factor w/ 681 levels "110152","110413",..: 524 597 670 50 473 276 86 396 345 133 ...
 $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
 $ Cabin      : Factor w/ 147 levels "A10","A14","A16",..: NA 82 NA 56 NA NA 130 NA NA NA ...
 $ Embarked   : Factor w/ 3 levels "C","Q","S": 3 1 3 3 3 2 3 3 3 1 ...

> summary(train)
  PassengerId       Survived          Pclass                                         Name    
 Min.   :  1.0   Min.   :0.0000   Min.   :1.000   Abbing, Mr. Anthony                  :  1  
 1st Qu.:223.5   1st Qu.:0.0000   1st Qu.:2.000   Abbott, Mr. Rossmore Edward          :  1  
 Median :446.0   Median :0.0000   Median :3.000   Abbott, Mrs. Stanton (Rosa Hunt)     :  1  
 Mean   :446.0   Mean   :0.3838   Mean   :2.309   Abelson, Mr. Samuel                  :  1  
 3rd Qu.:668.5   3rd Qu.:1.0000   3rd Qu.:3.000   Abelson, Mrs. Samuel (Hannah Wizosky):  1  
 Max.   :891.0   Max.   :1.0000   Max.   :3.000   Adahl, Mr. Mauritz Nils Martin       :  1  
                                                  (Other)                              :885  
     Sex           Age            SibSp           Parch             Ticket         Fare       
 female:314   Min.   : 0.42   Min.   :0.000   Min.   :0.0000   1601    :  7   Min.   :  0.00  
 male  :577   1st Qu.:20.12   1st Qu.:0.000   1st Qu.:0.0000   347082  :  7   1st Qu.:  7.91  
              Median :28.00   Median :0.000   Median :0.0000   CA. 2343:  7   Median : 14.45  
              Mean   :29.70   Mean   :0.523   Mean   :0.3816   3101295 :  6   Mean   : 32.20  
              3rd Qu.:38.00   3rd Qu.:1.000   3rd Qu.:0.0000   347088  :  6   3rd Qu.: 31.00  
              Max.   :80.00   Max.   :8.000   Max.   :6.0000   CA 2144 :  6   Max.   :512.33  
              NA's   :177                                      (Other) :852                   
         Cabin     Embarked  
 B96 B98    :  4   C   :168  
 C23 C25 C27:  4   Q   : 77  
 G6         :  4   S   :644  
 C22 C26    :  3   NA's:  2  
 D          :  3             
 (Other)    :186             
 NA's       :687   

```
From the above summary the basic statistical mesaure of each variable is obtained and then it is notices that there are missing values in the variables - Age, Cabin  and Embarked
          
Converting the Catergorical Variables from numeric to factors
```
train$Pclass = factor(train$Pclass, levels = c(1,2,3), labels = c("1st", "2nd","3rd"))
train$Survived = factor(train$Survived, levels=c(0,1), labels = c("Not Survived", "Survived"))

```
Taking a preliminary look at the dataset and answering basic questions
  - How many people survied?
    - How many male and female passengers?
      - How many people were in each Class?
```
> table(train$Survived)
Not Survived     Survived 
549          342 

> table(train$Pclass)
1st 2nd 3rd 
216 184 491 

> table(train$Sex)
female   male 
   314    577 
```
Histogram is used to get a better understanding of data visually
```
barplot(table(train$Pclass), xlab="Passenger Class", ylab="Frequency", main="Histogram of Passenger Class", col = "lightblue")













