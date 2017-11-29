## Exploratory Data Analysis
### Dataset - [Titanic Datset] from Kaggle (https://www.kaggle.com/c/titanic/data) 

#### Setting the working directory
```
setwd()
getwd()
```

#### Installing and Importing the required libraries
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
#### Importing the titanic dataset
```
train = read.csv("train.csv", header=T, na.strings="")
View(train)
head(train)
tail(train)
```
#### Checking the structure of the dataset and its summary
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
From the above summary the basic statistical mesaure of each variable is obtained and then it is noticed that there are missing values in the variables - Age, Cabin  and Embarked
          
#### Converting the Catergorical Variables from numeric to factors
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
#### Histogram is used to get a better understanding of data visually
```
barplot(table(train$Pclass), xlab="Passenger Class", ylab="Frequency", main="Histogram of Passenger Class", col = "lightblue")
barplot(table(train$Sex), xlab="Sex", ylab="Frequency", main="Histogram of Sex", col = "red")
barplot(table(train$Embarked), xlab="Port of Embarkment", ylab="Frequency", main="Histogram of Port of Embarkment", col = "green")
```

![hist_pclass](https://user-images.githubusercontent.com/16735822/33371508-fc810d40-d520-11e7-84b2-2065180f0095.png)
![hist_sex](https://user-images.githubusercontent.com/16735822/33371491-eba6d612-d520-11e7-8992-63854d71d911.png)
![hist_emb](https://user-images.githubusercontent.com/16735822/33371530-10b8b1d2-d521-11e7-8193-e22219810746.png)


#### Effect of each variable on the Survival Rate is identified
```
sur_sex = c(0,0)
sur_sex[1] =filter(train, Survived ==1 & Sex =="1") %>% nrow  #Sex and Survived should be numeric
sur_sex[2] =filter(train, Survived ==1 & Sex =="2") %>% nrow

plot(sur_sex,type = "o", xlab = "Sex", ylab="No. of Survivors" , xaxt="n")
axis(1, at=c(1,2), labels=c("Female", "Male"))
```
![eff_sex](https://user-images.githubusercontent.com/16735822/33371678-87bc73e0-d521-11e7-82d3-b623944a063c.png)

The female survivors are much greater than the male survivors


```
sur_cls = c(0,0,0)
sur_cls[1] =filter(train, Survived ==1 & Pclass ==1) %>% nrow 
sur_cls[2] =filter(train, Survived ==1 & Pclass ==2) %>% nrow
sur_cls[3] =filter(train, Survived ==1 & Pclass ==3) %>% nrow

plot(sur_cls,type = "o", xlab = "Sex", ylab="No. of Survivors" , xaxt="n")
axis(1, at=c(1,2,3), labels=c("1st", "2nd","3rd"))
```
![eff_class](https://user-images.githubusercontent.com/16735822/33371701-9bfe7fe2-d521-11e7-9d59-2af4802f96cc.png)

The first class passengers survived the most and lowest number of survivors were in the 2nd class of the ship


```
sur_p = c(0,0,0)
sur_p[1] =filter(train, Survived ==1 & Embarked =="C") %>% nrow 
sur_p[2] =filter(train, Survived ==1 & Embarked =="Q") %>% nrow
sur_p[3] =filter(train, Survived ==1 & Embarked =="S") %>% nrow

plot(sur_p,type = "o", xlab = "Sex", ylab="No. of Survivors" , xaxt="n")
axis(1, at=c(1,2,3), labels=c("Cherbourg", "Queenstown", "Southampton"))
```
![eff_emb](https://user-images.githubusercontent.com/16735822/33371734-b6874f10-d521-11e7-93fc-f1dcfb692e89.png)

The people who boarded at the Queenstown port survived the least while the maximum corresponds to the Southhampton port



#### Interaction effect 
 - Interaction effect is used to the combined interaction effect on the survival rate of the passengers
 
```
interaction.plot(train$Pclass, train$Sex, train$Survived, fun=mean, legend = TRUE, xlab="Passenger Class", ylab="Mean number of Survivors", main="Interaction Effect between Passenger Class and Sex")
```
![iplot_class_sex](https://user-images.githubusercontent.com/16735822/33371761-c924f47e-d521-11e7-973b-438165fc2c9f.png)

Intraction effect is there between the Sex and the Class variable. The first class and second female passengers has the highest mean number of survivor than the third class female passengers. Regarding male, first class passengers survived more than the other two class.

```
interaction.plot(train$Pclass, train$Embarked, train$Survived, fun=mean, legend = TRUE, xlab="Passenger Class", ylab="Mean number of Survivors", main="Interaction Effect between Passenger Class and Sex")
```
![iplot_class_emb](https://user-images.githubusercontent.com/16735822/33371779-d61d9a46-d521-11e7-9156-16155d275b64.png)

Iteraction effect also exist between Class and the Embarkment Port. 
- First class passengers boarded from Queenstown and Southhampton survived the most, while 3rd class passengers from these two ports survived the least
 - Mean number of Survivors for the 1st and 2nd class from Cherbourg port seems to be the same.
 - There is no interaction effect for 2nd and 3rd class passengers boarded from Cherbourg and Queenstown
 
 
### Missing data Imputation 
#### Checking the missing values in each column respectively
```
> sapply(train, function(x) sum(is.na(x)))
PassengerId    Survived      Pclass        Name         Sex         Age       SibSp       Parch 
          0           0           0           0           0         177           0           0 
     Ticket        Fare       Cabin    Embarked 
          0           0         687           2 
```
#### Imputing the 2 missing value for the Embarked Column
```
> which(is.na(train$Embarked))
[1]  62 830

> train[c(62,830),]
    PassengerId Survived Pclass                                      Name    Sex Age SibSp Parch
62           62        1      1                       Icard, Miss. Amelie female  38     0     0
830         830        1      1 Stone, Mrs. George Nelson (Martha Evelyn) female  62     0     0
    Ticket Fare Cabin Embarked
62  113572   80   B28     <NA>
830 113572   80   B28     <NA>

> "S" -> train[c(62,830), "Embarked"]

> train[c(62,830),]
    PassengerId Survived Pclass                                      Name    Sex Age SibSp Parch
62           62        1      1                       Icard, Miss. Amelie female  38     0     0
830         830        1      1 Stone, Mrs. George Nelson (Martha Evelyn) female  62     0     0
    Ticket Fare Cabin Embarked
62  113572   80   B28        S
830 113572   80   B28        S
```
The above two missing values are imputed by using the data avaiable publicly in Encyclopedia about the two passengers. Since, they both have started their journey from Southhampton, the corresponding levels are assigned to them.

#### Imputation of values in Age column is doing using MICE package
```
init = mice(train, maxit = 0)
Multiply imputed data set
Call:
mice(data = train, maxit = 0)
Number of multiple imputations:  5
Missing cells per column:
PassengerId    Survived      Pclass        Name         Sex         Age       SibSp       Parch 
          0           0           0           0           0         177           0           0 
     Ticket        Fare       Cabin    Embarked 
          0           0         687           0 
Imputation methods:
PassengerId    Survived      Pclass        Name         Sex         Age       SibSp       Parch 
         ""          ""          ""          ""          ""       "pmm"          ""          "" 
     Ticket        Fare       Cabin    Embarked 
         ""          ""   "polyreg"          "" 
VisitSequence:
  Age Cabin 
    6    11 
PredictorMatrix:
            PassengerId Survived Pclass Name Sex Age SibSp Parch Ticket Fare Cabin Embarked
PassengerId           0        0      0    0   0   0     0     0      0    0     0        0
Survived              0        0      0    0   0   0     0     0      0    0     0        0
Pclass                0        0      0    0   0   0     0     0      0    0     0        0
Name                  0        0      0    0   0   0     0     0      0    0     0        0
Sex                   0        0      0    0   0   0     0     0      0    0     0        0
Age                   1        1      1    1   1   0     1     1      1    1     1        1
SibSp                 0        0      0    0   0   0     0     0      0    0     0        0
Parch                 0        0      0    0   0   0     0     0      0    0     0        0
Ticket                0        0      0    0   0   0     0     0      0    0     0        0
Fare                  0        0      0    0   0   0     0     0      0    0     0        0
Cabin                 1        1      1    1   1   1     1     1      1    1     0        1
Embarked              0        0      0    0   0   0     0     0      0    0     0        0
Random generator seed value:  NA 
predM = init$predictorMatrix
meth = init$method
```
The initial itertaion is run with maxit set as zero. The predictor matrix and the method are extracted and stored separately in 
variables.
Now the, the predictor matrix is modified by assigning the value zero - to the columns which are useless in predicting the Age. Since, we are only predicting Age ,the imputation method for Cabin is set as NULL.

```
predM[, c("PassengerId", "Name","Ticket","Cabin")]=0    
meth[c("Cabin")]=""
```
As the changes are made, the imputation is iterated for 5 times with the respective predictor matrix and imputation methods. Complete function of MICE package will append the imputed values to the original data set.
Finally, checking is done to see whether the values are imputed or not.

```
imp = mice(train, m=5, predictorMatrix = predM, method = meth)
train = complete(imp)

> sapply(train, function(x) sum(is.na(x)))
PassengerId    Survived      Pclass        Name         Sex         Age       SibSp       Parch      Ticket 
          0           0           0           0           0           0           0           0           0 
       Fare       Cabin    Embarked 
          0         687           0 

```
Since, the Cabin Variable is highly having unique values and its very difficult to interpret and obtain those values, they are not imputed and it is also removed from the modelling because of the high number of missing values. 


#### Correlation matrix 
  - Correlation matrix is used to see the correlation between the variables and it is useful in finding if multicollinearity exists between the explanatory variables.
Note : Correlation matrix cannot be run with NA values. So, the values must be imputed beforehand.

```
> cor_vars = train[,c("Age", "SibSp", "Fare", "Parch")]

> cor(cor_vars)
              Age      SibSp       Fare      Parch
Age    1.00000000 -0.3071980 0.09398979 -0.2052178
SibSp -0.30719798  1.0000000 0.15965104  0.4148377
Fare   0.09398979  0.1596510 1.00000000  0.2162249
Parch -0.20521777  0.4148377 0.21622494  1.0000000

> corrplot(cor(cor_vars))
```

![corrplot](https://user-images.githubusercontent.com/16735822/33371809-e6f12892-d521-11e7-8567-34e33534f711.png)

From the correlation matrix and the correlation plot, confirms our inital intution that there must be correlation between the Size of the family and the Ticket Fares. But not of them are severe enough to cause multicollinearity.


The suitable modelling can be applied to the dataset as the inital analysis is done to predict the survivors. 











