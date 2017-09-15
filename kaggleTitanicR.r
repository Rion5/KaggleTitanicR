#Chai Grindean
#March-2017

#Titanic Dataset
#change working directory
#setwd("C:/Users/.....")
titanic.train <- read.csv(file ='train.csv')
titanic.test <- read.csv(file='test.csv')


#Is Train Set T/F?
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

#Need to create a Survived column
titanic.test$Survived <- ''
#Rbind to combined the datasets
titanic.full <-rbind(titanic.train, titanic.test)

##Making Pclass display
install.packages("ggplot2")
library(ggplot2)

#Barchart to Show Survivibility & Sex
ggplot(data=titanic.train, aes(x = Sex , fill = factor(Survived)))+
  geom_bar(width = 0.5)+ #NOTE: changing to geom_bar fixed error
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill="Survived")
table(titanic.train$Pclass, titanic.train$Survived)

#Barchart to Show Survivibility & Pclass
ggplot(data=titanic.train, aes(x = Pclass, fill = factor(Survived)))+
  geom_bar(width = 0.5)+ #NOTE: changing to geom_bar fixed error
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill="Survived")
table(titanic.train$Pclass, titanic.train$Survived)

#Barchart to Show Survivibility & Age
ggplot(data=titanic.train, aes(x = Age, fill = factor(Survived)))+
  geom_bar(width = 0.5)+ #NOTE: changing to geom_bar fixed error
  xlab("Age")+
  ylab("Total Count")+
  labs(fill="Survived")
table(titanic.train$Age, titanic.train$Survived)

#Barchart to Show Survivibility & SibSp
ggplot(data=titanic.train, aes(x = SibSp, fill = factor(Survived)))+
  geom_bar(width = 0.5)+ #NOTE: changing to geom_bar fixed error
  xlab("SibSp")+
  ylab("Total Count")+
  labs(fill="Survived")
table(titanic.train$SibSp, titanic.train$Survived)

#Barchart to Show Survivibility & Parch
ggplot(data=titanic.train, aes(x = Parch, fill = factor(Survived)))+
  geom_bar(width = 0.5)+ #NOTE: changing to geom_bar fixed error
  xlab("Parch")+
  ylab("Total Count")+
  labs(fill="Survived")
table(titanic.train$Parch, titanic.train$Survived)


######WORKING ON THIS PART**********************
#geom_smooth(method = 'lm', formula=survived.formula)
#library(ggplot2)
#ggplot(data=titanic.train, aes(x = Age, fill = factor(Survived)))+
#  geom_point, aes(fill=(Survived)))+
#  xlab("Sex")+
#  ylab("Age")+
#  labs(fill="Survived")
#table(titanic.train$Age, titanic.train$Survived)


##
#Need to fill missing NA Values
##
table(titanic.full$Embarked) #Notice we have 2 NA values
titanic.full[titanic.full$Embarked=='', "Embarked"] <-'S' #replacing ''(empty value) with the mode which is S for 914
table(titanic.full$Embarked)
#DONE WITH EMBARKED



#############################
#PART 1 TESTING
#DO NOT RUN THIS SECTION - Only for testing purposes
##
#Finding Median Age
##
#table(is.na(titanic.full$Age)) #checking to see how many values are left out
#age.median <- median(titanic.full$Age, na.rm = TRUE) #Median age = 28
#titanic.full[is.na(titanic.full$Age), "Age"] <- age.median #Assigning 28 to people with missing age
#table(is.na(titanic.full$Age))
##
#Finding Median Fare
##
#head(titanic.full$Fare)
#table(is.na(titanic.full$Fare)) #1 Missing value in fares
#fare.median <-median(titanic.full$Fare, na.rm =TRUE)
#titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median
#table(is.na(titanic.full$Fare))
##############################



#############################
#PART 2 TESTING
##Cleaning up Data PART 2: Using Predictive Model
#Prior: Used Median for age and fare, now going to create
#Linear Regression Model to predict 'Fare' using Linear Ordinary Squares
###
##
#Linear Regression Model to Predict 'Age' using OLS
##
#Checking for outliers
boxplot(titanic.full$Age)
boxplot.stats(titanic.full$Age)
plot(titanic.full$Age)
#Create filters to get rid of people <= 1

age.equation = "Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked"
age.model <-lm(formula = age.equation, data = titanic.full) #lm model
#Getting Rows of Missing Values we need to Predict
age.row<- titanic.full[
  is.na(titanic.full$Age),
  c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked")
  ]
head(age.row)
age.predictions <- predict (age.model, newdata = age.row)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.predictions
titanic.full[6,] #Checking to see if Passenger 6's Age has been fixed
table(is.na(titanic.full$Age))
#DONE FILLING IN MISSING AGE VALUES

###
#Linear Regression Model to Predict 'Fare' using OLS
###
#Step 1: Checking for Outliers
boxplot(titanic.full$Fare)
boxplot.stats(titanic.full$Fare)
upper.whisker <-boxplot.stats(titanic.full$Fare)$stats[5] #UpperBound $65
outlier.filter <-titanic.full$Fare < upper.whisker #Outlier Filter
titanic.full[outlier.filter,] #Applying Outlier Filter
#Building Predictor Model for Fare
fare.equation ="Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <-lm(formula = fare.equation, data = titanic.full[outlier.filter,]) #Predictive Model using Linear Ordinary Squares
#Getting Rows of Missing Values we need to Predict
fare.row<- titanic.full[
  is.na(titanic.full$Fare),
  c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
  ]
head(fare.row) #Only 1 missing value for Fare
fare.predictions <- predict(fare.model, newdata = fare.row) #Creating a Predicting for Passenger 1044, he is predicted to have paid $7.03
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions
titanic.full[1044,] #Checking to see Passenger 1044's fare has been fixed.
table(is.na(titanic.full$Fare))
#DONE FILLING IN SINGLE MISSING FARE VALUES

##############################
#Run Part2 of filling in missing NA values
#############################
#Categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked<- as.factor(titanic.full$Embarked)


##
#Now we can make a predictive model, but we need to split up data
#back into train and test
##
titanic.train <-titanic.full[titanic.full$IsTrainSet==TRUE,]
titanic.test <-titanic.full[titanic.full$IsTrainSet==FALSE,]
titanic.train$Survived <- as.factor(titanic.train$Survived) #as factor
table(titanic.full$Survived)

##
#RANDOM FOREST PREDICTIVE MODEL
##
#install.packages("randomForest")
library (randomForest)
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
#Creating Random Forest
titanicRandomForest.model <- randomForest(formula = survived.formula, data = titanic.train, ntree=3000, mtry=5, nodesize =0.01 *nrow(titanic.train))
features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"                                     
Survived <- predict(titanicRandomForest.model, newdata = titanic.test) #Creates Survived = Titanic predictions
#Writing to CSV to submit predictions to kaggle
PassengerId <- titanic.test$PassengerId
output.df <-as.data.frame(PassengerId) #Creating new dataframe with PassengerId
output.df$Survived <- Survived #Passing along 'Survived' predictions to the new dataframe
head(output.df)
write.csv(output.df, file='kaggle_submission.csv', row.names=FALSE) #Your submission scored 0.78469

##
#Creating Descision Tree
##
library(rpart)
#install.packages("rattle")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
my_tree <- rpart(Survived ~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = titanic.train, method = "class")
#FancyRpartPlot
fancyRpartPlot(my_tree)
table(titanic.train$Sex)

