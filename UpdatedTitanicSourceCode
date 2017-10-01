titanic.train <- read.csv(file ='train.csv')
titanic.test <- read.csv(file='test.csv')
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE
titanic.test$Survived <- ''
titanic.full <-rbind(titanic.train, titanic.test)
library(ggplot2)
ggplot(data=titanic.train, aes(x = Sex , fill = factor(Survived)))+
geom_bar(width = 0.5)+ #NOTE: changing to geom_bar fixed error
xlab("Sex")+
ylab("Total Count")+
labs(fill="Survived",  title = "Survivibility & Sex")
table(titanic.train$Pclass, titanic.train$Survived)
ggplot(data=titanic.train, aes(x = Pclass, fill = factor(Survived)))+
geom_bar(width = 0.5)+ #NOTE: changing to geom_bar fixed error
xlab("Pclass")+
ylab("Total Count")+
labs(fill="Survived", title = "Survivbility & Pclass")
table(titanic.train$Pclass, titanic.train$Survived)
ggplot(data=titanic.train, aes(x = Age, fill = factor(Survived)))+
geom_bar(width = 0.5)+ #NOTE: changing to geom_bar fixed error
xlab("Age")+
ylab("Total Count")+
labs(fill="Survived", title = "Survivibility & Age")
table(titanic.train$Age, titanic.train$Survived)
ggplot(data=titanic.train, aes(x = SibSp, fill = factor(Survived)))+
geom_bar(width = 0.5)+ #NOTE: changing to geom_bar fixed error
xlab("SibSp")+
ylab("Total Count")+
labs(fill="Survived", title = "Survivibility & Siblings/Spouse")
table(titanic.train$SibSp, titanic.train$Survived)
table(titanic.full$Embarked) #Notice we have 2 NA values
titanic.full[titanic.full$Embarked=='', "Embarked"] <-'S' #replacing ''(empty value) with the mode which is S for 914
table(titanic.full$Embarked)
boxplot(titanic.full$Age)
boxplot.stats(titanic.full$Age)
plot(titanic.full$Age)
age.equation = "Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked"
age.model <-lm(formula = age.equation, data = titanic.full) #lm model
age.row<- titanic.full[
is.na(titanic.full$Age),
c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked")
]
head(age.row)
age.predictions <- predict (age.model, newdata = age.row)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.predictions
titanic.full[6,] #Checking to see if Passenger 6's Age has been fixed
table(is.na(titanic.full$Age))
boxplot(titanic.full$Fare)
boxplot.stats(titanic.full$Fare)
upper.whisker <-boxplot.stats(titanic.full$Fare)$stats[5] #UpperBound $65
outlier.filter <-titanic.full$Fare < upper.whisker #Outlier Filter
titanic.full[outlier.filter,] #Applying Outlier Filter
fare.equation ="Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <-lm(formula = fare.equation, data = titanic.full[outlier.filter,]) #Predictive Model using Linear Ordinary Squares
fare.row<- titanic.full[
is.na(titanic.full$Fare),
c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
]
head(fare.row) #Only 1 missing value for Fare
fare.predictions <- predict(fare.model, newdata = fare.row) #Creating a Predicting for Passenger 1044, he is predicted to have paid $7.03
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions
titanic.full[1044,] #Checking to see Passenger 1044's fare has been fixed.
table(is.na(titanic.full$Fare))
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked<- as.factor(titanic.full$Embarked)
library(stringr)
library(dplyr)
zero.fare <- titanic.train %>%
filter(Fare == 0.0)
zero.fare
zero.fare.pclass <- zero.fare %>%
group_by(Pclass) %>%
summarize(Total = n()) %>%
arrange(desc(Total))
titanic.full <- titanic.full %>%
mutate(Title = str_extract(Name, "[a-zA-Z]+\\."))
table(titanic.full$Title)
titles.lookup <- data.frame(Title = c("Mr.", "Capt.","Col.","Don.","Dr.","Jonkheer.","Major.",
"Rev.","Master.","Sir.",
"Mrs.","Dona.","Lady.","Mme.","Countess.",
"Miss.","Mlle.","Ms."),
New.Title = c(rep("Mr.",10),
rep("Mrs.",5),
rep("Miss",3)),
stringsAsFactors = FALSE)
titles.lookup
titanic.full <- titanic.full %>%
left_join(titles.lookup, by = "Title") %>%
mutate(Title = New.Title) %>%
select(-New.Title)
misses <- titanic.train[which(str_detect(titanic.train$Name, "Miss.")),]
head(misses)
ggplot(data=misses, aes(x = Pclass , fill = factor(Survived)))+
geom_bar(width = 0.5)+ #NOTE: changing to geom_bar fixed error
xlab("Pclass of Misses")+
ylab("Total Count")+
labs(fill="Survived", title = "Survival Rate for Misses")
table(misses$Pclass, misses$Survived)
mr <- titanic.train[which(str_detect(titanic.train$Name, "Mr.")),]
head(mr)
ggplot(data=mr, aes(x = Pclass , fill = factor(Survived)))+
geom_bar(width = 0.5)+ #NOTE: changing to geom_bar fixed error
xlab("Pclass of Mr")+
ylab("Total Count")+
labs(fill="Survived" ,title = "Survival Rate for Mr")
table(misses$Pclass, misses$Survived)
titanic.full$Title <- as.factor(titanic.full$Title)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
my_tree <- rpart(Survived ~Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = titanic.train, method = "class")
fancyRpartPlot(my_tree)
