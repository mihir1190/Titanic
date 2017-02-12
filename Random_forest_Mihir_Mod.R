#Try  logistic regression, random forest in the 2 ways, ticket number variable, family

#Download packages
install.packages("caret",dependencies = TRUE)
install.packages("randomForst")
install.packages("ggplot2")
install.packages("stringr")
install.packages("rpart")


setwd("~/Course Material/R/Titanic")

#Install Packages
library(caret)
library(randomForest) #random forest for classification
library(ggplot2) #fancy plots
library(stringr)
library(readr) #reading csv
library(rpart) #regression for continuous variables

#Import training data
trainSet = read.table("train.csv",sep = ",",header =  TRUE)
testSet = read.table("test.csv",sep = ",", header = TRUE)
testSet$Survived = 0
comb = rbind(trainSet,testSet)

#Quick view and summary
head(trainSet)
head(testSet)
summary(trainSet)
summary(testSet)
str(trainSet)
str(testSet)
names(trainSet)
names(testSet)
names(combi)

#1) Missing Value treatment
# Age estimated using regression and Fare estimnated as mean
Age_est = rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                data=comb[!is.na(comb$Age),], method="anova")

comb$Age[is.na(comb$Age)] = predict(Age_est, comb[is.na(comb$Age),])

#missing value fare
which(is.na(comb$Fare))
comb$Fare[1044] = mean(comb$Fare,na.rm = T)

#Fixing Embarked
prop.table(table(trainSet[,c("Survived","Embarked")]),margin = 2)
#Some people have no embarked information but all of them have survived.
summary(comb$Embarked)
#Since they are only 2 and both of them survived, I am going to assign them to C as it has the highest survival rate
which(comb$Embarked =="")
comb$Embarked[62] = "C"
comb$Embarked[830] = "C"

summary(comb)
train = comb[1:891,]

#2) - Secondary Parameters
#Classifying age into buckets
comb$Age_log = cut(comb$Age,breaks = c(0,15,60,Inf),labels = c("c","a","o"))
# comb$Age_log = ifelse(is.na(comb$Age_log),"2", comb$Age_log)
comb$Age_log = as.factor(comb$Age_log)


#finding appropriate buckets to cutoff of age
n = 16
d = cut(trainSet$Age,breaks = n)
e = summary(d)[1:n]
f = seq(0,max(trainSet$Age,na.rm = T),max(trainSet$Age,na.rm = T)/n)[1:n+1]
g = paste(e,round(f,1),sep ="_")
barplot(prop.table(table(trainSet$Survived,d),2),legend.text = sort(unique(trainSet$Survived)),names.arg = g,cex.names = .6,ylim = c(.4,.8))

#Finalizing on a new age_bucket
comb$Age_Bucket = cut(comb$Age,breaks = c(0,6.4,65,Inf),labels = c("c","a","o"))
#Imputing age missing values with adult as default value. 
# comb$Age_Bucket = ifelse(is.na(comb$Age_Bucket),"2", comb$Age_Bucket)
comb$Age_Bucket = as.factor(comb$Age_Bucket)

#To ensure that the proportion of people in age buckets is the same.
prop.table(summary(comb$Age_Bucket))
prop.table(summary(train$Age_Bucket))


#finding appropriate buckets to cutoff of Fare
l = cut(trainSet$Fare,c(0,10,20,30,40,50,60,80,100,120,550,Inf))
o = summary(l)
barplot(prop.table(table(trainSet$Survived,l),2),legend.text = unique(trainSet$Survived))
o
table(trainSet$Survived,l)
# The buckets are made keeping the table and counts in mind. Beyond 80, the survival rate reaches 85%. 
# The last bucket has a smaller percentage because it contains a family. 
# It has been vizualized in the next couple of lines.
comb$Fare_bucket = cut(comb$Fare,breaks = c(-1,10,50,80,Inf),labels = c("l","m","h","u"))
comb$Fare_bucket = as.factor(comb$Fare_bucket)

# Exploring the fare outliers
s1 = trainSet[trainSet$Fare >120 & trainSet$Survived ==0,]
View(s1)
s2 = trainSet[trainSet$Fare >40 & trainSet$Fare<50,]
View(s2)

# Extracting information out of Cabin
summary(trainSet$Cabin)
str(trainSet$Cabin)
table(trainSet$Cabin,trainSet$Survived)
barplot(trainSet$Survived,trainSet$Cabin)
comb$Cabin1 = as.factor(substr(comb$Cabin,1,1))


#Getting count of people in cabins
trainSet$Cabin1 = as.factor(substr(trainSet$Cabin,1,1))
trainSet$Cabin2 = paste(sort(unique(trainSet$Cabin1)),summary(trainSet$Cabin1),sep = "_")
prop.table(table(trainSet$Cabin1,trainSet$Survived),margin = 1)
summary(as.factor(trainSet$Cabin2))


train = comb[1:891,]

#Adding titles to people.
comb$Name <- as.character(comb$Name)
comb$title = sapply(comb$Name, FUN = function(x) {strsplit(x,split='[,.]')[[1]][2]})
comb$title = sub(' ','',comb$title)

#To find title buckets
train = comb[1:891,]
summary(train$title)
table(comb$title)
prop.table(table(train$Survived,train$title),2)

comb$title[comb$title %in% c('Mme', 'Mlle','the Countess','Lady','Ms','Sir','Dona')] ='Elite'
comb$title[comb$title %in% c('Capt','Don','Jonkheer','Rev')] ='Martry'
comb$title[comb$title %in% c('Dr','Major')] ='Senior'
unique(comb$title)


#adding a new secondary variable - family size
comb$Family_size = comb$SibSp + comb$Parch
comb$Family_size = as.factor(comb$Family_size)

prop.table(table(Survived,Family_size),margin = 2)
h = paste(sort(unique(Family_size)),summary(Family_size),sep = "_")
barplot(prop.table(table(Survived,Family_size),2),legend.text = sort(unique(Survived)),names.arg = h,cex.names = .6)


#Has_cabin = T or F. Fancy_cabin = T or F
comb$Has_cabin = ifelse(comb$Cabin1 == "",F,T)
summary(comb$Has_cabin)
comb$Fancy_cabin = ifelse(comb$Cabin1 %in% c("B","D","E"),T,F)

comb$Pclass = as.factor(comb$Pclass)

#3) Exploratory Analysis

#relation of class, gender and boarding position to survived. All three significant.
prop.table(table(train[,c("Survived","Pclass")]),margin = 2)
prop.table(table(train[,c("Survived","Sex")]),margin = 2)
prop.table(table(train[,c("Survived","Embarked")]),margin = 2)
prop.table(table(Survived,Age_log),margin = 2)

# Pclass, gender, age buckets determine survival
barplot(prop.table(table(train[,c("Survived","Sex")]),margin = 2),col = c("red","green"),legend.text = unique(Survived),main = "Survived?")
barplot(prop.table(table(train[,c("Survived","Pclass")]),margin = 2),col = c("red","green"),legend.text = unique(Survived),main = "Survived?")
barplot(prop.table(table(train[,c("Survived","Embarked")]),margin = 2),col = c("red","green"),legend.text = unique(Survived),main = "Survived?")
barplot(prop.table(table(Survived,Age_log),margin = 2),legend.text = sort(unique(Survived)),col = c("red","green"),names.arg = paste(summary(Age_log),c("c","a","o"),sep = "_"))


# #To predict age if fare was age dependent. The graph shoes that it is not.
# plot(Age,Fare)
# plot(Embarked,Fare)
# unique(Fare)
# plot(Fare,Age)
# mean_fare = tapply(Fare,Cabin,mean)
# plot(unique(Cabin),mean_fare)
# mean_fare


#To predict age if fare was age dependent. The graph shoes that it is not. Embarked significantly governs fare
plot(Age,Fare)
tapply(Fare,d,mean)
tapply(Fare,Age_log,mean)
tapply(Fare,Age_Bucket,mean)
tapply(Fare,list(Embarked,Sex),mean)
tapply(Fare,Sex,mean)
table(Cabin1,Pclass)
testSet$Cabin1 = substr(testSet$Cabin,1,1)
testSet$Cabin1 = as.factor(testSet$Cabin1)
table(as.factor(trainSet_secondary$Cabin1),as.factor(trainSet_secondary$Pclass))
table(testSet$Cabin1,testSet$Pclass)

  
# #trying to see other relevant variables with survived
# a = names(trainSet)
# 
# for (x in a)
# {print(typeof(x))}
# 
# View(trainSet)
# typeof(a)
# a
# 
# for (x in a)
# {print(prop.table(table(trainSet[,c("Survived",x)]),margin = 2))}

#Mostly useless
boxplot(Age~Survived,na.rm = T)
boxplot(Fare~Survived,na.rm = T)
plot(Fare,Survived)


trainSet_secondary = cbind(trainSet,c(Age_Bucket),c(Family_size),c(Cabin1),c(Has_cabin),Fancy_cabin,Age_log)
colnames(trainSet_secondary) = c(names(trainSet),"Age_Bucket","Family_size","Cabin1","Has_cabin","Fancy_cabin","Age_log")

names(comb)

train = comb[1:891,]
test = comb[892:1309,]

model <- train(factor(Survived) ~ Pclass + Sex + SibSp +Age + Embarked + Parch + Cabin1 + Age_Bucket 
   + Fare + Family_size +Age_log + Has_cabin +title + Fare_bucket + Fancy_cabin, # Survived is a function of the variables we decided to include
               data = train,method = "rf",
               trControl = trainControl(method = "cv",
                                        number = 5))
model
test$Survived = predict(model,newdata = test)
train$Survived = as.factor(train$Survived)

model1 = glm(Survived~Pclass + Sex + SibSp +Age + Embarked + Parch + Cabin1 + Age_Bucket 
             + Fare + Family_size +Age_log + Has_cabin +title + Fare_bucket + Fancy_cabin, # Survived is a function of the variables we decided to include
             data = train,family = binomial(logit))
model1
plot(model1)
test$Survived1 = predict(model1,newdata = test,type = "response")
test$Survived = sapply(test$Survived1,FUN = function(x) {ifelse(x>.6,1,0)})
mean(test$Surviced)
submission = test[,c("PassengerId","Survived")]

write.table(submission,file = "submission_glm.csv",col.names = TRUE,row.names = FALSE,sep = ",")

#see cross validation, selection of variables, improvements and next steps
test = c(893,920,924,931,981,1010,1041,1057,1086,1106,1117,1162,1175,1183,1194,1200,1237,1246,1251,1274,1275,1284)
test1 = subset(testSet,testSet$PassengerId %in% test)
View(test1)
