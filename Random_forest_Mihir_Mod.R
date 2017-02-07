install.packages("caret",dependencies = TRUE)
library(caret)
install.packages("randomForst")
library(randomForest)
setwd("~/Course Material/R/Titanic")
install.packages("ggplot2")
library(ggplot2)
install.packages("stringr")
library(stringr)

trainSet = read.table("train.csv",sep = ",",header =  TRUE)
testSet = read.table("test.csv",sep = ",", header = TRUE)

head(trainSet)
head(testSet)
summary(trainSet)
attach(trainSet)


# exploratory
summary(trainSet)
# Pclass, gender, age buckets determine survival

#relation of class, gender and boarding position to survived. All three significant.
prop.table(table(trainSet[,c("Survived","Pclass")]),margin = 2)
prop.table(table(trainSet[,c("Survived","Sex")]),margin = 2)
prop.table(table(trainSet[,c("Survived","Embarked")]),margin = 2)
names(trainSet)

c = barplot(table(Survived,Pclass),col = c("green","red"),legend.text = sort(unique(Survived),decreasing = T))
text(c,0,round(a,1),pos = 3)

#Classifying age into buckets
Age_log = cut(Age,breaks = c(0,15,60,Inf),labels = c("c","a","o"))
summary(Age_log)
prop.table(table(Survived,Age_log),margin = 2)
barplot(table(Survived,Age_log),legend.text = sort(unique(Survived)))
Age_log = ifelse(is.na(Age_log),"2", Age_log)
Age_log = as.factor(Age_log)
summary(Age_log)

#finding appropriate buckets to cutoff
n = 16
d = cut(Age,breaks = n)
e = summary(d)[1:n]
f = seq(0,max(Age,na.rm = T),max(Age,na.rm = T)/n)[1:n+1]
g = paste(e,round(f,1),sep ="_")
barplot(prop.table(table(Survived,d),2),legend.text = sort(unique(Survived)),names.arg = g,cex.names = .6,ylim = c(.4,.8))
prop.table(table(Survived,d),margin = 2)
e
View(table(Survived,d))

m = 10
l = cut(Fare,c(-Inf,0,10,20,50,100,150,200,250,300,Inf))
o = summary(l)
p = c(10,20,50,100,150,200,250,300,Inf)
q = paste(o,round(p,1),sep = "_")
barplot(prop.table(table(Survived,l),2),legend.text = sort(unique(Survived)),names.arg = q,cex.names = .6)
prop.table(table(Survived,l),margin = 2)

#Finalizing on a new age_bucket
Age_Bucket = cut(Age,breaks = c(0,6.4,65,Inf),labels = c("c","a","o"))


#To predict age if fare was age dependent. The graph shoes that it is not.
plot(Age,Fare)
plot(Embarked,Fare)
unique(Fare)
plot(Fare,Age)
mean_fare = tapply(Fare,Cabin,mean)
plot(unique(Cabin),mean_fare)
mean_fare
summary(Cabin)
str(Cabin)
table(Cabin,Survived)
barplot(Survived,Cabin)
Cabin1 = substr(Cabin,1,1)
Cabin1 = as.factor(Cabin1)

#Getting count of people in cabins
Cabin2 = paste(sort(unique(Cabin1)),summary(Cabin1),sep = "_")
prop.table(table(Cabin1,Survived),margin = 1)
summary(as.factor(Cabin2))


#To predict age if fare was age dependent. The graph shoes that it is not.
plot(Age,Fare)

#adding a new secondary variable - family size
Family_size = SibSp + Parch
Family_size = as.factor(Family_size)
prop.table(table(Survived,Family_size),margin = 2)
h = paste(sort(unique(Family_size)),summary(Family_size),sep = "_")
barplot(prop.table(table(Survived,Family_size),2),legend.text = sort(unique(Survived)),names.arg = h,cex.names = .6)

  
#trying to see other relevant variables with survived
a = names(trainSet)

for (x in a)
{print(typeof(x))}

View(trainSet)
typeof(a)
a

for (x in a)
{print(prop.table(table(trainSet[,c("Survived",x)]),margin = 2))}

#Mostly useless
boxplot(Age~Survived,na.rm = T)
boxplot(Fare~Survived,na.rm = T)
plot(Fare,Survived)

#Imputing age missing values with adult as default value. 
summary(Age_Bucket)
Age_Bucket = ifelse(is.na(Age_Bucket),"2", Age_Bucket)
Age_Bucket = as.factor(Age_Bucket)
summary(Age_Bucket)

Has_cabin = ifelse(Cabin1 == "",F,T)
summary(Has_cabin)

Fancy_cabin = ifelse(Cabin1 %in% c("B","D","E"),T,F)


trainSet_secondary = cbind(trainSet,c(Age_Bucket),c(Family_size),c(Cabin1),c(Has_cabin),Fancy_cabin,Age_log)
colnames(trainSet_secondary) = c(names(trainSet),"Age_Bucket","Family_size","Cabin1","Has_cabin","Fancy_cabin","Age_log")

summary(trainSet_secondary)

Pclass = as.factor(Pclass)
Age_Bucket = as.factor(Age_Bucket)
Family_size = as.factor(Family_size)




summary(Age_Bucket)
str(Age_Bucket)

model <- train(factor(Survived) ~ Pclass + Sex + SibSp + Embarked + Parch + Fare + Family_size +Age_log, # Survived is a function of the variables we decided to include
               data = trainSet_secondary, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 5))
model
testSet$Survived = predict(model,newdata = testSet)

summary(testSet)
str(testSet)
names(testSet)

testSet$Fare = ifelse(test = is.na(testSet$Fare),yes = mean(testSet$Fare,na.rm = T),no = testSet$Fare)

# testSet$Age = ifelse(is.na(testSet$Age),mean(testSet$Age,na.rm = T),testSet$Age)
testSet$Age_Bucket = cut(testSet$Age,breaks = c(0,6.4,65,Inf),labels = c("c","a","o"))
testSet$Age_Bucket = ifelse(is.na(testSet$Age_Bucket),"2",testSet$Age_Bucket)
testSet$Age_Bucket = as.factor(testSet$Age_Bucket)
summary(testSet$Age_Bucket)

testSet$Family_size = testSet$SibSp + testSet$Parch
summary(testSet)

testSet$Survived = predict(model,newdata = testSet)


trainSet$Survived = factor(trainSet$Survived)
summary(trainSet)
prop.table(table(Survived))
prop.table(table(testSet$Survived))

submission = testSet[,c("PassengerId","Survived")]
write.table(submission,file = "submission.csv",col.names = TRUE,row.names = FALSE,sep = ",")

#see cross validation, selection of variables, improvements and next steps
test = c(893,920,924,931,981,1010,1041,1057,1086,1106,1117,1162,1175,1183,1194,1200,1237,1246,1251,1274,1275,1284)
test1 = subset(testSet,testSet$PassengerId %in% test)
View(test1)
