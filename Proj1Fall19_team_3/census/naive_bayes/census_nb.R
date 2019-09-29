# find elements
idx <- census.adult == " ?"
# replace elements with NA
is.na(census.adult) <- idx
View(census.adult)
complete.cases(census.adult)
na_vec<-which(!complete.cases(census.adult)) #storing in a vector
census.adult_clean <- census.adult[-na_vec,] #Data cleaned after removal of NA values
is.na(census.adult_clean)
sum(is.na(census.adult_clean)) #making sure no missing values
set.seed(10) #using given seed value of 10
sample_census<-sample(1:nrow(census.adult_clean), 2000) #using my sample size of 2k
sample_censust<- census.adult_clean[sample_census, ] #my sample of 2k generated from cleaned data file


#Splitting data into test and train sets
set.seed(10) 
pd<-sample(2,nrow(sample_censust),replace=TRUE,prob = c(0.7,0.3)) #train/test : 70/30
train<-sample_censust[pd==1,] 
test<-sample_censust[pd==2,]
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
model <- naive_bayes(V15 ~ .,data=train)
p1<-predict(model,test)
table(test[,15],p1)
t<-table(test[,15],p1)
n = sum(t) # number of instances
diag = diag(t) # number of correctly classified instances per class 
accuracy = sum(diag) / n 
accuracy 
miscalculation_rate=1-accuracy
miscalculation_rate
library(caret) 
res<-confusionMatrix(t)
precision <- res$byClass['Pos Pred Value']
precision
recall <- res$byClass['Sensitivity']
recall
f_measure <- 2 * ((precision * recall) / (precision + recall))
f_measure
