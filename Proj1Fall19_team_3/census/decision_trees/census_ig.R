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
library(caret)
library(rpart.plot)

dtm <-rpart(V15~.,train, method="class", parms = list(split = 'information')) #for splitting nodes we use information gain
rpart.plot(dtm)
library(caret)
varImp(dtm) #finding least important variables
p<-predict(dtm, test, type="class")
table(test[,15],p)
t<-table(test[,15],p)
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
df = subset(sample_censust, select = -c(3) ) #withholding the 3rd column as it is less important
pd2<-sample(2,nrow(df),replace=TRUE,prob = c(0.7,0.3)) #train/test : 70/30
train2<-df[pd2==1,] 
test2<-df[pd2==2,]
library(caret)
library(rpart.plot)
dtm2 <-rpart(V15~.,train2, method="class", parms = list(split = 'information')) #for splitting nodes we use information gain
rpart.plot(dtm2)
p<-predict(dtm2, test2, type="class")
table(test2[,14],p)
t2<-table(test2[,14],p)
n2 =sum(t2)
diag2 = diag(t2) # number of correctly classified instances per class 
accuracy2 = sum(diag2) / n2 
accuracy2 
miscalculation_rate2=1-accuracy2
miscalculation_rate2
t2<-table(test2[,14],p)
res2<-confusionMatrix(t2)
precision2 <- res2$byClass['Pos Pred Value']
precision2
recall2 <- res2$byClass['Sensitivity']
recall2
f_measure2 <- 2 * ((precision2 * recall2) / (precision2 + recall2))
f_measure2
