library(tidyr)
library(tidyverse)
library("arules")
library("arulesViz")
#Storing dataset---------------------------------------------------------------
data1actor<-`1990.1998.actor`
data2actor<-`2009.2013.actor` 
data1genre<-`1990.1998.genre`
data2genre<-`2009.2013.genre`
tabledata1 <- merge(data1actor,data1genre,by="tconst")
tabledata2 <- merge(data2actor,data2genre,by="tconst")
alldata <- rbind(tabledata1,tabledata2) #adding all the data files for pre-processing
filter_for_value<-alldata[-grep("[?]", alldata$actor.primaryname), ] #removing names with ??
library(tidyr)
library(tidyverse)
res<-gather(filter_for_value, key, val, - tconst) %>%
  filter(val != "") %>%
  select(-key) %>%
  arrange(tconst) #for converting to single format
result <- res %>% distinct() #removing duplicates
finaldata<-separate_rows(result,val,sep=",") #seperated values with delimiter in between and getting single dataframe.
dir.create(path = "tmp", showWarnings = FALSE)
write.csv(finaldata, "./tmp/transactions.csv")
# Read that csv back in
library("arules")
library("arulesViz")
transaction <- read.transactions("./tmp/transactions.csv", format = "single", header=TRUE,sep=',',cols=c("tconst","val"))
inspect(transaction)
summary(transaction)
frequentItems <- eclat (transaction, parameter = list(supp = 0.3, maxlen = 15))
inspect(frequentItems)
itemFrequency(transaction)
itemFrequencyPlot(transaction, topN=10, type="absolute", main="Item Frequency")
#Starting Apriori Algorithm for Visualizating 100 points.
rules<- apriori (transaction, parameter = list(supp = 0.001, conf = 0.2))
prules<- apriori (transaction, parameter = list(supp = 0.001, conf = 0.437))
library(colorspace)
plot(prules, shading="confidence", control=list(main = "Two-key plot",col=rainbow(5)))
#For filtering 5 rules according to each life categories.
Rules<- apriori (transaction, parameter = list(supp = 0.001, conf = 0.1))
rules.sort <- subset(rules, lift < 1)
Rules.sort <- subset(Rules, lift == 1)
inspect(rules.sort)
inspect(Rules.sort)
#Ploting each rule Lift >1
Vrule1 <- c(0.9342105,0.7240437,0.7219251,0.6109661,0.5798344)
png(file = "Lift>1.png")
par(mar=c(15,7,7,7))
barplot(Vrule1, ylab="Confidence Value" ,ylim=c(0,1), names.arg=c("{News}=>{Documentary}", "{2012,Biography}=>{Documentary}", "{2011,Biography} => {Documentary}", "{2013, History} => {Documentary}", "{Romance} => {Drama}"), horiz=F,las=2, cex.names=.5)
dev.off()
#Ploting each rule Lift =1
Vrule2 <- c(0.4768152,0.1338042,0.1080787,0.10707210,0.1063211)
png(file = "Lift=1.png")
par(mar=c(15,7,7,7))
barplot(Vrule2, ylab="Confidence Value" ,ylim=c(0,0.5), names.arg=c("{} => {Drama}", "{} => {Action}", "{} => {Romance}", "{} => {Thriller}", "{} => {Documentary}"), horiz=F,las=2, cex.names=.5)
dev.off()
#Ploting each rule Lift<1
Vrule3 <- c(0.3908629,0.3867661,0.2310469,0.2287823,0.2111010)
png(file = "Lift<1.png")
par(mar=c(15,7,7,7))
barplot(Vrule3, ylab="Confidence Value" ,ylim=c(0,0.5), names.arg=c("{Brahmanandam}=> {Drama}", "{Biography} =>{Drama}", "{1998, Crime}=>{Comedy}", "{2013,Adventure}=>{Drama}", "{Action, Thriller}=>{Drama}"), horiz=F,las=2, cex.names=.5)
dev.off()
####Support 0.3% ---------------------------------------------------------------------
rules1 <- apriori (transaction, parameter = list(supp = 0.003, conf = 0.5))
inspect(rules1) 
freq <- apriori(transaction, parameter = list(minlen=4, maxlen=4, supp = 0.003, conf = 0.5, target = "frequent itemsets"))
inspect(freq)
freqplot <- c(38,198,63,0)
png(file = "freqplot.png")
barplot(freqplot, xlab="Iteration Number", ylab="Number of Frequent Itemsets", ylim=c(0,200), names.arg=c("Iteration 1", "Iteration 2", "Iteration 3", "Iteration 4"))
dev.off()
plot(rules1)
rules1.sort <- subset(rules1, lift > 1)
inspect(rules1.sort)
rules2 <- apriori (transaction, parameter = list(supp = 0.003, conf = 0.7))
plot(rules2)
inspect(rules2)
rules2.sort <- subset(rules2, lift > 1)
inspect(rules2.sort)
rules3 <- apriori (transaction, parameter = list(supp = 0.003, conf = 0.8))
plot(rules3)
inspect(rules3)
rules3.sort <- subset(rules3, lift > 1)
inspect(rules3.sort)
####Support 0.7% ---------------------------------------------------------------------
rules4 <- apriori (transaction, parameter = list(supp = 0.007, conf = 0.5))
plot(rules4)
inspect(rules4)
rules4.sort <- subset(rules4, lift = 1)
inspect(rules4.sort)
freq2 <- apriori(transaction, parameter = list(minlen=4, maxlen=4, supp = 0.007, conf = 0.5, target = "frequent itemsets"))
inspect(freq2)
freqplot2 <- c(35,104,11,0)
png(file = "freqplot2.png")
barplot(freqplot2, xlab="Iteration Number", ylab="Number of Frequent Itemsets", ylim=c(0,200), names.arg=c("Iteration 1", "Iteration 2", "Iteration 3", "Iteration 4"))
dev.off()
rules5 <- apriori (transaction, parameter = list(supp = 0.007, conf = 0.7))
plot(rules5)
rules5.sort <- subset(rules5, lift = 1)
inspect(rules5.sort)
rules6 <- apriori (transaction, parameter = list(supp = 0.007, conf = 0.8))
plot(rules6)
####Support 1.2% ---------------------------------------------------------------------
rules7 <- apriori (transaction, parameter = list(supp = 0.012, conf = 0.5))
plot(rules7)
inspect(rules7)
freq3 <- apriori(transaction, parameter = list(minlen=4, maxlen=4, supp = 0.012, conf = 0.5, target = "frequent itemsets"))
inspect(freq3)
freqplot3 <- c(34,63,3,0)
png(file = "freqplot3.png")
barplot(freqplot3, xlab="Iteration Number", ylab="Number of Frequent Itemsets", ylim=c(0,100), names.arg=c("Iteration 1", "Iteration 2", "Iteration 3", "Iteration 4"))
dev.off()
rules8 <- apriori (transaction, parameter = list(supp = 0.012, conf = 0.7))
plot(rules8)
rules9 <- apriori (transaction, parameter = list(supp = 0.012, conf = 0.8))
plot(rules9)
#Candidate Itemsets plotting-------------------------------------------------------------------------------------------
cfreq <- apriori(transaction, parameter = list(minlen=8, maxlen=8, supp = 0.0001, conf = 0.5, target = "frequent itemsets"))
inspect(cfreq)
cfreqplot<- c(2960,3675,2174,690,85,32,5,0)
png(file = "cfreqplot.png")
barplot(cfreqplot, xlab="Iteration Number", ylab="Number of Frequent Itemsets", ylim=c(0,3700), names.arg=c("Iteration 1", "Iteration 2", "Iteration 3", "Iteration 4","Iteration 5","Iteration 6","Iteration 7","Iteration 8"))
dev.off()
