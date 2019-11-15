library(dplyr)
library(ggplot2)
library(stringr)
library(janitor)
library(lubridate)
library(tidyverse)

my2006  <- hourly_2006[grep("200602", hourly_2006$YEARMODA), ]
#subsetting data with month february
list.st2006<-unique(my2006$STN...)
my2007  <- hourly_2007[grep("200702", hourly_2007$YEARMODA), ]
list.st2007<-unique(my2007$STN...)
setdiff(list.st2006, list.st2007)
my2008  <- hourly_2008[grep("200802", hourly_2008$YEARMODA), ]
list.st2008<-unique(my2008$STN...)
#-------------------------------------FOR YEAR 2006 Y1--------------------------------------
my2006$YEARMODA <- lubridate::ymd_h(my2006$YEARMODA)
my2006$Date <- as.Date(my2006$YEARMODA)
my2006$Time <- format(as.POSIXct(my2006$YEARMODA) ,format = "%H:%M:%S")
my2006$year <- year(ymd(my2006$Date))
my2006$month <- month(ymd(my2006$Date)) 
my2006$day <- day(ymd(my2006$Date))
library(tidyr)
library(naniar)
my2006$STP <- replace(my2006$STP, my2006$STP ==9999.9 , NA)
my2006$TEMP <- replace(my2006$TEMP, my2006$TEMP ==9999.9 , NA)
my2006$DEWP <- replace(my2006$DEWP, my2006$DEWP ==9999.9 , NA)
my2006$WDSP <- replace(my2006$WDSP, my2006$WDSP ==999.9 , NA)
#For Daily average calculation.
dtemp_avg2006<-aggregate(TEMP~STN...+day, data=my2006,mean,na.rm=TRUE, na.action=na.pass)
dstp_avg2006<-aggregate(STP~STN...+day, data=my2006,mean,na.rm=TRUE, na.action=na.pass)
ddewp_avg2006<-aggregate(DEWP~STN...+day, data=my2006,mean,na.rm=TRUE, na.action=na.pass)
dwdsp_avg2006<-aggregate(WDSP~STN...+day, data=my2006,mean,na.rm=TRUE, na.action=na.pass)
#For Monthly average temp. calculation
mtemp_avg2006<-aggregate(TEMP~STN..., data=dtemp_avg2006,mean,na.rm=TRUE, na.action=na.pass)
mstp_avg2006<-aggregate(STP~STN..., data=dstp_avg2006,mean,na.rm=TRUE, na.action=na.pass)
mdewp_avg2006<-aggregate(DEWP~STN..., data=ddewp_avg2006,mean,na.rm=TRUE, na.action=na.pass)
mwdsp_avg2006<-aggregate(WDSP~STN..., data=dwdsp_avg2006,mean,na.rm=TRUE, na.action=na.pass)
#For Clustering
# merge two data frames by ID
table <- merge(mtemp_avg2006,mstp_avg2006,by="STN...")
t2<- merge(table,mdewp_avg2006,by="STN...")
table2006 <- merge(t2,mwdsp_avg2006,by="STN...") #this data frame will be used for clustering
set.seed(10)
table2006t = table2006
library(imputeTS)
table2006.features<-na_mean(table2006t)
View(table2006.features)
library(amap)
library(clusteval)
set.seed(10)
eres2006<-Kmeans(table2006.features,5,method='euclidean',iter.max = 40)
pres2006<-Kmeans(table2006.features,5,method='pearson')
Ecluster2006s1 <- Kmeans(table2006.features,5,method='euclidean',iter.max = 40)$cluster
Pcluster2006s1 <-Kmeans(table2006.features,5,method='pearson')$cluster
View(eres2006$withinss)
cluster_similarity(Ecluster2006s1 , Pcluster2006s1, similarity = "jaccard", method="independence") #Using Jaccard
##With a different seed value
set.seed(150)
e1res2006<-Kmeans(table2006.features,5,method='euclidean',iter.max = 40)
p1res2006<-Kmeans(table2006.features,5,method='pearson')
View(e1res2006$withinss)
Ecluster2006s2 <- Kmeans(table2006.features,5,method='euclidean',iter.max = 40)$cluster
Pcluster2006s2 <-Kmeans(table2006.features,5,method='pearson')$cluster
library(clusteval) #Using Jaccard
cluster_similarity(Ecluster2006s2 , Pcluster2006s2, similarity = "jaccard", method="independence")
#---------------------------------FOR YEAR 2007 Y2---------------------------------------------#
my2007$YEARMODA <- lubridate::ymd_h(my2007$YEARMODA)
my2007$Date <- as.Date(my2007$YEARMODA)
my2007$Time <- format(as.POSIXct(my2007$YEARMODA) ,format = "%H:%M:%S")
my2007$year <- year(ymd(my2007$Date))
my2007$month <- month(ymd(my2007$Date)) 
my2007$day <- day(ymd(my2007$Date))
library(tidyr)
library(naniar)
my2007$STP <- replace(my2007$STP, my2007$STP ==9999.9 , NA)
my2007$TEMP <- replace(my2007$TEMP, my2007$TEMP ==9999.9 , NA)
my2007$DEWP <- replace(my2007$DEWP, my2007$DEWP ==9999.9 , NA)
my2007$WDSP <- replace(my2007$WDSP, my2007$WDSP ==999.9 , NA)
#For Daily average calculation.
dtemp_avg2007<-aggregate(TEMP~STN...+day, data=my2007,mean,na.rm=TRUE, na.action=na.pass)
dstp_avg2007<-aggregate(STP~STN...+day, data=my2007,mean,na.rm=TRUE, na.action=na.pass)
ddewp_avg2007<-aggregate(DEWP~STN...+day, data=my2007,mean,na.rm=TRUE, na.action=na.pass)
dwdsp_avg2007<-aggregate(WDSP~STN...+day, data=my2007,mean,na.rm=TRUE, na.action=na.pass)
#For Monthly average temp. calculation
mtemp_avg2007<-aggregate(TEMP~STN..., data=dtemp_avg2007,mean,na.rm=TRUE, na.action=na.pass)
mstp_avg2007<-aggregate(STP~STN..., data=dstp_avg2007,mean,na.rm=TRUE, na.action=na.pass)
mdewp_avg2007<-aggregate(DEWP~STN..., data=ddewp_avg2007,mean,na.rm=TRUE, na.action=na.pass)
mwdsp_avg2007<-aggregate(WDSP~STN..., data=dwdsp_avg2007,mean,na.rm=TRUE, na.action=na.pass)
#For Clustering
# merge two data frames by ID
table1 <- merge(mtemp_avg2007,mstp_avg2007,by="STN...")
t3<- merge(table1,mdewp_avg2007,by="STN...")
table2007 <- merge(t3,mwdsp_avg2007,by="STN...") #this data frame will be used for clustering
set.seed(10)
table2007t = table2007
library(imputeTS)
table2007.features<-na_mean(table2007t)
View(table2007.features)
library(amap)
library(clusteval)
set.seed(10)
eres2007<-Kmeans(table2007.features,5,method='euclidean',iter.max = 40)
pres2007<-Kmeans(table2007.features,5,method='pearson')
Ecluster2007s1 <- Kmeans(table2007.features,5,method='euclidean',iter.max = 40)$cluster
Pcluster2007s1 <-Kmeans(table2007.features,5,method='pearson')$cluster
cluster_similarity(Ecluster2007s1 , Pcluster2007s1, similarity = "jaccard", method="independence") #Using Jaccard
##With a different seed value
set.seed(150)
e1res2007<-Kmeans(table2007.features,5,method='euclidean',iter.max = 40)
p1res2007<-Kmeans(table2007.features,5,method='pearson')
View(e1res2007$withinss)
Ecluster2007s2 <- Kmeans(table2007.features,5,method='euclidean',iter.max = 40)$cluster
Pcluster2007s2 <-Kmeans(table2007.features,5,method='pearson')$cluster
library(clusteval) #Using Jaccard
cluster_similarity(Ecluster2007s2 , Pcluster2007s2, similarity = "jaccard", method="independence")
#-----------------------------FOR YEAR 2008 Y3-------------------------------------------------
my2008$YEARMODA <- lubridate::ymd_h(my2008$YEARMODA)
my2008$Date <- as.Date(my2008$YEARMODA)
my2008$Time <- format(as.POSIXct(my2008$YEARMODA) ,format = "%H:%M:%S")
my2008$year <- year(ymd(my2008$Date))
my2008$month <- month(ymd(my2008$Date)) 
my2008$day <- day(ymd(my2008$Date))
library(tidyr)
library(naniar)
my2008$STP <- replace(my2008$STP, my2008$STP ==9999.9 , NA)
my2008$TEMP <- replace(my2008$TEMP, my2008$TEMP ==9999.9 , NA)
my2008$DEWP <- replace(my2008$DEWP, my2008$DEWP ==9999.9 , NA)
my2008$WDSP <- replace(my2008$WDSP, my2008$WDSP ==999.9 , NA)
#For Daily average calculation.
dtemp_avg2008<-aggregate(TEMP~STN...+day, data=my2008,mean,na.rm=TRUE, na.action=na.pass)
dstp_avg2008<-aggregate(STP~STN...+day, data=my2008,mean,na.rm=TRUE, na.action=na.pass)
ddewp_avg2008<-aggregate(DEWP~STN...+day, data=my2008,mean,na.rm=TRUE, na.action=na.pass)
dwdsp_avg2008<-aggregate(WDSP~STN...+day, data=my2008,mean,na.rm=TRUE, na.action=na.pass)
#For Monthly average temp. calculation
mtemp_avg2008<-aggregate(TEMP~STN..., data=dtemp_avg2008,mean,na.rm=TRUE, na.action=na.pass)
mstp_avg2008<-aggregate(STP~STN..., data=dstp_avg2008,mean,na.rm=TRUE, na.action=na.pass)
mdewp_avg2008<-aggregate(DEWP~STN..., data=ddewp_avg2008,mean,na.rm=TRUE, na.action=na.pass)
mwdsp_avg2008<-aggregate(WDSP~STN..., data=dwdsp_avg2008,mean,na.rm=TRUE, na.action=na.pass)
#For Clustering
# merge two data frames by ID
table2 <- merge(mtemp_avg2008,mstp_avg2008,by="STN...")
t4<- merge(table2,mdewp_avg2008,by="STN...")
table2008 <- merge(t4,mwdsp_avg2008,by="STN...") #this data frame will be used for clustering
set.seed(10)
table2008t = table2008
library(imputeTS)
table2008.features<-na_mean(table2008t)
library(amap)
library(clusteval)
set.seed(10)
eres2008<-Kmeans(table2008.features,5,method='euclidean',iter.max = 40)
pres2008<-Kmeans(table2008.features,5,method='pearson')
Ecluster2008s1 <- Kmeans(table2008.features,5,method='euclidean',iter.max = 40)$cluster
Pcluster2008s1 <-Kmeans(table2008.features,5,method='pearson')$cluster
cluster_similarity(Ecluster2008s1 , Pcluster2008s1, similarity = "jaccard", method="independence") #Using Jaccard
##With a different seed value
set.seed(150)
e1res2008<-Kmeans(table2008.features,5,method='euclidean',iter.max = 40)
p1res2008<-Kmeans(table2008.features,5,method='pearson')
View(e1res2008$cluster)
Ecluster2008s2 <- Kmeans(table2008.features,5,method='euclidean',iter.max = 40)$cluster
Pcluster2008s2 <-Kmeans(table2008.features,5,method='pearson')$cluster
library(clusteval) #Using Jaccard
cluster_similarity(Ecluster2008s2 , Pcluster2008s2, similarity = "jaccard", method="independence")
#!............................Y1, Y2 COMPARISON...........................................................!
#using set diff we find station number that is not present in column
setdiff(list.st2007, list.st2006)
setdiff(list.st2006, list.st2007)
Ytable2007 <-table2007
Ytable2006 <-table2006
Ytable2007  = filter(Ytable2007, !(STN... %in% c("997368","997729")))
Ytable2006  = filter(Ytable2006, !(STN... %in% c("722008")))
list.Ytable2006 <-unique(Ytable2006$STN...)
list.Ytable2007 <-unique(Ytable2007$STN...)
setdiff(list.Ytable2006, list.Ytable2007 )
Ytable2007.features<-na_mean(Ytable2007)
Ytable2006.features<-na_mean(Ytable2006)
comp2006 <- Ytable2006.features
comp2007 <- Ytable2007.features
set.seed(10)
Ecluster2007Y2 <- Kmeans(Ytable2007.features,5,method='euclidean',iter.max = 40)$cluster
Ecluster2006Y1 <- Kmeans(Ytable2006.features,5,method='euclidean',iter.max = 40)$cluster
set.seed(150)
Pcluster2007Y2 <- Kmeans(Ytable2007.features,5,method='pearson',iter.max = 40)$cluster
Pcluster2006Y1 <- Kmeans(Ytable2006.features,5,method='pearson',iter.max = 40)$cluster
library(tibble)
Pcomp2006 <- Ytable2006.features
Pcomp2007 <- Ytable2007.features
Pcomp2006$clusters <- as.factor(Pcluster2006Y1) #Preparing set for yearwise comparison
PComp2006<-within(comp2006, rm(TEMP,STP,WDSP,DEWP))
Pcomp2007$clusters <- as.factor(Pcluster2007Y2)
PComp2007<-within(comp2007, rm(TEMP,STP,WDSP,DEWP))
Pc1_2006 <- PComp2006[4:26,] #cluster1
Pc2_2006 <- PComp2006[143:151,] #cluster2
Pc3_2006 <- PComp2006[27:142,] #cluster3
Pc4_2006 <- PComp2006[2:3,] #cluster4
Pc5_2006 <- PComp2006[1,] #cluster5
list.Pc1_2006 <-Pc1_2006$STN...
list.Pc2_2006 <-Pc2_2006$STN...
list.Pc3_2006 <-Pc3_2006$STN...
list.Pc4_2006 <-Pc4_2006$STN...
list.Pc5_2006 <-Pc5_2006$STN...
list.Pc3_2006 <-Pc3_2006$STN...
Pc1_2007 <- PComp2007[143:151,] #cluster1
Pc2_2007 <- PComp2007[27:131,] #cluster2
Pc3_2007 <- PComp2007[140:142,] #cluster3
Pc4_2007 <- PComp2007[1:26,] #cluster4
Pc5_2007 <- PComp2007[132:139,] #cluster5
list.Pc1_2007 <-Pc1_2007$STN...
list.Pc2_2007 <-Pc2_2007$STN...
list.Pc3_2007 <-Pc3_2007$STN...
list.Pc4_2007 <-Pc4_2007$STN...
list.Pc5_2007 <-Pc5_2007$STN...
comp2006$clusters <- as.factor(Ecluster2006Y1) #Preparing set for yearwise comparison
Comp2006<-within(comp2006, rm(TEMP,STP,WDSP,DEWP))
comp2007$clusters <- as.factor(Ecluster2007Y2)
Comp2007<-within(comp2007, rm(TEMP,STP,WDSP,DEWP))
c1_2006 <- Comp2006[4:26,] #cluster1
c2_2006 <- Comp2006[143:151,] #cluster2
c3_2006 <- Comp2006[27:142,] #cluster3
c4_2006 <- Comp2006[2:3,] #cluster4
c5_2006 <- Comp2006[1,] #cluster5
list.c1_2006 <-c1_2006$STN...
list.c2_2006 <-c2_2006$STN...
list.c3_2006 <-c3_2006$STN...
list.c4_2006 <-c4_2006$STN...
list.c5_2006 <-c5_2006$STN...
list.c3_2006 <-c3_2006$STN...
c1_2007 <- Comp2007[143:151,] #cluster1
c2_2007 <- Comp2007[27:131,] #cluster2
c3_2007 <- Comp2007[140:142,] #cluster3
c4_2007 <- Comp2007[1:26,] #cluster4
c5_2007 <- Comp2007[132:139,] #cluster5
list.c1_2007 <-c1_2007$STN...
list.c2_2007 <-c2_2007$STN...
list.c3_2007 <-c3_2007$STN...
list.c4_2007 <-c4_2007$STN...
list.c5_2007 <-c5_2007$STN...
common <-length(intersect(list.Pc1_2006,list.Pc5_2007))
total <-length(union(list.Pc1_2006,list.Pc5_2007 ))
View(jaccard_val <- common/total)


#!............................Y2, Y3 COMPARISON...........................................................!
setdiff(list.st2007, list.st2008)
setdiff(list.st2008, list.st2007)
Y2table2007 <-table2007
Y2table2008 <-table2008
Y2table2007  = filter(Y2table2007, !(STN... %in% c("997729")))
Y2table2008  = filter(Y2table2008, !(STN... %in% c("720391", "720393", "720395", "720404", "722008", "722231", "722369", "722561", "998165")))
list.Y2table2007 <-unique(Y2table2007$STN...)
list.Y2table2008 <-unique(Y2table2007$STN...)
setdiff(list.Y2table2007, list.Y2table2008 )
Y2table2007.features<-na_mean(Y2table2007)
Y2table2008.features<-na_mean(Y2table2008)
comp2008 <- Y2table2008.features
Pcomp2008 <- Y2table2008.features
c2omp2007 <- Y2table2007.features
set.seed(10)
E2cluster2007Y2 <- Kmeans(Y2table2007.features,5,method='euclidean',iter.max = 40)$cluster
Ecluster2008Y3 <- Kmeans(Y2table2008.features,5,method='euclidean',iter.max = 40)$cluster
set.seed(150)
P2cluster2007Y2 <- Kmeans(Y2table2007.features,5,method='euclidean',iter.max = 40)$cluster
Pcluster2008Y3 <- Kmeans(Y2table2008.features,5,method='euclidean',iter.max = 40)$cluster
library(tibble)
Pcomp2008$clusters <- as.factor(Pcluster2008Y3) #Preparing set for yearwise comparison
PComp2008<-within(comp2008, rm(TEMP,STP,WDSP,DEWP))
Pc2omp2007$clusters <- as.factor(P2cluster2007Y2)
PC2omp2007<-within(Pc2omp2007, rm(TEMP,STP,WDSP,DEWP))
Pc1_2008 <- PComp2008[4:26,] #cluster1
Pc2_2008 <- PComp2008[143:152,] #cluster2
Pc3_2008 <- PComp2008[27:142,] #cluster3
Pc4_2008 <- PComp2008[2:3,] #cluster4
Pc5_2008 <- PComp2008[1,] #cluster5
list.Pc1_2008 <-Pc1_2008$STN...
list.Pc2_2008 <-Pc2_2008$STN...
list.Pc3_2008 <-Pc3_2008$STN...
list.Pc4_2008 <-Pc4_2008$STN...
list.Pc5_2008 <-Pc5_2008$STN...
comp2008$clusters <- as.factor(Ecluster2008Y3) #Preparing set for yearwise comparison
Comp2008<-within(comp2008, rm(TEMP,STP,WDSP,DEWP))
c2omp2007$clusters <- as.factor(E2cluster2007Y2)
C2omp2007<-within(c2omp2007, rm(TEMP,STP,WDSP,DEWP))
c1_2008 <- Comp2008[132:139,] #cluster1
c2_2008 <- Comp2008[143:152,] #cluster2
c3_2008 <- Comp2008[140:142,] #cluster3
c4_2008 <- Comp2008[1:26,] #cluster4
c5_2008 <- Comp2008[27,131] #cluster5
list.c1_2008 <-c1_2008$STN...
list.c2_2008 <-c2_2008$STN...
list.c3_2008 <-c3_2008$STN...
list.c4_2008 <-c4_2008$STN...
list.c5_2008 <-c5_2008$STN...
c1_2007 <- C2omp2007[143:152,] #cluster1
c2_2007 <- C2omp2007[27:131,] #cluster2
c3_2007 <- C2omp2007[140:142,] #cluster3
c4_2007 <- C2omp2007[1:26,] #cluster4
c5_2007 <- C2omp2007[132:139,] #cluster5
list.c1_2007 <-c1_2007$STN...
list.c2_2007 <-c2_2007$STN...
list.c3_2007 <-c3_2007$STN...
list.c4_2007 <-c4_2007$STN...
list.c5_2007 <-c5_2007$STN...
common <-length(intersect(list.Pc5_2006,list.Pc5_2008))
total <-length(union(list.Pc5_2006,list.Pc5_2008))
View(jaccard_val <- common/total)
#!----------------------------Y1,Y2,Y3 COMPARISON...............................................
#for pearson comparisons

##############***************** PLOTTING **********************############################
ptestdata2006 <- table2006.features
list.ptestdata2006<-unique(ptestdata2006$STN...)
ptestdata2006$clusters <- as.factor(Ecluster2006s1)
map_location <- stations
library(data.table)
uniquemap_loc <-unique(setDT(map_location), by = "StationNumber")
list.uniquemap_loc<-unique(uniquemap_loc$StationNumber)
setdiff(list.uniquemap_loc, list.ptestdata2006)
map_loc  = filter(uniquemap_loc, !(StationNumber %in% c("720150", "720391", "720393" ,"720395" ,"720396", "720403", "720542", "720579", "720577", "720578",
  "720584", "720594", "720617", "720622" ,"720626", "720637", "720648", "720659" ,"720733" ,"720742"
, "720743" ,"720771", "720779" ,"720926" ,"720965" ,"721032" ,"721044" ,"721048", "722102" ,"722231"
  ,"722422" ,"722423", "722433" ,"722518" ,"722521", "722525" ,"722545", "722546" ,"722556" ,"722572",
  "722581" ,"722585" ,"722586", "722626" ,"722635", "722655" ,"722657", "722665" ,"722658" ,"722673",
  "722675" ,"722707" ,"723348" ,"723513", "723601", "725021", "727511" ,"747300", "747350", "749016",
  "749017", "749018", "749019", "749055", "749056" ,"749057", "749058", "749091" ,"749155" ,"749159",
  "749161" ,"749160", "749162" ,"749488" ,"994730" ,"997362", "722415", "722425" ,"722519" ,"690290",
 "722413" ,"722437", "722426" ,"722507", "722557", "998226" ,"690280" ,"722369", "722414", "994510",
  "998165" ,"998174" ,"998181" ,"720313", "720404" ,"998490" ,"997368", "998189" ,"998482" ,"994790",
"998182" ,"998183", "998184", "998185", "998481" ,"998483", "998487" ,"998225" ,"998236" ,"721043",
"722561", "722704", "720639" ,"720647" ,"720943", "721034", "722580" ,"723660", "747360", "747390",
 "747460", "A00002" ,"A00019" ,"A05735", "722440" ,"A00008")))
list.map_loc<-unique(map_loc$StationNumber)
setdiff(list.map_loc,list.ptestdata2006)
ptestdata2006 = filter(ptestdata2006, !(STN... %in% c("997363")))
library(tibble)
map_loc["TEMP"] <- tibble(TEMP=c(ptestdata2006$TEMP))
map_loc["WDSP"] <- tibble(WDSP=c(ptestdata2006$WDSP))
map_loc["STP"] <- tibble(STP=c(ptestdata2006$STP))
map_loc["DEWP"] <- tibble(DEWP=c(ptestdata2006$DEWP))
map_loc["clusters"] <- tibble(clusters=c(ptestdata2006$clusters))
library(ggmap)
library(ggplot2)
register_google(key = "AIzaSyBBbM_LMT1CxQzWTercmeA_uV1HCbu_hlo")
TXmap <- get_map("Texas",zoom=6, scale="auto",source = "google")
ggmap(TXmap)+geom_point(aes(x=Lon[],y=Lat[],colour= as.factor(clusters)),data=map_loc)+ggtitle("TX region 5 KMeansYear 2006 ")
#for Pearson
p2testdata2006 <- table2006.features
list.p2testdata2006<-unique(p2testdata2006$STN...)
p2testdata2006$clusters <- as.factor(Pcluster2006s2)
list.p2testdata2006<-unique(p2testdata2006$STN...)
p2testdata2006 = filter(p2testdata2006, !(STN... %in% c("997363")))
list.p2testdata2006<-unique(p2testdata2006$STN...)
setdiff(list.p2testdata2006,list.map_loc)
library(tibble)
Pmap_loc =map_loc
Pmap_loc["TEMP"] <- tibble(TEMP=c(p2testdata2006$TEMP))
Pmap_loc["WDSP"] <- tibble(WDSP=c(p2testdata2006$WDSP))
Pmap_loc["STP"] <- tibble(STP=c(p2testdata2006$STP))
Pmap_loc["DEWP"] <- tibble(DEWP=c(p2testdata2006$DEWP))
Pmap_loc["clusters"] <- tibble(clusters=c(p2testdata2006$clusters))
library(ggmap)
library(ggplot2)
register_google(key = "AIzaSyBBbM_LMT1CxQzWTercmeA_uV1HCbu_hlo")
TXmap <- get_map("Texas",zoom=6, scale="auto",source = "google")
ggmap(TXmap)+geom_point(aes(x=Lon[],y=Lat[],colour= as.factor(clusters)),data=Pmap_loc)+ggtitle("TX region 5 KMeans  2006(Pearson)")
########YEAR 2007 PLOTTING##############################
ptestdata2007 <- table2007.features
list.ptestdata2007<-unique(ptestdata2007$STN...)
ptestdata2007$clusters <- as.factor(Ecluster2007s1)
setdiff(list.uniquemap_loc, list.ptestdata2007)
map_loc2 = filter(uniquemap_loc, !(StationNumber %in% c( "720150", "720391" ,"720393", "720395" ,"720396" ,"720403" ,"720542", "720579",
                                                        "720577" ,"720578" ,"720584" ,"720594" ,"720617" ,"720622" ,"720626" ,"720637",
                                                        "720648" ,"720659" ,"720733" ,"720742" ,"720743" ,"720771" ,"720779" ,"720926",
                                                        "720965", "721032" ,"721044" ,"721048" ,"722102" ,"722231" ,"722422" ,"722423",
                                                        "722433" ,"722518" ,"722521" ,"722525" ,"722545" ,"722546" ,"722556" ,"722572",
                                                         "722581" ,"722585" ,"722586" ,"722626" ,"722635", "722655" ,"722657" ,"722665",
                                                         "722658", "722673" ,"722675" ,"722707" ,"723348" ,"723513", "723601" ,"725021",
                                                        "727511" ,"747300" ,"747350" ,"749016" ,"749017" ,"749018" ,"749019" ,"749055",
                                                        "749056" ,"749057" ,"749058" ,"749091" ,"749155" ,"749159" ,"749161" ,"749160",
                                                         "749162", "749488" ,"994730" ,"997362" ,"722415" ,"722425" ,"722519" ,"690290",
                                                         "722413" ,"722437" ,"722426" ,"722507" ,"722557" ,"998226", "690280" ,"722369",
                                                        "722414" ,"994510" ,"998165", "998174" ,"998181" ,"720313" ,"722008" ,"720404",
                                                        "998490" ,"998189" ,"998482" ,"994790" ,"998182" ,"998183" ,"998184" ,"998185",
                                                         "998481" ,"998483" ,"998487" ,"998225" ,"998236" ,"721043", "722561", "722704",
                                                        "720639", "720647" ,"720943" ,"721034" ,"722580" ,"723660", "747360" ,"747390",
                                                        "747460" ,"A00002", "A00019" ,"A05735", "722440" ,"A00008")))
list.map_loc2<-unique(map_loc2$StationNumber)
setdiff(list.ptestdata2007,list.map_loc2)
setdiff(list.map_loc2,list.ptestdata2007)
ptestdata2007 = filter(ptestdata2007, !(STN... %in% c("997363","997729")))
library(tibble)
map_loc2["TEMP"] <- tibble(TEMP=c(ptestdata2007$TEMP))
map_loc2["WDSP"] <- tibble(WDSP=c(ptestdata2007$WDSP))
map_loc2["STP"] <- tibble(STP=c(ptestdata2007$STP))
map_loc2["DEWP"] <- tibble(DEWP=c(ptestdata2007$DEWP))
map_loc2["clusters"] <- tibble(clusters=c(ptestdata2007$clusters))
library(ggmap)
library(ggplot2)
register_google(key = "AIzaSyBBbM_LMT1CxQzWTercmeA_uV1HCbu_hlo")
TXmap <- get_map("Texas",zoom=6, scale="auto",source = "google")
ggmap(TXmap)+geom_point(aes(x=Lon[],y=Lat[],colour= as.factor(clusters)),data=map_loc2)+ggtitle("TX region 5 KMeans 2007 ")
#FOR PEARSON 2007
p2testdata2007 <- table2007.features
list.p2testdata2007<-unique(p2testdata2007$STN...)
p2testdata2007$clusters <- as.factor(Pcluster2007s2)
list.p2testdata2007<-unique(p2testdata2007$STN...)
p2testdata2007 = filter(p2testdata2007, !(STN... %in% c("997363","997729")))
list.p2testdata2007<-unique(p2testdata2007$STN...)
setdiff(list.p2testdata2007,list.map_loc2)
library(tibble)
Pmap_loc2 =map_loc2
Pmap_loc2["TEMP"] <- tibble(TEMP=c(p2testdata2007$TEMP))
Pmap_loc2["WDSP"] <- tibble(WDSP=c(p2testdata2007$WDSP))
Pmap_loc2["STP"] <- tibble(STP=c(p2testdata2007$STP))
Pmap_loc2["DEWP"] <- tibble(DEWP=c(p2testdata2007$DEWP))
Pmap_loc2["clusters"] <- tibble(clusters=c(p2testdata2007$clusters))
library(ggmap)
library(ggplot2)
register_google(key = "AIzaSyBBbM_LMT1CxQzWTercmeA_uV1HCbu_hlo")
TXmap <- get_map("Texas",zoom=6, scale="auto",source = "google")
ggmap(TXmap)+geom_point(aes(x=Lon[],y=Lat[],colour= as.factor(clusters)),data=Pmap_loc2)+ggtitle("TX region 5 KMeans 2007(Pearson)")
##-------------------------YEAR 2008----------------------------------------__################
ptestdata2008 <- table2008.features
list.ptestdata2008<-unique(ptestdata2008$STN...)
ptestdata2008$clusters <- as.factor(Ecluster2008s1)
setdiff(list.uniquemap_loc, list.ptestdata2008)
map_loc3 = filter(uniquemap_loc, !(StationNumber %in% c(  "720150", "720396", "720403", "720542" ,"720579", "720577" ,"720578" ,"720584" ,"720594",
                                                          "720617" ,"720622", "720626", "720637" ,"720648" ,"720659" ,"720733" ,"720742" ,"720743",
                                                          "720771" ,"720779", "720926" ,"720965", "721032" ,"721044" ,"721048" ,"722102" ,"722422",
                                                          "722423" ,"722433", "722518", "722521", "722525" ,"722545" ,"722546" ,"722556" ,"722572",
                                                          "722581" ,"722585", "722586", "722626", "722635", "722655" ,"722657", "722665" ,"722658",
                                                          "722673" ,"722675", "722707" ,"723348" ,"723513" ,"723601" ,"725021", "727511" ,"747300",
                                                          "747350" ,"749016", "749017", "749018" ,"749019" ,"749055" ,"749056", "749057" ,"749058",
                                                          "749091" ,"749155" ,"749159" ,"749161" ,"749160" ,"749162" ,"749488", "994730" ,"997362",
                                                         "722415" ,"722425", "722519" ,"690290" ,"722413" ,"722437" ,"722426" ,"722507" ,"722557",
                                                          "998226", "690280", "722414", "994510" ,"998174", "998181", "720313" ,"998490" ,"998189",
                                                          "998482" ,"994790", "998182" ,"998183" ,"998184" ,"998185", "998481" ,"998483", "998487",
                                                          "998225" ,"998236", "721043" ,"722704" ,"720639" ,"720647", "720943" ,"721034" ,"722580",
                                                          "723660", "747360", "747390" ,"747460" ,"A00002" ,"A00019", "A05735" ,"722440" ,"A00008")))
list.map_loc3<-unique(map_loc3$StationNumber)
setdiff(list.ptestdata2008,list.map_loc3)
setdiff(list.map_loc3,list.ptestdata2008)
ptestdata2008 = filter(ptestdata2008, !(STN... %in% c("997363")))
library(tibble)
map_loc3["TEMP"] <- tibble(TEMP=c(ptestdata2008$TEMP))
map_loc3["WDSP"] <- tibble(WDSP=c(ptestdata2008$WDSP))
map_loc3["STP"] <- tibble(STP=c(ptestdata2008$STP))
map_loc3["DEWP"] <- tibble(DEWP=c(ptestdata2008$DEWP))
map_loc3["clusters"] <- tibble(clusters=c(ptestdata2008$clusters))
library(ggmap)
library(ggplot2)
register_google(key = "AIzaSyBBbM_LMT1CxQzWTercmeA_uV1HCbu_hlo")
TXmap <- get_map("Texas",zoom=6, scale="auto",source = "google")
ggmap(TXmap)+geom_point(aes(x=Lon[],y=Lat[],colour= as.factor(clusters)),data=map_loc3)+ggtitle("TX region 5 KMeans 2008 ")
#FOR PEARSON 2008
p2testdata2008 <- table2008.features
list.p2testdata2008<-unique(p2testdata2008$STN...)
p2testdata2008$clusters <- as.factor(Pcluster2008s2)
list.p2testdata2008<-unique(p2testdata2008$STN...)
p2testdata2008 = filter(p2testdata2008, !(STN... %in% c("997363")))
list.p2testdata2008<-unique(p2testdata2008$STN...)
setdiff(list.p2testdata2008,list.map_loc3)
library(tibble)
Pmap_loc3 =map_loc3
Pmap_loc3["TEMP"] <- tibble(TEMP=c(p2testdata2008$TEMP))
Pmap_loc3["WDSP"] <- tibble(WDSP=c(p2testdata2008$WDSP))
Pmap_loc3["STP"] <- tibble(STP=c(p2testdata2008$STP))
Pmap_loc3["DEWP"] <- tibble(DEWP=c(p2testdata2008$DEWP))
Pmap_loc3["clusters"] <- tibble(clusters=c(p2testdata2008$clusters))
library(ggmap)
library(ggplot2)
register_google(key = "AIzaSyBBbM_LMT1CxQzWTercmeA_uV1HCbu_hlo")
TXmap <- get_map("Texas",zoom=6, scale="auto",source = "google")
ggmap(TXmap)+geom_point(aes(x=Lon[],y=Lat[],colour= as.factor(clusters)),data=Pmap_loc3)+ggtitle("TX region 5 KMeans 2008(Pearson)")
