library(readxl)
ANZ_synthesised_transaction_dataset <- read_excel("Downloads/ANZ synthesised transaction dataset.xlsx", 
                                                    +     col_types = c("text", "numeric", "numeric", 
                                                                        +         "text", "text", "text", "text", "text", 
                                                                        +         "text", "text", "numeric", "numeric", 
                                                                        +         "text", "numeric", "text", "text", 
                                                                        +         "text", "numeric", "text", "text", 
                                                                        +         "text", "text", "text"))

ANZ_synthesised_transaction_dataset<-ANZ_synthesised_transaction_dataset[,-3]

ANZ_synthesised_transaction_dataset<-ANZ_synthesised_transaction_dataset[,-4]

library(dplyr)
library(tidyr)

#splitting long and lat into two seperate columns
ANZ_synthesised_transaction_dataset<-ANZ_synthesised_transaction_dataset%>%separate(long_lat,c("longtitude","latitude"),sep = " ")


ANZ_synthesised_transaction_dataset<-ANZ_synthesised_transaction_dataset[,-8]



library(lubridate)

ANZ_synthesised_transaction_dataset<-ANZ_synthesised_transaction_dataset%>%separate(extraction,c("date","time"),sep = "T")
ANZ_synthesised_transaction_dataset$time<-substr(ANZ_synthesised_transaction_dataset$time,1,2)


ANZ_synthesised_transaction_dataset$time<-as.numeric(ANZ_synthesised_transaction_dataset$time)
ANZ_synthesised_transaction_dataset$time<-ANZ_synthesised_transaction_dataset$time+1


ANZ_synthesised_transaction_dataset$date<-as.Date(ANZ_synthesised_transaction_dataset$date)

ANZ_synthesised_transaction_dataset$longtitude<-as.numeric(ANZ_synthesised_transaction_dataset$longtitude)
ANZ_synthesised_transaction_dataset$latitude<-as.numeric(ANZ_synthesised_transaction_dataset$latitude)

#NA values in many columns are present for the status "posted"

boxplot(ANZ_synthesised_transaction_dataset$amount)
summary(ANZ_synthesised_transaction_dataset$amount)
#we can see there is a lot of outliers and most of the transaction amount tends to be around 29


ANZ_synthesised_transaction_dataset$day<-day(ANZ_synthesised_transaction_dataset$date)
ANZ_synthesised_transaction_dataset$month<-month(ANZ_synthesised_transaction_dataset$date)


ANZ_synthesised_transaction_dataset$age<-ifelse(ANZ_synthesised_transaction_dataset$age<39,"18-38",
                                                ifelse(ANZ_synthesised_transaction_dataset$age<59,"39-58","59-78"))


ANZ_synthesised_transaction_dataset<-ANZ_synthesised_transaction_dataset%>%separate(merchant_long_lat,c("longtitude_merch","latitude_merch"),sep = " ")


#average distance between the customer and merchant 

library(geosphere)

s<-subset(ANZ_synthesised_transaction_dataset,status=="authorized")

long_pick<-s$longtitude
lat_pick<-s$latitude
p1<-matrix(c(long_pick,lat_pick),ncol=2)


long_drop<-s$longtitude_merch
lat_drop<-s$latitude_merch
p2<-matrix(c(long_drop,lat_drop),ncol=2)

distance<-distHaversine(p1,p2)/1000    #in km


#there is one customers whose location is outside australia 
#this is an error ( latitude=-573)



#Finding correlation ,features and building a model



Q<-quantile(ANZ_synthesised_transaction_dataset$amount, probs=c(.25, .75))
iqr <- IQR(ANZ_synthesised_transaction_dataset$amount)
new<- ANZ_synthesised_transaction_dataset%>% filter(amount > (Q[1] - 1.5*iqr) &  +amount< (Q[2] + 1.5*iqr))


library(ggplot2)

ggplot(new,aes(age,amount))+geom_boxplot()  #the average transaction amount is roughly the same


new<-new[,-3]
new<-new[,-6]
new<-new[,-15]
new<-new[,-c(13,15,16)]
new<-new[,-6]
new<-new[,-6]


ggplot(new,aes(balance,amount))+geom_point()   #no relation with the balance

new<-new[-which(new$latitude==-573),]  #removing all the points outside australia

new$latitude_merch<-as.numeric(new$latitude_merch)
new$longtitude_merch<-as.numeric(new$longtitude_merch)


library(geosphere)

long_pick<-new$longtitude
lat_pick<-new$latitude
p1<-matrix(c(long_pick,lat_pick),ncol=2)


long_drop<-new$longtitude_merch
lat_drop<-new$latitude_merch
p2<-matrix(c(long_drop,lat_drop),ncol=2)

distance<-distHaversine(p1,p2)/1000

new2<-cbind(new,distance)


new2<-new2[,-c(3,4)]
new2<-new2[,-c(10,11)]
new2<-new2[,-10]


new2<-new2%>%mutate_if(is.character,as.factor)
new2$card_present_flag<-as.factor(new2$card_present_flag)
new2$day<-as.numeric(new2$day)


library(corrplot)

corr_features <-new2[,c(8,9,10,11,12)]                                 #only numeric
corrplot(cor(corr_features, use='complete.obs'), type='lower')

library(caret)

#due to presence of missing valuse when the status is posted ,we neglet it for this model prediction


new3<-subset(new2,status=="authorized")

intrain<-createDataPartition(y=new3$amount,p=0.7,list = FALSE)

train<-new3[intrain,]
test<-new3[-intrain,]

train.control<-trainControl(method="cv",number =5)


model<-train(amount~.,data=train,method="rpart",trControl=train.control)


pred<-predict(model,data=test)


check<-cbind(test$amount,pred)

library(DMwR)
regr.eval(test$amount,pred)

#mae=32
