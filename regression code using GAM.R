library(dplyr)
library(lubridate)
library(data.table)
library(zoo)
library(reshape2)
library(stringr)
library(timeDate)
library(xts)
library(readxl)
library(car)
library(lmtest)
library(normtest)
library(corrplot)
library(tree)
library(ISLR)
library(randomForest)
library(adabag)
install.packages("dplyr")

mydata <- read.csv("P1.csv")
library(caTools)

str(mydata)
library(gam)
mydata$No..of.Adults7<-as.numeric(mydata$No..of.Adults==7)
mydata$No..of.Adults2<-as.numeric(mydata$No..of.Adults==2)
mydata$No..of.Adults3<-as.numeric(mydata$No..of.Adults==3)
mydata$No..of.Adults4<-as.numeric(mydata$No..of.Adults==4)
mydata$No..of.Adults5<-as.numeric(mydata$No..of.Adults==5)
mydata$No..of.Adults6<-as.numeric(mydata$No..of.Adults==6)
mydata$No..of.Children0<-as.numeric(mydata$No..of.Children==0)
mydata$No..of.Children1<-as.numeric(mydata$No..of.Children==1)
mydata$No..of.Children2<-as.numeric(mydata$No..of.Children==2)
mydata$No..of.Infants1<-as.numeric(mydata$No..of.Infants==1)
mydata$Type.of.Journey..O...one.way.R...Return.1<-as.numeric(mydata$Type.of.Journey..O...one.way.R...Return.=="R")
mydata$Payment.Amount1<-mydata$Payment.Amount^1.918
split <- sample.split(mydata$No..of.days.prior.to.departure, SplitRatio=0.7)
train <- mydata[split==T,]
test <- mydata[split==F,]
split1<-sample.split(test$No..of.days.prior.to.departure,SplitRatio = 2/3)
valid<-test[split1==T,]
test1 <- test[split1==F,]
train$Payment.Amount1<-train$Payment.Amount^1.905


install.packages("mgcv")
library(mgcv)
modelgam<-gam(No..of.days.prior.to.departure ~ No..of.Adults7+No..of.Adults2+No..of.Adults3+No..of.Adults4+No..of.Adults5+No..of.Adults6+No..of.Children0+No..of.Children1+Type.of.Journey..O...one.way.R...Return.1+No..of.Infants1+s(Payment.Amount), data=train)
summary(modelgam)
plot.gam(modelgam)
model<-lm(No..of.days.prior.to.departure ~ No..of.Adults7+No..of.Adults2+No..of.Adults3+No..of.Adults4+No..of.Adults5+No..of.Adults6+No..of.Children0+No..of.Children1+Type.of.Journey..O...one.way.R...Return.1+No..of.Infants+Payment.Amount1, data=train)
summary(model)
vif(model)
plot(model, which=c(4)) ##infleuential observation is not there
#################
model<-lm(No..of.days.prior.to.departure ~ No..of.Adults7+No..of.Adults2+No..of.Adults4+No..of.Children0+No..of.Infants, data=train)
summary(model)
######################validation step
model<-lm(No..of.days.prior.to.departure ~ No..of.Adults7+No..of.Adults2+No..of.Adults4+No..of.Children0+No..of.Infants, data=valid)
summary(model)
###################testing
test1$Payment.Amount1<-test1$Payment.Amount^1.905
predict1<-predict(model,test1)
mape<-mean((abs(predict1-test1$No..of.days.prior.to.departure)/test1$No..of.days.prior.to.departure))*100


##################decision tree
split <- sample.split(mydata$No..of.days.prior.to.departure, SplitRatio=0.9)
train <- mydata[split==T,]
test <- mydata[split==F,]
treeclass <- tree(No..of.days.prior.to.departure ~ No..of.Adults + No..of.Children+Type.of.Journey..O...one.way.R...Return.+No..of.Infants+Payment.Amount, data=train)
summary(treeclass)
cv.x<-cv.tree(treeclass)
plot(cv.x$size ,cv.x$dev ,type='b')
prune.new =prune.tree(treeclass ,best =5)
plot(treeclass)
text(treeclass,pretty = 0)
treeclass

predict1 = predict(prune.new,test1)
mape<-mean((abs(predict1-test1$No..of.days.prior.to.departure)/test1$No..of.days.prior.to.departure))*100

####################randomforest 
rf.mydata1 = randomForest(No..of.days.prior.to.departure ~ No..of.Adults + No..of.Children+Type.of.Journey..O...one.way.R...Return.+No..of.Infants+Payment.Amount, data=train,mtry =2, importance = TRUE,na.action=na.roughfix)
#randomforest - testing
yhat.rf1 = predict(rf.mydata1, newdata=test)
mape<-mean((abs(yhat.rf1-test1$No..of.days.prior.to.departure)/test1$No..of.days.prior.to.departure))*100



