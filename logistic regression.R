mydata<-read.csv("P3.csv")
mydata$History1<-as.numeric(mydata$History==1)
mydata$History2<-as.numeric(mydata$History==2)
mydata$History3<-as.numeric(mydata$History==3)
mydata$History4<-as.numeric(mydata$History==4)
mydata$Saving.Acct1<-as.numeric(mydata$Saving.Acct==1)
mydata$Saving.Acct2<-as.numeric(mydata$Saving.Acct==2)
mydata$Saving.Acct3<-as.numeric(mydata$Saving.Acct==3)
mydata$Saving.Acct4<-as.numeric(mydata$Saving.Acct==4)
mydata$Employment1<-as.numeric(mydata$Employment==1)
mydata$Employment2<-as.numeric(mydata$Employment==2)
mydata$Employment3<-as.numeric(mydata$Employment==3)
mydata$Employment4<-as.numeric(mydata$Employment==4)
mydata$Resident1<-as.numeric(mydata$Resident==1)
mydata$Resident2<-as.numeric(mydata$Resident==2)
mydata$Resident3<-as.numeric(mydata$Resident==3)

mydata$Job0<-as.numeric(mydata$Job==0)
mydata$Job1<-as.numeric(mydata$Job==1)
mydata$Job2<-as.numeric(mydata$Job==2)




str(mydata)
train<-mydata[c(seq(1:175)),]
valid<-mydata[c(seq(176:250)),]
test<-mydata[c(seq(251:300)),]

train1<-train[-48,]

model1<-glm(Response~ Duration+History1+History3+History4+Education+Amount.of.credit+ Saving.Acct1+Saving.Acct2+Saving.Acct3+Saving.Acct4+Employment1+Employment3+Employment4+Install_rate+Male_married+Gurantor+Resident1+Resident2+Resident3+Real.Estate+Age+Other.installment+Rent+Job0+Job1+Job2+Number.of.Dependents,family = "binomial",data = train1)
vif(model1)
plot(model1,which = c(4))

summary(model1)
plot(model1$residuals)
#################removing insifnicant variable
model1<-glm(Response~ Duration+History4+Amount.of.credit+Employment3+Install_rate+Resident3,family = "binomial",data = train1)
summary(model1)
########################
predict1 <- predict(model1,newdata=valid,type = 'response')
table(valid$Response, predict1>0.1)
table(valid$Response, predict1>0.2)
table(valid$Response, predict1>0.3)
table(valid$Response, predict1>0.4)
table(valid$Response, predict1>0.5)
table(valid$Response, predict1>0.6)
table(valid$Response, predict1>0.7)
table(valid$Response, predict1>0.8)
table(valid$Response, predict1>0.9)
################
predict2 <- predict(model1,newdata=test,type = 'response')
table(test$Response, predict2>0.5)
error<-(8/50)*100
###################
mydata<-read.csv("P3.csv")
colnames(mydata)[5] <- "Amount_of_credit"
colnames(mydata)[6] <- "Saving_Acct"
colnames(mydata)[12] <- "Real_Estate"
colnames(mydata)[14] <- "Other_installment"
colnames(mydata)[17] <- "Number_of_Dependents"
mydata$Response<-as.factor(mydata$Response)

traindec<-mydata[c(1:250),]
testdec<-mydata[c(251:300),]
library(tree)
library(MASS)
treeclass <- tree(Response~ .,data = traindec)
summary(treeclass)
plot(treeclass)
treeclass.pred = predict(treeclass,testdec, type="class")
table(treeclass.pred, testdec$Response)
str(mydata)
###########################SVM
mydata<-read.csv("P3.csv")
mydata$History1<-as.numeric(mydata$History==1)
mydata$History2<-as.numeric(mydata$History==2)
mydata$History3<-as.numeric(mydata$History==3)
mydata$History4<-as.numeric(mydata$History==4)
mydata$Saving.Acct1<-as.numeric(mydata$Saving.Acct==1)
mydata$Saving.Acct2<-as.numeric(mydata$Saving.Acct==2)
mydata$Saving.Acct3<-as.numeric(mydata$Saving.Acct==3)
mydata$Saving.Acct4<-as.numeric(mydata$Saving.Acct==4)
mydata$Employment1<-as.numeric(mydata$Employment==1)
mydata$Employment2<-as.numeric(mydata$Employment==2)
mydata$Employment3<-as.numeric(mydata$Employment==3)
mydata$Employment4<-as.numeric(mydata$Employment==4)
mydata$Resident1<-as.numeric(mydata$Resident==1)
mydata$Resident2<-as.numeric(mydata$Resident==2)
mydata$Resident3<-as.numeric(mydata$Resident==3)

mydata$Job0<-as.numeric(mydata$Job==0)
mydata$Job1<-as.numeric(mydata$Job==1)
mydata$Job2<-as.numeric(mydata$Job==2)

library (e1071)

train <-mydata[c(1:250),]
test<-mydata[c(251:300),]

svmmodel = svm(Response~ Duration+History1+History3+History4+Education+Amount.of.credit+ Saving.Acct1+Saving.Acct2+Saving.Acct3+Saving.Acct4+Employment1+Employment3+Employment4+Install_rate+Male_married+Gurantor+Resident1+Resident2+Resident3+Real.Estate+Age+Other.installment+Rent+Job0+Job1+Job2+Number.of.Dependents,data = train, 
             kernel='linear', cost=100,scale=FALSE)
plot(svmmodel,train)
summary(svmmodel)
tune.out<-tune(svm,Response~ .,data = traindec, 
               kernel='linear',ranges = list(cost=c(0.001,0.01,0.1,1.5,10,50,100,150)))
bestmodel=tune.out$best.model
summary(bestmodel)
pred<-predict(bestmodel,testdec)
table(pred,testdec$Response)
print(svm_tune)
############
tune.out<-tune(svm,Response~ .,data = traindec, 
               kernel='polynomial',ranges = list(cost=c(0.001,0.01,0.1,1.5,10,50,100,150)))
bestmodel=tune.out$best.model
summary(svmmodel)
pred<-predict(bestmodel,testdec)
table(pred,testdec$Response)
#############
tune.out<-tune(svm,Response~ .,data = traindec, 
               kernel='radial',ranges = list(cost=c(0.001,0.01,0.1,1.5,10,50,100,150)))
bestmodel=tune.out$best.model
summary(svmmodel)
pred<-predict(bestmodel,testdec)
table(pred,testdec$Response)

