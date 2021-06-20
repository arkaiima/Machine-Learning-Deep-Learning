mydata<-read.csv("P2.csv")
library(forecast)
library(tseries)
library(seasonal)
library(sarima)
library(astsa)

str(mydata)
data_bangladesh<-ts(mydata$Bangladesh,start = c(1991),frequency = 1)
adf.test(data_bangladesh)
adf.test(diff(data_bangladesh))
adf.test(diff(diff(data_bangladesh)))
acf(data_bangladesh, lag.max = 26)
pacf(data_bangladesh, lag.max = 26)

bangla <- ts(mydata$Bangladesh,start = c(1991),frequency = 1)
decomp = stl(bangla, s.window=4)
plot(data_bangladesh)
model_Bangla <- auto.arima(data_bangladesh, ic="aic", trace=TRUE) ####69.54
summary(model_Bangla)

#################################finding best model

i<-1
z<-c()
for (p in 0:3){
  for (d in 0:2){
    for (q in 0:4){
x<-arima(data_bangladesh, c(p, d, q))
y<-summary(arima(data_bangladesh, c(p, d, q)))
z<-c(z,y[1,5])
    }}}

outputmatrix <- matrix(ncol=5, nrow=60)
i <- 1
for (p in 0:3){
  for (d in 0:2){
    for (q in 0:4){
      outputmatrix[i,] <-  c(p, d, q, arima(data_bangladesh, c(p, d, q))$aic,z[i])
      i=i+1
    }
  }
}
#BEST MODEL IS ARIMA(0,1,3), ARIMA(0,1,2),ARIMA(0,2,3), ARIMA(0,2,4)
model_best_013 <- arima(data_bangladesh, c(0, 1, 3))
model_best_012 <- arima(data_bangladesh, c(0, 1, 2))
model_best_023<- arima(data_bangladesh, c(0, 2, 3))
model_best_024<- arima(data_bangladesh, c(0, 2, 4))
data_bangladesh_013 <- forecast(model_best_013, h=1)
data_bangladesh_012 <- forecast(model_best_012, h=1)
data_bangladesh_023 <- forecast(model_best_023, h=1)
data_bangladesh_024 <- forecast(model_best_024, h=1)
mape_wt1 <- 1/summary(model_best_013)[1,5]
mape_wt2 <- 1/summary(model_best_012)[1,5]
mape_wt3 <- 1/summary(model_best_023)[1,5]
mape_wt4<- 1/summary(model_best_024)[1,5]
totalwt<-mape_wt1+mape_wt2+mape_wt3+mape_wt4
w1<-mape_wt1/totalwt
w2<-mape_wt2/totalwt
w3<-mape_wt3/totalwt
w4<-mape_wt4/totalwt
assemble_forecast<-(w1*data_bangladesh_013$mean)+(w2*data_bangladesh_013$mean)+(w3*data_bangladesh_023$mean)+(w4*data_bangladesh_024$mean)

###################(b)
data_pak<-ts(mydata$Pakistan,start = c(1991),frequency = 1)
adf.test(data_pak)
adf.test(diff(data_pak))
adf.test(diff(diff(data_pak)))
acf(data_pak, lag.max = 26) 
pacf(data_pak, lag.max = 26) 
pak <- ts(mydata$Pakistan,start = c(1991),frequency = 1)
decomp = stl(pak, s.window=4)
plot(data_pak)
model_pak <- auto.arima(data_pak, ic="aic", trace=TRUE) ####69.54
summary(model_pak)

#################################finding best model

i<-1
z<-c()
for (p in 0:3){
  for (d in 0:2){
    for (q in 0:4){
      x<-arima(data_pak, c(p, d, q))
      y<-summary(arima(data_pak, c(p, d, q)))
      z<-c(z,y[1,5])
    }}}

outputmatrix <- matrix(ncol=5, nrow=60)
i <- 1
for (p in 0:3){
  for (d in 0:2){
    for (q in 0:4){
      outputmatrix[i,] <-  c(p, d, q, arima(data_pak, c(p, d, q))$aic,z[i])
      i=i+1
    }
  }
}

model_best_1 <- arima(data_pak, c(0, 1, 4))
model_best_2 <- arima(data_pak, c(1, 1, 4))
model_best_3<- arima(data_pak, c(0, 1, 2))
model_best_4<- arima(data_pak, c(3, 1, 2))
#######################


data_pak_1 <- forecast(model_best_1, h=1)
data_pak_2 <- forecast(model_best_2, h=1)
data_pak_3 <- forecast(model_best_3, h=1)
data_pak_4 <- forecast(model_best_4, h=1)
mape_wt1 <- 1/summary(model_best_1)[1,5]
mape_wt2 <- 1/summary(model_best_2)[1,5]
mape_wt3 <- 1/summary(model_best_3)[1,5]
mape_wt4<- 1/summary(model_best_4)[1,5]
totalwt<-mape_wt1+mape_wt2+mape_wt3+mape_wt4
w1<-mape_wt1/totalwt
w2<-mape_wt2/totalwt
w3<-mape_wt3/totalwt
w4<-mape_wt4/totalwt
assemble_forecast<-(w1*data_pak_1$mean)+(w2*data_pak_2$mean)+(w3*data_pak_3$mean)+(w4*data_pak_4$mean)

###################(c)
data_sril<-ts(mydata$Sri.Lanka,start = c(1991),frequency = 1)
adf.test(data_sril)
adf.test(diff(data_sril))
acf(data_sril, lag.max = 26) #q=1
pacf(data_sril, lag.max = 26) #p=0
sril <- ts(mydata$Sri.Lanka,start = c(1991),frequency = 1)
decomp = stl(sril, s.window=4)
plot(data_sril)
model_sril <- auto.arima(data_sril, ic="aic", trace=TRUE) ####69.54
summary(model_sril)

#####################################
i<-1
z<-c()
for (p in 0:1){
  for (d in 0:1){
    for (q in 0:2){
      x<-arima(data_sril, c(p, d, q))
      y<-summary(arima(data_sril, c(p, d, q)))
      z<-c(z,y[1,5])
    }}}

outputmatrix <- matrix(ncol=5, nrow=12)
i <- 1
for (p in 0:1){
  for (d in 0:1){
    for (q in 0:2){
      outputmatrix[i,] <-  c(p, d, q, arima(data_sril, c(p, d, q))$aic,z[i])
      i=i+1
    }
  }
}
#BEST MODEL IS ARIMA(0,1,2), ARIMA(0,1,1),ARIMA(1,1,1), ARIMA(1,1,2)
model_best_1 <- arima(data_sril, c(0, 1, 2))
model_best_2 <- arima(data_sril, c(0, 1, 1))
model_best_3<- arima(data_sril, c(1, 1, 1))
model_best_4<- arima(data_sril, c(1, 1, 2))
#######################


data_sril_1 <- forecast(model_best_1, h=1)
data_sril_2 <- forecast(model_best_2, h=1)
data_sril_3 <- forecast(model_best_3, h=1)
data_sril_4 <- forecast(model_best_4, h=1)
mape_wt1 <- 1/summary(model_best_1)[1,5]
mape_wt2 <- 1/summary(model_best_2)[1,5]
mape_wt3 <- 1/summary(model_best_3)[1,5]
mape_wt4<- 1/summary(model_best_4)[1,5]
totalwt<-mape_wt1+mape_wt2+mape_wt3+mape_wt4
w1<-mape_wt1/totalwt
w2<-mape_wt2/totalwt
w3<-mape_wt3/totalwt
w4<-mape_wt4/totalwt
assemble_forecast<-(w1*data_sril_1$mean)+(w2*data_sril_2$mean)+(w3*data_sril_3$mean)+(w4*data_sril_4$mean)

#######################India (d)

data_Ind<-ts(mydata$India,start = c(1991),frequency = 1)
adf.test(data_Ind)
adf.test(diff(data_Ind))
acf(data_Ind, lag.max = 26) #q=1
pacf(data_Ind, lag.max = 26) #p=0
Ind <- ts(mydata$India,start = c(1991),frequency = 1)
decomp = stl(Ind, s.window=4)
plot(data_Ind)
model_Ind <- auto.arima(data_Ind, ic="aic", trace=TRUE) ####69.54
summary(model_Ind)

#####################################
i<-1
z<-c()
for (p in 0:1){
  for (d in 0:1){
    for (q in 0:2){
      x<-arima(data_Ind, c(p, d, q))
      y<-summary(arima(data_Ind, c(p, d, q)))
      z<-c(z,y[1,5])
    }}}

outputmatrix <- matrix(ncol=5, nrow=12)
i <- 1
for (p in 0:1){
  for (d in 0:1){
    for (q in 0:2){
      outputmatrix[i,] <-  c(p, d, q, arima(data_Ind, c(p, d, q))$aic,z[i])
      i=i+1
    }
  }
}
model_best_1 <- arima(data_Ind, c(0, 0, 1))
model_best_2 <- arima(data_Ind, c(0, 1, 1))
model_best_3<- arima(data_Ind, c(1, 1, 1))
model_best_4<- arima(data_Ind, c(1, 0, 0))
#######################


data_Ind_1 <- forecast(model_best_1, h=1)
data_Ind_2 <- forecast(model_best_2, h=1)
data_Ind_3 <- forecast(model_best_3, h=1)
data_Ind_4 <- forecast(model_best_4, h=1)
mape_wt1 <- 1/summary(model_best_1)[1,5]
mape_wt2 <- 1/summary(model_best_2)[1,5]
mape_wt3 <- 1/summary(model_best_3)[1,5]
mape_wt4<- 1/summary(model_best_4)[1,5]
totalwt<-mape_wt1+mape_wt2+mape_wt3+mape_wt4
w1<-mape_wt1/totalwt
w2<-mape_wt2/totalwt
w3<-mape_wt3/totalwt
w4<-mape_wt4/totalwt
assemble_forecast<-(w1*data_Ind_1$mean)+(w2*data_Ind_2$mean)+(w3*data_Ind_3$mean)+(w4*data_Ind_4$mean)

################(e)

########bangladesh
model_best_013 <- arima(data_bangladesh, c(0, 1, 3))
model_best_012 <- arima(data_bangladesh, c(0, 1, 2))
model_best_023<- arima(data_bangladesh, c(0, 2, 3))
model_best_024<- arima(data_bangladesh, c(0, 2, 4))
data_bangladesh_013 <- forecast(model_best_013, h=3)
data_bangladesh_012 <- forecast(model_best_012, h=3)
data_bangladesh_023 <- forecast(model_best_023, h=3)
data_bangladesh_024 <- forecast(model_best_024, h=3)
mape_wt1 <- 1/summary(model_best_013)[1,5]
mape_wt2 <- 1/summary(model_best_012)[1,5]
mape_wt3 <- 1/summary(model_best_023)[1,5]
mape_wt4<- 1/summary(model_best_024)[1,5]
totalwt<-mape_wt1+mape_wt2+mape_wt3+mape_wt4
w1<-mape_wt1/totalwt
w2<-mape_wt2/totalwt
w3<-mape_wt3/totalwt
w4<-mape_wt4/totalwt
assemble_forecast19<-(w1*data_bangladesh_013$mean[2])+(w2*data_bangladesh_013$mean[2])+(w3*data_bangladesh_023$mean[2])+(w4*data_bangladesh_024$mean[2])
assemble_forecast20<-(w1*data_bangladesh_013$mean[3])+(w2*data_bangladesh_013$mean[3])+(w3*data_bangladesh_023$mean[3])+(w4*data_bangladesh_024$mean[3])

##########pakistan
model_best_102 <- arima(data_pak, c(1, 0, 2))
model_best_114 <- arima(data_pak, c(1, 1, 4))
model_best_202<- arima(data_pak, c(2, 0, 2))
model_best_303<- arima(data_pak, c(3, 0, 3))
#######################


data_pak_1 <- forecast(model_best_102, h=3)
data_pak_2 <- forecast(model_best_114, h=3)
data_pak_3 <- forecast(model_best_202, h=3)
data_pak_4 <- forecast(model_best_303, h=3)
mape_wt1 <- 1/summary(model_best_102)[1,5]
mape_wt2 <- 1/summary(model_best_114)[1,5]
mape_wt3 <- 1/summary(model_best_202)[1,5]
mape_wt4<- 1/summary(model_best_303)[1,5]
totalwt<-mape_wt1+mape_wt2+mape_wt3+mape_wt4
w1<-mape_wt1/totalwt
w2<-mape_wt2/totalwt
w3<-mape_wt3/totalwt
w4<-mape_wt4/totalwt
assemble_forecast19<-(w1*data_pak_1$mean[2])+(w2*data_pak_2$mean[2])+(w3*data_pak_3$mean[2])+(w4*data_pak_4$mean[2])
assemble_forecast20<-(w1*data_pak_1$mean[3])+(w2*data_pak_2$mean[3])+(w3*data_pak_3$mean[3])+(w4*data_pak_4$mean[3])
##############srilanka
model_best_1 <- arima(data_sril, c(0, 1, 2))
model_best_2 <- arima(data_sril, c(0, 1, 1))
model_best_3<- arima(data_sril, c(1, 1, 1))
model_best_4<- arima(data_sril, c(1, 1, 2))
#######################


data_sril_1 <- forecast(model_best_1, h=3)
data_sril_2 <- forecast(model_best_2, h=3)
data_sril_3 <- forecast(model_best_3, h=3)
data_sril_4 <- forecast(model_best_4, h=3)
mape_wt1 <- 1/summary(model_best_1)[1,5]
mape_wt2 <- 1/summary(model_best_2)[1,5]
mape_wt3 <- 1/summary(model_best_3)[1,5]
mape_wt4<- 1/summary(model_best_4)[1,5]
totalwt<-mape_wt1+mape_wt2+mape_wt3+mape_wt4
w1<-mape_wt1/totalwt
w2<-mape_wt2/totalwt
w3<-mape_wt3/totalwt
w4<-mape_wt4/totalwt
assemble_forecast19<-(w1*data_sril_1$mean[2])+(w2*data_sril_2$mean[2])+(w3*data_sril_3$mean[2])+(w4*data_sril_4$mean[2])
assemble_forecast20<-(w1*data_sril_1$mean[3])+(w2*data_sril_2$mean[3])+(w3*data_sril_3$mean[3])+(w4*data_sril_4$mean[3])
######################India
model_best_1 <- arima(data_Ind, c(0, 0, 1))
model_best_2 <- arima(data_Ind, c(0, 1, 1))
model_best_3<- arima(data_Ind, c(1, 1, 1))
model_best_4<- arima(data_Ind, c(1, 0, 0))
#######################


data_Ind_1 <- forecast(model_best_1, h=3)
data_Ind_2 <- forecast(model_best_2, h=3)
data_Ind_3 <- forecast(model_best_3, h=3)
data_Ind_4 <- forecast(model_best_4, h=3)
mape_wt1 <- 1/summary(model_best_1)[1,5]
mape_wt2 <- 1/summary(model_best_2)[1,5]
mape_wt3 <- 1/summary(model_best_3)[1,5]
mape_wt4<- 1/summary(model_best_4)[1,5]
totalwt<-mape_wt1+mape_wt2+mape_wt3+mape_wt4
w1<-mape_wt1/totalwt
w2<-mape_wt2/totalwt
w3<-mape_wt3/totalwt
w4<-mape_wt4/totalwt
assemble_forecast19<-(w1*data_Ind_1$mean[2])+(w2*data_Ind_2$mean[2])+(w3*data_Ind_3$mean[2])+(w4*data_Ind_4$mean[2])
assemble_forecast20<-(w1*data_Ind_1$mean[3])+(w2*data_Ind_2$mean[3])+(w3*data_Ind_3$mean[3])+(w4*data_Ind_4$mean[3])



