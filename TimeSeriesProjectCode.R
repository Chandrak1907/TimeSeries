library(lubridate)
library(tseries)
library(forecast)
library(zoo)
library(astsa)
#  ________________________ DATA CLEANING , I THINK THIS CODE CAN BE DELETED________________________________
# #Below csvs are output from Python program
# dat = read.csv("2013_Final_Dec_2.csv",header=FALSE)
# fut= read.csv("2014_Final_Dec_2.csv",header=FALSE,as.is=TRUE)
# dat1= subset(dat,dat$V1=="A")
# names(dat1)=c("X","timestamp","value")
# dat2 = subset(dat1,select=-c(X))
# 
# filldata<-as.data.frame(format(seq(from=ISOdate(2013,1,1,hour=0),to=ISOdate(2014,1,1,hour=0), by="5 min"), "%Y-%m-%d %H:%M:%S"))
# colnames(filldata)<- "timestamp"
# fi<- merge(filldata,dat2, by="timestamp", all=TRUE)
# filleddata= fi
# sum(is.na(filleddata$value))
# nrow(filleddata)
# filleddata$value[is.na(filleddata$value)]<- 0
# 
# #Adding minutes and hour values to dataset
# filleddata$minutes =minute(filleddata$timestamp)
# filleddata$hours =hour(filleddata$timestamp)
# #Below two rows are not at 5 min - interval - to be deleted
# filleddata = filleddata[-c(105122,105123),]
# #Writing this sereis to csv file
# #write.csv(filleddata,"TEST_TIME_SERIES.csv")

# _________________________ EDA __________________
filleddata=read.csv("TEST_TIME_SERIES.csv")

#ACF or PACF test
acf(filleddata$value,lag=50000)
pacf(filleddata$value,lag=50000)
tdata=filleddata$value
#Lags upto two weeks
la=12*24*7*2
Box.test(tdata,lag=la)
Box.test(tdata,type = "Ljung")
Box.test(tdata,lag=la,type = "Ljung")
adf.test(filleddata$value)

# _______________ 5 - MIN Interval TIME SERIES _______________
model1=auto.arima(tdata,max.p=la,max.q=la,max.P=la,max.Q=la,max.d=la,max.D=la,stationary=FALSE, seasonal=TRUE,stepwise=TRUE,trace=TRUE)
#Above Auto.Arima suggests that (6,1,4) is best model

#Below model auto.arima was used to specify start p,q, and start P,Q
model2=auto.arima(tdata,max.p=la,max.q=la,max.P=la,max.Q=la,max.d=la,max.D=la,start.p=7,start.q=7,start.P=28,start.Q=28,stationary=FALSE, seasonal=TRUE,stepwise=TRUE,trace=TRUE)
#Above gives Best model: ARIMA(8,1,7) with drift 

#Below model is taking long time
model3= sarima(tdata,14,0,14,1,0,1,288)

# ______________  10,000 Data Points ___________________
# Last 10,000 points model to be paste here
nrow(filleddata)
tenth = filleddata[95121:105121,]
nrow(tenth)
head(tenth)
tail(tenth)
acf(tenth$value,lag=5000)
pacf(tenth$value,lag=5000)
model4= auto.arima(tenth$value)
#Below code will not work for large lags
model4=arima(resid,order=c(7*48*6,0,7*48*6), seasonal=list(order=c(1,0,1), period=7*48*2*6))
# Below code gives error with one week lag 
hi=rep(0,10082)
hi[10080]=NA
hi[10081]=NA
hi[10082]=NA
model = arima(tenth$value,order=c(10080,0,1),fixed=hi)


# ______________ Should we include code for genearting 30-min frequency??
# ____________________ 30- min Frequency ______________
# -------- ARIMA model ---- 
dat3=read.csv("30minFreq.csv")
z1= ts(dat3$max_value,frequency = 336)
model5= auto.arima(z1,trace=TRUE)
# 1344 pertains to 4 weeks lag on 30-min frequency
# below  vector will is to specify time lags for arima.
hi=rep(0,2701)
hi[1344]=NA
hi[2694]=NA
hi[2701]=NA
hi
model6 = arima(z1,order=c(1350,0,1350),fixed=hi)
# This gives an error of maximum supported lag is 350

# -------- ARMA model ---- 

# So used ARMA Model with list of lags 
u = seq(1:25)
par3=u*48*7
par3
model7=arma(z1,lag=list(ar=par3,ma=par3))
summary(model7)
Box.test(model$residuals)
model7$residuals

#Below code will give an error of missing values in object
acf(model7$residuals)
res=model7$residuals[par3]

#Above line will gives out missing values. Below code will generate error
#acf(res)

# -------- STL model ---- 

#STL On 30-min frequency
model8= stl(z1,s.window='periodic')
plot(model8)
resid = model8$time.series[,3]
model9=arima(resid,order=c(7*48,0,7*48), seasonal=list(order=c(1,0,1), period=7*48*2))
Box.test(model9$residuals)

# -------- LOWESS Regression model + ARIMA on residuals  ---- 

# Lowess regression with span 0.75
y.loess <- loess(max_value ~ bucket, span=.25,dat3)
y.loess
y.predict <- predict(y.loess, dat3$bucket)
plot(dat3$bucket,dat3$max_value)
lines(dat3$bucket,y.predict,col='blue')
resid = y.predict-dat3$max_value
Box.test(resid)
LM1=auto.arima(resid)
LM1
Box.test(LM1$residuals)
model9=arima(resid,order=c(7*48,0,7*48), seasonal=list(order=c(1,0,1), period=7*48*2))


# _____________________ ZERO MINUTES TIME SERIES _______________________
zerohours = subset(filleddata,(filleddata$hours==0))
zerominutes = subset(zerohours, zerohours$minutes==0)

acf(zerominutes$value,lag=200)
pacf(zerominutes$value,lag=200)
Box.test(zerominutes$value)
adf.test(zerominutes$value)

# ________ PREPARTING HOLD-OUT SAMPLE  THIS CODE CAN BE DELETED______________
# 
# fut1= subset(fut,fut$V1=="A")
# names(fut1)=c("X","timestamp","value")
# fut2 = subset(fut1,select=-c(X))
# filldata2<-as.data.frame(format(seq(from=ISOdate(2014,1,1,hour=0,min=5),to=ISOdate(2014,10,10,hour=0,min=0), by="5 min"), "%Y-%m-%d %H:%M:%S"))
# head(filldata2)
# colnames(filldata2)<- "timestamp"
# fi<- merge(filldata2,fut2, by="timestamp", all=TRUE)
# head(fi)
# tail(fi,n=10)
# futuredata=fi
# futuredata$value[is.na(futuredata$value)]<- 0
# 
# futuredata$minutes =minute(futuredata$timestamp)
# futuredata$hours =hour(futuredata$timestamp)
# futuredata$mont=month(futuredata$timestamp)
# head(futuredata)
# futuredata2=subset(futuredata,(futuredata$mont==1))
# zerohourstest = subset(futuredata2,(futuredata$hours==0))
# zerominutestest = subset(zerohourstest, zerohourstest$minutes==0)
# head(zerominutestest)
# tail(zerominutestest)
# nrow(zerominutestest)
# write.csv(zerominutestest,"TEST_2014.csv")
# ____________ ARCH LM FUNCTION ______________________

archlmtest <- function (x, lags, demean = FALSE) 
{
  x <- as.vector(x)
  if(demean) x <- scale(x, center = TRUE, scale = FALSE)
  lags <- lags + 1
  mat <- embed(x^2, lags)
  arch.lm <- summary(lm(mat[, 1] ~ mat[, -1]))
  STATISTIC <- arch.lm$r.squared * length(resid(arch.lm))
  names(STATISTIC) <- "Chisq"
  PARAMETER <- lags - 1
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  names(PVAL) <- "PVAL"
  METHOD <- "ARCH LM-test"
  result <- arch.lm
  df <- data.frame("Chisq" = STATISTIC, "Pval" = PVAL)
  return(df)
}

archlmbatch <- function (x, maxlags, demean = FALSE) 
{
  cat('Lag','\t','ChiSq','\t','PVal','\n')
  for (i in 1:maxlags)
  {
    temp <- archlmtest(x, i, demean)
    cat(i,'\t',temp$Chisq,'\t',temp$Pval,'\n')
  }
}



#_________________ READING TEST DATA _________________
fut = read.csv("TEST_2014.csv")
zerominutestest = fut

#_________________ MEASURING IN-SAMPLE & OUT-OF SAMPLE PERFORMANC at lags upto 15 weeks
# __________ OUTPUT OF THIS CODE, I ALREADY SENT TO MONICA

lagst=c(13)
for(p in lagst)
{
q=seq(from=5,to=p,by=1)
tg=q*7
tg
tg
hi= rep(0,221)
hi[tg]=NA
hi[tg+110]=NA
hi[221]=NA
hi

model = arima(zerominutes$value,order=c(110,0,110),fixed=hi)
print(model)
print(Box.test(model$residuals))
acf(model$residuals)
acf(model$residuals,lag=110)
tg
res = model$residuals[tg]
res

print(acf(res))
print(Box.test(res))
MSE = sum(model$residuals^2)/(nrow(zerominutes))
print("In-sample performance metrics")
print("MSE")
print(MSE)
MAD = sum(abs(model$residuals))/(nrow(zerominutes))
print("MAD")
print(MAD)

print("AIC")
print(model$aic)


print("Out-of-Sample performance metrics")

fore=forecast(model,h=31)
plot(fore)
dev= fore$mean-zerominutestest$value

MSE = sum(dev^2)/nrow(fut)
print("MSE")
print(MSE)

MAD = sum(abs(dev))/nrow(fut)
print("MAD")
print(MAD)
archlmbatch(model$residuals,110)
print("p-value of coeficients")
(1-pnorm(abs(model$coef)/sqrt(diag(model$var.coef))))*2



}

