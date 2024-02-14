setwd("C:/Users/yonghakim/Desktop/용하/따릉이공모전")

library(dplyr)
library(FNN)
library(caret)
library(e1071)
library(rpart)
library(stringr)
library(randomForest)
library(adabag)
library(neuralnet)
library(nnet)
library(naniar)

df<-read.csv("train.csv")
dft<-read.csv("test.csv")
dftt<-read.csv("따릉이 21년도 데이터.csv")
dftt$rental<-as.numeric(dftt$rental)

df<-df%>%mutate(precipitation=ifelse(is.na(precipitation),0,precipitation))
df<-df%>%mutate(sunshine_sum=ifelse(is.na(sunshine_sum),0,sunshine_sum))

dft<-dft%>%mutate(precipitation=ifelse(is.na(precipitation),0,precipitation))
dft<-dft%>%mutate(sunshine_sum=ifelse(is.na(sunshine_sum),0,sunshine_sum))

df$precipitationw<-ifelse(df$precipitation>60,1,0)
df$temp_lowestw<-ifelse(df$temp_lowest<=-12,1,0)
df$temp_highestw<-ifelse(df$temp_highest>=33,1,0)

dft$precipitationw<-ifelse(dft$precipitation>60,1,0)
dft$temp_lowestw<-ifelse(dft$temp_lowest<=-12,1,0)
dft$temp_highestw<-ifelse(dft$temp_highest>=33,1,0)

##train 파생변수
tw<-function(x,y){
  x*atan(0.151977*((y+8.313659)**(1/2))) + atan(x+y) -
    atan(y-1.67633) + 0.00391838*(y**(3/2))*atan(0.023101*y) - 4.686035
}
df$tw <- tw(df$temp_mean, df$humidity)
df$tw

ftmax<-function(x,y){
  (-0.2442) + 0.55399*x + 0.45535*y - 0.0022*(x**2) +
    0.00278*x*y + 3.0
}
df$ftmax <- ftmax(df$tw, df$temp_highest)
df$ftmax
options(scipen = 9999)

ftmin <- function(x,y){
  13.12 + 0.6215*x - 11.37*(y**(16/100)) + 0.3965*(y**(16/100))*x
}
df$ftmin <- ftmin(df$temp_lowest, df$wind_mean)
df$ftmin

di <- function(x,y){
  0.72*(x+y) +40.6
}
df$di <- di(df$temp_mean, df$tw)
df$di

##test 파생변수
tw<-function(x,y){
  x*atan(0.151977*((y+8.313659)**(1/2))) + atan(x+y) -
    atan(y-1.67633) + 0.00391838*(y**(3/2))*atan(0.023101*y) - 4.686035
}
dft$tw <- tw(dft$temp_mean, dft$humidity)
dft$tw

ftmax<-function(x,y){
  (-0.2442) + 0.55399*x + 0.45535*y - 0.0022*(x**2) +
    0.00278*x*y + 3.0
}
dft$ftmax <- ftmax(dft$tw, dft$temp_highest)
dft$ftmax
options(scipen = 9999)

ftmin <- function(x,y){
  13.12 + 0.6215*x - 11.37*(y**(16/100)) + 0.3965*(y**(16/100))*x
}
dft$ftmin <- ftmin(dft$temp_lowest, dft$wind_mean)
dft$ftmin

dft$di <- di(dft$temp_mean, dft$tw)

##미세먼지 결측치 제거
jongro2018<-read.csv("기간별_일평균_대기환경_정보_2018년_종로구.csv", fileEncoding = "euc-kr")
jongro2019<-read.csv("기간별_일평균_대기환경_정보_2019년_종로구.csv", fileEncoding = "euc-kr")
jongro2020<-read.csv("기간별_일평균_대기환경_정보_2020년_종로구.csv", fileEncoding = "euc-kr")

jongro2018<-jongro2018[,c(1,6,7)]
jongro2019<-jongro2019[,c(1,6,7)]
jongro2020<-jongro2020[,c(1,6,11)]
View(df)
PM<-rbind(jongro2018, jongro2019, jongro2020)
names(PM)<-c("date", "PM10", "PM2.5")
PM$date <- as.Date(as.character(PM$date), format='%Y%m%d')
PM$date <- as.character(PM$date)

dfpm<-left_join(df, PM, 'date')
View(dfpm)
df$PM10<-dfpm$PM10.y
df$PM2.5<-dfpm$PM2.5.y
sum(is.na(df$PM10))
sum(is.na(df$PM2.5))
View(df)
df.PM10.02<-df[which(str_detect(df$date, "-02-")),6]
df.PM10.03<-df[which(str_detect(df$date, "-03-")),6]
df.PM2.5.02<-df[which(str_detect(df$date, "-02-")),7]
df.PM2.5.03<-df[which(str_detect(df$date, "-03-")),7]
round(mean(df.PM10.02, na.rm=TRUE))
round(mean(df.PM10.03, na.rm=TRUE))
round(mean(df.PM2.5.02, na.rm=TRUE))
round(mean(df.PM2.5.03, na.rm=TRUE))
df.PM10.02<-ifelse(is.na(df.PM10.02),round(mean(df.PM10.02, na.rm=TRUE)),df.PM10.02)
df.PM10.03<-ifelse(is.na(df.PM10.03),round(mean(df.PM10.03, na.rm=TRUE)),df.PM10.03)
df.PM2.5.02<-ifelse(is.na(df.PM2.5.02),round(mean(df.PM2.5.02, na.rm=TRUE)),df.PM2.5.02)
df.PM2.5.03<-ifelse(is.na(df.PM2.5.03),round(mean(df.PM2.5.03, na.rm=TRUE)),df.PM2.5.03)

df[which(str_detect(df$date, "-02-")),6]<-df.PM10.02
df[which(str_detect(df$date, "-03-")),6]<-df.PM10.03
df[which(str_detect(df$date, "-02-")),7]<-df.PM2.5.02
df[which(str_detect(df$date, "-03-")),7]<-df.PM2.5.03

sum(df[which(str_detect(df$date, "2018")),13])
sum(df[which(str_detect(df$date, "2019")),13])
sum(df[which(str_detect(df$date, "2020")),13])

df$date_n <- as.Date(df$date)
df$date_y <- 0
df$date_y <- as.factor(format(df$date_n,'%Y'))
df$date_m <- 0
df$date_m <- as.factor(format(df$date_n,'%m'))
df$date_d <- 0
df$date_d <- as.factor(format(df$date_n,'%d'))
df$date_w <- 0
df$date_w <- as.factor(format(df$date_n,'%w'))

dft$date_n<- as.Date(dft$date)
dft$date_y <- 0
dft$date_y <- as.factor(format(dft$date_n,'%Y'))
dft$date_m <- 0
dft$date_m <- as.factor(format(dft$date_n,'%m'))
dft$date_d <- 0
dft$date_d <- as.factor(format(dft$date_n,'%d'))
dft$date_w <- 0
dft$date_w <- as.factor(format(dft$date_n,'%w'))

df$snow<-ifelse(df$temp_mean<0&df$precipitation>0,1,0)


normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalize1 <- function(x) {
  return ((x * (max(fore$mean) - min(fore$mean)))+min(fore$mean))
}
df11 <- as.data.frame(lapply(df[,c()], normalize))
dft11 <- as.data.frame(lapply(dft[,-1], normalize))


##변수선택
colSums(is.na(df))
colSums(is.na(df1))
colSums(is.na(dft))
colSums(is.na(dft1))
df1<-df[,-c(1,3:12,14:17,19,20,22:23,25)]
dft1<-dft[,-c(1,3:12,13:16,18,19,21:22,24)]

##변수 분할
idx <- sample(1:nrow(df1), size = nrow(df1)*0.7, replace=FALSE)
train <- df1[idx,]
test <- df1[-idx,]


library(dplyr)
library(randomForest)
set.seed(1234)
rf.fit = randomForest(rental~.,data = df1,mtry=5,ntree=1000,importance=T)
p<-predict(rf.fit,dft1)
p1<-normalize1(p)
caret::RMSE(p1,dftt$rental)
nmae(p1,dftt$rental)
varImpPlot(rf.fit)
dtt<-as.data.frame(p)
write.csv(dtt,file="pred.csv")
library(ggplot2)
par(mfrow=c(1,2))
plot(actual$p)
plot(actual$rental)

sum(fore$mean)/3


library(xgboost)
library(Matrix)
xgb_df<-sparse.model.matrix(rental~.-1,data = df1)
trainx<-xgb_df
trainy<-df1[,'rental']
dtrain<-xgb.DMatrix(data = trainx,label=as.matrix(trainy))

xgb_dft<-sparse.model.matrix(~.-1,data = dft1)

set.seed(3218)
xgb<-xgboost(data = trainx,label = trainy,max.depth = 4, nrounds = 300)



xgbp<-predict(xgb,xgb_dft)
xgbp<-normalize1(xgbp)
caret::RMSE(xgbp,dftt$rental)
nmae(xgbp,dftt$rental)
names<-dimnames(trainx)[[2]]
importance_matrix<-xgb.importance(names,model = xgb)
xgb.plot.importance(importance_matrix[1:20,])

plot(xgbp)
plot(dftt$rental)
nmae(cv_model$pred,dftt$rental)
dfpred<-as.data.frame(xgbp)
write.csv(dfpred,file="pred.csv")
cv_model$pred
nmae(cv_model$pred,dftt$rental)

pd11<-read.csv("pred.csv")
nmae(pd11$xgbp,dftt$rental)

xgb_grid_1 = expand.grid(
  nrounds = 1000,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1
)
# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all"                                                      
)
# train the model for each parameter combination in the grid,
#   using CV to evaluate
xgb_train_1 = train(
  x =  as.matrix(df1 %>%
                   select(-rental)),
  y = as.numeric(df1$rental),
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)










rf.fit = randomForest(rental~.,data = df1,mtry=5,ntree=1000,importance=T)
varImpPlot(rf.fit)

p<-predict(rf.fit, dft1)
View(p)
View(round(p))
p<-round(p)
caret::RMSE(p,dftt$rental)
dftt<-as.data.frame(p)
write.csv(dftt,file="pred.csv")

nmae<-function(x,y) {
  return (mean(abs(y-x)/y))
}
nmae(xgbp,dftt$rental)
compute.nmae(p,dftt$rental)


##시계열 분석
library(tseries)
library(TTR)
library(forecast)

#계절
set.seed(123)
tsdata <- ts(data=df$rental,start = c(2018,1),
             frequency = 365)

ts_model2 <- auto.arima(tsdata)
ts_model2

model <- arima(tsdata, c(5,1,1), seasonal=list(order=c(0,1,0)))

fore <- forecast(model,h=1095)
nmae(forecast,dftt$rental)
sum(fore$mean)

prd<-read.csv('predict.csv')

nmae(prd$rfdf.pd,dftt$rental)


