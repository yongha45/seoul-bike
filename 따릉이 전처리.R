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
library(NeuralNetTools)

#df불러오기
df<-read.csv("train.csv")
dft<-read.csv("test.csv")
dftt<-read.csv("따릉이 21년도 데이터.csv")
dftt

##df변수변환
df<-df%>%mutate(precipitation=ifelse(is.na(precipitation),0,precipitation))
df<-df%>%mutate(PM10=ifelse(is.na(PM10),0,PM10))
df<-df%>%mutate(PM2.5=ifelse(is.na(PM2.5),0,PM2.5))
df<-df%>%mutate(sunshine_sum=ifelse(is.na(sunshine_sum),0,sunshine_sum))
df$PM10<-as.numeric(df$PM10)
df$PM2.5<-as.numeric(df$PM2.5)

df$precipitationw<-ifelse(df$precipitation>60,1,0)
df$precipitationw <- as.factor(df$precipitationw)
df$temp_lowestw<-ifelse(df$temp_lowest<=-12,1,0)
df$temp_lowestw <- as.factor(df$temp_lowestw)
df$temp_highestw<-ifelse(df$temp_highest>=33,1,0)
df$temp_highestw <- as.factor(df$temp_highestw)

##dft변수변환
dft<-dft%>%mutate(precipitation=ifelse(is.na(precipitation),0,precipitation))
dft<-dft%>%mutate(PM10=ifelse(is.na(PM10),0,PM10))
dft<-dft%>%mutate(PM2.5=ifelse(is.na(PM2.5),0,PM2.5))
dft<-dft%>%mutate(sunshine_sum=ifelse(is.na(sunshine_sum),0,sunshine_sum))

dft$precipitationw<-ifelse(dft$precipitation>60,1,0)
dft$precipitationw<-as.factor(dft$precipitationw)
dft$temp_lowestw<-ifelse(dft$temp_lowest<=-12,1,0)
dft$temp_lowestw<-as.factor(dft$temp_lowestw)
dft$temp_highestw<-ifelse(dft$temp_highest>=33,1,0)
dft$temp_highestw<-as.factor(dft$temp_highestw)

##파생변수 함수
tw<-function(x,y){
  x*atan(0.151977*((y+8.313659)**(1/2))) + atan(x+y) -
    atan(y-1.67633) + 0.00391838*(y**(3/2))*atan(0.023101*y) - 4.686035
}

ftmax<-function(x,y){
  (-0.2442) + 0.55399*x + 0.45535*y - 0.0022*(x**2) +
    0.00278*x*y + 3.0
}

ftmin <- function(x,y){
  13.12 + 0.6215*x - 11.37*(y**(16/100)) + 0.3965*(y**(16/100))*x
}

di <- function(x,y){
  0.72*(x+y) +40.6
}

options(scipen = 9999)
##train 파생변수
df$tw <- tw(df$temp_mean, df$humidity)

df$ftmax <- ftmax(df$tw, df$temp_highest)

df$ftmin <- ftmin(df$temp_lowest, df$wind_mean)

df$di <- di(df$temp_mean, df$tw)

df$dt <- df$temp_highest - df$temp_lowest

##test 파생변수
dft$tw <- tw(dft$temp_mean, dft$humidity)

dft$ftmax <- ftmax(dft$tw, dft$temp_highest)

dft$ftmin <- ftmin(dft$temp_lowest, dft$wind_mean)

dft$di <- di(dft$temp_mean, dft$tw)

dft$dt <- dft$temp_highest - dft$temp_lowest

##미세먼지 결측치 제거
jongro2018<-read.csv("기간별_일평균_대기환경_정보_2018년_종로구.csv", fileEncoding = "euc-kr")
jongro2019<-read.csv("기간별_일평균_대기환경_정보_2019년_종로구.csv", fileEncoding = "euc-kr")
jongro2020<-read.csv("기간별_일평균_대기환경_정보_2020년_종로구.csv", fileEncoding = "euc-kr")

jongro2018<-jongro2018[,c(1,6,7)]
jongro2019<-jongro2019[,c(1,6,7)]
jongro2020<-jongro2020[,c(1,6,11)]
PM<-rbind(jongro2018, jongro2019, jongro2020)
names(PM)<-c("date", "PM10", "PM2.5")
PM$date <- as.Date(as.character(PM$date), format='%Y%m%d')
PM$date <- as.character(PM$date)

dfpm<-left_join(df, PM, 'date')
df$PM10<-dfpm$PM10.y
df$PM2.5<-dfpm$PM2.5.y
sum(is.na(df$PM10))
sum(is.na(df$PM2.5))
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

##렌탈 변수에 보정


## 일별, 월별, 요일별 변수

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

df$sunshineph <- df$sunshine_rate/df$sunshine_sum
df$skycondition<-ifelse(df$sunshine_rate<3.225,2,
                        ifelse(df$sunshine_rate>9.700,0,1))
df$skycondition<-as.factor(df$skycondition)


dft$sunshineph <- dft$sunshine_rate/dft$sunshine_sum
dft$skycondition<-ifelse(dft$sunshine_rate<3.225,2,
                         ifelse(dft$sunshine_rate>9.700,0,1))
dft$skycondition<-as.factor(dft$skycondition)

df<-df%>%mutate(sunshineph=ifelse(is.na(sunshineph),0,sunshineph))
dft<-dft%>%mutate(sunshineph=ifelse(is.na(sunshineph),0,sunshineph))

s_s18<- read.csv('2018_snow.csv')
s_s19<- read.csv('2019_snow.csv')
s_s20<- read.csv('2020_snow.csv')
s_s21<- read.csv('2021_snow.csv')

s_s<- rbind(s_s18,s_s19,s_s20,s_s21)
tail(s_s)
colnames(s_s)
s_s <- s_s[c("일시","적설.cm.")]
s_s$increase <- rep(0,957)

library(data.table)
s_s$적설_1시간전전<- shift(s_s$적설.cm.,1)

snow_t <- s_s$일시[s_s$적설.cm.-s_s$적설_1시간전전>=0]
snow_t<- snow_t[-1]


snow_tm <- snow_t[as.ITime("06:00:00")<=as.ITime(snow_t)&as.ITime(snow_t)<=as.ITime("22:00:00")]
snow_tm

snow_tm <- unique(as.Date(snow_tm))


library(lubridate)
year(snow_tm)
month(snow_tm)
day(snow_tm)

ind <- c()
for(i in 1:66){
  ind <- c(ind,which(df$date==unique(as.Date(snow_tm))[i],TRUE))
}
df$snow_d <- rep(0,1095)
df$snow_d[ind] <- 1
df$snow_d <- as.factor(df$snow_d)
df$snow_d

ind <- c()
for(i in 1:66){
  ind <- c(ind,which(dft$date==unique(as.Date(snow_tm))[i],TRUE))
}
dft$snow_d <- rep(0,365)
dft$snow_d[ind] <- 1
dft$snow_d <- as.factor(dft$snow_d)
dft$snow_d

r21 <- read.csv("rain_2021.csv")
r20 <- read.csv("rain_2020.csv")
r19 <- read.csv("rain_2019.csv")
r18 <- read.csv("rain_2018.csv")
rain <- rbind(r21,r20,r19,r18)
rain <- rain[,-c(1,2)]

time(rain$강수량.mm.)

rain_d<- rain$일시[as.ITime("06:00:00")<=as.ITime(rain$일시)&as.ITime(rain$일시)<=as.ITime("22:00:00")]
unique(as.Date(rain_d))


indr <- c()
for(i in 1:486){
  indr <- c(indr,which(df$date==unique(as.Date(rain_d))[i],TRUE))
}
df$rain_d <- rep(0,1095)
df$rain_d[indr] <- 1
df$rain_d <- as.factor(df$rain_d)
df$rain_d[df$snow_d==1] <- 0
df$rain_d

indr <- c()
for(i in 1:486){
  indr <- c(indr,which(dft$date==unique(as.Date(rain_d))[i],TRUE))
}
dft$rain_d <- rep(0,365)
dft$rain_d[indr] <- 1
dft$rain_d <- as.factor(dft$rain_d)
dft$rain_d
dft$rain_d[dft$snow_d==1] <- 0
dft$rain_d

df$ft<-0
df$ft[which(str_detect(df$date, "-01-"),TRUE)] <- df$ftmin[which(str_detect(df$date, "-01-"),TRUE)]
df$ft[which(str_detect(df$date, "-02-"),TRUE)] <- df$ftmin[which(str_detect(df$date, "-02-"),TRUE)]
df$ft[which(str_detect(df$date, "-03-"),TRUE)] <- df$ftmin[which(str_detect(df$date, "-03-"),TRUE)]
df$ft[which(str_detect(df$date, "-04-"),TRUE)] <- df$ftmax[which(str_detect(df$date, "-04-"),TRUE)]
df$ft[which(str_detect(df$date, "-05-"),TRUE)] <- df$ftmax[which(str_detect(df$date, "-05-"),TRUE)]
df$ft[which(str_detect(df$date, "-06-"),TRUE)] <- df$ftmax[which(str_detect(df$date, "-06-"),TRUE)]
df$ft[which(str_detect(df$date, "-07-"),TRUE)] <- df$ftmax[which(str_detect(df$date, "-07-"),TRUE)]
df$ft[which(str_detect(df$date, "-08-"),TRUE)] <- df$ftmax[which(str_detect(df$date, "-08-"),TRUE)]
df$ft[which(str_detect(df$date, "-09-"),TRUE)] <- df$ftmax[which(str_detect(df$date, "-09-"),TRUE)]
df$ft[which(str_detect(df$date, "-10-"),TRUE)] <- df$ftmax[which(str_detect(df$date, "-10-"),TRUE)]
df$ft[which(str_detect(df$date, "-11-"),TRUE)] <- df$ftmin[which(str_detect(df$date, "-11-"),TRUE)]
df$ft[which(str_detect(df$date, "-12-"),TRUE)] <- df$ftmin[which(str_detect(df$date, "-12-"),TRUE)]

dft$ft<-0
dft$ft[which(str_detect(dft$date, "-01-"),TRUE)] <- dft$ftmin[which(str_detect(dft$date, "-01-"),TRUE)]
dft$ft[which(str_detect(dft$date, "-02-"),TRUE)] <- dft$ftmin[which(str_detect(dft$date, "-02-"),TRUE)]
dft$ft[which(str_detect(dft$date, "-03-"),TRUE)] <- dft$ftmin[which(str_detect(dft$date, "-03-"),TRUE)]
dft$ft[which(str_detect(dft$date, "-04-"),TRUE)] <- dft$ftmax[which(str_detect(dft$date, "-04-"),TRUE)]
dft$ft[which(str_detect(dft$date, "-05-"),TRUE)] <- dft$ftmax[which(str_detect(dft$date, "-05-"),TRUE)]
dft$ft[which(str_detect(dft$date, "-06-"),TRUE)] <- dft$ftmax[which(str_detect(dft$date, "-06-"),TRUE)]
dft$ft[which(str_detect(dft$date, "-07-"),TRUE)] <- dft$ftmax[which(str_detect(dft$date, "-07-"),TRUE)]
dft$ft[which(str_detect(dft$date, "-08-"),TRUE)] <- dft$ftmax[which(str_detect(dft$date, "-08-"),TRUE)]
dft$ft[which(str_detect(dft$date, "-09-"),TRUE)] <- dft$ftmax[which(str_detect(dft$date, "-09-"),TRUE)]
dft$ft[which(str_detect(dft$date, "-10-"),TRUE)] <- dft$ftmax[which(str_detect(dft$date, "-10-"),TRUE)]
dft$ft[which(str_detect(dft$date, "-11-"),TRUE)] <- dft$ftmin[which(str_detect(dft$date, "-11-"),TRUE)]
dft$ft[which(str_detect(dft$date, "-12-"),TRUE)] <- dft$ftmin[which(str_detect(dft$date, "-12-"),TRUE)]


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

fore <- forecast(model,h=1065)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
normalize1 <- function(x) {
  return ((x * (max(fore$mean) - min(fore$mean)))+min(fore$mean))
}

df[which(str_detect(df$date, "2018")) ,13]<-normalize(df[which(str_detect(df$date, "2018")) ,13])
df[which(str_detect(df$date, "2019")) ,13]<-normalize(df[which(str_detect(df$date, "2019")) ,13])
df[which(str_detect(df$date, "2020")) ,13]<-normalize(df[which(str_detect(df$date, "2020")) ,13])

df[which(str_detect(df$date, "2018-01-")| 
           str_detect(df$date, "2018-02-")|
           str_detect(df$date, "2018-03-")|
           str_detect(df$date, "2018-10-")|
           str_detect(df$date, "2018-11-")|
           str_detect(df$date, "2018-12-")),13] <-
  normalize(df[which(str_detect(df$date, "2018-01-")|
                       str_detect(df$date, "2018-02-")|
                       str_detect(df$date, "2018-03-")|
                       str_detect(df$date, "2018-10-")|
                       str_detect(df$date, "2018-11-")|
                       str_detect(df$date, "2018-12-")),13])

df[which(str_detect(df$date, "2018-04-")| 
           str_detect(df$date, "2018-05-")|
           str_detect(df$date, "2018-06-")|
           str_detect(df$date, "2018-07-")|
           str_detect(df$date, "2018-08-")|
           str_detect(df$date, "2018-09-")),13] <-
  normalize(df[which(str_detect(df$date, "2018-04-")|
                       str_detect(df$date, "2018-05-")|
                       str_detect(df$date, "2018-06-")|
                       str_detect(df$date, "2018-07-")|
                       str_detect(df$date, "2018-08-")|
                       str_detect(df$date, "2018-09-")),13])

df[which(str_detect(df$date, "2019-01-")| 
           str_detect(df$date, "2019-02-")|
           str_detect(df$date, "2019-03-")|
           str_detect(df$date, "2019-10-")|
           str_detect(df$date, "2019-11-")|
           str_detect(df$date, "2019-12-")),13] <-
  normalize(df[which(str_detect(df$date, "2019-01-")|
                       str_detect(df$date, "2019-02-")|
                       str_detect(df$date, "2019-03-")|
                       str_detect(df$date, "2019-10-")|
                       str_detect(df$date, "2019-11-")|
                       str_detect(df$date, "2019-12-")),13])

df[which(str_detect(df$date, "2019-04-")| 
           str_detect(df$date, "2019-05-")|
           str_detect(df$date, "2019-06-")|
           str_detect(df$date, "2019-07-")|
           str_detect(df$date, "2019-08-")|
           str_detect(df$date, "2019-09-")),13] <-
  normalize(df[which(str_detect(df$date, "2019-04-")|
                       str_detect(df$date, "2019-05-")|
                       str_detect(df$date, "2019-06-")|
                       str_detect(df$date, "2019-07-")|
                       str_detect(df$date, "2019-08-")|
                       str_detect(df$date, "2019-09-")),13])

df[which(str_detect(df$date, "2020-01-")| 
           str_detect(df$date, "2020-02-")|
           str_detect(df$date, "2020-03-")|
           str_detect(df$date, "2020-10-")|
           str_detect(df$date, "2020-11-")|
           str_detect(df$date, "2020-12-")),13] <-
  normalize(df[which(str_detect(df$date, "2020-01-")|
                       str_detect(df$date, "2020-02-")|
                       str_detect(df$date, "2020-03-")|
                       str_detect(df$date, "2020-10-")|
                       str_detect(df$date, "2020-11-")|
                       str_detect(df$date, "2020-12-")),13])

df[which(str_detect(df$date, "2020-04-")| 
           str_detect(df$date, "2020-05-")|
           str_detect(df$date, "2020-06-")|
           str_detect(df$date, "2020-07-")|
           str_detect(df$date, "2020-08-")|
           str_detect(df$date, "2020-09-")),13] <-
  normalize(df[which(str_detect(df$date, "2020-04-")|
                       str_detect(df$date, "2020-05-")|
                       str_detect(df$date, "2020-06-")|
                       str_detect(df$date, "2020-07-")|
                       str_detect(df$date, "2020-08-")|
                       str_detect(df$date, "2020-09-")),13])

df[which(str_detect(df$date, "2018")) ,13]<-df[which(str_detect(df$date, "2018")) ,13]*{(sum(fore$mean)/3)/sum(df[which(str_detect(df$date, "2018")) ,13])}
df[which(str_detect(df$date, "2019")) ,13]<-df[which(str_detect(df$date, "2019")) ,13]*{(sum(fore$mean)/3)/sum(df[which(str_detect(df$date, "2019")) ,13])}
df[which(str_detect(df$date, "2020")) ,13]<-df[which(str_detect(df$date, "2020")) ,13]*{(sum(fore$mean)/3)/sum(df[which(str_detect(df$date, "2020")) ,13])}

## 여름 겨울 보정
df[which(str_detect(df$date, "2018-01-")| 
           str_detect(df$date, "2018-02-")|
           str_detect(df$date, "2018-03-")|
           str_detect(df$date, "2018-10-")|
           str_detect(df$date, "2018-11-")|
           str_detect(df$date, "2018-12-")),13] <-
  normalize(df[which(str_detect(df$date, "2018-01-")|
                       str_detect(df$date, "2018-02-")|
                       str_detect(df$date, "2018-03-")|
                       str_detect(df$date, "2018-10-")|
                       str_detect(df$date, "2018-11-")|
                       str_detect(df$date, "2018-12-")),13])

df[which(str_detect(df$date, "2018-04-")| 
           str_detect(df$date, "2018-05-")|
           str_detect(df$date, "2018-06-")|
           str_detect(df$date, "2018-07-")|
           str_detect(df$date, "2018-08-")|
           str_detect(df$date, "2018-09-")),13] <-
  normalize(df[which(str_detect(df$date, "2018-04-")|
                       str_detect(df$date, "2018-05-")|
                       str_detect(df$date, "2018-06-")|
                       str_detect(df$date, "2018-07-")|
                       str_detect(df$date, "2018-08-")|
                       str_detect(df$date, "2018-09-")),13])

df[which(str_detect(df$date, "2019-01-")| 
           str_detect(df$date, "2019-02-")|
           str_detect(df$date, "2019-03-")|
           str_detect(df$date, "2019-10-")|
           str_detect(df$date, "2019-11-")|
           str_detect(df$date, "2019-12-")),13] <-
  normalize(df[which(str_detect(df$date, "2019-01-")|
                       str_detect(df$date, "2019-02-")|
                       str_detect(df$date, "2019-03-")|
                       str_detect(df$date, "2019-10-")|
                       str_detect(df$date, "2019-11-")|
                       str_detect(df$date, "2019-12-")),13])

df[which(str_detect(df$date, "2019-04-")| 
           str_detect(df$date, "2019-05-")|
           str_detect(df$date, "2019-06-")|
           str_detect(df$date, "2019-07-")|
           str_detect(df$date, "2019-08-")|
           str_detect(df$date, "2019-09-")),13] <-
  normalize(df[which(str_detect(df$date, "2019-04-")|
                       str_detect(df$date, "2019-05-")|
                       str_detect(df$date, "2019-06-")|
                       str_detect(df$date, "2019-07-")|
                       str_detect(df$date, "2019-08-")|
                       str_detect(df$date, "2019-09-")),13])

df[which(str_detect(df$date, "2020-01-")| 
           str_detect(df$date, "2020-02-")|
           str_detect(df$date, "2020-03-")|
           str_detect(df$date, "2020-10-")|
           str_detect(df$date, "2020-11-")|
           str_detect(df$date, "2020-12-")),13] <-
  normalize(df[which(str_detect(df$date, "2020-01-")|
                       str_detect(df$date, "2020-02-")|
                       str_detect(df$date, "2020-03-")|
                       str_detect(df$date, "2020-10-")|
                       str_detect(df$date, "2020-11-")|
                       str_detect(df$date, "2020-12-")),13])

df[which(str_detect(df$date, "2020-04-")| 
           str_detect(df$date, "2020-05-")|
           str_detect(df$date, "2020-06-")|
           str_detect(df$date, "2020-07-")|
           str_detect(df$date, "2020-08-")|
           str_detect(df$date, "2020-09-")),13] <-
  normalize(df[which(str_detect(df$date, "2020-04-")|
                       str_detect(df$date, "2020-05-")|
                       str_detect(df$date, "2020-06-")|
                       str_detect(df$date, "2020-07-")|
                       str_detect(df$date, "2020-08-")|
                       str_detect(df$date, "2020-09-")),13])

##xgboost 계절별 변수
xgdfspring <- df[which(str_detect(df$date, "-03-")| 
                         str_detect(df$date, "-04-")|
                         str_detect(df$date, "-05-")), -c(1,6:12,16,22:23,25)]
xgdftspring <- dft[which(str_detect(dft$date, "-03-")| 
                           str_detect(dft$date, "-04-")|
                           str_detect(dft$date, "-05-")), -c(1,6:12,15,21,22,24)]

xgdfsummer <- df[which(str_detect(df$date, "-06-")| 
                         str_detect(df$date, "-07-")|
                         str_detect(df$date, "-08-")), -c(1,6:12,16,22:23,25)]
xgdftsummer <- dft[which(str_detect(dft$date, "-06-")| 
                           str_detect(dft$date, "-07-")|
                           str_detect(dft$date, "-08-")), -c(1,6:12,16,22:23,25)]

xgdffall <- df[which(str_detect(df$date, "-09-")| 
                       str_detect(df$date, "-10-")|
                       str_detect(df$date, "-11-")), -c(1,6:12,16,22:23,25)]
xgdftfall <- dft[which(str_detect(dft$date, "-09-")| 
                         str_detect(dft$date, "-10-")|
                         str_detect(dft$date, "-11-")), -c(1,6:12,16,22:23,25)]

xgdfwinter <- df[which(str_detect(df$date, "-01-")| 
                         str_detect(df$date, "-02-")|
                         str_detect(df$date, "-12-")), -c(1,6:12,16,22:23,25)]
xgdftwinter <- dft[which(str_detect(dft$date, "-01-")| 
                           str_detect(dft$date, "-02-")|
                           str_detect(dft$date, "-12-")), -c(1,6:12,15,21,22,24)]


