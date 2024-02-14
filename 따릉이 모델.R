##변수선택
colSums(is.na(df))
colSums(is.na(df1))
colSums(is.na(dft))
colSums(is.na(dft1))
df1<-df[,-c(1,3:8,11:12,14:17,22:23,25)]
dft1<-dft[,-c(1,3:8,11:12,13:16,21:22,24)]

df1<-df[,-c(1,6:12,16,22:23,25)]
dft1<-dft[,-c(1,6:12,15,21:22,24)]


colSums(is.na(dfwinter))
colSums(is.na(dftwinter))
library(dplyr)
library(randomForest)
set.seed(1234)
rf.fit = randomForest(rental~.,data = dfsummer,mtry=5,ntree=1000,importance=T)
p<-predict(rf.fit,dftsummer)
p1<-normalize1(p)
caret::RMSE(p1,dftts)
nmae(p1,dftts)
varImpPlot(rf.fit)

p<-predict(rf.fit,dft1)
caret::RMSE(p,dftt$rental)
nmae(p,dftt$rental)
varImpPlot(rf.fit)
library(xgboost)
library(Matrix)
xgb_df<-sparse.model.matrix(rental~.-1,data = df1)
trainx<-xgb_df
trainy<-df1[,'rental']
dtrain<-xgb.DMatrix(data = trainx,label=as.matrix(trainy))
xgb_dft<-sparse.model.matrix(~.-1,data = dft1)

xgb_df<-sparse.model.matrix(rental~.-1,data = dfwinter)
trainx<-xgb_df
trainy<-dfwinter[,'rental']
dtrain<-xgb.DMatrix(data = trainx,label=as.matrix(trainy))
xgb_dft<-sparse.model.matrix(~.-1,data = dftwinter)

set.seed(1234)
xgb<-xgboost(data = trainx,label = trainy,max.depth = 8, nrounds = 100)

xgbp<-predict(xgb,xgb_dft)
xgbp<-normalize1(xgbp)
caret::RMSE(xgbp,dftt$rental)
nmae(xgbp,dftt$rental)

names<-dimnames(trainx)[[2]]
importance_matrix<-xgb.importance(names,model = xgb)
xgb.plot.importance(importance_matrix[1:20,])
plot(df$rental)
plot(xgbp)
plot(dftt$rental)
colSums()
plot(xgdfspring$rental)
plot(xgdfwinter$rental)

colSums(is.na(xgdfspring))
colSums(is.na(xgdftspring))
xgdfspring1<-xgdfspring[,-c(7,13,14,16,17)]
xgdftspring1<-xgdftspring[,-c(6,12,13,15,16)]
fit<-lm(rental~.,data = xgdfspring1)
summary(lm1)
preds<-predict(lm1,xgdftspring1)
predss<-normalize1(preds)
nmae(predss,dftt[60:151,2])

fit.con <- lm(rental~1,data=xgdfspring1) 
fit.both <- step(fit.con,scope=list(lower=fit.con,upper=fit), direction = "both")
fit.both
summary(fit.both)
sqrt(car::vif(fit.both)) > 2
lms<-lm(formula = rental ~ temp_highest + precipitation + rain_d + 
     sunshineph , data = xgdfspring1)

preds<-predict(lms,xgdftspring1)
predss<-normalize1(preds)
nmae(predss,dftt[60:151,2])
cbind(predss,dftt[60:151,2])
predss[[1]]<-0
colSums(is.na(xgdfwinter))
xgdfwinter1<-xgdfwinter[,-c(2:4,6:11,13:14,16,17)]
xgdftwinter1<-xgdftwinter[,-c(2:4,5:10,12:13,15,16)]

xgdfwinter1<-xgdfwinter[c(1:59),-c(6,13,14,16)]
xgdftwinter1<-xgdftwinter[c(1:59),-c(5,12,13,15)]
fit<-lm(rental~.,data = xgdfwinter1)
summary(lm1)
preds<-predict(lm1,xgdftwinter1)
predss<-normalize1(preds)
nmae(predss,dftt[c(1:59,335:365),2])

fit.con <- lm(rental~1,data=xgdfwinter1) 
fit.both <- step(fit.con,scope=list(lower=fit.con,upper=fit), direction = "both")
fit.both

lmw<-lm(formula = rental ~  sunshineph + precipitation + 
          temp_lowest  + snow_d , data = xgdfwinter1)
predw<-predict(lmw,xgdftwinter1)
predww<-normalize1(predw)
nmae(predww,dftt[c(1:59),2])
predww[[7]]<-0
predww
cbind(predww,dftt[c(1:59,335:365),2])
predww<-predww*0.7
library(car)
sqrt(car::vif(fit.both)) > 2

dfpred<-as.data.frame(predss)
write.csv(dfpred,file="preds.csv")

dfpred<-as.data.frame(predww)
write.csv(dfpred,file="predw.csv")
