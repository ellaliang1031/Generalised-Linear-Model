bodyfat.data<-read.table('D:/2018 first semester/linear regression/ASSIGNMENT1/bodyfat.txt', header = TRUE)
dim(bodyfat.data)
head(bodyfat.data)
summary(bodyfat.data)
h<-hist(bodyfat.data_all$BodyFat,breaks=20,xlab='bodyfat distribution')
xfit<-seq(min(bodyfat.data_all$BodyFat),max(bodyfat.data_all$BodyFat),length=40)
yfit<-dnorm(xfit,mean=mean(bodyfat.data_all$BodyFat),sd=sd(bodyfat.data_all$BodyFat))
yfit<-yfit*diff(h$mid[1:2])*length(bodyfat.data_all$BodyFat)
lines(xfit,yfit,col='blue',lwd=2)
box()
boxplot(bodyfat.data$BodyFat,main='Boxplot of Bodyfat')
match(0,bodyfat.data[,2])
bodyfat.data<-bodyfat.data[-182,]
match(148.10,bodyfat.data[,8])
bodyfat.data[39,]
bodyfat.data<-bodyfat.data[-39,]
attach(bodyfat.data)
library(GGally)
ggpairs(bodyfat.data[,2:length(bodyfat.data)])
lm_all<-lm(BodyFat~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Biceps+Forearm+Wrist)
lm.step<-step(lm_all,direction = 'backward')
summary(lm.step)
lm.step1<-update(lm.step,~.-Neck)
summary(lm.step1)
lm.step2<-update(lm.step1,~.-Forearm)
summary(lm.step2)
leaps <- regsubsets(BodyFat~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Biceps+Forearm+Wrist,data=bodyfat.data,nbest=8)
plot(leaps,scale = "adjr2")
par(mfrow=c(2,2))
plot(lm.step2)
library(car)
crPlots(lm.step2)
qqPlot(lm.step2,labels = row.names(bodyfat.data),id.method = "identify",simulate = TRUE,main = "Q-Q Plot")
lm.step3<-update(lm.step2,~.-Chest)
summary(lm.step3)
influencePlot(lm.step3,id.method = "identity", main="Influence Plot",sub="Circle size is proportional to Cook's distance")
bodyfat.data<-bodyfat.data[-248,]
bodyfat.data<-bodyfat.data[-222,]
bodyfat.data<-bodyfat.data[-40,]
detach(bodyfat.data)
attach(bodyfat.data)
lm_all<-lm(BodyFat~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Biceps+Forearm+Wrist)
lm.step<-step(lm_all,direction = 'backward')
summary(lm.step)
lm.step1<-update(lm.step,~.-Neck)
summary(lm.step1)
lm.step2<-update(lm.step1,~.-Forearm)
summary(lm.step2)
lm.step3<-update(lm.step2,~.-Chest)
summary(lm.step3)
influencePlot(lm.step3,id.method = "identity", main="Influence Plot",sub="Circle size is proportional Influence")
bodyfat.data<-bodyfat.data[-234,]
bodyfat.data<-bodyfat.data[-221,]
bodyfat.data<-bodyfat.data[-213,]
detach(bodyfat.data)
attach(bodyfat.data)
lm_all<-lm(BodyFat~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Biceps+Forearm+Wrist)
lm.step<-step(lm_all,direction = 'backward')
summary(lm.step)
lm.step1<-update(lm.step,~.-Neck)
summary(lm.step1)
lm.step3<-update(lm.step,~.-Neck,-Forearm,-Chest)
summary(lm.step3)
lm.step3<-update(lm.step,~.-Neck-Forearm-Chest)
summary(lm.step3)
par(mfrow=c(1,1))
influencePlot(lm.step3,id.method = "identity", main="Influence Plot",sub="Circle size is proportional Influence")
qqPlot(lm.step3,labels = row.names(bodyfat.data),id.method = "identify",simulate = TRUE,main = "Q-Q Plot")
par(mfrow=c(1,3))
plot(lm.step3,which = 1:3)
durbinWatsonTest(lm.step3)
vif(lm.step3)
detach(bodyfat.data)
bodyfat.data1<-read.table('D:/2018 first semester/linear regression/ASSIGNMENT1/bodyfat.txt', header = TRUE)
attach(bodyfat.data1)
Chest1<-scale(Chest,center=T,scale=F) 
Hip1<-scale(Hip,center=T,scale=F) 
Abdomen1<-scale(Abdomen,center=T,scale=F) 
head(bodyfat.data1)
bodyfat.data1<-bodyfat.data1
bodyfat.data1[,7]<-Chest1
bodyfat.data1[,8]<-Abdomen1
bodyfat.data1[,9]<-Hip1
detach(bodyfat.data1)
attach(bodyfat.data1)
lm.all.1<-lm(BodyFat~Age+Weight+Height+Neck+Chest+Abdomen+Hip+Thigh+Knee+Ankle+Biceps+Forearm+Wrist+Chest:Abdomen)
lm.step.1<-step(lm.all.1,direction = 'backward')
summary(lm.step.1)
lm.step.2<-update(lm.step.1,~.-Forearm-Neck)
summary(lm.step.2)
lm.step.3<-update(lm.step.2,~.-Age)
summary(lm.step.3)
par(mfrow=c(1,1))
qqPlot(lm.step.3,labels = row.names(bodyfat.data),id.method = "identify",simulate = TRUE,main = "Q-Q Plot")
durbinWatsonTest(lm.step.3)
vif(lm.step.3)
AIC(lm.step3,lm.step.3)
shrinkage <- function(fit, k = 10) { 
  require(bootstrap)
  # define functions 
  theta.fit <- function(x, y) {
    lsfit(x, y) 
  } 
  theta.predict <- function(fit, x) { 
    cbind(1, x) %*% fit$coef 
  } # matrix of predictors 
  x <- fit$model[, 2:ncol(fit$model)] # vector of predicted values 
  y <- fit$model[, 1] 
  results <- crossval(x, y, theta.fit, theta.predict, ngroup = k) 
  r2 <- cor(y, fit$fitted.values)^2 
  r2cv <- cor(y, results$cv.fit)^2 
  cat("Original R-square =", r2, "\n") 
  cat(k, "Fold Cross-Validated R-square =", r2cv, "\n") 
  cat("Change =", r2 - r2cv, "\n")
} 
shrinkage(lm.step3) 
shrinkage(lm.step.3)
