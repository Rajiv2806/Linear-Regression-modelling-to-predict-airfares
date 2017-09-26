rm(list = ls())

#Dataset used for analysis can be found at:
#install.packages("RCurl")
library(RCurl)
Airfares <- read.table(text = getURL("https://raw.githubusercontent.com/Rajiv2806/Linear-Regression-modelling-to-predict-airfares/master/Airfares%20-%20Dataset.csv"), header = T, sep = ",")


sum(is.na(Airfares))#to check missing values
Airfares <- na.omit(Airfares)

dim(Airfares) 
nrow(Airfares)
colnames(Airfares)
str(Airfares)


#Airfares$NEW <- as.factor(Airfares$NEW)
Airfares$VACATION<-as.factor(Airfares$VACATION)
Airfares$SW<-as.factor(Airfares$SW)
Airfares$SLOT<-as.factor(Airfares$SLOT)
Airfares$GATE<-as.factor(Airfares$GATE)
Airfares$S_INCOME  <- as.numeric(substr(gsub(",","",Airfares$S_INCOME),2,6))
Airfares$E_INCOME  <- as.numeric(substr(gsub(",","",Airfares$E_INCOME),2,6))
Airfares$FARE <- as.numeric(substr(Airfares$FARE,2,7))
str(Airfares)

row.number<- sample(1:nrow(Airfares), size=0.2*nrow(Airfares))
Airfares_test<-Airfares[row.number, ] 
dim(Airfares_test)

Airfares_train<-Airfares[-row.number, ] 
dim(Airfares_train)

par(mfrow=c(2,2)) #to create multipanelled plotting window of 2 rows 2 columns
boxplot(Airfares_train$COUPON, main="COUPON")
boxplot(Airfares_train$NEW, main="NEW")
boxplot(Airfares_train$HI, main="HI")
boxplot(Airfares_train$E_INCOME, main="E_INCOME")
par(mfrow=c(2,2)) #to create multipanelled plotting window of 2 rows 2 columns
boxplot(Airfares_train$S_INCOME, main="S_INCOME")
boxplot(Airfares_train$DISTANCE, main="DISTANCE")
boxplot(Airfares_train$S_POP, main="S_POP")
boxplot(Airfares_train$E_POP, main="E_POP")
par(mfrow=c(1,2)) #to create multipanelled plotting window of 2 rows 2 column
boxplot(Airfares_train$PAX, main="PAX")
boxplot(Airfares_train$FARE, main="FARE")
#hist(table(Airfares$VACATION),lables=c("NO","YES"))


library(PerformanceAnalytics)
chart.Correlation(Airfares_train[,-c(2,3,4,10,11)])


#scatterplot matrix
pairs(Airfares_train[,-c(3,4,5,6,7,13,14)])
pairs(~COUPON+NEW+HI+S_INCOME+E_INCOME+S_POP+E_POP+DISTANCE+PAX+FARE,data=Airfares_train,main="simple scatterplot matrix")

#correlation matrix
nums <- sapply(Airfares_train, is.numeric)
Airfares_train[nums]
mcor<-round(cor(Airfares_train[nums]),2)
library(corrplot)
corrplot(mcor, method="number") 

#####fitting a model


#Model 1
library("car")
fit1<-lm(FARE~.,data = Airfares_train)
summary(fit1)

qqPlot(fit1)

confint(fit1)
predict(fit1,newdata = Airfares_train[1,],interval = "confidence",level = 0.90)
predict(fit1,newdata = Airfares_train[1,],interval = "prediction")


library(car)
residualPlot(fit1, id.n=5)
residualPlots(fit1,id.n=3)


#transformation1
Airfares_train$SINCOME_sq<-(Airfares_train$S_INCOME)^2
Airfares_train$DISTANCE_sq<-(Airfares_train$DISTANCE)^2
Airfares_train$LCOUPON<-log(Airfares_train$COUPON)
Airfares_train$LPAX<-(log(max(Airfares_train$PAX)+1-Airfares_train$PAX))
Airfares_train$HI_sq<-(Airfares_train$HI)^2

#MODEL2
fit2<-  lm(log(FARE)~LCOUPON+factor(NEW)+VACATION+SW+HI+HI_sq+S_INCOME+SINCOME_sq+E_INCOME+S_POP+E_POP+SLOT+GATE+DISTANCE+DISTANCE_sq+LPAX,data = Airfares_train)
summary(fit2)
library(car)
residualPlots(fit2,id.n=3)

# #TRANSFORMATION2

 Airfares_train$Pax_sq<-(Airfares_train$PAX)^2


#model3
fit3<-  lm(log(FARE)~LCOUPON+factor(NEW)+VACATION+SW+HI+HI_sq+S_INCOME+SINCOME_sq+E_INCOME+S_POP+E_POP+SLOT+GATE+DISTANCE+DISTANCE_sq+PAX+Pax_sq,data = Airfares_train)

summary(fit3)
library(car)
residualPlots(fit3,id.n=3)

###transformation 3
Airfares_train$SPOP_sq<-(Airfares_train$S_POP)^2
Airfares_train$EINCOME_sq<-(Airfares_train$E_INCOME)^2
Airfares_train$EPOP_sq<-(Airfares_train$E_POP)^2

#Model 4
fit4<-  lm(log(FARE)~LCOUPON+factor(NEW)+VACATION+SW+HI+HI_sq+S_INCOME+SINCOME_sq+E_INCOME+EINCOME_sq+S_POP+SPOP_sq+E_POP+EPOP_sq+SLOT+GATE+DISTANCE+DISTANCE_sq+PAX+Pax_sq,data = Airfares_train)
summary(fit4)
library(car) #residual plots
residualPlots(fit4,id.n=3)

##transformation4
Airfares_train$NEW<- as.factor(Airfares_train$NEW)
cont_treatment<-matrix(c(0,0,0,1,0,0,1,1,0,1,1,1),nrow=4,ncol=3,byrow=TRUE)
colnames(cont_treatment)<-c("1","2","3")
contrasts(Airfares_train$NEW) <-cont_treatment

###Model5
fit5<-  lm(log(FARE)~LCOUPON+NEW+VACATION+SW+HI+HI_sq+S_INCOME+SINCOME_sq+E_INCOME+EINCOME_sq+S_POP+SPOP_sq+E_POP+EPOP_sq+SLOT+GATE+DISTANCE+DISTANCE_sq+PAX+Pax_sq,data = Airfares_train)
summary(fit5)
residualPlots(fit5,id.n=3)
##transformation5
Airfares_train$COUPON_sq<-(Airfares_train$COUPON)^2

###model6
fit6<-  lm(log(FARE)~COUPON+COUPON_sq+NEW+VACATION+SW+HI+HI_sq+S_INCOME+SINCOME_sq+E_INCOME+EINCOME_sq+S_POP+SPOP_sq+E_POP+EPOP_sq+SLOT+GATE+DISTANCE+DISTANCE_sq+PAX+Pax_sq,data = Airfares_train)
summary(fit6)
par(mfrow=c(2,2))
plot(fit6)

###Influenstial diagnostics

influenceIndexPlot(fit6)
#Cooks Distance
cutoff <- 4/((nrow(Airfares_train)-length(fit6$coefficients)-2))
plot(fit6, which=4, cook.levels=cutoff)

lev=hat(model.matrix(fit6))
plot(lev)
t(Airfares_train[lev>0.2,])
plot( p.adjust(fit6$residuals, method = "bonferroni"))



##Model after deletion diagnistics

fit7<-  lm(log(FARE)~COUPON+COUPON_sq+NEW+VACATION+SW+HI+HI_sq+S_INCOME+SINCOME_sq+E_INCOME+EINCOME_sq+S_POP+SPOP_sq+E_POP+EPOP_sq+SLOT+GATE+DISTANCE+DISTANCE_sq+PAX+Pax_sq,data = Airfares_train[-319,])
summary(fit7)
par(mfrow=c(2,2))
plot(fit7)
residualPlots(fit7,id.n=3)

lev=hat(model.matrix(fit7))
plot(lev)

which(lev>0.2)
cutoff <- 4/((nrow(Airfares_train)-1-length(fit7$coefficients)-2))
plot(fit7, which=4, cook.levels=cutoff)


####multicollinearity

library(perturb)
library(MASS)
#colldiag(fit1,add.intercept=FALSE,center=TRUE)
vif(fit6)
colldiag(Airfares_train[,-c(1:4,6,7,8,14,15)])


vif.default(fit6)
###Best subset regression
step <- stepAIC(fit6, direction="both")

 
####Validation of the model
####Transformation of test data

Airfares_test $SINCOME_sq<-(Airfares_test$S_INCOME)^2
Airfares_test$DISTANCE_sq<-(Airfares_test$DISTANCE)^2
Airfares_test$LCOUPON<-log(Airfares_test$COUPON)
Airfares_test$LPAX<-(log(max(Airfares_test$PAX)+1-Airfares_test$PAX))
Airfares_test$HI_sq<-(Airfares_test$HI)^2
Airfares_test$Pax_sq<-(Airfares_test$PAX)^2
Airfares_test$SPOP_sq<-(Airfares_test$S_POP)^2
Airfares_test$EINCOME_sq<-(Airfares_test$E_INCOME)^2
Airfares_test$EPOP_sq<-(Airfares_test$E_POP)^2
Airfares_test$NEW<- as.factor(Airfares_test$NEW)
cont_treatment<-matrix(c(0,0,0,1,0,0,1,1,0,1,1,1),nrow=4,ncol=3,byrow=TRUE)
colnames(cont_treatment)<-c("1","2","3")
contrasts(Airfares_test$NEW) <-cont_treatment
Airfares_test$COUPON_sq<-(Airfares_test$COUPON)^2


y_hat<-predict.lm (fit6, newdata= Airfares_test, se.fit=TRUE)$fit
y_hat<-as.vector (y_hat)
dev<-log(Airfares_test$FARE)-(y_hat)
num<-sum(dev^2)
dev1<-log(Airfares_test$FARE)-mean(log(Airfares_test$FARE))
den<-sum(dev1^2)
Predicted.Rsq<-1-(num/den)
Predicted.Rsq

####Transformation of full data

Airfares$SINCOME_sq<-(Airfares$S_INCOME)^2
Airfares$DISTANCE_sq<-(Airfares$DISTANCE)^2
Airfares$LCOUPON<-log(Airfares$COUPON)
Airfares$LPAX<-(log(max(Airfares$PAX)+1-Airfares$PAX))
Airfares$HI_sq<-(Airfares$HI)^2
Airfares$Pax_sq<-(Airfares$PAX)^2
Airfares$SPOP_sq<-(Airfares$S_POP)^2
Airfares$EINCOME_sq<-(Airfares$E_INCOME)^2
Airfares$EPOP_sq<-(Airfares$E_POP)^2
Airfares$NEW<- as.factor(Airfares$NEW)
cont_treatment<-matrix(c(0,0,0,1,0,0,1,1,0,1,1,1),nrow=4,ncol=3,byrow=TRUE)
colnames(cont_treatment)<-c("1","2","3")
contrasts(Airfares$NEW) <-cont_treatment
Airfares$COUPON_sq<-(Airfares$COUPON)^2

y_hat<-predict.lm ( fit6, newdata= Airfares, se.fit=TRUE)$fit
y_hat<-as.vector (y_hat)
dev<-log(Airfares_test$FARE)-(y_hat)
num<-sum(dev^2)
dev1<-log(Airfares_test$FARE)-mean(log(Airfares_test$FARE))
den<-sum(dev1^2)
Predicted.Rsq<-1-(num/den)
Predicted.Rsq

Fmodel<-  lm(log(FARE)~COUPON+COUPON_sq+NEW+VACATION+SW+HI+HI_sq+S_INCOME+
               SINCOME_sq+E_INCOME+EINCOME_sq+S_POP+SPOP_sq+E_POP+EPOP_sq+SLOT+
               GATE+DISTANCE+DISTANCE_sq+PAX+Pax_sq,data = Airfares)

summary(Fmodel)
library(car) #residual plots
residualPlots(Fmodel,id.n=3)
qqPlot(Fmodel, main="QQ Plot of residuals: Fmodel") #qqplot

#################################################################
#setwd("D:/ISB/21-StatAnalysis-2/Practice Data Analysis-20170313")
#Airfares<-read.csv("Airfares1.csv", header=T)
