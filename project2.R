#######read the  file
setwd("G:/data")
traindata=read.csv("cs-training.csv",header = T)
#####counting NA
sapply(traindata,function(x) sum(is.na(x)))
sapply(traindata,function(x) length(unique(x)))
##draw missing plot
library(Amelia)
missmap(newx,main = "Missing values vs observed")
md.pattern(traindata)
##make up for these colomns with many NAs
install.packages("DMwR")
require(DMwR)
traindata=knnImputation(traindata,k=10,meth="weighAvg")
write.csv(traindata,file="traindata.csv")
traindata=read.csv("traindata.csv",header = T)
##unique analysis
#RevolvingUtilizationOfUnsecuredLines
a=boxplot(traindata$RevolvingUtilizationOfUnsecuredLines,main="RevolvingUtilizationOfUnsecuredLines")
install.packages("Rlof")
#traindata$DebtRatio
b=boxplot(traindata$DebtRatio,main="DebtRatio")
#age
unique(traindata$age)
traindata=traindata[-which(traindata$age==0),]
##NumberOfTime30.59DaysPastDueNotWorse
unique(traindata$NumberOfTime30.59DaysPastDueNotWorse)
traindata=traindata[-which(traindata$NumberOfTime30.59DaysPastDueNotWorse==96),]
traindata=traindata[-which(traindata$NumberOfTime30.59DaysPastDueNotWorse==98),]
##traindata$NumberOfTimes90DaysLate
unique(traindata$NumberOfTimes90DaysLate)
##traindata$NumberOfTime60.89DaysPastDueNotWorse
unique(traindata$NumberOfTime60.89DaysPastDueNotWorse)
#####analysis for each variable
library(ggplot2)
##age
ggplot(traindata,aes(x=traindata$age,y=..density..))+geom_histogram(fill="blue",colour="grey60",size=0.2,alpha=0.2)+geom_density()
##monthly income
ggplot(traindata,aes(x=traindata$MonthlyIncome,y=..density..))+geom_histogram(fill="blue",colour="grey60",size=0.2,alpha=0.2)+geom_density()+xlim(1,20000)
####correlation test
cor1=cor(traindata[,1:11])
install.packages("corrplot")
library(corrplot)
corrplot(cor1)
corrplot(cor1,method="number")
##cut dataset
table(traindata$Default_Status)
install.packages("caret")
library(caret)
set.seed(1234)
splitIndex=createDataPartition(traindata$Default_Status,time=1,p=0.5,list=FALSE)
train=traindata[splitIndex,]
test=traindata[-splitIndex,]
write.csv(test,file="test.csv")
table(traindata$Default_Status)
prop.table(table(train$Default_Status))
prop.table(table(test$Default_Status))
####logistic regression
fit=glm(Default_Status~.,train,family = binomial(link = "logit"))
summary(fit)
pre=predict(fit,test,type = "response")
#reduced model
fit2=glm(Default_Status~age+NumberOfTime30.59DaysPastDueNotWorse+MonthlyIncome+NumberOfTimes90DaysLate+NumberRealEstateLoansOrLines+NumberOfTime60.89DaysPastDueNotWorse+NumberOfDependents,train,family="binomial")
summary(fit2)
##anova
anova(fit,fit2,test="Chisq")
###prediction(log number)
pre=predict(fit2,test,type = "response")
result=cbind(test,pre,pre.result)
write.csv(result,file="result.csv")
default_status=test$Default_Status
pre.result=ifelse(pre>0.2,"1","0")
table(pre.result,default_status)
###evaluation
#ROC
install.packages("pROC")
library(pROC)
modelroc=roc(test$Default_Status,pre.result)
plot(modelroc,print.auc=TRUE,auc.polygon=TRUE)
plot(modelroc,print.auc=TRUE,auc.polygon=TRUE,grid=c(0.1,0.2),grid.col=c("green","red"),max.auc.polygon=TRUE,auc.polygon.col="skyblue",print.thres=TRUE)
######model comparison
###model 2 mode
##fill the missings with mode
setwd("G:/project/New Compressed (zipped) Folder")
traindata=read.csv("cs-trainmode.csv",header = T)
##unique
#age
unique(traindata$age)
traindata=traindata[-which(traindata$age==0),]
##NumberOfTime30.59DaysPastDueNotWorse
unique(traindata$NumberOfTime30.59DaysPastDueNotWorse)
traindata=traindata[-which(traindata$NumberOfTime30.59DaysPastDueNotWorse==96),]
traindata=traindata[-which(traindata$NumberOfTime30.59DaysPastDueNotWorse==98),]
##cut dataset
library(caret)
set.seed(1234)
splitIndex=createDataPartition(traindata$Default_Status,time=1,p=0.5,list=FALSE)
train=traindata[splitIndex,]
test=traindata[-splitIndex,]
table(traindata$Default_Status)
prop.table(table(train$Default_Status))
prop.table(table(test$Default_Status))
####logistic regression
fit=glm(Default_Status~.,train,family = binomial(link = "logit"))
summary(fit)
#reduced model
fit2=glm(Default_Status~age+NumberOfTime30.59DaysPastDueNotWorse+MonthlyIncome+NumberOfTimes90DaysLate+NumberRealEstateLoansOrLines+NumberOfTime60.89DaysPastDueNotWorse+NumberOfDependents,train,family="binomial")
summary(fit2)
###prediction(log number)
pre=predict(fit2,test,type = "response")
result=cbind(test,pre,pre.result)
write.csv(result,file = "resultmode.csv")
default_status=test$Default_Status
pre.result=ifelse(pre>0.5,"1","0")
table(pre.result,default_status)
##fill the missings with mode
traindata=read.csv("cs-trainmedian.csv",header = T)
##unique
#age
unique(traindata$age)
traindata=traindata[-which(traindata$age==0),]
##NumberOfTime30.59DaysPastDueNotWorse
unique(traindata$NumberOfTime30.59DaysPastDueNotWorse)
traindata=traindata[-which(traindata$NumberOfTime30.59DaysPastDueNotWorse==96),]
traindata=traindata[-which(traindata$NumberOfTime30.59DaysPastDueNotWorse==98),]
##cut dataset
library(caret)
set.seed(1234)
splitIndex=createDataPartition(traindata$Default_Status,time=1,p=0.5,list=FALSE)
train=traindata[splitIndex,]
test=traindata[-splitIndex,]
table(traindata$Default_Status)
prop.table(table(train$Default_Status))
prop.table(table(test$Default_Status))
####logistic regression
fit=glm(Default_Status~.,train,family = binomial(link = "logit"))
summary(fit)
#reduced model
fit2=glm(Default_Status~age+NumberOfTime30.59DaysPastDueNotWorse+MonthlyIncome+NumberOfTimes90DaysLate+NumberRealEstateLoansOrLines+NumberOfTime60.89DaysPastDueNotWorse+NumberOfDependents,train,family="binomial")
summary(fit2)
###prediction(log number)
pre=predict(fit2,test,type = "response")
result=cbind(test,pre,pre.result)
write.csv(result,file = "resultmode.csv")
default_status=test$Default_Status
pre.result=ifelse(pre>0.5,"1","0")
table(pre.result,default_status)
####class
#age
cutage=c(-Inf,30,35,40,45,50,55,60,65,75,Inf)
plot(cut(train$age,cutage))
#NumberOfTime30.59DaysPastDueNotWorse
cutNumberOfTime30.59DaysPastDueNotWorse=c(-Inf,0,1,3,5,Inf)
plot(cut(train$NumberOfTime30.59DaysPastDueNotWorse,cutNumberOfTime30.59DaysPastDueNotWorse))
#MonthlyIncome
cutMonthlyIncome=c(-Inf,1000,2000,3000,4000,5000,6000,7500,9500,12000,Inf)
plot(cut(train$MonthlyIncome,cutMonthlyIncome))
#NumberOfTimes90DaysLate
cutNumberOfTimes90DaysLate=c(-Inf,0,1,3,5,10,Inf)
plot(cut(train$NumberOfTimes90DaysLate,cutNumberOfTimes90DaysLate))
#NumberRealEstateLoansOrLines
cutNumberRealEstateLoansOrLines=c(-Inf,0,1,2,3,5,Inf)
plot(cut(train$NumberRealEstateLoansOrLines,cutNumberRealEstateLoansOrLines))
#NumberOfTime60.89DaysPastDueNotWorse
cutNumberOfTime60.89DaysPastDueNotWorse=c(-Inf,0,1,3,5,Inf)
plot(cut(train$NumberOfTime60.89DaysPastDueNotWorse,cutNumberOfTime60.89DaysPastDueNotWorse))
#NumberOfDependents
cutNumberOfDependents=c(-Inf,0,1,2,3,5,Inf)
plot(cut(train$NumberOfDependents,cutNumberOfDependents))
###calc WOE
totalgood=as.numeric(table(train$Default_Status))[1]
totalbad=as.numeric(table(train$Default_Status))[2]
getWOE=function(a,p,q)
{
  Good=as.numeric(table(train$Default_Status[a>p&a<=q]))[1]
  Bad=as.numeric(table(train$Default_Status[a>p&a<=q]))[2]
  WOE=log((Bad/totalbad)/(Good/totalgood),base=exp(1))
  return(WOE)
}
#age
Agelessthan30.WOE=getWOE(train$age,-Inf,30)
Age30to35.WOE=getWOE(train$age,30,35)
Age35to40.WOE=getWOE(train$age,35,40)
Age40to45.WOE=getWOE(train$age,40,45)
Age45to50.WOE=getWOE(train$age,45,50)
Age50to55.WOE=getWOE(train$age,50,55)
Age55to60.WOE=getWOE(train$age,55,60)
Age60to65.WOE=getWOE(train$age,60,65)
Age65to75.WOE=getWOE(train$age,65,75)
Agemorethan.WOE=getWOE(train$age,75,Inf)
age.WOE=c(Agelessthan30.WOE,Age30to35.WOE,Age35to40.WOE,Age40to45.WOE,Age45to50.WOE,Age50to55.WOE,Age55to60.WOE,Age60to65.WOE,Age65to75.WOE,Agemorethan.WOE)
age.WOE
#NumberOfTime30-59DaysPastDueNotWorse(N3059N)
N3059Nlessthan0.WOE=getWOE(train$NumberOfTime30.59DaysPastDueNotWorse,-Inf,0)
N3059N0to1.WOE=getWOE(train$NumberOfTime30.59DaysPastDueNotWorse,0,1)
N3059N1to3.WOE=getWOE(train$NumberOfTime30.59DaysPastDueNotWorse,1,3)
N3059N3to5.WOE=getWOE(train$NumberOfTime30.59DaysPastDueNotWorse,3,5)
N3059Nmorethan.WOE=getWOE(train$NumberOfTime30.59DaysPastDueNotWorse,5,Inf)
N3059N.WOE=c(N3059Nlessthan0.WOE,N3059N0to1.WOE,N3059N1to3.WOE,N3059N3to5.WOE,N3059Nmorethan.WOE)
N3059N.WOE
#MonthlyIncome(MI)
MIlessthan.WOE=getWOE(train$MonthlyIncome,-Inf,1e+03)
MI1to2.WOE=getWOE(train$MonthlyIncome,1e+03,2e+03)
MI2to3.WOE=getWOE(train$MonthlyIncome,2e+03,3e+03)
MI3to4.WOE=getWOE(train$MonthlyIncome,3e+03,4e+03)
MI4to5.WOE=getWOE(train$MonthlyIncome,4e+03,5e+03)
MI5to6.WOE=getWOE(train$MonthlyIncome,5e+03,6e+03)
MI6to7.WOE=getWOE(train$MonthlyIncome,6e+03,7e+03)
MI7to8.WOE=getWOE(train$MonthlyIncome,7e+03,8e+03)
MI8to9.WOE=getWOE(train$MonthlyIncome,8e+03,9e+03)
MI9to10.WOE=getWOE(train$MonthlyIncome,9e+03,10e+03)
MI10to11.WOE=getWOE(train$MonthlyIncome,10e+03,11e+03)
MI11to12.WOE=getWOE(train$MonthlyIncome,11e+03,12e+03)
MImorethan.WOE=getWOE(train$MonthlyIncome,12e+03,Inf)
MI.WOE=c(MIlessthan.WOE,MI1to2.WOE,MI2to3.WOE,MI3to4.WOE,MI4to5.WOE,MI5to6.WOE,MI6to7.WOE,MI7to8.WOE,MI8to9.WOE,MI9to10.WOE,MI10to11.WOE,MI11to12.WOE,MImorethan.WOE)
MI.WOE
#NumberOfTimes90DaysLate(N90N)
N90Nlessthan.WOE=getWOE(train$NumberOfTimes90DaysLate,-Inf,0)
N90N0to1.WOE=getWOE(train$NumberOfTimes90DaysLate,0,1)
N90N1to3.WOE=getWOE(train$NumberOfTimes90DaysLate,1,3)
N90N3to5.WOE=getWOE(train$NumberOfTimes90DaysLate,3,5)
N90N5to10.WOE=getWOE(train$NumberOfTimes90DaysLate,5,10)
N90Nmorethan.WOE=getWOE(train$NumberOfTimes90DaysLate,10,Inf)
N90N.WOE=c(N90Nlessthan.WOE,N90N0to1.WOE,N90N1to3.WOE,N90N3to5.WOE,N90N5to10.WOE,N90Nmorethan.WOE)
N90N.WOE
#NumberRealEstateLoansOrLines(NREL)
NRELlessthan.WOE=getWOE(train$NumberRealEstateLoansOrLines,-Inf,0)
NREL0to1.WOE=getWOE(train$NumberRealEstateLoansOrLines,0,1)
NREL1to2.WOE=getWOE(train$NumberRealEstateLoansOrLines,1,2)
NREL2to3.WOE=getWOE(train$NumberRealEstateLoansOrLines,2,3)
NREL3to5.WOE=getWOE(train$NumberRealEstateLoansOrLines,3,5)
NRELmorethan.WOE=getWOE(train$NumberRealEstateLoansOrLines,5,Inf)
NREL.WOE=c(NRELlessthan.WOE,NREL0to1.WOE,NREL1to2.WOE,NREL2to3.WOE,NREL3to5.WOE,NRELmorethan.WOE)
NREL.WOE
#NumberOfTime60-89DaysPastDueNotWorse(N6089N)
N6089Nlessthan.WOE=getWOE(train$NumberOfTime60.89DaysPastDueNotWorse,-Inf,0)
N6089N0to1.WOE=getWOE(train$NumberOfTime60.89DaysPastDueNotWorse,0,1)
N6089N1to3.WOE=getWOE(train$NumberOfTime60.89DaysPastDueNotWorse,1,3)
N6089N3to5.WOE=getWOE(train$NumberOfTime60.89DaysPastDueNotWorse,3,5)
N6089Nmorethan.WOE=getWOE(train$NumberOfTime60.89DaysPastDueNotWorse,5,Inf)
N6089N.WOE=c(N6089Nlessthan.WOE,N6089N0to1.WOE,N6089N1to3.WOE,N6089N3to5.WOE,N6089Nmorethan.WOE)
N6089N.WOE
#NumberOfDependents(NOD)
NODlessthan.WOE=getWOE(train$NumberOfDependents,-Inf,0)
NOD0to1.WOE=getWOE(train$NumberOfDependents,0,1)
NOD1to2.WOE=getWOE(train$NumberOfDependents,1,2)
NOD2to3.WOE=getWOE(train$NumberOfDependents,2,3)
NOD3to5.WOE=getWOE(train$NumberOfDependents,3,5)
NODmorethan.WOE=getWOE(train$NumberOfDependents,5,Inf)
NOD.WOE=c(NODlessthan.WOE,NOD0to1.WOE,NOD1to2.WOE,NOD2to3.WOE,NOD3to5.WOE,NODmorethan.WOE)
NOD.WOE
#####WOE transformation
#age
tmp.age=0
for(i in 1:nrow(train)){
  if(train$age[i]<=30)
    tmp.age[i]=Agelessthan30.WOE
  else if(train$age[i]<=35)
    tmp.age[i]=Age30to35.WOE
  else if(train$age[i]<=40)
    tmp.age[i]=Age35to40.WOE
  else if(train$age[i]<=45)
    tmp.age[i]=Age40to45.WOE
  else if(train$age[i]<=50)
    tmp.age[i]=Age45to50.WOE
  else if(train$age[i]<=55)
    tmp.age[i]=Age50to55.WOE
  else if(train$age[i]<=60)
    tmp.age[i]=Age55to60.WOE
  else if(train$age[i]<=65)
    tmp.age[i]=Age60to65.WOE
  else if(train$age[i]<=75)
    tmp.age[i]=Age65to75.WOE
  else
    tmp.age[i]=Agemorethan.WOE
}
table(tmp.age)
tmp.age[1:10]
train$age[1:10]
#NumberOfTime30-59DaysPastDueNotWorse(N3059N)
tmp.N3059N=0
for(i in 1:nrow(train)){
  if(train$NumberOfTime30.59DaysPastDueNotWorse[i]<=0)
    tmp.N3059N[i]=N3059Nlessthan0.WOE
  else if(train$NumberOfTime30.59DaysPastDueNotWorse[i]<=1)
    tmp.N3059N[i]=N3059N0to1.WOE
  else if(train$NumberOfTime30.59DaysPastDueNotWorse[i]<=3)
    tmp.N3059N[i]=N3059N1to3.WOE
  else if(train$NumberOfTime30.59DaysPastDueNotWorse[i]<=5)
    tmp.N3059N[i]=N3059N3to5.WOE
  else
    tmp.N3059N[i]=N3059Nmorethan.WOE
}
table(tmp.N3059N)
tmp.N3059N[1:10]
train$NumberOfTime30.59DaysPastDueNotWorse[1:10]
#MonthlyIncome(MI)
tmp.MI=0
for(i in 1:nrow(train)){
  if(train$MonthlyIncome[i]<=1e+03)
    tmp.MI[i]=MIlessthan.WOE
  else if(train$MonthlyIncome[i]<=2e+03)
    tmp.MI[i]=MI1to2.WOE
  else if(train$MonthlyIncome[i]<=3e+03)
    tmp.MI[i]=MI2to3.WOE
  else if(train$MonthlyIncome[i]<=4e+03)
    tmp.MI[i]=MI3to4.WOE
  else if(train$MonthlyIncome[i]<=5e+03)
    tmp.MI[i]=MI4to5.WOE
  else if(train$MonthlyIncome[i]<=6e+03)
    tmp.MI[i]=MI5to6.WOE
  else if(train$MonthlyIncome[i]<=7e+03)
    tmp.MI[i]=MI6to7.WOE
  else if(train$MonthlyIncome[i]<=8e+03)
    tmp.MI[i]=MI7to8.WOE
  else if(train$MonthlyIncome[i]<=9e+03)
    tmp.MI[i]=MI8to9.WOE
  else if(train$MonthlyIncome[i]<=10e+03)
    tmp.MI[i]=MI9to10.WOE
  else if(train$MonthlyIncome[i]<=11e+03)
    tmp.MI[i]=MI10to11.WOE
  else if(train$MonthlyIncome[i]<=12e+03)
    tmp.MI[i]=MI11to12.WOE
  else
    tmp.MI[i]=MImorethan.WOE
}
table(tmp.MI)
tmp.MI[1:10]
train$MonthlyIncome[1:10]
#NumberOfTimes90DaysLate(N90N)
tmp.N90N=0
for(i in 1:nrow(train)){
  if(train$NumberOfTimes90DaysLate[i]<=0)
    tmp.N90N[i]=N90Nlessthan.WOE
  else if(train$NumberOfTimes90DaysLate[i]<=1)
    tmp.N90N[i]=N90N0to1.WOE
  else if(train$NumberOfTimes90DaysLate[i]<=3)
    tmp.N90N[i]=N90N1to3.WOE
  else if(train$NumberOfTimes90DaysLate[i]<=5)
    tmp.N90N[i]=N90N3to5.WOE
  else if(train$NumberOfTimes90DaysLate[i]<=10)
    tmp.N90N[i]=N90N5to10.WOE
  else
    tmp.N90N[i]=N90Nmorethan.WOE
}
table(tmp.N90N)
tmp.N90N[1:10]
train$NumberOfTimes90DaysLate[1:10]
#NumberRealEstateLoansOrLines(NREL)
tmp.NREL=0
for(i in 1:nrow(train)){
  if(train$NumberRealEstateLoansOrLines[i]<=0)
    tmp.NREL[i]=NRELlessthan.WOE
  else if(train$NumberRealEstateLoansOrLines[i]<=1)
    tmp.NREL[i]=NREL0to1.WOE
  else if(train$NumberRealEstateLoansOrLines[i]<=2)
    tmp.NREL[i]=NREL1to2.WOE
  else if(train$NumberRealEstateLoansOrLines[i]<=3)
    tmp.NREL[i]=NREL2to3.WOE
  else if(train$NumberOfTimes90DaysLate[i]<=5)
    tmp.NREL[i]=NREL3to5.WOE
  else
    tmp.NREL[i]=NRELmorethan.WOE
}
table(tmp.NREL)
tmp.NREL[1:10]
train$NumberRealEstateLoansOrLines[1:10]
#NumberOfTime60-89DaysPastDueNotWorse(N6089N)
tmp.N6089N=0
for(i in 1:nrow(train)){
  if(train$NumberOfTime60.89DaysPastDueNotWorse[i]<=0)
    tmp.N6089N[i]=N6089Nlessthan.WOE
  else if(train$NumberOfTime60.89DaysPastDueNotWorse[i]<=1)
    tmp.N6089N[i]=N6089N0to1.WOE
  else if(train$NumberOfTime60.89DaysPastDueNotWorse[i]<=3)
    tmp.N6089N[i]=N6089N1to3.WOE
  else if(train$NumberOfTime60.89DaysPastDueNotWorse[i]<=5)
    tmp.N6089N[i]=N6089N3to5.WOE
  else
    tmp.N6089N[i]=N6089Nmorethan.WOE
}
table(tmp.N6089N)
tmp.N6089N[1:10]
train$NumberOfTime60.89DaysPastDueNotWorse[1:10]
#NumberOfDependents（NOD）
tmp.NOD=0
for(i in 1:nrow(train)){
  if(train$NumberOfDependents[i]<=0)
    tmp.NOD[i]=NODlessthan.WOE
  else if(train$NumberOfDependents[i]<=1)
    tmp.NOD[i]=NOD0to1.WOE
  else if(train$NumberOfDependents[i]<=2)
    tmp.NOD[i]=NOD1to2.WOE
  else if(train$NumberOfDependents[i]<=3)
    tmp.NOD[i]=NOD2to3.WOE
  else if(train$NumberOfDependents[i]<=5)
    tmp.NOD[i]=NOD3to5.WOE
  else
    tmp.NOD[i]=NODmorethan.WOE
}
table(tmp.NOD)
tmp.NOD[1:10]
train$NumberOfDependents[1:10]
#######WOE DataFrame
trainWOE=cbind.data.frame(tmp.age,tmp.N3059N,tmp.MI,tmp.N6089N,tmp.N90N,tmp.NREL,tmp.NOD)
####gradecard
trainWOE$Default_Status=1-train$Default_Status
glm.fit=glm(Default_Status~.,data=trainWOE,family=binomial(link=logit))
summary(glm.fit)
coe=(glm.fit$coefficients)
p=-20/log(2)
q=600-20*log(15)/log(2)
Score=q+p*(as.numeric(coe[1])+as.numeric(coe[2])*tmp.age+as.numeric(coe[3])*tmp.N3059N+p*as.numeric(coe[4])*tmp.MI+p*as.numeric(coe[5])*tmp.N6089N+p*as.numeric(coe[6])*tmp.N90N+p*as.numeric(coe[7])*tmp.NREL+p*as.numeric(coe[8])*tmp.NOD)
base=q+p*as.numeric((coe[1]))
base
###score
#age
Agelessthan30.SCORE = p*as.numeric(coe[2])*Agelessthan30.WOE
Age30to35.SCORE = p*as.numeric(coe[2])*Age30to35.WOE
Age35to40.SCORE = p*as.numeric(coe[2])*Age35to40.WOE
Age40to45.SCORE = p*as.numeric(coe[2])*Age40to45.WOE
Age45to50.SCORE = p*as.numeric(coe[2])*Age45to50.WOE
Age50to55.SCORE = p*as.numeric(coe[2])*Age50to55.WOE
Age55to60.SCORE = p*as.numeric(coe[2])*Age55to60.WOE
Age60to65.SCORE = p*as.numeric(coe[2])*Age60to65.WOE
Age65to75.SCORE = p*as.numeric(coe[2])*Age65to75.WOE
Agemorethan.SCORE=p*as.numeric(coe[2])*Agemorethan.WOE
Age.SCORE=c(Agelessthan30.SCORE,Age30to35.SCORE,Age35to40.SCORE,Age40to45.SCORE,Age45to50.SCORE,Age50to55.SCORE,Age55to60.SCORE,Age60to65.SCORE,Age65to75.SCORE,Agemorethan.SCORE)
Age.SCORE
#N3059N
N3059Nlessthan30.SCORE = p*as.numeric(coe[2])*N3059Nlessthan0.WOE
N3059N0to1.SCORE = p*as.numeric(coe[2])*N3059N0to1.WOE
N3059N1to3.SCORE = p*as.numeric(coe[2])*N3059N1to3.WOE
N3059N3to5.SCORE = p*as.numeric(coe[2])*N3059N3to5.WOE
N3059Nmorethan.SCORE = p*as.numeric(coe[2])*N3059Nmorethan.WOE
N3059N.SCORE=c(N3059Nlessthan30.SCORE,N3059N0to1.SCORE,N3059N1to3.SCORE,N3059N3to5.SCORE,N3059Nmorethan.SCORE)
N3059N.SCORE
#MI
MIlessthan.SCORE = p*as.numeric(coe[2])*MIlessthan.WOE
MI1to2.SCORE = p*as.numeric(coe[2])*MI1to2.WOE
MI2to3.SCORE = p*as.numeric(coe[2])*MI2to3.WOE
MI3to4.SCORE = p*as.numeric(coe[2])*MI3to4.WOE
MI4to5.SCORE = p*as.numeric(coe[2])*MI4to5.WOE
MI5to6.SCORE = p*as.numeric(coe[2])*MI5to6.WOE
MI6to7.SCORE = p*as.numeric(coe[2])*MI6to7.WOE
MI7to8.SCORE = p*as.numeric(coe[2])*MI7to8.WOE
MI8to9.SCORE = p*as.numeric(coe[2])*MI8to9.WOE
MI9to10.SCORE = p*as.numeric(coe[2])*MI9to10.WOE
MI10to11.SCORE = p*as.numeric(coe[2])*MI10to11.WOE
MI11to12.SCORE = p*as.numeric(coe[2])*MI11to12.WOE
MImorethan.SCORE=p*as.numeric(coe[2])*MImorethan.WOE
MI.SCORE=c(MIlessthan.SCORE,MI1to2.SCORE,MI2to3.SCORE,MI3to4.SCORE,MI4to5.SCORE,MI5to6.SCORE,MI6to7.SCORE,MI7to8.SCORE,MI8to9.SCORE,MI9to10.SCORE,MI10to11.SCORE,MI11to12.SCORE,MImorethan.SCORE)
MI.SCORE
#N90N
N90Nlessthan.SCORE = p*as.numeric(coe[2])*N90Nlessthan.WOE
N90N0to1.SCORE = p*as.numeric(coe[2])*N90N0to1.WOE
N90N1to3.SCORE = p*as.numeric(coe[2])*N90N1to3.WOE
N90N3to5.SCORE = p*as.numeric(coe[2])*N90N3to5.WOE
N90N5to10.SCORE = p*as.numeric(coe[2])*N90N5to10.WOE
N90Nmorethan.SCORE = p*as.numeric(coe[2])*N90Nmorethan.WOE
N90N.SCORE=c(N90Nlessthan.SCORE,N90N0to1.SCORE,N90N1to3.SCORE,N90N3to5.SCORE,N90N5to10.SCORE,N90Nmorethan.SCORE)
N90N.SCORE
#NREL
NRELlessthan.SCORE = p*as.numeric(coe[2])*NRELlessthan.WOE
NREL0to1.SCORE = p*as.numeric(coe[2])*NREL0to1.WOE
NREL1to2.SCORE = p*as.numeric(coe[2])*NREL1to2.WOE
NREL2to3.SCORE = p*as.numeric(coe[2])*NREL2to3.WOE
NREL3to5.SCORE = p*as.numeric(coe[2])*NREL3to5.WOE
NRELmorethan.SCORE = p*as.numeric(coe[2])*NRELmorethan.WOE
NREL.SCORE=c(NRELlessthan.SCORE,NREL0to1.SCORE,NREL1to2.SCORE,NREL2to3.SCORE,NREL3to5.SCORE,NRELmorethan.SCORE)
NREL.SCORE
#N6089N
N6089Nlessthan.SCORE = p*as.numeric(coe[2])*N6089Nlessthan.WOE
N6089N0to1.SCORE = p*as.numeric(coe[2])*N6089N0to1.WOE
N6089N1to3.SCORE = p*as.numeric(coe[2])*N6089N1to3.WOE
N6089N3to5.SCORE = p*as.numeric(coe[2])*N6089N3to5.WOE
N6089Nmorethan.SCORE = p*as.numeric(coe[2])*N6089Nmorethan.WOE
N6089N.SCORE=c(N6089Nlessthan.SCORE,N6089N0to1.SCORE,N6089N1to3.SCORE,N6089N3to5.SCORE,N6089Nmorethan.SCORE)
N6089N.SCORE
#NOD
NODlessthan.SCORE = p*as.numeric(coe[2])*NODlessthan.WOE
NOD0to1.SCORE = p*as.numeric(coe[2])*NOD0to1.WOE
NOD1to2.SCORE = p*as.numeric(coe[2])*NOD1to2.WOE
NOD2to3.SCORE = p*as.numeric(coe[2])*NOD2to3.WOE
NOD3to5.SCORE = p*as.numeric(coe[2])*NOD3to5.WOE
NODmorethan.SCORE = p*as.numeric(coe[2])*NODmorethan.WOE
NOD.SCORE=c(NODlessthan.SCORE,NOD0to1.SCORE,NOD1to2.SCORE,NOD2to3.SCORE,NOD3to5.SCORE,NODmorethan.SCORE)
NOD.SCORE
##score fuction
getscore=function(i,x){
  score=round(p*as.numeric(coe[i])*x,0)
  return(score)
}
#age
Agelessthan30.SCORE = getscore(2,Agelessthan30.WOE)
Age30to35.SCORE = getscore(2,Age30to35.WOE)
Age35to40.SCORE = getscore(2,Age35to40.WOE)
Age40to45.SCORE = getscore(2,Age40to45.WOE)
Age45to50.SCORE = getscore(2,Age45to50.WOE)
Age50to55.SCORE = getscore(2,Age50to55.WOE)
Age55to60.SCORE = getscore(2,Age55to60.WOE)
Age60to65.SCORE = getscore(2,Age60to65.WOE)
Age65to75.SCORE = getscore(2,Age65to75.WOE)
Agemorethan.SCORE = getscore(2,Agemorethan.WOE)
Age.SCORE = c(Agelessthan30.SCORE,Age30to35.SCORE,Age35to40.SCORE,Age40to45.SCORE,Age45to50.SCORE,Age50to55.SCORE,Age55to60.SCORE,Age60to65.SCORE,Age65to75.SCORE,Agemorethan.SCORE)
Age.SCORE
#N3059N
N3059Nlessthan0.SCORE = getscore(2,N3059Nlessthan0.WOE)
N3059N0to1.SCORE = getscore(2,N3059N0to1.WOE)
N3059N1to3.SCORE = getscore(2,N3059N1to3.WOE)
N3059N3to5.SCORE = getscore(2,N3059N3to5.WOE)
N3059Nmorethan.SCORE = getscore(2,N3059Nmorethan.WOE)
N3059N.SCORE=c(N3059Nlessthan0.SCORE,N3059N0to1.SCORE,N3059N1to3.SCORE,N3059N3to5.SCORE,N3059Nmorethan.SCORE)
N3059N.SCORE
#MI
MIlessthan.SCORE = getscore(2,MIlessthan.WOE)
MI1to2.SCORE = getscore(2,MI1to2.WOE)
MI2to3.SCORE = getscore(2,MI2to3.WOE)
MI3to4.SCORE = getscore(2,MI3to4.WOE)
MI4to5.SCORE = getscore(2,MI4to5.WOE)
MI5to6.SCORE = getscore(2,MI5to6.WOE)
MI6to7.SCORE = getscore(2,MI6to7.WOE)
MI7to8.SCORE = getscore(2,MI7to8.WOE)
MI8to9.SCORE = getscore(2,MI8to9.WOE)
MI9to10.SCORE = getscore(2,MI9to10.WOE)
MI10to11.SCORE = getscore(2,MI10to11.WOE)
MI11to12.SCORE = getscore(2,MI11to12.WOE)
MImorethan.SCORE= getscore(2,MImorethan.WOE)
MI.SCORE=c(MIlessthan.SCORE,MI1to2.SCORE,MI2to3.SCORE,MI3to4.SCORE,MI4to5.SCORE,MI5to6.SCORE,MI6to7.SCORE,MI7to8.SCORE,MI8to9.SCORE,MI9to10.SCORE,MI10to11.SCORE,MI11to12.SCORE,MImorethan.SCORE)
MI.SCORE
#N90N
N90Nlessthan.SCORE = getscore(2,N90Nlessthan.WOE)
N90N0to1.SCORE = getscore(2,N90N0to1.WOE)
N90N1to3.SCORE = getscore(2,N90N1to3.WOE)
N90N3to5.SCORE = getscore(2,N90N3to5.WOE)
N90N5to10.SCORE = getscore(2,N90N5to10.WOE)
N90Nmorethan.SCORE = getscore(2,N90Nmorethan.WOE)
N90N.SCORE=c(N90Nlessthan.SCORE,N90N0to1.SCORE,N90N1to3.SCORE,N90N3to5.SCORE,N90N5to10.SCORE,N90Nmorethan.SCORE)
N90N.SCORE
#N6089N
N6089Nlessthan.SCORE = getscore(2,N6089Nlessthan.WOE)
N6089N0to1.SCORE = getscore(2,N6089N0to1.WOE)
N6089N1to3.SCORE = getscore(2,N6089N1to3.WOE)
N6089N3to5.SCORE = getscore(2,N6089N3to5.WOE)
N6089Nmorethan.SCORE = getscore(2,N6089Nmorethan.WOE)
N6089N.SCORE=c(N6089Nlessthan.SCORE,N6089N0to1.SCORE,N6089N1to3.SCORE,N6089N3to5.SCORE,N6089Nmorethan.SCORE)
N6089N.SCORE
#NREL
NRELlessthan.SCORE = getscore(2,NRELlessthan.WOE)
NREL0to1.SCORE = getscore(2,NREL0to1.WOE)
NREL1to2.SCORE = getscore(2,NREL1to2.WOE)
NREL2to3.SCORE = getscore(2,NREL2to3.WOE)
NREL3to5.SCORE = getscore(2,NREL3to5.WOE)
NRELmorethan.SCORE = getscore(2,NRELmorethan.WOE)
NREL.SCORE=c(NRELlessthan.SCORE,NREL0to1.SCORE,NREL1to2.SCORE,NREL2to3.SCORE,NREL3to5.SCORE,NRELmorethan.SCORE)
NREL.SCORE
#NOD
NODlessthan.SCORE = getscore(2,NODlessthan.WOE)
NOD0to1.SCORE = getscore(2,NOD0to1.WOE)
NOD1to2.SCORE = getscore(2,NOD1to2.WOE)
NOD2to3.SCORE = getscore(2,NOD2to3.WOE)
NOD3to5.SCORE = getscore(2,NOD3to5.WOE)
NODmorethan.SCORE = getscore(2,NODmorethan.WOE)
NOD.SCORE=c(NODlessthan.SCORE,NOD0to1.SCORE,NOD1to2.SCORE,NOD2to3.SCORE,NOD3to5.SCORE,NODmorethan.SCORE)
NOD.SCORE