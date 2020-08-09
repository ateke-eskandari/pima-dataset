library(faraway)
data(pima)
d<-pima
head(d,5)
dim(d)
nrow(d)
d[768,]
summary(d)
d$glucose==0
sum(d$glucose==0)
d$diastolic[d$diastolic==0]=NA
d$glucose[d$glucose==0]=NA
d$triceps[d$triceps==0]=NA
d$bmi[d$bmi==0]=NA
d$insulin[d$insulin==0]=NA
summary(d)
hist(d$diastolic,xlab="Diastolic",main="",col = "green",border="red")
boxplot(d$diastolic,ylab="Diastolic",main="",col="blue")
d$test<-factor(d$test)
levels(d$test)
levels(d$test)=c("negative","positive")
negcount=sum(d$test=='negative')
poscount=sum(d$test=='positive')
x<-c(sum(d$test=="positive"), sum(d$test=="negative"))
library(plotrix)
pie3D(x,labels = levels(d$test),explode = 0.1, main = "Pie Chart of Test
Results")
pie(x,labels = levels(d$test), main = "Pie Chart of Test
Results")
plot(d$bmi,d$triceps,xlab="BMI",ylab="Triceps",main="")
plot(d$test,d$diastolic,ylab="Diastolic",main="")
pairs(d[,c("glucose","bmi","triceps","insulin")])
par(mfrow=c(2,2))
plot(d$bmi,d$triceps,xlab="BMI",ylab="Triceps",main="")
plot(d$test,d$diastolic,ylab="Diastolic",main="")
hist(d$diastolic,xlab="Diastolic",main="",col = "green",border = "red")
library(plotrix)
lbl=levels(d$test)
pie3D(x,labels = lbl,explode = 0.1, main = "Pie Chart of Test Results")
library(mice)
library(VIM)
aggr<- aggr(d, col=c('black','red'), numbers=TRUE, sortVars=TRUE,
            labels=names(d), cex.axis=.7, gap=1, ylab=c("Barplot of missing data","Patterns"))
md.pattern(d)
marginplot(d[,c(2,5)])
marginplot(d[,c(4,5)])
marginplot(d[,c(8,4)])
missnum<-function(x){a<-sum(is.na(x))/length(x); return(a)}
missnum(head(d,1))
miss<-apply(d,1,missnum)
which(miss>0.3)
miss[which(miss>0.3)]
mean(miss[which(miss>0.3)])
miss[which.max(miss)]
miss[which(miss==max(miss))]
sum(miss>0.1 & miss<0.3)
mean(d$age)
mean(d$age[is.na(d$triceps)])
mean(d$age[is.finite(d$triceps)])
tapply(d$age,is.na(d$triceps),mean)
mean(d$diastolic[d$test==0],na.rm=T)
mean(d$diastolic[d$test==1],na.rm=T)
tapply(d$diastolic[is.finite(d$diastolic)],d$test[is.finite(d$diastolic)],mean)

impu=mice(d,m=5,meth='pmm')
impu$imp$bmi
com=complete(impu,1)
head(com)
head(d)
xyplot(impu, insulin ~ triceps+glucose+diastolic| .imp)
stripplot(impu)
densityplot(impu)
