library(e1071)#Package for regression method
library(rpart)#Packages for decision tree method
library(psych)#Packages for principal component analysis
library(rgl)#Packages for plot 3D clustering image
library(corrplot)#Packages for plot Correlation image
library(pROC)#Packages used to draw ROC curves


getwd()#seek workspace
rawdata=read.csv(file = "AdjustedData.csv")# read data in workspace



copydata=rawdata

#Count the number of missing values in each row
f<-function(x) sum(x==0)
NaCount=apply(copydata,2,f)
for (i in 1:ncol(copydata)) {
  if (is.na(NaCount[i])) {
    NaCount[i]=0
  }
}
#Calculate total price
copydata$room_price=copydata$room_price+copydata$service_fee
copydata$service_fee=NA
copydata$room_price=copydata$room_price+copydata$cleaning_fee
copydata$cleaning_fee=NA

#Set selection scale lower limit
Lower_limit=0.2
#Select columns that meet the lower limit to compose new data
data=data.frame(matrix(nrow = nrow(copydata),ncol=0))
for (i in 1:ncol(copydata)) {
  if (NaCount[i]>as.integer((nrow(copydata)*(1-Lower_limit)))) {
    copydata[,i]=NA
  }
}
data = copydata[,-which(apply(copydata,2,function(x) all(is.na(x))))]

#Row processing: delete rows with missing values
data<-data[-which(apply(data,1,function(x) any(is.na(x)))),]
barplot(data$room_price)


#Linear regression analysis

##Create data
lmdata=data
##Delete category variable line
lmdata$type_airbnb=NA
lmdata = lmdata[,-which(apply(lmdata,2,function(x) all(is.na(x))))]
##Training model
mylm <- lm(lmdata$room_price ~., data = lmdata)
##summary(mylm)
##Forecast results
predict1=predict(mylm,lmdata,type = "response")
lmdata$predict=predict(mylm,lmdata,tpredictpe = "response")
lmdata$predict[which((lmdata$predict >=lmdata$room_price*0.5) &( lmdata$predict <=lmdata$room_price*2) )]=1
lmdata$predict[which(lmdata$predict!=1)]=0
##accuracy
"accuracy";sum(lmdata$predict)/nrow(lmdata)
##Adjust drawing area
par(mfrow=c(2,2))
##Draw regression analysis chart
plot(mylm)



#Decision tree analysis

##Create data
treedata=data
##Delete category variable line
treedata$type_airbnb=NA
treedata = treedata[,-which(apply(treedata,2,function(x) all(is.na(x))))]
##Training model
mytree <- rpart(room_price ~ ., data =treedata)
##Forecast results
treedata$predict=predict=predict(mytree,treedata)


#Algorithm test--ROC
##Extract ROC data
roc1=roc(lmdata$predict,lmdata$room_price)
roc2=roc(treedata$predict,treedata$room_price)
##Adjust drawing area
par(mfrow=c(1,1))
##Draw ROC image
plot(roc1,col='blue',print.auc=TRUE,print.auc.x=0.4,print.auc.y=0.4,auc.polygon=TRUE,auc.polygon.col="gray",
     smooth=TRUE,
     grid=c(0.5, 0.2),
     grid.col=c("black", "black"), 
     max.auc.polygon=TRUE,
     lty=1,main=" ROC Curve",mfrow=c(1,1)) 
plot.roc(roc2,add=T,col="red", print.auc=TRUE,print.auc.x=0.3,print.auc.y=0.3)


#Correlation
cordata=data
#Delete category variable line
cordata$type_airbnb=NA
cordata = cordata[,-which(apply(cordata,2,function(x) all(is.na(x))))]
#Normalize data
cordata=scale(cordata)
#Calculation of correlation coefficient matrix
cor_=cor(cordata,use = "all.obs",method = "pearson")#pearson,spearman,kendall
#Draw correlation image
corrplot(cor_,tl.cex = 0.7)


#principal component analysis
fa=fa(cor_,nfactors = 3,n.obs = nrow(cordata),scores = T,)
#Drawing principal component images
factor.plot(fa)
#Extract main components
fawei=fa$weights
fawei=data.frame(fawei)
fawei
##MR1
mr1weig1=fawei$MR1
mr1weig1
matrixdata=as.matrix(cordata)
matrixmr1weig=as.matrix(mr1weig1)
fadata1=matrixdata%*%matrixmr1weig
summary(fadata1)
##MR2
mr2weig2=fawei$MR2
mr2weig2
matrixdata=as.matrix(cordata)
matrixmr2weig=as.matrix(mr2weig2)
fadata2=matrixdata%*%matrixmr2weig
summary(fadata2)
##MR3
mr3weig3=fawei$MR3
mr3weig3
matrixdata=as.matrix(cordata)
matrixmr3weig=as.matrix(mr3weig3)
fadata3=matrixdata%*%matrixmr3weig
summary(fadata3)

##Put the extracted principal component in the data frame
fadata=data.frame(fadata1,fadata2,fadata3)


#cluster
##Copy data
clusterdata=fadata
#Set random number seed
set.seed(1234)
##Clustering with kmeans method
cluster_n3=kmeans(clusterdata,2)
##Normalize data
clusterdata=data.frame(scale(clusterdata))
cluster_n3$cluster
##Compare cluster data with normalized data
cluster_plot_data=cbind(clusterdata,cluster_n3$cluster)
##Draw clustering image
cluster_plot=plot3d(cluster_plot_data$fadata1,cluster_plot_data$fadata2,cluster_plot_data$fadata3,xlab = 'Equipment and Service',ylab='Basic conditions',zlab='Price',col=cluster_plot_data$`cluster_n3$cluster`,type='p',size=9)



