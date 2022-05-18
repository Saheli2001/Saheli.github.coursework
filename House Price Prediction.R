rm(list=ls())
library(ggplot2)
library(MASS)
library(reshape2)
library(tidyverse)
library(lattice)
library(caret)
library(car)
library(trafo)
library(moments)
library(lmtest)
library(fastDummies)
library(DescTools)
library(ppcor)
library(dgof)
library(corpcor)
library(gridExtra)
library(factoextra)
par(mfrow=c(1,1))
data_new = readxl::read_xlsx('C:/Users/Saheli/Desktop/dissertation/project data.xlsx')
attach(data_new)
nrow(data_new)

#analysis of the variables
#--------------------------------------------
summary(SalePrice)
ggplot(data=data_new,aes(x=SalePrice,y=..density..))+
  geom_histogram(bins=50,fill='orange')+
  labs(title = 'Histogram of Sale Price of
       House',x='Sale Price of House')+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))

qqPlot(data_new$SalePrice, main =
         'qqplot of saleprice',xlab='Theoretical
       normal constants',
       ylab='Sample quantiles of Sale Price')

#Analysis of Predictors
#-----------------------------------------------
#1.GrlivArea
ggplot(data=data_new,aes(x=GrLivArea,y=..density..))+
  geom_histogram(bins=50,fill='orange')+
  labs(title = 'Histogram of General
       Living Area ("GrLivArea") of House',
       x='GrLivArea(in sqft)')+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))
#2. LotArea
ggplot(data=data_new,aes(x=LotArea,y=..density..))+
  geom_histogram(bins=50,fill='orange')+
  labs(title = 'Histogram of Area of the
       Property ("LotArea") of House',
       x='LotArea(in sqft)')+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))
#3. BedroomAbvGr
ggplot(data=data_new,aes(y=BedroomAbvGr))+
  geom_boxplot(fill='orange')+
  labs(title = 'Boxplot of Number of bedrooms
       ("BedroomAbvGr") of House',
       x='BedroomAbvGr',y='Number of Rooms')+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))

#missing value percentage
#---------------------------------------
group=missval=array(0)
for(i in 1:ncol(data_new))
{
  count = 0
  for(j in 1:nrow(data_new))
  {
  if(data_new[j,i]=='NA'| is.na(data_new[,i]
                                )[j]== 'TRUE')
  {
    count = count+1
  }
  }
  group[i] = colnames(data_new)[i]
  missval[i] = count*100/nrow(data_new)
}
m= data.frame(group1=factor(group,levels = group),missval)
miss.val.ratio = ( m %>% arrange(m$missval))


ggplot(miss.val.ratio[-(1:60),],aes(x=group1,y=missval))+
  geom_col(fill='black')+
  labs(title = 'Missing Value Percentage',
       x='Housing Parameters',
       y='Percentage of missing values')+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))
#relationships between predictor and response
#----------------------------------------
ggplot(data=NULL,aes(x=GrLivArea,y=SalePrice))+
  geom_point()+
  labs(title = 'Scatterplot of Sale Price of House
       vs General Living Area (in sqft)',x='General
       Living Area (in sqft)',y='Sale Price
       of House(in dollars)')+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))
ggplot(data=NULL,aes(x=LotArea,y=SalePrice))+
  geom_point()+
  labs(title = 'Scatterplot of Sale Price of House
       vs Area of the property(in sqft)',x=
       'General Living Area
       (in sqft)',y='Sale Price of House(in
       dollars)')+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))

ggplot(data=NULL,aes(x=as.factor(BedroomAbvGr),y=SalePrice
                     ,fill=BedroomAbvGr))+
  geom_boxplot()+
  labs(title = 'Boxplot of Sale Price of House
       with respect to number of bedrooms',
       x='Number of bedrooms',
       y='Sale Price of House(in dollars)')+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))

#Imputing Missing Val
#----------------------------------------
data_new$PoolQC[data_new$PoolQC=='NA']= 'None'
data_new$MiscFeature[data_new$MiscFeature=='NA']= 'None'
data_new$Alley[data_new$Alley=='NA']= 'None'
data_new$Fence[data_new$Fence=='NA']= 'None'
data_new$FireplaceQu[data_new$FireplaceQu=='NA']= 'None'
data_new$LotFrontage[data_new$LotFrontage=='NA']= 'None'

data_new$LotFrontage= as.numeric(data_new$LotFrontage)
c = which(is.na(data_new$LotFrontage)== 'TRUE')
a = aggregate(data_new$LotFrontage[-c],by=
                list(data_new$Neighborhood[-c]),median)
b = data_new$Neighborhood[c]
d = array(0)
for(i in 1:length(b))
{ for(j in 1:nrow(a))
{
  if(b[i]==a$Group.1[j]){
    d[i]= a$x[j]
  }}}

data_new$LotFrontage[c] = d #missing values replaced by median

data_new$GarageCond[data_new$GarageCond=='NA']= 'None'
data_new$GarageQual[data_new$GarageQual=='NA']= 'None'
data_new$GarageType[data_new$GarageType=='NA']= 'None'
data_new$GarageArea[is.na(data_new$GarageArea)== TRUE]= 0
data_new$GarageFinish[data_new$GarageFinish=='NA']= 'None'

data_new$GarageYrBlt = as.numeric(data_new$GarageYrBlt)
data_new$GarageYrBlt[is.na(data_new$GarageYrBlt)== TRUE]= 0
data_new$GarageCars = as.numeric(data_new$GarageCars)
data_new$GarageCars[is.na(data_new$GarageCars)== TRUE]= 0
data_new$BsmtFinSF1 = as.numeric(data_new$BsmtFinSF1)
data_new$BsmtFinSF1[is.na(data_new$BsmtFinSF1)== TRUE]= 0
data_new$BsmtFinSF2 = as.numeric(data_new$BsmtFinSF2)
data_new$BsmtFinSF2[is.na(data_new$BsmtFinSF2)== TRUE]= 0
data_new$BsmtUnfSF = as.numeric(data_new$BsmtUnfSF)
data_new$BsmtUnfSF[is.na(data_new$BsmtUnfSF)== TRUE]= 0
data_new$TotalBsmtSF = as.numeric(data_new$TotalBsmtSF)
data_new$TotalBsmtSF[is.na(data_new$TotalBsmtSF)== TRUE]= 0
data_new$BsmtFullBath = as.numeric(data_new$BsmtFullBath)
data_new$BsmtFullBath[is.na(data_new$BsmtFullBath)== TRUE]= 0
data_new$BsmtHalfBath = as.numeric(data_new$BsmtHalfBath)
data_new$BsmtHalfBath[is.na(data_new$BsmtHalfBath)== TRUE]= 0

data_new$BsmtQual[data_new$BsmtQual=='NA']= 'None'
data_new$BsmtCond[data_new$BsmtCond=='NA']= 'None'
data_new$BsmtExposure[data_new$BsmtExposure=='NA']= 'None'
data_new$BsmtFinType1[data_new$BsmtFinType1=='NA']= 'None'
data_new$BsmtFinType2[data_new$BsmtFinType2=='NA']= 'None'
data_new$YearBuilt=as.factor(data_new$YearBuilt)


data_new$MasVnrType[data_new$MasVnrType=='NA']= 'None'
data_new$MasVnrArea = as.numeric(data_new$MasVnrArea)
data_new$MasVnrArea[is.na(data_new$MasVnrArea)== TRUE]= 0

data_new$MSZoning[data_new$MSZoning=='NA']= 'RL'
data_new$Utilities[data_new$Utilities=='NA']= 'None'
data_new$Functional[data_new$Functional=='NA']= 'Typ'
data_new$KitchenQual[data_new$KitchenQual=='NA']= 'TA'
data_new$Exterior1st[data_new$Exterior1st=='NA']= 'Sdng'
data_new$Exterior2nd[data_new$Exterior2nd=='NA']= 'VinylSd'
data_new$SaleType[data_new$SaleType=='NA']= 'WD'
data_new$MSSubClass[data_new$MSSubClass=='NA']= 'None'

any(is.na(data_new))  #no missing values

#feature engineering
#------------------------------------------------
data_new$MSSubClass = as.factor(data_new$MSSubClass)
data_new$OverallCond = as.factor(data_new$OverallCond)
data_new$OverallQual = as.factor(data_new$OverallQual)
data_new$YrSold = as.factor(data_new$YrSold)
data_new$MoSold = as.factor(data_new$MoSold)

summary(data_new)

#correlation heatmap
#--------------------------------------------
data.na.omit = na.omit(data_new[,-80])
corr = data.matrix(cor(data.na.omit[sapply(data.na.omit,
                                           is.numeric)]))
mel = melt(corr)
ggplot(mel, aes(Var1,Var2))+geom_tile(aes(fill=value)) +
  geom_text(aes(label = round(value, 1)))+
  scale_fill_gradient2(low='blue',mid = 'White' ,high='red')
+ labs(title = 'Correlation Heatmap')

#skewed freatures
#--------------------------------------------
sk =name =array(0)
for(i in 1:ncol(data_new[sapply(data_new,is.numeric)]))
{


  sk[i] = skewness(data_new[sapply(data_new,is.numeric)][,i])
  name[i] = colnames(data_new[sapply(data_new,is.numeric)])[i]
}
 t = data.frame(name, sk)
 t1 = t %>% arrange(t$sk)

writexl::write_xlsx(t1,'C:/Users/Saheli/
                    Desktop/dissertation/
                    Latex Dissertation/skewness.xlsx')

#Partial Correlation heatmap
#------------------------------------------------
partial.cor_new = corpcor::cor2pcor(cov(
  data_new[,which(sapply(data_new[,-c(80,81)],is.numeric))]))


colnames(partial.cor_new)=colnames(
  data_new[,which(sapply(data_new[,-c(80,81)],is.numeric))])
rownames(partial.cor_new)=colnames(
  data_new[,which(sapply(data_new[,-c(80,81)],is.numeric))])
mel.partial_new = melt(data.matrix(partial.cor_new))
ggplot(mel.partial_new, aes(Var1,Var2))+geom_tile(
  aes(fill=value)) +
  geom_text(aes(label = round(value, 1)))+
  scale_fill_gradient2(low='blue' ,mid='white',high='red')
+ labs(title = 'Partial Correlation Heatmap')

#transforming skewed features
#--------------------------------------------------
data_new$GarageArea = log(max(data_new$GarageArea)
                          +data_new$GarageArea )
skewness(data_new$GarageArea)
data_new$LotFrontage =sqrt(data_new$LotFrontage)
skewness(data_new$LotFrontage)
data_new$FullBath =log(max(FullBath)+data_new$FullBath)
skewness(data_new$FullBath)
data_new$BedroomAbvGr =log(max(data_new$BedroomAbvGr)
                           +data_new$BedroomAbvGr)
skewness(data_new$BedroomAbvGr)
data_new$LotArea=sqrt(data_new$LotArea)
skewness(data_new$LotArea)
data_new$GrLivArea=log(data_new$GrLivArea)
skewness(data_new$GrLivArea)
data_new$YearRemodAdd=log(data_new$YearRemodAdd)
skewness(data_new$YearRemodAdd)
data_new$SalePrice=log(data_new$SalePrice)
skewness(data_new$SalePrice)
skewness(SalePrice)
skewness(sqrt(data_new$WoodDeckSF))
skewness(sqrt(data_new$MasVnrArea))
skewness(sqrt(data_new$ScreenPorch))
skewness(sqrt(data_new$LowQualFinSF))
skewness(sqrt(data_new$MiscVal))
skewness(sqrt(data_new$PoolArea))
skewness(sqrt(data_new$OpenPorchSF))
skewness(sqrt(data_new$EnclosedPorch))

#PCA of whole data
data.pca = prcomp(data_new[,-c(80,81)][ , unlist(
  lapply(data_new[,-c(80,81)], is.numeric)) ] ,
  center = TRUE,scale. = TRUE)
summary(data.pca)
trans.data1 <- preProcess(data_new[,-c(80,81)], method  = "pca")
PC = as.data.frame(trans.data1$rotation)

writexl::write_xlsx(PC,'C:/Users/Saheli/Desktop /dissertation/Latex Dissertation/PCwholedata.xlsx')
transformedData1 <- predict(trans.data1,data_new[,-c(80,81)] )
nrow(transformedData1)
colnames(transformedData1)
factoextra::fviz_eig(data.pca,ncp= 22,
                     xlab='Principal Components')
age.pca = as.numeric(data_new$YrSold)-
  as.numeric(data_new$YearBuilt)
indep.pca = cbind(select(transformedData1,
                         -one_of(

                   'YearBuilt','YrSold')),age.pca)
fit.pca1 = lm(data_new$SalePrice~.,indep.pca)
options(max.print = 2000)
summary(fit.pca1)
fit.pca1.data = broom::tidy(fit.pca1)
writexl::write_xlsx(fit.pca1.data,'C:/Users/Saheli/Desktop/dissertation/Latex Dissertation/summarypca1.xlsx')

residualPlot(fit.pca1, main = 'Residual Plot')
qqPlot(resid(fit.pca1), main = 'Q-Q Plot',
       xlab = 'Theoretical Normal Quantiles',
       ylab = 'Observed Residual Quantiles')
hist(resid(fit.pca1),freq=F)

#correlation between errors
DurbinWatsonTest(fit.pca1,alternative='two.sided')

#checking for normality- Shapiro-Wilk test, qqplot
shapiro.test(resid(fit.pca1))

qqPlot(resid(fit.pca1))

#PCA excluding skewed features
dd.pca.out = subset(data_new,
                    select = -c(
                    `3SsnPorch`,LowQualFinSF,MiscVal,
                    PoolArea, PoolQC,
                    KitchenAbvGr,EnclosedPorch,OpenPorchSF,
                    ScreenPorch,BsmtHalfBath,WoodDeckSF,
                    OverallQual,MoSold,
                    BsmtFinSF2,MasVnrArea,GarageYrBlt,id,
                   YearBuilt,  SalePrice,YrSold))
age.pca.out = as.numeric(data_new$YrSold)-
  as.numeric(data_new$YearBuilt)
dd.pca1.out = cbind(dd.pca.out,age.pca.out)
prcomp(dd.pca1.out[ , unlist(lapply(
  dd.pca1.out, is.numeric)) ] ,
  center = TRUE,scale. = TRUE)
trans.data.out <- preProcess(dd.pca1.out[ ,
    unlist(lapply(dd.pca1.out, is.numeric)) ] ,
    method  = "pca")
transformedData.out <- predict(trans.data.out, dd.pca1.out)
trans.data.out$rotation
PC.out= as.data.frame(trans.data.out$rotation)
rownames(PC.out)
writexl::write_xlsx(PC.out,'C:/Users/Saheli/
                    Desktop/dissertation/Latex Dissertation
                    /PC.outwholedata.xlsx')
fit.pca.out = lm(data_new$SalePrice~.,
                 data=transformedData.out)
summary(fit.pca.out)
fit.pca.out.data = broom::tidy(fit.pca.out)
writexl::write_xlsx(fit.pca.out.data,'C:/
                    Users/Saheli/Desktop/
                    dissertation/Latex Dissertation
                    /summarypca2.xlsx')

residualPlot(fit.pca.out, main ='Residual Plot')
residualPlots(fit.pca.out)

qqPlot(resid(fit.pca.out), main = 'Q-Q Plot',
       xlab = 'Theoretical Normal Quantiles',
       ylab = 'Observed Residual Quantiles')
shapiro.test(resid(fit.pca.out))
DurbinWatsonTest(fit.pca.out,alternative = 'two.sided')
summary(resid(fit.pca.out))
resi.pca.out =  SalePrice - exp(predict(fit.pca.out))
data.frame(Actual_Price= SalePrice,
           Fitted_Price = exp(predict(fit.pca.out)))
summary(resi.pca.out)
summary(exp(predict(fit.pca.out)))
A = data.frame(id,actual_price =SalePrice,
               Fitted_price=exp(predict(fit.pca.out)))

p1 = ggplot(data=NULL,aes(x=SalePrice,y=..density..))+
  geom_histogram(bins=50,fill='orange')+
  labs(title = 'Histogram of Actual House Price',
       x='Actual House Price(in Dollars)')+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))
p2 = ggplot(data=NULL,aes(x=exp(predict(
  fit.pca.out)),y=..density..))+
  geom_histogram(bins=50,fill='orange')+
  labs(title = 'Histogram of Predicted House
       Price',x='Predicted House Price(in Dollars)')+
  theme(plot.title =
          element_text(size=
                         16,
                       hjust=.5,face='bold'),
        plot.subtitle = element_text(
          size=14,hjust=.5,face='italic'
        ),
        legend.title = element_text(hjust=.5),
        axis.title = element_text(face='bold'),
        axis.text=element_text(face='bold'))

grid.arrange(p1,p2,ncol=2)

coeff.pval = data.frame(summary(fit.pca.out)$coefficients[,c(1,4)] )
coeff.sig = data.frame(signi.pred=rownames(coeff.pval[which(coeff.pval[,2]<0.05),]),oneunitchange=mean(SalePrice)*exp(coeff.pval[which(coeff.pval[,2]<0.05),][,1])-mean(SalePrice))

writexl::write_xlsx(coeff.sig,'C:/Users/Saheli/Desktop/dissertation/Latex Dissertation/significantpreds.xlsx')
