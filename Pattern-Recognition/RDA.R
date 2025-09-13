rm(list=ls())
set.seed(seed=2023)
## Calling packages-----------------
library(lattice)
library(MASS)
library(ggplot2)
library(latticeExtra)
library(gridExtra)
library(MVN)
library(biotools)
library(caret)
library(klaR)

## calling the dataset---------------
data=iris
colnames(data)
nrow(data)
ncol(data)
## EDA of the dataset---------------------
summary(data)

#cutom functions
plot_custom = function(data,plot){
  if(plot=='boxplot'){
    box = list()
    for(i in 1:4)
    {box[[i]] = bwplot(as.matrix(data[i]),xlab=paste(colnames(data)[i]))
    }
    do.call(grid.arrange, c(box, ncol = 4,nrow=1, 
                            top='Boxplot of Individual Variable') )
  }else if(plot=='density'){
    plist.j = list()
    for(i in 1:4)
    {
      plist.j[[i]] = densityplot(~scale(data[,i]),data=data,
                                 xlab=paste(colnames(data)[i]),plot.points=F,rev=F)+
        layer(panel.curve(dnorm(x),lty=2))
    }
    do.call(grid.arrange, c(plist.j, ncol = 4,nrow=1, 
                            top='Density Plot of Indviidual'))
    
  }else if(plot=='heatmap'){
    corr = data.matrix(cor(data[,-5]) )
    mel = melt(corr)
    ggplot(mel, aes(X1,X2))+geom_tile(aes(fill=value)) +
      geom_text(aes(label = round(value, 1)))+
      scale_fill_gradient2(low='red',mid = 'white' ,high='maroon')+
      labs(title="Correlation Heatmap")
  }else if(plot=='qq'){
    plist_qq.j = list()
    for(i in 1:4){
      plist_qq.j[[i]]=qqmath(~ scale(data[,i]),data = data,
                             prepanel = prepanel.qqmathline,
                             panel = function(x, ...) {
                               panel.qqmathci(x, ...)
                               panel.qqmathline(x, ...)
                               panel.qqmath(x, ...)
                             },pch=19,xlab="Theortical Quantile of N(0,1)",
                             ylab="Observed Quantiles",
                             main=paste(colnames(data)[i]))
    }
    do.call(grid.arrange, c(plist_qq.j, ncol = 4,nrow=1, 
                            top="QQPlot of Individual Variables vs N(0,1)") )
    
  }
}
shapiro.table=function(data){
  Shap1=matrix(0,nrow=4,ncol=3)
  Shap1[,1]=colnames(data)[-5]
  for(i in 1:4)
  {
    Shap1[i,2]=shapiro.test(data[,i])$p
  }
  Shap1[,3]=ifelse(as.numeric(Shap1[,2])<0.01,"Reject","Accept")
  colnames(Shap1)=c("Variable","p value","Decision")
  return(Shap1)
}

##Not considering any group
#Summary
summary(data[,-5])

# Density plot of individual variable 
plot_custom(data,plot='density')

## GROUPING 
data_s = data[which(data$Species=='setosa'),]
data_vc = data[which(data$Species=='versicolor'),]
data_vg = data[which(data$Species=='virginica'),]

##EDA within the species------------------------------------
## GROUP - SETOSA
#Summary
summary(data_s[,-5])
#Plots
plot_custom(data_s,plot='boxplot')
plot_custom(data_s,plot='heatmap')

## GROUP - VERSICOLOR
#Summary
summary(data_vc[,-5])
#Plots
plot_custom(data_vc,plot='boxplot')
plot_custom(data_vc,plot='heatmap')

## GROUP - VIRGINICA
#Summary
summary(data_vg[,-5])
#Plots
plot_custom(data_vg,plot='boxplot')
plot_custom(data_vg,plot='heatmap')


##Normality Checking-------------------

## GROUP - SETOSA
# Density plot 
plot_custom(data_s,plot='density')
# QQplot 
plot_custom(data_s,plot='qq')
# Roystone and Shapiro test
shapiro.table(data_s)
mvn(data_s[,-5],mvnTest = "royston")$multivariateNormality

## GROUP - VERSICOLOR
# Density plot 
plot_custom(data_vc,plot='density')
# QQplot 
plot_custom(data_vc,plot='qq')
# Roystone and Shapiro test
shapiro.table(data_vc)
mvn(data_vc[,-5],mvnTest = "royston")$multivariateNormality

## GROUP - VIRGINICA
# Density plot 
plot_custom(data_vg,plot='density')
# QQplot 
plot_custom(data_vg,plot='qq')
# Roystone and Shapiro test
shapiro.table(data_vg)
mvn(data_vg[,-5],mvnTest = "royston")$multivariateNormality

#Checking for equality of covariance matrix
data_comb = rbind(data_s,data_vc,data_vg)
# Box M-------------------
boxM(data_comb[,-5],data_comb$Species)
#RDA
cv_5_rand = trainControl(method = "cv", number = 5, search = "random")
fit_rda_rand = train(Species ~ ., data = train_data, method = "rda",
                     trControl = cv_5_rand, tuneLength = 9)
fit_rda_rand
ggplot(fit_rda_rand)
rda_model <- rda(Species~. ,data = train_data,gamma = 0.10175956, 
                 lamda = 0.2854968)
rda_pred_train <- predict(rda_model, newdata = train_data)$class
rda_pred_test <- predict(rda_model, newdata = test_data)$class
confusionMatrix(rda_pred_train,train_data[,5])
confusionMatrix(rda_pred_test,test_data[,5])

#QDA
qda_fit = qda(Species~. ,data = train_data)
qda_pred_train <- predict(qda_fit, newdata = train_data)$class
qda_pred_test <- predict(qda_fit, newdata = test_data)$class
confusionMatrix(qda_pred_train,train_data[,5])
confusionMatrix(qda_pred_test,test_data[,5])
