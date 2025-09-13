rm(list=ls())
library(mlbench)
library(dplyr)
library(ggplot2)
library(smotefamily)
library(ssc)
library(caret)
#Import the Data
data("Glass")
data=Glass
any(is.na(data))

#overview of the dataset
dim(data)
levels(data$Type)
head(data,10)
colnames(data)
summary(data)
#EDA
data$Type=as.factor(data$Type)
# Classification of variables:
cls = which(colnames(data) == "Type")
x = data[,-cls]
y = data[,cls]

# Target variable:
data %>% count(Type) %>% 
  ggplot(aes(x = Type, y = n)) +
  geom_bar(stat = 'identity', width = 0.3,
           colour = 'black', fill = 'yellow') +
  labs(title = 'Frequency distribution of Glass Type',
       x = 'Glass Type', y = 'Count')

data1=SMOTE(x,y,dup_size = 6)$data
data2=SMOTE(data1[,-cls],data1[,cls],dup_size = 4)$data
data3=SMOTE(data2[,-cls],data2[,cls],dup_size = 3)$data
data4=SMOTE(data3[,-cls],data3[,cls],dup_size = 1)$data

data4 %>% count(class) %>% 
  ggplot(aes(x = class, y = n)) +
  geom_bar(stat = 'identity', width = 0.3,
           colour = 'black', fill = 'yellow') +
  labs(title = 'Frequency distribution of Glass Type',
       x = 'Glass Type', y = 'Count')


# Prepare data------------
cls = which(colnames(data4) == "class")
x = data4[,-cls]
y = as.factor(data4[,cls])
# Use 70% of instances for training
tra.idx <- sample(x = length(y), size = ceiling(length(y) * 0.7))
xtrain <- as.matrix(x[tra.idx,]) # training instances
ytrain <- y[tra.idx]  # classes of training instances

# Use 70% of train instances as unlabeled set
tra.na.idx <- sample(x = length(tra.idx), size = ceiling(length(tra.idx) * 0.7))
ytrain[tra.na.idx] <- NA # remove class information of unlabeled instances

# Use the other 50% of instances for inductive testing
tst.idx <- setdiff(1:length(y), tra.idx)
xitest <- as.matrix(x[tst.idx,])# testing instances
yitest <- y[tst.idx] # classes of testing instances

## Example: Training from a set of instances with 1-NN as base classifier.
m1 <- selfTraining(x = xtrain, y = ytrain, 
                   learner = knn3, 
                   learner.pars = list(k = 1),
                   pred = "predict",thr.conf=.9)
pred1 <- predict(m1, xitest)
table(pred1, yitest)

## Metrics
accuracy = sum(diag(table(pred1, yitest)))/nrow(xitest)
accuracy
