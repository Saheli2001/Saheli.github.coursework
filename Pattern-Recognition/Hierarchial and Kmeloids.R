################ PROBLEM 1 #########################
rm(list=ls())
set.seed(2023)

library(ggplot2)
library(reshape2)
library(factoextra)
library(fpc)
library(gplots)
library(proxy)
library(cluster)
library(geosphere)

# Cities Vector
city_list = c("Leh", "Pondicherry", "New Delhi", 
               "Chandigarh", "Srinagar", "Daman", 
               "Port Blair", "Kavaratti")

# Ariel Distances (longitude, latitude) of the cities
city_coordinates = matrix(c(
  77.5771, 34.1526, # Leh
  79.8083, 11.9416, # Pondicherry
  77.2090, 28.6139, # New Delhi
  76.7794, 30.7333, # Chandigarh
  74.7973, 34.0836, # Srinagar
  72.8328, 20.3974, # Daman
  92.7265, 11.6234, # Port Blair
  72.6369, 10.5626  # Kavaratti
), ncol = 2, byrow=T)
colnames(city_coordinates) = c("longitude", "latitude")
city_coordinates

# Dissimilarity matrix based on aerial distances
city_dist_matrix = distm(city_coordinates, fun = distHaversine)
rownames(city_dist_matrix) = city_list
colnames(city_dist_matrix) = city_list
city_dist_matrix
# Plot the dissimilarity matrix as a heatmap
ggplot(melt(city_dist_matrix), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() + scale_fill_gradient(high = "blue", low = "white") +
  labs(title = "Dissimilarity Matrix")

# Hierarchical Clustering - Average Linkage
hc_average = agnes(city_dist_matrix, method = "average")
dend_avg = as.dendrogram(as.hclust(hc_average))
plot(dend_avg, main = "Dendrogram - Average Linkage", xlab = "Cities", ylab = "Aerial Distance")

# Hierarchical Clustering - Single Linkage
hc_single = agnes(city_dist_matrix, method = "single")
dend_single = as.dendrogram(as.hclust(hc_single))
plot(dend_single, main = "Dendrogram - Single Linkage", xlab = "Cities", ylab = "Aerial Distance")

# Within Cluster Sum of Squares - Average Linkage
wcss_avg = array(0)
cluster_assignments_avg = cutree(hc_average, k = 2:8)

for(i in 1:6) {
  wcss_avg[i] = cluster.stats(city_dist_matrix, cluster_assignments_avg[, i])$within.cluster.ss
}
wcss_avg[7] = 0

plot(2:8, wcss_avg, type = "b", pch = 19, 
     main = "Plot of Within Cluster Sum of Squares for Average Linkage", 
     xlab = "Number of Clusters", ylab = "WCSS")

# Within Cluster Sum of Squares - Single Linkage
wcss_single = array(0)
cluster_assignments_single = cutree(hc_single, k = 2:8)

for(i in 1:6) {
  wcss_single[i] = cluster.stats(city_dist_matrix, cluster_assignments_single[, i])$within.cluster.ss
}
wcss_single[7] = 0

plot(2:8, wcss_single, type = "b", pch = 19, 
     main = "Plot of Within Cluster Sum of Squares - Single Linkage",
     xlab = "Number of Clusters", ylab = "WCSS")

# Manhattan distance
manhattan = function(x, y) {
  return(sum(abs(x - y)))
}

# User Defined Function to perform k-medoid clustering
kmedoid_clus = function(data, k, max.iter = 100) {
  n = nrow(data)
# Randomly initialize centroids
  centroids = sample(1:n, k)
  
  for (iter in 1:max.iter) {
    # Assigning points to nearest cluster centroids
    assign = sapply(1:n, function(i) which.min(sapply(centroids, function(j) 
      manhattan(data[i, ], data[j, ]))))
    # Updation
    new_centroids = sapply(1:k, function(cluster) {
      points = which(assign == cluster)
      distances = sapply(points, function(p) 
        sum(sapply(points, function(other) 
          manhattan(data[p, ], data[other, ]))))
      return(points[which.min(distances)])
    })
    
    # Check for convergence
    if (all(centroids == new_centroids)) {
      break
    } else {
      centroids = new_centroids
    }
  }
  
  # Return cluster assignments and centroids
  return(list(assignments = assign, centroids = centroids))
}

# Choose appropriate k for k-medoid clustering
clusters = kmedoid_clus(city_coordinates, k = 4)

# Print cluster assignments
print(clusters$assign)


################ PROBLEM 3 #########################
rm(list=ls())
library(rpart)
library(rpart.plot)
library(randomForest)
library(vip)
library(dplyr)
library(ggplot2)
# Loading the data
data("ptitanic")

# Randomly sample 800 observations
set.seed(123)  # For reproducibility
sampled_data <- ptitanic[sample(nrow(ptitanic), 800), ]
head(sampled_data,8) 

#Visualising the data
sampled_data %>% count(survived) %>% 
  ggplot(aes(x = survived, y = n)) +
  geom_bar(stat = 'identity', width = 0.3,
           colour = 'black', fill = 'yellow') +
  labs(title = 'Frequency distribution of People who survived and who did not survived',
       x = 'Survival Status', y = 'Count')

# Plots of categorical variables:
f <- function(var1, var2, option){
    sampled_data %>% group_by({{var1}}, {{var2}}) %>% 
      count() %>% ggplot(aes(x = {{var1}}, 
                             y = n, fill = {{var2}})) +
      geom_bar(stat = 'identity', position = position_dodge2()) +
      scale_y_continuous(n.breaks = 10) + theme_light()
  
}

attach(sampled_data)
a=f(survived, pclass)
b=f(survived,sex)
c=f(sex,pclass)
gridExtra::grid.arrange(a,b,c,ncol=3)

# Train-test split (80-20)
train_index <- sample(seq_len(nrow(sampled_data)), size = 0.8 * nrow(sampled_data))
train_data <- sampled_data[train_index, ]
test_data <- sampled_data[-train_index, ]

# Fit classification tree
tree_model <- rpart(survived ~ ., data = sampled_data,
                    control = rpart.control(maxdepth = 7, split='gini'))

# Pruning the tree
pruned_tree <- prune(tree_model, cp = 0.02)
rpart.plot(pruned_tree)

# Get the two most important variables
imp <- vip(pruned_tree)
print(imp)

# Predict on train and test data
train_pred <- predict(pruned_tree, newdata=train_data,type='class')
test_pred <- predict(pruned_tree, newdata=test_data, type='class')

# Calculate specificity and sensitivity for both train and test sets
train_conf_matrix <- table(observed = train_data$survived, predicted = train_pred)
test_conf_matrix <- table(observed = test_data$survived, predicted = test_pred)

train_TN <- train_conf_matrix[2, 2]
train_FP <- train_conf_matrix[2, 1]
train_FN <- train_conf_matrix[1, 2]
train_TP <- train_conf_matrix[1, 1]

test_TN <- test_conf_matrix[2, 2]
test_FP <- test_conf_matrix[2, 1]
test_FN <- test_conf_matrix[1, 2]
test_TP <- test_conf_matrix[1, 1]

train_specificity <- train_TN / (train_TN + train_FP)
train_sensitivity <- train_TP / (train_TP + train_FN)

test_specificity <- test_TN / (test_TN + test_FP)
test_sensitivity <- test_TP / (test_TP + test_FN)

# Report the results
print("Train Set:")
print(paste("Specificity:", train_specificity))
print(paste("Sensitivity:", train_sensitivity))

print("Test Set:")
print(paste("Specificity:", test_specificity))
print(paste("Sensitivity:", test_sensitivity))

# Impute missing values with median
# Identify numeric columns
numeric_cols <- sapply(sampled_data, is.numeric)

# Impute missing values in numeric columns with column-wise median
for (col in names(train_data)[sapply(train_data, is.numeric)]) {
  train_data[is.na(train_data[, col]), col] <- median(sampled_data[, col], na.rm = TRUE)
  test_data[is.na(test_data[, col]), col] <- median(sampled_data[, col], na.rm = TRUE)
}
# Fit Random Forest
rf_model <- randomForest(survived ~ ., data = train_data, ntree = 250,
                         mtry = 2, importance = TRUE)

# Get variable importance
importance(rf_model)
varImpPlot(rf_model)

# Predict on train and test data
train_pred_rf <- predict(rf_model, newdata=train_data,type='class')
test_pred_rf <- predict(rf_model, newdata=test_data, type='class')

# Calculate specificity and sensitivity for both train and test sets
train_conf_matrix_rf <- table(observed = train_data$survived, predicted = train_pred_rf)
test_conf_matrix_rf <- table(observed = test_data$survived, predicted = test_pred_rf)

train_TN_rf <- train_conf_matrix_rf[2, 2]
train_FP_rf <- train_conf_matrix_rf[2, 1]
train_FN_rf <- train_conf_matrix_rf[1, 2]
train_TP_rf <- train_conf_matrix_rf[1, 1]

test_TN_rf <-test_conf_matrix_rf[2, 2]
test_FP_rf <- test_conf_matrix_rf[2, 1]
test_FN_rf <- test_conf_matrix_rf[1, 2]
test_TP_rf <- test_conf_matrix_rf[1, 1]

train_specificity_rf <- train_TN_rf / (train_TN_rf + train_FP_rf)
train_sensitivity_rf <- train_TP_rf / (train_TP_rf + train_FN_rf)

test_specificity_rf <- test_TN_rf / (test_TN_rf + test_FP_rf)
test_sensitivity_rf <- test_TP_rf / (test_TP_rf + test_FN_rf)

# Report the results
print("Train Set:")
print(paste("Specificity:", train_specificity_rf))
print(paste("Sensitivity:", train_sensitivity_rf))

print("Test Set:")
print(paste("Specificity:", test_specificity_rf))
print(paste("Sensitivity:", test_sensitivity_rf))

