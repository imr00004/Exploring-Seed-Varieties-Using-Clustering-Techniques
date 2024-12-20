#Isabella Reeser
#DSCI 393A
#Project 3

#Load required libraries
library(ggplot2)
library(cluster)

rm(list=ls())

#Load in the data and clean it
data <- read.table("/Users/isabellareeser/seeds_dataset.txt", header = FALSE, sep = "\t", fill = TRUE)
colnames(data) <- c("area", "perimeter", "compactness", "kernel_length", "kernel_width", "asymmetry_coefficient", "kernel_groove_length", "class")

#Performing mean imputation to replace missing values
data_clean <- data
for (col in names(data_clean)) {
  if (is.numeric(data_clean[[col]])) {
    data_clean[[col]][is.na(data_clean[[col]])] <- mean(data_clean[[col]], na.rm = TRUE)
  }
}

#Split the data into features
X <- data_clean[, 1:7]

#Scale the features for equal contributions
X_scaled <- scale(X)

#Determine the optimal number of clusters by finding the elbow point
wss <- sapply(1:10, function(k) {
  kmeans(X_scaled, centers = k, nstart = 10)$tot.withinss
})
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of Clusters (k)", ylab = "Within-cluster Sum of Squares")

#Apply K-means clustering with the optimal number of clusters
set.seed(123)  #For reproducibility
k <- 3  #Optimal number of clusters based on the elbow plot
kmeans_result <- kmeans(X_scaled, centers = k, nstart = 10)

#Visualize the clusters
data_clean$cluster <- as.factor(kmeans_result$cluster)
ggplot(data_clean, aes(x = kernel_length, y = kernel_width, color = cluster)) +
  geom_point() +
  ggtitle("Scatter Plot of Seeds Dataset")

#Analyze the clusters
for (i in 1:k) {
  cat("Cluster", i, "characteristics:\n")
  print(summary(data_clean[data_clean$cluster == i, 1:7]))
  cat("\n")
}

#Compare clustering results with the true class labels
table(data_clean$cluster, data_clean$class)

#Perform hierarchical clustering
hc <- hclust(dist(X_scaled), method = "complete")

#Plot the dendrogram
par(mar = c(5, 4, 4, 0)) 
plot(hc, cex = 0.8, xlab = "", sub = "")
axis(1, at = seq(1, 12, by = 2), labels = seq(1, 12, by = 2), cex.axis = 0.9, las = 2)









