# Set the seed for reproducibility
set.seed(123)

# Number of observations and variables
n_obs_per_class <- 20  # Observations per class
n_vars <- 50           # Number of variables
n_classes <- 3         # Number of classes

# Generate random data from a normal distribution
# Shift the mean for each class so that they are distinguishable
data_class1 <- matrix(rnorm(n_obs_per_class * n_vars, mean = 0), nrow = n_obs_per_class, ncol = n_vars)
data_class2 <- matrix(rnorm(n_obs_per_class * n_vars, mean = 3), nrow = n_obs_per_class, ncol = n_vars)  # Shifted mean
data_class3 <- matrix(rnorm(n_obs_per_class * n_vars, mean = 6), nrow = n_obs_per_class, ncol = n_vars)  # Further shifted mean

# Combine the data into one matrix
data_matrix <- rbind(data_class1, data_class2, data_class3)

# Create class labels (factor variable)
class_labels <- factor(rep(1:n_classes, each = n_obs_per_class))

# Create a data frame for the data and labels
data_df <- data.frame(data_matrix, Class = class_labels)

# Check the data structure
head(data_df)
# Load necessary libraries
library(ggplot2)

# Perform PCA on the data matrix (excluding the Class column)
pca_result <- prcomp(data_matrix, scale = TRUE)

# Extract the first two principal components
pca_scores <- data.frame(pca_result$x[, 1:2], Class = class_labels)

# Plot the first two principal components and color the points by class
ggplot(pca_scores, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(size = 3) +
  labs(title = "PCA: First Two Principal Components",
       x = "PC1",
       y = "PC2") +
  theme_minimal()

#KNN

# Perform K-means clustering with K = 3
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(data_matrix, centers = 3, nstart = 20)

# Extract cluster assignments
kmeans_clusters <- kmeans_result$cluster

# Compare K-means clusters to the true class labels
comparison_table <- table(True_Class = class_labels, Cluster = kmeans_clusters)
print(comparison_table)
# 2 and 4
  kmeans_result <- kmeans(data_matrix, centers = 2, nstart = 20)

# Extract cluster assignments
kmeans_clusters <- kmeans_result$cluster

# Compare K-means clusters to the true class labels
comparison_table <- table(True_Class = class_labels, Cluster = kmeans_clusters)
print(comparison_table)

kmeans_result <- kmeans(data_matrix, centers = 4, nstart = 20)

# Extract cluster assignments
kmeans_clusters <- kmeans_result$cluster

# Compare K-means clusters to the true class labels
comparison_table <- table(True_Class = class_labels, Cluster = kmeans_clusters)
print(comparison_table)

# Extract the first two principal components (PC1 and PC2)
pca_scores_2d <- pca_result$x[, 1:2]  # This is a 60x2 matrix
# Perform K-means clustering with K = 3 on the first two principal components
set.seed(123)
kmeans_pca <- kmeans(pca_scores_2d, centers = 3, nstart = 20)

# Extract cluster assignments
kmeans_pca_clusters <- kmeans_pca$cluster
# Compare K-means clusters on PCA-reduced data to the true class labels
comparison_table_pca <- table(True_Class = class_labels, Cluster = kmeans_pca_clusters)
print(comparison_table_pca)

#scale
# Scale the data so that each variable has mean = 0 and sd = 1
scaled_data <- scale(data_matrix)
# Set seed for reproducibility
set.seed(123)

# Perform K-means clustering with K = 3 on the scaled data
kmeans_scaled <- kmeans(scaled_data, centers = 3, nstart = 20)

# Extract the cluster assignments
kmeans_scaled_clusters <- kmeans_scaled$cluster
# Compare K-means clusters on scaled data to the true class labels
comparison_table_scaled <- table(True_Class = class_labels, Cluster = kmeans_scaled_clusters)
print(comparison_table_scaled)


