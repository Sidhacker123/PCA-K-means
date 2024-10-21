library(ggplot2)
x_train <- read.csv("cancer.train.csv")
X_test <- read.csv("cancer.test.csv")
y_train <- read.csv("label.train.csv")
Y_test <- read.csv("label.test.csv")

head(x_train)
head(y_train)
x_scaled <- scale(x_train)
pr_result <- prcomp(x_scaled,scale=FALSE)
first_component <- pr_result$x
Z_train <- pr_result$x
loading_vector <- pr_result$rotation
dim(first_component)
dim(loading_vector)

# Extract the first two principal components
pca_scores <- data.frame( PC1 = first_component[,1] , PC2 = first_component[,2] , Label = y_train$M)
ggplot(pca_scores, aes(PC1, PC2, color = Label)) + geom_point(size = 3) +
  labs(title = "PCA Plot: First Two Principal Components", 
       x = "PC1", y = "PC2") +
  scale_color_manual(values = c("B" = "blue", "M" = "red")) +
  theme_minimal() 
#cumulative variance

pov <- pr_result$sdev^2/sum(pr_result$sdev^2);
cum_pov <- cumsum(pov);
plot(cum_pov, type = "b", xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained",
     main = "Cumulative Variance Explained by Principal Components")
abline(v = which.max(cum_pov >= 0.9), col = "red", lty = 2)  # Mark where 90% variance is explained

# How many components explain 90% of the variance?
components_to_retain <- which(cum_pov >= 0.9)[1]
explained_variance <- cum_pov[components_to_retain]
print(paste("Number of components to retain: ", components_to_retain))
print(paste("Variance explained by these components: ", round(explained_variance * 100, 2), "%"))

#Logistic 

# Extract the first 7 principal components from Z_train
Z_train_7 <- Z_train[, 1:7]
y_train$V1 <- as.factor(y_train$M)  # Assuming 'V1' is the column name for the labels

str(y_train)
# Fit the logistic regression model
logistic_model <- glm(y_train$V1 ~ Z_train_7, family = binomial)

# Display the summary of the model to see the results
summary(logistic_model)


# last part
# Assuming the test design matrix X_test is already loaded
X_test <- read.csv("cancer.test.csv")
X_test_scaled <- scale(X_test)
X_test_scaled <- scale(X_test, center = attr(X_train_scaled, "scaled:center"), scale = attr(X_train_scaled, "scaled:scale"))
pr_result_test <- prcomp(X_test_scaled,scale=FALSE)
Z_test <- pr_result_test$x
loading_vector <- pr_result_test$rotation
Z_test_k <- Z_test[, 1:7]

# Compute Z_test by multiplying X_test with the loading matrix from training PCA
Z_test <- X_test_scaled %*% loading_vector
dim(Z_test)
dim(Y_test)
# Extract the first k principal components for the test set
k <- 7  # Number of principal components determined earlier
# Check the structure of Y_test
str(Y_test)

# If Y_test is a list, convert it to a factor vector
Y_test <- as.factor(unlist(Y_test))

logistic_model_test <- glm(Y_test ~ Z_test_k, family = binomial)
head(logistic_model_test)

# Predict using the test set principal components
predicted_probs <- predict(logistic_model_test, newdata = as.data.frame(Z_test_k), type = "response")

# Convert predicted probabilities to class labels (0.5 threshold for classification)
predicted_labels <- ifelse(predicted_probs > 0.5, "M", "B")

# Assuming y_test is already loaded
# Create a confusion matrix comparing predicted labels and true labels
confusion_matrix <- table(Predicted = predicted_labels, True = Y_test)
print(confusion_matrix)










