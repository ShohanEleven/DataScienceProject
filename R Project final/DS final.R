BankNote_Authentication <- read.csv("E:/R Project/BankNote_Authentication.csv", header = TRUE, sep = ",")
table(BankNote_Authentication$class)
colSums(is.na(BankNote_Authentication))

install.packages("caret")
library("caret")

numeric_attributes <- BankNote_Authentication[, c("variance", "skewness", "curtosis", "entropy")]
normalized_attributes <- preProcess(numeric_attributes, method = c("range"))
normalized_attributes <- predict(normalized_attributes, newdata = numeric_attributes)
normalized_data <- cbind(normalized_attributes, class = BankNote_Authentication$class)
head(normalized_data)


correlation_matrix <- cor(BankNote_Authentication)

print(correlation_matrix)

install.packages("corrplot")
library("corrplot")
corrplot(correlation_matrix, method = "color")

install.packages("class")
library("class") 
set.seed(123)
splitIndex <- createDataPartition(normalized_data$class, p = 0.7, list = FALSE)
train_data <- normalized_data[splitIndex, ]
test_data <- normalized_data[-splitIndex, ]

predictors <- colnames(train_data)[1:4]
target <- "class"

k=sqrt(nrow(BankNote_Authentication))
k

knn_model <- knn(train = train_data[, predictors], test = test_data[, predictors], cl = train_data$class, k = 37)

accuracy <- sum(knn_model == test_data$class) / nrow(test_data)
print(paste("Accuracy (Test Set):", round(accuracy * 100, 2), "%"))

normalized_data$class <- factor(normalized_data$class, levels = c(0, 1))

ctrl <- trainControl(method = "cv", number = 10)
knn_model_cv <- train(class ~ ., data = normalized_data, method = "knn", trControl = ctrl, tuneGrid = data.frame(k = 1:20))
print(paste("Accuracy (10-Fold Cross-Validation):", round(knn_model_cv$results$Accuracy[which.max(knn_model_cv$results$Accuracy)] * 100, 2), "%"))

install.packages("e1071")
library("e1071")

conf_matrix <- table(Actual = test_data$class, Predicted = knn_model)

true_positive <- sum(knn_model == 1 & test_data$class == 1)
false_positive <- sum(knn_model == 1 & test_data$class == 0)
false_negative <- sum(knn_model == 0 & test_data$class == 1)

recall <- true_positive / (true_positive + false_negative)
precision <- true_positive / (true_positive + false_positive)

print("Confusion Matrix:")
print(conf_matrix)
print(paste("Recall (Sensitivity):", recall))
print(paste("Precision:", precision))

