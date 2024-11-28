# Ładowanie danych
library(MASS)
data(crabs)

# Przygotowanie danych
crabs$class <- paste(crabs$sp, crabs$sex, sep = "_")
crabs$class <- as.factor(crabs$class)

# Podział zmiennych
X <- crabs[, 4:8]  # zmienne numeryczne
y <- crabs$class   # klasy

# QDA - quadratic discriminant analysis
qda_model <- qda(X, grouping = y)

# LDA - linear discriminant analysis (uogólniona analiza Fishera)
lda_model <- lda(X, grouping = y)

# Wyświetlenie wyników
print(qda_model)
print(lda_model)

# Leave-one-out cross-validation dla QDA
library(caret)

qda_pred_loo <- train(
  X, y,
  method = "qda",
  trControl = trainControl(method = "LOOCV")
)
qda_error_loo <- 1 - qda_pred_loo$results$Accuracy

# Leave-one-out cross-validation dla LDA
lda_pred_loo <- train(
  X, y,
  method = "lda",
  trControl = trainControl(method = "LOOCV")
)
lda_error_loo <- 1 - lda_pred_loo$results$Accuracy

# 10-krotna walidacja krzyżowa dla QDA
qda_pred_10cv <- train(
  X, y,
  method = "qda",
  trControl = trainControl(method = "cv", number = 10)
)
qda_error_10cv <- 1 - qda_pred_10cv$results$Accuracy

# 10-krotna walidacja krzyżowa dla LDA
lda_pred_10cv <- train(
  X, y,
  method = "lda",
  trControl = trainControl(method = "cv", number = 10)
)
lda_error_10cv <- 1 - lda_pred_10cv$results$Accuracy

# Wyświetlenie błędów
cat("QDA LOO Error:", qda_error_loo, "\n")
cat("LDA LOO Error:", lda_error_loo, "\n")
cat("QDA 10-CV Error:", qda_error_10cv, "\n")
cat("LDA 10-CV Error:", lda_error_10cv, "\n")


# Normalizacja danych
library(class)
X_normalized <- scale(X)

# kNN dla k=3
knn_pred_k3 <- knn(train = X_normalized, test = X_normalized, cl = y, k = 3)
knn_accuracy_k3 <- sum(knn_pred_k3 == y) / length(y)

# kNN dla k=5
knn_pred_k5 <- knn(train = X_normalized, test = X_normalized, cl = y, k = 5)
knn_accuracy_k5 <- sum(knn_pred_k5 == y) / length(y)

# Wyświetlenie wyników
cat("kNN (k=3) Accuracy:", knn_accuracy_k3, "\n")
cat("kNN (k=5) Accuracy:", knn_accuracy_k5, "\n")

