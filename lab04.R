install.packages("MASS")
library(MASS)


data(iris)
data <- iris[, c("Sepal.Length", "Sepal.Width", "Species")]

plot(data$Sepal.Length, data$Sepal.Width, col = as.numeric(data$Species),
     xlab = "Sepal Length", ylab = "Sepal Width", pch = 19)
legend("topright", legend = levels(data$Species), col = 1:3, pch = 19)

# -------------------------------------

qda_model <- qda(Species ~ Sepal.Length + Sepal.Width, data = data)

sepal_length_range <- seq(min(data$Sepal.Length), max(data$Sepal.Length), length.out = 100)
sepal_width_range <- seq(min(data$Sepal.Width), max(data$Sepal.Width), length.out = 100)
grid <- expand.grid(Sepal.Length = sepal_length_range, Sepal.Width = sepal_width_range)
grid_pred <- predict(qda_model, grid)$class

plot(data$Sepal.Length, data$Sepal.Width, col = as.numeric(data$Species), pch = 19,
     xlab = "Sepal Length", ylab = "Sepal Width")
points(grid$Sepal.Length, grid$Sepal.Width, col = as.numeric(grid_pred), pch = 3)
legend("topright", legend = levels(data$Species), col = 1:3, pch = 19)

lda_model <- lda(Species ~ Sepal.Length + Sepal.Width, data = data)
lda_pred <- predict(lda_model, data)$class
qda_pred <- predict(qda_model, data)$class

# Tabela wyników
table(LDA = lda_pred, QDA = qda_pred)

lda_accuracy <- mean(lda_pred == data$Species)
qda_accuracy <- mean(qda_pred == data$Species)
cat("Dokładność LDA:", lda_accuracy, "\nDokładność QDA:", qda_accuracy)

# ------------------------------------

lda_model_full <- lda(Species ~ ., data = iris)
qda_model_full <- qda(Species ~ ., data = iris)

lda_pred_full <- predict(lda_model_full, iris)$class
qda_pred_full <- predict(qda_model_full, iris)$class


lda_accuracy_full <- mean(lda_pred_full == iris$Species)
qda_accuracy_full <- mean(qda_pred_full == iris$Species)

cat("Dokładność LDA (pełne dane):", lda_accuracy_full, "\nDokładność QDA (pełne dane):", qda_accuracy_full)

cat("Czy dokładność różni się przy pełnych danych vs dwóch cechach?\n")
cat("LDA - różnica:", lda_accuracy_full - lda_accuracy, "\n")
cat("QDA - różnica:", qda_accuracy_full - qda_accuracy, "\n")
