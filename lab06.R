library(MASS)

# Przygotowanie danych
data(crabs)
crabs_O <- crabs[crabs$sp == "O", ]
X <- crabs_O[, c("FL", "RW")]
y <- as.factor(crabs_O$sex)

# Analiza dyskryminacyjna Fishera "ręczna"
means <- aggregate(X, by=list(y), FUN=mean)
mean_diff <- t(means[2, 2:3] - means[1, 2:3])

cov_matrix <- cov(X)

# Wagi funkcji dyskryminacyjnej
w <- solve(cov_matrix) %*% mean_diff
scores <- as.matrix(X) %*% w

# Klasyfikacja i wizualizacja
threshold <- mean(scores[y == "M"])  # Próg klasyfikacji
predicted <- ifelse(scores > threshold, "M", "F")

# Zgodność predykcji
accuracy <- mean(predicted == y)
print(accuracy)

# Wizualizacja
plot(X, col=c("red", "blue")[as.numeric(y)], pch=19)
abline(-threshold / w[2], -w[1] / w[2], col="green")
legend("topright", legend=c("Male", "Female"), col=c("red", "blue"), pch=19)


data(iris)

# Przygotowanie danych
X <- iris[, 1:4]
y <- iris$Species

# Model lda
lda_model <- lda(Species ~ ., data=iris)
predictions <- predict(lda_model)

# Wektory kanoniczne (zmienne dyskryminacyjne)
canonical_vars <- as.matrix(X) %*% lda_model$scaling

# Zgodność predykcji
accuracy <- mean(predictions$class == y)
print(accuracy)

# Wizualizacja
plot(canonical_vars[, 1:2], col=c("red", "blue", "green")[as.numeric(y)], pch=19)
legend("topright", legend=levels(y), col=c("red", "blue", "green"), pch=19)
