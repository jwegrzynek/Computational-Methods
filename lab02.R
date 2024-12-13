#install.packages("MASS")
library(MASS)
k1 = 30 # liczba wektorów w klasie 1
k0 = 20 # liczba wektorów w klasie 0

G1= mvrnorm(k1, mu=c(0,0), Sigma = matrix(c(1, 0.1, 0.1, 1), nrow = 2))
G0 = mvrnorm(k0, mu = c(2,2), Sigma = matrix(c(1, 0.1, 0.1, 1), nrow = 2))
G=rbind(G1, G0)

G

klasa = c(rep(1, k1), rep(0,k0)) # etykiety klas
length(klasa)
#generujemy ramkę danych dla próby uczącej
daneL = data.frame("x1"=G[,1], "x2"=G[,2], "klasa"=klasa)
daneL

#graficzna prezentacja danych
plot(daneL$x1, daneL$x2, type = "n", xlab = "x1", ylab = "x2", main = "Przygotowanie do analizy dyskryminacyjnej LDA")
points(daneL$x1[daneL$klasa==1], daneL$x2[daneL$klasa==1], col = "red", cex = 0.5)
points(daneL$x1[daneL$klasa==0], daneL$x2[daneL$klasa==0], col = "blue", cex = 0.5)

#obliczamy stosowane estymatory macierzy kowariancji 
S1 = cov(G[daneL$klasa == 1,1:2])
S0 = cov(G[daneL$klasa == 0,1:2])
S = 1/(k1+k0-2)*((k1-1)*S1 + (k0-1)*S0)
S

#obliczamy średnie w klasach
mu1 = c(mean(G[daneL$klasa == 1,1]), mean(G[daneL$klasa == 1,2]))
mu1
mu11 = apply(daneL[daneL$klasa == 1,1:2],2, mean)

mu0 = c(mean(G[daneL$klasa == 0,1]), mean(G[daneL$klasa == 0,2]))
mu0
mu00 = apply(daneL[daneL$klasa == 0,1:2], 2, mean)
mu00

#funkacja predykcji przy założeniu równych prawdopodobieństw apriori dla klas
predykcja = function(x) {
  # Calculate the discriminant score
  dd = t(x - 0.5 * (mu1 + mu0)) %*% solve(S) %*% (mu1 - mu0)
  
  # Return the predicted class based on the score
  if (dd >= 0) {
    return(1)  # Class 1
  } else {
    return(0)  # Class 0
  }
}

# Test the prediction function with an example input
example_point = c(-1, -0.2)  # Change this to test different points
predykcja(example_point)

# funkcja predykcji przy założeniu różnych prawdopodobieństw apriori dla klas
predykcja2 = function(x){
  
  dd = t(x-0.5*(mu1+mu0))%*%solve(S)%*%(mu1-mu0)+log((k1/(k1+k0))/(k0/(k1+k0)))
  if (dd>=0){
    return(1)
  }
  else{
    return(0)
  }
}

pred = apply(daneL[,1:2], 1, predykcja)
pred
acc = length(klasa[klasa == pred]) / length(klasa)
acc

pred2 = apply(daneL[,1:2], 1, predykcja2)
pred2
acc2 = length(klasa[klasa == pred2]) / length(klasa)
acc2

poprawka = log((k1/(k1+k0))/(k0/(k1+k0)))

daneL$pred=pred
head(daneL)

w = solve(S)%*%(mu1 - mu0)
w
s = -0.5*(t(mu1+mu0)%*%w)
s
plot(daneL$x1, daneL$x2, type = "n", xlab = "x1", ylab = "x2", main = "Liniowa funkcja dyskryminacyjna")
points(daneL$x1[daneL$klasa == 1], daneL$x2[daneL$klasa == 1], col = "red", cex = 0.5)
points(daneL$x1[daneL$klasa == 0], daneL$x2[daneL$klasa == 0], col = "blue", cex = 0.5)
abline(-s/w[2], -w[1]/w[2], col='black', lty=2)
abline(-(s-poprawka)/w[2], -w[1]/w[2], col='green', lty=2)

# ---------------------------------------------------------------------------

# Zadanie 3
install.packages("ggplot2")
library(ggplot2)

# Fit the LDA model
lda_model <- lda(klasa ~ x1 + x2, data = daneL)

# Print LDA result
print(lda_model)

# Plot the data with LDA decision boundary
plot_lda <- function(model, data) {
  # Extract linear discriminants and the mean vectors
  means <- model$means
  slope <- -model$scaling[1] / model$scaling[2]
  intercept <- means[1,] %*% model$scaling
  
  # Create ggplot
  p <- ggplot(data, aes(x = x1, y = x2, color = as.factor(klasa))) +
    geom_point() +
    geom_abline(intercept = intercept, slope = slope, color = 'blue', linetype = "dashed") +
    labs(title = "LDA Decision Boundary", x = "x1", y = "x2", color = "Class") +
    theme_minimal()
  
  return(p)
}

# Plot LDA result
plot_lda(lda_model, daneL)

# ---------------------------------------------------------------------------

# Zadanie 4

# Load the iris dataset
data(iris)

# Filter for Setosa and Virginica
iris_subset <- subset(iris, Species %in% c("setosa", "virginica"))

# Use Sepal.Length and Sepal.Width, and convert Species to a factor
iris_subset <- iris_subset[, c("Sepal.Length", "Sepal.Width", "Species")]
iris_subset$Species <- factor(iris_subset$Species)

# Split into training (first 25 for each class) and test sets
train_set <- rbind(iris_subset[1:25, ], iris_subset[76:100, ])
test_set <- rbind(iris_subset[26:50, ], iris_subset[101:150, ])

# Fit LDA on the training set
lda_iris <- lda(Species ~ Sepal.Length + Sepal.Width, data = train_set)

# Print LDA result
print(lda_iris)

# Test the LDA model on the test set
predictions <- predict(lda_iris, newdata = test_set)

# Print predictions and compare with actual values
table(predictions$class, test_set$Species)

# Plot the decision boundary for the iris data
plot_lda(lda_iris, iris_subset)

# Extract linear discriminants and the mean vectors
means <- lda_iris$means
slope <- -lda_iris$scaling[1] / lda_iris$scaling[2]
intercept <- means[1,] %*% lda_iris$scaling

# Create ggplot
p <- ggplot(iris_subset, aes(x = Sepal.Length, y = Sepal.Width, color = as.factor(Species))) +
  geom_point() +
  geom_abline(intercept = intercept, slope = slope, color = 'blue', linetype = "dashed") +
  labs(title = "LDA Decision Boundary", x = "x1", y = "x2", color = "Class") +
  theme_minimal()

p
