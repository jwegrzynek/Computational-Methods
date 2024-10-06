# ZADANIE 1

# Przywrócenie ustawień do domyślnych
par(mfrow = c(1, 1))

# Ustawienie zakresu wartości x
x <- seq(-10, 10, length.out = 100)

# Obliczenie gęstości i dystrybuanty dla N(0,1)
density1 <- dnorm(x, mean = 0, sd = 1)
cumulative1 <- pnorm(x, mean = 0, sd = 1)

# Obliczenie gęstości i dystrybuanty dla N(2,3)
density2 <- dnorm(x, mean = 2, sd = 3)
cumulative2 <- pnorm(x, mean = 2, sd = 3)

# Obliczenie gęstości i dystrybuanty dla N(0,2)
density3 <- dnorm(x, mean = 0, sd = 2)
cumulative3<- pnorm(x, mean = 2, sd = 2)

# Obliczenie gęstości i dystrybuanty dla N(5,0.5)
density4 <- dnorm(x, mean = 5, sd = 0.5)
cumulative4 <- pnorm(x, mean = 5, sd = 0.5)

# Rysowanie wykresów
par(mfrow = c(2, 1))  # Ustawienie dwóch wykresów w kolumnie

# Wykres gęstości
plot(x, density1, type = "l", col = "blue", lwd = 2,
     main = "Wykres gęstości rozkładów normalnych",
     xlab = "x", ylab = "Gęstość", ylim = c(0, max(density1, density2, density3, density4)))
lines(x, density2, col = "red", lwd = 2)
lines(x, density3, col = "yellow", lwd = 2)
lines(x, density4, col = "green", lwd = 2)
legend("topright", legend = c("N(0,1)", "N(2,3)", "N(0,2)", "N(5,0.5)"), col = c("blue", "red", "yellow", "green"), lwd = 2)

# Wykres dystrybuanty
plot(x, cumulative1, type = "l", col = "blue", lwd = 2,
     main = "Wykres dystrybuanty rozkładów normalnych",
     xlab = "x", ylab = "Dystrybuanta", ylim = c(0, 1))
lines(x, cumulative2, col = "red", lwd = 2)
lines(x, cumulative3, col = "yellow", lwd = 2)
lines(x, cumulative4, col = "green", lwd = 2)
legend("bottomright", legend = c("N(0,1)", "N(2,3)", "N(0,2)", "N(5,0.5)"), col = c("blue", "red", "yellow", "green"), lwd = 2)

# Przywrócenie ustawień do domyślnych
par(mfrow = c(1, 1))

# -----------------------------------------------------------------------------

# ZADANIE 2

par(mar=c(5, 5, 5, 5))
x <- seq(0, 10, length.out = 100)
density_exp <- dexp(x)
cumulative_exp <- pexp(x)
plot(x, density_exp, type = 'l', lwd = 2, col = "blue", ylab = "Density")
lines(x, cumulative_exp, lwd = 2, col = "red")
axis(side=4)
mtext("Cumulative", side = 4, line = 3)

# -----------------------------------------------------------------------------

# ZADANIE 3

# Ustalmy parametry
set.seed(123)  # Dla reprodukowalności
lambda <- 1   # Parametr rozkładu wykładniczego

# Wylosowanie 100 liczb z rozkładu wykładniczego
sample_data <- rexp(100, rate=lambda)

# a) Porównanie za pomocą kwantyli (Q-Q plot)
qqplot(qexp(ppoints(100), rate=lambda), sample_data,
       main="Wykres Q-Q: Rozkład empiryczny vs teoretyczny",
       xlab="Teoretyczne kwantyle", ylab="Empiryczne kwantyle")
abline(0, 1, col="red", lwd=2)  # Linie odniesienia

# b) Porównanie dystrybuanty (empirycznej i teoretycznej)
# Wykres dystrybuanty empirycznej
plot(ecdf(sample_data), col="blue", lwd=2, main="Dystrybuanta empiryczna vs teoretyczna", 
     xlab="x", ylab="Dystrybuanta", xlim=c(0, 10))

# Dodanie teoretycznej dystrybuanty
curve(pexp(x, rate=lambda), col="red", lwd=2, add=TRUE)

# Legenda do wykresu
legend("bottomright", legend=c("Empiryczna", "Teoretyczna"),
       col=c("blue", "red"), lwd=2)



