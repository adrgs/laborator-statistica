(X <- RV(1:12))
(X1 <- RV(1:4, c(1/2, 1/4, 1/6, 1/12)))

P(X1==3)
library(MASS)
fractions(P(X1==3))
fractions(pi)

fractions(P(X>3))

fractions(P((X1 > 1.5) %AND% (X1<3.5)))
fractions(P((X1 > 1.5) %OR% (X1<3.5)))
P(X1 > 1.5 | X1<3.5)

plot(X1)
Valori <- 1:4
Probabilitati <- c(1/2, 1/4, 1/6, 1/12)

v <- outcomes(X1)
p <- probs(X1)
plot(v,p, col="magenta", pch=17, cex=2, main="Ceva grafic", xlab="Valorile v.a.", ylab="Probabilitati")

#crearea unei functii in R

f1 <- function()
{
  print("Mesaj!")
}

f2 <- function(x)
{
  x^2+1
}

f2(5:10)

f3 <- function(x)
{
  x <- x^2
  x <- 3
  x
}
f3(3:56)

f4 <- function(x)
{
  if (x<9) x <- x^3
  else x <- 23
  return(x)
}

f4(33)

x <- seq(-3, 4, 0.001)
plot(x, f2(x))