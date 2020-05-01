# Repartitii clasice de v.a discrete

# Conventii:
# toate functiile care incep cu d
# d+prescurtare_nume_repartitie = functie de masa/densitatea de probabilitate
# p+prescurtare_nume_repartitie = functia de repartitie
# prototip d+p este: (vectorul de valori in care evaluam functia, parametrii repartitiei)
# r+prescurtare_nume_repartitie = genereaza valori din repartitia respectiva
# prototipul functiilor cu r: (numarul de valori de generat, parametrii repartitiei)


# 0)Repartitia uniforma pe cazul discret
#X~Unif({1,2,3,4,5}) => 1/5


# Functia sample
n <- 10
sample(c(0,1), n, replace=T)
sample(1:100, n)

?sample
?rbinom

# 1) Repartitia Bernoulli X-Bern(p)
# X:(0 1) cu prob 1-p si p
p <- 0.2
sample(c(0,1), 1, prob=c(1-p,p))

x <- sample(c(0,1),100000,replace=T,prob=c(1-p, p))
nr1 <- sum(x)
nr0 <- length(x[x==0])
p0 <- nr0/length(x)
p1 <- nr1/length(x)

x <- sample(c(0,1),10,replace=T,prob=c(1-p, p))
nr1 <- sum(x)
nr0 <- length(x[x==0])
p0 <- nr0/length(x)
p1 <- nr1/length(x)

# 2) Repartitia Binomiala(n,p)
# P(X=k) succese din cele n incercari
library(MASS)
k <- 0
fractions(dbinom(k, n, p))
# P(X=0)= Combinari(n,0)*p^k*(1-p)^(n-k)
# facem verificarea de mana
# choose - combinari
fractions(choose(n, k) * p^k * (1-p)^(n-k))

n <- 100
p <- 0.1
k <- 0:n
functie_masa <- dbinom(k,n,p)
plot(k, dbinom(k,n,p))
dbinom(9:10,n,p)

n <- 3
p <- 0.4
fbinom <- function(x,n,p)
{
  prob <- choose(n, x) * p^x * (1-p)^(n-x)
  prob
}
fbinom(0:n,n,p)
dbinom(0:n,n,p)

# X ia valorile 0 1 2 cu prob 1/2 1/3 si 1/6


# Daca X1, X2, ... Xm sunt v.a. IID Bernoulli(p) (IID - independente si identic distribuite)
# Atunci X=X1+X2+...+Xm are repartitia Binom(m,p)

library(discreteRV)

X1 <- RV(0:1, c(1/4,3/4))
X3 <- RV(0:1, c(1/4,3/4))
X2 <- RV(0:1, c(1/4,3/4))

Xsuma3 <- SofIID(X1, 3)
Xsuma3bis <- SofI(X1,X2,X3)
Xsuma3bis

#Xbinom <- RV("binomial", n=4, p=3/4)
#Xbinom

probs(Xsuma3)
dbinom(0:3, 3, 3/4)

cos(pi/2)