
# Repartitia geometrica
#X~Geom(p) p- probabilitatea de success de la fiecare incercare (incercarile sunt independente)

#Notam q = 1-p
#X:(1 2 3 4...k...Inf;p q*p q^2*p...q^(k-1)*p)

p <- 0.3
#P(X=3)
q <- 1-p
(q)*p

(q^3)*p
dgeom(3, p)

#In teorie ea numara de cate incercari e nevoie pana la primul succes
#Implementarea din R numara cate esecuri avem pana la primul succes

t <- seq(-3, 20, 0.001)
plot(t,pgeom(t, p))

#Creez o v.a. repartizata geometric cu pachetul discrete RV

fgeometric <- function(x, pp) { pp*(1-pp)^(x-1) }
X <- RV(outcomes=c(1, Inf), probs=fgeometric, pp=0.3)
X
P(X==5)
dgeom(4, 0.3)

#Repartitia Poisson

(X <- RV("poisson", lambda = 3))
X

poisfunc <- function(x, lambda) { lambda^x * exp(-lambda) / factorial(x) }
(Y <- RV(c(0, Inf), poisfunc, lambda = 5))
P(X==12)
E(X)
V(X)

dpois(12, 3)
t <- seq(-3, 20 , 0.001)
plot(t, ppois(t,3))

#Repartitia hipergeometrica
#In R: m-nr de bile albe din urna, n-nr de bile negre din urna, k-nr de extrageri facute

#N-nr total de bile din urna, N1-nr de bile albe(de culoarea dorita), n-nr de extrageri
#P(X==k) = choose(N1, k)*choose(N-N1, n-k)/choose(N,n)

m <- 3
n <- 3
k <- 2
#P(X==2) = 3/6*2/5
dhyper(2, m, n, k)

m <- 5
n <- 2
k <- 4
dhyper(3,m,n,k)


choose(m, 3)*choose(n , k-3)/choose(m+n, k)


#Repartitii de v.a. continue

a <- 3
b <- 17
dunif(17,a,b)
t <- seq(-1,20,0.001)
plot(t, dunif(t, a, b))
plot(t, punif(t, a, b), col="magenta")
#Densitatea sa este constanta pe o zona si 0 in rest

#Repartitia exponentiala X~Exp(lambda)

#densitatea are suportul (0,Inf) [pentru care densitatea este nenula] f(x)=lambda*exp(-lambda*x)

lambda <- 1
plot(t, dexp(t, lambda), col="orange")
plot(t, pexp(t, lambda), col="orange")

#Repartitia normala X~Norm(m, sigma) sigma= abaterea medie patratica/ abaterea sntadard = radical din dispersie
#suportul lui f este toata axa reala
#f(x) = 1/sqrt(2*pi*sigma^2) * exp(-(x-m)^2/2*sigma^2)

t <- seq(-10, 10, 0.001)
plot(t, dnorm(t, 0, 1),col="blue")
abline(v=0, col="black")
#abline(h=0.3, col="red")

plot(t,dnorm(t, 0,1), col="blue")
for (i in 1:5)
{
  lines(t,dnorm(t,i-3,1), col=i)
  abline(v=i-3,col="black")
}

plot(t,dnorm(t, 0, 0.5), col="blue")
for (i in 1:5)
{
  lines(t,dnorm(t,0,i/2), col=i)
}