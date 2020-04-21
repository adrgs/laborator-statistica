# Calcule prima ora

(X <- RV(2:3, c(1/5,4/5)))
(Y <- RV(-3:-2, c(4/5,1/5)))

P(4*Y-X < 0)

X1 <- RV(c(-50,0,60), c(1/2,1/3,1/6))
X2 <- RV(c(-4,40),c(1/2,1/2))
X3 <- RV(c(-18, 90),c(2/3,1/3))

C1 <- (2*X+3*Y)
C2 <- (3*X-Y)

-50*1/2+60*1/6
-4*1/2+40*1/2
-18*2/3+90*1/3

var(X2)
var(X3)

X2p <- (X2-18)^2
X3p <- (X3-18)^2
?ExpectedValue

-36*2/3+72*1/3

# Calcule a doua ora

X1 <- RV(c(-50,0,60), c(1/2,1/3,1/6))
X2 <- RV(c(-4,40),c(1/2,1/2))
X3 <- RV(c(-18, 90),c(2/3,1/3))

m1 <- E(X1)
m2 <- E(X2)
m3 <- E(X3)

d1 <- V(X1)
d2 <- V(X2)
d3 <- V(X3)

X23 <- jointRV(list(outcomes(X2),outcomes(X3)), probs=rep(c(1/3,1/6),2))
X2marg <- marginal(X23, 1)
X3marg <- marginal(X23, 2)

X2
X2marg
X3marg

P(X2<X3)
P(X2marg<X3marg)
P(x2marg>3|x3marg<87)
#Prob ca X2 < X3

independent(X2,X3)
independent(X2marg,X3marg)