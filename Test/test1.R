suppressMessages(library(discreteRV, quietly = T))

(X <-  RV(c(2,3),c(1/5,4/5)))
(Y <-  RV(c(-3,-2),c(4/5,1/5)))

(X2pY3 <- 2*X+3*Y)

negY <- Y*(-1)

produsCartezian <- expand.grid(probs(X2pY3),probs(negY))
jointProbs <- produsCartezian$Var1 * produsCartezian$Var2
jointNegYsiX2pY3 <- jointRV(list(outcomes(negY), outcomes(X2pY3)), probs=jointProbs)

(margNegY <- discreteRV::marginal(jointNegYsiX2pY3, 1))
(margX2pY3 <- discreteRV::marginal(jointNegYsiX2pY3, 2))

(pachetP <- P(margX2pY3 < 3 | margNegY > 2))