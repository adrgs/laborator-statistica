hearthstone <- tosscoin(5, makespace = T)

?tosscoin

?rolldie

rolldie(2, makespace=T, nsides = 15)

rolldie(1, nsides=5)


?roulette

roulette(european=T, makespace=T)

cards(jokers = T)

a <- 1:100
b <- a[a%%11==0]

length(a[(a%%11 == 0) & (a%%7!=0)])

u <- 1:10
v <- 11:20

m1 <- cbind(u, v)
m2 <- rbind(u, v)
m1
m2

m3 <- m1[1:3,]
m4 <- m1[1:3,2]

# Selectati din matricea m1 toate valorile divizibile cu 11
# si nedivizibile cu 7

r <- m1[m1%%2==0]
r <- c(m1[,1][(m1[,1]%%11 == 0) & (m1[,1]%%7!=0)],m1[,2][(m1[,2]%%11 == 0) & (m1[,2]%%7!=0)])
r
r <- m1[(m1%%11 == 0) & (m1%%7!=0)]
r

carti <- cbind(cards()[1:13,],cards()[14:26,],cards()[27:39,],cards()[40:52,])


S <- cards()

A <- S[8:28,]
B <- S[22:35,]

union(A,B)
intersect(A,B)
setdiff(S,A) # Complementara lui A

# Aruncarea unui zar cu 10 fete de doua ori

aruncari = rolldie(2, nsides = 10)
o = sum(aruncari[1,1:2]):sum(aruncari[length(aruncari[,1]),1:2])
o

# C = Suma de pe cele doua fete este divizibila cu 3

C <- o[o%%3==0]
Cc <- setdiff(o, C)

# D = Suma de pe cele doua fete mai mici ca 5

D <- o[o<5]
Dc <- setdiff(o, D)

un <- union(C,D)
it <- intersect(C,D)
