# Problema 1
# Doua carti sunt selectate dintr-un pachet de 52 de carti, care e pb de blackjack?

# AS + o carte mare (10, J, Q, K)

S <- cards()
S[,1][S[,1]=='A']

# A - extrag un as
# B - extrag 10 sau J sau Q sau K

# P(B/A) = P(A si B) / P(A)
#  2 * (16/51)  *  (4/52)


#

?cards

S2 <- cards()
S2 <- merge(S2,S2,by=NULL)

S2 <- subset(S2, S2$rank.x!=S2$rank.y | S2$suit.x!=S2$suit.y)
S2 <- S2[S2$rank.x!=S2$rank.y | S2$suit.x!=S2$suit.y,]
#S2 <- S2[which(S2[,1]==S2[,3]) | which(S2[,2]==S2[,4]),]

S2
nrow(S2)

?which


A <- S[c(13,26,39,52),]
A <- S[S[,1]=='A',]

B <- S[c(9:12,22:25,35:38,48:51),]
B <- S[S[,1] %in% c('10','J','Q','K'),]

AsiB <- intersect(A,B)

#probAsiB 


# Dintr-un pachet de carti se extrage o carte
# Calculati probabilitatea ca aceasta sa fie de inima rosie si sa fie o carte mare

# Cartea este de inima
A <- S[S[,2]=='Heart',]

# Cartea este mare
B <- S[S[,1] %in% c('10','J','Q','K', 'A'),]

AintersectatB = intersect(A,B)

prob <- nrow(AintersectatB)/nrow(S)
prob_teoretic <- 5/52



# Arunc 2 zaruri 

zar <- rolldie(2,nsides=3)

zar_dep <- zar[which(zar[,1]!=zar[,2]),]
zar_dep


# Se extrag 5 carti din 52
# a) prob ca cele 5 carti sa fie cu acelasi simbol
prob <- 4*choose(13,5)/choose(52,5)
prob <- 12/51*11/50*10/49*9/48
# b) prob sa obtinem o pereche
prob <- 13*choose(4,2)*choose(12,3)*choose(4,1)*choose(4,1)*choose(4,1)/choose(52,5)
# c) prob de a obtine doua perechi
prob <- choose(13,2)*choose(4,2)*choose(4,2)*choose(44,1)/choose(52,5)
# d) prob de a obtine 3 carti de acelasi fel
# e) prob de a obtine 4 carti de acelasi fel