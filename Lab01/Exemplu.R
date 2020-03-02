# Calcul elementar

5-1+10
7 * 10 / 2
10 * 7 /2
7 * 7 / 3
exp(-2.19)
pi
sin(2*pi/3)
sin(pi)
sin(pi/2)

# Folosirea documentatiei

apropos("mean")

x <- "curs probabilitati si statistica"
typeof(x)

attributes(x)

y <- 1:10

y

typeof(y)

z <- as.numeric(y)

z

typeof(z)

# Scalari si vectori

vector() # vector logic gol
logical(0)
vector("character", length=5)
logical(5)
character(5)
numeric(5)


# Functiile c, rep si seq

x <- c(0.5,0.6)
x <- c(TRUE, FALSE)
x <- c(T,F)
x <- c("a", "b", "c")
x <- 9:29
x <- c(1+0i,2+4i)
x

z <- c("Sandra", "Traian", "Ionel")
z <- c(z, "Ana")
z <- c("George", z)
z

(rep(c(1,2,3),5))

(rep(c(1,2,3),each=5))

seq(1,10,length.out=15)


# Operatii cu vectori

a = 1:4
b = c(5,5,6,7)

a+b
a+10
a-b
a-15
a*b
a/b
a/100

a^b # ridicarea la putere

a^7 # ridicarea la putere cu scalari

x = seq(0,2*pi, length.out=20)

exp(x)
sin(x)
tan(pi/2)
atan(x)

length(x)
min(x)
sum(x)
mean(x)
round(x, digits=4)

y = c("M","M","F","F","F","M","F","M","F")

unique(y)
table(y)
y

# Indexare vectori

x = seq(1,10,length.out = 21)
x
x[1]
x[4:10]
x[c(2,5,9)]
x[-5]
x[-(1:3)]
x = x[-10]
x
x>3
x[x>3]
x[FALSE]
x[(x>5)&(x<19)]

x == 3
x != 3
x <= 8.6
(x<8) & (x>2)
(x<8) && (x>2)
(x<7) | (x>3)
(x<7) || (x>3)
x %in% c(1,9)

# Matrice

a <- 1:5
b <- 6:10
c <- 11:15
cbind(a,b,c)
rbind(a,b,c)

M = matrix(x, 4, 4)

diag(M) # diagonala matricei M
dim(M) # dim matricei M
nrow(M) # nr linii M
ncol(M) # nr col M
t(M) # transpusa
colSums(M)
rowSums(M)

B = M
A = B
A/B
A*B
A %*% B
crossprod(A,B)

det(A)
sum(diag(A))

m = matrix(1:20, nrow=4, byrow = TRUE)
m[1,]

# Liste

a <- list(nume="Ionel", salariu=1500, apartenenta=T)

a
a$nume
names(a)

# Data frame-uri

survey <- data.frame("index"=1:5,
                     "sex"=c(rep("m",3),"f","f"),
                     "age"=c(99,46,23,54,23))

survey
str(survey)

survey <- data.frame("index"=1:5,
                     "sex"=c(rep("m",3),"f","f"),
                     "age"=c(99,46,23,54,23), stringsAsFactors = F)

str(survey)


head(survey)
tail(survey)
View(survey)
nrow(survey)
ncol(survey)
rownames(survey)
colnames(survey)
names(survey)
str(survey)
head(mtcars)

# Metode de indexare

mtcars[1,1:4]
mtcars$mpg
mtcars[c(1,2),2]
mtcars[mtcars$mpg > 25,]

subset(x=mtcars, subset=mpg<12 & cyl > 6, select=c(disp,wt))

cars_increasing = rownames(mtcars[order(mtcars$wt),])
cars_increasing[1:10]

cars_decreasing = rownames(mtcars[order(mtcars$wt, decreasing = T),])

cars_decreasing[1:10]

stat_course = data.frame(student = c("Ionel", "Maria", "Gigel", "Vasile", "Ana"), note_stat = c(9, 8, 5, 7, 9))

alg_course = data.frame(student = c("Maria", "Ana" , "Gigel", "Ionel", "Vasile"), note_alg = c(10, 8, 9, 7, 9))

merge(x=stat_course, y=alg_course, by="student")

ChickWeight
max(ChickWeight$weight)

aggregate(formula = weight ~ Diet + Time, # DV este weight, IV sunt Diet s, i Time
          FUN = mean, # calculeaza media pentru fiecare grup
          data = ChickWeight)