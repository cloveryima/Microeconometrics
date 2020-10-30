# Question 1
# set working directory
setwd("C:/Users/think/Documents/R/Homework8")
require(stats)
library(foreign)
library(sandwich)
library(lmtest)
load("wagepan.rda")

# part a calculate the mean log wage
tapply(wagepan$lwage, wagepan$year, mean)

# part b OLS regression with constant
ModelwCon <- lm(lwage ~ d81 + d82 + d83 + d84 + d85 + d86 + d87, data = wagepan)
summary (ModelwCon) 

# part c OLD regression without constant
# generate d80
wagepan$d80 <- 1 - wagepan$d81- wagepan$d82 - wagepan$d83 - wagepan$d84 - wagepan$d85 - wagepan$d86 - wagepan$d87
ModelnoCon <- lm(lwage ~ d80 + d81 + d82 + d83 + d84 + d85 + d86 + d87 + 0, data = wagepan)
summary(ModelnoCon)

# part d is in the pdf file

# part e pooled OLS
install.packages("plm") 
library("plm")
model.pooled <- plm(lwage ~  educ + black + hisp + exper + expersq + married + union, data = wagepan, index = c("nr", "year") , model = "pooling")
summary(model.pooled)

# robust standard errors
coeftest(model.pooled, vcov = vcovHC(model.pooled, type = "HC1"))

# part f FE
model.fe <- plm(lwage ~ educ + black + hisp + exper + expersq + married + union, data = wagepan, index = c("nr", "year") , model = "within")
summary(model.fe)

# part g
# interaction terms
wagepan$d81educ <- wagepan$d81*wagepan$educ
wagepan$d82educ <- wagepan$d82*wagepan$educ
wagepan$d83educ <- wagepan$d83*wagepan$educ
wagepan$d84educ <- wagepan$d84*wagepan$educ
wagepan$d85educ <- wagepan$d85*wagepan$educ
wagepan$d86educ <- wagepan$d86*wagepan$educ
wagepan$d87educ <- wagepan$d87*wagepan$educ

model.fe <- plm(lwage ~ educ + d81educ + d82educ + d83educ + d84educ + d85educ + d86educ + d87educ + black + hisp + exper + expersq + married + union, data = wagepan, index = c("nr", "year") ,model = "within")
summary(model.fe)

# Question 2

# part a
# constructing matrix X
X <- matrix(c(1, 1, 1, 1, 1, 1, 
              1, 0, 1, 0, 1, 0,
              0, 1, 0, 1, 0, 1,
              2, 3, 3, 4, 6, 7,
              4, 9, 9, 16, 36, 49),
            nrow = 6,
            ncol = 5)
# rank of matrix X
K <- qr(X)$rank

# part b
# matrx X1, X2 and X3
X1 <- matrix(c(1, 1, 1, 0, 0, 1, 2, 3, 4, 9),
             nrow = 2,
             ncol = 5)
X2 <- matrix(c(1, 1, 1, 0, 0, 1, 3, 4, 9, 16),
             nrow = 2,
             ncol = 5)
X3 <- matrix(c(1, 1, 1, 0, 0, 1, 6, 7, 36, 49),
             nrow = 2,
             ncol = 5)
# matrix j2
j2 <- matrix(c(1, 1), nrow = 2, ncol = 1)
# matrix M2
M2 <- diag(2) - j2 %*% (solve(t(j2)%*%j2)) %*% t(j2)
# within transformation
Xdot1 <- M2 %*% X1
Xdot2 <- M2 %*% X2
Xdot3 <- M2 %*% X3
Xdot <- rbind(Xdot1, Xdot2, Xdot3)

# part c
A <- t(Xdot) %*% Xdot
# rank of A
k <- qr(A)$rank

# part d
dX <- matrix(c(0, 0, 0, -1, -1, -1, 1, 1, 1, 1, 1, 1, 5, 7, 13),
                  nrow = 3,
                  ncol = 5)

# part e is in the pdf

# part f is in the pdf