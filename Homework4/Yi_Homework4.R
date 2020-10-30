# Quesiton 1. Poisson distribution
# Part a, b, c, f, g are in the pdf file
# Load the data
mydata <- read.csv("hw4.1.csv", header = TRUE)
y <- mydata$y #make it a vector

# Part d 
uniquey <- unique(y)
uniquey <- sort(uniquey) #obtain all the values that y can take on
uniquey # there are 12 unique values (from 0 to 11)
prob <- integer(12) #generate a numeric vector of size 12
for (i in 1:12){
  prob[i] <- length(y[ which(y == uniquey[i])])/1000
}
prob # a vector of probability each value takes on

# Part e
# (-1) * Empirical expectation of log-likelihood
myfun1 <- function(theta){
  result1 <- 0
  for (i in 1:12){
    result1 <- result1 - prob[i]*(uniquey[i]*log(theta) - theta - log(factorial(uniquey[i])))
  }
  return(result1)
}

# Empirical log-likelihood for the first observation (y = 5)
myfun2 <- function(theta){
  result2 <- prob[6]*(5*log(theta)-theta-log(factorial(5)))
  return(result2)
}

# Empirical log-likelihood for the second observation (y = 2)
myfun3 <- function(theta){
  result3 <- prob[3]*(2*log(theta)-theta-log(factorial(2)))
  return(result3)
}

# Plotting
ellf <- integer(10000) #expected likelihood function
elff <- integer(10000) #likelihood of the first observation
elfs <- integer(10000) # likelihood of the second observation
index <- integer(10000)

for (i in 1:10000){
  ellf[i] <- (-1)*myfun1(i/1000) #because it would minimize if we don't use negative one
  elff[i] <- myfun2(i/1000)
  elfs[i] <- myfun3(i/1000)
  index[i] <- i/1000
}

par(mfrow=c(1,3))
plot(index, ellf, main = "Empirical log-likelihood function", xlab = "theta", ylab  ="")
plot(index, elff, main = "Log-likelihood function for the first observation (5)", xlab = "theta", ylab ="", col = "blue")
plot(index, elfs, main = "Log-likelihood function for the second observation (2)", xlab = "theta", ylab ="", col = "red")

# Part h

optimal <- optim(mean(y),myfun1)
optimizer <- optimal$par
optimizer #2.747
mean(y) #2,747

# Question 2. Use the data set grogger.rda to answer the following

library(foreign)
 
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)


# Import the data
load("grogger.rda") #We use load instead of read because it's rda

# Part a

# Create a dummy arr86
grogger$arr86 <- as.numeric(grogger$narr86 %in% c(1:12))
# Linear Probability Model Estimation
lpm <- lm(arr86 ~ pcnv + avgsen + tottime + ptime86 + inc86 + black + hispan + born60, data = grogger)

# Report the result
coeftest(lpm, vcovHC, type = "const")
coeftest(lpm, vcovHC, type = "HC0")
coeftest(lpm, vcov = vcovHC(lpm, type = "const"))
coeftest(lpm, vcov = vcovHC(lpm, type = "HC"))
# vcovHC: A function for extracting the covariance matrix from x is supplied
# const: usual standard errors
# HC: heteroskedasticity-consistent errors

# Save the result as text file
sink("lpm_usual.txt")
print(coeftest(lpm, vcov = vcovHC(lpm, type = "const")))
sink()

sink("lpm_robust.txt")
print(coeftest(lpm, vcoc = vcocHC(lpm, type = "HC")))
sink()

# Comment: Since heteroskedasticity is baked into LPM, use a robust variance-covariance matrix is compulsory.

# Part b
#Test the joint significance of avgsen and tottime

install.packages("aod")
library(aod)
require(stats)

# Select the two variables avgsen and tottime
null <- c(0,0)
testcoef <- as.matrix(rbind(c(0,0,1,0,0,0,0,0,0),c(0,0,0,1,0,0,0,0,0)))

# Wald test and the result as a text file
sink("waldtest_non.txt")
print(wald.test(b = coef(lpm), Sigma = vcovHC(lpm, type = "const"), L = testcoef, H0 = null))
sink() #nonrobust

sink("waldtest_rbst.txt")
print(wald.test(b = coef(lpm), Sigma = vcovHC(lpm, type = "HC"), L = testcoef, H0 = null))
sink() #robust

# Part c
require(aod)

install.packages("mfx")
library(mfx)

# Pobit model and output the result
myprobit <- glm(arr86 ~ pcnv + avgsen + tottime + ptime86 + inc86 + black + hispan + born60, data = grogger)
# Get the coefficient
coeftest(myprobit)
# Get the average
summary(grogger$avgsen)
summary(grogger$tottime)
summary(grogger$ptime86)
summary(grogger$inc86)
# Marginal effects
prob = pnorm(-.5529 * .75 - .119) - pnorm(-.5529 * .25 - .119); prob

sink("myprobit.txt")
print(coeftest(myprobit))
sink()

# Part d
# Generate fitted values
predicted <- fitted(myprobit)
predictedarr <- as.numeric(predicted >= .5)

# Tabulate the fitted values against the actual data and output the result
sink("predicted.txt")
print(table(predictedarr, grogger$arr86))
sink()

# Part e
# New probit model
myprobit2 <- glm(arr86 ~ pcnv + avgsen + tottime + ptime86 + inc86 + black + hispan + born60 + pcnvsq + pt86sq + inc86sq, data = grogger)

# Test for individual significance and output the result
sink("mypobit2.txt")
print(coeftest(myprobit2))
sink()

# Test for joint significance

# Select the three quadratic variables
null2 <- c(0,0,0)
testcoef2 <- as.matrix(rbind(c(0,0,0,0,0,0,0,0,0,1,0,0),
                             c(0,0,0,0,0,0,0,0,0,0,1,0),
                             c(0,0,0,0,0,0,0,0,0,0,0,1)))
# Wald test and output the result
sink("waldtest2.txt")
wald.test(b = coef(myprobit2), Sigma = vcovHC(myprobit2, type = "const"), L = testcoef2, H0 = null2)
sink()

