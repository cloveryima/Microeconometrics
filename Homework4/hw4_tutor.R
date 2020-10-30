grogger$arr86 <- as.numeric(grogger$narr86 %in% c(1:12))

library(foreign)
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)

#part a

m <- lm(arr86 ~ pcnv + avgsen + tottime + ptime86 + inc86 + black + hispan + born60, data = grogger)

sink("output1.txt")
print(coeftest(m, vcov = vcovHC(m, type = "const")))
sink()

sink("output2.txt")
print(coeftest(m, vcov = vcovHC(m, type = "HC")))
sink()

# part b

install.packages("aod")
library(aod)
require(stats)

vR <- c(0, 0)
mR <- as.matrix(rbind(c(0, 0, 1, 0, 0, 0, 0, 0, 0), c(0, 0, 0, 1, 0, 0, 0, 0, 0)))

sink("output3.txt")
print(wald.test(b = coef(m), Sigma = vcovHC(m, type = "const"), L = mR, H0 = vR))
sink()

sink("output4.txt")
print(wald.test(b = coef(m), Sigma = vcovHC(m, type = "HC"), L = mR, H0 = vR))
sink()

# part c

require(aod)
myprobit <- glm(arr86 ~ pcnv + avgsen + tottime + ptime86 + inc86 + black + hispan + born60, data = grogger)
summary(myprobit)

install.packages("mfx")
library(mfx)

sink("output5.txt")
probitmfx(myprobit, grogger)
sink()

# part d
probabilities <- fitted(myprobit)
predictedarr <- as.numeric(probabilities>=.5)

sink("output6.txt")
table(predictedarr, grogger$arr86)
sink()

sink("output7.txt")
table(predictedarr, grogger$arr86, grogger$narr86)
sink()

myprobit2 <- glm(arr86 ~ pcnv + avgsen + tottime + ptime86 + inc86 + black + hispan + born60 + pcnvsq + pt86sq + inc86sq, data = grogger)
summary(myprobit2)

vR <- c(0, 0, 0)
mR <- as.matrix(rbind(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), 
                      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),
                      c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)))

sink("output8.txt")
print(wald.test(b = coef(myprobit2), Sigma = vcovHC(myprobit2, type = "const"), L = mR, H0 = vR))
sink()
