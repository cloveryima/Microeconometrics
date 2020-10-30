install.packages('wooldridge')
library(wooldridge)

install.packages("lmtest")
library(lmtest)

install.packages("sandwich")
library(sandwich)

data("smoke")

# Part a
# Linear regression model
lrm = lm(cigs ~ lcigpric + lincome + restaurn + white + educ + age + agesq, data = smoke)
# Result without HC
coeftest(lrm, vcov = vcovHC(lrm, type = "const"))
# Result with HC
coeftest(lrm, vcov = vcovHC(lrm, type = "HC0"))

# Part b
table(smoke$cigs)
?smoke
hist(predict(lrm))
plot(lrm$model$cigs, fitted(lrm))

glm.pois = glm(cigs ~ lcigpric + lincome + restaurn + white + educ + age + agesq, 
               data = smoke, family = poisson(link = "log"))
summary(glm.pois)


# Part c
u.P <- resid(glm.pois, type = "pearson") # Pearson residual
u.P.sq <- u.P^2
res.glm.quasipois<- glm(cigs~ lcigpric+lincome+restaurn+white +educ +age+agesq, data=smoke, family = "quasipoisson")
summary(res.glm.quasipois)


#part c 
# pearson residual
# U.P <-resid(res.glm.pois, type = "pearson")
# U.Psq <- U.P^2
res.glm.quasipois<- glm(cigs~ lcigpric+lincome+restaurn+white +educ +age+agesq, data=smoke, family = "quasipoisson")
summary(res.glm.quasipois)

# Part d
# See the pdf document