## Putting csv files into R
## Since you have STATA, I suggest you to convert dta files to csv on STATA and then R can read them by using the following function
## Note: Be sure that the first row is the title of the variables in your csv file
data <- read.table("filename.csv", header = TRUE, sep = ",", stringsAsFactors = False)
unlist(data)

## Structure of Linear Regression
mod <- lm(Dependent_Var ~ Independent_Var1 + Independent_Var2 + Independent_Var1)

## Summary table with coefficients
summary(mod)

#Set working directory
setwd("C:/Users/think/Documents/R/Homework2&3")
#Import the dataset "cornwell. I have converted .dta to .csv in Stata
cornwell <- read.csv("cornwell.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#1. Estimate a model relating the crime rate to the deterrent variables
lm(lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen, data = cornwell, year == 87)

#Regression result
Call:
lm(formula = lcrmrte ~ lprbarr + lprbconv + lprbpris + lavgsen, 
     data = cornwell, subset = year == 87)

Coefficients:
(Intercept)   lprbarr     lprbconv     lprbpris      lavgsen  
-4.86792     -0.72397     -0.47251      0.15967      0.07642 

#2. Add logecrmrteT for 1986 as an additional explanatory variable
cornwell86 <- cornwell[ which(cornwell$year=='86'), ]
cornwell87 <- cornwell[ which(cornwell$year=='87'), ]
model1 <- lm(cornwell87$lcrmrte ~ cornwell87$lprbarr + cornwell87$lprbconv + cornwell87$lprbpris + 
     cornwell87$lavgsen + cornwell86$lcrmrte)

summary(model1)

#Regression result
Call:
  lm(formula = cornwell87$lcrmrte ~ cornwell87$lprbarr + cornwell87$lprbconv + 
       cornwell87$lprbpris + cornwell87$lavgsen + cornwell86$lcrmrte)

Coefficients:
(Intercept)   cornwell87$lprbarr  cornwell87$lprbconv  cornwell87$lprbpris   cornwell87$lavgsen  
-0.76663             -0.18504             -0.03868             -0.12669             -0.15202  
cornwell86$lcrmrte  
0.77981 

#Comment: 

#3. Compute the F statistic for joint signifiance of all the wage variables

model2 <- lm(cornwell87$lcrmrte ~ cornwell87$lprbarr + cornwell87$lprbconv + cornwell87$lprbpris + 
               cornwell87$lavgsen + cornwell86$lcrmrte + cornwell87$lwmfg + cornwell87$lwcon +
               cornwell87$lwfed + cornwell87$lwfir + cornwell87$lwloc + cornwell87$lwser + cornwell87$lwsta
             + cornwell87$lwtrd + cornwell87$lwtuc)
summary(model2)
anova(model1, model2)

#4. Make the test robust to heteroskedasticity
library(MASS)
model1R <- rlm(cornwell87$lcrmrte ~ cornwell87$lprbarr + cornwell87$lprbconv + cornwell87$lprbpris + 
                cornwell87$lavgsen + cornwell86$lcrmrte)
model2R <- rlm(cornwell87$lcrmrte ~ cornwell87$lprbarr + cornwell87$lprbconv + cornwell87$lprbpris + 
                cornwell87$lavgsen + cornwell86$lcrmrte + cornwell87$lwmfg + cornwell87$lwcon +
                cornwell87$lwfed + cornwell87$lwfir + cornwell87$lwloc + cornwell87$lwser + cornwell87$lwsta
              + cornwell87$lwtrd + cornwell87$lwtuc)
anova(model1R, model2R)
anova(model1, model2)

ssr1 <- 

