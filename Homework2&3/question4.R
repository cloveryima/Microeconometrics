#Import CPS data
cps09 <- read.csv("cps09mar.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
#Subsetting data with white hispanic male
cps09whm <- cps09[ which(cps09$female == '0' & cps09$race == '1' & cps09$hisp == '1'), ]
#log wage
logwage <- log(cps09whm$earnings)
#create experience
exp <- (cps09whm$age - cps09whm$education)

#create dummies
northeast <- integer(4230)
south <- integer(4230)
west <- integer(4230)

married <- integer(4230)
widoweddivorced <- integer(4230)
separated <- integer(4230)

hsd <- integer(4230)
ba <- integer(4230)
ma <- integer(4230)
pd <- integer(4230)

for (i in 1:4230){
  if (cps09whm$region[i] == 1){
    northeast[i] = 1
    south[i] = 0
    west[i] = 0
  }
  if (cps09whm$region[i] == 2){
    northeast[i] = 0
    south[i] = 0
    west[i] = 0
  }
  if (cps09whm$region[i] == 3){
    northeast[i] = 0
    south[i] = 1
    west[i] = 0
  }
  if (cps09whm$region[i] == 4){
    northeast[i] = 0
    south[i] = 0
    west[i] = 1
  }
  
  if(cps09whm$marital[i] >= 1 & cps09whm$marital[i] <= 3){
    married[i] <- 1
    widoweddivorced[i] <- 0
    separated[i] <- 0
  }
  if(cps09whm$marital[i] >= 4 & cps09whm$marital[i] <= 5){
    married[i] <- 0
    widoweddivorced[i] <- 1
    separated[i] <- 0
  }
  if(cps09whm$marital[i] == 6){
    married[i] <- 0
    widoweddivorced[i] <- 0
    separated[i] <- 1
  }

  if(cps09whm$education[i] >= 12 & cps09whm$education[i] <= 14){
    hsd[i] <- 1
    ba[i] <- 0
    ma[i] <- 0
    pd[i] <- 0
  }
  if(cps09whm$education[i] == 16 ){
    hsd[i] <- 0
    ba[i] <- 1
    ma[i] <- 0
    pd[i] <- 0
  }
  if(cps09whm$education[i] == 18 ){
    hsd[i] <- 0
    ba[i] <- 0
    ma[i] <- 1
    pd[i] <- 0
  }
  if(cps09whm$education[i] == 20 ){
    hsd[i] <- 0
    ba[i] <- 0
    ma[i] <- 0
    pd[i] <- 1
  }
  
}

model1 <- lm(logwage ~ northeast + south + west + married + widoweddivorced + separated + hsd + ba 
             + ma + pd + exp + I(exp^2))
summary(model1)

model2 <- lm(cps09whm$earnings ~ northeast + south + west + married + widoweddivorced + separated 
             + hsd + ba + ma + pd + exp + I(exp^2))
summary(model2)

#1. Estimate the marginal effect on the mean log wage of "Married" versus "Single/Never Married."

Call:
  lm(formula = logwage ~ northeast + south + west + married + widoweddivorced + 
       separated + hsd + ba + ma + pd + exp + I(exp^2))

Coefficients:
(Intercept)       northeast            south             west          married  widoweddivorced  
9.3774845        0.0737242       -0.0488916        0.0355255        0.2037306        0.1318542  
separated              hsd               ba               ma               pd              exp  
0.0343420        0.3921270        0.7791347        1.1724133        1.5530346        0.0350898  
I(exp^2)  
-0.0004652 

#The coefficient on "Married", which is 0.2037306, is the marginal effect

#2. Estimate the marginal effect on the mean wage implied by your result from the previous part.

Call:
  lm(formula = cps09whm$earnings ~ northeast + south + west + married + 
       widoweddivorced + separated + hsd + ba + ma + pd + exp + 
       I(exp^2))

Coefficients:
  (Intercept)        northeast            south             west          married  widoweddivorced  
-3755.77          6827.06         -1390.74          2217.38          8385.50          3759.81  
separated              hsd               ba               ma               pd              exp  
-800.90         14829.90         36479.63         79359.06        115074.00          1565.03  
I(exp^2)  
-21.18 

#The coefficient on "Married", which is 8385.50, is the marginal effect

#3. Estimate the marginal effect on the mean wage implied by an increase in educational attainment
#from "BA or equivalent" to "MA or equivalent."
diffmaba <- 79359.06 - 36479.63
diffmaba
#The marginal effect on the mean wage implied by an increase in educational attainment from "BA" to
#"MA" is 42879.43

#4. Estimate the median marginal effect on the mean log wage, across your sample, of an increase
#in experience of one year.
#log wage
logwage1 <- log(cps09$earnings)
#create experience
exp1 <- (cps09$age - cps09$education)

model <- lm(logwage1 ~ northeast + south + west + married + widoweddivorced + separated + hsd + ba 
             + ma + pd + exp + I(exp^2))