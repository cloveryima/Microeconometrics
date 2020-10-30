install.packages("texreg") 
install.packages("systemfit")
install.packages("plm")
library(texreg)
library(systemfit)
library(plm)
library(haven)
suppressMessages(library(foreign))
suppressMessages(library(texreg))
suppressMessages(library(systemfit)) 

#Supress srandard warning on Stata 5 data labels
df=suppressWarnings(read.dta(file.path("C:/Users/think/Documents/R/Homework7", "fringe.dta")))

# part a

# hourly wage
eqn.1 = hrearn ~ educ + exper + expersq + tenure + tenuresq + union + south + nrtheast + nrthcen + married + white + male
# hourly benefits
eqn.2 = hrbens ~ educ + exper + expersq + tenure + tenuresq + union + south + nrtheast + nrthcen + married + white + male

res.FGLSa = systemfit(system.base,method="SUR",data=df)
summary(res.FGLSa)

# part b

res.SOLS <- systemfit (system.base,method="OLS",data=df)
summary(res.SOLS)

# part c is in the pdf file

# part d

# generate new variables
df$hrvacdays <- df$vacdays/df$annhrs
df$hrsicklve <- df$sicklve/df$annhrs
df$hrinsur <- df$insur/df$annhrs
df$hrpension <- df$pension/df$annhrs

# Estimate a SUR model using these four components
eqn.1 = hrvacdays ~ educ + exper + expersq + tenure + tenuresq + union + south + nrtheast + nrthcen + married + white + male
eqn.2 = hrsicklve ~ educ + exper + expersq + tenure + tenuresq + union + south + nrtheast + nrthcen + married + white + male
eqn.3 = hrinsur ~ educ + exper + expersq + tenure + tenuresq + union + south + nrtheast + nrthcen + married + white + male
eqn.4 = hrpension ~ educ + exper + expersq + tenure + tenuresq + union + south + nrtheast + nrthcen + married + white + male
eqn.5 = hrearn ~ educ + exper + expersq + tenure + tenuresq + union + south + nrtheast + nrthcen + married + white + male

system.base = list(vacation = eqn.1, sickleave = eqn.2, insurance = eqn.3, pension = eqn.4, earnings = eqn.5)
res.FGLS = systemfit(system.base, method="SUR", data=df)
summary(res.FGLS)

# part e 

m1 = matrix(0, nrow = 5, ncol = 65)
m1[1, 11] = 1
m1[2, 24] = 1
m1[3, 37] = 1
m1[4, 50] = 1
m1[5, 63] = 1
linearHypothesis(res.FGLS, hypothesis.matrix = m1, test = "F")

# part f
m2 = matrix(0, nrow = 1, ncol = 65)
m2[1, 41] = 1
m2[1, 54] = -1
linearHypothesis(res.FGLS, hypothesis.matrix = m2, test = "F")
