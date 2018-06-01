### R code from vignette source '/home/travis/build/SPE-R/SPE/build/causal-s.rnw'

###################################################
### code chunk number 1: beerdata_1
###################################################
set.seed(02062017)
bdat= data.frame(sex = c(rep(0,500),rep(1,500))  )
                   # a data frame with 500 females, 500 males
bdat$beer <- rbinom(1000,1,0.2+0.5*bdat$sex)
bdat$weight <- 60 + 10*bdat$sex + rnorm(1000,0,7)
bdat$bp <- 110 + 0.5*bdat$weight + 10*bdat$beer + rnorm(1000,0,10)


###################################################
### code chunk number 2: beermodels_1
###################################################
library( Epi )
m1a<-lm(weight~beer, data=bdat)
m2a<-lm(weight~beer+sex, data=bdat)
m3a<-lm(weight~beer+sex+bp, data=bdat)
ci.lin(m1a)
ci.lin(m2a)
ci.lin(m3a)


###################################################
### code chunk number 3: beerdata_2
###################################################
bdat$weight <- 60 + 10*bdat$sex + 2*bdat$beer + rnorm(1000,0,7)


###################################################
### code chunk number 4: beermodels_2b
###################################################
bdat$bp <- 110 +0.5*bdat$weight  + 10*bdat$beer+ rnorm(1000,0,10)  #
m1b<-lm(weight~beer,data=bdat)
m2b<-lm(weight~beer+sex,data=bdat)
m3b<-lm(weight~beer+sex+bp,data=bdat)
ci.lin(m1b)
ci.lin(m2b)    # the correct model
ci.lin(m3b)


###################################################
### code chunk number 5: bpmodel
###################################################
m1bp<-lm(bp~beer,data=bdat)
m2bp<-lm(bp~beer+weight+sex,data=bdat)
m3b<-lm(weight~beer+sex+bp,data=bdat)
ci.lin(m1bp)
ci.lin(m2bp)    # the correct model


###################################################
### code chunk number 6: mrdat1
###################################################
 n <- 10000
 mrdat <- data.frame(G = rbinom(n,2,0.2))
 table(mrdat$G)


###################################################
### code chunk number 7: mrdat2
###################################################
mrdat$U <- rnorm(n)


###################################################
### code chunk number 8: mrdat3
###################################################
mrdat$BMI <- with(mrdat, 25 + 0.7*G + 2*U + rnorm(n) )


###################################################
### code chunk number 9: mrdat4
###################################################
mrdat$Y <- with(mrdat, 3 + 0.1*BMI - 1.5*U + rnorm(n,0,0.5) )


###################################################
### code chunk number 10: mrmod1
###################################################
mxy<-lm(Y ~ BMI, data=mrdat)
ci.lin(mxy)


###################################################
### code chunk number 11: mrmod2
###################################################
mxyg<-lm(Y ~ G + BMI, data=mrdat)
ci.lin(mxyg)


###################################################
### code chunk number 12: mrmod3
###################################################
mgx<-lm(BMI ~ G, data=mrdat)
ci.lin(mgx)  # check the instrument effect
bgx<-mgx$coef[2]   # save the 2nd coefficient (coef of G) 
mgy<-lm(Y ~ G, data=mrdat)
ci.lin(mgy)
bgy<-mgy$coef[2]
causeff <- bgy/bgx
causeff    # closer to 0.1?


###################################################
### code chunk number 13: mrsim
###################################################
n <- 10000
# initializing simulations:
# 30 simulations (change it, if you want more):
nsim<-30       
mr<-rep(NA,nsim)   # empty vector for the outcome parameters
for (i in 1:nsim) { # start the loop
### Exactly the same commands as before:
mrdat <- data.frame(G = rbinom(n,2,0.2))
mrdat$U <- rnorm(n)
mrdat$BMI <- with(mrdat, 25 + 0.7*G + 2*U + rnorm(n) )
mrdat$Y <- with(mrdat, 3 + 0.1*BMI - 1.5*U + rnorm(n,0,0.5) )
mgx<-lm(BMI ~ G, data=mrdat)
bgx<-mgx$coef[2]
mgy<-lm(Y ~ G, data=mrdat)
bgy<-mgy$coef[2]
# Save the i'th parameter estimate:
mr[i]<-bgy/bgx
}   # end the loop


###################################################
### code chunk number 14: mrsim2
###################################################
summary(mr)    


###################################################
### code chunk number 15: mrsim3
###################################################
mrdat$Y <- with(mrdat, 3 + 0.1*BMI - 1.5*U + 0.05*G + rnorm(n,0,0.5) )


###################################################
### code chunk number 16: tsls
###################################################
library(sem)
summary(tsls(Y ~ BMI, ~G, data=mrdat))


