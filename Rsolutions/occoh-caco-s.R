### R code from vignette source '/home/travis/build/SPE-R/SPE/build/occoh-caco-s.rnw'

###################################################
### code chunk number 1: Read in occoh data
###################################################
library(Epi)
library(survival)
url <- "http://bendixcarstensen.com/SPE/data"
oc <- read.table( paste(url, "occoh.txt", sep = "/"), header=TRUE)
str(oc)
summary(oc)


###################################################
### code chunk number 2: cal.yr
###################################################
oc$ybirth <- cal.yr(oc$birth) 
oc$yentry <- cal.yr(oc$entry) 
oc$yexit <- cal.yr(oc$exit)


###################################################
### code chunk number 3: age.yr
###################################################
oc$agentry <- oc$yentry - oc$ybirth
oc$agexit <- oc$yexit - oc$ybirth 


###################################################
### code chunk number 4: oclexis
###################################################
oc.lex <- Lexis( entry = list( per = yentry, 
                               age = yentry - ybirth ), 
                  exit = list( per = yexit),
           exit.status = chdeath,
                    id = id, data = oc)
str(oc.lex)
summary(oc.lex)


###################################################
### code chunk number 5: plotfile
###################################################
source( paste(url,"plots-caco-ex.R", sep = "/") )
plot1


###################################################
### code chunk number 6: plotlexis
###################################################
plot1()


###################################################
### code chunk number 7: plotlexage
###################################################
oc.ord <- cbind(ID = 1:1501, oc[ order( oc$agexit, oc$agentry), ] )  
oc.lexord <- Lexis( entry = list( age = agentry ), 
                     exit = list( age = agexit),
              exit.status = chdeath,
                       id = ID, data = oc.ord)
plot2
plot2()


###################################################
### code chunk number 8: plotlexage2
###################################################
plot3
plot3()


###################################################
### code chunk number 9: agentry2
###################################################
oc.lex$agen2 <- cut(oc.lex$agentry, br = seq(40, 62, 1) )


###################################################
### code chunk number 10: risksetsample
###################################################
set.seed(9863157)
cactrl <- 
   ccwc(entry=agentry, exit=agexit, fail=chdeath, 
        controls = 2, match= agen2, 
        include = list(id, agentry), 
        data=oc.lex, silent=FALSE)
str(cactrl)


###################################################
### code chunk number 11: ocX
###################################################
ocX <- read.table( paste(url, "occoh-Xdata.txt", sep = "/"), header=TRUE)
str(ocX)


###################################################
### code chunk number 12: merge
###################################################
oc.ncc <- merge(cactrl, ocX[, c("id", "smok", "tchol", "sbp")], 
   by = "id")
str(oc.ncc)


###################################################
### code chunk number 13: factor smol
###################################################
oc.ncc$smok <- factor(oc.ncc$smok, 
    labels = c("never", "ex", "1-14/d", ">14/d"))          


###################################################
### code chunk number 14: cccrude smok
###################################################
stat.table( index = list( smok, Fail ), 
          contents = list( count(), percent(smok) ),
           margins = T, data = oc.ncc )
smok.crncc <- glm( Fail ~ smok, family=binomial, data = oc.ncc)
round(ci.exp(smok.crncc), 3) 


###################################################
### code chunk number 15: clogit
###################################################
m.clogit <- clogit( Fail ~ smok + I(sbp/10) + tchol + 
       strata(Set), data = oc.ncc )
summary(m.clogit)
round(ci.exp(m.clogit), 3)


###################################################
### code chunk number 16: subc sample
###################################################
N <- 1501; n <- 260
set.seed(1579863)
subcids <- sample(N, n )
oc.lex$subcind <- 1*(oc.lex$id %in% subcids)


###################################################
### code chunk number 17: casecoh data
###################################################
oc.cc <- subset( oc.lex, subcind==1 | chdeath ==1)
oc.cc <- merge( oc.cc, ocX[, c("id", "smok", "tchol", "sbp")], 
   by ="id")
str(oc.cc) 


###################################################
### code chunk number 18: casecoh-lines
###################################################
plot4
plot4()


###################################################
### code chunk number 19: grouping
###################################################
oc.cc$smok <- factor(oc.cc$smok, 
    labels = c("never", "ex", "1-14/d", ">14/d"))


###################################################
### code chunk number 20: cc-crude HR by smok
###################################################
sm.cc <- stat.table( index = smok, 
   contents = list( Cases = sum(lex.Xst), Pyrs = sum(lex.dur) ),
	 margins = T, data = oc.cc)
print(sm.cc, digits = c(sum=0, ratio=1))
HRcc <- (sm.cc[ 1, -5]/sm.cc[ 1, 1])/(sm.cc[ 2, -5]/sm.cc[2, 1])		
round(HRcc, 3)			


###################################################
### code chunk number 21: weights
###################################################
N.nonc <- N-sum(oc.lex$chdeath)  # non-cases in whole cohort
n.nonc <- sum(oc.cc$subcind * (1-oc.cc$chdeath)) # non-cases in subcohort
wn <- N.nonc/n.nonc          # weight for non-cases in subcohort
c(N.nonc, n.nonc, wn)
oc.cc$w <- ifelse(oc.cc$subcind==1 & oc.cc$chdeath==0, wn, 1)


###################################################
### code chunk number 22: weighted cox
###################################################
oc.cc$surob <- with(oc.cc, Surv(agentry, agexit, chdeath) )
cc.we <- coxph( surob ~  smok + I(sbp/10)  + tchol, robust = TRUE,  
       weight = w, data = oc.cc)
summary(cc.we)
round( ci.exp(cc.we), 3)


###################################################
### code chunk number 23: weighted cox dfb
###################################################
dfbw <- resid(cc.we, type='dfbeta')
covdfb.we <- cc.we$naive.var + 
   (n.nonc*(N.nonc-n.nonc)/N.nonc)*var(dfbw[ oc.cc$chdeath==0, ] )
cbind( sqrt(diag(cc.we$naive.var)), sqrt(diag(cc.we$var)),  
    sqrt(diag(covdfb.we))  )


###################################################
### code chunk number 24: weighted cox LinYing
###################################################
cch.LY <- cch( surob ~  smok + I(sbp/10)  + tchol, stratum=NULL,
   subcoh = ~subcind, id = ~id,  cohort.size = N, data = oc.cc, 
    method ="LinYing" )
summary(cch.LY)


###################################################
### code chunk number 25: weighted cox SEs
###################################################
cbind( coef( cc.we), coef(cch.LY) )
round( cbind( sqrt(diag(cc.we$naive.var)), sqrt(diag(cc.we$var)),  
    sqrt(diag(covdfb.we)), sqrt(diag(cch.LY$var))  ), 3)


###################################################
### code chunk number 26: fullcoh
###################################################
oc.full <- merge( oc.lex, ocX[, c("id", "smok", "tchol", "sbp")], 
   by.x = "id", by.y = "id") 
oc.full$smok <- factor(oc.full$smok, 
    labels = c("never", "ex", "1-14/d", ">14/d"))


###################################################
### code chunk number 27: cox-crude HR by smok
###################################################
sm.coh <- stat.table( index = smok, 
   contents = list( Cases = sum(lex.Xst), Pyrs = sum(lex.dur) ),
	 margins = T, data = oc.full)
print(sm.coh, digits = c(sum=0, ratio=1))
HRcoh <- (sm.coh[ 1, -5]/sm.coh[ 1, 1])/(sm.coh[ 2, -5]/sm.coh[2, 1])		
round(HRcoh, 3)			


###################################################
### code chunk number 28: cox full
###################################################
cox.coh <- coxph( Surv(agentry, agexit, chdeath) ~ 
        smok + I(sbp/10)  + tchol, data = oc.full)
summary(cox.coh)        


###################################################
### code chunk number 29: comparison
###################################################
betas <- round(cbind( coef(cox.coh), 
     coef(m.clogit),
     coef(cc.we), coef(cch.LY) ), 3)
colnames(betas) <-  c("coh", "ncc", "cc.we", "cch.LY")
betas

SEs <- round(cbind( sqrt(diag(cox.coh$var)), 
    sqrt(diag(m.clogit$var)), sqrt(diag(cc.we$naive.var)),
    sqrt(diag(cc.we$var)),  sqrt(diag(covdfb.we)),
    sqrt(diag(cch.LY$var)) ), 3)
colnames(SEs) <- c("coh", "ncc", "ccwe-nai", 
       "ccwe-rob", "ccwe-dfb", "cch-LY")
SEs


