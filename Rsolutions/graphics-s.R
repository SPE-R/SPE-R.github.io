### R code from vignette source '/home/travis/build/SPE-R/SPE/build/graphics-s.rnw'

###################################################
### code chunk number 1: graphics-s.rnw:6-7
###################################################
( alkfos <- read.csv("./data/alkfos.csv") )


###################################################
### code chunk number 2: graphics-s.rnw:12-13
###################################################
(available <- aggregate( !is.na(alkfos), list(alkfos$grp), sum))


###################################################
### code chunk number 3: graphics-s.rnw:18-22
###################################################
alkfos.pctchange <- (sweep(alkfos[-1], 1, alkfos$c0, "/") - 1)*100
(means <- aggregate(alkfos.pctchange, list(alkfos$grp), mean, na.rm=TRUE))
(sds   <- aggregate(alkfos.pctchange, list(alkfos$grp),   sd, na.rm=TRUE))
available <- as.matrix(available[-(1:2)])


###################################################
### code chunk number 4: graphics-s.rnw:27-32
###################################################
means <- as.matrix(means[-1])
  sds <- as.matrix(sds[-1])
 sems <- sds/sqrt(available)
  upr <- means + sems
  lwr <- means - sems


###################################################
### code chunk number 5: graphics-s.rnw:36-38
###################################################
time <- c(0,3,6,9,12,18,24)
(ylim <- range(means+sems,means-sems))


###################################################
### code chunk number 6: alkfos
###################################################
par(mar=.1 + c(8,4,4,2))
plot( time, means[1,],
      type="b",   # boths dots and lines
      xaxt="n",   # no x-axis
      ylim=ylim,
      ylab="alkaline phosphatase" )
# Add the points for the second gropu
points(time,means[2,], type="b")
# The the vertial error-bars
segments( time, upr[1,], time, lwr[1,] )
segments( time, upr[2,], time, lwr[2,])
# Draw the x-axis at the times
axis( 1, at=time )
# Plot the availabe numbers below the x-axis
mtext( available[1,], side=1, line=5, at=time)
mtext( available[2,], side=1, line=6, at=time)


###################################################
### code chunk number 7: alkfos-x
###################################################
par(mar=.1 + c(8,5,4,2))
plot(time,means[1,], type="b", ylim=ylim, xaxt="n",
                     ylab="% change in alkaline phosphatase",
                     xlab="Months after randomization",
                     las=1, # All axis labels horizontal
                     pch=16, # Dot as plotting symbol
                     bty="n", lwd=2 ) # No box around the plot
segments(time ,  upr[1,], time , lwr[1,], lwd=2 )
time2 <- time + 0.25 # Plot the points and bars a little bit offset
points  (time2,means[2,], type="b", pch=18, lwd=2, cex=1.3)
segments(time2,  upr[2,], time2, lwr[2,], lwd=2 )
axis(1,at=time)
mtext(available[1,],side=1, line=5, at=time)
mtext(available[2,],side=1, line=6, at=time)
# par("usr") reaturns the actual x- and y coordinates of the axis ends.
mtext("Placebo"  , side=1, line=5, adj=1, at=par("usr")[1] )
mtext("Tamoxifen", side=1, line=6, adj=1, at=par("usr")[1] )
abline(h=0) # Horizontal line


