getSvc1 = function() { rgamma(1, shape = 1.0, scale = 0.9) }
getSvc2 = function() { rgamma(1, shape = 1.05, scale = 0.9) }
getSvc3 = function() { rgamma(1, shape = 1.1, scale = 0.9) }



plot0 <- function(){
  jobs <- c(seq(from=100,to=5000,by=100))
  
  avgTimes1 <- c()
  for (i in seq(from=100,to=5000,by=100))
    avgTimes1 <- append(avgTimes1, ssq(i, 224565, showOutput=FALSE, saveSojournTimes=TRUE)$avgSojourn)
  
  avgTimes2 <- c()
  for (i in seq(from=100,to=5000,by=100))
    avgTimes2 <- append(avgTimes2, ssq(i, 234576, showOutput=FALSE, saveSojournTimes=TRUE)$avgSojourn)
  
  avgTimes3 <- c()
  for (i in seq(from=100,to=5000,by=100))
    avgTimes3 <- append(avgTimes3, ssq(i, 86753, showOutput=FALSE, saveSojournTimes=TRUE)$avgSojourn)
  
  plot(jobs, avgTimes1, xlim=c(0,5000), ylim=c(0,20), ylab="average sojourn times", bty="l")
  points(jobs, avgTimes2, pch=5)
  points(jobs, avgTimes3, pch=19)
  legend(x="topleft", legend=c("224565", "234576", "86753"), title="Initial Seed",cex=0.7, pch=c(1, 5, 19))
  abline(h=9)
  title(main="convergence-to-steady-state")
}
plot0()

##first gamma service
plot1 <- function(){
  jobs <- c(seq(from=100,to=5000,by=100))
  
  avgTimes1 <- c()
  for (i in seq(from=100,to=5000,by=100))
    avgTimes1 <- append(avgTimes1, ssq(i, 224565, showOutput=FALSE, saveSojournTimes=TRUE, serviceFcn = getSvc1)$avgSojourn)
  
  avgTimes2 <- c()
  for (i in seq(from=100,to=5000,by=100))
    avgTimes2 <- append(avgTimes2, ssq(i, 234576, showOutput=FALSE, saveSojournTimes=TRUE, serviceFcn = getSvc1)$avgSojourn)
  
  avgTimes3 <- c()
  for (i in seq(from=100,to=5000,by=100))
    avgTimes3 <- append(avgTimes3, ssq(i, 86753, showOutput=FALSE, saveSojournTimes=TRUE,serviceFcn = getSvc1)$avgSojourn)
  
  plot(jobs, avgTimes1, xlim=c(0,5000), ylim=c(0,20), ylab="average sojourn times", bty="l")
  points(jobs, avgTimes2, pch=5)
  points(jobs, avgTimes3, pch=19)
  legend(x="topleft", legend=c("224565", "234576", "86753"), title="Initial Seed",cex=0.7, pch=c(1, 5, 19))
  abline(h=1/(1/(1*0.9)-1))
  title(main="convergence-to-steady-state-first gamma service")
}
plot1()

##second gamma service
plot2 <- function(){
  jobs <- c(seq(from=100,to=5000,by=100))
  
  avgTimes1 <- c()
  for (i in seq(from=100,to=5000,by=100))
    avgTimes1 <- append(avgTimes1, ssq(i, 224565, showOutput=FALSE, saveSojournTimes=TRUE, serviceFcn = getSvc2)$avgSojourn)
  
  avgTimes2 <- c()
  for (i in seq(from=100,to=5000,by=100))
    avgTimes2 <- append(avgTimes2, ssq(i, 234576, showOutput=FALSE, saveSojournTimes=TRUE, serviceFcn = getSvc2)$avgSojourn)
  
  avgTimes3 <- c()
  for (i in seq(from=100,to=5000,by=100))
    avgTimes3 <- append(avgTimes3, ssq(i, 86753, showOutput=FALSE, saveSojournTimes=TRUE,serviceFcn = getSvc2)$avgSojourn)
  
  plot(jobs, avgTimes1, xlim=c(0,5000), ylim=c(0,30), ylab="average sojourn times", bty="l")
  points(jobs, avgTimes2, pch=5)
  points(jobs, avgTimes3, pch=19)
  legend(x="topleft", legend=c("224565", "234576", "86753"), title="Initial Seed", cex=0.7, pch=c(1, 5, 19))
  abline(h=1/(1/(1.05*0.9)-1))
  title(main="convergence-to-steady-state- second gamma service")
}
plot2()




##third gamma service
plot3 <- function(){
  jobs <- c(seq(from=100,to=5000,by=100))
  
  avgTimes1 <- c()
  for (i in seq(from=100,to=5000,by=100))
    avgTimes1 <- append(avgTimes1, ssq(i, 224565, showOutput=FALSE, saveSojournTimes=TRUE, serviceFcn = getSvc3)$avgSojourn)
  
  avgTimes2 <- c()
  for (i in seq(from=100,to=5000,by=100))
    avgTimes2 <- append(avgTimes2, ssq(i, 234576, showOutput=FALSE, saveSojournTimes=TRUE, serviceFcn = getSvc3)$avgSojourn)
  
  avgTimes3 <- c()
  for (i in seq(from=100,to=5000,by=100))
    avgTimes3 <- append(avgTimes3, ssq(i, 86753, showOutput=FALSE, saveSojournTimes=TRUE,serviceFcn = getSvc3)$avgSojourn)
  
  plot(jobs, avgTimes1, xlim=c(0,5000), ylim=c(0,100), ylab="average sojourn times", bty="l")
  points(jobs, avgTimes2, pch=5)
  points(jobs, avgTimes3, pch=19)
  legend(x="topleft", legend=c("224565", "234576", "86753"), title="Initial Seed", cex=0.7, pch=c(1, 5, 19))
  abline(h=1/(1/(1.1*0.9)-1))
  title(main="convergence-to-steady-state- third gamma service")
}
plot3()


