# Code to test single server priority queue
# Discrete event simulation
# (C) Feburary 2016, Digitialnonplus.com
# dave@digitalnonplus.com

rm(list = ls())
set.seed(1002)
source("single_server_q.R")
source("single_server_priority_q.R")

# Test model for correctness. See if values converge to theoretical values as test grows large

# Initialization -----
max.length <- 150000
tri <- seq(100,max.length,by=50)
class1.wait <- rep(0, length(tri))
class1.total <- rep(0, length(tri))
class1.q <-  rep(0, length(tri))
class1.sys <- rep(0, length(tri))
class2.wait <- rep(0, length(tri))
class2.total <- rep(0, length(tri))
class2.q <-  rep(0, length(tri))
class2.sys <- rep(0, length(tri))

arrival1=0.245
arrival2=0.03
service1=2
service1sd=0.25
service2=3
service2sd=0.25
count <- 1

for (i in tri) {

  p2 <- single_server_priority_q(arrival1.rate=arrival1, arrival1.sd=NA, arrival2.rate=arrival2, arrival2.sd=NA,
                                 service1.rate=service1, service1.sd=service1sd, service2.rate=service2, service2.sd=service2sd,
                                 arrival1.dist="exp", arrival2.dist="exp", service1.dist="norm", service2.dist="norm",
                                 max.time=i)
                               
 class1.wait[count] <- p2$avg.wait.class1
 class1.total[count] <- p2$avg.total.class1
 class1.q[count] <- p2$avg.in.q.class1
 class1.sys[count] <- p2$avg.in.system.class1
 class2.wait[count] <- p2$avg.wait.class2
 class2.total[count] <- p2$avg.total.class2
 class2.q[count] <- p2$avg.in.q.class2
 class2.sys[count] <- p2$avg.in.system.class2
 
 count <- count + 1
 cat(paste(count,"[",i,"] ", sep=""))

}

# The theoretical values -----
ES12 <- service1sd^2 + service1^2
ES22 <- service2sd^2 + service2^2
WQ2 <- ((arrival1 * ES12) + (arrival2 * ES22)) / (2 * (1 - arrival1*service1 - arrival2*service2)*(1-arrival1*service1) )
WQ1 <- ((arrival1 * ES12) + (arrival2 * ES22)) / (2*(1-(arrival1*service1)))

# Take a look at the results -----

# Class 1
plot(x=tri, y=class1.wait, type="l", col="blue", main="Class 1 Queue Average Wait Time", xlab="Length of test", ylab="Avg Wait Time")
abline(h=WQ1,col="red", lwd=2)

plot(x=tri, y=class1.total, type="l", col="blue", main="Class 1 Average Total Time", xlab="Length of test", ylab="Avg Total Time")
samps <- length(class1.total)
WE <- mean(class1.total[(samps-1000):samps])
abline(h=WE, col="red", lwd=3, lty=2)

plot(x=tri, y=class1.q, type="l", col="blue", main="Class 1 Average Queue Size", xlab="Length of test", ylab="Avg Queue Size")
samps <- length(class1.q)
LQE <- mean(class1.q[(samps-1000):samps])
abline(h=LQE, col="red", lwd=3, lty=2)

plot(x=tri, y=class1.sys, type="l", col="blue", main="Class 1 Average Total In System", xlab="Length of test", ylab="Avg Customers In System")
samps <- length(class1.sys)
LE <- mean(class1.sys[(samps-200):samps])
abline(h=LE, col="red", lwd=3, lty=2)

# class 2
plot(x=tri, y=class2.wait, type="l", col="blue", main="Class 2 Queue Average Wait Time", xlab="Length of test", ylab="Avg Wait Time")
abline(h=WQ2,col="red", lwd=2)

plot(x=tri, y=class2.total, type="l", col="blue", main="Class 2 Average Total Time", xlab="Length of test", ylab="Avg Total Time")
samps <- length(class2.total)
WE <- mean(class2.total[(samps-1000):samps])
abline(h=WE, col="red", lwd=3, lty=2)

plot(x=tri, y=class2.q, type="l", col="blue", main="Class 2 Average Queue Size", xlab="Length of test", ylab="Avg Queue Size")
samps <- length(class2.q)
LQE <- mean(class2.q[(samps-1000):samps])
abline(h=LQE, col="red", lwd=3, lty=2)

plot(x=tri, y=class2.sys, type="l", col="blue", main="Class 2 Average Total In System", xlab="Length of test", ylab="Avg Customers In System")
samps <- length(class2.sys)
LE <- mean(class2.sys[(samps-200):samps])
abline(h=LE, col="red", lwd=3, lty=2)

# save(class1.wait, class1.total, class1.q, class1.sys, class2.wait, class2.total, class2.q,class2.sys, file="priority_converge3.Rda")
# load(file="priority_converge3.Rda")


