# Code to test single server priority queue
# Discrete event simulation
# (C) Feburary 2016, digitalnonplus.com
# dave@digitalnonplus.com

rm(list = ls())
set.seed(1002)
source("single_server_q.R")
source("single_server_priority_q.R")

# Test model for correctness. See if values converge to theoretical values as test grows large
# Initialize ----
max.length <- 100000
tri <- seq(100,max.length,by=50)

t.WQ <- rep(0, length(tri))
t.W <- rep(0, length(tri))
t.L <- rep(0, length(tri))
t.LQ <- rep(0, length(tri))

arrival1=0.245
service1=2
service1sd=0.25
count <- 1

for (i in tri) {

  p <- single_server_q(arrival.rate = arrival1, arrival.sd = NA, service.rate = service1, service.sd = service1sd, arrival.dist = "exp", service.dist = "norm", 
                       max.time = i)
  
  cat(paste(count,"[",i,"] ", sep=""))
  t.WQ[count] <- p$avg.wait
  t.W[count] <- p$avg.time.in.system
  t.L[count] <- p$avg.in.system
  t.LQ[count] <- p$avg.in.queue
  count <- count + 1

}

# The theoretical values ----
ES2 <- service1sd^2 + service1^2
WQ <- (arrival1*ES2) / (2*(1-(arrival1*service1)))
W <- WQ + service1
LQ <- arrival1*WQ
L <- arrival1*W

# Take a look at the results ----
plot(x=tri, y=t.WQ, type="l", col="blue", main="Single Server Average Queue Time", xlab="Length of test", ylab="Avg Wait Time")
abline(h=WQ,col="red",lwd=2)

plot(x=tri, y=t.W, type="l", col="blue", main="Single Server Average Total Time", xlab="Length of test", ylab="Avg Total Time")
abline(h=W,col="red", lwd=2)

plot(x=tri, y=t.LQ, type="l", col="blue", main="Single Server Average Queue customers", xlab="Length of test", ylab="Avg Queue Length")
abline(h=LQ,col="red", lwd=2)

plot(x=tri, y=t.L, type="l", col="blue", main="Single Server Average Total customers", xlab="Length of test", ylab="Avg Customers In System")
abline(h=L,col="red", lwd=2)


# save(t.WQ, t.W, t.L, t.LQ, file="single_converge.Rda")


