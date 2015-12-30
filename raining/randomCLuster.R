# Code used for raining/pouring post
# Digitalnonplus.com
# dave@digitalnonplus.com

# Set seed for reproducability
set.seed(1001)


# Quick simulation of three trials of patients coming in to our fake hospital --------
l <- (1/15)
patients <- 20

events <- rexp(patients, l)
events.csum <- cumsum(events)

events2 <- rexp(patients, l)
events.csum2 <- cumsum(events2)

events3 <- rexp(patients, l)
events.csum3 <- cumsum(events3)
x.max <- max(events.csum3, events.csum2,events.csum)

plot(x=events.csum, y=rep(1, patients), pch=19, col="blue", xlab="Minutes", ylim=c(0,4), xlim=c(0,x.max), cex=1, ylab="Trial", 
     main="Simulated Patients Arriving at a Hosptial\n On Average Every 15 mins (λ = 1/15)")
points(x=events.csum2, y=rep(2, patients), col="orange", pch=19)
points(x=events.csum3, y=rep(3, patients), col="purple", pch=19)


# The underlying exponential distribution ---------
x <- seq(0,60,length.out=1000)
y <- dexp(x,rate=l)
plot(x,y, type='l', col="black", lwd=4,yaxs="i",xaxs="i", ylab="Density", xlab="Minutes until next arrival", main="Patient Arrival Time")
e.median <- log(2)/l
polygon(x=c(0,x[x<=e.median],max(x[x<=e.median])), y=c(0,y[x<=e.median],0), col="grey40")
polygon(x=c(min(x[x>=e.median]),x[x>=e.median],min(x[x>=e.median])), y=c(0,y[x>=e.median],0), col="grey60")
legend("topright",legend=c("50% < 10.39 mins", "50% > 10.39 mins"), fill=c("grey40","grey60"))

lines(x=c(e.median,e.median),y=c(0,dexp(e.median,rate=l)), lty=2)
abline(h=0)


#Simulation: Probability of 3 or more arriving within 7 minutes of each other -------
tgt.time <- 7
tgt.patients <- 3
trials <- 200000

#Preinitialize for speed
results <- rep(0,trials)
max.cpatients <- rep(0, trials)


for (i in 1:trials) {
  # Generate our arrival times between each patient starting with "now"
  events.test <- rexp(patients, l)
  consec <- 0
  tcount <- 0
  
  # Iterate over all arrival times, starting with the second
  for (j in 2:length(events.test)) {
    
    # If this arrival time and the previous both meet our criteria, then we have a consecutive series.
    if (events.test[j-1] <= tgt.time && events.test[j] <= tgt.time) {
      tcount = tcount + 1
      
    # If we were in a series and the current arrival time is not, that terminates the series. Record the results.
    } else if (tcount > 0 && events.test[j] > tgt.time) {
        consec <- max(consec, (tcount+1)) # adds back the first patient in the sequence since a sequence must be > 1
        tcount <- 0
    }
  }
    
  # Store the results using indicator variable showing our criteria was met this iteration
  if (consec >= tgt.patients) {
    results[i] <- 1
  }
  
  # Record the max number of patients in a row
  max.cpatients[i] <- consec
  
}
mean(results)
results.hist <- hist(max.cpatients, breaks=seq(0,13),right=F,freq=F, xlab="Max Number of Patients In a Row Per Trial", col="darkred", xaxs="i", main="Maximum Number of Consecutive Patients\n Arriving in < 7 Minutes")

# Random points in a box ----------------------------
x <- runif(500, min=0,max=100)
y <- runif(500, min=0,max=100)
plot(x,y,pch=19,col="blue",cex=.5, xlab="", ylab="", main="500 Random Points")


# Bernoulli Representation --------------------------
# I broke this out into Bernoulli trials instead of geomtric distribution draws
# for illustration purposes only. We are conducting the same thing here.

# Configuration
prob.arrival <- (1/15)
patients <- 20
iterations <- 3

# Initialization
intra.times <- rep(NA,patients*iterations)

for (i in 0:(iterations-1)) {
  count <- 0
  intra.miss <- 0

  # Count the number of flips between patient arrivals
  while (count < patients) {
    flip <- rbinom(1,1,prob.arrival)
    if (flip == 1) {
      count <- count + 1  # A patient arrived
      intra.times[count+(i*patients)] <- intra.miss
      intra.miss <- 0
    } else {
      intra.miss <- intra.miss + 1
    }
  }
  
}

## Used to generate the Bernoulli trial charts
pevents1 <- intra.times[1:20]
pevents1.csum <- cumsum(intra.times[1:20])
pevents2 <- intra.times[21:40]
pevents2.csum <- cumsum(intra.times[21:40])
pevents3 <- intra.times[41:60]
pevents3.csum <- cumsum(intra.times[41:60])
x.max <- max(pevents1.csum, pevents2.csum, pevents3.csum)

plot(x=cumsum(intra.times[1:20]), y=rep(1, patients), pch=19, col="blue", xlab="Minutes", ylim=c(0,4), xlim=c(0,x.max), cex=1, ylab="Trial", 
     main="Simulated Patients Arriving at a Hosptial\n On Average Every 15 mins (p = 0.6667)")
points(x=pevents2.csum, y=rep(2, patients), col="darkorange", pch=19)
points(x=pevents3.csum, y=rep(3, patients), col="purple", pch=19)


# Used when iterations were set to 200000 to generate the histogram
intra.times.trim <- intra.times[intra.times < 60] # truncate to keep same range as previous chart
hist(intra.times.trim, breaks=seq(0,60), right=F, freq=F, xlab="Minutes Between Arrivals", ylab="Density", main="Patient Inter-Arrival Times Using Bernoulli Method")



