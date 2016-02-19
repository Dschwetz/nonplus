# Single server priority queue
# Discrete event simulation
# (C) Feburary 2016, digitalnonplus.com
# dave@digitalnonplus.com

single_server_priority_q <- function(arrival1.rate, arrival1.sd=NA, arrival2.rate, arrival2.sd=NA, 
                                     service1.rate, service1.sd=NA, service2.rate, service2.sd=NA, 
                                     arrival1.dist="exp", arrival2.dist="exp",
                                     service1.dist="exp", service2.dist="exp", max.time = 60) {
  # Simulates a single server priority queue using discrete time simulation. Assumes stationary distributions throughout simulation.
  # Allows for different service distributions for each class of customer if desired, but only one server. 
  # Also allows for  different arrival distributions for each customer class.
  # Can be modified to support general G/G/1 queues. Has been tested with exponential and normal service 
  # and arrival times based on the functions passed in.
  
  # Note that class 1 is assumed the higher priority class
  
# Define our distribution functions ------
  if (arrival1.dist == "exp") {
    arrival1 <- function() rexp(1, rate=arrival1.rate)
  } else if (arrival1.dist == "norm") {
    arrival1 <- function() rnorm(1, mean=arrival1.rate, sd=arrival1.sd)
  } else {
    stop("Non-supported arrival1 function specified (exp, norm supported)")
  }
  
  if (arrival2.dist == "exp") {
    arrival2 <- function() rexp(1, rate=arrival2.rate)
  } else if (arrival2.dist == "norm") {
    arrival2 <- function() rnorm(1, mean=arrival2.rate, sd=arrival2.sd)
  } else {
    stop("Non-supported arrival2 function specified (exp, norm supported)")
  }
  
  if (service1.dist == "exp") {
    service1 <- function() rexp(1, rate=service1.rate)
  } else if (service1.dist == "norm") {
    service1 <- function() rnorm(1, mean=service1.rate, sd=service1.sd)
  } else {
    stop("Non-supported service function1 specified (exp, norm supported)")
  }
  
  if (service2.dist == "exp") {
    service2 <- function() rexp(1, rate=service2.rate)
  } else if (service2.dist == "norm") {
    service2 <- function() rnorm(1, mean=service2.rate, sd=service2.sd)
  } else {
    stop("Non-supported service function1 specified (exp, norm supported)")
  }

  
# Sanity checks for unbounded infinite queue configuration -----
# Check for conditions for G/G/1
  WQ1.sanity.check <- arrival1.rate*service1.rate
  
  if (WQ1.sanity.check >= 1) {
    warning(paste("Breaking on infinite queue for class 1. Value: ", WQ1.sanity.check, " not less than 1.", sep=""))
  } 
  
  WQ2.sanity.check <- arrival1.rate*service1.rate + arrival2.rate*service2.rate
  
  if (WQ2.sanity.check >= 1) {
    warning(paste("Breaking on infinite queue for class 2. Value: ", WQ2.sanity.check, " not less than 1.", sep=""))
  }
  
# Initialize the simulation ------
  cars.total <- 0
  n1 <- n2 <- t <- 0
  n1.total.in <- n2.total.in <- n1.total.out <- n2.total.out <- 0

  t.a1 <- arrival1()
  t.a2 <- arrival2()
  t.s1 <- t.s2 <- t.d1 <- t.d2 <- Inf

  length.counter.prog <- 10000
  counter.prog <- 1
 
  prog <- matrix(as.numeric(NA), ncol=13, nrow=length.counter.prog)
  prog[counter.prog,] <- c(t, n1, n2, t.a1, t.a2, t.s1, t.s2, t.d1, t.d2, n1.total.in, n1.total.out, n2.total.in, n2.total.out)
  colnames(prog) <- c("t", "n1", "n2", "t.a1", "t.a2", "t.s1", "t.s2", "t.d1", "t.d2", "n1.total.in", "n1.total.out", "n2.total.in", "n2.total.out")
  
  length.counter <- length.counter.n1 <- 100
  entry.class2 <- vector("numeric",length.counter)
  process.class2 <- vector("numeric", length.counter)
  exit.class2 <- vector("numeric", length.counter)
  
  entry.class1 <- vector("numeric",length.counter)
  process.class1 <- vector("numeric", length.counter)
  exit.class1 <- vector("numeric", length.counter)

while (min(t.a1, t.a2, t.d1, t.d2) < max.time) {

  # Case 1: N2 > 0 and Ta1 is min
  if (n2 > 0 && t.a1 == min(t.a1, t.a2, t.d1, t.d2)) {
    t <- t.a1
    n1 <- n1 + 1
    n1.total.in <- n1.total.in + 1
    t.a1 <- t + arrival1()
    entry.class1[n1.total.in] <- t
    if (n1==1) {
      t.s1 <- t.d2
      t.d1 <- t.s1 + service1()
      process.class1[n1.total.in] <- t.s1
      exit.class1[n1.total.in] <- t.d1
    }
    
  } else 
    
    # Case 2: N2 == 0 and Ta1 is min
    if (n2 == 0 && t.a1 == min(t.a1, t.a2, t.d1, t.d2)) {
      t <- t.a1
      n1 <- n1 + 1
      n1.total.in <- n1.total.in + 1
      t.a1 <- t + arrival1()
      entry.class1[n1.total.in] <- t
      if (n1 == 1) {
        t.s1 <- t
        t.d1 <- t.s1 + service1()
        process.class1[n1.total.in] <- t.s1
        exit.class1[n1.total.in] <- t.d1
      }

    } else
      
    # Case 3: N1 > 0 and Ta2 is min
    if (n1 > 0 && t.a2 == min(t.a1, t.a2, t.d1, t.d2)) {
        t <- t.a2
        n2 <- n2 + 1
        n2.total.in <- n2.total.in + 1
        t.a2 <- t + arrival2()
        entry.class2[n2.total.in] <- t
    } else
        
    # Case 4: N1 == 0 and Ta2 is min
    if (n1 == 0 && t.a2 == min(t.a1, t.a2, t.d1, t.d2)) {
        t <- t.a2
        n2 <- n2 + 1
        n2.total.in <- n2.total.in + 1
        t.a2 <- t + arrival2()
        entry.class2[n2.total.in] <- t
        if (n2 == 1) {
          t.s2 <- t
          t.d2 <- t.s2 + service2()
          process.class2[n2.total.in] <- t.s2
          exit.class2[n2.total.in] <- t.d2
          } # otherwise it was set already
    } else
          
    # Case 5: N2 > 0 and Td1 is min
    if (n2 > 0 && t.d1 == min(t.a1, t.a2, t.d1, t.d2)) {
        t <- t.d1
        n1 <- n1 - 1
        n1.total.out <- n1.total.out + 1
        if (n1 == 0) {
          t.s2 <- t
          # t.d2 <- t.s2 + cars2.service[n2.total.out]
          t.d2 <- t.s2 + service2()
          process.class2[n2.total.out+1] <- t.s2
          exit.class2[n2.total.out+1] <- t.d2
          t.d1 <- Inf
          t.s1 = Inf
        } else {
          t.s1 <- t
          t.d1 <- t.s1 + service1()
          process.class1[n1.total.out+1] <- t.s1
          exit.class1[n1.total.out+1] <- t.d1
          t.s2 <- Inf
          t.d2 <- Inf
        }
      } else
          
      # Case 6: N2 == 0 and Td1 is min
      if (n2 == 0 && t.d1 == min(t.a1, t.a2, t.d1, t.d2)) {
        t <- t.d1
        n1 <- n1 - 1
        n1.total.out <- n1.total.out + 1
        if (n1 > 0) {
          t.s1 <- t
          t.d1 <- t.s1 + service1()
          process.class1[n1.total.out+1] <- t.s1
          exit.class1[n1.total.out+1] <- t.d1
        } else {
          t.s1 <- Inf
          t.d1 <- Inf
        }
      } else
              
      # Case 7: N1 > 0 and Td2 is min
      if (n1 > 0 && t.d2 == min(t.a1, t.a2, t.d1, t.d2)) {
        t <- t.d2
        n2 <- n2 - 1
        n2.total.out <- n2.total.out + 1
        t.s2 <- Inf
        t.d2 <- Inf
      } else 
                
      # Case 8: N1 == 0 and Td2 is min
      if (n1 == 0 && t.d2 == min(t.a1, t.a2, t.d1, t.d2)) {
        t <- t.d2
        n2 <- n2 - 1
        n2.total.out <- n2.total.out + 1
        if (n2 > 0) {
          t.s2 <- t
          t.d2 <- t.s2 + service2()
          process.class2[n2.total.out+1] <- t.s2
          exit.class2[n2.total.out+1] <- t.d2
        } else {
          t.s2 <- Inf
          t.d2 <- Inf
        }
      }  
  

  counter.prog <- counter.prog + 1
  prog[counter.prog,] <- c(t, n1, n2, t.a1, t.a2, t.s1, t.s2, t.d1, t.d2, n1.total.in, n1.total.out, n2.total.in, n2.total.out)
  
  # Reallocate. Done this way as a poor man's tradeoff with memory for speed
  if (length.counter.prog - counter.prog <= 1) {
    p.temp <- matrix(as.numeric(NA), ncol=13, nrow=length.counter.prog)
    prog <- rbind(prog,p.temp)
    length.counter.prog <- length.counter.prog + 10000
  }
  
  if (n2.total.in - length.counter >= -1) {
    filler <- rep(0,100)
    entry.class2 <- c(entry.class2,filler)
    process.class2 <- c(process.class2, filler)
    exit.class2 <- c(exit.class2,filler)
    length.counter <- length.counter + 100
  }
  
  if (n1.total.in - length.counter.n1 >= -1) {
    filler <- rep(0,100)
    entry.class1 <- c(entry.class1,filler)
    process.class1 <- c(process.class1, filler)
    exit.class1 <- c(exit.class1,filler)
    length.counter.n1 <- length.counter.n1 + 100
  }
}
  
# Calculate quantities of interest from run -----
  
# Class 2: Results for all entrants   
delay.class2 <- process.class2 - entry.class2
delay.class2[which(delay.class2 < 0)] <- NA
results.class2 <- cbind(entry.class2,process.class2,exit.class2,delay.class2)

# Class 2: Average wait (WQ)
avg.wait.class2 <- mean(na.omit(delay.class2))

# Class 2: Average overall time in system (W)
avg.total.class2 <- mean(exit.class2[which(exit.class2 > 0)] - entry.class2[which(exit.class2 > 0)])

# Class 2: Average number of customers in the system (L) and in the queue (LQ)
max.size <- max(prog[1:counter.prog,"n2"])
counts <- seq(0,max.size)
times <- rep(0, (max.size+1))

q.size <- (prog[1:counter.prog,"n2"])   # Start with the number of class members in the system
# Then, if there are any in service (at most 1), subtract 1 from the class members in the system, and thats the queue size.
q.size[which(!is.infinite(prog[1:counter.prog,"t.s2"]))] <- (q.size[which(!is.infinite(prog[1:counter.prog,"t.s2"]))] - 1)
q.size[which(q.size < 0)] <- 0
q.max.size <- max(q.size)
q.counts <- seq(0,q.max.size)
q.times <- rep(0, (q.max.size+1))

for (i in 1:(nrow(prog[1:counter.prog,])-1)) {
  idx <-   prog[i,"n2"] +1 #(prog[i,]$n + 1)
  times[idx] <- times[idx] + (prog[(i+1),"t"] - prog[(i),"t"])
  
  q.idx <- (q.size[i] + 1)
  q.times[q.idx] <- q.times[q.idx] + (prog[(i+1),"t"] - prog[(i),"t"])
}

times <- times / prog[counter.prog,"t"]
avg.in.system.class2 <- counts %*% times

q.times <- q.times / prog[counter.prog,"t"]
avg.in.q.class2 <- q.counts %*% q.times

# Class 1: Results for all entrants
delay.class1 <- process.class1 - entry.class1
delay.class1[which(delay.class1 < 0)] <- NA
results.class1 <- cbind(entry.class1,process.class1,exit.class1,delay.class1)

# Class 1: Average wait time (WQ)
avg.wait.class1 <- mean(na.omit(delay.class1))

# Class 1: Average wait time in total system
avg.total.class1 <- mean(exit.class1[which(exit.class1 > 0)] - entry.class1[which(exit.class1 > 0)])

# Class 1: Average number of customers in the system (L) and in the queue (LQ)
max.size <- max(prog[1:counter.prog,"n1"])
counts <- seq(0,max.size)
times <- rep(0, (max.size+1))

# Same reasoning as above. 
q.size <- (prog[1:counter.prog,"n1"])
q.size[which(!is.infinite(prog[1:counter.prog,"t.s1"]))] <- (q.size[which(!is.infinite(prog[1:counter.prog,"t.s1"]))] - 1)
q.size[which(q.size < 0)] <- 0
q.max.size <- max(q.size)
q.counts <- seq(0,q.max.size)
q.times <- rep(0, (q.max.size+1))

# Can be refactored into same loop above for performance bump -- presented this way for clarity.
for (i in 1:(nrow(prog[1:counter.prog,])-1)) {
  idx <-   prog[i,"n1"] +1 
  times[idx] <- times[idx] + (prog[(i+1),"t"] - prog[(i),"t"])
  
  q.idx <- (q.size[i] + 1)
  q.times[q.idx] <- q.times[q.idx] + (prog[(i+1),"t"] - prog[(i),"t"])
}

times <- times / prog[counter.prog,"t"]
avg.in.system.class1 <- counts %*% times

q.times <- q.times / prog[counter.prog,"t"]
avg.in.q.class1 <- q.counts %*% q.times


q.output <- list(ledger=prog[1:counter.prog,], results.by.customer.class2=results.class2[1:(n2.total.in),], results.by.customer.class1=results.class1[1:(n1.total.in),],
                 avg.wait.class2=avg.wait.class2, avg.total.class2=avg.total.class2, avg.in.system.class2=avg.in.system.class2, avg.in.q.class2=avg.in.q.class2,
                 avg.wait.class1=avg.wait.class1, avg.total.class1=avg.total.class1, avg.in.system.class1=avg.in.system.class1, avg.in.q.class1=avg.in.q.class1)
q.output
  
}