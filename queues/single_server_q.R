# Single server priority queue
# Discrete event simulation
# (C) Feburary 2016, Digitialnonplus.com
# dave@digitalnonplus.com


single_server_q <- function(arrival.rate, arrival.sd=NA, service.rate, service.sd=NA, arrival.dist="exp", service.dist="exp", max.time = 60) {
  # Simulates a single server queue using discrete time simulation. Assumes stationary distributions throughout simulation.
  # Can be modified to support G/G/1 queues. Has been tested with exponential and normal service and arrival times based on the functions passed in.

  # Define our arrival and service distributions ---------
  if (arrival.dist == "exp") {
    arrival <- function() rexp(1, rate=arrival.rate)
  } else if (arrival.dist == "norm") {
    arrival <- function() rnorm(1, mean=arrival.rate, sd=arrival.sd)
  } else {
    stop("Non-supported arrival function specified (exp, norm supported)")
  }
  
  if (service.dist == "exp") {
    service <- function() rexp(1, rate=service.rate)
  } else if (service.dist == "norm") {
    service <- function() rnorm(1, mean=service.rate, sd=service.sd)
  } else {
    stop("Non-supported service function specified (exp, norm supported)")
  }

  # Test for queue sanity for M/G/1 queues
  if (arrival.dist == "exp") {
    if (arrival.rate*service.rate >=1) {
      warning("Unbounded conditions detected: queue likely to grow to infinity.")
    }
  }
    
  # Initialize the simulation ----------
  t <- n <- n.out <- n.in <- 0
  t.a <- arrival()
  t.d <- t.s <- Inf
  
  # Initialize result vectors
  entry <- vector("numeric",length=100)
  process <- vector("numeric", length=100)
  exit <- vector("numeric", length=100)
  length.counter <- 100
  
  length.counter.prog <- 10000
  counter.prog <- 1
  
  prog <- matrix(as.numeric(NA), ncol=7, nrow=length.counter.prog)
  prog[counter.prog,] <- c(t, n, t.a, t.s, t.d, n.in, n.out)
  colnames(prog) <- c("t", "n", "t.a", "t.s", "t.d", "n.in", "n.out")
  
  # Keep going as long as our arrival and departure times are less than 
  # a passed max.time
  while (min(t.a,t.d) < max.time) {

    # If the next event is an arrival.
    if (t.a < t.d) {
      t <- t.a             # Update our time counter
      n <- n + 1           # Update the number of customers in the queue
      n.in <- n.in + 1     # Update our arrival counter
      entry[n.in] <- t     # Record the time of the nth arrival for analysis later
      t.a <- t + arrival() # Go ahead and define the next arrival time
      
      if (n == 1) {               # If this is the only arrival in the system
        t.s <- t                  # they go straight to the server
        t.d <- t.s + service()    # and get served. So we define the departure time.
        process[n.in] <- t.s      # And record both
        exit[n.in] <- t.d
      }
      
      # Similar setup for depatures happening...
    } else if (t.d < t.a) {
        t <- t.d
        n <- n - 1
        n.out <- n.out + 1
        process[n.out] <- t.s
        exit[n.out] <- t.d
        
        if (n >=1 ) {
          t.s <- t                # If there are more in the queue, the next customer
          t.d <- t.s + service()  # gets serverd.
        } else {
          t.s <- Inf              # Otherwise its empty.
          t.d <- Inf
        }
    }
    
    counter.prog <- counter.prog + 1
    prog[counter.prog,] <- c(t, n, t.a, t.s, t.d, n.in, n.out)
    
    # Poor man's memory reallocation. Rather than tacking on results every single iteration, which is of course 
    # crazy slow in R, we only do it every 10000 iterations. 
    if (length.counter.prog - counter.prog <= 1) {
      p.temp <- matrix(as.numeric(NA), ncol=7, nrow=length.counter.prog)
      prog <- rbind(prog,p.temp)
      length.counter.prog <- length.counter.prog + 10000
    }
    
    if (n.in - length.counter >= -1) {
      filler <- rep(0,100)
      entry <- c(entry,filler)
      process <- c(process, filler)
      exit <- c(exit,filler)
      length.counter <- length.counter + 100
    }
  }
  # Performance metrics for this run -----
  # Results by customer
  delay <- process - entry
  results <- cbind(entry,process,exit,delay)
  
  # Avg wait time for the customers (WQ)
  avg.wait <- mean(delay[1:n.out])
  
  # Avg total time in system
  service <- exit - process
  avg.time.in.system <- mean(delay[1:n.out] + service[1:n.out])

  # Avg number of customers in the system (L) and in the queue
  max.size <- max(prog[1:counter.prog,"n"])
  counts <- seq(0,max.size)
  times <- rep(0, (max.size+1))
  
  q.size <- (prog[1:counter.prog,"n"] - 1)
  q.size[which(q.size < 0)] <- 0
  q.max.size <- max(q.size)
  q.counts <- seq(0,q.max.size)
  q.times <- rep(0, (q.max.size+1))
  
  for (i in 1:(nrow(prog[1:counter.prog,])-1)) {
    idx <- (prog[i,"n"] + 1)
    times[idx] <- times[idx] + (prog[i+1,"t"] - prog[i,"t"])
    
    q.idx <- (q.size[i] + 1)
    q.times[q.idx] <- q.times[q.idx] + (prog[i+1,"t"] - prog[i,"t"])
  }

  
  times <- times / prog[counter.prog,"t"]
  avg.in.system <- counts %*% times
  
  q.times <- q.times / prog[counter.prog,"t"]
  avg.in.q <- q.counts %*% q.times

  # Put it all together and return
  q.output <- list(ledger=prog[1:counter.prog,], results.by.customer=results[1:n.out,], avg.wait=avg.wait, avg.time.in.system = avg.time.in.system, avg.in.system=avg.in.system, avg.in.queue=avg.in.q)
  q.output
}