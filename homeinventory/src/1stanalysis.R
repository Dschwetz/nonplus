library(RPostgreSQL)
library(fitdistrplus)
library(ggplot2)
rm(list=ls())

# Connect to our database and fetch our button data
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="yourdbname", user="yourdbuser", password="yourdbpw", host="192.168.1.2")
query <- "SELECT date_trunc('day', diapertick), count(diaperid) as numused 
             from diapertrack
             group by date_trunc('day', diapertick)
             order by date_trunc('day', diapertick)"
rs <- dbSendQuery(con, query)
results <- fetch(rs,n=-1)

# Subtract our deterministic value from our counts
results[,2] <- results[,2] - 2

# Fit the distribution and plot the theoretical distribution to visualize
dist <- fitdist(results[,2], "pois")
ourdensity <- data.frame(x=factor(0:10), y=dpois(0:10, lambda=dist$estimate))
ggplot(ourdensity, aes(x,y)) + geom_bar(stat="identity", color="black", fill="darkorange") +
  labs(title = paste("Poisson Distribution With Lambda = ", dist$estimate, sep=""), x = "Count", y = "Probability") 

# Chances of running out: 1 - (chances of using 0-8)
today <- 1 - ppois(8,lambda=dist$estimate)

# Or we can use the lower.tail parameter instead of subtracting from 1
today <- ppois(8,lambda=dist$estimate, lower.tail = FALSE)

# Chances of running out tomorrow: 1 - (chances of using 0-6) with the new Poisson
tomorrow <- 1 - ppois(6,lambda=(2*dist$estimate))

# Generalized for any n day, currently set to 3, with 10 on hand
any.n <- 3
onhand <- 10
anyday <- 1- ppois((onhand - 2*any.n),lambda=(any.n*dist$estimate))

