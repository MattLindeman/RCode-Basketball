# BYU Basketball 22-23 Season
score <- c(109, 60, 75, 66, 87, 76, 70, 79, 100, 68, 60, 83, 97, 75, 90,
            63, 69, 71, 59, 68, 74, 91, 76, 74, 56, 89, 81, 80, 81, 74, 65)

# 1 for Home, 0 for Away/Neutral
venue <- c(1,1,0,1,1,0,0,0,0,0,1,0,1,1,1,1,0,1,0,0,1,1,0,0,1,1,1,0,0,1,0)

# My prior (75 points per game)
a = 750
b = 10



theta <- seq(50, 115, length = 1001)

ll <- sapply(theta, function(x) prod(dpois(score, x)))

plot(theta, ll, type='l', lwd=2, ylab="Likelihood", xlab="Avg Points Scored")

astar = a + sum(score)
bstar = b + length(score)

# Plotting the prior and posterior
plot(theta, dgamma(theta, shape=astar, rate=bstar), type='l', lwd=3,
     main="Distributions for Expected Goals in a Game",
     xlab=expression(theta),
     ylab=expression(paste(pi, "(", theta, ")")))
lines(theta, dgamma(theta, shape=a, rate=b), type='l', col='gray', lwd=3)
legend("topright", c("Prior: Gamma(1500, 20)", "Posterior: Gamma(3116, 41)"),
       lwd=2, col=c("gray", "black"))

# Mean of the posterior
astar/bstar

# Monte-Carlo mean
mean(rgamma(1000000, shape=astar, rate=bstar))

# Std Dev of posterior
sqrt(astar)/bstar

# 95% credible interval
qgamma(c(0.025, 0.975), shape=astar, rate=bstar)



# Comparison of goal scoring by venue
# Posterior for home
M <- 1e5
astar <- a + sum(score[venue==1])
bstar <- b + sum(venue==1)
thetaH <- rgamma(M, astar, bstar)

# Posterior for away
astar <- a + sum(score[venue==0])
bstar <- b + sum(venue==0)
thetaA <- rgamma(M, astar, bstar)

# Histogram of the difference
hist(thetaH - thetaA)

# 95% credible interval
quantile(thetaH - thetaA, c(0.025, 0.975))

# Probability that home scoring is greater than away scoring
mean(thetaH > thetaA)
