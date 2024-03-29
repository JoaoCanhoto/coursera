



curve( 0.4*dexp(x, 1.0) + 0.6*dnorm(x, 3.0, 1.0), from=-2.0, to=7.0, ylab="density", xlab="y", main="40/60 mixture of exponential and normal distributions", lwd=2)



#Let’s draw the weighted PDFs for each population.
curve( 0.4*dexp(x, 1.0) + 0.6*dnorm(x, 3.0, 1.0), from=-2.0, to=7.0, ylab="density", xlab="y", main="40/60 mixture of exponential and normal distributions", lwd=2)
curve( 0.4*dexp(x, 1.0), from=-2.0, to=7.0, col="red", lty=2, add=TRUE)
curve( 0.6*dnorm(x, 3.0, 1.0), from=-2.0, to=7.0, col="blue", lty=2, add=TRUE)



#------------------------------------------
# Let’s simulate from our example mixture distribution.
set.seed(117)
n = 1000
z = numeric(n)
y = numeric(n)
for (i in 1:n) {
  z[i] = sample.int(2, 1, prob=c(0.4, 0.6)) # returns a 1 with probability 0.4, or a 2 with probability 0.6
  if (z[i] == 1) {
    y[i] = rexp(1, rate=1.0)
  } else if (z[i] == 2) {
    y[i] = rnorm(1, mean=3.0, sd=1.0)
  }
}
hist(y, breaks=30)





#---------------------------------------------------------------------------------------------
# data
dat = read.csv("mixture.csv", header=FALSE)
y = dat$V1
(n = length(y))


hist(y, breaks=20)
plot(density(y))




#---------------------------------------------------------------------------------------------
# model
library("rjags")



mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[z[i]], prec)
      z[i] ~ dcat(omega)
    }
  
  mu[1] ~ dnorm(-1.0, 1.0/100.0)
    mu[2] ~ dnorm(1.0, 1.0/100.0) T(mu[1],) # ensures mu[1] < mu[2]

    prec ~ dgamma(1.0/2.0, 1.0*1.0/2.0)
  sig = sqrt(1.0/prec)
    
    omega ~ ddirich(c(1.0, 1.0))
} "

set.seed(11)

data_jags = list(y=y)

params = c("mu", "sig", "omega", "z[1]", "z[31]", "z[49]", "z[6]") # Select some z's to monitor

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim, ask=TRUE)

autocorr.diag(mod_sim)
effectiveSize(mod_sim)







#---------------------------------------------------------------------------------------------
# results
summary(mod_sim)


## for the population parameters and the mixing weights
par(mfrow=c(3,2))
densplot(mod_csim[,c("mu[1]", "mu[2]", "omega[1]", "omega[2]", "sig")])

## for the z's
par(mfrow=c(2,2))



densplot(mod_csim[,c("z[1]", "z[31]", "z[49]", "z[6]")])


table(mod_csim[,"z[1]"]) / nrow(mod_csim) ## posterior probabilities for z[1], the membership of y[1]
table(mod_csim[,"z[31]"]) / nrow(mod_csim) ## posterior probabilities for z[31], the membership of y[31]
table(mod_csim[,"z[49]"]) / nrow(mod_csim) ## posterior probabilities for z[49], the membership of y[49]
table(mod_csim[,"z[6]"]) / nrow(mod_csim) ## posterior probabilities for z[6], the membership of y[6]

y[c(1, 31, 49, 6)]
# If we look back to the y values associated with these z variables we monitored, we see that y1 is clearly in Population 1’s territory, y31 is ambiguous, y49 is ambiguous but is closer to Population 2’s territory, and y6 is clearly in Population 2’s territory. The posterior distributions for the z variables closely reflect our assessment.




