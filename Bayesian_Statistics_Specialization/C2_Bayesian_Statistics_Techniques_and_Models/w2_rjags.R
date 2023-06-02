#https://d3c33hcgiwev3.cloudfront.net/_4ec003e4706226af504d524aafb2c527_JAGSintro.html?Expires=1684627200&Signature=k4W3IuQ1JEfq8kOW9e~J24ZooQD~gjp-NI~2xPeDd3voxLtFrAEoOc9penUGEJJ88~iZTbjI8aGjJAp7vqsSekgs3sIidr9l1H1OCmJy-zpM86zrVQAc7avfZ4xH6tilpibjDmxqeMKjVuc~tq4u1uoyJw2xX6YrKXmbhNRuFxo_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A


install.packages("rjags")
library("rjags")

mod_string = " model {
  for (i in 1:n) {
    y[i] ~ dnorm(mu, 1.0/sig2)
  }
  mu ~ dt(0.0, 1.0/1.0, 1.0) # location, inverse scale, degrees of freedom
  sig2 = 1.0
} "


# setup the model
set.seed(50)
y = c(1.2, 1.4, -0.5, 0.3, 0.9, 2.3, 1.0, 0.1, 1.3, 1.9)
n = length(y)

data_jags = list(y=y, n=n)
params = c("mu")
data_jags
params

inits = function() {
  inits = list("mu"=0.0)
} # optional (and fixed)

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits)


# Run the MCMC sampler
update(mod, 500) # burn-in

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=1000)


# post processing
summary(mod_sim)

library("coda")
plot(mod_sim)

