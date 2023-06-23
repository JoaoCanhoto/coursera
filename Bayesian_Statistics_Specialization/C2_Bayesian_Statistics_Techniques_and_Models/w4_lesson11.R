


#--------------------------------------------------------------------------------------------------------
# 11.2
getwd( )
dat = read.table(file="cookies.dat", header=TRUE)
head(dat)

table(dat$location)


hist(dat$chips)
boxplot(chips ~ location, data=dat)


#----------------------------------
set.seed(112)
n_sim = 500
alpha_pri = rexp(n_sim, rate=1.0/2.0)
beta_pri = rexp(n_sim, rate=5.0)
mu_pri = alpha_pri/beta_pri
sig_pri = sqrt(alpha_pri/beta_pri^2)

summary(mu_pri)
summary(sig_pri)

lam_pri = rgamma(n=n_sim, shape=alpha_pri, rate=beta_pri)
summary(lam_pri)

(lam_pri = rgamma(n=5, shape=alpha_pri[1:5], rate=beta_pri[1:5]))
(y_pri = rpois(n=150, lambda=rep(lam_pri, each=30)))


#--------------------------------------------------------------------------------------------------------
# 11.3

library("rjags")

mod_string = " model {
for (i in 1:length(chips)) {
  chips[i] ~ dpois(lam[location[i]])
}

for (j in 1:max(location)) {
  lam[j] ~ dgamma(alpha, beta)
}

alpha = mu^2 / sig^2
beta = mu / sig^2

mu ~ dgamma(2.0, 1.0/5.0)
sig ~ dexp(1.0)

} "

set.seed(113)

data_jags = as.list(dat)

params = c("lam", "mu", "sig")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)


#----------------------------------------
# model checking
# After assessing convergence, we can check the fit via residuals. With a hierarhcical model, there are now two levels of residuals: the observation level and the location mean level. To simplify, we’ll look at the residuals associated with the posterior means of the parameters.
## observation level residuals
(pm_params = colMeans(mod_csim))


yhat = rep(pm_params[1:5], each=30)
resid = dat$chips - yhat
plot(resid)


plot(jitter(yhat), resid)

var(resid[yhat<7])
var(resid[yhat>11])


# Also, we can look at how the location means differ from the overall mean 
## location level residuals
head(pm_params[1:5])
lam_resid = pm_params[1:5] - pm_params["mu"]
plot(lam_resid)
abline(h=0, lty=2)


#----------------------------------------
# Results 

summary(mod_sim)








#--------------------------------------------------------------------------------------------------------
# 11.4

(n_sim = nrow(mod_csim))
post_alpha = mod_csim[,"mu"]^2/mod_csim[,"sig"]^2
post_beta = mod_csim[,"mu"]/mod_csim[,"sig"]^2
lam_pred = rgamma(n=n_sim, shape= post_alpha, 
                  rate= post_beta)
hist(lam_pred)

mean(lam_pred > 15)


# Using these λ draws, we can go to the observation level and simulate the number of chips per cookie, which takes into account the uncertainty in λ:
y_pred = rpois(n=n_sim, lambda=lam_pred)
hist(y_pred)

mean(y_pred > 15)


hist(dat$chips)


# Finally, we could answer questions like: what is the posterior probability that the next cookie produced in Location 1 will have fewer than seven chips?
y_pred1 = rpois(n=n_sim, lambda=mod_csim[,"lam[1]"])
hist(y_pred1)


mean(y_pred1 < 7)










#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------
# 11.6

library("car")
data("Leinhardt")
?Leinhardt
str(Leinhardt)

pairs(Leinhardt)
head(Leinhardt)


dat = na.omit(Leinhardt)
dat$logincome = log(dat$income)
dat$loginfant = log(dat$infant)
str(dat)

library("rjags")

mod_string = " model {
  for (i in 1:length(y)) {
    y[i] ~ dnorm(mu[i], prec)
    mu[i] = a[region[i]] + b[1]*log_income[i] + b[2]*is_oil[i]
  }
  
  for (j in 1:max(region)) {
    a[j] ~ dnorm(a0, prec_a)
  }
  
  a0 ~ dnorm(0.0, 1.0/1.0e6)
  prec_a ~ dgamma(1/2.0, 1*10.0/2.0)
  tau = sqrt( 1.0 / prec_a )
  
  for (j in 1:2) {
    b[j] ~ dnorm(0.0, 1.0/1.0e6)
  }
  
  prec ~ dgamma(5/2.0, 5*10.0/2.0)
  sig = sqrt( 1.0 / prec )
} "

set.seed(116)
data_jags = list(y=dat$loginfant, log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=="yes"), region=as.numeric(dat$region))
data_jags$is_oil
table(data_jags$is_oil, data_jags$region)

params = c("a0", "a", "b", "sig", "tau")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3) # burn-in

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)

mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combine multiple chains

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)


#------------------
# results
dic.samples(mod, n.iter=1e3)
# nonhierarchical model: 230.1


summary(mod_sim)
