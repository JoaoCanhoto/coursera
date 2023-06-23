


#----------------------------------------
# ex3
datpct = read.csv(file="pctgrowth.csv", header=TRUE)
head(datpct)
tail(datpct)



library("rjags")

mod_pct_string = " model {
	for (i in 1:n) {
	  y[i] ~ dnorm(theta[grp[i]], prec)
	}

	for (j in 1:5) {
	  theta[j] ~ dnorm(mu, tau2)
	}

	mu ~ dnorm(0, 1e6)
	tau2 ~ dgamma(1/2 , 1*3/2)
	prec ~ dgamma(2/2 , 2*1/2)
} "

set.seed(42)

data_jags_pct = as.list(datpct)
data_jags_pct = list(y=datpct$y, n=nrow(datpct), grp=datpct$grp)

params_pct = c("theta", "mu", "tau2", "prec")

mod_pct = jags.model(textConnection(mod_pct_string), data=data_jags_pct, n.chains=3)
update(mod_pct, 1e3)

mod_pct_sim = coda.samples(model=mod_pct,
                       variable.names=params_pct,
                       n.iter=5e3)
mod_pct_csim = as.mcmc(do.call(rbind, mod_pct_sim))

## convergence diagnostics
plot(mod_pct_sim)

gelman.diag(mod_pct_sim)
autocorr.diag(mod_pct_sim)
autocorr.plot(mod_pct_sim)
effectiveSize(mod_pct_sim)

## compute DIC
dic = dic.samples(mod_pct, n.iter=1e3)




## observation level residuals
(pm_params_pct = colMeans(mod_pct_csim))
head(pm_params_pct[4:8])
mu_resid = pm_params_pct[4:8] - pm_params["mu"]
plot(mu_resid)
abline(h=0, lty=2)







#----------------------------------------
# ex4
means_anova = tapply(datpct$y, INDEX= datpct$grp, FUN=mean)
head(means_anova)

means_theta = mean(means_anova)

par(new=FALSE)
plot(pm_params_pct[4:8])
par(new=TRUE)
plot(means_anova, col="blue",  axes = FALSE)
points(means_theta, col="red") ## where means_theta are the posterior point estimates for the industry means.
abline(h=means_theta, lty=2, col="red")
abline(h= pm_params_pct[1], lty=2, col="brown")
abline(h= mean(pm_params_pct[4:8]), lty=3, col="brown")


