library("rjags")


#------------------------------------------------------------------------
# from ex10
## set R's working directory to the same directory
## as this file, or use the full path to the file.
dat = read.csv(file="./callers.csv", header=TRUE)
dat$calls_visit<-(dat$calls/dat$days_active)

head(dat)
mod6_string = " model {
    for (i in 1:length(calls)) {
        calls[i] ~ dpois( days_active[i]* lam[i] )
		log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1e2)
    for (i in 1:2) {
		b[i]~ dnorm(0.0, 1.0/1e2)
    }
} "
set.seed(102)

data_jags6 = as.list(dat)
head(data_jags6)

params6 = c("b0", "b")

mod6 = jags.model(textConnection(mod6_string), data=data_jags6, n.chains=3)
update(mod6, 1e3)

mod6_sim = coda.samples(model=mod6,
                        variable.names=params6,
                        n.iter=5e3)
mod6_csim = as.mcmc(do.call(rbind, mod6_sim))

## convergence diagnostics
plot(mod6_sim)

## compute DIC
dic = dic.samples(mod6, n.iter=1e3)

summary(mod6_sim)



(pm_params = colMeans(mod6_csim))
pm_params*c(29,1,1)
loglam = sum(pm_params*c(29,1,1))
lam = exp(loglam)
lam
ppois(3, lam*30)




