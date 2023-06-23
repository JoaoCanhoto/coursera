

library("COUNT")
data("badhealth")
head(badhealth)

# Model
library("rjags")

mod_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] + b_intx*age[i]*badh[i]
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
    b_intx ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

data_jags = as.list(badhealth)

params = c("int", "b_badh", "b_age", "b_intx")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)





#------------------------------------------------------------------------
# ex1

exp(1.5 -0.3*0.8 +1.0*1.2)








#------------------------------------------------------------------------
# ex2
mod2_string = " model {
    for (i in 1:length(numvisit)) {
        numvisit[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_badh*badh[i] + b_age*age[i] 
    }
    
    int ~ dnorm(0.0, 1.0/1e6)
    b_badh ~ dnorm(0.0, 1.0/1e4)
    b_age ~ dnorm(0.0, 1.0/1e4)
} "
set.seed(102)


params2 = c("int", "b_badh", "b_age")

mod2 = jags.model(textConnection(mod2_string), data=data_jags, n.chains=3)
update(mod2, 1e3)

mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5e3)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))

## convergence diagnostics
plot(mod2_sim)

## compute DIC
dic2 = dic.samples(mod2, n.iter=1e3)

dic
dic2




#------------------------------------------------------------------------
# ex4
ppois(21, lambda = 30)



#------------------------------------------------------------------------
# ex5
## set R's working directory to the same directory
## as this file, or use the full path to the file.
dat = read.csv(file="./callers.csv", header=TRUE)
dat$calls_visit<-(dat$calls/dat$days_active)

head(dat)

pairs(dat)

boxplot(dat$age, dat$isgroup2)
boxplot(dat$calls, dat$isgroup2,ylim = c(0, 0.35))
boxplot(dat$calls_visit, dat$isgroup2, ylim = c(0, 0.1))
boxplot(dat$calls_visit, dat$age )





#------------------------------------------------------------------------
# ex7
head(dat)
mod6_string = " model {
    for (i in 1:length(calls)) {
        calls[i] ~ dpois( lam[i] )
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




