


#-------------------
library("rjags")

data("PlantGrowth")
mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec)
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*1.0/2.0)
    sig = sqrt( 1.0 / prec )
} "
set.seed(82)

data_jags = list(y=PlantGrowth$weight, 
              grp=as.numeric(PlantGrowth$group))

params = c("mu", "sig")

inits = function() {
    inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5e3)


#------------------------------------------------
# ex 3

mod3_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
    }
    
    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    for (j in 1:3) {
    		prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
    		sig[j] = sqrt( 1.0 / prec[j] )
    }

} "


params3 = c("mu", "sig")

inits3 = function() {
    inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(3,1.0,1.0))
}

mod3 = jags.model(textConnection(mod3_string), data=data_jags, inits=inits3, n.chains=3)
update(mod3, 1e3)

mod3_sim = coda.samples(model=mod3,
                        variable.names=params3,
                        n.iter=5e3)



summary(mod_sim)
summary(mod3_sim)




#------------------------------------------------
# ex 4

dic1 = dic.samples(mod, n.iter=1e6)
dic3 = dic.samples(mod3, n.iter=1e6)

dic1-dic3






#------------------------------------------------
# ex 6 

HPDinterval(mod_csim[,3] - mod_csim[,1])






#------------------------------------------------
# ex 8
mod_cm = lm(weight ~ -1 + group, data=PlantGrowth)
summary(mod_cm)

