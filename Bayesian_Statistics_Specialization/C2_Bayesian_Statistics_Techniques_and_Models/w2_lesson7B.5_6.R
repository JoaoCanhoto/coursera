#-------------------------------------------------------
# lesson 7.4
print("Lesson 7.4")

library("rjags")

mod2_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig = sqrt( 1.0 / prec )
} "


set.seed(73)
data2_jags = list(y=dat$loginfant, log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=="yes"))
data2_jags$is_oil

params2 = c("b", "sig")

inits2 = function() {
    inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod2 = jags.model(textConnection(mod2_string), data=data2_jags, inits=inits2, n.chains=3)
update(mod2, 1e3) # burn-in


mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5e3)
head(mod2_sim)

mod2_csim = as.mcmc(do.call(rbind, mod2_sim)) # combine multiple chains


summary(mod2_sim)
summary(mod2_csim)


plot(mod2_sim)

gelman.diag(mod2_sim)
autocorr.diag(mod2_sim)

autocorr.plot(mod2_sim)



#-------------------------------------------------------
print("residuals")


X2 = cbind(rep(1.0, data1_jags$n), data2_jags$log_income, data2_jags$is_oil)
head(X2)


(pm_params2 = colMeans(mod2_csim)) # posterior mean



yhat2 = drop(X2 %*% pm_params2[1:3])
resid2 = data2_jags$y - yhat2
plot(resid2) # against data index

plot(yhat2 , resid2) # against data index
plot(yhat1, resid1) # residuals from the first model




sd(resid2) # standard deviation of residuals
sd(resid1) # standard deviation of residuals


#-------------------------------------------------------
# t likelihood
print("t likelihood")



curve(dnorm(x), from=-5, to=5)
curve(dt(x,1), from=-5, to=5, col="red", add=TRUE)

mod3_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dt( mu[i], tau, df )
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    df = nu + 2.0 # we want degrees of freedom > 2 to guarantee existence of mean and variance
    nu ~ dexp(1.0)
    
    tau ~ dgamma(5/2.0, 5*10.0/2.0) # tau is close to, but not equal to the precision
    sig = sqrt( 1.0 / tau * df / (df - 2.0) ) # standard deviation of errors
} "







#-------------------------------------------------------
# lesson 7.6
print("Lesson 7.6")

dic.samples(mod1, n.iter=1e3)

dic.samples(mod2, n.iter=1e3)

?dic.samples
