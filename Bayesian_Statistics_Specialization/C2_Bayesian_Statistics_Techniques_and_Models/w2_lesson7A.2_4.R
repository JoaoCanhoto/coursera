#install.packages("car")

library("car")
data("Leinhardt")
?Leinhardt
head(Leinhardt)


# lesson 7.2
print("Lesson 7.2")
str(Leinhardt)

pairs(Leinhardt)

plot(infant ~ income, data=Leinhardt)

hist(Leinhardt$infant)

hist(Leinhardt$income)

Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)

plot(loginfant ~ logincome, data=Leinhardt)




lmod = lm(loginfant ~ logincome, data=Leinhardt)
summary(lmod)



dat = na.omit(Leinhardt)


#----------------------------------------------
# lesson 7.3
print("Lesson 7.3")
library("rjags")

mod1_string = " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i] 
    }
    
    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(y=dat$loginfant, n=nrow(dat), 
              log_income=dat$logincome)
data1_jags


params1 = c("b", "sig")
inits1 = function() {
    inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
inits1


mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)
update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains

mod1




#----------------------------------------------
# lesson 7.4
print("Lesson 7.4")

plot(mod1_sim)

gelman.diag(mod1_sim)

autocorr.diag(mod1_sim)

effectiveSize(mod1_sim)

autocorr.plot(mod1_sim)


summary(mod1_sim)
summary(lmod)


#residual checks
lmod0 = lm(infant ~ income, data=Leinhardt)
plot(resid(lmod0)) # to check independence (looks okay)
plot(predict(lmod0), resid(lmod0)) # to check for linearity, constant variance (looks bad)
qqnorm(resid(lmod0)) # to check Normality assumption (we want this to be a straight line)


X = cbind(rep(1.0, data1_jags$n), data1_jags$log_income)
head(X)

(pm_params1 = colMeans(mod1_csim)) # posterior mean
pm_params1[1:2]
dim(X)

yhat1 = drop(X %*% pm_params1[1:2])
resid1 = data1_jags$y - yhat1
plot(resid1) # against data index

plot(yhat1, resid1) # against predicted values

qqnorm(resid1) # checking normality of residuals

plot(predict(lmod), resid(lmod)) # to compare with reference linear model


rownames(dat)[order(resid1, decreasing=TRUE)[1:5]] # which countries have the largest positive residuals?


