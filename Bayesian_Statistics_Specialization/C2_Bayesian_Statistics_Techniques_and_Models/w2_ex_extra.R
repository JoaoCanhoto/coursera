# install.packages('nimble')
library("nimble") 


#-------------------------------------------------------------------------
#-------------------------------------------------------------------------
# previous exercise
library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data


pairs(Anscombe)  # scatter plots for each pair of variables
library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data


pairs(Anscombe)  # scatter plots for each pair of variables





mod_lm = lm(education ~ income + young + urban, data= Anscombe)
summary(mod_lm)


#---------------------------------------------------
# ex 6
library("rjags")

mod_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
        ## Initial guess of variance based on overall
        ## variance of education variable. Uses low prior
        ## effective sample size. Technically, this is not
        ## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

data_jags = as.list(Anscombe)

data_jags


params1 = c("b0","b", "sig")
inits1 = function() {
    inits = list("b0"=rnorm(1,0.0,100.0),"b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
inits1

mod1 = jags.model(textConnection(mod_string), data= data_jags, inits=inits1, n.chains=5)


update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)


mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains





#---------------------------------------------
# exercise 9.2

Xc = scale(Anscombe, center=TRUE, scale=TRUE)
str(Xc)

data_jags = as.list(data.frame(Xc))

head(data_jags)


mod2_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }
    
    for (i in 1:3) {
        b[i] ~ ddexp(0.0, 1.0)
    }
    
    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
        ## Initial guess of variance based on overall
        ## variance of education variable. Uses low prior
        ## effective sample size. Technically, this is not
        ## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

params2 = c("b", "sig")
inits2 = function() {
    inits = list("b"=  rdexp(3, 0.0,1.0) , "prec"=rgamma(1,1.0,1.0))
}

mod2 = jags.model(textConnection(mod2_string), data= data_jags, inits=inits2, n.chains=5)


update(mod2, 1000) # burn-in

mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5000)
mod2_csim = do.call(rbind, mod2_sim) # combine multiple chains


summary(mod1_sim)
summary(mod2_sim)





