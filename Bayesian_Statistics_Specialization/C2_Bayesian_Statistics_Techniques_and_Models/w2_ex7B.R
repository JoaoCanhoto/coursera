
library("rjags")

#---------------------------------------------------
# from exerxise 7A

library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data


pairs(Anscombe)  # scatter plots for each pair of variables



mod_lm = lm(education ~ income + young + urban, data= Anscombe)
summary(mod_lm)




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




#---------------------------------------------------
#---------------------------------------------------
#---------------------------------------------------
# from exerxise 7B
dic.samples(mod1,  n.iter=1e5)

data_jags

mod_string_7B_2 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] 
    }
    
    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:2) {
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

mod_string_7B_3 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]*young[i]
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


params_7B_2 = c("b0","b", "sig")
inits_7B_2 = function() {
    inits = list("b0"=rnorm(1,0.0,100.0),"b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
params_7B_2

head(data_jags[ -c(4) ])
# subset(data_jags, select = -c(urban) )
# mydata[,!(names(mydata) %in% drop)]# data_jags[ -c(4) ]
# mod_7B_2 = jags.model(textConnection(mod_string_7B_2), data= subset(data_jags, select = -c("urban") )
mod_7B_2 = jags.model(textConnection(mod_string_7B_2), data= data_jags[ -c(4) ] , inits=inits_7B_2, n.chains=4)
update(mod_7B_2, 1000) # burn-in


params_7B_3  = c("b0","b", "sig")
inits_7B_3  = function() {
    inits = list("b0"=rnorm(1,0.0,100.0),"b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
inits_7B_3 

mod_7B_3  = jags.model(textConnection(mod_string_7B_3 ), data= data_jags, inits=inits_7B_3 , n.chains=5)

update(mod_7B_3 , 1000) # burn-in



dic.samples(mod1,  n.iter=1e5)
dic.samples(mod_7B_2,  n.iter=1e5)
dic.samples(mod_7B_3,  n.iter=1e5)







#---------------------------------------------------
# exercise 5

mod_7B_3_sim = coda.samples(model= mod_7B_3,
                       variable.names= params_7B_3,
                       n.iter=1000)
mod_7B_3_csim = do.call(rbind, mod_7B_3_sim) # combine multiple chains

head(mod_7B_3_csim)


head(mod_7B_3_csim[,1])

mod_mean = mean(mod_7B_3_csim[,1])
mod_sd = sd(mod_7B_3_csim[,1])
mod_mean/mod_sd
quantile(x=mod_7B_3_csim[,4], probs=0.9)

1-pnorm(q=0, mod_mean, mod_sd)


#---------------------------------------------------
# exercise 6

mod1_sim = coda.samples(model= mod1,
                       variable.names= params1,
                       n.iter=1000)
mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains

head(mod1_csim)

head(mod1)
head(mod_7B_3_csim)
tail(mod_7B_3_csim)


