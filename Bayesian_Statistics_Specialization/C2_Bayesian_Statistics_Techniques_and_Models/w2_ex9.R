

#--------------------------------------------------------------
# ex 9.2
library("MASS")
data("OME")
?OME # background on the data
head(OME)

any(is.na(OME)) # check for missing values
dat = subset(OME, OME != "N/A") # manually remove OME missing values identified with "N/A"
dat$OME = factor(dat$OME)
str(dat)

plot(dat$Age, dat$Correct / dat$Trials )
plot(dat$OME, dat$Correct / dat$Trials )
plot(dat$Loud, dat$Correct / dat$Trials )
plot(dat$Noise, dat$Correct / dat$Trials )




#--------------------------------------------------------------
# ex 9.3

mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
summary(mod_glm)


plot(residuals(mod_glm, type="deviance"))
plot(fitted(mod_glm), dat$Correct/dat$Trials)




#--------------------------------------------------------------
# ex 9.4

X = model.matrix(mod_glm)[,-1] # -1 removes the column of 1s for the intercept
head(X)




#--------------------------------------------------------------
# ex 9.5

mod5_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dbin(phi[i], n[i])
        logit(phi[i]) = b0 + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
    }
    
    b0 ~ dnorm(0.0, 1.0/5.0^2)
    for (j in 1:4) {
        b[j] ~ dnorm(0.0, 1.0/4.0^2)
    }
    
} "

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags$n = dat$Trials
str(data_jags) # make sure that all variables have the same number of observations (712).


raftery.diag(mod5_string)






mod5 = jags.model(textConnection(mod5_string), data=data_jags, n.chains=3)

update(mod5, 1e3)
params5 = c("b0", "b")


mod5_sim = coda.samples(model=mod5,
                        variable.names=params5,
                        n.iter=5e3)
mod5_csim = as.mcmc(do.call(rbind, mod5_sim))



#--------------------------------------------------------------
# ex 9.7


(pm_coef5 = colMeans(mod5_csim))
colnames(mod5_csim)

b = -7.30667942  +  0.01882018*60 -0.24327787*1  +0.17221439*50 + 1.57965544*0

1/(1+exp(-b))




#--------------------------------------------------------------
# ex 9.8
pm_coef5
pm_Xb = pm_coef5["b0"] + X %*% pm_coef5[1:4]
phat = 1.0 / (1.0 + exp(-pm_Xb))
head(phat)


(tab0.7 = table(phat > 0.7, (dat$Correct / dat$Trials) > 0.7))
sum(diag(tab0.7)) / sum(tab0.7)




