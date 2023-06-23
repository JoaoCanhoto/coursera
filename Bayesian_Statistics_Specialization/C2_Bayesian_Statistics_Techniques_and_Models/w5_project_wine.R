


#----------------------------------------
# Read data
data = read.csv(file="winequality-red`.csv", header=TRUE)
head(data)

	
str(data)
dim(data) 
print(colnames(data))


pairs(data[1:11])


print("Look at the data dispersion")
quality<-data[,12]
l <- length(unique(quality))
pairs(data[1:11] , 
	 cex = .25,
	bg = hcl.colors(l, "Temps")[quality] ,
	col = hcl.colors(l, "Temps")[quality] 
	)
	
print("some features seem to be correlated")
print("Such as the free.sulfur.dioxide with total.sulfur.dioxide")
print("     or the pH with total.sulfur.dioxide")
print("     or the density with total.sulfur.dioxide")
pairs(data[,c(1,6,7,8,9,12)]  , 
	 cex = .25,
	bg = hcl.colors(l, "Temps")[quality] ,
	col = hcl.colors(l, "Temps")[quality] 
	)



sprintf("Is there nan?:", sum(is.na(data)) == nrow(data) ) 
print("column with nan?:")
print(which(colSums(is.na(data)) == nrow(data)) )




#----------------------------------------
# Create Model
library("rjags")

mod1_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*fixed.acidity[i] + b[3]*volatile.acidity[i] +b[4]* citric.acid[i]  +b[5]*residual.sugar[i]  +b[6]*chlorides[i]  +b[7]*free.sulfur.dioxide[i]  +b[8]* total.sulfur.dioxide[i]  +b[9]*density[i]  +b[10]*pH[i]  +b[11]*sulphates[i]  +b[12]*alcohol[i]
    }
    
    for (i in 1:12) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig = sqrt( 1.0 / prec )
} "






#----------------------------------------
# Fit the model
set.seed(72)
data$y = data$quality

params1 = c("b", "sig")
inits1 = function() {
    inits = list("b"=rnorm(12,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
inits1



mod1 = jags.model(textConnection(mod1_string), data=data, inits=inits1, n.chains=3)
update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=1e5)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains




#----------------------------------------------
# Check the results
print("-------------------------")
print("Check the results")

plot(mod1_sim)

gelman.diag(mod1_sim)

autocorr.diag(mod1_sim)

effectiveSize(mod1_sim)

autocorr.plot(mod1_sim)

summary(mod1_sim)




#----------------------------------------------
# Check the results
(pm_params1 = colMeans(mod1_csim)) # posterior mean



class( as.matrix(data[1:11]) )
head(as.matrix(data[1:11]))
head( data.matrix( data[1:11] ) )
X1 = as.matrix(data[1:11])
yhat1 = drop(pm_params1[1] + X1 %*% pm_params1[2:12])

resid1 = drop( drop(data[12] - yhat1 ) )
resid1 = apply(resid1, 2, as.array)
Y = apply(data[12], 1, as.array)

plot( yhat1 ) # against data index
plot( resid1 ) # against data index

plot(Y , yhat1) # against data index
plot(yhat1 , Y) # against data index
plot(yhat1 , resid1) # against data index



qqnorm(resid1) # checking normality of residuals

