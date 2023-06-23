
#----------------------------------------------------------------------------------------
# Read data
data = read.csv(file="concrete_data.csv", header=TRUE)
head(data)

	
str(data)
dim(data) 
print(colnames(data))
summary(data)


#----------------------------------------------------------------------------------------
# Checking the data

# Look at the data dispersion
print("Look at the data dispersion")
pairs(data[1:9])

# Look at the data dispersion collored by the target= Concrete compressive strength (9)
print("Look at the data dispersion")
Y<-data[,9]
l <- length(unique(Y))
pairs(data[1:8] , 
	 cex = .5,
	bg = hcl.colors(l, "Temps")[Y] ,
	col = hcl.colors(l, "Temps")[Y] 
	)
	
# Plot distribution of the target = Concrete compressive strength
hist(Y, xlim=range(c(0,90)))

par(new=FALSE)
densY=density(Y)

# Plot y-values scaled by number of observations against x values
plot(densY$x,length(data)* densY$y,type="l",xlab="Concrete compressive strength",ylab="distribution",
	 xlim=range(c(0,90)), lwd = 2
 )


#----------------------------------------
# Checking for nans
sprintf("Is there nan?:", sum(is.na(data)) == nrow(data) ) 
print("column with nan?:")
print(which(colSums(is.na(data)) == nrow(data)) )




#----------------------------------------------------------------------------------------
# Create Model
library("rjags")

mod1_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*Cement_density[i] + b[3]* Blast_Furnace_Slag[i] +b[4]* Fly_Ash[i]  +b[5]* Water[i]  +b[6]* Superplasticizer[i]  +b[7]* Coarse_Aggregate[i]  +b[8]* Fine_Aggregate[i]  +b[9]* Age[i]  
        }
    
    for (i in 1:9) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig = sqrt( 1.0 / prec )
} "






#----------------------------------------------------------------------------------------
# Fit the model
set.seed(72)
data$y = data$Concrete_compressive_strength

params1 = c("b", "sig")
inits1 = function() {
    inits = list("b"=rnorm(9,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
inits1



mod1 = jags.model(textConnection(mod1_string), data=data, inits=inits1, n.chains=3)
mod1 = jags.model(textConnection(mod1_string), data=data, n.chains=3)
update(mod1, 50000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=1e5)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains
mod2_csim = as.mcmc(do.call(rbind, mod1_sim))


colMeans(mod1_csim)
HPDinterval(mod2_csim)



#----------------------------------------------------------------------------------------
# Check the results
print("-------------------------")
print("Check the results")

plot(mod1_sim)

gelman.diag(mod1_sim)

autocorr.diag(mod1_sim)

effectiveSize(mod1_sim)

autocorr.plot(mod1_sim)

summary(mod1_sim)







#----------------------------------------------------------------------------------------
# Check the results
# get average parameters:
(pm_params1 = colMeans(mod1_csim)) # posterior mean


# get fields to obtain model prediction
#class( as.matrix(data[1:11]) )
#head(as.matrix(data[1:11]))

# data points into a matrix
X1 = as.matrix(data[1:8])
# get model prediction:
yhat1 = drop(pm_params1[1] + X1 %*% pm_params1[2:9])

# get model residuos:
resid1 = drop( drop(Y - yhat1 ) )
#resid1 = apply(resid1, 1, as.array)
sd(resid1) # standard deviation of residuals


#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('blue','red'))
#This adds a column of color values
# based on the y values
col_resid1 <- rbPal(10)[as.numeric(cut(abs(resid1),breaks = 10))]


# plot model predictions vs index, collored by residuos
plot( yhat1 , col = col_resid1) # against data index
# plot residuos vs index
plot( resid1 ) # against data index


# Check the residues
densRes=density(resid1)
# Plot y-values scaled by number of observations against x values
plot(densRes$x,length(data)* densRes$y,type="l",xlab="residues",ylab="distribution", 
	xlim=range(c(-40,40)) , ylim=range(c(0,0.450)), 
	lwd = 2
)


# plot model predictions vs original label, collored by residuos
plot(Y , yhat1, col = col_resid1) # against data index
legend(60, 0.3, legend=c("original", "model"),
       col=c("black", "blue"), lty=1:1, cex=0.8)

blue.axes <- list(col.axis = "blue", col.ticks = "blue")
legend.scale(c(0, 1), axis.args = blue.axes)      
legend.col(col = col_resid1, lev = resid1)

 
# plot residuos vs model predictions 
plot(yhat1 , resid1) 

#rownames(data)[order(resid1, decreasing=TRUE)[1:8]] # which countries have the largest positive residuals?


# checking normality of residuals
qqnorm(resid1)  

dic.samples(mod1, n.iter=1e3)




#----------------------------------------------------------------------------------------
# Check the results
densY=density(Y)
densYhat1 =density(yhat1)

# Plot y-values scaled by number of observations against x values
plot(densY$x,length(data)*densY$y,type="l",xlab="Concrete compressive strength",ylab="distribution", 
	xlim=range(c(0,90)) , ylim=range(c(0,0.350)), 
	lwd = 2
)
par(new=TRUE)
plot(densYhat1 $x,length(data)* densYhat1 $y,type="l",
	xlab="",ylab="", 
	xlim=range(c(0,90)) , ylim=range(c(0,0.350)),
	col = "blue", lwd = 2
 )

legend(60, 0.3, legend=c("original", "model"),
       col=c("black", "blue"), lty=1:1, cex=0.8)



densplot(mod1_csim[,1], xlim=c(-3.0, 3.0))



point1 = c(1.0, 281.2, 73.9, 54.19, 181.6, 6.205, 972.9, 773.6, 45.66)
point1 = c(1.0, 281.2, 73.9, 54.19, 181.6, 6.205, 972.9, 773.6, 30)

expected1 = drop(mod1_csim[,1:9] %*% point1)
dens1=density(expected1)
# Plot y-values scaled by number of observations against x values
plot(dens1$x,dens1$y)
print("Probability that the strength is above 35 for concrete with 30 day")
mean(expected1>35.5)

point2 = c(1.0, 281.2, 73.9, 54.19, 181.6, 6.205, 972.9, 773.6, 45)
expected2 = drop(mod1_csim[,1:9] %*% point2)
dens1=density(expected2)
# Plot y-values scaled by number of observations against x values
plot(dens1$x,dens1$y)
print("Probability that the strength is above 35 for concrete with 80 day")
mean(expected2>35.5)


point3 = c(1.0, 281.2, 73.9, 54.19, 181.6, 6.205, 972.9, 773.6, 50)
expected3 = drop(mod1_csim[,1:9] %*% point3)
dens1=density(expected3)
# Plot y-values scaled by number of observations against x values
plot(dens1$x,dens1$y)
print("Probability that the strength is above 35 for concrete with 50 day")
mean(expected3>35.5)



HPDinterval(expected1)
head(expected1)


