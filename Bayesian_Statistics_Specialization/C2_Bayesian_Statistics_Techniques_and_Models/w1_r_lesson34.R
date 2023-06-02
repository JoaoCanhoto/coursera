set.seed(32) # Initializes the random number generator so we can replicate these results. To get different random numbers, change the seed.

print("-----------------------------")
print(" Lesson 3.3")

m = 100
a = 2.0
b = 1.0 / 3.0


theta = rgamma(n=m, shape=a, rate=b)

hist(theta, freq=FALSE)
curve(dgamma(x=x, shape=a, rate=b), col="blue", add=TRUE)

sum(theta) / m # sample mean
mean(theta) # sample mean

a / b # true expected value

m = 1e4
theta = rgamma(n=m, shape=a, rate=b)
mean(theta)
var(theta) # sample variance

head(theta)
tail(theta)

a / b^2 # true variance of Gamma(a,b)

ind = theta < 5.0 # set of indicators, TRUE if theta_i < 5
mean(ind) # automatically converts FALSE/TRUE to 0/1
pgamma(q=5.0, shape=a, rate=b) # true value of Pr( theta < 5 )

quantile(x=theta, probs=0.9)
qgamma(p=0.9, shape=a, rate=b) # true value of 0.9 quantile



#--------------------------------------
# Lesson 3.4
print("-----------------------------")
print(" Lesson 3.4")

se = sd(theta) / sqrt(m)
2.0 * se # we are reasonably confident that the Monte Carlo estimate is no more than this far from the truth


ind = theta < 5.0
se = sd(ind) / sqrt(m)
2.0 * se # we are reasonably confident that the Monte Carlo estimate is no more than this far from the truth



#Marginalization
#Let’s also do the second example of simulating a hierarchical model. In our example from the previous segment, we had a binomial random variable where 


m = 10e4

y = numeric(m) # create the vectors we will fill in with simulations
phi = numeric(m)


for (i in 1:m) {
  phi[i] = rbeta(n=1, shape1=2.0, shape2=2.0)
  y[i] = rbinom(n=1, size=10, prob=phi[i])
}
# which is equivalent to the following 'vectorized' code
phi = rbeta(n=m, shape1=2.0, shape2=2.0)
y = rbinom(n=m, size=10, prob=phi)


mean(y)
plot(prop.table(table(y)), ylab="P(y)", main="Marginal distribution of y")




