set.seed(32) # Initializes the random number generator so we can replicate these results. To get different random numbers, change the seed.

print("Q5")
m=1e5
theta = rbeta(m , 5, 3)
head(theta)

val = theta/(1-theta)
head(val)

mean(val)

print("Q6")
mean(val>1)



print("Q7")
nx = rnorm(1e5,0,1)
head(nx)
quantile(x=nx, probs=0.3)

qnorm(p=0.3,0,1)



print("Q8")
sqrt(5.2/5000)
