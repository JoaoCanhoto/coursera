




Q = matrix(c(0,1,0.3,0.7), 
           nrow=2, byrow=TRUE)
Q

ini_v = c(1,0)

ini_v%*% Q %*% Q %*% Q



Q30 = Q
for (i in 2:30) {
  Q30 = Q30 %*% Q
}
round(Q30, 3) # h=30 steps in the future


Q30 = Q
v30 = ini_v
for (i in 2:50) {
  v30 = v30 %*% Q
}
round(v30, 3) # h=30 steps in the future