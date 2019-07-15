
install.packages("devtools")
library(devtools)
install_github("sbfnk/fitR")

pacman::p_load(fitR)
library(fitR)

init.state <- c(S = 990, I = 10, R = 0)
times <- 1:75

#### make sure the correct values are used for obs=... should be targets

### fills matrix from top left, works!
### ng-j, works
mat <- matrix(NA, nrow=nb, ncol=ng)
for(i in 1:nb){
  for(j in 0:(ng-1)){
    D_inf= 1/gammagrid[ng-j]
    R0= betagrid[i]* D_inf
    theta <- c(R0 = R0, D_inf=D_inf) 
    traj <- SIR$simulate(theta, init.state, times)
    modpnt= traj$I[50]
    ## obs in below is obs at time 50 # comes from SIR model abc two unknwn param, two unknwn stats
    savit <- SIR$dPointObs(data.point = c(obs=604), model.point = c(I = modpnt), theta, log = TRUE)
    modpnt= traj$I[75]
    ## obs in below is obs at time 75 # comes from SIR model abc two unknwn param, two unknwn stats
    mat[j+1,i] <- savit + SIR$dPointObs(data.point = c(obs=383), model.point = c(I = modpnt), theta, log = TRUE)
    
  }
  print(i)
}

mat

likmat= exp(mat)

### FOR PLOTTING USE 3_2

### standardize the likelihood to sum to 1
sum(likmat)

likmatstd= likmat/sum(likmat)

sum(likmatstd)

options(max.print=999999)
likmatstd
ras.abc0.1.mat
