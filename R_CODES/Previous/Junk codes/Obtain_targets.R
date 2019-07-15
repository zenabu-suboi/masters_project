
library(EasyABC)
library(SimInf)

### create function to use in ABC_mcmc
modelforABCmcmc2= function(parameters){
  library(SimInf)
  u0= data.frame(S=c(990), I=c(10), R=c(0))
  
  model <- SIR(u0, 1:75, beta= parameters[1], gamma=parameters[2])
  result <- run(model, threads = 1)#, seed=sample.int(1000000000,1)) 
  pop[1]=result@U[2,30]
  pop[2]=result@U[2,60]
  return(c(pop[1],pop[2]))
}

pop= numeric()
### try running it once, should return two population prevalence percentages
modelforABCmcmc2(c(0.2,0.02))



### set.seed for reproducability
set.seed(123)

### save the results from 10000 runs, take the means as the targets
saveres= matrix(c(0,0),100,2)
for(i in 1:100){
  saveres[i,]= modelforABCmcmc2(c(0.2,0.02))
}

### we call the target: truepop.prev 
truepop.prev= c(ceiling(mean(saveres[,1])), ceiling(mean(saveres[,2])))
truepop.prev
plot(saveres[,1], saveres[,2], pch=16)
points(mean(saveres[,1]), mean(saveres[,2]), cex=1.5, col="red", pch=16)

