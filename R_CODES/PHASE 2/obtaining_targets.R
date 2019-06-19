
library(EasyABC)
library(SimInf)

### create function to use in ABC_mcmc
modelABC= function(parameters, N, inf=0.1, I=inf*N, samplesize){
  library(SimInf)
  u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
  model <- SIR(u0, 1:75, beta=parameters[1], gamma=parameters[2]) # specifying the model
  result <- run(model, threads = 1)     # running the model
  pop= numeric()
  pop[1]=result@U[2,50]   # number of infected individuals at time 50
  pop[2]=result@U[2,75]  # number of infected individuals at time 75
  prevs=pop/sum(u0) # prevalence at times 50 and 75
  return(prevs)
}

pop= numeric()
### try running it once, should return two population prevalence percentages
modelforABCmcmc2(c(0.2,0.02))

##############################################################
library(deSolve)

modelABC <- function(t, x, parms)  {
  with(as.list(c(parms, x)), {
    N = S+I+R
    dS = -beta * I/N * S
    dI = beta * I/N * S - gamma * I
    dR = gamma * I
    dCumI = beta * I/N * S
    output <- c(dS, dI, dR, dCumI)
    list(output)
  })
}

#the Initial values
start<-c(S=990, I=10, R=0, CumI=0)

## The parameters 
times <- seq(0, 75, 1)

save_targets <- matrix(c(0,0,0),1000,3)
for (i in 1:1000) {
  parms = numeric()
  parms[1] = rlnorm(1,0,1)
  parms[2] = rlnorm(1,0,1)
  run <- ode(times=times, y=start, func=modelABC,parms=parms)
  save_targets[i,] = c(run[20,5],run[50,5], run[75,5])
}


plot(run[,2], col="red", ylim=c(0,1000), type="l", main="SEIR Model")
# you can also use matplot toplot evert column
lines(run[,3], col="blue")
lines(run[,4], col="green")
lines(run[,5], col="purple")
legend("topright",legend=c("S","E" ,"I", "R"), col=c("red","black", "blue", "green"), lty=c(1,1,1))
Sys.sleep(0.3)








parms <- c(beta=0.8, gamma=0.1)

## vector of timesteps


run<-ode(times=times, y=start, func=sir,parms=parms)




##################################################################
transitions <- c("S -> b*S*I/(S+I+R) -> I", "I -> g*I -> R")
 compartments <- c("S", "I", "R")


 m <- mparse(c("S -> b*S*I/(S+I+R) -> I + Icum", "I -> g*I -> R"),
             + c("S", "I", "Icum", "R"), b = 0.16, g = 0.077)
 model <- init(m, cbind(S = 99, I = 1, Icum = 0, R = 0), 1:180)
 result <- run(model, threads = 1, seed = 22)
 plot(stepfun(result@tspan[-1], diff(c(0, U(result)["Icum",]))),
         + main = "", xlab = "Time", ylab = "Number of cases",
         + do.points = FALSE)






################################################################

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

