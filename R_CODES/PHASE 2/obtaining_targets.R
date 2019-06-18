
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

sir <- function(t, x, parms)  {
  with(as.list(c(parms, x)), {
    dS= -beta*I/1000*S
    dI=beta*I/1000*S-gamma*I
    # dE=beta*I/1000*S-sigma*E
    dR=gamma*I
    output <- c(dS, dI, dR)
    list(output)
  })
}

#the Initial values
start<-c(S=999, I=1, R=0 )

## The parameters 
parms <- c(beta=0.8, gamma=0.1)

## vector of timesteps
times <- seq(0, 100, 1)

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

