
library(EasyABC)
library(SimInf)

##############################################################
################################################################

library(deSolve)

##############################################################3

# sir model to be used for ABC simulations

sir <- function(t, x, parms)  {
  with(as.list(c(parms, x)), {
    N = S+I+R
    dS = -1 * beta * I/N * S
    dI = beta * I/N * S - gamma * I
    dR = gamma * I
    dCumInc = beta * I/N * S
    output <- c(dS, dI, dR, dCumInc)
    list(output)
  })
}
start<-c(S=990, I=10, R=0, CumInc=0) #the Initial values

times <- seq(0, 75, 1) # time period 
parms <- c(beta=0.2, gamma= 0.02) ## The parameters 

run<-ode(times=times, y=start, func=sir,parms=parms) # ode solver
Inc = c(0,diff(run[,5]))
model_res_inc = cbind(run,Inc) # adds incidence column to the results from te ode solver



plot(model_res_inc[,2],col="red", type="l",ylim=c(0,1000),main="plot of the SIR model"
     ,xlab="Time", ylab="model_ouputs")
lines(model_res_inc[,3], col="green")
lines(model_res_inc[,4],col="blue4")
lines(model_res_inc[,5], col="purple")
lines(model_res_inc[,6], col="orange")

lines(model_res_inc[,2]+model_res_inc[,3]+model_res_inc[,4])
legend("topleft", legend=c("S", "I", "R","CumInc", "Inc"),col=c("red","green",
       "blue","purple","orange"), pch = 19, cex = 0.65,bg=8 )


##############################################################
################################################################
# create model to be used for ABC 

modelABC <- function(parameters) {
sir <- function(t, x, parms)  {
  with(as.list(c(parms, x)), {
    N = S+I+R
    dS = -1 * beta * I/N * S
    dI = beta * I/N * S - gamma * I
    dR = gamma * I
    dCumInc = beta * I/N * S
    output <- c(dS, dI, dR, dCumInc)
    list(output)
  })
}
start<-c(S=990, I=10, R=0, CumInc=0) #the Initial values

times <- seq(0, 75, 1) # time period 
parms <- c(beta= parameters[1], gamma= parameters[2]) ## The parameters 

model_result <-ode(times=times, y=start, func=sir,parms=parms) # ode solver
Incidence = c(0,diff(model_result[,5]))
model_res_inc = cbind(model_result,Incidence) # adds incidence column to the results from te ode solver

targ = numeric() # create empty vector called targ
targ[1] = model_res_inc[20,6] # incidence at time 20
targ[2] = model_res_inc[50,6]  # incidence at time 50
targ[3] = model_res_inc[75,6] # incidence at time 75
targ[4] = model_res_inc[75,5] # CumIncncmulatve incidence
targ[5] = max(model_res_inc[,6])  #  peak incidence
targ[6] = which.max(model_res_inc[,6]) - 1
return(targ)
}

modelABC(c( runif(1,0,1) ,runif(1,0,0.5))) # run model to obtain targets once

#############################################################################
# run model several times and save targets in a matrix

set.seed(123)

### save the results from 10000 runs, take the means as the targets
save_targets= matrix(c(0,0,0,0),100,4)
for(i in 1:100){
  save_targets[i,]= modelABC(c( runif(1,0,1) ,runif(1,0,0.5)))
}


####################################################################
# mean targets
 
targets = c(mean(save_targets[,1]), mean(save_targets[,2]), mean(save_targets[,3]),
            mean(save_targets[,4]))

######################################################################



## rough work

tail(model_res_inc)
head(model_res_inc)
sum(Inc)== subset(run[,5],times==75)

head(save_targets)
dim(save_targets)

par(mfrow=c(3,2))
hist(save_targets[,1])
hist(save_targets[,2])
hist(save_targets[,3])
hist(save_targets[,4])
hist(save_targets[,5])
hist(save_targets[,6])

##################################################################