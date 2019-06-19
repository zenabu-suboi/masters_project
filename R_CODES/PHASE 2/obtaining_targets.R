
library(EasyABC)
library(SimInf)

##############################################################
################################################################

library(deSolve)

modelABC <- function(parameters) {
sir <- function(t, x, parms)  {
  with(as.list(c(parms, x)), {
    N = S+I+R
    dS = -1 * beta * I/N * S
    dI = beta * I/N * S - gamma * I
    dR = gamma * I
    dCumI = beta * I/N * S
    output <- c(dS, dI, dR, dCumI)
    list(output)
  })
}
start<-c(S=990, I=10, R=0, CumI=0) #the Initial values

times <- seq(0, 75, 1) # time period 
parms <- c(beta= parameters[1], gamma= parameters[2]) ## The parameters 

run<-ode(times=times, y=start, func=sir,parms=parms) # ode solver
Inc = c(0,diff(run[,5]))
run1 = cbind(run,Inc) # adds incidence column to the results from te ode solver

targ = numeric() # create empty vector called targ
targ[1] = run1[21,6] # incidence at time 20
targ[2] = run1[51,6]  # incidence at time 50
targ[3] = run1[76,6] # incidence at time 75
targ[4] = run1[76,5] # cummulatve incidence
#targ[5] = #  peak incidence

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

tail(run1)
head(run1)
sum(Inc)== subset(run[,5],times==75)

head(save_targets)
dim(save_targets)

hist(save_targets[,1])
hist(save_targets[,2])
hist(save_targets[,3])
hist(save_targets[,4])


##################################################################