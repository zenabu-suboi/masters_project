



### create function to use in ABC_mcmc
# scenario 1 == two targets

targets2 <- function(parm1, parm2){

  library(SimInf)

modelforABC <- function(parm1, parm2){

  u0 = data.frame(S=c(990), I=c(10), R=c(0))

  model <- SIR(u0, 1:75, beta = parm1,
               gamma = parm2)

  result <- run(model, threads = 1)#, seed=sample.int(1000000000,1))
  prev   <- prevalence(result, I~.)
  targ   <- numeric()
  targ[1] <- prev[50,2]
  targ[2] <- prev[75,2]
  # peak_prev <- max(prev[,2])

  # pop[1]=result@U[2,30]
  # pop[2]=result@U[2,60]
  return(c(targ[1],targ[2]))
}

### try running it once, should return two population prevalence percentages
##modelforABC(c(0.2,0.02))



### set.seed for reproducability
set.seed(123)

### save the results from 10000 runs, take the means as the targets
saveres = matrix(c(0,0),100,2)
for(i in 1:100){
  saveres[i,] = modelforABC(parm1,
                             parm2)
}
### we call the target
  targs2 = c(mean(saveres[,1]),
             mean(saveres[,2]))

return(targs2)

}


#######################################################################


targets3 <- function(parm1, parm2){

  library(SimInf)

  modelforABC <- function(parm1, parm2){

    u0 = data.frame(S=c(990), I=c(10), R=c(0))

    model <- SIR(u0, 1:75, beta = parm1,
                 gamma = parm2)

    result <- run(model, threads = 1)#, seed=sample.int(1000000000,1))
    prev   <- prevalence(result, I~.)
    targ   <- numeric()
    targ[1] <- prev[50,2]
    targ[2] <- prev[75,2]
    peak_prev <- max(prev[,2])

    # pop[1]=result@U[2,30]
    # pop[2]=result@U[2,60]
    return(c(targ[1], targ[2], peak_prev))
  }

  ### try running it once, should return two population prevalence percentages
  ##modelforABC(c(0.2,0.02))



  ### set.seed for reproducability
  set.seed(123)

  ### save the results from 10000 runs, take the means as the targets
  saveres = matrix(c(0,0,0),100,3)
  for(i in 1:100){
    saveres[i,] = modelforABC(parm1,
                              parm2)
  }
  ### we call the target
  targs3 = c(mean(saveres[,1]),
             mean(saveres[,2]),
             mean(saveres[,3]))

  return(targs3)

}
targets3(0.2, 0.02)
