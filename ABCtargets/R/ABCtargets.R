




targets <- function(parameters){

  library(SimInf)

  modelforABC = function(parameters,
                         times = 1:75,
                         targetTimes = c(50,75),
                         peakPrevalence = FALSE){


    u0 = data.frame(S = c(990), # initial compartmental values
                    I = c(10),
                    R = c(0))

    model <- SIR(u0, # initial compartmental values
                 times,                  # stochastic sir model that outputs different epicurves
                 beta = parameters[1],      # per run
                 gamma = parameters[2])

    result <- run(model,
                  threads = 1)   # runs the SIR model and outputs results

    prev <- prevalence(result, I~.)
    # targ <- numeric()
    targs <- prev[targetTimes,2]

    if (peakPrevalence){

      return(c(targs,max(prev[,2])))}

    else(return(targs))

     }


### set.seed for reproducability
set.seed(123)

### save the results from 10000 runs, take the means as the targets
savetargs = matrix(c(0,0),100,2)
for(i in 1:100){
  savetargs[i,] = modelforABC(parameters)
}

### we call the target
  targs = c(mean(savetargs[,1]),
             mean(savetargs[,2]))

return(targs)

}

