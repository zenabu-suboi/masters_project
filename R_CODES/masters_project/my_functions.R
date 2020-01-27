
# 1. ABC function
## this function runs a stochastic SIR model and outputs peak prevalence and/or 
##prevalence at times 50 & 75 as targets to used for the ABC simulations

modelforABC = function(parameters, 
                      times=1:75, 
                      targetTimes=c(50,75),
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
  
  if (peakPrevalence){return(c(targs,max(prev[,2])))}
  else(return(targs))
}


# modelforABCmcmc2_ArbitraryTargets(c(0.2,0.02),
#                                   times=1:75,
#                                   targetTimes = c(50,75),
#                                   peakPrevalence = TRUE)



