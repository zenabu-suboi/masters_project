modelforABCmcmc2 = function(parameters){
  library(SimInf)
  u0= data.frame(S = c(990), # initial compartmental values
                 I = c(10), 
                 R = c(0))
  
  model <- SIR(u0, 1:75,                  # stochastic sir model that outputs different epicurves
               beta = parameters[1],      # per run
               gamma = parameters[2]) 
  
  result <- run(model, 
                threads = 1)   # runs the SIR model and outputs results
  
  prev <- prevalence(result, I~.)
  targ <- numeric()
  targ[1] <- prev[50,2]
  targ[2] <- prev[75,2]
  
  return(c(targ[1],targ[2]))
}


modelforABCmcmc2(c(0.2,0.02))