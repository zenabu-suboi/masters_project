
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




###########################################################################################
# 2. BMLE function

#samSize <- 100

library(SimInf)

sir_bmle <- function(beta,
                     gamma, 
                     N = 1000,
                     inf = 0.1, 
                     sampleSize = 100){
  
  
  u0 <- data.frame(S=N*(1-inf),
                   I=N*inf, R=0)
  
  model <- SIR(u0, tspan = seq(0,75,by=1),
               beta = beta, gamma=gamma)
  
  result <- run(model)
  
  individuals50 <- c(rep("S", result@U[1, 50]),
                     rep("I", result@U[2, 50]),
                     rep("R", result@U[3, 50]))
  
  individuals75 <- c(rep("S", result@U[1, 75]),
                     rep("I", result@U[2, 75]),
                     rep("R", result@U[3, 75]))
  
  
  samplePop <- c(summary(as.factor
                        (sample(individuals50,
                        size = sampleSize))),
                        summary(as.factor(sample(individuals75,
                        size = sampleSize))))
  
  pop <- samplePop[names(samplePop) == "I"] #lret it return zero instead of numeric(0)
  
  # prev <- prevalence(result, I~.)
  # peak_prev <- max(prev[,2])
  
  for(i in 1:2){
    if(is.na(pop[i])){
      pop[i] <- 0
    }
  }
  
  
  return(pop)
  
}

#sir_bmle(0.2, 0.02)

