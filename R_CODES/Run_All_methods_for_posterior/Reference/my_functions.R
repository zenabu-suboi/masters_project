
# 1. ABC function
## this function runs a stochastic SIR model and outputs peak prevalence and/or 
##prevalence at times 50 & 75 as targets to used for the ABC simulations

modelforABC = function(parameters, 
                      times=1:75, 
                      targetTimes=c(50,75),
                      peakPrevalence = F){

  
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


## Run model once to get new targets
#modelforABC(c(0.2,0.02))
# 2 Targets = c(0.644, 0.404)
# 3 Targets = c(0.622, 0.371, 0.677)

# modelforABCmcmc2_ArbitraryTargets(c(0.2,0.02),
#                                   times=1:75,
#                                   targetTimes = c(50,75),
#                                   peakPrevalence = TRUE)




###########################################################################################
# 2. function for obtaining targets

targets2 <- function(my_parameters){
  
  ### set.seed for reproducibility
  set.seed(123)
  
  ### save the results from 1000 runs, take the means as the targets
  targetStats = matrix(c(0,0),100,2)
  for(i in 1:100){
    targetStats[i,] = modelforABC(c(my_parameters[1],
                                    my_parameters[2]))
  }
  ### we call the target: meanTargetStats 
  meanTargetStats = c(mean(targetStats[,1]),
                      mean(targetStats[,2]))
  return(meanTargetStats) 
  
}

#targets(c(0.2, 0.02))

############################################


# 2. function for obtaining targets

targets3 <- function(my_parameters){
  
  ### set.seed for reproducibility
  set.seed(123)
  
  ### save the results from 1000 runs, take the means as the targets
  targetStats = matrix(c(0,0,0),100,3)
  for(i in 1:100){
    targetStats[i,] = modelforABC(c(my_parameters[1],
                                    my_parameters[2]))
  }
  ### we call the target: meanTargetStats 
  meanTargetStats3 = c(mean(targetStats[,1]),
                      mean(targetStats[,2]),
                      mean(targetStats[,3]))
  return(meanTargetStats3) 
  
}

#targets3(c(0.2, 0.02))





############################################################################################
# 2. BMLE function

#samSize <- 100

library(SimInf)

sir_bmle <- function(beta,  # this fuction creats the model to be used 
                     #in obtaining both observed (calibraton targets) and modle outcomes
                     gamma, 
                     N = 1000,
                     inf = 0.1, 
                     sampleSize = samSize){
  
  
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


########################################################################################

targets_bmle <- function(my_params){
  
  ### set.seed for reproducibility
  set.seed(123)
  
  ### save the results from 1000 runs, take the means as the targets
  targetStats_bmle = matrix(c(0,0),100,2)
  for(i in 1:100){
    targetStats_bmle[i,] = sir_bmle(my_params[1], my_params[2])
  }
  ### we call the target: meanTargetStats 
  meanTargetStats_bmle = c(mean(targetStats_bmle[,1]),
                      mean(targetStats_bmle[,2]))
  return(meanTargetStats_bmle) 
  
}



######################################################################
# this function 

bmle <- function(randDraw, betaGamma, samSize){ # func takes 3 arguments
  
  #testing the method
  # randDraw = 10
  # betaGamma = c(0.2, 0.02)
  # samSize = 100
  
  
  # step 1
  
  joint_logliks <- c() # empty vector 
  
  betaPrior <- runif(randDraw, min = 0.0, max = 1.0) # specifies beta prior
  gammaPrior <- runif(randDraw, min = 0.0, max = 0.5) # specifies gamma prior
  
  ###################################################################
  
  #step 2
  #store all the model outputs
  model_outputs <- matrix(c(0, 0), randDraw, 2)
  
  for(i in 1:randDraw){
    #store all the model outputs  
    model_outputs[i,] <- round(sir_bmle(betaPrior[i], gammaPrior[i]))
    
    #trueData
    observed_output <- round(targets_bmle(c( betaGamma[1],  betaGamma[2])))
    
    ####################################################################
    
    #step 3
    #store the likehoods of each time point
    loglik <- c()  # creats empty vector
    
    
    #3.
    # L(p) = (n choose x) * p^x * (1-p)^(n - x)
    # log(L) = log(n choose x) + xlog(p) + (n-x)log(1-p)
    
    
    for(j in 1:length(observed_output)){
      
      loglik[j] <- dbinom(observed_output[j], 
                          samSize,
                          (model_outputs[i,j]/samSize),
                          log = TRUE)
    }
    
    #sums the two log-likelihood values of the time points 
    #per model run 
    
    joint_logliks[i] <- sum(loglik) 
    
  }
  
  #rescaling the log likelihoods back to likelihood values, for each
  #parameter combination
  
  # the likelihood is high for values of the parameter combination
  #that make observed output = model output more 
  #likely, and small for values of parameter combination that
  # make observed output = model output unlikely
  
  
  likelihoods <- exp(joint_logliks) # computes the likelihoods as the exp 
  #  of the loglikelihoods
  
  weights <- likelihoods/sum(likelihoods)  
  
  BMLE.result <- data.frame(betaPrior, gammaPrior, 
                            joint_logliks, likelihoods, weights)
  
  
  return(list(BMLE.result))
}

