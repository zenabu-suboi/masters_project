
##############################################################################
# run model using all 5000 parms

samSize <- 100

library(SimInf)

sir_bmle <- function(beta,
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

sir_bmle(0.2, 0.02)



#####################################################################
############################################################################


## Data simulation with Bayesian Maximum Likelihood Estimation

## Steps for BMLE algorithm (Menzies, et.al., 2017): Sampling from 
#the posterior distribution

# 1. Draw a large number of parameter sets from the prior distribtion
# 2. For each parameter set, run the model and estimate model outcomes.
# 3. Using these model outcomes, estimate the likelihood for the
#parameter set and retain this value (log likelihood)
# 4. Resample from the original parameter sample with replacement,
#using the likelihood values as sampling weights


#1.


#3.
# L(p) = p^x*(1-p)^(n - x)
# log(L) = xlog(p) + (n-x)log(1-p)

#2.


library(gmp)


bmle <- function(randDraw, betaGamma, samSize){ # func takes 3 arguments
  
  #testing the method
  # randDraw = 10
  # betaGamma = c(0.2, 0.02)
  # samSize = 100
  
 
# step 1
  
  joint_logliks <- c() # empty vector 
  
  betaPrior <- runif(randDraw, min = 0.0, max = 1.0) # specifies beta prior
  gammaPrior <- runif(randDraw, min = 0.0, max = 0.05) # specifies gamma prior
  
###################################################################
  
#step 2
  #store all the model outputs
   model_outputs <- matrix(c(0, 0), randDraw, 2)
  
  for(i in 1:randDraw){
    #store all the model outputs  
    model_outputs[i,] <- sir_bmle(betaPrior[i], gammaPrior[i])
   
    #trueData
    observed_output <- sir_bmle(betaGamma[1], betaGamma[2])

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


# 100 runs = 10.39s
# 1 run = 10.39/100 = 0.1039
# 10000 runs = 0.1039*10000 = 1039s
#SABC time to run 10000 simulations = 6590.6s

###################################################################
#using estimated time to run parameter combinations
#library(SimDesign)

ptm <- proc.time() #records time for running randDraw 
#parameter combinations

randDraw = 509163
func_results <- bmle(randDraw = 509163, betaGamma = c(0.2, 0.02),
                     samSize = 100)

set.seed(123)

#4 resampling from the posterior
 
resample <- sample(x = c(1 : randDraw), size=5000, replace=T,
                    prob = func_results[[1]]$weights)
 
resample_output <- func_results[[1]][sort(unique(resample)),]

post <- data.frame(resample_output[,c(1,2,4)],
                        table(resample))
posterior <- post[,c(1:3,5)]

# Stop the clock
proc.time() - ptm


saveRDS(object = posterior, file = 'bmleposterior.rds')
bmle_posterior <- readRDS('bmleposterior.rds')

summary(as.factor(bmle_posterior$Freq))
#############################################################################

# posterior plot
betapost <- bmle_posterior[,1]
gammapost <- bmle_posterior[,2]
plot(betapost, gammapost,
     main = "Plot of the BMLE posterior")


#bmle_posterior$Freq == 1
unique(bmle_posterior$Freq)

  beta <- bmle_posterior$betaPrior[bmle_posterior$Freq == 1]
  gamma <- bmle_posterior$gammaPrior[bmle_posterior$Freq == 1]
  
  plot(beta,gamma,
       col="black",
       pch = 20,
       legend = 
       main="Plot of the BMLE posterior") # plot all 5000 I curves as one plot
  
  points(bmle_posterior$betaPrior[bmle_posterior$Freq == 2],
         bmle_posterior$gammaPrior[bmle_posterior$Freq == 2],
         pch = 20,
        col="red")
  
  points(bmle_posterior$betaPrior[bmle_posterior$Freq == 3],
         bmle_posterior$gammaPrior[bmle_posterior$Freq == 3],
         pch = 19,
        col="blue")

  points(bmle_posterior$betaPrior[bmle_posterior$Freq == 4],
         bmle_posterior$gammaPrior[bmle_posterior$Freq == 4],
         pch = 19,
        col="orange")
 


###################################################################
plot(betapost, gammapost,
     main = "Plot of the BMLE posterior and referrence")
points(abc2ref1$unadj.values[,1],
       abc2ref1$unadj.values[,2],
       col = "red")

