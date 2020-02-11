

#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R")

################################################################################
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

#3.
# L(p) = p^x*(1-p)^(n - x)
# log(L) = xlog(p) + (n-x)log(1-p)


library(gmp)

samSize <- 100

# 100 runs = 10.39s
# 1 run = 10.39/100 = 0.1039
# 10000 runs = 0.1039*10000 = 1039s
#SABC time to run 10000 simulations = 6590.6s

###################################################################
#using estimated time to run parameter combinations
#library(SimDesign)

ptm <- proc.time() #records time for running randDraw 
#parameter combinations

#randDraw = 509163 # original value

randDraw = 50

func_results <- bmle(randDraw = 50, betaGamma = c(0.2, 0.02),
                     samSize = 100) # samples randDraw samples from the prior

set.seed(122)

#4 resampling from the posterior
 # size = 5000 original value
resample <- sample(x = c(1 : randDraw), size = 40, replace=T,
                    prob = func_results[[1]]$weights)
 
resample_output <- func_results[[1]][sort(unique(resample)),]

post <- data.frame(resample_output[,c(1,2,4)],
                        table(resample))
posterior <- post[,c(1:3,5)]

# Stop the clock
proc.time() - ptm


saveRDS(object = posterior, file = 'bmleposterior.rds')
bmle_posterior <- readRDS('bmleposterior.rds')

#summary(as.factor(bmle_posterior$Freq))
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
       main="Plot of the BMLE posterior"
       )
  
  points(bmle_posterior$betaPrior[bmle_posterior$Freq == 2],
         bmle_posterior$gammaPrior[bmle_posterior$Freq == 2],
         pch = 20,
        col="red")
  
  points(bmle_posterior$betaPrior[bmle_posterior$Freq == 7],
         bmle_posterior$gammaPrior[bmle_posterior$Freq == 7],
         pch = 19,
        col="blue")

  points(bmle_posterior$betaPrior[bmle_posterior$Freq == 10],
         bmle_posterior$gammaPrior[bmle_posterior$Freq == 10],
         pch = 19,
        col="green")
 
  legend("topright", 
         legend = c("Freq 1", "Freq 2", "Freq 3", "Freq 4"), 
         col = c("black", "red", "blue", "green"), 
         pch = 19, 
         # bty = "n", 
         # pt.cex = 2, 
         cex = 1.2)

###################################################################
plot(betapost, gammapost,
     main = "Plot of the BMLE posterior and referrence")
points(abc2ref1$unadj.values[,1],
       abc2ref1$unadj.values[,2],
       col = "red")

