


library(gmp)        #for chooseZ() function computes the binomial coefficient
library(dplyr)      # for sample_n function, samples rows using probs
#library(SimInf)
###################################################################
setwd("C:/Users/Zee/Documents/GitHub/masters_project/R_CODES/
      Recording_Efficiency_(time)/BMLE/")

source("my_functions.R")
####################################################################

# ii. obtaining two targets for bmle. Use same targets as ABC methods

# c(0.644, 0.404) targets / observed data for bmle2

###############################################################################

# Following Steps from Menzies paper

# 1 - 3a. This function generates a prior distribution for the parameters and assigns likelihood values 
#to the parameter combinations

bayesianML <- function(randDraw, 
                      popsize = 1000){
  
  BML2.loglik2 <- c()
  
  betaPrior <- runif(randDraw, min = 0, max = 1)
  gammaPrior <- runif(randDraw, min = 0, max = 0.5)
  
  p2 <- matrix(c(0, 0), randDraw, 2)
 
  
  for(i in 1:randDraw){
   
    p2[i,] <- modelforABC(c(betaPrior[i], gammaPrior[i])) # model output
   
    x2 <- c(0.644, 0.404) * popsize # observed output / targets
    
    loglik2 <- c()
    
    #3.
    # L(p) = p^x*(1-p)^(n - x)
    # log(L) = log(nCx) + xlog(p) + (n-x)log(1-p)
    
    
    for(j in 1:length(x2)){
      p2[i, j] <- ifelse(p2[i, j]==0, 0.0001, p2[i, j])
      
    
      loglik2[j] <- log(chooseZ(popsize, x2[j])) + 
                        (x2[j])*log(p2[i,j]) +
                        (popsize-(x2[j]))*
                        log(1-p2[i,j]) # computes the joint log likelihood
    }
    

    BML2.loglik2[i] <- sum(loglik2)  
   
   # cat(paste0(i, ", "))
    
  }
  
  #Now to assosciate each parameter combination its log-likelihood
  BMLE.result2 <- data.frame(betaPrior,gammaPrior, BML2.loglik2)
  
  return(BMLE.result2)
  
}


# library(tictoc)
# tic()
# baysianML(10, c(0.2,0.02),100)
# toc()

################################################################
#Running the calibration method and storing the results

randDraw <- 10 #60000 # number of modle runs (simulations)
parameters <- c(0.2, 0.02)   #True values of the parameters
popsize <- 1000

set.seed(121)
# open file connection
record_time_bmle2 <- file("mytime_bmle_2targets.txt")
open(record_time_bmle2, "w")

BMLE2 <- bayesianML(randDraw, popsize) 

close(record_time_bmle2) ## close file connection
unlink(record_time_bmle2)


nameCols2 <- c("betaPrior", "gammaPrior", "likelihood", "weight2")

##################################################################

## 3b. Weight calculation Method 

  likelihood <- exp( BMLE2[[1]]$BML2.loglik2) # computes likelihood

  weight2 <- likelihood/sum(likelihood) # computes weights
  
  BMLE2.weight2 <- data.frame( BMLE3[[1]]$betaPrior,
                               BMLE3[[1]]$gammaPrior,
                               likelihood,
                                   weight2) # change bmle3 to 2 
  
  colnames(BMLE2.weight2) <- nameCols2
  
######################################################

#4.
#ReSample step 
#BMLE.post.2 <- list()

resampleSize <- 10

## Finding the posterior distribution using the weights calculated above.
  
  BMLE.post.2 <- sample_n(BMLE2.weight2,
                               size = resampleSize,
                               replace = T, 
                               weight = BMLE2.weight2$weight2) 

 #sum(BMLE2.weight2$weight2)

  plot(BMLE.post.2$betaPrior, BMLE.post.2$gammaPrior)
  ###########################################
  
  # resample2 <- sample(x = c(1 : randDraw), size = 40, replace=F,
  #                    prob = BMLE2.weight2$weight2)
  # 
  # resample_output2 <- BMLE2.weight2[sort(unique(resample2)),]
  # 
  # post2 <- data.frame(resample_output2[,c(1,2,4)],
  #                    table(resample2))
  # posterior2 <- post2[,c(1:3,5)]
  # 
