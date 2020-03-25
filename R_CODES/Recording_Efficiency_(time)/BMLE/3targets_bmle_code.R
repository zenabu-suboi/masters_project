


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

bayesianML3 <- function(randDraw, 
                       popsize = 1000){
  
  BML3.loglik3 <- c()
  
  betaPrior <- runif(randDraw, min = 0, max = 1)
  gammaPrior <- runif(randDraw, min = 0, max = 0.5)
  
  p3 <- matrix(replicate(3, 0), randDraw, 3)
  
  
  for(i in 1:randDraw){
    
    p3[i,] <- modelforABC(c(betaPrior[i], gammaPrior[i])) # model output
    
    x3 <- c(0.622, 0.371, 0.677) * popsize # observed output / targets
    
    loglik3 <- c()
    
    #3.
    # L(p) = p^x*(1-p)^(n - x)
    # log(L) = log(nCx) + xlog(p) + (n-x)log(1-p)
    
    
    for(j in 1:length(x3)){
      p3[i, j] <- ifelse(p3[i, j]==0, 0.0001, p3[i, j])
      
      
      loglik3[j] <- log(chooseZ(popsize, x3[j])) + 
        (x3[j])*log(p3[i,j]) +
        (popsize-(x3[j]))*
        log(1-p3[i,j]) # computes the joint log likelihood
    }
    
    
    BML3.loglik3[i] <- sum(loglik3)  
    
   # cat(paste0(i, ", "))
    
  }
  
  #Now to assosciate each parameter combination its log-likelihood
  BMLE.result3 <- data.frame(betaPrior,gammaPrior, BML3.loglik3)
  
  return(BMLE.result3)
  
}


# library(tictoc)
# tic()
# baysianML(10, c(0.2,0.02),100)
# toc()

################################################################
#Running the entire calibration method and storing the results


#bmle_time3 <- function(randDraw, # number of modle runs (simulations)
 #                      parameters,   #True values of the parameters
  #                     popsize = 1000){
  
set.seed(121)
  randDraw = 75000 # number of modle runs (simulations)
  parameters = c(0.2,0.02)  #True values of the parameters
  popsize = 1000
  
  #open file connection
  record_time_bmle3 <- file("mytime_bmle_3targets.txt")
   open(record_time_bmle3, "w")
  
  BMLE3 <- bayesianML3(randDraw, popsize) 
  
   close(record_time_bmle3) ## close file connection
  unlink(record_time_bmle3)
  
  # read in time data and record model runtimes
  model_time3 <- read.csv("mytime_bmle_3targets.txt", header = F)
  sum(model_time3)/10^9
  
  
  nameCols3 <- c("betaPrior", "gammaPrior", "likelihood", "weight3")
  
  ##################################################################
  
  ## 3b. Weight calculation Method 
  
  likelihood <- exp( BMLE3$BML3.loglik3) # computes likelihood
  
  weight3 <- likelihood/sum(likelihood) # computes weights
  
  BMLE3.weight3 <- data.frame( BMLE3$betaPrior,
                               BMLE3$gammaPrior,
                               likelihood,
                               weight3) # change bmle3 to 2 
  
  colnames(BMLE3.weight3) <- nameCols3
  
  ######################################################
  
  #4.
  #ReSample step 
  #BMLE.post.2 <- list()
  
  resampleSize <- 5000
  
  ## Finding the posterior distribution using the weights calculated above.
  
  BMLE.post.3 <- sample_n(BMLE3.weight3,
                          size = resampleSize,
                          replace = T, 
                          weight = BMLE3.weight3$weight3) 
  
  
  #return(BMLE.post.3)
#}



#################################################################
# recording the total runtime
#set.seed(121)

#bmle3_time <- microbenchmark(
 # bmle3_tot_time <- bmle_time3(randDraw = 75000, # number of modle runs (simulations)
  #           parameters = c(0.2, 0.02),   #True values of the parameters
   #          popsize = 1000)
 # , times = 1)

###################################################################  
  # save prior choices and posterior
  saveRDS(BMLE3, "bmle3_prior_choices.rds")
  bmle3_prior_choices <- readRDS("bmle3_prior_choices.rds")
  
  saveRDS(BMLE.post.3, "bmle3_posterior.rds")
  bmle3_posterior <- readRDS("bmle3_posterior.rds")
  
  
  
  unique_param_sets3  <- length(unique(BMLE.post.3$weight3)) # = 37

  #table(BMLE.post.3$weight3)
  
  
plot(BMLE.post.3$betaPrior, BMLE.post.3$gammaPrior
     , xlim = c(0,1), ylim = c(0,0.05))



#################################################################

# resample2 <- sample(x = c(1 : randDraw), size = 40, replace=T,
#                    prob = BMLE2.weight2$weight2)
# 
# resample_output2 <- BMLE2.weight2[sort(unique(resample2)),]
# 
# post2 <- data.frame(resample_output2[,c(1,2,4)],
#                    table(resample2))
# posterior2 <- post2[,c(1:3,5)]
# 
