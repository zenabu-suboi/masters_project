


library(gmp)        #for chooseZ() function
library(dplyr)      # for sample_n function
library(SimInf)

###############################################################################

## i. Setting up the Model that generates observed data and output data from explored parameters

samSize <- 100 # size of the sample to be randomly selected at the two time points per model run
               # and then the number of infected ind. recorded

#SIR Model with 2 target features
sirModel2 <- function(beta,
                      gamma, 
                      N = 1000, 
                      inf = 0.1, 
                      sampleSize = samSize){
  
  u0 <- data.frame(S=N*(1-inf),
                   I=N*inf, 
                   R=0)
  
  model <- SIR(u0, 
               tspan = seq(0,100,by=1),
               beta= beta, 
               gamma=gamma)
  
  result <- run(model)
  
  individuals50 <- c(rep("S", result@U[1, 50]),
                     rep("I", result@U[2, 50]), 
                     rep("R", result@U[3, 50]))
  
  individuals65 <- c(rep("S", result@U[1, 65]),
                     rep("I", result@U[2, 65]),
                     rep("R", result@U[3, 65]))
  
  
  samplePop <- c(summary(as.factor(sample(individuals50, size = sampleSize))),
                 summary(as.factor(sample(individuals65, size = sampleSize)))) 
  
  pop <- samplePop[names(samplePop) == "I"] #lret it return zero instead of numeric(0)
  
  for(i in 1:2){
    if(is.na(pop[i])){
      pop[i] <- 0
    }
  }
  
  
  return(pop/sampleSize)
  
}


#sirModel2(0.2,0.02)

####################################################

# ii. obtaining two targets for bmle


targets2_bmle <- function(beta, gamma){
  
  ### set.seed for reproducibility
  set.seed(123)
  
  ### save the results from 1000 runs, take the means as the targets
  targetStats_bmle = matrix(c(0,0),100,2)
  for(i in 1:100){
    targetStats_bmle[i,] = sirModel2(beta, gamma)
  }
  ### we call the target: meanTargetStats 
  meanTargetStats_bmle = c(mean(targetStats_bmle[,1]),
                           mean(targetStats_bmle[,2]))
  return(meanTargetStats_bmle) 
  
}

#targets2_bmle(0.2,0.02) targets / observed data for bmle2

###############################################################################

# Following Steps from Menzies paper

# 1 - 3a. This function generates a prior distribution for the parameters and assigns likelihood values 
#to the parameter combinations


baysianML <- function(randDraw, 
                      betaGamma, 
                      samSize = samSize){
  
  BML2.loglik2 <- c()
  
  betaPrior <- runif(randDraw, min = 0.18, max = 0.22)
  gammaPrior <- runif(randDraw, min = 0.01, max = 0.03)
  
  p2 <- matrix(c(0, 0), randDraw, 2)
 
  
  for(i in 1:randDraw){
    
    p2[i,] <- sirModel2(betaPrior[i], gammaPrior[i]) # model output
   
    x2 <- targets2_bmle(betaGamma[1], betaGamma[2]) * samSize # observed output / targets
    
    loglik2 <- c()
    
    #3.
    # L(p) = p^x*(1-p)^(n - x)
    # log(L) = xlog(p) + (n-x)log(1-p)
    
    
    for(j in 1:length(x2)){
      p2[i, j] <- ifelse(p2[i, j]==0, 0.0001, p2[i, j])
      
      print(p2)
      loglik2[j] <- log(chooseZ(samSize, x2[j])) + 
                        (x2[j])*log(p2[i,j]) +
                        (samSize-(x2[j]))*
                        log(1-p2[i,j])
      print(loglik2)
    }
    

    BML2.loglik2[i] <- sum(loglik2)  
   
    #cat(paste0(i, ", "))
    
  }
  
  #Now to assosciate each parameter combination its log-likelihood
  BMLE.result2 <- data.frame(betaPrior,gammaPrior, BML2.loglik2)
  
  return(list(BMLE.result2))
  
}


library(tictoc)
tic()
baysianML(10, c(0.2,0.02),100)
toc()

#Running the calibration method and storing the results

randDraw <- 509163
betaGamma <- c(0.2, 0.02)   #True values of the parameters
 
BMLE2 <- baysianML(randDraw, betaGamma, samSize) 

nameCols2 <- c("betaPrior", "gammaPrior", "likelihood", "weight2")

##################################################################

## 3b. Weight calculation Method 

BMLE3= baysianML(10, c(0.2,0.02),100)
BMLE3
likelihood <- exp(BMLE3[[1]]$BML2.loglik2) # computes likelihood

round(likelihood,3)
  
  likelihood <- exp( BMLE2[[1]]$BML2.loglik2) # computes likelihood

### my rubbish (please remove)
likelihood= c(0.1,0.2,0.01,0.1)
 


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
