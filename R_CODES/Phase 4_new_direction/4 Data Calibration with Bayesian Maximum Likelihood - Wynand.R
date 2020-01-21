
library(gmp) #to use chooseZ() function in likelihood calculations

samSize <- 100

#SIR Model with 2 target features
sirModel2 <- function(beta, gamma, N = 10000, inf = 0.1, sampleSize = samSize){
  library(SimInf)
  
  # ## Test variables
  # beta = 0.1
  # gamma = 0.09
  # N = 10000
  # inf = 0.1
  # sampleSize = 100
  
  u0 <- data.frame(S=N*(1-inf), I=N*inf, R=0)
  
  model <- SIR(u0, tspan = seq(0,100,by=1), beta= beta, gamma=gamma)
  result <- run(model)
  
  individuals50 <- c(rep("S", result@U[1, 50]), rep("I", result@U[2, 50]), rep("R", result@U[3, 50]))
  individuals65 <- c(rep("S", result@U[1, 65]), rep("I", result@U[2, 65]), rep("R", result@U[3, 65]))
  
  
  samplePop <- c(summary(as.factor(sample(individuals50, size = sampleSize))), summary(as.factor(sample(individuals65, size = sampleSize))))
  
  pop <- samplePop[names(samplePop) == "I"] #lret it return zero instead of numeric(0)
  
  for(i in 1:2){
    if(is.na(pop[i])){
      pop[i] <- 0
    }
  }
  
  
  return(pop/sampleSize)
  
}

sirModel2(0.2, 0.02)



baysianML <- function(randDraw, betaGamma, samSize = 100){
  
  # randDraw = 1000
  # betaGamma = c(0.2, 0.02)
  # samSize = 1000

  BML2<- c()
 
  betaPrior <- runif(randDraw, min = 0.01, max = 0.5)
  gammaPrior <- runif(randDraw, min = 0.01, max = 0.1)
  
  #store all the model outputs
  p2 <- matrix(c(0, 0), randDraw, 2)
  
  for(i in 1:randDraw){
    
    #model output data (estimated data)  
    p2[i,] <- sirModel2(betaPrior[i], gammaPrior[i])
   
     #trueData
    x2 <- sirModel2(betaGamma[1], betaGamma[2]) * samSize
  
     #store the likehoods of each time point, per model
    loglik2 <- c()
    
        #3.
    # L(p) = p^x*(1-p)^(n - x)
    # log(L) = xlog(p) + (n-x)log(1-p)
    
    
    for(j in 1:length(x2)){
      p2[i, j] <- ifelse(p2[i, j]==0, 0.0001, p2[i, j])
      
      loglik2[j] <- log(chooseZ(samSize, x2[j])) + (x2[j])*log(p2[i,j]) + (samSize-(x2[j]))*log(1-p2[i,j])
      
    }
  
    
    #sums the likelihood values of each of the time poitns: negative log-likelihood
    BML2[i] <- sum(loglik2)  
    
    #counts the number of run times
   # cat(paste0(i, ", "))
   
  }
  
  #Now to resample from weights we need to convert log-likelihood back to a probability on a closer scale
  #Subtract the highest value from all the other values and take the exponent of the answer
  weight2 <- BML2/sum(BML2)                    #old way: exp(BML2 -max(BML2))
  like <- exp(BML2)
  
  #creating a data.frame that assigns weights to its parameter combinations
  BMLE.result2 <- data.frame(betaPrior, gammaPrior,
                             BML2, like, weight2
                             )
 
  return(list(BMLE.result2))

}
################################
# starting the main calibration method test

baysianML(randDraw = 10, betaGamma = c(0.2, 0.02) )

###################################################################
randDraw = 10
modelRuns <- 10
betaGamma = c(0.2, 0.02)

BML.post <- list()
BMLE2 <- list()


for(i in 1:modelRuns){
  #print(paste0("Model run: ", i  ))
  
  BML.post[[i]] <- baysianML(randDraw, betaGamma) 
  
  BMLE2[i] <- BML.post[[i]][1] 

}


#4.
#ReSample step and finding the mode (most common parameter) for each random Draw
b2 <- list()
g2 <- list()

b4 <- list()
g4 <- list()

b64 <- list()
g64 <- list()

resampleSize <- 5

#FIX
# SIR 100 out of 1000, consider using replace = F, if too many duplicates 
#Don't use mode -> median
for(i in 1:modelRuns){
  
  b2[[i]] <- sample(BMLE2[[i]]$betaPrior, size = resampleSize, replace = T, prob = BMLE2[[i]]$weight2) 
  g2[[i]] <- sample(BMLE2[[i]]$gammaPrior, size = resampleSize, replace = T, prob = BMLE2[[i]]$weight2)
  
  # b4[[i]] <- sample(BMLE4[[i]]$betaPrior, size = resampleSize, replace = T, prob = BMLE4[[i]]$weight4) 
  # g4[[i]] <- sample(BMLE4[[i]]$gammaPrior, size = resampleSize, replace = T, prob = BMLE4[[i]]$weight4)
  # 
  # b64[[i]] <- sample(BMLE64[[i]]$betaPrior, size = resampleSize, replace = T, prob = BMLE64[[i]]$weight64) 
  # g64[[i]] <- sample(BMLE64[[i]]$gammaPrior, size = resampleSize, replace = T, prob = BMLE64[[i]]$weight64)

}

## This would be an interesting plot to have in the thesis
# plot(BMLE2[[1]]$betaPrior, BMLE2[[1]]$gammaPrior)
# plot(b2[[1]], g2[[1]])



median.2 <- matrix(c(0, 0), modelRuns, 2)
median.4 <- matrix(c(0, 0), modelRuns, 2)
median.64 <- matrix(c(0, 0), modelRuns, 2)

for(i in 1:modelRuns){
  median.2[i,] <- c(median(b2[[i]]), median(g2[[i]]))
  median.4[i,] <- c(median(b4[[i]]), median(g4[[i]]))
  median.64[i,] <- c(median(b64[[i]]), median(g64[[i]]))
  
}

# Calculating the average calibrated parameteres
BMLE.avgPar2 <- c(round(mean(median.2[,1]), 3) , round(mean(median.2[,2]), 3))
BMLE.avgPar4 <- c(round(mean(median.4[,1]), 3) , round(mean(median.4[,2]), 3))
BMLE.avgPar64 <- c(round(mean(median.64[,1]), 3) , round(mean(median.64[,2]), 3))

print("Average parameter estimates of beta and gamma:")
print(paste0('Model with 2 target features: beta = ', BMLE.avgPar2[1] , ', gamma = ', BMLE.avgPar2[2]))
print(paste0('Model with 4 target features: beta = ', BMLE.avgPar4[1] , ', gamma = ', BMLE.avgPar4[2]))
print(paste0('Model with 64 target features: beta = ', BMLE.avgPar64[1] , ', gamma = ', BMLE.avgPar64[2]))



#Calculating the Bias
BMLE.bgBias2 <- BMLE.avgPar2 - betaGamma
BMLE.bgBias4 <- BMLE.avgPar4 - betaGamma
BMLE.bgBias64 <- BMLE.avgPar64 - betaGamma


print("The Percentage Bias of each parameter estimates of beta and gamma:")
print(paste0('Bias for Model with 2 target features: beta Bias = ', BMLE.bgBias2[1]/betaGamma[1] *100, '%, gamma Bias = ',
             BMLE.bgBias2[2]/betaGamma[2] *100, '%' ))
print(paste0('Bias for Model with 4 target features: beta Bias = ', BMLE.bgBias4[1]/betaGamma[1] *100, '%, gamma Bias = ',
             BMLE.bgBias4[2]/betaGamma[2] *100, '%' ))
print(paste0('Bias for Model with 64 target features: beta Bias = ', BMLE.bgBias64[1]/betaGamma[1] *100, '%, gamma Bias = ',
             BMLE.bgBias64[2]/betaGamma[2] *100, '%' ))



#Calculating the accuracy using the Root Mean Square Error
BMLE.bgAccu2 <- c(sqrt((sum((b2 - betaGamma[1])^2)/modelRuns)), 
                 sqrt((sum((g2 - betaGamma[2])^2)/modelRuns)))

BMLE.bgAccu4 <- c(sqrt((sum((b4 - betaGamma[1])^2)/modelRuns)), 
                 sqrt((sum((g4 - betaGamma[2])^2)/modelRuns)))

BMLE.bgAccu64 <- c(sqrt((sum((b64 - betaGamma[1])^2)/modelRuns)), 
                  sqrt((sum((g64 - betaGamma[2])^2)/modelRuns)))

print("The accuracy of each parameter estimates of beta and gamma using RMSE:")
print(paste0('RMSE for Model with 2 target features: beta = ', round(BMLE.bgAccu2[1], 3), ' gamma = ',  round(BMLE.bgAccu2[2], 3)))
print(paste0('RMSE for Model with 4 target features: beta = ', round(BMLE.bgAccu4[1], 3), ' gamma = ',  round(BMLE.bgAccu4[2], 3)))
print(paste0('RMSE for Model with 64 target features: beta = ', round(BMLE.bgAccu64[1], 3), ' gamma = ',  round(BMLE.bgAccu64[2], 3)))


# Calculating confidence intervals
BML_2.5 <- randDraw * 0.025
BML_97.5 <- randDraw * 0.975

BML.CI_2_b <- matrix(c(0, 0), modelRuns, 2)
BML.CI_2_g <- matrix(c(0, 0), modelRuns, 2)

BML.CI_4_b <- matrix(c(0, 0), modelRuns, 2)
BML.CI_4_g <- matrix(c(0, 0), modelRuns, 2)

BML.CI_64_b <- matrix(c(0, 0), modelRuns, 2)
BML.CI_64_g <- matrix(c(0, 0), modelRuns, 2)

#FIX

for(i in 1:modelRuns){
  BML.CI_2_b[i,] <- c(sort(b2[[i]])[BML_2.5], sort(b2[[i]])[BML_97.5])
  BML.CI_2_g[i,] <- c(sort(g2[[i]])[BML_2.5], sort(g2[[i]])[BML_97.5])

  BML.CI_4_b[i,] <- c(sort(b4[[i]])[BML_2.5], sort(b4[[i]])[BML_97.5])
  BML.CI_4_g[i,] <- c(sort(g4[[i]])[BML_2.5], sort(g4[[i]])[BML_97.5])
  
  BML.CI_64_b[i,] <- c(sort(b64[[i]])[BML_2.5], sort(b64[[i]])[BML_97.5])
  BML.CI_64_g[i,] <- c(sort(g64[[i]])[BML_2.5], sort(g64[[i]])[BML_97.5])
 
}

# Now to calculate coverage of the true estimate given the confidence intervals of the parameter estimates
BML.2_bcov <- sum((betaGamma[1] > BML.CI_2_b[,1]) == TRUE & (betaGamma[1] < BML.CI_2_b[,2]) == TRUE)/modelRuns * 100
BML.2_gcov <- sum((betaGamma[2] > BML.CI_2_g[,1]) == TRUE & (betaGamma[2] < BML.CI_2_g[,2]) == TRUE)/modelRuns * 100

BML.4_bcov <- sum((betaGamma[1] > BML.CI_4_b[,1]) == TRUE & (betaGamma[1] < BML.CI_4_b[,2]) == TRUE)/modelRuns * 100
BML.4_gcov <- sum((betaGamma[2] > BML.CI_4_g[,1]) == TRUE & (betaGamma[2] < BML.CI_4_g[,2]) == TRUE)/modelRuns * 100

BML.64_bcov <- sum((betaGamma[1] > BML.CI_64_b[,1]) == TRUE & (betaGamma[1] < BML.CI_64_b[,2]) == TRUE)/modelRuns * 100
BML.64_gcov <- sum((betaGamma[2] > BML.CI_64_g[,1]) == TRUE & (betaGamma[2] < BML.CI_64_g[,2]) == TRUE)/modelRuns * 100

print("The coverage of each parameter estimates of beta and gamma given the CI's:")
print(paste0('Coverage for Model with 2 target features: beta = ', BML.2_bcov, '%, gamma = ',  BML.2_gcov, '%'))
print(paste0('Coverage for Model with 4 target features: beta = ', BML.4_bcov, '%, gamma = ',  BML.4_gcov, '%'))
print(paste0('Coverage for Model with 64 target features: beta = ', BML.64_bcov, '%, gamma = ',  BML.64_gcov, '%'))



