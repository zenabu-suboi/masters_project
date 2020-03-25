
#########################################################
setwd("C:/Users/Zee/Documents/GitHub/masters_project/R_CODES/
      Recording_Efficiency_(time)/BMLE")

#########################################################

#bmle
targets2_bmle_prior <- as.data.frame(readRDS("bmle2_prior_choices.rds"))
targets3_bmle_prior <- as.data.frame(readRDS("bmle3_prior_choices.rds"))

#likelihood <- exp( BMLE3$BML3.loglik3) # computes likelihood

targets2_bmle_prior$weight2 <- exp( targets2_bmle_prior$BML2.loglik2)/
  sum(exp( targets2_bmle_prior$BML2.loglik2)) # computes weights



targets3_bmle_prior$weight3 <- exp( targets3_bmle_prior$BML3.loglik3)/
  sum(exp( targets3_bmle_prior$BML3.loglik3)) # computes weights


# ESS = (sum(weights))^2 / sum(weight^2)
attach(targets2_bmle_prior)
attach(targets3_bmle_prior)

# mydat_2targets_bmle$squared_weight2 <- weight2^2
# mydat_3targets_bmle$squared_weight3 <- weight3^2


ess_2targets <- (sum(weight2))^2 / sum(weight2^2) # = 28 , eff = 28/6e4
  
ess_3targets <- (sum(weight3))^2 / sum(weight3^2) # = 7


####################################################
# example
equalweights <- rep(0.1,10)

oneweightdoms <- c(0.9, rep(0.01/9, 9))

ESS <-  function(weights){
  
  sum(weights)^2/(sum(weights^2))
  
}

ESS(equalweights)

ESS(oneweightdoms)  
  