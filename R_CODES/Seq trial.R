
library(EasyABC)
library(SimInf)



## this time, the model has two parameters and outputs two summary statistics.
## defining a simple toy model:
toy_model<-function(x){ c( x[1] + x[2] + rnorm(1,0,0.1) , x[1] * x[2] + rnorm(1,0,0.1) ) }

toy_model(c(5,8))

## define prior information
toy_prior=list(c("unif",0,1),c("normal",1,2))
# a uniform prior distribution between 0 and 1 for parameter 1, and a normal distribution
# of mean 1 and standard deviation of 2 for parameter 2.

## define the targeted summary statistics
sum_stat_obs=c(1.5,0.5)


pacc=0.4
# Only uniform priors are supported for the method "Lenormand" (since it performs a Latin
# Hypercube sampling at the beginning)
toy_prior=list(c("unif",0,1),c("unif",0.5,1.5))

c_drov=0.7
ABC_Drovandi<-ABC_sequential(method="Drovandi", model=toy_model, prior=toy_prior,
                             nb_simul=20, summary_stat_target=sum_stat_obs, tolerance_tab=tolerance, c=c_drov)
ABC_Drovandi



tolerance=c(1.5,0.5)
ABC_Beaumont<-ABC_sequential(method="Beaumont", model=toy_model, prior=toy_prior,
                             nb_simul=20, summary_stat_target=sum_stat_obs, tolerance_tab=tolerance)
ABC_Beaumont



## this time, the model has two parameters and outputs two summary statistics.
## defining a simple toy model:
toy_model2<-function(x){ c( x[1] + x[2] + rnorm(1,0,0.1) , x[1] * x[2] + rnorm(1,0,0.1) ) }

toy_model2(c(5,8))

## define prior information
toy_prior2=list(c("unif",0,1),c("normal",1,2))
# a uniform prior distribution between 0 and 1 for parameter 1, and a normal distribution
# of mean 1 and standard deviation of 2 for parameter 2.

## define the targeted summary statistics
sum_stat_obs2=c(1.5,0.5)


pacc=0.4
# Only uniform priors are supported for the method "Lenormand" (since it performs a Latin
# Hypercube sampling at the beginning)
toy_prior2=list(c("unif",0,1),c("unif",0.5,1.5))


ABC_Lenormand<-ABC_sequential(method="Lenormand", model=toy_model2, prior=toy_prior2,
                              nb_simul=20, summary_stat_target=sum_stat_obs2, p_acc_min=pacc)
ABC_Lenormand
plot(ABC_Lenormand$param[, 1], ABC_Lenormand$param[, 2])



?ABC_sequential
