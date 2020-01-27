
library(SimInf)
library(EasyABC)

##########################################################################

### create function to use in ABC_mcmc
# scenario 1 == two targets

modelforABCmcmc2 = function(parameters){
  library(SimInf)
  u0= data.frame(S = c(990), # initial compartmental values
                 I = c(10), 
                 R = c(0))
  
  model <- SIR(u0, 1:75,                  # stochastic sir model that outputs different epicurves
               beta = parameters[1],      # per run
               gamma = parameters[2]) 
  
  result <- run(model, 
                threads = 1)   # runs the SIR model and outputs results
  
  prev <- prevalence(result, I~.)
  targ <- numeric()
  targ[1] <- prev[50,2]
  targ[2] <- prev[75,2]
  
  return(c(targ[1],targ[2]))
}


### try running it once, should return two population prevalence percentages
modelforABC(c(0.2,0.02))



### set.seed for reproducability
set.seed(123)

### save the results from 10000 runs, take the means as the targets
saveres = matrix(c(0,0),100,2)
for(i in 1:100){
  saveres[i,] = modelforABC(c(0.2,0.02))
}
### we call the target: truepop.prev 
truepop = c(mean(saveres[,1]),
            mean(saveres[,2]))
truepop


######################################################################
# scenario 1 == 2 targets

set.seed(234)
#tol=100%
ABC_rej2<-ABC_rejection(model = modelforABC, 
                        prior = list(c("unif",0,1),
                                     c("unif",0,0.5)), 
                        summary_stat_target = truepop,
                        nb_simul = 216601,
                        tol = 1, 
                        progress_bar = T)
ABC_rej2$computime


Tabc0.1 = proc.time()
abc0.1 <- abc(target = c(truepop),
                 param = ABC_rej2$param,
                 sumstat = ABC_rej2$stats,
                 tol = 0.03,
                 method = "rejection") ### change method here!
Tabc0.1 = proc.time()-Tabc0.1
Tabc0.1

#par(mfrow=c(2,2))

## appears in creating a raster
plot(abc0.1$unadj.values[1:5000,1],
     abc0.1$unadj.values[1:5000,2],
     xlab = "beta",
     ylab = "gamma",
     main ="S1_posterior_for_Rejection")


#################################################
# s1_seq

set.seed(123)
ABC_seq2<-ABC_sequential(method = "Lenormand",
                         model = modelforABC,
                         prior = list(c("unif",0,1),
                                      c("unif",0,0.5)),
                         nb_simul = 10000,
                         summary_stat_target = truepop, 
                         p_acc_min = 0.4, 
                         progress_bar = T)

#par(mfrow=c(3,1))

## appears in creating  a raster
plot(ABC_seq2$param[, 1],
     ABC_seq2$param[, 2],
     xlab = "beta",
     ylab = "gamma",
     main = "S1_posterior_for_sequential")

ABC_seq2$computime


