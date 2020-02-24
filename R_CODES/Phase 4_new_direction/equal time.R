library(SimInf)
library(EasyABC)



##########################################################################
# scenario 2 == 3 targets 

modelforABCmcmc2 = function(parameters){
  library(SimInf)
  u0= data.frame(S = c(990),
                 I = c(10), 
                 R = c(0))
  
  model <- SIR(u0, 1:75, 
               beta = parameters[1],
               gamma = parameters[2])  
  
  result <- run(model, 
                threads = 1)#, seed=sample.int(1000000000,1)) 
  
  prev <- prevalence(result, I~.)
  targ <- numeric()
  targ[1] <- prev[50,2]
  targ[2] <- prev[75,2]
  peak_prev <- max(prev[,2])
  
  return(c(targ[1],targ[2],peak_prev))
}


### try running it once, should return two population prevalence percentages
modelforABCmcmc2(c(0.2,0.02))



### set.seed for reproducability
set.seed(123)

### save the results from 10000 runs, take the means as the targets
saveres= matrix(c(0,0,0),100,3)
for(i in 1:100){
  saveres[i,]= modelforABCmcmc2(c(0.2,0.02))
}

### we call the target: truepop.prev 
truepop = c(mean(saveres[,1]), 
                mean(saveres[,2]),
            mean(saveres[,3]))
truepop


###################################### SEQ_ABC


set.seed(123) # scenario 2
ABC_seq1<-ABC_sequential(method = "Lenormand", 
                         model = modelforABCmcmc2,
                         prior = list(c("unif",0,1),
                                    c("unif",0,0.5)),
                         nb_simul = 10000,
                         summary_stat_target = truepop,
                         p_acc_min = 0.4,
                         progress_bar = T)# time = 6590.6s


par(mfrow = c(1,2))

plot(ABC_seq1$param[, 1], 
     ABC_seq1$param[, 2],
     xlab = "beta", 
     ylab = "gamma",
     main ="posterior_for_sequential",
     ylim = c(0,0.06),
     xlim = c(0,0.8))

ABC_seq1$computime



# Rejection ---------------------------------------------------------------

#################################

set.seed(234)
#tol=100%
ABC_rej<-ABC_rejection(model = modelforABCmcmc2,
                       prior = list(c("unif",0,1),
                                  c("unif",0,0.5)), 
                       summary_stat_target = truepop.prev,
                       nb_simul = 277690,
                       tol = 1,
                       progress_bar = T) # run simulations for rejection ABC
ABC_rej$computime


Tabc0.1lin = proc.time() # Scenario two
abc0.1lin <- abc(target = c(truepop.prev),
                 param = ABC_rej$param,
                 sumstat = ABC_rej$stats,
                 tol = 0.025,
                 method = "rejection") ### change method here!
Tabc0.1lin = proc.time()-Tabc0.1lin
Tabc0.1lin

?abc


#appears in creating a raster
plot(abc0.1lin$unadj.values[1:5000,1],
     abc0.1lin$unadj.values[1:5000,2],
     xlab = "beta", ylab = "gamma",
     main ="posterior_for_Rejection",
     ylim = c(0,0.06),
     xlim = c(0,0.8))


#####################################################################

 