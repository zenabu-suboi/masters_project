
getwd()

library(SimInf)
library(EasyABC)

#####################################################################
source("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project/my_functions.R")
source("my_functions.R")



### set.seed for reproducibility
set.seed(123)

### save the results from 10000 runs, take the means as the targets
targetStats = matrix(c(0,0),100,2)
for(i in 1:100){
  targetStats[i,] = modelforABC(c(0.2,0.02))
}
### we call the target: truepop.prev 
meanTargetStats = c(mean(targetStats[,1]),
            mean(targetStats[,2]))
meanTargetStats


######################################################################
# scenario 1 == 2 targets

set.seed(234)
#tol=100%

## Specify number of simulations (from command line)

ABC_rej2 <- ABC_rejection(model = modelforABC, 
                        prior = list(c("unif",0,1),
                                     c("unif",0,0.5)), 
                        summary_stat_target = meanTargetStats,
                        nb_simul = 100,
                        tol = 1, 
                        progress_bar = T)

 # + save output to file (filename?)

ABC_rej2$computime
ABC_rej2$nsim

Tabc0.1 = proc.time()
abc0.1 <- abc(target = c(meanTargetStats),
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
                         nb_simul = 100,
                         summary_stat_target = meanTargetStats, 
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


