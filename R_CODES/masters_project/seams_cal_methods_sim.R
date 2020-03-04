


library(SimInf)
library(EasyABC)

#####################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R")

######################################################################
# scenario 1 == 2 targets

set.seed(234)
#tol=100%

## Specify number of simulations (from command line)

zztime <- file("mytime.txt")
open(zztime, "w")

ABC_rej2 <- ABC_rejection(model = modelforABC, 
                          prior = list(c("unif",0,1),
                                       c("unif",0,0.5)), 
                          summary_stat_target = targets(c(0.2, 0.02)),
                          nb_simul = 10,
                          tol = 1, 
                          progress_bar = T,
                          use_seed = T)

close(zztime) ## close file connection
unlink(zztime)

# + save output to file (filename?)

ABC_rej2$computime


Tabc0.1 = proc.time()
abc0.1 <- abc(target = c(meanTargetStats),
              param = ABC_rej2$param,
              sumstat = ABC_rej2$stats,
              tol = 0.5,
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

#open file handle timerecord

record_time <- file("mytime_seq_2targets.txt")
open(record_time, "w")

ABC_seq2<-ABC_sequential(method = "Lenormand",
                         model = modelforABC,
                         prior = list(c("unif",0,1),
                                      c("unif",0,0.5)),
                         nb_simul = 10,
                         summary_stat_target = c(0.60848, 0.38441), 
                         p_acc_min = 0.4, 
                         progress_bar = T,
                         verbose=T)

close(record_time) ## close file connection
unlink(record_time)


#par(mfrow=c(3,1))

## appears in creating  a raster
plot(ABC_seq2$param[, 1],
     ABC_seq2$param[, 2],
     xlab = "beta",
     ylab = "gamma",
     main = "S1_posterior_for_sequential")

ABC_seq2$computime


