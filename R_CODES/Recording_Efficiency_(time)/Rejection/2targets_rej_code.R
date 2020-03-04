
#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/
      R_CODES/Recording_Efficiency_(time)/Rejection/")

source("my_functions.R")

################################################################################
library(EasyABC)
library(SimInf)

############################################################################
# two targets

# 1. Rejection ABC

record_time_rej2 <- file("mytime_rej_2targets.txt")
open(record_time_rej2, "w")

set.seed(121)
ABC_rej2 <- ABC_rejection(model = modelforABC, 
                          prior = list(c("unif",0,1),
                                       c("unif",0,0.5)), 
                          summary_stat_target = c(0.60848, 0.38441),
                          nb_simul = 60000,
                          tol = 1, 
                          progress_bar = T)
                          #use_seed = T)

close(record_time_rej2) ## close file connection
unlink(record_time_rej2)

########################################################
# record times
ABC_rej2$computime # total time
timedata_rej2 <- read.csv("mytime_rej_2targets.txt")
dim(timedata_rej2)
Rej2time <- ABC_rej2$computime - sum(timedata_rej2)
# algorithm time = total time - model runtime


######################################################

Tabc0.1 = proc.time()
abc0.1 <- abc(target = targets2(c(0.2, 0.02)),
              param = ABC_rej2$param,
              sumstat = ABC_rej2$stats,
              tol = 0.025,
              method = "rejection") ### change method here!
Tabc0.1 = proc.time()-Tabc0.1
Tabc0.1


saveRDS(abc0.1$unadj.values[1:5000,], file = "2targets_rej_post")
targets2_rej_post <- readRDS("2targets_rej_post")

plot(targets2_rej_post[,1], targets2_rej_post[,2])
