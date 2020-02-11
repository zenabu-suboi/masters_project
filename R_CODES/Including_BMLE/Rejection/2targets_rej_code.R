
#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R")

################################################################################
library(EasyABC)
library(SimInf)

############################################################################
# two targets

# 1. Rejection ABC


ABC_rej2 <- ABC_rejection(model = modelforABC, 
                          prior = list(c("unif",0,1),
                                       c("unif",0,0.5)), 
                          summary_stat_target = targets2(c(0.2, 0.02)),
                          nb_simul = 216601,
                          tol = 1, 
                          progress_bar = T,
                          use_seed = T)

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


saveRDS(abc0.1$unadj.values, file = "2targets_rej_post")
