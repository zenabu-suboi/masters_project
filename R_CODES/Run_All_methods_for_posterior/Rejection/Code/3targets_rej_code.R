

#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R") # change peakprev to TRUE in func file

################################################################################
library(EasyABC)
library(SimInf)

############################################################################
# two targets

# 1. Rejection ABC

set.seed(121)
ABC_rej <- ABC_rejection(model = modelforABC, 
                          prior = list(c("unif",0,1),
                                       c("unif",0,0.5)), 
                          summary_stat_target = targets3(c(0.2, 0.02)),
                          nb_simul = 277690,
                          tol = 1, 
                          progress_bar = T)
                         # use_seed = T)

# + save output to file (filename?)

ABC_rej$computime


Tabc0.1 = proc.time()
abcrej <- abc(target = targets3(c(0.2, 0.02)),
              param = ABC_rej$param,
              sumstat = ABC_rej$stats,
              tol = 0.025,
              method = "rejection") ### change method here!
Tabc0.1 = proc.time()-Tabc0.1
Tabc0.1


saveRDS(abcrej$unadj.values[1:5000,], file = "3targets_rej_post")
targets3_rej_post <- readRDS("3targets_rej_post")

plot(targets3_rej_post[,1], targets3_rej_post[,2])
