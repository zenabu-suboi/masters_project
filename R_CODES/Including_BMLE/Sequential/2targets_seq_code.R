

#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R")

################################################################################
############################################################################
# two targets

# 1. Sequential ABC

set.seed(121)
ABC_seq2<-ABC_sequential(method = "Lenormand",
                         model = modelforABC,
                         prior = list(c("unif",0,1),
                                      c("unif",0,0.5)),
                         nb_simul = 10000,
                         summary_stat_target = targets2(c(0.2, 0.02)), 
                         p_acc_min = 0.4, 
                         progress_bar = T)
