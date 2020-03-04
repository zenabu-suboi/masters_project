

library(EasyABC)
library(SimInf)

#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R") # change peakprev to TRUE in func file

################################################################################
############################################################################
# three targets

# 1. Sequential ABC

set.seed(121)
ABC_seq3 <- ABC_sequential(method = "Lenormand",
                         model = modelforABC,
                         prior = list(c("unif",0,1),
                                      c("unif",0,0.5)),
                         nb_simul = 10000,
                         summary_stat_target = c(0.60848, 0.38441, 0.67556), 
                         p_acc_min = 0.4, 
                         progress_bar = T)

saveRDS(ABC_seq3$param, file = "3targets_seq_post")
targets3_seq_post <- readRDS("3targets_seq_post")

#plot(targets3_seq_post[,1], targets3_seq_post[,2])
