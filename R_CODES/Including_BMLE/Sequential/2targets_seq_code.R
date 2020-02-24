library(EasyABC)
library(SimInf)

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
                         summary_stat_target = c( 0.60848, 0.38441), 
                         p_acc_min = 0.4, 
                         progress_bar = T)

saveRDS(ABC_seq2$param, file = "2targets_seq_post")
targets2_seq_post <- readRDS("2targets_seq_post")

plot(targets2_seq_post[,1], targets2_seq_post[,2])
