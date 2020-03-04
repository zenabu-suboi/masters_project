library(EasyABC)
library(SimInf)
library(tictoc)

#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/
      R_CODES/Recording_Efficiency_(time)/Sequential/")

source("my_functions.R")

################################################################################
############################################################################
# two targets

# 1. Sequential ABC

record_time-seq2 <- file("mytime_seq_2targets.txt")
open(record_time_seq2, "w")

set.seed(121)
ABC_seq2<-ABC_sequential(method = "Lenormand",
                         model = modelforABC,
                         prior = list(c("unif",0,1),
                                      c("unif",0,0.5)),
                         nb_simul = 10000,
                         summary_stat_target = c(0.60848, 0.38441), 
                         p_acc_min = 0.4, 
                         progress_bar = T)

close(record_time_seq2) ## close file connection
unlink(record_time_seq2)

ABC_seq2$computime

saveRDS(ABC_seq2$param, file = "2targets_seq_post")
targets2_seq_post <- readRDS("2targets_seq_post")

plot(targets2_seq_post[,1], targets2_seq_post[,2])
