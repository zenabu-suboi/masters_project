library(EasyABC)
library(SimInf)
library(microbenchmark)

#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/
      R_CODES/Recording_Efficiency_(time)/Sequential/")

source("my_functions.R")

################################################################################
############################################################################
# two targets

# 1. Sequential ABC

record_time_seq2 <- file("mytime_seq_2targets.txt")
open(record_time_seq2, "w")

set.seed(121)
ABC_seq2 <- ABC_sequential(method = "Lenormand",
                         model = modelforABC,
                         prior = list(c("unif",0,1),
                                      c("unif",0,0.5)),
                         nb_simul = 10000,
                         summary_stat_target = c(0.644, 0.404), 
                         p_acc_min = 0.4, 
                         progress_bar = T)

close(record_time_seq2) ## close file connection
unlink(record_time_seq2)

##########################################################
# record times
ABC_seq2$computime

timedata2 <- read.csv("mytime_seq_2targets.txt", header = F)
dim(timedata2)
Seq2time <- ABC_seq2$computime - (sum(timedata2)/10^9)

hist(timedata2[,1]/10^9, breaks = 1000)
#class(timedata2)

x = sum(timedata2[1:10000,1])
head(timedata2)

#########################################################

saveRDS(ABC_seq2$param, file = "2targets_seq_post")
targets2_seq_post <- readRDS("2targets_seq_post")

plot(targets2_seq_post[,1], targets2_seq_post[,2])
