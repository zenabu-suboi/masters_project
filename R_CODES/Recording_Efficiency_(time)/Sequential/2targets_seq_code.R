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

Seq2time <- ABC_seq2$computime - (sum(timedata2)/10^9)

hist(timedata2[,1]/10^9, breaks = 10000, xlim = c(0,0.15))

x = sum(timedata2[1:10000,1])


##########################################################
# records sequential time in steps

s1 <- sum(timedata2[1:10000,1])/10^9
s2 <- sum(timedata2[10001:15000,1])/10^9
s3 <- sum(timedata2[15001:20000,1])/10^9
s4 <- sum(timedata2[20001:25000,1])/10^9
s5 <- sum(timedata2[25001:30000,1])/10^9
s6 <- sum(timedata2[30001:35000,1])/10^9
s7 <- sum(timedata2[35001:40000,1])/10^9
s8 <- sum(timedata2[40001:45000,1])/10^9
s9 <- sum(timedata2[45001:50000,1])/10^9
s10 <- sum(timedata2[50001:55000,1])/10^9
s11 <- sum(timedata2[55001:60000,1])/10^9

#########################################################

saveRDS(ABC_seq2$param, file = "2targets_seq_post")
targets2_seq_post <- readRDS("2targets_seq_post")

plot(targets2_seq_post[,1], targets2_seq_post[,2])
