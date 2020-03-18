

library(EasyABC)
library(SimInf)

#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R") # change peakprev to TRUE in func file

################################################################################
############################################################################
# three targets

# 1. Sequential ABC

#record_time_seq3 <- file("mytime_seq_3targets.txt")
#open(record_time_seq3, "w")


set.seed(121)

seq3_total_time <- microbenchmark(
ABC_seq3 <- ABC_sequential(method = "Lenormand",
                         model = modelforABC,
                         prior = list(c("unif",0,1),
                                      c("unif",0,0.5)),
                         nb_simul = 10000,
                         summary_stat_target = c(0.622, 0.371, 0.677), 
                         p_acc_min = 0.4, 
                         progress_bar = T,
                         verbose = T),times = 1)

seq3_total_time$time # total time

#close(record_time_seq3) ## close file connection
#unlink(record_time_seq3)

##########################################################
# record times
ABC_seq3$computime

timedata3 <- read.csv("mytime_seq_3targets.txt", header = F)

Seq3time <- ABC_seq3$computime - (sum(timedata3)/10^9)

hist(timedata3[,1]/10^9, breaks = 10000, xlim = c(0,0.15))


#########################################################
##########################################################
# records sequential time in steps

s1 <- sum(timedata3[1:10000,1])/10^9
s2 <- sum(timedata3[10001:15000,1])/10^9
s3 <- sum(timedata3[15001:20000,1])/10^9
s4 <- sum(timedata3[20001:25000,1])/10^9
s5 <- sum(timedata3[25001:30000,1])/10^9
s6 <- sum(timedata3[30001:35000,1])/10^9
s7 <- sum(timedata3[35001:40000,1])/10^9
s8 <- sum(timedata3[40001:45000,1])/10^9
s9 <- sum(timedata3[45001:50000,1])/10^9
s10 <- sum(timedata3[50001:55000,1])/10^9
s11 <- sum(timedata3[55001:60000,1])/10^9
s12 <- sum(timedata3[60001:65000,1])/10^9
s13 <- sum(timedata3[65001:70000,1])/10^9
s14 <- sum(timedata3[70001:75000,1])/10^9

#####################################################################
saveRDS(ABC_seq3$param, file = "3targets_seq_post")
targets3_seq_post <- readRDS("3targets_seq_post")

plot(targets3_seq_post[,1], targets3_seq_post[,2])
