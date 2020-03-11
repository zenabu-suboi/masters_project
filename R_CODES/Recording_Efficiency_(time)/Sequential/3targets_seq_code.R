

library(EasyABC)
library(SimInf)

#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R") # change peakprev to TRUE in func file

################################################################################
############################################################################
# three targets

# 1. Sequential ABC

record_time_seq3 <- file("mytime_seq_3targets.txt")
open(record_time_seq3, "w")


set.seed(121)
ABC_seq3 <- ABC_sequential(method = "Lenormand",
                         model = modelforABC,
                         prior = list(c("unif",0,1),
                                      c("unif",0,0.5)),
                         nb_simul = 10000,
                         summary_stat_target = c(0.622, 0.371, 0.677), 
                         p_acc_min = 0.4, 
                         progress_bar = T)

close(record_time_seq3) ## close file connection
unlink(record_time_seq3)

##########################################################
# record times
ABC_seq3$computime

timedata3 <- read.csv("mytime_seq_3targets.txt", header = F)
dim(timedata3)
Seq3time <- ABC_seq3$computime - (sum(timedata3)/10^9)

hist(timedata3[,1]/10^9, breaks = 10000)
#class(timedata2)

#x = sum(timedata3[1:10000,1])

#########################################################


#####################################################################
saveRDS(ABC_seq3$param, file = "3targets_seq_post")
targets3_seq_post <- readRDS("3targets_seq_post")

plot(targets3_seq_post[,1], targets3_seq_post[,2])
