

#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R") # change peakprev to TRUE in func file

################################################################################
library(EasyABC)
library(SimInf)

############################################################################
# two targets

# 1. Rejection ABC

record_time_rej3 <- file("mytime_rej_3targets.txt")
open(record_time_rej3, "w")

set.seed(121)
ABC_rej3 <- ABC_rejection(model = modelforABC, 
                          prior = list(c("unif",0,1),
                                       c("unif",0,0.5)), 
                          summary_stat_target = c(0.622, 0.371, 0.677),
                          nb_simul = 75000,
                          tol = 1, 
                          progress_bar = T)
                         # use_seed = T)

close(record_time_rej3) ## close file connection
unlink(record_time_rej3) ## unlink connection

####################################################################

# record times
ABC_rej3$computime # total time
timedata_rej3 <- read.csv("mytime_rej_3targets.txt", header = F)


#dim(timedata_rej2)
Rej3time <- ABC_rej3$computime - (sum(timedata_rej3)/10^9)
# algorithm time = total time - model runtime

#Histogram of model runtimes
hist(timedata_rej3[,1]/10^9, breaks = 10000, xlim = c(0,0.15))

## Pretty plot:
library(ggplot2)
if (requireNamespace("ggplot2")) {
  ggplot2::autoplot(timedata_rej3[,1])
}

boxplot(timedata_rej3[,1]/10^9)
####################################################################

Tabc0.1 = proc.time()
abcrej <- abc(target = c(0.622, 0.371, 0.677),
              param = ABC_rej3$param,
              sumstat = ABC_rej3$stats,
              tol = 0.07,
              method = "rejection") ### change method here!
Tabc0.1 = proc.time()-Tabc0.1
Tabc0.1

##################################################################

saveRDS(abcrej$unadj.values[1:5000,], file = "3targets_rej_post")
targets3_rej_post <- readRDS("3targets_rej_post")

plot(targets3_rej_post[,1], targets3_rej_post[,2])
