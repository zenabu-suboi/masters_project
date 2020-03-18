
#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/
      R_CODES/Recording_Efficiency_(time)/Rejection/")

source("my_functions.R")

################################################################################
library(EasyABC)
library(SimInf)
library(microbenchmark)
############################################################################
# two targets

# 1. Rejection ABC

#record_time_rej2 <- file("mytime_rej_2targets.txt")
#open(record_time_rej2, "w")

set.seed(121)

rej2_total_time <- microbenchmark(
ABC_rej2 <- ABC_rejection(model = modelforABC, 
                          prior = list(c("unif",0,1),
                                       c("unif",0,0.5)), 
                          summary_stat_target = c(0.644, 0.404),
                          nb_simul = 60000,
                          tol = 1, 
                          progress_bar = T,
                          verbose = T, 
                         # use_seed = T,
                          #seed_count = 121
                         ), times = 1)
                          #use_seed = T)
rej2_total_time$time

#close(record_time_rej2) ## close file connection
#unlink(record_time_rej2)

########################################################
# save the prior parameter combinations chosen

saveRDS(ABC_rej2$param, file = "selected_prior2")
rej2_prior_choice <- readRDS("selected_prior2")

plot(rej2_prior_choice[,1], rej2_prior_choice[,2])

########################################################
# record times
ABC_rej2$computime # total time
timedata_rej2 <- read.csv("mytime_rej_2targets.txt", header = F)


#dim(timedata_rej2)
Rej2time <- ABC_rej2$computime - (sum(timedata_rej2)/10^9)
# algorithm time = total time - model runtime

hist(timedata_rej2[,1]/10^9, breaks = 10000, xlim = c(0,0.15))
######################################################
# get poserior

Tabc0.1 = proc.time()
abc0.1 <- abc(target = c(0.644, 0.404),
              param = ABC_rej2$param,
              sumstat = ABC_rej2$stats,
              tol = 0.09,
              method = "rejection") ### change method here!
Tabc0.1 = proc.time()-Tabc0.1
Tabc0.1

#####################################################
# save posterior

saveRDS(abc0.1$unadj.values[1:5000,], file = "2targets_rej_post")
targets2_rej_post <- readRDS("2targets_rej_post")


plot(targets2_rej_post[,1], targets2_rej_post[,2])
