
library(SimInf)
library(EasyABC)


#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R")

#################################################################################
# scenario 1 = 2 targets

set.seed(121)
#tol=100%
ABC_ref1 <- ABC_rejection(model = modelforABC, 
                            prior = list(c("unif",0.1,0.3),
                                       c("unif",0.01,0.03)), 
                           summary_stat_target = c(0.644, 0.404),
                           nb_simul = 1000000,
                           tol = 1,
                           progress_bar = T)
                           #use_seed = T)
ABC_ref1$computime


#Tabcref1 = proc.time()
abc2ref1 <- abc(target = c(0.644, 0.404),
               param = ABC_ref1$param,
               sumstat = ABC_ref1$stats,
               tol = 0.05,
               method = "rejection") ### change method here!
#Tabcref1= proc.time()-Tabcref1
#Tabcref1

saveRDS(object = abc2ref1$unadj.values , file = 'refposterior.rds')
#ref_posterior <- readRDS('refposterior.rds')


plot(abc2ref1$unadj.values[,1],abc2ref1$unadj.values[,2], xlab = "beta", ylab = "gamma",
     ylim=c(0,0.5),
     xlim=c(0,1), main ="posterior_for_abc2ref1") # retained 0.5% 0f thw 1000000 runs




#####################################################################



# scenario 2 = 3 targets

#set.seed(234)
#tol=100%
ABC_ref2 <- ABC_rejection(model = modelforABC, 
                          prior = list(c("unif",0.1,0.3),
                                       c("unif",0.01,0.03)), 
                          summary_stat_target = c(0.622, 0.371, 0.677),
                          nb_simul = 1000000,
                          tol = 1,
                          progress_bar = T,
                          use_seed = T)
ABC_rej2ref1$computime


#Tabcref1 = proc.time()
abc2ref2 <- abc(target = c(0.622, 0.371, 0.677),
                param = ABC_ref2$param,
                sumstat = ABC_ref2$stats,
                tol = 0.05,
                method = "rejection") ### change method here!
#Tabcref1= proc.time()-Tabcref1
#Tabcref1

saveRDS(object = abc2ref2$unadj.values , file = 'refposterior.rds')
#ref_posterior <- readRDS('refposterior.rds')


plot(abc2ref1$unadj.values[,1],abc2ref1$unadj.values[,2], xlab = "beta", ylab = "gamma",
     ylim=c(0,0.5),
     xlim=c(0,1), main ="posterior_for_abc2ref1") # retained 0.5% 0f thw 1000000 runs




#####################################################################
