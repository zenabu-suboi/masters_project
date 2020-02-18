
#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R")

################################################################################
library(EasyABC)
library(SimInf)

############################################################################



# scenario 1 = 2 targets

set.seed(121)
#tol=100%
ABC_rej2ref1<-ABC_rejection(model=modelforABC, 
                            prior=list(c("unif",0.1,0.4),
                                       c("unif",0.01,0.03)), 
                           summary_stat_target = targets3(c(0.2, 0.02)),
                           nb_simul=500000,
                           tol=1,
                           progress_bar = T)
ABC_rej2ref1$computime


#Tabcref1 = proc.time()
abc2ref1 <- abc(target = targets3(c(0.2, 0.02)),
               param = ABC_rej2ref1$param,
               sumstat = ABC_rej2ref1$stats,
               tol = 0.01,
               method = "rejection") ### change method here!
#Tabcref1= proc.time()-Tabcref1
#Tabcref1

saveRDS(object = abc2ref1$unadj.values , file = 'targets3_refposterior.rds')
ref3_posterior <- readRDS('targets3_refposterior.rds')


plot(ref3_posterior[,1],ref3_posterior[,2], xlab = "beta", ylab = "gamma",
     ylim=c(0,0.5),
     xlim=c(0,1), main ="posterior_for_abc2ref1") # retained 0.5% 0f thw 1000000 runs




#####################################################################
