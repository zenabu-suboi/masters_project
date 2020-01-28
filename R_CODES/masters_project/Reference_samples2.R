

source("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project/my_functions")

# scenario 1 = 2 targets

set.seed(234)
#tol=100%
ABC_rej2ref1<-ABC_rejection(model=modelforABC, 
                            prior=list(c("unif",0.1,0.4),
                                       c("unif",0.01,0.03)), 
                           summary_stat_target=truepop,
                           nb_simul=1000000,
                           tol=1, progress_bar = T)
ABC_rej2ref1$computime


#Tabcref1 = proc.time()
abc2ref1 <- abc(target = c(truepop),
               param = ABC_rej2ref1$param,
               sumstat = ABC_rej2ref1$stats,
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
