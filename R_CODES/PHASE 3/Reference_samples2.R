


# scenario 1 = 2 targets

set.seed(234)
#tol=100%
ABC_rej2ref1<-ABC_rejection(model=modelforABCmcmc2, 
                            prior=list(c("unif",0.1,0.4),
                                       c("unif",0.01,0.03)), 
                           summary_stat_target=truepop,
                           nb_simul=500000,
                           tol=1, progress_bar = T)
ABC_rej2ref1$computime


#Tabcref1 = proc.time()
abc2ref1 <- abc(target = c(truepop),
               param = ABC_rej2ref1$param,
               sumstat = ABC_rej2ref1$stats,
               tol = 0.01,
               method = "rejection") ### change method here!
#Tabcref1= proc.time()-Tabcref1
#Tabcref1

par(mfrow=c(2,2))

plot(abc2ref1$unadj.values[,1],abc2ref1$unadj.values[,2], xlab = "beta", ylab = "gamma",
     ylim=c(0,0.5),
     xlim=c(0,1), main ="posterior_for_abc2ref1") # retained 0.5% 0f thw 1000000 runs




#####################################################################
# scenario 2


set.seed(234)
#tol=100%
ABC_rej2ref2<-ABC_rejection(model=modelforABCmcmc2, prior=list(c("unif",0.1,0.4),c("unif",0.01,0.03)), 
                           summary_stat_target=truepop.prev, nb_simul=500000,
                           tol=1, progress_bar = T)
ABC_rej2ref2$computime


#Tabcref2 = proc.time() # Scenario two
abc2ref2 <- abc(target = c(truepop.prev),
               param = ABC_rej2ref2$param,
               sumstat = ABC_rej2ref2$stats,
               tol = 0.01,
               method = "rejection") ### change method here!
#Tabcref2= proc.time()-Tabcref2
#Tabcref2

plot(abc2ref2$unadj.values[,1],abc2ref2$unadj.values[,2], xlab = "beta", ylab = "gamma",ylim=c(0,0.5),
     xlim=c(0,1), main ="posterior_for_abc2ref2") # retained 0.5% 0f thw 1000000 runs
