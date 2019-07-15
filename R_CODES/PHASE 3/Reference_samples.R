set.seed(234)
#tol=100%
ABC_rejref1<-ABC_rejection(model=modelforABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                        summary_stat_target=truepop, nb_simul=1000000,
                        tol=1, progress_bar = T)
ABC_rejref1$computime


Tabcref1 = proc.time()
abcref1 <- abc(target = c(truepop),
              param = ABC_rejref1$param,
              sumstat = ABC_rejref1$stats,
              tol = 0.005,
              method = "rejection") ### change method here!
Tabcref1= proc.time()-Tabcref1
Tabcref1

par(mfrow=c(2,2))

plot(abcref1$unadj.values[,1],abcref1$unadj.values[,2], xlab = "beta", ylab = "gamma",
     ylim=c(0,0.5),
     xlim=c(0,1), main ="posterior_for_abcref1.0.5p") # retained 0.5% 0f thw 1000000 runs

#########

Tabcref1.0.1p = proc.time()
abcref1.0.1p <- abc(target = c(truepop),
               param = ABC_rejref1$param,
               sumstat = ABC_rejref1$stats,
               tol = 0.001,
               method = "rejection") ### change method here!
Tabcref1.0.1p= proc.time()-Tabcref1.0.1p
Tabcref1

par(mfrow=c(1,2))

plot(abcref1.0.1p$unadj.values[,1],abcref1.0.1p$unadj.values[,2], xlab = "beta", ylab = "gamma",
     ylim=c(0,0.5),
     xlim=c(0,1), main ="posterior_for_abcref10.1p") # retained 0.5% 0f thw 1000000 runs

#################################################################
set.seed(234)
#tol=100%
ABC_rejref2<-ABC_rejection(model=modelforABCmcmc2, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=truepop.prev, nb_simul=1000000,
                       tol=1, progress_bar = T)
ABC_rejref2$computime


Tabcref2 = proc.time() # Scenario two
abcref2 <- abc(target = c(truepop.prev),
                 param = ABC_rejref2$param,
                 sumstat = ABC_rejref2$stats,
                 tol = 0.005,
                 method = "rejection") ### change method here!
Tabcref2= proc.time()-Tabcref2
Tabcref2

plot(abcref2$unadj.values[,1],abcref2$unadj.values[,2], xlab = "beta", ylab = "gamma",ylim=c(0,0.5),
     xlim=c(0,1.0), main ="posterior_for_abcref2.0.5p") # retained 0.5% 0f thw 1000000 runs


Tabcref2.0.1p = proc.time() # Scenario two
abcref2.0.1p <- abc(target = c(truepop.prev),
               param = ABC_rejref2$param,
               sumstat = ABC_rejref2$stats,
               tol = 0.001,
               method = "rejection") ### change method here!
Tabcref2.0.1p= proc.time()-Tabcref2.0.1p
Tabcref2

plot(abcref2.0.1p$unadj.values[,1],abcref2.0.1p$unadj.values[,2], xlab = "beta", ylab = "gamma",ylim=c(0,0.5),
     xlim=c(0,1.0), main ="posterior_for_abcref2.0.1p") # retained 0.5% 0f thw 1000000 runs
