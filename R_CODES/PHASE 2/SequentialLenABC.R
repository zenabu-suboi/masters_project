
library(EasyABC)

par(mfrow=c(2,2))
set.seed(123)
ABC_seq1<-ABC_sequential(method="Lenormand", model=modelABC, prior=list(c("unif",0,1),
                         c("unif",0,0.5)), nb_simul=100,
                         summary_stat_target=targets, p_acc_min=0.4, progress_bar = T)


ABC_seq1$computime
plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2], main ="#simul = 100")


#R0<-ABC_seq1$param[, 1]/ ABC_seq1$param[, 2]
#plot(R0,ABC_seq1$param[, 2], main = "nb_simul=100")




set.seed(123)
ABC_seq1<-ABC_sequential(method="Lenormand", model=modelABC, prior=list(c("unif",0,1),
                                                                                c("unif",0,0.5)), nb_simul=1000,
                         summary_stat_target=targets, p_acc_min=0.4, progress_bar = T)

plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2], main ="#simul = 1000")
ABC_seq1$computime


#R0<-ABC_seq1$param[, 1]/ ABC_seq1$param[, 2]
#plot(R0,ABC_seq1$param[, 2], main = "nb_simul=1000")




set.seed(123)
ABC_seq1<-ABC_sequential(method="Lenormand", model=modelforABCmcmc2, prior=list(c("unif",0,1),
                                                                                c("unif",0,0.5)), nb_simul=10000,
                         summary_stat_target=truepop.prev, p_acc_min=0.4, progress_bar = T)

R0<-ABC_seq1$param[, 1]/ ABC_seq1$param[, 2]
plot(R0,ABC_seq1$param[, 2], main = "nb_simul=10000")

plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2], main ="#simul = 10000")
ABC_seq1$computime


set.seed(123)
ABC_seq1<-ABC_sequential(method="Lenormand", model=modelforABCmcmc2, prior=list(c("unif",0,1),
                                                                                c("unif",0,0.5)), nb_simul=100000,
                         summary_stat_target=truepop.prev, p_acc_min=0.4, progress_bar = T)

R0<-ABC_seq1$param[, 1]/ ABC_seq1$param[, 2]
plot(R0,ABC_seq1$param[, 2], main = "nb_simul=10000")

plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2], main ="#simul = 100000")
ABC_seq1$computime



set.seed(123)
ABC_seq1<-ABC_sequential(method="Lenormand", model=modelforABCmcmc2, prior=list(c("unif",0,1), 
                                                                                c("unif",0,0.5)), nb_simul=1000000,
                         summary_stat_target=truepop.prev, p_acc_min=0.4, progress_bar = T)

plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2], main ="#simul = 1000000")
ABC_seq1$computime


