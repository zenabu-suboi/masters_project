

nrepseq=100000
#alpha=0.5 by default
set.seed(123)
ABC_seq1<-ABC_sequential(method="Lenormand", model=modelforABCmcmc2, prior=list(c("unif",0,1), c("unif",0,0.5)), nb_simul=nrepseq,
                         summary_stat_target=truepop.prev, p_acc_min=0.4, progress_bar = T)

par(mfrow=c(1,1))
plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2], main ="#simul = 10000")

ABC_seq1$computime

#nrow(ABC_seq1$param)

nrepseq=100000
#alpha=0.5 by default
set.seed(123)
ABC_seq1<-ABC_sequential(method="Lenormand", model=modelforABCmcmc2, prior=list(c("unif",0,1), c("unif",0,0.5)), nb_simul=nrepseq,
                         summary_stat_target=truepop.prev, p_acc_min=0.4, progress_bar = T)

par(mfrow=c(1,1))
plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2], main ="#simul = 10000")
 
ABC_seq1$computime
