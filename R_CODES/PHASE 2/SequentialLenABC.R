
library(EasyABC)

nrepseq=400
#alpha=0.5 by default
set.seed(123)
ABC_seq1<-ABC_sequential(method="Lenormand", model=modelABC,
                         prior=list(c("unif",0,1), c("unif",0,0.5)),
                         nb_simul=nrepseq, summary_stat_target= targets,
                         p_acc_min=0.2, progress_bar = T)

plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2])

ABC_seq1$computime
