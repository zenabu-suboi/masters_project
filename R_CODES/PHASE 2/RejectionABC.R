
library(EasyABC)

nreprej= 1000
tolp= 1  


set.seed(234)
ABC_rej<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=targets, nb_simul=nreprej,
                       tol=tolp, progress_bar = T)

ABC_rej$computime

### plot the posterior obtained by ABC rejection
plot(ABC_rej$param[, 1], ABC_rej$param[, 2])
