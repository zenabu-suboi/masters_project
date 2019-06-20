
library(EasyABC)

par(mfrow=c(3,2))

set.seed(234)
ABC_rej<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=targets, nb_simul=100,
                       tol=0.5, progress_bar = T)
ABC_rej$computime
plot(ABC_rej$param[, 1],ABC_rej$param[, 2], main = "nb_simul=100")


#R0<-ABC_rej$param[, 1]/ ABC_rej$param[, 2]
#plot(ABC_rej$param[, 2],R0, main = "nb_simul=100")




set.seed(234)
ABC_rej<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=targets, nb_simul=1000,
                       tol=0.5, progress_bar = T)
ABC_rej$computime
plot(ABC_rej$param[, 1],ABC_rej$param[, 2], main = "nb_simul=1000")


#R0<-ABC_rej$param[, 1]/ ABC_rej$param[, 2]
#plot(ABC_rej$param[, 1], R0, main = "nb_simul=1000")



set.seed(234)
ABC_rej<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=targets, nb_simul=10000,
                       tol=0.5, progress_bar = T)
ABC_rej$computime
plot(ABC_rej$param[, 1],ABC_rej$param[, 2], main = "nb_simul=10000")


#R0<-ABC_rej$param[, 1]/ ABC_rej$param[, 2]
#plot(ABC_rej$param[, 1], R0, main = "nb_simul=10000")




set.seed(234)
ABC_rej<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=targets, nb_simul=100000,
                       tol=0.5, progress_bar = T)
ABC_rej$computime
plot(ABC_rej$param[, 1],ABC_rej$param[, 2], main = "nb_simul=100000")


#R0<-ABC_rej$param[, 1]/ ABC_rej$param[, 2]
#plot(ABC_rej$param[, 1], R0, main = "nb_simul=100000")



set.seed(234)
ABC_rej<-ABC_rejection(model=modelforABCmcmc2, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=truepop.prev, nb_simul=1000000,
                       tol=0.5, progress_bar = T)
ABC_rej$computime
plot(ABC_rej$param[, 1], ABC_rej$param[, 2], main = "nb_simul=1000000")
