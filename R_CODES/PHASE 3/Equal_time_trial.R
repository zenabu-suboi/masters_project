
library(EasyABC)
library(SimInf)

### create function to use in ABC_mcmc
modelforABCmcmc2= function(parameters){
  library(SimInf)
  u0= data.frame(S=c(990), I=c(10), R=c(0))
  
  model <- SIR(u0, 1:75, beta= parameters[1], gamma=parameters[2])
  result <- run(model, threads = 1)#, seed=sample.int(1000000000,1)) 
  prev <- prevalence(result, I~.)
  targ<- numeric()
  targ[1] <- prev[50,2]
  targ[2] <- prev[75,2]
  peak_prev <- max(prev[,2])
  
  # pop[1]=result@U[2,30]
  # pop[2]=result@U[2,60]
  return(c(targ[1],targ[2],peak_prev))
}


### try running it once, should return two population prevalence percentages
modelforABCmcmc2(c(0.2,0.02))



### set.seed for reproducability
set.seed(123)

### save the results from 10000 runs, take the means as the targets
saveres= matrix(c(0,0,0),100,3)
for(i in 1:100){
  saveres[i,]= modelforABCmcmc2(c(0.2,0.02))
}

### we call the target: truepop.prev 
truepop.prev= c(mean(saveres[,1]), mean(saveres[,2]),mean(saveres[,3]))
truepop.prev


###################################### SEQ_ABC
set.seed(123)
ABC_seq1<-ABC_sequential(method="Lenormand", model=modelforABCmcmc2, prior=list(c("unif",0,1),
                                        c("unif",0,0.5)), nb_simul=10000,
                         summary_stat_target=truepop.prev, p_acc_min=0.4, progress_bar = T)

par(mfrow=c(3,1))
R0<-ABC_seq1$param[, 1]/ ABC_seq1$param[, 2]
plot(R0,ABC_seq1$param[, 2], main = "nb_simul=10000 + peak prev")
plot(ABC_seq1$param[, 2],R0, main = "nb_simul=10000 + peak prev")



plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2], main ="#simul = 10000 + peak prev")
ABC_seq1$computime



# Rejection ---------------------------------------------------------------


set.seed(234)
ABC_rej<-ABC_rejection(model=modelforABCmcmc2, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=truepop.prev, nb_simul=10000,
                       tol=0.5, progress_bar = T)
ABC_rej$computime
R0<-ABC_rej$param[, 1]/ ABC_rej$param[, 2]
plot(R0,ABC_rej$param[, 2], main = "nb_simul=10000+ peak prev")
plot(ABC_rej$param[, 2],R0, main = "nb_simul=10000+ peak prev")
plot(ABC_rej$param[, 1], ABC_rej$param[, 2], main ="#simul = 10000+ peak prev")


head(cbind(ABC_rej$param[, 1], ABC_rej$param[, 2],R0))
par(mfrow=c(2,2))
hist(ABC_rej$param[, 1], xlab = "Parameter_beta_values", main = "Histogram of ABC_rej$param_beta")
hist(ABC_rej$param[, 2], xlab = "Parameter_gamma_values", main = "Histogram of ABC_rej$param_gamma")
hist(ABC_seq1$param[, 1], xlab = "Parameter_beta_values", main = "Histogram of ABC_seq$param_beta")
hist(ABC_seq1$param[, 2], xlab = "Parameter_gamma_values", main = "Histogram of ABC_seq$param_gamma")

