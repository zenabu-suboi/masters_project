
library(SimInf)
library(EasyABC)


u0= data.frame(S=c(990), I=c(10), R=c(0))

model <- SIR(u0, 1:75, beta= 0.2, gamma=0.02)
result <- run(model, threads = 1)#
plot(result)
prev <- prevalence(result, I~.)

result@U[2,]

### create function to use in ABC_mcmc
# scenario 1

modelforABC= function(parameters){
  library(SimInf)
  u0= data.frame(S=c(990), I=c(10), R=c(0))
  
  model <- SIR(u0, 1:75, beta= parameters[1],
               gamma=parameters[2])
  result <- run(model, threads = 1)#, seed=sample.int(1000000000,1)) 
  prev <- prevalence(result, I~.)
  targ<- numeric()
  targ[1] <- prev[50,2]
  targ[2] <- prev[75,2]
 # peak_prev <- max(prev[,2])
  
  # pop[1]=result@U[2,30]
  # pop[2]=result@U[2,60]
  return(c(targ[1],targ[2]))
}


### try running it once, should return two population prevalence percentages
modelforABC(c(0.2,0.02))



### set.seed for reproducability
set.seed(123)

### save the results from 10000 runs, take the means as the targets
saveres= matrix(c(0,0),100,2)
for(i in 1:100){
  saveres[i,]= modelforABC(c(0.2,0.02))
}
### we call the target: truepop.prev 
truepop= c(mean(saveres[,1]), mean(saveres[,2]))
truepop

########################
# scenario 1 == 2 targets

set.seed(234)
#tol=100%
ABC_rej2<-ABC_rejection(model=modelforABC, 
                        prior=list(c("unif",0,1),
                        c("unif",0,0.5)), 
                        summary_stat_target=truepop,
                        nb_simul=216601,
                        tol=1, progress_bar = T)
ABC_rej2$computime


Tabc0.1 = proc.time()
abc0.1 <- abc(target = c(truepop),
                 param = ABC_rej2$param,
                 sumstat = ABC_rej2$stats,
                 tol = 0.03,
                 method = "rejection") ### change method here!
Tabc0.1= proc.time()-Tabc0.1
Tabc0.1

par(mfrow=c(2,2))

plot(abc0.1$unadj.values[,1],
     abc0.1$unadj.values[,2],
     xlab = "beta", ylab = "gamma",
     main ="#params = 5066")

plot(abc0.1$unadj.values[1:5000,1],
     abc0.1$unadj.values[1:5000,2],
     xlab = "beta", ylab = "gamma",
     main ="#params = 5000")


## appears in creating a raster
plot(abc0.1$unadj.values[1:5000,1]
     ,abc0.1$unadj.values[1:5000,2],
     xlab = "beta", ylab = "gamma",
     main ="S1_posterior_for_Rejection")


#################################################
# s1_seq

set.seed(123)
ABC_seq2<-ABC_sequential(method="Lenormand",
                         model=modelforABC,
                         prior=list(c("unif",0,1),
                         c("unif",0,0.5)),
                         nb_simul=10000,
                         summary_stat_target=truepop, 
                         p_acc_min=0.4, progress_bar = T)

#par(mfrow=c(3,1))

## appears in creating  a raster
plot(ABC_seq2$param[, 1], ABC_seq2$param[, 2],
     xlab = "beta", ylab = "gamma",
     main ="S1_posterior_for_sequential")

ABC_seq2$computime


##########################################################################
# scenario 2 == 3 targets 

modelforABCmcmc2= function(parameters){
  library(SimInf)
  u0= data.frame(S=c(990), I=c(10), R=c(0))
  
  model <- SIR(u0, 1:75, beta= parameters[1],
               gamma=parameters[2])
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
truepop.prev= c(mean(saveres[,1]), 
                mean(saveres[,2]),
                mean(saveres[,3]))
truepop.prev


###################################### SEQ_ABC


set.seed(123) # scenario 2
ABC_seq1<-ABC_sequential(method="Lenormand", 
                         model=modelforABCmcmc2,
                         prior=list(c("unif",0,1),
                         c("unif",0,0.5)), nb_simul=10000,
                         summary_stat_target=truepop.prev,
                         p_acc_min=0.4, progress_bar = T)

#par(mfrow=c(3,1))
#R0<-ABC_seq1$param[, 1]/ ABC_seq1$param[, 2]
#plot(R0,ABC_seq1$param[, 2], main = "nb_simul=10000 + peak prev")
#plot(ABC_seq1$param[, 2],R0, main = "nb_simul=10000 + peak prev")



plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2],
     xlab = "beta", ylab = "gamma",
     main ="posterior_for_sequential")

ABC_seq1$computime



# Rejection ---------------------------------------------------------------


set.seed(234)
ABC_rej<-ABC_rejection(model=modelforABC, 
                       prior=list(c("unif",0,1),
                       c("unif",0,0.5)), 
                       summary_stat_target=truepop,
                       nb_simul=10000,
                       tol=0.5, progress_bar = T)
ABC_rej$computime
# R0<-ABC_rej$param[, 1]/ ABC_rej$param[, 2]
# plot(R0,ABC_rej$param[, 2], main = "nb_simul=10000+ peak prev")
# plot(ABC_rej$param[, 2],R0, main = "nb_simul=10000+ peak prev")
# plot(ABC_rej$param[, 1], ABC_rej$param[, 2] )
# 
# head(cbind(ABC_rej$param[, 1], ABC_rej$param[, 2],R0))
# par(mfrow=c(2,2))
# hist(ABC_rej$param[, 1], xlab = "Parameter_beta_values", main = "Histogram of ABC_rej$param_beta")
# hist(ABC_rej$param[, 2], xlab = "Parameter_gamma_values", main = "Histogram of ABC_rej$param_gamma")
# hist(ABC_seq1$param[, 1], xlab = "Parameter_beta_values", main = "Histogram of ABC_seq$param_beta")
# hist(ABC_seq1$param[, 2], xlab = "Parameter_gamma_values", main = "Histogram of ABC_seq$param_gamma")

#################################

set.seed(234)
#tol=100%
ABC_rej<-ABC_rejection(model=modelforABCmcmc2,
                       prior=list(c("unif",0,1),
                       c("unif",0,0.5)), 
                       summary_stat_target=truepop.prev,
                       nb_simul=277690,
                       tol=1, progress_bar = T)
ABC_rej$computime


Tabc0.1lin = proc.time() # Scenario two
abc0.1lin <- abc(target = c(truepop.prev),
                 param = ABC_rej$param,
                 sumstat = ABC_rej$stats,
                 tol = 0.025,
                 method = "rejection") ### change method here!
Tabc0.1lin= proc.time()-Tabc0.1lin
Tabc0.1lin

?abc


plot(abc0.1lin$unadj.values[,1],
     abc0.1lin$unadj.values[,2],
     xlab = "beta", ylab = "gamma",
     main ="#params = 5096")

plot(abc0.1lin$unadj.values[1:5000,1],
     abc0.1lin$unadj.values[1:5000,2],
     xlab = "beta", ylab = "gamma",
     main ="#params = 5000")


#appears in creating a raster
plot(abc0.1lin$unadj.values[1:5000,1],
     abc0.1lin$unadj.values[1:5000,2],
     xlab = "beta", ylab = "gamma",
     main ="posterior_for_Rejection")

#####################################################################

write.csv(abc0.1lin[["unadj.values"]], file = "mydata", 
            append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = F,
            col.names = TRUE)
mydata
