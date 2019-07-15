


library(EasyABC)
library(SimInf)

####################################################################################################
parameters=c(0.5,0.025)
u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
model <- SIR(u0, 1:75, beta=parameters[1], gamma=parameters[2]) # specifying the model
result1 <- run(model, threads = 1) 
plot(result1)
result1@U
#####################################################################################################

 modelABC= function(parameters,N=100000,inf=0.1){
  library(SimInf)
  u0 = data.frame(S=c((1-inf)*N), I=c(inf*N), R=c(0))    #specifying initial compartmental values
  model <- SIR(u0, 1:75, beta=parameters[1], gamma=parameters[2]) # specifying the model
  result <- run(model, threads = 1)     # running the model, this simulates the spread of the disease over the  specified time period
  time_50= c(rep("S",result@U[1,50]),rep("I",result@U[2,50]),rep("R",result@U[3,50])) #replicate something a number of times
  samplefrom50= sample(time_50, size=100)
  time_75= c(rep("S",result@U[1,75]),rep("I",result@U[2,75]),rep("R",result@U[3,75])) #replicate something a number of times
  samplefrom75= sample(time_75, size=100)
  target= c(summary(as.factor(samplefrom50)), summary(as.factor(samplefrom75)))
  targets=target[c(1,3)]
  return(targets)
  }


modelABC(c(0.5,0.025),100000,0.1)



debug(modelABC)
##############################################################################################################

# running the model 100 times
set.seed(123)
save_targets= matrix(c(0,0),100,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 
for(i in 1:100){
  save_targets[i,]= modelABC(c(0.5,0.025),100000,0.1)
}

save_targ = c(mean(save_targets[,1]),mean(save_targets[,2])) # the discrepancy shows in the means- mean of T75 is very small


####################################################################################################
#ABC REJECTION 

nreprej= 10000
tolp= 0.1 # retaining all results
set.seed(234)
ABC_rej<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=c(save_targ), nb_simul=nreprej,
                       tol=tolp, progress_bar = T)


ABC_rej$computime*100
plot(ABC_rej$param[, 1], ABC_rej$param[, 2], main="ABC_rejection")

nrow(ABC_rej$param)

#?EasyABC

##############################################################################################
# ABC SEQUENTIAL

nrepseq=400
#alpha=0.5 by default
set.seed(123)
ABC_seq<-ABC_sequential(method="Lenormand", model=modelABC, prior=list(c("unif",0,1), c("unif",0,0.5)), nb_simul=nrepseq,
                        summary_stat_target=c(save_targets[1,]), p_acc_min=0.4, progress_bar = T)


ABC_seq$computime

### plot the posterior obtained by ABC sequential (Lenormand)
plot(ABC_seq$param[, 1], ABC_seq$param[, 2], main="nrepseq=400,p_acc_min=0.4 ")

nrow(ABC_seq$param)



#######################################################################################
#nb_simul=400, p_acc_min=0.2

nrepseq=400
#alpha=0.5 by default
set.seed(123)
ABC_seq1<-ABC_sequential(method="Lenormand", model=modelABC, prior=list(c("unif",0,1), c("unif",0,0.5)), nb_simul=nrepseq,
                         summary_stat_target=c(save_targets[1,]), p_acc_min=0.2, progress_bar = T)
plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2], main="nrepseq=400,p_acc_min=0.2 ")

ABC_seq1$computime

nrow(ABC_seq1$param)

###########################################################################################
#????????????????????
  nrepseq=200
#alpha=0.5 by default
set.seed(123)
ABC_seq1<-ABC_sequential(method="Lenormand", model=modelABC, prior=list(c("unif",0,1), c("unif",0,0.5)), nb_simul=nrepseq,
                         summary_stat_target=c(save_targets[1,]), p_acc_min=0.5, progress_bar = T)
 plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2], main="nrepseq=200,p_acc_min=0.5")

ABC_seq1$computime

nrow(ABC_seq1$param)




 
 
 
 