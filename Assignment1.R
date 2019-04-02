
# TOL = 0.1


library(EasyABC)
library(SimInf)

#####################################################################################################

parameters=c(0.2,0.02)
u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
model <- SIR(u0, 1:75, beta=parameters[1], gamma=parameters[2]) # specifying the model
result1 <- run(model, threads = 1) 
plot(result1)


##########################################################################################

modelABC= function(parameter,N=100000,inf=0.1){
  u0 = data.frame(S=c((1-inf)*N), I=c(inf*N), R=c(0))    #specifying initial compartmental values
  model <- SIR(u0, 1:75, beta=parameter[1], gamma=parameter[2]) # specifying the model
  result <- run(model, threads = 1)     # running the model
  time_50<- c(rep("S",result@U[1,50]),rep("I",result@U[2,50]),rep("R",result@U[3,50])) #replicate something a number of times
  samplefrom50<- sample(time_50, size=100)
  time_75<- c(rep("S",result@U[1,75]),rep("I",result@U[2,75]),rep("R",result@U[3,75])) #replicate something a number of times
  samplefrom75<- sample(time_75, size=100)
  targetS<- c(summary(as.factor(samplefrom50)), summary(as.factor(samplefrom75)))
  output <- targetS[names(targetS)=="I"]
  model_output<-matrix(output,1,2)
  colnames(model_output) <- c("I","I")
  return(model_output)
}

modelABC(c(0.2,0.02),100000,0.1)


########################################################################################################
# running the model several times and storing the model outputs at the two time points in a matrix 

set.seed(123)
save_targets1= matrix(c(0,0),100,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 
#colnames(save_targets1) <- c("I","I")
for(i in 1:100){
  save_targets1[i,]= modelABC(c(0.2,0.02),100000,0.1)
}

# obtaining the targets as the mean number of infected at the time points
save_targ1 = c(ceiling(mean(save_targets1[,1])),ceiling(mean(save_targets1[,2])))
#sv<-matrix(save_targ1,1,2)
#colnames(sv) <- c("I","I")
#rownames(sv) <- NULL
#sv
###################################################################################################
# REJECTION ABC 1

nreprej= 10000
tolp= 0.1 # retaining all results
set.seed(234)
ABC_rej1<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target= save_targ1, nb_simul=nreprej,
                       tol=tolp, progress_bar = T)




ABC_rej1$computime
plot(ABC_rej1$param[, 1], ABC_rej1$param[, 2], main="ABC_rejection, beta=0.2, gamma=0.02, tolerance = 0.1")


########################################################################################################
# running the model several times and storing the model outputs at the two time points in a matrix 

set.seed(123)
save_targets2= matrix(c(0,0),100,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 
for(i in 1:100){
  save_targets2[i,]= modelABC(c(0.3,0.02),100000,0.1)
}

# obtaining the targets as the mean number of infected at the time points
save_targ2 = c(ceiling(mean(save_targets2[,1])),ceiling(mean(save_targets2[,2]))) 


###################################################################################################
# REJECTION ABC 2

nreprej= 10000
tolp= 0.1 # retaining all results
set.seed(234)
ABC_rej2<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=save_targ2, nb_simul=nreprej,
                       tol=tolp, progress_bar = T)




ABC_rej2$computime
plot(ABC_rej2$param[, 1], ABC_rej2$param[, 2], main="ABC_rejection, beta=0.3, gamma=0.02, tolerance = 0.1")

#nrow(ABC_rej2$param)
#head(ABC_rej2$param)



########################################################################################################
# running the model several times and storing the model outputs at the two time points in a matrix 

set.seed(123)
save_targets3= matrix(c(0,0),100,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 
for(i in 1:100){
  save_targets3[i,]= modelABC(c(0.5,0.025),100000,0.1)
}

# obtaining the targets as the mean number of infected at the time points
save_targ3 = c(ceiling(mean(save_targets3[,1])),ceiling(mean(save_targets3[,2]))) 


###################################################################################################
# REJECTION ABC 3

nreprej= 10000
tolp= 0.1 # retaining all results
set.seed(234)
ABC_rej3<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=save_targ3, nb_simul=nreprej,
                       tol=tolp, progress_bar = T)




ABC_rej3$computime
plot(ABC_rej3$param[, 1], ABC_rej3$param[, 2], main="ABC_rejection, beta=0.5, gamma=0.025, tolerance = 0.1")

#nrow(ABC_rej$param)
head(ABC_rej3$param)



########################################################################################################
# running the model several times and storing the model outputs at the two time points in a matrix 

set.seed(123)
save_targets4= matrix(c(0,0),100,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 
for(i in 1:100){
  save_targets4[i,]= modelABC(c(0.2,0.05),100000,0.1)
}

# obtaining the targets as the mean number of infected at the time points
save_targ4 = c(ceiling(mean(save_targets4[,1])),ceiling(mean(save_targets4[,2]))) 


###################################################################################################
# REJECTION ABC 4

nreprej= 10000
tolp= 0.1 # retaining all results
set.seed(234)
ABC_rej4<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=save_targ4, nb_simul=nreprej,
                       tol=tolp, progress_bar = T)




ABC_rej4$computime
plot(ABC_rej4$param[, 1], ABC_rej4$param[, 2], main="ABC_rejection, beta=0.2, gamma=0.05, tolerance = 0.1")

#nrow(ABC_rej$param)
#head(ABC_rej4$param)


########################################################################################################
# running the model several times and storing the model outputs at the two time points in a matrix 

set.seed(123)
save_targets5= matrix(c(0,0),100,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 
for(i in 1:100){
  save_targets5[i,]= modelABC(c(0.4,0.025),100000,0.1)
}

# obtaining the targets as the mean number of infected at the time points
save_targ5 = c(ceiling(mean(save_targets5[,1])),ceiling(mean(save_targets5[,2]))) 


###################################################################################################
# REJECTION ABC 5

nreprej= 10000
tolp= 0.1 # retaining all results
set.seed(234)
ABC_rej5<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=save_targ5, nb_simul=nreprej,
                       tol=tolp, progress_bar = T)




ABC_rej5$computime
plot(ABC_rej5$param[, 1], ABC_rej5$param[, 2], main="ABC_rejection, beta=0.4, gamma=0.025, tolerance = 0.1")

#nrow(ABC_rej5$param)
#head(ABC_rej5$param)




########################################################################################################
# running the model several times and storing the model outputs at the two time points in a matrix 

set.seed(123)
save_targets6= matrix(c(0,0),100,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 
for(i in 1:100){
  save_targets6[i,]= modelABC(c(0.5,0.04),100000,0.1)
}

# obtaining the targets as the mean number of infected at the time points
save_targ6 = c(ceiling(mean(save_targets6[,1])),ceiling(mean(save_targets6[,2]))) 


###################################################################################################
# REJECTION ABC 6

nreprej= 10000
tolp= 0.1 # retaining all results
set.seed(234)
ABC_rej6<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=save_targ6, nb_simul=nreprej,
                       tol=tolp, progress_bar = T)




ABC_rej6$computime
plot(ABC_rej6$param[, 1], ABC_rej6$param[, 2], main="ABC_rejection, beta=0.5, gamma=0.04, tolerance = 0.1")

#nrow(ABC_rej6$param)
#head(ABC_rej6$param)



##################################################################################

par(mfrow=c(3,2))
plot(ABC_rej1$param[, 1], ABC_rej1$param[, 2], main="ABC_rejection, beta=0.2, gamma=0.02, tolerance = 0.1")
plot(ABC_rej2$param[, 1], ABC_rej2$param[, 2], main="ABC_rejection, beta=0.3, gamma=0.02, tolerance = 0.1")
plot(ABC_rej3$param[, 1], ABC_rej3$param[, 2], main="ABC_rejection, beta=0.5, gamma=0.025, tolerance = 0.1")
plot(ABC_rej4$param[, 1], ABC_rej4$param[, 2], main="ABC_rejection, beta=0.2, gamma=0.05, tolerance = 0.1")
plot(ABC_rej5$param[, 1], ABC_rej5$param[, 2], main="ABC_rejection, beta=0.4, gamma=0.025, tolerance = 0.1")
plot(ABC_rej6$param[, 1], ABC_rej6$param[, 2], main="ABC_rejection, beta=0.5, gamma=0.04, tolerance = 0.1")



