
library(EasyABC)
library(SimInf)

#####################################################################################################

parameters=c(0.8,0.02)
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
  #colnames(model_output) <- c("I","I")
  return(model_output)
}

modelABC(c(0.8,0.02),100000,0.1)

############################
# running the model several times and storing the model outputs at the two time points in a matrix 

set.seed(123)
save_targets1= matrix(c(0,0),100,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 
#colnames(save_targets1) <- c("I","I")
for(i in 1:100){
  save_targets1[i,]= modelABC(c(0.8,0.02),100000,0.1)
}

# obtaining the targets as the mean number of infected at the time points
save_targ1 = c(ceiling(mean(save_targets1[,1])),ceiling(mean(save_targets1[,2])))

##############################################################################################
##############################################################################################

nrepseq=400
#alpha=0.5 by default
set.seed(123)
ABC_seq1<-ABC_sequential(method="Lenormand", model=modelABC, prior=list(c("unif",0,1), c("unif",0,0.5)), nb_simul=nrepseq,
                        summary_stat_target=c(save_targ1), p_acc_min=0.2, progress_bar = T)
plot(ABC_seq1$param[, 1], ABC_seq1$param[, 2])

ABC_seq1$computime

nrow(ABC_seq1$param)

###########################################################################################

c_drov=0.7
nrepseq=100
set.seed(123)
ABC_seq<-ABC_sequential(method="Drovandi", model=modelABC, prior=list(c("unif",0,1), c("unif",0,0.5)), nb_simul=nrepseq,
                        summary_stat_target=c(save_targ1), tolerance_tab=c(0.5,0.3), c=c_drov, progress_bar = T)


ABC_seq$computime

### plot the posterior obtained by ABC sequential (Drovandi)
plot(ABC_seq$param[, 1], ABC_seq$param[, 2])

nrow(ABC_seq$param)



