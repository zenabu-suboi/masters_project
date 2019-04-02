
library(EasyABC)
library(SimInf)

#####################################################################################################

parameters=c(1.0,0.02)
u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
model <- SIR(u0, 1:75, beta=parameters[1], gamma=parameters[2]) # specifying the model
result1 <- run(model, threads = 1) 
plot(result1)


##########################################################################################

modelABC2= function(parameter,N=100000,inf=0.1){
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

modelABC2(c(1,0.02),100000,0.1)

########################################################################################################
# running the model several times and storing the model outputs at the two time points in a matrix 

set.seed(123)
save_targets2= matrix(c(0,0),100,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 
#colnames(save_targets1) <- c("I","I")
for(i in 1:100){
  save_targets2[i,]= modelABC2(c(1,0.02),100000,0.1)
}

# obtaining the targets as the mean number of infected at the time points
save_targ2 = c(ceiling(mean(save_targets2[,1])),ceiling(mean(save_targets2[,2])))
