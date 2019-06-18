

library(EasyABC)
library(SimInf)

# specifying initial compartmental values
u0 = data.frame(S=c(90000), I=c(10000), R=c(0)) # where N=100000


# specifying model
model <- SIR(u0, 1:75, beta = 0.5, gamma = 0.025 )
result <- run(model, threads = 1)
plot(result)
result@U[2,50]
result@U[2,75]



modelABC= function(parameters, N, inf=0.1, I=inf*N, samplesize){
  library(SimInf)
  u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
  model <- SIR(u0, 1:75, beta=parameters[1], gamma=parameters[2]) # specifying the model
  result <- run(model, threads = 1)     # running the model
  pop= numeric()
  pop[1]=result@U[2,50]   # number of infected individuals at time 50
  pop[2]=result@U[2,75]  # number of infected individuals at time 75
  prevs=pop/sum(u0) # prevalence at times 50 and 75
  return(prevs)
}


# creating a loop around the function
set.seed(123)
save_prevs= matrix(c(0,0),10000,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 
for(i in 1:10000){
save_prevs[i,]= modelABC(c(0.5,0.025))
}

### random selection of rows implies random selection of model runs

targets=save_prevs[sample(nrow(save_prevs), 1000), ]  ### obtaining a sample of size, 1000 as the targets


hist(targets[,1]) # histogram of sample prev at 50
hist(targets[,2]) # histogram of sample prev at 75

c(max(targets[,1]), max(targets[,2])) #Peak prevalence level
