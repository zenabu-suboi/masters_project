

library(EasyABC)
library(SimInf)

# specifying initial compartmental values
u0 = data.frame(S=c(90000), I=c(10000), R=c(0)) # where N=100000


# specifying model
model <- SIR(u0, 1:75, beta = 0.5, gamma = 0.025 )
result <- run(model, threads = 1)
plot(result)
result@U
result@U[2,50]
result@U[2,75]


modelABC= function(parameters){
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





#############################################################################################################

### some illustrative code

u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
model <- SIR(u0, 1:75, beta=0.5, gamma=0.025) # specifying the model
result <- run(model, threads = 1) 
plot(result)

resultst50= c(result@U[1,50], result@U[2,50], result@U[3,50])
sum(resultst50)
target1 = data.frame(sample(result@U[2,50],100), sample(result@U[2,75],100))



# creat function to be used

modelABC= function(parameters,N,inf=0.1){
  library(SimInf)
  u0 = data.frame(S=c((1-inf)*N), I=c(inf*N), R=c(0))    #specifying initial compartmental values
  model <- SIR(u0, 1:75, beta=parameters[1], gamma=parameters[2]) # specifying the model
  result <- run(model, threads = 1)     # running the model
  pop= numeric()
  pop[1]=result@U[2,50]   # number of infected individuals at time 50
  pop[2]=result@U[2,75]  # number of infected individuals at time 75
  prevs=pop/sum(u0) # prevalence at times 50 and 75
  #return(prevs)
  return(plot(result))
}


res1= modelABC(c(0.5,0.025),100000)
#plot(res1)


sample(result@U[2,50],1000)  # sample 1000 number of infected out of the prevs at time 50

summat= matrix(c(0,0),10000,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 


# creating a loop around the function
set.seed(123)
save_prevs= matrix(c(0,0),10000,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 
target_1= matrix(c(0,0),10000,2)
for(i in 1:10000){
  save_prevs[i,]= modelABC(c(0.5,0.025),100000)
  target_1=c(sample(result@U[2,50],100), sample(result@U[2,75],100))
}





for(i in 1:10000){
  xsamp= sample(x=c("S","I","R"),prob= resultst50/10000, size= 1000, replace=T)
  summat[i,]= summary(as.factor(xsamp))
}
mean(summat[,1])



###############################################################################################################

modelABC= function(parameters){
  library(SimInf)
  u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
  model <- SIR(u0, 1:75, beta=parameters[1], gamma=parameters[2]) # specifying the model
  result <- run(model, threads = 1)     # running the model
  pop= numeric()
  pop[1]=result@U[2,50]   # number of infected individuals at time 50
  pop[2]=result@U[2,75]  # number of infected individuals at time 75
  prevs=pop/sum(u0) # prevalence at times 50 and 75
  return(plot(result))
}


res2= modelABC(c(0.5,0.025))
plot(res2)



###############################################################################################


?sample

summat= matrix(c(0,0),10000,2) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 

for(i in 1:10000){
xsamp= sample(x=c("S","I","R"),prob= resultst50/10000, size= 1000, replace=T)
summat[i,]= summary(as.factor(xsamp))
}
mean(summat[,1])


# peak prevalence
max(result@U[2,])
# time at peak prevalence
which.max(result@U[2,])

#################################################################################################

?rnorm()
x1= rnorm(100000, mean=10,sd=1) # generate 1000000 random numbers from the normal distribution
mean(x1)

mean(sample(x1,10)) # sample 10 out of 100000 randomentries and compute the mean




####################################################################################################


######################################################################################################################
target = data.frame("Prev@50"= sample(result@U[2,50], 30000), "Prev@75"= sample(result@U[2,75],10000))
c(mean(target[1:30000,1]), mean(target[1:10000,2]))


target = data.frame("Prev@50"= sample(result@U[2,50], 1000), "Prev@75"= sample(result@U[2,75],1000))
c(mean(target[1:1000,1]),mean(target[1:1000,2]))


target = data.frame("Prev@50"= sample(result@U[2,50], 100), "Prev@75"= sample(result@U[2,75],100))
c(mean(target[1:100,1]), mean(target[1:100,2]))

target = data.frame("Prev@50"= sample(result@U[2,50], 90), "Prev@75"= sample(result@U[2,75],90))
c(mean(target[1:90,1]), mean(target[1:90,2]))


