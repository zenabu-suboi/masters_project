### some illustrative code

library(EasyABC)
library(SimInf)

u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
model <- SIR(u0, 1:75, beta=0.5, gamma=0.025) # specifying the model
result <- run(model, threads = 1) 
resultst50= c(result@U[1,50], result@U[2,50], result@U[3,50])
sum(resultst50)
resultst75= c(result@U[1,75], result@U[2,75], result@U[3,75])
sum(resultst75)

target1 = data.frame("Prev@50"= sample(result@U[2,50], 10), "Prev@75"= sample(result@U[2,75],10))

#target_1 = data.frame("Prev@50"= sample(result@U[2,50], 10), "Prev@75"= sample(result@U[2,75],10))/sum(u0)



##############################################################################################################

# creating function

modelABC= function(parameters,N,inf,samplesize){
  library(SimInf)
  u0 = data.frame(S=c((1-inf)*N), I=c(inf*N), R=c(0))    #specifying initial compartmental values
  model <- SIR(u0, 1:75, beta=parameters[1], gamma=parameters[2]) # specifying the model
  result <- run(model, threads = 1)     # running the model
  target= data.frame("Prev@50"= sample(result@U[2,50], samplesize), "Prev@75"= sample(result@U[2,75],samplesize)) # prevalence at times 50 and 75
  #return(target)
 return(list(max(result@U[2,]), which.max(result@U[2,]), target))
}

#modelABC(c(0.5,0.025),100000,0.1,10)[[2]]
modelABC(c(0.5,0.025),100000,0.1,10)

plot(result)


###############################################################################################################


# running the model 100 times
set.seed(123)
save_prevs= as.list(rep(NA, 100)) # a 10000x2 matrix that receives the prevalences at 50 and 75 per model run 
for(i in 1:100){
 # save_prevs[[i]]= modelABC(c(0.5,0.025),100000,0.1,10)[[2]]
 save_prevs[[i]]= modelABC(c(0.5,0.025),100000,0.1,10)
}

save_prevs



