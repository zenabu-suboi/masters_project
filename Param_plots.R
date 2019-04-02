
library(EasyABC)
library(SimInf)


parameters1=c(0.2,0.02)
u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
model1 <- SIR(u0, 1:75, beta=parameters1[1], gamma=parameters1[2]) # specifying the model
result1 <- run(model1, threads = 1) 


parameters2=c(0.3,0.02)
u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
model2 <- SIR(u0, 1:75, beta=parameters2[1], gamma=parameters2[2]) # specifying the model
result2 <- run(model2, threads = 1) 


parameters3=c(0.5,0.025)
u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
model3 <- SIR(u0, 1:75, beta=parameters3[1], gamma=parameters3[2]) # specifying the model
result3 <- run(model3, threads = 1) 


parameters4=c(0.2,0.05)
u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
model4 <- SIR(u0, 1:75, beta=parameters4[1], gamma=parameters4[2]) # specifying the model
result4 <- run(model4, threads = 1) 


parameters5=c(0.4,0.025)
u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
model5 <- SIR(u0, 1:75, beta=parameters5[1], gamma=parameters5[2]) # specifying the model
result5 <- run(model5, threads = 1) 


parameters6=c(0.5,0.04)
u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
model6 <- SIR(u0, 1:75, beta=parameters6[1], gamma=parameters6[2]) # specifying the model
result6 <- run(model6, threads = 1) 


par(mfrow=c(1,2))
plot(result1, main=" beta=0.2, gamma=0.02")
plot(result2, main=" beta=0.3, gamma=0.02")
plot(result3,main=" beta=0.5, gamma=0.025")
plot(result4,main=" beta=0.2, gamma=0.05")
plot(result5,main=" beta=0.4, gamma=0.025")
plot(result6,main=" beta=0.5, gamma=0.04")

