library(SimInf)


for (i in 1:5000){
u0    <- data.frame(S=c(990), I=c(10), R=c(0))
model <- SIR(u0, 1:75, beta = seqdata[i,1],
             gamma = seqdata[i,2])
result <- run(model, threads = 1)#

if (i == 1){
  plot(result@U[2,], col="red",
       ylim=c(0,1000),
       type="l", 
       main="Plot of the Infected curves for Sequential posterior",
       ylab = "I",
       xlab = "Time(days)") # plot all 5000 I curves as one plot
 } else {
  lines(result@U[2,], col="red")
 lines(run_true[,3], col="black", lwd = 4)
 }

}
