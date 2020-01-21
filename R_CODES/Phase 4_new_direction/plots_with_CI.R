

########################################################################

# plot of the I-curve for the true posterio (beta=0.2,gamma=0.02)

sir_true <- function(t, x, parms)  {
  with(as.list(c(parms, x)), {
    dS = - beta * I / 1000 * S
    dI = beta * I / 1000 * S - gamma * I
    dR = gamma * I
    output <- c(dS, dI, dR)
    list(output)
  })
}

start <- c(S=990, I=10, R=0 )

parms_true <- c(beta = 0.2,
                gamma = 0.02)

times <- seq(0, 75, 1)

run_true <- ode(times=times,
                y=start,
                func=sir_true,
                parms=parms_true)
#plot(run_true[,3], type = "l", lwd=5)





#############################################################################
#############################################################################
# seq projection curves

seqdata <- data.frame("betas" = ABC_seq2[["param"]][,1],
                     "gammas" = ABC_seq2[["param"]][,2])# stores seq posterior 

saveRDS(object = seqdata , file = 'seqposterior.rds')
seq_posterior <- readRDS('seqposterior.rds')


# mean(seqdata$betas) # mean estimate of beta for the true posterio = 0.1126364
#                                                        
# mean(seqdata$gammas) # mean estimate of gamma for the true posterio  = 0.01973983


library(deSolve)

sir <- function(t, x, parms)  {
  with(as.list(c(parms, x)), {
    dS = - beta * I / 1000 * S
    dI = beta * I / 1000 * S - gamma * I
    dR = gamma * I
    output <- c(dS, dI, dR)
    list(output)
  })
}

#the Initial values

## The parameters 
for (i in 1:5000){
  start<-c(S=990, I=10, R=0 )
  
  parms <- c( beta= seqdata[i,1],
            gamma= seqdata[i,2])

  times <- seq(0, 75, 1)

  runs <- ode(times=times,
           y=start,
           func=sir,
           parms=parms)

  if (i == 1){
  plot(runs[,3], col="red",
     ylim=c(0,1000),
     type="l", 
     main="Plot of the Infected curves for Sequential posterior",
     ylab = "I",
     xlab = "Time(days)") # plot all 5000 I curves as one plot
  } else {
    lines(runs[,3], col="red")
    lines(run_true[,3], col="black", lwd = 5)
    }
 }
 
head(run[1])

###############################################################################
###############################################################################
# rej projection curves

rejdata <- data.frame("betas" = abc0.1$unadj.values[1:5000,1],
                      "gammas" = abc0.1$unadj.values[1:5000,2])

saveRDS(object = rejdata , file = 'rejposterior.rds')
rej_posterior <- readRDS('rejposterior.rds')


# mean(rejdata$betas) # mean estimate of beta for the true posterio =  0.3538303
# 
# mean(rejdata$gammas) # mean estimate of gamma for the true posterio  = 0.03118745


sir <- function(t, x, parms)  {
  with(as.list(c(parms, x)), {
    dS = - beta * I / 1000 * S
    dI = beta * I / 1000 * S - gamma * I
    dR = gamma * I
    output <- c(dS, dI, dR)
    list(output)
  })
}

#the Initial values

## The parameters 
for (i in 1:5000){
  start<-c(S=990, I=10, R=0 )
  
  parms <- c( beta= rejdata[i,1],
              gamma= rejdata[i,2])
  
  times <- seq(0, 75, 1)
  
  runr <- ode(times=times,
             y=start,
             func=sir,
             parms=parms)
  if (i == 1){
    plot(runr[,3], col="orange",
         ylim=c(0,1000),
         type="l", 
         main="Plot of the Infected curves for Rejection posterior",
         ylab = "I",
         xlab = "Time(days)") # plot all 5000 I curves as one plot
  } else {
    lines(runr[,3], col="orange")
    lines(run_true[,3], col="black", lwd = 5)
    
  }  
}

########################################################################
#################################################################
# BMLE projection

sir <- function(t, x, parms)  {
  with(as.list(c(parms, x)), {
    dS = - beta * I / 1000 * S
    dI = beta * I / 1000 * S - gamma * I
    dR = gamma * I
    output <- c(dS, dI, dR)
    list(output)
  })
}

#the Initial values

## The parameters 
for (i in 1:5000){
  start<-c(S=990, I=10, R=0 )
  
  parms <- c( beta= asdf[i,1],
              gamma= asdf[i,2])
  
  times <- seq(0, 75, 1)
  
  runb <- ode(times=times,
             y=start,
             func=sir,
             parms=parms)
  if (i == 1){
    plot(runb[,3], col="purple",
         ylim=c(0,1000),
         type="l", 
         main="Plot of the Infected curves for BMLE posterior",
         ylab = "I",
         xlab = "Time") # plot all 5000 I curves as one plot
  } else {
    lines(runb[,3], col="purple")
    lines(run_true[,3], col="black", lwd = 4)
    
  }  
}

