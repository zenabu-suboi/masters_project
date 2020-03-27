
library(ggplot2)
library(SimInf)
library(cowplot)

#runtimes <- c()

modelforABC = function(parameters, 
                       tspan = seq(0,75, by=1), 
                       targetTimes = c(50,75),
                       peakPrevalence = T){
  
  #tic()# begin timer 
  
  u0 = data.frame(S = c(990), # initial compartmental values
                  I = c(10),
                  R = c(0))
  #model <- SIR(u0, tspan = seq(0,75,by=1),
  #            beta = beta, gamma=gamma)
  
  model <- SIR(u0, # initial compartmental values
               tspan = seq(1,75,by=1),                  # stochastic sir model that outputs different epicurves
               beta = parameters[1],      # per run
               gamma = parameters[2]) 
  
  result <- run(model, 
                threads = 1)
  
  #   time <- microbenchmark(result <- run(model, 
  #               threads = 1),   # runs the SIR model and outputs results
  # times = 1) # measures time in nanoseconds (/10^9)
  #  
  #toctime <- toc(quiet=T) # end timer
  
  # writeLines( as.character(time$time),
  #             record_time_bmle3, sep = "\n") 
  # 
  ########################################## 
  
  prev <- prevalence(result, I~.)
  # targ <- numeric()
  targs <- prev[targetTimes,2]
  
  if (peakPrevalence){return(c(targs,max(prev[,2])))}
  else(return(targs))
}

### set.seed for reproducability
set.seed(121)

### save the results from 1000 runs, take the means as the targets
savetargets2 = matrix(c(0,0),1000,2)
for(i in 1:1000){
  savetargets2[i,]= modelforABC(c(0.2,0.02))
}

##############################################################
targ2_50 <- ggplot(data=as.data.frame(savetargets2), aes(savetargets2[,1])) + 
  geom_histogram(aes(y =..density..), 
                 #breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="blue", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title = "Distribution of prevalence at time 50",
       x="Prevalence at time 50", y="Frequency")+
  annotate("segment", x = 0.644, xend = 0.644,
           y = 20, yend = 4.0, colour = "black",
           size=1.2, alpha=0.6, arrow=arrow())

annotation50 <- data.frame(
  x = c(0.645),
  y = c(22),
  label = "target used (0.644)"
)

targ2_50 <- targ2_50 + geom_text(data=annotation50, aes( x=x, y=y, label=label),                 , 
                     color="orange", 
                     size=4, angle=0, fontface="bold")

############################

targ2_75 <-  ggplot(data=as.data.frame(savetargets2), aes(savetargets2[,2])) + 
  geom_histogram(aes(y =..density..), 
                 #breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title = "Distribution of prevalence at time 75",
       x="Prevalence at time 75", y="Frequency")+
  annotate("segment", x = 0.404, xend = 0.404,
           y = 22, yend = 12.1, colour = "black",
           size=1.2, alpha=0.6, arrow=arrow())

annotation75 <- data.frame(
  x = c(0.404),
  y = c(25),
  label = "target used (0.404)"
)

targ2_75 <- targ2_75 + geom_text(data=annotation75, aes( x=x, y=y, label=label),                 , 
                     color="orange", 
                     size=4, angle=0, fontface="bold")

plot_grid(targ2_50,
          targ2_75,
    #labels = c("A", "B", "C"),
  ncol = 2, nrow = 1)

#################################################################
# scenario 2

### set.seed for reproducability
set.seed(121)

### save the results from 1000 runs, take the means as the targets
savetargets3 = matrix(c(0,0),1000,3)
for(i in 1:1000){
  savetargets3[i,]= modelforABC(c(0.2,0.02))
}

##############################################################
targ3_50 <- ggplot(data=as.data.frame(savetargets3),
                   aes(savetargets3[,1])) + 
  geom_histogram(aes(y =..density..), 
                 #breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="blue", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title = "Distribution of prevalence at 50",
       x="Prevalence at time 50", y="Frequency")+
  annotate("segment", x = 0.622, xend = 0.622,
           y = 20, yend = 12.3, colour = "black",
           size=1, alpha=0.6, arrow=arrow())

annotation50 <- data.frame(
  x = c(0.622),
  y = c(22),
  label = "target used (0.622)"
)

targ3_50 <- targ3_50 + geom_text(data=annotation50, aes( x=x, y=y, label=label),                 , 
                                 color="orange", 
                                 size=4, angle=0, fontface="bold")

############################

targ3_75 <-  ggplot(data=as.data.frame(savetargets3), aes(savetargets3[,2])) + 
  geom_histogram(aes(y =..density..), 
                 #breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="green", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title = "Distribution of prevalence at 75",
       x="Prevalence at time 75", y="Frequency")+
  annotate("segment", x = 0.371, xend = 0.371,
           y = 23, yend = 18.3, colour = "black",
           size=1, alpha=0.6, arrow=arrow())

annotation75 <- data.frame(
  x = c(0.371),
  y = c(24),
  label = "target used (0.371)"
)

targ3_75 <- targ3_75 + geom_text(data=annotation75, aes( x=x, y=y, label=label),                 , 
                                 color="orange", 
                                 size=4, angle=0, fontface="bold")


peak_prev <-  ggplot(data=as.data.frame(savetargets3), 
                     aes(savetargets3[,3])) + 
  geom_histogram(aes(y =..density..), 
                 #breaks=seq(20, 50, by = 2), 
                 col="red", 
                 fill="black", 
                 alpha=.2) + 
  geom_density(col=2) + 
  labs(title = "Distribution of peak prevalence",
       x="Peak Prevalence", y="Frequency")+
  annotate("segment", x = 0.677, xend = 0.677,
           y = 29, yend = 24.7, colour = "black",
           size=1, alpha=0.6, arrow=arrow())

annotationprev <- data.frame(
  x = c(0.677),
  y = c(30),
  label = "target used (0.677)"
)

peak_prev <- peak_prev + geom_text(data=annotationprev, aes( x=x, y=y, label=label),                 , 
                                 color="orange", 
                                 size=4, angle=0, fontface="bold")


plot_grid(targ3_50,
          targ3_75,
          peak_prev,
          #labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

