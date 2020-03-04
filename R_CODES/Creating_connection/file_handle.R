library(SimInf)
library(tictoc)
library(EasyABC)

setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")


modelforABC = function(parameters, 
                       times=1:75, 
                       targetTimes=c(50,75),
                       peakPrevalence = F){
### you must have already defined and opened a file object called zzfile
  
 ###########################################
  # open file connection
  
  
  tic()# begin timer 
  
  u0 = data.frame(S = c(990), # initial compartmental values
                  I = c(10),
                  R = c(0))
  
  model <- SIR(u0, # initial compartmental values
               times,                  # stochastic sir model that outputs different epicurves
               beta = parameters[1],      # per run
               gamma = parameters[2]) 
  
  
  result <- run(model, 
                threads = 1)   # runs the SIR model and outputs results
  toctime <- toc(quiet=T) # end timer
  
  #print(toctime)
  
  writeLines( as.character(toctime$toc-toctime$tic),
              record_time, sep = "\n") 
  
 ########################################## 
  
  prev <- prevalence(result, I~.)
  # targ <- numeric()
  targs <- prev[targetTimes,2]
  
  if (peakPrevalence){return(c(targs,max(prev[,2])))}
  else(return(targs))
  ## add timestand to file timerecord
}

record_time <- file("mytime.txt")
open(record_time, "w")
 
modelforABC(c(0.2,0.02))
 
close(record_time) ## close file connection
unlink(record_time)

library(tictoc)
?toc
