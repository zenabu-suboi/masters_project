
library(tictoc)
modelforABC = function(parameters, 
                       times=1:75, 
                       targetTimes=c(50,75),
                       peakPrevalence = T){
  
 ###########################################
  # open file handle
  v <- numeric()
  #zztime <- tempfile(fileext = ".time")
  #mytime <- file(zztime, "w")
  
  tic("runtime")# begin timer 
  
  u0 = data.frame(S = c(990), # initial compartmental values
                  I = c(10),
                  R = c(0))
  
  model <- SIR(u0, # initial compartmental values
               times,                  # stochastic sir model that outputs different epicurves
               beta = parameters[1],      # per run
               gamma = parameters[2]) 
  
  
  result <- run(model, 
                threads = 1)   # runs the SIR model and outputs results
  toc() # end timer
  v <- append(v,toc())

 #as.character(toc())
  #cat( as.character(toc()), file = mytime) 
  #close(mytime) ## close file handle
  #readLines(zztime)
  #unlink(zztime)
  
 ########################################## 
  
  prev <- prevalence(result, I~.)
  # targ <- numeric()
  targs <- prev[targetTimes,2]
  
  if (peakPrevalence){return(c(targs,max(prev[,2])))}
  else(return(targs))
  ## add timestand to file timerecord
}


modelforABC(c(0.2,0.02))
v

