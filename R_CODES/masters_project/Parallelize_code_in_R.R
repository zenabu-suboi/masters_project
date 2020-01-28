
#######################################################
#installing/loading the latest installr package:
  #install.packages("installr"); library(installr) # install+load installr

#updateR() # updating R.
######################################################
####################################################
library(foreach)
library(doParallel)

####################################################################
#Eg. running the sir model in parallel

# no_cores <- detectCores() - 4
# cl <- makeCluster(no_cores)
# registerDoParallel(cl)
# 
# ABCmodel <- foreach(simulation = 1:100,
#                     .combine = rbind,
#                     .packages = "SimInf") %dopar%
# 
#                          modelforABC(c(runif(1,0,1), 
#                                        runif(1,0,0.05)))
# 
# stopImplicitCluster()


##########################################################################
##########################################################################
## runnunig rej ABC in parallel 


# Calculate the number of cores
no_cores <- detectCores() - 4 # detects number of cores on computer and puts four out of use

cl <- makeCluster(no_cores) # makes cluster with number of cores assigned for the simulation

registerDoParallel(cl)  # registers the cluster

ABC_rejref <- foreach(simulation = 1:4,# runs simulations four times to obtain 
                                            #hence gives one to each core
                       .combine = c,
                       .packages = c("SimInf", "EasyABC")) %dopar%
  
                ABC_rejection(model = modelforABC, 
                              prior = list(c("unif",0.1,0.4),
                                         c("unif",0.01,0.03)), 
                              summary_stat_target = meanTargetStats,
                              nb_simul = 2500,
                              tol = 1,
                              progress_bar = T,
                              use_seed = T) # each core runs 25 simulations in the calibration
#                                          #method and and retains 25 parameter combinations


stopImplicitCluster() # stops cluster and reverts back to using one core

################################################################################
# check compute time

computime =  ABC_rejref$computime  


 ##################################################################################
#gathering desired output and save 

gather_params <- ABC_rejref[seq(1, length(ABC_rejref), 7)] # Gathers all parameter combinations

posterior <- data.frame(rbind(gather_params[[1]], 
                        gather_params[[2]],
                        gather_params[[3]],
                        gather_params[[4]])) # 


gather_params[[1]]

#saveRDS(posterior, "ref_posterior.rds")
###################################################################################
# class(posterior)
# class(gather_params[[1]])
