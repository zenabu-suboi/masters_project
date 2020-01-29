
#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R")

#################################################################################
################################################################################
library(foreach)
library(doParallel)

##########################################################################
##########################################################################
## runnunig rej ABC in parallel 


library(foreach)
# Calculate the number of cores
no_cores <- detectCores() - 3 # detects number of cores on computer and no_cores in use

cl <- makeCluster(no_cores) # makes cluster with number of cores assigned for the simulation

registerDoParallel(cl)  # registers the cluster

ABC_rejref <- foreach(simulations = 1:5,# runs simulations five times to obtain 
                                        # hence gives one to each core
                      .combine = c,
                      .packages = c("SimInf", "EasyABC")) %dopar%
  
  ABC_rejection(model = modelforABC, 
                prior = list(c("unif",0.1,0.4),
                             c("unif",0.01,0.03)), 
                summary_stat_target = targets(c(0.2, 0.02)),
                nb_simul = 200000,
                tol = 1,
                progress_bar = T,
                use_seed = T) # each core runs 2e5 simulations in the calibration
                             #method and and retains 2e5 parameter combinations


stopImplicitCluster() # stops cluster and reverts back to using only one core/ exit cluster


################################################################################
################################################################################
# check compute time

computime =  ABC_rejref$computime  


 ##################################################################################
#gathering desired output and save 

gather_params <- ABC_rejref[seq(1, length(ABC_rejref), 7)] # Gathers all parameter combinations

posterior <- data.frame(rbind(gather_params[[1]], 
                        gather_params[[2]],
                        gather_params[[3]],
                        gather_params[[4]],
                        gather_params[[5]])) # 


#gather_params[[1]]

#saveRDS(posterior, "ref_posterior.rds")
###################################################################################
# class(posterior)
# class(gather_params[[1]])
