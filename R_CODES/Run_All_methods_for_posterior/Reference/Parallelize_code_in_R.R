
#################################################################################
setwd("C:/Users/ZENABU/Documents/GitHub/masters_project/R_CODES/masters_project")
source("my_functions.R")

#################################################################################
##########################################################################
## runnunig rej ABC in parallel 
library(doParallel)
library(foreach)

##########################################################################
# Calculate the number of cores
no_cores <- detectCores() - 3 # detects number of cores on computer and no_cores in use

cl <- makeCluster(no_cores) # makes cluster with number of cores assigned for the simulation

registerDoParallel(cl)  # registers the cluster

ABC_ref1 <- foreach(simulations = 1:5,# runs simulations five times to obtain 
                                        # hence gives one to each core
                      .combine = c,
                      .packages = c("SimInf", "EasyABC")) %dopar%
  
  ABC_rejection(model = modelforABC, 
                prior = list(c("unif",0.1,0.3),
                             c("unif",0.01,0.03)), 
                summary_stat_target = c(0.644, 0.404),
                nb_simul = 200000,
                tol = 1,
                progress_bar = T
                ) # each core runs 2e5 simulations in the calibration
                             #method and and retains 2e5 parameter combinations

stopImplicitCluster() # stops cluster and reverts back to using only one core/ exit cluster


############################################
# check compute time

computime =  ABC_ref1$computime  


 ##########################################
#gathering desired output and save 

gather_params1 <- ABC_ref1[seq(1, length(ABC_ref1), 7)] # Gathers all parameter combinations

posterior1 <- data.frame(rbind(gather_params1[[1]], 
                        gather_params1[[2]],
                        gather_params1[[3]],
                        gather_params1[[4]],
                        gather_params1[[5]])) 

saveRDS(posterior1, "ref1_posterior.rds")
ref1 <- readRDS("ref1_posterior.rds")

###############################

gather_stats1 <- ABC_ref1[seq(2, length(ABC_ref1), 7)] # Gathers all parameter combinations

stats1 <- data.frame(rbind(gather_stats1[[1]], 
                               gather_stats1[[2]],
                               gather_stats1[[3]],
                               gather_stats1[[4]],
                               gather_stats1[[5]])) # 

saveRDS(stats1, "stats1.rds")
stats1 <- readRDS("stats1.rds")


abc2ref1 <- abc(target = c(0.644, 0.404),
                param = ref1,
                sumstat = stats1,
                tol = 0.05,
                method = "rejection")


plot(abc2ref1$unadj.values[,1],abc2ref1$unadj.values[,2],
     xlab = "beta", ylab = "gamma",
    # ylim=c(0,0.5),
    # xlim=c(0,1)
      main ="posterior_for_abc2ref1") # retained 0.5% 0f thw 1000000 runs


##################################################################
###################################################################################
# scenario 2 = 3 targets

# Calculate the number of cores
no_cores <- detectCores() - 3 # detects number of cores on computer and no_cores in use

cl <- makeCluster(no_cores) # makes cluster with number of cores assigned for the simulation

registerDoParallel(cl)  # registers the cluster

ABC_ref2 <- foreach(simulations = 1:5,# runs simulations five times to obtain 
                    # hence gives one to each core
                    .combine = c,
                    .packages = c("SimInf", "EasyABC")) %dopar%
  
  ABC_rejection(model = modelforABC, 
                prior = list(c("unif",0.1,0.3),
                             c("unif",0.01,0.03)), 
                summary_stat_target = c(0.622, 0.371, 0.677),
                nb_simul = 200000,
                tol = 1,
                progress_bar = T,
                use_seed = T) # each core runs 2e5 simulations in the calibration
#method and and retains 2e5 parameter combinations

stopImplicitCluster() # stops cluster and reverts back to using only one core/ exit cluster


################################################################################
################################################################################
# check compute time

computime =  ABC_ref2$computime  


##################################################################################
#gathering desired output and save 

gather_params12 <- ABC_ref2[seq(1, length(ABC_ref2), 7)] # Gathers all parameter combinations

posterior2 <- data.frame(rbind(gather_params1[[1]], 
                               gather_params1[[2]],
                               gather_params1[[3]],
                               gather_params1[[4]],
                               gather_params1[[5]])) # 


#gather_params1[[1]]

saveRDS(posterior2, "ref2_posterior.rds")
###################################################################################

