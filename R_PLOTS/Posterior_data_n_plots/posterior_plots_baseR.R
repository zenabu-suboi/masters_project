#######################################################
setwd("C:/Users/Zee/Documents/GitHub/masters_project/R_PLOTS/
      Posterior_data_n_plots/")

#########################################################
library(ggplot2)

########################################################
#read in the data required

# reference data
mydat_2targets_ref <- as.data.frame(readRDS("targets2_refposterior.rds"))
mydat_3targets_ref <- as.data.frame(readRDS("targets3_refposterior.rds"))

# seq
mydat_2targets_seq <- as.data.frame(readRDS("2targets_seq_post"))
mydat_3targets_seq <- as.data.frame(readRDS("3targets_seq_post"))

#rej
mydat_2targets_rej <- as.data.frame(readRDS("2targets_rej_post"))
mydat_3targets_rej <- as.data.frame(readRDS("3targets_rej_post"))

#bmle
mydat_2targets_bmle <- as.data.frame(readRDS("bmle2_posterior.rds"))
mydat_3targets_bmle <- as.data.frame(readRDS("bmle3_posterior.rds"))

#Tom's method
mydat_2targets_tom
mydat_3targets_tom

##############################################################
par(mfrow = c(2,3))
# Scenario 1

# rejection
## appears in creating a raster
plot(mydat_2targets_rej[,1],
     mydat_2targets_rej[,2], 
     xlab = "beta", ylab = "gamma",
     # col = "purple",
     main ="Rejection ABC" ,
     xlim = c(0,1.0),
     ylim = c(0,0.2),
     cex.axis = 1.3,
     cex.lab = 1.3)

points(mydat_2targets_ref[,1],
       mydat_2targets_ref[,2],
       col = "red", 
       main ="Scenario_1_True_posterior") # retained 0.5% 0f thw 1000000 runs



# sequential
## appears in creating a raster
plot(mydat_2targets_seq[,1],
     mydat_2targets_seq[,2], 
     xlab = "beta", ylab = "gamma",
     # col = "purple",
     main ="Sequential ABC" ,
     xlim = c(0,1.0),
     ylim = c(0,0.2),
     cex.axis = 1.3,
     cex.lab = 1.3)

points(mydat_2targets_ref[,1],
       mydat_2targets_ref[,2],
       col = "red", 
       main ="Scenario_1_True_posterior") # retained 0.5% 0f thw 1000000 runs


# bmle
## appears in creating a raster
plot(mydat_2targets_bmle[,1],
     mydat_2targets_bmle[,2], 
     xlab = "beta", ylab = "gamma",
     # col = "purple",
     main ="BMLE" ,
     xlim = c(0,1.0),
     ylim = c(0,0.2),
     cex.axis = 1.3,
     cex.lab = 1.3)

points(mydat_2targets_ref[,1],
       mydat_2targets_ref[,2],
       col = "red", 
       main ="Scenario_1_True_posterior") # retained 0.5% 0f thw 1000000 runs

###############################################################
# Scenario 2 = 3 targets

# rejection
## appears in creating a raster
plot(mydat_3targets_rej[,1],
     mydat_3targets_rej[,2], 
     xlab = "beta", ylab = "gamma",
     # col = "purple",
     main ="Rejection ABC" ,
     xlim = c(0,1.0),
     ylim = c(0,0.2),
     cex.axis = 1.3,
     cex.lab = 1.3)

points(mydat_3targets_ref[,1],
       mydat_3targets_ref[,2],
       col = "red", 
       main ="Scenario_2_True_posterior") # retained 0.5% 0f thw 1000000 runs



# sequential
## appears in creating a raster
plot(mydat_3targets_seq[,1],
     mydat_3targets_seq[,2], 
     xlab = "beta", ylab = "gamma",
     # col = "purple",
     main ="Sequential ABC" ,
     xlim = c(0,1.0),
     ylim = c(0,0.2),
     cex.axis = 1.3,
     cex.lab = 1.3)

points(mydat_3targets_ref[,1],
       mydat_3targets_ref[,2],
       col = "red", 
       main ="Scenario_2_True_posterior") # retained 0.5% 0f thw 1000000 runs


# bmle
## appears in creating a raster
plot(mydat_3targets_bmle[,1],
     mydat_3targets_bmle[,2], 
     xlab = "beta", ylab = "gamma",
     # col = "purple",
     main ="BMLE" ,
     xlim = c(0,1.0),
     ylim = c(0,0.2),
     cex.axis = 1.3,
     cex.lab = 1.3)

points(mydat_3targets_ref[,1],
       mydat_3targets_ref[,2],
       col = "red", 
       main ="Scenario_1_True_posterior") # retained 0.5% 0f thw 1000000 runs




##############################################################
