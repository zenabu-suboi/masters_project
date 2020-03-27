#######################################################
setwd("C:/Users/Zee/Documents/GitHub/masters_project/R_PLOTS/
      Posterior_data_n_plots/")

#########################################################
library(ggplot2)
library(tidyverse)
library(viridis)
library(ggpubr)
library(reshape2)
library(cowplot)

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

######################################################
# name of columns of posterior data

colnames(mydat_2targets_ref) <- c("beta", "gamma")
colnames(mydat_3targets_ref) <- c("beta", "gamma")
colnames(mydat_2targets_rej) <- c("beta", "gamma")
colnames(mydat_3targets_rej) <- c("beta", "gamma")
colnames(mydat_2targets_seq) <- c("beta", "gamma")
colnames(mydat_3targets_seq) <- c("beta", "gamma")
colnames(mydat_2targets_bmle) <- c("beta", "gamma")
colnames(mydat_3targets_bmle) <- c("beta", "gamma")

##############################################################
#par(mfrow = c(2,2))
# Scenario 1

# rejection2
 
rej_ref2_plot <- ggplot(mydat_2targets_rej,
                        aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5) +
  
  ggtitle("Rejection ABC") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=11)
  ) +
  ylab('Gamma') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_2targets_ref,
             color = "red",size = 0.5)
 # geom_density(alpha=0.1)

# rej_ref2_plot + theme(
#   legend.position = c(.9, .1),
#   legend.justification = c("right"),
#   legend.title = element_text(color = "legend")
#   #legend.box.just = "right",
#   #legend.margin = margin(6, 6, 6, 6)
# )

##############################
# seq2

seq_ref2_plot <- ggplot(mydat_2targets_seq,
                        aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5) +
  
  ggtitle("Sequential ABC") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=11)
  ) +
  ylab('Gamma') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_2targets_ref,
             colour = "red", size = 0.5)

#######################
# bmle2

bmle_ref2_plot <- ggplot(mydat_2targets_bmle,
                        aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5) +
  
  ggtitle("BMLE") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=11)
  ) +
  ylab('Gamma') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_2targets_ref,
             colour = "red", size = 0.5)

###################################################
# scenario 2

# rejection 3

rej_ref3_plot <- ggplot(mydat_3targets_rej,
                        aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5) +
  
  ggtitle("Rejection ABC") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=11)
  ) +
  ylab('Gamma') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_3targets_ref,
             colour = "blue", size = 0.5)


###################################

# seq 3

seq_ref3_plot <- ggplot(mydat_3targets_seq,
                        aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5) +
  
  ggtitle("Sequential ABC") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=11)
  ) +
  ylab('Gamma') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_3targets_ref,
             colour = "blue", size = 0.5)

####################################
# bmle3

bmle_ref3_plot <- ggplot(mydat_3targets_bmle,
                        aes(x=beta, y=gamma)) +
  geom_point(color="black", size = 0.5) +
  
  ggtitle("BMLE") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=11)
  ) +
  ylab('Gamma') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))+
  
  geom_point(aes(x=beta, y=gamma),
             data = mydat_3targets_ref,
             colour = "blue", size = 0.5)

####################################################

# putting all plots together on the same page for scenarios 1

plot_grid(rej_ref2_plot,
          seq_ref2_plot,
          bmle_ref2_plot,
          #labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)



# putting all plots together on the same page for scenarios 2

plot_grid(
  rej_ref3_plot,
  seq_ref3_plot,
  bmle_ref3_plot,
  #labels = c("A", "B", "C"),
  ncol = 2, nrow = 2)


#########################################################
# legend

# par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),
#     new = TRUE)
# plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", 
       c("True posterior 1","True posterior 2"),
       xpd = TRUE, horiz = TRUE, inset = c(0, 0), 
       bty = "n", fill = c( "red" ,"blue"),
       cex = 1.2)

