#######################################################
setwd("C:/Users/Zee/Documents/GitHub/masters_project/R_PLOTS/
      Posterior_data_n_plots/")

#########################################################
library(ggplot2)
library(cowplot)
library(reshape2)
library(tidyverse)

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

###########################################################
# contour plots
###############################################################
###############################################################
# scenario 1
# rej2

rej2_raster <- ggplot(mydat_2targets_rej, aes(x=mydat_2targets_rej[,1],
                               y=mydat_2targets_rej[,2]) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  scale_fill_viridis_c()+
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(size=12))+
  ggtitle("Rejection ABC") +
  
  xlab(label = "Beta")+
  ylab(label = "Gamma")+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "red")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "red")

annotation_b <- data.frame(
  x = c(0.175),
  y = c(0.08),
  label = "0.2"
)

rej2_raster <- rej2_raster + geom_text(data=annotation_b,
                                           aes( x=x, y=y, label=label),                 , 
                                           color="cyan1", 
                                           size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.09),
  y = c(0.025),
  label = "0.02"
)

rej2_raster <- rej2_raster + geom_text(data=annotation_g,
                                           aes( x=x, y=y, label=label),                 , 
                                           color="cyan1", 
                                           size=3, angle=0, fontface="bold")

#
  ####################################


seq2_raster <- ggplot(mydat_2targets_seq, aes(x=mydat_2targets_seq[,1],
                                              y=mydat_2targets_seq[,2]) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  scale_fill_viridis_c()+
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(size=12))+
  ggtitle("Sequential ABC") +
  
  xlab(label = "Beta")+
  ylab(label = "Gamma")+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "red")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "red")

annotation_b <- data.frame(
  x = c(0.175),
  y = c(0.023),
  label = "0.2"
)

seq2_raster <- seq2_raster + geom_text(data=annotation_b,
                                       aes( x=x, y=y, label=label),                 , 
                                       color="cyan1", 
                                       size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.75),
  y = c(0.021),
  label = "0.02"
)

seq2_raster <- seq2_raster + geom_text(data=annotation_g,
                                       aes( x=x, y=y, label=label),                 , 
                                       color="cyan1", 
                                       size=3, angle=0, fontface="bold")

####################################


bmle2_raster <- ggplot(mydat_2targets_bmle, aes(x=mydat_2targets_bmle[,1],
                                              y=mydat_2targets_bmle[,2]) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  scale_fill_viridis_c()+
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(size=12))+
  ggtitle("BMLE") +
  
  xlab(label = "Beta")+
  ylab(label = "Gamma")+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "red")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "red")

annotation_b <- data.frame(
  x = c(0.175),
  y = c(0.013),
  label = "0.2"
)

bmle2_raster <- bmle2_raster + geom_text(data=annotation_b,
                                       aes( x=x, y=y, label=label),                 , 
                                       color="cyan1", 
                                       size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.6),
  y = c(0.0205),
  label = "0.02"
)

bmle2_raster <- bmle2_raster + geom_text(data=annotation_g,
                                       aes( x=x, y=y, label=label),                 , 
                                       color="cyan1", 
                                       size=3, angle=0, fontface="bold")


##############################################################
# scenario2


rej3_raster <- ggplot(mydat_3targets_rej, aes(x=mydat_3targets_rej[,1],
                                              y=mydat_3targets_rej[,2]) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  scale_fill_viridis_c()+
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(size=12))+
  ggtitle("Rejection ABC") +
  
  xlab(label = "Beta")+
  ylab(label = "Gamma")+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "red")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "red")

annotation_b <- data.frame(
  x = c(0.175),
  y = c(0.08),
  label = "0.2"
)

rej3_raster <- rej3_raster + geom_text(data=annotation_b,
                                       aes( x=x, y=y, label=label),                 , 
                                       color="cyan1", 
                                       size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.75),
  y = c(0.028),
  label = "0.02"
)

rej3_raster <- rej3_raster + geom_text(data=annotation_g,
                                       aes( x=x, y=y, label=label),                 , 
                                       color="cyan1", 
                                       size=3, angle=0, fontface="bold")

####################################

seq3_raster <- ggplot(mydat_3targets_seq, aes(x=mydat_3targets_seq[,1],
                                              y=mydat_3targets_seq[,2]) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  scale_fill_viridis_c()+
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(size=12))+
  ggtitle("Sequential ABC") +
  
  xlab(label = "Beta")+
  ylab(label = "Gamma")+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "red")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "red")

annotation_b <- data.frame(
  x = c(0.195),
  y = c(0.025),
  label = "0.2"
)

seq3_raster <- seq3_raster + geom_text(data=annotation_b,
                                       aes( x=x, y=y, label=label),                 , 
                                       color="cyan1", 
                                       size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.26),
  y = c(0.0205),
  label = "0.02"
)

seq3_raster <- seq3_raster + geom_text(data=annotation_g,
                                       aes( x=x, y=y, label=label),                 , 
                                       color="cyan1", 
                                       size=3, angle=0, fontface="bold")

####################################


bmle3_raster <- ggplot(mydat_3targets_bmle, aes(x=mydat_3targets_bmle[,1],
                                                y=mydat_3targets_bmle[,2]) ) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  
  scale_fill_viridis_c()+
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(size=12))+
  ggtitle("BMLE") +
  
  xlab(label = "Beta")+
  ylab(label = "Gamma")+
  geom_hline(yintercept=0.02, alpha = 0.7,color = "red")+
  geom_vline(xintercept=0.2, alpha = 0.7,color = "red")

annotation_b <- data.frame(
  x = c(0.197),
  y = c(0.023),
  label = "0.2"
)

bmle3_raster <- bmle3_raster + geom_text(data=annotation_b,
                                         aes( x=x, y=y, label=label),                 , 
                                         color="cyan1", 
                                         size=3, angle=90, fontface="bold")

annotation_g <- data.frame(
  x = c(0.235),
  y = c(0.0205),
  label = "0.02"
)

bmle3_raster <- bmle3_raster + geom_text(data=annotation_g,
                                         aes( x=x, y=y, label=label),                 , 
                                         color="cyan1", 
                                         size=3, angle=0, fontface="bold")



####################################

# put all contour plots on one page for scenario 1
plot_grid(rej2_raster,
          seq2_raster,
          bmle2_raster,
          ncol = 2, nrow = 2)


# put all contour plots on one page for scenario 2
plot_grid(
  rej3_raster,
  seq3_raster,
  bmle3_raster,
  ncol = 2, nrow = 2)
