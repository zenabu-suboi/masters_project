
#######################################################
setwd("C:/Users/Zee/Documents/GitHub/masters_project/R_PLOTS/
      Posterior_data_n_plots/")

#########################################################
library(ggplot2)
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

###########################################################


# contour plots

#par(mfrow = c(2,3))
# Area + contour
rej2_cont <- ggplot(mydat_2targets_rej, aes(x=mydat_2targets_rej[,1], #rej2
                                            y=mydat_2targets_rej[,2]) ) +
                   stat_density_2d(aes(fill = ..level..),
                  geom = "polygon", colour="white")+
  ggtitle("Rejection ABC") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  ) +
  ylab('Gamma') +
  xlab('Beta')
 # xlim(c(0,1))+
  #ylim(c(0,0.1))


rej3_cont <- ggplot(mydat_3targets_rej, aes(x=mydat_3targets_rej[,1],
                                            y=mydat_3targets_rej[,2]) ) +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon", colour="white")+
  ggtitle("Rejection ABC") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  ) +
  ylab('Gamma') +
  xlab('Beta')
 # xlim(c(0,1))+
  #ylim(c(0,0.1))

###################### seq

seq2_cont <- ggplot(mydat_2targets_seq, aes(x=mydat_2targets_seq[,1],
                                            y=mydat_2targets_seq[,2]) ) +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon", colour="white")+
  ggtitle("Sequential ABC") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  ) +
  ylab('Gamma') +
  xlab('Beta')
  #xlim(c(0,1))+
  #ylim(c(0,0.1))


seq3_cont <- ggplot(mydat_3targets_seq, aes(x=mydat_3targets_seq[,1],
                                            y=mydat_3targets_seq[,2]) ) +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon", colour="white")+
  ggtitle("Sequential ABC") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  ) +
  ylab('Gamma') +
  xlab('Beta')
  #xlim(c(0,1))+
  #ylim(c(0,0.1))

############################# bmle

bmle2_cont <- ggplot(mydat_2targets_bmle, aes(x=mydat_2targets_bmle[,1],
                                              y=mydat_2targets_bmle[,2]) ) +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon", colour="white")+
  ggtitle("BMLE") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  ) +
  ylab('Gamma') +
  xlab('Beta')
 # xlim(c(0,1))+
  #ylim(c(0,0.1))
  



bmle3_cont <- ggplot(mydat_3targets_bmle, aes(x=mydat_3targets_bmle[,1],
                                              y=mydat_3targets_bmle[,2]) ) +
  stat_density_2d(aes(fill = ..level..),
                  geom = "polygon", colour="white")+
  ggtitle("BMLE") +
  #theme_ipsum() +
  theme(
    plot.title = element_text(size=12)
  ) +
  ylab('Gamma') +
  xlab('Beta')
  #xlim(c(0,1))+
  #ylim(c(0,0.1))


###################################################

# put all contour plots on one page for scenario 1
plot_grid(rej2_cont,
          seq2_cont,
          bmle2_cont,
          ncol = 2, nrow = 2)


# put all contour plots on one page for scenario 2
plot_grid(
          rej3_cont,
          seq3_cont,
          bmle3_cont,
          #labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)


###########################################################
# # hexbin charts
# 
# # Hexbin chart with default option
# ggplot(mydat, aes(x = mydat[,1], y = mydat[,2]) ) +
#   geom_hex() +
#   theme_bw()
# 
# # Bin size control + color palette
# ggplot(mydat, aes(x = mydat[,1], y = mydat[,2]) ) +
#   geom_hex(bins = 50) +
#   scale_fill_continuous(type = "viridis") +
#   theme_bw()