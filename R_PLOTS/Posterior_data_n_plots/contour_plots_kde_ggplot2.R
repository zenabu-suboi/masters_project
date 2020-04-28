#######################################################
setwd("C:/Users/Zee/Documents/GitHub/masters_project/R_PLOTS/
      Posterior_data_n_plots/")

#########################################################
library(ggplot2)
library(cowplot)
library(reshape2)
library(tidyverse)
library(ks)

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
#data
d <- mydat_2targets_rej
## density function
kd <- ks::kde(mydat_2targets_rej, compute.cont=TRUE, h=0.2)

## extract results
get_contour <- function(kd_out=kd, prob="5%") {
  contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                          z=estimate, levels=cont[prob])[[1]])
  as_tibble(contour_95) %>% 
    mutate(prob = prob)
}

dat_out <- map_dfr(c("10%", "20%","80%", "90%"), ~get_contour(kd, .)) %>% 
  group_by(prob) %>% 
  mutate(n_val = 1:n()) %>% 
  ungroup()

## clean kde output
kd_df <- expand_grid(x=kd$eval.points[[1]], y=kd$eval.points[[2]]) %>% 
  mutate(z = c(kd$estimate %>% t))

rej2_cont <- ggplot(data=kd_df, aes(x, y)) +
  geom_tile(aes(fill=z)) +
 # geom_point(data = d, alpha = I(0.4), size = I(0.4), colour = I("yellow")) +
 # geom_path(aes(x, y, group = prob), 
  #         data=filter(dat_out, !n_val %in% 1:3), colour = I("white")) +
  #geom_text(aes(label = prob), data = 
   #           filter(dat_out, (prob%in% c("10%", "20%","80%") & n_val==1) | (prob%in% c("90%") & n_val==20)),
    #        colour = I("black"), size =I(3))+
  scale_fill_viridis_c()+
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(size=12))+
  ggtitle("Rejection ABC") +
 
  xlab(label = "Beta")+
  ylab(label = "Gamma")
  
###################################
#seq2
#data
d <- mydat_2targets_seq
## density function
kdseq2 <- ks::kde(mydat_2targets_seq, compute.cont=TRUE, h=0.2)

## extract results
get_contour <- function(kd_out=kdseq2, prob="5%") {
  contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                          z=estimate, levels=cont[prob])[[1]])
  as_tibble(contour_95) %>% 
    mutate(prob = prob)
}

dat_out <- map_dfr(c("10%", "20%","80%", "90%"), ~get_contour(kdseq2, .)) %>% 
  group_by(prob) %>% 
  mutate(n_val = 1:n()) %>% 
  ungroup()

## clean kde output
kd_df <- expand_grid(x=kdseq2$eval.points[[1]], y=kdseq2$eval.points[[2]]) %>% 
  mutate(z = c(kdseq2$estimate %>% t))

seq2_cont <- ggplot(data=kd_df, aes(x, y)) +
  geom_tile(aes(fill=z)) +
  
  scale_fill_viridis_c()+
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(size=12))+
  ggtitle("Sequential ABC") +
  xlab(label = "Beta")+
  ylab(label = "Gamma")

#######################################
#data
d <- mydat_2targets_bmle
## density function
kdbmle2 <- ks::kde(mydat_2targets_bmle, compute.cont=TRUE, h=0.2)

## extract results
get_contour <- function(kd_out=kdbmle2, prob="5%") {
  contour_95 <- with(kd_out, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                          z=estimate, levels=cont[prob])[[1]])
  as_tibble(contour_95) %>% 
    mutate(prob = prob)
}

dat_out <- map_dfr(c("10%", "20%","80%", "90%"), ~get_contour(kdbmle2, .)) %>% 
  group_by(prob) %>% 
  mutate(n_val = 1:n()) %>% 
  ungroup()

## clean kde output
kd_df <- expand_grid(x=kdbmle2$eval.points[[1]], y=kdbmle2$eval.points[[2]]) %>% 
  mutate(z = c(kdbmle2$estimate %>% t))

bmle2_cont <- ggplot(data=kd_df, aes(x, y)) +
  geom_tile(aes(fill=z)) +
  
  scale_fill_viridis_c()+
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(size=12))+
  ggtitle("BMLE") +
  xlab(label = "Beta")+
  ylab(label = "Gamma")
