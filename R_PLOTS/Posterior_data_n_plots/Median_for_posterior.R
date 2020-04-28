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

#########################################################

# ref 2
ref2_beta_sort <- sort(mydat_2targets_ref$beta)
ref2_gamma_sort <- sort(mydat_2targets_ref$gamma)

ref2_beta_95CI <- ref2_beta_sort[126:4875]
ref2_gamma_95CI <- ref2_gamma_sort[126:4875]

ref2_beta_median <- (ref2_beta_95CI[2375]+ref2_beta_95CI[2376])/2
ref2_gamma_median <- (ref2_gamma_95CI[2375]+ref2_gamma_95CI[2376])/2


#############
# ref 3
ref3_beta_sort <- sort(mydat_3targets_ref$beta)
ref3_gamma_sort <- sort(mydat_3targets_ref$gamma)

ref3_beta_95CI <- ref3_beta_sort[126:4875]
ref3_gamma_95CI <- ref3_gamma_sort[126:4875]

ref3_beta_median <- (ref3_beta_95CI[2375]+ref3_beta_95CI[2376])/2
ref3_gamma_median <- (ref3_gamma_95CI[2375]+ref3_gamma_95CI[2376])/2


############################################

# rej 2
rej2_beta_sort <- sort(mydat_2targets_rej$beta)
rej2_gamma_sort <- sort(mydat_2targets_rej$gamma)

rej2_beta_95CI <- rej2_beta_sort[126:4875]
rej2_gamma_95CI <- rej2_gamma_sort[126:4875]

rej2_beta_median <- (rej2_beta_95CI[2375]+rej2_beta_95CI[2376])/2
rej2_gamma_median <- (rej2_gamma_95CI[2375]+rej2_gamma_95CI[2376])/2


#############
# rej 3
rej3_beta_sort <- sort(mydat_3targets_rej$beta)
rej3_gamma_sort <- sort(mydat_3targets_rej$gamma)

rej3_beta_95CI <- rej3_beta_sort[126:4875]
rej3_gamma_95CI <- rej3_gamma_sort[126:4875]

rej3_beta_median <- (rej3_beta_95CI[2375]+rej3_beta_95CI[2376])/2
rej3_gamma_median <- (rej3_gamma_95CI[2375]+rej3_gamma_95CI[2376])/2


############################################
# seq2
seq2_beta_sort <- sort(mydat_2targets_seq$beta)
seq2_gamma_sort <- sort(mydat_2targets_seq$gamma)

seq2_beta_95CI <- seq2_beta_sort[126:4875]
seq2_gamma_95CI <- seq2_gamma_sort[126:4875]

seq2_beta_median <- (seq2_beta_95CI[2375]+seq2_beta_95CI[2376])/2
seq2_gamma_median <- (seq2_gamma_95CI[2375]+seq2_gamma_95CI[2376])/2

#################
# seq 3
seq3_beta_sort <- sort(mydat_3targets_seq$beta)
seq3_gamma_sort <- sort(mydat_3targets_seq$gamma)

seq3_beta_95CI <- seq3_beta_sort[126:4875]
seq3_gamma_95CI <- seq3_gamma_sort[126:4875]

seq3_beta_median <- (seq3_beta_95CI[2375]+seq3_beta_95CI[2376])/2
seq3_gamma_median <- (seq3_gamma_95CI[2375]+seq3_gamma_95CI[2376])/2


############################################
# bmle2
bmle2_beta_sort <- sort(mydat_2targets_bmle$beta)
bmle2_gamma_sort <- sort(mydat_2targets_bmle$gamma)

bmle2_beta_95CI <- bmle2_beta_sort[126:4875]
bmle2_gamma_95CI <- bmle2_gamma_sort[126:4875]

bmle2_beta_median <- (bmle2_beta_95CI[2375]+bmle2_beta_95CI[2376])/2
bmle2_gamma_median <- (bmle2_gamma_95CI[2375]+bmle2_gamma_95CI[2376])/2

#################
# bmle 3
bmle3_beta_sort <- sort(mydat_3targets_bmle$beta)
bmle3_gamma_sort <- sort(mydat_3targets_bmle$gamma)

bmle3_beta_95CI <- bmle3_beta_sort[126:4875]
bmle3_gamma_95CI <- bmle3_gamma_sort[126:4875]

bmle3_beta_median <- (bmle3_beta_95CI[2375]+bmle3_beta_95CI[2376])/2
bmle3_gamma_median <- (bmle3_gamma_95CI[2375]+bmle3_gamma_95CI[2376])/2


