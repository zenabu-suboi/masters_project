
setwd("C:/Users/Zee/Documents/GitHub/masters_project/R_CODES/
      Percentage_overlap/")

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
colnames(mydat_2targets_bmle) <- c("beta", "gamma",
                                   "likelihood2", "weight2")
colnames(mydat_3targets_bmle) <- c("beta", "gamma",
                                   "likelihood3", "weight3")

###################################################################

### set raster/grid width based on minimum and maximum obtained parameter ranges

minrangeBeta= c(range(mydat_2targets_ref$beta)[1],
                range(mydat_2targets_rej$beta)[1],
                range(mydat_2targets_seq$beta)[1],
                range(mydat_2targets_bmle$beta)[1],
                # tom's 2targ
                range(mydat_3targets_ref$beta)[1],
                range(mydat_3targets_rej$beta)[1],
                range(mydat_3targets_seq$beta)[1],
                range(mydat_3targets_bmle$beta)[1]
                # tom's 3targs
                )


maxrangeBeta= c(range(mydat_2targets_ref$beta)[2],
                range(mydat_2targets_rej$beta)[2],
                range(mydat_2targets_seq$beta)[2],
                range(mydat_2targets_bmle$beta)[2],
                # tom's 2targ
                range(mydat_3targets_ref$beta)[2],
                range(mydat_3targets_rej$beta)[2],
                range(mydat_3targets_seq$beta)[2],
                range(mydat_3targets_bmle$beta)[2]
                # tom's 3targs
)



minrangeGamma= c(range(mydat_2targets_ref$gamma)[1],
                 range(mydat_2targets_rej$gamma)[1],
                 range(mydat_2targets_seq$gamma)[1],
                 range(mydat_2targets_bmle$gamma)[1],
                 # tom's 2targ
                 range(mydat_3targets_ref$gamma)[1],
                 range(mydat_3targets_rej$gamma)[1],
                 range(mydat_3targets_seq$gamma)[1],
                 range(mydat_3targets_bmle$gamma)[1]
                 # tom's 3targs
)


maxrangeGamma= c(range(mydat_2targets_ref$gamma)[2],
                 range(mydat_2targets_rej$gamma)[2],
                 range(mydat_2targets_seq$gamma)[2],
                 range(mydat_2targets_bmle$gamma)[2],
                 # tom's 2targ
                 range(mydat_3targets_ref$gamma)[2],
                 range(mydat_3targets_rej$gamma)[2],
                 range(mydat_3targets_seq$gamma)[2],
                 range(mydat_3targets_bmle$gamma)[2]
                 # tom's 3targs
)


### get the values to calculate the likelihood for (based on the raster below!)
##  see explanation on paper

## check this for a small values of nbeta and ngamma (e.g. 3 each)
##  !! the raster below is now nbeta-1 * ngamma-1 !!! 
## e.g. to get a 3x3 raster you need to put nbeta=4 ngamma=4
nbeta <- 101
ngamma <- 101

bgrid <- seq(min(minrangeBeta),max(maxrangeBeta), length=nbeta)
ggrid <- seq(min(minrangeGamma),max(maxrangeGamma), length=ngamma)   ### how large should this be? now 0.15

### check that these betagrid values (used in the calculation of the likelihood)
### are in the middle of the bgrid values (which are used in the raster below)
nb= nbeta-1
ng= ngamma-1

betagrid= seq((bgrid[1]+bgrid[2])/2,(bgrid[nbeta]+bgrid[nbeta-1])/2, length=nb)
gammagrid= seq((ggrid[1]+ggrid[2])/2,(ggrid[ngamma]+ggrid[ngamma-1])/2, length=ng)



library(raster)
#### make function: rasterize

rasterize= function(param1, param2){
  r <- raster(xmn=min(bgrid), ymn=min(ggrid), xmx=max(bgrid), ymx=max(ggrid), nrows=nb, ncols=ng)
  r[] <- 0
  
  df1= data.frame(param1, param2)
  head(df1)
  colnames(df1) =c("p1","p2")
  coordinates(df1) <- ~ p1 + p2
  tab <- table(cellFromXY(r, df1))
  sum(tab)
  r[as.numeric(names(tab))] <- tab
  return(r)
}

