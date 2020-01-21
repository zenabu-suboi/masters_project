
### set raster/grid width based on maximum obtained parameter ranges

minrangeBeta= c(range(abc0.1$unadj.values[1:5000, 1])[1],
                range(ABC_seq2$param[, 1])[1],
                range(abc0.1lin$unadj.values[1:5000, 1])[1],
                range(ABC_seq1$param[, 1])[1],
                range(abc2ref1$unadj.values[, 1])[1],
                range(abc2ref2$unadj.values[,1])[1]
                )


maxrangeBeta= c(range(abc0.1$unadj.values[1:5000, 1])[2],
                range(ABC_seq2$param[, 1])[2],
                range(abc0.1lin$unadj.values[1:5000, 1])[2],
                range(ABC_seq1$param[, 1])[2],
                range(abc2ref1$unadj.values[, 1])[2],
                range(abc2ref2$unadj.values[, 1])[2]
                )



minrangeGamma= c(range(abc0.1$unadj.values[1:5000, 2])[1],
                 range(abc0.1lin$unadj.values[1:5000, 2])[1],
                 range(ABC_seq2$param[, 2])[1],
                 range(ABC_seq1$param[, 2])[1],
                 range(abc2ref1$unadj.values[,2])[1],
                 range(abc2ref2$unadj.values[,2])[1]
                 )


maxrangeGamma= c(range(abc0.1$unadj.values[1:5000,2])[2],
                 range(abc0.1lin$unadj.values[1:5000,2])[2],
                 range(ABC_seq2$param[, 2])[2],
                 range(ABC_seq1$param[, 2])[2],
                 range(abc2ref1$unadj.values[,2])[2],
                 range(abc2ref2$unadj.values[,2])[2]
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

