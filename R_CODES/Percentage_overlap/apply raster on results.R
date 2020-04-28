
library(raster)

#Scenario 1 (two targets)

#par(mfrow=c(1,1))

### raster results for rejection 2 targets

ras.rej2 <- rasterize(mydat_2targets_rej$beta,
                    mydat_2targets_rej$gamma)
plot(ras.rej2, 
     col=grey(100:1/100), 
     useRaster=F,
     main="Rejection ABC")
ras.rej2.mat <- as.matrix(ras.rej2)
sum(ras.rej2.mat)

### raster results for Sequential 2 targets

ras.seq2 <- rasterize(mydat_2targets_seq$beta,
                    mydat_2targets_seq$gamma)
plot(ras.seq2, 
     col=grey(100:1/100), 
     useRaster=F,
     main="Sequential ABC")
ras.seq2.mat <- as.matrix(ras.seq2)
sum(ras.seq2.mat)

### raster results for bmle 2 targets

ras.bmle2 <- rasterize(mydat_2targets_bmle$beta,
                    mydat_2targets_bmle$gamma)
plot(ras.bmle2, 
     col=grey(100:1/100), 
     useRaster=F,
     main="BMLE")
ras.bmle2.mat <- as.matrix(ras.bmle2)
sum(ras.bmle2.mat)


##############################################################
# scenario 2 , 3 targets

#par(mfrow=c(2,2))

### raster results for rejection 3 targets

ras.rej3 <- rasterize(mydat_3targets_rej$beta,
                    mydat_3targets_rej$gamma)
plot(ras.rej3, 
     col=grey(100:1/100), 
     useRaster=F,
     main="Rejection ABC")
ras.rej3.mat <- as.matrix(ras.rej3)
sum(ras.rej3.mat)

### raster results for Sequential 3 targets

ras.seq3 <- rasterize(mydat_3targets_seq$beta,
                    mydat_3targets_seq$gamma)
plot(ras.seq3, 
     col=grey(100:1/100), 
     useRaster=F,
     main="Sequential ABC")
ras.seq3.mat <- as.matrix(ras.seq3)
sum(ras.seq3.mat)

### raster results for bmle 3 targets

ras.bmle3 <- rasterize(mydat_3targets_bmle$beta,
                     mydat_3targets_bmle$gamma)
plot(ras.bmle3, 
     col=grey(100:1/100), 
     useRaster=F,
     main="BMLE")
ras.bmle3.mat <- as.matrix(ras.bmle3)
sum(ras.bmle3.mat)



##############################################################################
######################################################################

# ref sample 
#par(mfrow=c(1,2))

ras.ref2 <- rasterize(mydat_2targets_ref$beta,
                    mydat_2targets_ref$gamma)
plot(ras.ref2, 
     col=grey(100:1/100), 
     useRaster=F,
     main="Reference posterior")
ras.ref2.mat <- as.matrix(ras.ref2)
sum(ras.ref2.mat)


ras.ref3 <- rasterize(mydat_3targets_ref$beta,
                    mydat_3targets_ref$gamma)
plot(ras.ref3, 
     col=grey(100:1/100), 
     useRaster=F,
     main="Reference posterior")
ras.ref3.mat <- as.matrix(ras.ref3)
sum(ras.ref3.mat)


