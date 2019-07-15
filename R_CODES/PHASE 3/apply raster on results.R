
library(raster)

#Scenario 1 (two targets)

par(mfrow=c(2,2))

### raster results for rejection

ras.rej1= rasterize(abc0.1$unadj.values[1:5000,1],abc0.1$unadj.values[1:5000,2])
plot(ras.rej1, col=grey(100:1/100), useRaster=F, main="Scenario_1_rejection")
ras.rej1.mat= as.matrix(ras.rej1)
sum(ras.rej1.mat)

### raster results for Sequential

ras.seq1 = rasterize(ABC_seq2$param[, 1],ABC_seq2$param[, 2])
plot(ras.seq1, col=grey(100:1/100), useRaster=F, main="Scenario_1_sequential")
ras.seq1.mat= as.matrix(ras.seq1)
sum(ras.seq1.mat)


### raster results for reference1 0.5%

ras.ref1.0.5 = rasterize(abcref1.0.5p$unadj.values[,1],abcref1.0.5p$unadj.values[,2])
plot(ras.ref1.0.5, col=grey(100:1/100), useRaster=F, main="ref_1_0.5p")
ras.ref1.0.5.mat= as.matrix(ras.ref1.0.5)
sum(ras.ref1.0.5.mat)


### raster results for reference1 0.1%

ras.ref1.0.1 = rasterize(abcref1.0.1p$unadj.values[,1],abcref1.0.1p$unadj.values[,2])
plot(ras.ref1.0.1, col=grey(100:1/100), useRaster=F, main="ref_1_0.1p")
ras.ref1.0.1.mat= as.matrix(ras.ref1.0.1)
sum(ras.ref1.0.1.mat)

################################################
# scenario 2 (three targets)

ras.rej2= rasterize(abc0.1lin$unadj.values[1:5000,1],abc0.1lin$unadj.values[1:5000,2])
plot(ras.rej2, col=grey(100:1/100), useRaster=F, main="Scenario_2_rejection")
ras.rej2.mat= as.matrix(ras.rej2)
sum(ras.rej2.mat)

### raster results for Sequential

ras.seq2 = rasterize(ABC_seq1$param[, 1],ABC_seq1$param[, 2])
plot(ras.seq2, col=grey(100:1/100), useRaster=F, main="Scenario_2_sequential")
ras.seq2.mat= as.matrix(ras.seq2)
sum(ras.seq2.mat)


### raster results for reference2 0.5%

ras.ref2.0.5 = rasterize(abcref2.0.5p$unadj.values[,1],abcref2.0.5p$unadj.values[,2])
plot(ras.ref2.0.5, col=grey(100:1/100), useRaster=F, main="ref_2_0.5p")
ras.ref2.0.5.mat= as.matrix(ras.ref2.0.5)
sum(ras.ref2.0.5.mat)



### raster results for reference1 0.1%

ras.ref2.0.1 = rasterize(abcref2.0.1p$unadj.values[,1],abcref2.0.1p$unadj.values[,2])
plot(ras.ref2.0.1, col=grey(100:1/100), useRaster=F, main="ref_2_0.1p")
ras.ref2.0.1.mat= as.matrix(ras.ref2.0.1)
sum(ras.ref2.0.1.mat)
