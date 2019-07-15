
library(raster)

### raster results for abc0.1

ras.abc0.1= rasterize(abc0.1lin$unadj.values[,1],abc0.1lin$unadj.values[,2])
plot(ras.abc0.1, col=grey(100:1/100), useRaster=F, main="Ordinary ABC0.1")
ras.abc0.1.mat= as.matrix(ras.abc0.1)
sum(ras.abc0.1.mat)

### raster results for abc0.01

ras.abc0.01= rasterize(abc0.01lin$unadj.values[,1],abc0.01lin$unadj.values[,2])
plot(ras.abc0.01, col=grey(100:1/100), useRaster=F, main="Ordinary ABC0.01")
ras.abc0.01.mat= as.matrix(ras.abc0.01)
sum(ras.abc0.01.mat)

### raster results for abc0.01

ras.abc0.001= rasterize(abc0.001lin$unadj.values[,1],abc0.001lin$unadj.values[,2])
plot(ras.abc0.001, col=grey(100:1/100), useRaster=F, main="Ordinary ABC0.001")
ras.abc0.001.mat= as.matrix(ras.abc0.001)
sum(ras.abc0.001.mat)





### raster results for abc0.1lin

ras.abc0.1lin= rasterize(abc0.1lin$adj.values[,1],abc0.1lin$adj.values[,2])
plot(ras.abc0.1lin, col=grey(100:1/100), useRaster=F, main="regression adjusted abc0.1")
ras.abc0.1lin.mat= as.matrix(ras.abc0.1lin)
sum(ras.abc0.1lin.mat)

### raster results for abc0.01lin

ras.abc0.01lin= rasterize(abc0.01lin$adj.values[,1],abc0.01lin$adj.values[,2])
plot(ras.abc0.01lin, col=grey(100:1/100), useRaster=F, main="regression adjusted abc0.01")
ras.abc0.01lin.mat= as.matrix(ras.abc0.01lin)
sum(ras.abc0.01lin.mat)

### raster results for abc0.01lin

ras.abc0.001lin= rasterize(abc0.001lin$adj.values[,1],abc0.001lin$adj.values[,2])
plot(ras.abc0.001lin, col=grey(100:1/100), useRaster=F, main="regression adjusted abc0.001")
ras.abc0.001lin.mat= as.matrix(ras.abc0.001lin)
sum(ras.abc0.001lin.mat)





### raster results for abc0.1nnet

ras.abc0.1nnet= rasterize(abc0.1nnet$adj.values[,1],abc0.1nnet$adj.values[,2])
plot(ras.abc0.1nnet, col=grey(100:1/100), useRaster=F, main="nnet adjusted abc0.1")
ras.abc0.1nnet.mat= as.matrix(ras.abc0.1nnet)
sum(ras.abc0.1nnet.mat)

### raster results for abc0.01nnet

ras.abc0.01nnet= rasterize(abc0.01nnet$adj.values[,1],abc0.01nnet$adj.values[,2])
plot(ras.abc0.01nnet, col=grey(100:1/100), useRaster=F, main="nnet adjusted abc0.01")
ras.abc0.01nnet.mat= as.matrix(ras.abc0.01nnet)
sum(ras.abc0.01nnet.mat)

### raster results for abc0.01nnet

ras.abc0.001nnet= rasterize(abc0.001nnet$adj.values[,1],abc0.001nnet$adj.values[,2])
plot(ras.abc0.001nnet, col=grey(100:1/100), useRaster=F, main="nnet adjusted abc 0.001")
ras.abc0.001nnet.mat= as.matrix(ras.abc0.001nnet)
sum(ras.abc0.001nnet.mat)



