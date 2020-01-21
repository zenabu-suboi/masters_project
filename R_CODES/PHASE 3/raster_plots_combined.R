library(rasterVis)
?plot

r <- raster(nrows=10, ncols=10)
r <- setValues(r, 1:ncell(r))
plot(r)

e <- extent(r)
plot(e, add=TRUE, col='red', lwd=4)
e <- e / 2
plot(e, add=TRUE, col='red')




par(mfrow = c(2,2))

# scenario 1
# rej & reference
ras.rej1= rasterize(abc0.1$unadj.values[1:5000,1],abc0.1$unadj.values[1:5000,2])
plot(ras.rej1, col=grey(100:1/100), useRaster=F, main="Scenario_1_rejection")


ras2.ref1 = rasterize(abc2ref1$unadj.values[,1], abc2ref1$unadj.values[,2])
plot(ras2.ref1, col, useRaster=F, add = F, legend = T)

#######################

plot(ras.rej1, col, alpha=1,
     col=grey(100:1/100),
     add=FALSE,
     useRaster=F,
     main="Scenario_1_rejection")

plot(ras2.ref1, alpha=0.4,
     col, 
     add = T, 
     useRaster=F,
     cex = 3,
     legend = F)

###############
### raster results for Sequential & reference

ras.seq1 = rasterize(ABC_seq2$param[, 1],ABC_seq2$param[, 2])
plot(ras.seq1, col=blue(100:1/100), useRaster=F, main="Scenario_1_sequential")



ras2.ref1 = rasterize(abc2ref1$unadj.values[,1],abc2ref1$unadj.values[,2])
plot(ras2.ref1, col=grey(100:1/100), useRaster=F, main="ref_scenario_1", add = F)



#######################################################################################
# scenario 2

# rej & reference

ras.rej2= rasterize(abc0.1lin$unadj.values[1:5000,1],abc0.1lin$unadj.values[1:5000,2])
plot(ras.rej2, col=grey(100:1/100), useRaster=F, main="Scenario_2_rejection")


ras2.ref2 = rasterize(abc2ref2$unadj.values[,1],abc2ref2$unadj.values[,2])
plot(ras2.ref2, col=rainbow(100), useRaster=F, main="ref_scenario_2", add = T)



### raster results for Sequential & reference

ras.seq2 = rasterize(ABC_seq1$param[, 1],ABC_seq1$param[, 2])
plot(ras.seq2, col=blue(100:1/100), useRaster=F, main="Scenario_2_sequential")


ras2.ref2 = rasterize(abc2ref2$unadj.values[,1],abc2ref2$unadj.values[,2])
plot(ras2.ref2, col=red(100:1/100), useRaster=F, main="ref_scenario_2", add = T)
