### use same as raster to determine plot limits:

minrangeBeta= c(range(abc0.1lin$unadj.values[,1])[1],range(abc0.01lin$unadj.values[,1])[1],range(abc0.001lin$unadj.values[,1])[1]
                , range(abc0.1lin$adj.values[,1])[1],range(abc0.01lin$adj.values[,1])[1],range(abc0.001lin$adj.values[,1])[1]
                , range(abc0.1nnet$adj.values[,1])[1],range(abc0.01nnet$adj.values[,1])[1],range(abc0.001nnet$adj.values[,1])[1])

maxrangeBeta= c(range(abc0.1lin$unadj.values[,1])[2],range(abc0.01lin$unadj.values[,1])[2],range(abc0.001lin$unadj.values[,1])[2]
                , range(abc0.1lin$adj.values[,1])[2],range(abc0.01lin$adj.values[,1])[2],range(abc0.001lin$adj.values[,1])[2]
                , range(abc0.1nnet$adj.values[,1])[2],range(abc0.01nnet$adj.values[,1])[2],range(abc0.001nnet$adj.values[,1])[2])

minrangeGamma= c(range(abc0.1lin$unadj.values[,2])[1],range(abc0.01lin$unadj.values[,2])[1],range(abc0.001lin$unadj.values[,2])[1]
                 , range(abc0.1lin$adj.values[,2])[1],range(abc0.01lin$adj.values[,2])[1],range(abc0.001lin$adj.values[,2])[1]
                 , range(abc0.1nnet$adj.values[,2])[1],range(abc0.01nnet$adj.values[,2])[1],range(abc0.001nnet$adj.values[,2])[1])

maxrangeGamma= c(range(abc0.1lin$unadj.values[,2])[2],range(abc0.01lin$unadj.values[,2])[2],range(abc0.001lin$unadj.values[,2])[2]
                 , range(abc0.1lin$adj.values[,2])[2],range(abc0.01lin$adj.values[,2])[2],range(abc0.001lin$adj.values[,2])[2]
                 , range(abc0.1nnet$adj.values[,2])[2],range(abc0.01nnet$adj.values[,2])[2],range(abc0.001nnet$adj.values[,2])[2])


### plot results for abc0.1lin
plot(abc0.1lin$unadj.values[,1],abc0.1lin$unadj.values[,2], xlim=c(min(minrangeBeta),max(maxrangeBeta)), ylim=c(min(minrangeGamma),max(maxrangeGamma)), xlab="Beta", ylab="Gamma",
     main="abc0.1lin")
points(abc0.1lin$adj.values[,1],abc0.1lin$adj.values[,2], col="red")
contour(x= betagrid,
        y= gammagrid,
        z=likmatV, add=TRUE, col="blue")

### plot results for abc0.1lin
plot(abc0.01lin$unadj.values[,1],abc0.01lin$unadj.values[,2], xlim=c(min(minrangeBeta),max(maxrangeBeta)), ylim=c(min(minrangeGamma),max(maxrangeGamma)), xlab="Beta", ylab="Gamma",
main="abc0.01lin")
points(abc0.01lin$adj.values[,1],abc0.01lin$adj.values[,2], col="red")
contour(x= betagrid,
        y= gammagrid,
        z=likmatV, add=TRUE, col="blue")

### plot results for abc0.1lin
plot(abc0.001lin$unadj.values[,1],abc0.001lin$unadj.values[,2], xlim=c(min(minrangeBeta),max(maxrangeBeta)), ylim=c(min(minrangeGamma),max(maxrangeGamma)), xlab="Beta", ylab="Gamma",
main="abc0.001lin")
points(abc0.001lin$adj.values[,1],abc0.001lin$adj.values[,2], col="red")
contour(x= betagrid,
        y= gammagrid,
        z=likmatV, add=TRUE, col="blue")



### plot results for abc0.1lin
plot(abc0.1nnet$unadj.values[,1],abc0.1nnet$unadj.values[,2], xlim=c(min(minrangeBeta),max(maxrangeBeta)), ylim=c(min(minrangeGamma),max(maxrangeGamma)), xlab="Beta", ylab="Gamma",
     main="abc0.1nnet")
points(abc0.1nnet$adj.values[,1],abc0.1nnet$adj.values[,2], col="red")
contour(x= betagrid,
        y= gammagrid,
        z=likmatV, add=TRUE, col="blue")

### plot results for abc0.1lin
plot(abc0.01nnet$unadj.values[,1],abc0.01nnet$unadj.values[,2], xlim=c(min(minrangeBeta),max(maxrangeBeta)), ylim=c(min(minrangeGamma),max(maxrangeGamma)), xlab="Beta", ylab="Gamma",
     main="abc0.01nnet")
points(abc0.01nnet$adj.values[,1],abc0.01nnet$adj.values[,2], col="red")
contour(x= betagrid,
        y= gammagrid,
        z=likmatV, add=TRUE, col="blue")

### plot results for abc0.1lin
plot(abc0.001nnet$unadj.values[,1],abc0.001nnet$unadj.values[,2], xlim=c(min(minrangeBeta),max(maxrangeBeta)), ylim=c(min(minrangeGamma),max(maxrangeGamma)), xlab="Beta", ylab="Gamma",
     main="abc0.001nnet")
points(abc0.001nnet$adj.values[,1],abc0.001nnet$adj.values[,2], col="red")
contour(x= betagrid,
        y= gammagrid,
        z=likmatV, add=TRUE, col="blue")





### plot results for abc0.1lin
plot(abc0.1ridge$unadj.values[,1],abc0.1ridge$unadj.values[,2], xlim=c(min(minrangeBeta),max(maxrangeBeta)), ylim=c(min(minrangeGamma),max(maxrangeGamma)), xlab="Beta", ylab="Gamma",
     main="abc0.1ridge")
points(abc0.1ridge$adj.values[,1],abc0.1ridge$adj.values[,2], col="red")
contour(x= betagrid,
        y= gammagrid,
        z=likmatV, add=TRUE, col="blue")

### plot results for abc0.1lin
plot(abc0.01ridge$unadj.values[,1],abc0.01ridge$unadj.values[,2], xlim=c(min(minrangeBeta),max(maxrangeBeta)), ylim=c(min(minrangeGamma),max(maxrangeGamma)), xlab="Beta", ylab="Gamma",
     main="abc0.01ridge")
points(abc0.01ridge$adj.values[,1],abc0.01ridge$adj.values[,2], col="red")
contour(x= betagrid,
        y= gammagrid,
        z=likmatV, add=TRUE, col="blue")

### plot results for abc0.1lin
plot(abc0.001ridge$unadj.values[,1],abc0.001ridge$unadj.values[,2], xlim=c(min(minrangeBeta),max(maxrangeBeta)), ylim=c(min(minrangeGamma),max(maxrangeGamma)), xlab="Beta", ylab="Gamma",
     main="abc0.001ridge")
points(abc0.001ridge$adj.values[,1],abc0.001ridge$adj.values[,2], col="red")
contour(x= betagrid,
        y= gammagrid,
        z=likmatV, add=TRUE, col="blue")


