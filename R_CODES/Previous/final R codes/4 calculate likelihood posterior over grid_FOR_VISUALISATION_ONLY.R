
library(fitR)

init.state <- c(S = 990, I = 10, R = 0)
times <- 1:75

#### make sure the correct values are used for obs=... should be targets


### ng-j experiment
matV <- matrix(NA, nrow=nb, ncol=ng)
for(i in 1:nb){
  for(j in 1:ng){
    D_inf= 1/gammagrid[j]
    R0= betagrid[i]* D_inf
    theta <- c(R0 = R0, D_inf=D_inf) 
    traj <- SIR$simulate(theta, init.state, times)
    modpnt= traj$I[50]
    ## obs in below is obs at time 50 # comes from SIR model abc two unknwn param, two unknwn stats
    savit <- SIR$dPointObs(data.point = c(obs=604), model.point = c(I = modpnt), theta, log = TRUE)
    modpnt= traj$I[75]
    ## obs in below is obs at time 75 # comes from SIR model abc two unknwn param, two unknwn stats
    matV[i,j] <- savit + SIR$dPointObs(data.point = c(obs=383), model.point = c(I = modpnt), theta, log = TRUE)
    
  }
  print(i)
}

matV

# plot likelihood
likmatV= exp(matV)
nrow(likmatV)

contour(x= betagrid,
        y= gammagrid,
        z=likmatV)

contour(x= betagrid,
        y= gammagrid,
        z=matV)




### new plot, better than contour (more similar to raster plot from abc)
forplot= data.frame(betgrd= rep(betagrid,nb),
                    gamgrd= rep(gammagrid, each=ng)
                  
)

head(forplot)

library(ggplot2)

likelihoodplot <-ggplot(forplot,
                        aes(x = betgrd,
                            y = gamgrd)) +
  geom_point(alpha = 0.2, size = 3) +
  xlab("beta") +
  ylab("gamma") +
  xlim(min(betagrid), max(betagrid)) +
  ylim(min(gammagrid), max(gammagrid))+
  geom_tile(data = forplot, aes(fill = likhd)) +
  scale_fill_gradient2(low = "white", high = "black", mid = "grey", 
                       midpoint = min(likmatV)+(max(likmatV)/2), limit = c(min(likmatV),max(likmatV)),
                       space = "Lab", 
                       name="LIK") +
  theme_minimal() # minimal theme
plot(likelihoodplot)


likmatstdV= likmatV/sum(likmatV)

sum(likmatstdV)


number=10000
scalelikmatstdV= likmatstdV*number

### new plot, better than contour (more similar to raster plot from abc)
forplot= data.frame(betgrd= rep(betagrid,nb),
                    gamgrd= rep(gammagrid, each=ng),
                    likhd= c(scalelikmatstdV)
)

head(forplot)

library(ggplot2)

likelihoodplot <-ggplot(forplot,
                        aes(x = betgrd,
                            y = gamgrd)) +
  geom_point(alpha = 0.2, size = 3) +
  xlab("beta") +
  ylab("gamma") +
  xlim(min(betagrid), max(betagrid)) +
  ylim(min(gammagrid), max(gammagrid))+
  geom_tile(data = forplot, aes(fill = likhd)) +
  scale_fill_gradient2(low = "white", high = "black", mid = "grey", 
                       midpoint = min(scalelikmatstdV)+(max(scalelikmatstdV)/2), limit = c(min(scalelikmatstdV),max(scalelikmatstdV)),
                       space = "Lab", 
                       name="LIK") +
  theme_minimal() # minimal theme
plot(likelihoodplot)
