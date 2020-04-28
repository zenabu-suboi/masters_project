require(IMIS)
require(SimDesign)
require(MASS)
require(mvtnorm)
require(SimInf)
require(faraway)
library(ggplot2)

sample.prior <- function(n){ # samples prior as a matrix 
  r1 <- logit(runif(n,0,1))
  r2 <- logit(runif(n,0,0.5))
  r3 <- as.matrix(cbind(r1,r2))
  colnames(r3) <- c("beta","gamma")
  return(r3)
}	

prior <- function(theta){ # gives the density of each row of prior combination
  r1 <- dunif(ilogit(theta[1,1]),0,1)
  r2 <- dunif(ilogit(theta[1,2]),0,0.5)
  r3 <- (as.numeric(r1*r2))
  return(r3)
}

likelihood <- function (theta) {
  likvect <- numeric()
  for(i in 1:nrow(theta)){
    liksav <- 0
    n=1
    u0 <- data.frame(S = rep(990, n),
                     I = rep(10, n), 
                     R = rep(0, n))
    
    tspan <- seq(from = 1, to = 75, by = 1)
    
    thetloc <- ilogit(theta)
    
    model <- SIR(u0 = u0, 
                 tspan = tspan,
                 beta = thetloc[i,1], 
                 gamma = thetloc[i,2])
    
    result <- run(model = model)
    
    dfresult <- trajectory(model = result, node = 1)
    
    model1 <- dfresult[dfresult$time==50,"I"]/1000 # prev at 50
    model2 <- dfresult[dfresult$time==75,"I"]/1000 # prev at 75
    liksav <- numeric()
    ntarget <- 1000
    ktarget1 <- 644
    liksav <- dbinom(x= ktarget1, 
                     size= ntarget,
                     prob= model1, 
                     log=F) # likelihood at time 50
    ktarget2 <- 404
    liksav <- liksav + dbinom(x= ktarget2,
                              size= ntarget, 
                              prob= model2,
                              log=F) # joint likelihood
    likvect <- append(likvect, 
                      liksav, 
                      after= length(likvect)) # store in a vecto
  }
  return(likvect)
}

result2 = IMIS(5000, 5000, 10, 0)


# plot IMIS posterior
imis2_post <- as.data.frame(ilogit(result2$resample))
saveRDS(imis_post, file = "imis2_posterior.rds" )

imis2_plot <- ggplot(imis2_post,
                   aes(x=imis2_post$beta, 
                       y=imis2_post$gamma)) +
  
  geom_point( size = 0.5, alpha = 0.1) +
  
  ggtitle("IMIS") +
  
  theme(
    plot.title = element_text(size=11)
  ) +
  ylab('Gamma') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))
