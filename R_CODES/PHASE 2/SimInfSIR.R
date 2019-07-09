
library(SimInf)


transitions <- c("S -> b*S*I/(S+I+R) -> I", "I -> g*I -> R")
compartments <- c("S", "I", "R")
m <- mparse(transitions, compartments, b = 0.16, g = 0.077)
model <- init(m, u0 = cbind(S = 99, I = 1, R = 0), tspan =  1:180)


######### 
m <- mparse(c("S -> b*S*I/(S+I+R) -> I + Icum", "I -> g*I -> R"),
            c("S", "I", "Icum", "R"), b = 0.16, g = 0.077)

model <- init(m, cbind(S = 99, I = 1, Icum = 0, R = 0), 1:180)
result <- run(model, threads = 1, seed = 22)

plot(stepfun(result@tspan[-1], diff(c(0, U(result)["Icum",]))),
     + main = "", xlab = "Time", ylab = "Number of cases",
     + do.points = FALSE)





###########################################################################
library(SimInf)


SIRown <- function (u0, tspan, events = NULL, beta = NULL, gamma = NULL) 
{
  compartments <- c("S", "I", "Icum", "R")
  u0 <- check_u0(u0, compartments)
  check_gdata_arg(beta, gamma)
  
  E <- matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1), nrow = 4, 
              ncol = 5, dimnames = list(compartments, c("1", "2", "3", "4", "5")))
###  
  
  G <- matrix(c(1, 1, 1, 1), nrow = 2, ncol = 2, dimnames = list(c("S -> beta*S*I/(S+I+R) -> I+Icum", 
                                                                   "I -> gamma*I -> R"), c("1", "2")))
 
 ### 
   S <- matrix(c(-1, 1, 0, 0, -1, 1), nrow = 3, ncol = 2, dimnames = list(compartments, 
                                                                         c("1", "2")))
  gdata <- as.numeric(c(beta, gamma))
  names(gdata) <- c("beta", "gamma")
  model <- SimInf_model(G = G, S = S, E = E, tspan = tspan, 
                        events = events, gdata = gdata, u0 = u0)
  as(model, "SIRown")
}
#####

u0= data.frame(S=c(990), I=c(10), R=c(0))

model <- SIRown(u0, 1:75, beta= 0.2, gamma=0.02)
result <- run(model, threads = 1)

?SimInf


###################################################################



matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1, 1, 1, 1), nrow = 3, 
       ncol = 4, dimnames = list(compartments, c("1", 
                                                 "2", "3", "4")))

compartments <- c("S", "I", "R")

 matrix(c(-1, 1, 0, 0, -1, 1), nrow = 3, ncol = 2, dimnames = list(compartments, 
                                                                       c("1", "2")))

 
 #######################################################################
 
 # trying prevalence
 
 # Create an 'SIR' model with 6 nodes and initialize
 ## it to run over 10 days.
 u0 <- data.frame(S = 100, I = 1, R = 0)
 model <- SIR(u0 = u0, tspan = 1:10, beta = 0.16, gamma = 0.077)
 
 ## Run the model to generate a single stochastic trajectory.
 result <- run(model, threads = 1)
 
 ## Determine the proportion of infected individuals (cases)
 ## in the population at the time-points in 'tspan'.
 prevalence(result, I~S+I+R)
 
 ## Identical result is obtained with the shorthand 'I~.'
prev<- prevalence(result, I~.)

cbind(c(result@U,prev)) 
##############################################


u0= data.frame(S=c(990), I=c(10), R=c(0))

model <- SIR(u0, 1:75, beta= 0.2, gamma=0.02)
result <- run(model, threads = 1)#, seed=sample.int(1000000000,1)) 
prev<- prevalence(result, I ~ .)

plot(result)
 
plot(prev, type="l",col="blue")


trac <- trajectory(result, compartments = "I")
cbind(trac[3],prev[2])

plot(trac[,3], type="l",col="red")
lines(prev[,2], type="l",col="blue")
