

library(EasyABC)
library(SimInf)

#####################################################################################################

parameters=c(0.2,0.02)
u0 = data.frame(S=c(90000), I=c(10000), R=c(0))    #specifying initial compartmental values
model <- SIR(u0, 1:75, beta=parameters[1], gamma=parameters[2]) # specifying the model
result1 <- run(model, threads = 1) 
plot(result1)

result1@U
sum(result1@U[,50])
sum(result1@U[,75])
 ####################################################################################################
func1 <- function(list1,state){
  Ordered_List = list1[order(sapply(list1,length),decreasing=T)] #ordering the list in decreasing number of length of entries
  x <- lengths(Ordered_List)
  #x1 <- length(x[x==state])
  a <- min(which(x == state))
  b <- max(which(x == state))
  M = matrix(unlist(Ordered_List[a:b]), ncol = state, byrow = T)

  if(state == 4){
    m1=M[,c(1,3)]
  }else{
    m1=M[,c(1,4)]
  }
  return(m1)
}


func2= function(parameters,N=100000,inf=0.1){
  library(SimInf)
  u0 = data.frame(S=c((1-inf)*N), I=c(inf*N), R=c(0))    #specifying initial compartmental values
  model <- SIR(u0, 1:75, beta=parameters[1], gamma=parameters[2]) # specifying the model
  result <- run(model, threads = 1)     # running the model, this simulates the spread of the disease over the  specified time period
  time_50<- c(rep("S",result@U[1,50]),rep("I",result@U[2,50]),rep("R",result@U[3,50])) #replicate something a number of times
  samplefrom50<- sample(time_50, size=100)
  time_75<- c(rep("S",result@U[1,75]),rep("I",result@U[2,75]),rep("R",result@U[3,75])) #replicate something a number of times
  samplefrom75<- sample(time_75, size=100)
  target<- c(summary(as.factor(samplefrom50)), summary(as.factor(samplefrom75)))
  return(target)
}

func2(c(0.2,0.02),100000,0.1)

#########################################################################################################

# running the model 100 times and storing the outputs in a list

modelABC <- function(n, state, parameters){
  List = list()  
  for(i in 1:n){
    List[[i]]= func2(parameters,100000,0.1)
  }
  
  Ordered_List = List[order(sapply(List,length),decreasing=T)] #ordering the list in decreasing number of length of entries
  x <- lengths(Ordered_List)
  #x1 <- length(x[x==state])
  a <- min(which(x == state))
  b <- max(which(x == state))
  M = matrix(unlist(Ordered_List[a:b]), ncol = state, byrow = T)
  
  if(state == 4){
    m1=M[,c(1,3)]
  }else{
    m1=M[,c(1,4)]
  }
  return(m1)
}





Ordered_List=List[order(sapply(List,length),decreasing=T)] #ordering the list in decreasing number of length of entries
lengths(Ordered_List)  # lengths of list entries
summary(as.factor(lengths(List))) 

##########################################################################################################

### DEPENDS ON LENGTHS OF INDIVIDUAL MODEL OUTPUTS
# BREAKING THE LIST INTO 3 MATRICES with different dimensions

M_6=matrix(unlist(Ordered_List[1:7]), ncol = 6, byrow = T)
colnames(M_6) <- c("I", "R", "S", "I", "R" , "S")
M_6




debug(func1); undebug(func1)
func1(Ordered_List,state = 4)


# THERE IS A PROBLEM!!!
M_5=matrix(unlist(Ordered_List[8:45]), ncol = 5, byrow = T)
colnames(M_5) <- c("I", "R/S", "S/I", "I/R", "R")
M_5

M_4=matrix(unlist(Ordered_List[46:100]), ncol = 4, byrow = T)
colnames(M_4) <- c("I", "S", "I", "S")
M_4

m1=M_6[,c(1,4)] ;m1 
m2=M_5[,c(1,4)]
m3=M_4[,c(1,3)]

lm=list(m1,m2,m3)
save_targets=rbind(m1, m2,m3) # obtain 100*2 matrix as targets


###############################################################################################
## creat a for loop to make matrix generation more general

save_targ = c(mean(save_targets[,1]),mean(save_targets[,2])) # the discrepancy shows in the means- mean of T75 is very small




for(i in 1:length(which.lengths(Ordered_List)=6))
which.max(lengths(List))

####################################################################################################
#ABC REJECTION 

nreprej= 10000
tolp= 0.1 # retaining all results
set.seed(234)
ABC_rej<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                         summary_stat_target=save_targ, nb_simul=nreprej,
                         tol=tolp, progress_bar = T)




ABC_rej$computime
plot(ABC_rej$param[, 1], ABC_rej$param[, 2], main="ABC_rejection, beta=0.2, gamma=0.02")

nrow(ABC_rej$param)
head(ABC_rej$param)
