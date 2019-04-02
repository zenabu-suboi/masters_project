


###  ABC rejection, we need to run it 1000000 times in the end
nreprej= 1000000
tolp= 1   ### see ?ABC_rejection, provides us with the parameters that produce
### summary statistics X% closest to the target
### e.g. when we set it to 0.1, and we start with nreprej=100, we end up with 10.

?ABC_rejection

set.seed(234)
### don't forget to run set.seed and code to get the targets on page 1 Obtaining the targets...
ABC_rej<-ABC_rejection(model=modelforABCmcmc2, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=truepop.prev, nb_simul=nreprej,
                       tol=tolp, progress_bar = T)

ABC_rej$computime/(60*60)
  
### plot the posterior obtained by ABC rejection
plot(ABC_rej$param[, 1], ABC_rej$param[, 2])

nrow(ABC_rej$param)


### save results for abc0.1lin

Tabc0.1lin= proc.time()
abc0.1lin <- abc(target = c(truepop.prev),
                  param = ABC_rej$param,
                  sumstat = ABC_rej$stats,
                  tol = 0.1,
                  method = "loclinear") ### change method here!
Tabc0.1lin= proc.time()-Tabc0.1lin
Tabc0.1lin



### save results for abc0.01lin

Tabc0.01lin= proc.time()
abc0.01lin <- abc(target = c(truepop.prev),
                  param = ABC_rej$param,
                  sumstat = ABC_rej$stats,
                  tol = 0.01,
                  method = "loclinear") ### change method here!
Tabc0.01lin= proc.time()-Tabc0.01lin
Tabc0.01lin



### save results for abc0.001lin

Tabc0.001lin= proc.time()
abc0.001lin <- abc(target = c(truepop.prev),
                   param = ABC_rej$param,
                   sumstat = ABC_rej$stats,
                   tol = 0.001,
                   method = "loclinear") ### change method here!
Tabc0.001lin= proc.time()-Tabc0.001lin
Tabc0.001lin


### save results for abc0.1nnet

Tabc0.1nnet= proc.time()
abc0.1nnet <- abc(target = c(truepop.prev),
                  param = ABC_rej$param,
                  sumstat = ABC_rej$stats,
                  tol = 0.1,
                  method = "neuralnet") ### change method here!
Tabc0.1nnet= proc.time()-Tabc0.1nnet
Tabc0.1nnet


### save results for abc0.01nnet

Tabc0.01nnet= proc.time()
abc0.01nnet <- abc(target = c(truepop.prev),
                   param = ABC_rej$param,
                   sumstat = ABC_rej$stats,
                   tol = 0.01,
                   method = "neuralnet") ### change method here!
Tabc0.01nnet= proc.time()-Tabc0.01nnet
Tabc0.01nnet

### save results for abc0.001nnet

Tabc0.001nnet= proc.time()
abc0.001nnet <- abc(target = c(truepop.prev),
                    param = ABC_rej$param,
                    sumstat = ABC_rej$stats,
                    tol = 0.001,
                    method = "neuralnet") ### change method here!
Tabc0.001nnet= proc.time()-Tabc0.001nnet
Tabc0.001nnet

?abc



#### below doesnt work, memory issues

### save results for abc0.1ridge

Tabc0.1ridge= proc.time()
abc0.1ridge <- abc(target = c(truepop.prev),
                  param = ABC_rej$param,
                  sumstat = ABC_rej$stats,
                  tol = 0.1,
                  method = "ridge") ### change method here!
Tabc0.1ridge= proc.time()-Tabc0.1ridge
Tabc0.1ridge


### save results for abc0.01ride

Tabc0.01ridge= proc.time()
abc0.01ridge <- abc(target = c(truepop.prev),
                   param = ABC_rej$param,
                   sumstat = ABC_rej$stats,
                   tol = 0.01,
                   method = "ridge") ### change method here!
Tabc0.01ridge= proc.time()-Tabc0.01ridge
Tabc0.01ridge


### save results for abc0.001ridge

Tabc0.001ridge= proc.time()
abc0.001ridge <- abc(target = c(truepop.prev),
                   param = ABC_rej$param,
                   sumstat = ABC_rej$stats,
                   tol = 0.001,
                   method = "ridge") ### change method here!
Tabc0.001ridge= proc.time()-Tabc0.001ridge
Tabc0.001ridge


?abc
