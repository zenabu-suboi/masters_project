###  ABC rejection, we need to run it 1000000 times in the end
nreprej= 100000
tolp= 1   ### see ?ABC_rejection, provides us with the parameters that produce
### summary statistics X% closest to the target
### e.g. when we set it to 0.1, and we start with nreprej=100, we end up with 10.

#?ABC_rejection

set.seed(234)
### don't forget to run set.seed and code to get the targets on page 1 Obtaining the targets...
ABC_rej1<-ABC_rejection(model=modelABC, prior=list(c("unif",0,1),c("unif",0,0.5)), 
                       summary_stat_target=save_targ1, nb_simul=nreprej,
                       tol=tolp, progress_bar = T)

ABC_rej1$computime


par(mfrow=c(3,1))
### save results for rejabc0.1

Tabc0.1rej= proc.time()
rejabc0.1 <- abc(target = c(save_targ1),
                  param = ABC_rej1$param,
                  sumstat = ABC_rej1$stats,
                  tol = 0.1,
                  method = "rejection") ### change method here!
Tabc0.1rej= proc.time()-Tabc0.1rej
Tabc0.1rej

plot(rejabc0.1$unadj.values[,1],rejabc0.1$unadj.values[,2],main="rejabc0.1, beta=0.8, gamma=0.02")



### save results for rejabc0.01

Tabc0.01rej= proc.time()
rejabc0.01 <- abc(target = c(save_targ1),
                   param = ABC_rej1$param,
                   sumstat = ABC_rej1$stats,
                   tol = 0.01,
                   method = "rejection") ### change method here!
Tabc0.01rej= proc.time()-Tabc0.01rej
Tabc0.01rej

plot(rejabc0.01$unadj.values[,1],rejabc0.01$unadj.values[,2],main="rejabc0.01, beta=0.8, gamma=0.02")


### save results for rejabc0.001

Tabc0.001rej= proc.time()
rejabc0.001 <- abc(target = c(save_targ1),
                  param = ABC_rej1$param,
                  sumstat = ABC_rej1$stats,
                  tol = 0.001,
                  method = "rejection") ### change method here!
Tabc0.001rej= proc.time()-Tabc0.01rej
Tabc0.01rej

plot(rejabc0.001$unadj.values[,1],rejabc0.001$unadj.values[,2],main="rejabc0.001, beta=0.8, gamma=0.02")


