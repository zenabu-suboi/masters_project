

####
#### scale the likelihood to the number of parameters obtained by our abc methods
#### in order to compare them

lik.abc0.1.mat=  likmatstd*sum(ras.abc0.1.mat)# rej_0.1

lik.abc0.01.mat=  likmatstd*sum(ras.abc0.01.mat) # rej_0.01

lik.abc0.001.mat=  likmatstd*sum(ras.abc0.001.mat)




lik.abc0.1lin.mat=  likmatstd*sum(ras.abc0.1lin.mat)

lik.abc0.01lin.mat=  likmatstd*sum(ras.abc0.01lin.mat)

lik.abc0.001lin.mat=  likmatstd*sum(ras.abc0.001lin.mat)




lik.abc0.1nnet.mat=  likmatstd*sum(ras.abc0.1nnet.mat)

lik.abc0.01nnet.mat=  likmatstd*sum(ras.abc0.01nnet.mat)

lik.abc0.001nnet.mat=  likmatstd*sum(ras.abc0.001nnet.mat)




