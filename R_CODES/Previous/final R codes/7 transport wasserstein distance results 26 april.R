
library(transport)


##### ORGINARY ABC

pgrid.ras.abc0.1.mat= pgrid(ras.abc0.1.mat)
pgrid.lik.abc0.1.mat= pgrid(lik.abc0.1.mat)

was.pgrid.ras.abc0.1.mat =wasserstein(pgrid.ras.abc0.1.mat, pgrid.lik.abc0.1.mat, prob=T)
was.pgrid.ras.abc0.1.mat

####

pgrid.ras.abc0.01.mat= pgrid(ras.abc0.01.mat)
pgrid.lik.abc0.01.mat= pgrid(lik.abc0.01.mat)

was.pgrid.ras.abc0.01.mat =wasserstein(pgrid.ras.abc0.01.mat, pgrid.lik.abc0.01.mat, prob=T)
was.pgrid.ras.abc0.01.mat

####

pgrid.ras.abc0.001.mat= pgrid(ras.abc0.001.mat)
pgrid.lik.abc0.001.mat= pgrid(lik.abc0.001.mat)

was.pgrid.ras.abc0.001.mat =wasserstein(pgrid.ras.abc0.001.mat, pgrid.lik.abc0.001.mat, prob=T)
was.pgrid.ras.abc0.001.mat

####


##### LIN

#####

pgrid.ras.abc0.1lin.mat= pgrid(ras.abc0.1lin.mat)
pgrid.lik.abc0.1lin.mat= pgrid(lik.abc0.1lin.mat)

was.pgrid.ras.abc0.1lin.mat =wasserstein(pgrid.ras.abc0.1lin.mat, pgrid.lik.abc0.1lin.mat, prob=T)
was.pgrid.ras.abc0.1lin.mat

####

pgrid.ras.abc0.01lin.mat= pgrid(ras.abc0.01lin.mat)
pgrid.lik.abc0.01lin.mat= pgrid(lik.abc0.01lin.mat)

was.pgrid.ras.abc0.01lin.mat =wasserstein(pgrid.ras.abc0.01lin.mat, pgrid.lik.abc0.01lin.mat, prob=T)
was.pgrid.ras.abc0.01lin.mat

####

pgrid.ras.abc0.001lin.mat= pgrid(ras.abc0.001lin.mat)
pgrid.lik.abc0.001lin.mat= pgrid(lik.abc0.001lin.mat)

was.pgrid.ras.abc0.001lin.mat =wasserstein(pgrid.ras.abc0.001lin.mat, pgrid.lik.abc0.001lin.mat, prob=T)
was.pgrid.ras.abc0.001lin.mat


#### NNET

#####

pgrid.ras.abc0.1nnet.mat= pgrid(ras.abc0.1nnet.mat)
pgrid.lik.abc0.1nnet.mat= pgrid(lik.abc0.1nnet.mat)

was.pgrid.ras.abc0.1nnet.mat =wasserstein(pgrid.ras.abc0.1nnet.mat, pgrid.lik.abc0.1nnet.mat, prob=T)
was.pgrid.ras.abc0.1nnet.mat

####

pgrid.ras.abc0.01nnet.mat= pgrid(ras.abc0.01nnet.mat)
pgrid.lik.abc0.01nnet.mat= pgrid(lik.abc0.01nnet.mat)

was.pgrid.ras.abc0.01nnet.mat =wasserstein(pgrid.ras.abc0.01nnet.mat, pgrid.lik.abc0.01nnet.mat, prob=T)
was.pgrid.ras.abc0.01nnet.mat

####

pgrid.ras.abc0.001nnet.mat= pgrid(ras.abc0.001nnet.mat)
pgrid.lik.abc0.001nnet.mat= pgrid(lik.abc0.001nnet.mat)

was.pgrid.ras.abc0.001nnet.mat =wasserstein(pgrid.ras.abc0.001nnet.mat, pgrid.lik.abc0.001nnet.mat, prob=T)
was.pgrid.ras.abc0.001nnet.mat