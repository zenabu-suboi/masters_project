install.packages("emdist")
library(emdist)


abc0.1.emd=    emd2d(ras.abc0.1.mat, lik.abc0.1.mat, dist="euclidean",
                     xdist= betagrid, ydist= gammagrid)

abc0.1.lin.emd=    emd2d(ras.abc0.1lin.mat, lik.abc0.1lin.mat, dist="euclidean",
                         xdist= betagrid, ydist= gammagrid)

abc0.1.nnet.emd=    emd2d(ras.abc0.1nnet.mat, lik.abc0.1nnet.mat, dist="euclidean",
                          xdist= betagrid, ydist= gammagrid)



abc0.01.emd=    emd2d(ras.abc0.01.mat, lik.abc0.01.mat, dist="euclidean",
                      xdist= betagrid, ydist= gammagrid)

abc0.01.lin.emd=    emd2d(ras.abc0.01lin.mat, lik.abc0.01lin.mat, dist="euclidean",
                          xdist= betagrid, ydist= gammagrid)

abc0.01.nnet.emd=    emd2d(ras.abc0.01nnet.mat, lik.abc0.01nnet.mat, dist="euclidean",
                           xdist= betagrid, ydist= gammagrid)


abc0.001.emd=    emd2d(ras.abc0.001.mat, lik.abc0.001.mat, dist="euclidean",
                       xdist= betagrid, ydist= gammagrid)

abc0.001.lin.emd=    emd2d(ras.abc0.001lin.mat, lik.abc0.001lin.mat, dist="euclidean",
                           xdist= betagrid, ydist= gammagrid)


abc0.001.nnet.emd=    emd2d(ras.abc0.001nnet.mat, lik.abc0.001nnet.mat, dist="euclidean",
                            xdist= betagrid, ydist= gammagrid)


