### COMPARE ABC POSTERIOR TO LIKELIHOOD POSTERIOR



################ REJECTION ABC
### abc0.1

sum((ras.abc0.1.mat-lik.abc0.1.mat)^2)

### percentage overlap:
1-((sum(abs(ras.abc0.1.mat-lik.abc0.1.mat))+(2*(nreprej*0.1-sum(ras.abc0.1.mat))))/ (2*(nreprej*0.1)))


### abc0.01

sum((ras.abc0.01.mat-lik.abc0.01.mat)^2)

### percentage overlap:
1-((sum(abs(ras.abc0.01.mat-lik.abc0.01.mat))+(2*(nreprej*0.01-sum(ras.abc0.01.mat))))/ (2*(nreprej*0.01)))


### abc0.001

sum((ras.abc0.001.mat-lik.abc0.001.mat)^2)

### percentage overlap:
1-((sum(abs(ras.abc0.001.mat-lik.abc0.001.mat))+(2*(nreprej*0.001-sum(ras.abc0.001.mat))))/ (2*(nreprej*0.001)))







################# REGRESSION LINEAR
### abc0.1

sum((ras.abc0.1lin.mat-lik.abc0.1lin.mat)^2)

### percentage overlap:
1-((sum(abs(ras.abc0.1lin.mat-lik.abc0.1lin.mat))+(2*(nreprej*0.1-sum(ras.abc0.1lin.mat))))/ (2*(nreprej*0.1)))


### abc0.01

sum((ras.abc0.01lin.mat-lik.abc0.01lin.mat)^2)

### percentage overlap:
1-((sum(abs(ras.abc0.01lin.mat-lik.abc0.01lin.mat))+(2*(nreprej*0.01-sum(ras.abc0.01lin.mat))))/ (2*(nreprej*0.01)))


### abc0.001

sum((ras.abc0.001lin.mat-lik.abc0.001lin.mat)^2)

### percentage overlap:
1-((sum(abs(ras.abc0.001lin.mat-lik.abc0.001lin.mat))+(2*(nreprej*0.001-sum(ras.abc0.001lin.mat))))/ (2*(nreprej*0.001)))






##################  REGRESSION NEURALNET
### 



sum((ras.abc0.1nnet.mat-lik.abc0.1nnet.mat)^2)

### percentage overlap:
1-((sum(abs(ras.abc0.1nnet.mat-lik.abc0.1nnet.mat))+(2*(nreprej*0.1-sum(ras.abc0.1nnet.mat))))/ (2*(nreprej*0.1)))


### abc0.01

sum((ras.abc0.01nnet.mat-lik.abc0.01nnet.mat)^2)

### percentage overlap:
1-((sum(abs(ras.abc0.01nnet.mat-lik.abc0.01nnet.mat))+(2*(nreprej*0.01-sum(ras.abc0.01nnet.mat))))/ (2*(nreprej*0.01)))


### abc0.001

sum((ras.abc0.001nnet.mat-lik.abc0.001nnet.mat)^2)

### percentage overlap:
1-((sum(abs(ras.abc0.001nnet.mat-lik.abc0.001nnet.mat))+(2*(nreprej*0.001-sum(ras.abc0.001nnet.mat))))/ (2*(nreprej*0.001)))
 