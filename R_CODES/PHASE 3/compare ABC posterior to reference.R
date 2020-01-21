### COMPARE ABC POSTERIOR TO reference POSTERIOR




### scenario 1

dim(ras.rej1.mat)
dim(ras2.ref1.mat)

sum((ras.rej1.mat-ras2.ref1.mat)^2)

### percentage overlap:
(1-(sum(abs(ras.rej1.mat-ras2.ref1.mat)))/ (2*5000))*100 # scenario1rej

(1-(sum(abs(ras.seq1.mat-ras2.ref1.mat)))/ (2*5000))*100  # scenario1seq


###################################################################
### scenario 2

sum((ras.rej1.mat-ras2.ref1.mat)^2)

### percentage overlap:
(1-(sum(abs(ras.rej2.mat-ras2.ref2.mat)))/ (2*5000))*100 # scenario2rej

(1-(sum(abs(ras.seq2.mat-ras2.ref2.mat)))/ (2*5000))*100  # scenario2seq



#############################################################################


#install.packages("ggpubr")
library(ggpubr)

############################################

rej <- c(7.48, 4.76) # percentage overlap for rej in both scenarios
seq <- c(26.14,70.16) # percentage overlap for seq in both scenarios
Scenario <- c(1, 2) # scenarios considered


toplot <- data.frame(rej, seq, Scenario) # dataframe from the abobe vectors 

dp <- ggdotplot(toplot, x = "Scenario", # plot of the p_overlaps 
                y = c("rej","seq"),
                #color = c("green","red"),
                add.params = list(shape = 25),
                binwidth = 3,
                title = "Plot of the percentage overlap",
                merge = TRUE, ylim = c(0, 80))
dp

?ggdotplot
