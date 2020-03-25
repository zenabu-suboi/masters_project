### COMPARE ABC POSTERIOR TO reference POSTERIOR

### scenario 1

### percentage overlap:
(1-(sum(abs(ras.rej2.mat-ras.ref2.mat)))/ (2*5000))*100 # 3.4

(1-(sum(abs(ras.seq2.mat-ras.ref2.mat)))/ (2*5000))*100 # 24.3

(1-(sum(abs(ras.bmle2.mat-ras.ref2.mat)))/ (2*5000))*100 # 32.3


###################################################################
### scenario 2

### percentage overlap:
(1-(sum(abs(ras.rej3.mat-ras.ref3.mat)))/ (2*5000))*100 # 2.1

(1-(sum(abs(ras.seq3.mat-ras.ref3.mat)))/ (2*5000))*100 # 71.6

(1-(sum(abs(ras.bmle3.mat-ras.ref3.mat)))/ (2*5000))*100 # 42.2



#############################################################################


#install.packages("ggpubr")
library(ggpubr)

############################################

Rej <- c(3.4, 2.1) # percentage overlap for rej in both scenarios
Seq <- c(24.3, 71.6) # percentage overlap for seq in both scenarios
BMLE <- c(32.3, 42.2) # percentage overlap for bmle in both scenarios
#tom <- c() # percentage overlap for tom in both scenarios

Scenario <- c(1, 2) # scenarios considered


toplot <- data.frame(Rej, Seq, BMLE, Scenario) # dataframe from the abobe vectors 

dp <- ggdotplot(toplot, x = "Scenario", # plot of the p_overlaps 
                y = c("Rej","Seq","BMLE"),
                #color = c("green","red"),
                palette =c( "red","green", "blue"), 
                fill= "grey", 
                #add.params = list(shape = 12),
                binwidth = 3,
                title = "Plot of the percentage overlap",
                ylab = "overlap (%)",
                merge = TRUE, 
                ylim = c(0, 100))
dp

?ggdotplot
