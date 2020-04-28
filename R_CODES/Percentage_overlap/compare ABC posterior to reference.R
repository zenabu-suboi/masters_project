### COMPARE ABC POSTERIOR TO reference POSTERIOR


### scenario 1

### percentage overlap:
(1-(sum(abs(ras.rej2.mat-ras.ref2.mat)))/ (2*5000))*100 # 3.4%

(1-(sum(abs(ras.seq2.mat-ras.ref2.mat)))/ (2*5000))*100 # 24.3%

(1-(sum(abs(ras.bmle2.mat-ras.ref2.mat)))/ (2*5000))*100 # 32.3%


###################################################################
### scenario 2

### percentage overlap:
(1-(sum(abs(ras.rej3.mat-ras.ref3.mat)))/ (2*5000))*100 # 2.1%

(1-(sum(abs(ras.seq3.mat-ras.ref3.mat)))/ (2*5000))*100 # 71.6%

(1-(sum(abs(ras.bmle3.mat-ras.ref3.mat)))/ (2*5000))*100 # 42.2%



#############################################################################


#install.packages("ggpubr")
library(ggpubr)
library(ggplot2)
library(cowplot)

############################################

Rej <- c(3.4, 2.1) # percentage overlap for rej in both scenarios
Seq <- c(24.3, 71.6) # percentage overlap for seq in both scenarios
BMLE <- c(32.3, 42.2) # percentage overlap for bmle in both scenarios
#tom <- c() # percentage overlap for tom in both scenarios

Scenario <- c(1, 2) # scenarios considered

################################################################
#######################################################

toplot = data.frame(Scenario = rep(Scenario, 3),
                    Percentage_overlap = c(Rej, Seq, BMLE),
                    class = c(rep('Rejection ABC',2),
                              rep('Sequential ABC',2),
                              rep('BMLE',2)))

#View(toplot)
q = ggdotplot(toplot, x = 'Scenario',
              #y = 'Percentage_overlap',
              color = "class",
              palette = "jco",
              binwidth = 2, fill = 'class') +
  facet_wrap(~class,ncol = 3)
p_overlap <- q + theme( panel.background = element_rect(fill = "gray93", 
                                                        colour = "white",
                                           size = 0.3, linetype = "solid"),
           panel.grid.major = element_line(size = 0.7, linetype = 'solid',
                                           colour = "white"), 
           panel.grid.minor = element_line(size = 0.02, linetype = 'solid',
                                           colour = "white", ))

library(scales)
p_overlap + scale_y_continuous(name = "Percentage Overlap",
                               breaks=seq(0,80,20),
                               labels = function(x) paste0(x,"%"))

