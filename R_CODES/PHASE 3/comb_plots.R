par(mar = c(4,4,3,2))

par(mfrow = c(2,2))
# s1
## appears in creating a raster
plot(abc0.1$unadj.values[1:5000,1],
     abc0.1$unadj.values[1:5000,2], 
     xlab = "beta", ylab = "gamma",
    # col = "purple",
     main ="Rejection ABC" ,ylim = c(0,0.05),
    cex.axis = 1.3,
    cex.lab = 1.3)

points(abc2ref1$unadj.values[,1],
     abc2ref1$unadj.values[,2],
     col = "red", 
     main ="Scenario_1_True_posterior") # retained 0.5% 0f thw 1000000 runs



# legend(x = "top",inset = 0,
#        legend = c( "True posterior","Rejection"), 
#        col=c( "red", "black"),
#        fill = c( "red" , "black") ,
#        cex=.5, horiz = TRUE)




# legend("topright", legend=c( "True posterior","Rejection"),
#         fill = c( "red" , "black"), cex=0.8)

#############

## appears in creating  a raster

## appears in creating  a raster
plot(ABC_seq2$param[, 1], 
     ABC_seq2$param[, 2],
     xlab = "beta", ylab = "gamma",
    # col = "darkgrey",
     main ="Sequential ABC", ylim = c(0,0.05),
    cex.axis = 1.3,
    cex.lab = 1.3)


points(abc2ref1$unadj.values[,1],
       abc2ref1$unadj.values[,2],
       col = "red", 
       main ="Scenario_1_True_posterior") # retained 0.5% 0f thw 1000000 runs

mtext("Scenario 1", side = 3, line = -1, outer = T, cex=1.2)

# 
# legend(x = "top",inset = 0,
#        legend = c( "True posterior","Rejection", "Sequential"), 
#        col=c( "red" ,"brown", "black"),
#        lwd=5, cex=.5, horiz = TRUE,xpd="NA" )



# legend("topright", legend=c( "True posterior", "Sequential"),
#       fill = c( "red", "black"), cex=0.8)
# # 
# box(lty = '1373', col = 'red')

########################################################################

#S2

par(mar = c(5,4,3,2))


#appears in creating a raster
plot(abc0.1lin$unadj.values[1:5000,1],
     abc0.1lin$unadj.values[1:5000,2],
     xlab = "beta", ylab = "gamma",
     #col = "grey",
     main ="Rejection ABC", 
     ylim = c(0,0.06), xlim = c(0,0.7),
     cex.axis = 1.3,
    cex.lab = 1.3)


points(abc2ref2$unadj.values[,1],
     abc2ref2$unadj.values[,2],
     xlab = "beta", ylab = "gamma",ylim=c(0,0.06),
     col = "blue",
     xlim=c(0,1), main ="posterior_for_abc2ref2") # retained 0.5% 0f thw 1000000 runs


# legend("bottomright", legend=c( "True posterior","Rejection"),
#        fill = c( "blue" , "black"), cex=0.8)
# 
# box(which = "figure", lty = "solid", col = 'red')

#################

plot(ABC_seq1$param[, 1],
     ABC_seq1$param[, 2],
     xlab = "beta", ylab = "gamma",
   # col = "darkgrey",
     main ="Sequential ABC",
   ylim = c(0,0.06), xlim = c(0,0.7),
   cex.axis = 1.3,
    cex.lab = 1.3)


points(abc2ref2$unadj.values[,1],
       abc2ref2$unadj.values[,2],
       xlab = "beta", ylab = "gamma",ylim=c(0,0.6),
       col = "blue",
       xlim=c(0,1), main ="posterior_for_abc2ref2") # retained 0.5% 0f thw 1000000 runs


mtext("Scenario 2", side = 3, line = -14, outer = TRUE, cex = 1.2)

# 
# legend("topright", legend=c( "True posterior","Sequential"),
#       fill = c( "blue" , "grey"), cex=0.8)
# 
# box(which = "figure", lty = "solid", col = 'red')

###################################################################
#Legend


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom", 
       c("True posterior 1","True posterior 2"),
       xpd = TRUE, horiz = TRUE, inset = c(0, 0), 
    bty = "n", fill = c( "red" ,"blue"),
    cex = 1.2)
#
