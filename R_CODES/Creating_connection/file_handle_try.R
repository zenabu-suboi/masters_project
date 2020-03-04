

zzfil <- file("tryfile.txt")
open(zzfil, "w")
writeLines(c("0.04"),zzfil, sep="\n")  # open an output file connection
writeLines(c("0.05"),zzfil, sep="\n")

close(zzfil)
#readLines(zzfil)
unlink(zzfil)

?writeLines
?open
#######################################################################


library(tictoc)
zz <- file("tryfile2.txt")
open(zz, "w")
tic()# begin timer 
5-3
toctime <- toc() # end timer

writeLines( as.character(toctime$toc-toctime$tic),
            zz, sep = "\n") 
close(zz) ## close file connection
unlink(zz)

#######################################################################




