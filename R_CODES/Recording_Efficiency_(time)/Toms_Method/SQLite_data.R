#install.packages("RSQLite") #perhaps needed
setwd()

library("RSQLite")
library(dplyr)
library(ggplot2)

# connect to the sqlite file
sqlite    <- dbDriver("SQLite")
abc_sqlite <- dbConnect(sqlite,"sacema.sqlite")

dbListTables(abc_sqlite)

param <- dbGetQuery(abc_sqlite, "SELECT * FROM par")

metrics <- dbGetQuery(abc_sqlite, "SELECT * FROM met")

job <- dbGetQuery(abc_sqlite, "SELECT * FROM job")


abc_all <- dbGetQuery(abc_sqlite, "SELECT * FROM par, met, job where 
                     par.serial = met.serial and par.serial = job.serial")

# extract retained parameter combinations
class(abc_all)
attach(abc_all)

abc_post <- abc_all[posterior != -1 & smcSet == 2, ]

abc_posterior <- abc_post[,3:4]

# plot posterior

Tom_plot <- ggplot(abc_posterior,
                        aes(x=abc_posterior$beta, 
                            y=abc_posterior$gamma)) +
  
  geom_point( size = 0.5, alpha = 0.1) +
  
  ggtitle("Seq ABC - Tom") +
  
  theme(
    plot.title = element_text(size=11)
  ) +
  ylab('Gamma') +
  xlab('Beta')+
  xlim(c(0,1))+
  ylim(c(0,0.15))
