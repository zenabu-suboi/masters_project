require("RSQLite")
require(beanplot)
drv = dbDriver("SQLite")
db = dbConnect(drv, "sacema.sqlite")
abc = dbGetQuery(db, 'select J.*, P.*, M.* from job J, par P, met M where J.serial = P.serial and J.serial = M.serial')
dbDisconnect(db)

extra_serials = grepl('serial', names(abc))
extra_serials[1] = FALSE
abc = abc[,!extra_serials]
abc = subset(abc, select=-c(startTime, duration, attempts, seed))
abc$post_bool = abc$posterior >= 0

par_cols = 6:7
met_cols = 8:10
obs_met = c(0.655, 0.371, 0.677)

incomplete_sets = unique(abc$smcSet[abc$status!='D']) # 6
all_sets = unique(abc$smcSet)
complete_sets = setdiff(all_sets, incomplete_sets)
last_complete_set = max(complete_sets)

pdf('marginal_pars.pdf', width=8, height=8)
par(mfrow=c(2,1))
par(mar=c(2.1, 4.1, 1.1, 0.5))
for (col in par_cols) {
    colname = names(abc)[col];
    cat(paste(colname, '\n'))
    beanplot( abc[,colname] ~ abc$post_bool*abc$smcSet, what=c(1,1,1,0), col=list(c(grey(0.3), 1,1, grey(0)), c(grey(0.9), 1,1, grey(0.7))), main='', ylab=colname, side='both', bw='nrd0')
}
dev.off()

# Plot metrics - tends to be trickier, because distributions can be weird (e.g, highly skewed or long-tailed)
pdf('marginal_mets.pdf', width=8, height=12)
par(mfrow=c(3,1))
par(mar=c(2.1, 4.1, 1.1, 0.5))
#alpha = 0.025 # plot middle 95% of distributions
alpha = 0.0 # plot middle 95% of distributions
for (col in met_cols) {
    met_idx = col - max(par_cols)
    colname = names(abc)[col]

    # calculate reasonable plot limits
    obs_val = obs_met[met_idx]
    val_lims = quantile(abc[,colname], na.rm=T, probs=c(alpha, 1-alpha))
    val_min = min(val_lims[1], obs_val) 
    val_max = max(val_lims[2], obs_val) 

    # filter out NAs
    complete = complete.cases(abc[,colname]) & abc[,colname] > val_lims[1] & abc[,colname] < val_lims[2]
    cat(paste0(colname, ' ', val_lims[1], ', ', val_lims[2], '\n'))

    # plot the stuff
    beanplot( abc[complete, colname] ~ abc$post_bool[complete] * abc$smcSet[complete], what=c(0,1,1,0), 
              col=list(c(grey(0.3), 1,1, grey(0)), c(grey(0.9), 1, 1, grey(0.7))), 
              #main='', ylab=colname, side='both', ylim=unlist(ylims[met_idx]) )
              main='', ylab=colname, side='both', ylim=c(val_min, val_max), na.rm=T, log='' , bw='nrd0')
    abline(h=mean(abc[abc$smcSet==last_complete_set, colname], na.rm=T), lty=3)
    abline(h=obs_val, col=2, lwd=1)
}

dev.off()
