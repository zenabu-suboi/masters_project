
library(ggplot2)
mydat <- as.data.frame(readRDS("2targets_seq_post"))


# Hexbin chart with default option
ggplot(mydat, aes(x = mydat[,1], y = mydat[,2]) ) +
  geom_hex() +
  theme_bw()

# Bin size control + color palette
ggplot(mydat, aes(x = mydat[,1], y = mydat[,2]) ) +
  geom_hex(bins = 50) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

