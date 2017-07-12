#!/usr/bin/env Rscript


### Libraries
library(ggplot2)


### Data
# Get filepaths and names
fp.cor.sp <- Sys.glob('output/replication/sparcc*cor*tsv')
fp.cor.fs <- Sys.glob('output/replication/fastspar*cor*tsv')

names(fp.cor.sp) <- sub('^(.+?)_cor_([0-9]+)\\.tsv$', '\\1_\\2', basename(fp.cor.sp))
names(fp.cor.fs) <- sub('^(.+?)_cor_([0-9]+)\\.tsv$', '\\1_\\2', basename(fp.cor.fs))

# Read in data and convert to list
d.cor.sp <- lapply(fp.cor.sp, read.table, comment.char='', header=TRUE, row.names=1, sep='\t')
l.cor.sp <- lapply(d.cor.sp, unlist)

d.cor.fs <- lapply(fp.cor.fs, read.table, comment.char='', header=TRUE, row.names=1, sep='\t')
l.cor.fs <- lapply(d.cor.fs, unlist)


### Processing
l.cor.fs.zip <- lapply(1:length(l.cor.fs[[1]]), function(n) { sapply(l.cor.fs, function(l) { l[[n]] }) })
l.cor.fs.zip.mn <- sapply(l.cor.fs.zip, mean)

l.cor.sp.zip <- lapply(1:length(l.cor.sp[[1]]), function(n) { sapply(l.cor.sp, function(l) { l[[n]] }) })
l.cor.sp.zip.mn <- sapply(l.cor.sp.zip, mean)

d.cor.mn <- data.frame(fs=l.cor.fs.zip.mn, sp=l.cor.sp.zip.mn)


### Plotting
# Mean of OTU correlations
png(filename='output/plots/mean_correlation_plot.png', height=1000, width=1000)
{
  g <- ggplot(d.cor.mn, aes(x=fs, y=sp)) + geom_point(alpha=0.1) + geom_abline(slope=1, intercept=0, colour="red")
  g <- g + xlim(-0.5, 1) + ylim(-0.5, 1)
  g <- g + labs(x='FastSpar mean correlation', y='SparCC mean correlation', title='FastSpar and SparCC mean correlation for each OTU pair')
  g
}
dev.off()

# FastSpar v Fastspar
d.ff <- data.frame(first=l.cor.fs[[1]], second=l.cor.fs[[2]])

png(filename='output/plots/single_fastspar_fastspar_plot.png', height=1000, width=1000)
{
  g <- ggplot(d.ff, aes(x=first, y=second)) + geom_point(alpha=0.1) + geom_abline(slope=1, intercept=0, colour="red")
  g <- g + xlim(-0.5, 1) + ylim(-0.5, 1)
  g <- g + labs(x='FastSpar correlation first run', y='FastSpar correlation second run', title='FastSpar first and second run of OTU correlations')
  g
}
dev.off()

# SparCC v SparCC
d.s <- data.frame(first=l.cor.sp[[1]], second=l.cor.sp[[2]])

png(filename='output/plots/single_sparcc_sparcc_plot.png', height=1000, width=1000)
{
  g <- ggplot(d.s, aes(x=first, y=second)) + geom_point(alpha=0.1) + geom_abline(slope=1, intercept=0, colour="red")
  g <- g + xlim(-0.5, 1) + ylim(-0.5, 1)
  g <- g + labs(x='SparCC correlation first run', y='SparCC correlation second run', title='SparCC first and second run of OTU correlations')
  g
}
dev.off()


### Other
# Linear modelling for r-sqaured value
linm <- lm(d.cor.mn$fs ~ d.cor.mn$sp)
summary(linm)

# Demostrating that differences between run correlates is normally distributed
hist(unlist(l.cor.fs) - unlist(l.cor.sp), xlim=c(-0.06, 0.06))

# Just for one fastspar run
hist(l.cor.fs[[1]] - l.cor.fs[[2]], xlim=c(-0.06, 0.06))

# Just for one sparcc run
hist(l.cor.sp[[1]] - l.cor.sp[[2]], xlim=c(-0.06, 0.06))
