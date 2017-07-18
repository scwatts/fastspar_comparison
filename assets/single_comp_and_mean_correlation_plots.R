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
# Round SparCC to four significant digits
l.cor.sp <- lapply(l.cor.sp, round, 4)

# Collect lists for each OTU correlate across all runs
l.cor.fs.zip <- lapply(1:length(l.cor.fs[[1]]), function(n) { sapply(l.cor.fs, function(l) { l[[n]] }) })
l.cor.sp.zip <- lapply(1:length(l.cor.sp[[1]]), function(n) { sapply(l.cor.sp, function(l) { l[[n]] }) })

# Get mean of each OTU correlate
l.cor.fs.zip.mn <- sapply(l.cor.fs.zip, mean)
l.cor.sp.zip.mn <- sapply(l.cor.sp.zip, mean)

d.cor.mn <- data.frame(fs=l.cor.fs.zip.mn, sp=l.cor.sp.zip.mn)

# Get variance of each OTU correlate
l.cor.fs.zip.var <- sapply(l.cor.fs.zip, var)
l.cor.sp.zip.var <- sapply(l.cor.sp.zip, var)

d.cor.var <- data.frame(fs=l.cor.fs.zip.var, sp=l.cor.sp.zip.var)


### Plotting
## Correlations
# Mean of OTU correlations
png(filename='output/plots/mean_correlation_plot.png', height=1000, width=1000, res=160)
{
  # Linear modelling for r-sqaured value
  linm <- lm(d.cor.mn$fs ~ d.cor.mn$sp)
  summary(linm)

  # Plot
  g <- ggplot(d.cor.mn, aes(x=fs, y=sp)) + geom_point(alpha=0.1) + geom_abline(slope=1, intercept=0, colour="red")
  g <- g + xlim(-0.5, 1) + ylim(-0.5, 1)
  g <- g + labs(x='FastSpar mean correlation', y='SparCC mean correlation', title='FastSpar and SparCC mean correlation for each OTU pair')
  g
}
dev.off()

# All correlates - FastSpar v Fastspar
d.ff <- data.frame(first=l.cor.fs[[1]], second=l.cor.fs[[2]])

png(filename='output/plots/single_fastspar_fastspar_plot.png', height=1000, width=1000, res=160)
{
  g <- ggplot(d.ff, aes(x=first, y=second)) + geom_point(alpha=0.1) + geom_abline(slope=1, intercept=0, colour="red")
  g <- g + xlim(-0.5, 1) + ylim(-0.5, 1)
  g <- g + labs(x='FastSpar correlation first run', y='FastSpar correlation second run', title='FastSpar first and second run of OTU correlations')
  g
}
dev.off()

# All correlates - SparCC v SparCC
d.s <- data.frame(first=l.cor.sp[[1]], second=l.cor.sp[[2]])

png(filename='output/plots/single_sparcc_sparcc_plot.png', height=1000, width=1000, res=160)
{
  g <- ggplot(d.s, aes(x=first, y=second)) + geom_point(alpha=0.1) + geom_abline(slope=1, intercept=0, colour="red")
  g <- g + xlim(-0.5, 1) + ylim(-0.5, 1)
  g <- g + labs(x='SparCC correlation first run', y='SparCC correlation second run', title='SparCC first and second run of OTU correlations')
  g
}
dev.off()
#
#
# ## Correlation difference
# # Difference FastSpar[[1]] - FastSpar[[2]]
# {
#   g <- ggplot(data.frame(d=l.cor.fs[[1]] - l.cor.fs[[2]]), aes(x=d))
#   g <- g + geom_histogram(bins=20)
#   g <- g + xlim(-0.06, 0.06) + scale_y_continuous(labels=scales::comma)
#   g
# }
#
# # Difference SparCC[[1]] - SparCC[[2]]
# {
#   g <- ggplot(data.frame(d=l.cor.sp[[1]] - l.cor.sp[[2]]), aes(x=d))
#   g <- g + geom_histogram(bins=20)
#   g <- g + xlim(-0.06, 0.06) + scale_y_continuous(labels=scales::comma)
#   g
# }
#
#
# # Difference all FastSpar - SparCC
# {
#   g <- ggplot(data.frame(d=unlist(l.cor.fs) - unlist(l.cor.sp)), aes(x=d))
#   g <- g + geom_histogram(bins=20)
#   g <- g + xlim(-0.06, 0.06) + scale_y_continuous(labels=scales::comma)
#   g
# }


## Variance of each OTU correlate
# Distribution of FastSpar and SparCC OTU correlate variance
png(filename='output/plots/fastspar_variance.png', height=1000, width=1000, res=160)
{
  g <- ggplot(d.cor.var, aes(x=fs))
  g <- g + geom_histogram(bins=20)
  g <- g + scale_x_continuous(labels=scales::comma, limits=c(0, 4e-4))
  g <- g + labs(title='Distribution of FastSpar OTU correlate variance', x='OTU correlate variance')
  g
}
dev.off()
png(filename='output/plots/sparcc_variance.png', height=1000, width=1000, res=160)
{
  g <- ggplot(d.cor.var, aes(x=sp))
  g <- g + geom_histogram(bins=20)
  g <- g + scale_x_continuous(labels=scales::comma, limits=c(0, 4e-4))
  g <- g + labs(title='Distribution of SparCC OTU correlate variance', x='OTU correlate variance')
  g
}
dev.off()

# qqplot of FastSpar and SparCC OTU correlate variance distributions
png(filename='output/plots/variance_qqplot.png', height=1000, width=1000, res=160)
{
  fs <- sort(l.cor.fs.zip.var)
  sp <- sort(l.cor.sp.zip.var)
  fs.len <- length(fs)
  sp.len <- length(sp)
  fs.qq <- approx(1L:fs.len, fs, n = sp.len)$y
  sp.qq <- approx(1L:sp.len, sp, n = fs.len)$y

  g <- ggplot(data.frame(fs=fs.qq, sp=sp.qq), aes(x=fs, y=sp)) + geom_point(alpha=0.25, shape=1)

  # Get plot object so we can find the largest axis range then make plot sqaure
  g.temp <- ggplot_build(g)
  x.max <- max(abs(g.temp$layout$panel_ranges[[1]]$x.range))
  y.max <- max(abs(g.temp$layout$panel_ranges[[1]]$y.range))
  axis.max <- max(x.max, y.max)

  # Finish plot and draw
  g <- g + scale_x_continuous(labels=scales::comma, limits=c(0, axis.max))
  g <- g + scale_y_continuous(labels=scales::comma, limits=c(0, axis.max))
  g <- g + labs(title='FastSpar and SparCC OTU correlation variance qqplot', x='FastSpar', y='SparCC')
  g
}
dev.off()

# Scatter plot of FastSpar and SparCC variance
png(filename='output/plots/variance_scatterplot.png', height=1000, width=1000, res=160)
{
  g <- ggplot(data.frame(fs=l.cor.fs.zip.var, sp=l.cor.sp.zip.var), aes(x=fs, y=sp)) + geom_point()
  g <- g + xlim(0, 4.5e-4) + ylim(0, 4.5e-4)
  g <- g + scale_y_continuous(labels=scales::comma) + scale_x_continuous(labels=scales::comma)
  g <- g + labs(title='FastSpar v. SparCC OTU correlate variance', x='FastSpar', y='SparCC')
  g
}
dev.off()
