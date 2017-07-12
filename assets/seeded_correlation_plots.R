#!/usr/bin/env Rscript


### Libraries
library(ggplot2)


### Data
# Load data
d.fastspar.cor <- read.table('output/seeded_estimates/fastspar_cor.tsv', header=TRUE, comment.char='', sep='\t', row.names=1)
d.sparcc.cov <- read.table('output/seeded_estimates/sparcc_cov.tsv', header=TRUE, comment.char='', sep='\t', row.names=1)

d.fastspar.cov <- read.table('output/seeded_estimates/fastspar_cov.tsv', header=TRUE, comment.char='', sep='\t', row.names=1)
d.sparcc.cor <- read.table('output/seeded_estimates/sparcc_cor.tsv', header=TRUE, comment.char='', sep='\t', row.names=1)


### Processing
# We must round to four decimal places for comparison
d.sparcc.cor.rnd <- round(d.sparcc.cor, 4)
d.sparcc.cov.rnd <- round(d.sparcc.cov, 4)

# Check equal of correlation and covariance matrices
equal_cor <- all(d.fastspar.cor == d.sparcc.cor.rnd)
if (equal_cor) {
  message('Correlation matrices are equal')
} else {
  message('Correlation matrices are not equal')
}

equal_cov <- all(d.fastspar.cov == d.sparcc.cov.rnd)
if (equal_cov) {
  message('Covariance matrices are equal')
} else {
  message('Covariance matrices are not equal')
}


### Plot
d <- data.frame(fs=unlist(d.fastspar.cor), sp=unlist(d.sparcc.cor.rnd))

png(filename='output/plots/seeded_fastspar_sparcc_plot.png', height=1000, width=1000)
{
  g <- ggplot(d, aes(x=fs, y=sp)) + geom_point(alpha=0.1) + geom_abline(slope=1, intercept=0, colour="red")
  g <- g + xlim(-0.5, 1) + ylim(-0.5, 1)
  g <- g + labs(x='FastSpar correlation', y='SparCC correlation', title='Seeded FastSpar and SparCC correlation for each OTU pair')
  g
}
dev.off()

# Linear modelling for r-sqaured value
linm <- lm(d$fs ~ d$sp)
summary(linm)