#!/usr/bin/env Rscript


### Libraries
library(ggplot2)


### Data
# Get filepaths and names
fp.cor <- Sys.glob('output/replication/*cor*tsv')
names(fp.cor) <- sub('^(.+?)_cor_([0-9]+)\\.tsv$', '\\1_\\2', basename(fp.cor))

# Include only the first 10


# Read in data and convert to list
d.cor <- lapply(fp.cor, read.table, comment.char='', header=TRUE, row.names=1, sep='\t', nrows=100)
l.cor <- lapply(d.cor, unlist)


### Processing
# Find the number of permutations
n <- length(l.cor)
perms <- list(); for (i in 1:n) { for (j in 1:n) { perms[[i+(j-1)*n]] <-  c(i, j) }}

# Create dataframes for each permutation
d.cor.perms <- lapply(perms, function(ij)
{
  i = ij[[1]];
  j = ij[[2]];
  data.frame(x=l.cor[[i]], y=l.cor[[j]], name=paste(names(l.cor)[ij], collapse='_') )
})

# Bind all permutations together
d.cor.perms.combined <- do.call('rbind', d.cor.perms)


### Plot
png(filename='output/plots/correlation_permutation_all.png', height=20000, width=20000)
{
  ggplot(d.cor.perms.combined, aes(x=x, y=y)) + geom_point(alpha=0.25) + facet_wrap( ~ name, scales='free')
}
dev.off()
