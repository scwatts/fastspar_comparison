#!/usr/bin/env Rscript


# Load data
d.fastspar.cor <- read.table('output/fastspar_cor.tsv', header=TRUE, comment.char='', sep='\t', row.names=1)
d.sparcc.cov <- read.table('output/sparcc_cov.tsv', header=TRUE, comment.char='', sep='\t', row.names=1)

d.fastspar.cov <- read.table('output/fastspar_cov.tsv', header=TRUE, comment.char='', sep='\t', row.names=1)
d.sparcc.cor <- read.table('output/sparcc_cor.tsv', header=TRUE, comment.char='', sep='\t', row.names=1)


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
