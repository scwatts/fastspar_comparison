#!/usr/bin/env Rscript


# Load data
d.fastspar <- read.table('output/fastspar_cor.tsv', header=TRUE, comment.char='', sep='\t', row.names=1)
d.sparcc <- read.table('output/sparcc_cor.tsv', header=TRUE, comment.char='', sep='\t', row.names=1)

# We must round to four decimal places for comparison
d.sparcc.rnd <- round(d.sparcc, 4)

# See if all values are equal
equal <- all(d.fastspar == d.sparcc.rnd)
if (equal) {
    message('Matrices are equal')
} else {
    message('Matrices are not equal')
}
