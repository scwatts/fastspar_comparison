#!/bin/bash


###
# Options
###
# Print commands as they're executed
set -x

# Required for SparCC to run without thread oversubscription on a single CPU
export OMP_NUM_THREADS=1


###
# Run FastSpar and SparCC using pre-generated and seeded fraction estimates
###
# Generate fractions
./assets/generate_fractions.py -c assets/otu_table_250_250.tsv -o fraction_estimates -n 50

# Clone FastSpar and patch
# -l is critical to applying patch correctly (ignores whitespace); -p1 removes the a/ and b/ from file descriptors
git clone https://github.com/scwatts/fastspar.git
(cd fastspar && patch -p1 < ../patches/fastspar.patch && ./configure && make -j)

# Clone SparCC and patch
# -l is critical to applying patch correctly (ignores whitespace); -p1 removes the a/ and b/ from file descriptors
hg clone https://bitbucket.org/yonatanf/sparcc
(cd sparcc && patch -lp1 < ../patches/sparcc.patch)

# Prepare for outputs
mkdir -p output/{seeded_estimates,replication}

# Run FastSpar
./fastspar/src/fastspar -f fraction_estimates/ -c assets/otu_table_250_250.tsv -r output/seeded_estimates/fastspar_cor.tsv -a output/seeded_estimates/fastspar_cov.tsv -y

# Run SparCC
./sparcc/SparCC.py assets/otu_table_250_250.tsv -f fraction_estimates/ -c output/seeded_estimates/sparcc_cor.tsv -v output/seeded_estimates/sparcc_cov.tsv

# Revert patches applied to FastSpar and SparCC applied above
(cd fastspar && git clean -fd && git checkout -- .)
(cd sparcc && hg revert --all)


###
# Run FastSpar and SparCC on same dataset 20 times
###
# Recompile FastSpar without patches
(cd fastspar && ./configure && make -j)

# Run replication series; using delay for FastSpar so we get different seeds (seeding based on time)
parallel './sparcc/SparCC.py assets/otu_table_250_250.tsv -c output/replication/sparcc_cor_{}.tsv -v output/replication/sparcc_cov_{}.tsv -i 48 -x 10' ::: {1..20}
parallel --delay 5 './fastspar/src/fastspar -c assets/otu_table_250_250.tsv -r output/replication/fastspar_cor_{}.tsv -a output/replication/fastspar_cov_{}.tsv -i 48 -x 10 -y' ::: {1..20}


###
# Generate plots
###
# Create output directory
mkdir -p output/plots/

# FastSpar v FastSpar; SparCC v SparCC; mean FastSpar v mean SparCC
./assets/plots_correlation_replication.R

# Seeded FastSpar v SparCC
./assets/plots_correlation_seeded.R

# Each correlation output; beware, this plot requires a considerable amount of RAM
#./assets/correlation_combination_plots.R
