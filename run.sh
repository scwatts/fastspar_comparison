#!/bin/bash
# Download counts and generate fractions
./assets/generate_fractions.py -c fake_data.txt -o fraction_estimates -n 50

# Clone FastSpar and patch
# -l is critical to applying patch correctly (ignores whitespace); -p1 removes the a/ and b/ from file descriptors
git clone https://github.com/scwatts/fastspar.git
(cd fastspar && patch -p1 < ../patches/fastspar.patch && ./configure && make -j)


# Clone SparCC and patch
# -l is critical to applying patch correctly (ignores whitespace); -p1 removes the a/ and b/ from file descriptors
hg clone https://bitbucket.org/yonatanf/sparcc
(cd sparcc && patch -lp1 < ../patches/sparcc.patch)

# Prepare for outputs
mkdir output

# Run FastSpar
./fastspar/src/fastspar -f fraction_estimates/ -c assets/fake_data.txt -r output/fastspar_cor.tsv -a output/fastspar_cov.tsv

# Run SparCC
./sparcc/SparCC.py assets/fake_data.txt -f fraction_estimates/ -c output/sparcc_cor.tsv -v output/sparcc_cov.tsv
