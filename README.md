# FastSpar and SparCC comparison of correlates
The scripts in this repository compare the correlations computed by FastSpar and SparCC. The high level description of processes taken here is:
1. Set up a clean and reproducible run environment using `chroot`
2. Resolve dependencies for FastSpar and SparCC
3. Provision the FastSpar and SparCC software
4. Run FastSpar and SparCC
    * with pre-generated OTU fractions, and
    * 20 independent replicates of the same dataset
5. Plot results


The repository containing a less detailed comparison of all SparCC implementations can be found [here](https://github.com/scwatts/sparcc_implementation_comparison).

# Performing this analysis
## Requirements
There are a few requirements to run this analysis:
* A modern computer with amd64 architecture running GNU/Linux
* Run commands as a supseruser (e.g. using `sudo`)
* Have `debootstrap` installed
* An internet connection

## Running
To run this analysis, a `chroot` environment is first required. The following commands will create a Ubuntu 16.04 (Xenial) `chroot` in the current working directory named `ubuntu_chroot` and run an interactive shell with it:
```bash
# Set up chroot environment
sudo debootstrap xenial ubuntu_chroot http://archive.ubuntu.com/ubuntu/

# Chroot into environment
sudo chroot ubuntu_chroot/
```

Next the appropriate dependencies must be installed within the `chroot`:
```bash
# Add Universe repository
echo 'deb http://archive.ubuntu.com/ubuntu xenial main universe' > /etc/apt/sources.list

# Install packages
apt-get update
apt-get install -y git mercurial autoconf build-essential libarmadillo-dev libgsl-dev libopenblas-dev python-numpy python-pandas python3-numpy r-base-core ca-certificates --no-install-recommends
```

Finally this repository can be cloned and the analysis run:
```bash
# Change into tmp directory
cd /tmp/

# Clone repo and perform comparison
git clone https://github.com/scwatts/fastspar_comparison.git
cd fastspar_comparison
./run.sh
```
