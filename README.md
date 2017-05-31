```bash
# Set up chroot environment
sudo -E debootstrap xenial ubuntu_chroot http://archive.ubuntu.com/ubuntu/

# Chroot into environment
sudo -E chroot ubuntu_chroot/

# Set HTTP proxy for apt and add Universe repository
echo 'Acquire::http::Proxy "http://mdhs-wwwproxy.mdhs.unimelb.edu.au:8000";' > /etc/apt/apt.conf
echo 'deb http://archive.ubuntu.com/ubuntu xenial main universe' > /etc/apt/sources.list

# Install packages
apt-get update
apt-get install -y git mercurial build-essential libarmadillo-dev libgsl-dev libopenblas-dev python-numpy python-pandas python3-numpy r-base-core ca-certificates --no-install-recommends

# Change into tmp directory
cd /tmp/

# Clone repo and perform comparison
git clone https://github.com/scwatts/fastspar_comparison.git
cd fastspar_comparison
./run.sh
```
