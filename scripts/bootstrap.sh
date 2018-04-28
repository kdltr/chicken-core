#!/bin/sh

set -e

# build 5.0.0pre7 development snapshot

mkdir -p boot/pre7
cd boot
wget http://code.call-cc.org/dev-snapshots/2018/04/29/chicken-5.0.0pre7.tar.gz
tar -xzf chicken-5.0.0pre7.tar.gz
cd chicken-5.0.0pre7
make "$@" PREFIX="$(pwd)"/../pre7
make "$@" PREFIX="$(pwd)"/../pre7 install
cd ../..

# build a boot-chicken from git head using pre7 chicken and
# then use that to build the real thing

make "$@" spotless
make "$@" CHICKEN="$(pwd)"/boot/pre7/bin/chicken boot-chicken

# remove pre7 installation and tarball
rm -fr boot/pre7
rm -fr boot/chicken-5.0.0pre7
rm -f  boot/chicken-5.0.0pre7.tar.gz

echo
echo 'Now, build chicken by passing "CHICKEN=./chicken-boot" to make,'
echo 'in addition to PREFIX, PLATFORM, and other parameters.'
echo
