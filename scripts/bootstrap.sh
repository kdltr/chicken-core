#!/bin/sh

set -e

# build 5.0.0pre8 development snapshot

mkdir -p boot/pre8
cd boot
wget http://code.call-cc.org/dev-snapshots/2018/07/12/chicken-5.0.0pre8.tar.gz
tar -xzf chicken-5.0.0pre8.tar.gz
cd chicken-5.0.0pre8
make "$@" PREFIX="$(pwd)"/../pre8
make "$@" PREFIX="$(pwd)"/../pre8 install
cd ../..

# build a boot-chicken from git head using pre8 chicken and
# then use that to build the real thing

make "$@" spotless
make "$@" CHICKEN="$(pwd)"/boot/pre8/bin/chicken boot-chicken

# remove pre8 installation and tarball
rm -fr boot/pre8
rm -fr boot/chicken-5.0.0pre8
rm -f  boot/chicken-5.0.0pre8.tar.gz

echo
echo 'Now, build chicken by passing "CHICKEN=./chicken-boot" to make,'
echo 'in addition to PREFIX, PLATFORM, and other parameters.'
echo
