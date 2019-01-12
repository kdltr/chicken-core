#!/bin/sh

set -e

# build 5.0.1 development snapshot

mkdir -p boot/snapshot
cd boot
wget https://code.call-cc.org/dev-snapshots/2019/01/12/chicken-5.0.1.tar.gz
tar -xzf chicken-5.0.1.tar.gz
cd chicken-5.0.1
make "$@" PREFIX="$(pwd)"/../snapshot
make "$@" PREFIX="$(pwd)"/../snapshot install
cd ../..

# build a boot-chicken from git head using the snapshot
# chicken and then use that to build the real thing

make "$@" spotless
make "$@" CHICKEN="$(pwd)"/boot/snapshot/bin/chicken boot-chicken

# remove snapshot installation and tarball
rm -fr boot/snapshot
rm -fr boot/chicken-5.0.1
rm -f  boot/chicken-5.0.1.tar.gz

echo
echo 'Now, build chicken by passing "CHICKEN=./chicken-boot" to make,'
echo 'in addition to PREFIX, PLATFORM, and other parameters.'
echo
