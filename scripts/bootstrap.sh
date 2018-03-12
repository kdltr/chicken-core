#!/bin/sh

set -e

# build 5.0.0pre6 development snapshot

mkdir -p boot/pre6
cd boot
wget http://code.call-cc.org/dev-snapshots/2018/03/12/chicken-5.0.0pre6.tar.gz
tar -xzf chicken-5.0.0pre6.tar.gz
cd chicken-5.0.0pre6
make "$@" PREFIX="$(pwd)"/../pre6
make "$@" PREFIX="$(pwd)"/../pre6 install
cd ../..

# build a boot-chicken from git head using pre6 chicken and
# then use that to build the real thing
#
# baf6363e535b26a67d0b9a7d71a93d8deb5de8c6 hardcodes
# some import libraries and removes deprecated internals, so
# a boot-chicken needs to be built.

make "$@" spotless
make "$@" CHICKEN="$(pwd)"/boot/pre6/bin/chicken boot-chicken

# remove pre6 installation and tarball
rm -fr boot/pre6
rm -fr boot/chicken-5.0.0pre6
rm -f  boot/chicken-5.0.0pre6.tar.gz

echo
echo 'Now, build chicken by passing "CHICKEN=./chicken-boot" to make,'
echo 'in addition to PREFIX, PLATFORM, and other parameters.'
echo
