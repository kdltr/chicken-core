#!/bin/sh

set -e

# build 5.0.0pre4 development snapshot

mkdir -p boot/pre4
cd boot
wget http://code.call-cc.org/dev-snapshots/2017/12/14/chicken-5.0.0pre4.tar.gz
tar -xzf chicken-5.0.0pre4.tar.gz
cd chicken-5.0.0pre4
make PREFIX="$(pwd)"/../pre4 "$@"
make PREFIX="$(pwd)"/../pre4 "$@" install
cd ../..

# build a boot-chicken from git head using pre4 chicken and
# then use that to build the real thing
#
# baf6363e535b26a67d0b9a7d71a93d8deb5de8c6 hardcodes
# some import libraries and removes deprecated internals, so
# a boot-chicken needs to be built.

make "$@" spotless
make CHICKEN="$(pwd)"/boot/pre4/bin/chicken "$@" boot-chicken

# remove pre4 installation and tarball
rm -fr boot/pre4
rm -fr boot/chicken-5.0.0pre4
rm -f  boot/chicken-5.0.0pre4.tar.gz

echo
echo 'Now, build chicken by passing "CHICKEN=./chicken-boot" to make,'
echo 'in addition to PREFIX, PLATFORM, and other parameters.'
echo
