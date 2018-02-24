#!/bin/sh

set -e

# build 5.0.0pre5 development snapshot

mkdir -p boot/pre5
cd boot
wget http://code.call-cc.org/dev-snapshots/2018/02/24/chicken-5.0.0pre5.tar.gz
tar -xzf chicken-5.0.0pre5.tar.gz
cd chicken-5.0.0pre5
make "$@" PREFIX="$(pwd)"/../pre5
make "$@" PREFIX="$(pwd)"/../pre5 install
cd ../..

# build a boot-chicken from git head using pre5 chicken and
# then use that to build the real thing
#
# baf6363e535b26a67d0b9a7d71a93d8deb5de8c6 hardcodes
# some import libraries and removes deprecated internals, so
# a boot-chicken needs to be built.

make "$@" spotless
make "$@" CHICKEN="$(pwd)"/boot/pre5/bin/chicken boot-chicken

# remove pre5 installation and tarball
rm -fr boot/pre5
rm -fr boot/chicken-5.0.0pre5
rm -f  boot/chicken-5.0.0pre5.tar.gz

echo
echo 'Now, build chicken by passing "CHICKEN=./chicken-boot" to make,'
echo 'in addition to PREFIX, PLATFORM, and other parameters.'
echo
