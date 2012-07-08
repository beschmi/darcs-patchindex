#!/usr/bin/env bash
# 2011, by Petr Rockai, Guillaume Hoffmann, public domain

# Tets that darcs get --verbose reports getting a pack when there is one,
# and does not report when there is none or when --no-packs is passed.

. lib

rm -rf R S && mkdir R
cd R
darcs init

darcs show repo > repo
grep "Format: hashed" repo || exit 200 # not supported with oldfashioned repos

for f in `seq 1 200`; do
    echo $f > $f
    darcs rec -lam $f
done

darcs optimize # FIXME the pristine.hashed in the basic tarball has extra files
               # without this step

darcs optimize --http
test -e _darcs/packs/basic.tar.gz
test -e _darcs/packs/patches.tar.gz
cd ..

serve_http # sets baseurl

# check that default behaviour is to get packs
darcs get $baseurl/R S --verbose |grep "Getting packed repository"

# check that it does really not get packs when --no-packs is passed
rm -rf S
darcs get $baseurl/R S --no-packs --verbose |not grep "Getting packed repository"

# check that it does not clam getting packs when there are not
rm -rf S
rm -rf R/_darcs/packs/
darcs get $baseurl/R S --verbose |not grep "Getting packed repository"
