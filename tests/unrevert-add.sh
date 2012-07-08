#!/usr/bin/env bash

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
echo foo > foo
darcs add foo
darcs whatsnew > correct
cat correct

darcs revert -a

not darcs whatsnew

darcs unrevert -a

darcs whatsnew > unrecorded
cat unrecorded

diff -u correct unrecorded

cd ..
rm -rf temp1
