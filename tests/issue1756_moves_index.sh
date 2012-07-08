#!/usr/bin/env bash
## Test for issue1756 - moving files between directories breaks index
##
## Copyright (C) 2010 Petr Rockai
##
## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this software and associated documentation
## files (the "Software"), to deal in the Software without
## restriction, including without limitation the rights to use, copy,
## modify, merge, publish, distribute, sublicense, and/or sell copies
## of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
## BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
## ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
## CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

. lib                           # Load some portability helpers.
rm -rf R S                      # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.

cd R
mkdir d e                       # Change the working tree.
echo 'a' > d/a
echo 'b' > d/b
echo 'c' > e/c
darcs record -lam '.'
darcs mv d/a e/
darcs check --no-ignore-times
cd ..
rm -rf R

darcs init --repo R
cd R
mkdir d e                       # Change the working tree.
echo 'a' > d/a
echo 'b' > e/b
darcs record -lam '.'
darcs mv d/a e/
darcs check --no-ignore-times
cd ..
rm -rf R
