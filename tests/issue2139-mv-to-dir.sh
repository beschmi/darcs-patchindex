#!/usr/bin/env bash
## Test for issue2139 - darcs should accept to mv to the
## current working directory
##
## Copyright (C) 2012 Eric Kow
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

darcs init      --repo R        # Create our test repos.

cd R

# move dir to root
mkdir a a/a2 a/a3
darcs record -lam 'Some directories (a)'
darcs mv a/a2 .
test -d a2 
cd a
darcs mv a3 ..
not test -d a3
cd ..
test -d a3

# move dir to non-root dir
mkdir b b2 b3
darcs record -lam 'Some directories (b)'
darcs mv b2 b
test -d b/b2
cd b
darcs mv ../b3 .
test -d b3
cd ..

cd ..
