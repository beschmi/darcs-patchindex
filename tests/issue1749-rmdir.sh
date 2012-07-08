#!/usr/bin/env bash
## Test for issueXXX - darcs remove <dir> corrupts the patch sequence
##
## Copyright (C) 2009 Eric Kow <kowey@darcs.net>
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

. lib                  # Load some portability helpers.
rm -rf R                        # Another script may have left a mess.
darcs init   --repo R
cd R

mkdir dir
touch dir/file

darcs add dir/file              # adds dir too (which is fine)
darcs rec -a -m"add dir and file"

darcs remove dir
# removed dir but not file - should be nothing to add
not darcs rec -a -m"remove dir"

darcs obliterate -a --patch "remove dir" | grep 'No patches selected!'
darcs check
