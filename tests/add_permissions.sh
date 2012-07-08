#!/usr/bin/env bash
## Darcs should refuse to add an unreadable file, because unreadable
## files aren't recordable.
##
## Copyright (C) 2005  Mark Stosberg
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

. lib

abort_windows                   # does not work on Windows.
mkdir temp1
cd temp1
darcs initialize
touch unreadable
chmod a-r unreadable      # Make the file unreadable.
not darcs add unreadable 2> log
fgrep -i 'permission denied' log
# Testing by hand with a directory works, but darcs-test gets
# stuck by having an unreadable subdir.
#mkdir d
#chmod a-r d
#not darcs add --debug --verbose d
#fgrep -i 'permission denied' log
