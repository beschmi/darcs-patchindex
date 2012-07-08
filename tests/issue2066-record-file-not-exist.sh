set -ev

rm -rf temp
mkdir -p temp
cd temp

darcs init
touch a b
darcs add a
darcs record -a -m "record a" a
rm a
darcs add b
darcs record -a -m "record a" a
