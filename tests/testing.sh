#!/bin/bash

OUT=$(mktemp -d "outXXX")

echo "For each .html file generate corresponding Lucid code
echo "in .hs file. Then produce html back from Lucid."
echo
echo "Processing html"

for i in *.html;
  do hs=${i%.html}.hs;
  echo $i;
  lucid-from-html -s -t $i > ${OUT}/$hs;
done
echo "---- Changing to $OUT"
cd ${OUT}/
echo "---- Generating html from the following .hs"
for i in *.hs;
  do echo $i;
  runhaskell $i > ${i%.hs}-${OUT}.html;
done
echo "---- Look into $OUT by"
echo "cd $OUT"
