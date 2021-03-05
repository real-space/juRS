#!/usr/bin/env bash

## clear all existing pawdata file
# rm -f pawdata.0*.xml


input_template=single_atom
# for El in {1..57} {71..83} ## H -- La, Lu-- Bi, 4f-elements taken out
for El in {1..5}
# input_template=cheap_atom
# for El in 11 12 23 25 28 41 42 44 45 46 47 52 73 74 76 77 78 ## "cheap"-elements, gpaw-setups-0.9.20000/El.*.PBE
do

# rm -f pawdata.0*.xml

  echo ""
  echo "Start for Z= $El"

  ## create an input file for this element
  inp=single_atom.Z$El
  sed -e "s/__El__/$El/g" $input_template > $inp

  echo " - generate new pawdata for Z= $El"
  ## generate new pawdata
  ./paw $inp --gen > $inp.gen
  grep 'valence state for' $inp.gen
  
  echo " - run checkmode for Z= $El"
  ## test logder and for ghost states
  ./paw $inp -cm 2 > $inp.chk
  grep ' PAW eigenvalues found for l=' $inp.chk

  echo " - run atomic calculation for Z= $El"
  ## run the calculation
  ./paw $inp #> $inp.out
  grep '^          1   1    ' $inp.out

  echo "Done with Z= $El"
  echo ""
  
done
