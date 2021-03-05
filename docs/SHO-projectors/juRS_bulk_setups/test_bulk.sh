#!/usr/bin/env bash
### 
### This script generates bulk calculation input files for juRS  
### for the Delta DFT project https://molmod.ugent.be/deltacodesdft
### 
### 

# exe=../paw
exe=echo ## script debugging mode
checkmode=" -cm"
loading="" ## "-l"

### all lattice parameters given in Ang

function spaces { 
# echo ""
  : 
} 

function sc {
    cat general_parameters > $1
    sed -e "s/__El__/$1/g" simple_cubic >> $1
    echo       "cell $2"  >> $1
    $exe $1 $checkmode $loading
    spaces
}

function bcc {
    cat general_parameters > $1
    sed -e "s/__El__/$1/g" body_centered_cubic >> $1
    echo       "cell $2"  >> $1
    $exe $1 $checkmode $loading
    spaces
}

function fcc {
    cat general_parameters > $1
    sed -e "s/__El__/$1/g" face_centered_cubic >> $1
    echo       "cell $2"  >> $1
    $exe $1 $checkmode $loading
    spaces
}

function hcp {
    cat general_parameters > $1
    echo       '$a = '"$2"  >> $1
    echo       '$c = '"$3"  >> $1
    sed -e "s/__El__/$1/g" hexagonal_close_packed >> $1
    $exe $1 $checkmode $loading
    spaces
#     ## show the c/a ratio
#     ca=`python -c "print($3 / $2)"`
#     ca1633=`python -c "print($3 / $2 / 1.633)"`
#     echo  "$1 c/a ratio = $ca = $ca1633 * sqrt(8/3)"
}

function dia {
    cat general_parameters > $1
    sed -e "s/__El__/$1/g" diamond_structure >> $1
    echo       "cell $2"  >> $1
    $exe $1 $checkmode $loading
    spaces
}

function inp {
    cat general_parameters > $1
    cat    $2  >> $1 ## $2 must be an input file defining a cell and positions
    $exe $1 $checkmode $loading
    spaces
}

### sc    _cell_length_a
    sc Po 3.34818
    
sc_elements="Po"

### fcc    _cell_length_a
    fcc Ag 4.16424
    fcc Al 4.04021 
    fcc Ar 5.95059 
    fcc Au 4.17410 
    fcc Ca 5.52507 
    fcc Cu 3.63689 
    fcc Ir 3.87675 
    fcc Kr 6.42930 
    fcc Mn 3.59552 
    fcc Ne 4.62262 
    fcc Ni 3.52414 
    fcc Pb 5.04386 
    fcc Pd 3.95315 
    fcc Pt 3.97675 
    fcc Rh 3.84210 
    fcc Rn 7.19417 
    fcc Sr 6.01969 
    fcc Xe 7.05482  

fcc_elements="Ag Al Ar Au Ca Cu Ir Kr Mn Ne Ni Pb Pd Pt Rh Rn Sr Xe"

### bcc    _cell_length_a
    bcc Ba 5.02792 
    bcc Cr 2.87100 
    bcc Cs 6.16153 
    bcc Fe 2.83351 
    bcc Hg 4.10305 
    bcc In 3.29841 
    bcc K  5.28589 
    bcc Mo 3.16930 
    bcc Nb 3.32239 
    bcc Rb 5.67197 
    bcc Ta 3.32169 
    bcc V  2.99894 
    bcc W  3.18968 

bcc_elements="Ba Cr Cs Fe Hg In K Mo Nb Rb Ta V W"

### hcp    _cell_length_a _cell_length_c
    hcp Be 2.26276 3.57316
    hcp Cd 3.03716 5.77061
    hcp Co 2.49680 4.03081
    hcp He 2.92661 4.77913
    hcp Hf 3.20273 5.06493
    hcp Lu 3.52388 5.48025
    hcp Mg 3.19405 5.17198
    hcp Os 2.75895 4.35694
    hcp Re 2.77447 4.47993
    hcp Ru 2.72661 4.30050
    hcp Sc 3.32145 5.16192
    hcp Tc 2.76187 4.41905
    hcp Ti 2.93664 4.65193
    hcp Tl 3.59255 5.64195
    hcp Y  3.66051 5.67399
    hcp Zn 2.66169 5.00397
    hcp Zr 3.23603 5.18953

hcp_elements="Be Cd Co He Hf Lu Mg Os Re Ru Sc Tc Ti Tl Y Zn Zr"

### diamond lattice
    dia Si 3.86709
    dia Sn 4.70647
    dia Ge 4.07419

dia_elements="Si Sn Ge"

### ToDo 
# Sb  #antimony
# Se  #selenium

### for these elements the input file only needs to append the general_parameters:

### input files given
    inp H  hydrogen
    inp C  carbon
    inp N  nitrogen
    inp P  phosphorous
    inp Cl chlorine
    inp Ga gallium
    inp As arsenic
    inp Se selenium
    inp Br bromine
    inp Sb antimony
    inp Te tellurium
    inp I  iodine
    inp Bi bismuth

inp_elements="H C N P Cl Ga As Se Br Sb Te I Bi"

## special lattices with angles not compliant with orthorhombic
not_elements="B Li F S O Na"

### check if all elements have been treated or are in the $not_elements list
# for El in $sc_elements $fcc_elements $bcc_elements $hcp_elements $dia_elements $inp_elements $not_elements
# do
#   rm $El.cif
# done

