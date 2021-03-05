#!/usr/bin/env bash
### 
### This script generates bulk calculation input files for juRS  
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

function inp {
    inpfile=$3.$1
    sed -e "s/__aLat__/$2/g" template.hcp_fcc > $inpfile
    sed -e "s/__El__/$1/g"   template.$3     >> $inpfile
    $exe $inpfile $checkmode $loading
    spaces
}

  inp Au 4.17410 fcc
  inp Au 4.17410 hcp


function fcc {
    cat general_parameters > fcc.$1
    sed -e "s/__El__/$1/g" face_centered_cubic >> fcc.$1
    echo       "cell $2"  >> fcc.$1
    $exe fcc.$1 $checkmode $loading
    spaces
}

function hcp {
    cat general_parameters > hcp.$1
    echo       '$L = '"$2"  >> hcp.$1
    echo       '$a = $L sqrt 1:2' >> hcp.$1
    echo       '$c = $a sqrt 8:3' >> hcp.$1
    sed -e "s/__El__/$1/g" hexagonal_close_packed >> hcp.$1
    $exe hcp.$1 $checkmode $loading
    spaces
}

#   fcc Au 4.17410
# CHM     1  Au  12  bs: 12x2.952  as: 24x60 12x90 24x120 6x180

#   hcp Au 4.17410
# CHM     1  Au  12  bs: 12x2.952  as: 24x60 12x90 3x109 18x120 6x146 3x180


### fcc    _cell_length_a
#     fcc Ag 4.16424
#     fcc Al 4.04021 
#     fcc Ar 5.95059 
#     fcc Au 4.17410 
#     fcc Ca 5.52507 
#     fcc Cu 3.63689 
#     fcc Ir 3.87675 
#     fcc Kr 6.42930 
#     fcc Mn 3.59552 
#     fcc Ne 4.62262 
#     fcc Ni 3.52414 
#     fcc Pb 5.04386 
#     fcc Pd 3.95315 
#     fcc Pt 3.97675 
#     fcc Rh 3.84210 
#     fcc Rn 7.19417 
#     fcc Sr 6.01969 
#     fcc Xe 7.05482  

fcc_elements="Ag Al Ar Au Ca Cu Ir Kr Mn Ne Ni Pb Pd Pt Rh Rn Sr Xe"

### hcp    _cell_length_a _cell_length_c
#     hcp Be 2.26276 3.57316
#     hcp Cd 3.03716 5.77061
#     hcp Co 2.49680 4.03081
#     hcp He 2.92661 4.77913
#     hcp Hf 3.20273 5.06493
#     hcp Lu 3.52388 5.48025
#     hcp Mg 3.19405 5.17198
#     hcp Os 2.75895 4.35694
#     hcp Re 2.77447 4.47993
#     hcp Ru 2.72661 4.30050
#     hcp Sc 3.32145 5.16192
#     hcp Tc 2.76187 4.41905
#     hcp Ti 2.93664 4.65193
#     hcp Tl 3.59255 5.64195
#     hcp Y  3.66051 5.67399
#     hcp Zn 2.66169 5.00397
#     hcp Zr 3.23603 5.18953

hcp_elements="Be Cd Co He Hf Lu Mg Os Re Ru Sc Tc Ti Tl Y Zn Zr"
