#version 3.6;
#include "colors.inc"
#include "textures.inc"
#include "shapes.inc"
#include "glass.inc"
#include "metals.inc"

background {color White}

plane { <0,1,0>, -11.514 pigment{color <.6,.6,.6>} finish{ reflection {.2} ambient .5 diffuse .5 metallic }}

camera { location <68.48,22.83,45.66> look_at <-2.75,-2.96,-2.65> right x*image_width/image_height/2 up y/2 }

// add lights
light_source { <0,200,400> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }
light_source { <400,200,0> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }


// add the atoms
sphere{ <.00,.00,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }

// add the cell borders
cylinder { <-11.238,-11.414,-11.414> <-9.834,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.482,-11.414,-11.414> <-8.078,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-7.726,-11.414,-11.414> <-6.322,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.970,-11.414,-11.414> <-4.566,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.214,-11.414,-11.414> <-2.810,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.458,-11.414,-11.414> <-1.054,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.702,-11.414,-11.414> <.702,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.054,-11.414,-11.414> <2.458,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.810,-11.414,-11.414> <4.214,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.566,-11.414,-11.414> <5.970,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.322,-11.414,-11.414> <7.726,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.078,-11.414,-11.414> <9.482,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <9.834,-11.414,-11.414> <11.238,-11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.238,-11.414,11.414> <-9.834,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.482,-11.414,11.414> <-8.078,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-7.726,-11.414,11.414> <-6.322,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.970,-11.414,11.414> <-4.566,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.214,-11.414,11.414> <-2.810,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.458,-11.414,11.414> <-1.054,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.702,-11.414,11.414> <.702,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.054,-11.414,11.414> <2.458,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.810,-11.414,11.414> <4.214,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.566,-11.414,11.414> <5.970,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.322,-11.414,11.414> <7.726,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.078,-11.414,11.414> <9.482,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <9.834,-11.414,11.414> <11.238,-11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.238,11.414,-11.414> <-9.834,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.482,11.414,-11.414> <-8.078,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-7.726,11.414,-11.414> <-6.322,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.970,11.414,-11.414> <-4.566,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.214,11.414,-11.414> <-2.810,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.458,11.414,-11.414> <-1.054,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.702,11.414,-11.414> <.702,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.054,11.414,-11.414> <2.458,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.810,11.414,-11.414> <4.214,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.566,11.414,-11.414> <5.970,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.322,11.414,-11.414> <7.726,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.078,11.414,-11.414> <9.482,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <9.834,11.414,-11.414> <11.238,11.414,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.238,11.414,11.414> <-9.834,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.482,11.414,11.414> <-8.078,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-7.726,11.414,11.414> <-6.322,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.970,11.414,11.414> <-4.566,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.214,11.414,11.414> <-2.810,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.458,11.414,11.414> <-1.054,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.702,11.414,11.414> <.702,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.054,11.414,11.414> <2.458,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.810,11.414,11.414> <4.214,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.566,11.414,11.414> <5.970,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.322,11.414,11.414> <7.726,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.078,11.414,11.414> <9.482,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <9.834,11.414,11.414> <11.238,11.414,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,-11.238> <-11.414,-11.414,-9.834>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,-9.482> <-11.414,-11.414,-8.078>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,-7.726> <-11.414,-11.414,-6.322>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,-5.970> <-11.414,-11.414,-4.566>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,-4.214> <-11.414,-11.414,-2.810>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,-2.458> <-11.414,-11.414,-1.054>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,-.702> <-11.414,-11.414,.702>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,1.054> <-11.414,-11.414,2.458>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,2.810> <-11.414,-11.414,4.214>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,4.566> <-11.414,-11.414,5.970>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,6.322> <-11.414,-11.414,7.726>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,8.078> <-11.414,-11.414,9.482>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.414,9.834> <-11.414,-11.414,11.238>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,-11.238> <-11.414,11.414,-9.834>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,-9.482> <-11.414,11.414,-8.078>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,-7.726> <-11.414,11.414,-6.322>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,-5.970> <-11.414,11.414,-4.566>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,-4.214> <-11.414,11.414,-2.810>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,-2.458> <-11.414,11.414,-1.054>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,-.702> <-11.414,11.414,.702>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,1.054> <-11.414,11.414,2.458>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,2.810> <-11.414,11.414,4.214>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,4.566> <-11.414,11.414,5.970>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,6.322> <-11.414,11.414,7.726>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,8.078> <-11.414,11.414,9.482>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,11.414,9.834> <-11.414,11.414,11.238>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,-11.238> <11.414,-11.414,-9.834>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,-9.482> <11.414,-11.414,-8.078>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,-7.726> <11.414,-11.414,-6.322>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,-5.970> <11.414,-11.414,-4.566>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,-4.214> <11.414,-11.414,-2.810>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,-2.458> <11.414,-11.414,-1.054>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,-.702> <11.414,-11.414,.702>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,1.054> <11.414,-11.414,2.458>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,2.810> <11.414,-11.414,4.214>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,4.566> <11.414,-11.414,5.970>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,6.322> <11.414,-11.414,7.726>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,8.078> <11.414,-11.414,9.482>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.414,9.834> <11.414,-11.414,11.238>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,-11.238> <11.414,11.414,-9.834>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,-9.482> <11.414,11.414,-8.078>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,-7.726> <11.414,11.414,-6.322>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,-5.970> <11.414,11.414,-4.566>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,-4.214> <11.414,11.414,-2.810>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,-2.458> <11.414,11.414,-1.054>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,-.702> <11.414,11.414,.702>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,1.054> <11.414,11.414,2.458>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,2.810> <11.414,11.414,4.214>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,4.566> <11.414,11.414,5.970>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,6.322> <11.414,11.414,7.726>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,8.078> <11.414,11.414,9.482>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,11.414,9.834> <11.414,11.414,11.238>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.238,-11.414> <-11.414,-9.834,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-9.482,-11.414> <-11.414,-8.078,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-7.726,-11.414> <-11.414,-6.322,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-5.970,-11.414> <-11.414,-4.566,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-4.214,-11.414> <-11.414,-2.810,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-2.458,-11.414> <-11.414,-1.054,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-.702,-11.414> <-11.414,.702,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,1.054,-11.414> <-11.414,2.458,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,2.810,-11.414> <-11.414,4.214,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,4.566,-11.414> <-11.414,5.970,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,6.322,-11.414> <-11.414,7.726,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,8.078,-11.414> <-11.414,9.482,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,9.834,-11.414> <-11.414,11.238,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.238,-11.414> <11.414,-9.834,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-9.482,-11.414> <11.414,-8.078,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-7.726,-11.414> <11.414,-6.322,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-5.970,-11.414> <11.414,-4.566,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-4.214,-11.414> <11.414,-2.810,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-2.458,-11.414> <11.414,-1.054,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-.702,-11.414> <11.414,.702,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,1.054,-11.414> <11.414,2.458,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,2.810,-11.414> <11.414,4.214,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,4.566,-11.414> <11.414,5.970,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,6.322,-11.414> <11.414,7.726,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,8.078,-11.414> <11.414,9.482,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,9.834,-11.414> <11.414,11.238,-11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-11.238,11.414> <-11.414,-9.834,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-9.482,11.414> <-11.414,-8.078,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-7.726,11.414> <-11.414,-6.322,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-5.970,11.414> <-11.414,-4.566,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-4.214,11.414> <-11.414,-2.810,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-2.458,11.414> <-11.414,-1.054,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,-.702,11.414> <-11.414,.702,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,1.054,11.414> <-11.414,2.458,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,2.810,11.414> <-11.414,4.214,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,4.566,11.414> <-11.414,5.970,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,6.322,11.414> <-11.414,7.726,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,8.078,11.414> <-11.414,9.482,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.414,9.834,11.414> <-11.414,11.238,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-11.238,11.414> <11.414,-9.834,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-9.482,11.414> <11.414,-8.078,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-7.726,11.414> <11.414,-6.322,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-5.970,11.414> <11.414,-4.566,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-4.214,11.414> <11.414,-2.810,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-2.458,11.414> <11.414,-1.054,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,-.702,11.414> <11.414,.702,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,1.054,11.414> <11.414,2.458,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,2.810,11.414> <11.414,4.214,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,4.566,11.414> <11.414,5.970,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,6.322,11.414> <11.414,7.726,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,8.078,11.414> <11.414,9.482,11.414>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.414,9.834,11.414> <11.414,11.238,11.414>, .1 pigment { color <.9,.9,.9> } }
