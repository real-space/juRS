#version 3.6;
#include "colors.inc"
#include "textures.inc"
#include "shapes.inc"
#include "glass.inc"
#include "metals.inc"

background {color White}

plane { <0,1,0>, -22.928 pigment{color <.6,.6,.6>} finish{ reflection {.2} ambient .5 diffuse .5 metallic }}

camera { location <136.97,45.66,91.31> look_at <-2.66,-2.89,-2.78> right x*image_width/image_height/2 up y/2 }

// add lights
light_source { <0,200,400> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }
light_source { <400,200,0> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }


// add the atoms
sphere{ <.00,.00,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.12,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,17.12,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }

// add the cell borders
cylinder { <-22.645,-22.828,-22.828> <-21.184,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-20.819,-22.828,-22.828> <-19.358,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-18.993,-22.828,-22.828> <-17.532,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.167,-22.828,-22.828> <-15.706,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.340,-22.828,-22.828> <-13.879,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.514,-22.828,-22.828> <-12.053,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.688,-22.828,-22.828> <-10.227,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.862,-22.828,-22.828> <-8.401,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-8.035,-22.828,-22.828> <-6.574,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.209,-22.828,-22.828> <-4.748,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.383,-22.828,-22.828> <-2.922,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.557,-22.828,-22.828> <-1.096,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.730,-22.828,-22.828> <.730,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.096,-22.828,-22.828> <2.557,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.922,-22.828,-22.828> <4.383,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.748,-22.828,-22.828> <6.209,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.574,-22.828,-22.828> <8.035,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.401,-22.828,-22.828> <9.862,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.227,-22.828,-22.828> <11.688,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <12.053,-22.828,-22.828> <13.514,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <13.879,-22.828,-22.828> <15.340,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <15.706,-22.828,-22.828> <17.167,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.532,-22.828,-22.828> <18.993,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <19.358,-22.828,-22.828> <20.819,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <21.184,-22.828,-22.828> <22.645,-22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.645,-22.828,22.828> <-21.184,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-20.819,-22.828,22.828> <-19.358,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-18.993,-22.828,22.828> <-17.532,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.167,-22.828,22.828> <-15.706,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.340,-22.828,22.828> <-13.879,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.514,-22.828,22.828> <-12.053,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.688,-22.828,22.828> <-10.227,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.862,-22.828,22.828> <-8.401,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-8.035,-22.828,22.828> <-6.574,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.209,-22.828,22.828> <-4.748,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.383,-22.828,22.828> <-2.922,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.557,-22.828,22.828> <-1.096,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.730,-22.828,22.828> <.730,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.096,-22.828,22.828> <2.557,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.922,-22.828,22.828> <4.383,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.748,-22.828,22.828> <6.209,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.574,-22.828,22.828> <8.035,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.401,-22.828,22.828> <9.862,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.227,-22.828,22.828> <11.688,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <12.053,-22.828,22.828> <13.514,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <13.879,-22.828,22.828> <15.340,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <15.706,-22.828,22.828> <17.167,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.532,-22.828,22.828> <18.993,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <19.358,-22.828,22.828> <20.819,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <21.184,-22.828,22.828> <22.645,-22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.645,22.828,-22.828> <-21.184,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-20.819,22.828,-22.828> <-19.358,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-18.993,22.828,-22.828> <-17.532,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.167,22.828,-22.828> <-15.706,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.340,22.828,-22.828> <-13.879,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.514,22.828,-22.828> <-12.053,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.688,22.828,-22.828> <-10.227,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.862,22.828,-22.828> <-8.401,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-8.035,22.828,-22.828> <-6.574,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.209,22.828,-22.828> <-4.748,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.383,22.828,-22.828> <-2.922,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.557,22.828,-22.828> <-1.096,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.730,22.828,-22.828> <.730,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.096,22.828,-22.828> <2.557,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.922,22.828,-22.828> <4.383,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.748,22.828,-22.828> <6.209,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.574,22.828,-22.828> <8.035,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.401,22.828,-22.828> <9.862,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.227,22.828,-22.828> <11.688,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <12.053,22.828,-22.828> <13.514,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <13.879,22.828,-22.828> <15.340,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <15.706,22.828,-22.828> <17.167,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.532,22.828,-22.828> <18.993,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <19.358,22.828,-22.828> <20.819,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <21.184,22.828,-22.828> <22.645,22.828,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.645,22.828,22.828> <-21.184,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-20.819,22.828,22.828> <-19.358,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-18.993,22.828,22.828> <-17.532,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.167,22.828,22.828> <-15.706,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.340,22.828,22.828> <-13.879,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.514,22.828,22.828> <-12.053,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.688,22.828,22.828> <-10.227,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.862,22.828,22.828> <-8.401,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-8.035,22.828,22.828> <-6.574,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.209,22.828,22.828> <-4.748,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.383,22.828,22.828> <-2.922,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.557,22.828,22.828> <-1.096,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.730,22.828,22.828> <.730,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.096,22.828,22.828> <2.557,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.922,22.828,22.828> <4.383,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.748,22.828,22.828> <6.209,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.574,22.828,22.828> <8.035,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.401,22.828,22.828> <9.862,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.227,22.828,22.828> <11.688,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <12.053,22.828,22.828> <13.514,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <13.879,22.828,22.828> <15.340,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <15.706,22.828,22.828> <17.167,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.532,22.828,22.828> <18.993,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <19.358,22.828,22.828> <20.819,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <21.184,22.828,22.828> <22.645,22.828,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-22.645> <-22.828,-22.828,-21.184>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-20.819> <-22.828,-22.828,-19.358>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-18.993> <-22.828,-22.828,-17.532>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-17.167> <-22.828,-22.828,-15.706>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-15.340> <-22.828,-22.828,-13.879>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-13.514> <-22.828,-22.828,-12.053>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-11.688> <-22.828,-22.828,-10.227>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-9.862> <-22.828,-22.828,-8.401>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-8.035> <-22.828,-22.828,-6.574>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-6.209> <-22.828,-22.828,-4.748>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-4.383> <-22.828,-22.828,-2.922>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-2.557> <-22.828,-22.828,-1.096>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,-.730> <-22.828,-22.828,.730>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,1.096> <-22.828,-22.828,2.557>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,2.922> <-22.828,-22.828,4.383>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,4.748> <-22.828,-22.828,6.209>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,6.574> <-22.828,-22.828,8.035>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,8.401> <-22.828,-22.828,9.862>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,10.227> <-22.828,-22.828,11.688>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,12.053> <-22.828,-22.828,13.514>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,13.879> <-22.828,-22.828,15.340>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,15.706> <-22.828,-22.828,17.167>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,17.532> <-22.828,-22.828,18.993>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,19.358> <-22.828,-22.828,20.819>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.828,21.184> <-22.828,-22.828,22.645>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-22.645> <-22.828,22.828,-21.184>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-20.819> <-22.828,22.828,-19.358>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-18.993> <-22.828,22.828,-17.532>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-17.167> <-22.828,22.828,-15.706>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-15.340> <-22.828,22.828,-13.879>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-13.514> <-22.828,22.828,-12.053>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-11.688> <-22.828,22.828,-10.227>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-9.862> <-22.828,22.828,-8.401>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-8.035> <-22.828,22.828,-6.574>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-6.209> <-22.828,22.828,-4.748>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-4.383> <-22.828,22.828,-2.922>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-2.557> <-22.828,22.828,-1.096>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,-.730> <-22.828,22.828,.730>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,1.096> <-22.828,22.828,2.557>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,2.922> <-22.828,22.828,4.383>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,4.748> <-22.828,22.828,6.209>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,6.574> <-22.828,22.828,8.035>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,8.401> <-22.828,22.828,9.862>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,10.227> <-22.828,22.828,11.688>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,12.053> <-22.828,22.828,13.514>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,13.879> <-22.828,22.828,15.340>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,15.706> <-22.828,22.828,17.167>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,17.532> <-22.828,22.828,18.993>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,19.358> <-22.828,22.828,20.819>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,22.828,21.184> <-22.828,22.828,22.645>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-22.645> <22.828,-22.828,-21.184>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-20.819> <22.828,-22.828,-19.358>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-18.993> <22.828,-22.828,-17.532>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-17.167> <22.828,-22.828,-15.706>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-15.340> <22.828,-22.828,-13.879>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-13.514> <22.828,-22.828,-12.053>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-11.688> <22.828,-22.828,-10.227>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-9.862> <22.828,-22.828,-8.401>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-8.035> <22.828,-22.828,-6.574>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-6.209> <22.828,-22.828,-4.748>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-4.383> <22.828,-22.828,-2.922>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-2.557> <22.828,-22.828,-1.096>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,-.730> <22.828,-22.828,.730>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,1.096> <22.828,-22.828,2.557>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,2.922> <22.828,-22.828,4.383>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,4.748> <22.828,-22.828,6.209>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,6.574> <22.828,-22.828,8.035>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,8.401> <22.828,-22.828,9.862>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,10.227> <22.828,-22.828,11.688>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,12.053> <22.828,-22.828,13.514>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,13.879> <22.828,-22.828,15.340>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,15.706> <22.828,-22.828,17.167>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,17.532> <22.828,-22.828,18.993>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,19.358> <22.828,-22.828,20.819>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.828,21.184> <22.828,-22.828,22.645>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-22.645> <22.828,22.828,-21.184>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-20.819> <22.828,22.828,-19.358>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-18.993> <22.828,22.828,-17.532>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-17.167> <22.828,22.828,-15.706>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-15.340> <22.828,22.828,-13.879>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-13.514> <22.828,22.828,-12.053>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-11.688> <22.828,22.828,-10.227>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-9.862> <22.828,22.828,-8.401>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-8.035> <22.828,22.828,-6.574>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-6.209> <22.828,22.828,-4.748>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-4.383> <22.828,22.828,-2.922>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-2.557> <22.828,22.828,-1.096>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,-.730> <22.828,22.828,.730>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,1.096> <22.828,22.828,2.557>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,2.922> <22.828,22.828,4.383>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,4.748> <22.828,22.828,6.209>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,6.574> <22.828,22.828,8.035>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,8.401> <22.828,22.828,9.862>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,10.227> <22.828,22.828,11.688>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,12.053> <22.828,22.828,13.514>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,13.879> <22.828,22.828,15.340>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,15.706> <22.828,22.828,17.167>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,17.532> <22.828,22.828,18.993>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,19.358> <22.828,22.828,20.819>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,22.828,21.184> <22.828,22.828,22.645>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.645,-22.828> <-22.828,-21.184,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-20.819,-22.828> <-22.828,-19.358,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-18.993,-22.828> <-22.828,-17.532,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-17.167,-22.828> <-22.828,-15.706,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-15.340,-22.828> <-22.828,-13.879,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-13.514,-22.828> <-22.828,-12.053,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-11.688,-22.828> <-22.828,-10.227,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-9.862,-22.828> <-22.828,-8.401,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-8.035,-22.828> <-22.828,-6.574,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-6.209,-22.828> <-22.828,-4.748,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-4.383,-22.828> <-22.828,-2.922,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-2.557,-22.828> <-22.828,-1.096,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-.730,-22.828> <-22.828,.730,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,1.096,-22.828> <-22.828,2.557,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,2.922,-22.828> <-22.828,4.383,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,4.748,-22.828> <-22.828,6.209,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,6.574,-22.828> <-22.828,8.035,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,8.401,-22.828> <-22.828,9.862,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,10.227,-22.828> <-22.828,11.688,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,12.053,-22.828> <-22.828,13.514,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,13.879,-22.828> <-22.828,15.340,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,15.706,-22.828> <-22.828,17.167,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,17.532,-22.828> <-22.828,18.993,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,19.358,-22.828> <-22.828,20.819,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,21.184,-22.828> <-22.828,22.645,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.645,-22.828> <22.828,-21.184,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-20.819,-22.828> <22.828,-19.358,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-18.993,-22.828> <22.828,-17.532,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-17.167,-22.828> <22.828,-15.706,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-15.340,-22.828> <22.828,-13.879,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-13.514,-22.828> <22.828,-12.053,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-11.688,-22.828> <22.828,-10.227,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-9.862,-22.828> <22.828,-8.401,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-8.035,-22.828> <22.828,-6.574,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-6.209,-22.828> <22.828,-4.748,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-4.383,-22.828> <22.828,-2.922,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-2.557,-22.828> <22.828,-1.096,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-.730,-22.828> <22.828,.730,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,1.096,-22.828> <22.828,2.557,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,2.922,-22.828> <22.828,4.383,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,4.748,-22.828> <22.828,6.209,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,6.574,-22.828> <22.828,8.035,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,8.401,-22.828> <22.828,9.862,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,10.227,-22.828> <22.828,11.688,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,12.053,-22.828> <22.828,13.514,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,13.879,-22.828> <22.828,15.340,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,15.706,-22.828> <22.828,17.167,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,17.532,-22.828> <22.828,18.993,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,19.358,-22.828> <22.828,20.819,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,21.184,-22.828> <22.828,22.645,-22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-22.645,22.828> <-22.828,-21.184,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-20.819,22.828> <-22.828,-19.358,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-18.993,22.828> <-22.828,-17.532,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-17.167,22.828> <-22.828,-15.706,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-15.340,22.828> <-22.828,-13.879,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-13.514,22.828> <-22.828,-12.053,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-11.688,22.828> <-22.828,-10.227,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-9.862,22.828> <-22.828,-8.401,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-8.035,22.828> <-22.828,-6.574,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-6.209,22.828> <-22.828,-4.748,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-4.383,22.828> <-22.828,-2.922,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-2.557,22.828> <-22.828,-1.096,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-.730,22.828> <-22.828,.730,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,1.096,22.828> <-22.828,2.557,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,2.922,22.828> <-22.828,4.383,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,4.748,22.828> <-22.828,6.209,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,6.574,22.828> <-22.828,8.035,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,8.401,22.828> <-22.828,9.862,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,10.227,22.828> <-22.828,11.688,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,12.053,22.828> <-22.828,13.514,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,13.879,22.828> <-22.828,15.340,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,15.706,22.828> <-22.828,17.167,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,17.532,22.828> <-22.828,18.993,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,19.358,22.828> <-22.828,20.819,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,21.184,22.828> <-22.828,22.645,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-22.645,22.828> <22.828,-21.184,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-20.819,22.828> <22.828,-19.358,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-18.993,22.828> <22.828,-17.532,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-17.167,22.828> <22.828,-15.706,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-15.340,22.828> <22.828,-13.879,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-13.514,22.828> <22.828,-12.053,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-11.688,22.828> <22.828,-10.227,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-9.862,22.828> <22.828,-8.401,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-8.035,22.828> <22.828,-6.574,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-6.209,22.828> <22.828,-4.748,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-4.383,22.828> <22.828,-2.922,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-2.557,22.828> <22.828,-1.096,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,-.730,22.828> <22.828,.730,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,1.096,22.828> <22.828,2.557,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,2.922,22.828> <22.828,4.383,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,4.748,22.828> <22.828,6.209,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,6.574,22.828> <22.828,8.035,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,8.401,22.828> <22.828,9.862,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,10.227,22.828> <22.828,11.688,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,12.053,22.828> <22.828,13.514,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,13.879,22.828> <22.828,15.340,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,15.706,22.828> <22.828,17.167,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,17.532,22.828> <22.828,18.993,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,19.358,22.828> <22.828,20.819,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <22.828,21.184,22.828> <22.828,22.645,22.828>, .1 pigment { color <.9,.9,.9> } }
