#version 3.6;
#include "colors.inc"
#include "textures.inc"
#include "shapes.inc"
#include "glass.inc"
#include "metals.inc"

background {color White}

plane { <0,1,0>, -17.221 pigment{color <.6,.6,.6>} finish{ reflection {.2} ambient .5 diffuse .5 metallic }}

camera { location <102.73,34.24,68.48> look_at <-2.34,-2.64,-2.97> right x*image_width/image_height/2 up y/2 }

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
sphere{ <-17.12,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }

// add the cell borders
cylinder { <-16.941,-17.121,-17.121> <-15.499,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.138,-17.121,-17.121> <-13.697,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.336,-17.121,-17.121> <-11.895,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.534,-17.121,-17.121> <-10.092,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.732,-17.121,-17.121> <-8.290,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-7.930,-17.121,-17.121> <-6.488,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.127,-17.121,-17.121> <-4.686,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.325,-17.121,-17.121> <-2.884,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.523,-17.121,-17.121> <-1.081,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.721,-17.121,-17.121> <.721,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.081,-17.121,-17.121> <2.523,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.884,-17.121,-17.121> <4.325,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.686,-17.121,-17.121> <6.127,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.488,-17.121,-17.121> <7.930,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.290,-17.121,-17.121> <9.732,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.092,-17.121,-17.121> <11.534,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.895,-17.121,-17.121> <13.336,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <13.697,-17.121,-17.121> <15.138,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <15.499,-17.121,-17.121> <16.941,-17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-16.941,-17.121,17.121> <-15.499,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.138,-17.121,17.121> <-13.697,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.336,-17.121,17.121> <-11.895,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.534,-17.121,17.121> <-10.092,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.732,-17.121,17.121> <-8.290,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-7.930,-17.121,17.121> <-6.488,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.127,-17.121,17.121> <-4.686,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.325,-17.121,17.121> <-2.884,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.523,-17.121,17.121> <-1.081,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.721,-17.121,17.121> <.721,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.081,-17.121,17.121> <2.523,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.884,-17.121,17.121> <4.325,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.686,-17.121,17.121> <6.127,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.488,-17.121,17.121> <7.930,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.290,-17.121,17.121> <9.732,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.092,-17.121,17.121> <11.534,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.895,-17.121,17.121> <13.336,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <13.697,-17.121,17.121> <15.138,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <15.499,-17.121,17.121> <16.941,-17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-16.941,17.121,-17.121> <-15.499,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.138,17.121,-17.121> <-13.697,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.336,17.121,-17.121> <-11.895,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.534,17.121,-17.121> <-10.092,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.732,17.121,-17.121> <-8.290,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-7.930,17.121,-17.121> <-6.488,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.127,17.121,-17.121> <-4.686,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.325,17.121,-17.121> <-2.884,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.523,17.121,-17.121> <-1.081,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.721,17.121,-17.121> <.721,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.081,17.121,-17.121> <2.523,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.884,17.121,-17.121> <4.325,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.686,17.121,-17.121> <6.127,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.488,17.121,-17.121> <7.930,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.290,17.121,-17.121> <9.732,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.092,17.121,-17.121> <11.534,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.895,17.121,-17.121> <13.336,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <13.697,17.121,-17.121> <15.138,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <15.499,17.121,-17.121> <16.941,17.121,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-16.941,17.121,17.121> <-15.499,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.138,17.121,17.121> <-13.697,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.336,17.121,17.121> <-11.895,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.534,17.121,17.121> <-10.092,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.732,17.121,17.121> <-8.290,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-7.930,17.121,17.121> <-6.488,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.127,17.121,17.121> <-4.686,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.325,17.121,17.121> <-2.884,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.523,17.121,17.121> <-1.081,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.721,17.121,17.121> <.721,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.081,17.121,17.121> <2.523,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.884,17.121,17.121> <4.325,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.686,17.121,17.121> <6.127,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.488,17.121,17.121> <7.930,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.290,17.121,17.121> <9.732,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.092,17.121,17.121> <11.534,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <11.895,17.121,17.121> <13.336,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <13.697,17.121,17.121> <15.138,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <15.499,17.121,17.121> <16.941,17.121,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,-16.941> <-17.121,-17.121,-15.499>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,-15.138> <-17.121,-17.121,-13.697>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,-13.336> <-17.121,-17.121,-11.895>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,-11.534> <-17.121,-17.121,-10.092>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,-9.732> <-17.121,-17.121,-8.290>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,-7.930> <-17.121,-17.121,-6.488>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,-6.127> <-17.121,-17.121,-4.686>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,-4.325> <-17.121,-17.121,-2.884>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,-2.523> <-17.121,-17.121,-1.081>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,-.721> <-17.121,-17.121,.721>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,1.081> <-17.121,-17.121,2.523>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,2.884> <-17.121,-17.121,4.325>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,4.686> <-17.121,-17.121,6.127>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,6.488> <-17.121,-17.121,7.930>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,8.290> <-17.121,-17.121,9.732>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,10.092> <-17.121,-17.121,11.534>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,11.895> <-17.121,-17.121,13.336>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,13.697> <-17.121,-17.121,15.138>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-17.121,15.499> <-17.121,-17.121,16.941>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,-16.941> <-17.121,17.121,-15.499>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,-15.138> <-17.121,17.121,-13.697>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,-13.336> <-17.121,17.121,-11.895>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,-11.534> <-17.121,17.121,-10.092>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,-9.732> <-17.121,17.121,-8.290>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,-7.930> <-17.121,17.121,-6.488>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,-6.127> <-17.121,17.121,-4.686>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,-4.325> <-17.121,17.121,-2.884>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,-2.523> <-17.121,17.121,-1.081>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,-.721> <-17.121,17.121,.721>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,1.081> <-17.121,17.121,2.523>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,2.884> <-17.121,17.121,4.325>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,4.686> <-17.121,17.121,6.127>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,6.488> <-17.121,17.121,7.930>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,8.290> <-17.121,17.121,9.732>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,10.092> <-17.121,17.121,11.534>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,11.895> <-17.121,17.121,13.336>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,13.697> <-17.121,17.121,15.138>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,17.121,15.499> <-17.121,17.121,16.941>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,-16.941> <17.121,-17.121,-15.499>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,-15.138> <17.121,-17.121,-13.697>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,-13.336> <17.121,-17.121,-11.895>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,-11.534> <17.121,-17.121,-10.092>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,-9.732> <17.121,-17.121,-8.290>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,-7.930> <17.121,-17.121,-6.488>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,-6.127> <17.121,-17.121,-4.686>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,-4.325> <17.121,-17.121,-2.884>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,-2.523> <17.121,-17.121,-1.081>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,-.721> <17.121,-17.121,.721>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,1.081> <17.121,-17.121,2.523>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,2.884> <17.121,-17.121,4.325>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,4.686> <17.121,-17.121,6.127>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,6.488> <17.121,-17.121,7.930>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,8.290> <17.121,-17.121,9.732>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,10.092> <17.121,-17.121,11.534>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,11.895> <17.121,-17.121,13.336>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,13.697> <17.121,-17.121,15.138>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-17.121,15.499> <17.121,-17.121,16.941>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,-16.941> <17.121,17.121,-15.499>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,-15.138> <17.121,17.121,-13.697>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,-13.336> <17.121,17.121,-11.895>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,-11.534> <17.121,17.121,-10.092>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,-9.732> <17.121,17.121,-8.290>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,-7.930> <17.121,17.121,-6.488>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,-6.127> <17.121,17.121,-4.686>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,-4.325> <17.121,17.121,-2.884>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,-2.523> <17.121,17.121,-1.081>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,-.721> <17.121,17.121,.721>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,1.081> <17.121,17.121,2.523>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,2.884> <17.121,17.121,4.325>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,4.686> <17.121,17.121,6.127>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,6.488> <17.121,17.121,7.930>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,8.290> <17.121,17.121,9.732>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,10.092> <17.121,17.121,11.534>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,11.895> <17.121,17.121,13.336>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,13.697> <17.121,17.121,15.138>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,17.121,15.499> <17.121,17.121,16.941>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-16.941,-17.121> <-17.121,-15.499,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-15.138,-17.121> <-17.121,-13.697,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-13.336,-17.121> <-17.121,-11.895,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-11.534,-17.121> <-17.121,-10.092,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-9.732,-17.121> <-17.121,-8.290,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-7.930,-17.121> <-17.121,-6.488,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-6.127,-17.121> <-17.121,-4.686,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-4.325,-17.121> <-17.121,-2.884,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-2.523,-17.121> <-17.121,-1.081,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-.721,-17.121> <-17.121,.721,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,1.081,-17.121> <-17.121,2.523,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,2.884,-17.121> <-17.121,4.325,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,4.686,-17.121> <-17.121,6.127,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,6.488,-17.121> <-17.121,7.930,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,8.290,-17.121> <-17.121,9.732,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,10.092,-17.121> <-17.121,11.534,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,11.895,-17.121> <-17.121,13.336,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,13.697,-17.121> <-17.121,15.138,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,15.499,-17.121> <-17.121,16.941,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-16.941,-17.121> <17.121,-15.499,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-15.138,-17.121> <17.121,-13.697,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-13.336,-17.121> <17.121,-11.895,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-11.534,-17.121> <17.121,-10.092,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-9.732,-17.121> <17.121,-8.290,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-7.930,-17.121> <17.121,-6.488,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-6.127,-17.121> <17.121,-4.686,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-4.325,-17.121> <17.121,-2.884,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-2.523,-17.121> <17.121,-1.081,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-.721,-17.121> <17.121,.721,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,1.081,-17.121> <17.121,2.523,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,2.884,-17.121> <17.121,4.325,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,4.686,-17.121> <17.121,6.127,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,6.488,-17.121> <17.121,7.930,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,8.290,-17.121> <17.121,9.732,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,10.092,-17.121> <17.121,11.534,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,11.895,-17.121> <17.121,13.336,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,13.697,-17.121> <17.121,15.138,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,15.499,-17.121> <17.121,16.941,-17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-16.941,17.121> <-17.121,-15.499,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-15.138,17.121> <-17.121,-13.697,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-13.336,17.121> <-17.121,-11.895,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-11.534,17.121> <-17.121,-10.092,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-9.732,17.121> <-17.121,-8.290,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-7.930,17.121> <-17.121,-6.488,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-6.127,17.121> <-17.121,-4.686,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-4.325,17.121> <-17.121,-2.884,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-2.523,17.121> <-17.121,-1.081,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,-.721,17.121> <-17.121,.721,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,1.081,17.121> <-17.121,2.523,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,2.884,17.121> <-17.121,4.325,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,4.686,17.121> <-17.121,6.127,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,6.488,17.121> <-17.121,7.930,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,8.290,17.121> <-17.121,9.732,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,10.092,17.121> <-17.121,11.534,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,11.895,17.121> <-17.121,13.336,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,13.697,17.121> <-17.121,15.138,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.121,15.499,17.121> <-17.121,16.941,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-16.941,17.121> <17.121,-15.499,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-15.138,17.121> <17.121,-13.697,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-13.336,17.121> <17.121,-11.895,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-11.534,17.121> <17.121,-10.092,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-9.732,17.121> <17.121,-8.290,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-7.930,17.121> <17.121,-6.488,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-6.127,17.121> <17.121,-4.686,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-4.325,17.121> <17.121,-2.884,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-2.523,17.121> <17.121,-1.081,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,-.721,17.121> <17.121,.721,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,1.081,17.121> <17.121,2.523,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,2.884,17.121> <17.121,4.325,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,4.686,17.121> <17.121,6.127,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,6.488,17.121> <17.121,7.930,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,8.290,17.121> <17.121,9.732,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,10.092,17.121> <17.121,11.534,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,11.895,17.121> <17.121,13.336,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,13.697,17.121> <17.121,15.138,17.121>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.121,15.499,17.121> <17.121,16.941,17.121>, .1 pigment { color <.9,.9,.9> } }
