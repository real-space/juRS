#version 3.6;
#include "colors.inc"
#include "textures.inc"
#include "shapes.inc"
#include "glass.inc"
#include "metals.inc"

background {color White}

plane { <0,1,0>, -28.635 pigment{color <.6,.6,.6>} finish{ reflection {.2} ambient .5 diffuse .5 metallic }}

camera { location <171.21,57.07,114.14> look_at <-2.61,-2.77,-3.02> right x*image_width/image_height/2 up y/2 }

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
sphere{ <22.83,.00,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
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
sphere{ <-28.53,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,.00,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,.00,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,.00,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
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
sphere{ <17.12,5.71,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,.00,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
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
sphere{ <22.83,11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
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
sphere{ <22.83,11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.12,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,17.12,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,17.12,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,22.83,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,22.83,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-28.53,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-28.53,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,22.83,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,22.83,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-28.53,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-28.53,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-17.12,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-17.12,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-22.83,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-22.83,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
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
sphere{ <22.83,-11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-11.41,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-11.41,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-11.41,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-11.41,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-11.41,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }

// add the cell borders
cylinder { <-28.351,-28.535,-28.535> <-26.878,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-26.510,-28.535,-28.535> <-25.037,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-24.669,-28.535,-28.535> <-23.196,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-28.535,-28.535> <-21.355,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-20.987,-28.535,-28.535> <-19.514,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-19.146,-28.535,-28.535> <-17.673,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.305,-28.535,-28.535> <-15.832,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.464,-28.535,-28.535> <-13.991,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.623,-28.535,-28.535> <-12.150,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.782,-28.535,-28.535> <-10.309,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.941,-28.535,-28.535> <-8.468,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-8.100,-28.535,-28.535> <-6.627,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.259,-28.535,-28.535> <-4.786,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.418,-28.535,-28.535> <-2.946,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.577,-28.535,-28.535> <-1.105,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.736,-28.535,-28.535> <.736,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.105,-28.535,-28.535> <2.577,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.946,-28.535,-28.535> <4.418,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.786,-28.535,-28.535> <6.259,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.627,-28.535,-28.535> <8.100,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.468,-28.535,-28.535> <9.941,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.309,-28.535,-28.535> <11.782,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <12.150,-28.535,-28.535> <13.623,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <13.991,-28.535,-28.535> <15.464,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <15.832,-28.535,-28.535> <17.305,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.673,-28.535,-28.535> <19.146,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <19.514,-28.535,-28.535> <20.987,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <21.355,-28.535,-28.535> <22.828,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <23.196,-28.535,-28.535> <24.669,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <25.037,-28.535,-28.535> <26.510,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <26.878,-28.535,-28.535> <28.351,-28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.351,-28.535,28.535> <-26.878,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-26.510,-28.535,28.535> <-25.037,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-24.669,-28.535,28.535> <-23.196,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,-28.535,28.535> <-21.355,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-20.987,-28.535,28.535> <-19.514,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-19.146,-28.535,28.535> <-17.673,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.305,-28.535,28.535> <-15.832,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.464,-28.535,28.535> <-13.991,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.623,-28.535,28.535> <-12.150,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.782,-28.535,28.535> <-10.309,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.941,-28.535,28.535> <-8.468,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-8.100,-28.535,28.535> <-6.627,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.259,-28.535,28.535> <-4.786,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.418,-28.535,28.535> <-2.946,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.577,-28.535,28.535> <-1.105,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.736,-28.535,28.535> <.736,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.105,-28.535,28.535> <2.577,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.946,-28.535,28.535> <4.418,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.786,-28.535,28.535> <6.259,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.627,-28.535,28.535> <8.100,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.468,-28.535,28.535> <9.941,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.309,-28.535,28.535> <11.782,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <12.150,-28.535,28.535> <13.623,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <13.991,-28.535,28.535> <15.464,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <15.832,-28.535,28.535> <17.305,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.673,-28.535,28.535> <19.146,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <19.514,-28.535,28.535> <20.987,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <21.355,-28.535,28.535> <22.828,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <23.196,-28.535,28.535> <24.669,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <25.037,-28.535,28.535> <26.510,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <26.878,-28.535,28.535> <28.351,-28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.351,28.535,-28.535> <-26.878,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-26.510,28.535,-28.535> <-25.037,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-24.669,28.535,-28.535> <-23.196,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,28.535,-28.535> <-21.355,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-20.987,28.535,-28.535> <-19.514,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-19.146,28.535,-28.535> <-17.673,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.305,28.535,-28.535> <-15.832,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.464,28.535,-28.535> <-13.991,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.623,28.535,-28.535> <-12.150,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.782,28.535,-28.535> <-10.309,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.941,28.535,-28.535> <-8.468,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-8.100,28.535,-28.535> <-6.627,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.259,28.535,-28.535> <-4.786,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.418,28.535,-28.535> <-2.946,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.577,28.535,-28.535> <-1.105,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.736,28.535,-28.535> <.736,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.105,28.535,-28.535> <2.577,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.946,28.535,-28.535> <4.418,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.786,28.535,-28.535> <6.259,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.627,28.535,-28.535> <8.100,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.468,28.535,-28.535> <9.941,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.309,28.535,-28.535> <11.782,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <12.150,28.535,-28.535> <13.623,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <13.991,28.535,-28.535> <15.464,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <15.832,28.535,-28.535> <17.305,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.673,28.535,-28.535> <19.146,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <19.514,28.535,-28.535> <20.987,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <21.355,28.535,-28.535> <22.828,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <23.196,28.535,-28.535> <24.669,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <25.037,28.535,-28.535> <26.510,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <26.878,28.535,-28.535> <28.351,28.535,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.351,28.535,28.535> <-26.878,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-26.510,28.535,28.535> <-25.037,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-24.669,28.535,28.535> <-23.196,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-22.828,28.535,28.535> <-21.355,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-20.987,28.535,28.535> <-19.514,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-19.146,28.535,28.535> <-17.673,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.305,28.535,28.535> <-15.832,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.464,28.535,28.535> <-13.991,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.623,28.535,28.535> <-12.150,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.782,28.535,28.535> <-10.309,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-9.941,28.535,28.535> <-8.468,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-8.100,28.535,28.535> <-6.627,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.259,28.535,28.535> <-4.786,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.418,28.535,28.535> <-2.946,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.577,28.535,28.535> <-1.105,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.736,28.535,28.535> <.736,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.105,28.535,28.535> <2.577,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.946,28.535,28.535> <4.418,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.786,28.535,28.535> <6.259,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.627,28.535,28.535> <8.100,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.468,28.535,28.535> <9.941,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.309,28.535,28.535> <11.782,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <12.150,28.535,28.535> <13.623,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <13.991,28.535,28.535> <15.464,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <15.832,28.535,28.535> <17.305,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.673,28.535,28.535> <19.146,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <19.514,28.535,28.535> <20.987,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <21.355,28.535,28.535> <22.828,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <23.196,28.535,28.535> <24.669,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <25.037,28.535,28.535> <26.510,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <26.878,28.535,28.535> <28.351,28.535,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-28.351> <-28.535,-28.535,-26.878>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-26.510> <-28.535,-28.535,-25.037>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-24.669> <-28.535,-28.535,-23.196>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-22.828> <-28.535,-28.535,-21.355>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-20.987> <-28.535,-28.535,-19.514>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-19.146> <-28.535,-28.535,-17.673>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-17.305> <-28.535,-28.535,-15.832>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-15.464> <-28.535,-28.535,-13.991>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-13.623> <-28.535,-28.535,-12.150>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-11.782> <-28.535,-28.535,-10.309>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-9.941> <-28.535,-28.535,-8.468>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-8.100> <-28.535,-28.535,-6.627>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-6.259> <-28.535,-28.535,-4.786>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-4.418> <-28.535,-28.535,-2.946>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-2.577> <-28.535,-28.535,-1.105>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,-.736> <-28.535,-28.535,.736>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,1.105> <-28.535,-28.535,2.577>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,2.946> <-28.535,-28.535,4.418>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,4.786> <-28.535,-28.535,6.259>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,6.627> <-28.535,-28.535,8.100>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,8.468> <-28.535,-28.535,9.941>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,10.309> <-28.535,-28.535,11.782>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,12.150> <-28.535,-28.535,13.623>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,13.991> <-28.535,-28.535,15.464>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,15.832> <-28.535,-28.535,17.305>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,17.673> <-28.535,-28.535,19.146>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,19.514> <-28.535,-28.535,20.987>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,21.355> <-28.535,-28.535,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,23.196> <-28.535,-28.535,24.669>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,25.037> <-28.535,-28.535,26.510>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.535,26.878> <-28.535,-28.535,28.351>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-28.351> <-28.535,28.535,-26.878>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-26.510> <-28.535,28.535,-25.037>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-24.669> <-28.535,28.535,-23.196>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-22.828> <-28.535,28.535,-21.355>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-20.987> <-28.535,28.535,-19.514>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-19.146> <-28.535,28.535,-17.673>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-17.305> <-28.535,28.535,-15.832>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-15.464> <-28.535,28.535,-13.991>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-13.623> <-28.535,28.535,-12.150>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-11.782> <-28.535,28.535,-10.309>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-9.941> <-28.535,28.535,-8.468>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-8.100> <-28.535,28.535,-6.627>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-6.259> <-28.535,28.535,-4.786>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-4.418> <-28.535,28.535,-2.946>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-2.577> <-28.535,28.535,-1.105>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,-.736> <-28.535,28.535,.736>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,1.105> <-28.535,28.535,2.577>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,2.946> <-28.535,28.535,4.418>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,4.786> <-28.535,28.535,6.259>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,6.627> <-28.535,28.535,8.100>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,8.468> <-28.535,28.535,9.941>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,10.309> <-28.535,28.535,11.782>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,12.150> <-28.535,28.535,13.623>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,13.991> <-28.535,28.535,15.464>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,15.832> <-28.535,28.535,17.305>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,17.673> <-28.535,28.535,19.146>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,19.514> <-28.535,28.535,20.987>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,21.355> <-28.535,28.535,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,23.196> <-28.535,28.535,24.669>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,25.037> <-28.535,28.535,26.510>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,28.535,26.878> <-28.535,28.535,28.351>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-28.351> <28.535,-28.535,-26.878>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-26.510> <28.535,-28.535,-25.037>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-24.669> <28.535,-28.535,-23.196>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-22.828> <28.535,-28.535,-21.355>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-20.987> <28.535,-28.535,-19.514>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-19.146> <28.535,-28.535,-17.673>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-17.305> <28.535,-28.535,-15.832>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-15.464> <28.535,-28.535,-13.991>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-13.623> <28.535,-28.535,-12.150>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-11.782> <28.535,-28.535,-10.309>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-9.941> <28.535,-28.535,-8.468>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-8.100> <28.535,-28.535,-6.627>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-6.259> <28.535,-28.535,-4.786>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-4.418> <28.535,-28.535,-2.946>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-2.577> <28.535,-28.535,-1.105>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,-.736> <28.535,-28.535,.736>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,1.105> <28.535,-28.535,2.577>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,2.946> <28.535,-28.535,4.418>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,4.786> <28.535,-28.535,6.259>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,6.627> <28.535,-28.535,8.100>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,8.468> <28.535,-28.535,9.941>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,10.309> <28.535,-28.535,11.782>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,12.150> <28.535,-28.535,13.623>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,13.991> <28.535,-28.535,15.464>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,15.832> <28.535,-28.535,17.305>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,17.673> <28.535,-28.535,19.146>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,19.514> <28.535,-28.535,20.987>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,21.355> <28.535,-28.535,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,23.196> <28.535,-28.535,24.669>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,25.037> <28.535,-28.535,26.510>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.535,26.878> <28.535,-28.535,28.351>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-28.351> <28.535,28.535,-26.878>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-26.510> <28.535,28.535,-25.037>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-24.669> <28.535,28.535,-23.196>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-22.828> <28.535,28.535,-21.355>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-20.987> <28.535,28.535,-19.514>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-19.146> <28.535,28.535,-17.673>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-17.305> <28.535,28.535,-15.832>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-15.464> <28.535,28.535,-13.991>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-13.623> <28.535,28.535,-12.150>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-11.782> <28.535,28.535,-10.309>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-9.941> <28.535,28.535,-8.468>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-8.100> <28.535,28.535,-6.627>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-6.259> <28.535,28.535,-4.786>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-4.418> <28.535,28.535,-2.946>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-2.577> <28.535,28.535,-1.105>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,-.736> <28.535,28.535,.736>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,1.105> <28.535,28.535,2.577>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,2.946> <28.535,28.535,4.418>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,4.786> <28.535,28.535,6.259>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,6.627> <28.535,28.535,8.100>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,8.468> <28.535,28.535,9.941>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,10.309> <28.535,28.535,11.782>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,12.150> <28.535,28.535,13.623>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,13.991> <28.535,28.535,15.464>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,15.832> <28.535,28.535,17.305>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,17.673> <28.535,28.535,19.146>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,19.514> <28.535,28.535,20.987>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,21.355> <28.535,28.535,22.828>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,23.196> <28.535,28.535,24.669>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,25.037> <28.535,28.535,26.510>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,28.535,26.878> <28.535,28.535,28.351>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.351,-28.535> <-28.535,-26.878,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-26.510,-28.535> <-28.535,-25.037,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-24.669,-28.535> <-28.535,-23.196,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-22.828,-28.535> <-28.535,-21.355,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-20.987,-28.535> <-28.535,-19.514,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-19.146,-28.535> <-28.535,-17.673,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-17.305,-28.535> <-28.535,-15.832,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-15.464,-28.535> <-28.535,-13.991,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-13.623,-28.535> <-28.535,-12.150,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-11.782,-28.535> <-28.535,-10.309,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-9.941,-28.535> <-28.535,-8.468,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-8.100,-28.535> <-28.535,-6.627,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-6.259,-28.535> <-28.535,-4.786,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-4.418,-28.535> <-28.535,-2.946,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-2.577,-28.535> <-28.535,-1.105,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-.736,-28.535> <-28.535,.736,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,1.105,-28.535> <-28.535,2.577,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,2.946,-28.535> <-28.535,4.418,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,4.786,-28.535> <-28.535,6.259,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,6.627,-28.535> <-28.535,8.100,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,8.468,-28.535> <-28.535,9.941,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,10.309,-28.535> <-28.535,11.782,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,12.150,-28.535> <-28.535,13.623,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,13.991,-28.535> <-28.535,15.464,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,15.832,-28.535> <-28.535,17.305,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,17.673,-28.535> <-28.535,19.146,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,19.514,-28.535> <-28.535,20.987,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,21.355,-28.535> <-28.535,22.828,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,23.196,-28.535> <-28.535,24.669,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,25.037,-28.535> <-28.535,26.510,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,26.878,-28.535> <-28.535,28.351,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.351,-28.535> <28.535,-26.878,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-26.510,-28.535> <28.535,-25.037,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-24.669,-28.535> <28.535,-23.196,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-22.828,-28.535> <28.535,-21.355,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-20.987,-28.535> <28.535,-19.514,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-19.146,-28.535> <28.535,-17.673,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-17.305,-28.535> <28.535,-15.832,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-15.464,-28.535> <28.535,-13.991,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-13.623,-28.535> <28.535,-12.150,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-11.782,-28.535> <28.535,-10.309,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-9.941,-28.535> <28.535,-8.468,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-8.100,-28.535> <28.535,-6.627,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-6.259,-28.535> <28.535,-4.786,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-4.418,-28.535> <28.535,-2.946,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-2.577,-28.535> <28.535,-1.105,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-.736,-28.535> <28.535,.736,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,1.105,-28.535> <28.535,2.577,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,2.946,-28.535> <28.535,4.418,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,4.786,-28.535> <28.535,6.259,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,6.627,-28.535> <28.535,8.100,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,8.468,-28.535> <28.535,9.941,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,10.309,-28.535> <28.535,11.782,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,12.150,-28.535> <28.535,13.623,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,13.991,-28.535> <28.535,15.464,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,15.832,-28.535> <28.535,17.305,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,17.673,-28.535> <28.535,19.146,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,19.514,-28.535> <28.535,20.987,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,21.355,-28.535> <28.535,22.828,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,23.196,-28.535> <28.535,24.669,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,25.037,-28.535> <28.535,26.510,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,26.878,-28.535> <28.535,28.351,-28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-28.351,28.535> <-28.535,-26.878,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-26.510,28.535> <-28.535,-25.037,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-24.669,28.535> <-28.535,-23.196,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-22.828,28.535> <-28.535,-21.355,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-20.987,28.535> <-28.535,-19.514,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-19.146,28.535> <-28.535,-17.673,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-17.305,28.535> <-28.535,-15.832,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-15.464,28.535> <-28.535,-13.991,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-13.623,28.535> <-28.535,-12.150,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-11.782,28.535> <-28.535,-10.309,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-9.941,28.535> <-28.535,-8.468,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-8.100,28.535> <-28.535,-6.627,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-6.259,28.535> <-28.535,-4.786,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-4.418,28.535> <-28.535,-2.946,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-2.577,28.535> <-28.535,-1.105,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,-.736,28.535> <-28.535,.736,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,1.105,28.535> <-28.535,2.577,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,2.946,28.535> <-28.535,4.418,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,4.786,28.535> <-28.535,6.259,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,6.627,28.535> <-28.535,8.100,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,8.468,28.535> <-28.535,9.941,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,10.309,28.535> <-28.535,11.782,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,12.150,28.535> <-28.535,13.623,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,13.991,28.535> <-28.535,15.464,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,15.832,28.535> <-28.535,17.305,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,17.673,28.535> <-28.535,19.146,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,19.514,28.535> <-28.535,20.987,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,21.355,28.535> <-28.535,22.828,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,23.196,28.535> <-28.535,24.669,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,25.037,28.535> <-28.535,26.510,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.535,26.878,28.535> <-28.535,28.351,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-28.351,28.535> <28.535,-26.878,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-26.510,28.535> <28.535,-25.037,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-24.669,28.535> <28.535,-23.196,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-22.828,28.535> <28.535,-21.355,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-20.987,28.535> <28.535,-19.514,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-19.146,28.535> <28.535,-17.673,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-17.305,28.535> <28.535,-15.832,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-15.464,28.535> <28.535,-13.991,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-13.623,28.535> <28.535,-12.150,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-11.782,28.535> <28.535,-10.309,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-9.941,28.535> <28.535,-8.468,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-8.100,28.535> <28.535,-6.627,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-6.259,28.535> <28.535,-4.786,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-4.418,28.535> <28.535,-2.946,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-2.577,28.535> <28.535,-1.105,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,-.736,28.535> <28.535,.736,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,1.105,28.535> <28.535,2.577,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,2.946,28.535> <28.535,4.418,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,4.786,28.535> <28.535,6.259,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,6.627,28.535> <28.535,8.100,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,8.468,28.535> <28.535,9.941,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,10.309,28.535> <28.535,11.782,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,12.150,28.535> <28.535,13.623,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,13.991,28.535> <28.535,15.464,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,15.832,28.535> <28.535,17.305,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,17.673,28.535> <28.535,19.146,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,19.514,28.535> <28.535,20.987,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,21.355,28.535> <28.535,22.828,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,23.196,28.535> <28.535,24.669,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,25.037,28.535> <28.535,26.510,28.535>, .1 pigment { color <.9,.9,.9> } }
cylinder { <28.535,26.878,28.535> <28.535,28.351,28.535>, .1 pigment { color <.9,.9,.9> } }
