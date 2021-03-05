#version 3.6;
#include "colors.inc"
#include "textures.inc"
#include "shapes.inc"
#include "glass.inc"
#include "metals.inc"

background {color White}

plane { <0,1,0>, -45.756 pigment{color <.6,.6,.6>} finish{ reflection {.2} ambient .5 diffuse .5 metallic }}

camera { location <273.93,91.31,182.62> look_at <-2.78,-2.97,-2.93> right x*image_width/image_height/2 up y/2 }

// add lights
light_source { <0,200,400> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }
light_source { <400,200,0> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }


// add the atoms
sphere{ <5.71,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,.00,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,.00,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,.00,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,.00,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,5.71,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,.00,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,5.71,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,.00,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,.00,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,5.71,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,5.71,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,.00,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,.00,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,5.71,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,.00,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,.00,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,5.71,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,.00,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,5.71,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,.00,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,.00,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,.00,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,.00,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,.00,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,5.71,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,.00,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,.00,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,.00,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,5.71,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,.00,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,.00,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,.00,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,5.71,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,5.71,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,.00,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,.00,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,.00,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,5.71,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,.00,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,.00,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,.00,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,.00,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,.00,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,.00,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,.00,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,.00,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,.00,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,.00,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,.00,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,.00,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,.00,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,.00,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,.00,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,5.71,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,.00,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,.00,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,.00,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,.00,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,.00,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,.00,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,.00,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,.00,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,5.71,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,.00,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,.00,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,.00,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,5.71,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,5.71,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,.00,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,.00,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,.00,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,.00,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,5.71,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,.00,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,.00,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,.00,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,5.71,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,5.71,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,.00,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,.00,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,.00,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,.00,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,.00,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,.00,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,.00,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,.00,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,5.71,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,.00,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,.00,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,.00,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,5.71,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,5.71,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,.00,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,.00,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,.00,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,5.71,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,.00,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,.00,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,.00,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,5.71,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,5.71,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,.00,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,.00,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,.00,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,.00,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,5.71,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,.00,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,.00,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,.00,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,.00,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,.00,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,.00,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.71,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,.00,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,.00,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,.00,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,.00,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,.00,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,.00,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,.00,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,.00,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,.00,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,.00,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,17.12,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,17.12,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,17.12,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,17.12,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,11.41,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.12,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,17.12,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,11.41,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,11.41,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,17.12,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,11.41,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,17.12,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,11.41,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,11.41,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,17.12,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,17.12,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.12,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,17.12,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,11.41,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,11.41,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,17.12,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,11.41,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,11.41,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,17.12,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,11.41,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,11.41,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,17.12,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,17.12,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,11.41,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,17.12,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.12,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,17.12,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,11.41,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,11.41,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,17.12,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,17.12,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,11.41,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,11.41,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,17.12,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,11.41,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,11.41,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,17.12,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,17.12,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,11.41,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.12,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,17.12,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,11.41,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,17.12,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,17.12,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,11.41,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,17.12,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,11.41,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,17.12,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,17.12,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,11.41,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,17.12,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,11.41,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,11.41,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,17.12,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,17.12,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,17.12,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,11.41,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,11.41,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,17.12,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,17.12,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,17.12,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
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
sphere{ <5.71,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,28.53,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,28.53,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,28.53,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,28.53,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,28.53,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,22.83,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,28.53,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,28.53,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,22.83,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,28.53,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,28.53,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,28.53,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,28.53,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,22.83,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,28.53,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,28.53,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,28.53,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,28.53,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,28.53,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,28.53,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,28.53,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,28.53,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,22.83,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,28.53,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,28.53,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,28.53,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,22.83,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,22.83,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,28.53,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,28.53,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,28.53,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,28.53,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,22.83,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,28.53,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,28.53,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,28.53,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,22.83,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,28.53,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,28.53,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,28.53,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,28.53,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,28.53,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,22.83,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,28.53,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,28.53,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,22.83,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,22.83,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,28.53,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,28.53,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.83,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,28.53,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,28.53,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,28.53,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,28.53,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,22.83,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,28.53,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,22.83,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,28.53,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,28.53,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,22.83,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,28.53,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,28.53,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,22.83,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,28.53,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,28.53,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,22.83,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,28.53,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,28.53,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.83,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,28.53,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,22.83,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,28.53,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,22.83,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,28.53,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,28.53,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,28.53,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,28.53,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,22.83,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,22.83,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,28.53,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,28.53,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,22.83,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,28.53,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,28.53,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,22.83,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,28.53,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,28.53,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,28.53,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,28.53,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,28.53,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,28.53,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,28.53,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,28.53,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,28.53,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,28.53,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,22.83,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,28.53,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,28.53,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,28.53,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,28.53,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.83,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,28.53,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,28.53,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,28.53,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,22.83,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.83,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,22.83,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,22.83,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,22.83,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,28.53,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,28.53,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,28.53,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.83,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,28.53,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,22.83,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,22.83,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,34.24,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,39.95,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,39.95,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,34.24,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,39.95,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,39.95,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,39.95,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,39.95,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,39.95,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,34.24,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,39.95,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,34.24,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.95,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,34.24,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,34.24,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,34.24,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,39.95,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,39.95,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,34.24,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,39.95,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,39.95,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,34.24,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,39.95,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,39.95,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,34.24,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,34.24,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,39.95,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,34.24,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,39.95,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,39.95,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,34.24,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,34.24,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.95,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,34.24,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,39.95,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,39.95,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,34.24,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,34.24,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,39.95,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,39.95,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,39.95,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,39.95,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,34.24,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,39.95,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,39.95,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,34.24,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,39.95,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,39.95,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,34.24,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,34.24,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,39.95,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,34.24,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,34.24,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,39.95,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,39.95,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.95,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,34.24,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,34.24,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,39.95,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,39.95,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,34.24,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,39.95,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,39.95,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,34.24,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,34.24,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,39.95,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,39.95,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,34.24,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,34.24,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,39.95,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,34.24,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,39.95,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,34.24,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,34.24,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,39.95,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,39.95,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,34.24,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,39.95,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,39.95,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,34.24,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,34.24,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,39.95,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.95,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,34.24,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,39.95,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,39.95,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,34.24,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,39.95,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,34.24,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,39.95,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,34.24,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,34.24,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,39.95,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,34.24,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,39.95,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,39.95,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,34.24,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,39.95,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,34.24,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,34.24,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,39.95,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,34.24,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,39.95,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.95,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,34.24,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,34.24,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,39.95,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,39.95,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,34.24,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,34.24,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,39.95,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,34.24,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,34.24,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,39.95,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,39.95,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,34.24,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,39.95,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,34.24,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,39.95,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,34.24,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,34.24,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,39.95,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,34.24,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,34.24,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,39.95,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,34.24,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,39.95,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.95,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,34.24,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,34.24,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,39.95,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,39.95,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,34.24,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,34.24,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,39.95,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,34.24,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,34.24,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,39.95,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,34.24,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,39.95,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,34.24,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,34.24,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,39.95,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,34.24,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,34.24,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,34.24,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,34.24,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,39.95,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,34.24,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,34.24,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,34.24,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,34.24,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,39.95,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,34.24,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,34.24,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,39.95,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,39.95,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,34.24,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,34.24,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,39.95,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,39.95,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,34.24,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,34.24,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,39.95,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,39.95,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,34.24,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,34.24,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,39.95,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,34.24,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,34.24,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,39.95,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,39.95,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,34.24,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,34.24,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.95,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,39.95,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,39.95,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-45.66,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-45.66,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-45.66,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-45.66,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-39.95,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-39.95,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-45.66,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-45.66,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-45.66,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-39.95,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-45.66,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-45.66,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-45.66,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-45.66,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-39.95,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-39.95,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-45.66,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-45.66,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-45.66,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-45.66,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-45.66,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-45.66,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-45.66,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-45.66,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-39.95,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,-39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-45.66,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-45.66,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-45.66,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-39.95,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-39.95,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-45.66,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-45.66,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-45.66,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-39.95,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-45.66,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-45.66,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-45.66,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-45.66,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-39.95,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-39.95,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-39.95,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-45.66,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-45.66,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-45.66,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-39.95,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-45.66,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-45.66,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-45.66,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-39.95,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-45.66,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-45.66,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-45.66,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-45.66,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-39.95,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-39.95,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-45.66,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-45.66,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-45.66,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-45.66,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-39.95,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-39.95,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-45.66,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-45.66,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-45.66,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-39.95,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-45.66,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-45.66,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-45.66,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-45.66,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-45.66,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-45.66,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-45.66,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-39.95,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-39.95,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-45.66,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-45.66,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-45.66,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-39.95,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-39.95,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-39.95,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-39.95,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-45.66,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-45.66,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-39.95,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-45.66,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-45.66,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-45.66,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-45.66,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-39.95,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-39.95,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-45.66,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-45.66,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-45.66,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-45.66,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-39.95,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-45.66,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-45.66,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-45.66,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-45.66,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-39.95,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-39.95,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-45.66,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-45.66,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-45.66,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-39.95,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-39.95,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-45.66,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-45.66,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-45.66,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-39.95,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-39.95,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-45.66,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-45.66,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-45.66,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-39.95,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-39.95,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-45.66,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-45.66,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-45.66,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-45.66,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-39.95,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-39.95,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-39.95,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-39.95,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-45.66,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-45.66,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-45.66,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-45.66,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-39.95,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-39.95,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-45.66,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-45.66,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-45.66,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-45.66,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-39.95,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-45.66,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-45.66,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-45.66,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-45.66,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-39.95,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-39.95,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-45.66,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-45.66,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-45.66,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-45.66,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-39.95,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-39.95,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-45.66,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-45.66,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-39.95,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-45.66,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-45.66,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-45.66,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-45.66,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-39.95,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-39.95,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-45.66,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-45.66,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-45.66,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-45.66,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-39.95,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-45.66,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-45.66,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-45.66,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-45.66,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-39.95,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-39.95,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-39.95,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-45.66,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-45.66,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-45.66,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-39.95,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-39.95,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-45.66,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-45.66,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-45.66,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-45.66,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-45.66,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-45.66,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-45.66,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-45.66,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-39.95,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-39.95,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-45.66,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-45.66,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-45.66,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-45.66,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-39.95,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-45.66,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-45.66,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-45.66,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-45.66,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-39.95,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-39.95,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,-39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-45.66,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-45.66,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-45.66,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-45.66,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-39.95,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-45.66,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-45.66,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-45.66,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-39.95,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-45.66,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-45.66,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-45.66,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-39.95,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-39.95,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-39.95,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-39.95,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-45.66,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-45.66,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-45.66,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-45.66,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-45.66,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-45.66,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-45.66,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-39.95,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-39.95,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-45.66,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-45.66,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-45.66,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-45.66,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-39.95,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-39.95,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-45.66,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-45.66,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-45.66,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-39.95,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-45.66,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-45.66,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-45.66,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-45.66,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-39.95,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-39.95,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-45.66,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-45.66,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-45.66,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-45.66,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-39.95,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-39.95,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-45.66,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-45.66,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-45.66,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-45.66,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-39.95,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-45.66,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-45.66,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-45.66,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-45.66,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-39.95,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-39.95,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-39.95,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-45.66,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-45.66,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-45.66,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-39.95,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-45.66,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-45.66,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-45.66,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-39.95,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-45.66,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-45.66,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-45.66,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-45.66,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-39.95,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-45.66,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-45.66,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-45.66,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-45.66,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-39.95,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-45.66,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-45.66,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-45.66,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-39.95,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-45.66,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-45.66,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-45.66,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-45.66,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-39.95,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-45.66,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-45.66,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-45.66,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-45.66,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-39.95,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-45.66,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-45.66,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-45.66,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-39.95,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-39.95,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-39.95,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-39.95,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-45.66,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-45.66,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-45.66,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-45.66,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-39.95,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-39.95,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-45.66,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-45.66,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-45.66,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-45.66,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-39.95,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-39.95,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-45.66,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-45.66,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-45.66,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-39.95,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-39.95,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-45.66,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-45.66,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-39.95,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-45.66,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-45.66,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,-45.66,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-39.95,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-45.66,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-45.66,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-45.66,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-39.95,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-39.95,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-45.66,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-45.66,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-45.66,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-45.66,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-39.95,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-39.95,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-45.66,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-45.66,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-45.66,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-45.66,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-39.95,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-39.95,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-39.95,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-34.24,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-28.53,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-28.53,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-34.24,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-28.53,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-28.53,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-34.24,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-28.53,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-28.53,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-34.24,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-34.24,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-34.24,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-34.24,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-34.24,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-34.24,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-28.53,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-28.53,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-34.24,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-34.24,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-28.53,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-34.24,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-34.24,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-28.53,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-34.24,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-34.24,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-34.24,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-28.53,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-34.24,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-34.24,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-28.53,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-34.24,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-34.24,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-34.24,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-34.24,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-34.24,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-34.24,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-34.24,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-34.24,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-28.53,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-34.24,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-34.24,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-28.53,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-34.24,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-34.24,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-28.53,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-34.24,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-34.24,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-28.53,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-34.24,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-28.53,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-28.53,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-34.24,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-28.53,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-28.53,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-34.24,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-34.24,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-34.24,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-34.24,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-34.24,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-34.24,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-28.53,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-34.24,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-34.24,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-28.53,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-34.24,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-28.53,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-34.24,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-34.24,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-34.24,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-34.24,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-28.53,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-28.53,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-34.24,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-28.53,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-34.24,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-34.24,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-34.24,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-34.24,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-34.24,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-28.53,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-34.24,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-34.24,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-34.24,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-34.24,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-28.53,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-34.24,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-34.24,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-28.53,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-28.53,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-34.24,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-28.53,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,-28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-34.24,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-28.53,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-34.24,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-34.24,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-34.24,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-34.24,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-34.24,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-34.24,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-28.53,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-34.24,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-34.24,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-28.53,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-34.24,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-28.53,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-34.24,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-28.53,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-34.24,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-28.53,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-28.53,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-34.24,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,-34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-34.24,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-34.24,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-28.53,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-34.24,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-34.24,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-28.53,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-34.24,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-34.24,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-34.24,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-34.24,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-28.53,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-34.24,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-34.24,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-34.24,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-34.24,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-28.53,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-34.24,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-34.24,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-28.53,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-28.53,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-34.24,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-34.24,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-28.53,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-28.53,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-28.53,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-28.53,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-34.24,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-34.24,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-34.24,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-34.24,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-34.24,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-28.53,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-34.24,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-34.24,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-34.24,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-34.24,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-28.53,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-34.24,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-28.53,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-34.24,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-34.24,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-28.53,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-28.53,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-34.24,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-34.24,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-28.53,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-34.24,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-34.24,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-28.53,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-34.24,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-34.24,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-34.24,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-34.24,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-34.24,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-34.24,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-28.53,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-28.53,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-28.53,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-28.53,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-22.83,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-22.83,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-22.83,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-22.83,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-22.83,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-17.12,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-22.83,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-22.83,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-17.12,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-22.83,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-22.83,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-17.12,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-22.83,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-17.12,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-17.12,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-22.83,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-17.12,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-17.12,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-22.83,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-22.83,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-17.12,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-22.83,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-22.83,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-22.83,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-22.83,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-17.12,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-22.83,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-17.12,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-17.12,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-22.83,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-22.83,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-17.12,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-22.83,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-17.12,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-17.12,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-22.83,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-22.83,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-17.12,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-17.12,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-22.83,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-17.12,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-17.12,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-22.83,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-22.83,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-17.12,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,-17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-17.12,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-22.83,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-17.12,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-22.83,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-17.12,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-17.12,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-17.12,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-22.83,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-17.12,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-17.12,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-17.12,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-22.83,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-17.12,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-17.12,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-22.83,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-22.83,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-17.12,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-17.12,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-11.41,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-11.41,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-11.41,11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-11.41,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-5.71,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-11.41,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-11.41,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-5.71,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-5.71,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-11.41,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-11.41,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,-11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-5.71,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-11.41,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-5.71,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-5.71,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-11.41,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-11.41,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-11.41,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-5.71,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-5.71,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-11.41,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-5.71,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-11.41,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-11.41,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-5.71,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-5.71,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-11.41,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-11.41,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-5.71,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-5.71,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-11.41,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-5.71,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-45.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-45.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-39.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-45.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-11.41,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-5.71,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-11.41,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-5.71,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-5.71,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-11.41,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.66,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-5.71,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-11.41,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-5.71,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-5.71,-34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-11.41,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-11.41,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.83,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-5.71,-22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-11.41,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-11.41,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.24,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-5.71,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.41,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-11.41,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,-5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,-5.71,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-11.41,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,-5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.24,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.95,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.66,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.95,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.66,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-11.41,-11.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.53,-5.71,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,-11.41,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.12,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-11.41,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-11.41,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,-5.71,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }

// add the cell borders
cylinder { <-45.469,-45.656,-45.656> <-43.979,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-43.606,-45.656,-45.656> <-42.115,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-41.742,-45.656,-45.656> <-40.252,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-39.879,-45.656,-45.656> <-38.388,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-38.015,-45.656,-45.656> <-36.525,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-36.152,-45.656,-45.656> <-34.661,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-34.288,-45.656,-45.656> <-32.798,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-32.425,-45.656,-45.656> <-30.934,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-30.561,-45.656,-45.656> <-29.071,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.698,-45.656,-45.656> <-27.207,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-26.834,-45.656,-45.656> <-25.344,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-24.971,-45.656,-45.656> <-23.480,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-23.107,-45.656,-45.656> <-21.617,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-21.244,-45.656,-45.656> <-19.753,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-19.380,-45.656,-45.656> <-17.890,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.517,-45.656,-45.656> <-16.026,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.653,-45.656,-45.656> <-14.163,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.790,-45.656,-45.656> <-12.299,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.926,-45.656,-45.656> <-10.436,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-10.063,-45.656,-45.656> <-8.572,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-8.199,-45.656,-45.656> <-6.709,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.336,-45.656,-45.656> <-4.845,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.472,-45.656,-45.656> <-2.982,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.609,-45.656,-45.656> <-1.118,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.745,-45.656,-45.656> <.745,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.118,-45.656,-45.656> <2.609,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.982,-45.656,-45.656> <4.472,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.845,-45.656,-45.656> <6.336,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.709,-45.656,-45.656> <8.199,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.572,-45.656,-45.656> <10.063,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.436,-45.656,-45.656> <11.926,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <12.299,-45.656,-45.656> <13.790,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <14.163,-45.656,-45.656> <15.653,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <16.026,-45.656,-45.656> <17.517,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.890,-45.656,-45.656> <19.380,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <19.753,-45.656,-45.656> <21.244,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <21.617,-45.656,-45.656> <23.107,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <23.480,-45.656,-45.656> <24.971,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <25.344,-45.656,-45.656> <26.834,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <27.207,-45.656,-45.656> <28.698,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <29.071,-45.656,-45.656> <30.561,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <30.934,-45.656,-45.656> <32.425,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <32.798,-45.656,-45.656> <34.288,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <34.661,-45.656,-45.656> <36.152,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <36.525,-45.656,-45.656> <38.015,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <38.388,-45.656,-45.656> <39.879,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <40.252,-45.656,-45.656> <41.742,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <42.115,-45.656,-45.656> <43.606,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <43.979,-45.656,-45.656> <45.469,-45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.469,-45.656,45.656> <-43.979,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-43.606,-45.656,45.656> <-42.115,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-41.742,-45.656,45.656> <-40.252,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-39.879,-45.656,45.656> <-38.388,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-38.015,-45.656,45.656> <-36.525,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-36.152,-45.656,45.656> <-34.661,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-34.288,-45.656,45.656> <-32.798,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-32.425,-45.656,45.656> <-30.934,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-30.561,-45.656,45.656> <-29.071,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.698,-45.656,45.656> <-27.207,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-26.834,-45.656,45.656> <-25.344,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-24.971,-45.656,45.656> <-23.480,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-23.107,-45.656,45.656> <-21.617,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-21.244,-45.656,45.656> <-19.753,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-19.380,-45.656,45.656> <-17.890,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.517,-45.656,45.656> <-16.026,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.653,-45.656,45.656> <-14.163,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.790,-45.656,45.656> <-12.299,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.926,-45.656,45.656> <-10.436,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-10.063,-45.656,45.656> <-8.572,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-8.199,-45.656,45.656> <-6.709,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.336,-45.656,45.656> <-4.845,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.472,-45.656,45.656> <-2.982,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.609,-45.656,45.656> <-1.118,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.745,-45.656,45.656> <.745,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.118,-45.656,45.656> <2.609,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.982,-45.656,45.656> <4.472,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.845,-45.656,45.656> <6.336,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.709,-45.656,45.656> <8.199,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.572,-45.656,45.656> <10.063,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.436,-45.656,45.656> <11.926,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <12.299,-45.656,45.656> <13.790,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <14.163,-45.656,45.656> <15.653,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <16.026,-45.656,45.656> <17.517,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.890,-45.656,45.656> <19.380,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <19.753,-45.656,45.656> <21.244,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <21.617,-45.656,45.656> <23.107,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <23.480,-45.656,45.656> <24.971,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <25.344,-45.656,45.656> <26.834,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <27.207,-45.656,45.656> <28.698,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <29.071,-45.656,45.656> <30.561,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <30.934,-45.656,45.656> <32.425,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <32.798,-45.656,45.656> <34.288,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <34.661,-45.656,45.656> <36.152,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <36.525,-45.656,45.656> <38.015,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <38.388,-45.656,45.656> <39.879,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <40.252,-45.656,45.656> <41.742,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <42.115,-45.656,45.656> <43.606,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <43.979,-45.656,45.656> <45.469,-45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.469,45.656,-45.656> <-43.979,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-43.606,45.656,-45.656> <-42.115,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-41.742,45.656,-45.656> <-40.252,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-39.879,45.656,-45.656> <-38.388,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-38.015,45.656,-45.656> <-36.525,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-36.152,45.656,-45.656> <-34.661,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-34.288,45.656,-45.656> <-32.798,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-32.425,45.656,-45.656> <-30.934,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-30.561,45.656,-45.656> <-29.071,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.698,45.656,-45.656> <-27.207,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-26.834,45.656,-45.656> <-25.344,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-24.971,45.656,-45.656> <-23.480,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-23.107,45.656,-45.656> <-21.617,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-21.244,45.656,-45.656> <-19.753,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-19.380,45.656,-45.656> <-17.890,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.517,45.656,-45.656> <-16.026,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.653,45.656,-45.656> <-14.163,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.790,45.656,-45.656> <-12.299,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.926,45.656,-45.656> <-10.436,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-10.063,45.656,-45.656> <-8.572,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-8.199,45.656,-45.656> <-6.709,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.336,45.656,-45.656> <-4.845,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.472,45.656,-45.656> <-2.982,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.609,45.656,-45.656> <-1.118,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.745,45.656,-45.656> <.745,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.118,45.656,-45.656> <2.609,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.982,45.656,-45.656> <4.472,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.845,45.656,-45.656> <6.336,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.709,45.656,-45.656> <8.199,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.572,45.656,-45.656> <10.063,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.436,45.656,-45.656> <11.926,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <12.299,45.656,-45.656> <13.790,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <14.163,45.656,-45.656> <15.653,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <16.026,45.656,-45.656> <17.517,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.890,45.656,-45.656> <19.380,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <19.753,45.656,-45.656> <21.244,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <21.617,45.656,-45.656> <23.107,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <23.480,45.656,-45.656> <24.971,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <25.344,45.656,-45.656> <26.834,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <27.207,45.656,-45.656> <28.698,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <29.071,45.656,-45.656> <30.561,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <30.934,45.656,-45.656> <32.425,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <32.798,45.656,-45.656> <34.288,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <34.661,45.656,-45.656> <36.152,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <36.525,45.656,-45.656> <38.015,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <38.388,45.656,-45.656> <39.879,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <40.252,45.656,-45.656> <41.742,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <42.115,45.656,-45.656> <43.606,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <43.979,45.656,-45.656> <45.469,45.656,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.469,45.656,45.656> <-43.979,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-43.606,45.656,45.656> <-42.115,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-41.742,45.656,45.656> <-40.252,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-39.879,45.656,45.656> <-38.388,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-38.015,45.656,45.656> <-36.525,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-36.152,45.656,45.656> <-34.661,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-34.288,45.656,45.656> <-32.798,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-32.425,45.656,45.656> <-30.934,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-30.561,45.656,45.656> <-29.071,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-28.698,45.656,45.656> <-27.207,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-26.834,45.656,45.656> <-25.344,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-24.971,45.656,45.656> <-23.480,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-23.107,45.656,45.656> <-21.617,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-21.244,45.656,45.656> <-19.753,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-19.380,45.656,45.656> <-17.890,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-17.517,45.656,45.656> <-16.026,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-15.653,45.656,45.656> <-14.163,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-13.790,45.656,45.656> <-12.299,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-11.926,45.656,45.656> <-10.436,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-10.063,45.656,45.656> <-8.572,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-8.199,45.656,45.656> <-6.709,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-6.336,45.656,45.656> <-4.845,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-4.472,45.656,45.656> <-2.982,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.609,45.656,45.656> <-1.118,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.745,45.656,45.656> <.745,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <1.118,45.656,45.656> <2.609,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.982,45.656,45.656> <4.472,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.845,45.656,45.656> <6.336,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <6.709,45.656,45.656> <8.199,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <8.572,45.656,45.656> <10.063,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <10.436,45.656,45.656> <11.926,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <12.299,45.656,45.656> <13.790,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <14.163,45.656,45.656> <15.653,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <16.026,45.656,45.656> <17.517,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <17.890,45.656,45.656> <19.380,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <19.753,45.656,45.656> <21.244,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <21.617,45.656,45.656> <23.107,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <23.480,45.656,45.656> <24.971,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <25.344,45.656,45.656> <26.834,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <27.207,45.656,45.656> <28.698,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <29.071,45.656,45.656> <30.561,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <30.934,45.656,45.656> <32.425,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <32.798,45.656,45.656> <34.288,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <34.661,45.656,45.656> <36.152,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <36.525,45.656,45.656> <38.015,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <38.388,45.656,45.656> <39.879,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <40.252,45.656,45.656> <41.742,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <42.115,45.656,45.656> <43.606,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <43.979,45.656,45.656> <45.469,45.656,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-45.469> <-45.656,-45.656,-43.979>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-43.606> <-45.656,-45.656,-42.115>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-41.742> <-45.656,-45.656,-40.252>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-39.879> <-45.656,-45.656,-38.388>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-38.015> <-45.656,-45.656,-36.525>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-36.152> <-45.656,-45.656,-34.661>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-34.288> <-45.656,-45.656,-32.798>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-32.425> <-45.656,-45.656,-30.934>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-30.561> <-45.656,-45.656,-29.071>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-28.698> <-45.656,-45.656,-27.207>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-26.834> <-45.656,-45.656,-25.344>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-24.971> <-45.656,-45.656,-23.480>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-23.107> <-45.656,-45.656,-21.617>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-21.244> <-45.656,-45.656,-19.753>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-19.380> <-45.656,-45.656,-17.890>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-17.517> <-45.656,-45.656,-16.026>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-15.653> <-45.656,-45.656,-14.163>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-13.790> <-45.656,-45.656,-12.299>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-11.926> <-45.656,-45.656,-10.436>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-10.063> <-45.656,-45.656,-8.572>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-8.199> <-45.656,-45.656,-6.709>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-6.336> <-45.656,-45.656,-4.845>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-4.472> <-45.656,-45.656,-2.982>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-2.609> <-45.656,-45.656,-1.118>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,-.745> <-45.656,-45.656,.745>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,1.118> <-45.656,-45.656,2.609>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,2.982> <-45.656,-45.656,4.472>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,4.845> <-45.656,-45.656,6.336>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,6.709> <-45.656,-45.656,8.199>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,8.572> <-45.656,-45.656,10.063>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,10.436> <-45.656,-45.656,11.926>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,12.299> <-45.656,-45.656,13.790>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,14.163> <-45.656,-45.656,15.653>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,16.026> <-45.656,-45.656,17.517>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,17.890> <-45.656,-45.656,19.380>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,19.753> <-45.656,-45.656,21.244>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,21.617> <-45.656,-45.656,23.107>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,23.480> <-45.656,-45.656,24.971>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,25.344> <-45.656,-45.656,26.834>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,27.207> <-45.656,-45.656,28.698>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,29.071> <-45.656,-45.656,30.561>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,30.934> <-45.656,-45.656,32.425>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,32.798> <-45.656,-45.656,34.288>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,34.661> <-45.656,-45.656,36.152>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,36.525> <-45.656,-45.656,38.015>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,38.388> <-45.656,-45.656,39.879>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,40.252> <-45.656,-45.656,41.742>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,42.115> <-45.656,-45.656,43.606>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.656,43.979> <-45.656,-45.656,45.469>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-45.469> <-45.656,45.656,-43.979>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-43.606> <-45.656,45.656,-42.115>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-41.742> <-45.656,45.656,-40.252>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-39.879> <-45.656,45.656,-38.388>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-38.015> <-45.656,45.656,-36.525>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-36.152> <-45.656,45.656,-34.661>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-34.288> <-45.656,45.656,-32.798>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-32.425> <-45.656,45.656,-30.934>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-30.561> <-45.656,45.656,-29.071>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-28.698> <-45.656,45.656,-27.207>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-26.834> <-45.656,45.656,-25.344>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-24.971> <-45.656,45.656,-23.480>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-23.107> <-45.656,45.656,-21.617>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-21.244> <-45.656,45.656,-19.753>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-19.380> <-45.656,45.656,-17.890>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-17.517> <-45.656,45.656,-16.026>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-15.653> <-45.656,45.656,-14.163>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-13.790> <-45.656,45.656,-12.299>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-11.926> <-45.656,45.656,-10.436>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-10.063> <-45.656,45.656,-8.572>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-8.199> <-45.656,45.656,-6.709>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-6.336> <-45.656,45.656,-4.845>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-4.472> <-45.656,45.656,-2.982>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-2.609> <-45.656,45.656,-1.118>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,-.745> <-45.656,45.656,.745>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,1.118> <-45.656,45.656,2.609>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,2.982> <-45.656,45.656,4.472>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,4.845> <-45.656,45.656,6.336>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,6.709> <-45.656,45.656,8.199>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,8.572> <-45.656,45.656,10.063>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,10.436> <-45.656,45.656,11.926>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,12.299> <-45.656,45.656,13.790>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,14.163> <-45.656,45.656,15.653>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,16.026> <-45.656,45.656,17.517>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,17.890> <-45.656,45.656,19.380>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,19.753> <-45.656,45.656,21.244>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,21.617> <-45.656,45.656,23.107>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,23.480> <-45.656,45.656,24.971>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,25.344> <-45.656,45.656,26.834>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,27.207> <-45.656,45.656,28.698>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,29.071> <-45.656,45.656,30.561>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,30.934> <-45.656,45.656,32.425>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,32.798> <-45.656,45.656,34.288>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,34.661> <-45.656,45.656,36.152>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,36.525> <-45.656,45.656,38.015>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,38.388> <-45.656,45.656,39.879>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,40.252> <-45.656,45.656,41.742>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,42.115> <-45.656,45.656,43.606>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,45.656,43.979> <-45.656,45.656,45.469>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-45.469> <45.656,-45.656,-43.979>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-43.606> <45.656,-45.656,-42.115>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-41.742> <45.656,-45.656,-40.252>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-39.879> <45.656,-45.656,-38.388>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-38.015> <45.656,-45.656,-36.525>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-36.152> <45.656,-45.656,-34.661>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-34.288> <45.656,-45.656,-32.798>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-32.425> <45.656,-45.656,-30.934>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-30.561> <45.656,-45.656,-29.071>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-28.698> <45.656,-45.656,-27.207>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-26.834> <45.656,-45.656,-25.344>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-24.971> <45.656,-45.656,-23.480>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-23.107> <45.656,-45.656,-21.617>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-21.244> <45.656,-45.656,-19.753>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-19.380> <45.656,-45.656,-17.890>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-17.517> <45.656,-45.656,-16.026>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-15.653> <45.656,-45.656,-14.163>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-13.790> <45.656,-45.656,-12.299>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-11.926> <45.656,-45.656,-10.436>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-10.063> <45.656,-45.656,-8.572>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-8.199> <45.656,-45.656,-6.709>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-6.336> <45.656,-45.656,-4.845>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-4.472> <45.656,-45.656,-2.982>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-2.609> <45.656,-45.656,-1.118>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,-.745> <45.656,-45.656,.745>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,1.118> <45.656,-45.656,2.609>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,2.982> <45.656,-45.656,4.472>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,4.845> <45.656,-45.656,6.336>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,6.709> <45.656,-45.656,8.199>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,8.572> <45.656,-45.656,10.063>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,10.436> <45.656,-45.656,11.926>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,12.299> <45.656,-45.656,13.790>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,14.163> <45.656,-45.656,15.653>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,16.026> <45.656,-45.656,17.517>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,17.890> <45.656,-45.656,19.380>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,19.753> <45.656,-45.656,21.244>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,21.617> <45.656,-45.656,23.107>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,23.480> <45.656,-45.656,24.971>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,25.344> <45.656,-45.656,26.834>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,27.207> <45.656,-45.656,28.698>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,29.071> <45.656,-45.656,30.561>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,30.934> <45.656,-45.656,32.425>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,32.798> <45.656,-45.656,34.288>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,34.661> <45.656,-45.656,36.152>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,36.525> <45.656,-45.656,38.015>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,38.388> <45.656,-45.656,39.879>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,40.252> <45.656,-45.656,41.742>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,42.115> <45.656,-45.656,43.606>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.656,43.979> <45.656,-45.656,45.469>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-45.469> <45.656,45.656,-43.979>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-43.606> <45.656,45.656,-42.115>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-41.742> <45.656,45.656,-40.252>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-39.879> <45.656,45.656,-38.388>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-38.015> <45.656,45.656,-36.525>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-36.152> <45.656,45.656,-34.661>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-34.288> <45.656,45.656,-32.798>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-32.425> <45.656,45.656,-30.934>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-30.561> <45.656,45.656,-29.071>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-28.698> <45.656,45.656,-27.207>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-26.834> <45.656,45.656,-25.344>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-24.971> <45.656,45.656,-23.480>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-23.107> <45.656,45.656,-21.617>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-21.244> <45.656,45.656,-19.753>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-19.380> <45.656,45.656,-17.890>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-17.517> <45.656,45.656,-16.026>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-15.653> <45.656,45.656,-14.163>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-13.790> <45.656,45.656,-12.299>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-11.926> <45.656,45.656,-10.436>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-10.063> <45.656,45.656,-8.572>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-8.199> <45.656,45.656,-6.709>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-6.336> <45.656,45.656,-4.845>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-4.472> <45.656,45.656,-2.982>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-2.609> <45.656,45.656,-1.118>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,-.745> <45.656,45.656,.745>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,1.118> <45.656,45.656,2.609>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,2.982> <45.656,45.656,4.472>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,4.845> <45.656,45.656,6.336>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,6.709> <45.656,45.656,8.199>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,8.572> <45.656,45.656,10.063>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,10.436> <45.656,45.656,11.926>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,12.299> <45.656,45.656,13.790>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,14.163> <45.656,45.656,15.653>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,16.026> <45.656,45.656,17.517>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,17.890> <45.656,45.656,19.380>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,19.753> <45.656,45.656,21.244>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,21.617> <45.656,45.656,23.107>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,23.480> <45.656,45.656,24.971>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,25.344> <45.656,45.656,26.834>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,27.207> <45.656,45.656,28.698>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,29.071> <45.656,45.656,30.561>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,30.934> <45.656,45.656,32.425>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,32.798> <45.656,45.656,34.288>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,34.661> <45.656,45.656,36.152>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,36.525> <45.656,45.656,38.015>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,38.388> <45.656,45.656,39.879>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,40.252> <45.656,45.656,41.742>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,42.115> <45.656,45.656,43.606>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,45.656,43.979> <45.656,45.656,45.469>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.469,-45.656> <-45.656,-43.979,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-43.606,-45.656> <-45.656,-42.115,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-41.742,-45.656> <-45.656,-40.252,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-39.879,-45.656> <-45.656,-38.388,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-38.015,-45.656> <-45.656,-36.525,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-36.152,-45.656> <-45.656,-34.661,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-34.288,-45.656> <-45.656,-32.798,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-32.425,-45.656> <-45.656,-30.934,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-30.561,-45.656> <-45.656,-29.071,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-28.698,-45.656> <-45.656,-27.207,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-26.834,-45.656> <-45.656,-25.344,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-24.971,-45.656> <-45.656,-23.480,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-23.107,-45.656> <-45.656,-21.617,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-21.244,-45.656> <-45.656,-19.753,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-19.380,-45.656> <-45.656,-17.890,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-17.517,-45.656> <-45.656,-16.026,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-15.653,-45.656> <-45.656,-14.163,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-13.790,-45.656> <-45.656,-12.299,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-11.926,-45.656> <-45.656,-10.436,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-10.063,-45.656> <-45.656,-8.572,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-8.199,-45.656> <-45.656,-6.709,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-6.336,-45.656> <-45.656,-4.845,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-4.472,-45.656> <-45.656,-2.982,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-2.609,-45.656> <-45.656,-1.118,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-.745,-45.656> <-45.656,.745,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,1.118,-45.656> <-45.656,2.609,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,2.982,-45.656> <-45.656,4.472,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,4.845,-45.656> <-45.656,6.336,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,6.709,-45.656> <-45.656,8.199,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,8.572,-45.656> <-45.656,10.063,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,10.436,-45.656> <-45.656,11.926,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,12.299,-45.656> <-45.656,13.790,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,14.163,-45.656> <-45.656,15.653,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,16.026,-45.656> <-45.656,17.517,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,17.890,-45.656> <-45.656,19.380,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,19.753,-45.656> <-45.656,21.244,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,21.617,-45.656> <-45.656,23.107,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,23.480,-45.656> <-45.656,24.971,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,25.344,-45.656> <-45.656,26.834,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,27.207,-45.656> <-45.656,28.698,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,29.071,-45.656> <-45.656,30.561,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,30.934,-45.656> <-45.656,32.425,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,32.798,-45.656> <-45.656,34.288,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,34.661,-45.656> <-45.656,36.152,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,36.525,-45.656> <-45.656,38.015,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,38.388,-45.656> <-45.656,39.879,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,40.252,-45.656> <-45.656,41.742,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,42.115,-45.656> <-45.656,43.606,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,43.979,-45.656> <-45.656,45.469,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.469,-45.656> <45.656,-43.979,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-43.606,-45.656> <45.656,-42.115,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-41.742,-45.656> <45.656,-40.252,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-39.879,-45.656> <45.656,-38.388,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-38.015,-45.656> <45.656,-36.525,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-36.152,-45.656> <45.656,-34.661,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-34.288,-45.656> <45.656,-32.798,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-32.425,-45.656> <45.656,-30.934,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-30.561,-45.656> <45.656,-29.071,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-28.698,-45.656> <45.656,-27.207,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-26.834,-45.656> <45.656,-25.344,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-24.971,-45.656> <45.656,-23.480,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-23.107,-45.656> <45.656,-21.617,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-21.244,-45.656> <45.656,-19.753,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-19.380,-45.656> <45.656,-17.890,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-17.517,-45.656> <45.656,-16.026,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-15.653,-45.656> <45.656,-14.163,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-13.790,-45.656> <45.656,-12.299,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-11.926,-45.656> <45.656,-10.436,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-10.063,-45.656> <45.656,-8.572,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-8.199,-45.656> <45.656,-6.709,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-6.336,-45.656> <45.656,-4.845,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-4.472,-45.656> <45.656,-2.982,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-2.609,-45.656> <45.656,-1.118,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-.745,-45.656> <45.656,.745,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,1.118,-45.656> <45.656,2.609,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,2.982,-45.656> <45.656,4.472,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,4.845,-45.656> <45.656,6.336,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,6.709,-45.656> <45.656,8.199,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,8.572,-45.656> <45.656,10.063,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,10.436,-45.656> <45.656,11.926,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,12.299,-45.656> <45.656,13.790,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,14.163,-45.656> <45.656,15.653,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,16.026,-45.656> <45.656,17.517,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,17.890,-45.656> <45.656,19.380,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,19.753,-45.656> <45.656,21.244,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,21.617,-45.656> <45.656,23.107,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,23.480,-45.656> <45.656,24.971,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,25.344,-45.656> <45.656,26.834,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,27.207,-45.656> <45.656,28.698,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,29.071,-45.656> <45.656,30.561,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,30.934,-45.656> <45.656,32.425,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,32.798,-45.656> <45.656,34.288,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,34.661,-45.656> <45.656,36.152,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,36.525,-45.656> <45.656,38.015,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,38.388,-45.656> <45.656,39.879,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,40.252,-45.656> <45.656,41.742,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,42.115,-45.656> <45.656,43.606,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,43.979,-45.656> <45.656,45.469,-45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-45.469,45.656> <-45.656,-43.979,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-43.606,45.656> <-45.656,-42.115,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-41.742,45.656> <-45.656,-40.252,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-39.879,45.656> <-45.656,-38.388,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-38.015,45.656> <-45.656,-36.525,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-36.152,45.656> <-45.656,-34.661,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-34.288,45.656> <-45.656,-32.798,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-32.425,45.656> <-45.656,-30.934,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-30.561,45.656> <-45.656,-29.071,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-28.698,45.656> <-45.656,-27.207,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-26.834,45.656> <-45.656,-25.344,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-24.971,45.656> <-45.656,-23.480,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-23.107,45.656> <-45.656,-21.617,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-21.244,45.656> <-45.656,-19.753,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-19.380,45.656> <-45.656,-17.890,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-17.517,45.656> <-45.656,-16.026,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-15.653,45.656> <-45.656,-14.163,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-13.790,45.656> <-45.656,-12.299,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-11.926,45.656> <-45.656,-10.436,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-10.063,45.656> <-45.656,-8.572,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-8.199,45.656> <-45.656,-6.709,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-6.336,45.656> <-45.656,-4.845,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-4.472,45.656> <-45.656,-2.982,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-2.609,45.656> <-45.656,-1.118,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,-.745,45.656> <-45.656,.745,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,1.118,45.656> <-45.656,2.609,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,2.982,45.656> <-45.656,4.472,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,4.845,45.656> <-45.656,6.336,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,6.709,45.656> <-45.656,8.199,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,8.572,45.656> <-45.656,10.063,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,10.436,45.656> <-45.656,11.926,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,12.299,45.656> <-45.656,13.790,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,14.163,45.656> <-45.656,15.653,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,16.026,45.656> <-45.656,17.517,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,17.890,45.656> <-45.656,19.380,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,19.753,45.656> <-45.656,21.244,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,21.617,45.656> <-45.656,23.107,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,23.480,45.656> <-45.656,24.971,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,25.344,45.656> <-45.656,26.834,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,27.207,45.656> <-45.656,28.698,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,29.071,45.656> <-45.656,30.561,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,30.934,45.656> <-45.656,32.425,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,32.798,45.656> <-45.656,34.288,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,34.661,45.656> <-45.656,36.152,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,36.525,45.656> <-45.656,38.015,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,38.388,45.656> <-45.656,39.879,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,40.252,45.656> <-45.656,41.742,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,42.115,45.656> <-45.656,43.606,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-45.656,43.979,45.656> <-45.656,45.469,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-45.469,45.656> <45.656,-43.979,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-43.606,45.656> <45.656,-42.115,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-41.742,45.656> <45.656,-40.252,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-39.879,45.656> <45.656,-38.388,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-38.015,45.656> <45.656,-36.525,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-36.152,45.656> <45.656,-34.661,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-34.288,45.656> <45.656,-32.798,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-32.425,45.656> <45.656,-30.934,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-30.561,45.656> <45.656,-29.071,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-28.698,45.656> <45.656,-27.207,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-26.834,45.656> <45.656,-25.344,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-24.971,45.656> <45.656,-23.480,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-23.107,45.656> <45.656,-21.617,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-21.244,45.656> <45.656,-19.753,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-19.380,45.656> <45.656,-17.890,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-17.517,45.656> <45.656,-16.026,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-15.653,45.656> <45.656,-14.163,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-13.790,45.656> <45.656,-12.299,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-11.926,45.656> <45.656,-10.436,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-10.063,45.656> <45.656,-8.572,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-8.199,45.656> <45.656,-6.709,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-6.336,45.656> <45.656,-4.845,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-4.472,45.656> <45.656,-2.982,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-2.609,45.656> <45.656,-1.118,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,-.745,45.656> <45.656,.745,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,1.118,45.656> <45.656,2.609,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,2.982,45.656> <45.656,4.472,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,4.845,45.656> <45.656,6.336,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,6.709,45.656> <45.656,8.199,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,8.572,45.656> <45.656,10.063,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,10.436,45.656> <45.656,11.926,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,12.299,45.656> <45.656,13.790,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,14.163,45.656> <45.656,15.653,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,16.026,45.656> <45.656,17.517,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,17.890,45.656> <45.656,19.380,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,19.753,45.656> <45.656,21.244,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,21.617,45.656> <45.656,23.107,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,23.480,45.656> <45.656,24.971,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,25.344,45.656> <45.656,26.834,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,27.207,45.656> <45.656,28.698,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,29.071,45.656> <45.656,30.561,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,30.934,45.656> <45.656,32.425,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,32.798,45.656> <45.656,34.288,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,34.661,45.656> <45.656,36.152,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,36.525,45.656> <45.656,38.015,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,38.388,45.656> <45.656,39.879,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,40.252,45.656> <45.656,41.742,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,42.115,45.656> <45.656,43.606,45.656>, .1 pigment { color <.9,.9,.9> } }
cylinder { <45.656,43.979,45.656> <45.656,45.469,45.656>, .1 pigment { color <.9,.9,.9> } }
