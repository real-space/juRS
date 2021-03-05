#version 3.6;
#include "colors.inc"
#include "textures.inc"
#include "shapes.inc"
#include "glass.inc"
#include "metals.inc"

background {color White}

plane { <0,1,0>, -45.756 pigment{color <.6,.6,.6>} finish{ reflection {.2} ambient .5 diffuse .5 metallic }}

camera { location <273.93,91.31,182.62> look_at <.20,.27,-.09> right x*image_width/image_height/2 up y/2 }

// add lights
light_source { <0,200,400> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }
light_source { <400,200,0> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }


// add the atoms
sphere{ <-.19,.07,-.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.64,.00,-.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.80,-.18,5.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.09,5.46,5.44>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.75,5.57,5.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.45,5.81,-.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.33,5.69,-.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.43,.50,-.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.15,.05,.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.75,.17,5.60>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.39,.55,5.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.93,5.61,5.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.05,5.92,.34>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.28,5.94,-.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.19,-.27,-.01>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.73,-.02,-.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.38,.15,5.84>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.66,-.29,5.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.56,5.26,5.51>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.51,5.81,5.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.85,5.41,-.04>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.81,5.81,-.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.97,-.64,-.33>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.31,.22,-.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.03,.05,5.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.20,-.22,5.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.12,5.57,6.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.96,5.95,5.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.27,5.47,.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.43,-.21,.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.83,.19,5.73>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.64,-.01,5.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.41,5.70,5.49>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.97,5.88,5.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-41.01,5.63,-.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.03,5.20,.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.88,-.01,.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.74,-.27,-.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.16,.15,5.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.30,6.65,5.62>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.63,6.03,5.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.83,5.31,.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.51,5.76,.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.14,.29,-.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.16,.03,-.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.49,.41,5.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.14,.06,5.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.94,6.47,5.68>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.15,6.00,5.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.75,5.91,-.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.86,5.87,.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.67,.26,.02>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.97,.32,.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.36,.39,5.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.89,5.33,5.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.43,5.77,.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.12,-.11,11.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.89,.29,11.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.18,-.18,17.01>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.14,-.13,17.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.53,5.64,17.11>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.16,5.59,16.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.08,5.68,11.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.99,.62,11.75>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.61,.35,11.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.72,.30,17.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.71,.01,17.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.08,5.75,17.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.27,5.95,12.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.23,.14,11.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.24,-.05,17.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.54,5.60,16.96>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.68,5.73,16.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.43,5.36,11.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.79,-.56,11.75>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.87,.01,11.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.49,-.70,17.11>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.33,.64,17.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,5.54,17.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.64,5.59,11.21>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.25,5.61,11.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.27,-.54,11.22>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.25,-.02,11.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.29,.34,17.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.43,.12,17.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.24,5.76,17.15>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.21,6.02,17.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.44,5.98,11.67>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.42,5.62,11.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.56,-.03,11.67>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.82,-.14,10.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.17,.04,17.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.53,5.98,17.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.61,4.87,17.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.83,5.50,11.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.43,5.85,11.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.44,-.10,11.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.27,-.07,11.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.57,.39,17.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.18,.09,17.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.27,6.54,16.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.24,6.03,17.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.09,5.85,11.40>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.94,5.81,11.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.60,.27,11.21>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.72,-.03,11.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.66,.08,16.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.51,.14,17.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.30,6.29,17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.60,5.77,17.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.35,5.56,11.38>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.29,5.91,11.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.42,-.35,22.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.08,-.27,22.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.95,.22,28.97>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.24,-.12,28.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.07,5.90,28.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.83,5.63,28.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.18,5.44,22.64>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.29,5.51,22.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.20,.08,22.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.18,-.12,28.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.37,-.32,28.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.60,5.85,27.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.31,5.68,28.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.59,5.30,22.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.66,-.46,22.69>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-.11,22.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.59,-.27,28.39>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.98,-.18,28.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.22,5.46,28.67>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.88,5.44,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.54,5.93,22.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.75,5.44,22.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.89,-.41,22.39>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.67,-.22,22.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.91,.40,29.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.18,.08,28.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.47,5.90,27.88>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.82,5.87,28.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.93,5.29,22.80>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.48,5.64,22.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.23,-.49,22.70>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.11,.06,23.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.22,-.29,29.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.53,-.34,28.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.57,6.12,29.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.80,5.78,28.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.26,5.63,23.58>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.62,5.77,22.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.48,.14,23.07>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.56,.20,22.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.51,-.21,28.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-.13,28.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.52,5.30,28.48>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.54,5.81,28.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.24,6.63,22.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.30,5.83,22.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.70,.19,22.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.03,.01,22.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.02,.36,28.48>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.59,-.23,28.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.81,5.45,28.60>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.87,5.69,28.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.54,6.04,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.11,.76,22.96>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.67,-.98,22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.52,.58,28.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.45,.29,28.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.11,6.52,28.48>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.68,6.02,28.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.45,6.11,22.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.19,-.61,34.48>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.72,.09,34.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.39,-.43,40.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.33,5.14,40.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.86,5.26,39.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.63,5.67,34.52>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.14,5.48,34.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.63,.42,33.92>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.30,.09,34.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.98,-.85,39.65>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-.26,39.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.67,5.18,40.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.34,5.28,39.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.21,5.56,33.88>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.51,5.91,34.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.96,-.02,34.57>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.42,.04,34.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.64,-.33,39.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.13,5.39,40.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.79,5.63,40.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.82,6.07,34.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.77,5.83,34.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.44,.02,34.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.04,-.59,34.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.34,-.01,39.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.74,5.83,39.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.27,5.39,39.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.55,5.75,34.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.32,.95,34.40>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.12,-1.58,34.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.72,.56,39.82>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.19,.26,39.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.30,6.04,39.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.61,6.01,34.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.88,-.71,34.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.68,-.02,34.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.88,.04,40.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.58,-.38,40.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.79,5.33,39.90>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.67,5.78,40.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.78,6.06,34.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.51,5.36,34.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.90,-.37,34.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.84,-.23,34.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.04,-.40,40.06>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.87,-.15,40.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.72,5.73,40.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.00,5.70,40.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.62,5.42,34.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.77,5.67,34.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.14,.02,34.37>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.54,.19,34.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.21,.19,39.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.24,.09,39.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-10.99,6.22,39.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.55,5.83,40.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.04,5.73,34.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.63,-.19,-45.25>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.83,-.04,-45.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.08,-.33,-40.39>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.30,-.35,-39.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.72,5.26,-40.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.08,5.41,-40.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.89,5.50,-45.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.81,-.95,-45.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.35,-.40,45.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.40,.03,-40.11>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,.66,-40.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.97,5.68,-39.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,5.54,-45.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.35,5.51,-45.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.15,.48,-45.30>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.26,.16,45.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.06,-.06,-39.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.54,5.49,-39.88>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.57,5.56,-40.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.04,5.80,-45.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.17,.16,-45.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.84,-.03,-44.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <32.98,.05,-39.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.24,5.27,-39.72>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.93,5.36,-40.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.92,5.42,45.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.16,5.83,-45.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.64,.31,-45.22>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.12,-.22,45.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.41,-.09,-39.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.42,-.01,-39.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.63,5.38,-39.90>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.97,5.70,-39.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.87,5.38,-45.60>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.63,5.64,-45.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.61,-.04,-45.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.14,.03,-39.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.41,.23,-41.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.23,5.70,-39.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.70,5.73,-40.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.11,5.41,-45.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.23,.22,-45.05>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.25,.20,-45.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.40,.29,-39.60>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.90,.18,-39.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.53,6.10,-39.93>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.23,5.70,-39.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.76,5.63,-45.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.45,.52,-45.52>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.52,.13,-45.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.53,-.13,-39.87>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.49,-.08,-39.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.01,5.93,-40.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.57,5.77,-39.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.85,5.93,-45.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.55,-.45,-34.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.28,-.22,-29.06>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.01,-.28,-28.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.03,5.19,-28.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.87,5.44,-28.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.86,5.00,-34.60>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.04,5.18,-34.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.94,-.50,-34.64>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.43,-1.41,-34.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.28,.08,-28.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.31,.11,-28.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.48,5.60,-29.04>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.05,5.80,-28.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.70,5.53,-34.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.31,-.27,-34.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.60,-.15,-28.68>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.95,-.02,-28.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.08,5.57,-28.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.69,5.59,-28.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.30,5.24,-34.60>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.59,5.58,-34.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.04,-.54,-33.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.08,-.39,-34.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.09,-.22,-28.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.13,-.06,-28.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.56,5.34,-28.26>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.97,5.62,-28.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.42,5.10,-34.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.26,5.59,-34.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.19,.21,-34.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.41,.14,-28.73>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.59,.06,-28.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.19,5.70,-28.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.40,5.44,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-30.06,-.05,-33.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.97,.05,-27.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.71,5.61,-28.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.65,6.21,-28.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.74,5.45,-34.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.51,5.53,-34.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.09,-.01,-34.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.91,-.34,-28.74>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-23.08,-.37,-28.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.23,5.01,-28.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.19,5.62,-28.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.99,5.51,-34.49>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.86,5.49,-34.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.76,.25,-34.26>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.69,.42,-34.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.87,.48,-28.65>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.35,.11,-28.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.48,6.13,-28.48>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.68,5.71,-28.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.53,5.66,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.59,.04,-22.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.06,.07,-16.91>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.18,.24,-17.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.13,6.28,-16.45>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.48,5.71,-16.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.27,5.89,-22.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.26,5.61,-22.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.24,.12,-22.50>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.32,-.05,-22.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.98,-.10,-17.09>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.41,-.15,-16.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.32,5.59,-16.74>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.17,5.49,-17.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.95,5.82,-23.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.33,5.85,-23.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.99,-.06,-22.65>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.55,-.20,-22.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.16,-.27,-17.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.30,5.29,-16.58>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.96,5.43,-16.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.74,5.53,-22.40>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.86,5.56,-22.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.35,-.29,-21.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.03,-.14,-22.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.42,.38,-16.48>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.24,-.17,-16.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.65,5.81,-16.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.55,5.64,-22.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.42,5.56,-22.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.42,.35,-23.21>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.04,.26,-22.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.48,.13,-17.15>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.33,.01,-17.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.22,6.01,-16.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.07,5.92,-17.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.35,5.68,-22.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.25,.10,-22.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.78,.53,-17.67>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.26,.20,-17.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.45,6.38,-16.76>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.57,5.90,-17.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.14,5.40,-23.03>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.46,5.53,-22.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.19,-.15,-23.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.39,-.30,-17.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.02,-.08,-17.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.81,5.82,-17.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.02,5.60,-16.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.07,5.49,-22.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.99,5.41,-22.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.39,.27,-22.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.56,-.05,-22.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.61,-.42,-17.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.43,-.07,-17.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.20,5.49,-16.76>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,5.55,-17.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.27,5.89,-22.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.63,.64,-11.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.97,.33,-11.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.90,.34,-5.69>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.04,.05,-5.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <4.88,5.61,-5.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.17,6.06,-11.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.70,-.05,-11.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.50,.29,-6.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.33,.38,-6.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.11,5.73,-5.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.84,5.91,-11.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.19,5.83,-11.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.71,.25,-11.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.11,.43,-6.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <23.07,.35,-5.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.56,5.81,-5.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.97,5.31,-11.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.90,.05,-11.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.76,-.04,-6.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.26,5.78,-5.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.08,5.84,-5.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.82,5.86,-10.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.22,5.59,-11.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.28,-.24,-11.27>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.12,.09,-11.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.39,.21,-5.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.41,.31,-5.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.10,5.93,-5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.14,5.95,-11.52>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.57,5.77,-11.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.43,-.14,-11.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.45,-.13,-11.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.82,.29,-5.64>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.26,.05,-5.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.80,5.65,-5.39>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.71,5.80,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.32,5.43,-11.50>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.35,5.77,-11.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.98,.06,-11.85>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.33,-.07,-11.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.45,.14,-5.91>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.96,.13,-5.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.86,6.42,-5.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.13,5.72,-5.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.61,5.57,-11.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.47,-.06,-11.34>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.72,-.05,-11.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.75,.12,-5.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.60,-.09,-5.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.10,5.68,-5.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.49,5.74,-5.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.78,5.53,-11.35>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.58,5.78,-11.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.21,11.62,-.52>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.86,11.49,-.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.73,11.05,5.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.12,11.50,5.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.20,17.64,5.51>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.68,17.23,5.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.44,17.38,.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.23,17.30,-.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.00,11.55,.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.33,11.73,6.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.43,11.15,5.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.09,16.43,5.99>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <16.96,17.32,5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.03,17.67,-.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.35,17.08,.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,11.34,.11>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.32,11.13,-.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.08,11.35,5.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.87,17.07,5.60>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.69,16.88,5.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.67,16.49,-.06>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.87,17.13,-.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.81,11.00,.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.30,10.89,.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.47,11.57,5.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.72,17.16,5.99>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.97,18.18,5.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.51,16.37,.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.44,17.04,.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.72,11.43,-.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.40,11.80,5.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.46,11.43,5.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.35,17.57,6.23>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.74,17.50,5.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.52,16.71,-.38>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.31,16.93,.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.51,11.30,-.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.16,11.68,5.89>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.34,11.69,5.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.72,17.43,5.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-27.98,16.65,.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.16,17.04,-.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.84,11.23,-.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.77,11.46,5.01>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-23.06,11.70,5.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.54,17.08,6.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.68,17.00,.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.89,18.44,-.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.86,11.54,-.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.18,10.73,5.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.79,11.40,5.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.81,17.53,5.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.73,17.18,5.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.10,17.31,.07>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.46,16.78,.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.02,11.76,11.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.80,11.14,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.26,11.34,17.31>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.07,11.44,17.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.49,17.30,17.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.66,17.03,17.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.59,16.44,11.48>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.02,17.35,11.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.48,11.39,11.22>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <16.99,11.18,11.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.76,11.10,17.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.51,16.57,17.60>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.20,16.82,17.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.82,17.01,11.59>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.14,17.07,11.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.86,10.73,11.51>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.60,11.04,11.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.39,10.97,16.82>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.84,11.37,17.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.17,17.31,17.33>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.71,17.11,17.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.64,16.62,11.85>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.75,17.00,11.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.62,11.25,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.11,11.29,11.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.29,11.23,17.23>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.31,11.13,17.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.81,16.77,17.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.26,17.21,17.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.32,17.25,11.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.36,17.19,11.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.26,11.54,11.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.67,11.57,11.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.63,11.97,17.13>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.42,11.35,17.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.92,17.24,16.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.36,17.82,11.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.40,17.36,11.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.91,11.60,11.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.18,11.69,16.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.04,17.68,16.76>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.60,17.44,17.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-27.96,17.61,12.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-33.92,16.95,11.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.45,12.04,11.02>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.42,11.69,11.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-18.14,12.07,16.76>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.64,12.05,17.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.15,17.18,17.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.62,17.41,11.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.79,11.79,11.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.81,11.22,11.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.95,11.69,17.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.57,11.84,17.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.10,17.41,17.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.15,17.24,11.52>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.74,17.43,11.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.05,11.13,22.81>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.87,11.32,23.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.92,11.91,28.93>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.09,11.58,28.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.40,17.52,28.47>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.85,17.15,28.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.53,17.02,23.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.24,17.04,22.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.99,11.34,22.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.72,11.79,28.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.24,11.41,27.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.04,17.35,28.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.52,16.62,23.16>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.53,16.72,23.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.42,11.01,23.19>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.76,11.58,22.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.88,11.59,28.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.80,11.34,28.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.06,17.13,29.13>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.78,17.05,28.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.01,16.90,23.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.34,11.32,22.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <40.05,11.06,23.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.58,11.37,28.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.66,17.35,29.03>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <40.11,16.96,28.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.71,16.61,23.31>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.09,16.85,23.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.58,11.20,23.62>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.88,11.29,22.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.54,11.51,28.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.18,17.14,28.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.04,16.76,22.52>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.59,16.71,21.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.82,11.60,22.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.22,11.92,22.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.30,11.14,28.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.28,16.63,28.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.58,17.20,28.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.06,17.15,22.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.89,11.54,22.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.27,11.69,28.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.99,11.37,28.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.87,17.14,27.75>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.97,17.30,28.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.45,16.95,22.58>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.70,17.14,22.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.37,12.26,23.26>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.79,11.01,23.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.73,12.25,28.82>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.26,11.69,28.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.69,17.28,27.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.24,17.31,23.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.09,11.31,34.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.97,11.32,34.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.90,10.89,40.59>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.08,11.27,40.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.02,16.99,39.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.67,17.04,40.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.83,17.14,34.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.22,17.03,34.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.32,11.24,34.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.79,10.93,40.23>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.54,11.32,39.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.76,17.29,39.44>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.34,17.13,40.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.55,16.59,34.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.45,17.50,34.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.58,11.61,34.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.84,10.94,39.78>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.11,11.34,39.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.25,16.77,39.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.05,16.94,34.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.42,11.64,34.07>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.25,11.43,33.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.73,11.16,39.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.26,11.54,40.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <41.08,17.13,39.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.56,16.79,34.49>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.32,17.21,34.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.45,11.04,33.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.22,11.24,39.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.43,17.15,39.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.30,16.47,33.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.52,16.78,34.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-35.09,11.01,33.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.64,11.63,33.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.88,11.69,39.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.61,11.36,39.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-35.01,17.34,39.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.67,17.33,39.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.93,16.91,34.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.61,11.48,33.82>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.80,11.56,34.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.84,11.50,39.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.84,11.47,39.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.79,17.59,39.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.98,17.24,39.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.86,17.09,34.08>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.96,17.16,34.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.70,10.46,33.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.89,11.85,39.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.33,11.72,40.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.37,17.62,39.44>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.88,17.33,39.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.39,17.13,34.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.02,10.83,-45.64>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.55,10.91,-45.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.34,11.29,-40.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.08,17.07,-40.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.84,16.91,-40.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.09,16.87,45.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.72,11.16,45.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.25,11.32,-45.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <10.81,11.28,-40.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.69,16.42,-40.55>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.10,17.37,-40.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <10.81,16.96,45.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.39,11.08,45.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.17,10.87,-40.34>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.75,11.37,-40.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.32,16.77,-40.32>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.20,16.96,-39.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <27.85,16.63,45.13>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.30,16.60,45.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.79,11.49,45.43>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.85,11.34,45.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.77,10.99,-40.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <33.99,11.17,-40.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.83,17.02,-40.30>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.80,17.15,-40.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.57,17.28,45.06>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <33.92,17.00,45.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.46,11.46,45.04>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.16,11.38,45.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.11,11.44,-40.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.56,11.28,-40.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.54,17.28,-40.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.91,17.25,-40.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.19,17.30,45.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.48,17.08,45.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.20,11.01,45.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.56,11.28,45.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.40,11.80,-39.84>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.22,11.36,-40.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.90,17.57,-39.80>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.24,17.52,-40.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.43,17.28,45.21>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.32,17.07,45.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.44,11.56,45.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.95,11.22,-45.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.93,11.83,-40.02>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.60,11.53,-39.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.09,17.14,-39.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.65,17.18,-45.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.78,17.25,-45.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.65,11.25,45.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.37,11.39,-40.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.43,11.66,-40.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.75,17.56,-40.03>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.88,17.24,-40.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.58,16.54,45.27>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.25,17.49,-45.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.20,10.65,-34.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.63,11.23,-34.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.52,11.12,-28.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.20,11.25,-28.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.32,17.32,-28.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.70,17.04,-28.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.73,17.02,-34.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.06,16.93,-34.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.38,11.10,-34.76>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,11.52,-34.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.14,11.75,-29.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.40,11.25,-28.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.53,17.05,-28.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.29,17.35,-28.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.28,16.98,-35.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.60,17.09,-34.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.85,11.56,-34.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,11.55,-34.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.60,11.45,-28.73>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,11.60,-28.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.86,17.63,-28.42>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.51,17.27,-28.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.19,17.56,-34.36>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.77,17.32,-34.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.22,11.21,-34.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.94,11.32,-34.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.13,11.46,-28.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.29,11.45,-28.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.02,17.24,-28.67>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.78,17.25,-28.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.74,17.27,-34.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.00,17.15,-34.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.62,11.07,-34.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.93,11.34,-34.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.62,11.56,-28.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.57,11.21,-28.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.59,17.00,-28.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.82,17.27,-28.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.91,17.42,-34.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.59,17.27,-34.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.28,11.13,-34.20>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.55,11.50,-34.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.00,11.54,-28.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.11,17.22,-28.19>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.55,16.97,-28.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.01,17.71,-34.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.15,17.24,-34.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,11.34,-34.15>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.17,11.41,-34.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.41,11.39,-28.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.01,11.34,-28.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.47,17.23,-28.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.89,17.25,-28.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.52,17.23,-34.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.55,11.84,-34.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.86,11.15,-34.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.91,11.62,-28.79>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.33,11.74,-28.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.98,17.09,-28.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.20,17.17,-34.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.70,17.36,-34.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.08,11.43,-22.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.73,11.60,-22.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.67,11.78,-16.92>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.04,11.62,-16.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.55,17.13,-17.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.16,17.51,-22.67>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.08,17.30,-22.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.34,11.74,-22.60>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <16.96,11.61,-23.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.18,11.41,-17.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.28,11.45,-17.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.46,17.30,-16.88>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.26,17.14,-17.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.35,17.57,-22.59>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.49,17.14,-22.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.90,11.80,-22.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.63,11.49,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.86,11.12,-16.73>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.87,11.57,-17.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.33,17.15,-17.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.40,17.54,-22.87>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.87,17.29,-22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.65,11.33,-22.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.26,11.53,-22.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.48,11.28,-16.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.98,17.13,-17.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.89,16.50,-17.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.72,17.60,-22.38>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.18,17.18,-22.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.23,11.31,-22.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.38,11.77,-16.50>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.42,11.60,-16.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.55,17.67,-16.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.93,17.42,-16.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.25,17.12,-22.30>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.37,16.97,-22.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.67,11.22,-22.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.63,11.27,-23.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.10,11.87,-16.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-27.90,17.10,-17.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.71,16.88,-22.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.35,17.16,-22.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.16,11.04,-22.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.06,11.41,-22.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.57,11.40,-16.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.22,11.52,-16.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.38,17.13,-17.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.15,17.19,-23.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.80,17.11,-22.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.55,11.67,-22.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.82,11.14,-22.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.93,11.10,-17.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.55,11.25,-17.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.58,17.28,-17.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.60,16.93,-16.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.55,16.78,-22.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.37,17.07,-23.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.64,11.34,-11.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.91,10.80,-5.99>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.01,11.48,-6.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.27,16.77,-6.47>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.43,16.82,-5.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.77,17.25,-11.48>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.20,16.76,-11.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <10.97,11.28,-11.07>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <16.99,11.59,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.34,11.48,-5.51>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.24,11.23,-5.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.19,17.12,-5.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.08,17.28,-5.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.50,17.17,-11.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.77,10.77,-11.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.56,11.42,-11.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.02,11.17,-5.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.92,16.91,-6.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.64,16.94,-5.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.21,17.02,-11.67>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.72,16.94,-11.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.48,11.18,-10.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.08,11.50,-11.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.12,11.37,-5.57>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <33.93,11.51,-5.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.58,17.54,-5.51>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.07,17.33,-5.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.07,17.63,-11.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.11,17.25,-11.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.63,11.25,-10.76>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.17,11.51,-10.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.56,11.16,-5.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.43,16.75,-5.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.86,16.85,-5.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.55,17.16,-11.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.29,11.70,-10.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.58,11.46,-11.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.58,11.75,-5.55>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.28,11.35,-5.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.79,17.08,-5.50>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.26,17.20,-5.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.79,17.36,-11.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.39,17.28,-11.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.89,12.13,-10.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.07,11.45,-11.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.98,11.92,-5.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.49,16.84,-5.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.23,16.88,-11.35>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.70,17.45,-11.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.19,11.30,-11.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.72,11.27,-11.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.05,11.78,-5.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.67,11.40,-5.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.95,17.20,-5.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.74,17.35,-5.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.54,16.78,-11.21>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.45,16.97,-11.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.22,23.20,.23>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.79,23.00,.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.11,22.89,5.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.59,28.62,5.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.48,28.45,-.28>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.12,28.77,.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.38,22.78,.30>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.07,23.16,-.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.41,22.39,5.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.15,27.73,5.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.15,28.08,5.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.85,30.06,.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.32,28.41,-.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.72,23.14,-.20>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.49,22.72,.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.68,22.53,6.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.63,22.85,5.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.78,28.64,5.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.67,28.53,5.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.72,28.38,-.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.80,28.55,.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.84,22.93,.58>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.32,22.61,-.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.39,22.78,6.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.60,28.80,5.50>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <40.02,28.52,5.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.83,28.03,-.82>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.45,28.61,.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.27,23.23,.78>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.87,22.63,.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.38,23.34,6.35>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.55,23.18,6.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.17,28.32,5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.62,28.02,.65>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.26,28.56,.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.58,22.58,.42>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.49,22.55,-.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.39,20.33,5.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.70,28.16,5.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.36,27.90,-.07>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.30,28.38,.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.56,22.82,.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-18.05,22.44,5.67>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-23.11,22.38,5.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.04,27.74,5.64>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.87,28.06,5.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.66,28.20,-.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.94,23.21,.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.80,22.82,.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.50,22.66,5.40>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.47,23.10,5.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.67,28.30,5.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.71,28.65,.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.74,22.44,11.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.51,23.06,16.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.25,22.93,16.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.34,28.77,17.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.72,28.66,17.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.66,27.73,11.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.05,28.32,11.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.81,22.85,11.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.18,22.54,17.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.43,22.59,16.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.74,28.12,16.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.46,28.54,16.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.76,29.18,11.04>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.47,29.16,11.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.32,22.82,11.23>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.51,22.65,11.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.02,23.08,17.44>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.00,23.01,17.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.36,28.49,17.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.44,28.29,11.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.60,28.66,11.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.85,22.75,12.15>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <40.03,22.81,12.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.31,22.82,17.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.05,28.65,17.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.90,28.32,17.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.38,28.60,11.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.23,28.27,11.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.66,23.22,11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.31,22.65,16.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.35,28.26,16.76>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.75,28.31,17.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.35,28.47,11.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.45,22.75,10.58>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-27.96,23.00,10.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.56,22.91,16.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.67,28.77,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.97,28.15,11.90>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.08,28.32,11.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.77,11.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.84,22.54,16.84>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.29,22.35,17.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.29,27.99,17.09>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.80,28.44,17.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.50,28.12,11.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.85,28.61,11.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.71,23.12,12.04>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.44,22.86,11.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.36,23.11,16.96>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.49,22.53,17.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.00,28.68,17.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.53,28.25,10.64>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.69,28.48,12.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.37,22.75,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.59,22.88,23.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.12,22.78,28.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.46,28.53,28.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.93,28.22,22.91>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.06,28.52,23.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.48,22.29,23.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.23,22.59,22.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.31,22.39,28.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.26,27.81,28.96>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.22,28.44,28.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.56,28.23,22.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.36,28.34,22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.32,22.54,22.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.76,21.93,22.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.37,23.29,28.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.66,22.99,28.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.42,28.61,28.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.63,28.64,27.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.16,28.46,22.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.69,22.61,22.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.00,22.77,28.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.23,22.71,28.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.82,28.51,28.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.43,28.35,22.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <33.86,28.40,23.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.46,22.24,21.99>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.93,22.69,23.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.92,22.48,28.62>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.57,22.76,28.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.16,28.11,28.65>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.23,28.63,28.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.13,28.21,22.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.51,28.30,22.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.04,22.99,21.90>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.43,22.52,22.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.52,22.34,28.18>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.28,22.68,28.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.69,28.46,28.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.69,28.56,28.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.60,28.15,22.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.31,28.67,22.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.62,22.44,22.43>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.14,22.68,22.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.93,22.81,28.32>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.87,22.88,28.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.13,28.64,28.80>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.17,28.45,28.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.40,28.33,22.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.94,28.86,22.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.02,22.68,22.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.42,22.97,28.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.49,22.61,28.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.19,28.12,28.68>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.49,28.71,28.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.80,28.20,22.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.52,28.08,22.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <4.86,22.86,33.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.40,22.71,39.30>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.43,22.76,39.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.09,28.53,39.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.87,28.49,39.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.39,28.15,33.98>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.14,28.18,33.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.20,22.65,34.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.19,22.53,39.57>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.43,23.02,39.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.95,28.24,39.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.93,28.18,33.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.47,28.03,34.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.61,22.56,34.31>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.60,21.72,33.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.68,22.37,39.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.72,22.51,39.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.03,28.18,39.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.72,28.39,39.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.36,28.30,34.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.84,22.93,33.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.83,22.70,34.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.47,23.10,39.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.37,22.59,39.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.44,28.06,39.40>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.80,28.69,39.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.67,28.29,34.63>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.23,28.29,34.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.59,22.25,34.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.90,22.81,34.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.33,22.71,40.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.15,28.36,40.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.78,28.97,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.91,28.23,34.51>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <45.55,28.54,34.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.19,22.56,33.80>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.61,24.01,34.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.78,23.41,39.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.67,23.13,39.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.90,28.73,40.70>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.68,28.58,41.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.41,28.50,34.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.93,22.79,34.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.04,22.78,34.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.41,23.08,40.07>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.85,23.15,39.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-18.51,28.46,40.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.66,28.45,34.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.19,28.61,34.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.13,22.37,34.11>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.67,22.60,34.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.04,22.92,39.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.84,28.41,40.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.69,28.18,34.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.48,28.29,34.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.77,22.55,45.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.51,22.64,45.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.86,22.52,-39.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.22,22.79,-39.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.41,28.62,-39.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.58,28.62,-39.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.53,28.31,45.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.18,28.52,45.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.19,22.71,45.11>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.00,22.41,45.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.57,22.56,-40.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.38,28.20,-40.31>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.16,28.18,-39.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.31,27.96,45.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.39,28.57,45.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.58,22.19,-45.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.45,22.80,45.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.30,22.77,-40.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.62,22.53,-39.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.18,28.29,-39.67>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.67,28.51,-39.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.89,28.56,45.55>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.99,28.27,45.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.29,23.31,45.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.98,22.98,-45.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.04,22.91,-39.72>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.29,22.84,-40.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.68,28.93,-40.13>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.21,28.47,-40.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.31,28.81,45.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.64,23.35,-45.33>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.16,23.05,45.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.79,23.42,-40.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.52,22.97,-39.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.16,28.99,-39.82>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.86,28.95,-39.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.11,29.01,-45.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.37,28.61,-45.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.05,22.90,45.40>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.53,22.95,-45.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.06,23.21,-40.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.05,28.19,-39.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.34,28.55,-45.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.29,23.40,-45.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.74,23.00,-45.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.71,23.60,-39.44>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.16,23.22,-39.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.92,28.59,-40.22>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.61,28.95,-40.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.22,28.82,-45.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.26,22.65,45.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.13,22.96,-40.27>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.57,22.91,-39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-12.07,29.04,-40.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-6.10,28.53,-39.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.47,28.52,-45.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.76,28.63,45.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.39,22.62,-33.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.66,22.87,-34.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.52,22.84,-27.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.29,22.94,-28.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.44,28.55,-28.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.61,28.44,-34.02>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.09,28.47,-34.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.76,22.90,-34.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.34,22.84,-34.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.36,23.04,-28.94>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.55,22.79,-28.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.20,28.58,-28.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.23,28.57,-28.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.73,28.17,-34.29>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.32,28.64,-34.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.31,22.95,-34.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.07,23.50,-28.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.60,22.89,-27.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.08,29.13,-28.96>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.39,28.88,-28.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.89,27.63,-34.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.16,23.36,-34.33>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.01,22.90,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.67,23.13,-28.16>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.19,22.85,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.92,28.97,-28.67>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.61,28.58,-28.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.03,28.86,-34.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <33.94,28.86,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.06,23.13,-34.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.25,23.51,-28.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.37,22.93,-28.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.41,29.04,-28.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.14,28.99,-28.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.65,28.38,-34.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.17,23.42,-34.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.26,23.27,-34.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.37,22.64,-28.75>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.29,22.85,-28.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.32,28.82,-29.31>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.57,28.60,-28.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.32,28.93,-34.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.09,22.47,-34.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.89,23.44,-28.82>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.59,22.91,-28.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.62,29.00,-29.51>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.73,28.71,-28.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.36,28.26,-34.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.96,23.41,-34.61>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-6.02,22.89,-34.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.27,23.25,-28.84>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.48,22.43,-28.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.18,28.54,-28.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-10.70,28.53,-34.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.00,22.80,-22.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.26,22.43,-16.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.18,27.81,-17.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.82,28.11,-16.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.11,28.07,-22.27>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.22,28.64,-22.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.74,22.76,-22.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.72,22.88,-17.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.06,23.00,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.35,28.40,-16.36>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <16.98,28.60,-17.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.67,28.65,-22.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,28.23,-22.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.10,22.90,-22.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.70,22.99,-17.09>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.78,22.71,-17.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.07,28.59,-17.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.63,28.50,-17.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.19,28.65,-23.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.64,23.26,-22.75>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.69,22.93,-22.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.18,22.95,-17.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.80,28.99,-16.84>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.17,28.72,-17.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.71,29.20,-22.51>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <33.86,28.81,-22.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.38,23.08,-22.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.11,23.04,-22.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.48,23.62,-16.90>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.27,23.28,-16.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.47,29.10,-17.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.13,29.32,-22.98>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <45.57,28.80,-22.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.34,23.35,-22.44>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.60,22.83,-22.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.76,22.34,-16.82>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.34,22.55,-16.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.90,28.81,-16.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.79,28.41,-16.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.58,28.93,-22.68>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.52,28.85,-22.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.85,22.95,-22.85>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.06,22.95,-23.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.16,22.94,-17.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.99,22.65,-16.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.78,28.16,-17.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.03,28.58,-17.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.73,29.00,-23.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.80,28.72,-22.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.04,23.40,-23.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.44,22.81,-23.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.13,22.51,-16.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.40,22.93,-17.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.91,28.25,-17.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.27,28.48,-23.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.21,28.82,-22.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.58,22.98,-11.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.47,22.51,-5.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.15,22.65,-5.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.36,28.19,-5.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.09,28.38,-5.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.25,28.67,-11.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <10.88,23.04,-11.39>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <16.86,22.60,-11.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.88,23.32,-5.45>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.43,22.87,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.97,28.61,-5.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.70,28.45,-11.06>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.12,28.49,-11.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.48,22.69,-11.88>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.49,22.85,-11.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.71,22.69,-5.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.73,22.81,-5.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.98,28.70,-5.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.76,28.64,-5.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.60,28.85,-11.78>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.88,28.53,-11.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.40,23.31,-11.19>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.96,23.15,-12.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.49,23.18,-5.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <38.78,29.46,-5.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.29,29.40,-10.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.19,28.99,-11.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.62,23.19,-10.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.96,22.91,-11.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.23,22.50,-5.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.41,22.85,-5.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.41,28.45,-5.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.79,28.44,-11.07>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.55,28.71,-11.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.83,23.43,-11.02>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.32,23.02,-11.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.22,22.84,-5.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.86,28.54,-5.13>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.83,28.39,-6.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.31,28.92,-11.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.33,22.73,-11.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.75,22.44,-5.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.17,22.59,-6.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.25,28.09,-6.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.30,28.45,-5.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.43,28.56,-11.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.94,29.55,-11.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.42,22.85,-11.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.54,22.50,-11.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.15,22.81,-5.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.59,28.52,-5.48>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.59,28.25,-5.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.17,28.15,-11.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.38,28.55,-11.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.03,34.49,-.31>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.84,34.09,.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.76,33.84,6.11>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.42,33.80,5.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.26,39.35,5.87>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.03,39.70,5.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.00,39.68,.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.03,40.00,.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.20,35.16,.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.97,33.87,6.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.51,39.26,5.75>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.59,40.44,6.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.65,39.81,.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.34,34.25,-.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.16,34.20,5.85>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.43,34.44,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.35,39.81,5.42>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.69,40.06,5.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.65,39.84,-.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.87,39.70,-.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.33,34.53,-.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.04,34.13,-.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.73,33.81,5.26>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.31,34.22,5.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.86,39.80,5.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.08,39.71,-.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.37,40.17,.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.83,34.03,.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.35,34.14,5.52>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.40,34.07,5.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.39,39.74,6.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.84,39.86,5.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.99,39.25,.25>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.47,39.79,-.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.57,33.97,-.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.72,33.83,5.87>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.35,34.03,5.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.54,39.36,5.94>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.79,40.05,5.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.78,39.49,-.09>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.37,39.89,.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.83,33.89,-.33>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.75,33.93,-.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.24,33.53,5.44>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.86,34.04,5.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.22,39.74,5.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.21,39.81,5.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,39.57,-.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.82,40.02,-.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.64,34.10,.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.34,33.68,5.68>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.32,34.02,5.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.47,39.57,5.74>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.60,39.98,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.76,39.31,.15>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.22,39.98,-.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.57,33.76,11.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.21,33.91,11.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.09,34.44,17.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.42,40.10,17.64>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.63,39.75,17.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.11,39.46,11.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.07,39.85,11.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.05,34.42,11.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.23,34.27,16.67>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.43,34.14,17.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.62,39.58,17.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.26,39.93,17.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.76,39.58,11.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.26,34.26,11.14>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.38,34.18,11.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.89,33.97,17.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.40,39.39,16.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.83,40.03,17.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.79,39.80,11.73>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.72,39.81,11.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.29,33.92,11.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.03,34.32,11.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.01,33.99,17.21>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.05,34.37,17.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.70,40.30,16.85>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <40.02,40.11,16.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.48,39.87,11.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.32,34.32,11.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.84,32.65,11.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.37,33.58,17.07>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.52,34.36,17.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.19,39.64,16.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.55,40.10,11.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.51,34.19,11.15>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.51,33.96,11.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.10,33.35,16.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.81,39.60,17.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.19,39.62,11.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.68,39.83,11.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.06,34.27,11.68>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.29,34.13,11.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.50,33.94,17.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.05,34.16,17.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.94,39.86,17.68>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.13,40.14,17.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.57,39.97,11.82>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.05,40.01,11.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.53,34.22,11.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.59,34.09,11.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.96,34.28,17.60>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.35,35.18,17.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.75,39.94,17.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.88,39.68,11.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.46,39.97,11.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.35,34.17,23.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.59,34.06,22.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.54,33.90,28.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.26,35.16,28.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.91,40.04,28.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.54,39.78,23.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.20,40.05,23.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.40,33.97,22.99>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.15,34.26,22.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.07,33.77,28.34>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.42,34.20,28.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.29,39.46,28.86>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.29,39.76,28.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.32,39.86,22.43>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.58,39.94,23.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.98,34.16,23.02>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.58,34.42,22.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.85,34.67,28.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.71,34.16,28.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.92,39.59,28.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.57,40.23,28.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.61,39.80,23.20>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.89,39.99,22.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.35,34.12,23.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.02,34.23,23.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.37,34.31,28.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.18,40.04,28.36>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.72,39.79,28.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.88,40.24,22.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.11,40.08,22.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.27,33.90,22.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.65,34.16,22.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.36,34.13,28.97>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <45.64,34.01,28.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.55,39.60,28.30>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.97,40.00,28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.58,39.90,22.21>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.65,39.88,22.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.67,34.24,22.04>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.49,34.13,22.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.73,34.51,27.90>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.49,34.30,28.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.16,40.08,28.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.59,40.13,27.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.01,39.84,22.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.42,34.32,22.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.89,34.20,28.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.91,34.40,28.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.11,40.05,28.76>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.14,40.13,28.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.75,41.20,22.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.92,33.62,22.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.95,34.39,23.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.27,34.32,28.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.21,39.84,28.48>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.76,39.77,28.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.30,39.92,23.09>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.59,39.82,23.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.38,33.71,33.88>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.06,34.07,34.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.97,34.16,39.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.02,34.36,39.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.25,40.40,39.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.70,40.04,40.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.86,39.97,34.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.12,40.00,34.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.20,34.07,34.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.39,33.86,39.51>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.46,33.95,40.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.59,39.34,39.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.28,39.83,39.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.46,39.44,33.96>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.50,40.82,34.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.31,33.96,34.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.02,34.24,40.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.94,34.23,39.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.13,40.01,39.96>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.51,40.09,39.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.11,39.52,34.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.77,34.12,34.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.91,33.85,33.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.52,34.14,40.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.56,40.36,40.06>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.04,40.59,39.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.70,39.31,34.33>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.07,39.88,34.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.01,33.84,34.91>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.26,34.31,34.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <44.52,34.07,40.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.20,39.27,40.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.71,39.84,39.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.66,39.79,34.43>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.52,39.59,34.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.69,34.18,34.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.67,33.41,34.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.97,33.93,39.76>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.57,34.23,40.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.77,39.75,40.17>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.53,39.85,39.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.14,39.98,34.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.32,34.75,34.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.24,34.27,34.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.75,34.01,40.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.11,35.15,40.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.50,39.98,40.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.52,40.16,34.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.21,40.33,34.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.41,33.90,34.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,34.34,34.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.91,34.18,39.76>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.59,33.91,40.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.95,39.57,40.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.89,40.21,39.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.96,40.19,34.14>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.46,40.02,34.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.18,34.34,-45.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.74,34.40,-45.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.49,34.16,-40.08>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.10,34.35,-39.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.66,40.46,-39.92>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.53,39.89,-40.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.69,39.97,-45.64>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.02,40.17,45.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.84,34.10,45.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.24,33.87,-40.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.31,34.19,-39.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.38,39.96,-40.11>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.20,39.92,-39.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.12,39.93,-45.60>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.23,39.80,45.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.86,34.12,45.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.59,34.11,-45.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.18,34.67,-39.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.95,34.10,-39.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.28,39.80,-39.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.85,40.26,-39.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.83,40.25,-45.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.85,39.95,45.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.11,33.74,-45.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.62,34.16,-40.30>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.46,34.39,-41.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <35.09,39.96,-39.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.54,39.70,-39.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.20,39.00,45.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.40,40.05,-45.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.10,34.59,45.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.37,34.20,-40.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.97,41.01,-39.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.83,39.72,-44.37>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.36,39.50,-45.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.68,34.22,-44.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.56,34.27,-45.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.55,33.55,-39.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.93,34.23,-39.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.60,39.75,-40.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.67,39.67,45.42>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.18,39.87,45.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.86,34.22,-44.92>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.26,34.02,-45.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.90,34.22,-39.32>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-23.08,34.31,-39.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.09,39.79,-39.89>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.16,39.78,-39.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.52,39.52,-45.01>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.05,39.88,-45.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.96,34.35,-45.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.40,34.48,-40.07>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.70,34.53,-39.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.25,40.01,-39.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.92,40.55,45.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.54,39.92,-45.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.24,34.10,-33.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,34.13,-34.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.61,34.45,-28.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.13,34.04,-28.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.01,39.87,-28.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.80,40.20,-28.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.78,39.92,-34.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.15,39.96,-34.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.82,34.09,-34.49>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.22,33.94,-34.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.54,34.27,-28.30>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.54,34.26,-28.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.94,40.13,-28.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.59,39.90,-28.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.89,39.55,-33.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.59,39.81,-34.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.44,33.32,-34.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.78,35.05,-29.08>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.91,34.36,-28.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.38,40.50,-27.96>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.67,40.24,-28.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.43,39.54,-33.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.62,34.37,-34.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.28,34.74,-28.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.15,34.33,-28.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.60,40.58,-28.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.36,40.31,-28.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.21,40.34,-33.74>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.27,39.78,-34.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.56,34.34,-34.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.02,33.73,-34.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.47,34.58,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.31,39.56,-28.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.12,39.11,-33.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.57,39.98,-35.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.47,33.90,-34.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.39,34.15,-28.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.78,34.15,-27.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.56,39.79,-28.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.42,39.80,-28.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.33,39.34,-34.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.35,39.44,-34.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.36,32.64,-34.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.73,34.83,-29.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.14,34.62,-28.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.44,40.31,-28.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.71,39.64,-34.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-12.19,33.56,-34.34>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-6.06,33.78,-34.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.46,33.77,-28.77>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.68,33.86,-28.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.88,39.46,-28.50>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.82,39.64,-28.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.36,39.28,-34.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.91,39.42,-34.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.32,34.24,-22.46>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.59,33.97,-22.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.64,33.70,-16.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.14,34.01,-17.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.17,39.58,-17.19>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.85,39.84,-17.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.94,39.63,-22.74>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.12,39.97,-22.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.58,33.96,-22.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.34,34.33,-22.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.45,34.35,-16.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.49,34.01,-16.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.15,39.76,-17.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.60,39.87,-18.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.57,39.96,-22.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.18,34.12,-23.14>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.57,34.34,-23.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.02,34.81,-17.33>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.98,34.43,-17.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.63,40.24,-17.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.62,39.79,-22.87>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.07,40.07,-22.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.60,34.84,-23.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.20,34.74,-22.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.66,34.74,-17.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.85,40.40,-16.75>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.67,39.91,-18.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.42,40.27,-22.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-44.93,34.83,-22.96>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.81,34.45,-23.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.45,34.03,-17.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.36,39.45,-17.81>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.13,39.79,-17.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.49,39.95,-23.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.44,34.52,-22.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.55,33.94,-16.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.86,34.33,-17.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.81,39.93,-17.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.39,39.54,-22.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.98,34.56,-23.08>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-16.87,34.56,-23.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.64,34.60,-17.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.10,34.31,-17.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.08,39.92,-17.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.09,40.14,-17.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.80,40.31,-23.25>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-23.06,40.06,-23.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.81,34.33,-22.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.38,33.89,-17.31>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.62,34.17,-16.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.93,39.87,-17.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.05,39.81,-22.89>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.46,39.54,-22.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.38,33.92,-11.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.72,33.93,-5.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.17,34.20,-5.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.09,39.71,-5.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.91,39.84,-5.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.27,39.28,-11.40>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.15,39.72,-11.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.05,34.30,-11.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.11,33.99,-5.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.07,39.44,-5.59>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.52,39.49,-4.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.03,39.58,-11.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.96,34.40,-11.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.73,34.27,-11.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.80,34.17,-5.92>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.72,34.36,-5.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.47,40.07,-6.40>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <29.04,39.86,-6.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.51,40.00,-11.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.85,35.33,-11.42>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.99,34.85,-11.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.12,34.32,-6.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.90,39.35,-5.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.49,40.74,-11.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.32,34.64,-11.76>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.75,34.28,-11.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.54,34.12,-6.01>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.34,34.09,-5.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.30,39.36,-5.92>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.03,39.85,-5.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.33,39.87,-11.77>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.65,40.17,-11.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.46,34.31,-11.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.18,33.66,-5.90>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.25,34.21,-4.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.27,39.59,-6.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.32,39.71,-5.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.91,39.48,-11.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.33,39.69,-11.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.90,34.38,-11.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.68,34.08,-6.07>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.13,34.37,-5.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.16,39.93,-5.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.58,40.22,-11.92>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.89,39.68,-11.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.36,34.01,-11.40>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.47,34.18,-11.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.76,33.80,-5.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.42,34.32,-5.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.81,39.75,-5.99>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.81,39.95,-5.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.97,39.77,-11.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.61,39.75,-11.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.52,-45.63,.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.02,45.40,5.80>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.10,45.55,5.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.11,-40.10,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.83,-39.95,5.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.81,-39.79,-.11>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.04,-40.13,-.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.33,45.53,-.07>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.18,45.41,.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.61,-45.63,5.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.50,-40.11,5.36>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.02,-40.05,5.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.14,-40.32,-.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.46,-39.77,.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.24,45.18,-.20>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.85,-45.61,-.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.46,45.57,5.74>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.80,45.53,5.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.85,-40.05,5.21>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.59,-39.93,5.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.63,-39.85,-.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,-39.91,-.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.64,45.41,-.49>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <40.11,-45.60,-.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.26,45.35,5.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.16,45.56,5.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.46,-40.21,5.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.17,-39.90,5.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.03,-39.49,-.52>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.32,-39.91,-.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.57,45.05,-.38>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.91,45.42,.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.47,-45.53,5.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.58,-39.64,5.67>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.14,-40.18,5.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.94,-40.19,-.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.61,-39.98,.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.26,45.14,.67>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.45,45.57,.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.08,45.25,5.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.37,-40.68,5.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.52,-39.97,5.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.52,-40.00,-.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.18,-40.04,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.75,-45.60,.34>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.09,-45.61,-.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.57,45.57,5.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.05,-45.61,5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.82,-39.55,5.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.93,-39.79,5.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.20,-39.59,-.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.82,-39.90,-.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.17,45.48,.35>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.61,45.52,.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.89,-45.62,5.72>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.30,-45.57,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.48,-40.18,5.48>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.83,-39.91,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.83,-40.16,-.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.40,-39.82,.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.36,45.52,11.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.66,45.52,11.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.85,45.37,17.40>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.06,-45.58,17.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.02,-39.35,17.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.73,-39.88,17.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.76,-39.96,11.60>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.04,-39.80,11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.59,45.16,11.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.22,45.33,11.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.50,-45.52,16.79>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.57,-45.65,17.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.67,-39.51,17.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.29,-39.89,17.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.25,-40.36,11.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.47,-40.00,11.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.14,45.48,11.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.68,-45.62,11.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.87,-45.61,17.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.38,-39.71,17.13>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.87,-40.03,17.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.72,-39.87,11.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.96,-39.80,11.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.70,45.47,11.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.24,45.38,11.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.51,-45.41,17.17>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.27,-45.60,17.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.67,-39.97,17.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.08,-40.24,11.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.40,-39.93,11.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.11,45.59,10.96>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.86,45.60,11.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.16,45.05,16.59>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.44,45.42,16.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.16,-40.24,17.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.05,-39.91,17.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.99,-39.91,11.59>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.61,-39.82,11.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.71,45.50,11.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.47,45.11,16.75>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.23,45.31,17.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.48,-40.44,17.34>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.40,-40.13,17.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.63,-40.17,11.48>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.30,-40.16,11.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.30,-45.60,11.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.66,45.48,16.90>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.77,-45.63,15.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.96,-40.56,17.13>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.29,-39.90,17.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.35,-39.70,11.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.89,-39.86,11.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.05,-45.60,11.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.98,-45.47,17.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.71,45.36,17.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.67,-40.34,17.07>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.82,-39.67,17.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.84,-39.70,11.52>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.51,-40.23,11.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.19,-45.33,22.99>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.72,-45.55,23.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.84,-45.57,28.75>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.20,45.49,28.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.27,-39.83,29.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.96,-39.90,28.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.80,-39.43,23.30>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.02,-39.58,23.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.66,-45.54,23.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.32,45.61,22.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.55,45.48,28.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.51,-45.50,28.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.76,-39.93,28.89>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.21,-39.87,28.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.44,-39.44,23.06>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.56,-39.71,23.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.51,-45.48,22.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.90,45.59,22.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.25,45.45,28.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.49,-39.90,28.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <29.02,-39.99,29.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.30,-40.12,23.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <23.16,-39.67,22.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.52,-45.35,23.17>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.88,-45.35,22.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.55,45.59,28.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <33.99,45.65,28.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.53,-39.82,28.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.64,-40.01,22.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.29,45.55,22.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.73,-45.54,22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.85,45.52,28.56>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.62,45.62,28.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <44.95,-40.03,28.73>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.05,-39.90,28.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.43,-39.62,23.17>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <45.50,-39.89,22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.75,45.11,22.35>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.47,45.15,22.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.25,-45.43,28.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.76,-39.85,28.98>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.83,-40.30,28.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.87,-40.77,23.19>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.56,-40.35,23.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.65,45.24,22.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.79,45.63,28.23>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-23.00,-45.61,27.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.15,-40.12,28.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.22,-39.93,28.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.42,-40.29,22.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.01,-40.23,22.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-12.04,45.36,22.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.89,-45.64,22.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.89,45.42,29.06>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.58,45.59,28.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.38,-39.69,28.85>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.66,-39.76,28.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.74,-39.45,23.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.51,-39.91,22.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.12,-45.47,34.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.81,-45.46,34.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.37,-45.19,40.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.37,-45.19,39.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.54,-39.63,40.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.79,-39.43,34.62>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.07,-39.70,34.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.99,-45.65,34.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.30,-45.51,39.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.34,-45.65,39.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.49,-39.81,40.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.32,-39.87,40.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.08,-39.77,34.67>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.43,-39.98,34.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.62,45.11,34.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.47,45.64,34.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.00,-45.44,40.20>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.96,45.56,39.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <27.72,-40.16,40.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.30,-40.45,35.23>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.86,-40.10,34.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.02,-45.17,34.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.84,45.57,34.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.29,-45.30,40.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.78,-40.25,40.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.55,-39.93,34.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <33.98,-39.69,34.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.59,45.20,34.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.89,45.65,34.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.30,45.20,40.10>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <45.62,45.49,40.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.36,-39.91,40.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.77,-40.16,39.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.91,-40.16,34.54>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.62,-40.04,34.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.86,-45.48,33.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.28,-45.62,33.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.52,-45.58,39.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.34,45.63,39.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.29,-40.23,40.26>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.55,-39.89,39.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.56,-40.31,34.74>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.25,-39.77,34.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.19,-45.49,34.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.94,-45.46,39.81>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.80,-45.63,39.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.87,-39.80,40.06>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.20,-39.72,40.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.10,-39.45,34.48>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.95,-40.05,34.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.72,45.53,34.32>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.85,-45.62,34.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.22,45.62,40.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-10.69,-39.88,40.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.34,-39.64,40.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.53,-39.26,35.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.27,-39.95,34.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.50,-45.06,-45.58>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.56,-45.62,-45.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.43,-45.44,-39.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.35,-45.30,-39.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.72,-39.81,-39.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.58,-39.82,-45.52>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.18,-39.74,-44.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.36,-45.44,45.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.20,-45.55,-45.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.39,-45.32,-39.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-45.50,-39.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.19,-39.75,-39.90>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.14,-39.76,-40.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.52,-39.80,-45.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.31,-45.18,-45.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.86,-45.42,-45.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.15,45.63,-39.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.22,-39.80,-40.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.69,-40.20,-40.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <21.85,-39.88,45.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.49,-45.20,-45.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.83,45.63,45.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.28,-45.42,-39.75>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.08,45.63,-39.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.72,-40.03,-39.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.22,-39.90,-39.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.50,-40.29,-45.14>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.30,-40.01,-45.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.50,45.14,-44.88>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.01,45.36,45.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.52,-45.65,-39.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.06,-40.44,-39.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.94,-40.14,-44.96>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.52,-39.99,-45.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.33,45.42,45.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.44,-45.65,45.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.91,45.25,-40.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.59,45.43,-40.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.91,-40.46,-39.85>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.74,-39.89,-40.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.95,-40.06,-45.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.04,45.34,45.20>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.13,45.58,-45.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.87,45.47,-39.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.89,45.53,-40.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.94,-39.81,-40.01>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.95,-39.81,-39.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.02,-40.03,-45.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.96,-40.04,45.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.73,45.18,45.59>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-6.03,-45.31,-45.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.31,-45.38,-40.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.83,-40.15,-40.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.17,-39.87,-45.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.24,-45.46,-33.67>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.89,-45.53,-33.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.02,45.62,-28.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.77,-40.11,-28.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.84,-39.57,-34.48>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.17,-39.86,-33.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.91,-45.64,-33.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.62,45.57,-33.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.55,-45.58,-28.40>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.61,-45.56,-28.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.58,-39.97,-28.58>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.07,-40.05,-28.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.81,-39.84,-34.59>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,-39.92,-34.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.88,45.46,-34.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <29.02,-43.83,-34.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.11,-44.82,-28.88>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.05,-45.48,-28.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.06,-39.68,-28.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.27,-40.05,-34.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.70,-45.54,-33.90>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.40,-45.43,-34.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.49,-45.28,-28.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.71,-40.52,-28.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.08,-39.98,-33.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.35,45.44,-34.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.71,45.03,-28.97>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.20,45.23,-29.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.23,-40.65,-29.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.93,-40.00,-28.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.55,-40.26,-34.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.96,44.88,-34.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.74,-45.61,-34.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.57,45.37,-28.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.45,-45.61,-28.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.60,-39.67,-28.84>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.69,-40.01,-28.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.03,-40.04,-34.69>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.41,-39.96,-34.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.09,45.05,-34.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.13,45.34,-33.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.80,45.38,-28.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.78,-40.34,-28.31>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.04,-40.17,-28.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.16,-40.31,-33.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.03,-40.03,-34.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.45,45.06,-34.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.61,45.42,-34.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.88,45.40,-28.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.63,45.61,-28.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.09,-39.82,-28.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-40.04,-28.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.35,-40.13,-35.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.35,-40.05,-34.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.11,45.51,-23.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.90,-45.51,-17.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.04,45.58,-17.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.04,-39.95,-17.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.84,-39.76,-17.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.66,-40.30,-23.30>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.16,-39.90,-23.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.26,45.39,-22.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <18.02,45.42,-17.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.80,45.58,-17.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.89,-39.81,-16.96>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.58,-39.83,-17.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.37,-40.45,-23.02>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.48,-40.11,-23.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.60,45.38,-23.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.82,45.57,-23.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.60,45.58,-17.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.48,-40.33,-17.32>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.54,-39.14,-16.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.84,-40.18,-23.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <23.07,-40.05,-22.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.47,45.48,-23.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.56,45.05,-18.54>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.19,45.39,-18.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.78,-40.61,-17.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.90,-40.22,-17.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.17,-40.75,-23.26>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.55,-40.41,-23.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.46,45.59,-22.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.66,45.21,-17.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.11,45.43,-16.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.30,-40.50,-17.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.04,-39.60,-17.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.96,-40.07,-23.32>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.58,-40.25,-23.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-35.01,44.78,-22.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.58,-45.60,-22.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.87,45.23,-17.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.32,45.21,-17.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.57,-40.51,-17.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.50,-40.08,-17.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.35,-40.21,-23.07>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.29,-40.16,-23.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.22,-45.55,-22.82>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.34,45.56,-23.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.49,-45.31,-17.92>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.88,-45.54,-17.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.67,-39.73,-16.59>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.24,-40.03,-18.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.89,-40.00,-22.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.51,45.03,-23.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.57,45.59,-22.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.93,45.58,-17.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.64,45.31,-17.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.85,-40.29,-17.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.80,-39.89,-17.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.14,-40.17,-23.19>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.52,-40.16,-23.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.16,45.36,-11.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.94,45.58,-11.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.02,45.64,-5.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.14,-45.61,-5.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.28,-39.75,-5.87>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.01,-39.81,-5.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.78,-39.72,-11.68>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.05,-39.91,-11.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.99,45.27,-11.74>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.60,45.34,-11.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.47,44.94,-5.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.67,-45.48,-5.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.33,-40.03,-5.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.88,-40.14,-11.30>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.59,-39.99,-11.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <24.14,45.59,-11.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <29.41,-45.54,-11.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.01,45.39,-6.38>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.23,-45.43,-5.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.49,-40.14,-6.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.33,-39.98,-11.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.32,42.93,-12.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.83,44.34,-7.08>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.45,45.30,-5.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.12,-40.40,-6.30>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.02,-39.76,-6.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <35.62,-40.52,-11.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.31,45.47,-11.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.25,45.49,-5.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.37,45.42,-5.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.60,-40.17,-5.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.85,-39.91,-5.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.27,-39.78,-11.81>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.46,-39.95,-11.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.75,45.37,-11.20>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.66,45.41,-11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.56,45.21,-5.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.33,45.61,-5.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.94,-39.80,-5.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.25,-39.80,-5.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.42,-40.23,-11.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.19,-39.77,-11.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.05,45.11,-11.15>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.21,-45.56,-11.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.41,-45.65,-5.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.92,-44.88,-5.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.28,-39.79,-6.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.59,-39.97,-11.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.60,45.52,-11.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.66,-45.51,-11.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.68,-45.42,-5.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.47,45.58,-5.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.69,-39.74,-5.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.66,-39.69,-5.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.09,-39.74,-11.65>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.48,-39.89,-11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.19,-34.33,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.88,-34.04,-.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.62,-34.05,5.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.24,-34.20,5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.41,-28.12,6.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.63,-28.41,5.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.83,-28.24,-.31>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.09,-28.46,-.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.88,-34.09,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.19,-34.05,6.01>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.38,-34.31,5.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.47,-28.30,5.87>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.18,-28.34,5.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.03,-28.39,-.34>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.42,-28.52,-.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.51,-33.89,-.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.52,-33.99,-.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.68,-34.03,5.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.87,-34.11,5.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.03,-27.80,5.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.80,-28.18,5.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.95,-27.74,-.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.71,-28.21,-.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.36,-33.83,-.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.05,-33.77,-.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.12,-34.04,5.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.38,-34.00,5.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.88,-28.16,5.90>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <40.19,-28.38,5.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.56,-28.09,.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.45,-33.99,.13>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.77,-34.20,-.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.80,-34.16,5.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.47,-34.12,5.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.02,-28.44,5.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.95,-28.32,.14>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.37,-28.28,.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.83,-34.02,-.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.17,-34.10,-.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.51,-34.36,5.56>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-33.98,-34.34,5.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.44,-28.25,5.85>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.35,-28.62,5.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.52,-28.30,.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.06,-28.31,-.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.83,-34.04,.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.32,-34.01,5.21>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.82,-33.98,5.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.27,-28.62,5.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.56,-28.31,.04>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.87,-28.62,.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.86,-34.23,.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.98,-33.88,5.60>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.50,-34.25,5.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-12.05,-28.68,5.59>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-6.00,-28.42,5.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.03,-28.51,.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.63,-28.59,.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.20,-33.65,11.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-34.13,11.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.82,-33.62,17.48>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.12,-33.77,17.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.10,-28.17,17.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.45,-28.14,11.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.29,-28.06,11.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.57,-34.14,11.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.29,-34.23,11.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.63,-33.91,17.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.27,-28.48,17.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.23,-28.23,11.76>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.39,-28.41,11.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.09,-33.90,11.64>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-34.05,11.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.82,-34.06,17.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.75,-33.99,17.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.64,-28.04,17.33>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.75,-28.31,17.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.00,-28.02,11.80>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <23.02,-28.42,11.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.80,-34.00,11.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.31,-34.10,11.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.46,-34.28,17.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.23,-28.55,17.50>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.95,-28.56,16.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.61,-28.37,10.98>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.65,-28.33,11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.05,-33.96,11.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.35,-34.26,17.09>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.65,-34.10,16.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.34,-28.43,16.56>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.27,-28.62,17.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.47,-28.59,11.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.51,-34.04,11.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.39,-34.21,11.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.56,-34.32,16.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.34,-34.26,17.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.85,-28.26,16.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.60,-28.53,16.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.35,-28.46,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.02,-34.33,10.91>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.23,-33.98,11.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.73,-34.13,17.03>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.63,-34.44,16.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.80,-28.74,16.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.95,-28.59,16.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.81,-28.61,11.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.69,-34.49,10.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.80,-33.98,11.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.27,-34.27,17.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.02,-28.50,17.40>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.40,-28.74,17.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.20,-28.45,11.87>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.49,-28.61,11.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.09,-33.03,23.20>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.55,-33.76,23.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.09,-34.09,28.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.30,-28.79,28.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <4.83,-27.55,23.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.59,-27.97,23.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.40,-33.44,23.07>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.20,-33.85,22.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.95,-33.75,28.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.30,-34.18,28.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <10.72,-28.13,28.19>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.01,-28.14,28.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.30,-28.05,22.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.11,-33.43,22.92>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.75,-34.08,23.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.91,-34.25,28.89>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.04,-34.12,28.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.67,-28.02,28.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.66,-28.63,28.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,-27.84,23.11>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.00,-27.94,22.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.61,-34.02,23.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.90,-34.22,21.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.35,-34.31,29.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.99,-29.00,28.91>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.70,-28.79,28.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.16,-28.79,22.30>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.42,-28.42,22.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.30,-33.98,22.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.08,-34.24,22.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.48,-34.24,28.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <45.47,-34.19,28.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.39,-28.38,28.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.03,-28.55,28.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.63,-28.38,22.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.50,-34.54,23.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.57,-34.44,22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.68,-34.54,28.99>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.43,-34.37,28.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.32,-28.27,28.58>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.48,-28.58,28.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.57,-28.71,22.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.38,-28.62,22.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.76,-34.08,22.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.03,-34.34,22.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.93,-34.17,28.51>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.70,-34.26,28.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.46,-28.40,28.96>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.80,-28.60,28.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.79,-28.12,22.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.74,-28.50,22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.17,-33.89,22.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.48,-33.98,22.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.27,-33.93,29.10>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-34.01,28.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.95,-28.90,27.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.04,-28.25,23.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.41,-33.82,33.94>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.78,-34.12,34.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.38,-33.71,39.49>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.07,-34.35,39.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.45,-28.46,39.48>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.66,-28.37,39.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.28,-27.97,34.14>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.08,-28.50,33.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.61,-34.49,34.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.06,-34.25,34.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.32,-33.87,40.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.34,-34.14,40.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.65,-28.06,39.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.31,-28.28,40.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.85,-28.28,34.07>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.40,-28.58,34.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.15,-34.29,34.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.77,-34.47,34.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.80,-35.03,40.40>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.07,-33.19,40.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.51,-28.79,40.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.56,-28.80,34.52>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.72,-28.50,34.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.82,-34.17,34.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.64,-34.67,40.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.09,-34.56,39.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.27,-29.10,39.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.12,-28.64,39.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.69,-28.54,34.17>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.30,-27.58,34.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.51,-34.30,34.58>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.81,-34.39,34.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.77,-34.42,40.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.60,-34.13,40.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.97,-28.50,40.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.99,-28.63,34.20>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.50,-28.46,34.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.54,-34.52,34.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.02,-34.42,39.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-28.70,40.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.15,-28.95,40.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.05,-28.91,34.70>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.16,-28.56,34.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.07,-34.39,34.46>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.20,-33.91,34.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.38,-33.72,39.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.84,-34.21,39.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.17,-28.41,39.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.34,-28.05,34.33>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.52,-28.68,34.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.14,-33.82,34.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.44,-33.94,34.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.43,-34.06,40.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.79,-28.68,40.04>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.91,-28.65,39.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.15,-28.26,34.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.10,-34.26,45.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.92,-34.30,45.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.08,-33.72,-40.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.10,-34.17,-40.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.28,-28.67,-40.33>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.82,-28.23,-39.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.58,-28.12,45.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.18,-28.41,45.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.22,-34.25,-45.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.81,-34.21,-40.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.25,-28.15,-40.15>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.02,-28.59,-39.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.44,-28.96,45.65>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.46,-28.56,-45.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.46,-34.37,45.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.88,-33.83,45.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.91,-34.82,-40.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.67,-34.20,-39.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.71,-28.13,-40.01>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.61,-28.60,-40.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.82,-28.56,45.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.82,-34.56,-45.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.41,-34.48,-40.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.43,-29.06,-39.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.13,-28.96,-39.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.91,-28.92,-45.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.17,-28.95,45.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.07,-34.64,-45.05>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.92,-34.22,-45.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-44.93,-34.65,-39.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.08,-29.18,-39.97>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.65,-27.94,-39.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.53,-28.57,-45.23>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.40,-28.85,45.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.22,-34.46,45.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.45,-33.87,-45.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.99,-34.54,-40.47>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.32,-34.36,-39.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.08,-29.03,-39.51>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.47,-28.72,-39.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.09,-28.52,-45.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.20,-34.67,45.28>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.07,-34.18,45.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.31,-33.86,-40.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.05,-34.04,-40.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.74,-28.26,-39.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.89,-28.38,-45.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.71,-28.79,-45.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.37,-34.51,-45.57>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.83,-34.34,45.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.81,-34.66,-40.74>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.42,-32.93,-40.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.59,-28.37,-40.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.16,-28.53,45.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.33,-34.20,-34.67>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.98,-34.19,-34.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.03,-34.55,-28.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.10,-34.33,-28.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.01,-28.50,-28.70>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.89,-28.47,-28.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.32,-28.47,-34.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.04,-34.13,-34.81>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.63,-34.14,-34.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.99,-34.54,-28.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.75,-34.35,-28.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.93,-28.78,-28.86>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.35,-28.85,-28.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.44,-28.41,-34.52>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.55,-28.57,-34.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.78,-33.70,-34.30>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <29.20,-34.21,-34.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.15,-35.35,-28.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.43,-29.09,-28.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.04,-28.65,-34.57>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.00,-28.26,-34.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.62,-34.72,-34.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.65,-34.97,-29.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.02,-34.75,-28.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.26,-29.04,-28.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.05,-28.77,-28.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.38,-28.52,-34.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <44.83,-35.08,-34.70>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.21,-34.65,-34.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.29,-34.73,-28.56>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.63,-34.35,-28.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.48,-28.91,-28.30>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.04,-28.89,-28.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.42,-29.12,-33.90>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.32,-28.80,-33.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.95,-34.55,-34.34>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.86,-34.28,-34.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.01,-34.30,-28.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.57,-34.18,-28.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.73,-28.50,-28.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.72,-28.36,-34.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.36,-28.68,-34.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.18,-34.33,-34.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.14,-34.25,-34.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.29,-34.22,-28.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.96,-34.24,-28.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.75,-28.27,-28.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.02,-28.41,-28.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.80,-28.36,-34.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.80,-28.44,-34.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.43,-34.29,-34.87>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.58,-34.29,-34.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.65,-34.26,-28.67>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.39,-34.30,-28.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.80,-28.50,-28.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.40,-28.45,-34.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.22,-28.54,-33.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.04,-34.52,-23.10>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.66,-34.32,-22.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.16,-34.14,-17.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.07,-28.29,-16.96>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.78,-28.72,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.72,-28.53,-23.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.07,-28.58,-22.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.65,-34.27,-22.87>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.34,-34.25,-22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.36,-34.19,-17.37>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.35,-34.20,-17.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.73,-28.17,-17.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.33,-28.56,-16.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.46,-28.27,-22.75>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.56,-28.38,-23.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.43,-34.27,-23.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.83,-34.19,-22.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,-34.32,-17.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.45,-28.48,-17.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.93,-28.47,-17.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.01,-29.00,-23.18>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.16,-28.28,-23.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.56,-34.93,-23.44>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.26,-34.34,-22.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.31,-34.88,-17.51>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.64,-34.38,-17.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.77,-28.95,-16.91>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <40.08,-28.78,-17.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.35,-28.65,-22.87>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.33,-28.77,-22.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.63,-34.99,-23.22>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.17,-34.25,-22.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.39,-34.35,-17.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.27,-28.54,-16.74>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.20,-28.12,-13.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.58,-28.66,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.75,-34.51,-22.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.52,-34.17,-22.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.71,-34.29,-17.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.47,-34.39,-16.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-35.00,-28.71,-16.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.84,-28.42,-17.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.08,-28.59,-22.79>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.46,-28.56,-22.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.11,-33.99,-22.60>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.28,-34.27,-22.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.65,-34.09,-17.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.16,-28.10,-16.90>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.23,-28.69,-17.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.46,-28.62,-22.51>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.89,-28.52,-22.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.65,-34.42,-22.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.63,-34.29,-22.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.82,-33.94,-16.92>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.62,-34.26,-17.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.63,-28.41,-17.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.73,-28.28,-17.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.92,-28.66,-23.01>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.42,-28.56,-23.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.13,-34.08,-11.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.70,-33.99,-11.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.35,-33.64,-6.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.32,-33.99,-5.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.14,-27.89,-5.54>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.55,-28.14,-5.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.10,-28.42,-11.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.68,-34.07,-11.21>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.30,-34.16,-11.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.15,-34.41,-5.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.33,-34.31,-5.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.36,-28.61,-5.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.12,-28.40,-5.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <10.57,-28.51,-11.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.28,-34.02,-11.84>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.84,-34.40,-11.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.82,-33.97,-6.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.89,-34.51,-5.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.03,-28.55,-6.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.74,-28.29,-5.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.94,-28.38,-11.82>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.68,-28.37,-11.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <35.10,-34.88,-11.82>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.38,-33.38,-11.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.36,-33.52,-6.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.42,-34.21,-5.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.92,-27.74,-5.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.37,-27.91,-6.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.50,-28.68,-11.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.30,-34.54,-11.82>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.85,-34.16,-11.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.74,-33.74,-6.33>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.32,-34.19,-5.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.23,-28.08,-5.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.67,-28.51,-5.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.29,-28.68,-11.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.79,-34.24,-11.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.54,-34.05,-5.59>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.08,-34.04,-5.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.38,-27.82,-5.44>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.69,-28.50,-5.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.57,-28.18,-11.09>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.22,-28.54,-11.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.15,-34.21,-10.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.15,-34.38,-11.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.01,-33.71,-6.06>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.94,-34.35,-5.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.01,-28.53,-4.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.20,-28.14,-5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.52,-28.49,-11.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.73,-33.59,-11.15>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.79,-34.19,-11.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.30,-34.01,-5.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.55,-27.78,-5.64>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.60,-28.48,-5.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.83,-28.01,-11.46>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.39,-28.06,-11.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.01,-22.58,.50>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.81,-22.76,-.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.82,-22.52,5.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.15,-22.53,6.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.05,-16.64,5.27>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.67,-16.91,5.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.06,-17.14,-.11>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.15,-16.93,-.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.75,-22.74,.03>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.20,-22.85,.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.59,-22.67,5.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.73,-16.60,5.44>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.41,-17.16,5.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.82,-16.95,.01>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.46,-17.12,-.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.43,-22.25,-.22>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.81,-22.22,-.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.49,-22.03,5.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.80,-22.29,5.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.86,-16.49,5.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.33,-16.92,.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.05,-22.52,-.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.25,-22.25,4.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.44,-22.63,5.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.63,-16.80,5.18>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <40.35,-16.73,5.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.53,-16.50,-.28>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.30,-16.22,-.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.23,-22.07,-.34>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-39.72,-22.85,-.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.14,-22.55,4.96>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.38,-23.47,5.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.21,-17.30,5.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.15,-16.71,-.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.60,-16.84,-.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-22.36,.40>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.31,-22.88,-.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.80,-22.91,5.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.36,-22.62,5.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.78,-17.10,5.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.87,-16.80,-.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.27,-17.03,.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.15,-22.78,-.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.01,-22.68,5.64>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,-23.10,5.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.09,-17.53,5.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.11,-17.08,5.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.28,-16.93,-.14>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.72,-17.39,-.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.77,-22.70,-.36>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.79,-22.78,.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.18,-23.00,5.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.33,-17.18,5.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.55,-17.22,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.78,-16.99,-.58>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.55,-17.14,-.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.50,-22.54,11.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.48,-23.00,17.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.01,-17.85,17.04>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.56,-17.46,17.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.50,-16.91,11.62>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.08,-17.45,11.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.17,-22.39,11.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.15,-22.65,11.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.16,-22.81,17.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.35,-22.97,17.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.42,-17.33,17.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.08,-16.70,17.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.93,-16.43,11.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.16,-16.77,11.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.77,-22.26,11.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.65,-22.46,11.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.04,-22.69,17.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.00,-16.36,16.80>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.61,-17.30,17.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.59,-16.29,10.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.79,-16.63,11.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.39,-22.38,11.20>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.12,-22.79,11.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.45,-23.12,16.85>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.20,-22.92,17.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.38,-16.38,16.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.00,-17.11,16.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.31,-16.73,10.84>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.38,-16.84,10.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.26,-22.91,10.73>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.62,-23.35,11.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.36,-23.00,16.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.37,-22.79,16.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.50,-16.44,16.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.77,-17.07,16.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.36,-17.76,10.73>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.37,-16.75,10.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.48,-22.96,11.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.51,-22.56,16.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.37,-22.57,16.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.66,-16.88,16.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.32,-17.19,11.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.11,-17.12,12.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.02,-23.15,11.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.09,-23.30,11.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.89,-22.58,16.99>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.80,-22.82,17.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.75,-17.17,16.82>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.04,-17.04,17.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.55,-17.21,11.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.44,-22.91,11.13>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.47,-22.98,11.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-4.88,-23.22,17.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.16,-22.64,17.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.33,-17.07,17.09>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.76,-17.08,17.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.03,-17.23,11.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.66,-17.15,11.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.63,-22.55,22.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.35,-23.11,28.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.22,-17.46,28.80>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.82,-17.53,28.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.21,-17.65,23.31>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.10,-17.67,22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.90,-23.22,22.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.48,-22.64,28.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.65,-16.58,28.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.33,-17.34,28.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.99,-17.85,22.99>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.26,-17.29,22.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.51,-22.66,23.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.73,-22.55,28.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.89,-16.80,27.85>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.36,-17.30,28.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.19,-16.87,22.35>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.89,-17.75,22.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.64,-22.50,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.68,-22.89,22.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.44,-23.20,28.23>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <33.95,-23.04,28.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.08,-17.32,28.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-17.03,28.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.68,-16.65,22.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <33.96,-17.07,22.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.22,-22.46,22.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.00,-22.95,22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.04,-22.63,28.68>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.59,-22.62,28.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.59,-16.38,28.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.77,-17.06,28.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.92,-17.37,22.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.60,-16.69,22.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.14,-22.79,22.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.36,-22.81,22.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.33,-22.63,28.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.28,-22.64,28.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.24,-16.42,28.22>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.59,-16.90,28.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-27.99,-16.93,22.58>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.15,-17.03,22.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.07,-22.69,22.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.08,-22.68,28.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.75,-22.80,27.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.74,-16.92,28.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.08,-16.99,28.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.50,-16.94,22.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.04,-22.46,22.77>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.75,-22.10,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.25,-23.66,28.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.35,-23.08,28.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.19,-17.49,28.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.62,-17.29,28.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-12.48,-17.26,22.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.16,-22.46,33.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.60,-22.68,34.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.45,-22.33,39.91>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.05,-22.67,39.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.58,-17.11,40.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.55,-16.95,35.19>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.12,-16.89,34.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.38,-22.75,33.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.06,-22.82,34.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.92,-22.30,39.56>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.40,-22.44,39.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.16,-16.99,39.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.91,-16.44,34.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.18,-16.94,34.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.98,-22.41,33.73>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.61,-22.87,33.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.17,-22.99,39.59>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.59,-23.03,39.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.60,-16.96,40.04>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.52,-17.15,39.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.35,-17.01,34.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.74,-16.88,34.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.83,-22.89,34.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.87,-22.86,39.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.19,-22.78,39.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.90,-17.08,40.03>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.81,-16.97,40.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.84,-16.43,34.31>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.19,-17.42,34.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.48,-22.66,34.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.78,-22.76,34.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.82,-22.89,40.16>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.52,-23.03,40.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.46,-17.29,40.20>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.90,-17.19,39.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.82,-16.68,34.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.64,-16.88,34.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.03,-23.07,34.18>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.37,-22.95,34.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.01,-23.47,39.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.21,-22.79,39.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.07,-16.67,39.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.27,-17.24,40.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.20,-17.20,34.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.09,-17.05,34.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.04,-22.75,34.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.41,-23.15,39.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.19,-17.83,40.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.05,-16.94,39.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.81,-16.61,34.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.49,-17.34,34.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.67,-22.41,34.11>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.85,-22.89,34.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.92,-23.55,39.64>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.31,-23.07,39.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-10.95,-17.42,39.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.32,-17.43,39.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.65,-17.42,33.90>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.34,-17.06,34.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.20,-22.86,45.14>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.44,-22.56,45.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.16,-22.82,-40.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.59,-16.90,-40.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.05,-17.43,-40.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.24,-17.31,45.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.04,-22.67,45.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <16.92,-22.99,45.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.72,-23.13,-40.30>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.38,-22.85,-39.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.65,-16.76,-40.39>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.03,-17.21,-39.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.57,-17.07,-45.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.08,-16.86,45.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.55,-22.80,45.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.42,-23.17,45.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.38,-22.96,-39.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.64,-22.57,-39.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.32,-17.05,-39.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.31,-17.28,-45.60>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.69,-17.07,-45.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.08,-23.34,45.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.95,-22.82,-45.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.30,-23.20,-39.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.25,-22.93,-39.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.97,-17.17,-39.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.05,-17.19,-39.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.85,-16.99,-45.45>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.33,-17.24,45.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.64,-23.23,-45.23>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.86,-22.91,45.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.38,-22.85,-39.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.48,-16.97,-39.47>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.08,-17.23,-39.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.01,-16.79,-45.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.55,-17.12,-45.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.15,-22.77,45.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.37,-23.02,-45.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.68,-23.13,-39.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.40,-22.98,-39.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.36,-17.13,-39.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.49,-17.02,-39.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.26,-17.10,-45.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.15,-16.96,-45.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.41,-23.51,45.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.75,-22.64,45.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.79,-22.60,-40.30>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.67,-23.05,-39.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.61,-17.42,-39.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.99,-17.08,-40.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.77,-16.82,45.38>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.54,-17.23,45.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.12,-23.31,45.27>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.58,-22.84,45.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.25,-22.62,-40.31>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.34,-23.06,-40.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.08,-17.39,-39.87>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.51,-17.06,-40.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.18,-17.45,45.42>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.30,-17.30,45.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.42,-22.49,-34.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.00,-23.02,-34.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.97,-23.07,-29.05>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.23,-22.63,-28.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.63,-17.25,-28.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.05,-17.18,-34.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.20,-16.93,-34.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.04,-22.81,-34.11>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.61,-22.94,-34.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.64,-23.20,-28.70>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.45,-23.09,-28.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.45,-17.35,-28.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.38,-17.16,-28.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.90,-16.55,-33.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.71,-17.10,-34.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.52,-22.87,-34.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.49,-23.57,-28.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.94,-21.92,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.48,-17.12,-28.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.07,-16.24,-33.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.98,-18.21,-34.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.86,-22.89,-34.13>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.94,-23.16,-33.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.72,-23.62,-28.19>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.28,-22.91,-28.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.87,-17.00,-28.30>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.96,-17.47,-28.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.91,-17.18,-33.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.16,-17.13,-33.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.24,-23.37,-33.85>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.25,-22.76,-34.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.29,-23.30,-28.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.40,-22.90,-28.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.54,-17.20,-28.08>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.06,-17.27,-28.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.89,-17.06,-33.93>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.58,-17.28,-34.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.74,-22.72,-34.11>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.65,-22.48,-34.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.45,-22.70,-28.31>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.24,-23.14,-28.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.92,-17.29,-28.11>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.36,-17.09,-28.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.37,-16.98,-34.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.88,-22.53,-34.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.05,-22.63,-34.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.99,-22.22,-28.54>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.66,-22.51,-28.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.04,-16.97,-28.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.34,-16.46,-33.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.02,-16.84,-34.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.28,-22.87,-34.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.54,-22.71,-34.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.59,-22.60,-28.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.46,-22.78,-28.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.46,-17.36,-28.23>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.60,-17.00,-28.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.57,-16.75,-34.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.35,-16.95,-34.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.49,-22.61,-23.02>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.08,-22.70,-22.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.78,-23.06,-16.74>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.12,-22.60,-17.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.22,-16.93,-17.11>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.71,-17.11,-17.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.97,-17.16,-22.89>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.15,-17.00,-22.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.13,-22.59,-22.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.55,-22.60,-16.97>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.50,-22.59,-17.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.94,-16.94,-17.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.44,-17.36,-22.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.56,-22.31,-23.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.39,-22.91,-22.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.81,-22.52,-16.67>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.13,-22.69,-17.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.74,-16.58,-17.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.55,-17.10,-17.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.60,-17.22,-22.42>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.91,-16.91,-23.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.11,-22.97,-22.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.00,-22.84,-22.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.25,-23.17,-16.82>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.21,-22.95,-16.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.47,-16.93,-16.84>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.10,-17.14,-17.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.27,-17.17,-22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.37,-23.38,-22.59>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.25,-23.18,-22.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.52,-22.74,-17.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.21,-17.35,-16.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.32,-17.49,-22.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.40,-17.35,-22.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.58,-22.72,-22.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.62,-22.76,-22.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.96,-22.43,-16.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.54,-22.82,-16.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.31,-17.02,-16.30>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.47,-16.95,-17.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.24,-16.86,-22.60>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.22,-17.03,-22.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.82,-21.88,-22.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.17,-22.89,-22.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.14,-22.88,-16.92>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.90,-22.44,-16.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.19,-17.06,-17.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.94,-16.62,-22.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.47,-22.72,-22.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.63,-22.94,-22.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.73,-22.20,-16.69>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.33,-22.68,-16.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.76,-16.82,-16.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.73,-16.98,-16.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.79,-16.95,-22.31>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.59,-17.08,-22.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.34,-22.48,-11.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.66,-22.81,-11.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.06,-22.66,-5.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.55,-17.28,-5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.30,-17.23,-11.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.08,-16.99,-11.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.65,-22.73,-11.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.33,-22.94,-11.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.23,-22.76,-5.28>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.38,-22.79,-5.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.17,-17.09,-6.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.20,-17.09,-5.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.26,-17.17,-11.68>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.29,-17.06,-11.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.44,-22.53,-11.51>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.72,-22.65,-11.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.85,-22.82,-5.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.22,-16.95,-5.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.82,-17.25,-5.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.76,-16.67,-10.98>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.03,-17.12,-11.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.99,-23.05,-11.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.30,-22.69,-5.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.91,-17.80,-5.25>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <40.09,-17.53,-5.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.32,-17.46,-11.51>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.21,-17.33,-11.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.65,-22.89,-11.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.78,-22.94,-12.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-44.35,-22.64,-5.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.23,-16.58,-5.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.76,-16.75,-6.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.40,-17.14,-11.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.52,-23.16,-10.91>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.65,-22.55,-11.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-29.27,-22.44,-5.61>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.54,-22.58,-5.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.61,-17.14,-6.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.66,-17.18,-10.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.50,-22.77,-10.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.88,-22.77,-11.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.19,-22.04,-5.62>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.73,-22.60,-5.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.78,-16.99,-6.21>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.16,-16.90,-5.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.18,-17.25,-11.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.77,-17.16,-11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.99,-22.65,-11.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.52,-23.06,-5.29>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.41,-22.37,-5.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.94,-17.31,-6.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.49,-17.40,-11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.16,-11.31,-.06>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.62,-11.57,-.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.36,-10.88,5.67>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.13,-11.31,5.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.67,-5.35,5.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.96,-5.75,-.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.03,-5.83,.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.44,-11.42,-.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <18.07,-11.50,6.09>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.76,-11.12,6.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.50,-4.98,5.84>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.36,-5.54,5.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.64,-5.90,.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.51,-5.82,-.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.83,-11.33,.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.35,-11.69,5.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.35,-6.58,5.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.61,-5.88,5.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <29.07,-6.15,-.02>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.22,-5.95,.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.01,-11.19,-.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <36.02,-11.38,5.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.32,-6.18,6.16>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <39.85,-5.67,5.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.36,-6.14,.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.24,-10.78,.03>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.10,-11.15,-.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.74,-11.54,5.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.16,-11.32,5.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.22,-5.99,6.29>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.21,-5.76,5.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.57,-5.49,.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.74,-11.19,.26>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.73,-11.20,-.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.90,-10.65,5.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.43,-11.51,5.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.53,-5.77,5.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.56,-5.28,5.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.56,-5.86,-.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.85,-11.62,-.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.07,-11.39,-.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.32,-11.13,5.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.96,-11.33,5.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.16,-5.47,6.07>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.31,-5.54,5.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.65,-5.57,.09>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-23.04,-5.72,.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.75,-11.32,-.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.59,-11.39,5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.43,-11.34,5.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.31,-5.20,5.53>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.58,-5.45,5.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.07,-5.31,.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.65,-5.59,-.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.17,-11.87,11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.86,-11.52,11.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.16,-11.88,17.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.08,-11.78,17.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.10,-6.12,16.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.95,-5.89,17.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.06,-5.28,11.33>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.05,-5.84,11.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.55,-11.13,11.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.65,-11.40,17.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.80,-5.41,17.44>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.14,-5.92,16.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.51,-4.90,11.55>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.65,-5.72,11.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.58,-10.97,11.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.99,-11.27,16.59>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <23.23,-11.33,18.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.14,-6.03,16.81>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.79,-5.60,16.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <27.96,-5.04,11.11>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.75,-6.25,11.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <37.72,-11.41,10.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.61,-11.08,17.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.50,-5.92,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.63,-5.89,11.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <33.86,-6.06,11.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.05,-11.68,11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.62,-11.15,17.51>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.31,-11.09,17.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.02,-5.62,17.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.88,-6.00,11.42>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.46,-5.95,11.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.71,-11.88,11.73>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.77,-11.24,11.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.89,-10.79,17.35>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.26,-11.26,17.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.70,-5.86,17.42>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.60,-5.47,16.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.19,-5.81,11.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.21,-11.17,11.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.22,-12.13,11.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.26,-11.26,16.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.97,-11.21,17.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.19,-5.53,17.35>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.28,-5.56,17.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.13,-5.63,11.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.76,-11.34,11.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.75,-11.32,11.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.59,-11.65,17.47>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.33,-11.52,17.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.72,-5.66,17.21>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.73,-5.82,17.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.91,-5.70,11.72>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.46,-5.60,11.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.10,-12.13,23.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.82,-11.87,23.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.06,-11.66,29.20>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.09,-11.48,28.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.37,-5.82,29.09>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.79,-5.80,28.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.51,-6.18,23.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.11,-5.96,22.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.00,-11.36,22.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.51,-11.53,22.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.56,-11.78,28.46>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.71,-11.06,28.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.95,-5.94,28.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.21,-5.67,23.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.50,-5.56,22.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.64,-11.64,22.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.52,-11.75,28.11>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.82,-11.69,29.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.63,-6.06,29.02>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.55,-5.86,28.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.81,-5.77,22.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.91,-5.82,22.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.06,-11.23,22.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.92,-11.27,28.19>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.09,-11.53,28.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.67,-5.61,27.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.21,-5.84,28.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.53,-6.01,22.99>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.46,-5.75,22.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.26,-11.24,23.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.44,-10.87,27.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.13,-5.82,28.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.42,-5.73,23.68>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.63,-5.96,22.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.73,-11.24,23.23>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.47,-11.29,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.02,-11.33,28.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-27.63,-5.75,28.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.67,-5.43,22.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.49,-5.62,22.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.91,-11.43,23.05>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.29,-11.31,22.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.51,-10.94,28.85>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.12,-11.22,28.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.29,-5.39,28.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.99,-5.73,22.84>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.80,-5.67,22.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.51,-11.48,23.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.67,-10.89,22.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.68,-11.49,28.85>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.48,-11.45,28.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.62,-5.48,28.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.61,-5.68,28.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.46,-5.71,22.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.72,-11.54,34.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.71,-10.87,40.40>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.40,-11.52,39.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.09,-5.56,40.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.55,-5.72,35.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.27,-5.87,34.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.71,-11.26,34.96>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.09,-11.17,34.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.73,-11.75,40.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <15.32,-5.88,39.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.35,-5.79,34.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.24,-11.43,34.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.26,-11.21,39.85>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.78,-11.48,39.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.21,-5.87,39.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.28,-5.47,34.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.56,-5.57,34.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.83,-11.57,33.97>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.85,-11.04,34.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.30,-11.59,39.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.00,-5.69,39.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.86,-5.11,39.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.26,-5.76,34.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.49,-10.59,33.58>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.78,-11.09,33.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.25,-10.94,39.59>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.39,-11.33,39.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.60,-5.36,39.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.90,-5.56,40.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.46,-5.28,33.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.16,-11.15,34.48>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.50,-11.49,34.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.21,-11.51,39.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.19,-11.08,39.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.75,-5.75,39.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.84,-5.69,33.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.29,-5.79,33.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.42,-11.90,34.41>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.13,-11.36,34.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.53,-11.72,39.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.88,-6.09,39.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.99,-5.92,39.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.61,-5.92,34.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.45,-11.52,34.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.11,-11.78,40.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.38,-11.48,39.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-10.88,-5.82,40.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.24,-5.75,39.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.74,-5.74,33.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.49,-5.58,34.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.81,-11.50,44.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.21,-11.41,45.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.52,-11.22,-40.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.89,-5.97,-40.50>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <6.01,-5.88,-40.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.55,-5.88,-45.25>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.49,-5.62,45.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.72,-11.29,45.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.79,-11.27,-40.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.22,-11.42,-40.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.68,-5.88,-39.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.73,-6.24,-45.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.37,-11.06,45.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.35,-11.45,45.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.24,-10.80,-39.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.61,-11.79,-39.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.17,-6.28,-39.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.32,-5.32,-39.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <27.99,-5.21,-45.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.35,-5.48,45.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <33.99,-11.39,-45.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.96,-11.49,45.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.03,-10.94,-40.19>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.23,-11.29,-39.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.70,-5.47,-39.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.23,-5.51,-39.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.10,-5.38,45.07>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.08,-5.65,-45.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.57,-10.93,45.51>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.78,-11.18,-45.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.81,-11.78,-39.82>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-45.47,-11.41,-39.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.30,-5.81,-40.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.60,-5.39,45.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.11,-11.02,-45.30>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.55,-11.33,-45.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.33,-11.36,-39.80>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.21,-11.33,-39.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.70,-5.36,-40.34>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.57,-5.78,-40.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.94,-5.38,45.57>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.31,-5.72,45.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.62,-11.35,-45.34>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.98,-11.43,-45.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.78,-11.16,-39.94>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.81,-11.44,-39.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.60,-5.53,-39.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.93,-5.65,-39.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.18,-5.43,-45.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.92,-5.59,-45.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.00,-11.53,45.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.38,-11.57,-45.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.38,-11.31,-39.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-10.97,-5.83,-39.82>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.61,-5.87,-39.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.19,-5.56,-45.60>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.25,-5.70,-45.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.11,-10.85,-34.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.76,-11.32,-34.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.57,-11.62,-29.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.20,-11.43,-28.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.02,-6.01,-29.05>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.49,-5.62,-28.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.08,-5.36,-34.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.96,-11.18,-34.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.62,-10.97,-34.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.29,-11.51,-28.48>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.38,-11.49,-28.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.28,-5.65,-28.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.11,-5.76,-28.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <10.15,-5.78,-34.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <27.58,-10.84,-34.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.71,-11.34,-28.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.91,-11.69,-28.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.31,-6.06,-28.84>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.77,-5.65,-28.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.31,-5.09,-34.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.82,-11.36,-34.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.02,-11.90,-28.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.25,-11.49,-28.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.95,-5.88,-28.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.12,-5.94,-33.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.65,-11.24,-34.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.78,-11.50,-34.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.78,-11.63,-28.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-45.59,-11.46,-28.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.38,-5.35,-28.86>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-40.03,-5.69,-28.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.34,-5.30,-34.52>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.41,-5.55,-34.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.39,-11.48,-34.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.08,-11.12,-28.44>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.04,-11.44,-28.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.31,-5.58,-29.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.34,-5.49,-28.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.44,-5.81,-34.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.27,-6.29,-34.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.96,-10.85,-33.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.15,-11.00,-34.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.56,-11.51,-28.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.84,-6.33,-28.15>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.38,-5.69,-28.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.69,-5.49,-33.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.75,-11.03,-34.35>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.94,-11.15,-34.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.29,-11.18,-28.52>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.88,-11.45,-28.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.72,-5.69,-28.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.68,-5.49,-28.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.83,-5.04,-34.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.62,-5.57,-34.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.32,-11.03,-23.06>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.67,-11.54,-22.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.71,-11.39,-17.21>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.22,-11.44,-17.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.01,-5.50,-17.20>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.78,-5.57,-16.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.66,-5.61,-22.58>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.23,-5.43,-22.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.55,-11.78,-22.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.34,-11.69,-22.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.72,-11.13,-16.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.56,-11.50,-17.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.80,-6.05,-17.25>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.18,-5.74,-17.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.49,-5.93,-23.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.44,-5.76,-22.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.92,-11.25,-22.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.43,-11.43,-22.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.95,-11.05,-17.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.74,-6.67,-16.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.71,-5.60,-21.72>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.06,-5.78,-22.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.15,-11.32,-22.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <39.92,-11.74,-22.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.40,-11.26,-16.73>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <33.87,-11.15,-16.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.54,-5.57,-17.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.65,-6.00,-23.16>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <33.95,-5.71,-23.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.48,-11.50,-22.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.75,-11.48,-22.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.25,-11.77,-16.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.43,-11.68,-16.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.12,-5.83,-16.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-40.08,-5.64,-16.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.97,-5.39,-22.64>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <45.61,-5.71,-22.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-33.81,-11.08,-22.98>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.37,-11.29,-22.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.64,-10.94,-16.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.35,-11.34,-17.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.32,-5.50,-16.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.47,-5.34,-16.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.54,-5.24,-22.35>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-34.03,-5.69,-22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.65,-11.44,-22.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.88,-11.64,-16.81>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.81,-11.74,-17.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.44,-6.20,-17.44>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.88,-5.96,-17.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.53,-6.17,-23.09>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.76,-4.98,-22.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-12.27,-11.34,-22.70>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.97,-11.23,-22.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.48,-11.41,-17.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.85,-5.97,-17.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.03,-5.33,-22.61>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.57,-5.61,-22.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.06,-11.07,-11.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.71,-11.41,-11.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.70,-11.68,-5.76>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.08,-11.66,-5.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.13,-5.76,-5.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.66,-5.56,-6.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.16,-5.54,-11.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.67,-10.89,-11.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.38,-11.25,-11.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.38,-11.49,-5.37>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.54,-11.31,-5.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.52,-5.14,-5.82>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.21,-5.77,-5.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.08,-5.43,-11.52>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.56,-5.41,-11.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.17,-11.02,-11.10>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.52,-11.43,-11.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.80,-11.62,-5.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.80,-11.26,-5.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.10,-5.25,-6.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.68,-5.71,-5.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.58,-4.91,-11.80>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.96,-5.66,-11.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.37,-11.47,-10.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.10,-11.47,-11.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <40.41,-11.41,-5.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <34.51,-11.69,-5.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <34.41,-6.01,-5.90>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <40.05,-5.84,-5.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <39.97,-5.70,-10.91>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <34.17,-5.66,-11.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-40.14,-11.41,-10.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <45.65,-11.11,-5.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-45.49,-5.22,-4.96>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-39.78,-5.77,-5.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-39.74,-5.97,-10.91>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <45.65,-5.68,-11.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.86,-11.76,-11.05>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.78,-12.39,-11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.89,-10.98,-5.96>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-34.48,-11.36,-5.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.39,-5.99,-5.24>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.80,-5.45,-6.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-34.28,-5.87,-11.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.29,-11.41,-11.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.30,-11.50,-11.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.66,-11.48,-5.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.35,-5.65,-5.64>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.35,-5.70,-5.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.45,-5.63,-11.35>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.90,-5.62,-11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.82,-12.04,-11.13>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.81,-11.46,-11.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.10,-11.82,-6.11>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.72,-11.78,-5.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.86,-6.21,-6.09>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.87,-5.77,-5.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.76,-5.97,-11.45>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.53,-5.83,-11.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }

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
