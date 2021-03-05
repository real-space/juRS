#version 3.6;
#include "colors.inc"
#include "textures.inc"
#include "shapes.inc"
#include "glass.inc"
#include "metals.inc"

background {color White}

plane { <0,1,0>, -28.635 pigment{color <.6,.6,.6>} finish{ reflection {.2} ambient .5 diffuse .5 metallic }}

camera { location <171.21,57.07,114.14> look_at <.13,.10,.24> right x*image_width/image_height/2 up y/2 }

// add lights
light_source { <0,200,400> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }
light_source { <400,200,0> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }


// add the atoms
sphere{ <-.13,-.07,-.01>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.62,.16,-.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.85,.27,5.87>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.03,.05,5.60>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.07,5.97,6.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.84,6.00,5.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.65,6.37,.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.12,5.72,.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.82,.33,.19>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.37,.28,-1.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.79,.17,5.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.14,6.04,5.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.54,5.81,5.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.34,5.80,.51>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.64,6.03,.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.77,.04,.92>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.41,-.06,.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.98,.15,5.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.46,5.33,5.67>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.26,5.62,5.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.40,5.71,.34>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.80,5.51,.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.02,.21,-.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.00,-.16,-.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.25,.26,5.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.06,.54,5.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.53,6.01,5.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.36,5.40,.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.87,5.93,-1.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.33,.02,.07>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,.14,-.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.34,.21,5.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.89,6.37,5.90>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.82,5.59,5.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.98,6.10,.09>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.43,5.71,.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.08,.05,11.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <6.06,.11,11.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.25,.19,17.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.35,5.49,17.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.15,6.01,11.50>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.08,5.81,11.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.51,-.02,11.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.32,.09,16.67>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.29,-.09,17.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.08,5.41,17.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <16.99,5.85,16.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.77,5.77,11.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.52,-.44,12.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.09,-.01,16.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.70,5.84,17.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.43,5.54,17.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.16,4.82,12.07>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.60,5.22,11.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.15,.09,11.61>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.05,.05,11.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.62,.10,17.09>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.13,.06,17.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.00,5.97,17.40>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.19,5.96,17.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.79,5.82,11.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.13,6.10,11.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.63,-.02,11.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.65,.26,17.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.46,.03,17.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.20,6.11,16.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.49,5.99,17.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.02,5.57,11.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.53,4.85,11.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.11,.01,22.83>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.72,.09,22.67>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.75,-.07,28.30>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.14,.04,28.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.24,5.55,28.20>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.66,5.75,28.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.63,5.93,22.80>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.11,5.76,22.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.81,-.19,22.43>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.12,-.16,22.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.69,-.33,-28.29>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.59,-.15,28.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.65,5.11,-28.52>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.11,5.37,-28.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.42,5.45,22.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,5.57,22.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.51,-.04,22.68>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.12,.06,22.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-27.92,-.18,27.56>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.01,.08,-28.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.12,5.66,-28.45>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.39,5.53,28.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.03,5.73,22.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.17,-.01,22.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.80,-.54,-28.33>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.84,1.08,28.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.29,5.55,28.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-18.08,5.28,22.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-23.15,5.09,23.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.80,.11,22.59>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.65,.03,23.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.22,-.35,-28.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.57,5.06,-28.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.66,6.58,-28.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.04,5.84,23.09>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.67,5.69,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.41,-.17,-22.86>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.82,-.08,-22.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.86,-.19,-17.02>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.03,-.05,-17.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.04,5.85,-17.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.80,5.54,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.62,5.55,-22.88>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.12,5.79,-23.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.92,-.26,-23.11>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.25,-.51,-22.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.42,-.23,-17.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.05,5.36,-17.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.44,5.88,-17.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.37,4.87,-22.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.43,5.48,-22.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.48,-.26,-23.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.31,-.18,-17.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.94,.16,-17.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.32,5.74,-16.84>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.49,5.68,-17.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.36,5.25,-23.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.99,5.29,-22.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.01,-.12,-23.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.59,-.25,-22.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.55,-.25,-17.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.47,4.96,-17.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.42,5.17,-22.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.89,5.43,-21.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.92,-.14,-23.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-10.30,-.27,-16.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.92,4.77,-17.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.78,5.49,-17.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.13,5.24,-22.88>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.55,6.42,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.30,-.03,-11.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.74,-.18,-11.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.86,.31,-5.75>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.05,-.19,-5.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.03,5.35,-5.60>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.80,5.70,-5.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.83,5.38,-11.34>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.04,5.90,-11.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.70,-.30,-11.58>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <16.95,-.03,-10.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.25,-.01,-5.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.03,5.58,-5.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.42,5.74,-5.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.47,5.04,-11.53>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.58,5.73,-11.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.41,-.52,-11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <27.98,-.11,-5.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.57,.09,-5.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.23,5.32,-5.70>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.49,5.65,-5.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.27,4.75,-11.33>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.78,5.51,-11.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.09,-.53,-11.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.99,.26,-5.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.49,5.23,-5.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.68,4.37,-11.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.88,5.55,-12.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.92,-.20,-11.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.97,-.31,-5.66>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.40,-.29,-5.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.79,5.15,-5.63>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.79,5.54,-5.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.03,5.37,-11.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.61,5.74,-11.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.12,11.50,-.45>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.62,11.59,-1.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.18,11.56,5.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.21,17.60,5.82>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.84,17.02,5.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.81,17.40,.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.04,17.08,-.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.97,11.29,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.34,11.34,5.45>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.37,11.55,5.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.30,17.23,6.07>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.74,17.07,5.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.61,17.21,.45>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.53,16.86,.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.71,11.28,.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.48,11.63,.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.20,10.95,6.03>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.79,11.80,5.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.25,17.05,5.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.97,17.00,-.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.68,11.18,.73>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.20,11.27,.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.87,11.34,5.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.58,16.68,5.52>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.57,17.94,5.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.71,16.90,.52>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.23,16.78,-.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.76,11.41,-.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.93,11.56,5.87>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.56,11.72,5.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.95,17.88,6.06>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.79,17.40,5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.76,17.57,.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.53,16.91,.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.08,11.84,11.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.85,11.65,11.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.27,11.39,16.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.28,11.52,17.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.11,16.94,17.30>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.69,17.17,17.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.19,17.57,11.62>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.12,17.45,11.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.80,11.19,11.10>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.07,11.94,11.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.11,11.99,16.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.36,11.64,16.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.23,17.27,17.39>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.40,17.42,17.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.60,17.26,11.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.00,10.80,11.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.39,11.21,11.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.32,11.36,17.78>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.76,11.50,17.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.30,17.17,17.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <27.92,16.88,11.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.48,16.93,11.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.42,11.47,11.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.61,11.89,16.51>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-23.00,11.36,19.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.24,16.92,17.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.02,17.31,16.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.71,16.48,11.26>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.90,16.77,11.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.87,11.44,11.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.33,11.73,16.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.45,17.63,17.49>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.72,17.16,17.28>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.81,17.10,11.68>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.58,17.02,11.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.20,11.47,22.78>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.55,11.62,22.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.31,11.61,28.51>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.36,11.38,28.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,17.10,-28.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.79,17.27,-28.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.93,17.49,22.86>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.04,17.15,22.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.89,11.47,22.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.38,11.54,22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.16,11.15,-28.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.33,11.37,-28.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.81,16.94,28.40>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.22,17.05,28.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.33,17.23,22.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <26.32,11.88,22.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.10,11.49,28.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.85,11.18,28.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.38,17.03,28.18>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.37,17.16,28.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.70,16.61,22.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.22,11.25,22.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.80,11.38,28.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.68,9.97,28.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.16,17.13,28.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.77,16.98,22.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.11,17.33,22.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.52,11.62,22.74>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.60,11.55,22.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.20,11.51,-28.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.75,16.81,-28.21>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.88,16.92,28.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.67,16.90,22.93>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.28,17.34,22.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.40,11.57,-22.50>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.63,11.55,-22.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.04,11.43,-16.59>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.04,11.61,-16.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.68,17.12,-17.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.70,17.24,-22.63>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.07,17.28,-22.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.43,11.32,-22.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.16,11.23,-22.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.69,11.44,-16.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.56,16.93,-17.18>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.06,16.82,-17.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.43,16.94,-23.31>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.32,17.12,-22.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.00,10.97,-22.96>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.42,11.44,-22.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.06,11.58,-16.85>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.65,11.22,-16.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.41,17.19,-17.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.50,17.28,-23.34>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.03,17.00,-22.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.53,10.77,-23.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.88,11.18,-22.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.49,10.74,-16.93>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.92,11.43,-17.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.00,17.02,-16.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.08,16.83,-17.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.42,16.85,-22.87>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.01,16.84,-22.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.00,11.48,-22.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.91,11.27,-17.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.50,11.34,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.38,16.64,-17.25>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.78,17.21,-17.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.00,17.18,-22.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.54,17.58,-22.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.67,11.50,-11.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.14,11.46,-5.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.33,17.41,-6.09>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.85,17.21,-5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.70,17.13,-11.18>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.05,17.26,-11.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.06,11.59,-11.02>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.09,11.09,-11.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.86,11.07,-6.10>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.36,11.39,-5.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.92,17.04,-5.81>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.43,16.95,-5.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.31,16.67,-11.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.26,17.11,-11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.58,10.84,-11.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.31,11.29,-11.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.19,11.96,-5.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.62,11.43,-5.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.39,17.45,-5.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.35,17.22,-10.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.87,17.00,-12.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.88,11.30,-11.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.06,11.14,-11.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.45,10.90,-5.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.91,11.73,-5.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.42,16.99,-5.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.34,17.07,-11.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.90,17.19,-11.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.17,11.15,-11.37>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.62,11.44,-11.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.43,11.54,-5.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.48,11.26,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.70,16.85,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,17.24,-5.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.51,16.92,-11.34>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.31,16.97,-11.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.22,23.36,.34>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.63,23.09,-.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.69,23.43,5.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.19,23.21,5.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.67,-28.05,5.51>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.88,-28.19,5.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.33,-28.29,.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.36,22.56,-.20>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.07,23.04,.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.55,23.01,5.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.05,28.35,5.73>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.26,28.26,5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.17,28.44,.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.86,22.29,.76>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.46,22.69,-.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.53,22.47,5.33>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.97,22.64,5.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.43,28.52,5.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.42,28.44,-.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.71,28.39,.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.00,22.48,-.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.11,22.94,.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.89,22.62,5.66>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.96,28.29,5.74>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.96,28.48,5.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.82,28.51,-.36>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.69,28.46,-.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.37,22.58,.34>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.77,23.02,-.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.26,23.29,5.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.56,28.27,5.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.35,-28.46,.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.69,22.84,11.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.70,23.17,17.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,22.91,17.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.22,28.49,16.74>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.60,-28.46,15.65>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.12,28.44,11.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.04,22.73,11.36>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.03,22.63,11.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.37,22.95,16.75>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.63,22.82,17.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.71,-28.27,17.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.19,28.51,17.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.04,28.27,11.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.31,28.45,11.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.71,22.39,11.96>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.52,22.84,11.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.29,23.10,17.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.77,24.33,17.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.41,-28.27,17.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.45,-28.28,11.54>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.83,28.47,10.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.69,22.45,11.75>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.04,22.67,10.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.65,22.86,17.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.91,28.52,17.63>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.06,28.30,17.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.86,28.22,11.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.78,28.51,11.57>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.39,22.71,11.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.57,22.86,11.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.96,23.02,16.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.62,22.94,17.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.67,-28.31,17.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.54,28.50,10.75>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.36,28.52,12.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.10,22.70,22.43>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.65,22.88,22.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.76,23.36,-28.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.09,22.95,28.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.12,-28.29,28.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.82,-28.18,-28.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.30,28.50,22.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.71,23.27,22.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.24,22.28,22.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.50,23.17,28.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.53,22.80,-28.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.20,-28.16,28.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.72,-28.19,28.06>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <10.70,-28.33,22.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.21,22.63,22.33>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.50,22.76,22.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-27.94,23.33,28.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <23.08,22.55,28.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.25,-28.35,-28.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <27.59,-28.43,23.19>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.28,27.80,22.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.95,22.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.63,23.28,28.06>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.76,21.32,28.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.52,-28.20,-28.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.77,-28.18,22.74>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.13,-28.48,22.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.83,22.92,23.28>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.64,22.75,22.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.00,22.60,-28.46>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.68,22.73,-28.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.83,-28.53,28.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.78,-28.49,-28.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.53,-28.51,22.66>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.47,28.48,22.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.45,23.34,-22.70>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.61,22.87,-22.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.57,22.83,-16.41>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.24,22.64,-16.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.11,28.38,-16.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.70,-28.41,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.55,-28.46,-22.96>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.01,-28.22,-22.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.34,22.97,-22.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.16,22.78,-22.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.30,22.36,-17.07>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.41,22.68,-16.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.69,-28.48,-16.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.32,28.51,-17.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.60,-28.24,-23.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.36,-28.37,-22.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.03,22.73,-23.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.42,22.79,-22.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.32,22.90,-16.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <23.01,22.86,-17.26>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.28,28.32,-17.52>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-28.52,28.37,-17.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.11,-28.45,-22.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.51,22.47,-22.88>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.86,22.71,-22.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.12,22.56,-16.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.72,22.75,-17.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.45,28.51,-16.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.80,28.44,-22.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.16,28.39,-22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.07,23.09,-22.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.80,23.14,-16.63>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.33,22.67,-17.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.81,28.40,-17.19>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.84,-28.43,-17.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.38,-28.32,-22.62>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.54,-27.74,-22.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.28,22.97,-11.00>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.72,22.72,-10.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.82,23.05,-5.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.47,28.35,-5.19>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.84,28.50,-5.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.76,-28.23,-11.09>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.01,-28.43,-11.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.92,22.73,-11.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.17,22.91,-5.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.25,22.94,-5.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.51,28.36,-5.32>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.07,-28.42,-5.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.64,-28.50,-11.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.50,28.43,-11.24>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.80,22.57,-11.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.48,22.97,-10.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.90,22.52,-5.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.83,28.13,-5.65>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.48,28.32,-5.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.21,-28.46,-11.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.30,22.88,-11.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.06,22.76,-5.35>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.83,22.43,-5.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.75,27.98,-6.44>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-16.98,28.53,-5.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.21,28.14,-11.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.58,22.70,-11.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.65,22.74,-11.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.18,22.79,-5.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.07,-28.43,-5.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.45,-28.38,-5.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.02,28.41,-11.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.65,-28.51,-11.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.30,-23.23,-.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.31,-22.81,5.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.53,-17.45,5.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.72,-17.66,-.49>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.01,-17.11,.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <10.77,-23.27,.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <16.98,-23.02,-.18>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.01,-23.24,5.67>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.17,-23.10,5.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.34,-17.35,6.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.18,-17.10,5.86>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.29,-17.40,.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.43,-17.23,-.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.68,-23.02,-.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.51,-22.66,.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.37,-22.90,5.75>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.99,-23.03,5.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.16,-17.40,5.74>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.34,-17.20,5.73>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.46,-16.62,.18>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.03,-17.03,-.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.85,-22.91,.06>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.01,-22.93,.03>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.01,-23.36,5.62>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.77,-22.67,5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.78,-16.95,5.72>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.20,-17.25,5.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.21,-17.09,-.27>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.94,-16.93,.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.03,-22.63,-.08>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.41,-23.11,-.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.25,-23.29,5.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.45,-23.07,5.72>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.19,-17.33,5.69>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.48,-17.16,5.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.64,-17.54,-.09>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.37,-16.77,-.14>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.14,-23.31,11.10>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.61,-23.09,11.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.36,-23.09,17.25>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.22,-22.84,17.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.37,-17.30,16.80>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.85,-17.13,17.08>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.65,-17.59,11.28>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.10,-17.35,11.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.42,-22.76,11.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.09,-22.72,11.56>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.17,-22.97,17.93>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.51,-22.62,17.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <16.95,-17.35,17.17>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.54,-17.01,11.52>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.46,-17.02,11.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.78,-23.19,11.85>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.52,-22.50,11.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.47,-22.38,17.48>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.82,-23.26,17.39>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.56,-17.67,17.20>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.47,-16.96,17.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.90,-17.09,11.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.86,-22.57,11.92>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.06,-22.70,11.30>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.83,-23.26,17.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.92,-22.87,17.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.00,-17.02,17.34>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.18,-17.16,17.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.61,-16.94,11.59>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.92,-16.92,11.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.29,-22.76,11.55>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.50,-22.76,11.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.12,-22.94,17.25>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.36,-17.27,17.24>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.60,-17.33,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.95,-17.24,11.45>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.35,-17.00,11.44>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.09,-22.80,22.94>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.84,-23.12,22.94>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.15,-22.55,-28.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.02,-16.95,28.21>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.60,-17.36,-28.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.81,-17.58,22.81>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.03,-16.92,22.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.25,-22.76,22.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <10.42,-22.83,28.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.60,-17.19,28.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.16,-17.26,28.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.48,-17.56,22.81>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.65,-17.44,22.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <27.04,-22.70,22.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.36,-22.57,28.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.82,-23.22,28.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.85,-17.86,28.31>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.53,-16.80,28.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.23,-16.22,22.51>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.93,-16.93,22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.36,-22.52,22.91>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.69,-22.90,27.88>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.82,-17.22,28.13>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.02,-17.28,28.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.92,-15.57,22.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.81,-22.79,23.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.75,-22.62,22.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.69,-22.75,-28.43>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.62,-22.73,-28.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.42,-16.44,-28.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.57,-16.91,28.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.01,-17.07,22.84>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.48,-16.99,22.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.59,-23.00,-22.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.23,-22.88,-17.31>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.00,-22.85,-17.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.05,-17.40,-16.95>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.91,-17.13,-16.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.82,-17.26,-22.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.02,-17.33,-22.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <10.85,-22.92,-23.44>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.17,-22.63,-23.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.03,-22.67,-17.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.26,-22.63,-17.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.52,-16.98,-16.87>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.08,-16.90,-17.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.18,-16.69,-23.09>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.59,-17.33,-22.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.63,-22.73,-23.26>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.40,-22.94,-22.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.49,-23.15,-17.19>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.82,-22.74,-17.19>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.63,-17.07,-17.33>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.28,-17.01,-16.99>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.35,-16.96,-22.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.62,-17.17,-22.84>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.85,-23.07,-23.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.06,-22.92,-23.10>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.80,-22.65,-17.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.77,-23.04,-17.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.15,-16.97,-16.83>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.16,-16.94,-17.07>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.25,-17.10,-22.89>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.91,-17.17,-22.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-4.89,-23.02,-22.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.15,-22.77,-17.42>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.52,-16.45,-17.10>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.71,-17.31,-16.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.87,-17.68,-22.62>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.49,-18.05,-22.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.54,-22.66,-11.35>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.87,-23.16,-5.68>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.05,-22.97,-5.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.06,-17.38,-5.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.79,-17.30,-6.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.01,-17.45,-11.32>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.09,-22.65,-11.58>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.27,-22.77,-11.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.42,-22.90,-5.68>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.49,-17.45,-5.82>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.04,-17.35,-5.80>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.18,-16.90,-11.61>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.27,-16.91,-11.45>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.91,-22.74,-11.80>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.51,-21.76,-11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.24,-23.00,-5.99>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <22.59,-22.86,-5.92>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.88,-16.82,-6.11>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <28.51,-17.01,-5.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.94,-17.12,-11.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.85,-23.30,-11.68>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.03,-23.76,-11.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.96,-22.42,-6.12>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.79,-22.89,-5.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.00,-16.84,-6.22>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.02,-16.78,-5.97>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-21.55,-17.29,-11.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.07,-22.49,-11.84>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.44,-22.69,-11.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.25,-22.84,-6.13>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.17,-22.55,-5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.52,-16.46,-5.85>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.75,-17.06,-5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.72,-17.22,-11.29>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.36,-16.90,-11.38>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.43,-11.68,-.12>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.59,-11.50,-.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.68,-11.80,5.52>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.04,-11.68,5.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.07,-5.87,5.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.91,-5.77,5.55>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.68,-5.50,-.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.07,-5.71,-.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.47,-11.39,-.39>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.26,-11.35,-.11>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.43,-10.96,5.73>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.52,-11.41,5.74>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.14,-5.45,5.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.69,-5.33,5.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.74,-5.31,-.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.55,-5.62,-.13>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.31,-10.72,-.02>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.25,-11.25,-.16>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.27,-10.71,6.04>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <23.04,-11.28,5.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.15,-5.28,5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.20,-5.26,-.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.49,-11.23,.29>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.02,-11.30,-.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.85,-11.63,5.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.65,-11.35,5.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.56,-4.93,5.79>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.35,-5.67,5.54>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.79,-5.79,-.49>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.99,-5.72,-.15>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.83,-11.48,-.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.06,-11.26,5.03>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.39,-11.44,6.63>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.74,-5.29,5.85>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.77,-5.55,5.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.27,-5.44,-.29>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.73,-5.71,-.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.12,-11.67,11.40>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.89,-11.46,11.33>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.94,-11.16,17.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.02,-11.54,17.22>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.04,-5.44,17.26>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.92,-5.44,17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.11,-5.64,11.33>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.11,-5.77,11.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.84,-10.93,11.36>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.19,-11.40,11.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.76,-11.44,17.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.61,-11.44,16.98>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.94,-6.00,17.45>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.28,-5.71,17.23>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.78,-5.21,11.75>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <11.80,-5.58,11.50>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.50,-10.73,11.56>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.22,-11.39,11.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.26,-11.35,17.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.47,-5.55,17.26>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.49,-5.94,15.81>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.09,-5.31,11.64>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-16.72,-11.52,11.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.83,-11.48,16.70>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.23,-11.36,16.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.59,-5.67,17.98>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.23,-5.67,17.36>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.87,-5.75,11.37>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.27,-5.52,11.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.82,-11.66,11.31>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.95,-11.65,17.05>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.59,-11.23,17.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.71,-5.41,17.51>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.83,-5.67,17.34>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.24,-5.80,11.50>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.57,-5.75,11.61>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.10,-11.17,22.78>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.64,-11.44,22.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.93,-11.71,-28.38>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.02,-11.31,28.41>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.08,-5.57,-28.34>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.75,-5.76,-28.47>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.18,-5.68,22.87>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.03,-5.63,22.95>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.85,-11.78,22.72>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.34,-11.46,22.90>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.52,-10.99,-28.14>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.53,-11.30,-28.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.10,-5.62,28.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.78,-5.70,22.82>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.32,-11.06,22.92>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.53,-10.95,22.52>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.42,-10.56,27.73>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.17,-11.49,-28.46>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.11,-5.55,28.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.32,-5.56,-28.21>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.46,-5.80,22.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.50,-11.28,22.79>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.35,-11.45,28.48>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-22.98,-11.26,28.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-18.79,-5.73,-28.37>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-18.27,-5.83,23.17>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-23.43,-5.52,23.27>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.94,-11.03,22.70>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-5.69,-11.32,22.85>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.86,-10.78,-28.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.55,-10.88,-28.20>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.68,-5.33,28.51>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.10,-5.57,23.04>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.52,-5.42,23.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-.44,-11.75,-22.80>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.69,-11.56,-22.58>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.18,-11.36,-16.93>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <.17,-11.32,-17.09>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.65,-16.79>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <5.70,-5.79,-17.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <6.03,-5.75,-22.76>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.03,-5.77,-22.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.67,-11.56,-22.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.35,-11.41,-22.49>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.53,-11.40,-16.89>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <12.08,-5.73,-17.54>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <17.24,-5.72,-17.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.71,-5.73,-22.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.05,-10.57,-22.95>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <28.41,-11.26,-22.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.18,-11.30,-17.31>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <22.77,-11.46,-17.40>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.46,-5.17,-17.21>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.22,-5.86,-16.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <22.58,-5.60,-22.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.88,-11.06,-22.99>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.02,-11.40,-22.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.34,-10.83,-16.47>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.03,-11.25,-16.93>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.21,-5.11,-16.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.97,-5.80,-22.34>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-23.29,-5.62,-21.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.10,-11.55,-22.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.69,-11.17,-16.57>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.45,-11.01,-17.12>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.95,-5.21,-17.52>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.72,-5.50,-16.78>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.40,-5.77,-22.36>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-11.76,-5.58,-22.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.04,-11.86,-11.31>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <5.83,-11.54,-11.59>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.30,-11.27,-5.85>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-.23,-11.24,-5.62>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.52,-5.61,-6.01>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <5.52,-6.17,-11.59>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-.04,-5.81,-11.48>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.05,-11.25,-11.77>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.29,-11.23,-6.15>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.43,-11.62,-6.04>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <11.65,-5.80,-6.18>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <17.33,-5.60,-5.96>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <17.47,-5.30,-12.32>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <11.31,-5.74,-11.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.11,-10.81,-11.77>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-28.36,-12.11,-11.43>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-28.10,-11.25,-5.98>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <23.03,-11.18,-5.87>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <28.15,-5.85,-6.02>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <23.04,-5.26,-11.53>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.18,-10.80,-11.34>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-17.25,-11.33,-11.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-22.73,-11.26,-5.75>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-23.49,-5.19,-6.17>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <-17.18,-5.19,-5.69>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-17.81,-5.87,-10.96>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-22.97,-5.52,-11.70>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.80,-10.99,-11.11>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.86,-11.43,-11.05>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.07,-11.34,-5.76>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-11.91,-5.91,-5.86>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.57,-5.65,-5.83>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-6.40,-5.92,-11.10>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-11.61,-5.73,-11.29>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }

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
