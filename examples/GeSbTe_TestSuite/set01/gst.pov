#version 3.6;
#include "colors.inc"
#include "textures.inc"
#include "shapes.inc"
#include "glass.inc"
#include "metals.inc"

background {color White}

plane { <0,1,0>, -5.807 pigment{color <.6,.6,.6>} finish{ reflection {.2} ambient .5 diffuse .5 metallic }}

camera { location <34.24,11.41,22.83> look_at <-3.26,-3.26,-3.26> right x*image_width/image_height/2 up y/2 }

// add lights
light_source { <0,200,400> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }
light_source { <400,200,0> color White area_light <200,0,0>, <0,0,200>, 99, 99 adaptive 1 jitter }


// add the atoms
sphere{ <-5.71,.00,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,.00,-5.71>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,.00,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <.00,-5.71,-5.71>, 1.79 pigment { color <1.00,.50,.10> } finish { phong .5 } }
sphere{ <-5.71,-5.71,-5.71>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }
sphere{ <-5.71,-5.71,.00>, 2.49 pigment { color <.70,1.00,.10> } finish { phong .5 } }
sphere{ <.00,-5.71,.00>, 2.21 pigment { color <.40,.40,1.00> } finish { phong .5 } }

// add the cell borders
cylinder { <-5.544,-5.707,-5.707> <-4.239,-5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-3.913,-5.707,-5.707> <-2.609,-5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.283,-5.707,-5.707> <-.978,-5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.652,-5.707,-5.707> <.652,-5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <.978,-5.707,-5.707> <2.283,-5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.609,-5.707,-5.707> <3.913,-5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.239,-5.707,-5.707> <5.544,-5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.544,-5.707,5.707> <-4.239,-5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-3.913,-5.707,5.707> <-2.609,-5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.283,-5.707,5.707> <-.978,-5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.652,-5.707,5.707> <.652,-5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <.978,-5.707,5.707> <2.283,-5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.609,-5.707,5.707> <3.913,-5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.239,-5.707,5.707> <5.544,-5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.544,5.707,-5.707> <-4.239,5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-3.913,5.707,-5.707> <-2.609,5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.283,5.707,-5.707> <-.978,5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.652,5.707,-5.707> <.652,5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <.978,5.707,-5.707> <2.283,5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.609,5.707,-5.707> <3.913,5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.239,5.707,-5.707> <5.544,5.707,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.544,5.707,5.707> <-4.239,5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-3.913,5.707,5.707> <-2.609,5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-2.283,5.707,5.707> <-.978,5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-.652,5.707,5.707> <.652,5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <.978,5.707,5.707> <2.283,5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <2.609,5.707,5.707> <3.913,5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <4.239,5.707,5.707> <5.544,5.707,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-5.707,-5.544> <-5.707,-5.707,-4.239>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-5.707,-3.913> <-5.707,-5.707,-2.609>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-5.707,-2.283> <-5.707,-5.707,-.978>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-5.707,-.652> <-5.707,-5.707,.652>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-5.707,.978> <-5.707,-5.707,2.283>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-5.707,2.609> <-5.707,-5.707,3.913>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-5.707,4.239> <-5.707,-5.707,5.544>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,5.707,-5.544> <-5.707,5.707,-4.239>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,5.707,-3.913> <-5.707,5.707,-2.609>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,5.707,-2.283> <-5.707,5.707,-.978>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,5.707,-.652> <-5.707,5.707,.652>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,5.707,.978> <-5.707,5.707,2.283>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,5.707,2.609> <-5.707,5.707,3.913>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,5.707,4.239> <-5.707,5.707,5.544>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-5.707,-5.544> <5.707,-5.707,-4.239>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-5.707,-3.913> <5.707,-5.707,-2.609>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-5.707,-2.283> <5.707,-5.707,-.978>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-5.707,-.652> <5.707,-5.707,.652>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-5.707,.978> <5.707,-5.707,2.283>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-5.707,2.609> <5.707,-5.707,3.913>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-5.707,4.239> <5.707,-5.707,5.544>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,5.707,-5.544> <5.707,5.707,-4.239>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,5.707,-3.913> <5.707,5.707,-2.609>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,5.707,-2.283> <5.707,5.707,-.978>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,5.707,-.652> <5.707,5.707,.652>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,5.707,.978> <5.707,5.707,2.283>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,5.707,2.609> <5.707,5.707,3.913>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,5.707,4.239> <5.707,5.707,5.544>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-5.544,-5.707> <-5.707,-4.239,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-3.913,-5.707> <-5.707,-2.609,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-2.283,-5.707> <-5.707,-.978,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-.652,-5.707> <-5.707,.652,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,.978,-5.707> <-5.707,2.283,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,2.609,-5.707> <-5.707,3.913,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,4.239,-5.707> <-5.707,5.544,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-5.544,-5.707> <5.707,-4.239,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-3.913,-5.707> <5.707,-2.609,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-2.283,-5.707> <5.707,-.978,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-.652,-5.707> <5.707,.652,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,.978,-5.707> <5.707,2.283,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,2.609,-5.707> <5.707,3.913,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,4.239,-5.707> <5.707,5.544,-5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-5.544,5.707> <-5.707,-4.239,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-3.913,5.707> <-5.707,-2.609,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-2.283,5.707> <-5.707,-.978,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,-.652,5.707> <-5.707,.652,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,.978,5.707> <-5.707,2.283,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,2.609,5.707> <-5.707,3.913,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <-5.707,4.239,5.707> <-5.707,5.544,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-5.544,5.707> <5.707,-4.239,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-3.913,5.707> <5.707,-2.609,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-2.283,5.707> <5.707,-.978,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,-.652,5.707> <5.707,.652,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,.978,5.707> <5.707,2.283,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,2.609,5.707> <5.707,3.913,5.707>, .1 pigment { color <.9,.9,.9> } }
cylinder { <5.707,4.239,5.707> <5.707,5.544,5.707>, .1 pigment { color <.9,.9,.9> } }
