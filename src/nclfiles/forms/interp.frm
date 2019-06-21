#HEADER#
/TITLE/ Tool Axis
/POSITION/ 50,50
/SIZE/150,105

#LABEL#
/LABEL/ Interpolate
/POSITION/ 10,8
/SIZE/100,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Final Vector:
/POSITION/ 10,26,70,26
/SIZE/120,14
/TYPE/ UD_DASSTRING
/PREC/ 15
/LEN/ 15
/INPUT/ FORM_PICK

#CHECKBOX#
/LABEL/ Smooth Interpolation
/POSITION/ 10,43
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Degree:
/POSITION/ 10,60
/SIZE/80,14
/TYPE/ UD_SCAINT
/PREC/ 0
/RANGE/2,100
/LEN/ 3

#EDIT#
/LABEL/ Rate:
/POSITION/ 70,60
/SIZE/80,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/RANGE/0,1
/LEN/ -7

#HELP#
============================
Tool Axis - Interpolate Form
============================
This sets the final tool axis vector and an optional smooth interpolation
mode parameters.

Final Vector:
-------------
Select a vector or point-vector to specify the final tool axis

Smooth Interpolation
--------------------
Check to enable, uncheck to disable. If the tool axis does not change much
before and/or after the current TLAXIS/INTERP motion, the SMOOTH modifier
creates a more gradual transition between the current and adjacent motions.

The smooth interpolation parameters could be used to fine tune the way the
tool axis changes with the SMOOTH modifier. 
The default values degree=5, rate=1 create a nice transition curve.

Degree
------
Sets the interpolation polynomial degree - can be any nonnegative integer.
A higher degree leads to a more gradual transition.
Setting degree to 0 or 1 effectively disables smooth interpolation.

Rate
----
A parameter defining a shape of the interpolation polynomial - can be any
positive number no more than 1. A higher rate makes for an extra smooth
transition, but a steeper change in the middle of the interpolating motion.
