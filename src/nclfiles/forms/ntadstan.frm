#HEADER#
/TITLE/ Tool Axis
/POSITION/ 50,50
/SIZE/190,250

#LABEL#
/LABEL/ Tanto Drive Surface
/POSITION/ 10,8
/SIZE/100,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Contact Height:
/POSITION/ 10,26,65,26
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -7

#CHOICEBOX#
/LABEL/ Control:
/POSITION/ 10,43
/SIZE/80,70
/TYPE/UD_DASSTRING
/CHOICES/ "Default", "Fan", "Combin"

#CHOICEBOX#
/LABEL/ Center:
/POSITION/ 100,43
/SIZE/80,70
/TYPE/UD_DASSTRING
/CHOICES/ "Off", "On", "Auto"

#CHECKBOX#
/LABEL/ Smooth Interpolation
/POSITION/ 10,60
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Degree:
/POSITION/ 10,77
/SIZE/80,14
/TYPE/ UD_SCAINT
/PREC/ 0
/RANGE/2,100
/LEN/ 3

#EDIT#
/LABEL/ Rate:
/POSITION/ 70,77
/SIZE/80,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/RANGE/0,1
/LEN/ -7

#CHECKBOX#
/LABEL/ Parelm
/POSITION/ 10,94
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Control Surface:
/POSITION/ 10,111,70,111
/SIZE/120,14
/TYPE/ UD_DASSTRING
/PREC/ 15
/LEN/ 15
/INPUT/ FORM_PICK

#EDIT#
/LABEL/ Perpto Vector  :
/POSITION/ 10,128,70,128
/SIZE/120,14
/TYPE/ UD_DASSTRING
/PREC/ 15
/LEN/ 15
/INPUT/ FORM_PICK

#CHECKBOX#
/LABEL/ Maintain perpto vector after modifiers
/POSITION/ 10,145
/SIZE/ 130,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Departure Dist:
/POSITION/ 15,181,65,181
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -7

#EDIT#
/LABEL/ Approach Dist:
/POSITION/ 15,198,65,198
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -7

#FRAME#
/TITLE/ Combin Control
/POSITION/ 10,167
/SIZE/ 170,50

#HELP#
============================
Tool Axis - Tanto DS Form
============================
This sets the parameters and modifiers for THAXIS/TANTO,DS or TLAXIS/COMBIN
commands, which describe motions with tool axis tangent to the drive surface
at a certain height.

Contact Height:
---------------
The height at which the tool will be maintained tangent to the drive surface.

Control:
--------
Fan - for "TLAXIS/TANTO,DS,height,FAN...", when the tool will approach the
check surface in a fanning motion; 
Combin - for "TLAXIS/COMBIN,...", when the tool has a fanning motion in the
middle of the move, while at the beginning and the end the motion is just
tangent to the drive surface;
Default - for all other commands.

Center:
-------
Available only for a fanning motion.
Off - specifies the fanning calculated relative to the tool end - the default
condition.
On - specifies the fanning calculated relative to the center of the tool ring.
Auto - changes the mode from "Off" to "On" if the default method is likely to
result in error.

Smooth Interpolation
--------------------
Available only for a fanning motion.
Check to enable, uncheck to disable. If the tool axis does not change much
before and/or after the current TLAXIS/FAN motion, the SMOOTH modifier
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

Parelm
------
This causes the tool axis to be maintained parallel to the isoparametric
lines on the drive surface.

Control Surface:
----------------
If selected, the tool axis will remain perpendicular to this entity instead
of the actual part surface.

Perpto Vector:
--------------
If selected, the tool axis will be maintained perpendicular to the specified
vector.

Maintain perpto vector after modifiers
--------------------------------------
Checking this box maintains the perpendicularity to the vector after all
dynamic modifications (FWD,RIGHT,GUIDE,GOUGCK) are made to the tool
axis.  If the box is unchecked, then the tool axis will be made perpendicular
to the specified vector during the initial calculations and will be changed
by the requested modification at then end, thereby losing its perpendicularity
to the vector.  If no modifiers are selected with this tool axis, then this
box is irrelevant.

Departure Dist:
---------------
A distance for the initial "TANTO,DS" move in the "TLAXIS/COMBIN" mode.

Approach Dist:
--------------
A distance for the final "TANTO,DS" move in the "TLAXIS/COMBIN" mode.
