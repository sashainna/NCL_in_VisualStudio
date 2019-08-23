#HEADER#
/TITLE/ Profile Loops
/POSITION/ 50,30
/SIZE/295,140

#FRAME#
/TITLE/ Offsets
/POSITION/ 10,12
/SIZE/ 270,45

#CHOICEBOX#
/LABEL/ Offset Type:
/POSITION/ 15,22
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "None", "Thickness","Passes"

#EDIT#
/LABEL/
/POSITION/ 115,22
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Transition Moves:
/POSITION/ 165,22
/SIZE/105,40
/TYPE/UD_DASSTRING
/CHOICES/ "Down", "Up", "Off"

#CHOICEBOX#
/LABEL/ Step Type:
/POSITION/ 15,39
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "Passes","Max-Step"

#EDIT#
/LABEL/
/POSITION/ 115,39
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#FRAME#
/TITLE/ Depths
/POSITION/ 10,62
/SIZE/ 270,45

#CHOICEBOX#
/LABEL/ Depth Type:
/POSITION/ 15,72
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "None","Top Plane","Distance","Passes"

#EDIT#
/LABEL/
/POSITION/ 115,72
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 170,72
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 215,72
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Step Type:
/POSITION/ 15,89
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Passes","Max-Step"

#EDIT#
/LABEL/
/POSITION/ 110,89
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Perform
/POSITION/ 160,89
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Offset Passes","Depth Passes"

#LABEL#
/LABEL/ first
/POSITION/ 255,89
/SIZE/ 15,14

#HELP#
=============
Profile Loops
=============
This form controls the multiple pass features of Profile Machining, including
offset curves and depths.

=======
Offsets
=======
The Offsets section controls the output of multiple passes around the profile.
Multiple passes around the profile can only be specified when the tool is "Off"
the profile.

Offset Type:
------------
Controls the type of offsets to apply to the profile motion.  'None' will take
a single pass around the profile, 'Thickness' defines the distance from the
first pass to the final pass around the profile, and 'Passes' defines the 
number of passes to take around the profile.

A value must be entered when either 'Thickness' or 'Passes' is selected.

Transition Moves:
-----------------
The transition moves between passes of a closed curve can be made with the tool 
moving directly to the next pass or by exiting and re-entering the part using
the programmed exit and entry methods.  Select "Down" if the tool should move
directly between the passes.  The ARCSLP/FILLET command can be used in this
case to create an S-shaped transitional move.  Select "Up" to have the tool
exit the current pass and enter the next pass using the programmed exit and
entry methods.  The Loop Retract Distance will be used to retract the tool
after exiting the part between passes. Select "Off" to disable the automatic
retract if the next tool entry level is higher than the retract level 
when the loop retract distance is set to 0.

Step Type:
----------
This field will only be enabled when the Offset Type is not set to 'None'.
'Passes' will use the number of passes entered in this field to calculate the
step over distance and can only be specified when the Offset Type is set to
'Thickness'.  'Max-step' specifies the (maximum) step of each offset pass.
When the Offset Type is set to 'Passes', this value is the actual step over
distance used.

======
Depths
======
The Depths section controls the output of multiple depth passes around the
profile.  Multiple depth passes around the profile can be defined when the tool
is "On" or "Off" the profile.

Depth Type:
-----------
Controls the type of depths to apply to the profile motion.  'None' will perform
at a single depth around the profile, 'Top Plane' defines a plane to use as the
top of the part.  The first pass will be the calculated depth of cut beneath
this plane.  'Distance' defines the distance from the calculated bottom of the
profile (lowest point on the profile) to the top of the part.  'Passes' defines
the number of depth passes to take around the profile.

Select
------
This button is only active when the Depth Type is set to 'Top Plane'.  Pressing
this button allows you to select the plane or planar surface to use as the top
of the part.

The color choice field specifies which color to use to highlight the top of the
part.

Step Type:
----------
This field will only be enabled when the Depth Type is not set to 'None'.
'Passes' will use the number of passes entered in this field to calculate the
depth distance and can only be specified when the Depth Type is set to
'Top Plane' or 'Distance'.  'Max-step' specifies the (maximum) step of each
depth pass.  When the Depth Type is set to 'Passes', this value is the actual
step down distance used.

Perform --- first.
------------------
This field specifies whether the 'Offset Passes' or 'Depth Passes' should be
machined first.  'Offset Passes' causes the tool to move in towards the profile
prior to moving down in depth.  'Depth Passes' causes the tool to cut each
profile loop to depth prior to advancing to the next profile offset pass.
