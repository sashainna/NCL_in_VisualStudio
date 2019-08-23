#HEADER#
/TITLE/ Create Bounding Box Solid
/POSITION/ 50,50
/SIZE/275,224

#FRAME#
/TITLE/ Geometry
/POSITION/ 8,10
/SIZE/ 260,66

#CHOICEBOX#
/LABEL/ Type:
/POSITION/ 15,21
/SIZE/ 70,30
/TYPE/ UD_DASSTRING
/CHOICES/ "Geometry","All","Motion"

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 92,21
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 145,21
/SIZE/65,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 215,21
/SIZE/50,15
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Include on Layer
/POSITION/ 15,38
/SIZE/70,15
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Select by Layer
/POSITION/ 92,38
/SIZE/70,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Layer
/POSITION/ 15,55
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 55,55,65,55
/SIZE/40,13
/TYPE/ UD_SCAINT
/LEN/ 10
/PREC/ 16

#PUSHBUTTON#
/LABEL/ Show Layer
/POSITION/ 130,55
/SIZE/ 45,14
/TYPE/UD_DASSTRING

#FRAME#
/TITLE/ Bounding Box
/POSITION/ 8,80
/SIZE/ 260,77

#LABEL#
/LABEL/ X
/POSITION/ 75,89

#LABEL#
/LABEL/ Y
/POSITION/ 138,89

#LABEL#
/LABEL/ Z
/POSITION/ 200,89

#DISPLAY#
/LABEL/ Lower Left:  
/POSITION/ 15,100, 60,100
/SIZE/90,14
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -12

#DISPLAY#
/LABEL/
/POSITION/ 123,100
/SIZE/50,14
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -12

#DISPLAY#
/LABEL/
/POSITION/ 185,100
/SIZE/50,14
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -12

#DISPLAY#
/LABEL/ Upper Right:
/POSITION/ 15,117, 60,117
/SIZE/90,14
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -12

#DISPLAY#
/LABEL/
/POSITION/ 123,117
/SIZE/50,14
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -12

#DISPLAY#
/LABEL/
/POSITION/ 185,117
/SIZE/50,14
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -12

#EDIT#
/LABEL/ Expansion:
/POSITION/ 15,134, 60,134
/SIZE/90,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -12

#EDIT#
/LABEL/
/POSITION/ 123,134
/SIZE/50,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -12

#EDIT#
/LABEL/
/POSITION/ 185,134
/SIZE/50,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -12

#EDIT#
/LABEL/ Solid Label:
/POSITION/ 15,164
/SIZE/40,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 16

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 15,183
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 70,183
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 200,183
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
#HELP#
=========================
Create Bounding Box Solid
=========================

This form is used to create a box solid that bounds selected geometry or
previously generated motion.

Type
----
This selection determines if the resultant box solid will enclose selected
geometry (Geometry), all defined geometry (All), or the generated motion
(Motion).

Select
------
Used to select the geometry to use for creating the bounding box solid.  If
the Type field is set to All or Motion, then pressing this button will display
the calculated box bounds in the Bounding Box fields.

Color
-----
Chooses a color to highlight the selected geometry.

Deselect All
------------
Deselects all of the currently selected geometry.

Include on Layer
----------------
If checked, the selected geometry will be put on the specified layer by
issuing the appropriate "DRAFT/MODIFY=sf2,sf3,LAYER=4" command.  This command
will be created prior to the SOLID defintion.  The SOLID definition will
then reference the layer number rather than the actual surfaces selected.

Layer
-----
Used to select a layer number from a list of existing layers. This field is
active only when the Select by Layer box is checked.  The text field contains
the layer number from which all of the geometry on the layer will be
projected, including any geometry selected in this form.

Select by Layer
---------------
If checked, the geometry on the specified layer will be included in the solid
calculation.

Show Layer
----------
Highlights the geometry currently residing on the selected layer.

Lower Left
----------
Contains the lower left of the bounding box after the geometry is selected using
the Select button.  These fields are for reference only and cannot be changed
by the user.
 
Upper Right
-----------
Contains the upper right of the bounding box after the geometry is selected
using the Select button.  These fields are for reference only and cannot be
changed by the user.
 
Expansion
---------
Contains the expansion values to apply to each side of the bounding box.  The
box will be expanded by the half the value for each axis in both the positive
and negative directions.  The bounding box values displayed in the Lower Left
and Upper Right fields are not adjusted for the Expansion values.

Solid Label:
------------
Contains an optional label for the defined bounding box solid.

View
----
Enters dynamic viewing mode.

Preview
-------
Previews the bounding box solid without creating permanent geometry.  The 
bounding box solid can be viewed while the form is active, but it will be
deleted when the form is exited or another solid is created.

Apply
-----
Creates the bounding box solid without taking down the form so that other
bounding box solids can be defined.
