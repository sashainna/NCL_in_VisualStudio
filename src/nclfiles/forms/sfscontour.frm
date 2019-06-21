#HEADER#
/TITLE/ Contour Curve of Multiple Surfaces
/POSITION/ 50,30
/SIZE/260,180

#EDIT#
/LABEL/ Contour Around Surfaces Offset by:
/POSITION/ 15,8
/SIZE/40,13
/TYPE/ UD_SCAVAL
/LEN/ -7
/PREC/ 3

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 15,38
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 55,38,65,38
/SIZE/65,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#CHECKBOX#
/LABEL/ Include on Layer
/POSITION/ 130,38
/SIZE/70,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 200,38
/SIZE/50,15
/TYPE/ UD_DASSTRING

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

#CHECKBOX#
/LABEL/ Select by Layer
/POSITION/ 130,55
/SIZE/70,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Show Layer
/POSITION/ 200,55
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Surfaces
/POSITION/ 8,26
/SIZE/ 245,49

#CHOICEBOX#
/LABEL/
/POSITION/ 15,93
/SIZE/55,33
/TYPE/UD_DASSTRING
/CHOICES/ "Z-level","Plane"

#EDIT#
/LABEL/
/POSITION/ 65,93,78,93
/SIZE/65,13
/TYPE/UD_DASSTRING
/LEN/ 10
/PREC/ 64

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 132,93
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 190,93
/SIZE/60,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#CHECKBOX#
/LABEL/ Not Used
/POSITION/ 20,110
/SIZE/50,15
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Not Used
/POSITION/ 65,110,78,110
/SIZE/65,13
/TYPE/ UD_SCAVAL
/LEN/ 10
/PREC/ 16

#CHOICEBOX#
/LABEL/ Not Used
/POSITION/ 122,110,132,110
/SIZE/60,33
/TYPE/UD_DASSTRING
/CHOICES/ "XLARGE","XSMALL","YLARGE","YSMALL","ZLARGE","ZSMALL"

#FRAME#
/TITLE/ Project to
/POSITION/ 8,81
/SIZE/ 245,33

#EDIT#
/LABEL/ Not Used
/POSITION/ 15,138,105,138
/SIZE/40,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 65

#EDIT#
/LABEL/ Curve Label:
/POSITION/ 15,122
/SIZE/40,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 16

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 15,139
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 70,139
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 200,139
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
==================================
Contour Curve of Multiple Surfaces
==================================

This form is used to create a contour curve as a two-dimensional outline of a
group of surfaces.

Contour Around Surfaces Offset by:
----------------------------------
This field contains an offset value will expand (positive value) or shrink
(negative value) the calculated curve.  In essence, a curve offset from the
contour curve by the value specified in this field will be created on the
planar level.

Select
------
Used to select the surfaces to project onto the planar level.

Color
-----
Chooses a color to highlight the selected surfaces or layer surfaces.

Include on Layer
----------------
If checked, the selected surfaces will be put on the specified layer by
issuing the appropriate "DRAFT/MODIFY=sf2,sf3,LAYER=4" command.  This command
will be created prior to the CURVE defintion.  The CURVE definition will
then reference the layer number rather than the actual surfaces selected.

Deselect All
------------
Deselects all of the currently selected surfaces.

Layer
-----
Used to select a layer number from a list of existing layers. This field is
active only when the Select by Layer box is checked.  The text field contains
the layer number from which all of the surfaces on the layer will be
projected, including any surfaces selected in this form.

Select by Layer
---------------
If checked, the surfaces on the specified layer will be included in the 2-D
projection.

Show Layer
----------
Highlights the surfaces currently residing on the selected layer.

Project to
----------
The projection plane can be a Z-level value, a plane, or a planar surface.
The text field will contain the Z-level value or the label of the projection
Plane/Surface.
 
Select
------
Allows picking of a plane or a planar surface. This button is inactive when
Z-Level is selected.

Color
-----
Chooses a color to highlight the selected plane or surface, active when Plane
is selected.

Curve Label:
------------
Contains an optional label for the defined contour curve.

View
----
Enters dynamic viewing mode.

Preview
-------
Previews the contour curve without creating permanent geometry.  The contour
curve can be viewed while the form is active, but it will be deleted when the
form is exited or another curve is created.

You cannot preview the curve if the Include on Layer box is checked, as there
is no provision for temporarily placing surfaces onto layers.

Apply
-----
Creates the contour curve without taking down the form so that other contours
can be defined.
