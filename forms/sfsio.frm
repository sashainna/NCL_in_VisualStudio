#HEADER#
/TITLE/ Curves from Multiple Surfaces
/POSITION/ 50,30
/SIZE/260,210

#CHOICEBOX#
/LABEL/ Surface Intersections:
/POSITION/ 15,8
/SIZE/147,33
/TYPE/UD_DASSTRING
/CHOICES/ "Individual","Closed Contours"

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
/PREC/ 65

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
/LABEL/ Offset by:
/POSITION/ 20,110
/SIZE/50,15
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 65,110,78,110
/SIZE/65,13
/TYPE/ UD_SCAVAL
/LEN/ 10
/PREC/ 16

#CHOICEBOX#
/LABEL/
/POSITION/ 122,110,132,110
/SIZE/60,33
/TYPE/UD_DASSTRING
/CHOICES/ "XLARGE","XSMALL","YLARGE","YSMALL","ZLARGE","ZSMALL"

#FRAME#
/TITLE/ Intersect at
/POSITION/ 8,81
/SIZE/ 245,50

#EDIT#
/LABEL/ Number-of-Curves Scalar:
/POSITION/ 15,138, 105,138
/SIZE/40,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 16

#EDIT#
/LABEL/ Initial Curve Label:
/POSITION/ 15,155, 105,153
/SIZE/40,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 16

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 15,170
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 70,170
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 200,170
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
=============================
Curves from Multiple Surfaces
=============================

This form is used to create curves at the intersection of a plane and multiple
surfaces.

Surface Intersections
---------------------
If "Individual" is selected, the generated curves will be created separately as
individual curves.  "Closed Contours" will connect the surface intersection
curves to create closed composite curves.

Select
------
Used to select the surfaces to intersect the plane with.

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
intersected, including any surfaces selected in this form.

Select by Layer
---------------
If checked, the specified layer will be included with the surfaces to be
intersected.

Show Layer
----------
Highlights the surfaces currently residing on the selected layer.

Intersect at
------------
The intersection plane can be a Z-level value, a plane, or a planar surface.
The text field will contain the Z-level value or the label of the intersecting
Plane/Surface.
 
Select
------
Allows picking of a plane or a planar surface. This button is inactive when
Z-Level is selected.

Color
-----
Chooses a color to highlight the selected plane or surface, active when Plane
is selected.

Offset by:
----------
This field is only displayed when a plane is used to intersect the multiple
surfaces.  If checked, an offset will be applied to the plane.  The text field
contains the offset value.

Offset direction modifier
-------------------------
This field is only displayed when the 'Offset by' box is checked.  It is used to
specify the offset direction.

Number-of-Curves Scalar:
------------------------
This field contains an optional scalar name, which will hold the number of
created curves after the command is processed.  

Initial Curve Label:
--------------------
Contains an optional first curve label - if entered, it determines the labelling
style for the subsequent curves.

View
----
Enters dynamic viewing mode.

Preview
-------
Previews the intersection curves without creating permanent geometry.  These
curves can be viewed while the form is active, but they will be deleted when the
form is exited or another curve is created.

You cannot preview the curve if the Include on Layer box is checked, as there
is no provision for temporarily placing surfaces onto layers.

Apply
-----
Performs the geometry intersection without taking down the form so that other
intersections can be made.
