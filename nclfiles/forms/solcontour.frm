#HEADER#
/TITLE/ Contour Solid of Multiple Surfaces
/POSITION/ 50,30
/SIZE/260,215

#EDIT#
/LABEL/ Contour Around Surfaces Offset by:
/POSITION/ 15,8
/SIZE/40,13
/TYPE/ UD_SCAVAL
/LEN/ -7
/PREC/ 3

#CHOICEBOX#
/LABEL/
/POSITION/ 180,8
/SIZE/65,30
/TYPE/UD_DASSTRING
/CHOICES/ "Profile","Box"

#FRAME#
/TITLE/ Surfaces
/POSITION/ 8,26
/SIZE/ 245,49

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
/TITLE/ Project to
/POSITION/ 8,81
/SIZE/ 245,67

#CHOICEBOX#
/LABEL/
/POSITION/ 15,93
/SIZE/55,33
/TYPE/UD_DASSTRING
/CHOICES/ "Z-level","Plane","Sf-Bounds"

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

#EDIT#
/LABEL/ Bottom Offset:
/POSITION/ 15,110
/SIZE/ 80,13
/TYPE/ UD_SCAVAL
/LEN/ -7
/PREC/ 3

#EDIT#
/LABEL/ Top Offset:
/POSITION/ 110,110
/SIZE/ 80,13
/TYPE/ UD_SCAVAL
/LEN/ -7
/PREC/ 3

#EDIT#
/LABEL/ Height:
/POSITION/ 15,127
/SIZE/70,13
/TYPE/ UD_SCAVAL
/LEN/ -7
/PREC/ 3

#PUSHBUTTON#
/LABEL/ Vertical Direction:
/POSITION/ 90,127
/SIZE/ 65,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 160,127
/SIZE/100,13
/TYPE/UD_DASSTRING
/LEN/ 10
/PREC/ 64

#EDIT#
/LABEL/ Solid Label:
/POSITION/ 15,156
/SIZE/40,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 16

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 15,173
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 70,173
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 200,173
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
==================================
Contour Solid of Multiple Surfaces
==================================

This form is used to create an extruded solid that follows the contour of a
group of surfaces.

Contour Around Surfaces Offset by:
----------------------------------
This field contains an offset value will expand (positive value) or shrink
(negative value) the calculated contour.  In essence, a solid offset from the
contour curve by the value specified in this field will be created.

Profile / Box
-------------
This selection determines if the resultant solid will follow the profile of
the contour (Profile) or be in the shape of a bounding box that encloses the
calculated contour (Box).

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
will be created prior to the SOLID defintion.  The SOLID definition will
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
If checked, the surfaces on the specified layer will be included in the contour
calculation.

Show Layer
----------
Highlights the surfaces currently residing on the selected layer.

Project to
----------
The projection plane can be a Z-level value, a Plane, a planar surface, or the
lower bounds of the selected surfaces (Sf-bounds).  The text field will contain
the Z-level value or the label of the projection Plane/Surface.
 
Select
------
Allows picking of a plane or a planar surface. This button is only active when
projecting to a plane.

Color
-----
Chooses a color to highlight the selected plane or surface, active when Plane
is selected.

Bottom Offset:
--------------
Specifies an offset value to apply to the calculated lower bounds of the
selected surfaces when Sf-bounds is active.

Top Offset:
-----------
Specifies an offset value to apply to the calculated upper bounds of the
selected surfaces when Sf-bounds is active.

Height:
-------
Specifies the height of the solid when projecting onto a Z-level or a Plane.

Vertical Direction:
-------------------
Specifies the directional vector to use to extrude the contour.  This vector
defaults to the plane normal used as the bottom of the contour or 0,0,1 if a
plane is not used as the projection method.  Pressing this button allows you
to select a vector from the screen or a vector label or vector components can
be entered into the field.

Solid Label:
------------
Contains an optional label for the defined contour solid.

View
----
Enters dynamic viewing mode.

Preview
-------
Previews the contour solid without creating permanent geometry.  The contour
solid can be viewed while the form is active, but it will be deleted when the
form is exited or another solid is created.

You cannot preview the solid if the Include on Layer box is checked, as there
is no provision for temporarily placing surfaces onto layers.

Apply
-----
Creates the contour solid without taking down the form so that other contour
solids can be defined.
