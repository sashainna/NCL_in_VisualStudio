#HEADER#
/TITLE/ Waterline Surface List
/POSITION/ 50,30
/SIZE/260,80

#LABEL#
/LABEL/ Stock Surfaces
/POSITION/ 8,8
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/FONT/ 1.25
/COLOR/ BLUE

#LABEL#
/LABEL/ Avoid Surfaces
/POSITION/ 8,8
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/FONT/ 1.25
/COLOR/ BLUE

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 10,20
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 55,20,65,20
/SIZE/65,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#CHECKBOX#
/LABEL/ Include on Layer
/POSITION/ 130,20
/SIZE/70,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 200,20
/SIZE/50,15
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Layer
/POSITION/ 10,37
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 55,37,65,37
/SIZE/40,13
/TYPE/ UD_SCAINT
/LEN/ 10
/PREC/ 16

#CHECKBOX#
/LABEL/ Select by Layer
/POSITION/ 130,37
/SIZE/70,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Show Layer
/POSITION/ 200,37
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#HELP#
======================
Waterline Surface List
======================

This form is used to create surface lists (Stock Surfaces and Avoid Surfaces)
for the waterline roughing command.

Select
------
Used to select the surfaces to by picking.

Color
-----
Chooses a color to highlight the selected surfaces or layer surfaces.

Include on Layer
----------------
If checked, the selected surfaces will be put on the specified layer by
issuing the appropriate "DRAFT/MODIFY=sf2,sf3,LAYER=4" command.  This command
will be created prior to the POCKET command.  The POCKET command will
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
If checked, the specified layer will be included in the surface list.

Show Layer
----------
Highlights the surfaces currently residing on the selected layer.

