#HEADER#
/TITLE/ Select Data Entities
/POSITION/ 50,30
/SIZE/215,110

#FRAME#
/TITLE/ Entities
/POSITION/ 8,10
/SIZE/ 180,32

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 15,22
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#COLOR#
/LABEL/
/POSITION/ 55,22,65,22
/SIZE/65,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 130,22
/SIZE/50,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Data Label:
/POSITION/ 15,51
/SIZE/80,13
/TYPE/ UD_DASSTRING
/LEN/ 24
/PREC/ 96

#CHECKBOX#
/LABEL/ Subscripts Only
/POSITION/ 10,68
/SIZE/80,13
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 95,68
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 155,68
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
====================
Select Data Entities
====================

This form is used to create a DATA statement consisting entirely of geometric
entities selected from the screen.

Select
------
Used to select the geometric entities to include in the DATA statement.  If a
geometric entity that was already selected is selected again then it will be
removed from the selection.

Color
-----
Chooses a color to highlight the selected entities.

Deselect All
------------
Deselects all of the currently selected surfaces.

Data Label:
-----------
Contains the label for the defined DATA statement.

Subscripts Only
---------------
Select how the geometry label should be output to the DATA statement.  If this
box is checked, then only the subscripts of picked geometry will be output.
If the box is unchecked then the label and subscript will be output.  If the
geometry entity is not subscripted then only the label will be output
regardless of the setting of this box.

View
----
Enters dynamic viewing mode.

Apply
-----
Creates the DATA statement without taking down the form so that other DATA
statements can be defined.
