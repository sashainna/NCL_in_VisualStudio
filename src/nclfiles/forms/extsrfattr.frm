#HEADER#
/TITLE/ Surface Attributes
/POSITION/ 50,30
/SIZE/185,105

#CHECKBOX#
/LABEL/ Change:
/POSITION/ 10,8
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Display Edges
/POSITION/ 65,8,110,6
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/ Edge Color:
/POSITION/ 65,26,110,24
/SIZE/ 110,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHECKBOX#
/LABEL/ Change:
/POSITION/ 10,44
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Shaded:
/POSITION/ 65,47,135,44
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHECKBOX#
/LABEL/ Change:
/POSITION/ 10,62
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Translucency:
/POSITION/ 65,65,135,62
/SIZE/ 100,14
/TYPE/ UD_DASINT
/RANGE/1,100
/PREC/ 4
/LEN/ 4

#HELP#
==================
Surface Attributes
==================
This form allows the user to modify attributes associated with existing
surfaces and solids.  Each attribute field has an associated Change checkbox.
Only the attributes that have the Change checkbox checked will be modified on the
selected surfaces.

Display Edges:
--------------
Determines if selected surfaces should be rendered with their edges displayed.

Edge Color:
-----------
Defines the color to display the surface edges with.  'Default' uses the same
color as the surface is displayed in, while the other choices select an actual
color.  This color will be used to display the edges of the surfaces when the
Display Edges box is checked.

Shaded:
-------
Toggles the display of the selected surfaces between shaded and wireframe modes.

Translucency:
-------------
Sets the translucency (opacity) of the selected surfaces.  A value of 100 will
display a solid surface with lesser values displaying a more see through
surface.
