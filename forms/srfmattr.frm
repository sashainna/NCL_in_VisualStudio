#HEADER#
/TITLE/ Modify Surface Attributes
/POSITION/ 0,0
/SIZE/ 200,235

#CHECKBOX#
/LABEL/ Change:
/POSITION/ 10,8
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ No. of U curves:
/POSITION/ 65,10,135,8
/SIZE/ 120,14
/TYPE/ UD_DASINT
/RANGE/1,200
/PREC/ 4
/LEN/ 4

#EDIT#
/LABEL/ No. of V curves:
/POSITION/ 65,29,135,27
/SIZE/ 120,14
/TYPE/ UD_DASINT
/RANGE/1,200
/PREC/ 4
/LEN/ 4

#CHECKBOX#
/LABEL/ Change:
/POSITION/ 10,45
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Points per U curve:
/POSITION/ 65,47,135,45
/SIZE/ 120,14
/TYPE/ UD_DASINT
/RANGE/0,200
/PREC/ 4
/LEN/ 4

#EDIT#
/LABEL/ Points per V curve:
/POSITION/ 65,66,135,64
/SIZE/ 120,14
/TYPE/ UD_DASINT
/RANGE/0,200
/PREC/ 4
/LEN/ 4

#CHECKBOX#
/LABEL/ Change:
/POSITION/ 10,82
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHOICE_LIST#
/LABEL/ Material:
/POSITION/ 65,84,100,82
/SIZE/ 110,80
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Change:
/POSITION/ 10,102
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/ Color:
/POSITION/ 65,105,100,102
/SIZE/ 110,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHECKBOX#
/LABEL/ Change:
/POSITION/ 10,122
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Display Edges
/POSITION/ 65,122,110,120
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/ Edge Color:
/POSITION/ 65,140,110,138
/SIZE/ 110,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHECKBOX#
/LABEL/ Change:
/POSITION/ 10,158
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Shaded:
/POSITION/ 65,161,135,158
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHECKBOX#
/LABEL/ Change:
/POSITION/ 10,178
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Translucency:
/POSITION/ 65,181,135,178
/SIZE/ 100,14
/TYPE/ UD_DASINT
/RANGE/1,100
/PREC/ 4
/LEN/ 4

#CHECKBOX#
/LABEL/ Output DRAFT/MODIFY command
/POSITION/ 10,195
/SIZE/ 160,14
/TYPE/ UD_DASSTRING

#HELP#
=========================
Modify Surface Attributes
=========================
This form allows the user to modify attributes associated with existing
surfaces and solids.  Each attribute field has an associated Change checkbox.
Only the attributes that have the Change box checked will be modified on the
selected surfaces.

No. of U curves:
----------------
Enter the number of curves in the U-direction are to be displayed on the
surface.

No. of V curves:
----------------
Enter the number of curves in the V-direction are to be displayed on the
surface.

Points per U curve:
-------------------
Enter the number of points that should be used to display each U-curve on the
surface.  A value of 0 will display the U-curves using the current display
tolerance and is the recommended value.

Points per V curve:
-------------------
Enter the number of points that should be used to display each V-curve on the
surface.  A value of 0 will display the V-curves using the current display
tolerance and is the recommended value.

Material:
---------
Uses the predefined material to display the surface with.

Color:
------
Displays the surface using the specified color.

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

Output DRAFT/MODIFY command
---------------------------
Checking this box will output a DRAFT/MODIFY=sf command to the part program
with the appropriate attribute modifications made on this form.

If any of the Number of Curves or Points per Curve fields are changed, then a
DISPLY/sf command will be output for each of the selected surfaces.
