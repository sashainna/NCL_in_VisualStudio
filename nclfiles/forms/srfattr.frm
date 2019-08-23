#HEADER#
/TITLE/ Surface Attributes
/POSITION/ 0,0
/SIZE/ 150,224

#EDIT#
/LABEL/ No. of U curves:
/POSITION/ 10,8,80,8
/SIZE/ 125,14
/TYPE/ UD_SCAINT
/RANGE/2,200
/PREC/ 4
/LEN/ 4

#EDIT#
/LABEL/ No. of V curves:
/POSITION/ 10,25,80,25
/SIZE/ 125,14
/TYPE/ UD_SCAINT
/RANGE/2,200
/PREC/ 4
/LEN/ 4

#EDIT#
/LABEL/ Points per U curve:
/POSITION/ 10,42,80,42
/SIZE/ 125,14
/TYPE/ UD_SCAINT
/RANGE/0,200
/PREC/ 4
/LEN/ 4

#EDIT#
/LABEL/ Points per V curve:
/POSITION/ 10,59,80,59
/SIZE/ 125,14
/TYPE/ UD_SCAINT
/RANGE/0,200
/PREC/ 4
/LEN/ 4

#CHOICE_LIST#      
/LABEL/ Material:
/POSITION/ 10,76,80,76
/SIZE/ 125,80
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Display Edges
/POSITION/ 10,93,55,93
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/ Edge Color:
/POSITION/ 10,110,80,110
/SIZE/ 125,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Shaded:
/POSITION/ 10,127,80,127
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#EDIT#
/LABEL/ Translucency:
/POSITION/ 10,144,80,144
/SIZE/ 100,14
/TYPE/ UD_DASINT
/RANGE/1,100
/PREC/ 4
/LEN/ 4

#CHOICEBOX#
/LABEL/ Drive as trimmed:
/POSITION/ 10,164,80,164
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default","Face","Base"

#CHOICEBOX#      
/LABEL/ Check for "bowtie":
/POSITION/ 10,184,80,184
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Yes", "No"

#HELP#
==================
Surface Attributes
==================
This form allows the user to modify the default attributes to use when defining
surfaces and solids.  The defaults for the following fields can be set in the
'ncl_srfattr.mod' file located in the modals directory.

    No. of U curves
    No. of V curves
    Points per U curve
    Points per V curve
    Material
    Display Edges
    Edge Color
    Shaded
    Translucency

This modal file will be updated with the specified settings for these fields
when this form is accepted.

No. of U curves:
----------------
Enter the number of curves in the U-direction are to be displayed by default on
surfaces.

No. of V curves:
----------------
Enter the number of curves in the V-direction are to be displayed by default on
surfaces.

Points per U curve:
-------------------
Enter the number of points that should be used to display each U-curve on by
default on surfacs.  A value of 0 will display the U-curves using the current
display tolerance and is the recommended value.

Points per V curve:
-------------------
Enter the number of points that should be used to display each V-curve on by
default on surfacs.  A value of 0 will display the V-curves using the current
display tolerance and is the recommended value.

Material:
---------
Uses the predefined material to display surfaces with.

Display Edges:
--------------
Determines if surfaces should be rendered with their edges displayed.

Edge Color:
-----------
Defines the color to display surface edges with.  'Default' uses the same
color as the surface is displayed in, while the other choices select an actual
color.  This color will be used to display the edges of the surfaces when the
Display Edges box is checked.

Shaded:
-------
Toggles the default display of surfaces between shaded and wireframe modes.

Translucency:
-------------
Sets the default translucency (opacity) of surfaces.  A value of 100 will
display a solid surface with lesser values displaying a more see through
surface.

Drive as trimmed:
-----------------
This field determines how trimmed surfaces will be driven.  'Default' uses
the setting defined with the trimmed surface using the command REDEF/SF,BASE-
FACE, 'Face' drives the surface as trimmed, and 'Base' drives the base surface
without respecting the boundary curves of the trimmed surface.  The recommended
setting is 'Default'.  This setting can also be changed using the *SET/TRIMMED
command.

Check for "bowtie":
-------------------
NCL has the capability of determining if a surface is twisted into a "bowtie"
shape when it is defined and correcting this problem upon definition.  Bowtie
surfaces are typically created when some of the curves used in the definition
of the surface run in opposite directions to the other definition curves.  The
recommended setting is 'Yes'.  This setting can also be changed using the
*SET/SCHECK command.
