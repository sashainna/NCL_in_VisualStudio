#HEADER#
/TITLE/ Geometry Settings
/POSITION/ 0,0
/SIZE/ 277,355

#FRAME#
/TITLE/Display
/POSITION/ 8,5
/SIZE/ 257,75

#CHECKBOX#      
/LABEL/Points
/POSITION/ 14,15
/SIZE/ 32,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Lines
/POSITION/ 82,15
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Circles
/POSITION/ 149,15
/SIZE/ 33,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Curves
/POSITION/ 214,15
/SIZE/ 35,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Vectors
/POSITION/ 14,30
/SIZE/ 37,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Point Vectors
/POSITION/ 82,30
/SIZE/ 55,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Patterns
/POSITION/ 149,30
/SIZE/ 39,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Matrices
/POSITION/ 214,30
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Solids
/POSITION/ 14,45
/SIZE/ 31,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Surfaces
/POSITION/ 82,45
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Annotations
/POSITION/ 149,45
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Planes
/POSITION/ 214,45
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Shapes
/POSITION/ 14,60
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Select All
/POSITION/ 149,60
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Clear All
/POSITION/ 205,60
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/Geometry Label Indexes
/POSITION/ 8,85
/SIZE/ 257,120

#EDIT#
/LABEL/ Points:
/POSITION/ 27, 100
/SIZE/35,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Lines:
/POSITION/ 114, 100
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Circles:
/POSITION/ 202, 100
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Curves:
/POSITION/ 24, 120
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Vectors:
/POSITION/ 105, 120
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Point Vectors:
/POSITION/ 177, 120
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Patterns:
/POSITION/ 19, 140
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Matrices:
/POSITION/ 103, 140
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Solids:
/POSITION/ 205, 140
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Surfaces:
/POSITION/ 17, 160
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Annotations:
/POSITION/ 90, 160
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Planes:
/POSITION/ 202, 160
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Shapes:
/POSITION/ 22, 180
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#FRAME#
/TITLE/Curve/Surface Display
/POSITION/ 8,210
/SIZE/ 257,100

#CHOICEBOX#
/LABEL/ Resolution:
/POSITION/ 15,225
/SIZE/85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Toler", "Lines", "Points"

#EDIT#
/LABEL/ Tolerance:
/POSITION/ 137, 225
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 6
/PREC/ 96

#EDIT#
/LABEL/ Points Per Curve:
/POSITION/ 15, 245
/SIZE/27,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ No. of U-lines:
/POSITION/ 26, 265
/SIZE/27,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ No. of V-lines:
/POSITION/ 129, 265
/SIZE/27,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Points Per U-line:
/POSITION/ 14, 285
/SIZE/27,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#EDIT#
/LABEL/ Points Per V-line:
/POSITION/ 118, 285
/SIZE/27,14
/TYPE/UD_DASSTRING
/LEN/ 4
/PREC/ 96

#CHECKBOX#      
/LABEL/Output Control Command
/POSITION/ 14,315
/SIZE/ 92,14
/TYPE/ UD_DASSTRING

#HELP#
=================
Geometry Settings
=================

Display
-------
Only entities with marked boxes will be displayed automatically when they are
generated.

Select All
----------
Press the Select All button to select all geometry to be displayed.

Clear All
---------
Press the Clear All button to unselect all geometry.

Geometry Label Indexes
----------------------
Use these fields to adjust the starting index used during Automatic Identifier
generation when creating a geometric entity.

Resolution:
-----------
Use the dropdown menu to select which fields to enable.

 - Toler: Enables the tolerance field for input.

 - Lines: Enables the Points Per Curve, No. U-Lines and No. V-lines fields
          for input.

 - Points: Enables the Points Per Curve, No. U-Lines, No. V-lines, Points Per
           U-Line and Points Per V-Line fields for input.

Tolerance:
----------
The Tolerance setting defines the accuracy at which curves and surfaces will
be displayed.

No. of U-lines:
---------------
Enter the number of curves in the U-direction are to be displayed on the
surface.

No. of V-lines:
---------------
Enter the number of curves in the V-direction are to be displayed on the
surface.

Points per U-line:
------------------
Enter the number of points that should be used to display each U-curve on the
surface.  A value of 0 will display the U-curves using the current display
tolerance and is the recommended value.

Points per V-line:
------------------
Enter the number of points that should be used to display each V-curve on the
surface.  A value of 0 will display the V-curves using the current display
tolerance and is the recommended value.

Output Control Command
----------------------
When this box is checked NCL will output the control commands associated with
any setting changes made.  If this form has been accessed through the another,
then use the box from the original form.  Note that the changes will be
applied even if this box is not checked.
