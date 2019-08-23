#HEADER#
/TITLE/ Contour Stock/Fixture
/POSITION/ 50,50
/SIZE/225,110

#PUSHBUTTON#
/LABEL/ Select Surfaces
/POSITION/ 10,10
/SIZE/ 65,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 80,10
/SIZE/60,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 145,10
/SIZE/70,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Expansion:
/POSITION/ 10,28
/SIZE/60,13
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ -12

#EDIT#
/LABEL/Vertical Direction:
/POSITION/ 10,45,80,45
/SIZE/150,14
/TYPE/ UD_DASVEC
/LEN/ 25
/PREC/ 3
/INPUT/ FORM_PICK

#EDIT#
/LABEL/ Bottom Offset:
/POSITION/ 10,64
/SIZE/90,13
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -12

#EDIT#
/LABEL/ Top Offset:
/POSITION/ 120,64
/SIZE/90,13
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -12

#HELP#
This form allows you to define a Stock (or a Fixture) using a contour around
the part's horizontal projection

Select Surfaces
---------------
Takes down the form, brings up the SELECT menu. Allows selection of surfaces.

Color
-----
Chooses a color to highlight the selected surfaces.

Deselect All
------------
Deselects the currently selected surfaces.

Expansion
---------
The horizontal offset parameter.

Vertical Direction
------------------
Used to define the vertical direction by inputting a vector - <0, 0, 1> is the
suggested default.

Bottom Offset
-------------
The bottom is defined as the level below the part by the specified parameter.

Top Offset
----------
The top is defined as the level above the part by the specified parameter.

