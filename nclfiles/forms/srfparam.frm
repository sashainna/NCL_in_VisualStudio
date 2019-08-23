#HEADER#
/TITLE/ Modify Surface Parameters
/POSITION/ 0,0
/SIZE/ 245,160

#PUSHBUTTON#
/LABEL/ Surface
/POSITION/ 15,12
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 75,12
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/PREC/ 20
/LEN/ 10

#COLOR#      
/LABEL/
/POSITION/ 133,12
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#PUSHBUTTON#
/LABEL/ New U Direction
/POSITION/ 15,42
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ New Normal Direction
/POSITION/ 115,42
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#      
/LABEL/ Parelm:
/POSITION/ 15,61,60,59
/SIZE/ 90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Auto", "Along V", "Along U"

#CHOICEBOX#      
/LABEL/ Drive as trimmed:
/POSITION/ 115,61,175,59
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#FRAME#
/TITLE/ Parameters
/POSITION/ 10,32
/SIZE/ 205,44

#COLOR#      
/LABEL/ Original U Dir
/POSITION/ 15,94,65,92
/SIZE/105,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ New U Dir
/POSITION/ 125,94,167,92
/SIZE/95,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Original Normal
/POSITION/ 15,111,65,109
/SIZE/105,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ New Normal
/POSITION/ 125,111,167,109
/SIZE/95,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#FRAME#
/TITLE/ Colors
/POSITION/ 10,82
/SIZE/ 220,44

#HELP#
=========================
Modify Surface Parameters
=========================

This form allows the user to manipulate surface parameters:
- swap the U and V parameters,
- reverse the U or V direction,
- reverse the direction of a surface normal,
- set the PARELM type,
- set the drive type for a trimmed surface.

Surface
-------
Select a surface.

(Text Field)
------------
Contains the current surface’s label.

Color
-----
Choose a color to highlight the selected surface.

New U Direction
---------------
Pick an assist-vector along the desired U-direction from the eight
possibilities. The other assist-vector adjacent to the picked U-direction
will be the new V-direction. The choice determines whether the U and V
parameters are to be swapped, and whether U or V is to be reversed. The user
can pick the current U-direction or press the Alt-Act button to leave the
current parameters in place.

New Normal
----------
Pick an assist-vector along the desired normal direction. The user can pick
the current normal direction or press the Alt-Act button to leave the current
choice in place.

Parelm
------
Choose between Auto, Along V, or Along U.

Drive as Trimmed
----------------
Choose between Yes (FACE in REDEF command syntax) and No (BASE in REDEF
command syntax). Ignored if the surface is untrimmed.

Colors
------
The subform allows the user to choose how to highlight assist-vectors:
the original and new U-direction, and the original and new normal direction. 
