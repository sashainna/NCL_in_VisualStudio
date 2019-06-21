#HEADER#
/TITLE/ NCLIPV Comparison Colors
/POSITION/ 50,50
/SIZE/175,230

#PUSHBUTTON#
/LABEL/ Auto Tolerance
/POSITION/ 15,8
/SIZE/70,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 90,8
/SIZE/ 40,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ -1000,1000

#PUSHBUTTON#
/LABEL/ Reset Colors
/POSITION/ 15,25
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#LABEL#
/LABEL/ Tolerances
/POSITION/ 22,45
/SIZE/ 40,14

#LABEL#
/LABEL/ Colors
/POSITION/ 90,45
/SIZE/ 40,14

#EDIT#
/LABEL/
/POSITION/ 20,57
/SIZE/ 50,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ -1000,1000

#COLOR#      
/LABEL/
/POSITION/ 90,57
/SIZE/60,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/
/POSITION/ 20,72
/SIZE/ 50,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ -1000,1000

#COLOR#      
/LABEL/
/POSITION/ 90,72
/SIZE/60,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/
/POSITION/ 20,87
/SIZE/ 50,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ -1000,1000

#COLOR#      
/LABEL/
/POSITION/ 90,87
/SIZE/60,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/
/POSITION/ 20,102
/SIZE/ 50,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ -1000,1000

#COLOR#      
/LABEL/
/POSITION/ 90,102
/SIZE/60,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#LABEL#
/LABEL/ 0.0000
/POSITION/ 25,120
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 90,117
/SIZE/60,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/
/POSITION/ 20,132
/SIZE/ 50,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ -1000,1000

#COLOR#      
/LABEL/
/POSITION/ 90,132
/SIZE/60,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/
/POSITION/ 20,147
/SIZE/ 50,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ -1000,1000

#COLOR#      
/LABEL/
/POSITION/ 90,147
/SIZE/60,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/
/POSITION/ 20,162
/SIZE/ 50,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ -1000,1000

#COLOR#      
/LABEL/
/POSITION/ 90,162
/SIZE/60,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/
/POSITION/ 20,177
/SIZE/ 50,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ -1000,1000

#COLOR#      
/LABEL/
/POSITION/ 90,177
/SIZE/60,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#HELP#
========================
NCLIPV Comparison Colors
========================
This form sets the tolerance values and their associated colors to use for
Report and Visual style comparisons.  Only the colors associated with the
values furthest away from 0. will be used with Undercuts and Overcuts style
comparisons, the values themselves will be ignored.

Auto Tolerance
--------------
Pressing this button will automatically set the Tolerance values based on the
increment entered in this field.  The gouge tolerances (upper fields) will be
set to a negative value (-tol), -tol*2, -tol*3, and -tol*4.  The excess
tolerances (lower fields) will be set to this value (tol), tol*2, tol*3, and
tol*4.

Reset Colors
------------
Reset the colors associated with the tolerance values to those specified in the
UL_NCLIPV_MODALS file.

Tolerances
----------
Sets the tolerance range values.  The middle of the tolerance fields is set
automatically to 0. and cannot be changed.  The next field up is the lowest
undercut (gouge) limit.  All undercuts between 0. and this value will be
displayed in the color to the right of the 0. field.  The next value up should
be a greater magnitude than the lowest undercut limit and all undercuts between
these two values will be displayed in the color to the right of the lowest
undercut limit.  You can specify 4 undercut limits.  All undercuts between the
current and previous field will be displayed in the color the right of the 
previous field.

The same logic applies to the overcut (excess) fields, which are below the 0.
field.

Colors
------
Defines the color to display for all undercuts/overcuts between the current
field and the next field.  Remember that the undercut fields go from 0. and
move to the top of the form, while the overcut fields go from 0. to the bottom
of the form.
