#HEADER#
/TITLE/ Load Solids from Stock File
/POSITION/ 50,50
/SIZE/200,105

#PUSHBUTTON#
/LABEL/ File:
/POSITION/ 10,8
/SIZE/30,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 45,8
/SIZE/135,14
/TYPE/ UD_DASSTRING
/PREC/ 32
/LEN/ 32

#EDIT#
/LABEL/ Number of Solids Scalar:
/POSITION/ 10,25
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/PREC/ 12
/LEN/ 12

#CHECKBOX#
/LABEL/ Transform:
/POSITION/ 10,42
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 65,42
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/PREC/ 24
/LEN/ 24

#EDIT#
/LABEL/ Initial Label:
/POSITION/ 10,59
/SIZE/ 100,14
/TYPE/ UD_DASSTRING
/PREC/ 12
/LEN/ 12

#HELP#
============================
Load Solids from Stock File
============================
This form is used to load an external NCLIPV stock file and create solid(s)
from the stock commands.

File:
-----
Specifies the stock file to load.  Pressing the File button will display a
file browser that can be used to select the file.

Number of Solids Scalar:
------------------------
Enter the scalar name that will receive the number of solids actually defined
from the stock file.  This scalar is optional.

Transform:
----------
Checking this box and entering a matrix name will transform the position and/or
orientation of the solid(s) when loading them by this matrix.  A nested matrix
definition can be specified in this field, i.e. (mx/transl,10,0,0).

Initial Label:
--------------
Contains an optional initial label for the loaded solid(s).
