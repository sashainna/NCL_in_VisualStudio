#HEADER#
/TITLE/ NCLIPV Load STL File
/POSITION/ 50,50
/SIZE/220,75

#PUSHBUTTON#
/LABEL/ STL File:
/POSITION/ 10,8
/SIZE/ 35,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 50,8
/SIZE/ 105,14
/TYPE/ UD_DASSTRING
/PREC/ 35
/LEN/ 35

#CHOICEBOX#
/LABEL/ Units:
/POSITION/ 10,28
/SIZE/ 65,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Inch", "MM"

#EDIT#
/LABEL/ Tolerance:
/POSITION/ 90,28
/SIZE/ 65,14
/TYPE/ UD_DASVAL
/PREC/ 3
/LEN/ -6
/RANGE/ .001,2.5

#HELP#
====================
NCLIPV Load STL File
====================
This form loads an STL model from an external file.  The STL file can be
generated from any CAD system, but it must be a closed solid.  This means that
there cannot be any gaps or overlapping faces in the STL model.  NCLIPV will
attempt to close the STL model within the specified tolerance.

If the solid cannot be closed, then an error message will be output and the STL
solid will be marked as invisible and inactive by default.  You can manually
visible and activate this solid and it may actually work in the session
depending on how badly the STL model was created.

STL File:
---------
Enter the external STL file name in this field either by typing it in or by
pressing the prompt button and selecting it from the file browser.

Units:
------
Since NCLIPV has no way of determining the proper units of the STL model, you
must select the units that the STL file was saved in, either Inch or MM.

Tolerance:
----------
Specify the tolerance to use to attempt to close the STL model.  This tolerance
is only used to fix gaps and overlapping faces in the STL model and does not
affect the accuracy of the model itself.
