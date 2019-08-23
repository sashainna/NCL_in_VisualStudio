#HEADER#
/TITLE/ NCLIPV STL File Status
/POSITION/ 50,50
/SIZE/240,105

#LABEL#
/LABEL/ Loading ...
/POSITION/ 10,8
/SIZE/ 200,14
/TYPE/ UD_DASSTRING

#PROGRESS#
/LABEL/ Progress:
/POSITION/ 10,25
/SIZE/ 160,14
/TYPE/ UD_DASINT

#LABEL#
/LABEL/ Checking Orientation...
/POSITION/ 10,42
/SIZE/ 160,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Interrupt
/POSITION/ 90,59
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
======================
NCLIPV STL File Status
======================
This form shows the status as an STL model is loaded from an external file and
its integrity is checked and attempted to be fixed.

Progress:
---------
Displays the progress as the STL model is loaded from an external file.

Checking Solid Orientation...
-----------------------------
This message is displayed when NCLIPV is checking the orientation of the
individual patches of the STL model.

Making Consistent...
--------------------
If the STL model patches are not consistent in their orientation, then this
message will be displayed and NCLIPV will attempt to make the model consistent.

Fixing Self Intersections...
----------------------------
If it is found that the STL model contains self intersections then an attempt
will be made to fix them and this message will be displayed.  This can be a
long procedure on large complicated STL models and can be stopped by pressing
the Interrupt button.

Merging Coplanar Faces...
-------------------------
The last step in improving the STL model is to merge connecting faces that are
coplanar.

Interrupt
---------
It is possible to interrupt the process of importing an STL model during the
loading phase and while it is fixing Self Intersections by pressing this
button.
