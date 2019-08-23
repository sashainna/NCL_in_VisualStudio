#HEADER#
/TITLE/ NCLIPV Section Part
/POSITION/ 50,50
/SIZE/210,130

#PUSHBUTTON#
/LABEL/ Plane vector:
/POSITION/ 10,8
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 65,8
/SIZE/100,14
/TYPE/ UD_DASVEC
/LEN/ 25
/PREC/ 3

#PUSHBUTTON#
/LABEL/ Plane origin:
/POSITION/ 10,25
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 65,25
/SIZE/100,14
/TYPE/ UD_DASCART
/LEN/ 25
/PREC/ 3

#EDIT#
/LABEL/ Distance along vector:
/POSITION/ 10,42
/SIZE/ 100,14
/TYPE/ UD_DASVAL
/LEN/ 10
/PREC/ 3

#CHOICEBOX#
/LABEL/ Reverse Vector:
/POSITION/ 10,59
/SIZE/ 100,80
/TYPE/ UD_DASSTRING
/CHOICES/ "No","Yes"

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 15,80
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 70,80
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Reset
/POSITION/ 125,80
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#HELP#
===================
NCLIPV Section Part
===================
This form allows you to section the cut model by defining a plane normal vector
and a point on the sectioning plane.  The sectioning can be either temporary
or permanent for the active verification session.  Pressing the OK button will
retain the sectioned model the remainder of the verification session, while
pressing the CANCEL button will discard the sectioning and return the model to
its original state.

Plane vector: 
-------------
Defines the section plane normal vector.  A vector from the NCL Graphics Window
can be selected as the plane normal by pressing the Plane vector button.

Plane origin:
-------------
Defines a point on the section plane.  A point from the NCL Graphics Window can
be selected as the plane point by pressing the Plane origin button.

Distance along vector:
----------------------
Enter the distance from the section plane to actually section the part.  This
distance is useful when creating a slice from the cut model.  For example,
sectioning the part with a distance of 0 will section the model exactly at the
supplied section plane and then using the same plane with a distance of .1 will
leave a .1 slice of the model on the screen.

Reverse Vector:
---------------
Enter "Yes" if the plane normal vector should be reversed.  The normal vector
determines which part of the model to keep during sectioning.

Apply
-----
Sections the model at the defined plane and distance.  This button allows you
to temporarily section the part.

View
----
Enters Dynamic Viewing mode so that the view of the sectioned model can be
changed without permanently applying the changes.  The NCLIPV Section Part form
will be taken down while Dynamic Viewing is in effect and redisplayed when it
is exited.

Reset
-----
Resets the sectioned model to its original state without leaving the NCLIPV
Section Part form.
