#HEADER#
/TITLE/ Chain Select Modals
/POSITION/ 50,50
/SIZE/280,170

#CHECKBOX#
/LABEL/ Conditional Chaining
/POSITION/ 10,8
/SIZE/110,14
/TYPE/UD_DASINT

#EDIT#
/LABEL/ Tolerance:
/POSITION/ 150,8,200,8
/SIZE/110,14
/TYPE/UD_DASDISTANCE
/PREC/ 4
/LEN/ -8

#CHECKBOX#
/LABEL/ Limit to planar elements
/POSITION/ 10,25
/SIZE/110,14
/TYPE/UD_DASINT

#EDIT#
/LABEL/ Plane:
/POSITION/ 150,25,200,25
/SIZE/110,14
/TYPE/UD_DASSTRING
/PREC/ 15
/LEN/ 15
/INPUT/ FORM_PICK

#CHECKBOX#
/LABEL/ Include Lines
/POSITION/ 10,42
/SIZE/110,14
/TYPE/UD_DASINT

#CHECKBOX#
/LABEL/ Include Circles
/POSITION/ 150,42
/SIZE/110,14
/TYPE/UD_DASINT

#CHECKBOX#
/LABEL/ Include Bsplines
/POSITION/ 10,59
/SIZE/110,14
/TYPE/UD_DASINT

#CHECKBOX#
/LABEL/ Include Composites
/POSITION/ 150,59
/SIZE/110,14
/TYPE/UD_DASINT

#FRAME#
/TITLE/Surface Chaining
/POSITION/5,76
/SIZE/260,67

#EDIT#
/LABEL/ Tolerance:
/POSITION/ 10,90,60,90
/SIZE/110,14
/TYPE/UD_DASDISTANCE
/PREC/ 4
/LEN/ -8

#EDIT#
/LABEL/ No. of Vectors:
/POSITION/ 150,90,200,90
/SIZE/110,14
/TYPE/UD_DASINT
/PREC/ 4
/LEN/ -8

#CHOICEBOX#
/LABEL/ Direction:
/POSITION/ 10,107,60,107
/SIZE/175,40
/TYPE/UD_DASSTRING
/CHOICES/ "Along U/V lines","Along Previous Chaining Direction"
/CHOICES/ "Perpendicular to Boundary Edge","Along Boundary Edge"

#CHECKBOX#
/LABEL/ Allow surfaces to be selected multiple times
/POSITION/ 10,124
/SIZE/200,14
/TYPE/UD_DASINT

#HELP#
===================
Chain Select Modals
===================
The settings defined in this form control the process of chain selecting
wireframe entities (lines, circles, curves) and surfaces.

Conditional Chaining
--------------------
When an intersection is reached during chain selection and there are two or
more possible curves to choose from, then the behavior can be set to either
automatically select the first curve that is found (Normal) or to prompt the
user for the proper curve to select (Conditional).  When the Conditional
Chaining box is checked, the curves that can be selected from will be displayed
in Red and the user simply selects the curve to be included in the chain
selection, which will then continue along the selected path.

Tolerance:
----------
The tolerance value determines how close the end points of two curves have to
be in order to be considered connected for chaining purposes.

Limit to planar elements
------------------------
Check this box if all entities considered for chaining must lie on a plane.
Entities that are not entirely lying on this plane will not be considered for
chaining.  Unchecking this box will allow the chaining operation to select any
curves that have a matching end point, whether they lie on the same planar or
not.

Plane:
------
Contains either the name of the plane to use or the canonical form of the plane
(i,j,k,d), when the Planar field is set to Yes.  You can press the Plane button
to interactively select a plane from the screen.

Include Lines
-------------
Lines can either be included or excluded from the chaining operation.

Include Circles
---------------
Circles can either be included or excluded from the chaining operation.

Include Curves
--------------
Curves can either be included or excluded from the chaining operation.  The
supported curve types include B-splines, NCL Curves, Conics, and
Surface-splines.

Include Composites
------------------
Composite curves can either be included or excluded from the chaining operation.

Tolerance:
----------
The tolerance value determines how close the boundaries of two surfaces have to
be in order to be considered connected for surface chaining purposes.

Number of Vectors:
------------------
Number of directions to be displayed on the first surface for chaining.

Direction:
----------
The chaining direction could be along the U or V lines of the base surface,
along the direction of chaining from the previous surface, along the direction
perpendicular to the boundary edge of the new surface, or, along the boundary 
edge of the new surface.
  
Allow surfaces to be selected multiple times
--------------------------------------------
Surface chaining will typically end when a previously selected surface is
encountered for a second time in the chaining process.  There are some cases
where a U-shaped surface could have multiple surfaces contained within its
pocket and this surface needs to be selected each time it is encountered, for
example when selecting the Drive Surfaces for a Flank Contouring operation.

Check this box if a surface should be selected multiple times during the
chaining operation.
