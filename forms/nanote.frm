#HEADER#
/TITLE/ Annotation
/POSITION/ 50,50
/SIZE/250,290

#FRAME#
/TITLE/ Geometry
/POSITION/ 8,8
/SIZE/ 230,50

#EDIT#
/LABEL/ Label:
/POSITION/ 12,18
/SIZE/50,15
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 64

#PUSHBUTTON#
/LABEL/ Along Curve:
/POSITION/ 90,18
/SIZE/50,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 145,18
/SIZE/50,15
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 64

#CHOICEBOX#
/LABEL/ Attach:
/POSITION/ 12,35
/SIZE/70,140
/TYPE/ UD_DASSTRING
/CHOICES/ "Point", "Letter", "Line",

#PUSHBUTTON#
/LABEL/ Point
/POSITION/ 100,35
/SIZE/30,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 135,35
/SIZE/50,15
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 64

#FRAME#
/TITLE/ Next Position
/POSITION/ 8,62
/SIZE/ 230,30

#EDIT#
/LABEL/ Letter:
/POSITION/ 12,72
/SIZE/50,15
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 64

#EDIT#
/LABEL/ Line:
/POSITION/ 100,72
/SIZE/50,15
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 64

#FRAME#
/TITLE/ Projection Method
/POSITION/ 8,98
/SIZE/ 230,70

#CHOICEBOX#
/LABEL/ Projection Type:
/POSITION/ 15,110
/SIZE/110,140
/TYPE/ UD_DASSTRING
/CHOICES/ "None","Normal","Vector","Wrap","Revolve","Radial"

#PUSHBUTTON#
/LABEL/ Surface
/POSITION/ 135,110
/SIZE/40,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 180,110
/SIZE/50,15
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 64

#PUSHBUTTON#
/LABEL/ Vector
/POSITION/ 12,127
/SIZE/40,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 58,127
/SIZE/120,15
/TYPE/ UD_DASSTRING
/LEN/ 20
/PREC/ 64

#CHOICEBOX#
/LABEL/ Attach Point:
/POSITION/ 15,144
/SIZE/ 90,100
/TYPE/ UD_DASSTRING
/CHOICES/ "Start", "Middle", "End", "Center", "Point"

#PUSHBUTTON#
/LABEL/ Point
/POSITION/ 115,144
/SIZE/40,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 160,144
/SIZE/100,15
/TYPE/ UD_DASSTRING
/LEN/ 15
/PREC/ 64

#FRAME#
/TITLE/ Text
/POSITION/ 8,172
/SIZE/ 230,67

#EDITBOX#
/LABEL/
/POSITION/ 15,182
/SIZE/ 210,50
/TYPE/ UD_DASSTRING
/LEN/ 1024

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 10,248
/SIZE/50,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 70,248
/SIZE/50,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 175,248
/SIZE/50,15
/TYPE/UD_DASSTRING

#HELP#
===============
Annotation Form
===============

The Annotation form is used to create annotation (text notes) using an NCL
command.

Label
-----
Input the optional geometry label name for the annotation.  If a label is not
entered, then the standard NCL naming convention will be used (AN1, AN2, etc).

Along Curve
-----------
Press this button to select a line or circle for the annotation text to follow.
The curve name may also be typed into the field to the right of this button.

Attach
------
The annotation can be positioned at a point or point-vector (Point), after the
last character of the previously define annotation (Letter), or on the line
following the last annotation (Line).

Point
-----
Press this button to select the point or location to position the annotation
at when the Attach field is set to Point.  The point or location may also be
typed into the field to the right of this button.

=============
Next Position
=============

Letter
------
Enter a point-vector or point label that will receive the defined location
for the character immediately following the previously defined annotation text.
This field can be left blank.

Line
----
Enter a point-vector or point label that will receive the defined location
for the character that will start the next line following the previously defined
annotation text.  This field can be left blank.

=================
Projection Method
=================

Projection Type
---------------
Select the type of projection that is desired from the following choices.

None    - The annotation is not projected onto a surface.

Normal  - The projection will occur normal to the surface.

Vector  - The projection will be along a fixed vector.

Wrap    - Wraps the annotation onto the surface using an attach point as
          reference and using the tangencies and distances of each geometry
          position to maintain the original shape of the geometry.

Revolve - Revolves the annotation onto a surface of revolution by using an
          attach point as reference and by travelling vertically and
          horizontally along the axis of rotation.  The size of the geometry
          will be approximately maintained, but the shape may not.

Radial  - Revolves the annotation onto a surface of revolution by using an
          attach point as reference and by travelling vertically and
          horizontally along the axis of rotation.  This type of projection is
          similar to Revolve except that the shape of the geometry will be
          approximately maintained, but not necessarily the size.

Surface
-------
Press this button to select the surface or plane to project the annotation to.
The surface/plane name may also be typed into the field to the right of this
button.

Vector
------
Selects a vector to be used to project the geometry onto the surface/plane when
the projection type is Vector.  For continuous projection types, this vector
will be used to project the attach point onto the surface.  Leaving this field
blank will use a vector normal to the surface.

A vector may be selected from the screen, a label typed in, or the i,j,k
components typed in.

Attach Point:
-------------
This field is only active when a continuous projection type is specified
(Wrap, Revolve, Radial).  It selects the initial projection point that will be
used to control the projection of the annotation in order to maintain its shape
and size.  The following types of attach points can be used.

Start   - Use the starting point of the annotation.

Middle  - Use a point half way along the length of the annotation.

End     - Use the ending point of the annotation.

Center  - Use a point calculated as the center of the bounding box of the
          annotation.

Point   - Use a user defined point.

Point
-----
Selects the user defined attach point to be used for continous projections.  A
point may be selected from the screen, a label typed in, or the x,y,z
components typed in.

====
Text
====
Enter the text of the annotation.  Multiple lines of text can be entered.

==============
Action Buttons
==============

View
----
Enters dynamic viewing mode.

Preview
-------
Previews the annotation without creating permanent geometry.  The annotation
can be viewed while the form is active, but it will be deleted when the form is
exited or another annotation is created.

Apply
-----
Creates the annotation without taking down the form so that other annotations
can be created.
