#HEADER#
/TITLE/ Project Onto Surface/Plane
/POSITION/ 50,50
/SIZE/250,200

#FRAME#
/TITLE/ Geometry
/POSITION/ 8,8
/SIZE/ 230,65

#PUSHBUTTON#
/LABEL/ Curve
/POSITION/ 12,18
/SIZE/40,15
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Pattern
/POSITION/ 10,18
/SIZE/40,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 60,18
/SIZE/50,15
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 64

#COLOR#      
/LABEL/
/POSITION/ 120,18
/SIZE/55,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#PUSHBUTTON#
/LABEL/ Surface
/POSITION/ 12,35
/SIZE/40,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 60,35
/SIZE/50,15
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 64

#COLOR#      
/LABEL/
/POSITION/ 120,35
/SIZE/55,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHECKBOX#
/LABEL/ Output Near Point
/POSITION/ 15,52
/SIZE/70,15
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Projection Method
/POSITION/ 8,80
/SIZE/ 230,70

#CHOICEBOX#
/LABEL/ Projection Type:
/POSITION/ 15,92
/SIZE/110,140
/TYPE/ UD_DASSTRING
/CHOICES/ "Normal","Vector","Atangle","Wrap","Revolve","Radial"

#PUSHBUTTON#
/LABEL/ Vector
/POSITION/ 12,109
/SIZE/40,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 58,109
/SIZE/120,15
/TYPE/ UD_DASSTRING
/LEN/ 20
/PREC/ 64

#EDIT#
/LABEL/ Start Angle:
/POSITION/ 15,109
/SIZE/ 80,13
/TYPE/ UD_SCAVAL
/LEN/ 8
/PREC/ 4

#EDIT#
/LABEL/ End Angle:
/POSITION/ 105,109
/SIZE/ 80,13
/TYPE/ UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ Attach Point:
/POSITION/ 15,126
/SIZE/ 90,100
/TYPE/ UD_DASSTRING
/CHOICES/ "Start", "Middle", "End", "Center", "Point"

#PUSHBUTTON#
/LABEL/ Point
/POSITION/ 115,126
/SIZE/40,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 160,126
/SIZE/100,15
/TYPE/ UD_DASSTRING
/LEN/ 15
/PREC/ 64

#PUSHBUTTON#
/LABEL/ Controlling Surface
/POSITION/ 12,126
/SIZE/70,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 90,126
/SIZE/50,15
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 64

#COLOR#      
/LABEL/
/POSITION/ 145,126
/SIZE/55,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 10,155
/SIZE/50,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 70,155
/SIZE/50,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 175,155
/SIZE/50,15
/TYPE/UD_DASSTRING

#HELP#
==========================
Project Onto Surface/Plane
==========================

The Projection form is used to project a curve or pattern onto a surface or a
plane.  Both non-continuous (NOWRAP) and continuous (WRAP, REVOLV, RADIAL)
projections can be accomplished from this form, with the ability to preview the
projection prior to creating a permanent geometry.

Curve
-----
Press this button to select the curve to project.  The curve name may also be
typed into the field to the right of this button.  This field will only be
displayed when projecting curves.

The color choice field specifies the color to use to highlight the selected
curve.

Pattern
-------
Press this button to select the pattern to project.  The pattern name may also
be typed into the field to the right of this button.  This field will only be
displayed when projecting patterns.

The color choice field specifies the color to use to highlight the selected
pattern.

Surface
-------
Press this button to select the surface or plane to project the geometry to.
The surface/plane name may also be typed into the field to the right of this
button.

The color choice field specifies the color to use to highlight the selected
surface/plane.

Output Near Point
-----------------
When projecting geometry onto a surface that can have multiple projection
possibilities at each geometry location (for example, a complete surface of
revolution), it is necessary to select the location on the surface near the
desired projection area.  Checking this box will output the location of the
surface that is picked and will output this location with the projection
command so that the projected geometry will be situated near this point.

Projection Type
---------------
Select the type of projection that is desired from the following choices.

Normal  - The projection will occur normal to the surface.

Vector  - The projection will be along a fixed vector.

Atangle - The projection will be at a calculated vector for each portion of the
          projected geometry.  This vector will be calculated by iterating an
          angle of projection based on user specified starting and ending
          angles.  The projection vector is calculated as an angular offset
          from a vector normal to the surface utilizing the tangency of the
          projection geometry as the rotation axis.

Wrap    - Wraps the geometry onto the surface using an attach point as
          reference and using the tangencies and distances of each geometry
          position to maintain the original shape of the geometry.

Revolve - Revolves the geometry onto a surface of revolution by using an
          attach point as reference and by travelling vertically and
          horizontally along the axis of rotation.  The size of the geometry
          will be approximately maintained, but the shape may not.

Radial  - Revolves the geometry onto a surface of revolution by using an
          attach point as reference and by travelling vertically and
          horizontally along the axis of rotation.  This type of projection is
          similar to Revolve except that the shape of the geometry will be
          approximately maintained, but not necessarily the size.

Vector
------
Selects a vector to be used to project the geometry onto the surface/plane when
the projection type is Vector.  For continuous projection types, this vector
will be used to project the attach point onto the surface.  Leaving this field
blank will use a vector normal to the surface.

A vector may be selected from the screen, a label typed in, or the i,j,k
components typed in.

Start Angle:
------------
This field is only active when the projection type is Atangle and specifies
the angular offset from the projection normal to use at the start of the
projection.

End Angle:
----------
This field is only active when the projection type is Atangle and specifies
the angular offset from the projection normal to use at the end of the geometry
being projected.

Attach Point:
-------------
This field is only active when a continuous projection type is specified
(Wrap, Revolve, Radial).  It selects the initial projection point that will be
used to control the projection of the geometry in order to maintain its shape
and size.  The following types of attach points can be used.

Start   - Use the starting point of the geometry.

Middle  - Use a point half way along the length of the geometry.

End     - Use the ending point of the geometry.

Center  - Use a point calculated as the center of the bounding box of the
          geometry.

Point   - Use a user defined point.

Point
-----
Selects the user defined attach point to be used for continous projections.  A
point may be selected from the screen, a label typed in, or the x,y,z
components typed in.

Controlling Surface
-------------------
When projecting a curve at an angle to the surface/plane, an optional secondary
surface/plane can be used to control the projection normal vectors.  This is
useful when the projection surface has a high curvature.  For example, a true
cone can be projected onto a surface of revolution by specifying a planar
surface as the controlling surface and using the same starting and ending
angles.

Press this button to select the controlling surface or plane used for
controlling the projection normals.  The surface/plane name may also be typed
into the field to the right of this button.

The color choice field specifies the color to use to highlight the selected
surface/plane.


==============
Action Buttons
==============

View
----
Enters dynamic viewing mode.

Preview
-------
Previews the projection without creating permanent geometry.  The projected
geometry can be viewed while the form is active, but it will be deleted when
the form is exited or another projection is created.

Apply
-----
Performs the geometry projection without taking down the form so that other
projections can be made.
