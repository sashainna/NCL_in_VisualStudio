#HEADER#
/TITLE/ NCLIPV Comparison Tolerances
/POSITION/ 50,50
/SIZE/300,90

#EDIT#
/LABEL/ Minimum Surface Grid:
/POSITION/ 10,8, 90,8
/SIZE/ 110,14
/TYPE/ UD_DASINT
/PREC/ 4
/LEN/ 4
/RANGE/ 1,1000

#EDIT#
/LABEL/ Surface Tolerance:
/POSITION/ 145,8, 220,8
/SIZE/ 110,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 6
/RANGE/ .0001,.1

#EDIT#
/LABEL/ Max Angular Deviation:
/POSITION/ 10,25, 90,25
/SIZE/ 110,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ 1.,90.

#EDIT#
/LABEL/ Max Linear Deviation:
/POSITION/ 145,25, 220,25
/SIZE/ 110,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ .001,1000.

#CHECKBOX#
/LABEL/ Surface Grid Only
/POSITION/ 10,42
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Minimum Volume:
/POSITION/ 145,42, 220,42
/SIZE/ 100,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ 0.,1000.

#HELP#
============================
NCLIPV Comparison Tolerances
============================

This form is used to set the various tolerances and Minimum/Maximum limit values
used for comparisons.

Minimum Surface Grid
--------------------
When using surface(s) as the target part, it is first necessary to tessellate
the surface(s) to obtain a mesh of points for the comparison.  This field
specifies the minimum grid to tessellate the surface at.  For example, if 20
is specified, then there will be a minimum of 400 points (20x20) generated for
the comparison.  Specifying a value of 0 will cause the Surface Tolerance to
be used only.

It is recommended that a surface grid be used with surfaces that contain large
flat areas, since using a tolerance only will generate a few points in this
area.

Surface Tolerance
-----------------
Specify the tolerance for tessellating the surface(s).  This can be used in
conjunction with , or in place of, the Minimum Surface Grid value.

Max Angular Deviation
---------------------
During Report style comparisons, a normal vector to the target part at each
mesh point location is intersected with the cut model.  The face of the cut
model is retrieved at this location and the normal for this face is compared
to the normal of the target part used to calculate the intersection.

The Maximum Angular Deviation value specifies the maximum angle between these
two normals that will be allowed for a valid comparison.  If the angle is
greater than this value, then this intersection will not be used during the
comparison.

This feature is used to assist the comparison in determining which faces on the
cut model relate to each mesh point from the target part.  Since the largest
distances to each face are used with the Report style comparison, it is
possible that incorrect faces can be used at the edge locations when a low
value is specified in this field.

Max Linear Deviation
--------------------
This value also assists in Report style comparisons to determine which faces 
relate to each mesh point.  Any face distance to the mesh point that is greater
than this value will not be used in the comparison.

Surface Grid Only
-----------------
Check this box if the surface(s) are to be tessellated using only a grid of
points and not at a tolerance.  This feature is good for typically flat
surfaces.  Surfaces that contain a great deal of curvature should always be
tessellated within a tolerance and optionally with a grid also.

Minimum Volume
--------------
During the Undercuts and Overcuts comparisons, difference solids will be created
wherever the cut model and target part (STL file) do not match.  In a typical
application where the cut model is being compared to a defined part (not
machined), then it is likely that the majority of the parts will have some type
of mismatch.

This value determines the minimum volume of a difference solid that will be
used in the comparison.  Any difference areas that fall below this volume will
not be part of the comparison.

