#HEADER#
/TITLE/ NCL Measurement Tolerances
/POSITION/ 50,50
/SIZE/280,85

#EDIT#
/LABEL/ Minimum Surface Grid:
/POSITION/ 10,8, 90,8
/SIZE/ 110,14
/TYPE/ UD_DASINT
/PREC/ 4
/LEN/ 4
/RANGE/ 0,1000

#EDIT#
/LABEL/ Surface Tolerance:
/POSITION/ 145,8, 220,8
/SIZE/ 110,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 6
/RANGE/ .0001,1.

#CHECKBOX#
/LABEL/ Surface Grid Only
/POSITION/ 10,25
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Radius Tolerance:
/POSITION/ 145,25, 220,25
/SIZE/ 110,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 6
/RANGE/ .0001,1.

#EDIT#
/LABEL/ Max Meas Range:
/POSITION/ 145,42, 220,42
/SIZE/ 110,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 8
/RANGE/ 0.,1000.
#HELP#
==========================
NCL Measurement Tolerances
==========================
This form allows you to change various tolerances and settings used for geometry
measurements.

Minimum Surface Grid:
---------------------
Specifies the minimum number of points in the U and V directions used for
comparing distances to a surface.  Specifying a value of zero will not generate
any grid points when measuring surfaces.

Surface Tolerance:
------------------
Specifies the tolerance used to create points in the U and V directions for
comparing distances to a surface.

Surface Grid Only:
------------------
When this box is checked, then only the surface grid will be used to compare
distances to a surface.  Measurement points within a tolerance of the surface
will not be used.  Uncheck this box if you would like to use both the grid and
tolerance generated points for surface measurements.

Radius Tolerance:
------------------
Specifies the tolerance used to determine if the section of the surface being
analyzed contains a circular arc (radius) in either the U or V direction.

Max Meas Range:
---------------
Surface distances will only be compared within this distance of the point
picked on the surface.
