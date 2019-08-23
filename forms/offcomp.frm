#HEADER#
/TITLE/ Offset Composite Curve Components
/POSITION/ 50,30
/SIZE/ 445,180

#EDIT#
/LABEL/ Curve:
/POSITION/ 8,13,33,11
/SIZE/40,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 64

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 92,10
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PICTURE/ offcomp_1,"Select Curve",32,6,86,79

#CHECKBOX#
/LABEL/ All
/POSITION/ 8,29
/SIZE/ 20,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ From:
/POSITION/ 40,32,65,30
/SIZE/20,13
/TYPE/ UD_DASINT
/RANGE/ 1,100000
/LEN/ 5
/PREC/ -5

#EDIT#
/LABEL/ To:
/POSITION/ 101,32,116,30
/SIZE/20,13
/TYPE/ UD_DASINT
/RANGE/ 1,100000
/LEN/ 5
/PREC/ -5

#PUSHBUTTON#
/LABEL/ Offset Direction
/POSITION/ 8,52
/SIZE/ 75,14
/TYPE/ UD_DASSTRING
/PICTURE/ offcomp_1,"Offset Direction",86,60,92,68

#CHOICEBOX#
/LABEL/
/POSITION/ 88,53
/SIZE/40,33
/TYPE/UD_DASSTRING
/CHOICES/"Xlarge", "Xsmall","Ylarge"
/CHOICES/ "Ysmall","Zlarge","Zsmall","Vector"

#EDIT#
/LABEL/ Distance:
/POSITION/ 133,54,168,52
/SIZE/30,13
/TYPE/ UD_SCAVAL
/RANGE/ 0.,100000.
/LEN/ 6
/PREC/ 10
/PICTURE/ offcomp_1,"Offset Distance",76,90,98,97

#EDIT#
/LABEL/ Offset Direction Vector:
/POSITION/ 8,77,85,75
/SIZE/40,13
/TYPE/ UD_DASSTRING
/LEN/ 9
/PREC/ 64

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 140,74
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Connection Type
/POSITION/ 8,99,70,97
/SIZE/110,33
/TYPE/UD_DASSTRING
/CHOICES/"Linear", "Smooth","Chamfer"
/PICTURE/ offcomp_1,"Linear",1,2,20,31,0
/PICTURE/ offcomp_1,"Smooth",2,35,20,65,1
/PICTURE/ offcomp_1,"Chamfer",2,70,20,98,2

#CHECKBOX#
/LABEL/ Use Existing Label
/POSITION/ 8,114
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Label:
/POSITION/ 125,116,150,114
/SIZE/40,13
/TYPE/ UD_DASSTRING
/LEN/ 9
/PREC/ 64

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 8,133
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 63,133
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 147,133
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ offcomp_1.jpg
/NAME/ offcomp_1
/POSITION/ 210,6
/SIZE/ 225,140

#HELP#
=================================
Offset Composite Curve Components
=================================
This form allows you to offset a chain of components of a composite curve to
define a new composite curve.

Curve:
------
Enter the label for the curve whose components will be offset.

Select:
-------
Use to select the curve whose components will be offset.  The direction to move
along the curve when defining the offset region is determined by picking the
direction vectors when prompted.  The form will automatically determine the
correct starting and ending component order.  The form will also attempt to
automatically determine the offset direction.

If the All box is checked, then you will not be prompted for the direction and
ending component.

All:
----
Check this box if you would like to offset all components of the selected curve.

From:
-----
Enter the index of the component at the beginning of the offset region.

To:
---
Enter the index of the component at the end of the offset region.

Offset Direction:
-----------------
Press the button to select the offset direction or select the offset direction
from the pulldown menu.  The choices are:

 - Xlarge: Base the offset vector on the positive X-axis.
 - Xsmall: Base the offset vector on the negative X-axis.
 - Ylarge: Base the offset vector on the positive Y-axis.
 - Ysmall: Base the offset vector on the negative Y-axis.
 - Zlarge: Base the offset vector on the positive Z-axis.
 - Zsmall: Base the offset vector on the negative Z-axis.
 - Vector: Use the supplied vector as the offset vector.

When using the interface, direction vectors will be displayed and the offset
direction will be generated based on your selection.  The direction vectors are
displayed on the first component of the offset region.

Note that an offset direction parallel to the tangent vector of the curve at the
beginning of the offset region is invalid and an error will be output.

Distance:
---------
Enter the desired offset distance.

Offset Direction Vector:
------------------------
Enter the vector or label of the vector to use when determining the offset
direction. The offset direction will be based on the direction of the offset
direction vector in relation to the first component selected to be offset. As
with the other offset directions, the offset direction using a vector will be
perpendicular to the components offset and not necessarily in the direction of
the offset vector.

Select:
-------
Use to select the offset direction vector.

Connection Type:
----------------
Select the desired connection type.  The connection type is used when connecting
components of the curve that become disconnected during the offset process.
There are three choices for connection:

 - Linear:  Lines will be used to connect components.  Note that if a component
            is a line and trimming/extending the line will connect the line with
            its neighboring component the line is trimmed/extended instead of
            adding another line.

 - Smooth:  B-Splines are used to connect components.  Note that the B-Splines
            are defined using the tangent vectors at the ends of the components
            to be connected.

 - Chamfer: Lines are created from the ends of each component to connect them
            with their neighboring component.  This choice differs from linear
            since lines will not be trimmed/extended.

Use Existing Label:
-------------------
Checking this box will cause the curve being offset to be redefined using the
same geometry label.  If CANON/ON is not in effect when the geometry is created,
then it will be automatically enabled prior to outputting the composite offset
command.  CANON/OFF will then be reinstated when the form is exited.

Label:
------
Enter the label for the new composite curve.

View
----
Enter dynamic viewing mode.

Preview
-------
Preview the composite curve without creating permanent geometry.  The curve can
be viewed while the form is active, but it will be deleted when the form is
exited or another curve is created.

Apply
-----
Create the curve without taking down the form so that other curves can be made.
