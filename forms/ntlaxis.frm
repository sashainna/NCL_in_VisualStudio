#HEADER#
/TITLE/ Tool Axis Settings
/POSITION/ 50,50
/SIZE/330,220

#SECTION#
/NAME/ Fixed
/COLOR/ *DEFAULT

#LABEL#
/LABEL/ Fixed Tool Axis
/POSITION/ 10,10
/SIZE/100,17
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ Tool Axis Mode:
/POSITION/ 10,27
/SIZE/100,40
/TYPE/UD_DASSTRING
/CHOICES/ "Same","Fixed","View"

#EDIT#
/LABEL/ Vector:
/POSITION/ 10,44
/SIZE/130,14
/TYPE/ UD_SCAVEC2
/PREC/ 24
/LEN/ 24
/INPUT/ FORM_PICK
/LIMIT/ VECTOR,PNTVEC

#PICTUREBOX#
/FILE/ ta_fixed.jpg
/NAME/ ta_fixed
/POSITION/ 180,10
/SIZE/ 141,125

#CHECKBOX#
/LABEL/ Normal to Part Surface
/POSITION/ 10,61
/SIZE/100,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Tlaxis Modification
/POSITION/ 10,78
/SIZE/130,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Translation up the Tool Axis:
/POSITION/ 10,95, 135,95
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -8

#EDIT#
/LABEL/ Translation in the Forward Direction:
/POSITION/ 10,112, 135,112
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -8

#EDIT#
/LABEL/ Translation to the Right:
/POSITION/ 10,129, 135,129
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -8

#EDIT#
/LABEL/ Forward Tilt Angle:
/POSITION/ 10,146, 80,146
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -8

#EDIT#
/LABEL/ Right Tilt Angle:
/POSITION/ 10,163, 80,163
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -8

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Tlaxis_fixed.mp4
/POSITION/ 270,180
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#SECTION#
/NAME/ Tanto DS
/COLOR/ DEFAULT

#LABEL#
/LABEL/ Tanto Drive Surface
/POSITION/ 10,10
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Contact Height:
/POSITION/ 10,27
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#CHECKBOX#
/LABEL/ Parallel to Elements
/POSITION/ 10,44
/SIZE/100,14
/TYPE/UD_DASSTRING

#PICTUREBOX#
/FILE/ ta_tanto.jpg
/NAME/ ta_tanto
/POSITION/ 180,10
/SIZE/ 141,125

#EDIT#
/LABEL/ Control Surface:
/POSITION/ 10,61
/SIZE/120,14
/TYPE/ UD_DASSTRING
/PREC/ 18
/LEN/ 18
/INPUT/ FORM_PICK
/LIMIT/ PLANE,SURF

#CHECKBOX#
/LABEL/ 4-axis Control
/POSITION/ 10,78
/SIZE/100,14
/TYPE/UD_DASSTRING


#EDIT#
/LABEL/ Perpto Vector:
/POSITION/ 10,95
/SIZE/130,14
/TYPE/ UD_SCAVEC2
/PREC/ 18
/LEN/ 18
/INPUT/ FORM_PICK
/LIMIT/ VECTOR,PNTVEC

#CHECKBOX#
/LABEL/ Maintain Perpto Vector after Modifiers
/POSITION/ 10,112
/SIZE/130,14
/TYPE/UD_DASSTRING

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Tlaxis_Tan.mp4
/POSITION/ 270,180
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#SECTION#
/NAME/ Fan
/COLOR/ DEFAULT

#LABEL#
/LABEL/ Fan to Check Surface
/POSITION/ 10,10
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Contact Height:
/POSITION/ 10,27
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#PICTUREBOX#
/FILE/ ta_fan.jpg
/NAME/ ta_fan
/POSITION/ 180,10
/SIZE/ 141,125

#CHOICEBOX#
/LABEL/ Control Fan at Tool Center
/POSITION/ 10,44
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","On","Auto"

#CHECKBOX#
/LABEL/ Smooth Interpolation
/POSITION/ 10,61
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Degree of Fan:
/POSITION/ 10,78
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#EDIT#
/LABEL/ Rate of Fan:
/POSITION/ 10,95
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Tlaxis_Fan.mp4
/POSITION/ 270,180
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#SECTION#
/NAME/ Combine
/COLOR/ DEFAULT

#LABEL#
/LABEL/ Combine Fan and Tanto Drive Surface
/POSITION/ 10,10
/SIZE/135,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Contact Height:
/POSITION/ 10,27
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#CHECKBOX#
/LABEL/ Parallel to Elements
/POSITION/ 10,44
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Departure Distance:
/POSITION/ 10,61
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#EDIT#
/LABEL/ Approach Distance:
/POSITION/ 10,78
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#PICTUREBOX#
/FILE/ ta_combin.jpg
/NAME/ ta_combin
/POSITION/ 180,10
/SIZE/ 141,125

#EDIT#
/LABEL/ Control Surface:
/POSITION/ 10,95
/SIZE/120,14
/TYPE/ UD_DASSTRING
/PREC/ 18
/LEN/ 18
/INPUT/ FORM_PICK
/LIMIT/ PLANE,SURF

#CHOICEBOX#
/LABEL/ Control Fan at Tool Center
/POSITION/ 10,112
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","On","Auto"

#CHECKBOX#
/LABEL/ Smooth Interpolation
/POSITION/ 10,129
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Degree of Fan:
/POSITION/ 10,146
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#EDIT#
/LABEL/ Rate of Fan:
/POSITION/ 10,163
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Tlaxis_Combin.mp4
/POSITION/ 270,180
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#SECTION#
/NAME/ Normal PS
/COLOR/ DEFAULT

#LABEL#
/LABEL/ Normal to Part Surface
/POSITION/ 10,10
/SIZE/100,14
/TYPE/UD_DASSTRING

#PICTUREBOX#
/FILE/ ta_normal.jpg
/NAME/ ta_normal
/POSITION/ 180,10
/SIZE/ 141,125

#EDIT#
/LABEL/ Control Surface:
/POSITION/ 10,27
/SIZE/120,14
/TYPE/ UD_DASSTRING
/PREC/ 18
/LEN/ 18
/INPUT/ FORM_PICK
/LIMIT/ PLANE,SURF

#CHECKBOX#
/LABEL/ 4-axis Control
/POSITION/ 10,44
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Perpto Vector:
/POSITION/ 10,61
/SIZE/130,14
/TYPE/ UD_SCAVEC2
/PREC/ 18
/LEN/ 18
/INPUT/ FORM_PICK
/LIMIT/ VECTOR,PNTVEC

#CHECKBOX#
/LABEL/ Maintain Perpto Vector after Modifiers
/POSITION/ 10,78
/SIZE/130,14
/TYPE/UD_DASSTRING

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Tlaxis_NPs.mp4
/POSITION/ 270,180
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#SECTION#
/NAME/ Atangle PS
/COLOR/ DEFAULT

#LABEL#
/LABEL/ Atangle to Part Surface
/POSITION/ 10,10
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Angle:
/POSITION/ 10,27
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#EDIT#
/LABEL/ Heel Clearance Distance:
/POSITION/ 10,44
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#PICTUREBOX#
/FILE/ ta_atangl.jpg
/NAME/ ta_atangl
/POSITION/ 180,10
/SIZE/ 141,125

#EDIT#
/LABEL/ Control Surface:
/POSITION/ 10,61
/SIZE/120,14
/TYPE/ UD_DASSTRING
/PREC/ 18
/LEN/ 18
/INPUT/ FORM_PICK
/LIMIT/ PLANE,SURF

#CHECKBOX#
/LABEL/ Maintain Contact Point on Drive Surface
/POSITION/ 10,78
/SIZE/140,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ 4-axis Control
/POSITION/ 10,95
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Perpto Vector:
/POSITION/ 10,112
/SIZE/130,14
/TYPE/ UD_SCAVEC2
/PREC/ 18
/LEN/ 18
/INPUT/ FORM_PICK
/LIMIT/ VECTOR,PNTVEC

#CHECKBOX#
/LABEL/ Maintain Perpto Vector after Modifiers
/POSITION/ 10,129
/SIZE/130,14
/TYPE/UD_DASSTRING

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Tlaxis_APS.mp4
/POSITION/ 270,180
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#SECTION#
/NAME/ Thru Point
/COLOR/ *DEFAULT

#LABEL#
/LABEL/ Thru a Point
/POSITION/ 10,10
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Point:
/POSITION/ 10,27
/SIZE/120,14
/TYPE/ UD_SCACART2
/PREC/ 24
/LEN/ 24
/INPUT/ FORM_PICK
/LIMIT/ POINT,PNTVEC

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Tlaxis_TPs.mp4
/POSITION/ 270,180
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ ta_point.jpg
/NAME/ ta_point
/POSITION/ 180,10
/SIZE/ 141,125

#SECTION#
/NAME/ Thru Curve
/COLOR/ *DEFAULT

#LABEL#
/LABEL/ Thru a Curve
/POSITION/ 10,10
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Curve:
/POSITION/ 10,27
/SIZE/120,14
/TYPE/ UD_DASSTRING
/PREC/ 24
/LEN/ 24
/INPUT/ FORM_PICK
/LIMIT/ CURVE,CIRCLE,LINE

#PICTUREBOX#
/FILE/ ta_curve.jpg
/NAME/ ta_curve
/POSITION/ 180,10
/SIZE/ 141,125

#EDIT#
/LABEL/ Approximate Length of Move:
/POSITION/ 10,44,105,44
/SIZE/130,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Tlaxis_TCv.mp4
/POSITION/ 270,180
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#SECTION#
/NAME/ Interpolate
/COLOR/ *DEFAULT

#LABEL#
/LABEL/ Interpolate to a Vector
/POSITION/ 10,10
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Vector:
/POSITION/ 10,27
/SIZE/130,14
/TYPE/ UD_SCAVEC2
/PREC/ 24
/LEN/ 24
/INPUT/ FORM_PICK
/LIMIT/ VECTOR,PNTVEC

#PICTUREBOX#
/FILE/ ta_interp.jpg
/NAME/ ta_interp
/POSITION/ 180,10
/SIZE/ 141,125

#CHECKBOX#
/LABEL/ Smooth Interpolation
/POSITION/ 10,44
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Degree of Fan:
/POSITION/ 10,61
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#EDIT#
/LABEL/ Rate of Fan:
/POSITION/ 10,78
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Tlaxis_Inter.mp4
/POSITION/ 270,180
/SIZE/ 50,14
/TYPE/ UD_DASSTRING


#SECTION#
/NAME/ ALL
/COLOR/ BLACK

#CHECKBOX#
/LABEL/ 
/POSITION/ 180, 160
/SIZE/15,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Tlaxis Modification
/POSITION/ 195, 160
/SIZE/90,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Force Output of Tlaxis Command
/POSITION/ 180, 143
/SIZE/180,14
/TYPE/UD_DASSTRING

#FRAME#
/TITLE/
/POSITION/ 5,154
/SIZE/ 310,78

#HELP#
<Fixed>
Selecting this section will lock the tool axis at a fixed vector.

Tool Axis Mode:
---------------
The fixed tool axis can be specfied as the current tool axis vector (Same),
a user defined vector (Fixed), or as the active view normal from the graphics
viewport (if more than one viewport is displayed, then you will be prompted
to select the viewport to use).

Vector:
-------
Displays the active tool axis vector and allows you to enter a tool axis vector
when the Tool Axis Mode is set to Fixed.

Normal to Part Surface
----------------------
Checking this box will cause NCL to calculate the tool end point using the
active fixed tool axis.  After calculating this point, the tool axis will be
modified so that it is normal to the part surface.  The tool will not be
adjusted to remain in contact with the part surface after modifying the tool
axis.

Tlaxis Modification
------------------------------------
Checking this box can have additional modifications made to them.
The optional modifications are used to offset the tool position and angle
from the calculated position as the tool is driven along the control surfaces.
Check this box if you want these modifications applied to the tool position
and/or orientation.

As a warning, these modifications will be made to the tool position/orientation
after the proper location has been calculated to apply the tool to the control
surfaces.  No check to determine the tool position in relationship to the
control surfaces is made after applying these modifications, so care must be
taken to ensure the part is not gouged.

Translation up the Tool Axis:
-----------------------------
Enter the distance you want the tool end point translated up the tool axis.  A
negative value can be entered.

Translation in the Forward Direction:
-------------------------------------
Enter the distance you want the tool end point translated along the forward
direction of the move.  A positive value will move the tool in the forward
direction, while a negative value will move the tool in the negative direction.

Translation to the Right:
-------------------------
Enter the distance you want the tool end point translated to the side of the
tool path.  A positive value will move the tool to the right of the tool path,
while a negative value will move the tool to the left.

Forward Tilt Angle:
-------------------
Enter the angle in degrees to tilt the tool along the forward direction of the
move.  A positive value will tilt the tool in the forward direction, while a
negative value will tilt the tool in the negative direction.

Right Angle:
------------
Enter the angle in degrees to tilt the tool to the side of the move.  A positive
value will tilt the tool the to the right, while a negative value will tilt the
tool to the left.

Force Output of TLAXIS command:
-------------------------------
Checking this box will force the minimum command output even the input values are not changed at all.

Tlaxis Modification:
--------------------
Checking this box will open the “TLAXIS Modification” form to add more command to the main Tool Axis Command.

Video Button:
-------------
Click the video button will play the a video file.

<END_SEC>
<Tanto DS>
Selecting this section specifies that the tool will be tangent to the drive surface
and perpendicular to the part surface.

Contact Height:
---------------
Specifies the height up the cutter to remain tangent to the drive surface.

Parallel to Elements
--------------------
Checking this box will cause NCL to remain parallel to the iso-parameteric
lines of the drive surface instead of maintaining a perpendicular orientation
to the part surface.

Control Surface:
----------------
The tool can be maintained perpendicular to a secondary surface/plane other
than the part surface the tool is currently applied to.  This feature is
typically used when the primary part surface will cause the tool axis to
vary too much.  Pressing this button allows you to select the controlling
part surface for the tool axis or you can type the label of the surface/plane
into the text field.

4-axis Control
--------------
Check this box if the tool axis must be maintained in a 4-axis mode, typically
because the machine only contains a single rotary axis.

Perpto Vector:
--------------
Select or enter the vector that the tool axis will remain perpendicular to.
This vector is typically the same as the vector that the rotary axis rotates
about.

Maintain Perpto Vector after Modifiers:
---------------------------------------
This box will usually be checked, since it waits until the tool axis modifiers
(tilt angles, guide curve, etc.) are applied until adjusting the tool axis
for the perpindicular vector.  This causes the tool axis to fully respect the
vector.  If this box is not checked, then the tool axis will be adjusted for
the perpendicular vector and then adjusted for the tool axis modifiers,
possibly invalidating the vector constraint.

Force Output of TLAXIS command:
-------------------------------
Checking this box will force the minimum command output even the input values are not changed at all.

Tlaxis Modification:
--------------------
Checking this box will open the “TLAXIS Modification” form to add more command to the main Tool Axis Command.

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
<Fan>
Selecting this section specifies that the tool axis control will remain tangent to
the drive surface while approaching the check surface with a fanning motion.

Contact Height:
---------------
Specifies the height up the cutter to remain tangent to the drive surface.


Control Fan at Tool Center:
---------------------------
Checking this box will cause NCL to calculate the tool axis and forward
direction based on the center of the tool ring.  The tool ring center is
defined as the center of the tool at the height of the corner radius.

This feauture is useful when the tool tip may move backwards while fanning to
the check surface, which could cause a motion failure.  'Off' disables this
feature and uses the tool end point, 'On' uses the tool ring center for
calculations for the entire move, and Auto will use the tool tip unless it is
deemed necessary to use the tool ring center, normally when the tool approaches
the check surface while still requiring a large tool axis change.

Smooth Interpolation
--------------------
Disabling 'Smooth Interpolation' causes the fanning motion of the tool to have
a standard rate of change in the tool axis from the start to the end.  Checking
this box causes the fanning motion to have less angular change at the beginning
and ending of the motion and more angular change in the middle part of the
motion.

Degree of Fan:
--------------
Sets the polynomial equation degree for fanning motion when 'Smooth
Interpolation' is enabled.  The higer the degree, the less the angular change
will be at the beginning and ending of the move.  The value should be set higher
than 1 and is typically set to 5 to create a nice transition along the fanning
move.

Rate of Fan:
------------
This value can be between 0 and 1.  A value of 1 will generate a change that is
consistent with the standard polynomial equation defined by the Degree of Fan,
while a lesser value will generate a lesser change in the tool axis during the
middle of the motion and more of a change at the beginning and ending of the
motion.

Force Output of TLAXIS command:
-------------------------------
Checking this box will force the minimum command output even the input values are not changed at all.

Tlaxis Modification:
--------------------
Checking this box will open the “TLAXIS Modification” form to add more command to the main Tool Axis Command.

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
<Combine>
Selecting this section enables a tool axis mode that will fan away from the current check surface for a specified distance,
drive tangent to the drive surface for a calculated distance,
and then fan into the check surface for a specified distance.

Contact Height:
---------------
Specifies the height up the cutter to remain tangent to the drive surface.

Parallel to Elements
--------------------
Checking this box will cause NCL to remain parallel to the iso-parameteric
lines of the drive surface instead of maintaining a perpendicular orientation
to the part surface.

Departure Distance:
-------------------
Enter the distance that the tool will fan away from the current check surface.
A value of 0 can be specified, which will disable this beginning fanning move.

Approach Distance:
------------------
Enter the distance that the tool will fan into the check surface.  A value of 0
can be specified, which will disable this ending fanning move.


Control Surface:
----------------
The tool can be maintained perpendicular to a secondary surface/plane other
than the part surface the tool is currently applied to.  This feature is
typically used when the primary part surface will cause the tool axis to
vary too much.  Pressing this button allows you to select the controlling
part surface for the tool axis or you can type the label of the surface/plane
into the text field.

Control Fan at Tool Center:
---------------------------
Checking this box will cause NCL to calculate the tool axis and forward
direction based on the center of the tool ring.  The tool ring center is
defined as the center of the tool at the height of the corner radius.

This feauture is useful when the tool tip may move backwards while fanning to
the check surface, which could cause a motion failure.  'Off' disables this
feature and uses the tool end point, 'On' uses the tool ring center for
calculations for the entire move, and Auto will use the tool tip unless it is
deemed necessary to use the tool ring center, normally when the tool approaches
the check surface while still requiring a large tool axis change.

Smooth Interpolation
--------------------
Disabling 'Smooth Interpolation' causes the fanning motion of the tool to have
a standard rate of change in the tool axis from the start to the end.  Checking
this box causes the fanning motion to have less angular change at the beginning
and ending of the motion and more angular change in the middle part of the
motion.

Degree of Fan:
--------------
Sets the polynomial equation degree for fanning motion when 'Smooth
Interpolation' is enabled.  The higer the degree, the less the angular change
will be at the beginning and ending of the move.  The value should be set higher
than 1 and is typically set to 5 to create a nice transition along the fanning
move.

Rate of Fan:
------------
This value can be between 0 and 1.  A value of 1 will generate a change that is
consistent with the standard polynomial equation defined by the Degree of Fan,
while a lesser value will generate a lesser change in the tool axis during the
middle of the motion and more of a change at the beginning and ending of the
motion.

Force Output of TLAXIS command:
-------------------------------
Checking this box will force the minimum command output even the input values are not changed at all.

Tlaxis Modification:
--------------------
Checking this box will open the “TLAXIS Modification” form to add more command to the main Tool Axis Command.

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
<Normal PS>
Selecting this section specifies that the tool will remain normal to the part surface.

Control Surface:
----------------
The tool can be maintained perpendicular to a secondary surface/plane other
than the part surface the tool is currently applied to.  This feature is
typically used when the primary part surface will cause the tool axis to
vary too much.  Pressing this button allows you to select the controlling
part surface for the tool axis or you can type the label of the surface/plane
into the text field.

4-axis Control
--------------
Check this box if the tool axis must be maintained in a 4-axis mode, typically
because the machine only contains a single rotary axis.

Perpto Vector:
--------------
Select or enter the vector that the tool axis will remain perpendicular to.
This vector is typically the same as the vector that the rotary axis rotates
about.

Maintain Perpto Vector after Modifiers:
---------------------------------------
This box will usually be checked, since it waits until the tool axis modifiers
(tilt angles, guide curve, etc.) are applied until adjusting the tool axis
for the perpindicular vector.  This causes the tool axis to fully respect the
vector.  If this box is not checked, then the tool axis will be adjusted for
the perpendicular vector and then adjusted for the tool axis modifiers,
possibly invalidating the vector constraint.

Force Output of TLAXIS command:
-------------------------------
Checking this box will force the minimum command output even the input values are not changed at all.

Tlaxis Modification:
--------------------
Checking this box will open the “TLAXIS Modification” form to add more command to the main Tool Axis Command.

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
<Atangle PS>
Selecting this section specifies that the tool will remain tilted to the part surface by the specified angle.

Angle:
------
Type in the angle that the tool should be tilted along the forward direction.
A positive number tilts the tool in the forward direction of the move and a
negative value tilts the tool backwards from the forward direction.

Heel Clearance Distance:
------------------------
Enter a distance that the heel of the tool should be kept away from the part
surface for each calculated location.  The tool axis will be further tilted to
make sure that the heel of the tool never gets closer than this distance to the
part surface.


Control Surface:
----------------
The tool can be maintained perpendicular to a secondary surface/plane other
than the part surface the tool is currently applied to.  This feature is
typically used when the primary part surface will cause the tool axis to
vary too much.  Pressing this button allows you to select the controlling
part surface for the tool axis or you can type the label of the surface/plane
into the text field.

4-axis Control
--------------
Check this box if the tool axis must be maintained in a 4-axis mode, typically
because the machine only contains a single rotary axis.

Perpto Vector:
--------------
Select or enter the vector that the tool axis will remain perpendicular to.
This vector is typically the same as the vector that the rotary axis rotates
about.

Maintain Perpto Vector after Modifiers:
---------------------------------------
This box will usually be checked, since it waits until the tool axis modifiers
(tilt angles, guide curve, etc.) are applied until adjusting the tool axis
for the perpindicular vector.  This causes the tool axis to fully respect the
vector.  If this box is not checked, then the tool axis will be adjusted for
the perpendicular vector and then adjusted for the tool axis modifiers,
possibly invalidating the vector constraint.

Force Output of TLAXIS command:
-------------------------------
Checking this box will force the minimum command output even the input values are not changed at all.

Tlaxis Modification:
--------------------
Checking this box will open the “TLAXIS Modification” form to add more command to the main Tool Axis Command.

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
<Thru Point>
Selecting this section will cause the tool axis to always pass through the specified point. The tool axis will pass through the closest end point of the curve at the start of the move and will move proportionally along the curve during the move until it passes through the other end point of the curve at the end of the move.

Point:
------
Select or enter the label of the point that the tool axis will pass through.

Force Output of TLAXIS command:
-------------------------------
Checking this box will force the minimum command output even the input values are not changed at all.

Tlaxis Modification:
--------------------
Checking this box will open the “TLAXIS Modification” form to add more command to the main Tool Axis Command.

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
<Thru Curve>
Selecting this section will cause the tool axis to always pass through the specified point.

Curve:
------
Select or enter the label of the curve that the tool axis will pass through.


Approximate Length of Move:
---------------------------
If the length of the move is much greater than the initial distance of the tool
to the check surface, then NCL may get an error completing the motion.  In this
case you should enter the approximate length of the curve to assist in the
motion calculations.

Force Output of TLAXIS command:
-------------------------------
Checking this box will force the minimum command output even the input values are not changed at all.

Tlaxis Modification:
--------------------
Checking this box will open the “TLAXIS Modification” form to add more command to the main Tool Axis Command.

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
<Interpolate>
Selecting this section causes the tool axis to start at the current tool axis and interpolate (fan) to the specified vector on the next move.

Vector:
-------
Select or enter the vector that the tool axis will interpolate to.


Smooth Interpolation
--------------------
Disabling 'Smooth Interpolation' causes the fanning motion of the tool to have
a standard rate of change in the tool axis from the start to the end.  Checking
this box causes the fanning motion to have less angular change at the beginning
and ending of the motion and more angular change in the middle part of the
motion.

Degree of Fan:
--------------
Sets the polynomial equation degree for fanning motion when 'Smooth
Interpolation' is enabled.  The higer the degree, the less the angular change
will be at the beginning and ending of the move.  The value should be set higher
than 1 and is typically set to 5 to create a nice transition along the fanning
move.

Rate of Fan:
------------
This value can be between 0 and 1.  A value of 1 will generate a change that is
consistent with the standard polynomial equation defined by the Degree of Fan,
while a lesser value will generate a lesser change in the tool axis during the
middle of the motion and more of a change at the beginning and ending of the
motion.

Force Output of TLAXIS command:
-------------------------------
Checking this box will force the minimum command output even the input values are not changed at all.

Tlaxis Modification:
--------------------
Checking this box will open the “TLAXIS Modification” form to add more command to the main Tool Axis Command.

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
