#HEADER#
/TITLE/ Tool Axis Modifications
/POSITION/ 50,50
/SIZE/330,180

#SECTION#
/NAME/ Tilt Angles
/COLOR/ *DEFAULT

#CHECKBOX#
/LABEL/ Apply Tilt Angles
/POSITION/ 10,10
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Forward Angle:
/POSITION/ 10,27
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#EDIT#
/LABEL/ Right Angle:
/POSITION/ 10,44
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#PICTUREBOX#
/FILE/ ta_tilt.jpg
/NAME/ ta_tilt
/POSITION/ 180,10
/SIZE/ 141,125

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Modify_tilt.mp4
/POSITION/ 260,140
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#SECTION#
/NAME/ Guide Curve
/COLOR/ *DEFAULT

#CHECKBOX#
/LABEL/ Enable Guide Curve
/POSITION/ 10,10
/SIZE/130,14
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

#CHOICEBOX#
/LABEL/ Contact Condition:
/POSITION/ 10,44
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "Left","On","Right"

#EDIT#
/LABEL/ Offset Distance:
/POSITION/ 10,61
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#CHECKBOX#
/LABEL/ Remain in Contact with Curve
/POSITION/ 10,78
/SIZE/130,14
/TYPE/UD_DASSTRING

#PICTUREBOX#
/FILE/ ta_guide.jpg
/NAME/ ta_guide
/POSITION/ 180,10
/SIZE/ 141,125

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Modify_curve.mp4
/POSITION/ 260,140
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#SECTION#
/NAME/ Gouge Check
/COLOR/ *DEFAULT

#CHECKBOX#
/LABEL/ Enable Tool Axis Gouge Checking
/POSITION/ 10,10
/SIZE/130,14
/TYPE/UD_DASSTRING

#PICTUREBOX#
/FILE/ ta_gougck.jpg
/NAME/ ta_gougck
/POSITION/ 180,10
/SIZE/ 141,125

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Modify_check.mp4
/POSITION/ 260,140
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#SECTION#
/NAME/ Lock Tlaxis
/COLOR/ *DEFAULT

#CHECKBOX#
/LABEL/ Lock Tool Axis
/POSITION/ 10,10
/SIZE/130,14
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ Lock Tool Axis at
/POSITION/ 10,27
/SIZE/155,40
/TYPE/UD_DASSTRING
/CHOICES/ "End of Move","Start and End of Move","Start of Move"

#CHOICEBOX#
/LABEL/ Lock Distance Specified as
/POSITION/ 10,44
/SIZE/155,40
/TYPE/UD_DASSTRING
/CHOICES/ "Linear","Radius"

#EDIT#
/LABEL/ Tool Axis Locked Distance:
/POSITION/ 10,61
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#CHECKBOX#
/LABEL/ Enable Transition Moves
/POSITION/ 10,78
/SIZE/130,14
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ Transition Move Strategy:
/POSITION/ 10,95
/SIZE/155,40
/TYPE/UD_DASSTRING
/CHOICES/ "Interpolate","Fan"

#EDIT#
/LABEL/ Transition Move Distance:
/POSITION/ 10,112
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -10

#PICTUREBOX#
/FILE/ ta_lock.jpg
/NAME/ ta_lock
/POSITION/ 180,10
/SIZE/ 141,125

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Modify_lock.mp4
/POSITION/ 260,140
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#SECTION#
/NAME/ Modify
/COLOR/ *DEFAULT

#CHECKBOX#
/LABEL/ Modify Tool Position and Orientation
/POSITION/ 10,10
/SIZE/130,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Translation up the Tool Axis:
/POSITION/ 10,27, 135,27
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -8

#EDIT#
/LABEL/ Translation in the Forward Direction:
/POSITION/ 10,44, 135,44
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -8

#EDIT#
/LABEL/ Translation to the Right:
/POSITION/ 10,61, 135,61
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -8

#EDIT#
/LABEL/ Forward Tilt Angle:
/POSITION/ 10,78, 80,78
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -8

#EDIT#
/LABEL/ Right Tilt Angle:
/POSITION/ 10,95, 80,95
/SIZE/120,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -8

#PICTUREBOX#
/FILE/ ta_modify.jpg
/NAME/ ta_modify
/POSITION/ 180,10
/SIZE/ 141,125

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/Modify_mod.mp4
/POSITION/ 260,140
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#

<Tilt Angles>
Apply Tilt Angles
-----------------
All variable tool axis modes can have a forward and right tilt applied to the
tool axis during motion calculations.  Checking this box allows you to enter
the forward and/or right tilt to apply to the tool axis.

Forward Angle:
--------------
Enter the angle in degrees to tilt the tool along the forward direction of the
move.  A positive value will tilt the tool in the forward direction, while a
negative value will tilt the tool in the negative direction.  A value of 0 can
be specified to disable this tilt direction.

Right Angle:
------------
Enter the angle in degrees to tilt the tool to the side of the move.  A positive
value will tilt the tool to the right, while a negative value will tilt the tool
to the left.  A value of 0 can be specified to disable this tilt direction.

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
<Guide Curve>
Enable Guide Curve
------------------
All variable tool axis modes can be modified to maintain a relationship with a
specified guide curve.  The guide curve will assist in preventing the tool from
gouging complex surfaces.  Checking this box allows you to specify the guide
curve and parameters.

Curve:
------
Select or enter the label of the curve that will be used as the guide curve.

Contact Condition:
------------------
Select whether the tool condition will be maintained to the Left or Right of
the guide curve.  Selecting the On option causes the tool axis to always go
through the guide curve.

Offset Distance:
----------------
Enter the distance that the tool should be kept away from the guide curve when
the tool condition has been specified as Left or Right.

Remain in Contact with Curve
----------------------------
Check this box if the tool should always remain in contact (or offset by the
specified distance) from the guide curve when the tool condition has been
specified as Left or Right.  The On condition will always maintain contact with
the guide curve.

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
<Gouge Check>
Enable Tool Axis Gouge Checking
-------------------------------
All variable tool axis modes can have an extra check to help prevent gouging of
the drive surface.  When this box is checked and gouge checking is enabled, then
NCL will tilt the tool axis away from the drive surface to prevent gouging.

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
<Lock Tlaxis>
Lock Tool Axis
--------------
All variable tool axis modes have the option of locking the tool axis when the
tool moves into a corner.  Checking this box enables the locking of the tool
axis at the beginning and ending of a programmed move.

Lock Tool Axis at
-----------------
This field determines when the tool axis should be locked.  Typically you will
only want the tool axis locked at the 'End of Move' when first initiating the
locking mode, so that it is not locked when moving from its current position.
Subsequent moves should be locked at the 'Start and End of Move'.  The final
move should only be locked at the 'Start of Move', when the tool is leaving
its current position.

Lock Distance Specified as
--------------------------
The distance along the beginning/ending of the move can either be specified as
a linear distance or circular radius.

Tool Axis Locked Distance:
--------------------------
Enter the linear distance/circular radius that the tool will be locked at the
beginning/ending of the move.

Enable Transition Moves
-----------------------
Check this box if there should be a smooth transition between the locked tool
axis and the calculated tool axis mode. 

Transition Move Strategy:
-------------------------
The transition move can be Interpolated between the locked tool axis and the
calculated tool axis mode or can be made using Fanning type movement.

Transition Move Distance:
-------------------------
Enter the distance of the transition move between the locked tool axis and the
calculated tool axis mode.

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
<Modify>
Modify Tool Position and Orientation
------------------------------------
Each of the tool axis modes can have additional modifications made to them.
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

Video Button:
-------------
Click the video button will play the a video file.
<END_SEC>
