#HEADER#
/TITLE/ Motion Calculation Assistance
/POSITION/ 50,50
/SIZE/320,180

#SECTION#
/NAME/ Maxdp
/COLOR/ BLACK

#EDIT#
/LABEL/ Maximum Step:
/POSITION/ 10,10, 90,10
/SIZE/80,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#CHECKBOX#
/LABEL/ Output All Steps
/POSITION/10,27
/SIZE/ 80,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Enable Automatic Settings
/POSITION/10,44
/SIZE/ 120,14
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ 
/POSITION/ 20,61
/SIZE/102,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default","Value"

#EDIT#
/LABEL/ Maximum Attempts:
/POSITION/ 20,78, 95,78
/SIZE/120,14
/TYPE/UD_SCAINT
/PREC/ 7
/LEN/ 7

#EDIT#
/LABEL/ Minimum Step:
/POSITION/ 20,95, 95,95
/SIZE/120,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#CHECKBOX#
/LABEL/ Output Warnings
/POSITION/ 10,112, 90,112
/SIZE/80,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Next Move Only
/POSITION/10,127
/SIZE/ 80,14
/TYPE/UD_DASSTRING

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/maxdp.mp4
/POSITION/ 240,140
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ mo_maxdp.jpg
/NAME/ mo_maxdp
/POSITION/ 145,10
/SIZE/ 150,125

#SECTION#
/NAME/ Iterations
/COLOR/ BLACK

#EDIT#
/LABEL/ Maximum Number of Points:
/POSITION/ 10,10, 100,10
/SIZE/150,14
/TYPE/UD_SCAINT
/PREC/ 7
/LEN/ 7

#EDIT#
/LABEL/ Maximum Angular Change:
/POSITION/ 10,27, 100,27
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#CHECKBOX#
/LABEL/ Next Move Only
/POSITION/10,44
/SIZE/ 80,14
/TYPE/UD_DASSTRING

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/maxang.mp4
/POSITION/ 240,140
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ mo_numpts.jpg
/NAME/ mo_numpts
/POSITION/ 145,10
/SIZE/ 150,125

#SECTION#
/NAME/ Thicks
/COLOR/ BLACK

#EDIT#
/LABEL/ Part Surface Thick:
/POSITION/ 10,10, 95,10
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#EDIT#
/LABEL/ Drive Surface Thick:
/POSITION/ 10,27, 95,27
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#EDIT#
/LABEL/ Check Surface Thick:
/POSITION/ 10,44, 95,44
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#CHECKBOX#
/LABEL/ Multiple Check Thicks
/POSITION/ 10,61
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Check Surface #2:
/POSITION/ 10,78, 95,78
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#EDIT#
/LABEL/ Check Surface #3:
/POSITION/ 10,95, 95,95
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#EDIT#
/LABEL/ Check Surface #4:
/POSITION/ 10,112, 95,112
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#EDIT#
/LABEL/ Check Surface #5:
/POSITION/ 10,129, 95,129
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/thick.mp4
/POSITION/ 240,140
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ mo_thick.jpg
/NAME/ mo_thick
/POSITION/ 145,10
/SIZE/ 150,125

#SECTION#
/NAME/ Tolerances
/COLOR/ BLACK

#EDIT#
/LABEL/ Chordal Tolerance:
/POSITION/ 10,10, 85,10
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#EDIT#
/LABEL/ Positional Tolerance:
/POSITION/ 10,27, 85,27
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#CHECKBOX#
/LABEL/ Auto Position if Out of Tolerance
/POSITION/ 10,44
/SIZE/120,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Omit Out of Toler Initial Move
/POSITION/ 10,61
/SIZE/120,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Linear Tolerance:
/POSITION/ 10,78, 85,78
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#EDIT#
/LABEL/ Angular Tolerance:
/POSITION/ 10,95, 85,95
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -7

#PICTUREBOX#
/FILE/ mo_toler.jpg
/NAME/ mo_toler
/POSITION/ 145,10
/SIZE/ 150,125

#SECTION#
/NAME/ Gouge Check
/COLOR/ BLACK

#CHOICEBOX#
/LABEL/ Part Surface Look Point:
/POSITION/ 10,10, 20,25
/SIZE/125,40
/TYPE/UD_DASSTRING
/CHOICES/ "None", "Optimize Look Point", "+ Front, Center, Back of Tool",
/CHOICES/ "+ Right and Left of Tool"

#CHOICEBOX#
/LABEL/ Drive Surface Look Point:
/POSITION/ 10,42,20,57
/SIZE/125,40
/TYPE/UD_DASSTRING
/CHOICES/ "None", "Along Top of Corner Radius", "+ Surface Optimization"

#CHOICEBOX#
/LABEL/ Check Surface Look Point:
/POSITION/ 10,74, 20,89
/SIZE/125,40
/TYPE/UD_DASSTRING
/CHOICES/ "None", "Middle & Top of Tool", "+ Tanto Surface",
/CHOICES/ "+ Surface Optimization"

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/gougck.mp4
/POSITION/ 240,140
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ mo_gougck.jpg
/NAME/ mo_gougck
/POSITION/ 145,10
/SIZE/ 150,125

#HELP#
<Maxdp>
=====
Maxdp
=====
The Maxdp for section controls the step distance that the tool will take along
the drive surface during a motion calculation.

Maximum Step:
-------------
Enter the length of the step the tool will take when it is looking for the
check surface.  The default step size is 4.0in/100mm and is valid for most
parts.  If the part has a high level of complexity or curvature, then this
value can be lowered to assist NCL in driving the tool along the geometry.

Next Move Only
--------------
Check this box if the changed Maxdp settings should only be applied to the next
motion.  The previous Maxdp settings will be restored after this motion
completes.

Output All Steps
----------------
Check this box when NCL should output all calculated motion steps to the
clfile.  Typically, only the required steps to keep the tool in tolerance
with the part will be output, minimizing the number of tool locations output.
This setting is typically only used when driving geometry during a DNTCUT
sequence and with GENPTS enabled, in order to obtain points to use for creating
geometry or checking the part.

Enable Automatic Settings
-------------------------
The are the rare times when NCL will have difficulty calculating a tool path
on a difficult part.  The Maxdp Automatic Settings feature allows NCL to
automatically lower the step size when it is having problems finding the check
surface.  This setting is typically enabled and will have no adverse effects
on the generated tool path.

Minimum Step:
-------------
Enter the minimum step size that NCL should set when automatically lowering
the Maxdp step due to the Automatic Maxdp Settings.  NCL will continually
try lower step values until either the motion completes successfully or
this step length is reached.

Maximum Attempts:
-----------------
Enter the maximum number of attempts of lowering the Maxdp step that NCL will 
try during the Automatic Maxdp Settings.  This value in combinations with the
Minimum Step value controls how many times NCL will attempt to complete the
motion with lesser step sizes.

Output Warnings
---------------
Check this box if a warning should be output whenever the Maxdp step value
is changed automatically by NCL in attempt to find the check surface.  This
setting is typically disabled.
<END_SEC>
<Iterations>
==========
Iterations
==========
The Iterations section defines the maximum number of points that can be
generated in a single motion and the maximum angular tool axis change allowed
in a single move.

Maximum Number of Points:
-------------------------
Enter the maximum number of points that can be generated in a single motion.
When the number of generated tool locations exceeds this value, then the
motion will be aborted with an error.  The higher this number the longer it
will take NCL to report a motion failure if the check surface cannot be
found.  A value of 400-1000 is normally used.

Maximum Angular Change:
-----------------------
Enter the maximum angle (in degrees) between two successive tool axis vectors
output by NCL during continuous path motion.  If a move causes the tool axis
to move more than this amount, then multiple tool locations will be output so
the angular change in the tool axis does not exceed this value.

Next Move Only:
---------------
Check this box if the changed settings should only be applied to the next
motion.  The previous settings will be restored after this motion completes.
<END_SEC>
<Thicks>
======
Thicks
======
The Thicks section defines the distance that the tool will be offset from the
part, drive, and check surface(s).  Thick values are typically used to leave
extra material on the part.  The motion can be driven with a thick value to
rough the part and then the same motion can be used with a thick value of 0
to finish the part.

Part Surface Thick:
-------------------
Enter the distance to offset the tool from the part surface.

Drive Surface Thick:
--------------------
Enter the distance to offset the tool from the drive surface.

Check Surface Thick:
--------------------
Enter the distance to offset the tool from the check surface.

Multiple Check Thicks
---------------------
NCL has the ability to check to 5 surfaces in a single motion.  Checking this
box allows you to apply a separate offset value for each of the check surfaces.

Check Surface #n:
-----------------
Enter the distance to offset the tool from the multiple check surfaces.  If a
field is left blank, then the value from the first check surface thick field
will be used.
<END_SEC>
<Tolerances>
==========
Tolerances
==========
The Tolerances section contains the tolerance values used when calculating
motion.

Chordal Tolerance:
------------------
The chordal tolerance specifies the maximum deviation that a single move can
be away from the geometry being driven.  The maximum distance is typically
at the midpoint of the move.

Positional Tolerance:
---------------------
Enter the maximum distance that the tool can be from the part, drive, and
check surfaces at the end of each single tool movement.  This value is
normally the same or less that the chordal tolerance.

Auto Position if Out of Tolerance
---------------------------------
Check this box if you want NCL to automatically move the tool to its calculated
position at the beginning of a move if it is deemed to be out of tolerance
with the drive geometry.  Unchecking this box will cause a warning message to
be output and give you the ability to manually determine if the tool should be
moved.

Omit Out of Toler Initial Move
------------------------------
Check this box if the initial move to the calculated tool position at the start
of a motion, when the tool is out of tolerance to the drive geometry, should
NOT be output to the clfile.  Checking this box will typically result in
smoother motion being generated when adjusting for out of tolerance moves.
Unchecking this box will cause the initial move to the drive geometry to be
output when the tool is deemed to be out of tolerance at the start of the
motion.

Linear Tolerance:
-----------------
Enter the distance that determines when the tool is considered out of tolerance
with the drive geometry at the start of a motion.  If the calculated position
is greater than this distance from the current tool position, then the tool
is considered to be out of tolerance with the drive geometry.

Angular Tolerance:
------------------
Enter the tool axis angular difference that determines when the tool is
considered out of tolerance with the drive geometry at the start of a motion.
If the calculated tool axis is greater than this angle from the current tool
axis, then the tool is considered to be out of tolerance with the drive
geometry.
<END_SEC>
<Gouge Check>
===========
Gouge Check
===========
The Gouge Check section controls how the tool is applied to the drive geometry
during a motion.  Additional gouge checking is sometimes necessary when
driving complex geometry, for example when driving a contoured part surface
where one side of the tool is in contact with the part surface, while the
other side of the tool violates the part surface.

Gouge checking is performed by checking additional "look points" on the
defined tool.  This process will slow the calculation time for motion
generation.

Part Surface Look Point:
------------------------
Enter the gouge checking level to be used with the part surface.  'None' will
use the tool look point from the previous motion to position the tool to
the part surface.  "Optimize Look Point" will calculate a new look point with
every motion step.  "+ Front, Center, Back of Tool" will add look points along
the front, center, and back of the tool.  "+ Right and Left of Tool" will add
additional look points at the right and left of the tool compared to the
forward direction of the tool.

Drive Surface Look Point:
-------------------------
Enter the gouge checking level to be used with the drive surface.  'None' will
calculate a new look point with every motion step.  "Along Top of Corner Radius"
will calculate an additional look points along the top of the corner radius
(tool ring) of the tool.  "+ Surface Optimization" will determine the drive
surface contact point based on the surface projection that is closest to the
actual tool.

Check Surface Look Point:
-------------------------
Enter the gouge checking level to be used with the check surface.  'None' will
calculate a new look point with every motion step.  "Middle & Top of Tool"
will calculate an additional look points at various heights of the tool; the
tool end point, corner radius, middle of the tool, and at the top of the tool.
"+ Tanto Surface" offers better gouge checking when driving tangent to a
surface.  "+ Surface Optimization" will determine the check surface contact
point based on the surface projection that is closest in the direction of the
forward vector.

