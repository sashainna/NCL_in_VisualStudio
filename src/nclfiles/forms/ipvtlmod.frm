#HEADER#
/TITLE/ NCLIPV Tool Modals
/POSITION/ 50,50
/SIZE/200,215

#EDIT#
/LABEL/ Cut Tolerance:
/POSITION/ 10,8
/SIZE/180,14
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -7
/RANGE/ .0001,2.5

#EDIT#
/LABEL/ Maximum angular change:
/POSITION/ 10,25
/SIZE/120,14
/TYPE/UD_DASVAL
/PREC/ 3
/LEN/ -7
/RANGE/ .001,360.

#EDIT#
/LABEL/ Rapid Rate:
/POSITION/ 10,42
/SIZE/120,14
/TYPE/UD_DASVAL
/PREC/ 2
/LEN/ -7
/RANGE/ .0,100000.

#EDIT#
/LABEL/ Translucency:
/POSITION/ 10,59
/SIZE/180,14
/TYPE/UD_DASINT
/PREC/ 3
/LEN/ 3
/RANGE/ 1,100

#CHECKBOX#
/LABEL/ Display Edges
/POSITION/10,76
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/ Edge Color:
/POSITION/ 85,78
/SIZE/90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ Minimum Height:
/POSITION/ 10,93
/SIZE/180,14
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -10
/RANGE/ 0.,100000.

#EDIT#
/LABEL/ Maximum Height:
/POSITION/ 10,110
/SIZE/180,14
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -10
/RANGE/ 0.,100000.

#EDIT#
/LABEL/ Minimum Diameter:
/POSITION/ 10,127
/SIZE/180,14
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -10
/RANGE/ 0.,100000.

#CHOICEBOX#
/LABEL/ Next location is a FROM:
/POSITION/ 10,144
/SIZE/ 125,14
/TYPE/ UD_DASSTRING
/CHOICES/ "No","Yes"

#CHOICEBOX#
/LABEL/ Treat Shanks as:
/POSITION/ 10,161
/SIZE/ 110,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Cutter","Holder"

#HELP#
This form allows you to modify the default cutting tool parameters and
attributes.  All tools defined in the clfile when it is scanned will be
given these attributes.  If the active tool list has already been defined,
then the Rescan button in the Edit Tool List form must be used to redefine
the tool attributes to the values setup in this form.  The tool colors are
defined in the NCLIPV Simulation Colors form.

Cut Tolerance:
--------------
This field sets the default cutting tolerance for all tools.  The smaller the
tolerance, the greater accuracy the cut model will have, but the process of
cutting the model will be slower.

Maxang:
-------
Defines the largest angular change in the tool axis allowed.  Any moves that
cause this value to be exceeded will be broken up into smaller moves.  Once
again the lower the value, the greater the accuracy, but the slower the
process.  This value is only used with multi-axis moves.

Rapid:
------
Sets the default rapid feedrate for all tools.  Any feedrates at or above this
value will be treated as rapid moves.

Translucency:
-------------
Sets the default display translucency for all tools from 1 to 100, where 100
displays a solid tool and a lower number displays a more transparent tool.

Display Edges:
--------------
Determines if all tools should be displayed with its edges rendered by default.
Displaying the edges of the tool with a low translucency setting is similar
to displaying a wireframe cutter.

Edge Color:
-----------
Defines the color to display the tool edges with.  'Default' uses the same
color as the tool is displayed in, while the other choices select an actual
color.  This color will be used to display the edges of the tool when the
Display Edges box is checked and when the viewing mode is set to wireframe.

Min Height:
----------:
Defines the minimum height to use for all tools.  Any tool that is defined
with a height of less than this value will be modified to have this height.

Max Height:
-----------
Defines the maximum height to use for all tools.  Any tool that is defined
with a height of more than this value will be modified to have this height.
Tools defined with an excess height will slow down the verification
process.

Min Diameter:
-------------
Defines the minimum diameter to use for all tools.  Any tool that is defined
with a diameter of less than this value will be modified to have this
diameter.

Next location is a FROM:
------------------------
Set this field to Yes if the locations immediately following an active tool
definition should be treated as a from location, rather than as a cutting
move.  Only locations following a tool definition contained in the Edit Tool
List form active list will be treated as a FROM.  Setting this field to No
causes the programmed location immediately following an active tool definition
to be treated exactly as programmed.  If it is a cutting or position motion,
then it will be treated as a cutting motion, if it is programmed using the
FROM statement, then it will be treated as a from location.

Treat Shanks as:
----------------
Cutter shanks are typically defined as the non-cutting part of a tool, but there
are times when the shank can be used for visual representation and actually
have a cutting edge, for example, on cone and bell shaped cutters.  Set this
field to Cutter if the default setting for shanks are to be considered as a
cutting portion of the tool.  Set it to Holder to have the cutter shanks
considered a non-cutting part of the tool.  This setting is used for clash
detection.
