#HEADER#
/TITLE/ Corner Rounding
/POSITION/ 50,50
/SIZE/310,180

#SECTION#
/NAME/ Radius
/COLOR/ DEFAULT

#CHECKBOX#
/LABEL/ Enable Corner Rounding
/POSITION/ 10,8
/SIZE/130,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Fillet Radius:
/POSITION/ 10,25, 75,25
/SIZE/130,14
/TYPE/ UD_SCAVAL
/LEN/ -7
/PREC/ 4
/RANGE/ .0,1000.

#EDIT#
/LABEL/Tolerance:
/POSITION/ 10,42, 75,42
/SIZE/130,14
/TYPE/ UD_SCAVAL
/LEN/ -7
/PREC/ 4
/RANGE/ .0,1.

#CHECKBOX#
/LABEL/ Combine Motion
/POSITION/ 10,59
/SIZE/130,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ Arcslp_Radius.jpg
/NAME/ fedrat
/POSITION/ 150,10
/SIZE/ 150,125

#SECTION#
/NAME/ Options
/COLOR/ DEFAULT

#CHECKBOX#
/LABEL/ Output Warnings
/POSITION/ 10,8
/SIZE/130,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Fixed Tool Axis
/POSITION/ 10,25
/SIZE/130,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/Maximum Deviation:
/POSITION/ 10,42
/SIZE/130,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -7
/RANGE/ 0.,180.

#PICTUREBOX#
/FILE/ Arcslp_Options.jpg
/NAME/ fedrat
/POSITION/ 150,10
/SIZE/ 150,125

#SECTION#
/NAME/ Feed Rates
/COLOR/ DEFAULT

#CHECKBOX#
/LABEL/ Feed Rate Control
/POSITION/ 10,8
/SIZE/130,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/Feed Rate:
/POSITION/ 10,25,87,25
/SIZE/130,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -9
/RANGE/ .0,99999.

#EDIT#
/LABEL/Maximum Feed Rate:
/POSITION/ 10,42,87,42
/SIZE/130,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -9
/RANGE/ .0,99999.

#CHOICEBOX#
/LABEL/ Tool Condition:
/POSITION/ 10,59,87,59
/SIZE/120,40
/TYPE/UD_DASSTRING
/CHOICES/ "Left","Right"

#EDIT#
/LABEL/Cutter Diameter:
/POSITION/ 10,76,87,76
/SIZE/130,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -9
/RANGE/ .0,99999.

#PICTUREBOX#
/FILE/ Arcslp_FeedRate.jpg
/NAME/ fedrat
/POSITION/ 150,10
/SIZE/ 150,125

#SECTION#
/NAME/ All
/COLOR/ DEFAULT

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/arcslp.mp4
/POSITION/ 225,140
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
<Radius>
Enable Corner Rounding
----------------------
Checking this box enables the automatic generation of fillets at the
intersection of successive motion records.

Fillet Radius:
--------------
Specifies the fillet radius to be generated.

Tolerance:
----------
Specifies the chordal tolerance used to calculate the points on the fillet.  A
smaller value will generate more points on the fillet.

Combine Motion
--------------
Fillets will normally be generated between two successive motions.  Sometimes
a move will be programmed that will be too short to allow a fillet to be
generated.  Enabling this option will allow for three successive motions to be
analyzed for fillet motion when a fillet cannot be generated between two
successive motions.  This feature is useful for parts that contain small tight
corners.
<END_SEC>
<Options>
Output Warnings
---------------
Checking this box will cause NCL to output a warning when a fillet cannot be
generated and when the tool axis changes more than the maximum angular
deviation allowed during Fixed Tool Axis filleting.

Fixed Tool Axis
---------------
Enable this option to disable tool axis changes during fillet motion.  The
tool axis will remain fixed at the tool axis of the corner point during the
fillet motion.  This feature should be used in conjunction with the
TLAXIS/...,LOCK tool axis control, so that the fillet tool axis will not differ
from the tool axis vectors generated near the corner point.

Maximum Deviation:
------------------
Specifies the maximum angular deviation allowed from the programmed tool axis
of the motions output directly on either side of the fillet and the tool axis
output with the fillet motion when Fixed Tool Axis is enabled.  A warning will
be output if the angular change exceeds this value and Output Warnings is
enabled.
<END_SEC>
<Feed Rates>
Feed Rate Control
----------------
Enabling this option allows NCL to control the true surface feed rate for the 
generated fillet motion.  The feed rate will be increased for outside fillets
and decreased for inside fillets in order that the programmed feed rate will
be maintained where the tool touches the work piece.  The following fields
apply to the Feedrate Control capability.

Feed Rate:
----------
Specifies the feed rate to maintain on the cutting edge of the tool.  A value
of zero will cause the programmed feedrate to be used.

Maximum Feedrate:
-----------------
Specifies the maximum feed rate that can be output with fillet motion.  This
value refers to the tool tip feed rate output to the clfile.

Tool Condition:
---------------
This field can be set to either LEFT or RIGHT and specifies the direction of
the cutter in reference to the work piece.  LEFT states that the cutter is to
the left of the work piece and RIGHT states that the cutter is to the right of
the work piece.

Cutter Diameter:
----------------
Specifies the diameter of the programmed cutter and is required in order to
calculate the true surface feed rate.
