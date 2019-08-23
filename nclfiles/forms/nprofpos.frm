#HEADER#
/TITLE/ Profile Positioning
/POSITION/ 50,30
/SIZE/290,110

#CHOICEBOX#
/LABEL/ Clearance:
/POSITION/ 10,12
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "Current","Plane","Distance","Incremental"

#EDIT#
/LABEL/
/POSITION/ 110,12
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 165,12
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 210,12
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#CHOICEBOX#
/LABEL/ Rapto:
/POSITION/ 10,29
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "Entry","Plane","Distance","Incremental"

#EDIT#
/LABEL/
/POSITION/ 110,29
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 165,29
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 210,29
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#CHOICEBOX#
/LABEL/ Retract:
/POSITION/ 10,46
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","On","Plane","Distance","Incremental"

#EDIT#
/LABEL/
/POSITION/ 110,46
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 165,46
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 210,46
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#EDIT#
/LABEL/ Loop Retract Distance:
/POSITION/ 10,63
/SIZE/100,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#HELP#
===================
Profile Positioning
===================
This form controls the positioning and tool retraction features of Profile
Machining.

Clearance:
----------
The clearance level of Profile Machining is used to position the tool prior to
cutting the profile and can be used as the tool retraction level at the end of
the end of the profile motion.  It is also used as the clearance level between
offset passes of an open profile.

'Current' uses the current tool position to specify the clearance level.  A
clearance plane will be created that goes through the tool end point and is
perpendicular to the tool axis.  'Plane' allows the user to specify/select a
predefined plane.  'Distance' specifies a distance above the top of the part
level for the clearance plane.  'Incremental' specifies an incremental distance
above the entry point when positioning above the entry location.  This same
distance above the exit point will be used when retracting to the clearance
level at the end of the profile motion and between loops of an open profile.

Select
------
This button is only active when the Clearance type is set to 'Plane'.  Pressing
this button allows you to select the plane or planar surface to use as the
clearance plane.

The color choice field specifies which color to use to highlight the clearance
plane.

Rapto Distance:
---------------
The Rapto Distance specifies the distance above the entry location to position
the tool at the Positioning feed rate.  The remainder of the entry move will be
made at the Entry feed rate.

'Entry' will position to the start of the entry move using the Positioning feed
rate.  If an entry move is not programmed, then the tool will move to the start
of the profile using the General feed rate.  'Plane' allows the user to specify/
select a predefined plane.  'Distance' specifies a distance above the top of the
part level for the rapto plane.  'Incremental' specifies an incremental distance
above the first point of the profile to use as the rapto plane.

Select
------
This button is only active when the Rapto type is set to 'Plane'.  Pressing
this button allows you to select the plane or planar surface to use as the
rapto plane.

The color choice field specifies which color to use to highlight the rapto
plane.

Retract:
--------
This field allows you to specify the retract logic to use at the end of the
profile motion, between depth passes, and between offset passes of an open
profile.

'Off' performs no tool retraction at the end of the profile motion, though the
tool may still be positioned to the clearance plane between passes.  'On'
retracts the tool to the Clearance level at the end of the profile motion.
'Plane' allows the user to specify/select a predefined plane.  'Distance'
specifies a distance above the top of the part level to retract the tool.
'Incremental' specifies an incremental distance above the last point of the
profile to use as the retract plane.

Select
------
This button is only active when the Final Retract type is set to 'Plane'.
Pressing this button allows you to select the plane or planar surface to use as
the retract plane.

The color choice field specifies which color to use to highlight the retract
plane.

Loop Retract Distance:
----------------------
The Loop Retract Distance specifies a distance above the ending location of the
profile that the tool should retract between depth passes of a closed profile
and offset passes when the transition moves are programmed in the UP mode.
This value will be ignored on open profiles and between offset passes in the
DOWN mode.
