#HEADER#
/TITLE/ VoluMill Pocket Modals
/POSITION/ 0,0
/SIZE/ 380,185

#SECTION#
/NAME/ Pocketing
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Pocket Direction:
/POSITION/ 10,12,71,12
/SIZE/ 105,57
/TYPE/UD_DASSTRING
/CHOICES/ "CLW","CCLW"

#EDIT#
/LABEL/ Maximum Stepover Distance:
/POSITION/ 10,29,110,29
/SIZE/ 141,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHOICEBOX#
/LABEL/ Step Down:
/POSITION/ 10,46,53,46
/SIZE/ 90,57
/TYPE/UD_DASSTRING
/CHOICES/ "Passes","Distance","Depth"

#EDIT#
/LABEL/ 
/POSITION/ 110,46
/SIZE/ 38,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#PICTUREBOX#
/FILE/ VoluMill_Modals.jpg
/NAME/ Pocketing
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Entry / Exit
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Method:
/POSITION/ 10,12,40,12
/SIZE/ 80,81
/TYPE/UD_DASSTRING
/CHOICES/ "Ramp","Helix"

#EDIT#
/LABEL/ Entry Angle:
/POSITION/ 10,29,50,29
/SIZE/ 80,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#EDIT#
/LABEL/ Helix Radius:
/POSITION/ 100,29,146,29
/SIZE/ 80,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHOICEBOX#
/LABEL/ Clearance:
/POSITION/ 10,46,50,46
/SIZE/ 90,57
/TYPE/UD_DASSTRING
/CHOICES/ "Distance","Plane"

#EDIT#
/LABEL/ 
/POSITION/ 104,46
/SIZE/ 50,12
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 146,46
/SIZE/ 40,13
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Reposition Retract Distance:
/POSITION/ 10,63,105,63
/SIZE/ 135,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#EDIT#
/LABEL/ Transition Retract Distance:
/POSITION/ 10,80,105,80
/SIZE/ 135,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#EDIT#
/LABEL/ Rapto Distance:
/POSITION/ 10,97,105,97
/SIZE/ 135,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#PICTUREBOX#
/FILE/ VoluMill_EntryExit.jpg
/NAME/ Entry / Exit
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Options
/COLOR/ DEFAULT

#CHECKBOX#
/LABEL/ Contour Ramping Permitted
/POSITION/ 10,12
/SIZE/ 100,15
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Finish Depth Cuts First
/POSITION/ 120,12
/SIZE/ 90,15
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Minimum Radius to Cut:
/POSITION/ 10,29,90,29
/SIZE/ 110,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHECKBOX#
/LABEL/ Smooth Boundaries
/POSITION/ 10,46
/SIZE/ 80,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Smooth Angle:
/POSITION/ 10,63,60,63
/SIZE/ 100,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 6

#EDIT#
/LABEL/ Smooth Radius:
/POSITION/ 100,63,155,63
/SIZE/ 110,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 6

#CHOICEBOX#
/LABEL/ Tranverse Style:
/POSITION/ 10,80,65,80
/SIZE/ 125,57
/TYPE/UD_DASSTRING
/CHOICES/ "Slot Milling", "Side Milling Only"

#EDIT#
/LABEL/ Depth of Cut:
/POSITION/ 10,97,57,97
/SIZE/ 80,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 6

#EDIT#
/LABEL/ Stepover Distance:
/POSITION/ 100,97,165,97
/SIZE/ 120,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 6

#PICTUREBOX#
/FILE/ VoluMill_Modals_Options.jpg
/NAME/ Options
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Tool Parameters
/COLOR/ DEFAULT

#EDIT#
/LABEL/ Number of Flutes:
/POSITION/ 10,12,68,12
/SIZE/ 94,12
/TYPE/UD_SCAINT
/INPUT/FORM_STRING
/LEN/ 6

#EDIT#
/LABEL/ Flute Length:
/POSITION/ 115,12,160,12
/SIZE/ 75,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 6

#EDIT#
/LABEL/ Tool Length:
/POSITION/ 10,29,68,29
/SIZE/ 75,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 6

#EDIT#
/LABEL/ Drill Diameter:
/POSITION/ 10,46,68,46
/SIZE/ 94,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 6

#EDIT#
/LABEL/ Drill Angle:
/POSITION/ 115,46,160,46
/SIZE/ 75,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 6

#PICTUREBOX#
/FILE/ VoluMill_ToolParameters.jpg
/NAME/ Tool Parameters
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Feed Rates
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ General:
/POSITION/ 10,12,50,12
/SIZE/ 88,57
/TYPE/UD_DASSTRING
/CHOICES/ "Current","Value"

#EDIT#
/LABEL/ 
/POSITION/ 85,12,105,12
/SIZE/ 58,13
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHOICEBOX#
/LABEL/ Positioning:
/POSITION/ 10,29,50,29
/SIZE/ 88,57
/TYPE/UD_DASSTRING
/CHOICES/ "General","Value","Factor"

#EDIT#
/LABEL/ 
/POSITION/ 85,29,105,29
/SIZE/ 58,14
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHOICEBOX#
/LABEL/ Entry:
/POSITION/ 10,46,50,46
/SIZE/ 88,57
/TYPE/UD_DASSTRING
/CHOICES/ "General","Value","Factor"

#EDIT#
/LABEL/ 
/POSITION/ 85,46,105,46
/SIZE/ 58,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHOICEBOX#
/LABEL/ Transition:
/POSITION/ 10,63,50,63
/SIZE/ 88,57
/TYPE/UD_DASSTRING
/CHOICES/ "General","Value","Factor"

#EDIT#
/LABEL/ 
/POSITION/ 85,63,105,63
/SIZE/ 58,13
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHOICEBOX#
/LABEL/ XY Rapid:
/POSITION/ 10,80,50,80
/SIZE/ 88,57
/TYPE/UD_DASSTRING
/CHOICES/ "Transition","Value"

#EDIT#
/LABEL/ 
/POSITION/ 85,80,105,80
/SIZE/ 42,13
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 8

#CHOICEBOX#
/LABEL/ Z Rapid:
/POSITION/ 10,97,50,97
/SIZE/ 88,57
/TYPE/UD_DASSTRING
/CHOICES/ "Transition","Value"

#EDIT#
/LABEL/ 
/POSITION/ 85,97,105,97
/SIZE/ 45,13
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 8

#CHECKBOX#
/LABEL/ Use Minimum Feedrate
/POSITION/ 10,114
/SIZE/ 85,11
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 85,114,105,114
/SIZE/ 39,13
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHECKBOX#
/LABEL/ Adjust Feed Rate For Entry
/POSITION/ 10,131
/SIZE/ 100,11
/TYPE/UD_DASSTRING

#LABEL#
/LABEL/ Current Feed rate:
/POSITION/ 150,56
/SIZE/100,14
/TYPE/UD_DASSTRING
/COLOR/ BLUE,DEFAULT

#DISPLAY#
/LABEL/
/POSITION/ 160,70
/SIZE/ 160,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8
/COLOR/ BLUE,DEFAULT
/PCOLOR/ BLUE,DEFAULT

#PICTUREBOX#
/FILE/ VoluMill_FeedRates.jpg
/NAME/ Feed Rates
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Spindle Speeds
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ General:
/POSITION/ 10,12,50,12
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","RPM"

#EDIT#
/LABEL/
/POSITION/ 85,12, 105,12
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ Entry:
/POSITION/ 10,29,50,29
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "General","RPM","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,29, 105,29
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ Transition:
/POSITION/ 10,46,50,46
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "General","RPM","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,46, 105,46
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#EDIT#
/LABEL/ Dwell After Spindle Change:
/POSITION/ 10,63,105,63
/SIZE/ 140,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#PICTUREBOX#
/FILE/ VoluMill_SpindleSpeeds.jpg
/NAME/ Spindle Speeds
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ ALL
/COLOR/ BLACK

#CHECKBOX#
/LABEL/ Force Out All Parameters
/POSITION/ 10,148
/SIZE/ 104,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Force Out VMPMOD command
/POSITION/ 120,148
/SIZE/ 120,14
/TYPE/UD_DASSTRING

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/vmpmod.mp4
/POSITION/ 275,140
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
#HELP#
<Pocketing>
VoluMill Pocket Modals
======================
This form allows you to input various VoluMill Pocket Modal parameters used by
the VoluMill Pocket and Waterline Roughing motion routines. The VMPMOD command
is issued if a change has been made or the Force Output box is checked.

Pocketing
=========
This form is used to change and set all the VoluMill Pocket Modals.

Pocket Direction
----------------
CLW specifies that the pocket motion will be in a clockwise direction.  CCLW
specifies a counter-clockwise direction.

Maximum Stepover Distance
-------------------------
Specifies the maximum stepover amount from one concentric pass to the next.
If set to 0, the current effective cutter diameter will be used (dia-cr).

Step Down
---------
The following methods can be used to determine the number of pocketing levels
(passes) to machine in a pocket.

Passes specifies the actual number of passes to take.  The depth of cut will
be an equal distance between each pass.

Distance specifies an approximate distance between each level.  The number of
passes will be determined by the total distance divided by this distance.  The
actual depth of cut may be less than or greater than the input value.

Depth specifies the maximum depth of cut for each level.  The number of
passes is calculated so that an equal depth of cut will be taken for each level
of the pocketing motion.  The depth of cut will actually be this value or
smaller.  One more pass could be taken when using the Depth parameter as
compared to the Distance parameter.
<END_SEC>
<Entry / Exit>
Pocket Entry and Exit
=====================
Entry Method	
------------
This field defines the tool entry method used.  Ramp indicates that the tool
will make a series of ramping motions to enter the pocket.  Helix indicates
the cutter will make a spiraling move until the current pocketing level is
reached.

Entry Angle
-----------
Specifies the angle to enter the pocket during the ramp/helix entry.

Helix Radius
------------
Active only with the Helix entry method, defines the radius of the helix.

Clearance
---------
Choose between using a Plane or a Distance for clearance level when exiting a
pocket section and positioning to a new section.  The distance is based on
the top plane of the pocket.  When Plane is specified, then the Select button
can be used to interactively pick the plane.

Reposition Retract Distance
---------------------------
Enter the distance above the pocket floor that the tool will retract to when
repositioning within the same pocket.

Transition Retract Distance
Force Output of VMPMOD Command
---------------------------------------------------------
Enter the distance above the pocket floor that the tool will retract to when
repositioning between pockets.

Rapto Distance
--------------
The tool will move down the tool axis at the retract feed rate until it is this
distance above the top of the pocket or the top plane of the last machined
level.
<END_SEC>
<Positioning>
Positioning
===========
Clearance
---------
Choose between using a Plane or a Distance for clearance level when exiting a
pocket section and positioning to a new section.  The distance is based on
the top plane of the pocket.  When Plane is specified, then the Select button
can be used to interactively pick the plane.

Select
------
Press the Select button to select the Clearance geometry. 

(Text Field)
------------
This field will display the label of the selected part geometry.  You can also 
manually type the label of the geometry in this field.

Reposition Retract Distance
---------------------------
Enter the distance above the pocket floor that the tool will retract to when
repositioning within the same pocket.

Transition Retract Distance
---------------------------
Enter the distance above the pocket floor that the tool will retract to when
repositioning between pockets.

Rapto Distance
--------------
The tool will move down the tool axis at the retract feed rate until it is this
distance above the top of the pocket or the top plane of the last machined
level.
<END_SEC>
<Options>
Options
=======
Contour Ramping Permitted
-------------------------
When enabled and the area to machine is too small for VoluMill to machine
normally, VoluMill will machine the pocket using RAMP/HELIX entry style motion
to machine the pocket when possible. The Entry Angle defined above will be used
to determine the depth of the cut for each RAMP/HELIX move.

Finish Depth Cuts First
-----------------------
Check this box if each individual pocket area should be cut to final depth
prior to moving to the next pocket area.  Leaving this box unchecked will 
cause all required cuts to be performed at the same level prior to moving to
the next depth level.

Minimum Radius to Cut
---------------------
Enter the minimum pocket boundary radius that can be machined.  If a pocket has
a maximum inscribed circle radius less than the tool radius plus this value,
it will not be machined.  A value of between 0. and .9 * tool radius can be
entered.

Smooth Boundaries
-----------------
Check this box if an attempt to smooth the perimeter and island boundaries
should be made.

Smooth Angle
------------
Enter the limit at which corner angles at less than this angle will be kept as
corners when boundary smoothing is enabled.

Smooth Radius
-------------
Enter the minimum allowable radius in the final toolpath when boundary
smoothing is enabled.  Smaller radii will go deeper into corners and cut
narrower channels, but will result in a longer tool path.

Side Milling Only
-----------------
Check this box if you want the motion style to use the side of the tool only.
This keeps the tool from making a full cut through the material.  Unchecking
this box allows the motion to make a full cut (slot) through the material in
order to traverse to separate pocket areas.

Slot Depth of Cut
-----------------
Specifies the maximum depth of cut the tool is allowed to take when cutting a
slot through the material when transitioning between pocket areas.

Slot Stepover Distance
----------------------
Enter the stepover distance to use when opening up a new pocket area by 
cutting a slot through the material.

<END_SEC>
<Tool Parameters>
Tool Parameters
===============

Number of Flutes
----------------
Enter the number of flutes on the cutting tool.

Flute Length
------------
Enter the length of the cutting portion of the cutter.

Tool Length
-----------
Enter the total length of the cutter.

Drill Diameter
--------------
Enter the diameter of the cutter used to drill the holes when predrilled holes
will be used as the pocket entry method.  A value of zero will use the 
diameter of the cutter used for pocketing.

Drill Angle
-----------
Enter the angle of the cutter used to drill the holes when predrilled holes
will be used as the pocket entry method.  The angle of the drill is based on
the axis of the cutter, the same as the CUTTER statement.  This means that it
will be 1/2 of the size of the actual drill tip angle.
<END_SEC>
<Feed Rates>
Feed Rates
==========
The Feed Rates section contains all of the feed rate settings available with
pocketing.

General
-------
Choose the feed rate to use for the general pocket motion.  The choices are
Current and Factor.  Current will use the active programmed feed rate.
Enter a percentage of the general feed rate when Factor is selected.

Positioning
-----------
Choose the feed rate to use when positioning the tool above the pocket. The
choices are Rapid, Value, and Factor.  Enter the desired feed rate when Value
is selected, or a percentage of the general feed rate when Factor is selected.

Entry 	
-----
Choose the feed rate to use when the tool enters the pocket.  The choices are
General, Value, and Factor.  Enter the desired feed rate when Value is selected,
or a percentage of the general feed rate when Factor is selected.

Transition 	
----------
Choose the feed rate to use when the tool transitions between pocket sections.
The choices are General, Value, and Factor.  Enter the desired feed rate when
Value is selected, or a percentage of the general feed rate when Factor is
selected.

XY Rapid
--------
Choose the feed rate to use when the tool moves at rapid rate in the XY plane.
The choices are Transition and Value.  Transition sets the XY Rapid rate to the 
transition rate. Enter the desired feed rate when Value is selected.

Z Rapid
-------
Choose the feed rate to use when the tool moves at a rapid rate in the Z-Axis.
The choices are Transition and Value. Transition sets the XY Rapid rate to the 
transition rate.  Enter the desired feed rate when Value is selected.

Current Feed Rate:
------------------
Displays the Current Feed Rate in effect.
   
Use Minimum Feed Rate
---------------------
Specifies the minimum feed rate allowed.

Adjust Feed Rate for Entry
--------------------------
When enabled the feed rate at the tool center is reduced during material
entry so that the periphery of the tool moves at the Plunge Feedrate.
<END_SEC>
<Spindle Speeds>
Spindle Speeds
==============
Specifies the spindle speeds for the VoluMill pocketing motion.

General
-------
Specifies the general pocketing spindle speed. Toggle between “Current”
and “RPM”. Enter the desired spindle speed when RPM is selected.

Entry
-----
Specifies the spindle speed to use when entering the part in a ramping
motion.  The choices are General, RPM, and Factor.  Enter the desired
spindle speed when RPM is selected, or a percentage of the general speed
when Factor is selected.

Transition
----------
Specifies the spindle speed to use when cutting a slot to enter a new 
pocket area.  The choices are General, RPM, and Factor.  Enter the desired 
spindle speed when RPM is selected, or a percentage of the general speed
when Factor is selected.

Dwell after Spindle Change
--------------------------
Specifies the amount of time in seconds to dwell after changing the
spindle speed.
<END_SEC>
<ALL>
Force Out All Parameters
------------------------
Check this box if all parameters should be output with the VMPMOD command.  If
this box is unchecked, then only the parameters that have been changed will be
output with the VMPMOD command.

Force Output of VMPMOD Command
------------------------------
If this box is checked, the VMPMOD statement is output unconditionally. If not
checked, the VMPMOD statement is output only if the VoluMill Pocket Modals have
been changed and a valid VMPOCK statement is also output by the main form.

Note: The "Force Out VMPMOD Command" check box is only available when this form
is opened as a sub-form through other forms such as VoluMill or Waterline form.

Video
-----
Plays a short Video of the basic use of the VoluMill Pocket Modals form.
