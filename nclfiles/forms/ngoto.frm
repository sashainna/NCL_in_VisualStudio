#HEADER#
/TITLE/ Goto Tool Location
/POSITION/ 50,30
/SIZE/ 445,180

#SECTION#
/NAME/ Goto
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/
/POSITION/ 10,10
/SIZE/ 35,40
/TYPE/UD_DASSTRING
/CHOICES/ "From","Goto"

#CHOICEBOX#
/LABEL/
/POSITION/ 52,10
/SIZE/ 57,40
/TYPE/UD_DASSTRING
/CHOICES/ "Point/Pt-Vec","Pattern","Location"

#EDIT#
/LABEL/
/POSITION/ 115,10
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 167,10
/SIZE/ 38,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Select Pt
/POSITION/ 167,10
/SIZE/ 38,14
/TYPE/ UD_DASSTRING
/PICTURE/ goto1,"Goto Point",0,0,50,49

#PUSHBUTTON#
/LABEL/ Select Pn
/POSITION/ 167,10
/SIZE/ 38,14
/TYPE/ UD_DASSTRING
/PICTURE/ goto1,"Goto Pattern",0,51,50,100

#PUSHBUTTON#
/LABEL/ Select Lo
/POSITION/ 167,10
/SIZE/ 38,14
/TYPE/ UD_DASSTRING
/PICTURE/ goto1,"Goto Location",54,0,100,49

#PUSHBUTTON#
/LABEL/ Tool Axis
/POSITION/ 10,27
/SIZE/ 45,14
/TYPE/ UD_DASSTRING
/PICTURE/ goto1,"Select Tool Axis Vector",54,51,100,100

#EDIT#
/LABEL/
/POSITION/ 65,27
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/LEN/ 15
/PREC/ 80

#CHOICEBOX#
/LABEL/ Feed Rate:
/POSITION/ 10,44
/SIZE/ 80,40
/TYPE/UD_DASSTRING
/CHOICES/ "Current","Rapid"

#EDIT#
/LABEL/
/POSITION/ 95,44
/SIZE/ 30,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 6

#CHECKBOX#
/LABEL/
/POSITION/ 130,44
/SIZE/ 15,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Advanced
/POSITION/ 145,44
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Single Selection
/POSITION/ 10,61
/SIZE/ 70,14
/TYPE/ UD_DASINT

#CHECKBOX#
/LABEL/ Automatically Preview Motion
/POSITION/ 90,61
/SIZE/ 110,14
/TYPE/ UD_DASINT

#PICTUREBOX#
/FILE/ Goto.jpg
/NAME/ goto1
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Limit Pattern
/COLOR/ DEFAULT

#CHECKBOX#
/LABEL/ Reverse Order
/POSITION/ 10,10
/SIZE/ 60,14
/TYPE/UD_DASINT

#CHOICEBOX#
/LABEL/ Goto Option:
/POSITION/ 80,10
/SIZE/ 85,40
/TYPE/UD_DASSTRING
/CHOICES/ "All","Omit","Retain"

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 10,27
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Select Om
/POSITION/ 10,27
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PICTURE/ goto2,"Select Points to Omit",0,15,100,30

#PUSHBUTTON#
/LABEL/ Select Re
/POSITION/ 10,27
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PICTURE/ goto2,"Select Points to Goto",0,37,100,60

#EDIT#
/LABEL/
/POSITION/ 60,27
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/LEN/ 8
/PREC/ 80

#CHECKBOX#
/LABEL/ Thru
/POSITION/ 110,27
/SIZE/ 30,14
/TYPE/UD_DASINT

#EDIT#
/LABEL/
/POSITION/ 150,27
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/LEN/ 8

#CHECKBOX#
/LABEL/ Avoid
/POSITION/ 10,44
/SIZE/ 30,14
/TYPE/UD_DASINT

#EDIT#
/LABEL/ Distance:
/POSITION/ 50,44
/SIZE/ 60,14
/TYPE/ UD_SCAVAL
/LEN/ 8
/PREC/ 4

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 10,61
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Select Av
/POSITION/ 10,61
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PICTURE/ goto2,"Select Points to Avoid",0,68,100,95

#EDIT#
/LABEL/
/POSITION/ 60,61
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/LEN/ 8

#CHECKBOX#
/LABEL/ Thru
/POSITION/ 110,61
/SIZE/ 30,14
/TYPE/UD_DASINT

#EDIT#
/LABEL/
/POSITION/ 150,61
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/LEN/ 8
/PREC/ 80

#PICTUREBOX#
/FILE/ Goto_Limit_Pattern.jpg
/NAME/ goto2
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION_BAR#

#SECTION#
/NAME/ Colors
/COLOR/ *DEFAULT

#COLOR#
/LABEL/ Points/Pt-Vec/Patterns:
/POSITION/ 10,10, 88,10
/SIZE/ 113,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Tool Axis:
/POSITION/ 130,10, 163,10
/SIZE/ 68,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Unused Geometry:     
/POSITION/ 10,27
/SIZE/112,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Hide","Fade"

#COLOR#
/LABEL/ Color:
/POSITION/ 130,27, 163,27
/SIZE/ 68,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#PICTUREBOX#
/FILE/ highlight.jpg
/NAME/ highlight
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ ALL
/COLOR/ BLACK

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 380,10
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/FONT/ 1.
/COLOR/ BLACK, SEAGRN

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 380,28
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, SEAGRN

#PUSHBUTTON#
/LABEL/ Reset
/POSITION/ 380,46
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, GREY

#PUSHBUTTON#
/LABEL/ Playback
/POSITION/ 380,64
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, LTTAN

#PUSHBUTTON#
/LABEL/ Verify
/POSITION/ 380,82
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, LTTAN

#PUSHBUTTON#
/LABEL/ Geometry
/POSITION/ 380,100
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, LTBLUE

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 380,118
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, LTBLUE

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/goto.mp4
/POSITION/ 380,136
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
<Goto>
Goto Tool Location
==================
This form is used to position the tool to a predefined or arbitrary point or
pattern of points.

From / Goto
===========
Select the type of positioning command that will be output from the Goto Tool
Location interface.  A From location is typically used to initially position
the tool after a tool change.  Goto locations are used for positioning the tool
for drilling sequences or prior to cutting sequences.

Point
-----
You can position the tool at a point, to a pattern of points, or to a location
arbitrarily picked from the screen.  The name of a point or pattern can be typed
into the text field or you can pick the point/pattern from the screen.

Select
------
Pressing the Select button allows you to select the point/pattern to position
the tool at.

Tool Axis
---------
You can either pick a vector or point-vector from the screen or type in a 
tool axis vector or point-vector into the text field. When the form is first
displayed this text field will contain the current tool axis vector.  Press
the Tool Axis button to select a vector or point-vector defining the tool axis
from the screen.

Feed Rate:
----------
The feed rate used to move the tool to the specified location(s) can remain the
Same as the current feed rate, the move can be moved in Rapid mode, or you can
specify a new feed rate Value for this move. Specifying the feed rate mode as
Same will leave the current feed rate mode in effect. Rapid causes the next
motion to move in rapid mode. 

If Value is selected the Mode selection box is activated, check this box to
select either FPM (Feed per Minute) or FPR (Feed per Spindle Revolution).

Mode:
-----
NCL supports both Feed per Minute and Feed per Spindle Revolution feed rates.
FPM and FPR will output the corresponding mode with the FEDRAT command.
(IPM,MMPM,IPR,MMPR). 

Single Selection
----------------
Check this box if you are only going to select a single point or pattern to 
position the tool at.  After selecting the point/pattern this form will
automatically be displayed.  Unchecking this box will allow you to select
multiple points/patterns to locate the tool at.

Automatically Preview Motion
----------------------------
Check this box if you would like to have the tool motion displayed on the screen
as you select geometry to position the tool at.  This allows you to view the
motion interactively as you select points/patterns on the screen.  If this box
is not checked then you must press the Preview button to preview the motion
that would be generated by this form.  Please note that the form must still
be accepted or the Apply button must be pressed to generate the NCL command
and permanently create the motion.
<END_SEC>
<Limit Pattern>
Limit Pattern
=============
The Limit Pattern section allows you to control which of the pattern points to
actually position the tool at or which ones to avoid.  It also contains an
option for reversing the order of the pattern points to position the tool at and
which points the tool should be retracted at. 

Reverse Order
-------------
When this box is not checked, then the tool will position at each of the pattern
points in the order that they were defined in the pattern.  Checking this box
will position the tool starting at the last point defined in the pattern and
proceeding in reverse order to the first point.

Goto Option:
------------
The tool can be positioned at All points defined in the pattern, or selected
points within the pattern can be Omitted, or the tool can be positioned at
selected points (Retain).

Select
------
Pressing the Select button allows you to pick the points that will be omitted
or retained when positioning the tool to the pattern locations.  If the Thru
box is checked you will be prompted for a range of points, by selecting the
first point and the last point that you want omitted/retained.  If the Thru
box is not checked, then you can individually select each of the points to be
omitted/retained.  You can also enter a list of point indexes within the
pattern to omit/retain by typing the numbers in the first field separated by
commas or by checking the Thru box and entering the first and last indexes in
the appropriate text fields.

Avoid
-----
Check this box if the tool should retract between points defined in the pattern.

Distance:
---------
The tool will be retracted up the tool axis by the amount entered in this field
at the selected points in the pattern.

Select
------
Pressing the Select button allows you to pick the points that will have the
tool retracted at when positioning the tool to the pattern locations.  If the
Thru box is checked you will be prompted for a range of points, by selecting
the first point and the last point to retract the tool at.  If the Thru box is
not checked, then you can individually select each of the points to retract the
tool at.  You can also enter a list of point indexes within the pattern to
retract the tool at by typing the numbers in the first field separated by
commas or by checking the Thru box and entering the first and last indexes in
the appropriate text fields.
<END_SEC>
<Colors>
Colors
======
The Colors section defines the colors that will be used to highlight the
geometry selected while in the Goto Tool Location form.  All entities that
can be picked from the screen are listed in this section.

Unused Geometry:
----------------
When pressing the 'Geometry' Action item button to "hide" geometry not used
in the Goto Tool Location operation you have the option of either invisible
this geometry (Hide) or displaying the geometry as translucent and with
dotted lines (Fade).  The Color field applies to the faded geometry.
<END_SEC>
<ALL>
Action Buttons
==============
The Action Buttons are located at the right hand of the motion generation
forms and allow you to perform specific actions that will assist you in
visualizing the results of the motion form settings.

Preview
-------
Previews the Goto Tool Location motion without writing out the command or
permanently storing the generated motion.  Press the OK or Apply button to
write out the command and motion.  The generated command can be saved after
the preview even if there is an error.  To save the command, make no changes
to the settings and press the OK button.  The command will then be available
for editing in the command line.

Apply
-----
Outputs and processes the command(s) without taking down the form so that
other Engraving motion can be created.

Reset Motion
------------
Resets all form fields to their settings/values when the form was first entered.
This button is useful after pressing the Apply button if you want to start
fresh or when you have made numerous changes to the form settings and are not
getting the output you desire.

Playback
--------
Displays Playback Preview Motion interface, allowing you to step through and
animate the motion generated using the Preview button.  This button is only
active when Preview motion is displayed on the screen.

Verify
------
Pressing this button allows you to verify the Preview motion by using the
material removal process of NCL/IPV.  It displays the Verify Preview motion
interface, allowing you to simulate the material removal for the Preview motion.
This button is only active when you have a valid NCL/IPV license and Preview
motion is displayed on the screen.

Geometry
--------
Pressing the Geometry button the first time will hide all unused geometry from
the screen, leaving only the geometry that was selected during this session 
displayed in the selected colors.  The 'Unused Geometry' field in the 'Colors'
section defines whether the unused geometry will be invisible or just faded.

Pressing this button a second time will redisplay the unused geometry.

View
----
Takes down the form(s) and enters dynamic viewing.
