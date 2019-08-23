#HEADER#
/TITLE/ Regional Milling
/POSITION/ 50,30
/SIZE/440,180

#SECTION#
/NAME/ RMill
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Surface
/POSITION/ 10,12
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PICTURE/ rmill,"Surface",47,59,62,69

#EDIT#
/LABEL/
/POSITION/ 65,12
/SIZE/60,14
/TYPE/UD_DASSTRING
/LEN/ 10
/PREC/ 80

#CHOICEBOX#
/LABEL/ Check 1:
/POSITION/ 10,29, 40,29
/SIZE/70,40
/TYPE/UD_DASSTRING
/CHOICES/ "To","On","Past"

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 90,29
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PICTURE/ rmill,"Check 1",30,54,45,64

#EDIT#
/LABEL/
/POSITION/ 135,29
/SIZE/60,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#CHOICEBOX#
/LABEL/ Check 2:
/POSITION/ 10,46,40,46
/SIZE/70,40
/TYPE/UD_DASSTRING
/CHOICES/ "To", "On","Past"

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 90,46
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PICTURE/ rmill,"Check 2",46,28,61,38

#EDIT#
/LABEL/
/POSITION/ 135,46
/SIZE/60,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#CHOICEBOX#
/LABEL/ Drive 1:
/POSITION/ 10,63,40,63
/SIZE/70,40
/TYPE/UD_DASSTRING
/CHOICES/ "To","On","Past"

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 90,63
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PICTURE/ rmill,"Drive 1",2,36,17,46

#EDIT#
/LABEL/
/POSITION/ 135,63
/SIZE/60,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#CHOICEBOX#
/LABEL/ Drive 2:
/POSITION/ 10,80,40,80
/SIZE/70,40
/TYPE/UD_DASSTRING
/CHOICES/ "To","On","Past"

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 90,80
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PICTURE/ rmill,"Drive 2",77,44,92,54

#EDIT#
/LABEL/
/POSITION/ 135,80
/SIZE/60,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Near Point
/POSITION/ 10,97
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 80,97
/SIZE/60,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PICTUREBOX#
/FILE/ Rmill.jpg
/NAME/ rmill
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Motion Type
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Motion Type:
/POSITION/ 10,12,65,12
/SIZE/105,40
/TYPE/UD_DASSTRING
/CHOICES/ "Lace","Scrub"
/PICTURE/ motion type,"Lace",0,0,100,50,0
/PICTURE/ motion type,"Scrub",0,50,100,100,1

#CHECKBOX#
/LABEL/ Perform Final Pass
/POSITION/ 125,12
/SIZE/ 75,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Stepover Type:
/POSITION/ 10,29,65,29
/SIZE/105,40
/TYPE/UD_DASSTRING
/CHOICES/ "Fixed","Scallop"

#EDIT#
/LABEL/
/POSITION/ 125,29
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#PICTUREBOX#
/FILE/ Rmill_MotionType.jpg
/NAME/ Motion Type
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Entry / Exit
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Clearance:
/POSITION/ 10,12,50,12
/SIZE/85,40
/TYPE/UD_DASSTRING
/CHOICES/ "Plane","Distance"

#EDIT#
/LABEL/
/POSITION/ 100,12
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 150,12
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Rapto:
/POSITION/ 10,29,50,29
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 10

#CHOICEBOX#
/LABEL/ Retract:
/POSITION/ 10,46,50,46
/SIZE/85,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","Entity","Distance"

#EDIT#
/LABEL/
/POSITION/ 100,46
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 150,46
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ Rmill_EntryExit.jpg
/NAME/ Entry / Exit
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Feed Rates
/COLOR/ DEFAULT

#DISPLAY#
/LABEL/ Current Feed rate:
/POSITION/ 10,12
/SIZE/ 120,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8
/COLOR/ BLUE,DEFAULT
/PCOLOR/ BLUE,DEFAULT

#CHOICEBOX#
/LABEL/ General:
/POSITION/ 10,32, 45,32
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value"

#EDIT#
/LABEL/
/POSITION/ 105,32
/SIZE/50,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Position:
/POSITION/ 10,49, 45,49
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid"

#EDIT#
/LABEL/
/POSITION/ 105,49
/SIZE/90,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Plunge:
/POSITION/ 10,66, 45,66
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value"

#EDIT#
/LABEL/
/POSITION/ 105,66
/SIZE/50,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#PICTUREBOX#
/FILE/ Rmill_FeedRates.jpg
/NAME/ Feed Rates
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Thicks
/COLOR/ DEFAULT

#FRAME#
/TITLE/ Roughing Thicks
/POSITION/ 5,8
/SIZE/ 180,50

#EDIT#
/LABEL/ Check 1:
/POSITION/ 15,19, 45,19
/SIZE/50,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8
/PICTURE/ Thicks,"Check 1",45,37,56,43

#EDIT#
/LABEL/ Check 2:
/POSITION/ 90,19, 120,19
/SIZE/50,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8
/PICTURE/ Thicks,"Check 2",45,13,56,19

#EDIT#
/LABEL/ Drive 1:
/POSITION/ 15,36, 45,36
/SIZE/50,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8
/PICTURE/ Thicks,"Drive 1",6,21,17,27

#EDIT#
/LABEL/ Drive 2:
/POSITION/ 90,36, 120,36
/SIZE/50,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8
/PICTURE/ Thicks,"Drive 2",80,20,90,26

#FRAME#
/TITLE/ Finishing Thicks
/POSITION/ 5,65
/SIZE/ 180,50

#EDIT#
/LABEL/ Check 1:
/POSITION/ 15,74, 45,74
/SIZE/50,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8
/PICTURE/ Thicks,"Check 1",45,87,56,93

#EDIT#
/LABEL/ Check 2:
/POSITION/ 90,74, 120,74
/SIZE/50,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8
/PICTURE/ Thicks,"Check 2",45,61,56,67

#EDIT#
/LABEL/ Drive 1:
/POSITION/ 15,91, 45,91
/SIZE/50,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8
/PICTURE/ Thicks,"Drive 1",6,71,17,77

#EDIT#
/LABEL/ Drive 2:
/POSITION/ 90,91, 120,91
/SIZE/50,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8
/PICTURE/ Thicks,"Drive 2",80,70,91,76

#PICTUREBOX#
/FILE/ Rmill_Thicks.jpg
/NAME/ Thicks
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Colors
/COLOR/ DEFAULT

#COLOR#      
/LABEL/ Surface:
/POSITION/ 10,12, 72, 12
/SIZE/92,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Check 1:
/POSITION/ 10,29,72,29
/SIZE/92,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Check 2:
/POSITION/ 130,29, 178, 29
/SIZE/78,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Drive 1:
/POSITION/ 10,46, 72, 46
/SIZE/92,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Drive 2:
/POSITION/ 130,46, 178,46
/SIZE/78,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Near Point:
/POSITION/ 10,63, 72,63
/SIZE/92,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Clearance Plane:
/POSITION/ 10,80,72,80
/SIZE/92,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Retract Plane:
/POSITION/ 130,80,178,80
/SIZE/78,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#CHOICEBOX#
/LABEL/ Unused Geometry:
/POSITION/ 10,97,72,97
/SIZE/107,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Hide","Fade"

#COLOR#
/LABEL/ Color:
/POSITION/ 130,97, 178,97
/SIZE/78,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#PICTUREBOX#
/FILE/ highlight.jpg
/NAME/ highlight
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ All
/COLOR/ DEFAULT

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
/FILE/rmill.mp4
/POSITION/ 380,136
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
<RMill>
Regional Milling
================
This form is used to perform an Regional Milling operation by machining
a region of a surface or plane that is bounded by two ends and two sides.
Either a LACE  or SCRUB series of motions will be generated.

Surface
-------
Pressing the Surface button allows the user to select the surface or
plane to mill.

(Text Field)
------------
This field will display the label of the selected part geometry. You
can also  manually type the label of the geometry in this field.

Check 1:
--------
The geometry that will limit the cutter motion in the region to be
milled and  can be thought of as the check surface for the motion that
is to be generated. It can be a line, circle, plane, curve, or surface.

'To', 'On', and 'Past' controls the relationship of the cutter to the
first check surface geometry. When the cutter approaches the check
surface geometry it will either stop as it touches it (To), is directly
on it (On), or after it is past it (Past).

Select
------
Press the Select button to select the Check 1 geometry.

(Text Field)
------------
This field will display the label of the selected part geometry.  You
can also manually type the label of the geometry in this field.

Check 2:
--------
The geometry that will limit the other side of the region along with
the Check 1 entity. It can be a line, circle, plane, curve, or surface.

'To', 'On', and 'Past' controls the relationship of the cutter to the
second check surface geometry. When the cutter approaches the check
surface geometry it will either stop as it touches it (To), is directly
on it (On), or after it is past it (Past).

Select
------
Press the Select button to select the Check 2 geometry.

(Text Field)
------------
This field will display the label of the selected part geometry. You
can also manually type the label of the geometry in this field.

Drive 1:
--------
The geometry that is the limit of one side of the region to be milled
and can be thought of as the drive surface for the motion that is to
be generated. It can be either a plane or a line.

'To', 'On', and 'Past' controls the relationship of the cutter to the
first drive surface geometry. When the cutter approaches the drive
surface geometry it will either stop as it touches it (To), is directly
on it (On), or after it is past it (Past).

Select
------
Press the Select button to select the Drive 1 geometry.

(Text Field)
------------
This field will display the label of the selected part geometry. You
can also manually type the label of the geometry in this field.

Drive 2:
--------
The geometry that will limit the other side of the region along with
the Drive 1 entity. It can be either a plane or a line.

'To', 'On', and 'Past' controls the relationship of the cutter to the
second drive surface geometry. When the cutter approaches the drive
surface geometry it will either stop as it touches it (To), is directly
on it (On), or after it 
is past it (Past).

Select
------
Press the Select button to select the Drive 2 geometry.

(Text Field)
------------
This field will display the label of the selected part geometry.  You
can also manually type the label of the geometry in this field.

Near Point
----------
Click this button to select the point near the Drive 1 and Check 1
intersection corner to help NCL locate the starting position. 


<END_SEC>
<Motion Type>
Motion Type
===========

Motion Type
-----------
Specifies the type of motion to be generated.  'Lace' motion will
retract the cutter at the end of each pass and then reposition it at
the start of the next pass.  All cutting motion will be in a single
direction.

'Scrub' will create scrub style motion and will not retract the cutter
at the end of each pass. Cutting motion will be generated in both
directions without retracting the cutter between passes.

Stepover Type
-------------
The stepover distance between passes can either be a fixed distance
(Fixed) or defined as being the maximum scallop height allowed between
passes (Scallop). This value cannot be zero.

Perform Final Pass
------------------
Check this box if a final pass should be taken around the drive and
check surfaces after the lace or scrub motion is completed.

<END_SEC>
<Entry / Exit>
Entry / Exit
============
The Entry / Exit section controls the settings used for entering onto
and exiting off of the regional mill surface.

Clearance:
----------
The Clearance fields are only active when the motion type is set to
Lace. They define the clearance level to retract the tool to at the end
of each pass. The clearance level can either be a plane or a distance.

Select
------
Press the Select button to select the clearance plane. This button is
only active with the Clearance type is set to Plane.

(Text Field)
------------
This field will display the label of the selected part geometry.  You
can also manually type the label of the geometry in this field.

Rapto:
------
The Rapto distance specifies the distance above the regional mill
surface to position at when moving down the tool axis from the clearance
plane during Lace style motion when starting the next pass. The Rapto
distance can also be used to set the plunge distance when the cutter
moves to the starting point when Scrub style motion is used. If a Rapto
distance greater than 0.0 is provided for a Scrub style motion, the
cutter will move to the point at the Rapto height above the starting
point with respect to the tool axis using a rapid feed rate and then
move to the cutting surface using the plunge feed 
rate.

Retract:
--------
The Retract fields define the level to retract the cutter to after the 
Regional Milling motion is complete. You can specify a Plane or a Point 
(Entity), a distance (Distance), or for the cutter not to be retracted
at the end of the motion (Off).

If a plane is selected, then the cutter will retract along the tool
axis to the level of the plane. If a point is selected, then the cutter
will move to that point at the end of the Regional Milling motion.

Select
------
Press the Select button to select the retract geometry.  This button
is only active with the Retract type is set to Entity. A plane or a
point can be selected.

(Text Field)
------------
This field will display the label of the selected part geometry.  You
can also manually type the label of the geometry in this field.

<END_SEC>
<Feed Rates>
Feed Rates
==========
This form defines the feed rates to use for Regional Milling.  Each
feed rate has the following modes that can be programmed.

   Current = Uses the currently programmed feed rate.
   Value   = Allows you to specify an absolute value as the feed rate.
   Rapid   = Uses RAPID. (Only valid for the Position field).

General:
--------
Defines the feed rate to use during the basic motion of regional milling.

Position:
---------
Defines the feed rate to use when positioning the tool to and along the
clearance plane and to the retract plane.

Plunge:
-------
Defines the feed rate to use when the tool is positioned to the rapto
distance above the regional milling part surface.

<END_SEC>
<Thicks>
Thicks
======
The Thick section provides settings for Roughing and Finishing thicks
for Check 1, Drive 1, Check 2 and Drive 2 for Regional Milling.

Roughing Thicks
---------------
The Roughing thicks section provides settings to offset the tool from
Check 1, Drive 1 and Check 2, Drive 2 for Regional Milling Roughing.

Check Surface 1 Thick
---------------------
Enter the distance to offset the tool from Check Surface 1

Drive Surface 1 Thick
---------------------
Enter the distance to offset the tool from Drive Surface 1

Check Surface 2 Thick
---------------------
Enter the distance to offset the tool from Check Surface 2

Drive Surface 2 Thick
---------------------
Enter the distance to offset the tool from Drive Surface 2

Finishing Thicks
----------------
The Finishing thicks section provides settings to offset the tool from
Check 1, Drive 1 and Check 2, Drive 2 for Regional Milling Finishing.

Check Surface 1 Thick
---------------------
Enter the distance to offset the tool from Check Surface 1

Drive Surface 1 Thick
---------------------
Enter the distance to offset the tool from Drive Surface 1

Check Surface 2 Thick
---------------------
Enter the distance to offset the tool from Check Surface 2

Drive Surface 2 Thick
---------------------
Enter the distance to offset the tool from Drive Surface 2

<END_SEC>
<Colors>
Colors
======
The Colors section defines the colors that will be used to highlight
the geometry selected while in the Regional Milling form.  All entities
that can be picked from the screen are listed in this section.

Unused Geometry:
----------------
When pressing the 'Geometry' Action item button to "hide" geometry not
used in the Regional Milling operation you have the option of either
invisible this geometry (Hide) or displaying the geometry as translucent
and with dotted lines(Fade). The Color field applies to the faded
geometry.

<END_SEC>
<ALL>
Action Buttons
==============
The Action Buttons are located at the right hand of the motion generation
forms and allow you to perform specific actions that will assist you in
visualizing the results of the motion form settings.

Preview
-------
Previews the motion without writing out the command or permanently
storing the generated motion.  Press the OK or Apply button to write
out the command and motion. The generated command can be saved after
the preview even if there is an error. To save the command, make no
changes to the settings and press the OK button. The command will then
be available for editing in the command line.

Apply
-----
Outputs and processes the command(s) without taking down the form so
that other motion can be created.

Reset Motion
------------
Resets all form fields to their settings/values when the form was first
entered.  This button is useful after pressing the Apply button if you
want to start fresh or when you have made numerous changes to the form
settings and are not getting the output you desire.

Playback
--------
Displays Playback Preview Motion interface, allowing you to step through
and animate the motion generated using the Preview button.  This button
is only active when Preview motion is displayed on the screen.

Verify
------
Pressing this button allows you to verify the Preview motion by using
the material removal process of NCL/IPV.  It displays the Verify Preview
motion interface, allowing you to simulate the material removal for the
Preview motion. This button is only active when you have a valid NCL/IPV
license and Preview motion is displayed on the screen.

Geometry
--------
Pressing the Geometry button the first time will hide all unused geometry 
from the screen, leaving only the geometry that was selected during this 
session  displayed in the selected colors.  The 'Unused Geometry' field
in the 'Colors' section defines whether the unused geometry will be
invisible or just faded.

Pressing this button a second time will redisplay the unused geometry.

View
----
Takes down the form(s) and enters dynamic viewing.
