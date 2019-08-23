#HEADER#
/TITLE/ Letter Engraving
/POSITION/ 50,30
/SIZE/ 445,175

#SECTION#
/NAME/ Text
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Text
/POSITION/ 10,10
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect
/POSITION/ 60,10
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Depth of Cut:
/POSITION/ 10,27
/SIZE/90,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Lift Between Letters:
/POSITION/ 10,44
/SIZE/100,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#PICTUREBOX#
/FILE/ Engrave_Text.jpg
/NAME/ profil1
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Levels
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Top of Part:
/POSITION/ 10,10
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "None","Top Plane","Distance","Passes"

#EDIT#
/LABEL/
/POSITION/ 110,10
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 160,10
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Step Type:
/POSITION/ 10,27
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Passes","Max-Step"

#EDIT#
/LABEL/
/POSITION/ 110,27
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#PICTUREBOX#
/FILE/ Engrave_Levels.jpg
/NAME/ engrave2
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Positioning
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Clearance:
/POSITION/ 10,10
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "Current","Plane","Distance","Incremental"

#EDIT#
/LABEL/
/POSITION/ 110,10
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 165,10
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Rapto:
/POSITION/ 10,27
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "Entry","Plane","Distance","Incremental"

#EDIT#
/LABEL/
/POSITION/ 110,27
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 165,27
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Retract:
/POSITION/ 10,44
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","On","Plane","Distance","Incremental"

#EDIT#
/LABEL/
/POSITION/ 110,44
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 165,44
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ Engrave_Positioning.jpg
/NAME/ engrave3
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Feed Rates
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ General:
/POSITION/ 10,30, 50,30
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,30
/SIZE/50,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Position:
/POSITION/ 10,47, 50,47
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,47
/SIZE/90,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Retract:
/POSITION/ 10,64, 50,64
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,64
/SIZE/50,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Entry:
/POSITION/ 10,81, 50,81
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,81
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#DISPLAY#
/LABEL/ Current Feed rate:
/POSITION/ 10,12
/SIZE/ 120,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8
/COLOR/ BLUE,DEFAULT
/PCOLOR/ BLUE,DEFAULT

#PICTUREBOX#
/FILE/ Engrave_FeedRates.jpg
/NAME/ engrave4
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION_BAR#

#SECTION#
/NAME/ Colors
/COLOR/ *DEFAULT

#COLOR#
/LABEL/ Text:
/POSITION/ 10,10, 70,10
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Top Plane:
/POSITION/ 125,10, 175,10
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Clearance Plane:
/POSITION/ 10,27, 70,27
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Rapto Plane:
/POSITION/ 125,27, 175,27
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Retract Plane:
/POSITION/ 10,44, 70,44
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Unused Geometry:
/POSITION/ 10,61
/SIZE/102,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Hide","Fade"

#COLOR#
/LABEL/ Color:
/POSITION/ 125,61, 175,61
/SIZE/ 90,14
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

#HELP#
<Text>
Letter Engraving
================
This form is used to engrave annotation (lettering). The annotation can be
machined with multiple depth passes.

Text
----
Pressing the Text button allows you to select the text annotation to engrave.
Multiple annotations may be selected.  A separate PROFIL command will
be output for each annotation.

Annotation created in NCL V9.6 or earlier is not labeled and cannot be selected
for engraving.  If multiple entities are selected for engraving and at least
one is a labeled annotation, then no error message will be output but any
unlabeled annotation will be ignored.  If all entities selected are unlabeled
annotations, then an error message will be displayed stating that no valid
entities have been selected.

Unlabeled annotation that is renamed with a valid label can be engraved.

Deselect All
------------
Deselects all previously selected annotation.

Depth of Cut:
-------------
The Depth of Cut value specifies the depth below the top level of the annotation
to machine to.  The top level defaults to the defined level of the annotation or
can be changed using the Levels section.

Lift Between Letters:
---------------------
The Lift Between Letters value specifies the distance above the top level of the
annotation to position the tool when moving between the line segments of the
annotation.
<END_SEC>
<Levels>
Levels
======
The levels section allows you to define the level at which the annotation should
be engraved (top of part) and optionally defines multiple depth cuts to take.

Top of Part
-----------
Defines the top level of the annotation 'None' will engrave the annotation
at the level that the annotation is defined (minus the depth of cut value).
'Top Plane' defines the top of the part and is used instead of the annotation
level.  'Distance' defines the distance from the calculated bottom of the
annotation (lowest point) to the top of the part. 'Passes' uses the top of the
annotation as the top machining level and defines the number of passes to take
to machine to the specified depth level.

Select
------
This button is only active when Levels is set to 'Top Plane'.  Pressing this
button allows you to select the plane or planar surface to use as the top
of the part.

Step Type:
----------
This field will only be enabled when the Levels field is not set to 'None'.
'Passes' will use the number of passes entered in this field to calculate the
depth distance and can only be specified when the Levels field is set to
'Top Plane' or 'Distance'.  'Max-Step' specifies the (maximum) step of each
depth pass.  When the Levels field is set to 'Passes', this value is the actual
step down distance used.

<END_SEC>
<Positioning>
Positioning
===========
The Positioning section controls the positioning and tool retraction features
of Letter Engraving.

Clearance:
----------
The clearance level of Letter Engraving is used to position the tool prior to
engraving the annotation and can be used as the tool retraction level at the
end of the engraving motion.

'Current' uses the current tool position to specify the clearance level.  A
clearance plane will be created that goes through the tool end point and is
perpendicular to the tool axis.  'Plane' allows the user to specify/select a
predefined plane.  'Distance' specifies a distance above the top of the part
level for the clearance plane.  'Incremental' specifies an incremental distance
above the entry point when positioning above the entry location.

Select
------
This button is only active when the Clearance type is set to 'Plane'.  Pressing
this button allows you to select the plane or planar surface to use as the
clearance plane.

Rapto:
------
The Rapto field specifies the distance above the entry location to position
the tool at the Positioning feed rate.  The remainder of the entry move will be
made at the Entry feed rate.

'Entry' will position to the start of the entry move using the Positioning feed
rate.  'Plane' allows the user to specify/select a predefined plane.  'Distance'
specifies a distance above the top of the part level for the rapto plane.
'Incremental' specifies an incremental distance above the first point of the
annotation to use as the rapto plane.

Select
------
This button is only active when the Rapto type is set to 'Plane'.  Pressing
this button allows you to select the plane or planar surface to use as the
rapto plane.

Retract:
--------
This field allows you to specify the retract logic to use at the end of the
letter engraving.

'Off' performs no tool retraction at the end of the engraving motion.  'On'
retracts the tool to the Clearance level at the end of the engraving motion.
'Plane' allows the user to specify/select a predefined plane.  'Distance'
specifies a distance above the top of the part level to retract the tool.
'Incremental' specifies an incremental distance above the last point of the
annotation to use as the retract plane.

Select
------
This button is only active when the Final Retract type is set to 'Plane'.
Pressing this button allows you to select the plane or planar surface to use as
the retract plane.

<END_SEC>
<Feed Rates>
Feed Rates
==========
The Feed Rates section defines the feed rates to use when Engraving Letters.
Each feed rate has the following modes that can be programmed.

   Current = Uses the currently programmed feed rate.
   Value   = Allows you to specify an absolute value as the feed rate.
   Rapid   = Uses RAPID.
   Factor  = Enter a percentage of the programmed feed rate to use, for example
             .85 specifies 85% of the programmed feed rate.

General:
--------
Defines the feed rate to use during the basic engraving motion.

Position:
---------
Defines the feed rate to use when positioning the tool to and from the clearance
plane and rapto plane.

Retract:
--------
Defines the feed rate to use when the tool is retracted at the end of the
engraving motion and between line segments.

Entry:
------
Defines the feed rate to use during the entry move.
<END_SEC>
<Colors>
Colors
======
The Colors section defines the colors that will be used to highlight the
geometry selected while in the Letter Engraving form.  All entities that
can be picked from the screen are listed in this section.

Unused Geometry:
----------------
When pressing the 'Geometry' Action item button to "hide" geometry not used
in the Letter Engraving operation you have the option of either invisible
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
Previews the Letter Engraving motion without writing out the command or
permanently storing the generated motion.  Press the OK or Apply button to
write out the command and motion.  The generated command can be saved after the
preview even if there is an error.  To save the command, make no changes to the
settings and press the OK button.  The command will then be available for
editing in the command line.

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
