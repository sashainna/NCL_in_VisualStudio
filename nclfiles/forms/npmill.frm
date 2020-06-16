#HEADER#
/TITLE/ Port Surface Machining
/POSITION/ 0,0
/SIZE/ 440,180

#SECTION#
/NAME/ PMill
/COLOR/ DEFAULT

#EDIT#
/LABEL/ Surface:
/POSITION/ 10,12
/SIZE/60,14
/TYPE/UD_DASSTRING
/LEN/ 8
/PREC/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 90,12
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect...
/POSITION/ 10,30
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ pmill.jpg
/NAME/ pmill
/POSITION/ 220,10
/SIZE/ 125,125

#SECTION#
/NAME/ Motion Type
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Motion Type:
/POSITION/ 10,12,55,12
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Linear","Helix","Arc"
/PICTURE/MotionType,"Linear",1,1,99,50,0
/PICTURE/MotionType,"Helix",1,51,50,99,1
/PICTURE/MotionType,"Arc",51,51,99,99,2

#EDIT#
/LABEL/ Radius:
/POSITION/ 105,12
/SIZE/40,14
/TYPE/ UD_SCAVAL
/LEN/ 6
/PREC/ 8

#CHOICEBOX#
/LABEL/ Orientation:
/POSITION/ 105,12
/SIZE/ 90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "CLW", "CCLW"

#EDIT#
/LABEL/ Step
/POSITION/ 10,29
/SIZE/ 40,14
/TYPE/ UD_SCAVAL
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ Tolerance:
/POSITION/ 10,46,55,46
/SIZE/ 90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Curr Toler","Tolerance"

#EDIT#
/LABEL/
/POSITION/ 105,46
/SIZE/ 40,14
/TYPE/ UD_SCADISTANCE
/PREC/ 8
/LEN/ 8

#LABEL#
/LABEL/ Direction:
/POSITION/ 10,63
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/Planes:
/POSITION/ 55,63,85,63
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#PUSHBUTTON#
/LABEL/ <-  Start
/POSITION/ 130,63
/SIZE/ 35,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ OR Select
/POSITION/ 170,63
/SIZE/ 39,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 85,80
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#PUSHBUTTON#
/LABEL/ <-  End
/POSITION/ 130,80
/SIZE/ 35,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ OR Select
/POSITION/ 170,80
/SIZE/ 39,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Retract at Pass End:
/POSITION/ 10,99, 80, 99
/SIZE/ 110,40
/TYPE/ UD_DASSTRING
/CHOICES/ "None", "Dist", "Entity"

#EDIT#
/LABEL/
/POSITION/ 123,99
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 164,99
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Fedrat:
/POSITION/ 81,116,110,116
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current", "Rapid", "Value"

#EDIT#
/LABEL/
/POSITION/ 155,116
/SIZE/ 50,14
/TYPE/ UD_SCAUNITLESS
/PREC/ 8
/LEN/ 8

#PICTUREBOX#
/FILE/ Pmill_MType.jpg
/NAME/ MotionType
/POSITION/ 220,10
/SIZE/ 125,125

#SECTION#
/NAME/ Entry 
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Start:
/POSITION/ 10,12,40,12
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default", "Point"
/PICTURE/EntryExit,"Default",70,5,96,20,0
/PICTURE/EntryExit,"Point",5,20,25,40,1

#EDIT#
/LABEL/
/POSITION/ 85,12
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 130,12
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ Pmill_EntryExit.jpg
/NAME/ EntryExit
/POSITION/ 220,10
/SIZE/ 150,60

#SECTION#
/NAME/ Colors
/COLOR/ DEFAULT

#COLOR#      
/LABEL/ Surface:
/POSITION/ 10,12, 68, 10
/SIZE/88,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Start Point:
/POSITION/ 10,29,68,27
/SIZE/88,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Direction:
/POSITION/ 110,29, 160, 27
/SIZE/80,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Pass Retract:
/POSITION/ 10,46,68,44
/SIZE/88,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#CHOICEBOX#
/LABEL/ Unused Geometry:
/POSITION/ 10,63,70,61
/SIZE/105,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Hide","Fade"

#COLOR#
/LABEL/ Color:
/POSITION/ 130,63, 160,61
/SIZE/ 60,14
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
/FILE/smill.mp4
/POSITION/ 380,136
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
<SMill>
SMill
Multiple Surface Machining
==========================
This form is used to machine multiple surfaces using scrub/lace type motion by
issuing a SMILL command.

Select
------
Used to select the surfaces to machine or put on the layer.

(Text Field)
------------
This field will display the label of the selected surfaces.  You can also 
manually type the label of the geometry in this field.

Include on Layer
----------------
If checked, the selected surfaces will be put on the specified layer by
issuing the appropriate "DRAFT/MODIFY=sf2,sf3,LAYER=4" command.  This command
will be created prior to the SMILL command.  The SMILL command will
then reference the layer number rather than the actual surfaces selected.

Layer
-----
Used to select a layer number from a list of existing layers. This field is
active only when the Select by Layer box is checked.  The text field contains
the layer number from which all of the surfaces on the layer will be
projected, including any surfaces selected in this form.

Select by Layer
---------------
If checked, the surfaces on the specified layer will be included in the SMILL
command.

Highlight Layer
---------------
Highlights the surfaces currently residing on the selected layer.

Deselect All
------------
Deselects all previously selected surfaces and layers.

<END_SEC>
<Motion Type>
Motion Type
===========
This Section controls the Motion Type, Passes, direction and Tolerances.

Motion Type:
------------
The cutting method can be specified as 'Scrub', 'Combin', or 'Lace'. 

'Scrub' performs scrub type motion (back and forth) across the surfaces.

'Combin' also performs scrub type motion, except it skips a pass on the back
motion and then cuts the pass that was skipped on the forth motion so that the
majority of the material will be cut in one direction, with the opposite
direction performing a cleanup type cut.

'Lace' performs lace type motion (retracts the tool at the end of each pass and
repositions it to the starting side so that all cutting will be done in one
direction.  An optional retract plane/surface and feed rate for the retract
motion at the end of each pass can be specified.  'None' specifies that the
tool will not retract between passes, 'Dist' specifies a fixed distance to
retract the tool, 'Entity' specifies a plane or surface to retract the tool to.

Passes:
-------
'Scallop' causes NCL to calculate the number of passes based on the scallop
height specified in the next field. Each pass can be at a varying distance
from the previous pass based on the curvature of the surface(s).  'Stepover'
specifies the pass stepover distance.  The number of passes will be based on
this stepover distance. 'Both' specifies that the stepover distance is used
for the flat area passes and scallop height is used for slope area passes when
a bull nose tool is defined. 'Passes' specifies the number of passes to
generate across the surface(s).

Tolerance:
----------
'Curr Toler' uses the default machining tolerance as specified by the TOLER
command for surface machining.  'Tolerance' allows you to enter a tolerance
value to use to calculate the motion for this surface machining.

Direction:
----------
Toggle between Planes and Vector. 'Planes' is used to machine the surfaces
between the two specified drive planes. The two drive planes must be parallel.
The motion will go in the same direction as the planes and the motion will go 
from the first plane to the second plane. 'Vector' defines the initial
direction of the motion.

Select
-------
The Select buttons allow you to pick the drive surface planes or initial
direction Vector based on "Direction" field.

Retract at Pass End:
--------------------
“None” specifies that the tool will not retract after the last pass.
“Distance” specifies a fixed distance to retract the tool.
“Entity” specifies a plane or surface to retract to.

Feed Rate
---------
Defines the feed rate to use for Retract at Pass End.
The feed rate has the following modes that can be programmed.

   Current = Uses the currently programmed feed rate.
   Rapid   = Uses RAPID
   Value   = Allows you to specify an absolute value as the feed rate.

Current:
--------
Uses the Current feed rate for the Retract at Pass End.

Rapid:
------
Uses the Rapid feed rate for the Retract at Pass End.

Value:
------
Uses a specified Value as the feed rate for the Retract at Pass End.

<END_SEC>
<Entry >
Entry 
============
The Entry / Exit section controls the settings used for entering onto and 
exiting off of the surface.

Start:
------
'Default' causes the motion start nearest to the last tool end point.  'Point'
will start the motion nearest to the selected point.

Rapto:
------
Pick None for no entry move, Distance to start at the distance
specified above the first point on the surface, or Plane or Surface
to start on the selected geometric entity.  
Select Current to have the entry move done at the current feed rate, 
Rapid to have the entry move done at the rapid feed rate,
or value to have the entry move done at the feed rate specified.

Select
------
Press the Select button to select the entry geometry.

Feed Rate
---------
Defines the Entry and Exit feed rates.

Fedrat:
-------
Specifies the entry move feed rate.  'Current' uses the currently programmed
feed rate, 'Rapid' makes the move in rapid mode, and 'Value' allows you to
enter an entry move feed rate.

Retract:
--------
Pick None for no exit move, Distance to end at the distance
specified above the last point on the surface or Plane or Surface to
end on the selected entity. 
Select Current to have the exit move done at the current feed rate, 
Rapid to have the exit move done at the rapid feed rate,
or value to have the exit move done at the feed rate specified.

Fedrat:
-------
Specifies the retract move feed rate.  'Current' uses the currently programmed
feed rate, 'Rapid' makes the move in rapid mode, and 'Value' allows you to
enter a retract move feed rate.

Select
------
Press the Select button to select the retract geometry. 

(Text Field)
------------
This field will display the label of the selected part geometry.  You can also 
manually type the label of the geometry in this field.

<END_SEC>
<Colors>
Colors
======
The Colors section defines the colors that will be used to highlight the 
geometry selected while in the Multiple Surface Milling form. All entities
that can be picked from the screen are listed in this section.


Unused Geometry:
----------------
When pressing the 'Geometry' Action item button to "hide" geometry not used in 
the Surface Milling operation you have the option of either invisible this 
geometry (Hide) or displaying the geometry as translucent and with dotted lines
(Fade). The Color field applies to the faded geometry.

<END_SEC>
<ALL>
Action Buttons
==============
The Action Buttons are located at the right hand of the motion generation forms
and allow you to perform specific actions that will assist you in visualizing 
the results of the motion form settings.

Preview
-------
Previews the motion without writing out the command or permanently storing the 
generated motion.  Press the OK or Apply button to write out the command and 
motion. The generated command can be saved after the preview even if there is
an error. To save the command, make no changes to the settings and press the 
OK button. The command will then be available for editing in the command line.

Apply
-----
Outputs and processes the command(s) without taking down the form so that other
motion can be created.

Reset Motion
------------
Resets all form fields to their settings/values when the form was first entered.
This button is useful after pressing the Apply button if you want to start fresh
or when you have made numerous changes to the form settings and are not getting
the output you desire.

Playback
--------
Displays Playback Preview Motion interface, allowing you to step through and 
animate the motion generated using the Preview button.  This button is only 
active when Preview motion is displayed on the screen.

Verify
------
Pressing this button allows you to verify the Preview motion by using the 
material removal process of NCL/IPV.  It displays the Verify Preview motion 
interface, allowing you to simulate the material removal for the Preview 
motion. This button is only active when you have a valid NCL/IPV license and 
Preview motion is displayed on the screen.

Geometry
--------
Pressing the Geometry button the first time will hide all unused geometry 
from the screen, leaving only the geometry that was selected during this 
session  displayed in the selected colors.  The 'Unused Geometry' field in 
the 'Colors' section defines whether the unused geometry will be invisible
or just faded.

Pressing this button a second time will redisplay the unused geometry.

View
----
Takes down the form(s) and enters dynamic viewing.
