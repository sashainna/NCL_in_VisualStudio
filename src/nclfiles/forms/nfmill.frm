#HEADER#
/TITLE/ Flowline Milling
/POSITION/ 0,0
/SIZE/ 440,180

#SECTION#
/NAME/ FMill
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Surface to Machine
/POSITION/ 10,12
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/PICTURE/Fmill,"Surface to Machine",3,67,45,85

#EDIT#
/LABEL/
/POSITION/ 85,12
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Bounding Curve
/POSITION/ 10,29
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/PICTURE/Fmill,"Boundary Curve",55,70,92,87

#EDIT#
/LABEL/
/POSITION/ 85,29
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#PICTUREBOX#
/FILE/ FMill.jpg
/NAME/ FMill
/POSITION/ 220,10
/SIZE/ 150,125


#SECTION#
/NAME/ Motion Type
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Passes:
/POSITION/ 10,12,47,12
/SIZE/ 85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Scallop", "Number"
/PICTURE/ Motion Type,"Scallop",2,18,15,38,0
/PICTURE/ Motion Type,"Number",15,38,37,49,1

#EDIT#
/LABEL/
/POSITION/ 100,12
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ Start:
/POSITION/ 10,29,47,29
/SIZE/ 85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default", "u,v", "Point"
/PICTURE/ Motion Type,"Default",55,25,71,37,0
/PICTURE/ Motion Type,"u,v",41,12,55,32,1
/PICTURE/ Motion Type,"Point",48,35,85,46,2

#EDIT#
/LABEL/
/POSITION/ 100,29
/SIZE/ 50,14
/TYPE/ UD_STRING
/PREC/ 8
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 145,29
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Direction :
/POSITION/ 10,46,47,46
/SIZE/ 85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Const U", "Const V"
/PICTURE/ Motion Type,"Const U",75,27,98,49,0
/PICTURE/ Motion Type,"Const V",75,7,98,26,1

#CHOICEBOX#
/LABEL/ Step calc:
/POSITION/ 10,63,47,63
/SIZE/ 85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Curr Toler","Toler", "Step"
/PICTURE/ Motion Type,"Curr Toler",2,62,20,69,0
/PICTURE/ Motion Type,"Toler",23,62,37,69,1
/PICTURE/ Motion Type,"Step",2,86,15,95,2

#EDIT#
/LABEL/
/POSITION/ 100,63
/SIZE/ 50,14
/TYPE/ UD_STRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ Omit :
/POSITION/ 10,80,47,80
/SIZE/ 85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off","Start","End","Both"
/PICTURE/ Motion Type,"Off",45,59,71,77,0
/PICTURE/ Motion Type,"Start",83,59,99,77,1
/PICTURE/ Motion Type,"End",83,78,99,96,2
/PICTURE/ Motion Type,"Both",45,78,71,96,3

#PICTUREBOX#
/FILE/ Fmill_MotionType.jpg
/NAME/ Motion Type
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Boundaries
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Boundary Condition:
/POSITION/ 10,12,85,12
/SIZE/ 115,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Contact", "On", "To", "Past"
/PICTURE/ Boundaries,"Contact",1,10,20,47,0
/PICTURE/ Boundaries,"On",25,10,48,47,1
/PICTURE/ Boundaries,"To",50,10,69,47,2
/PICTURE/ Boundaries,"Past",70,10,99,47,3

#CHOICEBOX#
/LABEL/ Boundary Intersections:
/POSITION/ 10,29,85,29
/SIZE/ 115,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default", "Thru", "Retract", "Down", "Both"
/PICTURE/ Boundaries,"Default",24,55,42,65,0
/PICTURE/ Boundaries,"Thru",25,66,42,72,1
/PICTURE/ Boundaries,"Retract",10,67,30,80,2
/PICTURE/ Boundaries,"Down",35,67,55,80,3
/PICTURE/ Boundaries,"Both",25,81,45,90,4

#CHOICEBOX#
/LABEL/Direction:
/POSITION/ 135,29,170,29
/SIZE/ 75,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Shortest", "Same", "CCLW", "CLW"
/PICTURE/ Boundaries,"Shortest",65,73,90,79,0
/PICTURE/ Boundaries,"Same",65,80,90,86,1
/PICTURE/ Boundaries,"CCLW",65,94,90,100,3
/PICTURE/ Boundaries,"CLW",65,87,90,93,2

#CHOICEBOX#
/LABEL/ Rapto:
/POSITION/ 40,46,70,46
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Dist", "Plane", "Surface"

#EDIT#
/LABEL/
/POSITION/ 115,46
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 160,46
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Fedrat:
/POSITION/ 40,63,70,63
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current", "Rapid", "Value"

#EDIT#
/LABEL/
/POSITION/ 115, 63
/SIZE/ 50,14
/TYPE/ UD_SCAUNITLESS
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ Retract:
/POSITION/ 40,80,70,80
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Dist", "Plane", "Surface"

#EDIT#
/LABEL/
/POSITION/ 115,80
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 160,80
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Fedrat:
/POSITION/ 40,97,70,97
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current", "Rapid", "Value"

#EDIT#
/LABEL/
/POSITION/ 115,97
/SIZE/ 50,14
/TYPE/ UD_SCAUNITLESS
/PREC/ 8
/LEN/ 8

#CHECKBOX#
/LABEL/ Avoid:
/POSITION/ 10,114
/SIZE/ 70,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Thick:
/POSITION/ 40,131,62,131
/SIZE/55,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 6

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 100,131
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 150,131
/SIZE/ 55,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Final Pass:
/POSITION/ 10,148
/SIZE/ 70,15
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Fedrat:
/POSITION/ 40,165,70,165
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current", "Value"

#EDIT#
/LABEL/
/POSITION/ 115,165
/SIZE/ 50,14
/TYPE/ UD_SCAUNITLESS
/PREC/ 8
/LEN/ 8

#PICTUREBOX#
/FILE/ Fmill_Boundaries.jpg
/NAME/ Boundaries
/POSITION/ 220,20
/SIZE/ 150,125

#SECTION#
/NAME/Entry / Exit
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Rapto:
/POSITION/ 10,15,35,15
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "None", "Dist", "Plane", "Surface"

#EDIT#
/LABEL/
/POSITION/ 83,15
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 125,15
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Fedrat:
/POSITION/ 35,32,65,32
/SIZE/ 85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current", "Rapid", "Value"

#EDIT#
/LABEL/
/POSITION/ 125,32
/SIZE/ 50,14
/TYPE/ UD_SCAUNITLESS
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ Retract:
/POSITION/ 10,49,35,49
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "None", "Dist", "Plane", "Surface"

#EDIT#
/LABEL/
/POSITION/ 83,49
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 125,49
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Fedrat:
/POSITION/ 35,66,65,66
/SIZE/ 85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current", "Rapid", "Value"

#EDIT#
/LABEL/
/POSITION/ 125,66
/SIZE/ 50,14
/TYPE/ UD_SCAUNITLESS
/PREC/ 8
/LEN/ 8

#PICTUREBOX#
/FILE/ Fmill_EntryExit.jpg
/NAME/ Options
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Colors
/COLOR/ DEFAULT

#COLOR#      
/LABEL/ Surface:
/POSITION/ 10,12, 70, 12
/SIZE/90,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Curve:
/POSITION/ 110,12, 175, 12
/SIZE/95,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Entry Rapto:
/POSITION/ 10,29,70,29
/SIZE/90,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Entry Retract:
/POSITION/ 110,29, 175, 29
/SIZE/95,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Start Point:
/POSITION/ 10,46, 70, 46
/SIZE/90,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Avoidance Geomtry:
/POSITION/ 110,46, 175,46
/SIZE/95,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Boundary Rapto:
/POSITION/ 10,63,70,63
/SIZE/90,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Retract Plane:
/POSITION/ 110,63,175,63
/SIZE/95,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#CHOICEBOX#
/LABEL/ Unused Geometry:
/POSITION/ 10,80,70,80
/SIZE/105,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Hide","Fade"

#COLOR#
/LABEL/ Color:
/POSITION/ 130,80, 175,80
/SIZE/ 75,14
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
/FILE/fmill.mp4
/POSITION/ 380,136
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
<FMill>
Flowline Milling
================
This form is used to generate a Flowline Milling operation and output
a FMILL statement.

Surface to Machine
------------------
Pick this button to select the surface to be FMILLed, or type the
surface name in the adjacent field.

Bounding Curve
--------------
Pick this button to select an optional curve to limit the motion or
type the curve name in the adjacent field.  If no curve is picked, the
motion will be limited by the surface boundary.

(Text Field)
------------
This field will display the label of the selected part geometry. You
can also  manually type the label of the geometry in this field.

<END_SEC>
<Motion Type>
Motion Type
===========
The Motion Type section controls the settings used for creating the
Flowline Milling motion For Passes, Start, Direction, Steps and Omit.

Passes:
-------
Pick Scallop to have NCL calculate the number of passes using the
scallop height specified in the next field. Pick Number to specify the
number of passes required.

Start:
------
Pick Default to have the motion start nearest to the u=0 and v=0 point
on the surface, pick u,v to have motion start nearest to the u and v
values specified or pick point to have motion start nearest to the
point specified.

Direction:
----------
Pick Const U to machine along the constant u flowlines or pick Const V
to machine alone the constant v flowlines.

Step Calc:
----------
This specifies the maximum number of points created along any flowline
and the current machining tolerance will be used to create the points
on the boundary.

The choices are 'Current Tolerance', 'Tolerance' and 'Step'. Caution
should be used when specifying 'Step' especially with complex surfaces.

Omit:
-----
OFF will keep all calculated passes of the FMILL motion, START will
eliminate the first pass only, END will eliminate the last pass only,
or BOTH will eliminate the first and last passes of the FMILL motion.

<END_SEC>
<Boundaries>
Boundaries
==========
The Boundaries section controls the Boundary conditions and intersections
plus avoidances. Modifiers (TO, ON, or PAST), and the multiple
flowline-boundary intersections behavior.

Boundary Condition 
------------------
This controls the relationship of the cutter to the outside boundary.

Contact:
--------
Specifies the tool is in contact with the surface boundary.

On:
---
Specifies the tool end is positioned above the boundary, while the
cutter is in contact with the surface itself. This corresponds to the
“ON” parameter of the command syntax.

To:
---
Specifies the tool is positioned on the inside of the boundary, as if
the boundary with this modifier were used as a Check Surface during
each milling past. This corresponds to the “TO” parameter of the
command syntax.

Past:
-----
Specifies the tool is positioned on the outside of the boundary, as if
the boundary with this modifier were used as a Check Surface during
each milling past. This corresponds to the “PAST” parameter of the
command syntax.

Boundary Intersections
----------------------
This controls the motion behavior when there are multi-intersection
between the flowlines and the outside boundary.

Default:
--------
Specifies motion will only be generated along the flowlines before the
first intersection. There will be no motion generated after the first
intersection. This is the default condition. 

Thru:
-----
Specifies all the intersections are ignored. Motion will be generated
from this end of the outside boundary to the other end of the outside
boundary.

Retract:
--------
Specifies the tool will retract at the first intersection, move to the
point above the next intersection and move down on the surface along
the flowline. This is repeated until the other end of the outside
boundary is reached.

Down:
-----
Specifies when an intermediate boundary intersection is encountered,
the pass leaves the flowline and takes the shortest way around the
boundary until it is reached again at the next intersection, then the
pass continues along the flowline.

Both:
-----
Specifies when an intermediate boundary intersection is first encountered,
the pass leaves the flowline and goes around the boundary until the
flowline is regained. The next time the same boundary is encountered,
instead of going around the already cut pass, the cutter is retracted
and moved to the other side of the boundary, where it is moved back to
the surface.

Direction
---------
The boundary cut direction can be specified if "DOWN" or "BOTH" is
specified. The choices are: Shortest, Same, CCLW or CLW.

Shortest:
---------
The pass takes the shortest way around the boundary. This is the
default.

Same:
-----
The shortest path is calculated only once for each boundary - each
subsequent cut around the boundary is made toward the previously cut
part of the boundary. (So the cutter stays down as little as
possible)

CCLW:
-----
The cut goes counter-clockwise (as viewed along the tool axis) around
the outer boundary and clockwise around the inner boundaries.

CLW:
----
The cut goes clockwise (as viewed along the tool axis) around the
outer boundary and counter-clockwise around inner boundaries.

Rapto
-----
Pick None for no entry move, Distance to start at the distance
specified above the first point on the surface, or Plane or Surface
to start on the selected geometric entity.  

Select Current to have the entry move done at the current feed rate, 
Rapid to have the entry move done at the rapid feed rate, or value to
have the entry move done at the feed rate specified.

Retract
-------
Pick None for no exit move, Distance to end at the distance specified
above the last point on the surface or Plane or Surface to end on the
selected entity. 

Select Current to have the exit move done at the current feed rate, 
Rapid to have the exit move done at the rapid feed rate, or value to
have the exit move done at the feed rate specified.

Avoid
-----
A flowline pass motion will stop at an Avoidance Entity, then move
along it to the next flowline and continue. An Avoidance Entity can be
any of the following geometry types; surface, plane, curve, line and
circle. Up to 40 Avoidance Entities are allowed in a single FMILL
command.

Thick:
------
The thick parameter to apply to selected Avoidance Entities. Each
Avoidance Entity can have an optional thick parameter.

Final Pass
----------
Click this button to perform a a final pass around the boundary. This
option is active only when Down, Retract, or Both is selected for
Boundary Intersections. If it is an inside boundary, the last pass
around it is performed when the tool touches it for the last time.

Fedrat:
-------
Specify the feedrate for the final pass motion. Toggle between Current
and Value.

Current - Specifies the final pass is done at the current primary
feedrate. Value - Specifies the final pass is done at a user specified
value. This activates the next Text Field to allow the user to enter
the specified feedrate value.

<END_SEC>
<Entry / Exit>
Entry / Exit
============
The Entry / Exit section controls the settings used for entering onto
and exiting off of the Flowline milling surface.

Rapto
-----
Pick None for no entry move, Distance to start at the distance specified
above the first point on the surface, or Plane or Surface to start on
the selected geometric entity. 

Fedrat:
-------
Select Current to have the entry move done at the current feed rate, 
Rapid to have the entry move done at the rapid feed rate, or value to
have the entry move done at the feed rate specified.

Retract
-------
Pick None for no exit move, Distance to end at the distance specified
above the last point on the surface or Plane or Surface to end on the
selected entity. 

Fedrat:
-------
Select Current to have the exit move done at the current feed rate, 
Rapid to have the exit move done at the rapid feed rate, or value to
have the exit move done at the feed rate specified.

<END_SEC>
<Colors>
Colors
======
The Colors section defines the colors that will be used to highlight
the geometry selected while in the Flowline Milling form.  All entities
that can be picked  from the screen are listed in this section.

Unused Geometry:
----------------
When pressing the 'Geometry' Action item button to "hide" geometry not
used in  the Flowline Milling operation you have the option of either
invisible this geometry (Hide) or displaying the geometry as translucent
and with dotted lines (Fade). The Color field applies to the faded
geometry.

<END_SEC>
<ALL>
Action Buttons
==============
The Action Buttons are located at the right hand of the motion generation
forms and allow you to perform specific actions that will assist you
in visualizing  the results of the motion form settings.

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
entered. This button is useful after pressing the Apply button if you
want to start fresh or when you have made numerous changes to the form
settings and are not getting the output you desire.

Playback
--------
Displays Playback Preview Motion interface, allowing you to step through
and animate the motion generated using the Preview button.  This button
is only  active when Preview motion is displayed on the screen.

Verify
------
Pressing this button allows you to verify the Preview motion by using
the material removal process of NCL/IPV.  It displays the Verify Preview
motion interface, allowing you to simulate the material removal for the
Preview motion. This button is only active when you have a valid NCL/IPV
license and Preview motion is displayed on the screen.

Geometry
--------
Pressing the Geometry button the first time will hide all unused
geometry  from the screen, leaving only the geometry that was selected
during this session  displayed in the selected colors.  The 'Unused
Geometry' field in the 'Colors' section defines whether the unused
geometry will be invisible or just faded.

Pressing this button a second time will redisplay the unused geometry.

View
----
Takes down the form(s) and enters dynamic viewing.
