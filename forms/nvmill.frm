#HEADER#
/TITLE/ VoluMill Pocket
/POSITION/ 50,30
/SIZE/440,180

#SECTION#
/NAME/ Boundaries
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Perimeter
/POSITION/ 10,12
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/
/POSITION/ 60,12
/SIZE/ 38,58
/TYPE/UD_DASSTRING
/CHOICES/ "In","Offset"

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 103,12
/SIZE/65,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Open Sides
/POSITION/ 10,29
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 60,29
/SIZE/65,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Use Perimeter Surface Inner Boundary as Island(s)
/POSITION/ 10,46
/SIZE/180,15
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Use Perimeter Surface as Bottom
/POSITION/ 10,63
/SIZE/140,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Island
/POSITION/ 10,80
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 60,80
/SIZE/65,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Boundary Thick:
/POSITION/ 10,97,65,97
/SIZE/90,13
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 6

#EDIT#
/LABEL/ Open Boundary Thick:
/POSITION/ 105,97,180,97
/SIZE/90,13
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 6

#PICTUREBOX#
/FILE/ VoluMill_Boundary.jpg
/NAME/ Boundary
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Levels
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Pocket Bottom
/POSITION/ 10,12
/SIZE/ 62,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 78,12
/SIZE/40,13
/TYPE/ UD_DASSTRING
/LEN/ 16
/PREC/ 8

#EDIT#
/LABEL/ Bottom Thick:
/POSITION/ 10,29,78,29
/SIZE/90,13
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 10

#CHOICEBOX#
/LABEL/ Top:
/POSITION/ 10,46
/SIZE/65,33
/TYPE/UD_DASSTRING
/CHOICES/ "Distance","Plane"

#EDIT#
/LABEL/
/POSITION/ 78,46
/SIZE/60,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 16

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 128,46
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ VoluMill_Levels.jpg
/NAME/ Level
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Options
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Predrilled Holes
/POSITION/ 10,12
/SIZE/ 65,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 80,12
/SIZE/65,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Outputs for 5-axis
/POSITION/ 10,46
/SIZE/180,15
/TYPE/UD_DASSTRING

#PICTUREBOX#
/FILE/ VoluMill_Options.jpg
/NAME/ Options
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Colors
/COLOR/ *DEFAULT

#COLOR#
/LABEL/ Perimeter:
/POSITION/ 10,12,72,12
/SIZE/ 97,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Island:
/POSITION/ 115,12, 155,12
/SIZE/ 75,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Open Sides:
/POSITION/ 10,29, 72,29
/SIZE/ 97,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Line Style:
/POSITION/ 115,29, 155,29
/SIZE/99,60
/TYPE/UD_DASSTRING
/CHOICES/ "Solid","Small Dash","Dots","Center Line","Phantom"
/CHOICES/ "Large Dash","Dash Dot","Dash Space"

#COLOR#
/LABEL/ Pocket Bottom:
/POSITION/ 10,46, 72,46
/SIZE/ 97,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Pocket Top:
/POSITION/ 115,46, 155,46
/SIZE/ 75,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ PreDrilled Holes:
/POSITION/ 10,63, 72,63
/SIZE/ 97,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Unused Geometry:
/POSITION/ 10,80,72,80
/SIZE/97,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Hide","Fade"

#COLOR#
/LABEL/ Color:
/POSITION/ 115,80, 155,80
/SIZE/ 75,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#PICTUREBOX#
/FILE/ highlight.jpg
/NAME/ highlight
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ All
/COLOR/ BLACK

#PUSHBUTTON#
/LABEL/ VoluMill Modals
/POSITION/ 85,120
/SIZE/60,14
/TYPE/UD_DASSTRING

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
/FILE/vmill.mp4
/POSITION/ 380,136
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
<Boundaries>
VoluMill Pocket
===============
This form is used to machine a pocket using the VoluMill high performance
pocketing motion.

Boundaries
==========
Perimeter:
----------
Takes down the form, brings up the SELECT menu. Allows selecting a list of 
entities - each perimeter entity accounts for a separate VMPOCK command. Note
that having selected more than one perimeter prevents you from selecting 
island geometry.

Any curve can be chosen as a perimeter. It should be closed however, 
otherwise the pocket algorithm will replace the end point to close it (the 
way it has been with the traditional perimeter geometry: composite curves, 
PATERNs, subscripted arrays of points).

A surface can be selected as a perimeter. The algorithm will internally use 
a composite curve made of the surface outer boundary.

Deselect All
------------
Deselects all the previously selected Perimeter Surfaces.

Direction-mod
-------------
The options for the pocket perimeter allow you to specify a closed pocket (In)
or a pocket boundary where NCL will determine the open/closed side (Offset).

Open Sides
----------
Allows selecting of open boundary sides.  Open sides can be defined for
composite curves and subscripted point arrays. 

To select the open side:

 - Select the first point or component defining the open side
 - Select the direction the open side will follow along the perimeter geometry
 - Select the last point or component defining the open side

The open side will be displayed using the line style and color defined.
To define additional open sides for a single entity, you can press the Open
Sides button again for each open side.

Deselect All
------------
Deselects all selected first point(s), direction, and last point(s).

Line Style
----------
Display style for open sides of a composite curve.  The line style will be
used when displaying the components of the composite curve perimeter geometry
that have been labeled as open.

Use Perimeter Surface Inner Boundary as Island(s)
-------------------------------------------------
If the perimeter surface is trimmed and has inner boundaries, a user could
choose to use the inner boundaries as pocket islands. This is automatically
disabled if all the trimmed surfaces were deselected by the user.

Use Perimeter Surface as Bottom
-------------------------------
A user could choose to use the perimeter surface as the pocket bottom. If 
chosen so, the Pocket Bottom fields become disabled.

Island
------
Takes down the form, brings up the SELECT menu. Allows selecting a list of 
entities. Now a user can select any closed curve as island geometry. Also, 
a trimmed surface with inner boundary(s) could be chosen, in which case the 
algorithm will use the inner boundary curves.

Deselect All
------------
Deselects all the previously selected island geometry.

Boundary Thick
--------------
Used for the pocket geometry (DS) thick parameter.

Open Side Thick
-------------------
Used for the pocket geometry (DS) thick parameter for open boundary sides when
the Offset perimeter modifier is selected.

<END_SEC>
<Levels>
Levels
======

Pocket Bottom
-------------
Contains the current pocket bottom, unless a pocket perimeter surface is 
used as pocket bottom (see above).

Bottom Thick
------------
Used for the pocket bottom (PS) thick parameter. The default value is "0"

Pocket Top
----------
Choose between using a plane or a distance for pocket top.  The text field
contains the current pocket top plane or distance. The default is distance
with a value of "0"

Select
------
Press the Select button to select the retract geometry.  This button is only 
active with the Retract type is set to Entity. A plane or a point can be 
selected.

(Text Field)
------------
This field will display the label of the selected part geometry.  You can also 
manually type the label of the geometry in this field.

<END_SEC>
<Options>
Options
=======

Predrilled Holes
----------------
Allows the selection of points to use as the positions of predrilled holes.
VoluMill will use these holes as the entry to the pocket instead of ramping
into the material.  These holes must be drilled prior to performing the
pocketing motion. Only point or point-vector entities are allowed.

Deselect All
------------
Deselects all the previously selected points for predrilled holes.

<END_SEC>
<Colors>
Colors
======
The Colors section defines the colors that will be used to highlight geometry
selected while in the VoluMill Pocket form.  All entities that can be picked 
from the screen are listed in this section.

Unused Geometry:
----------------
When pressing the 'Geometry' Action item button to "hide" geometry not used in 
the VoluMill Pocket operation you have the option of either invisible this 
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
material removal process of NCLIPV.  It displays the Verify Preview motion 
interface, allowing you to simulate the material removal for the Preview 
motion. This button is only active when you have a valid NCLIPV license and 
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
