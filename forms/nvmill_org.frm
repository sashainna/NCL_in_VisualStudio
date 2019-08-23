#HEADER#
/TITLE/ VoluMill Pocket
/POSITION/ 50,30
/SIZE/280,303

#PUSHBUTTON#
/LABEL/ Perimeter
/POSITION/ 15,22
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/
/POSITION/ 66,22
/SIZE/ 38,58
/TYPE/UD_DASSTRING
/CHOICES/ "In","Offset"

#COLOR#      
/LABEL/
/POSITION/ 108,22
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 168,22
/SIZE/65,15
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Open Sides
/POSITION/ 15,41
/SIZE/ 46,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/
/POSITION/ 66,42
/SIZE/57,40
/TYPE/UD_DASSTRING
/CHOICES/ "Solid","Small Dash","Dots","Center Line","Phantom"
/CHOICES/ "Large Dash","Dash Dot","Dash Space"

#COLOR#      
/LABEL/
/POSITION/ 128,41
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 188,41
/SIZE/65,15
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Use Perimeter Surface Inner Boundary as Island(s)
/POSITION/ 15,59
/SIZE/180,15
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Use Perimeter Surface as Bottom
/POSITION/ 15,76
/SIZE/140,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Island
/POSITION/ 15,93
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#
/LABEL/
/POSITION/ 60,93
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 120,93
/SIZE/65,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Boundary Thick:
/POSITION/ 15,110
/SIZE/60,13
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Open Boundary Thick:
/POSITION/ 120,110
/SIZE/60,13
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#FRAME#
/TITLE/ Boundaries
/POSITION/ 10,12
/SIZE/ 260,119

#EDIT#
/LABEL/ Pocket Bottom:
/POSITION/ 15,147
/SIZE/60,13
/TYPE/ UD_DASSTRING
/LEN/ 16
/PREC/ 16

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 145,147
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#
/LABEL/
/POSITION/ 190,147
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#EDIT#
/LABEL/ Bottom Thick:
/POSITION/ 15,164
/SIZE/60,13
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Top:
/POSITION/ 15,183
/SIZE/65,33
/TYPE/UD_DASSTRING
/CHOICES/ "Distance","Plane"

#EDIT#
/LABEL/
/POSITION/ 90,183
/SIZE/60,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 16

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 145,183
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#
/LABEL/
/POSITION/ 190,183
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#FRAME#
/TITLE/ Levels
/POSITION/ 10,137
/SIZE/ 260,64

#PUSHBUTTON#
/LABEL/ Predrilled Holes
/POSITION/ 15,211
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#
/LABEL/
/POSITION/ 80,211
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 140,211
/SIZE/65,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ VoluMill Modals
/POSITION/ 90,232
/SIZE/80,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 22,255
/SIZE/ 55,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 82,255
/SIZE/ 55,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Erase Motion
/POSITION/ 142,255
/SIZE/ 55,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Display Cutter
/POSITION/ 202,255
/SIZE/ 55,14
/TYPE/ UD_DASSTRING

#HELP#
===============
VoluMill Pocket
===============
This form is used to machine a pocket using the VoluMill high performance
pocketing motion.

Perimeter
---------
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

Direction-mod
-------------
The options for the pocket perimeter allow you to specify a closed pocket (In)
or a pocket boundary where NCL will determine the open/closed side (Offset).

Color
-----
Chooses a color to highlight the selected perimeter geometry.

Deselect All
------------
Deselects the currently selected perimeter geometry entities.

Open Sides
----------
Allows selecting of open boundary sides.  Open sides can be defined for
composite curves and subscripted point arrays. To select the open side:

 - Select the first point or component defining the open side
 - Select the direction the open side will follow along the perimeter geometry
 - Select the last point or component defining the open side

The open side will be displayed using the line style and color defined.
To define additional open sides for a single entity, you can press the Open
Sides button again for each open side.

Line Style
----------
Display style for open sides of a composite curve.  The line style will be
used when displaying the components of the composite curve perimeter geometry
that have been labeled as open.

Color
-----
Chooses a color to highlight the selected open sides of perimeter geometry.

Deselect All
------------
Deselects the currently selected open sides of perimeter geometry entities.

Use Perimeter Surface Inner Boundary as Island(s)
-------------------------------------------------
If the perimeter surface is trimmed and has inner boundaries, a user could
choose to use the inner boundaries as pocket islands.

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

Color
-----
Chooses a color to highlight the selected island geometry.

Deselect All
------------
Deselect the currently selected island geometry entities.

Boundary Thick
--------------
Used for the pocket geometry (DS) thick parameter.

Open Boundary Thick
-------------------
Used for the pocket geometry (DS) thick parameter for open boundary sides when
the Offset perimeter modifier is selected.

Pocket Bottom
-------------
Contains the current pocket bottom, unless a pocket perimeter surface is 
used as pocket bottom (see above).

Select
------
Allows picking a plane or planar surface as pocket bottom. 

Color
-----
Chooses a color to highlight the selected pocket bottom.

Bottom Thick
------------
Used for the pocket bottom (PS) thick parameter

Pocket Top
----------
Choose between using a plane or a distance for pocket top.  The text field
contains the current pocket top plane or distance.

Select
------
Allows picking a plane or planar surface as the pocket top - active only when 
Plane is chosen. 

Color
-----
Chooses a color to highlight the selected pocket top.

Predrilled Holes
----------------
Allows the selection of points to use as the positions of predrilled holes.
VoluMill will use these holes as the entry to the pocket instead of ramping
into the material.  These holes must be drilled prior to performing the
pocketing motion.

Color
-----
Chooses a color to highlight the selected predrilled hole positions.

Deselect All
------------
Deselects the currently selected predrilled hole positions.

VoluMill Modals
---------------
Opens the VoluMill Modals form.

==============
Action Buttons
==============
View
----
Takes down the form(s) and enters dynamic viewing.

Preview
-------
Previews the Pocket motion.  To save the generated motion and command do not
make any changes to the settings prior to pressing the OK button.  Pressing
the Preview or OK button after a change will overwrite the current motion
and command.  The generated command can be saved after the preview even if
there is an error.  To save the command, make no changes to the settings
and press the OK button.  The command will then be available for editing in
the command line.

Erase Motion
------------
Erases all motion.

Display Cutter
--------------
Displays the current cutter position.
