#HELP#
<Boundaries>
VoluMill Pocket
===============
This form is used to machine a pocket using the VoluMill high performance
pocketing motion.

Boundaries
----------
This section specifieFeed Rate:
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
(IPM,MMPM,IPR,MMPR).s the outside and inside boundaries of the pocket(s).

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
The Colors section defines the colors that will be used to highlight the geometry 
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
