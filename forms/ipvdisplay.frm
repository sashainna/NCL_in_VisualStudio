#HEADER#
/TITLE/ NCLIPV Display Properties
/POSITION/ 50,50
/SIZE/250,240

#CHOICEBOX#
/LABEL/ Display Mode:
/POSITION/ 10,8
/SIZE/100,40
/TYPE/UD_DASSTRING
/CHOICES/ "Shaded","Wireframe","Hidden"

#CHECKBOX#
/LABEL/ Display Viewing Axes
/POSITION/ 120,8
/SIZE/80,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Buffer Copy:
/POSITION/ 10,25
/SIZE/ 110,40
/TYPE/UD_DASSTRING
/CHOICES/ "Swap Buffer","Pixel Copy"

#CHECKBOX#
/LABEL/ Start in Main Window
/POSITION/ 10,42
/SIZE/ 100,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Minimize Window on Startup
/POSITION/ 120,42
/SIZE/ 110,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Obstructed Solids
/POSITION/ 8,59
/SIZE/ 230,47

#CHECKBOX#
/LABEL/ Auto-Hide Solids
/POSITION/ 10,69
/SIZE/ 100,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Translucency:
/POSITION/ 120,69
/SIZE/180,14
/TYPE/UD_DASINT
/PREC/ 0
/LEN/ 3
/RANGE/ 1,100

#CHECKBOX#
/LABEL/ Display Edges
/POSITION/ 10,86
/SIZE/ 100,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Background
/POSITION/ 8,113
/SIZE/ 230,98
 
#CHOICEBOX#
/LABEL/ Shader:
/POSITION/ 10,123
/SIZE/ 85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Solid","Graduated","4-Corner","Image"

#LABEL#
/LABEL/ Color:
/POSITION/ 10,140
/SIZE/ 45,14

#LABEL#
/LABEL/ Top Color:
/POSITION/ 10,140
/SIZE/ 45,14

#LABEL#
/LABEL/ Upper Left:
/POSITION/ 10,140
/SIZE/ 45,14

#COLOR#      
/LABEL/ 
/POSITION/ 55,140
/SIZE/ 55,14
/TYPE/ UD_DASSTRING
/CHOICES/ RGB

#EDIT#
/LABEL/ RGB:
/POSITION/ 120,140
/SIZE/ 100,14
/TYPE/ UD_DASVEC
/LEN/ 20
/PREC/ 3

#LABEL#
/LABEL/ Bottom Color:
/POSITION/ 10,157
/SIZE/ 45,14

#LABEL#
/LABEL/ Upper Right:
/POSITION/ 10,157
/SIZE/ 45,14

#COLOR#      
/LABEL/
/POSITION/ 55,157
/SIZE/ 55,14
/TYPE/ UD_DASSTRING
/CHOICES/ RGB

#EDIT#
/LABEL/ RGB:
/POSITION/ 120,157
/SIZE/ 100,14
/TYPE/ UD_DASVEC
/LEN/ 20
/PREC/ 3

#COLOR#      
/LABEL/ Lower Left:
/POSITION/ 10,174, 55,174
/SIZE/ 100,14
/TYPE/ UD_DASSTRING
/CHOICES/ RGB

#EDIT#
/LABEL/ RGB:
/POSITION/ 120,174
/SIZE/ 100,14
/TYPE/ UD_DASVEC
/LEN/ 20
/PREC/ 3

#COLOR#      
/LABEL/ Lower Right:
/POSITION/ 10,191, 55,191
/SIZE/ 100,14
/TYPE/ UD_DASSTRING
/CHOICES/ RGB

#EDIT#
/LABEL/ RGB:
/POSITION/ 120,191
/SIZE/ 100,14
/TYPE/ UD_DASVEC
/LEN/ 20
/PREC/ 3

#PUSHBUTTON#
/LABEL/ Image:
/POSITION/ 12,140
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 65,140
/SIZE/ 105,14
/TYPE/ UD_DASSTRING
/PREC/ 40
/LEN/ 40

#CHOICEBOX#
/LABEL/ Rotate:
/POSITION/ 12,157, 50,157
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "0 Degrees","90 Degrees","180 Degrees","270 Degrees"

#CHECKBOX#
/LABEL/ Stretch to Fit
/POSITION/ 130,157
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#HELP#
=========================
NCLIPV Display Properties
=========================
This form controls various NCLIPV display properties.

Display Mode:
-------------
Visicut supports three different modes for displaying the solid model.  These
modes are Shaded, Wireframe, and Hidden line removal.

Display Viewing Axes
--------------------
Check this box if you would like an axes representation of the current view
orientation displayed in the bottom left corner of the NCLIPV window.

Buffer Copy
-----------
Determines the style to use when refreshing the graphics.  "Swap Buffer" swaps
the back buffer with the front buffer.  In order to use this mode, the graphics
in the back buffer must remain unchanged when swapping the buffers.  Most
graphics cards on Windows systems support this feature (it may be a setting
such as "Force Copy Swap").

"Pixel Copy" copies the affected area of the screen from the back to the front
buffer.  This method should work on all platforms, but may be slower than the
"Swap Buffer" method on some platforms.

Start in Main Window
--------------------
You can display the NCLIPV session in either a separate Pocket Window or in
the main NCL window.  If you choose to display it in the main window, then the
NCL graphics will be displayed in the Pocket Window.  Check this box to have
the NCLIPV session displayed in the main window automatically when starting a
session.  You can swap the NCLIPV and NCL graphics at any time by using the
Swap IPV and NCL Screens function.


Minimize Window on Startup
--------------------------
When the NCLIPV session is displayed in the main NCL window on startup, you
have the option of minimizing the Pocket Window, which contains the NCL
graphics, automatically.  This provides you with an unobstructed view of the
NCLIPV graphics in full window mode.

=================
Obstructed Solids
=================
The Obstructed Solids section defines how solids that obscure a stock or
fixture that has been marked as important are displayed.


Auto-Hide Solids:
-----------------
NCLIPV will dynamically set the translucency and edge display of any solid that 
obscures an important solid when the Auto-Hide solids box is checked.  The
solids will only be affected when they obscure the important solid and will
revert back to their normal translucency and edge display when the view is such
that it does not obscure the important solid anymore.

Translucency:
-------------
Defines the translucency value between 0-100 that a solid that obscures an
important solid will be displayed at.


Display Edges
-------------
Determines whether the edges of a solid that obscure an important solid will be
displayed.  Setting the Translucency to 0 and checking the Display Edges box
will display the obscuring solids as wireframe.

==========
Background
==========
The Background section defines the shader that will be used as the background 
color and effects.  Valid background shaders include a solid color, graduated,
four corner, and an image file.

Shader:
-------
Defines which background shader to use.  Solid uses a solid color, Graduated
varies smoothly from the top color to the bottom color, 4-Corner varies smoothly
from colors defined for each corner of the window, and Image uses a JPEG image
file as the background.  The actual fields displayed in this section vary with
the shader selected.

RGB:
----
The RGB fields in this section define the Red, Green, and Blue color components
for the corresponding color field when it is set to RGB instead of a fixed
color. These values can be between 0 and 1. To change these RGB values first
select the Color button and check RGB Color and then ckick OK.


Solid Shader
============

Color:
------
Defines the solid color to use as the background.

Graduated Shader
================

Top Color:
----------
Defines the color to use at the top of the screen for the background.  The
color of the background will vary smoothly between this color and a different
color specified for the bottom of the screen.

Bottom Color:
-------------
Defines the color to use at the bottom of the screen for the background.

4-Corner Shader
===============

Upper Left:
-----------
Defines the color to use at the upper left corner of the screen for the
background.  The colors of the background will vary smoothly between each of
the colors defined at the four corners.

Upper Right:
------------
Defines the color to use at the upper right corner of the screen for the
background.

Lower Left:
-----------
Defines the color to use at the lower left corner of the screen for the
background.

Lower Right:
------------
Defines the color to use at the lower right corner of the screen for the
background.

Image
=====

Image:
------
Pressing this button will display a file browser that allows you to select a
JPEG file to use as the background image.  The text field contains the name
of the selected file or you can simply type it in.

Rotate:
-------
The background image can be rotated 0, 90, 180, or 270 degrees.

Stretch to Fit
--------------
When this box is checked, the background image will be stretched to fit the
window size, otherwise it will be displayed at its stored aspect ratio.
