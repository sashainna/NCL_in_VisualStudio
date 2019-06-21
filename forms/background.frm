#HEADER#
/TITLE/ Background Display
/POSITION/ 50,50
/SIZE/245,137

#CHOICEBOX#
/LABEL/ Shader:
/POSITION/ 10,8
/SIZE/ 85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Solid","Graduated","4-Corner"

#LABEL#
/LABEL/ Color:
/POSITION/ 10,25
/SIZE/ 45,14

#LABEL#
/LABEL/ Top Color:
/POSITION/ 10,25
/SIZE/ 45,14

#LABEL#
/LABEL/ Upper Left:
/POSITION/ 10,25
/SIZE/ 45,14

#COLOR#
/LABEL/ 
/POSITION/ 55,25
/SIZE/ 55,14
/TYPE/ UD_DASSTRING
/CHOICES/ RGB

#EDIT#
/LABEL/ RGB:
/POSITION/ 120,25
/SIZE/ 100,14
/TYPE/ UD_DASVEC
/LEN/ 20
/PREC/ 3

#LABEL#
/LABEL/ Bottom Color:
/POSITION/ 10,42
/SIZE/ 45,14

#LABEL#
/LABEL/ Upper Right:
/POSITION/ 10,42
/SIZE/ 45,14

#COLOR#
/LABEL/
/POSITION/ 55,42
/SIZE/ 55,14
/TYPE/ UD_DASSTRING
/CHOICES/ RGB

#EDIT#
/LABEL/ RGB:
/POSITION/ 120,42
/SIZE/ 100,14
/TYPE/ UD_DASVEC
/LEN/ 20
/PREC/ 3

#COLOR#
/LABEL/ Lower Left:
/POSITION/ 10,59, 55,59
/SIZE/ 100,14
/TYPE/ UD_DASSTRING
/CHOICES/ RGB

#EDIT#
/LABEL/ RGB:
/POSITION/ 120,59
/SIZE/ 100,14
/TYPE/ UD_DASVEC
/LEN/ 20
/PREC/ 3

#COLOR#
/LABEL/ Lower Right:
/POSITION/ 10,76, 55,76
/SIZE/ 100,14
/TYPE/ UD_DASSTRING
/CHOICES/ RGB

#EDIT#
/LABEL/ RGB:
/POSITION/ 120,76
/SIZE/ 100,14
/TYPE/ UD_DASVEC
/LEN/ 20
/PREC/ 3

#PUSHBUTTON#
/LABEL/ Image:
/POSITION/ 12,25
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 65,25
/SIZE/ 105,14
/TYPE/ UD_DASSTRING
/PREC/ 40
/LEN/ 40

#CHOICEBOX#
/LABEL/ Rotate:
/POSITION/ 12,42, 50,42
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "0 Degrees","90 Degrees","180 Degrees","270 Degrees"

#CHECKBOX#
/LABEL/ Stretch to Fit
/POSITION/ 130,42
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 90,93
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#HELP#
==================
Background Display
==================
This form defines the shader that will be used as the background color and
effects.  Valid background shaders include a solid color, graduated, and
four corner.

Shader:
-------
Defines which background shader to use.  Solid uses a solid color, Graduated
varies smoothly from the top color to the bottom color, and 4-Corner varies
smoothly from colors defined for each corner of the window.

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

Preview
-------
Pressing the Preview button will change the background without saving the
settings so that you can get an idea of what the new background will look like.
