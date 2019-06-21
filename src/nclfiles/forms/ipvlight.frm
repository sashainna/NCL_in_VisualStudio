#HEADER#
/TITLE/ NCLIPV Light Sources
/POSITION/ 0,0
/SIZE/ 290,110

#CHOICEBOX#
/LABEL/ Light:
/POSITION/ 10,8
/SIZE/ 65,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Light #1", "Light #2", "Light #3", "Light #4"

#CHOICEBOX#
/LABEL/ Active:
/POSITION/ 90,8
/SIZE/ 60,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHOICEBOX#
/LABEL/ Type:
/POSITION/ 175,8
/SIZE/ 75,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Ambient", "Distant", "Eye", "Point"

#COLOR#      
/LABEL/ Color:
/POSITION/ 10,25
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/CHOICES/ RGB

#EDIT#
/LABEL/ RGB:
/POSITION/ 90,25
/SIZE/ 100,14
/TYPE/ UD_DASVEC
/LEN/ 20
/PREC/ 3

#EDIT#
/LABEL/ Intensity:
/POSITION/ 205,25
/SIZE/ 45,14
/TYPE/ UD_DASINT
/RANGE/ 0,100
/PREC/ 3
/LEN/ 4

#PUSHBUTTON#
/LABEL/ Position:
/POSITION/ 10,42
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 60,42
/SIZE/ 50,14
/TYPE/ UD_DASCART
/PREC/ 3
/LEN/ 20

#EDIT#
/LABEL/ Direction:
/POSITION/ 155,42
/SIZE/ 95,14
/TYPE/ UD_DASVEC
/PREC/ 3
/LEN/ 20

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 110,65
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#HELP#
=========================
NCLIPV Light Sources Form
=========================
This form is used to define the active lights and their properties for NCLIPV.
These lights determine how the NCLIPV solids are shaded.  Accepting this form
will save the light settings in the modals file 'nclipv_lights.mod', so that
the next time NCLIPV is started, these settings will be used as the defaults.

Light
-----
This toggle field determines which light is currently being defined.  Up to 4
lights can be defined and active at any given time.

Active
------
Determines whether this light is On or Off.

Type
----
Defines the type of light being defined.

Ambient lights illuminate all surfaces in the scene evenly with a constant
level of light, irrespective of each surface's position or orientation.  If an
ambient light is used, it is recommended that the intensity be set low to
minimize the effects on shadowing.

Distant lights are defined at a set position and give off parallel rays of
light in a given direction across the entire screen.  An example of a Distant
light is the sun.

Eye lights are always positioned in the center of the screen and will illuminate
everything from the same point as the viewing position.

Point lights can be positioned by the user, they display light in all
directions, similar to a light bulb.

Color
-----
Defines the color of the light.  Using a color other than White will cause the
surfaces to "glow" with this color.

RGB
---
Defines the Red, Green, and Blue color components of the light when Color is
set to RGB instead of a fixed color.  These values can be between 0 and 1.
To change these RGB values first select the Color button and check RGB Color
and then ckick OK.

Intensity
---------
Defines the intensity or brightness of the light.  It can be in the range of
0 to 100 percent.

Position
--------
Selects the position of the light for Distant and Point lights.  This position
can be selected in the NCLIPV window by pressing the Position button.

Direction
---------
Defines the vector direction of Distant lights.

Apply
-----
Applies the form light settings to the NCLIPV window, so that the results can
be viewed without exiting the form.
