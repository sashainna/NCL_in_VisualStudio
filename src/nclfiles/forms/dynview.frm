#HEADER#
/TITLE/ Dynamic View Modals
/POSITION/ 0,0
/SIZE/ 250,125

#CHOICEBOX#      
/LABEL/ Dynamic Display:
/POSITION/ 10,8,72,8
/SIZE/ 130,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Part", "Axis", "Both"

#CHOICEBOX#
/LABEL/ Rotation Center:
/POSITION/ 10,25,72,25
/SIZE/ 130,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Viewport Center","User Defined","Auto Center"

#CHOICEBOX#
/LABEL/ Z-Center:
/POSITION/ 150,25
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Pick","Calculate","Off"

#CHOICEBOX#
/LABEL/ Status Line:
/POSITION/ 10,42,72,42
/SIZE/ 130,40
/TYPE/ UD_DASSTRING
/CHOICES/ "OFF", "ON"

#EDIT#
/LABEL/ Display Segments:
/POSITION/ 10,59,72,59
/SIZE/ 130,14
/TYPE/ UD_DASINT
/RANGE/1,100000
/PREC/ 6
/LEN/ 6

#CHECKBOX#
/LABEL/ Enable Mouse Wheel
/POSITION/ 10,76
/SIZE/ 130,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Sensitivity
/POSITION/ 135, 76
/SIZE/60,14
/TYPE/ UD_DASSTRING

#HELP#      
========================
Dynamic View Modals Form
========================
The Dynamic View Modals Form allows you to modify settings that will be used
during dynamic viewing.  These settings will be saved in the 'ncl_view.mod'
file and will be used as the default values the next time this form is
displayed.

Dynamic Display:
----------------
During dynamic viewing you have the option to view the part, coordinate axis,
or both.  On older machines it could take a long time to orientate the part
during dynamic viewing, which is why the Axis option is available.  On today's
machines part orientation is quite fast so Part is typically selected in this
field.

Rotation Center:
----------------
The view can be orientated about the viewport center, user defined location, or
centered around the current mouse position.  The user defined location is
selected by pressing the KEY_FUNCTION key during dynamic viewing.  You will
then be prompted to select the dynamic center point.  This field will
automatically be updated to User Defined whenever the KEY_FUNCTION key is used
during dynamic viewing.  You will have to access this form to change it back to
Viewport Center if you want to reset it.

Selecting Auto Center will use the mouse cursor position at the time that a
mouse button is pressed as the dynamic viewing center location, allowing for
the view center to be easily manipulated during dynamic viewing.

Z-Center:
---------
'Pick' will attempt to pick an entity and use it for the center location.  
If an entity cannot be picked it will calculate the Z-level based on the 
geometry displayed in the window.

'Calculate' will bypass the picking of the geometry and go straight to the 
calculation of the Z-level based on the geometry displayed in the window.

'Off' will work as it did in previous versions, it will not calculate the 
Z-level center but use the reference center Z-level.

Status Line:
------------
The Status Line can be updated with the current rotation, viewport center, or
zoom facter whenever dynamic viewing other than Mouse Viewing is enabled, such
as XY-Rotate, Zoom, Pan, etc.  Since Mouse Viewing allows you to rotate, zoom,
and pan at the same time there is no reference to base the Status Line values
on.

Display Segments:
-----------------
Some models can be quite large and take a longer time to manipulate than what
is desired.  This field allows you to limit the maximum number of geometry
entities that can be displayed during dynamic viewing so that the display is
quicker.

Enable Mouse Wheel
------------------
Check this box if you would like to use the the mouse wheel to zoom the view
while in Dynamic Viewing.  Some users find it confusing that the zoom feature
can be accessed both by pressing the middle mouse button (wheel) and by moving
the mouse wheel.  In this case uncheck this box and moving the mouse wheel will
no no longer zoom the view.  You will need to press the mouse wheel to access
the zoom feature.

Sensitivity
-----------
Pressing this button will bring up the View Sensitivity form that allows you to
define the Mouse, Keyboard, and Spacemouse gain values, which determines how
much the view will change with each movement during dynamic viewing.
