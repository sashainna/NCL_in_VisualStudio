#HEADER#
/TITLE/ Dynamic View Modals
/POSITION/ 0,0
/SIZE/ 250,68

#CHOICEBOX#
/LABEL/ Rotation Center:
/POSITION/ 10,8,72,8
/SIZE/ 130,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Viewport Center","User Defined","Auto Center"

#CHOICEBOX#
/LABEL/ Z-Center:
/POSITION/ 150,8
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Pick","Calculate","Off"

#CHECKBOX#
/LABEL/ Enable Mouse Wheel
/POSITION/ 10,25
/SIZE/ 130,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Sensitivity
/POSITION/ 135,25
/SIZE/60,14
/TYPE/ UD_DASSTRING

#HELP#      
========================
Dynamic View Modals Form
========================
The Dynamic View Modals Form allows you to modify settings that will be used
during dynamic viewing.  These settings will be saved in the 'nclipv_view.mod'
file and will be used as the default values the next time this form is
displayed.

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
