#HEADER#
/TITLE/ Display Buffer Modals
/POSITION/ 50,50
/SIZE/130,90

#CHOICEBOX#
/LABEL/ Buffer Copy:
/POSITION/ 10,15, 60,15
/SIZE/110,40
/TYPE/UD_DASSTRING
/CHOICES/ "Swap Buffer","Pixel Copy"

#CHOICEBOX#
/LABEL/ Erase Entity:
/POSITION/ 10,32, 60,32
/SIZE/110,40
/TYPE/UD_DASSTRING
/CHOICES/ "Redraw","Erase"

#CHOICEBOX#
/LABEL/ Cutter Display:
/POSITION/ 10,49, 60,49
/SIZE/110,40
/TYPE/UD_DASSTRING
/CHOICES/ "Fast","Smooth"

#HELP#
This form controls how the screen is refreshed after the graphics have been
updated.  Since different implementations of OpenGL can vary on the speed
graphics is displayed (normally dependant on the graphics card installed),
this setting allows the user to optimize the graphics display for their system.

Buffer Copy
-----------
Determines the style to use when refreshing the graphics.  "Swap Buffer" swaps
the back buffer with the front buffer.  In order to use this mode, the graphics
in the back buffer must remain unchanged when swapping the buffers.  Most
graphics cards on Windows systems support this feature (it may be a setting
such as "Force Copy Swap"), but most Unix systems do not support this feature
and must use the "Pixel Copy" method.

"Pixel Copy" copies the affected area of the screen from the back to the front
buffer.  This method should work on all platforms, but may be slower than the
"Swap Buffer" method on some platforms.

Erase Entity
------------
Determines how a geometric entity is to be erased.  Either all entities that
are in the same screen area of the erased entity can be redrawn (Redraw) or
the entity being erased can be drawn in black (Erase).

Using the "Redraw" option provides for a much cleaner display, but will usually
take longer to complete.  Using the "Erase" option will leave black areas on
the screen where the entity was erased, but can be much faster than the "Redraw"
option depending on how many entities are defined.

Cutter Display
--------------
This field controls the display of the wireframe cutter.  It can be displayed
using a "Fast" method, which will display the cutter much faster on most
systems, but could cause a flashing effect of the cutter.  "Smooth" will
display the wireframe cutter in a manner that generates a less jumpy display,
but will take much longer (2 to 5 times) to animate on most systems.

A solid cutter will always be displayed using the "Smooth" method, since it
does not display well using the "Fast" method.
