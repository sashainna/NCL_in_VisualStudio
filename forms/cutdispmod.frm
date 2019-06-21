#HEADER#
/TITLE/ Cutter Display Modals
/POSITION/ 50,50
/SIZE/140,180

#FRAME#
/TITLE/ Shaded
/POSITION/ 10,12
/SIZE/ 120,75

#CHOICEBOX#
/LABEL/ Style:
/POSITION/ 20,25,45,25
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Redraw","Pixel Copy"

#CHOICEBOX#
/LABEL/ Buffer:
/POSITION/ 20,40,45,40
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Front","Back","Both"

#CHOICEBOX#
/LABEL/ Copy:
/POSITION/ 20,55,45,55
/SIZE/90,50
/TYPE/UD_DASSTRING
/CHOICES/ "Swap Buffer","Small","Large","Full"

#CHOICEBOX#
/LABEL/ Depth
/POSITION/ 20,70,45,70
/SIZE/90,60
/TYPE/UD_DASSTRING
/CHOICES/ "Disable","Enable"

#FRAME#
/TITLE/ Wireframe
/POSITION/ 10,90
/SIZE/ 120,60

#CHOICEBOX#
/LABEL/ Style:
/POSITION/ 20,100,45,100
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Redraw","Pixel Copy"

#CHOICEBOX#
/LABEL/ Buffer:
/POSITION/ 20,115,45,115
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Front","Back","Both"

#CHOICEBOX#
/LABEL/ Copy:
/POSITION/ 20,130,45,130
/SIZE/90,50
/TYPE/UD_DASSTRING
/CHOICES/ "Swap Buffer","Small","Large","Full"

#HELP#
==========================
Cutter Display Modals Form
==========================
This form controls how the cutter is animated during a motion playback sequence.
These settings are valid for OpenGL supported graphics only.  Since different
implementations of OpenGL can vary on the speed graphics is displayed (normally
dependant on the graphics card installed), these settings allow the user to
optimize the settings for their system.

The NclAnimate program can be used to automatically run through the various
combination of settings and make a selection based on the timing and visual
results.  There are two different sections in this form, one for a shaded
cutter and one for the display of a wireframe cutter.

Style
-----
Determines the style for recreating the graphics behind a displayed cutter
after it is moved or erased.  "Redraw" will redraw the graphics behind the
cutter, while "Pixel Copy" makes a copy of the area behind the cutter prior
to drawing it and then restores this area when erasing the cutter.

Buffer
------
This field determines which buffer to draw the cutter into.  When "Front" is
selected, the cutter will be drawn into the front buffer.  This usually causes
a flashing display of the cutter, but may be useful for speed on slower
graphics cards.  "Back" draws the cutter into the back buffer, which is then
copied to the front buffer using the Copy field setting. "Both" will draw the
cutter in the front buffer and then simply copy the back buffer to the front
in order to erase it.  This method can cause flashing on some graphics cards.

Copy
----
This field determines the method for copying the back buffer to the front
buffer during animation.  "Swap Buffer" swaps the back buffer with the front
buffer (on Windows NT it simply copies the contents of the back buffer to the
front buffer leaving the back buffer undisturbed.  On Unix machines the buffers
are actually swapped, so the entire contents of the back buffer has to be
redrawn after performing this operation, which can cause slowness).  "Small"
copies the area vacated by the cutter.  "Large" copies the area vacacted by
the cutter and also the area where the cutter is currently positioned at.  This
creates a smoother animation and is only valid when the Buffer field is set to
"Back".  "Full" copies the entire back buffer to the front buffer.

Depth
-----
Enabling the depth buffer for the cutter animation creates a visually correct
representation of the shaded cutter.  Disabling the depth buffer will create
a less accurate visualization of the cutter, but can be used to speed up the
Pixel Copy style of erasing the cutter.  The depth buffer is never used with
wireframe cutters.
