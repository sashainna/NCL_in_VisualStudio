#HEADER#
/TITLE/ Motion Display Modals
/POSITION/ 0,0
/SIZE/ 280,310

#COLOR#      
/LABEL/ Color:
/POSITION/ 10,21,50,21
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Style:
/POSITION/ 115,21,140,21
/SIZE/ 90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Solid", "Dashed", "Dotted", "Center Line", "Phantom", "Large Dash"
/CHOICES/ "Dash Dot", "Dash Space"

#EDIT#
/LABEL/ Pen:
/POSITION/ 215,21
/SIZE/ 50,14
/TYPE/ UD_DASINT
/RANGE/1,256
/PREC/ 3
/LEN/ 3

#COLOR#      
/LABEL/ Rapid:
/POSITION/ 10,38,50,38
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Style:
/POSITION/ 115,38,140,38
/SIZE/ 90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Solid", "Dashed", "Dotted", "Center Line", "Phantom", "Large Dash"
/CHOICES/ "Dash Dot", "Dash Space"

#EDIT#
/LABEL/ Pen:
/POSITION/ 215,38
/SIZE/ 50,14
/TYPE/ UD_DASINT
/RANGE/1,256
/PREC/ 3
/LEN/ 3

#CHOICEBOX#
/LABEL/ Tracut:
/POSITION/ 10,55, 50,55
/SIZE/ 90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Ignore","Apply"

#CHOICEBOX#      
/LABEL/ Line Weight:
/POSITION/ 115,55,165,55
/SIZE/ 105,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Standard", "Medium", "Heavy", "Extra Heavy"

#FRAME#
/TITLE/ Motion
/POSITION/ 8,10
/SIZE/ 260,64

#COLOR#      
/LABEL/ Color:
/POSITION/ 10,87, 50,87
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Steps:
/POSITION/ 115,87, 140,87
/SIZE/ 90,14
/TYPE/ UD_DASINT
/PREC/ 0
/LEN/ -3

#EDIT#
/LABEL/ Pen:
/POSITION/ 215,87
/SIZE/ 50,14
/TYPE/ UD_DASINT
/RANGE/1,256
/PREC/ 3
/LEN/ 3

#CHOICEBOX#
/LABEL/ Shaded:
/POSITION/ 10,104, 50,104
/SIZE/ 90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#EDIT#
/LABEL/ Translucency:
/POSITION/ 115,104, 165,104
/SIZE/ 120,14
/TYPE/ UD_DASINT
/RANGE/1,100
/PREC/ 0
/LEN/ -3

#FRAME#
/TITLE/ Cutter
/POSITION/ 8,74
/SIZE/ 260,50

#COLOR#      
/LABEL/ Color:
/POSITION/ 10,136, 50,136
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Pen:
/POSITION/ 215,136
/SIZE/ 50,14
/TYPE/ UD_DASINT
/RANGE/1,256
/PREC/ 3
/LEN/ 3

#CHOICEBOX#
/LABEL/ Shaded:
/POSITION/ 10,153, 50,153
/SIZE/ 90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#EDIT#
/LABEL/ Translucency:
/POSITION/ 115,153, 165,153
/SIZE/ 120,14
/TYPE/ UD_DASINT
/RANGE/1,100
/PREC/ 0
/LEN/ -3

#FRAME#
/TITLE/ Shank
/POSITION/ 8,125
/SIZE/ 260,50

#COLOR#      
/LABEL/ Color:
/POSITION/ 10,187, 50,187
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Pen:
/POSITION/ 215,187
/SIZE/ 50,14
/TYPE/ UD_DASINT
/RANGE/1,256
/PREC/ 3
/LEN/ 3

#CHOICEBOX#
/LABEL/ Shaded:
/POSITION/ 10,204, 50,204
/SIZE/ 90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#EDIT#
/LABEL/ Translucency:
/POSITION/ 115,204, 165,204
/SIZE/ 120,14
/TYPE/ UD_DASINT
/RANGE/1,100
/PREC/ 0
/LEN/ -3

#FRAME#
/TITLE/ Holder
/POSITION/ 8,176
/SIZE/ 260,50

#EDIT#
/LABEL/ Size:
/POSITION/ 10,244, 50,244
/SIZE/ 90,14
/TYPE/ UD_DASINT
/PREC/ 0
/LEN/ -4

#CHECKBOX#
/LABEL/ Iterative Cutter Display
/POSITION/ 115,244
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Motion Stack
/POSITION/ 8,230
/SIZE/ 260,33

#CHECKBOX#
/LABEL/ Output NCL Command(s)
/POSITION/ 92,268
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#HELP#
==========================
Motion Display Modals Form
==========================
This form controls various motion display settings.

Motion Color:
-------------
Specifies the color for displayed motion that is programmed with a feedrate
rather than in rapid mode.

Motion Line Style:
------------------
Defines the line style to use for displayed motion that is programmed with a
feedrate rather than in rapid mode.

Motion Pen:
-----------
Specifies the pen number to be associated with the programmed feedrate motion
when projected onto a drawing.  Enter a value between 1 and 256.

Rapid Motion Color:
-------------------
Specifies the color for displayed motion that is programmed in RAPID mode.

Rapid Motion Line Style:
------------------------
Defines the line style to use for displayed motion that is programmed in RAPID
mode.

Rapid Pen:
----------
Specifies the pen number to be associated with the programmed RAPID motion
when projected onto a drawing.  Enter a value between 1 and 256.

Tracut:
-------
Determines if the displayed motion should honor the active TRACUT matrix.  This
setting is normally set to "Ignore", since the TRACUT matrix usually moves the
motion away from the part display.

Line Weight:
------------
Defines the line weight to use for displayed motion. The available choices are
"Standard", "Medium", "Heavy" and "Extra Heavy".

Cutter Color:
-------------
Specifies the color of the displayed cutter.

Cutter Steps:
-------------
Enter the number of motion steps to display a cutter when static cutter
display is in effect (non-moving cutter).  The cutter will always be displayed
at the end of a motion regardless of this value.

Cutter Pen:
-----------
Specifies the pen number to be associated with the cutter shapes when projected
onto a drawing.  Enter a value between 1 and 256.

Cutter Shaded:
--------------
Specifies whether the cutter is displayed as shaded.

Cutter Translucency:
--------------------
Specifies the translucency of a shaded cutter. Enter a value between 0 and 100.

Shank Color:
------------
Specifies the color of the shank portion of the displayed cutter.  The cutter
shank is defined as the non-cutting portion of the cutter.

Shank Pen:
----------
Specifies the pen number to be associated with the cutter shank when projected
onto a drawing.  Enter a value between 1 and 256.

Shank Shaded:
-------------
Specifies whether the cutter shank is displayed as shaded.

Shank Translucency:
-------------------
Specifies the translucency of a shaded shank. Enter a value between 0 and 100.

Holder Color:
-------------
Specifies the color of the holder portion of the displayed cutter.

Holder Pen:
-----------
Specifies the pen number to be associated with the cutter holder when projected
onto a drawing.  Enter a value between 1 and 256.

Holder Shaded:
--------------
Specifies whether the cutter holder is displayed as shaded.

Holder Translucency:
--------------------
Specifies the translucency of a shaded holder. Enter a value between 0 and 100.

Motion Stack Size:
------------------
The Motion Display Stack keeps track of the previous 'n' motions.  It is used
with the Playback Current Motion form to allow for a quick playback of the
last programmed motions.  A value of zero will disable this stack.  Specifying
a large value can increase memory storage dramatically.

WARNING - Changing the size of the Motion Display Stack will reinitialize it,
deleting any motions that are currently on the stack.

Iterative Cutter Display
------------------------
Check this box if you would like the cutter visualized at each step of the
programmed motion.  If this box is not checked, then the cutter will only be
displayed at the end of a programmed motion, which will speed up the motion
display.  This field will only affect moving cutters, not static cutters.  If
the Motion Display Stack is active and Iterative Cutter Display is turned off,
then the cutter can be visualized at each step using the Playback Current
Motion form.

Output NCL Command(s)
---------------------
Check this box if you want the associated DRAFT and CUTTER/DISPLY commands
output to the part program when a setting changes.  If this box is not checked,
then only the settings will be changed without any commands being created.
