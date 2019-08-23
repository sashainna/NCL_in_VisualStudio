#HEADER#
/TITLE/ Playback Control
/POSITION/ 50,50
/SIZE/ 225,245

#FRAME#
/TITLE/ Playback
/POSITION/ 8,8
/SIZE/ 210,66

#CHOICEBOX#
/LABEL/ Start:
/POSITION/ 10,21,50,21
/SIZE/90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Beginning", "Current"

#EDIT#
/LABEL/ Speed:
/POSITION/ 110,21,145,21
/SIZE/70,14
/TYPE/ UD_DASINT
/RANGE/1,9999
/LEN/ 4
/PREC/ 0

#CHOICEBOX#      
/LABEL/ Run Mode:
/POSITION/ 10,38,50,38
/SIZE/90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Go", "Step"

#EDIT#
/LABEL/ Steps:
/POSITION/ 110,38,145,38
/SIZE/70,14
/TYPE/ UD_DASINT
/RANGE/1,9999
/LEN/ 4
/PREC/ 0

#CHOICEBOX#
/LABEL/ Erase First:
/POSITION/ 10,55,50,55
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "No","Yes"

#CHOICEBOX#
/LABEL/ Dynamic:
/POSITION/ 110,55,145,55
/SIZE/80,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","Cutter","Part"

#FRAME#
/TITLE/ Shade Properties
/POSITION/ 8,78
/SIZE/ 210,83

#CHOICEBOX#
/LABEL/ Shade:
/POSITION/ 10,90,50,90
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","On","Auto"

#EDIT#
/LABEL/ Translucency:
/POSITION/ 110,90,160,90
/SIZE/ 100,14
/TYPE/ UD_DASINT
/RANGE/1,100
/PREC/ 4
/LEN/ 4

#CHOICEBOX#
/LABEL/ Cutter:
/POSITION/ 10,107,50,107
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","On"

#EDIT#
/LABEL/
/POSITION/ 110,107,160,107
/SIZE/ 100,14
/TYPE/ UD_DASINT
/RANGE/1,100
/PREC/ 4
/LEN/ 4

#CHOICEBOX#
/LABEL/ Shank:
/POSITION/ 10,124,50,124
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","On"

#EDIT#
/LABEL/
/POSITION/ 110,124,160,124
/SIZE/ 100,14
/TYPE/ UD_DASINT
/RANGE/1,100
/PREC/ 4
/LEN/ 4

#CHOICEBOX#
/LABEL/ Holder:
/POSITION/ 10,141,50,141
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","On"

#EDIT#
/LABEL/
/POSITION/ 110,141,160,141
/SIZE/ 100,14
/TYPE/ UD_DASINT
/RANGE/1,100
/PREC/ 4
/LEN/ 4

#FRAME#
/TITLE/ Simulation
/POSITION/ 8,167
/SIZE/ 210,49

#CHOICEBOX#
/LABEL/ Cycle Display:
/POSITION/ 10,180,60,180
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","Simple","Detail","Lathe"

#CHOICEBOX#
/LABEL/ Show Source:
/POSITION/ 110,180,160,180
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "No","Yes"

#CHOICEBOX#
/LABEL/ Rapid:
/POSITION/ 10,197,60,197
/SIZE/ 90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","X-axis","Y-axis","Z-axis","Tlaxis"

#CHOICEBOX#
/LABEL/ Analyzation:
/POSITION/ 110,197
/SIZE/105,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","Feed Rate","Interpolation"

#HELP#
=====================
Playback Control Form
=====================
This form controls the playback of the clfile motion.

Start:
------
You can start motion playback from either the Beginning of the clfile or from
the clfile position where motion playback last ended (Current).

Speed:
------
Specify the speed that you would like the motion playback to proceed at, in the
range of 1% to 100%.  Specifying a speed of less than 100% will cause the tool
to iterate at incremental moves along a long motion in order to better represent
a continuous path, rather than jumping from position to position.

Run Mode:
---------
Motion playback can be continuous (Go) or by one move at a time (Step).  When
in Step mode, pressing the Return key will step through the motion.

Steps:
------
Enter the number of moves to output during Step mode each time the Return key
is pressed.  This field is disabled when the Run Mode is set to Go.

Erase First:
------------
When this field is set to Yes the displayed motion will be erased prior to
starting motion playback.  A setting of No will not erase the motion first.
No is typically used when motion playback is started from the Current clfile
position.

Dynamic:
--------
The cutter or part can be animated during motion playback.  Off will display a
static cutter at the end of each clfile motion record, Cutter will display a
cutter that moves with the programmed motion, and Part will display a moving
part while the cutter remains stationary at the center of the screen.  Part
is useful when viewing motion on a long narrow part.

Shade:
------
The cutter can be displayed as either a wireframe or shaded entity.  Auto will
use the settings from the DRAFT/CUTTER and CUTTER/DISPLY commands to display
the cutter.  Off will display all parts of the cutter (cutter, shank, holder)
in wireframe.  On will display all parts of the cutter as shaded.

Translucency:
-------------
When the cutter is shaded it can be displayed with a level of translucency
that gives it a transparent look.  Enter a value between 1 and 100, where 100
displays a solid cutter and 1 barely displays a visual cutter.  This field will
affect all parts of the cutter (cutter, shank, holder) and will be applied to
the appropriate form fields.  It is only active when the Shade field is set to
On.

Cutter Shade:
-------------
This field affects the shaded property of the cutter and is only active when
the Shade field is not set to Auto.

Cutter Translucency:
--------------------
This field affects the translucency property of the cutter and is only active
when the Shade field is not set to Auto and the Cutter Shade field is set to
On.

Shank Shade:
------------
This field affects the shaded property of the cutter shank and is only active
when the Shade field is not set to Auto.

Shank Translucency:
-------------------
This field affects the translucency property of the cutter shank and is only
active when the Shade field is not set to Auto and the Shank Shade field is set
to On.

Holder Shade:
-------------
This field affects the shaded property of the Holder and is only active when
the Shade field is not set to Auto.

Holder Translucency:
--------------------
This field affects the translucency property of the holder and is only active
when the Shade field is not set to Auto and the Holder Shade field is set to
On.

Cycle Display:
--------------
Canned cycles in the clfile file can be simulated in different ways.  Off will
display only the cycle positions contained in the clfile as if they were simple
goto points (usually at the top of the part), Simple will simulate mill cycles
with a simple motion to the bottom of the hole and back up again, Detail will
simulate mill cycles in their entirety, including the pecking motion, and
Lathe will simulate lathe cycles.

Show Source:
------------
The clfile records can be displayed in the scrolling window during motion
playback by entering Yes in this field.

Rapid:
------
Rapid motion in the clfile can be simulated in different ways.  Off will display
rapid moves in a straight line as they are programmed.  Rapid motion will be
altered as follows when any other selection is made.

   If the selected axis move is in the positive direction, then the tool will
   move in this axis first and then move in the other 2 axes.

   If the selected axis move is in the negative direction, then the tool will
   move in the other 2 axes first and then in the selected axis.

When Tlaxis is selected the major axis will be determined based on the tool
axis vector of each point.  For example, if the tool axis is 1,0,0, then the
major axis for this move is the X-axis.

Analyzation:
------------
The motion playback can be displayed using the standard colors as defined in
the Motion Display form or the colors can be set depending on the machining
mode or programmed feed rate.  Off will use the standard colors.

Feed Rate will set the motion display colors based on the programmed feed rate.
The colors used to represent various feed rate ranges are defined in the
Feedrate Analyzation form.

Interpolation will set the motion display colors based on the current machining
mode.  Recognized machining modes are Linear, Circular, Rapid, Cycle, and
Multi-axis.  The machining mode interpolation colors are defined in the
Interpolation Analyzation form.

