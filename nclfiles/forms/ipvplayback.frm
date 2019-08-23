#HEADER#
/TITLE/ NCLIPV Playback Control
/POSITION/ 50,50
/SIZE/220,137

#CHOICEBOX#
/LABEL/ Start:
/POSITION/ 10,8,50,8
/SIZE/90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Beginning", "Current"

#CHOICEBOX#      
/LABEL/ Run Mode:
/POSITION/ 10,25,50,25
/SIZE/90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Go", "Step", "Next Tool"

#EDIT#
/LABEL/ Speed:
/POSITION/ 110,8,140,8
/SIZE/70,14
/TYPE/ UD_DASINT
/RANGE/1,9999
/LEN/ 4
/PREC/ 0

#EDIT#
/LABEL/ Steps:
/POSITION/ 110,25,140,25
/SIZE/70,14
/TYPE/ UD_DASINT
/RANGE/1,9999
/LEN/ 4
/PREC/ 0

#CHOICEBOX#
/LABEL/ Dynamic:
/POSITION/ 10,42,50,42
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Cutter","Part"

#CHOICEBOX#
/LABEL/ Cycles:
/POSITION/ 110,42,140,42
/SIZE/70,14
/TYPE/UD_DASSTRING
/CHOICES/ "Off","Simple","Detail","Lathe"

#CHOICEBOX#
/LABEL/ Rapid:
/POSITION/ 10,59,50,59
/SIZE/90,14
/TYPE/UD_DASSTRING
/CHOICES/ "Off","X-axis","Y-axis","Z-axis","Tlaxis"

#CHOICEBOX#
/LABEL/ Analyzation:
/POSITION/ 110,59
/SIZE/100,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","Feed Rate","Interpolation"

#CHOICEBOX#
/LABEL/ Show Source:
/POSITION/ 10,76,60,76
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "No","Yes"

#PUSHBUTTON#
/LABEL/ Reset Session
/POSITION/ 80,95
/SIZE/70,14
/TYPE/UD_DASSTRING

#HELP#
============================
NCLIPV Playback Control Form
============================
The NCLIPV Playback Control form controls the NCLIPV motion playback for both
simple material removal and machine simulations.  These settings are saved in
the 'nclipv_playback.mod' file and will be used as the default values the next
time this form is displayed.

Start:
------
The Start field allows you to start the simulation from either the beginning of
the motion playback range as defined in the Playback File form (Beginning) or
from the location where the simulation was last stopped (Current).

Run Mode:
---------
This field determines how to proceed after simulating a single clfile location.
It can have the following settings.

   Go         = Process the source file continuously until the end is reached.
   Step       = Process 'n' clfile locations at a time.  'n' is defined in the
                Steps: field.
   Next Tool  = Process the source file until the next tool is loaded.


Speed:
------
The Speed: field allows the simulation to be slowed down.  Entering a value of
100 causes the simulation to be performed at full speed.  You can enter a
lower value to slow it down if it becomes too fast.


Steps:
------
The Steps: field determines the number of tool movements to simulate prior to
updating the NCLIPV window in Go mode or prior to prompting to continue the
simulation in Step mode.  A value of 1 will update/prompt after each tool
movement.  Increasing this value while in Go mode will cause the simulation to
process faster.

Dynamic:
--------
The Dynamic: field determines if the tool or part will move during simulation.
Setting this field to 'Cutter' causes the part to be stationary while the tool
moves arount the part.  'Part' will display the tool in a stationary position
while the part appears to move about the tool.  Either of these display
methods will produce exactly the same results.

Cycles:
-------
The Cycles: field determines how to emulate cycle blocks programmed in the
source file.  Cycles can be emulated in the following manners.

   Off     = No cycle simulation is done.
   Simple  = Perform simple single plunge drilling cycles without any pecking.
   Detail  = Fully emulate the drilling cycles, including pecking and shift
             type cycles.
   Lathe   = Emulate Lathe style cycles.  This is the default method when the
             machine type is set to a Lathe.

This field is not active when a Simulation file is used as input, since the
Simulation file already contains the cycle emulations.

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
The simulated cuts can be displayed using the default colors setup for NCLIPV
or the colors can be set depending on the machining mode or programmed feed
rate.  Off will use the standard colors.

Feed Rate will set the simulation cutters based on the programmed feed rate.
The colors used to represent the various feed rate ranges are defined in the
Feed Rate Analyzation form.

Interpolation will set the simulation colors based on the current machining
mode.  Recognized machining modes are Linear, Circular, Rapid, Cycle, and
Multi-axis.  The machine mode interpolation colors are defined in the
Interpolation Analyzation form.

Show Source:
------------
The clfile records can be displayed in the scrolling window during motion
playback by entering Yes in this field.

Reset Session
-------------
Pressing the Reset Session button will reset the NCLIPV session so that it
is fresh when starting the simulation process.
