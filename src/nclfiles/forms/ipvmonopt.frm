#HEADER#
/TITLE/ NCLIPV Monitor Options
/POSITION/ 50,50
/SIZE/230,300

#CHECKBOX#
/LABEL/ ISN
/POSITION/ 10,18
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Machining Mode
/POSITION/ 120,18
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Machine Type
/POSITION/ 10,35
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Tool End Point
/POSITION/ 10,52
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Tool Axis Vector
/POSITION/ 120,52
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Linear Axes
/POSITION/ 10,69
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Rotary Axes
/POSITION/ 120,69
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Head 2 Axes
/POSITION/ 10,86
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Head 3 Axes
/POSITION/ 120,86
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Head 4 Axes
/POSITION/ 10,103
/SIZE/90,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Block
/POSITION/ 8,8
/SIZE/ 210,115

#CHECKBOX#
/LABEL/ Tool Number
/POSITION/ 10,137
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Diameter
/POSITION/ 120,137
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Corner Radius
/POSITION/ 10,154
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Height
/POSITION/ 120,154
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Cutter Comp
/POSITION/ 10,171
/SIZE/90,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Tool
/POSITION/ 8,129
/SIZE/ 210,59

#CHECKBOX#
/LABEL/ Feed Rate
/POSITION/ 10,201
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Move Time
/POSITION/ 120,201
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Spindle
/POSITION/ 10,218
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Coolant
/POSITION/ 120,218
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Machine Time
/POSITION/ 10,235
/SIZE/90,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Attributes
/POSITION/ 8,194
/SIZE/ 210,61

#CHECKBOX#
/LABEL/ Progress Bar
/POSITION/ 10,258
/SIZE/90,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Docked:
/POSITION/ 120,258
/SIZE/ 90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","Left","Right"

#HELP#
===========================
NCLIPV Monitor Options Form
===========================
The NCLIPV Monitor Options form controls the fields that are displayed in the
NCLIPV Monitor Panel during simulation and how the panel is positioned.  These
settings are saved in the 'nclipv_modals.mod' file along with the NCLIPV modals
settings and will be used as the default values the next time the form is
displayed.

ISN
---
Displays the Input Sequence Number field in the NCLIPV Monitor Panel when
enabled.

Machining Mode
--------------
Displays the machining mode for the current move in the NCLIPV Monitor Panel
when enabled.  Valid machining modes that can be displayed are Linear, Rapid,
Circular, Cycle, and Multi-axis.

Machine Type
------------
Displays the type of machine used for simulation.  Valid machine types are Mill,
Lathe, and Mill/Turn.  The machine type is set in the NCLIPV Modals form and
if the machine type does not match what the input cl/simulation file is
programmed for, then the simulation process will not be correct.


Tool End Point
--------------
Displays the XYZ location of the tool end point in the NCLIPV Monitor Panel
when enabled.

Tool Axis Vector
----------------
Displays the IJK components of the tool axis vector in the NCLIPV Monitor Panel
when enabled.

Linear Axes
-----------
Displays the linear axis positions of the machine in the NCLIPV Monitor Panel
when enabled.  This field can only be enabled when a Simulation file is used
as input to NCLIPV.

Rotary Axes
-----------
Displays the rotary axis positions of the machine in the NCLIPV Monitor Panel
when enabled.  This field can only be enabled when a Simulation file is used
as input to NCLIPV.

Tool Number
-----------
Displays the active tool number and length as defined by the LOADTL statement
in the NCLIPV Monitor Panel when enabled.

Diameter
--------
Displays the active cutter diameter in the NCLIPV Monitor Panel when enabled.

Corner Radius
-------------
Displays the active cutter corner radius in the NCLIPV Monitor Panel when
enabled.

Height
------
Displays the active cutter height in the NCLIPV Monitor Panel when enabled.

Cutter Comp
-----------
Displays the active cutter compensation direction in the NCLIPV Monitor Panel
when enabled.  Valid directions are Off, Left, and Right.

Feed Rate
---------
Displays the active feed rate and mode in the NCLIPV Monitor Panel when enabled.

Move Time
---------
Displays the machining time that the current move will take in the NCLIPV
Monitor Panel when enabled.

Spindle
-------
Displays the active spindle speed and direction in the NCLIPV Monitor Panel when
enabled.

Coolant
-------
Displays the active coolant setting in the NCLIPV Monitor Panel when enabled.
Recognized coolant settings are Off, Flood, Mist, and Air.

Machine Time
------------
Displays the total machining time since the NCLIPV Playback Control Form was
accepted.

Progress Bar
------------
Displays the percentage of simulation completed since the NCLIPV Playback Form
was accepted.

Docked:
-------
Determines the default position of the NCLIPV Monitor Panel when simulation is
started.  It can either be floating (Off), docked to the left side of the NCL
window (Left), or docked to the right side (Right).  This panel can be manually
positioned by the user after it is displayed.
