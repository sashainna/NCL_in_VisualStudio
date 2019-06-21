#HEADER#
/TITLE/ Machine Clash Detection
/POSITION/ 0,0
/SIZE/ 200,110

#FRAME#
/TITLE/ Check Clashes Between:
/POSITION/ 10,8
/SIZE/ 180,68

#CHECKBOX#
/Label/ Heads and Heads
/POSITION/ 15,18
/SIZE/ 150,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Heads and Axes
/POSITION/ 15,36
/SIZE/ 150,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Axes and Axes
/POSITION/ 15,54
/SIZE/ 150,14
/TYPE/ UD_DASSTRING

#HELP#
============================
Machine Clash Detection Form
============================

This form controls the processing of clashes during a Machine Simulation
session.  Clashes can be detected between "Heads and Heads", "Heads and Axes",
and "Axes and Axes".  Disabling any clash detection that is not necessary will
speed up the performance of machine simulation.

Heads and Heads
---------------
Check this box if you would like Machine Simulation to stop when a head type
axis collides with another head type axis.  This type of collision is normally
not checked for.

Heads and Axes
--------------
Check this box if you would like Machine Simulation to stop when a head type
axis collides with a table type axis.

Axes and Axes
-------------
Check this box if you would like Machine Simulation to stop when a table type
axis collides with another table type axis.  This type of collision is normally
not checked for.
