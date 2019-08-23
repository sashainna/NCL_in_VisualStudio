#HEADER#
/TITLE/ NCLIPV Clash Settings
/POSITION/ 0,0
/SIZE/ 215,270

#LABEL#
/LABEL/ Check Clashes Between:
/POSITION/ 10,8
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Rapid
/POSITION/ 10,32
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Fixtures
/POSITION/ 10,49
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Heads
/POSITION/ 10,66
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Axes
/POSITION/ 10,83
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Stocks
/POSITION/ 115,32
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Fixtures
/POSITION/ 115,49
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Heads
/POSITION/ 115,66
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Axes
/POSITION/ 115,83
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Stocks
/POSITION/ 10,117
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Fixtures
/POSITION/ 10,134
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Heads
/POSITION/ 10,151
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Axes
/POSITION/ 10,168
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Stocks
/POSITION/ 115,117
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Fixtures
/POSITION/ 115,134
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Axes
/POSITION/ 115,151
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Post-processor Errors
/POSITION/ 10,190
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 65,32
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 65,49
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 65,66
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 65,83
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 170,32
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 170,49
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 170,66
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 170,83
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 65,117
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 65,134
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 65,151
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 65,168
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 170,117
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 170,134
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Log
/POSITION/ 170,151
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Tools and
/POSITION/ 8,20
/SIZE/ 90,80

#FRAME#
/TITLE/ Holders and
/POSITION/ 112,20
/SIZE/ 90,80

#FRAME#
/TITLE/ Heads and
/POSITION/ 8,107
/SIZE/ 90,80

#FRAME#
/TITLE/ Axes and
/POSITION/ 112,107
/SIZE/ 90,80

#CHECKBOX#
/Label/ Log
/POSITION/ 110,190
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/Label/ Log File
/POSITION/ 160,190
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/ Clash Color:
/POSITION/ 10,207
/SIZE/100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#PUSHBUTTON#
/Label/ Clear All
/POSITION/ 10,224
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/Label/ Set All
/POSITION/ 75,224
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/Label/ Defaults
/POSITION/ 140,224
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#HELP#
==========================
NCLIPV Clash Settings Form
==========================

This form controls the processing of clashes during an NCLIPV session.  Clashes
can be detected between all machine components, including Tools, Holders,
Stocks, Fixtures, Heads, and Tables.  Disabling any clash detection that is not
necessary will speed up the performance of NCLIPV.


Tools and Rapid
---------------
Check this box if you would like NCLIPV playback to stop when the part or
fixture is cut during a RAPID move.  Checking the Log box will cause this clash
to be output to the log file and counted as an error.

Tools and Fixtures
------------------
Check this box if you would like NCLIPV playback to stop when the fixture is
cut during a programmed move.  Checking the Log box will cause this clash to be
output to the log file and counted as an error.

Tools and Heads
---------------
Check the this box if you would like NCLIPV playback to stop when the tool and
a head axis collide during a programmed move.  This type of clash is usually
not checked for, since the tool is mounted in the spindle, which is the basis
for determining which axes are heads.  A head type axis is defined as any axis
that is connected to the spindle.

Tools and Axes
--------------
Check the this box if you would like NCLIPV playback to stop when the tool and
a table axis collide during a programmed move.  A table type axis is defined as
any axis that is not connected to the spindle.

Holders and Stocks
------------------
Check this box if you would like NCLIPV playback to stop when the part is cut
cut by the tool holder during a programmed move.  Checking the Log box will
cause this clash to be output to the log file and counted as an error.

Holders and Fixtures
--------------------
Check this box if you would like NCLIPV playback to stop when a fixture is cut
by the tool holder during a programmed move.  Checking the Log box will cause
this clash to be output to the log file and counted as an error.

Holders and Heads
-----------------
Check the this box if you would like NCLIPV playback to stop when the tool
holder and a head axis collide during a programmed move.  This type of clash
checking is usually not enabled, for the same reason as for tools and heads.

Holders and Axes
----------------
Check the this box if you would like NCLIPV playback to stop when the tool
holder and a table axis collide during a programmed move.  Checking the Log box
will cause this clash to be output to the log file and counted as an error.

Heads and Stocks
----------------
Check this box if you would like NCLIPV playback to stop when a head type axis
collides with the part. Checking the Log box will cause this clash to be output
to the log file and counted as an error.

Heads and Fixtures
------------------
Check this box if you would like NCLIPV playback to stop when a head type axis
collides with a fixture. Checking the Log box will cause this clash to be output
to the log file and counted as an error.

Heads and Heads
---------------
Check this box if you would like NCLIPV playback to stop when a head type axis
collides with another head type axis.  This type of collision is normally not
checked for.

Heads and Axes
--------------
Check this box if you would like NCLIPV playback to stop when a head type axis
collides with a table type axis. Checking the Log box will cause this clash to
be output to the log file and counted as an error.

Axes and Stocks
---------------
Check this box if you would like NCLIPV playback to stop when a table type axis
collides with the part.  This type of collision is usually not checked for,
since the part is normally mounted on a table type axis.

Axes and Fixtures
-----------------
Check this box if you would like NCLIPV playback to stop when a table type axis
collides with a fixture.  This type of collision is usually not checked for,
since fixtures are normally mounted on a table type axis.

Axes and Axes
-------------
Check this box if you would like NCLIPV playback to stop when a table type axis
collides with another table type axis.  This type of collision is normally not
checked for.

Post-processor Errors
---------------------
Check this box if you would like NCLIPV to stop whenever a post-processor error
is encountered.  Post-processor errors are only reported when a Simulation
file is used for playback.  Checking the Log box will cause this error to be
output to the log file and counted as an error.

Log File
--------
Pressing this button will bring up the NCLIPV Log File form.

Clash Color:
------------
When two entities clash you can have NCLIPV momentarily change the color of the
entities during the clash to the color set in this field.  The color of the
entities will be restored on the next move.

All entity types, fixtures, tools, and machine components will change color
during a clash except for stocks.  This is because when the color of the
entity is restored, then all faces on the entity are reset to the entity's
original color, including cut faces.

Clashes must be enabled in order to detect a clash and be represented by the
clash color.  They can be enabled for stopping and/or logging of the clashes.

Clear All
---------
The Clear All button will clear all check boxes in this form, disabling all
clash detection.

Set All
-------
The Set All button will set all check boxes in this form, enabling all types of
clash detection.

Defaults
--------
The Defaults button will set the clash detection fields to their default
condition as specified in the 'nclipv.mod' file.
