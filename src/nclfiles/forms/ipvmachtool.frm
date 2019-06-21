#HEADER#
/TITLE/ Tool Lengths
/POSITION/ 0,0
/SIZE/ 245,120

#DISPLAY#
/LABEL/ Tool:
/POSITION/ 10,8
/SIZE/ 40,12
/TYPE/ UD_DASINT
/PREC/ 8
/LEN/ 8

#EDIT#
/LABEL/ Length:
/POSITION/ 80,8
/SIZE/ 40,12
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 10

#EDIT#
/LABEL/ Offset:
/POSITION/ 155,8
/SIZE/ 40,12
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 10

#LISTBOX#
/LABEL/
/POSITION/ 10,25
/SIZE/ 220,80
/TYPE/ UD_DASSTRING

#HELP#
=================
Tool Lengths Form
=================
The Tool Lengths form is used to view and modify the programmed tool lengths
used in the Machine Simulation.  It is very important that the correct tool
length is programmed for each tool, as this length is used to determine where
to mount the tool in the spindle.  The tool length is typically defined as
follows.

    'cutter height' + 'shank height' + 'fixed distance'

Where the fixed distance is the distance from the bottom of the holder to the
tool stop position of the spindle as defined by the AxisLine definition of the
ToolSpindle axis.

Tool:
-----
The Tool field shows the tool currently being viewed or modified.  This field
cannot be changed by the user.

Length:
-------
The Length field contains the tool length as programmed by the LOADTL command.
This length can be modified by the user.

Offset:
-------
The Offset field provides a method of modifying the tool length without changing
the actual length programmed in the Simulation file.  The value entered in the
Offset field will be added to the programmed tool length when attaching the tool
to the spindle.

Tool List
---------
The Tool List contains a list of all defined tools for this Simulation file.
Selecting one of these tools will automatically fill in the form fields with
the settings from this tool.  These settings can then be changed by the user.
