#HEADER#
/TITLE/ NCLIPV Simulation Colors
/POSITION/ 50,50
/SIZE/160,227

#COLOR#      
/LABEL/ Cut Color:
/POSITION/ 10,8
/SIZE/90,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#COLOR#      
/LABEL/ Cutter Color:
/POSITION/ 10,25
/SIZE/100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#      
/LABEL/ Holder Color:
/POSITION/ 10,42
/SIZE/100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#      
/LABEL/ Shank Color:
/POSITION/ 10,59
/SIZE/100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#      
/LABEL/ Fixture Collision Color:
/POSITION/ 10,76
/SIZE/135,14
/TYPE/UD_DASSTRING

#COLOR#      
/LABEL/ Holder Collision Color:
/POSITION/ 10,93
/SIZE/135,14
/TYPE/UD_DASSTRING

#COLOR#      
/LABEL/ Rapid Collision Color:
/POSITION/ 10,110
/SIZE/135,14
/TYPE/UD_DASSTRING

#COLOR#      
/LABEL/ Initial Color:
/POSITION/ 10,140
/SIZE/100,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Use Stock Colors:
/POSITION/ 10,154
/SIZE/80,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Use Fixture Colors:
/POSITION/ 10,171
/SIZE/85,14
/TYPE/UD_DASSTRING

#FRAME#
/TITLE/ Auto Colors
/POSITION/ 7,127
/SIZE/ 140,70

#HELP#
This form is used to set the default colors used for NCLIPV simulation.  It
contains fields for the actual cut made and various aspects of the cutting
tool.

Cut Color:
----------
Defines the default color of the material cut by any tool.  This field can be
set to Auto, which automatically changes the cut color each time the tool is
changed.

Cutter Color:
-------------
Defines the default display color of all tools.  'Default' will display the
tool using the current Cut Color.

Holder Color:
-------------
Defines the default display color of all tool holders.  'Default' will display
the holder using the current Cutter Color.

Shank Color:
------------
Defines the default display color of all tool shanks.  'Default' will display
the shank using the current Cutter Color.

Fixture Collision Color:
------------------------
Defines the color of the cut material whenever a fixture is cut by a tool.

Holder Collision Color:
-----------------------
Defines the color of the cut material whenever a stock is cut by the tool
holder.

Rapid Collision Color:
-----------------------
Defines the color of the cut material whenever a stock is cut by the tool at a
Rapid rate.  The Rapid rate for each tool can be setup separately in the
Edit Tool List form.

===========
Auto Colors
===========
The Auto Color field settings are used then the Cut Color field is set to Auto,
in which case the Cut Color will automatically change whenever a new tool is
defined.

Initial Color:
--------------
Selects the initial color to use with automatic cut color changes.  The first
tool will use this color and the subsequent tools will use the next colors in
order.  When the last color is reached, the color set in this field will be 
reused and the sequence of colors will be repeated.

Use Stock Colors:
-----------------
Check this box if you would like to have colors that have been assigned to
stocks used in the list of automatic cut colors.  This field will usually be
disabled, since the cut color would not be discernable from the stock it was
cut out of.


Use Fixture Colors:
-------------------
Check this box if you would like to have colors that have been assigned to
fixtures used in the list of automatic cut colors.  This field can be enabled,
since fixture cuts are assigned a separate color from the normal cut color.
