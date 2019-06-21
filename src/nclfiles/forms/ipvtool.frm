#HEADER#
/TITLE/ Edit Tool List
/POSITION/ 0,0
/SIZE/ 280,356

#FRAME#
/TITLE/ Cutter
/POSITION/ 8,8
/SIZE/ 255,65

#EDIT#
/LABEL/ Diameter:
/POSITION/ 10,20,65,20
/SIZE/ 70,12
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 12

#EDIT#
/LABEL/ Corner Radius:
/POSITION/ 10,37,65,37
/SIZE/ 40,12
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 12

#EDIT#
/LABEL/ Height:
/POSITION/ 10,54,65,54
/SIZE/ 60,12
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 12

#EDIT#
/LABEL/ Side Angle/Radius:
/POSITION/ 130,20,200,20
/SIZE/ 100,12
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 12

#EDIT#
/LABEL/ Z-Height:
/POSITION/ 130,37,200,37
/SIZE/ 70,12
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 12

#EDIT#
/LABEL/ Flat Angle:   
/POSITION/ 130,54,200,54
/SIZE/ 80,12
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 12

#COLOR#      
/LABEL/ Cut Color:
/POSITION/ 10,76
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ Tolerance:
/POSITION/ 10,93
/SIZE/ 50,12
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ -7
/RANGE/ .0001,2.5

#EDIT#
/LABEL/ Maxang:
/POSITION/ 90,93
/SIZE/ 50,12
/TYPE/ UD_DASVAL
/PREC/ 3
/LEN/ -7
/RANGE/ .001,360.

#EDIT#
/LABEL/ Rapid:
/POSITION/ 165,93
/SIZE/ 50,12
/TYPE/ UD_DASVAL
/PREC/ 2
/LEN/ -7
/RANGE/ .0,100000.

#FRAME# 
/TITLE/ Tool Display
/POSITION/ 10,108
/SIZE/ 255,83

#CHECKBOX#
/LABEL/ Display Edges
/POSITION/15,120
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/ Edge Color:
/POSITION/ 83,120
/SIZE/90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ Translucency:
/POSITION/ 180,120
/SIZE/ 70,12
/TYPE/ UD_DASINT
/PREC/ 3
/LEN/ 3
/RANGE/ 1,100

#CHECKBOX#
/LABEL/
/POSITION/ 15,137
/SIZE/ 10,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Symbol
/POSITION/ 28,137
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#DISPLAY#
/LABEL/
/POSITION/ 75,137
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/PREC/ 80
/LEN/ 15

#COLOR#      
/LABEL/ Color:
/POSITION/ 155,137
/SIZE/ 75,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHECKBOX#
/LABEL/
/POSITION/ 15,154
/SIZE/ 10,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Shank
/POSITION/ 28,154
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#DISPLAY#
/LABEL/
/POSITION/ 75,154
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/PREC/ 80
/LEN/ 15

#COLOR#      
/LABEL/ Color:
/POSITION/ 155,154
/SIZE/ 75,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHECKBOX#
/LABEL/
/POSITION/ 15,171
/SIZE/ 10,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Holder
/POSITION/ 28,171
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#DISPLAY#
/LABEL/
/POSITION/ 75,171
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/PREC/ 80
/LEN/ 15

#COLOR#      
/LABEL/ Color:
/POSITION/ 155,171
/SIZE/ 75,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#FRAME#
/TITLE/ Global Settings
/POSITION/ 8,195
/SIZE/ 255,48

#EDIT#
/LABEL/ Min Height:
/POSITION/ 10,206, 65,206
/SIZE/ 85,12
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 12

#EDIT#
/LABEL/ Max Height:
/POSITION/ 130,206
/SIZE/ 85,12
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 12

#EDIT#
/LABEL/ Min Diameter:
/POSITION/ 10,223, 65,223
/SIZE/ 85,12
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ 12

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 150,223
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#LABEL#
/LABEL/Tool    ISN    CLREC    CUTTER
/POSITION/ 15,250
/SIZE/ 120,14
/TYPE/ UD_DASSTRING

#LISTBOX#
/LABEL/
/POSITION/ 10,259
/SIZE/ 250,60
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Delete
/POSITION/ 10,314
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Undelete
/POSITION/ 70,314
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Reset
/POSITION/ 140,314
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Rescan
/POSITION/ 200,314
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
==============
Edit Tool List
==============
This form allows you to modify the cutting tool parameters and attributes and
to remove tools from the active tool list.

======
Cutter
======
These fields display the actual parameters for the selected mill tool.  You can
modify the parametrics of the cutting tools by changing the appropriate
field(s) in this section.

Cut Color:
----------
Defines the color of the material cut by the selected tool.  This should be set
to 'Default' if the cut color defined in the NCLIPV Simulation Colors form is
used, especially if the cut color is automatically changed each time a new
tool is loaded.

Tolerance:
----------
This field sets the cutting tolerance for the selected tool.  The smaller the
tolerance, the greater accuracy the cut model will have, but the process of
cutting the model will be slower.  You can set high tolerances for roughing
tools and lower (tighter) tolerances for finishing tools and still get the
same results for the final part.

Maxang:
-------
Defines the largest angular change in the tool axis allowed.  Any moves that
cause this value to be exceeded will be broken up into smaller moves.  Once
again the lower the value, the greater the accuracy, but the slower the
process.  This value is only used with multi-axis moves.

Rapid:
------
Sets the rapid feedrate for this tool.  Any feedrates at or above this value
will be treated as rapid moves.

============
Tool Display
============
This section defines a symbol that will be used as the cutter and optional
shank and holder definitions.  The cutter, shank, and holder are considered
parts of a tool and are referenced as such here.

Display Edges:
--------------
Determines if the selected tool should be rendered with its edges displayed.
Displaying the edges of the tool with a low translucency setting is similar
to displaying a wireframe cutter.

Edge Color:
-----------
Defines the color to display the tool edges with.  'Default' uses the same
color as the tool is displayed in, while the other choices select an actual
color.  This color will be used to display the edges of the tool when the
Display Edges box is checked and when the viewing mode is set to wireframe.

Translucency:
-------------
Sets the display translucency of the selected tool from 1 to 100, where 100
displays a solid tool and a lower number displays a more transparent tool.
Changing the value in this field will affect all parts of the tool, i.e., the
cutter, shank, and holder.  Individual translucency settings can be changed
in the Tool Display forms.

Symbol
------
This box is checked when a symbol is used for the actual cutter, for example
when a form tool is used.  Pressing the Symbol button will bring up the
Cutter Display form that allows you to define the cutter symbol parameters.

Symbol Color:
-------------
Defines the display color for the cutter.  'Default' specifies that the color
specified in the NCLIPV Simulation Colors form should be used.

Shank
-----
Enabling the Shank checkbox will cause this tool to include a shank.  The tool
shank can be an extension of the cutter or can be a non-cutting portion of the
tool holder.  Pressing the Shank button will bring up the Shank Display form
that allows you to define the shank parameters.

Shank Color:
------------
Defines the display color for the tool shank.  'Default' specifies that the
color specified in the NCLIPV Simulation Colors form should be used.

Holder
------
Enabling the Holder checkbox will cause this tool to include a holder.
Pressing the Holder button will bring up the Holder Display form that allows you
to define the holder parameters.

Holder Color:
-------------
Defines the display color for the tool holder.  'Default' specifies that the
color specified in the NCLIPV Simulation Colors form should be used.

===============
Global Settings
===============
This section defines minimum and maximum values that certain cutter fields can
have.  Unlike the other sections in this form, the Global Settings fields will
apply to all defined tools in the list.

Min Height:
-----------
Defines the minimum height to use for the defined tools.  Any tool that is
defined with a height of less than this value will be modified to have this
height.

Max Height:
-----------
Defines the maximum height to use for the defined tools.  Any tool that is
defined with a height of more than this value will be modified to have this
height.  Tools defined with an excess height will slow down the verification
process.  This height does not include the height of the tool shank or holder
nor does it affect the tool height of a cutter symbol.

Min Diameter:
-------------
Defines the minimum diameter to use for the defined tools.  Any tool that is
defined with a diameter of less than this value will be modified to have this
diameter.

Apply
-----
The Apply button applies all of the minimum and maximum values in the Global
Settings section to all of the defined tools.  This button will only affect
the tools already contained in the list, if tools are constantly being
redefined to modify the cutter height, but in reality are a single tool, then
set the Min Height value to the desired height and use the Rescan button to
weed out duplicate tools.

Tool List
---------
This field contains the list of active tools for this NCLIPV session.
Originally it will contain all defined non-duplicate tools in the clfile.  It
also reflects any changes that were made to the tools using this form.  It
will not contain any tools that were deleted by the user, as these tools are
no longer active.

Delete
------
Deletes the selected tool.  This tool is no longer active and will not be used
during the NCLIPV session.

Undelete
--------
Restores the previously deleted tool to the active tool list.  Up to 5 tools
can be restored after being deleted.  If you need to restore more than 5
tools, then use the Reset button.

Reset
-----
Restores all tools that have been manually deleted from the active tool list.
It does not reset the attributes that have been changed by the user.

Rescan
------
Rescans the clfile and creates a new active tool list.  Any changes made by
you to the tool list, including cutter parameters, tool attributes, and symbol
modifications will be lost.  In this sense, Rescan can be considered a master
Reset button.  All field values set in the Global Settings section will be
used during the Rescan.
