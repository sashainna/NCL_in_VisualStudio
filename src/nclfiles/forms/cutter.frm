#HEADER#
/TITLE/ Cutter Definition
/POSITION/ 0,0
/SIZE/ 340,400

#LABEL#
/LABEL/ Diagram:
/POSITION/ 125,45
/SIZE/ 35,17
/TYPE/UD_DASSTRING

#PICTUREBOX#
/FILE/ toolib_End Mill.jpg
/NAME/ cuttype
/POSITION/ 161,48
/SIZE/ 130,120

#DISPLAY#
/LABEL/ Tool:
/POSITION/ 10,8, 32,8
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/PREC/ 15
/LEN/ 15

#DISPLAY#
/LABEL/ Description:
/POSITION/ 110,8
/SIZE/ 180,14
/TYPE/ UD_DASSTRING
/PREC/ 30
/LEN/ 30

#CHOICE_LIST#
/LABEL/ Cutter Type:
/POSITION/ 10,25,57,25
/SIZE/ 120,70
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Pseudo Cutter: 
/POSITION/ 140,25
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Define...
/POSITION/ 220,25
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#FRAME# 
/TITLE/ Parameters
/POSITION/ 10,45
/SIZE/ 110,115

#EDIT#
/LABEL/ Diameter:  
/POSITION/ 15,58, 60, 55
/SIZE/ 120,14
/TYPE/ UD_SCAVAL
/PREC/ 8
/LEN/ 8

#EDIT#
/LABEL/ Corner Rad:
/POSITION/ 15,75, 60, 72
/SIZE/ 120,14
/TYPE/ UD_SCAVAL
/PREC/ 8
/LEN/ 8

#EDIT#
/LABEL/ Height:    
/POSITION/ 15,92, 60, 89
/SIZE/ 120,14
/TYPE/ UD_SCAVAL
/PREC/ 8
/LEN/ 8

#EDIT#
/LABEL/ Side Angle:
/POSITION/ 15,110, 60, 107
/SIZE/ 120,14
/TYPE/ UD_SCAVAL
/PREC/ 8
/LEN/ 8

#EDIT#
/LABEL/ Z-Height:  
/POSITION/ 15,127, 60, 124
/SIZE/ 120,14
/TYPE/ UD_SCAVAL
/PREC/ 8
/LEN/ 8

#EDIT#
/LABEL/ Flat Angle:
/POSITION/ 15,144, 60, 141
/SIZE/ 120,14
/TYPE/ UD_SCAVAL
/PREC/ 8
/LEN/ 8

#FRAME# 
/TITLE/ Tool Display
/POSITION/ 10,170
/SIZE/ 320,82

#CHOICEBOX#      
/LABEL/ Segment:
/POSITION/ 15,180
/SIZE/ 100,60
/TYPE/ UD_DASSTRING
/CHOICES/ "Partial", "Full"

#CHOICEBOX#      
/LABEL/ Moving:
/POSITION/ 140,180
/SIZE/ 70,60
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#CHOICEBOX#      
/LABEL/ Shaded:
/POSITION/ 220,180
/SIZE/ 70,60
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHECKBOX#
/LABEL/
/POSITION/ 15,200
/SIZE/ 10,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Symbol
/POSITION/ 28,200
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#DISPLAY#
/LABEL/
/POSITION/ 75,200
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/PREC/ 80
/LEN/ 15

#PUSHBUTTON#
/LABEL/ Symbol Lib:
/POSITION/ 160, 200
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/PREC/ 15
/LEN/ 15

#EDIT#
/LABEL/
/POSITION/ 225,200
/SIZE/ 100,14
/TYPE/ UD_DASSTRING
/PREC/ 15
/LEN/ 15

#CHECKBOX#
/LABEL/
/POSITION/ 15,217
/SIZE/ 10,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Shank
/POSITION/ 28,217
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#DISPLAY#
/LABEL/
/POSITION/ 75,217
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/PREC/ 80
/LEN/ 15

#CHECKBOX#
/LABEL/
/POSITION/ 160,217
/SIZE/ 10,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Holder
/POSITION/ 175,217
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#DISPLAY#
/LABEL/
/POSITION/ 225,217
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/PREC/ 80
/LEN/ 15

#CHOICEBOX#
/LABEL/ Cutter/Disply Commands:
/POSITION/ 15,234
/SIZE/ 145,60
/TYPE/ UD_DASSTRING
/CHOICES/ "Reference", "Output"

#PUSHBUTTON#
/LABEL/ View Tool
/POSITION/ 175,234
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#      
/LABEL/ Tool Library...
/POSITION/ 10,257
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#EDIT#      
/LABEL/ 
/POSITION/ 70,257
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/PREC/ 40
/LEN/ 30

#PUSHBUTTON#      
/LABEL/ Parameters...
/POSITION/ 200,257
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/Output Tool Library Commands
/POSITION/ 10,278
/SIZE/ 110,14
/TYPE/ UD_DASSTRING

#LISTTABLE#
/LABEL/
/POSITION/ 10,295
/SIZE/ 300,85
/TYPE/ UD_DASSTRING
/DOUBLE_CLICK/ *ON

#HELP#
======================
Cutter Definition Form
======================
This form is used to define the active cutter along with cutter display
parameters.  The cutter can be defined using its canonical parameters or it can
be selected from an existing tool library.  A separate cutter can also be
defined for motion purposes while another cutter definition is used for display
purposes.

Tool:
-----
This field is for display purposes only and is either blank or contains the tool
number selected from the tool library list at the bottom of the form.

Description:
------------
This field is also for display purposes only and contains the cutter description
of the cutter selected from the tool library.

Cutter Type:
------------
Select the type of cutter you are defining.  There are basically three different
styles of cutters, Mills, Blades, and Lathe Bits.  The cutter parameter field
labels and accessibility will change depending on the cutter type selected.  The
cutter diagram will also display a sample cutter with the appropriate fields
labeled.

Pseudo Cutter:
--------------
Check this box if you need to drive a cutter with different parameters than the
cutter actually used on the machine.  This allows you to display the real cutter
on the screen while driving the pseudo cutter.  A form will be displayed after
checking this box that contains the pseudo cutter parameters.

Define...
---------
Press this button to bring up the Pseudo Cutter form.  This button is only
active when the Pseudo Cutter box is checked.

==============
Standard Mills
==============
The following fields describe the cutter parameters as they apply to the
standard Mill style cutters Face Mill, End Mill, Cone, Bell, Drill, Boring Tool,
Reamer, and Chamfer Tool.

Diameter
--------
The Diameter parameter defines the diameter of the tool at the top of the corner
radius.

Corner Rad:
-----------
Defines the bottom corner radius of the cutter.  This field will not be active
for all styles of Mill cutters.

Height:
-------
Defines the height of the cutter.

Side Angle:
-----------
The Side Angle parameter specifies the draft angle on the side of the cutter.  A
positive value defines a cone shaped cutter and a negative value defines a bell
shaped cutter.

=============
Barrel Cutter
=============
The following fields describe the cutter parameters as they apply to the Barrel
Mill style cutter.

Diameter
--------
The Diameter parameter defines the diameter of the tool at the top of the corner
radius.

Corner Rad:
-----------
Defines the bottom corner radius of the cutter.

Height:
-------
Defines the height of the cutter.

Side Radius:
------------
The Side Radius parameter specifies the radius of the side of the barrel cutter.
This parameter is the one that defines the "barrel bulge" of the cutter.

Z-Height:
---------
The Z-Height parameter specifies the center point of the Side Radius.  This
height is measured from the bottom of the cutter to the center point of the arc
describing the Side Radius.  The Z-Height may be a negative value.

Flat Angle:
-----------
The optional Flat Angle parameter specifies the angle of the flat on the side
of the barrel cutter.  If this field is set to zero, then there is no flat side
on the cutter.

The flat extends from a tangency point at the top of the side radius to the
specified height of the cutter.  A positive angle defines a cutter with the flat
angling away from the top of the cutter.  A negative angle leans toward the
center of the cutter.

Side Angle:
-----------
The Side Angle parameter specifies the draft angle on the side of the cutter.  A
positive value defines a cone shaped cutter and a negative value defines a bell
shaped cutter.

============
Blade Cutter
============
The following fields describe the cutter parameters as they apply to the Blade
style cutter.  This cutter is only valid for machines that support Ultrasonic
Blades.

Width
-----
The Width parameter defines the width of the blade as viewed from the front.

Chizel:
-------
Defines the width of the flat spot along the bottom of the blade.  Setting this
parameter to zero will define a blade with a sharp point at the end.

Height:
-------
Defines the height of the blade.

Angle:
------
The Angle parameter specifies the draft angle on the side of the blade.  This
is typically the cutting edge of the blade.  This angle will be represented on
both sides of the blade even if the actual blade only contains one cutting side.

=============
Lathe Inserts
=============
The following fields describe the cutter parameters as they apply to the
standard Lathe style insert bit shapes, Square, Diamond, Triangle, and Circular.

Radius:
-------
The Radius parameter defines the radius at the tip of the Lathe Insert.  This
is the only parameter used when driving a Lathe style cutter.  The remaining
parameters are used for display only.

Diameter:
---------
The Diameter parameter defines the diameter of the inscribed circle within the
Lathe Insert.

Height:
-------
The Height parameter defines the height of the tool.  Lathe Inserts will be
displayed starting at Z=0, with the lower part of the tool displayed at
Z=-Height.

Angle:
------
The Angle parameter defines the included angle of the nose tip.  It is
automatically set to 90 for Square Inserts, 60 for Triangle Inserts, and 0 for
Circular Inserts.

Mount Angle:
------------
The Mount Angle parameter defines the angle to rotate the tool between the
X-axis and the tool tip.

=============
Grooving Tool
=============
The following fields describe the cutter parameters as they apply to the
standard Lathe Grooving Tool.

Radius:
-------
The Radius parameter defines the radius at the leading tip of the Grooving Tool.
This is the only parameter used when driving a Grooving Tool.  The remaining
parameters are used for display only.

Width:
------
The Width parameter defines the width along the X-axis for a Grooving Tool.

Height:
-------
The Height parameter defines the height of the tool.  Lathe Inserts will be
displayed starting at Z=0, with the lower part of the tool displayed at
Z=-Height.

Length:
-------
The Length parameter defines the length of a Grooving Tool along the Y-axis.

==================
Display Parameters
==================
The Display Parameter fields define attributes that are only used for display
purposes in NCL.

Segment:
--------
Setting this field to Partial will draw a minimum representation of the cutter
in wireframe mode.  Full draws a more complete representation of the cutter,
with multiple line segments drawn along the side of the cutter.

Moving:
-------
Setting this field to On will cause the cutter to move along with the motion as
it is displayed.  Only a single cutter will be displayed at the current tool
location.  Setting it to Off will display a cutter at each move.  These cutters
will remain visible as the tool moves to the next location.

Shaded:
-------
Setting this field to On will result in a shaded tool being displayed.  Setting
it to Off displays a wireframe cutter.  A shaded cutter is more realistic, but
will take longer to animate.  This field will affect all parts of the tool
(cutter, shank, holder).  Each of these tool parts can be shaded individually
by setting the Shaded field in the Tool Display form.

Symbol
------
This box is checked when a symbol is used for the actual cutter, for example
when a form tool is used.  Pressing the Symbol button will bring up the Cutter
Display form that allows you to define the cutter symbol parameters.

Symbol Lib:
-----------
This field specifies the default symbol library to use when loading symbols to
use as the cutter or holder display.  NCL will first look for this library in
the local directory and then in the system directory.

Shank
-----
Enabling the Shank box will cause the tool display to include a shank.  For Mill
style cutters, this shank will be displayed at the top of the cutter.  The shank
will be attached at the inscribed circle for Lathe Inserts and at the top of the
cutter for Grooving Tools.

Pressing the Shank button will bring up the Shank Display form that allows
you to define the shank parameters.

Holder
------
Enabling the Holder checkbox will cause this tool to include a holder.  Pressing
the Holder button will bring up the Holder Display form that allows you to
define the holder parameters.

Cutter/Disply Commands:
-----------------------
Use this field to designate how the CUTTER/DISPLY parameters specified in this
section are processed.  When Reference is specified, then the parameters will
be set without the corresponding commands being output to the part program.
Setting this field to Output will cause all of the CUTTER/DISPLY commands to be
output to the part program.

View Tool
---------
This button will only be enabled when a tool from the Tool Library is selected
as the active cutter and a drawing has been associated with this tool.  Pressing
the View Tool button will display this drawing in a pocket window.

============
Tool Library
============
The Tool Library fields allow a tool from an existing library to be used to
define the tool parameters.  The default tool library can be specified by the
NCL_TOOLIB environmental variable.

Tool Library...
---------------
Pressing this button will bring up a file browser form that allows you to select
an existing tool library to load.  This tool library must be reside in either
the Local or System directories.  The System directory for tool libraries is
defined by the NCL_TOOL environmental variable.

Parameters...
-------------
The Parameters button will only be enabled when a tool is selected from the Tool
List that contains parameter definitions.  Pressing this button will bring up a
form with the defined tool parameters and their default values.  Please refer to
the TOOLIB documentation for a description of these parameters.

Output Tool Library Commands
----------------------------
Check this box to enable output of the tool library commands associated with the
selected tool.  Marking this box will cause a CUTTER/READ statement to be issued
instead of a CUTTER/TOOL statement.

Tool List
---------
The Tool List contains a list of all tools contained in the active Tool Library
that pass the Filter selection.  Selecting one of these tools will automatically
fill in all appropriate fields in the form with the settings from this tool.
When a tool is selected and no changes are made to the other form fields, then a
CUTTER/TOOL statement will be output.  If any field is changed after selecting a
tool from this list, then this tool is no longer active and individual CUTTER
commands will be output for all parameters, with no reference to this tool.  In
this manner you can use a tool from the library as a template.

Please refer to the TOOLIB documentation for a complete description of Tool
Libraries.

