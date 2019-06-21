#HEADER#
/TITLE/ Tool Display
/POSITION/ 0,0
/SIZE/ 340,180

#PUSHBUTTON#
/LABEL/ Symbol:
/POSITION/ 10,25
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 54,25
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/PREC/ 80
/LEN/ 15

#PUSHBUTTON#
/LABEL/ Axis-PV:
/POSITION/ 133,25
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 178,25
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/PREC/ 15
/LEN/ 15

#CHOICEBOX#      
/LABEL/ Shaded:
/POSITION/ 252,25
/SIZE/ 70,60
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHOICEBOX#      
/LABEL/ Clashes:
/POSITION/ 252,25
/SIZE/ 70,60
/TYPE/ UD_DASSTRING
/CHOICES/ "Cutter", "Holder"

#EDIT#
/LABEL/ Diameter:
/POSITION/ 10,42, 45,42
/SIZE/ 60,14
/TYPE/ UD_SCAVAL
/PREC/ 8
/LEN/ 8

#EDIT#
/LABEL/ Height:
/POSITION/ 90,42, 120,42
/SIZE/ 60,14
/TYPE/ UD_SCAVAL
/PREC/ 8
/LEN/ 8

#EDIT#
/LABEL/ Z-Attach:
/POSITION/ 170,42, 202,42
/SIZE/ 60,14
/TYPE/ UD_SCAVAL
/PREC/ 8
/LEN/ 8

#EDIT#
/LABEL/ Z-Depth:
/POSITION/ 250,42, 282,42
/SIZE/ 60,14
/TYPE/ UD_SCAVAL
/PREC/ 8
/LEN/ 8

#CHECKBOX#
/LABEL/ Display Edges
/POSITION/10,59
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/ Edge Color:
/POSITION/ 85,59
/SIZE/90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ Translucency:
/POSITION/ 190,59
/SIZE/ 70,12
/TYPE/ UD_DASINT
/PREC/ 3
/LEN/ 3
/RANGE/ 1,100

#CHOICEBOX#      
/LABEL/ Clashes:
/POSITION/ 10,59
/SIZE/ 70,60
/TYPE/ UD_DASSTRING
/CHOICES/ "Cutter", "Holder"

#CHOICE_LIST#
/LABEL/ Class:
/POSITION/ 62,84, 85,79
/SIZE/ 87,70
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Load...
/POSITION/ 280,79
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#LISTBOX#
/LABEL/
/POSITION/ 10,94
/SIZE/ 140,65
/TYPE/ UD_DASSTRING

#LISTBOX#
/LABEL/
/POSITION/ 170,94
/SIZE/ 140,65
/TYPE/ UD_DASSTRING

#LABEL#
/LABEL/ Cutter Display
/POSITION/ 15,8
/SIZE/ 75,14
/TYPE/ UD_DASSTRING
/FONT/ 1.25
/COLOR/ BLUE

#LABEL#
/LABEL/ Profiles
/POSITION/ 15,86
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#LABEL#
/LABEL/ Symbols
/POSITION/ 175,86
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
============
Tool Display
============
This form is used to define the cutter, shank, and holder display parameters.
It can be activated either from the Cutter Definition form or from the NCLIPV
Edit Tool List form.

Symbol:
-------
Valid entities that can be used for the tool display include curves, surfaces
of revolution, external profiles, CADD symbols, and Visual Solids.  When a CADD
symbol representation of the tool is used as a mill cutter with NCLIPV, then it
should contain only a single surface of revolution, otherwise this symbol will
be ignored in NCLIPV.  The symbol is used for display purposes only in NCL and
can contain any geometry.  This is the same with Visual Solids, only solids of
revolution can be used as mill cutters in NCLIPV.

With lathe cutters, the CADD symbol should contain a single composite curve
when used with NCLIPV, otherwise it will be ignored.  This composite curve
will automatically be extruded in NCL and NCLIPV in order to represent a three
dimensional solid shape.  The symbol is used for display purposes only in NCL
and can contain any geometry.  This is the same with Visual Solids, only
extruded solids can be used as lathe cutters in NCLIPV.


Pressing the Symbol: button allows you to pick a curve, surface of
revolution, or Visual Solid from the screen.

Axis-PV:
--------
This field is only active when a curve is used for the tool display.  It allows
you to specify an axis of rotation for the curve display.  By default, the
curve should be defined in the XY-plane and a point-vector of 0,0,0,0,1,0 will
be used to revolve the curve.  Pressing the Axis-PV: button allows you to pick
a point-vector from the screen.

Shaded:
-------
Setting this field to On will result in the tool part being defined to be
displayed as shaded.  Setting it to Off displays the tool part in wireframe.
Each part of the tool (cutter, shank, holder) can be shaded individually.  This
field will only be active when a tool is being defined in NCL.

Clashes:
--------
This field is only active when a shank is being defined.  It controls how the
shank is treated during the NCLIPV simulation session.  Specifying 'Cutter'
causes the tool shank to be treated as a part of the cutting portion of the
tool as defined by the cutter parameters.  Clashes will only be reported if the
tool shank violates a fixture or during Rapid moves.

'Holder' specifies that the tool shank should be classified as a part of the
tool holder, causing clashes to be reported whenever it violates a fixture or
a stock.

Display Edges:
--------------
Determines if the NCLIPV tool part should be rendered with its edges displayed.
Displaying the edges of the tool with a low translucency setting is similar
to displaying a wireframe cutter.  This field will only be active when a tool
is being edited in the NCLIPV Tool List.

Edge Color:
-----------
Defines the color to display the tool edges with.  'Default' uses the same
color as the tool is displayed in, while the other choices select an actual
color.  This color will be used to display the edges of the tool when the
Display Edges box is checked and when the viewing mode is set to wireframe.
This field will only be active when a tool is being edited in the NCLIPV Tool
List.

Translucency:
-------------
Sets the display transparency of the tool part from 1 to 100, where 100 displays
a solid tool and a lower number displays a more transparent tool.  Each part of
the tool (cutter, shank, holder) can have their own translucency setting.  This
field will only be active when a tool is being edited in the NCLIPV Tool List.

================
Mill Style Tools
================
Diameter:
---------
Defines the diameter of a Mill style shank or holder.  Specifying a value of
zero will cause the diameter to be the same as the defined cutter at the top
of the tool.  This field is only active when defining a shank or holder and a
symbol has not been defined as the tool shape.

Height:
-------
Defines the height of a Mill style shank or holder.  This field is only active
when defining a shank or holder and a symbol has not been defined as the tool
shape.

Z-Attach:
---------
Defines the attachment offset of the shank or holder along the Z-axis.  The
shank and holder can be offset in either the negative or positive direction.

=================
Lathe Style Tools
=================

Width:
------
The Width parameter defines the width along the X-axis of the tool shank or
holder.

Length:
-------
The Length parameter defines the length along the Y-axis of the tool shank or
holder.

Z-Depth:
--------
Defines the Z-axis depth of the tool part.  The default depth is the defined
cutter height.  This field is only valid when the tool shape is a composite
curve that will be extruded.

Z-Attach:
---------
Defines the starting Z-axis position of the tool part.  The default starting
position is at the bottom of the cutter or top of the shank.

X-offset:
---------
This field will only be active when a symbol is defined as a tool part.  It
specifies the X-axis offset value for attaching the symbol to the Lathe tool.
Specifying an Attach point of 0 will attach the tool part at the center of
the inscribed circle for Lathe Inserts or at the top of a Grooving Tool or
shank.

Y-offset:
---------
This field specifies the Y-axis offset value for attaching the active tool part
to the rest of the Lathe tool.  Specifying an Attach point of 0 will attach the
tool part at the center of the inscribed circle for Lathe Inserts or at the top
of a Grooving Tool or shank.

========
Profiles
========
The Profiles List contains a list of all profiles contained in the NCL_TOOL_DESC
library that pass the Class selection.  Selecting one of these profiles will
automatically update the Symbol field with the profile name.

Class:
------
The Class field allows you to diplay only the profiles that belong to the same
class as selected in this field.  Selecting 'All' will display all profiles
defined in the library.

=======
Symbols
=======
The Symbols List contains a list of all defined CADD Symbols.  Selecting one of
these symbols will automatically update the Symbol field with the symbol name.

Load...
-------
The load button brings up the Load Symbol form and allows you to load an
external symbol into the active Unibase.  This symbol will then be displayed
in the Symbols List and will automatically be selected as the tool symbol.
