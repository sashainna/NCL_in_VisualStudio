#HEADER#
/TITLE/ NCLIPV Geometry Modals
/POSITION/ 50,50
/SIZE/245,266

#LABEL#
/LABEL/ Stock Modals
/POSITION/ 8,8
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/FONT/ 1.25
/COLOR/ BLUE

#LABEL#
/LABEL/ Fixture Modals
/POSITION/ 8,8
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/FONT/ 1.25
/COLOR/ BLUE

#EDIT#
/LABEL/ Starting ID:
/POSITION/ 10,25
/SIZE/140,14
/TYPE/UD_DASINT
/PREC/ 0
/LEN/ 3
/RANGE/ 1,1000000000

#COLOR#
/LABEL/ Color:
/POSITION/ 10,52
/SIZE/75,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Translucency:
/POSITION/ 95,52
/SIZE/70,14
/TYPE/UD_DASINT
/PREC/ 0
/LEN/ 3
/RANGE/ 1,100

#CHECKBOX#
/LABEL/ Important
/POSITION/ 180,52
/SIZE/60,14
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ Visible:
/POSITION/ 10,69
/SIZE/60,40
/TYPE/UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHOICEBOX#
/LABEL/ Active:
/POSITION/ 85,69
/SIZE/60,40
/TYPE/UD_DASSTRING
/CHOICES/ "No", "Yes"

#EDIT#
/LABEL/ Tolerance:
/POSITION/ 160,69
/SIZE/80,14
/TYPE/UD_DASVAL
/PREC/ 4
/LEN/ -6
/RANGE/ .00001,.5

#CHECKBOX#
/LABEL/ Display Edges
/POSITION/ 12,86
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#
/LABEL/ Edge Color:
/POSITION/ 85,86
/SIZE/95,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHECKBOX#
/LABEL/ Reset on Redefinition
/POSITION/ 12,103
/SIZE/ 85,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Delete on Session Reset
/POSITION/ 115,103
/SIZE/ 115,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Attributes
/POSITION/ 8,42
/SIZE/ 230,81

#CHOICEBOX#
/LABEL/ Format:
/POSITION/ 10,135
/SIZE/80,40
/TYPE/UD_DASSTRING
/CHOICES/ "Ascii", "Binary"

#CHECKBOX#
/LABEL/ Stop on Error
/POSITION/ 12,152
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Deactivate on Error
/POSITION/ 90,152
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Skip Error Checking
/POSITION/ 12,169
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ STL Files
/POSITION/ 8,125
/SIZE/ 230,62

#CHECKBOX#
/LABEL/
/POSITION/ 12,201
/SIZE/ 15,14
/TYPE/ UD_DASSTRING

#CHOICE_LIST#
/LABEL/ Matrix:
/POSITION/ 30,201
/SIZE/ 90,70
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Load
/POSITION/ 133,201
/SIZE/ 40,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Modify
/POSITION/ 180,201
/SIZE/ 40,14
/TYPE/ UD_DASINT

#CHECKBOX#
/LABEL/ Display axes
/POSITION/ 12,218
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#
/LABEL/ Axes Color:
/POSITION/ 85,218
/SIZE/90,14
/TYPE/UD_DASSTRING

#FRAME#
/TITLE/ Transformation
/POSITION/ 8,191
/SIZE/ 230,45

#HELP#
Use this form to set the default solid attributes for stocks and fixtures.  The
settings in this form will be used for all new stocks or fixtures defined.

Starting ID
-----------
Defines the numeric ID of the next stock/fixture to be defined.  All subsequent
solids will be numbered consecutively.  The stock/fixture ID values are used
to reference the individual solids in the STOCK and FIXTUR commands.

Color
-----
Defines the color of the unadulterated solid.  The cut faces will have the color
defined by the tool.

Translucency
------------
Sets the display translucency of defined solids from 1 to 100, where 100
displays an opaque solid and a lower value displays a more transparent solid.

Important
---------
When this box is checked then the solid will be deemed important during the
simulation and any unimportant solids that obscure this solid will be made
translucent so that this solid can be clearly seen.  The dynamic feature of
making obscuring solids translucent can be enabled or disabled using the
NCLIPV Display Properties form.

Visible
-------
Defines whether the solid is visible or not.  You will normally make the solid
inactive if you make it invisible.

Active
------
Activates or deactivates the solid in the machinining session.  A deactivated
solid will not be cut by the tool and will remain in its pristine form.
The machining session will be faster with less solids defined and activated.

Tolerance
---------
This field sets the tolerance to use when creating solids of revolution, profile
solids, and solids from STL files.

Display Edges:
--------------
Determines if the defined solids should be rendered with their edges displayed.
When a solid has its edges displayed then both the outline of the solid and
the cut faces will have edges displayed.  This can highlight the complexity of
a displayed cut solid, sometimes to an extent that the display is overwhelming.

Edge Color:
-----------
Defines the color to display the solid edges with.  'Default' uses the same
color as the solid is displayed in, while the other choices select an actual
color.  This color will be used to display the edges of the solids when the
Display Edges box is checked and when the viewing mode is set to wireframe.

Reset on Redefinition
---------------------
Checking this box will cause solids to take on the default attributes as
defined in this form whenever they are redefined, either interactively or
when restarting the simulation.  When this box is not checked, then the solids
will maintain their current color, translucency, visibility, active state,
tolerance, and edge display when they are redefined.

Delete on Session Reset
-----------------------
This box should be checked if the defined solids should be deleted when the
NCLIPV session is reset.  This ensures that the session will start fresh with
no defined solids.  If you interactively create any solids, then this setting
should not be used, since these solids will not be automatically created
when the simulation is rerun.  NCLIPV will keep all solids defined interactively
and through the simulation file when the session is reset if this box is not
checked.

STL File Format
---------------
Specifies the format of STL files saved during the machining session, either
ASCII or Binary.  The format of external STL files that are loaded into a
machining session will automatically be determined.

Stop On Error
-------------
Enabling this field will cause NCLIPV to output an error message whenever an
STL file is loaded that contains an invalid solid.  The solid could have self
intersections or not be closed.  An error message will be output to the NCLIPV
log file regardless of this setting.

Deactivate on Error
-------------------
Enabling this field will cause NCLIPV to deactivate and invisible an invalid
solid loaded from an STL file.  This is to ensure that the model is not
corrupted and a fatal error is not encountered during the material removal
process.  If you choose to disable this field, then the results of the
NCLIPV session cannot be guaranteed to be correct.  You can also manually
activate this solid using the NCLIPV Geometry Attributes form.

Skip Error Checking
-------------------
Enabling this field will disable error checking when loading an STL model.
Error checking consists of making sure that the model does not have any self
intersecting patches and ensuring that it is closed.  These are requirements
for solids that are to be used for simulation (cutting) purposes, but not
really required for solids that are used for visual representation only
(machine components, fixtures, etc.).

There have been cases of large STL models that are used for a minimal amount
of machining that do not pass the required tests.  In this case it may be
acceptable to skip the error checking to see if the simulation works.  If the
simulation fails, then you know that error checking is required for this model.

Transformation
--------------
Enabling the Matrix field applies a transformation matrix to the defined solids.
This matrix can be a predefined matrix or can be typed in by the user.  Once
the Matrix field is enabled, you can either type in a matrix name or select one
from the display.  Once a matrix name is entered, you must press the Load button
to actually load the selected matrix.

The Modify button brings up a form that displays the canonical data of the
transformation matrix.  You can create your own matrix by typing in values in
this form or just simply view the current matrix.

Enabling the Display Axes checkbox displays the part coordinate system as
modified through the transformation matrix associated with the solid.  This
provides a visual representation on how the solid is transformed from the
original coordinate system.  These coordinate axes will not be displayed
during machine simulation, since the solid can be further transformed through
the machine movement.
