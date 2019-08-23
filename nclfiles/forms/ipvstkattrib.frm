#HEADER#
/TITLE/ NCLIPV Geometry Attributes
/POSITION/ 0,0
/SIZE/ 250,260

#LABEL#
/LABEL/ Modify Stock Attributes
/POSITION/ 8,8
/SIZE/ 100,14
/TYPE/ UD_DASSTRING
/FONT/ 1.25
/COLOR/ BLUE

#LABEL#
/LABEL/ Modify Fixture Attributes
/POSITION/ 8,8
/SIZE/ 100,14
/TYPE/ UD_DASSTRING
/FONT/ 1.25
/COLOR/ BLUE

#COLOR#
/LABEL/ Color:
/POSITION/ 10,25
/SIZE/75,14
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ Visible:
/POSITION/ 90,25
/SIZE/60,40
/TYPE/UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHOICEBOX#
/LABEL/ Active:
/POSITION/ 160,25
/SIZE/60,40
/TYPE/UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHECKBOX#
/LABEL/ Reset Cut Colors
/POSITION/ 10,42
/SIZE/ 75,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Important
/POSITION/ 90,42
/SIZE/ 65,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Translucency:
/POSITION/ 10,59
/SIZE/ 70,14
/TYPE/UD_DASINT
/PREC/ 0
/LEN/ 3
/RANGE/ 1,100

#EDIT#
/LABEL/ Toler:
/POSITION/ 90,59
/SIZE/ 70,14
/TYPE/UD_DASVAL
/PREC/ 5
/LEN/ -7
/RANGE/ .00001,1.

#CHECKBOX#
/LABEL/ Display edges
/POSITION/ 10,76
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#
/LABEL/ Edge Color:
/POSITION/ 85,76
/SIZE/90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHECKBOX#
/LABEL/ 
/POSITION/ 10,103
/SIZE/ 20,14
/TYPE/ UD_DASSTRING

#CHOICE_LIST#
/LABEL/ Matrix:
/POSITION/ 30,103
/SIZE/ 90,70
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Load
/POSITION/ 133,103
/SIZE/ 40,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Modify
/POSITION/ 180,103
/SIZE/ 40,14
/TYPE/ UD_DASINT

#CHECKBOX#
/LABEL/ Incremental
/POSITION/ 10,120
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Display axes
/POSITION/ 72,120
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#
/LABEL/ Axes Color:
/POSITION/ 133,120
/SIZE/90,14
/TYPE/UD_DASSTRING

#FRAME#
/TITLE/ Transformation
/POSITION/ 8,93
/SIZE/ 225,47

#COMBLIST_SIMPLE#
/LABEL/ Geometry:
/POSITION/12,150
/SIZE/220,65
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Apply to All
/POSITION/ 20,219
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Delete
/POSITION/ 100,219
/SIZE/ 40,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 150,219
/SIZE/ 40,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 200,219
/SIZE/ 40,14
/TYPE/ UD_DASINT

#HELP#
Use this form to modify the attributes of existing stock or fixture solids.
Though a solid can be deleted from the machining session from this form, you
may not add nor modify the canonical data of the solid here.  Use the Stock or
Fixture menus to add a solid.  You must first delete the existing stock and
then create a new stock to modify the canonical data of a solid.

Color
-----
Defines the color of the unadulterated solid.  The cut faces will have the color
defined by the tool.

Visible
-------
Defines whether the solid is visible or not.  You will normally make the solid
inactive if you make it invisible.

Active
------
Activates or deactivates the solid in the machinining session.  A deactivated
solid will not be cut by the tool and will remain in its pristine form.
The machining session will be faster with less solids defined and activated.

Reset Cut Colors
----------------
Normally when a solid's color is changed only the uncut faces of the solid are
changed, the cut faces remain the same color as when they were generated.
Checking this box will cause the cut faces to be set to the same color as the
solid color defined in the Color field.

Important
---------
When this box is checked then the solid will be deemed important during the
simulation and any unimportant solids that obscure this solid will be made
translucent so that this solid can be clearly seen.  The dynamic feature of
making obscuring solids translucent can be enabled or disabled using the
NCLIPV Display Properties form.

Translucency
------------
Sets the display translucency of the selected solid from 1 to 100, where 100
displays an opaque solid and a lower value displays a more transparent solid.

Toler
-----
This field sets the tolerance to use for solids of revolution, profile solids,
and solids from STL files.

Display Edges:
--------------
Determines if the selected solid should be rendered with its edges displayed.
When a solid has its edges displayed then both the outline of the solid and
the cut faces will have edges displayed.  This can highlight the complexity of
a displayed cut solid, sometimes to an extent that the display is overwhelming.

Edge Color:
-----------
Defines the color to display the solid edges with.  'Default' uses the same
color as the solid is displayed in, while the other choices select an actual
color.  This color will be used to display the edges of the solid when the
Display Edges box is checked and when the viewing mode is set to wireframe.

Transformation
--------------
Enabling the Matrix field applies a transformation matrix to the selected solid.
This matrix can be a predefined matrix or can be typed in by the user.  Once
the Matrix field is enabled, you can either type in a matrix name or select one
from the display.  Once a matrix name is entered, you must press the Load button
to actually load the selected matrix.

The Modify button brings up a form that displays the canonical data of the
transformation matrix.  You can create your own matrix by typing in values in
this form or just simply view the current matrix.

It is recommended for speed and memory purposes that any transformation matrix
that is to be applied to a solid be done using the Stock/Fixture modals form.

By default the transformation matrix will apply to the position of the stock
as it was originally defined, not to its current position as transformed by
any matrix.  Enabling the Incremental check box will cause the specified
transformation matrix to be applied to the stock's current position, i.e. it
will be multiplied to the the stock's active transformation matrix.

Enabling the Display Axes checkbox displays the part coordinate system as
modified through the transformation matrix associated with the solid.  This
provides a visual representation on how the solid is transformed from the
original coordinate system.  These coordiante axes will not be displayed
during machine simulation, since the solid can be further transformed through
the machine movement.

Geometry List
-------------
Displays a list of all defined solids.  Selecting a solid from this list will
show the attributes for this solid in the previous fields.  Changing the
attribute fields will apply to the selected solid in this list.

Apply to All
------------
Enabling this checkbox causes all attribute changes made in the form to be
applied to all defined solids in the Geometry List.  Changes will only be made
if the OK button is used to close the form.  Pressing CANCEL will discard all
changes.

Delete
------
The Delete button will delete the current solid selection from the Geometry
List.  This is an immediate action (OK does not have to be pressed) and once
a solid is deleted, the only way to get it back is to redefine it.

View
----
Displays a representation of the defined solid in the NCL graphics window.
This representation is not a permanent addition to the display and will be
erased whenever a screen repaint occurs.

Apply
-----
Applies the attribute changes to the current solid selection from the Geometry
List.  If the 'Apply to All' box is checked, then the attributes will be
assigned to all defined solids.  This is an immediate action, the new
attributes are applied without closing the form.
