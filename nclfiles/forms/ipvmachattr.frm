#HEADER#
/TITLE/ Machine Component Attributes
/POSITION/ 0,0
/SIZE/ 210,175

#COLOR#      
/LABEL/ Color:
/POSITION/ 10,8
/SIZE/75,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Visible:
/POSITION/ 90,8
/SIZE/70,40
/TYPE/UD_DASSTRING
/CHOICES/ "No", "Yes"

#EDIT#
/LABEL/ Translucency:
/POSITION/ 10,25
/SIZE/180,14
/TYPE/UD_DASINT
/PREC/ 0
/LEN/ 3
/RANGE/ 1,100

#CHOICEBOX#
/LABEL/ Reversed:
/POSITION/ 90,25
/SIZE/70,40
/TYPE/UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHECKBOX#
/LABEL/ Display Edges
/POSITION/8,42
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/ Edge Color:
/POSITION/ 90,42
/SIZE/100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#PUSHBUTTON#
/LABEL/ Component:
/POSITION/ 10,59
/SIZE/ 40,14
/TYPE/ UD_DASINT

#DISPLAY#
/LABEL/
/POSITION/ 60,59
/SIZE/80,14
/TYPE/UD_DASSTRING
/LEN/ 20

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 150,59
/SIZE/ 40,14
/TYPE/ UD_DASINT

#LISTBOX#
/LABEL/
/POSITION/12,86
/SIZE/70,70
/TYPE/UD_DASSTRING

#LISTBOX#
/LABEL/
/POSITION/110,86
/SIZE/70,70
/TYPE/UD_DASSTRING

#FRAME#
/TITLE/ Axes
/POSITION/ 8,76
/SIZE/ 80,70

#FRAME#
/TITLE/ Solids
/POSITION/ 105,76
/SIZE/ 80,70

#HELP#
=================================
Machine Component Attributes Form
=================================
Use this form to modify the attributes of existing machine components.  You
can modify the attributes of an entire machine component (axis) or a single
solid within a component.

Color
-----
Defines the color of the machine component or solid.

Visible
-------
Defines whether the machine component or solid is visible or not.  Collision
checking will still be active for a component that is invisible.

Translucency
------------
Sets the display translucency of the selected machine component or solid. The
translucency value can be in the range of 1 to 100, where 100 displays an opaque
solid and a lower value displays a more transparent solid.

Reversed
--------
The direction that a component moves on the machine is normally in reference
to the tool direction.  Therefore, a table type axis will usually move in the
opposite direction from its programmed direction, so that the tool moves in
the correct direction in relationship to the part.  Enabling the Reversed
attribute causes this axis to move in the opposite direction from its programmed
direction.  NCLIPV will automatically set all table type axes to be reversed
when the machine is loaded.  There is also a Reverse command that can be
placed in the 'postworks.mdl' file to manually set the default reversal flag
for each axis.

Display Edges:
--------------
Determines if the selected machine component should be rendered with its edges
displayed.  Displaying the edges of a solid in essence outlines the solid with
a solid color and improves the appearance of extruded solids and solids of
revolution.

Edge Color:
-----------
Defines the color to display the solid edges with.  'Default' uses the same
color as the solid is displayed in, while the other choices select an actual
color.  This color will be used to display the edges of the machine component
when the Display Edges box is checked and when the viewing mode is set to
wireframe.

Component
---------
Press the Component button to interactively select an individual solid to
modify the attributes for.  The Component text field will show the active
machine axis or individual solid that is currently active for modification,
whether it was interactively picked or selected from the Axes or Solids lists.

Apply
-----
The Apply button must be pressed for any attributes to be changed for the active
component or solid.  Pressing the Close button on the form will simply close the
form without applying any of the changes.

Axes
----
The Axes field contains a list of all defined components (axes) for the loaded
machine.  Selecting one of these components will make it the active component
for modification and any changes made to this component will be applied to all
solids contained in the component.  All solids within the selected component
will be highlighted in the NCLIPV window.

Solids
------
The Solids field contains a list of all solids that are part of the currently
selected machine component.  This list will change whenever another component
is selected.  Selecting a solid from this list will make it the active solid
for modification and any attribute changes made will be applied to this solid
only.  The selected solid will be highlighted in the NCLIPV window.
