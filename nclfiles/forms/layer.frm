#HEADER#
/TITLE/ Layer Management
/POSITION/ 50,50
/SIZE/240,250

#EDIT#
/LABEL/ Layer:
/POSITION/ 10,8
/SIZE/55,14
/TYPE/UD_DASINT
/PREC/ 4
/LEN/ 4
/RANGE/ 0,9999

#EDIT#
/LABEL/ Description:
/POSITION/ 10,25
/SIZE/120,14
/TYPE/UD_DASSTRING
/PREC/ 80
/LEN/ 40
/RANGE/ 0,9999

#PUSHBUTTON#
/LABEL/ Add / Modify
/POSITION/ 80,8
/SIZE/ 60,14
/TYPE/ UD_DASINT

#CHECKBOX#
/LABEL/ Visible
/POSITION/ 12,52
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ All
/POSITION/ 72,52
/SIZE/ 40,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Swap
/POSITION/ 122,52
/SIZE/ 40,14
/TYPE/ UD_DASINT

#CHECKBOX#
/LABEL/ Pickable
/POSITION/ 12,69
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ All
/POSITION/ 72,69
/SIZE/ 40,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Swap
/POSITION/ 122,69
/SIZE/ 40,14
/TYPE/ UD_DASINT

#CHECKBOX#
/LABEL/ Active
/POSITION/ 12,86
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Find
/POSITION/ 72,86
/SIZE/ 40,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Reset
/POSITION/ 122,86
/SIZE/ 40,14
/TYPE/ UD_DASINT

#FRAME#
/TITLE/ Attributes
/POSITION/ 8,42
/SIZE/ 220,64

#LISTBOX#
/LABEL/
/POSITION/ 12,121
/SIZE/ 210,65
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Layers
/POSITION/ 8,111
/SIZE/ 220,75

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 17,200
/SIZE/ 60,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Delete
/POSITION/ 87,200
/SIZE/ 60,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 157,200
/SIZE/ 60,14
/TYPE/ UD_DASINT

#FRAME#
/TITLE/ Actions
/POSITION/ 8,190
/SIZE/ 220,30

#HELP#
Use this form to define new layers, modify existing layers, and to delete
layers.  Except for the Action buttons, no action will be taken on the layers
until the OK button on the form is pressed.  Pressing the CANCEL button will
discard all changes on the form, except for the Action buttons which are acted
upon immediately.

Layer
-----
Enter the layer number that you want to add or modify.  This field will
automatically be set when a layer is selected from the list.

Description
-----------
Enter the description of the layer currently being defined.  This field will
automatically be set when a layer is selected from the list.  You can modify
an existing layer's description using this field.

Add / Modify
------------
Pressing this button will add a new layer or change the attributes of an
existing layer.  The Layer list in the form will be modified, but these changes
will not take effect until the form is accepted or required by one of the Action
buttons.  Selecting a new layer from the list will automatically perform this
function.

Visible
-------
If this box is checked, then all entities on this layer and any entities that
are later defined on this layer will be made visible.  Clearing this box will
make all entities on this layer invisible.

All
---
Change the visibility of all layers to visible or invisible depending on the
state of the Visible check box.

Swap
----
Toggles the visibility of all defined layers.  Entities on layers that were
visible before will become invisible and vice versa.

Pickable
--------
If this box is checked, then all entities on this layer and any entities that
are later defined on this layer can selected during a picking sequence.
Clearing this box will make it so all entities on this layer cannot be selected
by the user.

All
---
Change the pickable state of all layers to pickable or not pickable depending
on the state of the Pickable check box.

Swap
----
Toggles the layer entities ability to be selected during a picking sequence.

Active
------
Makes the current layer the active layer.  All entities defined after this
change will be placed on this layer until the active layer is changed again.
Clearing this box has no effect, since at least one layer has to be the active
layer.  You can only change the active layer by checking this box for the
desired layer.

Find
----
Locates the active layer and makes it the current layer in the form.

Reset
-----
Resets all form fields to their default values.  Press this button if changes
were made to the layer or its attributes that you do not want to save.

Layers
------
Displays a list of all defined layers.  Selecting a layer from this list will
first save any changes made to the current layer (or add it if the layer is new)
and will then make the selected layer the current layer.

Select
------
Allows you to select entities that will be placed onto the current layer.  This
in effect changes the layer attribute of the selected geometry.

Delete
------
Moves all entities on the current layer to another layer and deletes the current
layer.  Another form with a list of available layers to move to will be
displayed.  After this action is completed, the current layer will no longer
exist in this Unibase.

Apply
-----
Applies all of the changes made to the layer attributes without closing the
form.
