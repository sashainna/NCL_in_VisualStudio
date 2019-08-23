#HEADER#
/TITLE/ Select Layer
/POSITION/ 50,50
/SIZE/240,130

#LABEL#
/LABEL/ Select a Layer
/POSITION/ 10,8
/SIZE/ 40,14

#LABEL#
/LABEL/ Select a Layer to move entities to
/POSITION/ 10,10,10,8
/SIZE/ 110,14

#LISTBOX#
/LABEL/
/POSITION/ 12,35
/SIZE/ 210,65
/TYPE/ UD_DASSTRING
/DOUBLE_CLICK/ *ON

#FRAME#
/TITLE/ Layers
/POSITION/ 8,25
/SIZE/ 220,75

#PUSHBUTTON#
/LABEL/ Cancel
/POSITION/ 160,8
/SIZE/ 40,14
/TYPE/ UD_DASINT

#HELP#
This form is used for simple layer selection and for selecting a layer to move
entities to.

Layers
------
Displays a list of all defined layers that you can select from.

Cancel
------
Cancels the form without selecting a layer.

Close
-----
Selects the layer and closes the form.
