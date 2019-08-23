#HEADER#
/TITLE/ Transfer Geometry By Layer
/POSITION/ 50,50
/SIZE/315,125

#CHECKBOX#
/LABEL/ Rename Geometry
/POSITION/ 115,10
/SIZE/ 100,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Offset:
/POSITION/ 220,11
/SIZE/ 90,14
/TYPE/ UD_DASINT
/PREC/ 0
/LEN/ -6

#LISTBOX#
/LABEL/
/POSITION/ 12,35
/SIZE/ 120,58
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Defined Layers
/POSITION/ 8,25
/SIZE/ 130,70

#LISTBOX#
/LABEL/
/POSITION/ 180,35
/SIZE/ 120,58
/TYPE/ UD_DASSTRING
/INPUT/ FORM_RECORD

#FRAME#
/TITLE/ Transfer Layers
/POSITION/ 175,25
/SIZE/ 130,70

#PUSHBUTTON#
/LABEL/ ->
/POSITION/ 145,35
/SIZE/ 25,15
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ <-
/POSITION/ 145,55
/SIZE/ 25,15
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Thru
/POSITION/ 145,75
/SIZE/ 25,15
/TYPE/ UD_DASINT

#LABEL#
/LABEL/ Select Layers to Get
/POSITION/ 10,10
/SIZE/ 80,14

#HELP#
==========================
Transfer Geometry By Layer
==========================
This form transfers geometry to and from a Secondary Unibase by layers.  Layers
are individually selected from the Defined Layers list and moved to the
Transfer Layers list.  All layers in the Transfer Layer list will be moved
between the Primary and Secondary Unibases.

This form is used for both GET and PUT operations.  The label at the top of the
form distinquishes which operation is in effect at this time.

Rename Geometry
---------------
When used as for a GET operation the user has the option of renaming geometry
retrieved from the Secondary Unibase if an entity with the same label already
exists in the Primary Unibase with the same label by checking this box.

Offset:
-------
Geometry transfered from the Secondary Unibase can have its layer attribute
offset by a value as specified in this field.

Defined Layers
--------------
Contains the list of all layers defined in the source Unibase (the Unibase
where the entities will be transfered from).

Transfer Layers
---------------
Contains a list of all layers selected to be transferred from the source Unibase
to the destination Unibase.

-> Button
---------
The process of selecting the layers to transfer consists of selecting a layer
in the Defined Layers list and then pressing the -> button to copy this layer
to the Transfer Layers list.

<- Button
---------
Selecting a layer in the Transfer Layers list and pressing the <- button will
delete this layer from the Transfer Layers list.  This layer will no longer
be transfered between the Unibases.

Thru Button
-----------
The Thru button allows you to easily select a range of layers to transfer.
Select a starting layer from the Defined Layers list and then press the Thru
button.  The layer number and THRU will be displayed in the Transfer Layers
list.  Now select the ending layer number.  The Transfer layers list will
contain the entry 'start THRU end'.  All of the defined layers in the defined
range will be transfered between the Unibases.

OK
--
Accept the form and perform the transfer.

Cancel
------
Cancel the transfer of geometry.
