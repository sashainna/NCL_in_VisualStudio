#HEADER#
/TITLE/ Transfer Geometry By Geometry Type
/POSITION/ 50,50
/SIZE/315,125

#CHECKBOX#
/LABEL/ Rename Geometry
/POSITION/ 175,10
/SIZE/ 100,14
/TYPE/ UD_DASSTRING

#LISTBOX#
/LABEL/
/POSITION/ 12,35
/SIZE/ 120,58
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Geometry Types
/POSITION/ 8,25
/SIZE/ 130,70

#LISTBOX#
/LABEL/
/POSITION/ 180,35
/SIZE/ 120,58
/TYPE/ UD_DASSTRING
/INPUT/ FORM_RECORD

#FRAME#
/TITLE/ Transfer Types
/POSITION/ 175,25
/SIZE/ 130,70

#PUSHBUTTON#
/LABEL/ ->
/POSITION/ 145,45
/SIZE/ 25,15
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ <-
/POSITION/ 145,70
/SIZE/ 25,15
/TYPE/ UD_DASINT

#LABEL#
/LABEL/ Select Geometry Type to Get
/POSITION/ 10,10
/SIZE/ 100,14

#HELP#
==========================
Transfer Geometry By Type
==========================
This form transfers geometry to and from a Secondary Unibase by Geometry Types. 
Types are individually selected from the Geometry Types list and moved to the
Transfer Types list.  All Types in the Transfer Type list will be moved
between the Primary and Secondary Unibases.

This form is used for both GET and PUT operations.  The label at the top of the
form distinquishes which operation is in effect at this time.

Rename Geometry
---------------
When used as for a GET operation the user has the option of renaming geometry
retrieved from the Secondary Unibase if an entity with the same label already
exists in the Primary Unibase with the same label by checking this box.

Geometry Types
--------------
Contains the list of all Geometry types defined in the source Unibase (the Unibase
where the entities will be transfered from).

Transfer Types
---------------
Contains a list of all Types selected to be transferred from the source Unibase
to the destination Unibase.

-> Button
---------
The process of selecting the Types to transfer consists of selecting a Type
in the Geometry Types list and then pressing the -> button to copy this Type
to the Transfer Types list.

<- Button
---------
Selecting a Type in the Transfer Types list and pressing the <- button will
delete this Type from the Transfer Types list.  This Type will no longer
be transfered between the Unibases.

OK
--
Accept the form and perform the transfer.

Cancel
------
Cancel the transfer of geometry.
