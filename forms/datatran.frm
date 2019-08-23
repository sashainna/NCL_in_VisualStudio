#HEADER#
/TITLE/ Transfer Variables
/POSITION/ 50,50
/SIZE/420,140

#PUSHBUTTON#      
/LABEL/ Filter
/POSITION/ 10,27
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#      
/LABEL/
/POSITION/ 60,27
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/LEN/ 9
/PREC/ 9

#CHOICEBOX#
/LABEL/Type:
/POSITION/ 110,27
/SIZE/ 90,70
/TYPE/ UD_DASSTRING
/CHOICES/ "All","Scalar","Text Variable", "Data Statement"

#CHOICE_LIST#
/LABEL/ Scalar Class:
/POSITION/ 210,27
/SIZE/ 120,70
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/Variable      Value
/POSITION/ 8,45
/SIZE/ 180,68


#LISTTABLE#
/LABEL/
/POSITION/ 12,55
/SIZE/ 170,58
/TYPE/ UD_DASSTRING

#LISTTABLE#
/LABEL/
/POSITION/ 230,55
/SIZE/ 170,58
/TYPE/ UD_DASSTRING
/INPUT/ FORM_RECORD

#FRAME#
/TITLE/ Transfer Variables
/POSITION/ 225,45
/SIZE/ 180,68

#PUSHBUTTON#
/LABEL/ ->
/POSITION/ 195,55
/SIZE/ 25,15
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ <-
/POSITION/ 195,90
/SIZE/ 25,15
/TYPE/ UD_DASINT

#LABEL#
/LABEL/ Select Variables to Put
/POSITION/ 10,10
/SIZE/ 150,14

#HELP#
==========================
Transfer Variables
==========================
This form transfers variables to and from a Secondary Unibase. 
Variables are individually selected from the Variables list and moved to the
Transfer Variables list.  All Variables in the Transfer Variables list will 
be moved between the primary and secondary Unibases.

This form is used for both GET and PUT operations. The label at the top of the
form distinquishes which operation is in effect at this time.

Variables
--------------
Contains the list of all Variables defined in the source Unibase (the 
Unibase where the entities will be transfered from).

Transfer Variables
---------------
Contains a list of all variables selected to be transferred from the source 
Unibase to the destination Unibase.

-> Button
---------
The process of selecting the variable to transfer consists of selecting a 
variable in the Variables list and then pressing the -> button to 
copy this variable to the Transfer Variables list.

<- Button
---------
Selecting a variable in the Transfer Variables list and pressing the 
<- button will delete this variable from the Transfer Types list.  This 
variable will no longer be transfered between the Unibases.

OK
--
Accept the form and perform the transfer.

Cancel
------
Cancel the transfer of geometry.
