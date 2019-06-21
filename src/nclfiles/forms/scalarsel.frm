#HEADER#
/TITLE/ Select Scalar
/POSITION/ 50,50
/SIZE/300,185

#CHOICE_LIST#
/LABEL/ Scalar Class:
/POSITION/ 10,14
/SIZE/ 120,70
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Filter:
/POSITION/ 140,14
/SIZE/30,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 175,14
/SIZE/ 30,14
/TYPE/ UD_DASSTRING
/LEN/ 8

#CHOICE_LIST#
/LABEL/ Exclude:
/POSITION/ 10,32
/SIZE/ 120,70
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Exclude:
/POSITION/ 140,32
/SIZE/30,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 175,32
/SIZE/ 30,14
/TYPE/ UD_DASSTRING
/LEN/ 8

#FRAME#
/TITLE/ 
/POSITION/ 8, 4
/SIZE/ 210,50

#LISTTABLE#
/LABEL/
/POSITION/ 12,70
/SIZE/ 270,90
/TYPE/ UD_DASSTRING
/DOUBLE_CLICK/ *ON

#FRAME#
/TITLE/ Scalars and Descriptions
/POSITION/ 8,60
/SIZE/ 280,100

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 225,8
/SIZE/ 50,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Cancel
/POSITION/ 225,26
/SIZE/ 50,14
/TYPE/ UD_DASINT

#HELP#
This form is used for displaying all defined scalars including their assigned
labels, classes, values, descriptions and modified date/time.  The selection of 
a scalar from the list will be entered automatically into the active form 
field or command line prompt when the "Apply" button is pressed or the form is
closed.

Scalar Class:
-------------
This field is used to filter the scalars displayed in the 'Scalars and
Descriptions' field.  You can select 'All' to display all scalars or select
an individual class of scalars to display.

Scalar Filter:
-------------
This field is used to filter the scalars displayed in the 'Scalars and
Descriptions' field. 

Exclude Class:
-------------
This field is used to filter the scalars displayed in the 'Scalars and
Descriptions' field.  This is an opposite filter to the "Scalar Class".  
Any Scalars belonging to the selected class in this line will NOT be displayed.

Exclude Filter:
-------------
This field is used to filter the scalars displayed in the 'Scalars and
Descriptions' field. This is an opposite filter to the "Scalar Filter".  
Any Scalars belonging to the filter in this line will NOT be displayed.

Scalars and Descriptions
------------------------
Displays a list of scalars including their labels, classes, values,
descriptions and modified date/time.  Selecting a scalar from this list and
pressing the Apply or Close button will place this scalar in the active text
field (command prompt or form field).

Apply
------
Apply will send the selected Scalar to the active text field without closing
the form.

Cancel
------
Cancels the form without selecting a scalar.

Close
-----
Selects the scalar and closes the form.
