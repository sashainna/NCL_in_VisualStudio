#HEADER#
/TITLE/ Select Data
/POSITION/ 50,50
/SIZE/300,195

#FRAME#
/TITLE/ 
/POSITION/ 8, 4
/SIZE/ 200,50

#DISPLAY#
/LABEL/ Selection:
/POSITION/ 10,14
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/PREC/ 15
/LEN/ 15

#DISPLAY#
/LABEL/ Type:
/POSITION/ 120,14
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/PREC/ 12
/LEN/ 12

#PUSHBUTTON#
/LABEL/ Filter:
/POSITION/ 10,32
/SIZE/35,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 50, 32
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/LEN/ 10

#PUSHBUTTON#
/LABEL/ Exclude:
/POSITION/ 110,32
/SIZE/35,15
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 150,32
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/LEN/ 10

#FRAME#
/TITLE/ Data and Data Items
/POSITION/ 8,60
/SIZE/ 280,108

#DATATABLE#
/LABEL/
/POSITION/ 12,70
/SIZE/ 270,95
/TYPE/ UD_DASSTRING


#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 220,8
/SIZE/ 60,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Cancel
/POSITION/ 220,26
/SIZE/ 60,14
/TYPE/ UD_DASINT

#HELP#
This form is used for displaying all defined DATA statment definitions.
The selection of a DATA element or a DATA label from the list will be entered 
automatically into the active form field or command line prompt when the 
"Apply" button is pressed or the form is closed.

Selection:
----------
Displays the currently selected DATA element's label or the DATA label as it 
will be output to the command line or a form field.  This field cannot be 
changed by the user.

Type:
-----
Displays the entity type of the data element.  The data element type can be a
geomtric entity (Point, Line, etc.), Vocabulary word, Value, or Text string.
This field is empty if the selection is the Data label itself.

Filter:
-------
This field is used to filter the data displayed in the 'Data and Data Items'
field. Press the Filter button to filter the list.

Exclude:
--------
This field is used to filter the data displayed in the 'Data and Data Items'
field. This is an opposite filter to the "Filter", in such that any Data  
belonging to the filter in this field will NOT be displayed.  Press the
Exclude button to filter the list.

Apply
------
Apply will send the selected data element or a DATA label from the displayed 
list to the active text field without closing the form.

Cancel
------
Cancels the form without making a selction.

Data and Data Items
-------------------
This list can be used to simply view the defined Data statements or you can
select an individual Data element or the Data label from the list and send
it to the active command line or form field by pressing the "Apply" or 
"Close" button.

When the Apply or Close button is pressed, the selected data will be sent
to the current active text field, which is a command window or an active
form field. In the case of a Command Window, the reference to the DATA
statement "D1[1]" will be inserted in the line and not the value shown in
the form. In the case of sending the data to the active form field, it
depends on the form field's type, if the form field type is a text string,
then the reference to the DATA statement element "D1[1]" will be inserted
in the form field, otherwise the data item value will be used.

Close
-----
Selects the data or data element and closes the form.
