#HEADER#
/TITLE/ Define Scalar
/POSITION/ 50,50
/SIZE/270,100

#EDIT#
/LABEL/ Label:  
/POSITION/ 10,8,40,8
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#COMBLIST_DROPDOWN#
/LABEL/ Class:
/POSITION/ 90, 8, 112,8
/SIZE/ 150,70
/TYPE/ UD_DASSTRING
/LEN/ 20

#EDIT#
/LABEL/ Value:
/POSITION/ 10,25,40,25
/SIZE/200,14
/TYPE/ UD_DASSTRING
/PREC/ 40
/LEN/ 40

#EDIT#
/LABEL/ Description:
/POSITION/ 10,42,55,42
/SIZE/130,14
/TYPE/ UD_DASSTRING
/PREC/ 40
/LEN/ 40

#PUSHBUTTON#
/LABEL/ Define
/POSITION/ 70,60
/SIZE/100,14
/TYPE/ UD_DASSTRING

#HELP#
This form is used to define a scalar, including the class of scalars it belongs
to and a description for the scalar.

Label:
------
The Label field contains the scalar label to assign the value to.  If the scalar
is already defined, then the Value, Class, and Description fields will be
updated with the appropriate values.  The label cannot exceed 63 characters.

Value:
------
The Value field accepts the value to assign to the scalar.  It will allow for
entry of any scalar value or number, including arithmetic expressions, such as
'a+b' providing a and b are valid scalars.

Class:
------
The Class field will accept the Class to assign the scalar to.  You can select
an existing class or define a new one by typing it into this field.  The Class
cannot exceed 20 characters.

Description:
------------
The Description field contains the Scalar description.  The description cannot
exceed 63 characters.

Define
------
When the Define button is pressed the scalar will be defined using the form
input without closing the form.

OK
--
Accepts the form and defines the scalar.  The form will be closed.

Cancel
------
Cancels the form without defining the scalar.
