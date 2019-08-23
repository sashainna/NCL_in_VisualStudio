#HEADER#
/TITLE/ Load Delimited File
/POSITION/ 50,30
/SIZE/260,140

#PUSHBUTTON#
/LABEL/ File:
/POSITION/ 8,10
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 42, 10
/SIZE/120,14
/TYPE/UD_DASSTRING
/LEN/ 31
/PREC/ 96

#EDIT#
/LABEL/ Label:
/POSITION/ 183, 10
/SIZE/65,14
/TYPE/UD_DASSTRING
/LEN/ 8
/PREC/ 96

#CHOICEBOX#
/LABEL/ Blank Field:
/POSITION/ 8,35
/SIZE/107,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Space", "Scalar", "Vocab", "Error"

#EDIT#
/LABEL/ Value:
/POSITION/ 122, 35
/SIZE/40,14
/TYPE/UD_DASSTRING
/LEN/ 21
/PREC/ 96

#CHECKBOX#
/LABEL/ Start Line Number
/POSITION/ 8,56
/SIZE/ 72,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 90, 57
/SIZE/25,14
/TYPE/UD_SCAVAL
/PREC/ 5
/LEN/ -5

#CHECKBOX#
/LABEL/ End Line Number
/POSITION/ 130,56
/SIZE/ 72,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 210, 57
/SIZE/25,14
/TYPE/UD_SCAVAL
/PREC/ 5
/LEN/ -5

#CHECKBOX#
/LABEL/ Store Vocabulary Words as Strings
/POSITION/ 8,77
/SIZE/ 122,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Store Entity Labels as Strings
/POSITION/ 140,77
/SIZE/ 122,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Store Number of Definitions
/POSITION/ 8,96
/SIZE/ 103,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Variable:
/POSITION/ 121, 98
/SIZE/30,14
/TYPE/UD_DASSTRING
/LEN/ 19
/PREC/ 96

#HELP#
===================
Load Delimited File
===================

This form creates a DATA definition that loads an existing comma or tab
delimited file.  The file can contain any valid DATA statement elements.
Each line read from the file will create a new DATA definition that will
be labeled based on the original label given for the DATA statement.

File
----
Enter the external file name in this field either by typing it in or by
pressing the prompt button and selecting it from the file browser.

Label
-----
All DATA statements must have a label.  There are three label choices:

  1. A label with no scalar suffix or subscript.

  2. A label with a scalar suffix.

  3. A label with a subscript.

All new statements created using an original label from case 1 will be
subscripted starting at 1.  Statements created using an original label from
case 2 will be labeled with the alphabetic prefix of the original label and
will have a numeric suffix that is incremented from the original suffix.
If the statements are created using a label from case 3, then the new
statements will be given the same label with an incremented subscript.

Blank Field
-----------
Select how to handle adjacent commas from the following choices:  

 Error  - Give an error at the first occurence of a blank entry.

 Scalar - Store a scalar value at each occurence of a blank entry.

 Vocab  - Insert a given vocabulary word at each occurence of a blank entry.

 Space  - Insert a blank string at each occurence of a blank entry.

If Value or Vocab are selected, then a corresponding value must be provided.

Start Line
----------
Check this box to provide a starting line position to start reading from in
the given file.  All lines prior to the starting position will be ignored.
A value of 0 or 1 will process from the beginning of the file.

End Line
--------
Check this box to provide an ending line position to read through in the given
file.  All lines after to the ending position will be ignored.  A value of 0
will process until the end of the file.

Store Vocabulary Words as Strings
---------------------------------
Check this box to select how to store any vocabulary words read from the 
file.  If the box is checked, then all vocabulary words read in will be stored
as text strings.

Store Entity Labels as Strings
------------------------------
Check this box to select how to store any text strings read from the file that
match previously labeled entities (geometry, scalars, etc.).  If this box is
checked, then all predefined entity labels read in will be stored as text
strings.

Store Number of Definitions
---------------------------
Check this box to provide a variable for storing the total number of DATA
definitions created.  The number of definitions will be the total number of
lines read from the input file.  Blank lines will be ignored.

Variable
--------
Provide the variable for storing the number of DATA definitions created.  If 
the Store Number of Definitions box is checked a variable must be provided.
