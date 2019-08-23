#HEADER#
/TITLE/ Select Macro
/POSITION/ 0,0
/SIZE/ 300,210

#PUSHBUTTON#      
/LABEL/ Filter
/POSITION/ 10,10
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#      
/LABEL/
/POSITION/ 60,10
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/LEN/ 11
/PREC/ 11

#CHOICE_LIST#
/LABEL/ Macro Class:
/POSITION/ 150,10
/SIZE/ 120,70
/TYPE/ UD_DASSTRING

#LISTTABLE#
/LABEL/ Macro:
/POSITION/12,27
/SIZE/270,140
/TYPE/UD_DASSTRING
/DOUBLE_CLICK/ *ON

#CHECKBOX#      
/LABEL/ Modal Call
/POSITION/ 10,168
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#      
/LABEL/ Load
/POSITION/ 80,168
/SIZE/ 40,14
/TYPE/ UD_DASINT

#PUSHBUTTON#      
/LABEL/ View
/POSITION/ 130,168
/SIZE/ 40,14
/TYPE/ UD_DASINT

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/abcd.mp4
/POSITION/ 180,168
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
==================
Dynamic Macro Call
==================
This form allows you to interactively call a Macro utilizing a form that
contains fields for all of the Macro parameters.

Filter
------
Pressing the Filter button will list only the Macros that match the specified
filter mask.  Asterisks (*) can be used as part of the filter mask.

Macro Class
-----------
You can choose to list all defined Macros (All) or only those that belong to
a specific Macro class.  The 'PROMPT/macro,class' command is used to define
the Macro class.

Macro
-----
Select a Macro from this list and press OK to bring up the form used to input
the Macro parameters and call it.

Modal Call
----------
When this box is checked, then the Macro form will be redisplayed so that it
can be called again with new parameters after initially calling the Macro.
Unchecking this box causes NCL to return to its previous state after calling
the Macro.

Load
----
Pressing the Load button will bring up a file browser that allows you to load
an external Macro.  Once the Macro file is loaded, NCL will automatically
process it so that any defined Macros in the file will be made available for
selection.

View
----
Pressing the View button will cause a pocket window to be displayed with a
pictorial representation of the Macro if the selected Macro from the list has
an associated graphic file.  Valid graphic files are NCL drawings (macro.dw),
JPEG files (macro.jpg), and GIF files (macro.gif).  The filename is the name
of the Macro (in capital letters) with the appropriate file extension.  NCL
running on Unix machines only support NCL drawing files.

Macro Call Form
===============
Once a Macro is selected and the Dynamic Macro Call form is accepted, then the
Macro will either be called, if there are no parameters, or the Macro Call form
for this Macro will be displayed.  The Macro Call form will contain fields for
each of the Macro parameters and fields that control the actual Macro call.
These Macro independent fields are described below.

Place in Source
---------------
When this box is checked the Macro CALL statement will be placed in the part
program file.  Unchecking this box causes the Macro to be called without
placing the CALL statement in the part program.  The default for this field
can be set using the 'PROMPT/macro,...,OUT/OMIT' statement.

Remember Input Values
---------------------
When this box is checked NCL will remember the values input for the Macro
arguments and the next time this Macro is dynamically called, then these values
will be displayed as the default values in the form.  Normally the default
values as defined in the 'macro=MACRO' command are used.  The default for this
field can be set using the 'PROMPT/macro,...,DEFALT/RETAIN' command.

Output Default Values
---------------------
When this box is checked then the Macro CALL statement will contain all Macro
arguments, even if they are the same as the default values defined in the
'macro=MACRO' command.  Unchecking this box will cause the Macro CALL statement
to leave off any Macro arguments that are set to their default value.

View
----
This button acts the same as the View button in the Dynamic Macro Call form.
