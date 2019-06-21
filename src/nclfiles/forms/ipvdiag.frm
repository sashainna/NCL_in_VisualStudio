#HEADER#
/TITLE/ NCLIPV Log File
/POSITION/ 0,0
/SIZE/ 280,200

#PUSHBUTTON#
/LABEL/ Filename:
/POSITION/ 10,8
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 65,8
/SIZE/ 180,14
/TYPE/ UD_DASSTRING
/PREC/ 40
/LEN/ 40

#CHECKBOX#
/Label/ Reset Log File on New Session
/POSITION/ 10,25
/SIZE/ 120,14
/TYPE/ UD_DASSTRING

#LISTBOX#
/LABEL/
/POSITION/ 10,52
/SIZE/ 250,93
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Reset
/POSITION/ 115,147
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Log File
/POSITION/ 8,42
/SIZE/ 260,125

#HELP#
====================
NCLIPV Log File Form
====================
This form controls and displays the the output diagnostics file.

Filename:
---------
The Filename button will bring up a file browser that allows you to define the
name of the NCLIPV log file.  You can either use this browser or type in the
name of the file in the text field.

Reset Log File on New Session
-----------------------------
Check this box if you would like to have the log file emptied each time a new
NCLIPV session is started.  This will prevent the log file from accumulating an
extraordinary amount of data, if left unchecked for multiple NCLIPV sessions.

Log File
--------
This field displays the current contents of the NCLIPV log file.

Reset
-----
Pressing this button will delete the entire contents of the NCLIPV log file.
