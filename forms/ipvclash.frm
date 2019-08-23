#HEADER#
/TITLE/ NCLIPV Clash Detection
/POSITION/ 0,0
/SIZE/ 300,240

#FRAME#
/TITLE/ Clash
/POSITION/ 8,8
/SIZE/ 275,72

#LISTBOX#
/LABEL/
/POSITION/ 10,22
/SIZE/ 265,60
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Source
/POSITION/ 8,84
/SIZE/ 140,75

#LISTBOX#
/LABEL/
/POSITION/ 10,94
/SIZE/ 130,65
/TYPE/ UD_DASSTRING
/LEN/ -1

#FRAME#
/TITLE/ Call Stack
/POSITION/ 146,84
/SIZE/ 140,75

#LISTBOX#
/LABEL/
/POSITION/ 150,94
/SIZE/ 130,65
/TYPE/ UD_DASSTRING
/LEN/ -1

#CHECKBOX#
/LABEL/ Reset line number after edit
/POSITION/ 10,164
/SIZE/ 160,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Edit
/POSITION/ 180,164
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/
/POSITION/ 70,181
/SIZE/ 170,32

#PUSHBUTTON#
/Label/ View
/POSITION/ 75,191
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/Label/ Find
/POSITION/ 135,191
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/Label/ Settings
/POSITION/ 195,191
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#HELP#
===========================
NCLIPV Clash Detection Form
===========================
This form is displayed whenever a clash or post-processor error occurs during
an NCLIPV session.  The user then has the option to disable this type of clash,
continue the session, or stop the current session.

Clash
-----
This field displays the cause of the clash.  It is possible that multiple
clashes can be reported for a single cut.

Source
------
Displays the source code that generated the clash.  If the part program is not
loaded then no source will be displayed.

Call Stack
----------
Displays the active call/loop stack when the clash was generated. The call/loop
stack consists of the actual command that generated the clash and any Macro
calls or DO loops that were active at the time of the clash.

Macro calls will be displayed simply as CALL/macro and DO loops will be
displayed as DO/lable,index=n, where 'index' is the name of the controlling
DO loop variable and 'n' is its value at the time that the clash was generated.

Selecting a line from the Call Stack list will display the associated source
lines in the Source list.

Reset line number after edit
----------------------------
Checking this box causes NCL to reset the current source line number to the
line that was active prior to displaying the NCLIPV Clash Detection form.  If
this box is not checked, then the current source line will be the line that was
active when Command Mode was exited after pressing the Edit button.

Edit
----
Pressing the Edit button when the source code is displayed will enter Command
Mode at the line where the clash was generated.

View
----
Takes down the form and enters dynamic viewing.

Find
----
Pressing this button will change the view so that the clash is shown in the
center of the NCLIPV window.  Pressing the Find button multiple times will
toggle the view between the reported clashes if more than one clash is
reported.

Settings
--------
Press this field to bring up the NCLIPV Clash Settings form.  You can then
enable or disable clash detection between different types of machine components.

OK
--
Press the OK button if you would like to continue with the NCLIPV playback
session.

CANCEL
------
Press the CANCEL button if you would like to stop the NCLIPV playback session.
