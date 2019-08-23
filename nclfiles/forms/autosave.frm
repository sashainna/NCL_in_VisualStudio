#HEADER#
/TITLE/ NCL Autosave
/POSITION/ 0,0
/SIZE/ 150,185

#FRAME#
/TITLE/ Unibase
/POSITION/ 5,3
/SIZE/ 136,65

#FRAME#
/TITLE/ Part Program
/POSITION/ 5,73
/SIZE/ 136,84

#CHECKBOX#
/LABEL/ Auto Save
/POSITION/ 12,16
/SIZE/ 47,11
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Save Every
/POSITION/ 12,34,55,31
/SIZE/ 73,13
/TYPE/UD_SCAINT
/INPUT/FORM_STRING
/LEN/ 6

#CHOICEBOX#
/LABEL/ 
/POSITION/ 95,35,87,31
/SIZE/ 37,47
/TYPE/UD_DASSTRING
/CHOICES/ "Minutes","Changes"

#PUSHBUTTON#
/LABEL/ Snap Save
/POSITION/ 16,48
/SIZE/ 47,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Restore
/POSITION/ 79,48
/SIZE/ 47,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Auto Save
/POSITION/ 12,86
/SIZE/ 47,11
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Save Every
/POSITION/ 12,105,55,102
/SIZE/ 73,13
/TYPE/UD_SCAINT
/INPUT/FORM_STRING
/LEN/ 6

#CHOICEBOX#
/LABEL/ 
/POSITION/ 95,106,87,102
/SIZE/ 37,47
/TYPE/UD_DASSTRING
/CHOICES/ "Minutes","Changes"

#EDIT#
/LABEL/ Maximun Number of Saves
/POSITION/ 12,121,102,119
/SIZE/ 120,13
/TYPE/UD_SCAINT
/RANGE/1,100
/INPUT/FORM_STRING
/LEN/ 6

#PUSHBUTTON#
/LABEL/ Snap Save
/POSITION/ 16,136
/SIZE/ 47,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Restore
/POSITION/ 79,136
/SIZE/ 47,14
/TYPE/UD_DASSTRING

#HELP#
Auto Save
---------
When selected, NCL will automatically save the Unibase when the Autosave
condition has been met. Selecting this box also enables the Autosave condition
fields. The condition can be:

 - Minutes: Autosave the Unibase after the given number of minutes have
            passed.
 - Changes: Autosave the Unibase after the given number of changes have
            been made.

Snap Save
---------
Press this button to manually generate an Autosave Unibase file. Autosave
does not need to be enabled to perform a Snap Save.

Restore
-------
Press this button to restore the Autosave Unibase as the active model.

Auto Save
---------
When selected, NCL will automatically save the part program when the Autosave
condition has been met. Selecting this box also enables the Autosave condition
fields.  The condition can be:

 - Minutes: Autosave the part program after the given number of minutes have
            passed.
 - Changes: Autosave the part program after the given number of changes have
            been made.

Maximum Number of Saves
-----------------------
Enter the maximun number of Autosave part program files that can be saved in
the current NCL session. Once the maximum is reached, the oldest Autosave file
will be deleted and the new Autosave file will be created. Valid responses are
1 to 100.

Snap Save
---------
Press this button to manually generate an Autosave part program file.
Autosave does not need to be enabled to perform a Snap Save.

Restore
-------
Press this button to restore an Autosave part program as the active part
program.  A file browser will be displayed requesting the name (version)
of the saved part program file to load.
