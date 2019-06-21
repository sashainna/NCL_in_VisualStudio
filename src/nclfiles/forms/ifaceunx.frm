#HEADER#
/TITLE/ Interface Modals
/POSITION/ 0,0
/SIZE/ 230,115

#FRAME#
/TITLE/ Menu
/POSITION/ 10,10
/SIZE/ 210,30

#CHOICEBOX#      
/LABEL/ Auto Cursor:
/POSITION/ 15,15,65,15
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#FRAME#
/TITLE/ Command Line
/POSITION/ 10,45
/SIZE/ 210,45

#CHOICEBOX#
/LABEL/ Text Cursor:
/POSITION/ 15,50,65,50
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Begin","End"

#CHOICEBOX#
/LABEL/ Auto Highlight:
/POSITION/ 120,50,180,50
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#CHOICEBOX#
/LABEL/ Keypad:
/POSITION/ 15,72,65,72
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Numeric","Functions"

#HELP#
=====================
Interface Modals Form
=====================
This form controls the Menu display attributes and Command Line settings.  These
settings are saved in the 'ncl_interface.mod' file and will be used as the
default values the next time NCL is started.

====
Menu
====
The Menu section controls the appearance of the displayed menus.

Auto Cursor:
------------
When Auto Cursor is enabled, then the cursor will automatically be positioned
at the menu that is activated.  When Auto Cursor is disabled, then the cursor
will remain where it was when the new menu is activated.

============
Command Line
============
The Command Line section controls the handling of the text cursor, keypad mode,
and the font appearance in the Command Line.

Text Cursor:
------------
This field specifies whether the cursor will be positioned at the Beginning or
End of the Command Line when the Command Line is initiated.

Auto Highlight:
---------------
Enabling the Auto Hightlight field will cause the Command Line text to be
automatically selected when it is first displayed and the lines are scrolled
through.  Typing a single character into the Command Line when the text is
selected/highlighted will overwrite the entire text line.  Clicking in the
Command Line with the mouse or entering a left or right arrow key will 
unselect the text.

Disabling this field will not automatically highlight the command line text,
you will have to use normal operating system methods to manually highlight the
text.

Keypad:
-------
This field determines how the Numeric Keypad will behave while in command mode.
Selecting Numeric allows you to enter numbers using the Numeric Keypad.
Selecting Functions will disable the Numeric Keypad for numeric input and
instead use it to call NCL functions that have been assigned to its keys.
