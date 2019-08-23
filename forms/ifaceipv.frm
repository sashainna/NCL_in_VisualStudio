#HEADER#
/TITLE/ Interface Modals
/POSITION/ 0,0
/SIZE/ 230,322

#FRAME#
/TITLE/ Menu
/POSITION/ 10,10
/SIZE/ 210,28

#CHOICEBOX#      
/LABEL/ Auto Cursor:
/POSITION/ 15,20,65,20
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#CHOICEBOX#
/LABEL/ Icon Size:
/POSITION/ 130,20,165,20
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "16x16", "24x24", "32x32", "40x40", "48x48"

#FRAME#
/TITLE/ File Browser
/POSITION/ 10,45
/SIZE/ 210,28

#CHOICEBOX#      
/LABEL/ Initial Directory:
/POSITION/ 15,56
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Local", "Saved"

#FRAME#
/TITLE/ Command Line
/POSITION/ 10,77
/SIZE/ 210,65

#CHOICEBOX#
/LABEL/ Text Cursor:
/POSITION/ 15,89,65,89
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Begin","End"

#CHOICEBOX#
/LABEL/ Auto Highlight:
/POSITION/ 130,89,180,89
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#CHOICEBOX#
/LABEL/ Keypad:
/POSITION/ 15,106,65,106
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Numeric","Functions"

#EDIT#
/LABEL/ Font Style:
/POSITION/ 15,123,65,123
/SIZE/ 100,14
/TYPE/ UD_DASSTRING
/PREC/ 20
/LEN/ 10
/FONT/ 1.0

#CHOICEBOX#
/LABEL/ Font Size:
/POSITION/ 130,123,180,123
/SIZE/ 80,100
/TYPE/ UD_DASSTRING
/CHOICES/ "8", "10" "12","14","16","18", "20", "24","36", "48"


#FRAME#
/TITLE/ Form Help
/POSITION/ 10,148
/SIZE/ 210,64

#CHOICEBOX#
/LABEL/ Auto Highlight:
/POSITION/ 15,160,65,160
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#EDIT#
/LABEL/ Font Style:
/POSITION/ 15,177,65,177
/SIZE/ 100,14
/TYPE/ UD_DASSTRING
/PREC/ 20
/LEN/ 12

#CHOICEBOX#
/LABEL/ Font Size:
/POSITION/ 130,177,180,177
/SIZE/ 80,100
/TYPE/ UD_DASSTRING
/CHOICES/ "8", "10" "12","14","16","18", "20", "24","36", "48"

#CHOICEBOX#
/LABEL/ Show Picture Position:
/POSITION/ 15,194
/SIZE/ 120,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#FRAME#
/TITLE/ Prompt Area
/POSITION/ 10,219
/SIZE/ 210,32

#EDIT#
/LABEL/ Font Style:
/POSITION/ 15,231,65,231
/SIZE/ 110,14
/TYPE/ UD_DASSTRING
/PREC/ 20
/LEN/ 12

#CHOICEBOX#
/LABEL/ Font Size:
/POSITION/ 130,231,180,231
/SIZE/ 80,100
/TYPE/ UD_DASSTRING
/CHOICES/ "8", "10" "12","14","16","18", "20", "24","36", "48"

#FRAME#
/TITLE/ Error Area
/POSITION/ 10,256
/SIZE/ 210,32

#EDIT#
/LABEL/ Font Style:
/POSITION/ 15,266,65,266
/SIZE/ 110,14
/TYPE/ UD_DASSTRING
/PREC/ 20
/LEN/ 12

#CHOICEBOX#
/LABEL/ Font Size:
/POSITION/ 130,266,180,266
/SIZE/ 80,100
/TYPE/ UD_DASSTRING
/CHOICES/ "8", "10" "12","14","16","18", "20", "24","36", "48"

#HELP#
=====================
Interface Modals Form
=====================
This form controls the Menu display attributes, Command Line settings, and
various interface font settings.  These settings are saved in the
'ncl_interface.mod' file and will be used as the default values the next time
NCL is started.

====
Menu
====
The Menu section controls the appearance of the displayed menus.

Auto Cursor:
------------
When Auto Cursor is enabled, then the cursor will automatically be positioned
at the menu that is activated.  When Auto Cursor is disabled, then the cursor
will remain where it was when the new menu is activated.

Icon Size:
----------
Determines the displayed size of the menu bitmaps on permanent menus.  Pulldown
menus are displayed with a bitmap size of 16x16 when the Icon Size is set to
32x32 or lower and with a bitmap size of 24x24 when the Icon Size is set larger
than 32x32.  This field is only valid for Windows platforms.

Menu Format:
------------
You have the option of displaying menus with Icons only, Text only, or with
both Icons and Text.  Menus that do not contain a defined bitmap will always
be displayed as Text only.  This field is only valid for Windows platforms.

============
File Browser
============
The File Browser section controls the default settings of the NCL file browser.

Initial Directory
-----------------
This field determines the initial directory displayed when a file browser is
initiated.  Either the local directory (Local) that NCL is currently running
in can be displayed or the last directory accessed (Saved) by an NCL file
browser can be displayed.

============
Command Line
============
The Command Line section controls the handling of the text cursor, keypad
mode, and the font appearance in the Command Line.

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

Disabling this field will not automatically highlight the Command Line text,
you will have to use normal operating system methods to manually highlight the
text.

Keypad:
-------
This field determines how the Numeric Keypad will behave while in command mode.
Selecting Numeric allows you to enter numbers using the Numeric Keypad.
Selecting Functions will disable the Numeric Keypad for numeric input and
instead use it to call NCL functions that have been assigned to its keys.

Font Style:
-----------
Type in the font name you want to use for the Command Line text.  A known font
name should be used, if it is not a known font, then the default operating
system font will be used.  Typical fonts are Arial, Courier, Helvetica, and
Times.  This field is only valid for Windows platforms.

Font Size:
----------
Select the font point size for the Command Line text.  The smaller the font
size, the smaller the text will appear.  Typical settings are 8, 10, or 12.
This field is only valid for Windows platforms.

=========
Form Help
=========
The Form Help section controls the font appearance in the Form Help window
(such as the text you are currently viewing).

Font Style:
-----------
Type in the font name you want to use for the Form Help text.  A known font
name should be used, if it is not a known font, then the default operating
system font will be used.  Typical fonts are Arial, Courier, Helvetica, and
Times.  This field is only valid for Windows platforms.

Font Size:
----------
Select the font point size for the Form Help text.  The smaller the font size,
the smaller the text will appear.  Typical settings are 8, 10, or 12.  This
field is only valid for Windows platforms.

===========
Prompt Area
===========
The Prompt Area section controls the font appearance in the prompt label

Font Style:
-----------
Type in the font name you want to use for the Prompt Area label.  A known font
name should be used, if it is not a known font, then the default operating
system font will be used.  Typical fonts are Arial, Courier, Helvetica, and
Times.  This field is only valid for Windows platforms.

Font Size:
----------
Select the font point size for the Prompt Area label.  The smaller the font
size, the smaller the text will appear.  Typical settings are 8, 10, or 12.
This field is only valid for Windows platforms.

==========
Error Area
==========
The Error Area section controls the font appearance in the error label

Font Style:
-----------
Type in the font name you want to use for the error area label.  A known font
name should be used, if it is not a known font, then the default operating
system font will be used.  Typical fonts are Arial, Courier, Helvetica, and
Times.  This field is only valid for Windows platforms.

Font Size:
----------
Select the font point size for the error area label.  The smaller the font size,
the smaller the text will appear.  Typical settings are 8, 10, or 12.  This
field is only valid for Windows platforms.
