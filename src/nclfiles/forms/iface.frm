#HEADER#
/TITLE/ Interface Modals
/POSITION/ 0,0
/SIZE/ 250,420

#FRAME#
/TITLE/ Menu
/POSITION/ 10,10
/SIZE/ 230,28

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
/SIZE/ 230,28

#CHOICEBOX#      
/LABEL/ Initial Directory:
/POSITION/ 15,56
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Local", "Saved"

#FRAME#
/TITLE/ Command Line
/POSITION/ 10,77
/SIZE/ 230,82

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

#COLOR#      
/LABEL/ Active Color:
/POSITION/ 15,140,65,140
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#COLOR#      
/LABEL/ Current Color:
/POSITION/ 120,140,170,140
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#FRAME#
/TITLE/ Forms
/POSITION/ 10,165
/SIZE/ 230,64

#CHOICEBOX#
/LABEL/ Auto Highlight:
/POSITION/ 15,177,70,177
/SIZE/ 90,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#EDIT#
/LABEL/ Font Style:
/POSITION/ 130,177, 180,177
/SIZE/ 100,14
/TYPE/ UD_DASSTRING
/PREC/ 20
/LEN/ 12

#CHOICEBOX#
/LABEL/ Form Font Size:
/POSITION/ 15,194,70,194
/SIZE/ 80,100
/TYPE/ UD_DASSTRING
/CHOICES/ "8", "10" "12","14","16","18", "20", "24","36", "48"

#CHOICEBOX#
/LABEL/ Help Font Size:
/POSITION/ 130,194,180,194
/SIZE/ 80,100
/TYPE/ UD_DASSTRING
/CHOICES/ "8", "10" "12","14","16","18", "20", "24","36", "48"

#CHOICEBOX#
/LABEL/ Show Picture Position:
/POSITION/ 15,211
/SIZE/ 120,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#FRAME#
/TITLE/ Status Window
/POSITION/ 10,236
/SIZE/ 230,32

#EDIT#
/LABEL/ Font Style:
/POSITION/ 15,248,65,248
/SIZE/ 110,14
/TYPE/ UD_DASSTRING
/PREC/ 20
/LEN/ 12

#CHOICEBOX#
/LABEL/ Font Size:
/POSITION/ 130,248,180,248
/SIZE/ 80,100
/TYPE/ UD_DASSTRING
/CHOICES/ "8", "10" "12","14","16","18", "20", "24","36", "48"

#FRAME#
/TITLE/ Prompt Area
/POSITION/ 10,273
/SIZE/ 230,32


#EDIT#
/LABEL/ Font Style:
/POSITION/ 15,285,65,285
/SIZE/ 110,14
/TYPE/ UD_DASSTRING
/PREC/ 20
/LEN/ 12

#CHOICEBOX#
/LABEL/ Font Size:
/POSITION/ 130,285,180,285
/SIZE/ 80,100
/TYPE/ UD_DASSTRING
/CHOICES/ "8", "10" "12","14","16","18", "20", "24","36", "48"

#FRAME#
/TITLE/ Error Area
/POSITION/ 10,308
/SIZE/ 230,32


#EDIT#
/LABEL/ Font Style:
/POSITION/ 15,320,65,320
/SIZE/ 110,14
/TYPE/ UD_DASSTRING
/PREC/ 20
/LEN/ 12

#CHOICEBOX#
/LABEL/ Font Size:
/POSITION/ 130,320,180,320
/SIZE/ 80,100
/TYPE/ UD_DASSTRING
/CHOICES/ "8", "10" "12","14","16","18", "20", "24","36", "48"

#FRAME#
/TITLE/ Miscellaneous
/POSITION/ 10,343
/SIZE/ 230,45

#CHOICEBOX#
/LABEL/ Live Mouse:
/POSITION/ 15,355,65,355
/SIZE/ 80,100
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#CHOICEBOX#
/LABEL/ Update Status Button:
/POSITION/ 15,372, 95,372
/SIZE/ 130,100
/TYPE/ UD_DASSTRING
/CHOICES/ "Changed", "Idle Mode"

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

Active Line:
------------
Select the color that the active line in the Command Line window will be
highlighted in when the Multi-line Command Line is active.  The active line is
defined as the line that should be processed next (the line after the last line
that was processed).

Current Line:
-------------
Select the color that the current line in the Command Line window will be
highlighted in when the Multi-line Command Line is active.  The current line is
defined as the line that the text cursor is positioned on.

=====
Forms
=====
The Forms section controls form actions and appearance, including the Form Help
window (such as the text you are currently viewing).

Auto Highlight:
---------------
Enabling the Auto Hightlight field will cause the Form Field text to be
automatically selected when the text field is first entered.  Typing a single
character into the Text Field when the text is selected/highlighted will
overwrite the entire text line.  Clicking in the Text Field with the mouse or
entering a left or right arrow key will unselect the text.

Disabling this field will not automatically highlight the Form Field text,
you will have to use normal operating system methods to manually highlight the
text.

Help Font Style:
----------------
Type in the font name you want to use for the Form Help text.  A known font
name should be used, if it is not a known font, then the default operating
system font will be used.  Typical fonts are Arial, Courier, Helvetica, and
Times.  This field is only valid for Windows platforms.

Help Font Size:
---------------
Select the font point size for the Form Help text.  The smaller the font size,
the smaller the text will appear.  Typical settings are 8, 10, or 12.  This
field is only valid for Windows platforms.

Show Picture Position:
----------------------
Selecting Off will cause the tooltip text to display when the cursor is within
a defined region of a form picture.  On will display the location of the cursor
within the picture, making it easier for the user to define the picture areas
referenced in the form fields.

=============
Status Window
=============
The Status Window section controls the font appearance in the Status Window.

Font Style:
-----------
Type in the font name you want to use for the Status Window text.  A known font
name should be used, if it is not a known font, then the default operating
system font will be used.  Typical fonts are Arial, Courier, Helvetica, and
Times.  This field is only valid for Windows platforms.

Font Size:
----------
Select the font point size for the Status Window text.  The smaller the font
size, the smaller the text will appear.  Typical settings are 8, 10, or 12.
This field is only valid for Windows platforms.

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

============
Micellaneous
============
The Miscellaneous section controls the handling various other interface
options that do not fit in the other categories.

Live Mouse:
-----------
This field enables or disables the usage of the Live Mouse.  When the Live
Mouse is active then geometry will automatically be highlighted when the
cursor is placed over it and the mouse buttons can be programmed to activate
certain functions as defined in the UZ_MOUSE_BUTDEFS mouse definition file.

When the Live Mouse is not active then while in choice mode, the geometry will
not be highlighted and pressing the mouse buttons will not perform any action.
This mode is convenient when there is so much geometry on the screen that it
causes NCL to slow down whenever the mouse is moved.

Update Status Button:
---------------------
The Status Buttons that are displayed in Status Menus and the Bottom Status Bar
can be updated whenever they change during *RUN mode or only when the system is
idle waiting for input.  It is advantageous to see some of the Status items
whenever they change, such as the Processing Mode, but when running large
programs this can noticably slow down the processing time.

Select 'Changed' to update the Status Menu text whenever it changes or
'Idle Mode' only update the Status Menu text when the system is idle waiting
for input.

