#HEADER#
/TITLE/ Save Solids to Stock File
/POSITION/ 50,30
/SIZE/200,115

#FRAME#
/TITLE/ Solids
/POSITION/ 8,10
/SIZE/ 180,32

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 15,22
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 55,22,65,22
/SIZE/65,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 125,22
/SIZE/50,15
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ File:
/POSITION/ 10,50
/SIZE/30,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 45,50
/SIZE/135,14
/TYPE/ UD_DASSTRING
/PREC/ 32
/LEN/ 32

#CHOICEBOX#
/LABEL/ Type:
/POSITION/ 10,70
/SIZE/65,40
/TYPE/UD_DASSTRING
/CHOICES/ "Stock","Fixture"


#HELP#
===========================
Save Solids to Stock File
===========================
This form is used to save previously defined solids to an external NCLIPV stock
file.

Select
------
Used to select the solids to save to the external file.

Color
-----
Chooses a color to highlight the selected solids.

Deselect All
------------
Deselects all of the currently selected solids.

File:
-----
Specifies the stock file to save.  Pressing the File button will display a
file browser that can be used to select the file.

Type:
-----
Solids can be saved as NCLIPV Stocks or Fixtures.

