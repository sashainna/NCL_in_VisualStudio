#HEADER#
/TITLE/ Create STL File
/POSITION/ 50,30
/SIZE/200,115

#FRAME#
/TITLE/ Geometry
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
/LABEL/ Format:
/POSITION/ 10,70
/SIZE/65,40
/TYPE/UD_DASSTRING
/CHOICES/ "Ascii","Binary"

#EDIT#
/LABEL/ Tolerance:
/POSITION/ 85,70
/SIZE/80,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ -6
/RANGE/ .00001,.5


#HELP#
===============
Create STL File
===============
This form creates an external STL file using selected solids and surfaces.

Select
------
Used to select the solids/surfaces to save to the external STL file.  STL files
typically only contain a single closed solid, so it is recommended that only
one solid be stored per STL file, but if the STL model is only going to be used
for visualization purposes it is possible to store multiple solids and surfaces
in a single file.

Color
-----
Chooses a color to highlight the selected solids/surfaces.

Deselect All
------------
Deselects all of the currently selected solids/surfaces.

File:
-----
Specifies the STL file to save.  Pressing the File button will display a
file browser that can be used to select the file.

Format:
-------
Specifies the format of the STL file saved, either ASCII or Binary.  The format
of external STL files that are loaded into NCL will automatically be determined.

Tolerance:
----------
This field sets the tolerance to use in analyzing the geometry stored in the
external STL file.
