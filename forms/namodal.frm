#HEADER#
/TITLE/ Geometry Naming Modals
/POSITION/ 0,0
/SIZE/ 180,284

#CHOICEBOX#
/LABEL/ AUTO NAME:
/POSITION/ 10,8,60,8
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No","Yes"

#CHOICEBOX#
/LABEL/ REDEFINE: 
/POSITION/ 10,23,60,25
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No","Yes"

#CHOICEBOX#
/LABEL/ POINTS:  
/POSITION/ 10,38,60,38
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,38
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ PNTVECS: 
/POSITION/ 10,53,60,53
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,53
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ LINES:   
/POSITION/ 10,68,60,68
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,68
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ VECTORS: 
/POSITION/ 10,83,60,83
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,83
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ PLANES:  
/POSITION/ 10,98,60,98
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,98
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ CIRCLES: 
/POSITION/ 10,113,60,113
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,113
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ CURVES:  
/POSITION/ 10,128,60,128
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,128
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ SURFS:   
/POSITION/ 10,143,60,143
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,143
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ SHAPES:  
/POSITION/ 10,158,60,158
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,158
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ MATRICES:
/POSITION/ 10,173,60,173
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,173
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ PATERNS: 
/POSITION/ 10,188,60,188
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,188
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ ANOTES: 
/POSITION/ 10,203,60,203
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,203
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ SYMBOLS: 
/POSITION/ 10,220,60,220
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,220
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ SOLIDS: 
/POSITION/ 10,237,60,237
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Prefix","Subscript"

#EDIT#
/LABEL/
/POSITION/ 115,237
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#HELP#
======================
Geometry Naming Modals
======================
This form controls the automatic naming of geometry created from the interface
and entered manually.

----------
AUTO NAME:
----------
Enabling the auto naming feature will cause NCL to aumotically add a label to
geometry created from the interface.  The label will be the associated prefix
and optional subscript assigned to the geometry type in this form.  When the
auto-naming feature is disabled, then you will be prompted for the label when
creating geometry from the interface.

---------
REDEFINE: 
---------
Enabling redefinitions will allow geometry that has already been defined to be
redefined.  When redefinitions are disabled, then an error message will be
displayed whenever an existing entity is attempted to be redefined.

---------------
Geometry Fields
---------------
The remaining fields in this form define the label prefixes and whether the 
geometry labels should include subscripts.  When subscripts are not output,
then the label should consist of 2 letters that are not a vocabulary word.
An ascending numeric suffix will be appended to the label prefix.

When subscripts are output with the labels, then a name other than the 2
letter prefix should be specified.
