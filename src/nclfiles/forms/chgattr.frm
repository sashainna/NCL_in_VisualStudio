#HEADER#
/TITLE/ Model Attributes
/POSITION/ 0,0
/SIZE/ 180,195

#CHECKBOX#      
/LABEL/ Change:
/POSITION/ 10,8
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Layer number:
/POSITION/ 65,8,115,8
/SIZE/ 105,14
/TYPE/ UD_DASINT
/RANGE/0,9999
/PREC/ 4
/LEN/ 4

#CHECKBOX#      
/LABEL/ Change:
/POSITION/ 10,25
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/ Color:
/POSITION/ 65,25,115,25
/SIZE/ 105,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHECKBOX#      
/LABEL/ Change:
/POSITION/ 10,42
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#      
/LABEL/ Line Style:
/POSITION/ 65,42,115,42
/SIZE/ 105,40
/TYPE/ UD_DASSTRING
/CHOICES/ "solid", "small dash", "dots", "center line", "phantom"
/CHOICES/ "large dash", "dash dot", "dash space"

#CHECKBOX#      
/LABEL/ Change:
/POSITION/ 10,59
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#      
/LABEL/ Line Weight:
/POSITION/ 65,59,115,59
/SIZE/ 105,40
/TYPE/ UD_DASSTRING
/CHOICES/ "standard", "medium", "heavy", "extra heavy"

#CHECKBOX#      
/LABEL/ Change:
/POSITION/ 10,76
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Logical Pen:
/POSITION/ 65,76,115,76
/SIZE/ 105,14
/TYPE/ UD_DASINT
/RANGE/1,256
/PREC/ 3
/LEN/ 3

#CHECKBOX#      
/LABEL/ Change:
/POSITION/ 10,93
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Marker Type:
/POSITION/ 66,95
/SIZE/ 105,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Dot", "Plus", "Asterisk", "Circle"
/CHOICES/ "Cross", "Triangle", "Diamond", "Square"
/CHOICES/ "Dbl. Circle", "Large Dot", "Cube"

#CHECKBOX#      
/LABEL/ Output DRAFT/MODIFY command
/POSITION/ 10,110
/SIZE/ 160,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/ Edit Sufrace Attributes
/POSITION/ 10,127
/SIZE/ 85,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Surface Attributes
/POSITION/ 97,127
/SIZE/ 70,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Extract Attributes
/POSITION/ 97,147
/SIZE/ 70,14
/TYPE/ UD_DASSTRING

#HELP#
================
Model Attributes
================
This form is used to modify entity attributes.

Layer
-----
Defines the layer that the geometric entity resides on.

Color
-----
The entity is displayed using the color defined here.

Line Style
----------
Defines the line style (solid, dashed, etc.) to display the entity with.

Line Weight
-----------
Defines the line weight (standard, heavy, etc.) to display the entity with.

Pen
---
Defines the logical pen used for the entity when a plot is created.

Marker Type
-----------
Defines the marker type used for the entity when a point or pattern is
created.

Output DRAFT/MODIFY command
---------------------------
When this box is checked a DRAFT/MODIFY command will be output to the part
program containing the attribute settings that have changed when the Apply or
OK button is pressed.  Unchecking this box will update the attributes without
outputting a command.

Edit Surface Attributes
-----------------------
When this box is checked the optional surface attributes form is available.
This box must be checked for any surface attribute changes to be made or
applied.

Surface Attributes
------------------
Pressing this button brings up the form for editing surface attributes.
The edge display, edge color, shading and transparency can all be modified
using the Surface Attributes form.

Extract Attributes
------------------
Pressing this button allows the attributes of a picked entity to be used to
fill the settings fields.  The color, layer, pen, line style and line weight
will be extracted from all entities.  Further, surface attibutes can be
extracted from a surface or solid. The marker type of a point or pattern can
be extracted as well.
