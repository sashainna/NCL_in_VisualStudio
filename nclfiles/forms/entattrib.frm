#HEADER#
/TITLE/ Entity Attributes
/POSITION/ 0,0
/SIZE/ 290,175

#DISPLAY#
/LABEL/ Label:
/POSITION/ 10,8
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/LEN/ 56
/PREC/ 64

#DISPLAY#
/LABEL/ Key:
/POSITION/ 10,25
/SIZE/ 60,14
/TYPE/ UD_DASINT
/LEN/ 8
/PREC/ 0

#DISPLAY#
/LABEL/ Relation:
/POSITION/ 80,25
/SIZE/ 90,14
/TYPE/ UD_DASSTRING
/LEN/ 12
/PREC/ 30

#PUSHBUTTON#
/LABEL/ Extract Attributes
/POSITION/ 193,25
/SIZE/ 70,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/ Color:
/POSITION/ 10,42
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ Layer:
/POSITION/ 95,42
/SIZE/ 70,14
/TYPE/ UD_DASINT
/RANGE/ 0,9999
/PREC/ 4
/LEN/ 4

#EDIT#
/LABEL/ Pen:
/POSITION/ 145,42
/SIZE/ 70,14
/TYPE/ UD_DASINT
/RANGE/ 0,256
/PREC/ 4
/LEN/ 4

#CHOICEBOX#
/LABEL/ Line Style:
/POSITION/ 10,59
/SIZE/ 105,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Solid", "Small Dash", "Dots", "Center Line", "Phantom"
/CHOICES/ "Large Dash", "Dash Dot", "Dash Space"

#CHOICEBOX#
/LABEL/ Line Weight:
/POSITION/ 130,59
/SIZE/ 105,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Standard", "Medium", "Heavy", "Extra Heavy"

#FRAME#
/TITLE/ Extended Attributes
/POSITION/ 9,81
/SIZE/ 267,45

#CHECKBOX#
/LABEL/ Display Edges
/POSITION/ 14,90,59,87
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/ Edge Color:
/POSITION/ 81,93,126,90
/SIZE/ 110,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Shaded:
/POSITION/ 14,110,45,108
/SIZE/ 65,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#EDIT#
/LABEL/ Translucency:
/POSITION/ 90,110,155,107
/SIZE/ 100,14
/TYPE/ UD_DASINT
/RANGE/1,100
/PREC/ 4
/LEN/ 4

#CHOICEBOX#
/LABEL/ Marker Type:
/POSITION/ 14,92
/SIZE/ 105,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Dot", "Plus", "Asterisk", "Circle"
/CHOICES/ "Cross", "Triangle", "Diamond", "Square"
/CHOICES/ "Dbl. Circle", "Large Dot", "Cube"

#CHECKBOX#
/LABEL/ Output DRAFT/MODIFY command
/POSITION/ 10,135
/SIZE/ 160,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 170,135
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 230,135
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#HELP#
=================
Entity Attributes
=================
This form displays the attributes for a geometric entity.

Label
-----
This is a read only field that displays the label of the geometric entity.

Key
---
This is a read only field that displays the Unibase key of the geometric entity.

Relation
--------
This is a read only field that displays the geometry type of the entity.
Defines the type of light being defined.

Extract Attributes
------------------
Pressing the Extract Attributes button allows the attributes of a picked
entity to be used to replace the attributes of the current entity.  The color,
layer, pen, line style and line weight will be extracted.  Further,
additional surface attibutes can be extracted from a surface or solid and
applied if the current entity is a surface or solid.  In addition, point or 
pattern marker types can also be extracted and assigned to the current entity.

Color
-----
The entity is displayed using the color defined here.

Layer
-----
Defines the layer that the geometric entity resides on.

Pen
---
Defines the logical pen used for the entity when a plot is created.

Line Style
----------
Defines the line style (solid, dashed, etc.) to display the entity with.

Line Weight
-----------
Defines the line weight (standard, heavy, etc.) to display the entity with.

Extended Attributes
-------------------
The extended attributes are accesible with solids, surfaces, points and 
patterns.  The edge display, edge color, shading and transparency can be 
modified.  Further, the marker type can be changed for points and patterns.

Output DRAFT/MODIFY command
---------------------------
When this box is checked a DRAFT/MODIFY command will be output to the part
program containing the attribute settings that have changed when the Apply or
OK button is pressed.  Unchecking this box will update the attributes of the
entity without outputting a command.

Apply
-----
Pressing the Apply button will assign any attributes that have been changed in
this form to the geometric entity without closing the form.

Select
------
Selects a new geometric entity display the attributes for.  If any changes were
made to the current entity's attributtes, they will NOT be set prior to
selecting a new entity, you must first press the Apply button and then the
Select button for these changes to take effect.
