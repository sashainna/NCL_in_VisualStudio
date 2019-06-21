#HEADER#
/TITLE/ Note Text Attributes
/POSITION/ 50,50
/SIZE/170,250

#COLOR#      
/LABEL/ Character Color:
/POSITION/ 10,8,85,8
/SIZE/ 130,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/Character Height:
/POSITION/ 10,24,85,24
/SIZE/150,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ -8

#EDIT#
/LABEL/Text Angle:
/POSITION/ 10,38,85,38
/SIZE/150,14
/TYPE/ UD_DASANGLE
/PREC/ 3
/LEN/ -12

#PUSHBUTTON#
/LABEL/Text Font:
/POSITION/ 10,53
/SIZE/50,14
/TYPE/ UD_DASSTRING

#CHOICE_LIST#
/LABEL/
/POSITION/ 85,53
/SIZE/55,70
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Text Precision:
/POSITION/ 10,68,85,68
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "String","Char","Stroke"

#CHOICEBOX#
/LABEL/ Text Site:
/POSITION/ 10,83,85,83
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "Top Left","Middle Left","Bottom Left","Top Center", "Middle Center"
/CHOICES/ "Bottom Center","Top Right","Middle Right","Bottom Right"

#CHOICEBOX#
/LABEL/ Text Path:
/POSITION/ 10,98,85,98
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "Right","Left","Up","Down"

#CHOICEBOX#
/LABEL/ Horizontal Alignment:
/POSITION/ 10,113,85,113
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "Normal","Left","Center","Right"

#CHOICEBOX#
/LABEL/ Vertical Alignment:
/POSITION/ 10,128,85,128
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "Normal","Top","Half","Base","Bottom"

#EDIT#
/LABEL/Character Expansion:
/POSITION/ 10,143,85,143
/SIZE/150,14
/TYPE/ UD_DASUNITLESS
/PREC/ 2
/LEN/ -5

#EDIT#
/LABEL/Character Spacing:
/POSITION/ 10,158,85,158
/SIZE/150,14
/TYPE/ UD_DASUNITLESS
/PREC/ 2
/LEN/ -5

#CHOICEBOX#
/LABEL/ Character Density:
/POSITION/ 10,173,85,173
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "Standard","Medium","Heavy", "Extra Heavy"

#EDIT#
/LABEL/Sub-Superscript Size:
/POSITION/ 10,188,85,188
/SIZE/150,14
/TYPE/ UD_DASUNITLESS
/PREC/ 2
/LEN/ -4

#EDIT#
/LABEL/Line Spacing:
/POSITION/ 10,203,85,203
/SIZE/150,14
/TYPE/ UD_DASUNITLESS
/PREC/ 2
/LEN/ -6

#HELP#
=========================
Note Text Attributes Form
=========================
This form is used to change the default annotation attributes and to modify an
existing annotation attributes.

Character Color
---------------
Defines the color of the annotation text.

Character Height
----------------
Specifies the character height.  The character width will be automatically
adjusted using its predefined ratio to the character height.

Text Angle
----------
Specifies the angle in degrees at which the annotation text is displayed.  A
positive value will rotate the text in the clockwise direction.

Text Font
---------
This is a toggle field that displays all defined fonts that can be used for
annotation.  Select the name of the font to use to display stroked text.

Text Precision
--------------
Either hardware or stroked (hershey) text can be displayed for annotation.
Select either "String" for hardware text or "Stroke" for stroked text.  "Char"
currently is treated the same as hardware text.

Hardware text is displayed using a graphics font and is the same as the style
used when displaying geometry labels.  When the view is rotated, hardware text
will remain displayed in the XY-plane of the graphics screen.

Stroked text is actually drawn using a set of polylines and can be considered
3-dimensional text as it will rotate with the view, can be projected onto
surfaces, and can be used for engraving.


Text Site
---------
Controls where the text is located.  Imagine an invisible box enclosing all text
associated with the annotation.  This toggle field determines where on this box
will be attached to the origin location of the annotation.  The following
diagram displays the available positions.

            Top Left----------------Top Center-----------------Top Right
            |                                                          |
            |                                                          |
            Middle Left            Middle Center            Middle Right
            |                                                          |
            |                                                          |
            Bottom Left------------Bottom Center------------Bottom Right


Text Path
---------
Defines the text path of the annotation.  The path direction can be Right, Left
Up, or Down.

Horizontal Alignment
--------------------
Specifies the horizontal alignment of the annotation and can be Normal, Left,
Right, or Center.  Normal selects the proper alignment based on the text path.
The horizontal alignment is dependant on the attach point.

Vertical Alignment
------------------
Specifies the vertical alignment of the annotation and can be Normal, Top,
Half, Base, or Bottom.  Normal selects the proper alignment based on the text
path.  Base offsets the vertical text position by the size of the descending
portion of the font.  The vertical alignment is dependant on the attach point.

Character Expansion
-------------------
The character expansion is a ratio of the character width to the character
height.  A value of 1.0 defines a width that is directly proportional to the
character height.

Character Spacing
-----------------
Defines an extra space between characters to add.  For historical fonts (ansi,
duplex, italic, etc.) this value is typically .02.  For the newer fonts (romans,
scriptc, timesi, etc.) this value is typically 0.


Character Density
-----------------
Specifies the character density (line weight) of the annotation.

Sub-Superscript Size
--------------------
The subscript/superscript size is specified as a percentage of the character
height of the annotaion and is usually smaller (less than 1.) than the character
height.

Line Spacing
------------
Defines the vertical spacing between lines of the annotation text.
