#HEADER#
/TITLE/ Dimension Text Attributes
/POSITION/ 50,50
/SIZE/175,275

#COLOR#
/LABEL/ Character Color:
/POSITION/ 10,8,85,8
/SIZE/ 130,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/Character Size:
/POSITION/ 10,23,85,23
/SIZE/150,14
/TYPE/ UD_DASVAL
/PREC/ 5
/LEN/ -8

#EDIT#
/LABEL/Text Angle:
/POSITION/ 10,38,85,38
/SIZE/150,14
/TYPE/ UD_DASANGLE
/PREC/ 3
/LEN/ -8

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
/LABEL/ Text Placement:
/POSITION/ 10,68,85,68
/SIZE/150,40
/TYPE/UD_DASSTRING
/CHOICES/ "auto placement","manual placement"

#CHOICEBOX#
/LABEL/ Text Site:
/POSITION/ 10,83,85,83
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "top left","middle left","bottom left","top center", "middle center"
/CHOICES/ "bottom center","top right","middle right","bottom right"

#CHOICEBOX#
/LABEL/ Text Entry:
/POSITION/ 10,98,85,98
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "system text","user text"

#CHOICEBOX#
/LABEL/ Appended Text:
/POSITION/ 10,113,85,113
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "append above","append below","append before","append after", "none"

#CHOICEBOX#
/LABEL/ Text Orientation:
/POSITION/ 10,128,85,128
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "horizontal","aligned","over dim line","under dim line"

#CHOICEBOX#
/LABEL/ Text Justification:
/POSITION/ 10,143,85,143
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "left justify","right justify","center justify"

#EDIT#
/LABEL/Grid Distance:
/POSITION/ 10,158,85,158
/SIZE/150,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ -8

#EDIT#
/LABEL/Character Expansion:
/POSITION/ 10,173,85,173
/SIZE/150,14
/TYPE/ UD_DASUNITLESS
/PREC/ 2
/LEN/ -5

#EDIT#
/LABEL/Character Spacing:
/POSITION/ 10,188,85,188
/SIZE/150,14
/TYPE/ UD_DASUNITLESS
/PREC/ 2
/LEN/ -5

#CHOICEBOX#
/LABEL/ Character Density:
/POSITION/ 10,203,85,203
/SIZE/130,40
/TYPE/UD_DASSTRING
/CHOICES/ "standard","medium","heavy", "extra heavy"

#EDIT#
/LABEL/Sub-Superscript Size:
/POSITION/ 10,218,85,218
/SIZE/150,14
/TYPE/ UD_DASUNITLESS
/PREC/ 2
/LEN/ -4

#EDIT#
/LABEL/Line Spacing:
/POSITION/ 10,233,85,233
/SIZE/150,14
/TYPE/ UD_DASUNITLESS
/PREC/ 2
/LEN/ -6
