#HEADER#
/TITLE/ Cross Hatch Attributes
/POSITION/ 50,50
/SIZE/140,140

#CHOICEBOX#      
/LABEL/ Material:
/POSITION/ 10,8,55,8
/SIZE/ 120,40
/TYPE/ UD_DASSTRING
/CHOICES/ "none", "iron", "steel","brass/copper", "rubber/plastic", "lead", "alum/mag"

#EDIT#
/LABEL/Angle:
/POSITION/ 10,25,55,25
/SIZE/150,14
/TYPE/ UD_DASANGLE
/LEN/ 10
/PREC/ 2
/RANGE/ -3.141593,3.141593

#EDIT#
/LABEL/Scale:
/POSITION/ 10,42,55,42
/SIZE/150,14
/TYPE/ UD_DASUNITLESS
/LEN/ 5
/PREC/ 2
/RANGE/ .01,10.0

#COLOR#      
/LABEL/ Color:
/POSITION/ 10,59,55,59
/SIZE/ 120,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#      
/LABEL/ Linestyle:
/POSITION/ 10,76,55,76
/SIZE/ 120,40
/TYPE/ UD_DASSTRING
/CHOICES/ "solid", "dashed", "center","phantom"

#EDIT#
/LABEL/Logical Pen:
/POSITION/ 10,93,55,93
/SIZE/150,14
/TYPE/ UD_DASINT
/RANGE/ 1,256
/PREC/ 0
/LEN/ 3
