#HEADER#
/TITLE/ Arrowhead Attributes
/POSITION/ 0,0
/SIZE/ 170,125

#CHOICEBOX#      
/LABEL/ Arrowhead Symbol:
/POSITION/ 10,8,85,8
/SIZE/ 140,40
/TYPE/ UD_DASSTRING
/CHOICES/ "single closed", "single open", "node", "point", "tilde", "single filled"
/CHOICES/ "double closed", "arch cross", "reversed closed", "arc back filled"

#EDIT#
/LABEL/ Arrowhead Size:
/POSITION/ 10,25,85,25
/SIZE/ 120,14
/TYPE/ UD_DASUNITLESS
/LEN/ -7
/PREC/ 4

#COLOR#      
/LABEL/ Arrowhead Color:
/POSITION/ 10,42,85,42
/SIZE/ 140,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#      
/LABEL/ Arrowhead Density::
/POSITION/ 10,59,85,59
/SIZE/ 140,40
/TYPE/ UD_DASSTRING
/CHOICES/ "standard", "medium", "heavy", "extra heavy"

#CHOICEBOX#      
/LABEL/ Arrowhead Placement:
/POSITION/ 10,76,85,76
/SIZE/ 140,40
/TYPE/ UD_DASSTRING
/CHOICES/ "inside ext line", "outside ext line"
