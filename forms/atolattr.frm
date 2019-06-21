#HEADER#
/TITLE/ Tolerance Attributes 
/POSITION/ 50,50
/SIZE/195,280

#EDIT#
/LABEL/ Tol Decimal Places:
/POSITION/ 10,8,90,8
/SIZE/150,14
/TYPE/ UD_DASINT
/PREC/ 0
/LEN/ -1

#EDIT#
/LABEL/Dual Tol Dec Places:
/POSITION/ 10,23,90,23
/SIZE/150,14
/TYPE/ UD_DASINT
/PREC/ 0
/LEN/ -1

#CHOICEBOX#
/LABEL/ Tolerance Method:
/POSITION/ 10,38,90,38
/SIZE/175,40
/TYPE/UD_DASSTRING
/CHOICES/ "no tolerance/limit","limit one line","limit two lines","limit larger first"
/CHOICES/ "limit larger below","bilateral one line","bilateral two lines","unilateral above"
/CHOICES/ "unilateral below","unilateral above one line","unilateral below one line"

#CHOICEBOX#
/LABEL/ Tolerance Site:
/POSITION/ 10,53,90,53
/SIZE/175,40
/TYPE/UD_DASSTRING
/CHOICES/ "above dimension","below dimension","after dim centered","after aligned top", "after aligned bottom"

#EDIT#
/LABEL/Linear Upper Tol:
/POSITION/ 10,68,90,68
/SIZE/150,14
/TYPE/ UD_DASVAL
/PREC/ 5
/LEN/ -8

#EDIT#
/LABEL/Linear Lower Tol:
/POSITION/ 10,83,90,83
/SIZE/150,14
/TYPE/ UD_DASVAL
/PREC/ 5
/LEN/ -8

#EDIT#
/LABEL/Dual Linear Upper Tol:
/POSITION/ 10,98,90,98
/SIZE/150,14
/TYPE/ UD_DASVAL
/PREC/ 5
/LEN/ -8

#EDIT#
/LABEL/Dual Linear Lower Tol:
/POSITION/ 10,113,90,113
/SIZE/150,14
/TYPE/ UD_DASVAL
/PREC/ 5
/LEN/ -8

#EDIT#
/LABEL/Angular Upper Tol:
/POSITION/ 10,128, 90,128
/SIZE/150,14
/TYPE/ UD_DASVAL
/PREC/ 5
/LEN/ -8

#EDIT#
/LABEL/Angular Lower Tol:
/POSITION/ 10,143,90,143
/SIZE/150,14
/TYPE/ UD_DASVAL
/PREC/ 5
/LEN/ -8

#CHOICEBOX#
/LABEL/ Tol Zero Suppression:
/POSITION/ 10,158,90,158
/SIZE/175,40
/TYPE/UD_DASSTRING
/CHOICES/ "suppress none","suppress before","suppress after","suppress both"

#CHOICEBOX#
/LABEL/ Dual Tol Zero Supp:
/POSITION/ 10,173,90,173
/SIZE/175,40
/TYPE/UD_DASSTRING
/CHOICES/ "suppress none","suppress before","suppress after","suppress both"

#CHOICEBOX#
/LABEL/ Tol Round-Off:
/POSITION/ 10,188,90,188
/SIZE/175,40
/TYPE/UD_DASSTRING
/CHOICES/ "truncate","round up","round down","round to nearest"

#CHOICEBOX#
/LABEL/ Dual Tol Round-Off:
/POSITION/ 10,203,90,203
/SIZE/175,40
/TYPE/UD_DASSTRING
/CHOICES/ "truncate","round up","round down","round to nearest"

#EDIT#
/LABEL/Tol Round Factor:
/POSITION/ 10,218,90,218
/SIZE/150,14
/TYPE/ UD_DASVAL
/PREC/ 5
/LEN/ -8

#EDIT#
/LABEL/Dual Tol Round Factor:
/POSITION/ 10,233,90,233
/SIZE/150,14
/TYPE/ UD_DASVAL
/PREC/ 5
/LEN/ -8
