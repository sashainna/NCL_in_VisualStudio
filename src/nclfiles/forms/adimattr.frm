#HEADER#
/TITLE/ Dimension Attributes
/POSITION/ 0,0
/SIZE/ 280,260

#CHOICEBOX#      
/LABEL/ Dimension Type:
/POSITION/ 10,8,72,8
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "normal", "basic"

#CHOICEBOX#      
/LABEL/ Dual Format:
/POSITION/ 145,8, 200, 8
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "no dual dim", "main over dual", "dual over main", "main/dual", "dual/main"
/CHOICES/ "main over [dual]", "[dual] over main", "main/[dual]", "[dual]/main"

#EDIT#
/LABEL/ Decimal Places:
/POSITION/ 10,25,72,25
/SIZE/ 125,14
/TYPE/ UD_DASINT
/LEN/ -1
/PREC/ 1

#EDIT#
/LABEL/ Decimal Symbol:
/POSITION/ 10,42,72, 42
/SIZE/ 125,14
/TYPE/ UD_DASSTRING
/LEN/ 1
/PREC/ 1

#CHOICEBOX#      
/LABEL/ Linear Units:
/POSITION/ 10,59,72,59
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "millimeters", "centimeters", "meters", "inches", "ft & in eng", "ft & in arch"

#CHOICEBOX#      
/LABEL/ Angular Units:
/POSITION/ 10,76,72,76
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "decimal deg", "deg min", "deg min sec", "radians"

#CHOICEBOX#      
/LABEL/ Zero Suppress:
/POSITION/ 10,93,72,93
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "none", "before", "after", "both"

#CHOICEBOX#      
/LABEL/ Dim Round-Off:
/POSITION/ 10,110,72,110
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "truncate", "round up", "round down", "round nearest"

#EDIT#
/LABEL/ Round-Off Factor:
/POSITION/ 10,127,72,127
/SIZE/ 125,14
/TYPE/ UD_DASVAL
/LEN/ -7
/PREC/ 4

#CHOICEBOX#      
/LABEL/ Fract Accuracy:
/POSITION/ 10,144,72, 144
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "1/2", "1/4", "1/8", "1/16", "1/32", "1/64"

#CHOICEBOX#      
/LABEL/ Fract Disp Mode:
/POSITION/ 10,161,72,161
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Standard", "Modified"

#CHOICEBOX#      
/LABEL/ Diameter Symbol:
/POSITION/ 10,178,72,178
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "no symbol", "DIA symbol", "user's symbol"

#EDIT#
/LABEL/ User Diam Symbol:
/POSITION/ 10,195,72,195
/SIZE/ 125,14
/TYPE/ UD_DASSTRING
/LEN/ 12
/PREC/ 12

#CHOICEBOX#      
/LABEL/ Symbol Placement:
/POSITION/ 10,212,72, 212
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "above dim", "below dim", "before dim", "after dim"

#EDIT#
/LABEL/ Dec Places:
/POSITION/ 145,25, 200, 25
/SIZE/ 125,14
/TYPE/ UD_DASINT
/LEN/ -1
/PREC/ 1

#EDIT#
/LABEL/ Dec Symbol:
/POSITION/ 145,42, 200, 42
/SIZE/ 125,14
/TYPE/ UD_DASSTRING
/LEN/ 1
/PREC/ 1

#CHOICEBOX#      
/LABEL/ Linear Units:
/POSITION/ 145,59, 200,59
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "millimeters", "centimeters", "meters", "inches","ft & in eng", "ft & in arch"

#CHOICEBOX#      
/LABEL/ Angular Units:
/POSITION/ 145,76, 200, 76
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "decimal deg", "deg min", "deg min sec","radians"

#CHOICEBOX#      
/LABEL/ Zero Sup:
/POSITION/ 145,93, 200,93
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "none", "before", "after", "both"

#CHOICEBOX#      
/LABEL/ Round-Off:
/POSITION/ 145,110, 200, 110
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "truncate", "round up", "round down", "round nearest"

#EDIT#
/LABEL/ Rnd-Off Fact:
/POSITION/ 145,127, 200, 127
/SIZE/ 125,14
/TYPE/ UD_DASVAL
/LEN/ -7
/PREC/ 4

#CHOICEBOX#      
/LABEL/ Accuracy:
/POSITION/ 145,144, 200, 144
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "1/2", "1/4", "1/8", "1/16", "1/32", "1/64"

#CHOICEBOX#      
/LABEL/ Disp Mode:
/POSITION/ 145,161,200,161
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Standard", "Modified"

#CHOICEBOX#      
/LABEL/ Radius Symbol:
/POSITION/ 145,178, 200, 178
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "no symbol", "R", "RAD", "user's symbol"

#EDIT#
/LABEL/ User Symbol:
/POSITION/ 145,195,200, 195
/SIZE/ 125,14
/TYPE/ UD_DASSTRING
/LEN/ 12
/PREC/ 12

#CHOICEBOX#      
/LABEL/ Sym Placement:
/POSITION/ 145,212,200,212
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "above dim", "below dim", "before dim", "after dim"
