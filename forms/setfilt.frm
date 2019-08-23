#HEADER#
/TITLE/ Selection Filter
/POSITION/ 0,0
/SIZE/ 230,155

#CHOICEBOX#      
/LABEL/ State:
/POSITION/ 10,8
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Include", "Exclude", "Inactive"

#COLOR#      
/LABEL/ Color:
/POSITION/ 105,8
/SIZE/ 100,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#      
/LABEL/ State:
/POSITION/ 10,25
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Include", "Exclude", "Inactive"

#CHOICEBOX#      
/LABEL/ Line Style:
/POSITION/ 105,25
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Solid", "Small Dash", "Dots", "Center Line", "Phantom"
/CHOICES/ "Large Dash", "Dash Dot", "Dash Space"

#CHOICEBOX#      
/LABEL/ State:
/POSITION/ 10,42
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Include", "Exclude", "Inactive"

#CHOICEBOX#      
/LABEL/ Entity type:
/POSITION/ 105,42
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Points", "Lines", "Circles", "Planes", "Vectors"
/CHOICES/ "Pointvecs", "Matrices", "All curves", "NCL curves", "Composites"
/CHOICES/ "Bsplines", "Ssplines", "Conics", "Paterns", "All surfs",
/CHOICES/ "NCL surf", "Nsurfs", "Trim surfs", "Rev surfs", "Net surfs",
/CHOICES/ "Mesh surfs", "Quilt surfs", "Shapes", "Solids", "Annotation",
/CHOICES/ "Drafting"

#CHOICEBOX#      
/LABEL/ State:
/POSITION/ 10,59
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Include", "Exclude", "Inactive"

#EDIT#
/LABEL/ Logical Pen:
/POSITION/ 105,59,150,59
/SIZE/ 80,14
/TYPE/ UD_DASINT
/RANGE/1,256
/PREC/ 3
/LEN/ 3

#EDIT#
/LABEL/ thru
/POSITION/ 170,59
/SIZE/ 80,14
/TYPE/ UD_DASINT
/RANGE/1,256
/PREC/ 3
/LEN/ 3

#CHOICEBOX#      
/LABEL/ State:
/POSITION/ 10,76
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Include", "Exclude", "Inactive"

#EDIT#
/LABEL/ Layer Numbers:
/POSITION/ 105,76, 160,76
/SIZE/ 90,14
/TYPE/ UD_DASINT
/RANGE/0,9999
/PREC/ 4
/LEN/ 4

#EDIT#
/LABEL/ thru
/POSITION/ 185,76
/SIZE/ 70,14
/TYPE/ UD_DASINT
/RANGE/0,9999
/PREC/ 4
/LEN/ 4

#CHOICEBOX#      
/LABEL/ Continue entering layer numbers:
/POSITION/ 10,93,125,93
/SIZE/ 200,40
/TYPE/ UD_DASSTRING
/CHOICES/ "No", "Yes"

#CHOICEBOX#      
/LABEL/ State:
/POSITION/ 10,110
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Include", "Exclude", "Inactive"

#CHOICEBOX#      
/LABEL/ Marker Type:
/POSITION/ 105,110
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Dot", "Plus", "Asterisk", "Circle", "Cross","Triangle", "Diamond"
/CHOICES/ "Square","Dbl. Circle", "Large Dot", "Cube"
