#HEADER#
/TITLE/ Connection Parameters
/POSITION/ 50,30
/SIZE/ 242,140

#CHOICEBOX#
/LABEL/ Connection Type
/POSITION/ 8,13,70,11
/SIZE/110,33
/TYPE/UD_DASSTRING
/CHOICES/"Linear", "Smooth","Chamfer"

#CHECKBOX#
/LABEL/ Create Closed Curve
/POSITION/ 145,11
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Help
/POSITION/ 5,30
/SIZE/ 229, 80

#LABEL#
/LABEL/ Connection Type:
/POSITION/ 10,40
/SIZE/ 125,14
/TYPE/ UD_DASSTRING

#LABEL#
/LABEL/ - Linear:    Connect using line segments. (Can extend lines in curve)
/POSITION/ 15,50
/SIZE/ 250,14
/TYPE/ UD_DASSTRING

#LABEL#
/LABEL/ - Smooth:  Connect using a smooth transition.
/POSITION/ 15,60
/SIZE/ 150,14
/TYPE/ UD_DASSTRING

#LABEL#
/LABEL/ - Chamfer: Connect using line segments. (End-Point to End-Point)
/POSITION/ 15,70
/SIZE/ 250,14
/TYPE/ UD_DASSTRING

#LABEL#
/LABEL/ Create Closed Curve:
/POSITION/ 10,85
/SIZE/ 125,14
/TYPE/ UD_DASSTRING

#LABEL#
/LABEL/ - Connect first and last components to close curve if box is checked.
/POSITION/ 15,95
/SIZE/ 250,14
