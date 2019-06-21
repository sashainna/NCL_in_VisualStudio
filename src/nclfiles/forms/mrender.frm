#HEADER#
/TITLE/ Rendering Modals
/POSITION/ 0,0
/SIZE/ 160,105

#CHOICEBOX#      
/LABEL/ Lighting model:
/POSITION/ 10,8,70,8
/SIZE/ 140,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Hardware Rendering", "Software Rendering"

#CHOICEBOX#      
/LABEL/ Rendering mode:
/POSITION/ 10,25,70,25
/SIZE/ 140,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Phong", "Gouraud"

#CHOICEBOX#      
/LABEL/ Dithering Mode:
/POSITION/ 10,42,70,42
/SIZE/ 140,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#EDIT#
/LABEL/ Ambient Intensity:
/POSITION/ 10,59,70,59
/SIZE/ 130,14
/TYPE/ UD_DASINT
/RANGE/0,100
/PREC/ 3
/LEN/ 3
