#HEADER#
/TITLE/ Pocket Final Pass
/POSITION/ 50,50
/SIZE/280,125

#CHOICEBOX#
/LABEL/ Outside Corners:
/POSITION/ 15,22,72,22
/SIZE/120,14
/TYPE/UD_DASSTRING
/CHOICES/ "Sharp","Arc"

#CHOICEBOX#
/LABEL/ Inside Corners:
/POSITION/ 15,39,72,39
/SIZE/120,14
/TYPE/UD_DASSTRING
/CHOICES/ "Sharp","Arc"

#EDIT#
/LABEL/ Inside Radius:
/POSITION/ 150,39,200,39
/SIZE/ 40,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#FRAME#
/TITLE/ Automatic Corner Rounding
/POSITION/ 10,12
/SIZE/ 260,48

#CHOICEBOX#
/LABEL/ Apply Slowdown Feedrate:
/POSITION/ 10,65,100,65
/SIZE/125,14
/TYPE/UD_DASSTRING
/CHOICES/ "No","Yes"

#EDIT#
/LABEL/ Slowdown Angle:
/POSITION/ 140,65,200,65
/SIZE/ 40,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHECKBOX#
/LABEL/ 
/POSITION/ 10,82
/SIZE/15,15
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ CUTCOM/
/POSITION/ 25,85,65,82
/SIZE/75,14
/TYPE/UD_DASSTRING
/CHOICES/ "Left","Right","On","None"

#CHOICEBOX#
/LABEL/ ,
/POSITION/ 105,85,115,82
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ "XYPLAN","YZPLAN","ZXPLAN","None"

#EDIT#
/LABEL/ ,
/POSITION/ 165,85,175,82
/SIZE/ 80,14
/TYPE/UD_DASSTRING
/LEN/ 20
/PREC/ 8

#HELP#
This form allows you to modify the final pass parameters as part of Pocket
Modal parameters.

Outside Corners
---------------
Choice between Arc and Sharp (default).

Inside Corners
---------------
Choice between Arc and Sharp (default).

Inside Radius
---------------
Contains current fillet parameter - active only when Arc is chosen for
Inside Corners.

Apply Slowdown Feedrate
-----------------------
Choice between Yes - for the optional ATANGL parameter, and No (default).

Slowdown Angle	
--------------
Contains current ATANGL parameter - active only when Yes is chosen above.

CUTCOM/	
-------
Contains current CUTCOM/ statement - active only when the checkbox is checked.
