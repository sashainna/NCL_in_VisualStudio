#HEADER#
/TITLE/ Profile Final Pass
/POSITION/ 50,50
/SIZE/280,80

#CHECKBOX#
/LABEL/ 
/POSITION/ 10,12
/SIZE/15,15
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ CUTCOM/
/POSITION/ 25,15,65,12
/SIZE/75,14
/TYPE/UD_DASSTRING
/CHOICES/ "Left","Right","On","None"

#CHOICEBOX#
/LABEL/ ,
/POSITION/ 105,15,115,12
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ "XYPLAN","YZPLAN","ZXPLAN","None"

#EDIT#
/LABEL/ ,
/POSITION/ 165,15,175,12
/SIZE/ 80,14
/TYPE/UD_DASSTRING
/LEN/ 20
/PREC/ 8

#EDIT#
/LABEL/ Positional Distance:
/POSITION/ 65,32, 135,29
/SIZE/100,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ ,
/POSITION/ 175,32,185,29
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ "ALL","START"

#HELP#
==================
Profile Final Pass
==================
This form allows you to modify the final pass parameters for Profile Machining.

CUTCOM/	
-------
If this box is checked, then the CUTCOM command defined by the CUTCOM parameters
will be output to the clfile just prior to making the final pass around the
profile.  A CUTCOM/OFF post-processor statement will be output to the clfile
to reset the CUTCOM condition after the profile motion and prior to the exit
move.

Positional Distance:
-------------------
The positional distance is used to calculate a new entry point after which
the CUTCOM command will be output.

ALL/START
---------
ALL will output CUTCOM on all entry moves when machining a closed profile
with looping and START will output CUTCOM on just the first entry move.
