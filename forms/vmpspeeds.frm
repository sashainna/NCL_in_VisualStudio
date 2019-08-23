#HEADER#
/TITLE/ VoluMill Spindle Speeds
/POSITION/ 50,50
/SIZE/170,100

#CHOICEBOX#
/LABEL/ General:
/POSITION/ 10,10,50,10
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Speed"

#EDIT#
/LABEL/
/POSITION/ 85,10, 105,10
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ Entry:
/POSITION/ 10,27,50,27
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "General","Speed","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,27, 105,27
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ Transition:
/POSITION/ 10,44,50,44
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "General","Speed","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,44, 105,44
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#EDIT#
/LABEL/ Dwell After Spindle Change:
/POSITION/ 10,61
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#HELP#
This form allows you to modify the pocket spindle speeds as part of the VoluMill
Pocket Modal parameters.

General 	
-------
This spindle speed is used as the general spindle speed for pocketing motion.
You can use either the Current spindle speed or specify a new spindle Speed for
pocketing.

Entry 	
-----
Specify the spindle speed to use when entering the part in a ramping motion.
You can use the General spindle speed, specify a unique spindle Speed, or use a
Factor (percentage) of the general spindle speed.

Transition 	
----------
Specify the spindle speed to use when cutting a slot to enter a new pocket area.
You can use the General spindle speed, specify a unique spindle Speed, or use a
Factor (percentage) of the general spindle speed.

Dwell After Spindle Change
--------------------------
Enter the amount of time in seconds to dwell after changing the spindle speed.
