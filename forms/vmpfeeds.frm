#HEADER#
/TITLE/ VoluMill Feedrates
/POSITION/ 0,0
/SIZE/ 168,170

#CHOICEBOX#
/LABEL/ General:
/POSITION/ 10,9,50,9
/SIZE/ 86,57
/TYPE/UD_DASSTRING
/CHOICES/ "Current","Feed"

#EDIT#
/LABEL/ 
/POSITION/ 85,9,105,9
/SIZE/ 58,13
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHOICEBOX#
/LABEL/ Positioning:
/POSITION/ 10,26,50,26
/SIZE/ 86,57
/TYPE/UD_DASSTRING
/CHOICES/ "General","Feed","Factor"

#EDIT#
/LABEL/ 
/POSITION/ 85,26,105,26
/SIZE/ 58,14
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHOICEBOX#
/LABEL/ Entry:
/POSITION/ 10,43,50,43
/SIZE/ 86,57
/TYPE/UD_DASSTRING
/CHOICES/ "General","Feed","Factor"

#EDIT#
/LABEL/ 
/POSITION/ 85,43,105,43
/SIZE/ 58,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHOICEBOX#
/LABEL/ Transition:
/POSITION/ 10,60,50,60
/SIZE/ 86,57
/TYPE/UD_DASSTRING
/CHOICES/ "General","Feed","Factor"

#EDIT#
/LABEL/ 
/POSITION/ 85,60,105,60
/SIZE/ 58,13
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHOICEBOX#
/LABEL/ XY Rapid:
/POSITION/ 10,77,50,77
/SIZE/ 86,47
/TYPE/UD_DASSTRING
/CHOICES/ "Default","Feed"

#EDIT#
/LABEL/ 
/POSITION/ 85,77,105,77
/SIZE/ 42,13
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 8

#CHOICEBOX#
/LABEL/ Z Rapid:
/POSITION/ 10,94,50,94
/SIZE/ 86,47
/TYPE/UD_DASSTRING
/CHOICES/ "Default","Feed"

#EDIT#
/LABEL/ 
/POSITION/ 85,94,105,94
/SIZE/ 45,13
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 8

#CHECKBOX#
/LABEL/ Use Minimum Feedrate
/POSITION/ 10,111
/SIZE/ 85,11
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 85,111,105,111
/SIZE/ 39,13
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#CHECKBOX#
/LABEL/ Adjust Feed Rate For Entry
/POSITION/ 10,128
/SIZE/ 100,11
/TYPE/UD_DASSTRING

#HELP#
This form allows you to modify the pocket feed rates as part of the VoluMill
Pocket Modal parameters.

General 	
-------
This feed rate is used as the general feed rate for pocketing motion.  You
can use either the Current feed rate or specify a new Feed rate for pocketing.

Positioning	
-----------
This is the feed rate used for positioning the tool when it is not engaged in
the material.  You can use the General feed rate, specify a unique Feed rate,
or use a Factor (percentage) of the general feed rate.

Entry 	
-----
Specify the feed rate to use when entering the part in a ramping motion.  You
can use the General feed rate, specify a unique Feed rate, or use a Factor
(percentage) of the general feed rate.

Transition 	
----------
Specify the feed rate to use when cutting a slot to enter a new pocket area.
You can use the General feed rate, specify a unique Feed rate, or use a Factor
(percentage) of the general feed rate.

XY Rapid
--------
This value defines the rate at which the machine moves at rapid in the
XY plane.  Default sets the XY Rapid rate to the Transition rate.  Feed
enables the text field where an XY Rapid rate can be specified.

Z Rapid
-------
This value defines the rate at which the machine moves at along the Z-Axis.
Default sets the Z Rapid rate to the Transition rate.  Feed enables the text
field where a Z Rapid rate can be specified.

Use Minimum Feedrate
--------------------
This value defines the minimum feed rate allowed.

Adjust Feed Rate For Entry
--------------------------
When enabled the feed rate at the tool center is reduced during material entry
so that the periphery of the tool moves at the Plunge Feedrate.
