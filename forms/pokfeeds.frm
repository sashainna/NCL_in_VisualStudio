#HEADER#
/TITLE/ Pocket Feedrates
/POSITION/ 50,50
/SIZE/170,157

#CHOICEBOX#
/LABEL/ General:
/POSITION/ 10,10,50,10
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Feed"

#EDIT#
/LABEL/
/POSITION/ 85,10, 105,10
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ Positioning:
/POSITION/ 10,27,50,27
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Rapid","Feed","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,27, 105,27
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ Retract:
/POSITION/ 10,44,50,44
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Rapid","Feed","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,44, 105,44
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ Entry:
/POSITION/ 10,61,50,61
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "General","Feed","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,61, 105,61
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ Transition:
/POSITION/ 10,78,50,78
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "General","Feed","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,78, 105,78
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ Finish:
/POSITION/ 10,95,50,95
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "General","Feed","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,95, 105,95
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ First Pass:
/POSITION/ 10,112,50,112
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Rapid","Feed","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,112, 105,112
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#HELP#
This form allows you to modify the pocket feedrates as part of Pocket Modal
parameters

General 	
-------
Choice between Feed (default) and Current.
Contains the current 'general feedrate' parameter. The current primary
feedrate will be used if Current is chosen.

Positioning	
-----------
Choice between Rapid (default), Feed, and Factor.
If Feed is chosen above, contains the current positioning feedrate parameter; 
if Factor is chosen above, the parameter is interpreted as a factor of the 
general feedrate.

Retract 	
-------
Choice between Rapid (default), Feed, and Factor.
If Feed is chosen above, contains the current retract feedrate parameter; 
if Factor is chosen above, the parameter is interpreted as a factor of the 
general feedrate.

Entry 	
-----
Choice between General, Feed, and Factor (default).
If Feed is chosen above, contains the current entry feedrate parameter; 
if Factor is chosen above, the parameter is interpreted as a factor of the 
general feedrate.

Transition 	
----------
Choice between General, Feed, and Factor (default).
If Feed is chosen above, contains the current transition  feedrate parameter; 
if Factor is chosen above, the parameter is interpreted as a factor of the 
general feedrate.

Finish 	
------
Choice between General, Feed, and Factor (default).
If Feed is chosen above, contains the current finish feedrate parameter; 
if Factor is chosen above, the parameter is interpreted as a factor of the 
general feedrate.

First Pass 	
----------
Choice between Rapid, Feed, and Factor (default).
If Feed is chosen above, contains the current finish feedrate parameter; 
if Factor is chosen above, the parameter is interpreted as a factor of the 
general feedrate.
