#HEADER#
/TITLE/ Tool Axis Modifier
/POSITION/ 50,50
/SIZE/140,140

#EDIT#
/LABEL/ Fixed Distance: 
/POSITION/ 10,8,75,8
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -6

#CHECKBOX#
/LABEL/ Radius distance:  
/POSITION/ 10,25
/SIZE/80,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Transition Distance: 
/POSITION/ 10,42,75,42
/SIZE/150,14
/TYPE/UD_SCAVAL
/PREC/ 4
/LEN/ -6

#CHOICEBOX#
/LABEL/ Transition Mode:
/POSITION/ 10,59
/SIZE/ 120,60
/TYPE/UD_DASSTRING
/CHOICES/ "Interpolate", "Fan"

#CHECKBOX#
/LABEL/ Omit first distance:  
/POSITION/ 10,76
/SIZE/80,14
/TYPE/UD_DASSTRING
#HELP#
This form generates a TLAXIS/LOCK statement which causes the tool axis to be
locked for a specified distance at the start and end of a move.

Fixed Distance
--------------
Enter the distance that the tool axis will be fixed at the start and/or end
of a move.

Radius Distance
---------------
Check this box to specify that the distance specified is a circular radius.
An actual linear distance will be estimated from this value and the drive
surface and check surface normals at the end of the move.

Transition Distance
-------------------
This specifies the distance that the tool will move to transition to or from
the fixed tool axis from or to the currently defined tool axis mode at the
start or end of a move.

Transition Mode
---------------
If Interpolate is specified, the transition move will be made by interpolating
to a vector. If Fan is specified, the transition move will be made by fanning.

Omit First Distance
-------------------
Check this box to have the tool axis lock not be applied to the start of
the first move following the tlaxis lock statement.
