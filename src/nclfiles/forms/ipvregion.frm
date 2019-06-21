#HEADER#
/TITLE/ Region of Interest
/POSITION/ 50,50
/SIZE/178,90

#CHECKBOX#
/LABEL/ Limit simulation to region
/POSITION/ 10,8
/SIZE/ 100,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Lower Left:
/POSITION/ 10,25
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 55,25, 60,25
/SIZE/150,14
/TYPE/ UD_DASCART
/LEN/ 25
/PREC/ 3

#PUSHBUTTON#
/LABEL/ Upper Right:
/POSITION/ 10,42
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 55,42, 60,42
/SIZE/150,14
/TYPE/ UD_DASCART
/LEN/ 25
/PREC/ 3

#HELP#
==================
Region of Interest
==================
The Region of Interest form allows you to limit the region of the NCLIPV
simulation.  Only the tool path that is encompassed by this region will be
used during simulation.  This feature is useful for speeding up the simulation
when you are only interested in analyzing a selected area of the part.

Limit simulation to region
--------------------------
Check this box if you would like to see only a specific area of the part
machined during NCLIPV simulation.  Unchecking this box causes the entire
simulation to be performed.

Lower Left:
-----------
Defines the lower left corner of the Region of Interest.

Upper Right:
------------
Defines the upper right corner of the Region of Interest.
