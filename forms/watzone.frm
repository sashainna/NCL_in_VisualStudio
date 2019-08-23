#HEADER#
/TITLE/ Zone Ordering
/POSITION/ 20,30
/SIZE/200,190

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 15,15
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 65,15
/SIZE/50,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 120,15
/SIZE/45,15
/TYPE/ UD_DASSTRING

#LISTBOX#
/LABEL/
/POSITION/ 15,42
/SIZE/ 170,56
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Delete
/POSITION/ 15,100
/SIZE/35,15
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Move Up: /\
/POSITION/ 55,100
/SIZE/55,15
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Move Down: \/
/POSITION/ 115,100
/SIZE/55,15
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Zone List
/POSITION/ 10,32
/SIZE/ 180,90

#CHOICEBOX#
/LABEL/ Contours Must Be
/POSITION/ 15,130,75,128
/SIZE/120,40
/TYPE/UD_DASSTRING
/CHOICES/ "Completely","Partially","Mostly"

#LABEL#
/LABEL/ Inside a Zone
/POSITION/ 140,130,160,128
/SIZE/40,40
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Cut All Remaining Pockets after Zones
/POSITION/ 15,145
/SIZE/140,15
/TYPE/UD_DASSTRING

#HELP#
Zone Ordering for the Waterline Roughing command is specified by:
defining a list of zones and indicating the rule that decides when
a pocket belongs to a zone.

A valid zone is the horizontal projection of a closed curve or circle. 
It defines a region on the pocket cutting plane.

Select
------
Used to add curves to the list of zones. If a curve already in
the list is selected, it will be removed form the list. 

Color
-----
Chooses a color to highlight the selected curves.
 
Deselect All
------------
Deselects the currently selected curves. The list is emptied as a result.

Zone List
---------
This list box contains the current curve list, in order. One can alter
the list by selecting lines and using the buttons 'Delete', 'Move Up',
and 'Move Down'.

Delete
------
Removes the highlighted curve from the list.

Move Up
-------
Moves the highlighted curve one position up in the zone list.

Move Down
---------
Moves the highlighted curve one position down in the zone list.

Contours Must Be Partially, (Completely, Mostly) Inside a Zone
--------------------------------------------------------------
This choice box defines the rule by which the pocket-in-a-zone decisions
are made, as follows:
	Partially - some part of the perimeter contour must be inside
	the zone curve.
	Completely - the perimeter must be completely inside the curve.
	Mostly - most of the perimeter area must be inside the curve.

Cut All Remaining Pockets After Zones
--------------------------------------
If checked, the pockets not in any of the listed zones will be cut after
the last zone is done.
If not checked, the pockets not in zones will not be cut.
