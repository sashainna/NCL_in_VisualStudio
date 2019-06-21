#HEADER#
/TITLE/ Boundary Conditions
/POSITION/ 0,50
/SIZE/ 240,210

#CHOICEBOX#
/LABEL/ Edge Modifier:
/POSITION/ 15,10,95,10
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Contact", "On", "To", "Past"

#CHOICEBOX#
/LABEL/ Boundary Intersections:
/POSITION/ 15,27,95,27
/SIZE/ 125,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default", "Thru", "Retract", "Down", "Both"

#CHOICEBOX#
/LABEL/
/POSITION/ 115,27,150,27
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Shortest", "Same", "CCLW", "CLW"

#FRAME#
/TITLE/ Avoid Geometry
/POSITION/ 8,42
/SIZE/ 221,33

#EDIT#
/LABEL/ Thick:
/POSITION/ 15,54,40,54
/SIZE/30,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 6

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 75,54
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 120,54
/SIZE/50,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 175,54
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Retract:
/POSITION/ 13,83,40,83
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Dist", "Plane", "Surface"

#EDIT#
/LABEL/
/POSITION/ 88,83
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 130,83
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 175,83
/SIZE/50,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#CHOICEBOX#
/LABEL/ Fedrat:
/POSITION/ 40,100,70,100
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current", "Rapid", "Value"

#EDIT#
/LABEL/
/POSITION/ 120,100
/SIZE/ 50,14
/TYPE/ UD_SCAUNITLESS
/PREC/ 8
/LEN/ 8

#CHOICEBOX#
/LABEL/ Rapto:
/POSITION/ 13,117,40,117
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Dist", "Plane", "Surface"

#EDIT#
/LABEL/
/POSITION/ 88,117
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 130,117
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#      
/LABEL/
/POSITION/ 175,117
/SIZE/50,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#CHOICEBOX#
/LABEL/ Fedrat:
/POSITION/ 40,134,70,134
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current", "Rapid", "Value"

#EDIT#
/LABEL/
/POSITION/ 120,134
/SIZE/ 50,14
/TYPE/ UD_SCAUNITLESS
/PREC/ 8
/LEN/ 8

#CHECKBOX#
/LABEL/ Final Pass:
/POSITION/ 13,151
/SIZE/ 70,15
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Fedrat:
/POSITION/ 40,168,70,168
/SIZE/ 70,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current", "Value"

#EDIT#
/LABEL/
/POSITION/ 120,168
/SIZE/ 50,14
/TYPE/ UD_SCAUNITLESS
/PREC/ 8
/LEN/ 8

#HELP#
Edge Modifier
-------------
The optional edge modifier can be specified as Contact, To, On, or Past. 

Contact is the default option - the tool is in contact with the surface
boundary.

If On is specified the tool end is positioned above the boundary, while the 
tool is in contact with the surface itself. 

If To or Past is specified, the tool is positioned on the inside or outside 
of the boundary, as if the boundary with this modifier were used as a Check 
Surface during each milling pass.

Boundary Intersections
----------------------
The Default means the motion will only be generated for the area before
the first intersection, and no motion will be generated after the first
intersection.

If Thru is specified, all the intersections are ignored. Motion will be
generated through all the intersections to the other side of the outer boundary.

If Down is specified, when an intermediate boundary intersection is
encountered, the pass leaves the flowline and takes the shortest way around the
boundary until it is reached again at the next intersection, then the pass
continues along the flowline. 

If Retract option is specified, when an intermediate boundary intersection is 
encountered, the tool is retracted, then moved to the point above the next 
intersection, after which the tool is moved back on the milled surface and the 
pass continues.

If Both is specified, it works as the Down option (when an intermediate
boundary intersection is encountered, the pass leaves the flowline and goes
around the boundary until the flowline is regained), until the already cut part
of the boundary is reached. Instead of going around the already cut pass (as it
would with the Down option), the motion follows the intermediate boundary
retract option: the cutter is retracted and moved to the other side of the
boundary, where it is moved back to the surface.
 
(Boundary Direction)
--------------------
Active when Both is selected to deal with Boundary Intersections. The choices
are: Shortest, Same, CCLW, CLW.

If Shortest is specified (the default option), the pass takes the shortest way
around the boundary.
If Same is specified, the shortest path is calculated only once for each
boundary - each subsequent cut around the boundary is made toward the
previously cut part of the boundary (so that the cutter stays down as little
as possible).
If CCLW is specified, the cut goes counter-clockwise (as viewed along the tool
axis) around the outer boundary and clockwise around inner boundaries.
If CLW is specified the direction is reversed: clockwise around the outer
boundary and counter-clockwise around inner boundaries.

Avoid Geometry
--------------
A flowline pass motion will stop at an Avoidance Entity, then move along it to
the next flowline and continue.
An Avoidance Entity can be any of the following geometry types: surface, plane,
curve, line, circle. Up to 40 Avoidance Entities are allowed in a single FMILL
command. Each Avoidance Entity can have an optional thick parameter.

Thick
-----
The thick parameter to apply to selected Avoidance Entities.

Select
------
Pick this button to select the geometric entity to be avoided. The currently
set thick parameter will apply to the selected entity. You can change the thick
parameter before selecting the next entity.

(Color)
-------
The avoidance geometry will be highlighted with the color shown.

Deselect All
------------
Empty the list of Avoidance Entities for this FMILL command.

Retract
-------
Pick Distance to retract to the specified distance above the intermediate
boundary intersection, or Plane or Surface to retract to the selected entity. 
Select Current to have the retract move done at the current retract feed rate, 
Rapid to have the retract move done at the rapid feed rate,
or value to have the retract move done at the feed rate specified.

Rapto
-----
Pick Distance to rapto from the specified distance above to the next
intermediate boundary intersection, or Plane or Surface to rapto from the
selected entity. 
Select Current to have the rapto move done at the current rapto feed rate, 
Rapid to have the rapto move done at the rapid feed rate,
or value to have the rapto move done at the feed rate specified.

Final Pass
----------
If checked, a final pass around the boundary is performed. This option is
active only when Down, Retract, or Both is selected for Boundary Intersections.
If there is an inner boundary, the pass around it is performed when the tool
touches it for the last time.

An optional feedrate can be specified for this final pass motion.
Select Current to have the final pass done at the current primary feed rate,
or Value to specify a different feedrate.


