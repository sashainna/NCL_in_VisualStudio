#HEADER#
/TITLE/ Profile Entry / Exit Methods
/POSITION/ 50,30
/SIZE/310,100

#CHOICEBOX#
/LABEL/ Entry:
/POSITION/ 15,8
/SIZE/65,40
/TYPE/UD_DASSTRING
/CHOICES/ "Omit","Angle","Arc","Both"

#EDIT#
/LABEL/ Distance:
/POSITION/ 85,8, 120,8
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Angle:
/POSITION/ 165,8
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Rise:
/POSITION/ 235,8
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Radius:
/POSITION/ 85,25, 120,25
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Rise:
/POSITION/ 165,25
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Exit:
/POSITION/ 15,42
/SIZE/65,40
/TYPE/UD_DASSTRING
/CHOICES/ "Omit","Angle","Arc","Both"

#EDIT#
/LABEL/ Distance:
/POSITION/ 85,42, 120,42
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Angle:
/POSITION/ 165,42
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Rise:
/POSITION/ 235,42
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Radius:
/POSITION/ 85,59, 120,59
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Rise:
/POSITION/ 165,59
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#HELP#
============================
Profile Entry / Exit Methods
============================
This form is used to define the methods used for entering onto and exiting off
of the profile.  The entry and exit methods can be omitted, at an angle, along
an arc, or a combination of both.

One special note to consider with entry and exit moves is that if the profile
condition is set to On, the profile is closed, and the entry/exit method is set
to Angle, then the entry move will actually follow the shape of the curve rather
than being purely a straight move.

Entry:
------
The Entry method specifies how the tool enters each profile pass.  'Omit'
does not generate any entry motion, the tool will move directly to the start
of the profile pass.  'Angle' will generate a linear move at an angle to the
start of the profile on entry.  'Arc' will generate a 90-degree circular move
to the start of the profile.

'Both' will generate a combination linear and circular move onto the profile.
The linear move will occur first and will always be tangent to the arc move,
which transitions between the linear move and the profile.  The circular move
will always be tangent to the profile.

Distance:
---------
This field is only enabled when the entry method is set to 'Angle' or 'Both'
and specifies the length of the linear entry move.

Angle:
------
This field is only enabled when the entry method is set to 'Angle' or 'Both'
and specifies the angular offset to the initial direction of the profile to
use as the entry direction.  The angle value is ignored whenever the profile
condition is set to On.

Rise:
-----
This field specifies the rise value or distance above the profile start location
for the beginning of the entry move.  The rise value is ignored if the profile
condition is set to On and the Entry method is set to Arc.

Radius:
-------
This field is only enabled when the entry method is set to 'Arc' or 'Both' and
specifies the radius of the circular entry arc.

Rise:
-----
This field specifies the rise value or distance above the profile start location
for the beginning of the entry move.  Specifying a Rise distance other than zero
will generate helical entry motion.

Exit:
-----
The Exit method specifies how the tool exits each profile pass.  'Omit' does
not generate any exit motion, the tool will be positioned at the end of the
profile when the retract motion is generated.  'Angle' will generate a linear
move at an angle to the end of the profile on exit.  'Arc' will generate a
90-degree circular move off of the end of the profile.

'Both' will generate a combination circular and linear move off of the profile.
The circular move will occur first and will always be tangent to the profile
and transitions between the profile and the linear move.  The linear move will
always be tangent to the circular move.

Distance:
---------
This field is only enabled when the exit method is set to 'Angle' or 'Both' and
specifies the length of the linear exit move.

Angle:
------
This field is only enabled when the exit method is set to 'Angle' or 'Both' and
specifies the angular offset to the final direction of the profile to use as
the exit direction.  The angle value is ignored whenever the profile condition
is set to On.

Rise:
-----
This field specifies the rise value or distance above the profile end location
for the ending of the exit move.  The rise value is ignored if the profile
condition is set to On and the Entry method is set to Arc.

Radius:
-------
This field is only enabled when the exit method is set to 'Arc' and specifies
the radius of the circular exit arc.

Rise:
-----
This field specifies the rise value or distance above the profile end location
for the ending of the exit move.  Specifying a Rise distance other than zero
will generate helical exit motion.
