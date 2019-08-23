#HEADER#
/TITLE/ Profile Feed Rates
/POSITION/ 50,30
/SIZE/310,90

#CHOICEBOX#
/LABEL/ General:
/POSITION/ 10,12, 45,12
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,12
/SIZE/50,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Position:
/POSITION/ 160,12, 200,12
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 255,12
/SIZE/90,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Retract:
/POSITION/ 10,29, 45,29
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,29
/SIZE/50,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Entry:
/POSITION/ 160,29, 200,29
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 255,29
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Transition:
/POSITION/ 10,46, 45,46
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,46
/SIZE/50,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Exit:
/POSITION/ 160,46, 200,46
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 255,46
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#HELP#
==================
Profile Feed Rates
==================
This form defines the feed rates to use when Profile Machining.  Each feed rate
has the following modes that can be programmed.

   Current = Uses the currently programmed feed rate.
   Value   = Allows you to specify an absolute value as the feed rate.
   Rapid   = Uses RAPID.
   Factor  = Enter a percentage of the programmed feed rate to use, for example
             .85 specifies 85% of the programmed feed rate.

General:
--------
Defines the feed rate to use during the basic motion along the profile.

Position:
---------
Defines the feed rate to use when positioning the tool to and from the clearance
plane and rapto plane.

Retract:
--------
Defines the feed rate to use when the tool is retracted at the end of the
profiling motion and between passes.

Entry:
------
Defines the feed rate to use during the entry move to the profile.

Transition:
-----------
Defines the feed rate to use when transitioning between "loops" offset from the
profile curve.

Exit:
-----
Defines the feed rate to use during the exit move from the profile.
