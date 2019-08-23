#HEADER#
/TITLE/ Rmill Feed Rates
/POSITION/ 50,30
/SIZE/310,70

#CHOICEBOX#
/LABEL/ General:
/POSITION/ 10,12, 45,12
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value"

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
/CHOICES/ "Current","Value","Rapid"

#EDIT#
/LABEL/
/POSITION/ 255,12
/SIZE/90,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Plunge:
/POSITION/ 10,29, 45,29
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value"

#EDIT#
/LABEL/
/POSITION/ 105,29
/SIZE/50,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#HELP#
================
Rmill Feed Rates
================
This form defines the feed rates to use for Regional Milling.  Each feed rate
has the following modes that can be programmed.

   Current = Uses the currently programmed feed rate.
   Value   = Allows you to specify an absolute value as the feed rate.
   Rapid   = Uses RAPID. (Only valid for the Position field).

General:
--------
Defines the feed rate to use during the basic motion of regional milling.

Position:
---------
Defines the feed rate to use when positioning the tool to and along the
clearance plane and to the retact plane.

Plunge:
-------
Defines the feed rate to use when the tool is positioned to the rapto distance
above the regional milling part surface.
