#HEADER#
/TITLE/ NCLIPV Universe
/POSITION/ 50,50
/SIZE/230,90

#EDIT#
/LABEL/ Lower Left:
/POSITION/ 10,8, 60,8
/SIZE/150,14
/TYPE/ UD_DASCART
/LEN/ 35
/PREC/ 3
/INPUT/ FORM_PICK

#EDIT#
/LABEL/ Upper Right:
/POSITION/ 10,25, 60,25
/SIZE/150,14
/TYPE/ UD_DASCART
/LEN/ 35
/PREC/ 3
/INPUT/ FORM_PICK

#PUSHBUTTON#
/LABEL/ Reset
/POSITION/ 65,45
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Rescan
/POSITION/ 130,45
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#HELP#
===============
NCLIPV Universe
===============
The NCLIPV Universe form is used to manually input the size of the entire
region (universe) that contains all stock and fixture definitions within
NCLIPV.  When NCLIPV is first started it will calculate the sizes of all
defined stocks and fixtures and will scan the tool path to determine its
bounding box.  The NCLIPV Universe will be automatically set based on these
calculated regions and with the default region defined in the NCLIPV modals
file.

Changing the values in this form should not be necessary, unless extra stocks
and/or fixtures are added or a new tool path is loaded to the NCLIPV session
during simulation.  If the NCLIPV Universe is changed, then the NCLIPV session
will have to be reset for these values to take effect.

Lower Left:
-----------
Defines the lower left corner of the NCLIPV Universe.

Upper Right:
------------
Defines the upper right corner of the NCLIPV Universe.

Reset
-----
The Reset button resets the NCLIPV Universe to its default size as defined in
the NCLIPV Modals file.

Rescan
------
The Rescan button causes the stock and tool path limits to be recalculated.
This newly calculated region will be combined with the currently defined region
to create the NCLIPV Universe size.  If an NCLIPV session is not active then
only the tool path will be scanned.
