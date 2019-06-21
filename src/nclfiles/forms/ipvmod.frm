#HEADER#
/TITLE/ NCLIPV Modals
/POSITION/ 50,50
/SIZE/200,120

#CHOICEBOX#
/LABEL/ Machining Mode:
/POSITION/ 10,8
/SIZE/120,40
/TYPE/UD_DASSTRING
/CHOICES/ "Visicut","RapidCut"

#CHOICEBOX#
/LABEL/ Machine Type:
/POSITION/ 10,25
/SIZE/120,40
/TYPE/UD_DASSTRING
/CHOICES/ "Auto","Mill","Lathe","Mill/Turn","Stringer"

#EDIT#
/LABEL/ RapidCut Grid:
/POSITION/ 10,42
/SIZE/180,14
/TYPE/UD_DASINT
/PREC/ 4
/LEN/ 4
/RANGE/ 300,1500

#CHOICEBOX#
/LABEL/ Reset Session on Exit:
/POSITION/ 10,59
/SIZE/120,50
/TYPE/UD_DASSTRING
/CHOICES/ "No","Yes"

#CHOICEBOX#
/LABEL/ Monitor Playback:
/POSITION/ 10,76
/SIZE/105,40
/TYPE/UD_DASSTRING
/CHOICES/ "No","Yes"

#PUSHBUTTON#
/LABEL/ Options
/POSITION/ 120,76
/SIZE/ 45,15
/TYPE/ UD_DASSTRING

#HELP#
==================
NCLIPV Modals Form
==================
The NCLIPV Modals Form is used to define the NCLIPV simulation settings.  These
settings will be saved in the 'nclipv_modals.mod' file and will be used as the
default values the next time the form is displayed.

Machining Mode:
---------------
Choose between Visicut and RapidCut.  Visicut is normally used and provides an
interactive simulation session.  It supports all machining modes, including
mills with multi-axis moves and lathes.  The tool is animated during a Visicut
session, so that you can watch the material actually being removed.

RapidCut is more limited.  It can only be used with a 0,0,1 tool axis and
works with a fixed grid as the stock.  For this reason it is typically only
used for smaller mold type parts or for roughing purposes.  For example,
you can use RapidCut to simulate 3-axis roughing and then switch to Visicut
to simulate the multi-axis finishing motion.  The advantage to RapidCut is that
it is extremely fast compared to Visicut.

Machine Type:
-------------
Visicut supports both Mills and Lathes.  Select the appropriate machine type
here.  Auto will automatically determine the machine type based on the types
of tools used during the program.

For lathe programming, it is assumed that the part is cut in the XY-plane, with
the center of the part at Z=0.  When machine simulation is active, then lathe
programming will be in the standard coordinate system, typically in the
ZX-plane.

RapidCut Grid:
--------------
As mentioned previously, RapidCut works with a fixed grid as the stock.  This
field is used to specify the grid size for RapidCut.  It can be in the range
of 300-1500.

Reset Session on Exit:
----------------------
It is possible to leave NCLIPV and restart it from where you left off at a
later time within the same NCL sesssion, or you can have the session
automatically reset (all cuts are removed and the stocks and fixtures appear
as uncut) when ending the session.  Choose No if you want the session to
remain intact when it is ended.  In either case, you can always use the Reset
IPV Session function to manually reset the session.

Monitor Playback:
-----------------
Enabling the Monitor Playback field will display a control panel during NCLIPV
simulation that contains current block data, including the tool position,
cutter definition, feed rate, spindle, and coolant settings, along with the
machining time and a progress bar representing the percentage of simulation
that has been completed.

Options
-------
Pressing the Options pushbutton will bring up the NCLIPV Monitor Options form
that allows you to specify the fields to be displayed in the NCLIPV Monitor
Panel during NCLIPV simulation.
