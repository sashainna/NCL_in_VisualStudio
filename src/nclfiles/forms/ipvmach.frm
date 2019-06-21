#HEADER#
/TITLE/ NCLIPV Machine Simulation
/POSITION/ 50,50
/SIZE/225,200

#CHECKBOX#
/LABEL/ Enable Machine Simulation
/POSITION/ 10,8
/SIZE/ 120,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Machine:
/POSITION/ 10,25
/SIZE/ 35,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 50,25
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/PREC/ 20
/LEN/ 20

#PUSHBUTTON#
/LABEL/ Load
/POSITION/ 150,25
/SIZE/ 35,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Tooling Pin
/POSITION/ 13,58
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Offsets
/POSITION/ 63,58
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Tool Length
/POSITION/ 113,58
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Position
/POSITION/ 163,58
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Attributes
/POSITION/ 13,75
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Settings
/POSITION/ 8,44
/SIZE/ 205,52

#LISTBOX#
/LABEL/
/POSITION/ 10,110
/SIZE/ 200,60
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Machine Description
/POSITION/ 8,98
/SIZE/ 205,70

#HELP#
==============================
NCLIPV Machine Simulation Form
==============================
This form handles all aspects of machine simulation within NCLIPV, except for
collisions, which are handled in the NCLIPV Diagnostic Handler form.  You must
be using a Simulation file as input in order to us Machine Simulation.

Enable Machine Simulation
-------------------------
Determines whether machine simulation is active during an NCLIPV session.
Enabling Machine Simulation will display a representation of the machine and
use the kinematics of the machine axes for material removal.  Disabling Machine
Simulation will use the tool position for material removal without displaying
the machine.

Machine:
--------
Press this button to bring up a file browser to interactively select the machine
definition files.  You can also directly type the machine definition directory
name into the text field to the right of this button.

Load:
-----
Loads the machine definition files.  If NCLIPV is active, then the machine will
be displayed in the NCLIPV window.  This is an immediate action button, so the
OK button does not have to be pressed to load the machine, though you do not
have to press the Load button, accepting the form (OK) will also load the
machine files.

Tooling Pin
-----------
The Tooling Pin button brings up the Tooling Pin form, which is used to place
the stocks and fixtures onto the machine.

Offsets
-------
The Offsets button brings up the Axis Offsets form, which is used to enter
offset values for each of the machine axes.  These offsets will be added to the
machine positions programmed in the Simulation file.

Tool Length
-----------
The Tool Length button will bring up the Tool Lengths form, which displays all
of the tools used in the simulation file along with their programmed tool
lengths.  You can change the length of any tool or add an offset to each tool
length.

Position
--------
The Position button brings up the Axes Position form, which is used to manually
position any of the defined axes.

Attributes
----------
The Attributes button brings up the Machine Component Attributes form, which is
used to change the display attributes of the various machine components.

Machine Description
-------------------
The Machine Description field contains the defintion of the currently loaded
machine as described in the 'machine.dat' file.
