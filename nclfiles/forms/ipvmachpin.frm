#HEADER#
/TITLE/ Tooling Pin
/POSITION/ 50,50
/SIZE/178,200

#CHOICE_LIST#
/LABEL/ Label:
/POSITION/ 13,18
/SIZE/ 100,70
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/Origin:
/POSITION/ 13,35,48,35
/SIZE/150,14
/TYPE/ UD_DASCART
/LEN/ 25
/PREC/ 3
/INPUT/ FORM_PICK

#EDIT#
/LABEL/Axis:
/POSITION/ 13,52,48,52
/SIZE/150,14
/TYPE/ UD_DASVEC
/LEN/ 25
/PREC/ 3
/INPUT/ FORM_PICK

#EDIT#
/LABEL/Flat:
/POSITION/ 13,69,48,69
/SIZE/150,14
/TYPE/ UD_DASVEC
/LEN/ 25
/PREC/ 3
/INPUT/ FORM_PICK

#FRAME#
/TITLE/ Machine
/POSITION/ 8,8
/SIZE/ 160,81

#EDIT#
/LABEL/Origin:
/POSITION/ 13,103,48,103
/SIZE/150,14
/TYPE/ UD_DASCART
/LEN/ 25
/PREC/ 3
/INPUT/ FORM_PICK

#EDIT#
/LABEL/Axis:
/POSITION/ 13,120,48,120
/SIZE/150,14
/TYPE/ UD_DASVEC
/LEN/ 25
/PREC/ 3
/INPUT/ FORM_PICK

#EDIT#
/LABEL/Flat:
/POSITION/ 13,137,48,137
/SIZE/150,14
/TYPE/ UD_DASVEC
/LEN/ 25
/PREC/ 3
/INPUT/ FORM_PICK

#FRAME#
/TITLE/ Part
/POSITION/ 8,91
/SIZE/ 160,64

#PUSHBUTTON#
/LABEL/ Place Stock
/POSITION/ 45,157
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Reset Stock
/POSITION/ 95,157
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#HELP#
================
Tooling Pin Form
================
The Tooling Pin form defines the tooling pins that are used to attach the stocks
and fixtures onto the machine in the correct position.  A tooling pin is a
circular pin with a flat on one side, so that the pin goes into the imaginary
tooling holes on the machine and part in a fixed position and direction.

Each stock and fixture can be placed on different tooling pins if so desired,
though it is typical to have only one tooling pin defined, allowing the solid
definitions to be the same whether or not Machine Simulation is enabled.

The ToolingPin command in the 'postworks.mdl' file can be used to setup the
default tooling pin location on the machine.

Label:
------
The Label field defines the label of the tooling pin and is typically the same
as the label of the axis it is attached to. You cannot change the label of the
defined tooling pins.

Machine Origin:
---------------
The Machine Origin field defines the location of the tooling pin on the machine.
The tooling pin attached to the part will be placed at this location.

Machine Axis:
-------------
The Machine Axis field defines the axis of the tooling pin cylinder.  The
tooling pin will be placed on the machine in such a way that the axis of the
imaginary tooling hole will be in this direction.  This is considered the
Z-axis of the tooling pin.

Machine Flat:
-------------
The Machine Flat field defines a vector that points from the center of the
tooling pin to its flat side.  This vector is used to determine the rotation of
the tooling pin on the machine.  This is considered the X-axis of the tooling
pin.

Part Origin:
------------
The Part Origin field defines the location of the tooling pin on the part and
is used to place the stocks and fixtures onto the machine at the location of the
machine tooling pin.

Part Axis:
----------
The Part Axis field defines the axis of the tooling pin cylinder on the part.
The part will be placed on the machine in such a way that the part axis and
machine axis will line up.

Part Flat:
----------
The Part Flat field defines a vector that points from the center of the
tooling pin to its flat side.  The part will be placed on the machine in such a
way that the part flat and machine flat will line up.

Place Stock
-----------
The Place Stock button places the stocks and fixtures onto the machine by
lining up the Part Tooling Pin with the Machine Tooling Pin.  All defined
stocks and fixtures will be moved to the new location.

Reset Stock
-----------
The Reset Stock button removes all stocks and fixtures from the machine and
places them back in their original location.
