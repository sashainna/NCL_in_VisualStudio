#HEADER#
/TITLE/ MCD Conversion Options
/POSITION/ 50,50
/SIZE/300,117

#PUSHBUTTON#
/LABEL/ Scan
/POSITION/ 10,8
/SIZE/30,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/
/POSITION/ 45,8
/SIZE/45,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current", "External"

#EDIT#
/LABEL/
/POSITION/ 95,8
/SIZE/135,14
/TYPE/ UD_DASSTRING
/PREC/ 32
/LEN/ 32

#PUSHBUTTON#
/LABEL/ Browse...
/POSITION/ 240,8
/SIZE/45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ MDF File:
/POSITION/ 10,25, 50,25
/SIZE/135,14
/TYPE/ UD_DASSTRING
/PREC/ 32
/LEN/ 32

#PUSHBUTTON#
/LABEL/ Edit
/POSITION/ 195,25
/SIZE/35,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Browse...
/POSITION/ 240,25
/SIZE/45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Cutter File:
/POSITION/ 10,42, 50,42
/SIZE/135,14
/TYPE/ UD_DASSTRING
/PREC/ 32
/LEN/ 32

#PUSHBUTTON#
/LABEL/ Edit
/POSITION/ 195,42
/SIZE/35,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Browse...
/POSITION/ 240,42
/SIZE/45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Commands:
/POSITION/ 10,59, 50,59
/SIZE/135,14
/TYPE/ UD_DASSTRING
/PREC/ 80
/LEN/ 32

#CHECKBOX#
/LABEL/ Load MCD File for Viewing:
/POSITION/ 10,76
/SIZE/100,14
/TYPE/ UD_DASSTRING

#HELP#
===========================
MCD Conversion Options Form
===========================
The MCD Conversion Options form specifies the Pworks machine number, cutter
file, and optional conversion commands to use when importing an MCD file for
motion playback.

Scan:
-----
Either the current clfile or an external clfile can be scanned to obtain the
Pworks machine number and defined cutters.  The defined cutters will be output
to the specified Cutter File.  Pressing the Scan button will perform the scan
of the specified clfile.

Browse:
-------
The Browse button brings up a file browser to locate the external clfile.

MDF File:
---------
This field contains the Pworks machine number to use for converting an MCD file
to a simulation file.  This field can be entered manually, selected by a file
browser (Browse...), or be automatically filled in by performing a clfile scan.

Edit:
-----
Brings up Mpost to view and modify the specified MDF file.

Browse:
-------
The Browse button brings up a file browser to locate the external MDF file.

Cutter File:
------------
This field contains the name of the Pted cutter file to use when converting an
MCD file to a simulation file.  This field can be entered manually or selected
by a file browser (Browse...).  If this field is left blank, then the file
'ncltemp.dat' will be created when scanning the clfile and will be automatically
deleted after loading the MCD file.  A message will still be displayed stating
that the 'ncltemp.dat' file was created.

Edit:
-----
Brings up a text editor to view and modify the specified cutter file.

Browse:
-------
The Browse button brings up a file browser to locate the external cutter file.

Commands:
---------
Optional commands can be entered into this field and will be passed to the
conversion routine for use in importing an MCD file.  For example, -SET:T(5)
will set the initial tool number register to 5.

Load MCD File for Viewing
-------------------------
Checking this box will load the actual MCD file into NCL and display it when the
source file would normally be displayed, such as displaying the source file with
motion playback and the source file list in the NCLIPV Measure form.
