#HEADER#
/TITLE/ Playback File
/POSITION/ 50,50
/SIZE/240,140

#CHOICE_LIST#
/LABEL/ Source:
/POSITION/ 10,8,40,8
/SIZE/100,60
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Options
/POSITION/ 120,8
/SIZE/45,14
/TYPE/ UD_DASSTRING

#CHOICE_LIST#
/LABEL/ Machine:
/POSITION/ 125,8
/SIZE/80,60
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ File:
/POSITION/ 10,25,40,25
/SIZE/135,14
/TYPE/ UD_DASSTRING
/PREC/ 32
/LEN/ 32

#PUSHBUTTON#
/LABEL/ Browse...
/POSITION/ 175,25
/SIZE/45,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Start:
/POSITION/ 10,42,40,42
/SIZE/80,40
/TYPE/UD_DASSTRING
/CHOICES/ "Begin","Sequnc", "Command"

#PUSHBUTTON#
/LABEL/ Sequnc:
/POSITION/ 95,42
/SIZE/35,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 135,42
/SIZE/70,14
/TYPE/ UD_DASSTRING
/PREC/ 20
/LEN/ 20

#EDIT#
/LABEL/ Command:
/POSITION/ 10,59,51,59
/SIZE/150,14
/TYPE/ UD_DASSTRING
/PREC/ 40
/LEN/ 40


#CHOICEBOX#
/LABEL/ End:
/POSITION/ 10,76,40,76
/SIZE/80,40
/TYPE/UD_DASSTRING
/CHOICES/ "End","Sequnc", "Command"

#PUSHBUTTON#
/LABEL/ Sequnc:
/POSITION/ 95,76
/SIZE/35,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 135,76
/SIZE/70,14
/TYPE/ UD_DASSTRING
/PREC/ 20
/LEN/ 20

#EDIT#
/LABEL/ Command:
/POSITION/ 10,93,51,93
/SIZE/150,14
/TYPE/ UD_DASSTRING
/PREC/ 40
/LEN/ 40

#HELP#
==================
Playback File Form
==================
The Playback File form is used to select the source and range for motion
playback in both NCL and NCLIPV.

Source:
-------
The Source field selects the clfile/simulation file that is to be used as input
for motion playback.  The following input sources can be used.

   Current    = Internal clfile being created by the active part program.
   External   = Either an external NCL clfile, Simulation file, or foriegn
                clfile.  The clfile type will be automatically determined.
   MCD        = An external MCD file.
   Simulate   = A Simulation file created automatically from the internal clfile
   Posted     = An MCD file created automatically from the internal clfile.
   APT Source = An external APT Source file.
   UG II      = An external Unigraphics II APT Source file.
   Catia V4   = An external Catia V4 binary clfile.
   Catia V5   = An external Catia V5 binary clfile.
   Mastercam  = An external Mastercam NCI file.
   Reversed   = Internal clfile after it is processed for tool path reversals.

The 'Current', 'Simulate', and 'Reversed' options are not available for
standalone NCLIPV.  'Reversed' is only an available option for internal clfiles
that have a REVERS/ON - REVERS/OFF sequence programmed.

Options
-------
This button will only be displayed when the input source is an MCD file.  It
will display the MCD Conversion Options form which allows you to input the
Pworks machine number and Pted cutter definition file.

Machine
-------
This field will only be displayed when the input source is set to Simulate and
a MACHIN/PWORKS statement is found that contains more than one machine number
(MDF file).  In this case the user may select which of the PostWorks machine
numbers to create the simulation for.

File:
-----
This field will only be enabled when the input source is an external file.  You
can enter a clfile, Simulation file, or MFD file or press the Browse... button
to bring up a file browser.

Start:
------
The Start field determines where in the source file to start motion playback.
The following starting location types can be specified.

   Begin    = Start at the beginning of the file.
   Sequnc   = Start at the specfied motion SEQUNC.
   Command  = Start at the command specified.

When the Start field is set to 'Sequnc', then the Sequnc: field will be enabled.
You can type the starting SEQUNC label here or press the Sequnc: button to
bring up a list of defined SEQUNC labels to choose from.

When 'Command' is specified, then the Command: field will be enabled, allowing
you to type in a post-processor command that marks the start of motion playback.
This command should be entered in the standard format for a post-processor
command, for example:

   LOADTL/3

End:
----
The End field determines where in the source file to end motion playback.  The
following ending location types can be specified.

   End      = End playback at the end of the file.
   Sequnc   = End after processing the specfied motion SEQUNC.
   Command  = End at the command specified.

The End Sequnc and Command fields behave in exactly the same manner as the
Start Sequnc and Command fields.
