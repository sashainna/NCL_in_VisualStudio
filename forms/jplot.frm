#HEADER#
/TITLE/ Create Plot
/POSITION/ 0,0
/SIZE/ 260,160

#CHOICEBOX#      
/LABEL/ Plot Type:  
/POSITION/ 10,8,58,8
/SIZE/ 115,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Drawing", "Quick"

#CHOICEBOX#
/LABEL/ Plot Output:
/POSITION/ 10,25,58,25
/SIZE/ 115,40
/TYPE/ UD_DASSTRING
/CHOICES/ "PL Files", "Plot File", "Plotter", "Print Queue"

#COMBLIST_SIMPLE#
/LABEL/ Plot Drawing:
/POSITION/10,41,58,41
/SIZE/170,48
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Plot File Name: 
/POSITION/ 10,95,70,95
/SIZE/ 180,14
/TYPE/ UD_DASSTRING
/PREC/ 32
/LEN/ 32

#PUSHBUTTON#
/LABEL/ Browse...
/POSITION/ 205,95
/SIZE/ 40,14
/TYPE/ UD_DASINT

#EDIT#
/LABEL/ Setup File Name:
/POSITION/ 10,113,70,113
/SIZE/ 180,14
/TYPE/ UD_DASSTRING
/PREC/ 32
/LEN/ 32

#PUSHBUTTON#
/LABEL/ Browse...
/POSITION/ 205,113
/SIZE/ 40,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Options...
/POSITION/ 135,25
/SIZE/ 50,14
/TYPE/ UD_DASINT

#HELP#
================
Create Plot Form
================
This form is used to create a plot from a scaled drawing or a quick plot of the
screen.  The plot output can be saved to a disk file or sent directly to the
printer/plotter.

Plot Type:
----------
Select Drawing to plot a scaled drawing stored in the active Unibase.  A Quick
plot will create a plot of the graphics area as it is currently displayed.

Plot Output:
------------
This field determines where the created plot will be directed for output.  You
can output PL files (.pl1,pl2), which are used for input to the NCL/Plot
routine or an actual Plot File that contains raw data that will be sent to the
plotter/printer.  You can also send the output directly to the Plotter port
on Unix systems or to a Print Queue on both Windows and Unix systems.  Creating
output other than PL files will automatically send the data to the NCL/Plot
routine.

Options
-------
When directing the output to the NCL/Plot routine (Plot File, Plotter, Print
Queue), the Options button can be pressed to bring up various options used by
NCL/Plot.  This options are described in the Plotter Options form.

Plot Drawing:
-------------
This field contains a list of all available drawings when the Plot Type field is
set to Drawing.  You can simply select a drawing from this list to plot or type
in the name of the drawing.

Plot File Name:
---------------
This field is only active when PL Files or a Plot File is being created.  It
contains the name of the output plot file.  If PL Files are being created, then
simply input the filename of the output file you wish to create without a file
extension or with a file extension of '.pl1'.

Browse
------
Pressing this button opens a file browser that allows you to select an input
Plot File.

Setup File Name:
----------------
The Setup File is a setup file created by the Modify Plot Setup File menu that
contains information about the plotter.  This file contains information on the
plotter and pen descriptions and is usually only used for actual pen plotters.
If you are creating output for a printer, then this file is not necessary and
you can leave this file name blank.

Browse
------
Pressing this button opens a file browser that allows you to select an input
Setup File.
