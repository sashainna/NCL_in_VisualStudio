#HEADER#
/TITLE/ Plotter Options
/POSITION/ 50,50
/SIZE/200,165

#LABEL#
/LABEL/ Press to Cancel Changes to Options:
/POSITION/ 10,8
/SIZE/ 40,14

#CHOICEBOX#
/LABEL/ Plotter:
/POSITION/ 15,32,65,32
/SIZE/100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "HP 7475", "HP 7580", "PostScript","Calcomp 1043"

#CHOICEBOX#      
/LABEL/ Plotting Size:
/POSITION/ 15,49,65,49
/SIZE/100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "AH", "AV","B","C","D","E","F","A0","A1", "A2",
/CHOICES/ "A3","A4","USER1","USER2","USER3","USER4"

#EDIT#
/LABEL/ Port:
/POSITION/ 15,66,65,66
/SIZE/120,14
/TYPE/ UD_DASSTRING
/PREC/ 12
/LEN/ 12

#EDIT#
/LABEL/Print Que:
/POSITION/ 15,83,65,83
/SIZE/120,14
/TYPE/ UD_DASSTRING
/PREC/ 12
/LEN/ 12

#EDIT#
/LABEL/ Line Width:
/POSITION/ 15,100,65,100
/SIZE/120,14
/TYPE/ UD_DASUNITLESS
/PREC/ 4
/LEN/ 10

#CHOICEBOX#
/LABEL/ Bypass Mode:
/POSITION/ 15,117,65,117
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "OFF","ON"

#FRAME#
/TITLE/ 
/POSITION/ 8,22
/SIZE/ 180,115

#PUSHBUTTON#
/LABEL/ Cancel
/POSITION/ 135,8
/SIZE/ 50,14
/TYPE/ UD_DASINT

#HELP#
===============
Plotter Options
===============
This form is used for setting the plotter options when outputting directly to
a plotter or printer.

Press to Cancel Changes to Options
----------------------------------
Pressing the Cancel button will close the form and not save any changes made
to the plotting options.  Press the Close button to accept the changes.

Plotter
-------
Select the plotter type that will be used to create the plot.

Plotting Size
-------------
Select the paper size to create the plot for.  Standard paper sizes are
included along with four user defined sizes.  The user defined paper sizes are
defined using the following environmental variables.

      Dplot_USERn_X = x
      Dplot_USERn_Y = y

   Where:

      plot  = The plotter this size of paper is being defined for.  Valid
              plotter types are 1043, 7475, 7580, and PS. 

      n     = The user paper size being defined in the range 1-4.

      x     = The paper size along the X direction.

      y     = The paper size along the Y direction.

   For example, DPS_USER1_X = 10.5


Port
----
The Port field contains the name of the output port (usually a serial or
parallel) port that the plotter is connected to.  This field is only used when
outputting directly to a plotter on a Unix system.

Print Que
---------
This field specifies the print queue to use when using a print que to output the
plot file on a Unix system.  On Windows systems, a print form will be displayed
that is used to specify the print queue.

Line Width
----------
This field specifies the default line width scale factor for PostScript output
and is used to compensate for the different resolutions of PostScript
printers.

Bypass Mode
-----------
Bypass Mode is only used by Hewlett Packard plotters when they are connected
to a serial line with another device connected to the plotter.  Enabling this
mode causes NCL/PLOT to output special codes that tell the plotter that the
data is meant for them and then to disable the plotter so that the device
connected to the plotter can be communicated with.  This field is only used
with the HP 7475 and HP 7580 plotters when outputting directly to a
communications port on Unix systems.
