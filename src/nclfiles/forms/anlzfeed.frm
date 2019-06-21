#HEADER#
/TITLE/ Feed Rate Analyzation
/POSITION/ 0,0
/SIZE/ 160,260

#COLOR#      
/LABEL/ Geometry Color:
/POSITION/ 10,8,65,8
/SIZE/ 110,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Style:
/POSITION/ 10,25,35,25
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Default", "Solid", "Small Dash", "Dots", "Center Line", "Phantom"
/CHOICES/ "Large Dash", "Dash Dot", "Dash Space"

#EDIT#
/LABEL/ 1.
/POSITION/ 10,59,25,59
/SIZE/ 60,14
/TYPE/ UD_DASVAL
/RANGE/0.,100000.
/PREC/ 3
/LEN/ 12

#COLOR#      
/LABEL/
/POSITION/ 90,59
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ 2.
/POSITION/ 10,76,25,76
/SIZE/ 60,14
/TYPE/ UD_DASVAL
/RANGE/0.,100000.
/PREC/ 3
/LEN/ 12

#COLOR#      
/LABEL/
/POSITION/ 90,76
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ 3.
/POSITION/ 10,93,25,93
/SIZE/ 60,14
/TYPE/ UD_DASVAL
/RANGE/0.,100000.
/PREC/ 3
/LEN/ 12

#COLOR#      
/LABEL/
/POSITION/ 90,93
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ 4.
/POSITION/ 10,110,25,110
/SIZE/ 60,14
/TYPE/ UD_DASVAL
/RANGE/0.,100000.
/PREC/ 3
/LEN/ 12

#COLOR#      
/LABEL/
/POSITION/ 90,110
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ 5.
/POSITION/ 10,127,25,127
/SIZE/ 60,14
/TYPE/ UD_DASVAL
/RANGE/0.,100000.
/PREC/ 3
/LEN/ 12

#COLOR#      
/LABEL/
/POSITION/ 90,127
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ 6.
/POSITION/ 10,144,25,144
/SIZE/ 60,14
/TYPE/ UD_DASVAL
/RANGE/0.,100000.
/PREC/ 3
/LEN/ 12

#COLOR#      
/LABEL/
/POSITION/ 90,144
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ 7.
/POSITION/ 10,161,25,161
/SIZE/ 60,14
/TYPE/ UD_DASVAL
/RANGE/0.,100000.
/PREC/ 3
/LEN/ 12

#COLOR#      
/LABEL/
/POSITION/ 90,161
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ 8.
/POSITION/ 10,178,25,178
/SIZE/ 60,14
/TYPE/ UD_DASVAL
/RANGE/0.,100000.
/PREC/ 3
/LEN/ 12

#COLOR#      
/LABEL/
/POSITION/ 90,178
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default


#EDIT#
/LABEL/ 9.
/POSITION/ 10,195,25,195
/SIZE/ 60,14
/TYPE/ UD_DASVAL
/RANGE/0.,100000.
/PREC/ 3
/LEN/ 12

#COLOR#      
/LABEL/
/POSITION/ 90,195
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#EDIT#
/LABEL/ 10.
/POSITION/ 10,212,25,212
/SIZE/ 65,14
/TYPE/ UD_DASVAL
/RANGE/0.,100000.
/PREC/ 3
/LEN/ 12

#COLOR#      
/LABEL/
/POSITION/ 90,212
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/CHOICES/ Default

#LABEL#
/LABEL/ Feed Rate
/POSITION/ 36,46
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#LABEL#
/LABEL/ Color
/POSITION/ 100,46
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
#HELP#
==========================
Feed Rate Analyzation Form
==========================
The Feed Rate Analyzation form sets the colors and line types to display motion
in based on the feed rate value when playing back motion in NCL with Analyzation
set to Feed Rate. 

NCLIPV will also use the color scheme defined in this form for displaying cuts
when Analyzation is set to Feed Rate during NCLIPV motion playback.  The
geometry attributes and line styles are ignored in NCLIPV.

The 'Default' settings will use the default geometry/motion colors for the
geometry or specified feed rate range.  'Black' will display the motion
as invisible.

Geometry:  
---------
Defines the color and line style attributes to display the model with during
motion playback.  All geometry in the model will be displayed using these
attributes during motion playback.  The original attributes for each geometry
will be restored when motion playback is exited.

Selecting any other color besides Default for geometry will display all
geometry in that color.  Shaded geometry will be transparent, in that the
motion displayed behind the geometry will show through the geometry along with
any other geometry that is defined.

Feed Rate:
----------
Defines feed rate ranges that control the color of the displayed motion.
Whenever the feed rate is less than or equal to the feed rate, then the
corresponding color will be used to display the motion.  Any programmed feed
rate that is greater than the highest feed rate specified in this form will be
displayed using the color assigned to the highest feed rate.

Color:    
------
Defines the color used to display the motion within the corresponding feed
rate range.
