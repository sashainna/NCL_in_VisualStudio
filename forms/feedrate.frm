#HEADER#
/TITLE/ Feed Rate Control
/POSITION/ 0,0
/SIZE/ 310,280

#CHOICEBOX#
/LABEL/ Mode:
/POSITION/ 10,10, 60,10
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "RAPID", "FPM", "FPR"

#CHOICEBOX#
/LABEL/ Mode:
/POSITION/ 10,10, 60,10
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "RAPID", "FPM", "FPR"

#EDIT#
/LABEL/ Feed Rate:
/POSITION/ 10,27, 60,27
/SIZE/ 100,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 10

#CHOICEBOX#
/LABEL/ Control Height:
/POSITION/ 10,44, 60,44
/SIZE/ 100,40
/TYPE/ UD_DASSTRING
/CHOICES/ "LENGTH", "SCALE"

#EDIT#
/LABEL/
/POSITION/ 112,44
/SIZE/ 40,14
/TYPE/ UD_SCAVAL
/RANGE/0.0,999.999
/PREC/ 4
/LEN/ 5

#PICTUREBOX#
/FILE/ Feedrate.jpg
/NAME/ fedrat
/POSITION/ 145,10
/SIZE/ 150,125

#FRAME#
/TITLE/
/POSITION/ 5,137
/SIZE/ 290,100

#CHECKBOX#
/LABEL/ Slowdown into Corner
/POSITION/ 10,147
/SIZE/ 100,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Slowdown Distance:
/POSITION/ 10,164, 78,164
/SIZE/ 120,14
/TYPE/ UD_SCAVAL
/RANGE/0.0,9999.999
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Slowdown Feed:
/POSITION/ 10,181, 78,181
/SIZE/ 120,14
/TYPE/ UD_SCAVAL
/RANGE/0.0,9999.999
/PREC/ 4
/LEN/ 8

#CHOICEBOX#      
/LABEL/ Feed Type:
/POSITION/ 10,198, 78,198
/SIZE/ 110,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Direct", "Scale"

#CHECKBOX#
/LABEL/ Apply to Next Move Only
/POSITION/ 10,215
/SIZE/ 120,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/ Accelerate out of Corner
/POSITION/ 140,147
/SIZE/ 100,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Acceleration Distance:
/POSITION/ 140,164, 220,164
/SIZE/ 120,14
/TYPE/ UD_SCAVAL
/RANGE/0.0,9999.999
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Acceleration Feed:
/POSITION/ 140,181, 220,181
/SIZE/ 120,14
/TYPE/ UD_SCAVAL
/RANGE/0.0,9999.999
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Feed Type:
/POSITION/ 140,198, 220,198
/SIZE/ 120,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Direct", "Scale"

#CHECKBOX#
/LABEL/ Apply to Next Move Only:
/POSITION/ 140,215
/SIZE/ 90,14
/TYPE/ UD_DASSTRING

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/fedrat.mp4
/POSITION/ 240,240
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
=================
Feed Rate Control
=================
The Feed Rate Control form is used to define the cutting feed rate of the tool.

Mode:
-----
NCL supports both Feed per Minute and Feed per Spindle Revolution feed rates.
Specifying the feed rate mode as Same will leave the current feed rate mode in
effect.  Rapid causes the next motion to move in rapid mode, FPM specifies that
the feed rate is entered as Feed per Minute, and FPR specifies Feed per Spindle
Revolution.  FPM and FPR will output the corresponding mode with the FEDRAT
command (IPM,MMPM,IPR,MMPR), while Same will only output the feed rate value.

Feed Rate:
----------
Specifies the feed rate value to use for the next moves.

=================
Advanced Settings
=================

Slowdown into Corner
--------------------
Check this box if the feed rate should be lowered when approaching the check
surface.  NCL will automatically modify the feed rate for the specified
distance at the end of each move while this feature is enabled.

Slowdown Distance:
------------------
Enter the distance from the end of the move where the slowdown feed rate will
be applied.  If the move is less than this distance, then the entire move will
be made at the slowdown feed rate.

Slowdown Feed:
--------------
Enter the feed rate to use at the end of the move.  This feed rate can either
be an absolute value or factor of the main feed rate depending on the setting
of the Feed Type field.

Feed Type:
----------
You can specify either a direct value (Direct) or a factor of the main feed
rate (Scale) in the Slowdown Feed field.

Apply to Next Move Only
-----------------------
Check this box if the slowdown feed rate should only be applied to the next
motion and the subsequent motions should be made using the programmed general
feed rate for their entire length.

Control Height:
---------------
Enter the height up the tool that will be the control point for both the
slowdown and acceleration feed rates.  These feed rates will be maintained at
the specified height up the tool axis.  Specifying a control height is useful
when the tool axis is expected to change a lot during the start and/or end of
the motion.

Accelerate out of Corner
------------------------
Check this box if the feed rate should be lowered for a specified distance at
the start of the motion.  NCL will automatically modify the feed rate for the
specified distance at the beginning of each move and restore the programmed
general feed rate for the remainder of the move (unless the Slowdown into Corner
box is checked, in which case only the middle part of the motion will be at the
general feed rate.

Acceleration Distance:
----------------------
Enter the distance at the beginning of the move where the acceleration feed rate
will be applied.  If the move is less than this distance, then the entire move
will be made at the acceleration feed rate.

Acceleration Feed:
------------------
Enter the feed rate to use at the beginning of the move.  This feed rate can
either be an absolute value or factor of the main feed rate depending on the
setting of the Feed Type field.

Feed Type:
----------
You can specify either a direct value (Direct) or a factor of the main feed
rate (Scale) in the Acceleration Feed field.

Apply to Next Move Only
-----------------------
Check this box if the acceleration feed rate should only be applied to the next
motion and the subsequent motions should be made using the programmed general
feed rate for their entire length.
