#HEADER#
/TITLE/ Position Tool at Model
/POSITION/ 50,30
/SIZE/ 445,180

#SECTION#
/NAME/ Go
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/
/POSITION/ 10,10
/SIZE/ 35,40
/TYPE/UD_DASSTRING
/CHOICES/ "Enter","Exit"

#PUSHBUTTON#
/LABEL/ Drive
/POSITION/ 60,10
/SIZE/ 38,14
/TYPE/ UD_DASSTRING
/PICTURE/ go1,"Drive Surface",50,40, 70,60

#CHOICEBOX#
/LABEL/
/POSITION/ 110,10
/SIZE/ 35,40
/TYPE/UD_DASSTRING
/CHOICES/ "To","On","Past"

#EDIT#
/LABEL/
/POSITION/ 155,10
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#CHECKBOX#
/LABEL/ NOPS
/POSITION/ 10,27
/SIZE/ 35,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Part
/POSITION/ 60,27
/SIZE/ 38,14
/TYPE/ UD_DASSTRING
/PICTURE/ go1,"Part Surface",25,70, 60,85

#CHOICEBOX#
/LABEL/
/POSITION/ 110,27
/SIZE/ 35,40
/TYPE/UD_DASSTRING
/CHOICES/ "To","On"

#EDIT#
/LABEL/
/POSITION/ 155,27
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Check
/POSITION/ 60,44
/SIZE/ 38,14
/TYPE/ UD_DASSTRING
/PICTURE/ go1,"Check Surface",80,50, 100,70

#CHOICEBOX#
/LABEL/
/POSITION/ 110,44
/SIZE/ 35,40
/TYPE/UD_DASSTRING
/CHOICES/ "To","On","Past"

#EDIT#
/LABEL/
/POSITION/ 155,44
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#CHOICEBOX#
/LABEL/ Initial Direction:
/POSITION/ 10,65, 60,65
/SIZE/ 105,40
/TYPE/UD_DASSTRING
/CHOICES/ "Current","From Tool","Toward Part"

#EDIT#
/LABEL/
/POSITION/ 120,65
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 170,65
/SIZE/ 38,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Tool Axis:
/POSITION/ 10,83
/SIZE/ 75,14
/TYPE/ UD_DASSTRING
/LEN/ 20
/PREC/ 80

#CHECKBOX#
/LABEL/
/POSITION/ 145,83
/SIZE/ 10,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Advanced
/POSITION/ 160,83
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Feed Rate:
/POSITION/ 10,101
/SIZE/ 80,40
/TYPE/UD_DASSTRING
/CHOICES/ "Current","Rapid"

#EDIT#
/LABEL/
/POSITION/ 95,101
/SIZE/ 30,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 6

#CHECKBOX#
/LABEL/
/POSITION/ 130,101
/SIZE/ 15,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Advanced
/POSITION/ 145,101
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Contact Mode:
/POSITION/ 10,119
/SIZE/ 85,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","On"

#PICTUREBOX#
/FILE/ Go.jpg
/NAME/ go1
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ To Side
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Drive Surface Side
/POSITION/ 10,10
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/PICTURE/ go_side,"Drive Surface Side",45,30, 70,48

#EDIT#
/LABEL/
/POSITION/ 95,10
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/LEN/ 20
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Check Surface Side
/POSITION/ 10,27
/SIZE/ 80,14
/TYPE/ UD_DASSTRING
/PICTURE/ go_side,"Check Surface Side",65,50, 100,65

#EDIT#
/LABEL/
/POSITION/ 95,27
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/LEN/ 20
/PREC/ 80

#PICTUREBOX#
/FILE/ Go_To_Side.jpg
/NAME/ go_side
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Thicks
/COLOR/ DEFAULT

#EDIT#
/LABEL/ Drive Surface Thick:
/POSITION/ 10,10, 90,10
/SIZE/ 60,14
/TYPE/ UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ go3,"Drive Surface Thick",70,20, 100,30

#EDIT#
/LABEL/ Part Surface Thick:
/POSITION/ 10,27, 90,27
/SIZE/ 60,14
/TYPE/ UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ go3,"Part Surface Thick",30,75, 60,85

#EDIT#
/LABEL/ Check Surface Thick:
/POSITION/ 10,44, 90,44
/SIZE/ 60,14
/TYPE/ UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ go3,"Check Surface Thick",80,50, 100,65

#PICTUREBOX#
/FILE/ Mo_Thick.jpg
/NAME/ go3
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Position
/COLOR/ DEFAULT

#EDIT#
/LABEL/ Drive Entry Offset:
/POSITION/ 10,10, 95,10
/SIZE/ 60,14
/TYPE/ UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ go_pos,"Drive Entry Offset",78,71, 92,85

#EDIT#
/LABEL/ Part Entry Offset:
/POSITION/ 10,27, 95,27
/SIZE/ 60,14
/TYPE/ UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ go_pos,"Part Entry Offset",60,75, 75,95

#EDIT#
/LABEL/ Check Entry Offset:
/POSITION/ 10,44, 95,44
/SIZE/ 60,14
/TYPE/ UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ go_pos,"Check Entry Offset",75,55, 100,70

#PUSHBUTTON#
/LABEL/ Position Above Part
/POSITION/ 10,61
/SIZE/ 75,14
/TYPE/ UD_DASSTRING
/PICTURE/ go_pos,"Position Above Part",40,30, 60,40

#EDIT#
/LABEL/
/POSITION/ 95,61
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/LEN/ 16
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Position Above Entry
/POSITION/ 10,78
/SIZE/ 75,14
/TYPE/ UD_DASSTRING
/PICTURE/ go_pos,"Position Above Entry",45,50, 70,60

#EDIT#
/LABEL/
/POSITION/ 95,78
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/LEN/ 16
/PREC/ 80

#PICTUREBOX#
/FILE/ Go_Position.jpg
/NAME/ go_pos
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION_BAR#

#SECTION#
/NAME/ Colors
/COLOR/ *DEFAULT

#COLOR#
/LABEL/ Drive Surface:
/POSITION/ 10,10, 70,10
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Part Surface:
/POSITION/ 125,10, 175,10
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Check Surface:
/POSITION/ 10,27, 70,27
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Above Part:
/POSITION/ 10,44, 70,44
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Above Entry:
/POSITION/ 125,44, 175,44
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Unused Geometry:
/POSITION/ 10,61
/SIZE/102,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Hide","Fade"

#COLOR#
/LABEL/ Color:
/POSITION/ 125,61, 175,61
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#PICTUREBOX#
/FILE/ highlight.jpg
/NAME/ highlight
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ ALL
/COLOR/ BLACK

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 380,10
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/FONT/ 1.
/COLOR/ BLACK, SEAGRN

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 380,28
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, SEAGRN

#PUSHBUTTON#
/LABEL/ Reset
/POSITION/ 380,46
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, GREY

#PUSHBUTTON#
/LABEL/ Playback
/POSITION/ 380,64
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, LTTAN

#PUSHBUTTON#
/LABEL/ Verify
/POSITION/ 380,82
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, LTTAN

#PUSHBUTTON#
/LABEL/ Geometry
/POSITION/ 380,100
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, LTBLUE

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 380,118
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, LTBLUE

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/go.mp4
/POSITION/ 380,136
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
<Go>
Position Tool at Model
======================
This form is used to position the tool at the model prior to the start of
cutting motion.

Enter / Exit
------------
Select whether the positioning move will be an entry or exit type move.  An
entry move will pre-position the tool prior to moving to the final position
at the model.  An exit move is used to position the tool off the part at the
end of cutting motion.

If the Drive, Part, and Check fields are blank, then selecting Exit will
automatically fill in these fields with the current geometry labels.

Drive
-----
Press the Drive button to select the drive geometry to position the tool at.
If the Part and Check fields are blank, then you will be prompted to select
the part and check geometries after selecting the drive geometry.

To / On / Past
--------------
Select the tool condition for the drive geometry.  To will position the tool
to the geometry based on the current position of the tool or based on the Drive
Surface Side defined in the 'To Side' section.  On will position the tool on
the drive geometry, and Past will position the tool past the drive geometry
based on the current position of the tool.

(Text Field)
------------
This field will display the label of the selected drive geometry.  You can also
manually type the label of the geometry in this field.

NOPS
----
Checking the NOPS box will output a NOPS command prior to the GO command,
which causes the tool to move in the closest direction to the drive geometry.
When this box is checked, you will not be able to select the Part and Check
geometry.

Part
----
Press the Part button to select the part geometry to position the tool at.

To / On
-------
The bottom of the tool can be positioned To the part geometry or On the part
geometry.  Unless you are engraving along a curve, you will select To the part
geometry.

(Text Field)
------------
This field will display the label of the selected part geometry.  You can also
manually type the label of the geometry in this field.

Check
-----
Press the Check button to select the check geometry to position the tool at.

To / On / Past
--------------
Select the tool condition for the check geometry.  To will position the tool
to the geometry based on the current position of the tool or based on the Check
Surface Side defined in the 'To Side' section.  On will position the tool on
the check geometry, and Past will position the tool past the check geometry
based on the current position of the tool.

(Text Field)
------------
This field will display the label of the selected check geometry.  You can also
manually type the label of the geometry in this field.

Initial Direction
-----------------
The tool forward vector should be in the direction that you want the tool to
move when positioning to the model.  The Current forward vector will be
displayed at the tool end when this form is first displayed.  If this is not
the desired direction then you can select a vector at 90, 180, or 270 degrees
from the current forward direction by toggling this field to 'From Tool'.
These vectors will be displayed at then end of the tool so that you can select
one.

You can also define the forward vector by selecting a point on the part by
toggling this field to 'Toward Part'.  Simply pick a location on the part where
you want the tool to be positioned at.

(Text Field)
------------
This field will display the defined forward vector.  You can also manually
type in the label of a vector or the components of the desired forward vector
in this field.

Select
------
Pressing the Select button allows you to select the forward vector or part
location if you want to change it after the initial selection.

Tool Axis
---------
This field contains the current tool axis.  For a fixed tool axis you can
manually type in the label of a vector or the desired components of the tool
axis.

Advanced
--------
Checking this box allows you to define multi-axis Tool Axis modes.  Pressing the
Advanced button will bring up the Tool Axis Settings form, giving you access
to all of the supported tool axis modes.

Feed Rate:
----------
The feed rate used to move the tool to the specified location(s) can remain the
Same as the current feed rate, the move can be moved in Rapid mode, or you can
specify a new feed rate Value for this move. Specifying the feed rate mode as
Same will leave the current feed rate mode in effect. Rapid causes the next
motion to move in rapid mode. 

If Value is selected the Mode selection box is activated, check this box to
select either FPM (Feed per Minute) or FPR (Feed per Spindle Revolution).

Mode:
-----
NCL supports both Feed per Minute and Feed per Spindle Revolution feed rates.
FPM and FPR will output the corresponding mode with the FEDRAT command.
(IPM,MMPM,IPR,MMPR).

<END_SEC>
<To Side>
To Side
=======
The To Side section allows you to specify the side of the drive and check
geometries to position the tool at.

Drive Surface Side
------------------
Press this button to select a point on the drive geometry to position the tool
at.  If a wireframe entity is used as the drive geometry, then two vectors will
be displayed on the geometry for you to select from.  When a surface is used
as the drive geometry simply pick the side of the surface you want the tool to
position to.

Check Surface Side
------------------
Press this button to select a point on the check geometry to position the tool
at.  If a wireframe entity is used as the check geometry, then two vectors will
be displayed on the geometry for you to select from.  When a surface is used
as the check geometry simply pick the side of the surface you want the tool to
position to.
<END_SEC>
<Thicks>
Thicks
======

Drive Surface Thick:
--------------------
Enter the distance to offset the tool from the drive surface.

Part Surface Thick:
-------------------
Enter the distance to offset the tool from the part surface.

Check Surface Thick:
--------------------
Enter the distance to offset the tool from the check surface.
<END_SEC>
<Position>
Position
========
The Position section allows you to define the entry/exit moves that will occur
when positioning the tool at the model or at the end of a cutting sequence. The
following tool motion when entry/exit moves are specified will be performed to
complete the entry/exit motion.

   Entry Move			Exit Move
   ----------			---------
   Position Above the Part		Position at the Part
   Position Above the Offset Location	Position to the Offset location
   Move to the Offset Location	Position Above the Offset location
   Position at the Part		Position Above the Part

Drive Entry Offset:
-------------------
Enter a distance to position the tool at in relation to the drive geometry when
entering/exiting the part.  This defines the Offset location for the drive
geometry.

Part Entry Offset:
------------------
Enter a distance to position the tool at in relation to the part geometry when
entering/exiting the part.  This defines the Offset location for the part
geometry.

Check Entry Offset:
-------------------
Enter a distance to position the tool at in relation to the check geometry when
entering/exiting the part.  This defines the Offset location for the check
geometry.

Position Above Part
-------------------
This field defines the tool retraction position for the entry/exit move.  For
entry moves, the tool will be positioned at this location prior to any other
entry moves.  For exit moves, the tool will be retracted to this location at
the end of the exit motion.

You can type in the label of a plane or surface, or type in a distance that the
tool will retract from the Offset location, or type in delta-x,delta-y,delta-z
values to retract the tool from the Offset location.  You can also select a
plane or surface from the screen by pressing this button.

Position Above Entry
--------------------
This field defines the tool entry/exit position for the entry/exit move.  For
entry moves, the tool will be positioned at this location prior to moving to
the Offset location.  For exit moves, the tool will be retracted to this
location after moving to the Offset location.

You can type in the label of a plane or surface, or type in a distance that the
tool will retract from the Offset location, or type in delta-x,delta-y,delta-z
values to retract the tool from the Offset location.  You can also select a
plane or surface from the screen by pressing this button.
<END_SEC>
<Colors>
Colors
======
The Colors section defines the colors that will be used to highlight the
geometry selected while in this form.  All entities that can be picked from
the screen are listed in this section.

Unused Geometry:
----------------
When pressing the 'Geometry' Action item button to "hide" geometry not used
in the Goto Tool Location operation you have the option of either invisible
this geometry (Hide) or displaying the geometry as translucent and with
dotted lines (Fade).  The Color field applies to the faded geometry.
<END_SEC>
<ALL>
Action Buttons
==============
The Action Buttons are located at the right hand of the motion generation
forms and allow you to perform specific actions that will assist you in
visualizing the results of the motion form settings.

Preview
-------
Previews the Position Tool at Model motion without writing out the command or
permanently storing the generated motion.  Press the OK or Apply button to
write out the command and motion.  The generated command can be saved after the
preview even if there is an error.  To save the command, make no changes to the
settings and press the OK button.  The command will then be available for
editing in the command line.

Apply
-----
Outputs and processes the command(s) without taking down the form so that
other motion can be created.

Reset Motion
------------
Resets all form fields to their settings/values when the form was first entered.
This button is useful after pressing the Apply button if you want to start
fresh or when you have made numerous changes to the form settings and are not
getting the output you desire.

Playback
--------
Displays Playback Preview Motion interface, allowing you to step through and
animate the motion generated using the Preview button.  This button is only
active when Preview motion is displayed on the screen.

Verify
------
Pressing this button allows you to verify the Preview motion by using the
material removal process of NCL/IPV.  It displays the Verify Preview motion
interface, allowing you to simulate the material removal for the Preview motion.
This button is only active when you have a valid NCL/IPV license and Preview
motion is displayed on the screen.

Geometry
--------
Pressing the Geometry button the first time will hide all unused geometry from
the screen, leaving only the geometry that was selected during this session 
displayed in the selected colors.  The 'Unused Geometry' field in the 'Colors'
section defines whether the unused geometry will be invisible or just faded.

Pressing this button a second time will redisplay the unused geometry.

View
----
Takes down the form(s) and enters dynamic viewing.
