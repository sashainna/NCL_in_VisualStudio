#HEADER#
/TITLE/ Advanced Pocket
/POSITION/ 0,0
/SIZE/ 460,180

#SECTION#
/NAME/ Boundaries
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Perimeter
/POSITION/ 10,10
/SIZE/ 46,14
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ 
/POSITION/ 66,10,66,10
/SIZE/ 38,58
/TYPE/UD_DASSTRING
/CHOICES/ "In","Out","On","Offset"

#EDIT#
/LABEL/
/POSITION/ 120,10
/SIZE/ 98,12
/TYPE/UD_DASSTRING
/LEN/ 12
/PREC/ 64

#PUSHBUTTON#
/LABEL/ Open Sides
/POSITION/ 10,27
/SIZE/ 46,13
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Clear
/POSITION/ 65,27
/SIZE/ 35,13
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Use Perimeter Surface Inner Boundary as Island(s)
/POSITION/ 10,44
/SIZE/ 180,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Use Perimeter Surface as Bottom
/POSITION/ 10,61
/SIZE/ 140,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Island
/POSITION/ 10,78
/SIZE/ 46,14
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ 
/POSITION/ 61,78,61,78
/SIZE/ 38,57
/TYPE/UD_DASSTRING
/CHOICES/ "In","Out","On","Offset"

#EDIT#
/LABEL/
/POSITION/ 105,78
/SIZE/ 110,12
/TYPE/ UD_DASSTRING
/LEN/ 12
/PREC/ 64

#EDIT#
/LABEL/ Boundary Thick:
/POSITION/ 10,95
/SIZE/ 98,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#EDIT#
/LABEL/ Open Side Thick:
/POSITION/ 120,95
/SIZE/ 120,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8

#PUSHBUTTON#
/LABEL/
/POSITION/ 235,10
/SIZE/ 40,14
/TYPE/UD_DASSTRING
/PICTURE/ pocket1,"Select Pocket Type",0,0,100,100

#PICTUREBOX#
/FILE/ Advanced_Pocket.jpg
/NAME/ pocket1
/POSITION/ 235,10
/SIZE/ 150,125

#SECTION#
/NAME/ Levels
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Pocket Bottom
/POSITION/ 10,10
/SIZE/ 70,14
/TYPE/UD_DASSTRING
/PICTURE/ pocket2,"Pocket Bottom",38,42, 67,87

#EDIT#
/LABEL/
/POSITION/ 90,10, 90,10
/SIZE/ 121,14
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 20

#EDIT#
/LABEL/ Bottom Thick:
/POSITION/ 10,27,61,27
/SIZE/ 89,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8
/PICTURE/ pocket2,"Material to Leave on Pocket Bottom",11,73, 50,77

#CHOICEBOX#
/LABEL/ Top
/POSITION/ 10,44,27,44
/SIZE/ 65,58
/TYPE/UD_DASSTRING
/CHOICES/ "Distance","Plane"

#EDIT#
/LABEL/ 
/POSITION/ 85,44,85,44
/SIZE/ 46,13
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 10
/PICTURE/ pocket2,"Enter Top Distance",73,51, 91,57

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 140,44
/SIZE/ 40,14
/TYPE/UD_DASSTRING
/PICTURE/ pocket2,"Select Top Plane",72,26, 194,31

#PICTUREBOX#
/FILE/ Advanced_Pocket_Levels.jpg
/NAME/ pocket2
/POSITION/ 235,10
/SIZE/ 150,125

#SECTION#
/NAME/ Options
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Tool Axis:
/POSITION/ 10,10,43,10
/SIZE/ 85,58
/TYPE/UD_DASSTRING
/CHOICES/ "Same","Normal","Normal PS"
/PICTURE/ pocket3,"Fixed Tool Axis",49,36, 61,42, 0
/PICTURE/ pocket3,"Normal To Pocket Bottom",0,55, 32,65, 1

#EDIT#
/LABEL/ 
/POSITION/ 98,10,98,10
/SIZE/ 46,12
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 10

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 148,10
/SIZE/ 40,14
/TYPE/UD_DASSTRING
/PICTURE/ pocket3,"Normal To Secondary Surface",64,74, 100,84

#CHOICEBOX#
/LABEL/ End:
/POSITION/ 10,27,30,27
/SIZE/ 65,57
/TYPE/UD_DASSTRING
/CHOICES/ "Default","Element","Point"

#CHOICEBOX#
/LABEL/ Start:
/POSITION/ 10,27,30,27
/SIZE/ 65,57
/TYPE/UD_DASSTRING
/CHOICES/ "Default","Point"

#EDIT#
/LABEL/ 
/POSITION/ 85,27,85,27
/SIZE/ 46,12
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 10

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 140,27
/SIZE/ 40,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Label Pocket:
/POSITION/ 10,44
/SIZE/ 60,15
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 70,44,70,44
/SIZE/ 46,13
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 10
/PICTURE/ pocket3,"Label Pocket",4,80, 41,88

#CHECKBOX#
/LABEL/ Maximum Loops:
/POSITION/ 125,44
/SIZE/ 70,15
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 195,44,195,44
/SIZE/ 46,13
/TYPE/UD_SCAINT
/INPUT/FORM_STRING
/LEN/ 6

#CHECKBOX#
/LABEL/ Bottom Offset for Finish Passes:
/POSITION/ 10,61
/SIZE/ 114,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 130,61,130,61
/SIZE/ 38,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8
/PICTURE/ pocket3,"Finish Pass Bottom Offset", 39,65, 74,72

#CHECKBOX#
/LABEL/ Enter Off Part When Possible
/POSITION/ 10,78
/SIZE/ 110,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Off Part Thick:
/POSITION/ 121,78
/SIZE/ 120,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 7
/PICTURE/ pocket3,"Enter Off Part", 3,18, 20,30

#PICTUREBOX#
/FILE/ Advanced_Pocket_Options.jpg
/NAME/ pocket3
/POSITION/ 235,10
/SIZE/ 150,125

#CHECKBOX#
/LABEL/ Outputs for 5-axis
/POSITION/ 10,98
/SIZE/180,15
/TYPE/UD_DASSTRING

#SECTION_BAR#

#SECTION#
/NAME/ Colors
/COLOR/ *DEFAULT

#COLOR#
/LABEL/ Perimeter:
/POSITION/ 10,10, 70,10
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Islands:
/POSITION/ 125,10, 175,10
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Open Sides:
/POSITION/ 10,27, 70,27
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Line Style:
/POSITION/ 125,27,175,27
/SIZE/ 105,58
/TYPE/UD_DASSTRING
/CHOICES/ "Solid","Small Dash","Dots","Center Line","Phantom","Large Dash","Dash Dot","Dash Space"

#COLOR#
/LABEL/ Pocket Botttom:
/POSITION/ 10,44, 70,44
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Pocket Top:
/POSITION/ 125,44, 175,44
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Control Surface:
/POSITION/ 10,61, 70,61
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ End Point:
/POSITION/ 125,61, 175,61
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Unused Geometry:
/POSITION/ 10,78
/SIZE/102,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Hide","Fade"

#COLOR#
/LABEL/ Color:
/POSITION/ 125,78, 175,78
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#PICTUREBOX#
/FILE/ highlight.jpg
/NAME/ highlight
/POSITION/ 235,10
/SIZE/ 150,125

#SECTION#
/NAME/ ALL
/COLOR/ BLACK

#PUSHBUTTON#
/LABEL/ Pocket Modals
/POSITION/ 100,124
/SIZE/ 54,13
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Preview
/POSITION/ 395,10
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/FONT/ 1.
/COLOR/ BLACK, SEAGRN

#PUSHBUTTON#
/LABEL/ Apply
/POSITION/ 395,28
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, SEAGRN

#PUSHBUTTON#
/LABEL/ Reset
/POSITION/ 395,46
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, GREY

#PUSHBUTTON#
/LABEL/ Playback
/POSITION/ 395,64
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, LTTAN

#PUSHBUTTON#
/LABEL/ Verify
/POSITION/ 395,82
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, LTTAN

#PUSHBUTTON#
/LABEL/ Geometry
/POSITION/ 395,100
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, LTBLUE

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 395,118
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/COLOR/ BLACK, LTBLUE

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/apocket.mp4
/POSITION/ 395,136
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
<Boundaries>
Advanced Pocket
===============
This form controls the creation of Advanced Pocketing motion, supporting
an outer pocket boundary and islands.  The boundary geometry can be defined
by curves or by the outer and inner boundaries of trimmed surfaces.

Perimeter
---------
Pressing the Perimeter button allows you to select the outer boundaries of the
pocket.  Curves, points, or a surface can be selected as the outer perimeter.
When a surface is selected, then the outer boundary of the surface will be used
as the pocket perimeter.

You can select boundaries defining multiple pockets.  It more than one outer
perimeter is selected, then you cannot manually select the island geometry,
but if trimmed surfaces are selected for the perimeters, then the inner
boundaries of the multiple surfaces can still be used as the island geometry.

You can type the name of the boundary curve/surface into the associated text
field instead of selecting geometry interactively.  The text field will contain
the name(s) of the selected perimeter geometry when the perimeter(s) are
selected from the screen.

Direction
---------
You can specify the side of the outer perimeter of the pocket to be machined.
'In' is the default and causes the tool to remain inside the pocket perimeter.
'On' positions the tool onto the perimeter, and 'Out' positions the tool past
the pocket perimeter.

'Offset' is only valid when a trimmed surface is selected as the pocket
perimeter and causes NCL to analyze the adjacent surfaces to the selected
surface and automatically adjust the boundary for open sides and filleted
corners.  For example, if you pick the bottom surface of a pocket, which
contains fillets between the pocket bottom and the walls, then selecting
'Offset' will cause the perimeter of the pocket to be automatically adjusted
so that the walls of the pocket become the pocket boundary (when the corner
radius of the tool equals the fillet radius).  If 'In' is specified in this
case, then the pocket will not be machined all the way to the pocket walls.

The Direction modifier will apply to all pocket perimeters selected during
the active session, whether it is set prior to or after selecting the pocket
perimeters.

Open Sides
----------
Allows selecting of open boundary sides.  Open sides can be defined for
composite curves and subscripted point arrays. To select the open side:

 - Select the first point or component defining the open side
 - Select the direction that the open side will follow along the perimeter
 - Select the last point or component defining the open side

The open side will be displayed using the line style and color defined in the
Colors section.  To define additional open sides for a single entity, you can
press the Open Sides button again for each open side.

Clear
-----
Removes all defined open sides of the pocket perimeter.

Use Perimeter Surface Inner Boundary as Island(s)
-------------------------------------------------
If the perimeter surface is trimmed and has inner boundaries, a user could
choose to use the inner boundaries as pocket islands.

Use Perimeter Surface as Bottom
-------------------------------
A user could choose to use the perimeter surface as the pocket bottom. If 
chosen so, the Pocket Bottom fields become disabled.

Island
------
Pressing this button allows you to select the curves or a trimmed surface with
inner boundaries to be used as the pocket islands.

Direction
---------
You can specify the side of the island geometry of the pocket to be machined.

'Out' is the default and causes the tool to remain outside of the pocket 
islands.  'On' positions the tool onto the islands, and 'In' positions the tool 
in the pocket islands.

'Offset' will automatically determine if the inner boundaries of the selected
trimmed surface(s) should be treated as islands or holes that can be machined
over.  Inner boundaries determined to be islands will also be analyzed for
fillets connecting the island and the pocket floor, and the boundaries of these
islands will be adjusted accordingly.

Boundary Thick
--------------
This value specifies the amount of material to leave on the pocket walls.

Open Side Thick
---------------
This value specifies the distance that the tool will move machine past the 
boundary of an open-sided pocket.  A value of zero will cause the tool to stop
on the open boundary, while a positive value will cause the tool to move past
this on condition by the distance specified.
<END_SEC>
<Levels>
Levels
======
The Levels section defines the top and bottom geometry of the pocket.

Pocket Bottom
-------------
Press this button to select the pocket bottom from the screen when a trimmed
surface has not already been defined to be the pocket bottom.  The associated
text field contains the label of the selected pocket bottom or you can enter
the geometry label of the pocket bottom or nested plane to use as the pocket
bottom, i.e. (PL/0,0,1,0).

Bottom Thick
------------
Specify the amount of material to leave on the pocket bottom.  A value of zero
will machine the pocket all the way to the defined pocket bottom.

Top
---
You can specify the top of the pocket as either a fixed distance from the
bottom of the pocket or as a plane or planar surface.  The distance or plane
label/definition can be entered in the text field.

Select
------
Allows picking a plane or planar surface as the top of the pocket.  The label
of the selected geometry will be displayed in the text field.
<END_SEC>
<Options>
Tool Axis
---------
The tool axis during pocketing can be fixed, using the currently active tool
axis (Same), it can be normal to the pocket bottom (Normal), or normal to a
Secondary Part Surface, such as the road surface of a tire mold.  When a 
Secondary Part Surface is selected, the name text field will contain the name
of the controlling surface, which can either be typed in or selected from the
screen.

Select
------
Push this button to select the Secondary Part Surface used for controlling
the tool axis during pocketing.

End
---
You can select the ending location of collapsing style pocket motion.  The
ending location can be set to 'Default', which uses the internally calculated
ending location, 'Element', which will end the pocket motion near the 'nth'
element of the outer perimeter as specified by the number entered in the
associated text field, or 'Point', which defines a point where the pocket
motion will end.  When an 'Element' or 'Point' is specified, then the pocket
motion will end as near to this entity as possible, it is not guaranteed that
the pocket motion will end exactly at this location.

The text field contains the element number or point of the requested ending
location, depending on the setting of the End field.

Select
------
Allows you to select a point as a the pocket starting location.

Start
-----
This field is only visible when Lace or Scrub style motion is selected as the
machining method for the pocket.  The machining method is defined in the
Advanced Pocket Modals form.

You can either use the 'Default' starting location as calculated by the
pocket routine or you can specify a 'Point' to start the pocket motion at a
defined location.

The text field contains the lable of the requested starting location.

Select
------
Allows you to select a point as a the pocket starting location.

Label Pocket
------------
Choose whether the pocket routine is to be named and saved for processing
later in the program.    When the pocket is labeled, then no pocket motion will
be generated, but the entry points of the pocket can be obtained using the
OBTAIN/pocket-label,... command, which can then be used to predrill the entry
holes.

The commands used when labelling the pocket motion is similar to the following.

   pok1=POCKET/sf1,...         $$ Generated from this interface
   OBTAIN/pok1,n,p,UP/DOWN     $$ Get the pocket entry locations
   CALL/PDRILL,PTS=p,NPTS=n    $$ Predrill the entry holes
   POCKET/pok1                 $$ Generate the pocket motion

Currently you can only generate the labeled pocket definition using this
interface, the other commands must be typed into the program.

The text field contains the label to assign to the pocket motion.

Maximum Loops
-------------
Chooses whether to limit the number of loops generated around the pocket when
collapsing style pocket motion is generated.  If this box is not checked, then
enough loops to clear out the entire pocket will be generated.

The text field contains the maximum number of loops.

Bottom Offset for Finish Passes
-------------------------------
Enabling this check box allows you to specify the amount of extra material to
leave on pocket bottom during the final pass around the pocket perimeter and
islands.  When this option is enabled, the value is typically a small value that
keeps the tool from digging into the bottom of the pocket when finishing the
perimeters and moving along already cut areas of the pocket.

The text field contains the offset value.

Enter Off Part When Possible
----------------------------
When this check box is enabled, NCL will enter the pocket from outside the
pocket perimeter when it is able to.  The current tool position is used as the
desired start location.  The tool is repositioned to this location at the
current cutting level before each entry move and then moved at the entry
feed rate horizontally to the first calculated pocketing move.

Off Part Positioning Thick
--------------------------
Used for positioning the tool when entering from off the part through an open
side.  The thick value given is used to offset the tool from the open pocket
pocket side when beginning its entry move.
<END_SEC>
<Colors>
Colors
======
The Colors section defines the colors that will be used to highlight the
geometry selected while in this form.  All entities that can be picked from
the screen are listed in this section.

Open Sides
----------
Defines the color to highlight the selected open sides of perimeter geometry.

Line Style
----------
Defines the displayed line style for open sides of a composite curve.  The line
style will be used when displaying the components of the composite curve
perimeter geometry that have been labeled as open.

Unused Geometry:
----------------
When pressing the 'Geometry' Action item button to "hide" geometry not used
in the Goto Tool Location operation you have the option of either invisibling
this geometry (Hide) or displaying the geometry as translucent and with
dotted lines (Fade).  The Color field applies to the faded geometry.
<END_SEC>
<ALL>
Pocket Modals
-------------
Opens the Pocket Modals form allowing you to enter the Pocket Modal parameters
to use when pocketing.

Action Buttons
==============
The Action Buttons are located at the right hand of the motion generation
forms and allow you to perform specific actions that will assist you in
visualizing the results of the motion form settings.

Preview
-------
Previews the motion without writing out the command or permanently storing the
generated motion.  Press the OK or Apply button to write out the command and
motion.  The generated command can be saved after the preview even if there is
an error.  To save the command, make no changes to the settings and press the
OK button.  The command will then be available for editing in the command line.

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
section defines whether the unused geometry will be invisibled or just faded.

Pressing this button a second time will redisplay the unused geometry.

View
----
Takes down the form(s) and enters dynamic viewing.
