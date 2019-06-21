#HEADER#
/TITLE/ Contouring111
/POSITION/ 50,30
/SIZE/ 445,200

#SECTION#
/NAME/ Contour
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Part Surface
/POSITION/ 10,10
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/PICTURE/ contour1,"Part Surface",25,70, 53,80

#CHOICEBOX#
/LABEL/
/POSITION/ 80,10
/SIZE/ 40,40
/TYPE/UD_DASSTRING
/CHOICES/ "Tlofps","Tlonps","Autops","Nops"

#EDIT#
/LABEL/
/POSITION/ 130,10
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Drive Surface
/POSITION/ 10,27
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/PICTURE/ contour1,"Drive Surface",68,39, 88,50

#CHOICEBOX#
/LABEL/
/POSITION/ 80,27
/SIZE/ 40,40
/TYPE/UD_DASSTRING
/CHOICES/ "Tllft","Tlon","Tlrgt"

#EDIT#
/LABEL/
/POSITION/ 130,27
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Check
/POSITION/ 10,44
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
/PICTURE/ contour1,"Check Surface",50,20, 68,35

#CHOICEBOX#
/LABEL/
/POSITION/ 80,44
/SIZE/ 40,40
/TYPE/UD_DASSTRING
/CHOICES/ "To","On","Past", "Tanto", "Pstan"

#EDIT#
/LABEL/
/POSITION/ 130,44
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#CHECKBOX#
/LABEL/ Use Initial DS as Final DS
/POSITION/ 10,61
/SIZE/ 100,14
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ Drive Style:
/POSITION/ 120,61
/SIZE/ 85,40
/TYPE/UD_DASSTRING
/CHOICES/ "Auto","Explicit","Implied"

#CHOICEBOX#
/LABEL/ Initial Direction:
/POSITION/ 10,88, 60,88
/SIZE/ 105,40
/TYPE/UD_DASSTRING
/CHOICES/ "Current","From Tool","Toward Part"

#EDIT#
/LABEL/
/POSITION/ 120,88
/SIZE/ 70,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 170,88
/SIZE/ 38,14
/TYPE/ UD_DASSTRING
/PICTURE/ contour1,"Initial Direction",65,70, 85,84

#EDIT#
/LABEL/ Tool Axis:
/POSITION/ 10,105
/SIZE/ 75,14
/TYPE/ UD_DASSTRING
/LEN/ 20
/PREC/ 80

#CHECKBOX#
/LABEL/
/POSITION/ 145,105
/SIZE/ 10,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Advanced
/POSITION/ 160,105
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Feed Rate
/POSITION/ 10,122
/SIZE/ 75,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHECKBOX#
/LABEL/
/POSITION/ 145,122
/SIZE/ 10,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Advanced
/POSITION/ 160,122
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ Contour.jpg
/NAME/ contour1
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Options
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Geometry Extensions:
/POSITION/ 10,10
/SIZE/ 130,40
/TYPE/UD_DASSTRING
/CHOICES/ "Ignore","Respect"

#CHOICEBOX#
/LABEL/ Look Ahead:
/POSITION/ 10,27
/SIZE/ 75,40
/TYPE/UD_DASSTRING
/CHOICES/ "1","2","3","4","5"

#CHOICEBOX#
/LABEL/ Contact Mode:
/POSITION/ 10,44
/SIZE/ 85,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","On"

#LABEL#
/LABEL/ Motion Calculation Settings:
/POSITION/ 10,61
/SIZE/ 90,14

#PUSHBUTTON#
/LABEL/ Assist
/POSITION/ 105,61
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PICTURE/ contour2,"Motion Calculation Assistance",70,65, 88,75

#LABEL#
/LABEL/ Corner Rounding:
/POSITION/ 10,78
/SIZE/ 90,14

#PUSHBUTTON#
/LABEL/ Fillet Settings
/POSITION/ 105,78
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PICTURE/ contour2,"Corner Rounding",70,45, 90,60

#PICTUREBOX#
/FILE/ Contour_Options.jpg
/NAME/ contour2
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Conditions
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Direction:
/POSITION/ 10,10
/SIZE/ 80,40
/TYPE/UD_DASSTRING
/CHOICES/ "Gofwd","Golft","Gorgt","Goback","Goup","Godown"
/PICTURE/ contour3,"Gofwd DS",7,88, 23,97,0
/PICTURE/ contour3,"Golft DS",52,59, 64,66,1
/PICTURE/ contour3,"Gorgt DS",30,69, 43,77,2

#EDIT#
/LABEL/ Drive:
/POSITION/ 110,10
/SIZE/ 75,14
/TYPE/ UD_DASSTRING
/LEN/ 15
/PREC/ 64

#CHOICEBOX#
/LABEL/ CS Condition:
/POSITION/ 10,27
/SIZE/ 85,40
/TYPE/UD_DASSTRING
/CHOICES/ "Auto","To","On","Past","Tanto","Pstan"
/PICTURE/ contour3,"To CS",14,66, 23,73,1
/PICTURE/ contour3,"On CS",80,4, 87,11,2
/PICTURE/ contour3,"Past CS",56,71, 66,78,3
/PICTURE/ contour3,"Tanto CS",34,42, 48,49,4

#EDIT#
/LABEL/ Intersection:
/POSITION/ 110,27
/SIZE/ 75,14
/TYPE/ UD_SCAVAL
/PREC/ 0
/LEN/ 4
/PICTURE/ contour3,"CS Intersection",68,37, 100,44

#PUSHBUTTON#
/LABEL/ Near Point
/POSITION/ 10,44
/SIZE/ 50,14
/TYPE/ UD_DASSTRING
/PICTURE/ contour3,"CS Near Point",65,12, 80,20

#EDIT#
/LABEL/
/POSITION/ 70,44
/SIZE/ 75,14
/TYPE/ UD_DASSTRING
/LEN/ 20
/PREC/ 64

#LISTTABLE#
/SORT/ *OFF
/FILTER/ *OFF
/LABEL/
/POSITION/ 10,61
/SIZE/ 200,85
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Delete
/POSITION/ 10,146
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ To End
/POSITION/ 70,146
/SIZE/ 40,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Insert
/POSITION/ 120,146
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ After
/POSITION/ 180,146
/SIZE/ 40,14
/TYPE/ UD_DASINT

#PICTUREBOX#
/FILE/ contour_Conditions.jpg
/NAME/ contour3
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION_BAR#

#SECTION#
/NAME/ Colors
/COLOR/ *DEFAULT

#COLOR#
/LABEL/ Part Surface:
/POSITION/ 10,10, 70,10
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Drive Surface:
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
/LABEL/ Current DS:
/POSITION/ 10,44, 70,44
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Current CS:
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
/FILE/contour.mp4
/POSITION/ 380,136
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
<Contour>
Contour
=======
This form is used to drive the tool along both surface and wireframe geometry
utilizing the Part, Drive, Check surface logic.  The motion can be created
using the contouring statement (GOFWDA) or continuous path statements (GOFWD,
GOLFT,GORGT,etc.) with either explicit or implied check surfaces.

Part Surface
------------
The Part Surface is used to control the bottom of the cutter.  Press the Part
Surface button to select the geometry to use as the part surface.  A single
plane or surface, multiple surfaces,  or a surface-spline can be selected as
the part surface.

(Part Surface Condition)
------------------------
Select the proper tool orientation for the part surface.  Typically the tool
will be positioned off of (touching) the part surface (Tlofps).  You can also
specify the center of the tool as being on the part surface (Tlonps).  Autops
will create the part surface as a plane going through the current tool end
point and perpendicular to the current tool axis.  Nops is only used when
driving a single curve and is used to drive a 3-D curve in space.

(Text Field)
------------
This field will display the label of the selected part geometry.  You can also
manually type the label of the geometry in this field.

Drive Surface
-------------
Press the Drive Surface button to select the drive geometry.  Multiple
entities can be selected, but they must be selected in the order that the tool
will contact them (the order in which they are driven).  If the Check Surface
field is blank, then you will be prompted to select the check geometry after
selecting the drive geometry, if one is required.

(Drive Surface Condition)
-------------------------
Select the tool condition for the drive geometry.  The tool can be positioned
on the geometry (Tlon), to the left of the geometry (Tllft), or to the right
of the geometry (Tlrgt).  The tool condition is based on the forward direction
of the tool.

(Text Field)
------------
This field will display the label of the selected drive geometry.  You can also
manually type the label of the geometry in this field.

Check Surface
-------------
Press this button to select the final check surface.  Up to five check surfaces
can be selected when the Drive Style is set to Auto, otherwise only a single
check surface can be selected.

(Check Surface Condition)
-------------------------
The tool position for the final check surface is specified in this field.  The
tool can stop To, On, Past, or Tangent to the final check surface or it can
stop at a surface that is tangent to the current part surface (Pstan).  The
tool conditions for intermediate drive/check surfaces can be automatically
calculated by the Contour routine when the Drive Style is set to Auto or they
can be manually set when specifying Implicit or Explicit check surfaces in the
Conditions section of this form.

(Text Field)
------------
This field will display the label of the final check geometry.  You can also
manually type the label of the geometry in this field.

Use Initial DS as Final DS
--------------------------
It is sometimes required that the initial drive surface be used as the last
drive surface, for example when machining completely around a part and starting
in the middle of a surface.

Since NCL does not allow the same geometry to be selected twice during a
Selection operation enabling this field will automatically use the first
selected drive surface as the last drive surface.  If the initial drive surface
is to be used as the final check surface, then do not check this box, but
rather select this surface as the check surface also.

Drive Style
-----------
The Contouring form can output the following types of motion commands.

  Auto - GOFWDA/drive1,...,drive,CHECK,check

  Explicit - GOFWD/drive1,TO,drive2
             ...
             GOFWD/drive,TO,check

  Implied  - GOFWD/drive1
             GOFWD/drive2
             ...
             GOFWD/drive,TO,check

In most cases you will use the Auto setting and let NCL automatically calculate
the check surface conditions between each drive geometry.  If the geometry
you are driving is not connected or the geometry does not physically match the
model (the model is not a solid) then you will use the Explicit or Implied
styles of contouring.

When Explicit is specified you will need to manually enter the check surface
condition for each drive surface.  This is accomplished using the Conditions
section in this form.

Initial Direction
-----------------
The tool forward vector should be in the direction that you want the tool to
move when moving along the first drive surface.  The Current forward vector
will be displayed at the tool end when this form is first displayed.  If this
is not the desired direction then you can select a vector at 90, 180, or 270
degrees from the current forward direction by toggling this field to
'From Tool'.  These vectors will be displayed at the end of the tool so that
you can select one.

You can also define the forward vector by selecting a point on the part by
toggling this field to 'Toward Part'.  Simply pick a location on the part where
you want the tool to be moved towards.

(Text Field)
------------
This field will display the defined forward vector.  You can also manually
manually type in the label of a vector or the components of the desired forward
vector in this field.

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
The feed rate used to move the tool to the specified location can remain the
Same as the current feed rate, the move can be moved in Rapid mode, or you can
specify a new feed rate Value for this move.  The current feed rate is contained
in the text field next to the Feed Rate prompt when the form is first displayed.  
You can specify advanced feed rate settings by checking the box next to the
Advanced button and then pressing the Advanced button to bring up the Feed Rate
Control form.
<END_SEC>
<Options>
Options
=======
The Options section allows you to specify settings that will affect the tool
motion calculations during the contouring operations.

Geometry Extensions:
--------------------
The drive surface extensions that are checked to by the tool can either be
Ignored or Respected.  It is typical to ignore the surface extensions when
driving connected drive surfaces.  This setting is only valid when the
Drive Style field is set to Auto.

Look Ahead:
-----------
You can tell the tool to look ahead up to five surfaces when driving each
surface.  This feature is useful when the tool may be too large to fit in some
areas, so it will attempt to check to multiple surfaces, checking to the surface
that it reaches first.  This setting is only valid when the Drive Style field
is set to Auto.  Since multilpe check surfaces for Explicit and Implied type
check surfaces are only valid within loops and Macros, this feature is not
supported in this form.

Contact Mode
------------
The CONTCT statement provides the ability to drive a curve as a wire in space.
This mode is activated or deactivated by ON or OFF. When ON is specified, NCL
will keep a line, circle or curve drive surface in contact with the side or 
corner radius of the tool as appropriate rather than treat the tool as an 
infinitely long cylinder. If the drive surface drops below the bottom of the
tool, contact will be maintained at a distance from the extension of the tool
axis equal to the radius of the bottom flat portion of the tool. An angled 
cutter may be used with a line, circle or curve as the drive surface.

Note: CONTCT/ON is modal. Use CONTCT/OFF to turn it off. CONTCT/ON can only be
used with fixed axis mode, no variable tool axis mode is allowed. 

Motion Calculate Settings:
--------------------------
Pressing the Assist button will bring up the Motion Calculation Assistance
form, giving you access to the various settings that control motion
calculations, such as Maxdp, Thicks, and Gouge Checking.  These settings can
be manipulated in NCL to assist in creating motion on the selected
geometry.

Corner Rounding:
----------------
Press the Fillet Settings button to bring up the Corner Rounding form, allowing
you to enable automatic filleting and to set the filleting values.
<END_SEC>
<Conditions>
Conditions
==========
The Conditions section contains a list of all of the selected drive geometry,
along with the drive surface and check surface tool conditions associated with
each drive surface.  Selecting a drive surface from this list will display
its conditions in the fields displayed above the list.

Direction:
----------
This field controls the direction of the tool at each drive surface.  When
using the automatic contouring method of machining, this field will be disabled
since NCL will automatically calculate the direction of the tool.

The default setting for all drive surfaces is GOFWD.  You will have to change
this direction for any drive surfaces that meet at a 90 degree or more angle or
if you need to use one of the special directions (GOBACK, GOUP, GODOWN).

Drive:
------
This field displays the drive surface that was selected in the list and cannot
be modified by the user.

CS Condition:
-------------
This field can be used to override the automatic check surface condition
calculations of NCL when the automatic contouring method of machining.  For the
Explicit and Implied methods of driving the geometry you will need to manually
set the check surface condition.  If the check surface condition is set to Auto
for the Explicit and Implied methods, then To is assumed.

Intersection:
-------------
Some geometry will intersect in multiple locations. When this is the case and
you do not want to stop at the first intersection of the drive and check
geometry, then enter the intersection number of the geometries where you want
the tool to stop.

Near Point:
-----------
Instead of entering an intersection number for drive and check geometry that
has multiple intersections, you can select a point on the model where you want
the tool to stop.  The selected point must be within the tool envelope at the
location you want the tool to stop.  You can select a point or any other type
of geometry.  If geometry that is not a point is selected, then pick the
geometry at the location where you want the tool to stop at and NCL will
calculate the actual point at this location.
<END_SEC>
<Colors>
Colors
======
The Colors section defines the colors that will be used to highlight the
geometry selected while in this form.  All entities that can be picked from
the screen are listed in this section.

Current DS:
-----------
The Current DS color will highlight the drive geometry selected in the
Conditions list using this color to give you a visual representation of the
active drive surface from the list.

Current CS:
-----------
The Current CS color will highlight the check surface geometry associated with
the active drive surface selected in the list.  If the final drive surface is
selected, then the Check Surface color will be used instead to highlight the
check surface for this drive surface.

Unused Geometry:
----------------
When pressing the 'Geometry' Action item button to "hide" geometry not used
in the Goto Tool Location operation you have the option of either invisibling
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
material removal process of NCLIPV.  It displays the Verify Preview motion
interface, allowing you to simulate the material removal for the Preview motion.
This button is only active when you have a valid NCLIPV license and Preview
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
