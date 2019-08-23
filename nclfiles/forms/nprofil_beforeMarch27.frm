#HEADER#
/TITLE/ Profile Machining
/POSITION/ 50,30
/SIZE/ 445,180

#SECTION#
/NAME/ Curve
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Curves
/POSITION/ 10,10
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 60,10
/SIZE/60,14
/TYPE/ UD_DASSTRING
/LEN/ 14
/PREC/ 80

#CHECKBOX#
/LABEL/ Machine to Side of Curve
/POSITION/ 10,27
/SIZE/100,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Apply Thickness to Curve
/POSITION/ 10,44
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 120,44
/SIZE/70,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Tool Condition:
/POSITION/ 10,61
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "Auto","Left","Right"

#PICTUREBOX#
/FILE/ Profile_Curve.jpg
/NAME/ profil1
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Start / End
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Start:
/POSITION/ 10,12, 30,10
/SIZE/75,40
/TYPE/UD_DASSTRING
/CHOICES/ "Tool End","Point","End"

#PUSHBUTTON#
/LABEL/ Start Point
/POSITION/ 90,10
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 150,10
/SIZE/60,14
/TYPE/ UD_DASSTRING
/LEN/ 14
/PREC/ 80

#CHOICEBOX#
/LABEL/ End:
/POSITION/ 10,29, 30,27
/SIZE/75,40
/TYPE/UD_DASSTRING
/CHOICES/ "Curve End","Point"

#PUSHBUTTON#
/LABEL/ End Point
/POSITION/ 90,27
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 150,27
/SIZE/60,14
/TYPE/ UD_DASSTRING
/LEN/ 14
/PREC/ 80

#CHOICEBOX#
/LABEL/ Direction:
/POSITION/ 10,46, 46,44
/SIZE/92,40
/TYPE/UD_DASSTRING
/CHOICES/ "Current","From Tool","Toward Part","CLW","CCLW"

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 108,44
/SIZE/ 35,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 150,44
/SIZE/60,14
/TYPE/ UD_DASSTRING
/LEN/ 14
/PREC/ 80

#CHOICEBOX#
/LABEL/ Overlap:
/POSITION/ 10,63, 43,61
/SIZE/75,40
/TYPE/UD_DASSTRING
/CHOICES/ "None","Both","Start","End"

#EDIT#
/LABEL/
/POSITION/ 95,61
/SIZE/60,14
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 20

#PICTUREBOX#
/FILE/ Profile_StartEnd.jpg
/NAME/ profil2
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Part Surface
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Part Surface
/POSITION/ 10,10
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 80,10
/SIZE/60,14
/TYPE/ UD_DASSTRING
/LEN/ 12
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Deselect
/POSITION/ 150,10
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Thick:
/POSITION/ 10,27
/SIZE/70,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Tool Axis:
/POSITION/ 10,44
/SIZE/80,40
/TYPE/UD_DASSTRING
/CHOICES/ "Fixed","Normal","Atangl"

#EDIT#
/LABEL/ Angle:
/POSITION/ 100,44
/SIZE/70,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Control Surface
/POSITION/ 10,61
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 80,61
/SIZE/60,14
/TYPE/ UD_DASSTRING
/LEN/ 12
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Deselect
/POSITION/ 150,61
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Tilt Tool at Start of Move
/POSITION/ 10,88
/SIZE/100,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Tilt Tool at End of Move
/POSITION/ 115,88
/SIZE/100,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Start Tilt Angle:
/POSITION/ 10,105
/SIZE/70,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ End Tilt Angle:
/POSITION/ 115,105
/SIZE/70,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ 
/POSITION/ 10,122
/SIZE/50,40
/TYPE/UD_DASSTRING
/CHOICES/ "Distance","Percent"

#EDIT#
/LABEL/
/POSITION/ 65,122
/SIZE/70,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ 
/POSITION/ 115,122
/SIZE/50,40
/TYPE/UD_DASSTRING
/CHOICES/ "Distance","Percent"

#EDIT#
/LABEL/
/POSITION/ 175,122
/SIZE/70,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#PICTUREBOX#
/FILE/ Profile_PartSurface.jpg
/NAME/ profil3
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Entry / Exit
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Entry Method:
/POSITION/ 10,10
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Omit","Angle","Arc","Both"

#EDIT#
/LABEL/ Distance:
/POSITION/ 10,27, 45,27
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Angle:
/POSITION/ 90,27
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Rise:
/POSITION/ 160,27
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Radius:
/POSITION/ 10,44, 45,44
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Rise:
/POSITION/ 90,44
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Exit Method:
/POSITION/ 10,71
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Omit","Angle","Arc","Both"

#EDIT#
/LABEL/ Distance:
/POSITION/ 10,88, 45,88
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Angle:
/POSITION/ 90,88
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Rise:
/POSITION/ 160,88
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Radius:
/POSITION/ 10,105, 45,105
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#EDIT#
/LABEL/ Rise:
/POSITION/ 90,105
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#PICTUREBOX#
/FILE/ Profile_EntryExit.jpg
/NAME/ profil4
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Positioning
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Clearance:
/POSITION/ 10,10
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "Current","Plane","Distance","Incremental"

#EDIT#
/LABEL/
/POSITION/ 110,10
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 165,10
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Rapto:
/POSITION/ 10,27
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "Entry","Plane","Distance","Incremental"

#EDIT#
/LABEL/
/POSITION/ 110,27
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 165,27
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Retract:
/POSITION/ 10,44
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "Off","On","Plane","Distance","Incremental"

#EDIT#
/LABEL/
/POSITION/ 110,44
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 165,44
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Loop Retract Distance:
/POSITION/ 10,61
/SIZE/100,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#PICTUREBOX#
/FILE/ Profile_Positioning.jpg
/NAME/ profil5
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Passes
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Offset Type:
/POSITION/ 10,10
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "None", "Thickness","Passes"

#EDIT#
/LABEL/
/POSITION/ 115,10
/SIZE/70,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Step Type:
/POSITION/ 10,27
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "Passes","Max-Step"

#EDIT#
/LABEL/
/POSITION/ 115,27
/SIZE/70,14
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Transition Moves:
/POSITION/ 10,44
/SIZE/105,40
/TYPE/UD_DASSTRING
/CHOICES/ "Down", "Up", "Off"

#CHOICEBOX#
/LABEL/ Depth Type:
/POSITION/ 10,78
/SIZE/95,40
/TYPE/UD_DASSTRING
/CHOICES/ "None","Top Plane","Distance","Passes"

#EDIT#
/LABEL/
/POSITION/ 110,76
/SIZE/60,12
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 80

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 165,76
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Step Type:
/POSITION/ 10,93
/SIZE/90,40
/TYPE/UD_DASSTRING
/CHOICES/ "Passes","Max-Step"

#EDIT#
/LABEL/
/POSITION/ 110,93
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ First Perform:
/POSITION/ 10,110
/SIZE/110,40
/TYPE/UD_DASSTRING
/CHOICES/ "Offset Passes","Depth Passes"

#PICTUREBOX#
/FILE/ Profile_Passes.jpg
/NAME/ profil6
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Options
/COLOR/ DEFAULT

#CHECKBOX#
/LABEL/ Maximum Step Size:
/POSITION/ 10,10, 25,12
/SIZE/80,15
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 100,10
/SIZE/ 100,14
/TYPE/UD_SCAVAL
/LEN/ 10
/PREC/ 8

#CHECKBOX#
/LABEL/ Minimum Fillet Angle:
/POSITION/ 10,27, 25,29
/SIZE/80,15
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 100,27
/SIZE/ 100,14
/TYPE/UD_SCAVAL
/LEN/ 10
/PREC/ 8

#PUSHBUTTON#
/LABEL/ Fillet Settings
/POSITION/ 155,27
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ CUTCOM/
/POSITION/ 10,52
/SIZE/55,15
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/
/POSITION/ 65,52
/SIZE/35,14
/TYPE/UD_DASSTRING
/CHOICES/ "Left","Right","On","None"

#CHOICEBOX#
/LABEL/ ,
/POSITION/ 103,54,112,52
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ "XYPLAN","YZPLAN","ZXPLAN","None"

#EDIT#
/LABEL/ ,
/POSITION/ 158,54,165,52
/SIZE/ 80,14
/TYPE/UD_DASSTRING
/LEN/ 10
/PREC/ 8

#EDIT#
/LABEL/ Positional Distance:
/POSITION/ 25,71, 90,69
/SIZE/100,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ ,
/POSITION/ 130,71,140,69
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ "All","Start"

#PICTUREBOX#
/FILE/ Profile_Options.jpg
/NAME/ profil7
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Feed Rates
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ General:
/POSITION/ 10,30, 50,30
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,30
/SIZE/50,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Position:
/POSITION/ 10,47, 50,47
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,47
/SIZE/90,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Retract:
/POSITION/ 10,64, 50,64
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,64
/SIZE/50,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Entry:
/POSITION/ 10,81, 50,81
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,81
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Transition:
/POSITION/ 10,98, 50,98
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,98
/SIZE/50,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#CHOICEBOX#
/LABEL/ Exit:
/POSITION/ 10,115, 50,115
/SIZE/85,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value","Rapid","Factor"

#EDIT#
/LABEL/
/POSITION/ 105,115
/SIZE/70,12
/TYPE/ UD_SCAVAL
/PREC/ 4
/LEN/ 8

#DISPLAY#
/LABEL/ Current Feed rate:
/POSITION/ 10,12
/SIZE/ 120,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8
/COLOR/ BLUE,DEFAULT
/PCOLOR/ BLUE,DEFAULT

#PICTUREBOX#
/FILE/ Profile_FeedRates.jpg
/NAME/ profil8
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION_BAR#

#SECTION#
/NAME/ Colors
/COLOR/ *DEFAULT

#COLOR#
/LABEL/ Curves:
/POSITION/ 10,10, 70,10
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Start Point:
/POSITION/ 125,10, 175,10
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ End Point:
/POSITION/ 10,27, 70,27
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Part Surface:
/POSITION/ 10,44, 70,44
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Control Surf:
/POSITION/ 125,44, 175,44
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Clearance Plane:
/POSITION/ 10,61, 70,61
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Rapto Plane:
/POSITION/ 125,61, 175,61
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Retract Plane:
/POSITION/ 10,78, 70,78
/SIZE/ 100,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Depth Plane:
/POSITION/ 125,78, 175,78
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Unused Geometry:
/POSITION/ 10,95
/SIZE/102,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Hide","Fade"

#COLOR#
/LABEL/ Color:
/POSITION/ 125,95, 175,95
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

#HELP#
<Curve>
Profile Machining
=================
This form is used to machine a profile curve.  The curve can be traced in a
TLON condition or machined with multiple offset and/or depth passes.  The tool
positioning, entry, and exit methods used for machining the profile can be
controlled using this form.

Curves
------
Pressing the Curve button allows the user to select the curve(s) to profile
machine.  The curve(s) may be of any type (B-spline, NCL, Composite, etc.),
closed or opened, and planar or non-planar.  Multiple curves can be selected
and profiled using the same settings using this form.

Deselect
--------
Deselects all previously selected curves.

Machine to Side of Curve
------------------------
Check this box if the cutter should be positioned to the side of the selected
curve(s) when creating the profile motion.  Profile curves that define the edge
of the part should have this box checked.  Leaving this box unchecked will 
cause the tool to exactly follow the profile curve(s), i.e. the tool will be on
the curve.

Apply Thickness to Curve
------------------------
This box should be checked when the tool is positioned to the side of the
profile curve and material should be left on the part where the curve is
defined.  You can also define a thick value when the tool is positioned on the
profile curve, in which case the thick value will be applied to the tool
condition side of the curve without regards to the cutter diameter.

Enter the thick value in the text box to the right of this check box.

Tool Condition:
---------------
'Auto' specifies that the tool position will be used to determine the side of
the profile curve to machine.  'Left' will offset the tool to the left of the
curve and 'Right' will offset the tool to the right of the curve as compared to
the initial direction as specified in the 'Start / End' section.
<END_SEC>
<Start / End>
Start / End
===========
The Start / End section defines the starting and ending positions on the curve
of the profile motion, the initial direction of the tool along the profile
curve, and whether the tool motion will overlap at the start and end of a
profile curve.

Start:
------
The starting location of the profile can either be specified as the 'Tool End'
point, as a near 'Point' to the profile curve, or as an 'End' point of the 
profile curve.  In the first two case a point on the curve will be calculated 
from the designated position and that is where the profile motion will start. 
In essence, the curve is internally trimmed to this location.  In order to 
start at the beginning of the curve, you can either position the tool there
prior to profiling it or specify a 'Point' of '(PT/ON,cv,0.)'. 

In the 'End' point case, either the beginning point or end point of the curve
will be the start location where the profile motion will start.  The current
tool position will determine which end of the curve to start at.

Start Point
-----------
This button is only active when the Start position is a user defined 'Point'.
Pressing this button allows you to select a point to use as the starting
location of the profile.

The point label or coordinates of the point can be typed into the text field
instead of picking the point from the screen.

End:
----
The ending location of the profile motion can either be specified as the
'Curve End' point or as a near 'Point' to the profile curve.  When a near
'Point' is used, a point on the curve will be calculated from this position and
that is where the profile motion will end.  In essence, the curve is internally
trimmed to this location.

If 'Curve End' is specified and the profile is closed, then the entire profile
will be machined, no matter where the starting location is.

End Point
---------
This button is only active when the End position is a user defined 'Point'.
Pressing this button allows you to select a point to use as the ending location
of the profile.

The point label or coordinates of the point can be typed into the text field
instead of picking the point from the screen.

Direction:
----------
The initial direction of motion around the profile is specified using this
field.  The Current forward vector will be displayed at the tool end when this
form is first displayed.  If this is not the desired direction then you can
select a vector at 90, 180, or 270 degrees from the current forward direction
by toggling this field to 'From Tool'.  These vectors will be displayed at
the end of the tool so that you can select one.

You can also define the forward vector by selecting a point on the part by
toggling this field to 'Toward Part'.  Simply pick a location on the part in
the direction of how the tool should move along the curve.

'CLW' specifies that the motion will be generated in a clockwise direction on a
closed profile or in the direction that the profile curve was defined on an
open profile.  'CCLW' specifies that the motion direction will be in the
counter-clockwise direction on a closed profile or opposite the direction that
the profile curve was defined on an open profile.

Select
------
Pressing the Select button allows you to select the forward vector or part
location if you want to change it after the initial selection.

(Text Field)
------------
This field will display the defined forward vector.  You can also manually
manually type in the label of a vector or the components of the desired forward
vector in this field.

Overlap:
--------
The Overlap setting specifies whether any extra motion will be generated at the
beginning and/or ending of the profile.  On a closed profile, any extra motion
will generate an overlap at the starting location of the profile motion.  On an
open profile any overlap distance specified will extend the beginning and/or
ending of the profile motion.

'None' disables any overlap motion from being generated.  'Both' specifies that
the value entered in the field will be used as the overlap distance for both the
start and end of the profile motion.  The actual overlap on closed profiles
will be twice this value.  'Start' specifies that the overlap distance should
only be applied to the start of the profile.  'End' applies the overlap distance
to the end of the profile.

Entering two values in this field, for example '.1,.05' will cause the first
value to be used as the starting overlap distance and the second value to be
used as the ending overlap distance, no matter what Overlap setting is
specified.
<END_SEC>
<Part Surface>
Part Surface
============
The Part Surface section controls the settings used for defining the part
surface and tool axis mode to use when profiling.

Part Surface
------------
Pressing the Select button allows the user to select a surface or plane to use
as the part surface when profiling.  If a part surface is not specified, then
the profile curve will be machined as it is defined, for example a non-planar
curve will be machined in 3-axis, while a planar curve will be machined in
2-axis.

You can type in the label of the part surface in the text field rather than
picking it from the screen.

Deselect
--------
Pressing the Deselect button will deselect the part surface and default back to
no part surface being defined.

Thick:
------
The Thick value specifies an offset distance that the tool will stay away from
the part surface.

Tool Axis:
----------
The profile can be machined with a 'Fixed' tool axis (as defined by the tool
position prior to the start of the profile), with a tool axis that is 'Normal'
to the part surface or a secondary part surface, or with a tool axis that is
tilted from the Normal angle along the forward direction of the motion (Atangl).

When 'Atangl' is specfied the tilt angle should be entered into the 'Angle'
text field.  This tilt angle will be applied along the whole length of the
profile motion.

Control Surface
---------------
Pressing the Control Surface button allows you to select a secondary surface
that will be used to control the tool axis when 'Normal' or 'Atangl' has been
specified as the tool axis mode.  If a controlling surface (secondary part
surface) is not specified, then the part surface entity will be used to control
the tool axis when 'Normal' or 'Atangl' is specified.

You can type in the label of the tool axis control surface in the text field
rather than picking it from the screen.

Deselect
--------
Pressing the Deselect button will deselect the tool axis control surface and
default back to the part surface being used to control the tool axis.

Tilt Tool At Start of Move
--------------------------
Checking this box will cause the tool to be tilted by the specified angle at the
start of the profile motion.  The tool will fan to the Normal orientation in the
distance specified.

Start Tilt Angle:
-----------------
Specifies the tilt angle of the tool at the start of the profile motion.  A
positive angle will tilt the tool in the forward direction so that it is angled
away from a possible wall present at the start of the profile curve.

Distance
--------
You can specify the distance the tool will travel while fanning to the
calculated Normal orientation by either a fixed distance (Distance) or as a
percentage of the entire profile motion (Percent).  A percentage value should
be entered in the range of 1-99.

Tilt Tool At End of Move
------------------------
Checking this box will cause the tool to be tilted by the specified angle at the
end of the profile motion.  The tool will fan from its Normal orientation in the
distance specified.

End Tilt Angle:
---------------
Specifies the tilt angle of the tool at the end of the profile motion.  A
positive angle will tilt the tool in the reverse direction of the move so that
it is angled away from a possible wall present at the end of the profile curve.

Distance
--------
You can specify the distance the tool will travel while fanning from the
calculated Normal orientation by either a fixed distance (Distance) or as a
percentage of the entire profile motion (Percent).  A percentage value should
be entered in the range of 1-99.
<END_SEC>
<Entry / Exit>
Entry / Exit
============
This Entry / Exit section is used to define the methods used for entering onto
and exiting off of the profile curve.  The entry and exit methods can be
omitted, at an angle, along an arc, or as a combination of both.

One special note to consider with entry and exit moves is that if the profile
condition is set to On, the profile curve is closed, and the entry/exit method
is set to Angle, then the entry move will actually follow the shape of the
curve rather than being purely a straight move.

Entry Method:
-------------
The Entry Method specifies how the tool enters each profile pass.  'Omit'
does not generate any entry motion, the tool will move directly to the start
of the profile pass.  'Angle' will generate a linear move at an angle to the
start of the profile on entry.  'Arc' will generate a 90-degree circular move
to the start of the profile.

'Both' will generate a combination linear and circular move onto the profile.
The linear move will occur first and will always be tangent to the arc move,
which transitions between the linear move and the profile.  The circular move
will always be tangent to the profile.

Distance:
---------
This field is only enabled when the entry method is set to 'Angle' or 'Both'
and specifies the length of the linear entry move.

Angle:
------
This field is only enabled when the entry method is set to 'Angle' or 'Both'
and specifies the angular offset to the initial direction of the profile to
use as the entry direction.  The angle value is ignored whenever the profile
condition is set to On.

Rise:
-----
This field specifies the rise value or distance above the profile start location
for the beginning of the entry move.

Radius:
-------
This field is only enabled when the entry method is set to 'Arc' or 'Both' and
specifies the radius of the circular entry arc.

Rise:
-----
This field specifies the rise value or distance above the profile start location
for the beginning of the entry move.  Specifying a Rise distance other than zero
will generate helical entry motion when the tool is to the side of the profile
curve.

Exit Method:
------------
The Exit method specifies how the tool exits each profile pass.  'Omit' does
not generate any exit motion, the tool will be positioned at the end of the
profile when the retract motion is generated.  'Angle' will generate a linear
move at an angle to the end of the profile on exit.  'Arc' will generate a
90-degree circular move off of the end of the profile.

'Both' will generate a combination circular and linear move off of the profile.
The circular move will occur first and will always be tangent to the profile
and transitions between the profile and the linear move.  The linear move will
always be tangent to the circular move.

Distance:
---------
This field is only enabled when the exit method is set to 'Angle' or 'Both' and
specifies the length of the linear exit move.

Angle:
------
This field is only enabled when the exit method is set to 'Angle' or 'Both' and
specifies the angular offset to the final direction of the profile to use as
the exit direction.  The angle value is ignored whenever the profile condition
is set to On.

Rise:
-----
This field specifies the rise value or distance above the profile end location
for the ending of the exit move.

Radius:
-------
This field is only enabled when the exit method is set to 'Arc' and specifies
the radius of the circular exit arc.

Rise:
-----
This field specifies the rise value or distance above the profile end location
for the ending of the exit move.  Specifying a Rise distance other than zero
will generate helical exit motion when the tool is to the side of the profile
curve.
<END_SEC>
<Positioning>
Positioning
===========
The Positioning section controls the positioning and tool retraction features of
Profile Machining.

Clearance:
----------
The clearance level of Profile Machining is used to position the tool prior to
cutting the profile and can be used as the tool retraction level at the end of
the end of the profile motion.  It is also used as the clearance level between
offset passes of an open profile.

'Current' uses the current tool position to define the clearance level.  A
clearance plane will be created that goes through the tool end point and is
perpendicular to the tool axis.  'Plane' allows the user to specify/select a
predefined plane.  'Distance' specifies a distance above the top of the part
level for the clearance plane.  'Incremental' specifies an incremental distance
above the entry point when positioning above the entry location.  This same
distance above the exit point will be used when retracting to the clearance
level at the end of the profile motion and between loops of an open profile.

Select
------
This button is only active when the Clearance type is set to 'Plane'.  Pressing
this button allows you to select the plane or planar surface to use as the
clearance plane.

Rapto:
------
The Rapto field specifies the distance above the entry location to position
the tool at the Positioning feed rate.  The remainder of the entry move will be
made at the Entry feed rate.

'Entry' will position to the start of the entry move using the Positioning feed
rate.  If an entry move is not programmed, then the tool will move to the start
of the profile using the General feed rate.  'Plane' allows the user to specify/
select a predefined plane.  'Distance' specifies a distance above the top of the
part level for the rapto plane.  'Incremental' specifies an incremental distance
above the first point of the profile to use as the rapto plane.

Select
------
This button is only active when the Rapto type is set to 'Plane'.  Pressing
this button allows you to select the plane or planar surface to use as the
rapto plane.

Retract:
--------
This field allows you to specify the retract logic to use at the end of the
profile motion, between depth passes, and between offset passes of an open
profile.

'Off' performs no tool retraction at the end of the profile motion, though the
tool may still be positioned to the clearance plane between passes.  'On'
retracts the tool to the Clearance level at the end of the profile motion.
'Plane' allows the user to specify/select a predefined plane.  'Distance'
specifies a distance above the top of the part level to retract the tool.
'Incremental' specifies an incremental distance above the last point of the
profile to use as the retract plane.

Select
------
This button is only active when the Retract type is set to 'Plane'.  Pressing
this button allows you to select the plane or planar surface to use as the
retract plane.

Loop Retract Distance:
----------------------
The Loop Retract Distance specifies a distance above the ending location of the
profile that the tool should retract between depth passes of a closed profile
and offset passes when the transition moves are programmed in the UP mode.
This value will be ignored on open profiles and between offset passes in the
DOWN mode.
<END_SEC>
<Passes>
Passes
======
The Passes section controls the multiple pass features of Profile Machining,
including passes around the profile curves and depth passes.

Offset Type:
------------
Controls the type of offsets to apply to the profile motion.  'None' will take
a single pass around the profile, 'Thickness' defines the distance from the
first pass to the final pass around the profile, and 'Passes' defines the 
number of passes to take around the profile.

A value must be entered when either 'Thickness' or 'Passes' is selected.

Step Type:
----------
This field will only be enabled when the Offset Type is not set to 'None'.
'Passes' will use the number of passes entered in this field to calculate the
step over distance and can only be specified when the Offset Type is set to
'Thickness'.  'Max-step' specifies the (maximum) step of each offset pass.
When the Offset Type is set to 'Passes' this value is the actual step over
distance used.

Transition Moves:
-----------------
The transition moves between passes of a closed curve can be made with the tool 
moving directly to the next pass or by exiting and re-entering the part using
the programmed exit and entry methods.  Select "Down" if the tool should move
directly between the passes.  Enabling filleting in the Options page can be used
in this case to create an S-shaped transitional move.  Select "Up" to have the
tool exit the current pass and enter the next pass using the programmed exit and
entry methods.  The Loop Retract Distance will be used to retract the tool
after exiting the part between passes. Select "Off" to disable the automatic
retract if the next tool entry level is higher than the retract level 
when the loop retract distance is set to 0.

Depth Type:
-----------
Controls the type of depths to apply to the profile motion.  'None' will perform
at a single depth around the profile, 'Top Plane' defines a plane to use as the
top of the part.  The first pass will be the calculated depth of cut beneath
this plane.  'Distance' defines the distance from the calculated bottom of the
profile (lowest point on the profile) to the top of the part.  'Passes' defines
the number of depth passes to take around the profile.

Select
------
This button is only active when the Depth Type is set to 'Top Plane'.  Pressing
this button allows you to select the plane or planar surface to use as the top
of the part.

Step Type:
----------
This field will only be enabled when the Depth Type is not set to 'None'.
'Passes' will use the number of passes entered in this field to calculate the
depth distance and can only be specified when the Depth Type is set to
'Top Plane' or 'Distance'.  'Max-step' specifies the (maximum) step of each
depth pass.  When the Depth Type is set to 'Passes', this value is the actual
step down distance used.

First Perform:
--------------
This field specifies whether the 'Offset Passes' or 'Depth Passes' should be
machined first.  'Offset Passes' causes the tool to move in towards the profile
prior to moving down in depth.  'Depth Passes' causes the tool to cut each
profile loop to depth prior to advancing to the next profile offset pass.
<END_SEC>
<Options>
Options
=======
The Options section controls miscellaneous options for Profile Machining.

Maximum Step Size:
------------------
Check this box to specify the maximum distance the tool can move in a single
step.  Motion steps generated by Profile Machining that exceed this distance
will have multiple points output so that no motion step exceeds this value.
Disabling this box causes the Profile Machining motion steps to be calculated
and output using the machining tolerance.

Minimum Fillet Angle:
---------------------
Check this box if you would like to enable automatic filleting of the sharp
corners of the profile motion.  Only moves that form a corner angle equal or
greater than the specified angle will be considered for filleting.  It is
recommended that you give some play in this value, especially when the curve is
being offset and/or projected to a surface.  For example, specify 29 if you want
corners with a 30 degree angle to be considered for filleting.

Fillet Settings:
----------------
Press this button to bring up the Corner Rounding form, allowing you to enable
automatic filleting and to set the filleting values.

CUTCOM/	
-------
If this box is checked, then the CUTCOM command defined by the CUTCOM parameters
will be output to the clfile just prior to making the final pass around the
profile.  A CUTCOM/OFF post-processor statement will be output to the clfile
to reset the CUTCOM condition after the profile motion and prior to the exit
move.

Positional Distance:
-------------------
The positional distance is used to calculate a new entry point after which
the CUTCOM command will be output.

ALL/START
---------
ALL will output the CUTCOM command on all entry moves when machining a closed
profile with looping and START will output CUTCOM on just the first entry move.
<END_SEC>
<Feed Rates>
Feed Rates
==========
This section defines the feed rates to use when Profile Machining.
Each feed rate has the following modes that can be programmed.

   Current = Uses the currently programmed feed rate.
   Value   = Allows you to specify an absolute value as the feed rate.
   Rapid   = Uses RAPID.
   Factor  = Enter a percentage of the programmed feed rate to use, for example
             .85 specifies 85% of the programmed feed rate.

General:
--------
Defines the feed rate to use during the basic motion along the profile.

Position:
---------
Defines the feed rate to use when positioning the tool to and from the clearance
plane and rapto plane.

Retract:
--------
Defines the feed rate to use when the tool is retracted at the end of the
profiling motion and between passes.

Entry:
------
Defines the feed rate to use during the entry move to the profile.

Transition:
-----------
Defines the feed rate to use when transitioning between "loops" offset from the
profile curve.

Exit:
-----
Defines the feed rate to use during the exit move from the profile.
<END_SEC>
<Colors>
Colors
======
The Colors section defines the colors that will be used to highlight the
geometry selected while in the Profile Machining form.  All entities that
can be picked from the screen are listed in this section.

Unused Geometry:
----------------
When pressing the 'Geometry' Action item button to "hide" geometry not used
in the Profile Machining operation you have the option of either invisible
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
Previews the Profile Machining motion without writing out the command or
permanently storing the generated motion.  Press the OK or Apply button to
write out the command and motion.  The generated command can be saved after the
preview even if there is an error.  To save the command, make no changes to the
settings and press the OK button.  The command will then be available for
editing in the command line.

Apply
-----
Outputs and processes the command(s) without taking down the form so that
other Profile motion can be created.

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
