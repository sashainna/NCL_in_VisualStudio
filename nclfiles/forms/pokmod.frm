#HEADER#
/TITLE/ Advanced Pocket Modals
/POSITION/ 50,50
/SIZE/ 425,180

#SECTION#
/NAME/ Pocketing
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Method:
/POSITION/ 10,10
/SIZE/100,33
/TYPE/UD_DASSTRING
/CHOICES/ "Collapse","Lace","Lace Optimized","Scrub","Scrub Optimized"

#CHOICEBOX#
/LABEL/ Boundary Direction:
/POSITION/ 120,10, 187,10
/SIZE/115,33
/TYPE/UD_DASSTRING
/CHOICES/ "Shortest","Same","CCLW","CLW"

#CHOICEBOX#
/LABEL/ Pocket Direction:
/POSITION/ 10,27
/SIZE/105,33
/TYPE/UD_DASSTRING
/CHOICES/ "CCLW","CLW"

#CHOICEBOX#
/LABEL/ Spiral Direction:
/POSITION/ 120,27
/SIZE/95,33
/TYPE/UD_DASSTRING
/CHOICES/ "Out","In"

#CHOICEBOX#
/LABEL/ Cutting Direction:
/POSITION/ 10,27
/SIZE/115,80
/TYPE/UD_DASSTRING
/CHOICES/ "Positive X","Negative X","Positive Y",
/CHOICES/ "Negative Y","Long","Short","Vector"

#EDIT#
/LABEL/
/POSITION/ 130,27
/SIZE/60,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 16

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 180,27
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Minimum Stepover: 
/POSITION/ 10,44, 80,44
/SIZE/80,13
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#EDIT#
/LABEL/ Scallop height: 
/POSITION/ 125,44, 175,44
/SIZE/80,13
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#EDIT#
/LABEL/ Maximum Stepover: 
/POSITION/ 10,61, 80,61
/SIZE/80,13
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ Step Down:
/POSITION/ 125,61
/SIZE/ 88,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Passes","Distance","Depth"

#EDIT#
/LABEL/
/POSITION/ 223,61
/SIZE/80,13
/TYPE/UD_SCAVAL
/LEN/ 6
/PREC/ 4

#CHOICEBOX#
/LABEL/ Transition Between Loops:
/POSITION/ 10,78, 100,78
/SIZE/135,14
/TYPE/UD_DASSTRING
/CHOICES/ "Sharp","Arc"

#PUSHBUTTON#
/LABEL/
/POSITION/ 265,10
/SIZE/ 40,14
/TYPE/ UD_DASSTRING
/PICTURE/ pokmod1,"Select Pocketing Method",0,0,100,100

#PICTUREBOX#
/FILE/ Pokmod_Collapse.jpg
/NAME/ pokmod1
/POSITION/ 265,10
/SIZE/ 150,125

#SECTION#
/NAME/ Entry / Exit
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Method:
/POSITION/ 10,10
/SIZE/81,84
/TYPE/UD_DASSTRING
/CHOICES/ "Ramp","Helix","Helix CLW","Arc","Arc CLW","Plunge","Omit"
/PICTURE/ pokmod2,"Helix Entry",0,0, 33,41, 1
/PICTURE/ pokmod2,"CLW Helix Entry",21,42, 33,50, 2
/PICTURE/ pokmod2,"Ramp Entry",34,0, 70,50, 0
/PICTURE/ pokmod2,"Arc Entry",71,0, 89,41, 3
/PICTURE/ pokmod2,"CLW Arc Entry",90,41, 100,50, 4
/PICTURE/ pokmod2,"Plunge Entry",0,51, 33,100, 5
/PICTURE/ pokmod2,"No Entry Move",34,51, 70,100, 6

#CHECKBOX#
/LABEL/ Tangent Entry
/POSITION/ 100,10
/SIZE/57,15
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Specify Maximum Angle
/POSITION/ 165,10
/SIZE/87,15
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Number of Ramps:
/POSITION/ 10,27, 90,27
/SIZE/80,13
/TYPE/UD_SCAVAL
/LEN/ 6
/PREC/ 4

#EDIT#
/LABEL/ Ramp Distance:
/POSITION/ 135,27, 195,27
/SIZE/80,13
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ Entry Gouge: 
/POSITION/ 10,44, 55,44
/SIZE/110,50
/TYPE/UD_DASSTRING
/CHOICES/ "Warn","NoWarn","Avoid","Avoid-NoWarn"

#EDIT#
/LABEL/ Rapto Distance:
/POSITION/ 135,44, 195,44
/SIZE/80,13
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHOICEBOX#
/LABEL/ CL File Options:
/POSITION/ 10,61, 63,61
/SIZE/100,50
/TYPE/UD_DASSTRING
/CHOICES/ "Default","Couple","Cycle","Text Var"

#EDIT#
/LABEL/
/POSITION/ 120,61
/SIZE/60,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 16

#CHECKBOX#
/LABEL/ Arc Exit off Perimeter
/POSITION/ 10,78
/SIZE/79,15
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Arc Entry/Exit off Islands
/POSITION/ 100,78
/SIZE/90,15
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Horizontal Exit Distance:
/POSITION/ 10,95, 90,95
/SIZE/65,13
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#EDIT#
/LABEL/ Vertical Exit Distance:
/POSITION/ 138,95, 215,95
/SIZE/65,13
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#PICTUREBOX#
/FILE/ Pokmod_EntryExit.jpg
/NAME/ pokmod2
/POSITION/ 265,10
/SIZE/ 150,125

#SECTION#
/NAME/ Final Pass
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Final Pass Direction:
/POSITION/ 10,10, 80,10
/SIZE/115,14
/TYPE/UD_DASSTRING
/CHOICES/ "Same","Omit","CCLW","CLW","Reverse"

#CHOICEBOX#
/LABEL/ Outside Corners:
/POSITION/ 145,10, 205,10
/SIZE/ 105,14
/TYPE/UD_DASSTRING
/CHOICES/ "Sharp","Arc"
/PICTURE/ pokmod3,"Sharp Outside Corners",6,55, 35,70,0
/PICTURE/ pokmod3,"Rounded Outside Corners",55,65, 90,72,1

#CHOICEBOX#
/LABEL/ Inside Corners:
/POSITION/ 10,27, 80,27
/SIZE/115,14
/TYPE/UD_DASSTRING
/CHOICES/ "Sharp","Arc"
/PICTURE/ pokmod3,"Sharp Inside Corners",5,15, 31,28,0
/PICTURE/ pokmod3,"Rounded Inside Corners",79,15, 100,28,1

#EDIT#
/LABEL/ Inside Radius:
/POSITION/ 145,27, 205,27
/SIZE/ 40,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ pokmod3,"Inside Radius",60,80, 94,88

#CHECKBOX#
/LABEL/ Apply Slowdown Feedrate:
/POSITION/ 10,44, 100,44
/SIZE/ 115,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Slowdown Angle:
/POSITION/ 145,44, 205,44
/SIZE/ 40,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4

#CHECKBOX#
/LABEL/ 
/POSITION/ 10,61
/SIZE/15,15
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ CUTCOM/
/POSITION/ 25,61, 65,61
/SIZE/75,14
/TYPE/UD_DASSTRING
/CHOICES/ "Left","Right","On","None"

#CHOICEBOX#
/LABEL/ ,
/POSITION/ 105,61, 115,61
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ "XYPLAN","YZPLAN","ZXPLAN","None"

#EDIT#
/LABEL/ ,
/POSITION/ 165,61, 175,61
/SIZE/ 80,14
/TYPE/UD_DASSTRING
/LEN/ 20
/PREC/ 8

#PICTUREBOX#
/FILE/ Pokmod_FinalPass.jpg
/NAME/ pokmod3
/POSITION/ 265,10
/SIZE/ 150,125

#SECTION#
/NAME/ Tool Retract
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Clearance:
/POSITION/ 10,10
/SIZE/90,33
/TYPE/UD_DASSTRING
/CHOICES/ "Distance","Plane"

#EDIT#
/LABEL/
/POSITION/ 105,10
/SIZE/60,13
/TYPE/ UD_DASSTRING
/LEN/ 10
/PREC/ 16

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 155,10
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Retract to Clearance Level at End:
/POSITION/ 10,27
/SIZE/155,33
/TYPE/UD_DASSTRING
/CHOICES/ "No","Yes","Step"

#CHOICEBOX#
/LABEL/ Retract Between Levels:
/POSITION/ 10,44, 95,44
/SIZE/ 155,33
/TYPE/UD_DASSTRING
/CHOICES/ "Clearance Plane","Incremental"

#CHECKBOX#
/LABEL/ Retract Between Sections
/POSITION/ 10,61
/SIZE/ 100,14
/TYPE/UD_DASSTRING

#PICTUREBOX#
/FILE/ Pokmod_ToolRetract.jpg
/NAME/ pokmod4
/POSITION/ 265,10
/SIZE/ 150,125

#SECTION#
/NAME/ Feed Rates
/COLOR/ DEFAULT

#LABEL#
/LABEL/ Current Feed rate:
/POSITION/ 160,40
/SIZE/100,14
/TYPE/UD_DASSTRING
/COLOR/ BLUE,DEFAULT

#CHOICEBOX#
/LABEL/ General:
/POSITION/ 10,10, 50,10
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Current","Value"

#EDIT#
/LABEL/
/POSITION/ 85,10, 105,10
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ pokmod5,"General Feed",44,33, 61,38

#CHOICEBOX#
/LABEL/ Entry:
/POSITION/ 10,27, 50,27
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "General", "Value", "Factor"

#EDIT#
/LABEL/ 
/POSITION/ 85,27, 105,27
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ pokmod5,"Entry Feed",4,65, 17,72

#CHOICEBOX#
/LABEL/ First Pass:
/POSITION/ 10,44, 50,44
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "General","Value","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,44, 105,44
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ pokmod5,"First Pass Feed",42,82, 63,87

#CHOICEBOX#
/LABEL/ Transition:
/POSITION/ 10,61, 50,61
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "General","Value","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,61, 105,61
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ pokmod5,"Transition Feed",9,74, 30,80

#CHOICEBOX#
/LABEL/ Final Pass:
/POSITION/ 10,78, 50,78
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "General","Value","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,78, 105,78
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ pokmod5,"Final Pass Feed",75,26, 90,34

#CHOICEBOX#
/LABEL/ Retract:
/POSITION/ 10,95, 50,95
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Rapid","Value","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,95, 105,95
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ pokmod5,"Retract Feed",67,65, 83,72

#CHOICEBOX#
/LABEL/ Positioning:
/POSITION/ 10,112, 50,112
/SIZE/ 85,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Rapid","Value","Factor"

#EDIT#
/LABEL/
/POSITION/ 85,112, 105,112
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/LEN/ 8
/PREC/ 4
/PICTURE/ pokmod5,"Positioning Feed",60,87, 86,95

#DISPLAY#
/LABEL/
/POSITION/ 160,56
/SIZE/ 160,14
/TYPE/ UD_DASSTRING
/PREC/ 8
/LEN/ 8
/COLOR/ BLUE,DEFAULT
/PCOLOR/ BLUE,DEFAULT

#PICTUREBOX#
/FILE/ Pokmod_FeedRates.jpg
/NAME/ pokmod5
/POSITION/ 265,10
/SIZE/ 150,125

#SECTION#
/NAME/ ALL
/COLOR/ BLACK

#CHECKBOX#
/LABEL/ Force Output of POKMOD Command
/POSITION/ 100,130
/SIZE/130,15
/TYPE/UD_DASSTRING

#IMGBUTTON#
/LABEL/ video.bmp
/FILE/pokmod.mp4
/POSITION/ 315,140
/SIZE/ 60,14
/TYPE/ UD_DASSTRING
#HELP#
<Pocketing>
Pocketing
=========
This form allows you to input various Pocket Modal parameters used by the 
Advanced Pocket and Waterline Roughing motion routines.  The Pocketing section
contains the settings for general pocketing methods.

Method
------
This field determines the method used when generating the pocket motion.
Collapse results in NCL generating standard collapsing pocket motion.

Lace generates lace style pocketing motion (unidirectional), which will retract
the tool each time a pocket boundary or island is reached.

Lace Optimized is similar to Lace, except the tool cuts along the boundary when
intermediate boundary intersections are found, and retracts to avoid
recutting an already cut piece of the boundary. When retracted, the tool is
repositioned above the beginning of the next uncut portion of the boundary.

Scrub generates scrub style (back and forth) pocketing motion.  The tool will
remain at the current level and navigate around the pocket boundary and islands
when traversing to the next pass or avoiding pocket geometry.

Scrub Optimized is similar to Scrub, except the tool is retracted to avoid
recutting an already cut piece of the boundary. When retracted, the tool is
repositioned above the beginning of the next uncut portion of the boundary
and then reenters the cutting level.

Boundary Direction
------------------
Active only when the Optimized Lace or Scrub method is selected.

 - Shortest: Pass takes the shortest way around the boundary
 - SAME:     Shortest path is calculated only once for each boundary - each
             subsequent cut around the boundary is made toward the previously
             cut part of the boundary (so that the cutter stays down as little
             as possible).
 - CCLW:     Cut goes counter-clockwise (based on the tool axis) around the
             perimeter and clockwise around islands.
 - CLW:      Directions are reverse of CCLW.

Pocket Direction
----------------
Choose between CCLW or CLW when the pocketing method is set to Collapse.

Spiral Direction
----------------
This field defines the spiral direction of collapsing pocket motion and is only
active when the Method field is set to Collapse.

 - Out: The tool will start in the center of each pocket section and work its
        way out towards the pocket perimeter
 - In:  The tool will start on the outside of the pocket and works its way in
        toward the center of the pocket

Cutting Direction
-----------------
When Lace or Scrub is in effect, then the pocket direction can be specified as:

 - Positive X, Negative Y, etc.
 - Long:   Along the longest side of a rectangle encompassing the pocket
           geometry
 - Short:  Along the shortest side of the rectangle
 - Vector: Along a user specified vector or point-vector

You can type in the Vector name or components into the text field or use the
Select button can be used to interactively pick the vector.

Minimum Stepover Distance
-------------------------
Specifies the minimum amount from one concentric loop to the next. If set to 0,
a value equal to 10% of the Maximum Stepover Distance will be used.

Scallop Height 
--------------
Specifies the maximum amount of material allowed to be left under the cutter's
corner radius.

This field is disabled for Lace or Scrub pocketing.

Maximum Stepover Distance
-------------------------
Specifies the maximum stepover amount from one concentric loop to the next.
If set to 0, the current effective cutter diameter will be used
(diameter-corner_radius).

Transition Between Loops
------------------------
Choose between Sharp and Arc.

 - Sharp: Use a straight line move between pocket loops.
 - Arc:   Use a double-arc or S-shaped transition between pocket loops

This field is disabled for Lace or Scrub pocketing.

Step Down
---------
The following methods can be used to determine the number of pocketing levels
(passes) to machine in a pocket.

Passes specifies the actual number of passes to take.

Distance specifies a fixed distance between each level.  The number of passes
will be determined by the total distance divided by this distance.

Depth specifies the maximum depth of cut for each level.  The number of passes
is calculated so the Step Down does not exceed this value.
<END_SEC>
<Entry / Exit>
Entry Method	
------------
This field defines the tool entry method used. The choices are as follows:

 - Ramp: Make linear ramping moves down to cutting level
 - Helix/Arc: Make helical moves down to cutting level
 - Plunge: Plunge directly down tool axis to cutting level
 - Omit: Make no entry move into the pocket

The difference between Helix and Arc is that Helix accepts the number of
revolutions as its parameter and Arc accepts the number of degrees. For
example, Helix with 1.25 revolutions is the same as Arc with 450 degrees. Note
that Arc will be converted to Helix when the command is output. Helix CLW and
Arc CLW will create clockwise helical entry motions, unless Tangent Entry is
selected. If Tangent Entry is selected, the CLW/CCLW orientation of the
Helix/Arc will be based on the direction of the first cut.

Tangent Entry
-------------
Active only for the Ramp, Helix, and Arc entry methods. Tangent Entry specifies
that the entry should be made so the forward direction of the entry is tangent
to the forward direction of the first move of the cut when the cutter reaches
the cutting plane.

Specify Maximum Angle
---------------------
With a Ramp or Helix entry method, a user can specify the ramp/helix maximum 
angle of descent as an alternative to the number of ramps/revolutions.

Number of Ramps
---------------
Specifies the number of ramping moves to output when the entry method is set
to Ramp.

Maximum Ramp Angle
------------------
This field is only displayed when the Specify Maximum Angle box is checked and
contains the maximum entry angle allowed for Ramp entries.  The number of ramp
moves is the smallest such that the entry angle does not exceed the maximum
entry angle specified.

Ramp Distance
-------------
Active only with the Ramp entry method. Defines the horizontal length of the
ramp motion at each level.  A value of zero uses the distance between the first
and second point of the pocket motion as the ramp distance. 

Number of Revolutions
---------------------
Specifies the number of revolutions to output when the entry method is set to
Helix.

Maximum Helix Angle
-------------------
This field is only displayed when the Specify Maximum Angle box is checked and
contains the maximum entry angle allowed for Helix entries.  The number of
helical revolutions output is the smallest such that the entry angle does not
exceed the maximum entry angle specified.

Helix Radius
------------
Active only with the Helix entry method. Defines the radius of the helix.  A
value of zero uses the distance between the first and second point of the
pocket motion as the helix radius.

Arc Size (degrees)
------------------
Active only with the Arc entry method. The Arc Size specifies the angular size
of the entry arc in degrees and determines the number of revolutions.

Arc Radius
----------
Active only with the Arc entry method. Defines the radius of the arc.  A value
of zero uses the distance between the first and second point of the pocket
motion as the arc radius.

Entry Gouge
-----------
Warn will warn the user when a pocket entry violates the pocket geometry, but
will not modify the entry motion in any way.

NoWarn will not output any warning when the pocket geometry is violated.  This
method is normally used when the pocket islands are actually "holes" in the
part and cannot be physically cut by the pocket motion.

Avoid will attempt to avoid the pocket geometry when it is gouged, first by
altering the entry location and then if the entry method is Ramp, Helix, or
Arc, by decreasing the ramp distance or circular radius if necessary.  A
warning will be output if the entry move was altered to avoid the pocket
geometry.

Avoid-NoWarn is the same as Avoid, but a warning will not be output when the
entry move is altered to avoid the pocket geometry.

Rapto Distance
--------------
The tool will move down the tool axis at the retract feed rate until it is this
distance above the top of the pocket or the top plane of the last machined
level.  The tool will then make its entry into the pocket using the Entry Type
specified.

CL File Options
---------------
When the Entry Method is Helix or Arc, a user can now specify one of the
following: Default, COUPLE, CYCLE, or a text variable. For more information,
please refer to the Further Explanation of CL File Output section at the end
of this document.

Arc Exit off Perimeter
----------------------
Setting this value to Yes results in NCL generating a 90 degree arc exit off of
the pocket perimeter.  No results in a straight line exit of 45 degrees to the
last perimeter move.

Arc Entry/Exit off Islands
--------------------------
Setting this value to Yes results in NCL generating a 90 degree arc exit off of
the pocket islands.  No results in a straight line exit of 45 degrees to the
last island move.

Horizontal Exit Distance
------------------------
The horizontal distance to use when calculating the 45 degree linear retract
move at the cutting level before retracting.

Vertical Exit Distance
----------------------
The vertical distance to use when calculating the 45 degree linear retract
move at the cutting level before retracting.

Further Explanation of CL File Output
-------------------------------------
If Default is selected as the CL file method, the helical entry is represented
as a sequence of GOTO points along the helix.

The other options will use the following internal helical entry parameters in
the output statement.

   d        - the helix radius
   zi       - the depth per one helical rotation
   f        - the entry feedrate
   r        - the POKMOD retract-dist parameter
   a        - the starting angle of the entry (relative to the positive X-axis)
   x1,y1,z1 - the first point of the helical entry
   xc,yc,zc - the center of the circle defined as the projection of the entry
              helix onto the pocket top plane
   x2,y2,z2 - the last point of the helical entry, also the first point of
              the pocketing motion. 

If COUPLE is selected, the helical entry motion in the CL file is replaced by
the commands:

	GOTO/x1,y1,z1
	COUPLE/zi,ATANGL,360
	GF/(CIRCLE/xc,yc,0,0,0,1,d)
	GOTO/x2,y2,z2

If CYCLE is specified, the helical entry motion in the CL file is replaced by
the commands:

	CYCLE/CIRCUL,DEPTH,z,RADIUS,d,ON,STEP,zi,IPM,f,RAPTO,r,$
	             ATANGL,a,DOWN,CCLW
	GOTO/xc,yc,zc
	GOTO/x2,y2,z2

The entry then is interpreted as the following motion: 

   go to the point (xc,yc,zc) - the center of the circle
   go to the normal helical entry starting point, but at the retract-dist lower
      (that is, at the pocket top plane)
   perform the helical entry
   perform one horizontal circular motion at the bottom.

If a text variable is specified, the helical entry motion in the CL file is
replaced by the commands:

	text-variable
	GOTO/xc,yc,zc
	[text-variable-two]
	GOTO/x2,y2,z2

Here the text variable is used to format a post-processor command in the
CL file. An acceptable text variable is of the form:

	abc="CYCLE/CIRCUL,%Z,%D,%I,%F,[IPM/IPR],%R,[UP/DOWN],[CCLW/CLW] $
		[; CYCLE/OFF]"

An optional second text variable can be entered after a semicolon, then it is
entered as the third command after 'GOTO/xc,yc,zc'. In this example, the
"CYCLE/OFF" command cancels the post-processor helical interpolation mode.

The parameters are as follows:

	%Z		- the total incremental depth of the entry
	%D		- the helix diameter + the cutter diameter
	%I		- the depth per one helical rotation
	%F		- the entry feedrate, per minute or per rotation
	%R		- the POKMOD retract-dist parameter

The entry then is interpreted as the following motion: 
   go to the point (xc,yc,zc) - the center of the circle at retract-dist above
      the pocket top plane
   go to the point on the circle at 45 degrees from the center and at the pocket
      top plane
   perform the helical entry, CCLW by default
   perform one horizontal circular motion at the bottom
   if UP is specified, retract by the retract-dist up at the RAPID rate
<END_SEC>
<Final Pass>
Final Pass
==========
This section allows you to modify the final pass parameters as part of Pocket
Modal parameters.

Final Pass Direction
--------------------
Active for Lace or Scrub pocketing. The choices are:

 - Same:    The final pass will continue in the direction of the last lace/scrub
            move.
 - Reverse: The final pass will go in the direction opposite to the last
            lace/scrub move.
 - CCLW:    The cut goes counter-clockwise (as viewed down the tool axis) around
            the perimeter.
 - CLW:     Reverse of CCLW.
 - Omit:    No final pass is performed for the perimeter or islands.

Note that passes around islands are always done in the direction opposite that
of the perimeter final pass.

Outside Corners
---------------
Outside corners can either be machined with a Sharp angle or an Arc can be
generated around the corner.  The radius of the Arc is half the cutter diameter.

Inside Corners
--------------
Inside corners can either be machined with a Sharp angle or an Arc can be
generated around the corner.

Inside Radius
-------------
Specifies the radius of the arc when machining inside corners.

Apply Slowdown Feedrate
-----------------------
Check this box if a slowdown feed rate should be applied to the final pass when
entering a corner.

Slowdown Angle	
--------------
Enter the minimum angle of a corner where slowdown feed rates should be applied.
Corners with an angle of this value or less will have the slowdown feed rate
applied.

CUTCOM/	
-------
Check the CUTCOM box if cutter compensation should be enabled during the final
pass.  You can then enter the cutter compensation direction, machining plan,
and optional parameters, such as the cutter compensation register.  The CUTCOM
command will be output exactly as specified using these fields.
<END_SEC>
<Tool Retract>
Tool Retract
============
The Tool Retract section handles the tool positioning and retraction options.

Clearance
---------
Choose between using a Plane or a Distance for the clearance level when exiting
a pocket section and positioning to a new section.  The distance is based on
the top plane of the pocket.  You can type in the distance or plane label or
when Plane is specified, then the Select button can be used to interactively
pick the plane.

Retract to Clearance Level at End 
---------------------------------
Yes will retract the tool to the clearance plane, while Off will omit this
tool retraction.  Step will retract the tool from the part using the retract
distance, but will not retract to the clearance plane.

Retract Between Levels
----------------------
The tool can either be retracted to the Clearance Plane or an Incremental
distance above the pocket top plane between successive pocketing levels.  If
the Clearance value is specified as a plane, then Incremental cannot be
specified.

Retract between Sections
------------------------
Yes will cause the tool to retract between each pocket section, while No causes
the tool to remain down at the pocket level when transitioning between pocket
sections.
<END_SEC>
<Feed Rates>
Feed Rates
==========
The Feed Rates section contains all of the feed rate settings available with
pocketing.

General
-------
Choose the feed rate to use for the general pocket motion.  The choices are
Current and Value.  Current will use the active programmed feed rate. Enter
the desired feed rate when Value is selected.

Entry 	
-----
Choose the feed rate to use when the tool enters the pocket.  The choices 
are General, Value, and Factor.  Enter the desired feed rate when Value is 
selected, or a percentage of the general feed rate when Factor is selected.

First Pass 	
----------
Choose the feed rate to use for the first pass in the pocket.  The choices are
General, Value and Factor.  Enter the desired feed rate when Value is selected, 
or a percentage of the general feed rate when Factor is selected.

Transition 	
----------
Choose the feed rate to use when the tool transitions between pocket sections.
The choices are General, Value, and Factor.  Enter the desired feed rate when
Value is selected, or a percentage of the general feed rate when Factor is
selected.

Final Pass 	
----------
Choose the feed rate to use during the final (finishing) pass.  The choices are
General, Value, and Factor.  Enter the desired feed rate when Value is selected, 
or a percentage of the general feed rate when Factor is selected.

Retract 	
-------
Choose the feed rate to use when retracting the tool from the pocket. The
choices are Rapid, Value, and Factor.  Enter the desired feed rate when Value is
selected, or a percentage of the general feed rate when Factor is selected.

Positioning
-----------
Choose the feed rate to use when positioning the tool above the pocket. The
choices are Rapid, Value, and Factor.  Enter the desired feed rate when Value is
selected, or a percentage of the general feed rate when Factor is selected.

Current Feed Rate:
------------------
Displays the Current Feed Rate in effect.

<END_SEC>
<ALL>
Force Output of POKMOD Command
------------------------------
If this box is checked, the POKMOD statement is output unconditionally. If not
checked, the POKMOD statement is output only if:

 1) Pocket Modals have been changed
 2) A valid POCKET statement is also output by the main form.

Note: The "Force Output of POKMOD Command" check box is only available when 
this form is opened as a sub-form through other forms such as Advanced Pocket
or Waterline form.

Video
-----
Plays a short Video of the basic use of the Advanced Pocket Modals form.

