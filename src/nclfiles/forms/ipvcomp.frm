#HEADER#
/TITLE/ NCLIPV Part Comparison
/POSITION/ 50,50
/SIZE/300,240

#CHOICE_LIST#
/LABEL/ Compare:
/POSITION/ 10,8,40,8
/SIZE/90,50
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 110,8
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#COLOR#
/LABEL/ 
/POSITION/ 160,8
/SIZE/55,14
/TYPE/UD_DASSTRING
/CHOICES/ Auto

#PUSHBUTTON#
/LABEL/ Reset
/POSITION/ 230,8
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ STL File:
/POSITION/ 10,25
/SIZE/ 35,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 50,25
/SIZE/ 105,14
/TYPE/ UD_DASSTRING
/PREC/ 35
/LEN/ 35

#CHOICEBOX#
/LABEL/ Units
/POSITION/ 205,25
/SIZE/ 65,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Inch", "MM"

#FRAME#
/TITLE/ Transformation
/POSITION/ 8,42
/SIZE/ 265,28

#CHECKBOX#
/LABEL/
/POSITION/ 12,52
/SIZE/ 15,14
/TYPE/ UD_DASSTRING

#CHOICE_LIST#
/LABEL/ Matrix:
/POSITION/ 30,52
/SIZE/ 90,70
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Load
/POSITION/ 158,52
/SIZE/ 40,14
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Modify
/POSITION/ 205,52
/SIZE/ 40,14
/TYPE/ UD_DASINT

#CHOICEBOX#
/LABEL/ 
/POSITION/ 10,74
/SIZE/ 65,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Report", "Visual", "Undercuts", "Overcuts", "Both"

#PUSHBUTTON#
/LABEL/ View
/POSITION/ 80,74
/SIZE/ 30,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Tolers
/POSITION/ 115,74
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Colors
/POSITION/ 160,74
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Translucency:
/POSITION/ 205,74
/SIZE/ 70,14
/TYPE/ UD_DASINT
/PREC/ 3
/LEN/ 3
/RANGE/ 1,100

#LISTBOX#
/LABEL/
/POSITION/ 15,101
/SIZE/ 265,75
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Face Distance
/POSITION/ 15,176
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Find
/POSITION/ 80,176
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Compare
/POSITION/ 125,176
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Create Log
/POSITION/ 180,176
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Clear Results
/POSITION/ 235,176
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Results
/POSITION/ 8,91
/SIZE/ 285,102

#PROGRESS#
/LABEL/ Progress:
/POSITION/ 10,200
/SIZE/ 140,12
/TYPE/ UD_DASINT

#PUSHBUTTON#
/LABEL/ Interrupt
/POSITION/ 170,200
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#HELP#
===========================
NCLIPV Part Comparison Form
===========================
This performs comparisons between the cut model and either standalone surfaces,
an STL file, or a single lathe shape.  Three types of comparison can be done; a
comparison between the cut model and a tessellated surface/model (Report), a
visual only comparison (Visual), or an under/over cut comparison between the
cut model and an STL file or lathe shape (Undercuts/Overcuts/Both).  Please
note that RapidCut only supports visual comparisons.

Compare:
--------
Select Surfaces, an STL file, or a Lathe Shape as the target part (the entity
to compare against the cut model).

Select
------
When Surfaces or Shape are selected as the target part, press this button to
begin or continue selection of surfaces or the shape to compare.  Picking a
surface that has already been selected will deselect this surface.  The
surfaces and shape are selected in the NCL window.

(Color)
-------
Defines the highlight color for the selected surfaces or shape.  The surfaces
or shape used in the comparison will be displayed in this color in the NCL
window.

Reset
-----
Deselects all surfaces.

STL File
--------
Loads the requested STL file as the target part.  Pressing the STL File button
will bring up a file browser.

Transformation
--------------
Enabling the Matrix field applies a transformation matrix to the defined solids.
This matrix can be a predefined matrix or can be typed in by the user.  Once
the Matrix field is enabled, you can either type in a matrix name or select one
from the display.  Once a matrix name is entered, you must press the Load button
to actually load the selected matrix.

The Modify button brings up a form that displays the canonical data of the
transformation matrix.  You can create your own matrix by typing in values in
this form or just simply view the current matrix.

Report
------
The comparison will be done using a tessellated (mesh of points) target model.
The number of points used for surface comparisons can be set using the NCLIPV
Comparison Tolerances form (see below).  The number of points used for STL
comparisons is fixed.  The Results window will contain a report of the
comparison, including the surface(s) compared, number of points used,
limits, and the undercut and overcut results.

Report style comparisons cannot be accomplished with RapidCut and can take
quite a bit of time when done with STL files that contain a large amount of
data.

Visual
------
Performs a visual comparison.  No report data will be generated.

Undercuts
---------
This comparison is only valid with STL files and Lathe Shapes and is not
supported by RapidCut.  A boolean operation with the cut model and the target
part will be done and any gouges will be displayed in the maximum undercut
color.  Depending on the size of the models, this can take a bit of time.
The minimum volume to be considered an undercut can be specified in the NCLIPV
Comparison Tolerances form.

Overcuts
--------
This comparison is similar to the Undercuts comparison, but excess material will
be displayed in the maximum overcut color.

Both
----
Performs an Undercuts and Overcuts comparison simultaneously.

View
----
Enters Dynamic Viewing mode.  The NCLIPV Part Comparison form will be taken
down while Dynamic Viewing is in effect and redisplayed when it is exited.
Please note that if a Visual only comparison is displayed in the NCLIPV window
it will be reset to the normal mode during and after Dynamic Viewing.

Tolers
------
Brings up the NCLIPV Comparison Tolerances form, which is described below.

Colors
------
Brings up the NCLIPV Comparison Colors form, which is described below.

Translucency
------------
Sets the display translucency of the cut model when performing Report, Undercut,
and Overcut comparisons.  Visual comparisons cannot be translucent.  The
translucency value can be from 1 to 100, where 100 displays a solid model and
a lower number displays a more transparent tool.

Results
-------
Displays the results of a Report, Undercut, or Overcut comparison.  It will
also display the results of a Face Distance query.

Face Distance
-------------
After a Report comparison, you can find the minimum and maximum undercut and
overcut distances for a single face on the model.  Each face will be displayed
in a color determined by the maximum distance the cut model is away from the
target part at this location.  Since Report comparisons use a tessellated list
of points for the comparison, a single face can have multiple distances from
the target part.  By pressing this button and then selecting a face from the
cut model, the range of distances for this face will be displayed in the Results
window along with information about the motion that performed the cut on this
face.

After an Undercuts or Overcuts comparison, the Face Distance function will
report the volume of the selected difference solid.

Find
----
When performing an Undercuts and/or Overcuts comparison, the Find button will
be activated and will be bring up the NCLIPV Gouge Finder form, which is used
to set the view to the various gouges/excesses and is described below.  While
the NCLIPV Gouge Finder form is displayed, the NCLIPV Part Comparison form will
be taken down.

Compare
-------
Press this button to perform the actual comparison.

Create Log
----------
Creates an output file of the information currently displayed in the Results
window.  A file browser will be displayed to enter the output filename.

Clear Results
-------------
Clears the Results window.

Progress:
---------
The Progress bar is only active during a Report style comparison, since this
comparison can take a long time based on the complexity of the models.  It
displays the progress of the actual comparison, not including loading of the
STL file or tessellation of the selected surfaces.

OK
--
Exits the NCLIPV Part Comparison form and saves any changes made to this form
and its subforms.  This button does not perform a comparison, instead the
Compare is used.

CANCEL
------
Exits the NCLIPV Part Comparison form and discards any changes made to this
form and its subforms.
