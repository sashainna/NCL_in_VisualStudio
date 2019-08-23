#HEADER#
/TITLE/ NCLIPV Measurement
/POSITION/ 0,0
/SIZE/ 310,250

#CHOICEBOX#      
/LABEL/ Measurement:
/POSITION/ 10,8
/SIZE/ 120,60
/TYPE/ UD_DASSTRING
/CHOICES/ "Attribute", "Point", "Geometry", "Thickness", "Gap", "Distance"
/CHOICES/ "Volume"

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 150,8
/SIZE/ 40,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Tolerance:
/POSITION/ 10,25
/SIZE/ 80,14
/TYPE/ UD_DASVAL
/PREC/ 4
/LEN/ -7
/RANGE/ .0001,2.5

#CHECKBOX#
/Label/ Single Selection
/POSITION/ 150,25
/SIZE/ 80,14
/TYPE/ UD_DASSTRING

#LISTBOX#
/LABEL/
/POSITION/ 10,52
/SIZE/ 280,60
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Status
/POSITION/ 8,42
/SIZE/ 290,72

#FRAME#
/TITLE/ Source Control
/POSITION/ 8,117
/SIZE/ 290,100

#FRAME#
/TITLE/ Source
/POSITION/ 10,127
/SIZE/ 135,65

#LISTBOX#
/LABEL/
/POSITION/ 15,137
/SIZE/ 125,60
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Call Stack
/POSITION/ 151,127
/SIZE/ 141,65

#LISTBOX#
/LABEL/
/POSITION/ 155,137
/SIZE/ 130,60
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Show Source
/POSITION/ 15,199
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#CHECKBOX#
/Label/ Reset line number after edit
/POSITION/ 95,199
/SIZE/ 100,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Edit
/POSITION/ 215,199
/SIZE/ 50,14
/TYPE/ UD_DASSTRING


#HELP#
==================
NCLIPV Measurement
==================
This form allows measurements to be taken from an NCLIPV cut model.

Measurement:
------------
The Measurement toggle field determines the type of measurement to take on the
cut model.  The following types of measurements can be taken.

Attribute = Displays the machining attributes that were active when the
            selected face was cut.  These attributes include the ISN of the
            cut, the MCD sequence number, the clfile record number, the NCLIPV
            cut number, the cutter parameters, the LOADTL parameters, the feed
            rate, the spindle parameters, the coolant mode, and the cutter
            compensation mode.

Point     = Displays the location and normal of the selected solid face.

Geometry  = Displays the plane parameters of the selected face and analyzes a
            section cut at this position to determine if a valid radius is
            defined.  If so, then the Radius, Arc Length, Center, and Normal
            vector of the radius is displayed.  The Tolerance value is used
            to analyze the section cut for a radius.

Thickness = Displays the thickness of the cut model normal to the selected
            face.

Gap       = Displays the gap distance normal to the selected face.

Distance  = Displays the actual and linear distances between two selected
            faces.  If the faces are parallel, then the planar distance is also
            displayed.

Volume    = Displays the volume of the selected solid.

Select
------
The Select button enters picking mode and allows you to select the face/solid
to measure.

Tolerance:
----------
Specifies the tolerance to use when determining if a valid radius is defined
at the selected face while performing a Geometry measurement.

Single Selection:
-----------------
Normally, the NCLIPV Measurement form will remain displayed after pressing the
Select button and the selection process will be modal, which means after a
selection is made, the results will be displayed and you will remain in
selection mode until you hit either Done or Reject on the mouse.

If the Single Selection box is checked, then the form will go away when
selection mode is entered and will be redisplayed after the selection is made.
Selection mode will automatically be ended after a valid selection is made.

Status
------
Displays the results of the measurement.

Source
------
Displays the source code that generated the cut.  If the part program is not
loaded or a solid face is selected that was not cut, then no source will be
displayed.

Call Stack
----------
Displays the active call/loop stack when the cut was generated. The call/loop
stack consists of the actual command that generated the cut and any Macro calls
or DO loops that were active at the time of the cut.

Macro calls will be displayed simply as CALL/macro and DO loops will be
displayed as DO/lable,index=n, where 'index' is the name of the controlling
DO loop variable and 'n' is its value at the time that the cut was generated.

Selecting a line from the Call Stack list will display the associated source
lines in the Source list.

Reset line number after edit
----------------------------
Checking this box causes NCL to reset the current source line number to the
line that was active prior to displaying the NCLIPV Measurement form.  If this
box is not checked, then the current source line will be the line that was
active when Command Mode was exited after pressing the Edit button.

Edit
----
Pressing the Edit button when the source code is displayed will enter Command
Mode at the line where the cut was made.

Show Source
-----------
Checking the Show Source box will display the source code that generated the
cut on the selected face.
