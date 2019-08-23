#HEADER#
/TITLE/ General Settings
/POSITION/ 0,0
/SIZE/ 270,245

#FRAME#
/TITLE/Processing
/POSITION/ 8,5
/SIZE/ 250,60

#LABEL#
/LABEL/Show Command:
/POSITION/ 14,17
/SIZE/80,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/After *RUN
/POSITION/ 75,15
/SIZE/ 47,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/After *STOP
/POSITION/ 135,15
/SIZE/ 52,14
/TYPE/ UD_DASSTRING

#LABEL#
/LABEL/Stop At:
/POSITION/ 42,32
/SIZE/80,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/*STOP
/POSITION/ 75,30
/SIZE/ 36,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/*PAUSE
/POSITION/ 135,30
/SIZE/ 41,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/WARNINGS
/POSITION/ 195,30
/SIZE/ 53,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Case Sensitive
/POSITION/ 14,45
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Version:
/POSITION/ 90, 47
/SIZE/40,14
/TYPE/UD_DASVALUE
/LEN/ 8
/PREC/ 96

#FRAME#
/TITLE/APT Source Output
/POSITION/ 8,70
/SIZE/ 250,115

#CHECKBOX#      
/LABEL/Header
/POSITION/ 14,80
/SIZE/ 37,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Comment
/POSITION/ 75,80
/SIZE/ 41,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Remarks
/POSITION/ 135,80
/SIZE/ 41,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Circular:
/POSITION/ 17,100
/SIZE/75,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Planar", "All"

#CHECKBOX#
/LABEL/ Tracut
/POSITION/ 118,100
/SIZE/ 33,14
/TYPE/ UD_DASSTRING

#CHOICE_LIST#
/LABEL/ Matrix:
/POSITION/ 162,102
/SIZE/ 90,70
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ Cutter:
/POSITION/ 23,120
/SIZE/70,14
/TYPE/ UD_DASSTRING
/CHOICES/ "APT", "NCL", "PPRINT"

#EDIT#
/LABEL/ Accuracy:
/POSITION/ 120, 120
/SIZE/40,14
/TYPE/UD_DASVALUE
/LEN/ 8
/PREC/ 96

#CHECKBOX#      
/LABEL/Simulation
/POSITION/ 14,138
/SIZE/ 45,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/PPRINT IPV
/POSITION/ 75,138
/SIZE/ 52,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Output Accuracy:
/POSITION/ 14,160
/SIZE/102,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Low", "High"

#CHOICEBOX#
/LABEL/ Integer Output:
/POSITION/ 135,160
/SIZE/100,14
/TYPE/ UD_DASSTRING
/CHOICES/ "Integer", "Real"

#CHECKBOX#      
/LABEL/Output Control Command
/POSITION/ 14,190
/SIZE/ 92,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Geometry Settings
/POSITION/ 54,205
/SIZE/ 70,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Motion Settings
/POSITION/ 137,205
/SIZE/ 70,14
/TYPE/ UD_DASSTRING

#HELP#
================
General Settings
================

After *RUN
----------
When this box is checked NCL will enter command mode when a stopping
condition for the *RUN command is reached.

After *STOP
-----------
When this box is checked NCL will enter command mode when a *STOP command
is reached.

*STOP
-----
When this box is checked NCL will stop at *STOP commands.

*PAUSE
------
When this box is checked NCL will stop at *PAUSE commands.

WARNINGS
--------
When this box is checked NCL will stop at warnings.

Case Sensitive
--------------
When this box is checked the text string functions FINDEX, RINDEX,
STRCMP will be case sensitive.

Version
-------
Set the desired NCL version.

Header
------
When this box is checked NCL will output the REMARK program-information line
at the start of the APT source file.

Comment
-------
When this box is checked NCL will output source statements that generate GOTO
points as a comment line.

Remarks
-------
When this box is checked NCL will output the REMARK statements to the APT
source file.

Circular
--------
Selecting 'Planar' will cause NCL to only output circular interpolation
records to the APT Source file when the motion is in one of the major planes
(XY, YZ, ZX).  'All' will output circular interpolation in any 3-D plane.


Tracut
------
When this box is checked NCL will apply a separate TRACUT matrix to the output
APT source file.  A matrix can be provided using the Matrix drop down
menu.

Matrix
------
Select the TRACUT matrix to apply to the APT source file.  If no matrix is
selected and the TRACUT box is marked, then NCL will apply the last defined
TRACUT matrix when writing to the APT source file.

Cutter
------
Select the format to use when writing CUTTER statement to the APT source file.
An APT style cutter statement, NCL style cutter statement, or a PPRINT
statement containing the cutter parameters can be output.

Accuracy
--------
Specify the number of digits to the right of the decimal point to use when
writing CUTTER statements to the APT source file.

Simulation Format
-----------------
When this box is checked NCL will format the output of the APT source file
so that it is compatible with motion verification programs (NCLIPV, Vericut,
etc.).

Output PPRINT IPV Statements
----------------------------
When this box is checked NCL will output the PPRINT IPV commands to
the APT source file.

Output Accuracy
---------------
Select the level of precision NCL will use when writing to the APT source
file.  'Low' = 4 digits to the right of the decimal point, 'High' = 6 digits.

Integer Output
--------------
Select the ouput type NCL will use when writing integer values as part of a
post-processor command to the APT source file.

Output Control Command
----------------------
When this box is checked NCL will output control commands for all modified
settings.  Note that any changes will be applied even if this box is not
marked.

Geometry Settings
-----------------
Press this button to bring up the Geometry Settings form.

Motion Settings
---------------
Press this button to bring up the Motion Settings form.
