#HEADER#
/TITLE/ Motion Settings
/POSITION/ 0,0
/SIZE/ 270,130

#FRAME#
/TITLE/Auto Start
/POSITION/ 8,5
/SIZE/ 250,50

#CHECKBOX#      
/LABEL/Auto Start
/POSITION/ 14,15
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Omit First Point
/POSITION/ 80,15
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Linear Tolerance:
/POSITION/ 16, 33
/SIZE/40,14
/TYPE/UD_DASVALUE
/LEN/ 9
/PREC/ 96

#EDIT#
/LABEL/ Angular Tolerance:
/POSITION/ 136, 33
/SIZE/40,14
/TYPE/UD_DASVALUE
/LEN/ 9
/PREC/ 96

#CHECKBOX#      
/LABEL/Automatic UV Settings
/POSITION/ 14,58
/SIZE/ 85,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Show Motion
/POSITION/ 14,73
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Output Expanded CL File
/POSITION/ 80,73
/SIZE/ 94,14
/TYPE/ UD_DASSTRING

#CHECKBOX#      
/LABEL/Output Control Command
/POSITION/ 14,88
/SIZE/ 92,14
/TYPE/ UD_DASSTRING

#HELP#
===============
Motion Settings
===============

Auto Start
----------
Check the Auto Start box to turn on the Automatic Tool Start feature and make
the additional Auto Start settings accessible.  

Omit First Point
----------------
If the Omit First Point box is checked NCL will omit from the CL file the
theoretical calculated starting point of the tool move.  Leaving the Omit
First Point box unmarked signals NCL to keep the calculated starting point.  

Linear Tolerance:
-----------------
Use the Linear Tolerance field to modify the positional tolerance used by the
Auto Start feature when calculating the starting point of the tool move.

Angular Tolerance:
------------------
Use the Angular Tolerance field to modify the angular tolerance used by the
Auto Start feature when calculating the starting point of the tool move.

Automatic UV Settings
---------------------
When this box is checked NCL will automatically use the U and V settings
calculated in the previous motion statement for the current motion statement.

Show Motion
-----------
When this box is checked NCL will display tool motion coordinate values in the
scrolling window.

Output Expanded CL File
-----------------------
When this box is checked NCL will enable the expanded motion CL file option.
This output consists of 5200, 5210, and 5220 records, which are used for
advanced post-processor functions (3-D cutter compensation, blade control,
simulation, etc.).


Output Control Command
----------------------
When this box is checked NCL will output the control commands associated with
any setting changes made.  If this form has been accessed through the another,
then use the box from the original form.  Note that the changes will be
applied even if this box is not checked.
