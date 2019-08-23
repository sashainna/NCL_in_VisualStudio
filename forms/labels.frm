#HEADER#
/TITLE/ Geometry Label Modals
/POSITION/ 0,0
/SIZE/ 250,220

#FRAME#
/TITLE/ Label Text
/POSITION/ 10,10
/SIZE/ 225,65

#CHOICEBOX#
/LABEL/ Labels:
/POSITION/ 15,20,65,20
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#COLOR#      
/LABEL/ Color:
/POSITION/ 120,20,170,20
/SIZE/ 110,14
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/ Font Width:
/POSITION/ 15,37,65,37
/SIZE/ 80,14
/TYPE/ UD_DASINT
/RANGE/0,100
/PREC/ 4
/LEN/ 4

#EDIT#
/LABEL/ Font Height:
/POSITION/ 120,37,170,37
/SIZE/ 110,14
/TYPE/ UD_DASINT
/RANGE/0,100
/PREC/ 4
/LEN/ 4

#EDIT#
/LABEL/ Minimal Overlap Distance:
/POSITION/ 15,54,170,54
/SIZE/ 150,14
/TYPE/ UD_DASINT
/RANGE/0,100
/PREC/ 4
/LEN/ 4

#FRAME#
/TITLE/ Label Background
/POSITION/ 10,80
/SIZE/ 225,30

#CHOICEBOX#
/LABEL/ Box:
/POSITION/ 15,90,65,90
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#COLOR#      
/LABEL/ Color:
/POSITION/ 120,90,170,90
/SIZE/ 110,14
/TYPE/ UD_DASSTRING

#FRAME#
/TITLE/ Leader Lines
/POSITION/ 10,115
/SIZE/ 225,45

#CHOICEBOX#
/LABEL/ Leader Lines:
/POSITION/ 15,125,65,125
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#COLOR#      
/LABEL/ Color:
/POSITION/ 120,125,170,125
/SIZE/ 110,14
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Arrows:
/POSITION/ 15,142,65,142
/SIZE/ 80,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Off", "On"

#CHECKBOX#
/LABEL/ Output 'DRAFT/LABEL' command
/POSITION/ 15,175
/SIZE/ 150,14
/TYPE/ UD_DASSTRING

#HELP#
==========================
Geometry Label Modals Form
==========================
This form controls the label text, label background and leader lines. These
settings are saved in the 'ncl_labels.mod' file and will be used as the default
values the next time NCL is started.

Label Text:
-----------
When Label Text is enabled the automatic labeling feature is turned on. 
Once set all subsequent geometric entities will be labeled as they are defined.

Label Text Color:
-----------------
Select the color to display label text.

Font Width:
-----------
Select the font width in pixels for the geometry labels.  A typical size is 5.

Font Height:
------------
Select the font height in pixels for the geometry labels.  A typical size is 12.

Minimal Overlap Distance:
-------------------------
Select the minimum distance in pixels between the labels to avoid overlap. A 
typical value is 6.

Label Background:
-----------------
When Label Background is enabled, then a background box is displayed behind 
every displayed label. When Label Background is disabled, the labels are 
displayed without this box.

Label Background Color:
-----------------------
Select the color to display the label background.

Leader Lines:
-------------
When Leader Lines are enabled the automatic leader lines feature is turned on. 
Once set all subsequent geometric entities will have leader lines (if altered) 
as they are defined.

Leader Line Color:
------------------
Select the color to display leader lines.

Leader Line Arrow:
------------------
When Leader Line Arrows are enabled ,then a arrow head is displayed on the 
leader line pointing towards the entity. When Leader Line Arrows are disabled,
the leader lines are displayed without this arrow head.

Output 'DRAFT/LABEL' command:
-----------------------------
Checking this field  will the DRAFT/LABEL command to be output to the part 
program.



