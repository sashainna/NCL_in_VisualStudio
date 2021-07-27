#HEADER#
/TITLE/ Waterline Roughing
/POSITION/ 0,0
/SIZE/ 440,200

#SECTION#
/NAME/ Pocket Motion
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Pocket Type:
/POSITION/ 10,12,58,12
/SIZE/ 123,57
/TYPE/UD_DASSTRING
/CHOICES/ "Entire Part","Single Pocket","Inner Pockets", "Custom"

#CHOICEBOX#
/LABEL/ Method:
/POSITION/ 10,29,58,29
/SIZE/ 123,57
/TYPE/UD_DASSTRING
/CHOICES/ "Advanced Pocket","VoluMill 2-Axis","VoluMill 3-Axis","VoluMill 5-Axis"

#CHOICEBOX#
/LABEL/ Cutting Levels:
/POSITION/ 10,46,58,46
/SIZE/ 123,57
/TYPE/UD_DASSTRING
/CHOICES/ "Planar","Revolved Surface"

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 135,46
/SIZE/ 40,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 178,46
/SIZE/ 40,14
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 8

#PUSHBUTTON#
/LABEL/
/POSITION/ 235,10
/SIZE/ 40,14
/TYPE/UD_DASSTRING
/PICTURE/ PocketM_1_1,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_1_2,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_1_3,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_1_4,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_2_1,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_2_2,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_2_3,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_2_4,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_3_1,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_3_2,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_3_3,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_3_4,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_4_1,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_4_2,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_4_3,"Select Pocket Type",0,0,100,100
/PICTURE/ PocketM_4_4,"Select Pocket Type",0,0,100,100

#PICTUREBOX#
/FILE/ H2OLine_EntirePart_Advanced.jpg
/NAME/ PocketM_1_1
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_EntirePart_Revolve.jpg
/NAME/ PocketM_1_2
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_EntirePart_VoluMill2.jpg
/NAME/ PocketM_1_3
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_EntirePart_VoluMill3.jpg
/NAME/ PocketM_1_4
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_SinglePocket_Advanced.jpg
/NAME/ PocketM_2_1
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_SinglePocket_Revolve.jpg
/NAME/ PocketM_2_2
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_SinglePocket_VoluMill2.jpg
/NAME/ PocketM_2_3
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_SinglePocket_VoluMill3.jpg
/NAME/ PocketM_2_4
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_InnerPockets_Advanced.jpg
/NAME/ PocketM_3_1
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_InnerPockets_Revolve.jpg
/NAME/ PocketM_3_2
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_InnerPockets_VoluMill2.jpg
/NAME/ PocketM_3_3
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_InnerPockets_VoluMill3.jpg
/NAME/ PocketM_3_4
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Custom_Advanced.jpg
/NAME/ PocketM_4_1
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Custom_Revolve.jpg
/NAME/ PocketM_4_2
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Custom_VoluMill2.jpg
/NAME/ PocketM_4_3
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Custom_VoluMill3.jpg
/NAME/ PocketM_4_4
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Geometry
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Layer
/POSITION/ 10,12
/SIZE/ 40,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 55,12
/SIZE/ 40,14
/TYPE/UD_SCAINT
/INPUT/FORM_STRING
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Show Layer
/POSITION/ 105,12
/SIZE/ 45,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Adding Surfaces to Layer
/POSITION/ 10,29
/SIZE/ 90,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 105,29
/SIZE/ 45,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 155,29
/SIZE/ 55,14
/TYPE/UD_DASSTRING

#PICTUREBOX#
/FILE/ H2OLine_Geometry.jpg
/NAME/ Geometry
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Stock
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Stock:
/POSITION/ 10,12
/SIZE/ 70,40
/TYPE/UD_DASSTRING
/CHOICES/ "Included","Calculate"

#CHECKBOX#
/LABEL/ Ignore Outer
/POSITION/ 85,12
/SIZE/ 55,14
/TYPE/UD_DASSTRING

#CHOICEBOX#
/LABEL/ 
/POSITION/ 85,12
/SIZE/ 55,40
/TYPE/UD_DASSTRING
/CHOICES/ "Contour","Box"
/PICTURE/ Stock_1,"Contour",0,13,50,49,0
/PICTURE/ Stock_1,"Box",51,13,100,49,1
/PICTURE/ Stock_2,"REV_Contour",0,13,50,49,0
/PICTURE/ Stock_2,"REV_Box",51,13,100,49,1
/PICTURE/ Stock_3,"VOlu_2 Contour",0,13,50,49,0
/PICTURE/ Stock_3,"VOlu_2 Box",51,13,100,49,1
/PICTURE/ Stock_4,"VOlu_3 Contour",0,13,50,49,0
/PICTURE/ Stock_4,"VOlu_3 Box",51,13,100,49,1

#EDIT#
/LABEL/ Expansion:
/POSITION/ 145,12,183,12
/SIZE/ 75,14
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 6

#CHECKBOX#
/LABEL/ Stock Surfaces
/POSITION/ 10,29
/SIZE/ 65,14
/TYPE/UD_DASSTRING
/PICTURE/ Stock_1,"Stock Surfaces",0,51,51,100,TOGGLE
/PICTURE/ Stock_2,"Rev Stock Surfaces",0,51,51,100,TOGGLE
/PICTURE/ Stock_3,"Volu_2 Stock Surfaces",0,51,51,100,TOGGLE
/PICTURE/ Stock_4,"Volu_3 Stock Surfaces",0,51,51,100,TOGGLE

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 30, 46
/SIZE/ 40,12
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Include on Layer
/POSITION/ 80,46
/SIZE/70,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Layer
/POSITION/ 10,63
/SIZE/ 40,12
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 55,63
/SIZE/40,14
/TYPE/ UD_SCAINT
/LEN/ 8
/PREC/ 16

#PUSHBUTTON#
/LABEL/ Highlight Layer
/POSITION/ 100,63
/SIZE/ 60,12
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 165,63
/SIZE/50,12
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Avoid Surfaces
/POSITION/ 10,80
/SIZE/ 65,14
/TYPE/UD_DASSTRING
/PICTURE/ Stock_1,"Avoid Surfaces",51,51,100,100,TOGGLE
/PICTURE/ Stock_2,"Rev Avoid Surfaces",51,51,100,100,TOGGLE
/PICTURE/ Stock_3,"Volu_2 Avoid Surfaces",51,51,100,100,TOGGLE
/PICTURE/ Stock_4,"Volu_3 Avoid Surfaces",51,51,100,100,TOGGLE

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 30, 97
/SIZE/ 40,12
/TYPE/ UD_DASSTRING

#CHECKBOX#
/LABEL/ Include on Layer
/POSITION/ 75,97
/SIZE/70,14
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Layer
/POSITION/ 10,114
/SIZE/ 40,12
/TYPE/ UD_DASSTRING

#EDIT#
/LABEL/
/POSITION/ 55,114
/SIZE/40,14
/TYPE/ UD_SCAINT
/LEN/ 8
/PREC/ 16

#PUSHBUTTON#
/LABEL/ Highlight Layer
/POSITION/ 100,114
/SIZE/ 60,12
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 165,114
/SIZE/50,12
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ H2OLine_Stock_Advanced.jpg
/NAME/ Stock_1
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Stock_Revolve.jpg
/NAME/ Stock_2
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Stock_VoluMill2.jpg
/NAME/ Stock_3
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Stock_VoluMill3.jpg
/NAME/ Stock_4
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Levels
/COLOR/ DEFAULT

#CHOICEBOX#
/LABEL/ Bottom:
/POSITION/ 10,12,36,12
/SIZE/ 93,55
/TYPE/UD_DASSTRING
/CHOICES/ "Stock","Plane","Depth from Top","Z-level"
/PICTURE/ Level_1,"Stock",50,70,65,75,0
/PICTURE/ Level_1,"Plane",50,76,65,81,1
/PICTURE/ Level_1,"Depth from Top",50,82,85,88,2
/PICTURE/ Level_1,"Z-Level",50,89,70,95,3
/PICTURE/ Level_3,"Stock",50,70,65,75,0
/PICTURE/ Level_3,"Plane",50,76,65,81,1
/PICTURE/ Level_3,"Depth from Top",50,82,85,88,2
/PICTURE/ Level_3,"Z-Level",50,89,70,95,3
/PICTURE/ Level_4,"Stock",50,70,65,75,0
/PICTURE/ Level_4,"Plane",50,76,65,81,1
/PICTURE/ Level_4,"Depth from Top",50,82,85,88,2
/PICTURE/ Level_4,"Z-Level",50,89,70,95,3

#CHOICEBOX#
/LABEL/ Bottom:
/POSITION/ 10,12,36,12
/SIZE/ 93,55
/TYPE/UD_DASSTRING
/CHOICES/ "Stock","Depth from Top","Base Offset"
/PICTURE/ Level_2,"Stock",40,74,55,80,0
/PICTURE/ Level_2,"Depth from Top",40,87,72,92,1
/PICTURE/ Level_2,"Base Offset",40,93,65,98,2

#EDIT#
/LABEL/ 
/POSITION/ 107,12
/SIZE/ 40,14
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 149,12
/SIZE/ 40,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Bottom Offset:
/POSITION/ 10,29,60,29
/SIZE/ 110,14
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8
/PICTURE/ Level_1,"Bottom Offset",70,73,100,80
/PICTURE/ Level_2,"Bottom Offset",48,75,78,86
/PICTURE/ Level_3,"Bottom Offset",70,73,100,80
/PICTURE/ Level_4,"Bottom Offset",70,73,100,80

#CHOICEBOX#
/LABEL/ Top:
/POSITION/ 10,46,35,46
/SIZE/ 110,55
/TYPE/UD_DASSTRING
/CHOICES/ "Stock","Plane","Distance from Bottom","Z-level"
/PICTURE/ Level_1,"Stock",55,0,70,5,0
/PICTURE/ Level_1,"Plane",55,6,70,11,1
/PICTURE/ Level_1,"Distance from Bottom",55,12,98,17,2
/PICTURE/ Level_1,"Z-Level",55,18,75,23,3
/PICTURE/ Level_3,"Stock",55,0,70,5,0
/PICTURE/ Level_3,"Plane",55,6,70,11,1
/PICTURE/ Level_3,"Distance from Bottom",55,12,98,17,2
/PICTURE/ Level_3,"Z-Level",55,18,75,23,3
/PICTURE/ Level_4,"Stock",55,0,70,5,0
/PICTURE/ Level_4,"Plane",55,6,70,11,1
/PICTURE/ Level_4,"Distance from Bottom",55,12,98,17,2
/PICTURE/ Level_4,"Z-Level",55,18,75,23,3

#CHOICEBOX#
/LABEL/ Top:
/POSITION/ 10,46,35,46
/SIZE/ 110,55
/TYPE/UD_DASSTRING
/CHOICES/ "Stock","Distance from Bottom","Base Offset"
/PICTURE/ Level_2,"Stock",30,0,45,6,0
/PICTURE/ Level_2,"Distance from Bottom",30,12,75,17,1
/PICTURE/ Level_2,"Base Offset",30,18,57,23,2

#EDIT#
/LABEL/ 
/POSITION/ 125,46
/SIZE/ 40,14
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 168,46
/SIZE/ 40,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Incremental Step Height:
/POSITION/ 10,97
/SIZE/ 90,14
/TYPE/UD_DASSTRING
/PICTURE/ Level_4,"Incremental Step Height",3,77,48,83,TOGGLE

#EDIT#
/LABEL/ 
/POSITION/ 103,97
/SIZE/ 40,14
/TYPE/UD_SCAUNITLESS
/INPUT/FORM_STRING
/LEN/ 8

#EDIT#
/LABEL/ Top Offset:
/POSITION/ 10,63,60,63
/SIZE/ 90,14
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8
/PICTURE/ Level_1,"Top Offset",75,4,98,10
/PICTURE/ Level_2,"Top Offset",40,7,62,11
/PICTURE/ Level_3,"Top Offset",75,4,98,10
/PICTURE/ Level_4,"Top Offset",75,4,98,10

#CHECKBOX#
/LABEL/ Finish Level Surfaces
/POSITION/ 10,80
/SIZE/ 80,14
/TYPE/UD_DASSTRING
/PICTURE/ Level_1,"Finish Level Surfaces",2,80,40,84,TOGGLE
/PICTURE/ Level_2,"Finish Level Surfaces",2,80,40,84,TOGGLE
/PICTURE/ Level_3,"Finish Level Surfaces",2,80,40,84,TOGGLE
/PICTURE/ Level_4,"Finish Level Surfaces",3,72,42,76,TOGGLE


#EDIT#
/LABEL/ Adjust Up:
/POSITION/ 12,97,50,97
/SIZE/ 70,14
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 6
/PICTURE/ Level_1,"Adjust Up",10,85,30,90
/PICTURE/ Level_3,"Adjust Up",10,85,30,90

#EDIT#
/LABEL/ Adjust Down:
/POSITION/ 90,97,135,97
/SIZE/ 70,12
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 6
/PICTURE/ Level_1,"Adjust Down",7,91,32,96
/PICTURE/ Level_3,"Adjust Down",7,91,32,96

#PICTUREBOX#
/FILE/ H2OLine_Level_Advanced.jpg
/NAME/ Level_1
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Level_Revolve.jpg
/NAME/ Level_2
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Level_VoluMill2.jpg
/NAME/ Level_3
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Level_VoluMill3.jpg
/NAME/ Level_4
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Options
/COLOR/ DEFAULT

#CHECKBOX#
/LABEL/ Cut Each Pocket to Depth First 
/POSITION/ 10,12
/SIZE/ 112,14
/TYPE/UD_DASSTRING
/PICTURE/Options_1,"Cut Each Pocket to Depth First",4,55,67,64,TOGGLE
/PICTURE/Options_2,"Cut Each Pocket to Depth First",4,55,67,64,TOGGLE
/PICTURE/Options_3,"Cut Each Pocket to Depth First",4,55,67,64,TOGGLE
/PICTURE/Options_4,"Cut Each Pocket to Depth First",4,55,67,64,TOGGLE

#CHOICEBOX#
/LABEL/ Start From:
/POSITION/ 10,29,48,29
/SIZE/ 85,70
/TYPE/UD_DASSTRING
/CHOICES/ "Random","Neg X","Pos X","Neg Y","Pos Y","Neg Z","Pos Z","Nearpt","Inside","Outside","Smallest","Largest"
/PICTURE/Options_1,"Random",72,8,95,15,0
/PICTURE/Options_1,"Neg X",72,16,95,22,1
/PICTURE/Options_1,"Pos X",72,23,95,28,2
/PICTURE/Options_1,"Neg Y",72,29,95,34,3
/PICTURE/Options_1,"Pos Y",72,35,95,40,4
/PICTURE/Options_1,"Neg Z",72,41,95,46,5
/PICTURE/Options_1,"Pos Z",72,48,95,53,6
/PICTURE/Options_1,"Nearpt",72,54,95,60,7
/PICTURE/Options_1,"Inside",72,61,95,67,8
/PICTURE/Options_1,"Outside",72,68,95,75,9
/PICTURE/Options_1,"Smallest",72,76,95,82,10
/PICTURE/Options_1,"Largest",72,83,95,89,11
/PICTURE/Options_2,"Random",72,8,95,15,0
/PICTURE/Options_2,"Neg X",72,16,95,22,1
/PICTURE/Options_2,"Pos X",72,23,95,28,2
/PICTURE/Options_2,"Neg Y",72,29,95,34,3
/PICTURE/Options_2,"Pos Y",72,35,95,40,4
/PICTURE/Options_2,"Neg Z",72,41,95,46,5
/PICTURE/Options_2,"Pos Z",72,48,95,53,6
/PICTURE/Options_2,"Nearpt",72,54,95,60,7
/PICTURE/Options_2,"Inside",72,61,95,67,8
/PICTURE/Options_2,"Outside",72,68,95,75,9
/PICTURE/Options_2,"Smallest",72,76,95,82,10
/PICTURE/Options_2,"Largest",72,83,95,89,11
/PICTURE/Options_3,"Random",72,8,95,15,0
/PICTURE/Options_3,"Neg X",72,16,95,22,1
/PICTURE/Options_3,"Pos X",72,23,95,28,2
/PICTURE/Options_3,"Neg Y",72,29,95,34,3
/PICTURE/Options_3,"Pos Y",72,35,95,40,4
/PICTURE/Options_3,"Neg Z",72,41,95,46,5
/PICTURE/Options_3,"Pos Z",72,48,95,53,6
/PICTURE/Options_3,"Nearpt",72,54,95,60,7
/PICTURE/Options_3,"Inside",72,61,95,67,8
/PICTURE/Options_3,"Outside",72,68,95,75,9
/PICTURE/Options_3,"Smallest",72,76,95,82,10
/PICTURE/Options_3,"Largest",72,83,95,89,11
/PICTURE/Options_4,"Random",72,8,95,15,0
/PICTURE/Options_4,"Neg X",72,16,95,22,1
/PICTURE/Options_4,"Pos X",72,23,95,28,2
/PICTURE/Options_4,"Neg Y",72,29,95,34,3
/PICTURE/Options_4,"Pos Y",72,35,95,40,4
/PICTURE/Options_4,"Neg Z",72,41,95,46,5
/PICTURE/Options_4,"Pos Z",72,48,95,53,6
/PICTURE/Options_4,"Nearpt",72,54,95,60,7
/PICTURE/Options_4,"Inside",72,61,95,67,8
/PICTURE/Options_4,"Outside",72,68,95,75,9
/PICTURE/Options_4,"Smallest",72,76,95,82,10
/PICTURE/Options_4,"Largest",72,83,95,89,11

#EDIT#
/LABEL/ 
/POSITION/ 100,29
/SIZE/ 40,14
/TYPE/UD_DASSTRING
/INPUT/FORM_STRING
/LEN/ 8

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 142,29
/SIZE/ 40,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ Maximum Gap:
/POSITION/ 10,46,60,46
/SIZE/ 90,14
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8
/PICTURE/ Options_1,"Maximum Gap",4,65,43,73
/PICTURE/ Options_2,"Maximum Gap",4,65,43,73
/PICTURE/ Options_3,"Maximum Gap",4,65,43,73
/PICTURE/ Options_4,"Maximum Gap",4,65,43,73

#CHECKBOX#
/LABEL/ Maximum Loops:
/POSITION/ 103,46
/SIZE/ 65,14
/TYPE/UD_DASSTRING

#EDIT#
/LABEL/ 
/POSITION/ 170,46
/SIZE/ 40,14
/TYPE/UD_SCAINT
/INPUT/FORM_STRING
/LEN/ 8
/PICTURE/ Options_1,"Maximum Loops",4,74,45,84
/PICTURE/ Options_2,"Maximum Loops",4,74,45,84

#CHECKBOX#
/LABEL/ Enter Off Part When Possible
/POSITION/ 10,63
/SIZE/ 110,14
/TYPE/UD_DASSTRING
/PICTURE/ Options_1,"Enter Off Part When Possible",4,80,85,87,TOGGLE
/PICTURE/ Options_2,"Enter Off Part When Possible",4,80,85,87,TOGGLE

#EDIT#
/LABEL/ Offset:
/POSITION/ 120,63,143,63
/SIZE/ 70,14
/TYPE/UD_SCAVAL
/INPUT/FORM_STRING
/LEN/ 8
/PICTURE/ Options_1,"Offset",4,88,27,97
/PICTURE/ Options_2,"Offset",0,0,100,100

#CHECKBOX#
/LABEL/ Outputs for 5-axis
/POSITION/ 10,76
/SIZE/180,15
/TYPE/UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Predrilled Holes
/POSITION/ 10,63
/SIZE/ 60,14
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 80,63
/SIZE/ 60,12
/TYPE/ UD_DASSTRING

#PICTUREBOX#
/FILE/ H2OLine_Options_Advanced.jpg
/NAME/ Options_1
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Options_Revolve.jpg
/NAME/ Options_2
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Options_VoluMill2.jpg
/NAME/ Options_3
/POSITION/ 220,10
/SIZE/ 150,125

#PICTUREBOX#
/FILE/ H2OLine_Options_VoluMill3.jpg
/NAME/ Options_4
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Zones
/COLOR/ DEFAULT

#PUSHBUTTON#
/LABEL/ Select
/POSITION/ 10,12
/SIZE/ 60,12
/TYPE/ UD_DASSTRING
/PICTURE/Zones,"Select Zones",0,10,100,70,0

#PUSHBUTTON#
/LABEL/ Deselect All
/POSITION/ 120,12
/SIZE/60,12
/TYPE/ UD_DASSTRING

#LISTBOX#
/LABEL/
/POSITION/ 10,30
/SIZE/ 170,60
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Delete
/POSITION/ 10,90
/SIZE/55,12
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Move Up: /\
/POSITION/ 70,90
/SIZE/55,12
/TYPE/ UD_DASSTRING

#PUSHBUTTON#
/LABEL/ Move Down: \/
/POSITION/ 130,90
/SIZE/55,12
/TYPE/ UD_DASSTRING

#CHOICEBOX#
/LABEL/ Contours Must Be
/POSITION/ 10,110,70,110
/SIZE/115,40
/TYPE/UD_DASSTRING
/CHOICES/ "Completely","Partially","Mostly"
/PICTURE/Zones,"Completely",39,67,67,75,0
/PICTURE/Zones,"Partially",39,76,67,84,1
/PICTURE/Zones,"Mostly",39,85,67,93,2

#LABEL#
/LABEL/ Inside a Zone
/POSITION/ 130,110
/SIZE/55,14
/TYPE/UD_DASSTRING

#CHECKBOX#
/LABEL/ Cut All Remaining Pockets after Zones
/POSITION/ 10,127
/SIZE/140,15
/TYPE/UD_DASSTRING

#PICTUREBOX#
/FILE/ H2OLine_Zones.jpg
/NAME/ Zones
/POSITION/ 220,10
/SIZE/ 150,125

#SECTION#
/NAME/ Colors
/COLOR/ *DEFAULT

#COLOR#
/LABEL/ Surfaces:
/POSITION/ 10,12, 72,12
/SIZE/ 95,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Revolved Surfaces:
/POSITION/ 112,12, 177,12
/SIZE/ 98,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Stock Surface:
/POSITION/ 10,29, 72,29
/SIZE/ 95,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Avoid Surface:
/POSITION/ 112,29, 177,29
/SIZE/ 98,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Bottom Level:
/POSITION/ 10,46, 72,46
/SIZE/ 95,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Top Level:
/POSITION/ 112,46, 177,46
/SIZE/ 98,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Start Point:
/POSITION/ 10,63, 72,63
/SIZE/ 95,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ PreDrilled Holes:
/POSITION/ 112,63, 177,63
/SIZE/ 98,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#COLOR#
/LABEL/ Zones:
/POSITION/ 10,80, 72,80
/SIZE/ 95,12
/TYPE/UD_DASSTRING
/CHOICES/ Default

#CHOICEBOX#
/LABEL/ Unused Geometry:
/POSITION/ 10,97,72,97
/SIZE/95,40
/TYPE/ UD_DASSTRING
/CHOICES/ "Hide","Fade"

#COLOR#
/LABEL/ Color:
/POSITION/ 112,97, 177,97
/SIZE/ 98,12
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
/LABEL/ Pocket Modals
/POSITION/ 78,144
/SIZE/ 54,14
/TYPE/ UD_DASSTRING

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
/FILE/waterline.mp4
/POSITION/ 380,136
/SIZE/ 50,14
/TYPE/ UD_DASSTRING

#HELP#
<Pocket Motion>
Waterline Roughing
==================
Waterline Roughing is an automatic routine used for roughing a part in multiple
levels. This form allows you to input Waterline Roughing parameters.
The POCKET/LAYER command is issued if the form is accepted.

Pocket Motion
-------------
This section selects the pocketing motion method for waterline roughing.

Pocket Type:
------------
This determines what Pocket Type will be used for the waterline roughing.

*Entire Part	Stock:*Calculate   *Contour or *Box   (disable Stock Field)
*Single Pocket	Stock:*Included    Ignore Outer       (disable both Fields)
*Inner Pockets	Stock:*Included    Ignore Outer       (disable both Fields)
*Custom		Stock:(same)       Ignore Outer(same) (enable  both Fields)

Method:
-------
Select the pocketing algorithm to use to create the pocketing motion.  The
choices are as follows:

 - Advanced Pocket (POCKET)
 - VoluMill 2-Axis (VMPOCK)
 - VoluMill 3-Axis (VMP3AX)
 - VoluMill 5-Axis (VMP5AX)

The VoluMill fields are only displayed when VoluMill is licensed.

Cutting Levels:
---------------
For this routine, just as for the Advanced Pocket or the VoluMill routine,
the "UP" direction is defined as the current tool axis.

The algorithm defines the cutting levels (as horizontal planes or revolved
surfaces), and the pocket geometry for each level.

The height of each level is defined the same way for the Advanced and VoluMill
Pocket routines - using the specified Bottom and Top planes or revolved surfaces
together with the Pocket Modal step down parameter (revolved surface is not
allowed for VoluMill).

At every level the surfaces (including Stock) are intersected by the level plane
or the revolved surface. The resulting curves are connected into contours, which
are used as the pocket geometry for the current level. At each lower level the
pocket is compared with the pocket at the immediate level above, and adjusted if
necessary, so that pockets at lower levels are completely inside those above.

<END_SEC>
<Geometry>
Geometry
========
This section is used to select the surfaces or layers for Waterline Roughing.

Layer
-----
Pressing the Layer button Brings up the Select Layer form, which allows you to
select a previously defined layer that contains the surfaces to be waterline
machined.  The layer number can be entered in the text field also.  All
surfaces that reside on this layer will be used in the pocketing algorithm.

The following command is output if surfaces are added to the specified layer.

	DRAFT/MODIFY=<list of surfaces>,LAYER=layernum

Show Layer
----------
Highlights the surfaces residing on the selected layer (including manually 
selected surfaces).

Add Surfaces to Layer
---------------------
Determines whether to add surfaces to the selected layer.

Select
------
Used to add surfaces to the selected layer. This button will only be enabled
when the Define box is checked. All surfaces selected via this form will be 
added to any existing entities already residing on the selected layer, and all
of these surfaces will be used for pocketing.

Deselect All
------------
Deselects the currently selected Layers.

<END_SEC>
<Stock>
Stock
=====
This section is for defining the Stock for the Waterline Roughing routine.

*Entire Part	Stock:*Calculate   *Contour or *Box   (disable Stock Field)
*Single Pocket	Stock:*Included    Ignore Outer       (disable both Fields)
*Inner Pockets	Stock:*Included    Ignore Outer       (disable both Fields)
*Custom		Stock:(same)       Ignore Outer(same) (enable  both Fields)

(Stock choice)
--------------
Included or Calculate. 
Included means no extra Stock surfaces will be used to machine the part. 
Calculate will cause the pocketing algorithm to automatically calculate a
stock based on the outer surfaces of the part.

Ignore Outer
------------
A choice possible only if the Stock is Included meaning the outermost
contours will be ignored when creating the pocket geometry.

Expansion
---------
The expansion distance is used to expand the boundary of the stock when it is
calculated or specified by boundary (stock) surfaces.

(Stock choice)
--------------
Contour or Box - A choice possible only if the Stock is Calculated.  
Contour means the stock has the shape of the part's outer surfaces vertical
projection. Box means the Stock is a rectangular box around the part.

Stock Surfaces
--------------
Opens the Waterline Surface List form, to specify Stock surfaces which will
be used only to create the perimeter contour at each cutting level.

Avoid Surfaces
--------------
Opens the Waterline Surface List form, to specify the surfaces which will
be used only in the overhanging logic, to avoid at each cutting level.

<END_SEC>
<Levels>
Levels

Pocket Bottom
-------------
IF BASE IS A PLANE
Stock, Plane, Depth from Top, or Z-level. Defines the type of the Pocket 
Bottom as follows: 
	Stock - a plane at the part's lowest level. 
	Plane - an existing plane or planar surface. 
	Depth from Top - the Bottom is defined relative to the Top. 
	Z-level - the horizontal plane at a specified height.

IF BASE IS A SURFACE OF REVOLUTION
Stock, Depth from Top, or Base Offset. Defines the type of the Pocket 
Bottom as follows: 
	Stock - the Road Surface offset at the part's lowest level. 
	Depth from Top - the Bottom is defined relative to the Top. 
	Base Offset - the specified Road Surface offset.

(Text Field)
Contains the current Pocket Bottom: the label if Plane, the height parameter
if Depth from Top, or Z-level.

Select
------
(Not active if based on a surface of revolution.)
Allows picking a plane or planar surface as Bottom.

Bottom Offset
-------------
Used only with Stock, or Plane Bottom types. The Bottom is defined as the 
ZLARGE offset of the selected plane by the specified parameter.

Pocket Top
----------
IF BASE IS A PLANE
Stock, Plane, Distance, or Z-level. Defines the type of the Pocket Top as 
follows: 
	Stock - a plane at the part's highest level. 
	Plane - an existing plane or planar surface. 
	Distance - the Top is defined relative to the Bottom. 
	Z-level - the horizontal plane at a specified height.

IF BASE IS A SURFACE OF REVOLUTION
Stock, Distance, or Base Offset. Defines the type of the Pocket Top as
follows: 
	Stock - the Road Surface offset at the part's highest level. 
	Distance - the Top is defined relative to the Bottom. 
	Base Offset - the specified Road Surface offset.

(Text Field)
------------
Contains the current Pocket Top: the label if Plane, the height parameter if 
Distance, or Z-level.

Select
------
(Not active if based on a surface of revolution.) Allows picking a plane or
planar surface as Top.

Top Offset
----------
Used only with Stock, or Plane Top types. The Top is defined as the offset of 
the selected plane by the specified parameter.

Incremental Step Height
-----------------------
When checked, intermediate machining levels will be internally created by
moving the cutting level up by the given height.  The boundaries from the
previous cutting level will be treated as islands to prevent unnecessary
motion.  The number of incremental levels will be the maximum number so the
incremental cutting level does not pass a previous standard cutting level.
This parameter is only recognized when VoluMill 3-Axis waterline is used.

(Text Field)
------------
Contains the current Incremental Step Height value.

Finish Level Surfaces
---------------------
When checked, intermediate machining levels will be internally created to
finish horizontal planar surfaces if such surfaces are found between regular
cutting levels. 

Adjust Up
---------
If an intermediate machining level is found within this value above the
current regular cutting level, the regular level will be moved upward to the
intermediate level. This parameter is not available for 3-Axis VoluMill.

Adjust Down
---------
If an intermediate machining level is found within this value below the
current regular cutting level, the regular level will be moved downward to the
intermediate level. This parameter is not available for 3-Axis VoluMill.

<END_SEC>
<Options>
Options
=======
The Options section controls the settings used for creating the Waterline 
Roughing motion.

Cut Each Pocket to Depth First
------------------------------
When enabled the Waterline Roughing motion cuts each individual pocket to
depth first before advancing to the next pocket.

Start From
----------
Defines the order of the pocketing as follows: 
	Random   - no particular order;
	Neg X    - order the pockets along the X-axis;
	Pos X    - reverse order along the X-axis;
	Neg Y    - order the pockets along the Y-axis;
	Pos Y    - reverse order along the Y-axis;
	Neg Z    - order the pockets along the Z-axis;
	Pos Z    - reverse order along the Z-axis;
	Nearpt   - first do the pockets closer to the near point;
	Inside   - first do the pockets closer to the center;
	Outside  - first do the pockets farser from the center;
	Smallest - first do the smallest pockets;
	Largest	 - first do the largest pockets.

(Text Field)
------------
Contains the current near point, ignored if the "Start From" choice is not
"Nearpt".

Select
------
Allows picking of a near point.

Maximum Gap
-----------
The parameter used by the algorithm when it connects separate surface/level 
plane intersections into a composite curve used as pocket geometry. 

Maximum Loops
-------------
Chooses whether to use the optional maximum number of loops.

(Text Field)
------------
Contains the current maximum number of loops - active only if the option
above is chosen.

Enter Off Part When Possible
----------------------------
If selected, for each entry move the algorithm tries to find a near
point on the stock from which the tool can enter without gouging the pocket
islands. If such a stock point is found, the tool is repositioned there at the
cutting level and then moved (at the entry feedrate) horizontally to the first
cutting contour point. If such point cannot be found, the tool enters by the
current POKMOD entry mode.

Offset
------
The offset distance is used to specify the distance at which the edge of the
cutter will be positioned outside of the part.

VoluMill
--------
Maximum Gap
-----------
The parameter used by the algorithm when it connects separate surface/level 
plane intersections into a composite curve used as pocket geometry.

Predrilled Holes
----------------
Allows the selection of points to use as the positions of predrilled holes.
VoluMill will use these holes as the entry to the pocket instead of ramping
into the material.  These holes must be drilled prior to performing the
pocketing motion. Predrilled holes are only supported by VoluMill pocketing.

Deselect All
------------
Deselects all the currently selected Predrilled Holes. 
The list is emptied as a result.

<END_SEC>
<Zones>
Zones
=====
Zone Ordering defines zones and sets options for the Waterline Roughing.
Zone Ordering for the Waterline Roughing command is specified by:
defining a list of zones and indicating the rule that decides when
a pocket belongs to a zone.

A valid zone is the horizontal projection of a closed curve or circle. 
It defines a region on the pocket cutting plane.

Select
------
Used to add curves to the list of zones. If a curve already in
the list is selected, it will be removed form the list.

Deselect All
------------
Deselects the currently selected curves. The list is emptied as a result.

Zone List
---------
This list box contains the current curve list, in order. One can alter
the list by selecting lines and using the buttons 'Delete', 'Move Up',
and 'Move Down'.

Delete
------
Removes the highlighted curve from the list.

Move Up
-------
Moves the highlighted curve one position up in the zone list.

Move Down
---------
Moves the highlighted curve one position down in the zone list.

Contours Must Be Partially, (Completely, Mostly) Inside a Zone
--------------------------------------------------------------
This choice box defines the rule by which the pocket-in-a-zone decisions
are made, as follows:
	Partially - some part of the perimeter contour must be inside
	the zone curve.
	Completely - the perimeter must be completely inside the curve.
	Mostly - most of the perimeter area must be inside the curve.

Cut All Remaining Pockets After Zones
--------------------------------------
If checked, the pockets not in any of the listed zones will be cut after
the last zone is done.
If not checked, the pockets not in zones will not be cut.

<END_SEC>
<Colors>
Colors
======
The Colors section defines the colors that will be used to highlight the geometry 
selected while in the Waterline Roughing form.  All entiities that can be  
picked from the screen are listed in this section.

Unused Geometry:
----------------
When pressing the 'Geometry' Action item button to "hide" geometry not used in 
the Waterline operation you have the option of either invisible this 
geometry (Hide) or displaying the geometry as translucent and with dotted lines
(Fade). The Color field applies to the faded geometry.

<END_SEC>
<ALL>
Action Buttons
==============
The Action Buttons are located at the right hand of the motion generation forms
and allow you to perform specific actions that will assist you in visualizing 
the results of the motion form settings.

Preview
-------
Previews the motion without writing out the command or permanently storing the 
generated motion.  Press the OK or Apply button to write out the command and 
motion. The generated command can be saved after the preview even if there is
an error. To save the command, make no changes to the settings and press the 
OK button. The command will then be available for editing in the command line.

Apply
-----
Outputs and processes the command(s) without taking down the form so that other
motion can be created.

Reset Motion
------------
Resets all form fields to their settings/values when the form was first entered.
This button is useful after pressing the Apply button if you want to start fresh
or when you have made numerous changes to the form settings and are not getting
the output you desire.

Playback
--------
Displays Playback Preview Motion interface, allowing you to step through and 
animate the motion generated using the Preview button.  This button is only 
active when Preview motion is displayed on the screen.

Verify
------
Pressing this button allows you to verify the Preview motion by using the 
material removal process of NCLIPV.  It displays the Verify Preview motion 
interface, allowing you to simulate the material removal for the Preview 
motion. This button is only active when you have a valid NCLIPV license and 
Preview motion is displayed on the screen.

Geometry
--------
Pressing the Geometry button the first time will hide all unused geometry 
from the screen, leaving only the geometry that was selected during this 
session  displayed in the selected colors.  The 'Unused Geometry' field in 
the 'Colors' section defines whether the unused geometry will be invisible 
or just faded.

Pressing this button a second time will redisplay the unused geometry.

View
----
Takes down the form(s) and enters dynamic viewing.


Pocketing Modals
================
Opens the Pocket Modals form allowing you to enter the Pocket Modal parameters
to use when pocketing using either Advanced Pocket or Waterline Roughing.
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

 - Shortest: Pass takes the shortest way around the boundary.
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
        way out towards the pocket perimeter.
 - In:  The tool will start on the outside of the pocket and works its way in
        toward the center of the pocket.

Cutting Direction
-----------------
When Lace or Scrub is in effect, then the pocket direction can be specified as:

 - Positive X, Negative Y, etc.
 - Long:   Along the longest side of a rectangle encompassing the pocket
           geometry.
 - Short:  Along the shortest side of the rectangle.
 - Vector: Along a user specified vector or point-vector.

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
 - Arc:   Use a double-arc or S-shaped transition between pocket loops.

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


Force Output of POKMOD Command
------------------------------
If this box is checked, the POKMOD statement is output unconditionally. If not
checked, the POKMOD statement is output only if:

 1) Pocket Modals have been changed.
 2) A valid POCKET statement is also output by the main form.
