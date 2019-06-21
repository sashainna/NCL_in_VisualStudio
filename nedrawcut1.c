/*********************************************************************
**    NAME         :  nedrawcut1.c
**       CONTAINS:
**          ncl_drawform_bore
**          ncl_drawform_reamer
**          ncl_drawform_chamfer
**          ncl_drawform_blade
**          ncl_drawform_square
**          ncl_drawform_diamond
**    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nedrawcut1.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:31
*********************************************************************/

#include "usysdef.h"
#include <math.h>

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_bore(cut_array)
**
**		This function draws a Boring Bar for a form picture.
**
**    PARAMETERS   
**       INPUT  : 
**           none
**       OUTPUT : 
**           cut_array   -  Array to hold the cutter drawing parameters.
**    RETURNS      : none
**    SIDE EFFECTS :
**		    Allocates memory to store the the cutter array for text strings.
**    WARNINGS     : none
*********************************************************************/
void ncl_drawform_bore(cut_array)
int cut_array[];
{
	int n;
/*
.....move to (20,15)
*/
	n = 0;
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 15;
/*
.....set text color to (20, 20, 20), red
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
/*
......draw text string "Boring Tool"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 11;
	cut_array[n] = (int)malloc(12*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Boring Tool");
/*
.....move to (20,180)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 170;
/*
......draw text string "Cutter/1.0,.25,2.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 16;
	cut_array[n] = (int)malloc(17*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Cutter/dia,0,hgt");
/*
.....move to (65,95)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 85;
	cut_array[n++] = 135;
/*
......draw text string
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[n++]), "dia");
/*
.....move to (160, 120)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 158;
	cut_array[n++] = 105;
/*
......draw text string "2.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[n++]), "hgt");
/*
......Draw Shank text
*/
	cut_array[n++] = 1;
	cut_array[n++] = 135;
	cut_array[n++] = 70;

	cut_array[n++] = 5;
	cut_array[n++] = 5;
	cut_array[n] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Shank");
/*
.....change pen color to (0,0,0) 
*/
	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....move to (40, 40)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 70;
	cut_array[n++] = 40;
/*
.....Line to (100,40)
*/
	cut_array[n++] = 2;
	cut_array[n++] = 110;
	cut_array[n++] = 40;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 110;
	cut_array[n++] = 120;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 70;
	cut_array[n++] = 120;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 70;
	cut_array[n++] = 40;
/*
......Move to 
*/
	cut_array[n++] = 1;
	cut_array[n++] = 70;
	cut_array[n++] = 100;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 55;
	cut_array[n++] = 105;
/*
.....draw circle 
*/
	cut_array[n++] = 4;
	cut_array[n++] = 55;
	cut_array[n++] = 110;
	cut_array[n++] = 5;
	cut_array[n++] = 1;
	cut_array[n++] = 55;
	cut_array[n++] = 115; 
/*
.....move to 
*/
	cut_array[n++] = 1;
	cut_array[n++] = 55;
	cut_array[n++] = 115;
/*
.....Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 70;
	cut_array[n++] = 120;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 75;
	cut_array[n++] = 140;
	cut_array[n++] = 3;
	cut_array[n++] = 50;
	cut_array[n++] = 140;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 105;
	cut_array[n++] = 140;
	cut_array[n++] = 3;
	cut_array[n++] = 130;
	cut_array[n++] = 140;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 150;
	cut_array[n++] = 70;
	cut_array[n++] = 3;
	cut_array[n++] = 150;
	cut_array[n++] = 40;
/*
.....Draw Height arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = 150;
	cut_array[n++] = 110;
	cut_array[n++] = 3;
	cut_array[n++] = 150;
	cut_array[n++] = 120;

	cut_array[n++] = 1;
	cut_array[n++] = 150;
	cut_array[n++] = 110;
	cut_array[n++] = 3;
	cut_array[n++] = 150;
	cut_array[n++] = 100;
/*
.....Draw Shank arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = 150;
	cut_array[n++] = 80;
	cut_array[n++] = 3;
	cut_array[n++] = 150;
	cut_array[n++] = 100;

	cut_array[n++] = 1;
	cut_array[n++] = 150;
	cut_array[n++] = 60;
	cut_array[n++] = 3;
	cut_array[n++] = 150;
	cut_array[n++] = 40;
/*
.....Set dash line
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 50;
	cut_array[n++] = 100;
	cut_array[n++] = 2;
	cut_array[n++] = 50;
	cut_array[n++] = 155;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 130;
	cut_array[n++] = 100;
	cut_array[n++] = 2;
	cut_array[n++] = 130;
	cut_array[n++] = 155;
/*
.....move to 
*/
	cut_array[n++] = 1;
	cut_array[n++] = 110;
	cut_array[n++] = 120;
/*
.....Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 125;
	cut_array[n++] = 115;
/*
.....draw circle 
*/
	cut_array[n++] = 4;
	cut_array[n++] = 125;
	cut_array[n++] = 110;
	cut_array[n++] = 5;
	cut_array[n++] = 1;
	cut_array[n++] = 125;
	cut_array[n++] = 105; 
/*
.....Move To
*/
	cut_array[n++] = 1;
	cut_array[n++] = 125;
	cut_array[n++] = 105;
/*
.....Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 110;
	cut_array[n++] = 100;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 135;
	cut_array[n++] = 120;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 120;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 135;
	cut_array[n++] = 100;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 100;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 125;
	cut_array[n++] = 40;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 40;

	cut_array[n++] = 99;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_reamer(cut_array)
**
**		This function draws a Reamer for a form picture.
**
**    PARAMETERS   
**       INPUT  : 
**           none
**       OUTPUT : 
**           cut_array   -  Array to hold the cutter drawing parameters.
**    RETURNS      : none
**    SIDE EFFECTS :
**		    Allocates memory to store the the cutter array for text strings.
**    WARNINGS     : none
*********************************************************************/
void ncl_drawform_reamer(cut_array)
int cut_array[];
{
	int n;
/*
.....move to (20,15)
*/
	cut_array[0] = 1;
	cut_array[1] = 20;
	cut_array[2] = 15;
/*
.....set text color to (20, 20, 20), red
*/
	cut_array[3] = 6;
	cut_array[4] = 257;
	cut_array[5] = 257;
	cut_array[6] = 257;
/*
......draw text string "Reamer"
*/
	cut_array[7] = 5;
	cut_array[8] = 6;
	cut_array[9] = (int)malloc(7*sizeof(char));
	strcpy((char*)(cut_array[9]), "Reamer");
/*
.....move to (20,180)
*/
	cut_array[10] = 1;
	cut_array[11] = 20;
	cut_array[12] = 170;
/*
......draw text string "Cutter/1.0,.25,2.0"
*/
	cut_array[13] = 5;
	cut_array[14] = 10;
	cut_array[15] = (int)malloc(11*sizeof(char));
	strcpy((char*)(cut_array[15]), "Cutter/dia");
/*
.....move to (65,95)
*/
	cut_array[16] = 1;
	cut_array[17] = 65;
	cut_array[18] = 95;
/*
......draw text string "1.0"
*/
	cut_array[19] = 5;
	cut_array[20] = 3;
	cut_array[21] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[21]), "dia");
/*
.....move to (160, 120)
*/
	n = 22;
	cut_array[n++] = 1;
	cut_array[n++] = 145;
	cut_array[n++] = 120;
/*
......draw text string "2.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[n++]), "hgt");
/*
.....change pen color to (0,0,0) 
*/
	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....move to (40, 40)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 40;
	cut_array[n++] = 40;
/*
.....Line to (100,40)
*/
	cut_array[n++] = 2;
	cut_array[n++] = 100;
	cut_array[n++] = 40;
/*
......Line to (100,145)
*/
	cut_array[n++] = 2;
	cut_array[n++] = 100;
	cut_array[n++] = 160;
/*
.....Line to (55, 160)
*/
	cut_array[n++] = 2;
	cut_array[n++] = 40;
	cut_array[n++] = 160;
/*
.....Line to (40, 40)
*/
	cut_array[n++] = 2;
	cut_array[n++] = 40;
	cut_array[n++] = 40;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 10;
	cut_array[n++] = 100;
	cut_array[n++] = 3;
	cut_array[n++] = 40;
	cut_array[n++] = 100;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 130;
	cut_array[n++] = 100;
	cut_array[n++] = 3;
	cut_array[n++] = 100;
	cut_array[n++] = 100;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 150;
	cut_array[n++] = 120;
	cut_array[n++] = 3;
	cut_array[n++] = 150;
	cut_array[n++] = 40;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 150;
	cut_array[n++] = 135;
	cut_array[n++] = 3;
	cut_array[n++] = 150;
	cut_array[n++] = 160;
/*
.....Set dash line
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 110;
	cut_array[n++] = 40;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 40;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 110;
	cut_array[n++] = 160;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 160;

	cut_array[n++] = 99;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_chamfer(cut_array)
**
**		This function draws a Chamfer Tool for a form picture.
**
**    PARAMETERS   
**       INPUT  : 
**           none
**       OUTPUT : 
**           cut_array   -  Array to hold the cutter drawing parameters.
**    RETURNS      : none
**    SIDE EFFECTS :
**		    Allocates memory to store the the cutter array for text strings.
**    WARNINGS     : none
*********************************************************************/
void ncl_drawform_chamfer(cut_array)
int cut_array[];
{
	int n;
/*
.....move to (20,15)
*/
	n = 0;
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 15;
/*
.....set text color to (20, 20, 20), red
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
/*
......draw text string "Chamfer Tool"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 12;
	cut_array[n] = (int)malloc(13*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Chamfer Tool");
/*
.....move to (20,180)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 170;
/*
......draw text string "Cutter/1.0,.25,2.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 22;
	cut_array[n] = (int)malloc(23*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Cutter/dia, hgt, angle");
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 73;
	cut_array[n++] = 150;
/*
......draw text string 
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[n++]), "dia");
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 13;
	cut_array[n++] = 115;
/*
......draw text string 
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[n++]), "hgt");
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 125;
	cut_array[n++] = 115;
/*
......draw text string 
*/
	cut_array[n++] = 5;
	cut_array[n++] = 5;
	cut_array[n] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[n++]), "angle");
/*
.....Draw Shank text
*/
	cut_array[n++] = 1;
	cut_array[n++] = 130;
	cut_array[n++] = 65;

	cut_array[n++] = 5;
	cut_array[n++] = 5;
	cut_array[n] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Shank");
/*
.....change pen color to (0,0,0) 
*/
	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....Draw Shank
*/
	cut_array[n++] = 1;
	cut_array[n++] = 60;
	cut_array[n++] = 100;
	cut_array[n++] = 2;
	cut_array[n++] = 60;
	cut_array[n++] = 40;
	cut_array[n++] = 2;
	cut_array[n++] = 100;
	cut_array[n++] = 40;
	cut_array[n++] = 2;
	cut_array[n++] = 100;
	cut_array[n++] = 100;
/*
.....move to (40, 40)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 120;
	cut_array[n++] = 100;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 95;
	cut_array[n++] = 140;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 65;
	cut_array[n++] = 140;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 40;
	cut_array[n++] = 100;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 120;
	cut_array[n++] = 100;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 35;
	cut_array[n++] = 155;
	cut_array[n++] = 3;
	cut_array[n++] = 65;
	cut_array[n++] = 155;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 125;
	cut_array[n++] = 155;
	cut_array[n++] = 3;
	cut_array[n++] = 95;
	cut_array[n++] = 155;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 115;
	cut_array[n++] = 3;
	cut_array[n++] = 20;
	cut_array[n++] = 100;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 125;
	cut_array[n++] = 3;
	cut_array[n++] = 20;
	cut_array[n++] = 140;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 75;
	cut_array[n++] = 105;
	cut_array[n++] = 3;
	cut_array[n++] = 95;
	cut_array[n++] = 115;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 130;
	cut_array[n++] = 115;
	cut_array[n++] = 3;
	cut_array[n++] = 112;
	cut_array[n++] = 115;
/*
.....Draw Shank arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = 140;
	cut_array[n++] = 75;
	cut_array[n++] = 3;
	cut_array[n++] = 140;
	cut_array[n++] = 100;

	cut_array[n++] = 1;
	cut_array[n++] = 140;
	cut_array[n++] = 65;
	cut_array[n++] = 3;
	cut_array[n++] = 140;
	cut_array[n++] = 40;
/*
.....Set dash line
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 65;
	cut_array[n++] = 130;
	cut_array[n++] = 2;
	cut_array[n++] = 65;
	cut_array[n++] = 165;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 95;
	cut_array[n++] = 105;
	cut_array[n++] = 2;
	cut_array[n++] = 95;
	cut_array[n++] = 165;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 10;
	cut_array[n++] = 100;
	cut_array[n++] = 2;
	cut_array[n++] = 40;
	cut_array[n++] = 100;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 10;
	cut_array[n++] = 140;
	cut_array[n++] = 2;
	cut_array[n++] = 60;
	cut_array[n++] = 140;
/*
.....Draw Shank lines
*/
	cut_array[n++] = 1;
	cut_array[n++] = 120;
	cut_array[n++] = 100;
	cut_array[n++] = 2;
	cut_array[n++] = 150;
	cut_array[n++] = 100;

	cut_array[n++] = 1;
	cut_array[n++] = 100;
	cut_array[n++] = 40;
	cut_array[n++] = 2;
	cut_array[n++] = 150;
	cut_array[n++] = 40;

	cut_array[n++] = 99;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_blade(cut_array)
**
**		This function draws a Ultrasonic Blade for a form picture.
**
**    PARAMETERS   
**       INPUT  : 
**           none
**       OUTPUT : 
**           cut_array   -  Array to hold the cutter drawing parameters.
**    RETURNS      : none
**    SIDE EFFECTS :
**		    Allocates memory to store the the cutter array for text strings.
**    WARNINGS     : none
*********************************************************************/
void ncl_drawform_blade(cut_array)
int cut_array[];
{
	int n;
/*
.....move to (20,15)
*/
	n = 0;
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 15;
/*
.....set text color to (20, 20, 20), red
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
/*
......draw text string "Blade"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 5;
	cut_array[n] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Blade");
/*
.....move to (20,180)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 170;
/*
......draw text string "Cutter/1.0,.25,2.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 32;
	cut_array[n] = (int)malloc(33*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Cutter/Blade,wid, chiz, hgt, ang");
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 60;
	cut_array[n++] = 150;
/*
......draw text string 
*/
	cut_array[n++] = 5;
	cut_array[n++] = 6;
	cut_array[n] = (int)malloc(7*sizeof(char));
	strcpy((char*)(cut_array[n++]), "chizel");
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 120;
	cut_array[n++] = 65;
/*
......draw text string 
*/
	cut_array[n++] = 5;
	cut_array[n++] = 6;
	cut_array[n] = (int)malloc(7*sizeof(char));
	strcpy((char*)(cut_array[n++]), "height");
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 5;
	cut_array[n++] = 50;
/*
......draw text string 
*/
	cut_array[n++] = 5;
	cut_array[n++] = 5;
	cut_array[n] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[n++]), "angle");
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 160;
	cut_array[n++] = 140;
/*
......draw text string 
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)cut_array[n++], "wid");
/*
.....change pen color to (0,0,0) 
*/
	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....move to (40, 40)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 40;
	cut_array[n++] = 40;
/*
.....Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 100;
	cut_array[n++] = 40;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 100;
	cut_array[n++] = 100;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 80;
	cut_array[n++] = 120;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 60;
	cut_array[n++] = 120;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 40;
	cut_array[n++] = 100;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 40;
	cut_array[n++] = 40;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 35;
	cut_array[n++] = 140;
	cut_array[n++] = 3;
	cut_array[n++] = 60;
	cut_array[n++] = 140;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 105;
	cut_array[n++] = 140;
	cut_array[n++] = 3;
	cut_array[n++] = 80;
	cut_array[n++] = 140;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 130;
	cut_array[n++] = 60;
	cut_array[n++] = 3;
	cut_array[n++] = 130;
	cut_array[n++] = 40;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 130;
	cut_array[n++] = 80;
	cut_array[n++] = 3;
	cut_array[n++] = 130;
	cut_array[n++] = 120;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 140;
	cut_array[n++] = 130;
	cut_array[n++] = 3;
	cut_array[n++] = 160;
	cut_array[n++] = 130;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 190;
	cut_array[n++] = 130;
	cut_array[n++] = 3;
	cut_array[n++] = 170;
	cut_array[n++] = 130;
/*
.....Move To
*/
	cut_array[n++] = 1;
	cut_array[n++] = 170;
	cut_array[n++] = 40;
/*
.....Line to
*/
	cut_array[n++] = 2;
	cut_array[n++] = 160;
	cut_array[n++] = 40;
/*
.....Line to
*/
	cut_array[n++] = 2;
	cut_array[n++] = 160;
	cut_array[n++] = 120;
/*
.....Line to
*/
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 120;
/*
.....Line to
*/
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 40;
/*
.....Set dash line
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 110;
	cut_array[n++] = 40;
	cut_array[n++] = 2;
	cut_array[n++] = 150;
	cut_array[n++] = 40;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 100;
	cut_array[n++] = 120;
	cut_array[n++] = 2;
	cut_array[n++] = 150;
	cut_array[n++] = 120;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 60;
	cut_array[n++] = 110;
	cut_array[n++] = 2;
	cut_array[n++] = 60;
	cut_array[n++] = 150;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 80;
	cut_array[n++] = 110;
	cut_array[n++] = 2;
	cut_array[n++] = 80;
	cut_array[n++] = 150;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 40;
	cut_array[n++] = 100;
	cut_array[n++] = 2;
	cut_array[n++] = 10;
	cut_array[n++] = 70;
/*
.....Move to and Line to
*/
/*	cut_array[n++] = 1;
	cut_array[n++] = 40;
	cut_array[n++] = 100;
	cut_array[n++] = 2;
	cut_array[n++] = 40;
	cut_array[n++] = 120;*/
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 170;
	cut_array[n++] = 120;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 140;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 160;
	cut_array[n++] = 120;
	cut_array[n++] = 2;
	cut_array[n++] = 160;
	cut_array[n++] = 140;

/*
.....Set solid line
*/
	cut_array[n++] = 8;
	cut_array[n++] = 0;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 60;
	cut_array[n++] = 3;
	cut_array[n++] = 10;
	cut_array[n++] = 70;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 30;
	cut_array[n++] = 55;
	cut_array[n++] = 3;
	cut_array[n++] = 40;
	cut_array[n++] = 55;

	cut_array[n++] = 99;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_square(cut_array)
**
**		This function draws a Lathe Square Insert for a form picture.
**
**    PARAMETERS   
**       INPUT  : 
**           none
**       OUTPUT : 
**           cut_array   -  Array to hold the cutter drawing parameters.
**    RETURNS      : none
**    SIDE EFFECTS :
**		    Allocates memory to store the the cutter array for text strings.
**    WARNINGS     : none
*********************************************************************/
void ncl_drawform_square(cut_array)
int cut_array[];
{
	int n,hcen[2],dia,rad,hofs,hgt;
	char str[80];
/*
.....Initialize routine
*/
	hcen[0] = 90;
	hcen[1] = 85;
	dia = 35;
	rad = 5;
	hofs = 20;
	hgt = 15;
/*
.....move to (20,15)
*/
	n = 0;
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 15;
/*
.....set text color to (20, 20, 20), red
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
/*
......draw text string
*/
	strcpy(str, "Square Insert");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 170;
/*
......draw text string
*/
	strcpy(str,"Cutter/Lathe,rad,dia,hgt,90,mntang");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[3] = 10;
	cut_array[4] = 0;
	cut_array[5] = 0;
	cut_array[6] = 0;
/*
.....Set dashed line
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1];
/*
.....Draw inscribed circle
*/
	cut_array[n++] = 4;
	cut_array[n++] = hcen[0];
	cut_array[n++] = hcen[1];
	cut_array[n++] = dia;
	cut_array[n++] = 0;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1];
/*
.....Set solid line
*/
	cut_array[n++] = 8;
	cut_array[n++] = 0;
/*
.....Draw bottom
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia - rad;
	cut_array[n++] = hcen[1] + dia;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] - dia + rad;
	cut_array[n++] = hcen[1] + dia;
/*
.....Draw radius
*/
	cut_array[n++] = 4;
	cut_array[n++] = hcen[0] - dia + rad;
	cut_array[n++] = hcen[1] + dia - rad;
	cut_array[n++] = rad;
	cut_array[n++] = 0;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] + dia - rad;
/*
......Draw left side
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] -dia;
	cut_array[n++] = hcen[1] + dia - rad;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] - dia + rad;
/*
.....Draw radius
*/
	cut_array[n++] = 4;
	cut_array[n++] = hcen[0] - dia + rad;
	cut_array[n++] = hcen[1] - dia + rad;
	cut_array[n++] = rad;
	cut_array[n++] = 0;
	cut_array[n++] = hcen[0] - dia + rad;
	cut_array[n++] = hcen[1] - dia;
/*
......Draw top
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia + rad;
	cut_array[n++] = hcen[1] - dia;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] + dia - rad;
	cut_array[n++] = hcen[1] - dia;
/*
.....Draw radius
*/
	cut_array[n++] = 4;
	cut_array[n++] = hcen[0] + dia - rad;
	cut_array[n++] = hcen[1] - dia + rad;
	cut_array[n++] = rad;
	cut_array[n++] = 0;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] - dia + rad;
/*
......Draw right side
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] - dia + rad;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] + dia - rad;
/*
.....Draw radius
*/
	cut_array[n++] = 4;
	cut_array[n++] = hcen[0] + dia - rad;
	cut_array[n++] = hcen[1] + dia - rad;
	cut_array[n++] = rad;
	cut_array[n++] = 0;
	cut_array[n++] = hcen[0] + dia - rad;
	cut_array[n++] = hcen[1] + dia;
/*
.....Draw height picture
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] + dia + hofs + hgt;

	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] + dia + hofs + hgt;

	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] + dia + hofs;
/*
......draw text string
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia - 30;
	cut_array[n++] = hcen[1] + dia - 5;

	strcpy(str, "rad");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....Draw lines with arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia - 20;
	cut_array[n++] = hcen[1] + dia + 5;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] - dia - 5;
	cut_array[n++] = hcen[1] + dia + 5;
	cut_array[n++] = 3;
	cut_array[n++] = hcen[0] - dia + 2;
	cut_array[n++] = hcen[1] + dia - 1;
/*
......draw text string
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - 6;
	cut_array[n++] = hcen[1] + dia + 5;

	strcpy(str, "dia");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....Draw side lines
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] + dia + 2;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] + dia + hofs - 2;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] + dia + 2;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] + dia + hofs - 2;

	cut_array[n++] = 8;
	cut_array[n++] = 0;
/*
.....Draw lines with arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - 10;
	cut_array[n++] = hcen[1] + dia + 10;
	cut_array[n++] = 3;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] + dia + 10;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + 10;
	cut_array[n++] = hcen[1] + dia + 10;
	cut_array[n++] = 3;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] + dia + 10;
/*
......draw text string
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia + 20;
	cut_array[n++] = hcen[1] + dia + hofs + hgt/2 - 5;

	strcpy(str, "hgt");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....Draw side lines
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia + 2;
	cut_array[n++] = hcen[1] + dia + hofs;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] + dia + 15;
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia + 2;
	cut_array[n++] = hcen[1] + dia + hofs + hgt;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] + dia + 15;
	cut_array[n++] = hcen[1] + dia + hofs + hgt;

	cut_array[n++] = 8;
	cut_array[n++] = 0;
/*
.....Draw lines with arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia + 10;
	cut_array[n++] = hcen[1] + dia + hofs + hgt/2;
	cut_array[n++] = 3;
	cut_array[n++] = hcen[0] + dia + 10;
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia + 10;
	cut_array[n++] = hcen[1] + dia + hofs + hgt/2;
	cut_array[n++] = 3;
	cut_array[n++] = hcen[0] + dia + 10;
	cut_array[n++] = hcen[1] + dia + hofs + hgt;
/*
......draw text string
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia + 12;
	cut_array[n++] = hcen[1] - dia + 20;

	strcpy(str,"mntang");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....Draw arc with arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia - 20;
	cut_array[n++] = hcen[1] - dia - 20;

	cut_array[n++] = 4;
	cut_array[n++] = hcen[0] + 5;
	cut_array[n++] = hcen[1];
	cut_array[n++] = dia + 20;
	cut_array[n++] = 0;
	cut_array[n++] = hcen[0] + dia + 20;
	cut_array[n++] = hcen[1] - dia + 20;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia - 20 + 3;
	cut_array[n++] = hcen[1] - dia - 20 + 3;

	cut_array[n++] = 3;
	cut_array[n++] = hcen[0] + dia - 20;
	cut_array[n++] = hcen[1] - dia - 20 + 2;
/*
.....End of routine
*/
	cut_array[n++] = 99;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_diamond(cut_array)
**
**		This function draws a Lathe Diamond Insert for a form picture.
**
**    PARAMETERS   
**       INPUT  : 
**           none
**       OUTPUT : 
**           cut_array   -  Array to hold the cutter drawing parameters.
**    RETURNS      : none
**    SIDE EFFECTS :
**		    Allocates memory to store the the cutter array for text strings.
**    WARNINGS     : none
*********************************************************************/
void ncl_drawform_diamond(cut_array)
int cut_array[];
{
	int n,hcen[2],dia,rad,hofs,hgt,temp;
	UU_REAL cen1[2],cen2[2],sina,cosa,htan;
	char str[80];
/*
.....Initialize routine
*/
	hcen[0] = 90;
	hcen[1] = 85;
	dia = 25;
	rad = 5;
	hofs = 25;
	hgt = 15;
/*
.....Calculate center of tool radii
*/
	sina = .707;
	cosa = .707;
	htan = .414;
	temp = dia - rad;
	cen1[0] = hcen[0] - temp/htan;
	cen1[1] = hcen[1] + temp;
	cen2[0] = hcen[0] + temp/htan;
	cen2[1] = hcen[1] - temp;
/*
.....move to (20,15)
*/
	n = 0;
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 15;
/*
.....set text color to (20, 20, 20), red
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
/*
......draw text string
*/
	strcpy(str, "Diamond Insert");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 170;
/*
......draw text string
*/
	strcpy(str,"Cutter/Lathe,rad,dia,hgt,ang,mntang");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;

/*
.....Set dashed line
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1];
/*
.....Draw inscribed circle
*/
	cut_array[n++] = 4;
	cut_array[n++] = hcen[0];
	cut_array[n++] = hcen[1];
	cut_array[n++] = dia;
	cut_array[n++] = 0;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1];
/*
.....Set solid line
*/
	cut_array[n++] = 8;
	cut_array[n++] = 0;
/*
.....Draw bottom
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia * htan;
	cut_array[n++] = hcen[1] + dia;
	cut_array[n++] = 2;
	cut_array[n++] = cen1[0];
	cut_array[n++] = cen1[1] + rad;
/*
.....Draw radius
*/
	cut_array[n++] = 4;
	cut_array[n++] = cen1[0];
	cut_array[n++] = cen1[1];
	cut_array[n++] = rad;
	cut_array[n++] = 0;
	cut_array[n++] = cen1[0] - dia * cosa;
	cut_array[n++] = cen1[1] - dia * sina;
/*
......Draw left side
*/
	cut_array[n++] = 1;
	cut_array[n++] = cen1[0] - rad * cosa;
	cut_array[n++] = cen1[1] - rad * sina;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] - dia * htan;
	cut_array[n++] = hcen[1] - dia;
/*
......Draw top
*/
	cut_array[n++] = 2;
	cut_array[n++] = cen2[0];
	cut_array[n++] = cen2[1] - rad;
/*
.....Draw radius
*/
	cut_array[n++] = 4;
	cut_array[n++] = cen2[0];
	cut_array[n++] = cen2[1];
	cut_array[n++] = rad;
	cut_array[n++] = 0;
	cut_array[n++] = cen2[0] + rad * cosa;
	cut_array[n++] = cen2[1] + rad * sina;
/*
......Draw right side
*/
	cut_array[n++] = 1;
	cut_array[n++] = cen2[0] + rad * cosa;
	cut_array[n++] = cen2[1] + rad * sina;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] + dia * htan;
	cut_array[n++] = hcen[1] + dia;
/*
.....Draw height picture
*/
	cut_array[n++] = 1;
	cut_array[n++] = cen1[0] - rad;
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 2;
	cut_array[n++] = cen2[0] + rad;
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 2;
	cut_array[n++] = cen2[0] + rad;
	cut_array[n++] = hcen[1] + dia + hofs + hgt;

	cut_array[n++] = 2;
	cut_array[n++] = cen1[0] - rad;
	cut_array[n++] = hcen[1] + dia + hofs + hgt;

	cut_array[n++] = 2;
	cut_array[n++] = cen1[0] - rad;
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 8;
	cut_array[n++] = 2;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] + dia + hofs + hgt;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] + dia + hofs + hgt;

	cut_array[n++] = 1;
	cut_array[n++] = cen1[0];
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 2;
	cut_array[n++] = cen1[0];
	cut_array[n++] = hcen[1] + dia + hofs + hgt;

	cut_array[n++] = 1;
	cut_array[n++] = cen2[0];
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 2;
	cut_array[n++] = cen2[0];
	cut_array[n++] = hcen[1] + dia + hofs + hgt;

	cut_array[n++] = 8;
	cut_array[n++] = 0;
/*
......draw text string
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	
	cut_array[n++] = 1;
	cut_array[n++] = cen1[0] - rad - 30;
	cut_array[n++] = cen1[1] + rad - 5;

	strcpy(str, "rad");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....Draw lines with arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = cen1[0] - rad - 20;
	cut_array[n++] = cen1[1] + rad + 5;
	cut_array[n++] = 2;
	cut_array[n++] = cen1[0] - rad - 5;
	cut_array[n++] = cen1[1] + rad + 5;
	cut_array[n++] = 3;
	cut_array[n++] = cen1[0] - rad + 2;
	cut_array[n++] = cen1[1] + rad - 1;
/*
......draw text string
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - 6;
	cut_array[n++] = hcen[1] + dia + 5;

	strcpy(str, "dia");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....Draw side lines
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] + dia + 2;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] + dia + hofs - 2;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] + dia + 2;
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] + dia + hofs - 2;

	cut_array[n++] = 8;
	cut_array[n++] = 0;
/*
.....Draw lines with arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - 10;
	cut_array[n++] = hcen[1] + dia + 10;
	cut_array[n++] = 3;
	cut_array[n++] = hcen[0] - dia;
	cut_array[n++] = hcen[1] + dia + 10;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + 10;
	cut_array[n++] = hcen[1] + dia + 10;
	cut_array[n++] = 3;
	cut_array[n++] = hcen[0] + dia;
	cut_array[n++] = hcen[1] + dia + 10;
/*
......draw text string
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	
	cut_array[n++] = 1;
	cut_array[n++] = cen2[0] + rad + 15;
	cut_array[n++] = hcen[1] + dia + hofs + hgt/2 - 5;

	strcpy(str, "hgt");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....Draw side lines
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;

	cut_array[n++] = 1;
	cut_array[n++] = cen2[0] + rad + 2;
	cut_array[n++] = hcen[1] + dia + hofs;
	cut_array[n++] = 2;
	cut_array[n++] = cen2[0] + rad + 15;
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 1;
	cut_array[n++] = cen2[0] + rad + 2;
	cut_array[n++] = hcen[1] + dia + hofs + hgt;
	cut_array[n++] = 2;
	cut_array[n++] = cen2[0] + rad + 15;
	cut_array[n++] = hcen[1] + dia + hofs + hgt;

	cut_array[n++] = 8;
	cut_array[n++] = 0;
/*
.....Draw lines with arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = cen2[0] + rad + 10;
	cut_array[n++] = hcen[1] + dia + hofs + hgt/2;
	cut_array[n++] = 3;
	cut_array[n++] = cen2[0] + rad + 10;
	cut_array[n++] = hcen[1] + dia + hofs;

	cut_array[n++] = 1;
	cut_array[n++] = cen2[0] + rad + 10;
	cut_array[n++] = hcen[1] + dia + hofs + hgt/2;
	cut_array[n++] = 3;
	cut_array[n++] = cen2[0] + rad + 10;
	cut_array[n++] = hcen[1] + dia + hofs + hgt;
/*
......draw text string
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia - 28;
	cut_array[n++] = hcen[1] - 18;

	strcpy(str,"ang");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....Draw angle line
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia * htan;
	cut_array[n++] = hcen[1] - dia;
	cut_array[n++] = 2;
	cut_array[n++] = cen1[0] - rad;
	cut_array[n++] = hcen[1] - dia;

	cut_array[n++] = 8;
	cut_array[n++] = 0;
/*
.....Draw lines with arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia - 20;
	cut_array[n++] = hcen[1] - 16;

	cut_array[n++] = 3;
	cut_array[n++] = hcen[0] - dia - 10;
	cut_array[n++] = hcen[1] - dia;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] - dia - 20;
	cut_array[n++] = hcen[1] - 7;

	cut_array[n++] = 3;
	cut_array[n++] = hcen[0] - dia - 10;
	cut_array[n++] = hcen[1];
/*
......draw text string
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia + 12 + 020;
	cut_array[n++] = hcen[1] - dia + 20;

	strcpy(str,"mntang");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....Draw arc with arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia - 20 + 020;
	cut_array[n++] = hcen[1] - dia - 20;

	cut_array[n++] = 4;
	cut_array[n++] = hcen[0] + 5 + 020;
	cut_array[n++] = hcen[1];
	cut_array[n++] = dia + 20;
	cut_array[n++] = 0;
	cut_array[n++] = hcen[0] + dia + 20 + 020;
	cut_array[n++] = hcen[1] - dia + 20;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia - 20 + 2 + 020;
	cut_array[n++] = hcen[1] - dia - 20 + 3 - 03;

	cut_array[n++] = 3;
	cut_array[n++] = hcen[0] + dia - 20 -1 + 020;
	cut_array[n++] = hcen[1] - dia - 20 + 2 - 03;
/*
.....End of routine
*/
	cut_array[n++] = 99;
}
