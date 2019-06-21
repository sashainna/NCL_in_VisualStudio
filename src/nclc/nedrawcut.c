/*********************************************************************
**    NAME         :  nedrawcut.c
**       CONTAINS:
**          ncl_create_cutarray
**          ncl_free_cutarray
**          ncl_drawform_facemill
**          ncl_drawform_endmill
**          ncl_drawform_barrel
**          ncl_drawform_cone
**          ncl_drawform_bell
**          ncl_drawform_drill
**    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nedrawcut.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:31
*********************************************************************/

#include "usysdef.h"
#include <math.h>

void ncl_drawform_facemill();
void ncl_drawform_endmill();
void ncl_drawform_barrel();
void ncl_drawform_cone();
void ncl_drawform_bell();
void ncl_drawform_drill();

/*********************************************************************
**    E_FUNCTION     : ncl_create_cutarray(cut_array)
**
**		This function sets the display for the following cutter types.
**    There are currently 15 supported cutter types.  The output array
**    must be of a dimension 'cut_array[15][300]'.
**
**			Face Mill, End Mill, Barrel, Cone, Bell, Drill, Boring Tool,
**			Reamer, Chamfer Tool, Blade, Lathe Square, Lathe Diamond,
**			Lathe Triangle, Lathe Circular, Lathe Grooving
**
**		   The display is based on 200 x 200 pixels.
**
**    PARAMETERS   
**       INPUT  : 
**           none
**       OUTPUT : 
**           cut_array   -  Array to hold the cutter drawing parameters.
**    RETURNS      : none
**    SIDE EFFECTS :
**		    Allocates memory to store the the cutter array for text strings.
**        You should call 'ncl_free_cutarray' when the cutter drawings are
**        no longer needed, to free this memory.
**    WARNINGS     : none
*********************************************************************/
void ncl_create_cutarray(cut_array)
int cut_array[13][300];
{
/*
.....Create drawings of the various tools
.....for display in forms
*/
	ncl_drawform_facemill(cut_array[0]);
	ncl_drawform_endmill(cut_array[1]);
	ncl_drawform_barrel(cut_array[2]);
	ncl_drawform_cone(cut_array[3]);
	ncl_drawform_bell(cut_array[4]);
	ncl_drawform_drill(cut_array[5]);
	ncl_drawform_bore(cut_array[6]);
	ncl_drawform_reamer(cut_array[7]);
	ncl_drawform_chamfer(cut_array[8]);
	ncl_drawform_blade(cut_array[9]);
	ncl_drawform_square(cut_array[10]);
	ncl_drawform_diamond(cut_array[11]);
	ncl_drawform_triangle(cut_array[12]);
	ncl_drawform_circular(cut_array[13]);
	ncl_drawform_groove(cut_array[14]);
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION     : ncl_free_cutarray(cut_array)
**
**		This function frees all memory allocated for drawing cutters in
**    forms (ncl_create_cutarray).
**
**    PARAMETERS   
**       INPUT  : 
**           cut_array   -  Array to hold the cutter drawing parameters.
**       OUTPUT : 
**           none
**    RETURNS      : none
**    SIDE EFFECTS :
**		    Frees allocated memory.
**    WARNINGS     : none
*********************************************************************/
void ncl_free_cutarray(cut_array)
int cut_array[13][300];
{
	int i,j;
	UU_LOGICAL done;

	for (i=0; i<13;i++)
	{
		done = UU_FALSE;
		for (j=0;j<300;j++)
		{
			switch (cut_array[i][j]) 
			{
			case 1:
			case 2:
			case 3:
				j = j + 2;
				break;
			case 4:
				j = j + 6;
				break;
			case 5:
				if (cut_array[i][j+2] != 0) free(cut_array[i][j+2]);
				cut_array[i][j+2] = 0;
				j = j + 2;
			case 6:
			case 10:
				j = j + 3;
				break;
			case 99:
				done = UU_TRUE;
			case 8:
			default:
				break;
			}
			if (done) break;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_facemill(cut_array)
**
**		This function draws a Face Mill for a form picture.
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
void ncl_drawform_facemill(cut_array)
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
......draw text string "Face Mill"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 9;
	cut_array[n] = (int)malloc(10*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Face Mill");
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
	cut_array[n++] = 19;
	cut_array[n] = (int)malloc(20*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Cutter/dia, cr, hgt");
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 75;
	cut_array[n++] = 125;
/*
......draw text string "1.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[n++]), "dia");
/*
.....move to (55,140)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 25;
	cut_array[n++] = 142;
/*
......draw text string "CR"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 2;
	cut_array[n] = (int)malloc(3*sizeof(char));
	strcpy((char*)(cut_array[n++]), "cr");
/*
.....move to (160, 120)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 140;
	cut_array[n++] = 100;
/*
......draw text string "2.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[n++]), "hgt");
/*
......draw text string "Shank"
*/
	cut_array[n++] = 1;
	cut_array[n++] = 35;
	cut_array[n++] = 78;
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
.....move to 
*/
	cut_array[n++] = 1;
	cut_array[n++] = 40;
	cut_array[n++] = 145;
/*
.....Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 40;
	cut_array[n++] = 120;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 72;
	cut_array[n++] = 120;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 72;
	cut_array[n++] = 40;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 90;
	cut_array[n++] = 40;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 90;
	cut_array[n++] = 120;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 120;
	cut_array[n++] = 120;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 120;
	cut_array[n++] = 145;
/*
.....draw circle 
*/
	cut_array[n++] = 4;
	cut_array[n++] = 115;
	cut_array[n++] = 145;
	cut_array[n++] = 5;
	cut_array[n++] = 0;
	cut_array[n++] = 115;
	cut_array[n++] = 150; 
/*
.....move to 
*/
	cut_array[n++] = 1;
	cut_array[n++] = 115;
	cut_array[n++] = 150;
/*
.....Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 45;
	cut_array[n++] = 150;
/*
.....draw circle 
*/
	cut_array[n++] = 4;
	cut_array[n++] = 45;
	cut_array[n++] = 145;
	cut_array[n++] = 5;
	cut_array[n++] = 0;
	cut_array[n++] = 40;
	cut_array[n++] = 145; 
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 70;
	cut_array[n++] = 130;
	cut_array[n++] = 3;
	cut_array[n++] = 40;
	cut_array[n++] = 130;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 90;
	cut_array[n++] = 130;
	cut_array[n++] = 3;
	cut_array[n++] = 120;
	cut_array[n++] = 130;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 145;
	cut_array[n++] = 150;
	cut_array[n++] = 3;
	cut_array[n++] = 145;
	cut_array[n++] = 120;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 145;
	cut_array[n++] = 120;
	cut_array[n++] = 3;
	cut_array[n++] = 145;
	cut_array[n++] = 150;
/*
.....Move To and draw a line
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 155;
/*
.....Line To and draw an arrow
*/
	cut_array[n++] = 2;
	cut_array[n++] = 35;
	cut_array[n++] = 155;
	cut_array[n++] = 3;
	cut_array[n++] = 42;
	cut_array[n++] = 149;
/*
.....Draw Shank lines and arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = 50;
	cut_array[n++] = 75;
	cut_array[n++] = 3;
	cut_array[n++] = 50;
	cut_array[n++] = 40;

	cut_array[n++] = 1;
	cut_array[n++] = 50;
	cut_array[n++] = 90;
	cut_array[n++] = 3;
	cut_array[n++] = 50;
	cut_array[n++] = 120;
/*
.....Set dash line
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;
/*
.....Draw Shank reference line
*/
	cut_array[n++] = 1;
	cut_array[n++] = 40;
	cut_array[n++] = 40;
	cut_array[n++] = 2;
	cut_array[n++] = 72;
	cut_array[n++] = 40;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 125;
	cut_array[n++] = 120;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 120;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 125;
	cut_array[n++] = 150;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 150;
	cut_array[n++] = 99;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_endmill(cut_array)
**
**		This function draws a End Mill for a form picture.
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
void ncl_drawform_endmill(cut_array)
int cut_array[];
{
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
......draw text string "End Mill"
*/
	cut_array[7] = 5;
	cut_array[8] = 8;
	cut_array[9] = (int)malloc(9*sizeof(char));
	strcpy((char*)(cut_array[9]), "End Mill");
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
	cut_array[14] = 17;
	cut_array[15] = (int)malloc(18*sizeof(char));
	strcpy((char*)(cut_array[15]), "Cutter/dia,cr,hgt");
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
.....move to (55,140)
*/
	cut_array[22] = 1;
	cut_array[23] = 65;
	cut_array[24] = 133;
/*
......draw text string ".25R"
*/
	cut_array[25] = 5;
	cut_array[26] = 2;
	cut_array[27] = (int)malloc(3*sizeof(char));
	strcpy((char*)(cut_array[27]), "cr");
/*
.....move to (160, 120)
*/
	cut_array[28] = 1;
	cut_array[29] = 140;
	cut_array[30] = 120;
/*
......draw text string "2.0"
*/
	cut_array[31] = 5;
	cut_array[32] = 3;
	cut_array[33] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[33]), "hgt");
/*
.....change pen color to (0,0,0) 
*/
	cut_array[34] = 10;
	cut_array[35] = 0;
	cut_array[36] = 0;
	cut_array[37] = 0;
/*
.....move to (40, 40)
*/
	cut_array[38] = 1;
	cut_array[39] = 40;
	cut_array[40] = 40;
/*
.....Line to (100,40)
*/
	cut_array[41] = 2;
	cut_array[42] = 100;
	cut_array[43] = 40;
/*
......Line to (100,145)
*/
	cut_array[44] = 2;
	cut_array[45] = 100;
	cut_array[46] = 145;
/*
.....draw circle 
*/
	cut_array[47] = 4;
	cut_array[48] = 85;
	cut_array[49] = 145;
	cut_array[50] = 15;
	cut_array[51] = 0;
	cut_array[52] = 85;
	cut_array[53] = 160; 
/*
.....move to (40, 40)
*/
	cut_array[54] = 1;
	cut_array[55] = 85;
	cut_array[56] = 160;
/*
.....Line to (55, 160)
*/
	cut_array[57] = 2;
	cut_array[58] = 55;
	cut_array[59] = 160;
/*
.....draw circle 
*/
	cut_array[60] = 4;
	cut_array[61] = 55;
	cut_array[62] = 145;
	cut_array[63] = 15;
	cut_array[64] = 0;
	cut_array[65] = 40;
	cut_array[66] = 145; 
/*
.....Move To
*/
	cut_array[67] = 1;
	cut_array[68] = 40;
	cut_array[69] = 145;
/*
.....Line to (40, 40)
*/
	cut_array[70] = 2;
	cut_array[71] = 40;
	cut_array[72] = 40;
/*
.....Move To and draw an arrow
*/
	cut_array[73] = 1;
	cut_array[74] = 10;
	cut_array[75] = 100;
	cut_array[76] = 3;
	cut_array[77] = 40;
	cut_array[78] = 100;
/*
.....Move To and draw an arrow
*/
	cut_array[79] = 1;
	cut_array[80] = 130;
	cut_array[81] = 100;
	cut_array[82] = 3;
	cut_array[83] = 100;
	cut_array[84] = 100;
/*
.....Move To and draw an arrow
*/
	cut_array[85] = 1;
	cut_array[86] = 145;
	cut_array[87] = 120;
	cut_array[88] = 3;
	cut_array[89] = 145;
	cut_array[90] = 40;

/*
.....Move To and draw an arrow
*/
	cut_array[91] = 1;
	cut_array[92] = 145;
	cut_array[93] = 135;
	cut_array[94] = 3;
	cut_array[95] = 145;
	cut_array[96] = 160;
/*
.....Move To and draw a line
*/
	cut_array[97] = 1;
	cut_array[98] = 70;
	cut_array[99] = 145;
/*
.....Line To and draw an arrow
*/
	cut_array[100] = 2;
	cut_array[101] = 85;
	cut_array[102] = 145;
	cut_array[103] = 3;
	cut_array[104] = 95;
	cut_array[105] = 153;
/*
.....Set dash line
*/
	cut_array[106] = 8;
	cut_array[107] = 2;
/*
.....Move to and Line to
*/
	cut_array[108] = 1;
	cut_array[109] = 110;
	cut_array[110] = 40;
	cut_array[111] = 2;
	cut_array[112] = 170;
	cut_array[113] = 40;
/*
.....Move to and Line to
*/
	cut_array[114] = 1;
	cut_array[115] = 110;
	cut_array[116] = 160;
	cut_array[117] = 2;
	cut_array[118] = 170;
	cut_array[119] = 160;
	cut_array[120] = 99;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_barrel(cut_array)
**
**		This function draws a Barrel Cutter for a form picture.
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
void ncl_drawform_barrel(cut_array)
int cut_array[];
{
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
......draw text string "Barrel"
*/
	cut_array[7] = 5;
	cut_array[8] = 6;
	cut_array[9] = (int)malloc(7*sizeof(char));
	strcpy((char*)(cut_array[9]), "Barrel");
/*
.....move to (20,180)
*/
	cut_array[10] = 1;
	cut_array[11] = 20;
	cut_array[12] = 170;
/*
......draw text string "Cutter/1.0,.25,2.0,2.0"
*/
	cut_array[13] = 5;
	cut_array[14] = 35;
	cut_array[15] = (int)malloc(36*sizeof(char));
	strcpy((char*)(cut_array[15]), "Cutter/dia, cr, hgt, sr, Z-hgt, ang");
/*
.....move to
*/
	cut_array[16] = 1;
	cut_array[17] = 95;
	cut_array[18] = 105;
/*
......draw text string "1.0"
*/
	cut_array[19] = 5;
	cut_array[20] = 3;
	cut_array[21] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[21]), "dia");
/*
.....move to (5,160)
*/
	cut_array[22] = 1;
	cut_array[23] = 85;
	cut_array[24] = 145;
/*
......draw text string "CR"
*/
	cut_array[25] = 5;
	cut_array[26] = 2;
	cut_array[27] = (int)malloc(3*sizeof(char));
	strcpy((char*)(cut_array[27]), "cr");
/*
.....move to 
*/
	cut_array[28] = 1;
	cut_array[29] = 160;
	cut_array[30] = 120;
/*
......draw text string "2.0"
*/
	cut_array[31] = 5;
	cut_array[32] = 3;
	cut_array[33] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[33]), "hgt");
/*
.....move to 
*/
	cut_array[34] = 1;
	cut_array[35] = 35;
	cut_array[36] = 55;
/*
......draw text string "ANG"
*/
	cut_array[37] = 5;
	cut_array[38] = 5;
	cut_array[39] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[39]), "angle");
/*
.....move to 
*/
	cut_array[40] = 1;
	cut_array[41] = 8;
	cut_array[42] = 130;
/*
......draw text string "0.8"
*/
	cut_array[43] = 5;
	cut_array[44] = 5;
	cut_array[45] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[45]), "Z-hgt");
/*
.....move to 
*/
	cut_array[46] = 1;
	cut_array[47] = 80;
	cut_array[48] = 120;
/*
......draw text string "2.0R"
*/
	cut_array[49] = 5;
	cut_array[50] = 2;
	cut_array[51] = (int)malloc(3*sizeof(char));
	strcpy((char*)(cut_array[51]), "sr");

/*
.....change pen color to (0,0,0) 
*/
	cut_array[52] = 10;
	cut_array[53] = 0;
	cut_array[54] = 0;
	cut_array[55] = 0;
/*
.....move to 
*/
	cut_array[56] = 1;
	cut_array[57] = 85;
	cut_array[58] = 40;
/*
.....Line to
*/
	cut_array[59] = 2;
	cut_array[60] = 115;
	cut_array[61] = 40;
/*
.....move to 
*/
	cut_array[62] = 1;
	cut_array[63] = 130;
	cut_array[64] = 145;
/*
.....draw arc 
*/
	cut_array[65] = 4;
	cut_array[66] = 16;
	cut_array[67] = 112;
	cut_array[68] = 120;
	cut_array[69] = 1;
	cut_array[70] = 135;
	cut_array[71] = 95;
/*
.....move to 
*/
	cut_array[72] = 1;
	cut_array[73] = 130;
	cut_array[74] = 145;
/*
.....draw circle 
*/
	cut_array[75] = 4;
	cut_array[76] = 115;
	cut_array[77] = 145;
	cut_array[78] = 15;
	cut_array[79] = 0;
	cut_array[80] = 115;
	cut_array[81] = 160; 
/*
.....move to
*/
	cut_array[82] = 1;
	cut_array[83] = 115;
	cut_array[84] = 160;
/*
.....Line to 
*/
	cut_array[85] = 2;
	cut_array[86] = 85;
	cut_array[87] = 160;
/*
.....draw circle 
*/
	cut_array[88] = 4;
	cut_array[89] = 85;
	cut_array[90] = 145;
	cut_array[91] = 15;
	cut_array[92] = 0;
	cut_array[93] = 70;
	cut_array[94] = 145; 
/*
.....Move To
*/
	cut_array[95] = 1;
	cut_array[96] = 70;
	cut_array[97] = 145;
/*
.....draw circle 
*/
	cut_array[98] = 4;
	cut_array[99] = 184;
	cut_array[100] = 112;
	cut_array[101] = 120;
	cut_array[102] = 0;
	cut_array[103] = 65;
	cut_array[104] = 95; 
/*
.....Move To and draw an arrow
*/
	cut_array[105] = 1;
	cut_array[106] = 90;
	cut_array[107] = 110;
	cut_array[108] = 3;
	cut_array[109] = 70;
	cut_array[110] = 110;
/*
.....Move To and draw an arrow
*/
	cut_array[111] = 1;
	cut_array[112] = 110;
	cut_array[113] = 110;
	cut_array[114] = 3;
	cut_array[115] = 130;
	cut_array[116] = 110;
/*
.....Move To and draw an arrow
*/
	cut_array[117] = 1;
	cut_array[118] = 165;
	cut_array[119] = 120;
	cut_array[120] = 3;
	cut_array[121] = 165;
	cut_array[122] = 40;

/*
.....Move To and draw an arrow
*/
	cut_array[123] = 1;
	cut_array[124] = 165;
	cut_array[125] = 135;
	cut_array[126] = 3;
	cut_array[127] = 165;
	cut_array[128] = 160;
/*
.....Move To and draw a line
*/
	cut_array[129] = 1;
	cut_array[130] = 100;
	cut_array[131] = 145;
/*
.....Line To and draw an arrow
*/
	cut_array[132] = 2;
	cut_array[133] = 85;
	cut_array[134] = 145;
	cut_array[135] = 3;
	cut_array[136] = 75;
	cut_array[137] = 154;
/*
.....Move To and draw an arrow
*/
	cut_array[138] = 1;
	cut_array[139] = 40;
	cut_array[140] = 70;
	cut_array[141] = 3;
	cut_array[142] = 65;
	cut_array[143] = 70;
/*
.....Move To and draw an arrow
*/
	cut_array[144] = 1;
	cut_array[145] = 90;
	cut_array[146] = 70;
	cut_array[147] = 3;
	cut_array[148] = 75;
	cut_array[149] = 70;
/*
.....Move To and draw an arrow
*/
	cut_array[150] = 1;
	cut_array[151] = 16;
	cut_array[152] = 112;
	cut_array[153] = 3;
	cut_array[154] = 130;
	cut_array[155] = 145;
/*
.....Move To and draw an arrow
*/
	cut_array[156] = 1;
	cut_array[157] = 16;
	cut_array[158] = 130;
	cut_array[159] = 3;
	cut_array[160] = 16;
	cut_array[161] = 112;
/*
.....Move To and draw an arrow
*/
	cut_array[162] = 1;
	cut_array[163] = 16;
	cut_array[164] = 140;
	cut_array[165] = 3;
	cut_array[166] = 16;
	cut_array[167] = 160;

/*
.....Set dash line
*/
	cut_array[168] = 8;
	cut_array[169] = 2;
/*
.....Move to and Line to
*/
	cut_array[170] = 1;
	cut_array[171] = 120;
	cut_array[172] = 40;
	cut_array[173] = 2;
	cut_array[174] = 190;
	cut_array[175] = 40;
/*
.....Move to and Line to
*/
	cut_array[176] = 1;
	cut_array[177] = 140;
	cut_array[178] = 160;
	cut_array[179] = 2;
	cut_array[180] = 190;
	cut_array[181] = 160;
/*
.....Move to and Line to
*/
	cut_array[182] = 1;
	cut_array[183] = 70;
	cut_array[184] = 100;
	cut_array[185] = 2;
	cut_array[186] = 70;
	cut_array[187] = 155;
/*
.....Move to and Line to
*/
	cut_array[188] = 1;
	cut_array[189] = 130;
	cut_array[190] = 100;
	cut_array[191] = 2;
	cut_array[192] = 130;
	cut_array[193] = 155;
/*
.....Move to and Line to
*/
	cut_array[194] = 1;
	cut_array[195] = 10;
	cut_array[196] = 112;
	cut_array[197] = 2;
	cut_array[198] = 40;
	cut_array[199] = 112;
/*
.....Move to and Line to
*/
	cut_array[200] = 1;
	cut_array[201] = 10;
	cut_array[202] = 160;
	cut_array[203] = 2;
	cut_array[204] = 40;
	cut_array[205] = 160;
/*
.....Move to and Line to
*/
	cut_array[206] = 1;
	cut_array[207] = 65;
	cut_array[208] = 40;
	cut_array[209] = 2;
	cut_array[210] = 65;
	cut_array[211] = 100;
/*
.....Set solid line
*/
	cut_array[212] = 8;
	cut_array[213] = 0;
/*
.....Move to and Line to
*/
	cut_array[214] = 1;
	cut_array[215] = 135;
	cut_array[216] = 95;
	cut_array[217] = 2;
	cut_array[218] = 115;
	cut_array[219] = 40;
/*
.....Move to and Line to
*/
	cut_array[220] = 1;
	cut_array[221] = 65;
	cut_array[222] = 95;
	cut_array[223] = 2;
	cut_array[224] = 85;
	cut_array[225] = 40;

	cut_array[226] = 99;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_cone(cut_array)
**
**		This function draws a Cone Cutter for a form picture.
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
void ncl_drawform_cone(cut_array)
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
......draw text string "Cone"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 4;
	cut_array[n] = (int)malloc(5*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Cone");
/*
.....move to (20,180)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 180;
/*
......draw text string "Cutter/1.0,.25,2.0,10"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 23;
	cut_array[n] = (int)malloc(24*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Cutter/dia,cr,hgt,angle");
/*
.....move to (65,95)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 65;
	cut_array[n++] = 115;
/*
......draw text string "1.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[n++]), "dia");
/*
.....move to (5,160)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 148;
/*
......draw text string ".25R"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 2;
	cut_array[n] = (int)malloc(3*sizeof(char));
	strcpy((char*)(cut_array[n++]), "cr");
/*
.....move to (160, 120)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 140;
	cut_array[n++] = 120;
/*
......draw text string "2.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[n++]), "hgt");
/*
......draw text string "Shank"
*/
	cut_array[n++] = 1;
	cut_array[n++] = 135;
	cut_array[n++] = 60;

	cut_array[n++] = 5;
	cut_array[n++] = 5;
	cut_array[n] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Shank");
/*
.....move to (160, 120)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 52;
	cut_array[n++] = 95;
/*
......draw text string "10"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 5;
	cut_array[n] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[n++]), "angle");
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
	cut_array[n++] = 40 - 65*sin(3.14/18);
	cut_array[n++] = 90;
/*
.....Line to (100,40)
*/
	cut_array[n++] = 2;
	cut_array[n++] = 100 + 65*sin(3.14/18);
	cut_array[n++] = 90;
/*
......Line to (100,145)
*/
	cut_array[n++] = 2;
	cut_array[n++] = 100;
	cut_array[n++] = 145;
/*
.....draw circle 
*/
	cut_array[n++] = 4;
	cut_array[n++] = 85;
	cut_array[n++] = 145;
	cut_array[n++] = 15;
	cut_array[n++] = 0;
	cut_array[n++] = 85;
	cut_array[n++] = 160; 
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 85;
	cut_array[n++] = 160;
/*
.....Line to (55, 160)
*/
	cut_array[n++] = 2;
	cut_array[n++] = 55;
	cut_array[n++] = 160;
/*
.....draw circle 
*/
	cut_array[n++] = 4;
	cut_array[n++] = 55;
	cut_array[n++] = 145;
	cut_array[n++] = 15;
	cut_array[n++] = 0;
	cut_array[n++] = 40;
	cut_array[n++] = 145; 
/*
.....Move To
*/
	cut_array[n++] = 1;
	cut_array[n++] = 40;
	cut_array[n++] = 145;
/*
.....Line to
*/
	cut_array[n++] = 2;
	cut_array[n++] = 40 - 65*sin(3.14/18);
	cut_array[n++] = 90;
/*
.....Draw cutter shank
*/
	cut_array[n++] = 2;
	cut_array[n++] = 40 - 65*sin(3.14/18);
	cut_array[n++] = 40;
	cut_array[n++] = 2;
	cut_array[n++] = 100 + 65*sin(3.14/18);
	cut_array[n++] = 40;
	cut_array[n++] = 2;
	cut_array[n++] = 100 + 65*sin(3.14/18);
	cut_array[n++] = 90;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 60;
	cut_array[n++] = 125;
	cut_array[n++] = 3;
	cut_array[n++] = 40;
	cut_array[n++] = 125;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 80;
	cut_array[n++] = 125;
	cut_array[n++] = 3;
	cut_array[n++] = 100;
	cut_array[n++] = 125;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 145;
	cut_array[n++] = 120;
	cut_array[n++] = 3;
	cut_array[n++] = 145;
	cut_array[n++] = 90;

/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 145;
	cut_array[n++] = 135;
	cut_array[n++] = 3;
	cut_array[n++] = 145;
	cut_array[n++] = 160;
/*
.....Draw shank arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = 145;
	cut_array[n++] = 70;
	cut_array[n++] = 3;
	cut_array[n++] = 145;
	cut_array[n++] = 90;

	cut_array[n++] = 1;
	cut_array[n++] = 145;
	cut_array[n++] = 60;
	cut_array[n++] = 3;
	cut_array[n++] = 145;
	cut_array[n++] = 40;
/*
.....Move To and draw a line
*/
	cut_array[n++] = 1;
	cut_array[n++] = 15;
	cut_array[n++] = 160;
/*
.....Line To and draw an arrow
*/
	cut_array[n++] = 2;
	cut_array[n++] = 35;
	cut_array[n++] = 160;
	cut_array[n++] = 3;
	cut_array[n++] = 45;
	cut_array[n++] = 155;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 10;
	cut_array[n++] = 115;
	cut_array[n++] = 3;
	cut_array[n++] = 32;
	cut_array[n++] = 115;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 60;
	cut_array[n++] = 105;
	cut_array[n++] = 3;
	cut_array[n++] = 40;
	cut_array[n++] = 115;
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
	cut_array[n++] = 90;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 90;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 110;
	cut_array[n++] = 160;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 160;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 40;
	cut_array[n++] = 90;
	cut_array[n++] = 2;
	cut_array[n++] = 40;
	cut_array[n++] = 155;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 100;
	cut_array[n++] = 90;
	cut_array[n++] = 2;
	cut_array[n++] = 100;
	cut_array[n++] = 155;

	cut_array[n++] = 99;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_bell(cut_array)
**
**		This function draws a Bell Cutter for a form picture.
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
void ncl_drawform_bell(cut_array)
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
......draw text string "Bell"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 4;
	cut_array[n] = (int)malloc(5*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Bell");
/*
.....move to (20,180)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 170;
/*
......draw text string "Cutter/1.0,.25,2.0,-15"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 23;
	cut_array[n] = (int)malloc(24*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Cutter/dia,cr,hgt,angle");
/*
.....move to (65,95)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 62;
	cut_array[n++] = 120;
/*
......draw text string "1.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[n++]), "dia");
/*
.....move to (5,160)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 148;
/*
......draw text string ".25R"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 2;
	cut_array[n] = (int)malloc(3*sizeof(char));
	strcpy((char*)(cut_array[n++]), "cr");
/*
.....move to (160, 120)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 140;
	cut_array[n++] = 120;
/*
......draw text string "2.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[n++]), "hgt");
/*
.....move to (160, 120)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 10;
	cut_array[n++] = 85;
/*
......draw text string "10"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 5;
	cut_array[n] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[n++]), "angle");
/*
.....Draw shank text
*/
	cut_array[n++] = 1;
	cut_array[n++] = 120;
	cut_array[n++] = 60;

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
	cut_array[n++] = 40 + 65*sin(3.14/12);
	cut_array[n++] = 90;
/*
.....Line to (100,40)
*/
	cut_array[n++] = 2;
	cut_array[n++] = 100 - 65*sin(3.14/12);
	cut_array[n++] = 90;
/*
......Line to (100,145)
*/
	cut_array[n++] = 2;
	cut_array[n++] = 100;
	cut_array[n++] = 145;
/*
.....draw circle 
*/
	cut_array[n++] = 4;
	cut_array[n++] = 85;
	cut_array[n++] = 145;
	cut_array[n++] = 15;
	cut_array[n++] = 0;
	cut_array[n++] = 85;
	cut_array[n++] = 160; 
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 85;
	cut_array[n++] = 160;
/*
.....Line to (55, 160)
*/
	cut_array[n++] = 2;
	cut_array[n++] = 55;
	cut_array[n++] = 160;
/*
.....draw circle 
*/
	cut_array[n++] = 4;
	cut_array[n++] = 55;
	cut_array[n++] = 145;
	cut_array[n++] = 15;
	cut_array[n++] = 0;
	cut_array[n++] = 40;
	cut_array[n++] = 145; 
/*
.....Move To
*/
	cut_array[n++] = 1;
	cut_array[n++] = 40;
	cut_array[n++] = 145;
/*
.....Line to
*/
	cut_array[n++] = 2;
	cut_array[n++] = 40 + 65*sin(3.14/12);
	cut_array[n++] = 90;
/*
.....Draw shank
*/
	cut_array[n++] = 2;
	cut_array[n++] = 40 + 65*sin(3.14/12);
	cut_array[n++] = 40;

	cut_array[n++] = 2;
	cut_array[n++] = 100 - 65*sin(3.14/12);
	cut_array[n++] = 40;

	cut_array[n++] = 2;
	cut_array[n++] = 100 - 65*sin(3.14/12);
	cut_array[n++] = 90;

/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 125;
	cut_array[n++] = 3;
	cut_array[n++] = 40;
	cut_array[n++] = 125;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 120;
	cut_array[n++] = 125;
	cut_array[n++] = 3;
	cut_array[n++] = 100;
	cut_array[n++] = 125;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 145;
	cut_array[n++] = 120;
	cut_array[n++] = 3;
	cut_array[n++] = 145;
	cut_array[n++] = 90;

/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 145;
	cut_array[n++] = 135;
	cut_array[n++] = 3;
	cut_array[n++] = 145;
	cut_array[n++] = 160;
/*
.....Draw Shank arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = 130;
	cut_array[n++] = 70;
	cut_array[n++] = 3;
	cut_array[n++] = 130;
	cut_array[n++] = 90;

	cut_array[n++] = 1;
	cut_array[n++] = 130;
	cut_array[n++] = 60;
	cut_array[n++] = 3;
	cut_array[n++] = 130;
	cut_array[n++] = 40;
/*
.....Move To and draw a line
*/
	cut_array[n++] = 1;
	cut_array[n++] = 15;
	cut_array[n++] = 160;
/*
.....Line To and draw an arrow
*/
	cut_array[n++] = 2;
	cut_array[n++] = 35;
	cut_array[n++] = 160;
	cut_array[n++] = 3;
	cut_array[n++] = 45;
	cut_array[n++] = 155;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 20;
	cut_array[n++] = 100;
	cut_array[n++] = 3;
	cut_array[n++] = 40;
	cut_array[n++] = 95;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 70;
	cut_array[n++] = 105;
	cut_array[n++] = 3;
	cut_array[n++] = 55;
	cut_array[n++] = 95;
/*
.....Set dash line
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 90;
	cut_array[n++] = 40;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 40;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 90;
	cut_array[n++] = 90;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 90;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 105;
	cut_array[n++] = 160;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 160;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 40;
	cut_array[n++] = 95;
	cut_array[n++] = 2;
	cut_array[n++] = 40;
	cut_array[n++] = 155;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 100;
	cut_array[n++] = 95;
	cut_array[n++] = 2;
	cut_array[n++] = 100;
	cut_array[n++] = 155;

	cut_array[n++] = 99;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_drill(cut_array)
**
**		This function draws a Drill for a form picture.
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
void ncl_drawform_drill(cut_array)
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
......draw text string "Drill"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 5;
	cut_array[n] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Drill");
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
	cut_array[n++] = 20;
	cut_array[n] = (int)malloc(21*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Cutter/0,0,hgt,angle");
/*
.....move to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 140;
	cut_array[n++] = 75;
/*
......draw text string 
*/
	cut_array[n++] = 5;
	cut_array[n++] = 5;
	cut_array[n] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[n++]), "Shank");
/*
.....move to (160, 120)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 145;
	cut_array[n++] = 135;
/*
......draw text string "2.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 3;
	cut_array[n] = (int)malloc(4*sizeof(char));
	strcpy((char*)(cut_array[n++]), "hgt");
/*
.....move to (160, 120)
*/
	cut_array[n++] = 1;
	cut_array[n++] = 25;
	cut_array[n++] = 145;
/*
......draw text string "2.0"
*/
	cut_array[n++] = 5;
	cut_array[n++] = 5;
	cut_array[n] = (int)malloc(6*sizeof(char));
	strcpy((char*)(cut_array[n++]), "angle");
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
	cut_array[n++] = 120;
	cut_array[n++] = 40;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 120;
	cut_array[n++] = 120;
/*
......Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 80;
	cut_array[n++] = 160;
/*
.....Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 40;
	cut_array[n++] = 120;
/*
.....Line to 
*/
	cut_array[n++] = 2;
	cut_array[n++] = 40;
	cut_array[n++] = 40;
/*
.....Draw cutter height
*/
	cut_array[n++] = 1;
	cut_array[n++] = 40;
	cut_array[n++] = 120;

	cut_array[n++] = 2;
	cut_array[n++] = 120;
	cut_array[n++] = 120;
/*
.....Draw angle arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = 30;
	cut_array[n++] = 155;
	cut_array[n++] = 2;
	cut_array[n++] = 45;
	cut_array[n++] = 155;
	cut_array[n++] = 3;
	cut_array[n++] = 60;
	cut_array[n++] = 140;

	cut_array[n++] = 1;
	cut_array[n++] = 95;
	cut_array[n++] = 135;
	cut_array[n++] = 3;
	cut_array[n++] = 80;
	cut_array[n++] = 135;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 150;
	cut_array[n++] = 135;
	cut_array[n++] = 3;
	cut_array[n++] = 150;
	cut_array[n++] = 120;
/*
.....Move To and draw an arrow
*/
	cut_array[n++] = 1;
	cut_array[n++] = 150;
	cut_array[n++] = 145;
	cut_array[n++] = 3;
	cut_array[n++] = 150;
	cut_array[n++] = 160;
/*
.....Draw Shank arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = 150;
	cut_array[n++] = 75;
	cut_array[n++] = 3;
	cut_array[n++] = 150;
	cut_array[n++] = 40;

	cut_array[n++] = 1;
	cut_array[n++] = 150;
	cut_array[n++] = 90;
	cut_array[n++] = 3;
	cut_array[n++] = 150;
	cut_array[n++] = 120;
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
	cut_array[n++] = 120;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 120;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 110;
	cut_array[n++] = 160;
	cut_array[n++] = 2;
	cut_array[n++] = 170;
	cut_array[n++] = 160;
/*
.....Move to and Line to
*/
	cut_array[n++] = 1;
	cut_array[n++] = 80;
	cut_array[n++] = 160;
	cut_array[n++] = 2;
	cut_array[n++] = 80;
	cut_array[n++] = 110;

	cut_array[n++] = 99;
}
