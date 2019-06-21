/*********************************************************************
**    NAME         :  nedrawcut2.c
**       CONTAINS:
**          ncl_drawform_triangle
**          ncl_drawform_circular
**          ncl_drawform_groove
**    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nedrawcut2.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:32
*********************************************************************/

#include "usysdef.h"
#include <math.h>

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_triangle(cut_array)
**
**		This function draws a Lathe Triangle Insert for a form picture.
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
void ncl_drawform_triangle(cut_array)
int cut_array[];
{
	int n,hcen[2],dia,rad,hofs,hgt,temp;
	UU_REAL cen1[2],cen2[2],cen3[2],sina,cosa,htan;
	char str[80];
/*
.....Initialize routine
*/
	hcen[0] = 100;
	hcen[1] = 85;
	dia = 25;
	rad = 5;
	hofs = 25;
	hgt = 15;
/*
.....Calculate center of tool radii
*/
	sina = .866;
	cosa = .5;
	htan = .577;
	temp = dia - rad;
	cen1[0] = hcen[0] - temp/htan;
	cen1[1] = hcen[1] + temp;
	cen2[0] = hcen[0] + temp/htan;
	cen2[1] = hcen[1] + temp;
	cen3[0] = hcen[0];
	cen3[1] = hcen[1] - temp/htan;
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
	strcpy(str, "Triangle Insert");
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
	strcpy(str,"Cutter/Lathe,rad,dia,hgt,60,mntang");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);
/*
.....set text color to (20, 20, 20), red
*/
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
	cut_array[n++] = cen2[0];
	cut_array[n++] = cen2[1] + rad;
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
	cut_array[n++] = cen1[0] - rad * cosa;
	cut_array[n++] = cen1[1] - rad * sina;
/*
......Draw left side
*/
	cut_array[n++] = 1;
	cut_array[n++] = cen1[0] - rad * cosa;
	cut_array[n++] = cen1[1] - rad * sina;
	cut_array[n++] = 2;
	cut_array[n++] = cen3[0] - rad * cosa;
	cut_array[n++] = cen3[1] - rad * sina;
/*
.....Draw radius
*/
	cut_array[n++] = 4;
	cut_array[n++] = cen3[0];
	cut_array[n++] = cen3[1];
	cut_array[n++] = rad;
	cut_array[n++] = 0;
	cut_array[n++] = cen3[0] + rad * cosa;
	cut_array[n++] = cen3[1] - rad * sina;
/*
......Draw right side
*/
	cut_array[n++] = 1;
	cut_array[n++] = cen3[0] + rad * cosa;
	cut_array[n++] = cen3[1] - rad * sina;
	cut_array[n++] = 2;
	cut_array[n++] = cen2[0] + rad * cosa;
	cut_array[n++] = cen2[1] - rad * sina;
/*
.....Draw radius
*/
	cut_array[n++] = 4;
	cut_array[n++] = cen2[0];
	cut_array[n++] = cen2[1];
	cut_array[n++] = rad;
	cut_array[n++] = 0;
	cut_array[n++] = cen2[0];
	cut_array[n++] = cen2[1] + rad;
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
	cut_array[n++] = hcen[0] + dia + 12;
	cut_array[n++] = hcen[1] - dia + 15;

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
	cut_array[n++] = hcen[1] - dia - 15;

	cut_array[n++] = 4;
	cut_array[n++] = hcen[0] + 5;
	cut_array[n++] = hcen[1] - 5;
	cut_array[n++] = dia + 20;
	cut_array[n++] = 0;
	cut_array[n++] = hcen[0] + dia + 20;
	cut_array[n++] = hcen[1] - dia + 15;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia - 20 + 3;
	cut_array[n++] = hcen[1] - dia - 25 + 1;

	cut_array[n++] = 3;
	cut_array[n++] = hcen[0] + dia - 20;
	cut_array[n++] = hcen[1] - dia - 25;
/*
.....End of routine
*/
	cut_array[n++] = 99;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_circular(cut_array)
**
**		This function draws a Lathe Circular Insert for a form picture.
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
void ncl_drawform_circular(cut_array)
int cut_array[];
{
	int n,hcen[2],dia,hofs,hgt;
	char str[80];
/*
.....Initialize routine
*/
	hcen[0] = 95;
	hcen[1] = 80;
	dia = 40;
	hofs = 15;
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
	strcpy(str, "Circular Insert");
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
	strcpy(str,"Cutter/Lathe,rad,0,hgt");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
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
	cut_array[n++] = hcen[0] - dia * .707;
	cut_array[n++] = hcen[1] + dia * .707;
/*
.....Draw line to center of circle
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0];
	cut_array[n++] = hcen[1];
	cut_array[n++] = 2;
	cut_array[n++] = hcen[0] - dia * .707;
	cut_array[n++] = hcen[1] + dia * .707;
/*
......draw text string
*/
	cut_array[n++] = 6;
	cut_array[n++] = 257;
	cut_array[n++] = 257;
	cut_array[n++] = 257;

	cut_array[n++] = 1;
	cut_array[n++] = hcen[0] + dia + 15;
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
.....End of routine
*/
	cut_array[n++] = 99;
}

/*********************************************************************
**    E_FUNCTION     : ncl_drawform_groove(cut_array)
**
**		This function draws a Lathe Grooving Tool for a form picture.
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
void ncl_drawform_groove(cut_array)
int cut_array[];
{
	int n,hcen[2],dia,rad,hofs,hgt,temp,leny;
	UU_REAL cen1[2],cen2[2];
	char str[80];
/*
.....Initialize routine
*/
	hcen[0] = 95;
	hcen[1] = 85;
	dia = 25;
	rad = 5;
	hofs = 25;
	hgt = 15;
	leny = 60;
/*
.....Calculate center of tool radii
*/
	temp = dia - rad;
	cen1[0] = hcen[0] - temp;
	cen1[1] = hcen[1] + temp;
	cen2[0] = hcen[0] + temp;
	cen2[1] = hcen[1] + temp;
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
	strcpy(str, "Grooving Tool");
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
	strcpy(str,"Cutter/Lathe,rad,wid,hgt,0,len");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....Draw bottom
*/
	cut_array[n++] = 1;
	cut_array[n++] = cen2[0];
	cut_array[n++] = cen2[1] + rad;
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
	cut_array[n++] = cen1[0] - rad;
	cut_array[n++] = cen1[1];
/*
......Draw left side
*/
	cut_array[n++] = 1;
	cut_array[n++] = cen1[0] - rad;
	cut_array[n++] = cen1[1];
	cut_array[n++] = 2;
	cut_array[n++] = cen1[0] - rad;
	cut_array[n++] = cen1[1] - leny;
/*
.....Draw top
*/
	cut_array[n++] = 2;
	cut_array[n++] = cen2[0] + rad;
	cut_array[n++] = cen2[1] - leny;
/*
......Draw right side
*/
	cut_array[n++] = 2;
	cut_array[n++] = cen2[0] + rad;
	cut_array[n++] = cen2[1];
/*
.....Draw radius
*/
	cut_array[n++] = 4;
	cut_array[n++] = cen2[0];
	cut_array[n++] = cen2[1];
	cut_array[n++] = rad;
	cut_array[n++] = 0;
	cut_array[n++] = cen2[0];
	cut_array[n++] = cen2[1] + rad;
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

	strcpy(str, "wid");
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
	cut_array[n++] = cen2[0] + rad + 8;
	cut_array[n++] = cen2[1] - leny/2 - 2;

	strcpy(str,"len");
	cut_array[n++] = 5;
	cut_array[n++] = strlen(str);
	cut_array[n] = (int)malloc((cut_array[n-1]+1)*sizeof(char));
	strcpy((char*)cut_array[n++],str);

	cut_array[n++] = 10;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
	cut_array[n++] = 0;
/*
.....Draw bounding lines
*/
	cut_array[n++] = 8;
	cut_array[n++] = 2;

	cut_array[n++] = 1;
	cut_array[n++] = cen2[0] + rad;
	cut_array[n++] = cen2[1] + rad;
	cut_array[n++] = 2;
	cut_array[n++] = cen2[0] + rad + 20;
	cut_array[n++] = cen2[1] + rad;

	cut_array[n++] = 1;
	cut_array[n++] = cen2[0] + rad;
	cut_array[n++] = cen2[1] - leny;
	cut_array[n++] = 2;
	cut_array[n++] = cen2[0] + rad + 20;
	cut_array[n++] = cen2[1] - leny;

	cut_array[n++] = 8;
	cut_array[n++] = 0;
/*
.....Draw lines with arrows
*/
	cut_array[n++] = 1;
	cut_array[n++] = cen2[0] + rad + 12;
	cut_array[n++] = cen2[1] - leny/2 - 2;
	cut_array[n++] = 3;
	cut_array[n++] = cen2[0] + rad + 12;
	cut_array[n++] = cen2[1] - leny;

	cut_array[n++] = 1;
	cut_array[n++] = cen2[0] + rad + 12;
	cut_array[n++] = cen2[1] - leny/2 + 7;
	cut_array[n++] = 3;
	cut_array[n++] = cen2[0] + rad + 12;
	cut_array[n++] = cen2[1] + rad;
/*
.....End of routine
*/
	cut_array[n++] = 99;
}
