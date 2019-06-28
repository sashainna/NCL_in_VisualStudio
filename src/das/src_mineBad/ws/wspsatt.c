
/*********************************************************************
**    NAME         :  wspsatt.c
**    CONTAINS:
**       names of functions in file
**			uw_pscolormap
**			uw_pslintype
**			uw_pspen
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       wspsatt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:14
*********************************************************************/

#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "ginqatt.h"
#include "wsps.h"
#include "xenv1.h"
#include <string.h>
static option = 0;      /* option for color, greyscale or blackwhite */
extern Gsps uw_ps;		/* declare workstation local data */

static Lwidth = -1;
static Ltype = -1;
/*********************************************************************
**    I_FUNCTION     :  uw_pslintype(n)
**       Set current line type.
**		
**    PARAMETERS   
**       INPUT  : 
**				n: line type
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_pslintype(n)
int n;
{	int element1, element2, element3, element4; 
	char num[40];

	if ((n<1)||(n>8)) n=0;	
	if (n==-1)
	{
		Ltype = -1;
		return;
	}
	if (n != Ltype)
	{
		Ltype = n;
		switch (n)
		{
/*
.....Solid Line
*/
		case 1:
			utp_ttputps(uw_ps.ttfd,"[] 0 ", 5);
			break;
/*
.....Short Dash Line
*/
		case 2:
			element1 = (int)(4/psrate_x + 0.5);
			sprintf(num, "[%d] 0 ", element1);
			utp_ttputps(uw_ps.ttfd,num,strlen(num));
			break;
/*
.....Dotted Line. original set [2 3] in graphics file
*/
		case 3:
         element1 = (int)(1/psrate_x + 0.5);
         element2 = (int)(2/psrate_x + 0.5);
         sprintf(num, "[%d %d] 0 ", element1, element2);
         utp_ttputps(uw_ps.ttfd,num,strlen(num));
			break;
/*
.....Dot Dash Line
*/
		case 4:
         element1 = (int)(2/psrate_x + 0.5);
         element2 = (int)(3/psrate_x + 0.5);
         element3 = (int)(8/psrate_x + 0.5);
         element4 = (int)(3/psrate_x + 0.5);
         sprintf(num, "[%d %d %d %d] 0 ", element1, element2, element3,element4);
         utp_ttputps(uw_ps.ttfd,num,strlen(num));
			break;
/*
.....Phantom Line
*/
		case 5:
         element1 = (int)(4/psrate_x + 0.5);
         element2 = (int)(3/psrate_x + 0.5);
         element3 = (int)(2/psrate_x + 0.5);
         element4 = (int)(3/psrate_x + 0.5);
         sprintf(num, "[%d %d %d %d %d %d] 0 ", element1, element2, 
						element3,element4, element3,element4);
         utp_ttputps(uw_ps.ttfd,num,strlen(num));
			break;
/*
.....Dashed Line
*/
		case 6:
         element1 = (int)(8/psrate_x + 0.5);
         element2 = (int)(8/psrate_x + 0.5);
         sprintf(num, "[%d %d] 0 ", element1, element2);
         utp_ttputps(uw_ps.ttfd,num,strlen(num));
			break;
/*
.....Center Line
*/
		case 7:	
         element1 = (int)(8/psrate_x + 0.5);
         element2 = (int)(3/psrate_x + 0.5);
         element3 = (int)(4/psrate_x + 0.5);
         element4 = (int)(3/psrate_x + 0.5);
         sprintf(num, "[%d %d %d %d] 0 ", element1, element2, element3,element4);
         utp_ttputps(uw_ps.ttfd,num,strlen(num));
			break;
/*
.....Space Dash Line
*/
		case 8:
         element1 = (int)(4/psrate_x + 0.5);
         element2 = (int)(8/psrate_x + 0.5);
         sprintf(num, "[%d %d] 0 ", element1, element2);
         utp_ttputps(uw_ps.ttfd,num,strlen(num));
			break;
		default:
		   utp_ttputps(uw_ps.ttfd,"[] 0 ", 5);
         break;

		}
		utp_ttputps(uw_ps.ttfd,"setdash\n ",8); 
	}
}	/* uw_pslintype */


/**********************************************************************
**    I_FUNCTION :  uw_pscolormap()
**       Allocates the colormap for PS.
**    PARAMETERS
**       INPUT  :
**          input
**       OUTPUT :
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_pscolormap()
{
#define WHITE ",\n \t\r"
	int i;
	char  *p, *ux_getenv();
	char  *str,buf[80];
	double atof();
	static int color_table[16][3] = 
	{  255,  255, 255,  /*   White  */
		0,    0,   0,    /*   Black  */
		30,   144, 255,  /*   DodgerBlue  */
		255,  0,   0,    /* Red     */
		0,    255, 0,    /* Green   */
		255,  0,   255,  /* Magenta */
		255,  255, 0,    /* Yellow  */
		0,    255, 255,  /* Cyan    */
		184,  134, 11,   /* DarkGoldenrod */
		210,  180, 140,  /*  Tan    */
		173,  216, 230,  /* LightBlue     */
		84,   255, 159,  /* SeaGreen1     */
		255,  165, 0,    /* Orange  */
		255,  195, 203,  /* Pink    */
		221,  160, 221,  /* Plum    */
		192,  192, 192,  /* Gray    */
	} 	; 

	p = ux_getenv("UU_PSCOLOR",UX_PRTERRS);
	if (p==NULL)
/*
.....default Color black white
*/
	{  
		option = 0;
	}    

	else
/*
.....get color option
*/
	{  
		if (strcmp(p,"BLACKWHITE") == 0 )
		{
			option = 0;
		}
		else if (strcmp(p,"GREYSCALE")==0)
		{  
			option = 1;
		}
		else if (strcmp(p,"COLOR")==0)
		{
			option = 2;
		}
	}

	for (i=0;i<UU_PSMAXPEN;i++)
	{
		sprintf(buf,"UU_PEN%d",i);
		p = ux_getenv(buf,UX_PRTERRS);
		if (p!=NULL)
		{
			str = strtok(p, WHITE);
			UW_pscolor[i][0] = (atof(str))/255;
			str = strtok(NULL, WHITE);
			UW_pscolor[i][1] = (atof(str))/255;
			str = strtok(NULL, WHITE);
			UW_pscolor[i][2] = (atof(str))/255;
		}
		else
		{	
			if (i < 16)
			{
				UW_pscolor[i][0] = (double)color_table[i][0]/255;
				UW_pscolor[i][1] = (double)color_table[i][1]/255;
				UW_pscolor[i][2] = (double)color_table[i][2]/255;
			}
			else
			{
				UW_pscolor[i][0] = 0;
				UW_pscolor[i][1] = 0;
				UW_pscolor[i][2] = 0;
			}
		}
	}
}



/*********************************************************************
**    E_FUNCTION :  uw_pspen(pen)
**      Selects a new pen if the requested pen is different than the
**      currently loaded pen.
**    PARAMETERS   
**       INPUT  : 
**          pen  -  Pen number to load.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_pspen(pen)
int pen;
{
	static int last_pen;
	char num[40];
	double pspen;
	static double Lpspen = -1;
	char  *ux_getenv();
	if (pen == -1)
	{
		last_pen = pen;
		Lpspen = -1;
		return;
	}
	else if (pen != last_pen)
	{  
		last_pen = pen;
	}
	else return;
	if (option == 0)
	{  
		if (pen==0) pen = 11;
		else pen = 1;
	}        
	else if (option==1)
	{	
		if ((pen<0)||(pen>11)) pen = 1;
		if (pen==0) pen = 11;
	}
	switch(option)
	{	case 0:
		case 1:		
			pspen =( (double)pen - 1.0)/10.0; 
			if (pspen==Lpspen) break;
			Lpspen = pspen;
			sprintf(num, "%f", pspen);
			utp_ttputps(uw_ps.ttfd,num,strlen(num));
			utp_ttputps(uw_ps.ttfd," setgray\n",9);
			break;
		case 2:
			sprintf(num ,"%f %f %f setrgbcolor\n",UW_pscolor[pen][0],
				    UW_pscolor[pen][1],UW_pscolor[pen][2]);
			utp_ttputps(uw_ps.ttfd,num,strlen(num));
			break;
	}
}

/*********************************************************************
**    I_FUNCTION     :  uw_pslinwid(wid)
**       Set current line width.
**    PARAMETERS   
**       INPUT  : 
**          wid: line width
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_pslinwid(wid)
int wid;
{
	char num[80];
	if (wid==-1)
	{
		Lwidth = -1;
		return;
	}
	if ( wid!= Lwidth)
	{
		Lwidth = wid;
		sprintf (num,"%d ",wid);
		utp_ttputps(uw_ps.ttfd,num,strlen(num));
		utp_ttputps(uw_ps.ttfd,"setlinewidth\n",13);
	}
}  /* uq_pslinwid */

