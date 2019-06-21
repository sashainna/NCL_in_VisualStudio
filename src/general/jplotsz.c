/**********************************************************************
**    NAME         :  jplotsz.c
**       CONTAINS:
**				uj_setps
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       jplotsz.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**      04/29/15 , 15:05:47
*********************************************************************/
#include "usysdef.h"
#include "gobas.h"
#include "xenv1.h"
#include "mdrwsize.h"
#include<string.h>
#include "tplot.h"
extern int wsplotdx, wsplotdy;

char  *ux_getenv();
extern int uw_758_longplot;
extern int uw_758fd_page;
extern float uw_758_maxxsize;
/*******************************************************************
**    I_FUNCTION :  uj_setps (drwsize,xy)
**       Sets the physical limits of the plotter sheet, dependant on
**       the plotter model and paper/drawing size.
**    PARAMETERS   
**       INPUT  : 
**          drwsize  = Current paper size (AH, B, etc.).
**       OUTPUT :  
**          xy       = Size of paper in X & Y.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uj_setps(drwsize,xy)
int *drwsize;
UU_REAL xy[];
{
	char *ev;
	double atof();
	if (strcmp(plotopts.type,"7475")==0)
		jp_7475size(drwsize,xy);
   if (strcmp(plotopts.type,"PS")==0 || strcmp(plotopts.type,"ps")==0)
		jp_pssize(drwsize,xy);
	if (strcmp(plotopts.type,"7580")==0)
      jp_7580size(drwsize, xy);
   if (strcmp(plotopts.type,"1043")==0)
      jp_1043size(drwsize,xy);
	return;
}

jp_7475size(drwsize, xy)
int *drwsize;
UU_REAL xy[];

{  char *ev;
   double atof();
   /* Set the X & Y offset values and the X & Y limit values based on the
   environmental values set by the run script.  Only set the values for
   the plotter model and drawing size requested.
   */
      ev = ux_getenv("D7475_PLOTDX",UX_PRTERRS);
      if (ev != NULL) 
			sscanf(ev,"%d",&wsplotdx);
      else
         wsplotdx = UM_drawing_size[DS_7475][2];

      ev = ux_getenv("D7475_PLOTDY",UX_PRTERRS);
      if (ev != NULL) 
			sscanf(ev,"%d",&wsplotdy);
      else
         wsplotdy = UM_drawing_size[DS_7475+0][3];
      switch (*drwsize)
      {
         case 0: 
            ev = ux_getenv("D7475_AH_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+0][0] = (float) atof(ev);
            ev = ux_getenv("D7475_AH_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+0][1] = (float) atof(ev);
            break;

         case 1:                                                  
            ev = ux_getenv("D7475_AV_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+1][0] = (float) atof(ev);
            ev = ux_getenv("D7475_AV_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+1][1] = (float) atof(ev);
            break;

         case 2: 
            ev = ux_getenv("D7475_B_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+2][0] = (float) atof(ev);
            ev = ux_getenv("D7475_B_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+2][1] = (float) atof(ev);
            break;
         case 3:
            ev = ux_getenv("D7475_C_X",UX_PRTERRS);
            ev = ux_getenv("D7475_C_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+3][1] = (float) atof(ev);
            break;

         case 4:                                                 
            ev = ux_getenv("D7475_D_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+4][0] = (float) atof(ev);
            ev = ux_getenv("D7475_D_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+4][1] = (float) atof(ev);
            break;

         case 5:                                                  
            ev = ux_getenv("D7475_E_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+5][0] = (float) atof(ev);
            ev = ux_getenv("D7475_E_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+5][1] = (float) atof(ev);
            break;

         case 6:                                                
            ev = ux_getenv("D7475_F_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+6][0] = (float) atof(ev);
            ev = ux_getenv("D7475_F_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+6][1] = (float) atof(ev); 
            break;
         case 7:                                                 
            ev = ux_getenv("D7475_A0_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+7][0] = (float) atof(ev); 
            ev = ux_getenv("D7475_A0_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+7][1] = (float) atof(ev); 
            break;

         case 8:                                                  
            ev = ux_getenv("D7475_A1_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+8][0] = (float) atof(ev);
            ev = ux_getenv("D7475_A1_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+8][1] = (float) atof(ev); 
            break;

         case 9:                                                  
            ev = ux_getenv("D7475_A2_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+9][0] = (float) atof(ev);
            ev = ux_getenv("D7475_A2_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+9][1] = (float) atof(ev); 
            break;

         case 10:                                            
            ev = ux_getenv("D7475_A3_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+10][0] = (float) atof(ev);
            ev = ux_getenv("D7475_A3_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+10][1] = (float) atof(ev);
            break;
         case 11:                                             
            ev = ux_getenv("D7475_A4_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+11][0] = (float) atof(ev);
            ev = ux_getenv("D7475_A4_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+11][1] = (float) atof(ev);
            break;

         case 12:                                                  
            ev = ux_getenv("D7475_USER1_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+12][0] = (double) atof(ev);
            ev = ux_getenv("D7475_USER1_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+12][1] = (float) atof(ev); 
            break;

         case 13:                                                  
            ev = ux_getenv("D7475_USER2_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+13][0] = (float) atof(ev);
            ev = ux_getenv("D7475_USER2_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+13][1] = (float) atof(ev); 
            break;

         case 14:                                            
            ev = ux_getenv("D7475_USER3_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+14][0] = (float) atof(ev);
            ev = ux_getenv("D7475_USER3_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+14][1] = (float) atof(ev);
            break;
         case 15:                                             
            ev = ux_getenv("D7475_USER4_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+15][0] = (float) atof(ev);
            ev = ux_getenv("D7475_USER4_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7475+15][1] = (float) atof(ev);
            break;
         default  :
            break;
      }

   xy[0] = UM_drawing_size[(*drwsize+DS_7475)][0];
   xy[1] = UM_drawing_size[(*drwsize+DS_7475)][1];
   return;
}
jp_pssize(drwsize, xy)
int *drwsize;

UU_REAL xy[];
{  char *ev;
   double atof();
   /* Set the X & Y offset values and the X & Y limit values based on the
   environmental values set by the run script.  Only set the values for
   the plotter model and drawing size requested.
   */
      ev = ux_getenv("DPS_PLOTDX",UX_PRTERRS);
      if (ev != NULL) 
			sscanf(ev,"%d",&wsplotdx);
		else
			wsplotdx = UM_drawing_size[DS_PS+0][2];
      ev = ux_getenv("DPS_PLOTDY",UX_PRTERRS);
      if (ev != NULL) 
			sscanf(ev,"%d",&wsplotdy);
      else
         wsplotdy = UM_drawing_size[DS_PS+0][3];
			
      switch (*drwsize)
      {
         case 0:                                               
            ev = ux_getenv("DPS_AH_X",UX_PRTERRS);
            if (ev != NULL) 
					UM_drawing_size[DS_PS+0][0] = (float) atof(ev);
            ev = ux_getenv("DPS_AH_Y",UX_PRTERRS);
            if (ev != NULL) 
					UM_drawing_size[DS_PS+1][0] = (float) atof(ev); 

            break;

         case 1:                                                  
            ev = ux_getenv("DPS_AV_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+1][0] = (float) atof(ev);
            ev = ux_getenv("DPS_AV_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+1][1] = (float) atof(ev);
            break;

         case 2:                                                  
            ev = ux_getenv("DPS_B_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+2][0] = (float) atof(ev);
            ev = ux_getenv("DPS_B_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+2][1] = (float) atof(ev); 
            break;

         case 3:                                                 
            ev = ux_getenv("DPS_C_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+3][0] = (float) atof(ev); 
            ev = ux_getenv("DPS_C_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+3][1] = (float) atof(ev);
            break;

         case 4:                             
            ev = ux_getenv("DPS_D_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+4][0] = (float) atof(ev);
            ev = ux_getenv("DPS_D_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+4][1] = (float) atof(ev);
            break;

         case 5:                                                  
            ev = ux_getenv("DPS_E_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+5][0] = (float) atof(ev);
            ev = ux_getenv("DPS_E_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+5][1] = (float) atof(ev);
            break;

         case 6:                                                  
            ev = ux_getenv("DPS_F_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+6][0] = (float) atof(ev);
            ev = ux_getenv("DPS_F_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+6][1] = (float) atof(ev);
            break;

         case 7:                                                  
            ev = ux_getenv("DPS_A0_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+7][0] = (float) atof(ev);
            ev = ux_getenv("DPS_A0_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+7][1] = (float) atof(ev);
            break;

         case 8:                                                  
            ev = ux_getenv("DPS_A1_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+8][0] = (float) atof(ev);
            ev = ux_getenv("DPS_A1_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+8][1] = (float) atof(ev);
            break;

         case 9:                                                  
            ev = ux_getenv("DPS_A2_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+9][0] = (float) atof(ev); 
            ev = ux_getenv("DPS_A2_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+9][1] = (float) atof(ev);

         case 10:                                                 
            ev = ux_getenv("DPS_A3_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+10][0] = (float) atof(ev);
            ev = ux_getenv("DPS_A3_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+10][1] = (float) atof(ev);
            break;

         case 11:                                                 
            ev = ux_getenv("DPS_A4_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+11][0] = (float) atof(ev);
            ev = ux_getenv("DPS_A4_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+11][1] = (float) atof(ev); 
            break;
         case 12:                                                  
            ev = ux_getenv("DPS_USER1_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+12][0] = (float) atof(ev);
            ev = ux_getenv("DPS_USER1_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+12][1] = (float) atof(ev); 
            break;

         case 13:                                                  
            ev = ux_getenv("DPS_USER2_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+13][0] = (float) atof(ev);
            ev = ux_getenv("DPS_USER2_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+13][1] = (float) atof(ev); 
            break;

         case 14:                                            
            ev = ux_getenv("DPS_USER3_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+14][0] = (float) atof(ev);
            ev = ux_getenv("DPS_USER3_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+14][1] = (float) atof(ev);
            break;
         case 15:                                             
            ev = ux_getenv("DPS_USER4_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+15][0] = (float) atof(ev);
            ev = ux_getenv("DPS_USER4_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_PS+15][1] = (float) atof(ev);
            break;

         default  :
            break;
      }
   	xy[0] = UM_drawing_size[(*drwsize)+DS_PS][0];
   	xy[1] = UM_drawing_size[(*drwsize)+DS_PS][1];
   return;
}
jp_7580size(drwsize, xy)
int *drwsize;
UU_REAL xy[];
{  char *ev;
   double atof();
   /* Set the X & Y offset values and the X & Y limit values based on the
   environmental values set by the run script.  Only set the values for
   the plotter model and drawing size requested.
   */
		ev = ux_getenv("D7580_PLOTDX",UX_PRTERRS);
      if (ev != NULL) 
			sscanf(ev,"%d",&wsplotdx);
      else
         wsplotdx = UM_drawing_size[DS_7580+0][2];
      ev = ux_getenv("D7580_PLOTDY",UX_PRTERRS);
      if (ev != NULL) 
			sscanf(ev,"%d",&wsplotdy);
      else
         wsplotdy = UM_drawing_size[DS_7580+0][3];

      switch (*drwsize)
      {
         case 0:                                                  
            ev = ux_getenv("D7580_AH_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+0][0] = (float) atof(ev);
            ev = ux_getenv("D7580_AH_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+0][1] = (float) atof(ev);
            break;

         case 1:                                                 
            ev = ux_getenv("D7580_AV_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+1][0] = (float) atof(ev);
            ev = ux_getenv("D7580_AV_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+1][1] = (float) atof(ev);
            break;

         case 2:                                                  
            ev = ux_getenv("D7580_B_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+2][0] = (float) atof(ev);
            ev = ux_getenv("D7580_B_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+2][1] = (float) atof(ev);
            break;
         case 3:                                                  
            ev = ux_getenv("D7580_C_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+3][0] = (float) atof(ev);
            ev = ux_getenv("D7580_C_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+3][1] = (float) atof(ev);
            break;

         case 4:                                                  
            ev = ux_getenv("D7580_D_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+4][0] = (float) atof(ev);
            ev = ux_getenv("D7580_D_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+4][1] = (float) atof(ev);
            break;

         case 5:                                                 
            ev = ux_getenv("D7580_E_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+5][0] = (float) atof(ev);
            ev = ux_getenv("D7580_E_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+5][1] = (float) atof(ev); 
            break;

         case 6:                                                  
            ev = ux_getenv("D7580_F_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+6][0] = (float) atof(ev);
            ev = ux_getenv("D7580_F_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+6][1] = (float) atof(ev); 
            break;
         case 7:                                                  
            ev = ux_getenv("D7580_A0_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+7][0] = (float) atof(ev);
            ev = ux_getenv("D7580_A0_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+7][1] = (float) atof(ev); 
            break;

         case 8:                                                  
            ev = ux_getenv("D7580_A1_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+8][0] = (float) atof(ev);
            ev = ux_getenv("D7580_A1_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+8][1] = (float) atof(ev);
            break;

         case 9:                                                  
            ev = ux_getenv("D7580_A2_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+9][0] = (float) atof(ev);
            ev = ux_getenv("D7580_A2_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+9][1] = (float) atof(ev); 
            break;

         case 10:                                                 
            ev = ux_getenv("D7580_A3_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+10][0] = (float) atof(ev);
            ev = ux_getenv("D7580_A3_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+10][1] = (float) atof(ev);
            break;
         case 11:                                                 
            ev = ux_getenv("D7580_A4_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+11][0] = (float) atof(ev);
            ev = ux_getenv("D7580_A4_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+11][1] = (float) atof(ev);
            break;

         case 12:                                                  
            ev = ux_getenv("D7580_USER1_X",UX_PRTERRS);
            if (ev != NULL) 
					UM_drawing_size[DS_7580+12][0] = (float) atof(ev); 
            ev = ux_getenv("D7580_USER1_Y",UX_PRTERRS);
            if (ev != NULL)
					UM_drawing_size[DS_7580+12][1] = (float) atof(ev);
            break;

         case 13:                                                  
            ev = ux_getenv("D7580_USER2_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+13][0] = (float) atof(ev);
            ev = ux_getenv("D7580_USER2_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+13][1] = (float) atof(ev); 
            break;

         case 14:                                            
            ev = ux_getenv("D7580_USER3_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+14][0] = (float) atof(ev);
            ev = ux_getenv("D7580_USER3_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+14][1] = (float) atof(ev);
            break;
         case 15:                                             
            ev = ux_getenv("D7580_USER4_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+15][0] = (float) atof(ev);
            ev = ux_getenv("D7580_USER4_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_7580+15][1] = (float) atof(ev);
            break;
         default  :
            break;
      }

/*
.....add check for using long axis plot
.....Yurong 8/1/97
*/
	if ( UM_drawing_size[(*drwsize)+DS_7580][0] > uw_758_maxxsize)
	{
		uw_758_longplot = 1;
/*
.....added extra page for outputing border
.....Yurong 9/4/98
*/
		uw_758fd_page = 1+UM_drawing_size[(*drwsize)+DS_7580][0]/uw_758_maxxsize;
	}
   xy[0] = UM_drawing_size[(*drwsize)+DS_7580][0];
   xy[1] = UM_drawing_size[(*drwsize)+DS_7580][1];
   return;
}

jp_1043size(drwsize, xy)
int *drwsize;
UU_REAL xy[];
{	char *ev;
	double atof();
	/* Set the X & Y offset values and the X & Y limit values based on the
	environmental values set by the run script.  Only set the values for
	the plotter model and drawing size requested.
	*/

	      ev = ux_getenv("D1043_PLOTDX",UX_PRTERRS);
      if (ev != NULL) 
			sscanf(ev,"%d",&wsplotdx);
      else
         wsplotdx = UM_drawing_size[DS_1043+0][2];

      ev = ux_getenv("D1043_PLOTDY",UX_PRTERRS);
      if (ev != NULL) 
			sscanf(ev,"%d",&wsplotdy);
      else
         wsplotdy = UM_drawing_size[DS_1043+0][3];

      switch (*drwsize)
      {
         case 0:                                                  
            ev = ux_getenv("D1043_AH_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+0][0] = (float) atof(ev);
            ev = ux_getenv("D1043_AH_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+0][1] = (float) atof(ev); 
            break;

         case 1:                                                  
            ev = ux_getenv("D1043_AV_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+1][0] = (float) atof(ev);
            ev = ux_getenv("D1043_AV_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+1][1] = (float) atof(ev); 
            break;

         case 2:                                                  
            ev = ux_getenv("D1043_B_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+2][0] = (float) atof(ev);
            ev = ux_getenv("D1043_B_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+2][1] = (float) atof(ev); 
            break;

         case 3:                                                  
            ev = ux_getenv("D1043_C_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+3][0] = (float) atof(ev); 
            ev = ux_getenv("D1043_C_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+3][1] = (float) atof(ev);
            break;

         case 4:                                                  
            ev = ux_getenv("D1043_D_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+4][0] = (float) atof(ev);
            ev = ux_getenv("D1043_D_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+4][1] = (float) atof(ev); 
            break;

         case 5:                                                  
            ev = ux_getenv("D1043_E_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+5][0] = (float) atof(ev);
            ev = ux_getenv("D1043_E_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+5][1] = (float) atof(ev);
            break;

         case 6:                                                 
            ev = ux_getenv("D1043_F_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+6][0] = (float) atof(ev); 
            ev = ux_getenv("D1043_F_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+6][1] = (float) atof(ev);
            break;

         case 7:                                                  
            ev = ux_getenv("D1043_A0_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+7][0] = (float) atof(ev);
            ev = ux_getenv("D1043_A0_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+7][1] = (float) atof(ev); 
            break;

         case 8:         
            ev = ux_getenv("D1043_A1_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+8][0] = (float) atof(ev);
            ev = ux_getenv("D1043_A1_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+8][1] = (float) atof(ev);
            break;

         case 9:          
            ev = ux_getenv("D1043_A2_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+9][0] = (float) atof(ev); 
            ev = ux_getenv("D1043_A2_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+9][1] = (float) atof(ev);  
            break;

         case 10:
            ev = ux_getenv("D1043_A3_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+10][0] = (float) atof(ev);
            ev = ux_getenv("D1043_A3_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+10][1] = (float) atof(ev);
            break;
			case 11:
            ev = ux_getenv("D1043_A4_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+11][0] = (float) atof(ev);
            ev = ux_getenv("D1043_A4_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+11][1] = (float) atof(ev);
            break;
         case 12:                                                  
            ev = ux_getenv("D1043_USER1_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+12][0] = (float) atof(ev);
            ev = ux_getenv("D1043_USER1_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+12][1] = (float) atof(ev); 
            break;

         case 13:                                                  
            ev = ux_getenv("D1043_USER2_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+13][0] = (float) atof(ev);
            ev = ux_getenv("D1043_USER2_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+13][1] = (float) atof(ev); 
            break;

         case 14:                                            
            ev = ux_getenv("D1043_USER3_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+14][0] = (float) atof(ev);
            ev = ux_getenv("D1043_USER3_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+14][1] = (float) atof(ev);
            break;
         case 15:                                             
            ev = ux_getenv("D1043_USER4_X",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+15][0] = (float) atof(ev);
            ev = ux_getenv("D1043_USER4_Y",UX_PRTERRS);
            if (ev != NULL) UM_drawing_size[DS_1043+15][1] = (float) atof(ev);
            break;

         default  :
            break;
      }



   xy[0] = UM_drawing_size[(*drwsize)+DS_1043][0];
   xy[1] = UM_drawing_size[(*drwsize)+DS_1043][1];
   return;
}
