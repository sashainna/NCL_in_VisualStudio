/*********************************************************************
**    NAME         :  nudrw.c
**       CONTAINS: user interface routines for drawings
**			OLD: int nclu_create_drawing_form(drawing)
**			OLD: int nclu_create_drawing()
**			int nclu_init_drawing_size()
**     MODULE NAME AND RELEASE LEVEL
**       nudrw.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**      04/29/15 , 15:09:07
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
/**
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "udfmracs.h"
**/
#include "mdcoord.h"
#include "mdunits.h"
#include "mdcpln.h"
#include "mxxx.h"
#include "mdebug.h"
#include "mdraw.h"
#include "ustdio.h"
#include "uims.h"
#include "dwindow.h"
#include "dasnog.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "nclfc.h"
/**
#include "gcolors.h"         
**/
#include "mdrwsize.h"
#include "xenv1.h"

#define NO_DEFAULT 0
#define DEFAULT 1

/**************************************************************************
**    E_FUNCTION     : int nclu_init_drawing_size()
**       Initialize UM_drawing_size from "drwsize.init" values.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error and all data filled in
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int
nclu_init_drawing_size()
	{
	int i;
	int plot;
	double atof();
	char *ev, *ux_getenv();


	/* Set values in UM_drawing_size array for selected plotter model 
			and drawing size */
/*
.....change size type up to 16
.....Yurong 7/31/97
*/
	for (plot = 0; plot < 5; plot++)
		for (i = 0; i < 16; i++)
			{
			switch (plot)
				{

				case 0:			/* HP 7475 */
					switch (i)
						{
						case 0:  		/* AH drawing */
							ev = ux_getenv("D7475_AH_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_AH_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 1:  		/* AV drawing */
							ev = ux_getenv("D7475_AV_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_AV_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 2:  		/* B drawing */
							ev = ux_getenv("D7475_B_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_B_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 3:  		/* C drawing */
							ev = ux_getenv("D7475_C_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_C_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 4:  		/* D drawing */
							ev = ux_getenv("D7475_D_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_D_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 5:  		/* E drawing */
							ev = ux_getenv("D7475_E_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_E_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 6:  		/* F drawing */
							ev = ux_getenv("D7475_F_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_F_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 7:  		/* A0 drawing */
							ev = ux_getenv("D7475_A0_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_A0_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 8:  		/* A1 drawing */
							ev = ux_getenv("D7475_A1_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_A1_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 9:  		/* A2 drawing */
							ev = ux_getenv("D7475_A2_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_A2_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 10:  		/* A3 drawing */
							ev = ux_getenv("D7475_A3_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_A3_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 11:  		/* A4 drawing */
							ev = ux_getenv("D7475_A4_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_A4_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
						case 12:  		/* USER1 drawing */
							ev = ux_getenv("D7475_USER1_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_USER1_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 13:  		/* USER2 drawing */
							ev = ux_getenv("D7475_USER2_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_USER2_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 14:  		/* USER3 drawing */
							ev = ux_getenv("D7475_USER3_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_USER3_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 15:  		/* USER4 drawing */
							ev = ux_getenv("D7475_USER4_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7475_USER4_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;


						default:
							break;

						}
					break;	

				case 1: 		/* HP 7580 */
					switch (i)
						{
						case 0:  	/* AH drawing */
							ev = ux_getenv("D7580_AH_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_AH_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
	
						case 1:  	/* AV drawing */
							ev = ux_getenv("D7580_AV_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_AV_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
	
						case 2:  		/* B drawing */
							ev = ux_getenv("D7580_B_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_B_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 3:  		/* C drawing */
							ev = ux_getenv("D7580_C_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_C_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 4:  		/* D drawing */
							ev = ux_getenv("D7580_D_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_D_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 5:  		/* E drawing */
							ev = ux_getenv("D7580_E_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_E_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 6:  		/* F drawing */
							ev = ux_getenv("D7580_F_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_F_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 7:  		/* A0 drawing */
							ev = ux_getenv("D7580_A0_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_A0_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 8:  		/* A1 drawing */
							ev = ux_getenv("D7580_A1_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_A1_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 9:  		/* A2 drawing */
							ev = ux_getenv("D7580_A2_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_A2_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 10:  		/* A3 drawing */
							ev = ux_getenv("D7580_A3_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_A3_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 11:  		/* A4 drawing */
							ev = ux_getenv("D7580_A4_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_A4_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
						case 12:  		/* USER1 drawing */
							ev = ux_getenv("D7580_USER1_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_USER1_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 13:  		/* USER2 drawing */
							ev = ux_getenv("D7580_USER2_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_USER2_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 14:  		/* USER3 drawing */
							ev = ux_getenv("D7580_USER3_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_USER3_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 15:  		/* USER4 drawing */
							ev = ux_getenv("D7580_USER4_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D7580_USER4_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;


						default:
							break;

						}
					break;	

				case 2: 		/* PS */
					switch (i)
						{
						case 0:  	/* AH drawing */
							ev = ux_getenv("DPS_AH_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_AH_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
	
						case 1:  	/* AV drawing */
							ev = ux_getenv("DPS_AV_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_AV_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
	
						case 2:  		/* B drawing */
							ev = ux_getenv("DPS_B_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_B_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 3:  		/* C drawing */
							ev = ux_getenv("DPS_C_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_C_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 4:  		/* D drawing */
							ev = ux_getenv("DPS_D_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_D_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 5:  		/* E drawing */
							ev = ux_getenv("DPS_E_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_E_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 6:  		/* F drawing */
							ev = ux_getenv("DPS_F_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_F_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 7:  		/* A0 drawing */
							ev = ux_getenv("DPS_A0_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_A0_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 8:  		/* A1 drawing */
							ev = ux_getenv("DPS_A1_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_A1_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 9:  		/* A2 drawing */
							ev = ux_getenv("DPS_A2_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_A2_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 10:  		/* A3 drawing */
							ev = ux_getenv("DPS_A3_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_A3_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 11:  		/* A4 drawing */
							ev = ux_getenv("DPS_A4_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_A4_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
						case 12:  		/* USER1 drawing */
							ev = ux_getenv("DPS_USER1_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_USER1_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 13:  		/* USER2 drawing */
							ev = ux_getenv("DPS_USER2_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_USER2_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 14:  		/* USER3 drawing */
							ev = ux_getenv("DPS_USER3_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_USER3_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 15:  		/* USER4 drawing */
							ev = ux_getenv("DPS_USER4_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("DPS_USER4_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;


						default:
							break;

						}
					break;	

				case 3:		/* Calcomp 1043 */
					switch (i)
						{
						case 0:  		/* AH drawing */
							ev = ux_getenv("D1043_AH_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_AH_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 1:  		/* AV drawing */
							ev = ux_getenv("D1043_AV_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_AV_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 2:  		/* B drawing */
							ev = ux_getenv("D1043_B_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_B_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 3:  		/* C drawing */
							ev = ux_getenv("D1043_C_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_C_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 4:  		/* D drawing */
							ev = ux_getenv("D1043_D_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_D_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 5:  		/* E drawing */
							ev = ux_getenv("D1043_E_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_E_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 6:  		/* F drawing */
							ev = ux_getenv("D1043_F_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_F_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;

						case 7:  		/* A0 drawing */
							ev = ux_getenv("D1043_A0_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_A0_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 8:  		/* A1 drawing */
							ev = ux_getenv("D1043_A1_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_A1_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 9:  		/* A2 drawing */
							ev = ux_getenv("D1043_A2_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_A2_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 10:  		/* A3 drawing */
							ev = ux_getenv("D1043_A3_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_A3_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 11:  		/* A4 drawing */
							ev = ux_getenv("D1043_A4_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_A4_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
						case 12:  		/* USER1 drawing */
							ev = ux_getenv("D1043_USER1_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_USER1_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 13:  		/* user2 drawing */
							ev = ux_getenv("D1043_USER2_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_USER2_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 14:  		/* USER3 drawing */
							ev = ux_getenv("D1043_USER3_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_USER3_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;
			
						case 15:  		/* USER4 drawing */
							ev = ux_getenv("D1043_USER4_X",UX_PRTERRS); /* X limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][0]=((float)atof(ev)); 
							ev = ux_getenv("D1043_USER4_Y",UX_PRTERRS); /* Y limit */
							if (ev != NULL) UM_drawing_size[plot*16+i][1]=((float)atof(ev)); 
							break;


						default:
							break;
						}
					break;	

				default:
					break;
				}	
			}
	}
	
