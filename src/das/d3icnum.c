/*********************************************************************
**    NAME         :  d1icnum.c -- DAS icon umber translator
**       CONTAINS:
**				ud_iconnum
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       d3icnum.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:05
*********************************************************************/
#include "usysdef.h"
#include "usysg.h"
#include "uims.h"
#include "unserve.h"
#include "math.h"
#include "udebug.h"

/*********************************************************************
**    E_FUNCTION :  int ud_iconnum(dev,choice) -- return icon number.
**       Convert (dev,choice) into a single icon number from 1..n
**    PARAMETERS   
**       INPUT  :  int dev -- choice device number of a UD_CHC area.
**						 int choice -- choice number(1..m)
**       OUTPUT :  
**    RETURNS      : icon number.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ud_iconnum(dev,choice)
int dev;
int choice;
{
	UD_AREA *areapt;
	int num,i;
	int scrn;
	int scr;
	int indx;
	char us[140];

	scrn=UD_curlayout.curr_screen;
	uu_denter2(UU_DTRC,(us,"ud_iconnum(%d,%d) noareas[%d]=%d",
		dev,choice,scrn,UD_duimsdeflt.screen[scrn].noareas[UD_CHC]));
	indx= -1;
	num=0;
	for (i=0; i<UD_duimsdeflt.screen[scrn].noareas[UD_CHC]; i++) {
		areapt= &(UD_duimsdeflt.screen[scrn].areas[UD_CHC])[i];
		if ((*areapt).devno==dev) {		/* found the choice device */
			indx=i;
			break;
		}
	}
	if (indx>=0) {						/* found the choice device */
		for (scr=0; scr<scrn; scr++) {	/* for each previous screen */
			for (i=0; i<UD_duimsdeflt.screen[scr].noareas[UD_CHC]; i++) {	
				/* add up previons choices on this scrn */
				areapt= &(UD_duimsdeflt.screen[scr].areas[UD_CHC])[i];
				num=num+(*areapt).cont.nchoice;
				uu_dprint(UU_DTRC,(us,"ud_iconnum. nchoice[%d]=%d",i,
					(*areapt).cont.nchoice));
			}
		}
		for (i=0; i<indx; i++) {	/* add up previons choices on this scrn */
			areapt= &(UD_duimsdeflt.screen[scrn].areas[UD_CHC])[i];
			num=num+(*areapt).cont.nchoice;
			uu_dprint(UU_DTRC,(us,"ud_iconnum. nchoice[%d]=%d",i,
				(*areapt).cont.nchoice));
		}
		num=num+choice;
	}
	uu_dprint(UU_DTRC,(us,"%d=ud_iconnum(%d,%d). indx=%d",
		num,dev,choice,indx));
	uu_dexit;
	return(num);
}
