/*********************************************************************
**
**    NAME         :  dmodul.h
**
**       CONTAINS:
**       	include file to control maximal and minimal DAS
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       dmodul.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:14
**
*********************************************************************/

#ifndef DMODUL
#ifdef MINIMAL
#ifdef DPGM

/***********  Minimal DAS defines  ************/

#include "usysdef.h"
#include "uims.h"
#include "dpipe.h"

static UD_AREA  grafarea={0,1,1,	/* no bkground, white border*/
			0.,.15,1.,.75,				/* graphics area */
			0,0,0							/* segno, devno, not visible */
};
static UD_AREA  sprmtarea={0,1,1,	/* no bkground, white border*/
			0.,.1,1.,.15,				/* area */
			0,0,0							/* segno, devno, not visible */
};
static UD_AREA  lprmtarea={0,1,1,	/* no bkground, white border*/
			0.,.05,1.,.10,				/* area */
			0,0,0							/* segno, devno, not visible */
};
static UD_AREA  errarea={0,1,1,	/* no bkground, white border*/
			0.,.0,1.,.05,				/* area */
			0,0,0							/* segno, devno, not visible */
};
UD_UIMS UD_duimsdeflt={				/* user interface */
	1,										/* 1 screen */
	0.,0.,1.,.75,						/* ws window */
	1,1,1,1,								/* a graf, Sprmt, Lprmt and error msg area*/
	0,0,0,0,0,0,0,						/* no other areas */
	&grafarea,&sprmtarea,&lprmtarea,&errarea,
	NULL,NULL,NULL,NULL,NULL,NULL,NULL,
};
UD_CURLAYOUT UD_curlayout={
	0,0,
	0,0,0,0,0,0,0,0,0,0,
	0,
	0,0,0
};											/* current areas in use */

#define UD_SUBSYSTLEN 1

	int UD_subsyslen = UD_SUBSYSTLEN;		/* length of choice table */ 

	UD_DSUBSYS UD_subsys[UD_SUBSYSTLEN] =  { 	/* subsystem table */
			{ "debug", NULL, 0, 0 }				/* debugger */
		};

/*	-- define special function for SAF choices -- */

#define UD_NUMSAF 1

	extern int ud_pmcurs();
	int UD_num_safs = UD_NUMSAF;
	UD_SAF UD_saf_table[UD_NUMSAF] = 		{  /* table of SAFs */
															{ "tog", ud_pmcurs}
														};

#define EPGM
#include "derror.h"
#undef EPGM

#include "usysg.h"
#include "dinput.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselect.h"
#include "dselmask.h"
#include "ddef.h"
#include "dmark.h"
#include "dsubcom.h"

int UU_application = 3;				/* minimal DAS */

#endif
#endif

#define DMODUL
#endif
