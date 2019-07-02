/*********************************************************************
**
**    NAME         :  z2pipe.h
**       CONTAINS: subsystems in the input pipe for UIDS
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       z2pipe.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:24
**
*********************************************************************/

#ifndef Z2PIPEH

#define DPGM 1
#include "dpipe.h"
#include "uhep.h"
		
#define UD_SUBSYSTLEN 22

	extern uz_zrecon(), uz_zplayback(), uz_zrecoff(), idebuger();
	extern uz_zresu(), uz_zpause(), ud_echoline(), uz_zpanic();
	extern uj_auto_test(), uc_deleteLast(), uz_z2repaint();
	extern uj_tut_on(), uj_tut_off(), uvu_vpzoomin(), uu_debug_off();
	extern uz_z2layed(), uz_z2gicon_ed(), uz_z2ricon_ed(), uz_z2grid_on();
	extern uz_z2flmgmt(), uz_z2view(), ur_undo();

	int UD_subsyslen = UD_SUBSYSTLEN;		/* length of choice table */ 

	UD_DSUBSYS UD_subsys[UD_SUBSYSTLEN] =  { 	/* subsystem table */

			{"at", uj_auto_test, 1, 0},				/* record on */
			{"recon", uz_zrecon, 1, 0},				/* record on */
			{"playb", uz_zplayback, 1, 0},			/* playback */
			{"recoff", uz_zrecoff, 0, 0},				/* record off */
			{"pause", uz_zpause, 0, 0},				/* suspend record or playback */
			{"resume", uz_zresu, 0, 0},				/* resume record or playback */
			{"echo", ud_echoline, 1, 0},				/* echo control */
			{"panic", uz_zpanic, 0, 0},				/* Panic stop */
			{"tuton", uj_tut_on, 0, 0},				/* tutorial on */
			{"tutoff", uj_tut_off, 0, 0},				/* tutorial off */
			{"debug", idebuger, 0, 0},					/* debugger */
			{"bugoff", uu_debug_off, 0, 0},			/* debugging off */
			{"deletelast", uc_deleteLast, 0, 0},	/* delete last */
			{"repaint", uz_z2repaint, 0, 0},			/* repaint */
			{"windowzoom", uvu_vpzoomin, 0, 0},		/* window zoom */
			{"layed", uz_z2layed, 0, 0},				/* layout editor */
			{"gicon", uz_z2gicon_ed, 0, 0},			/* graphics icon editor */
			{"ricon", uz_z2ricon_ed, 0, 0},			/* raster icon editor */
			{"setgrid", uz_z2grid_on, 0, 0},			/* axis - grid */
			{"filemgmt", uz_z2flmgmt, 0, 0},			/* file mgmt */
			{"viewing", uz_z2view, 0, 0},				/* viewing */
			{"undo", ur_undo, 0, 0}						/* undo */
		};

/*	-- define special function for SAF choices -- */

#define UD_NUMSAF 1

	extern int ud_pmcurs();
	int UD_num_safs = UD_NUMSAF;
	UD_SAF UD_saf_table[UD_NUMSAF] = 		{  /* table of SAFs */
															{ "tog", ud_pmcurs}
														};

#define Z2PIPEH
#endif
