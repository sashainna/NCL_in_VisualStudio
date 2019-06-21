/*********************************************************************
**
**    NAME         :  zpipe.h
**       CONTAINS: subsystems in the input pipe for unicad
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       zpipe.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:25
**
*********************************************************************/

#ifndef ZPIPEH

#define DPGM 1
#include "dpipe.h"
#undef DPGM
#include "uhep.h"
		
/*
.....added 2 for begin-of-line, end-of-line
*/
/*
#define UD_SUBSYSTLEN 32
*/
#define UD_SUBSYSTLEN 34

	extern uz_zrecon(), uz_zplayback(), uz_zrecoff();
	extern uz_zresu(), uz_zpause(), ud_echoline(), uz_zpanic();
	extern uj_auto_test(), umu_m31_tweak(), uc_deleteLast(), um_repaint();
	extern uj_tut_on(), uj_tut_off(), uvu_vpzoomin(), uu_debug_off();

	extern int uz_zdrafting(), zdrftmdl();
	extern int zstatus();
	extern int zwire(), uq_calc(), uj_help();

	extern int uj_refpnt(), ur_undo();
	extern int ud_bcomlin(), ud_ecomlin();

	int UD_subsyslen = UD_SUBSYSTLEN;		/* length of choice table */ 

	UD_DSUBSYS UD_subsys[UD_SUBSYSTLEN] =  { 	/* subsystem table */

			{ "at", uj_auto_test, 1, 0 },				/* auto-test playback */
			{ "axgrid", NULL, 0, 0 },				/* axis - grid */
			{ "drafting", uz_zdrafting, 0, 0 },		/* drafting */
			{ "drftmdl", NULL, 0, 0 },			/* draft modals */
			{ "edit", NULL, 0, 0 },					/* edit */
			{ "env", NULL, 0, 0 },						/* environment */
			{ "feat", 0, 0 },					/* features */
			{ "filemgmt", NULL, 0, 0 },		/* file mgmt */
			{ "status", NULL, 0, 0 },				/* status */
			{ "symmgt", NULL, 0, 0 },				/* symbol mgmt */
			{ "viewing", NULL, 0, 0 },		/* viewing */
			{ "wire", NULL, 0, 0 },					/* wireframe */
			{ "calc", uq_calc, 0, 0 },					/* calculator */
			{ "drawmgmt", NULL, 0, 0 },		/* drawing mgmt */
			{ "help", uj_help, 0, 0 },					/* help */
			{ "refpnt", uj_refpnt, 0, 0 },			/* reference point */
			{ "undo", ur_undo, 0, 0 },					/* undo */
			{ "recon", uz_zrecon, 1, 0 },				/* record on */
			{ "playb", uz_zplayback, 1, 0 },			/* playback */
			{ "recoff", uz_zrecoff, 0, 0 },			/* record off */
			{ "pause", uz_zpause, 0, 0 },				/* suspend record or playback */
			{ "resume", uz_zresu, 0, 0 },				/* resume record or playback */
			{ "echo", ud_echoline, 1, 0 },			/* echo control */
			{ "panic", uz_zpanic, 0, 0 },				/* Panic stop */
			{ "tuton", uj_tut_on, 0, 0 },				/* tutorial on */
			{ "tutoff", uj_tut_off, 0, 0 },			/* tutorial off */
			{ "debug", NULL, 0, 0 },				/* debugger */
			{ "bugoff", uu_debug_off, 0, 0 },		/* debugging off */
			{ "deletelast", uc_deleteLast, 0, 0 },	/* delete last */
			{ "repaint", um_repaint, 0, 0 },			/* repaint */
			{ "windowzoom", uvu_vpzoomin, 0, 0 },	/* window zoom */
			{ "tweak", umu_m31_tweak, 0, 0 },			/* Romulus Tweak */
			{ "blin", ud_bcomlin, 1, 0 },          /* begin-of-line */
			{ "elin", ud_ecomlin, 1, 0 }           /* end-of-line */
		};

/*	-- define special function for SAF choices -- */

#define UD_NUMSAF 1

	extern int ud_pmcurs();
	int UD_num_safs = UD_NUMSAF;
	UD_SAF UD_saf_table[UD_NUMSAF] = 		{  /* table of SAFs */
															{ "tog", ud_pmcurs}
														};

#define ZPIPEH
#endif
