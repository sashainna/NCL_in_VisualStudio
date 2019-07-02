/*********************************************************************
**
**    NAME         :  ipipe.h
**
**       CONTAINS:
**
**       	subsystems in the input pipe for unicad
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       ipipe.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:26
**
*********************************************************************/

#ifndef IPIPEH


#define DPGM 1
#include "dpipe.h"
#include "uhep.h"
		
#define UD_SUBSYSTLEN 17

	extern int um_feamain(), uq_calc(), iconstr(), iuniview(), ux_udos_uni();
	extern int ud_recoff(), idebuger(), um_delete_last(), ud_resu(), ud_susp();

	extern UU_LOGICAL	ud_recon(), ud_playback();
	extern int ud_echoline();
	extern int itim(), ud_delay(), ud_eraser(), ud_help(); 

	int UD_subsyslen = UD_SUBSYSTLEN;	/* length of choice table */ 
	UD_DSUBSYS UD_subsys[UD_SUBSYSTLEN] =  { /* subsystem table */

			{ "feat", um_feamain, 0, 0 },			/* features subsystem */
			{ "calc", uq_calc, 0, UQ_CALCERROR },	/* calculator subsystem */
			{ "constr", iconstr, 0, 0 },			/* construction subsystem */
			{ "view", iuniview, 0, 0 },			/* features view control */
			{ "udos", ux_udos_uni, 0, 0 },		/* udos subsystem */

			{ "recon", ud_recon, 1, 0 },			/* record on */
			{ "playb", ud_playback, 1, 0 },		/* playback */
			{ "recoff", ud_recoff, 0, 0 },		/* record off */
			{ "echo", ud_echoline, 1, 0 },		/* echo control */
			{ "susp", ud_susp, 0, 0 },				/* suspend record or playback */
			{ "resu", ud_resu, 0, 0 },				/* resume record or playback */
			{ "delay", ud_delay, 1, 0 },			/* delay playback n seconds */

			{ "debug", idebuger, 0, 0 },			/* debugger */
			{ "tim", itim, 0, 0 },					/* interaction techniques */
			{ "delast", um_delete_last, 0, 0 },	/* delete last entity */
			{ "eraser", ud_eraser, 1, 0 },		/* erase error message */
			{ "help", ud_help, 1, 0 } 				/* help */
		};

/*	-- define special function for SAF choices -- */

#define UD_NUMSAF 1

	extern int ud_pmcurs();
	int UD_num_safs = UD_NUMSAF;
	UD_SAF UD_saf_table[UD_NUMSAF] = 		{  /* table of SAFs */
															{ "tog", ud_pmcurs}
														};

#define IPIPEH
#endif
