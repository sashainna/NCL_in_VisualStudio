/*********************************************************************
**
**    NAME         :  dpipe.h
**
**       CONTAINS:
**       	mechanism to hook new subsystems int the input pipe
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       dpipe.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:14
**
*********************************************************************/

#ifndef DPIPEH

#define EXT extern

	typedef struct
	{
		char *name;							/* name of subsystem */
		int (*ddentry)();						/* entry point of activation routine */
		int nump;								/* number of parameters expected */
		int subhelpno;							/* number for system level help */
	} UD_DSUBSYS;

	typedef struct
	{
		char safname[7];						/* saf name */
		int (*safprc)();						/* saf procedure */
	} UD_SAF;


	EXT int UD_subsyslen;					/* length of choice table */ 

#ifndef DPGM
	EXT UD_DSUBSYS UD_subsys[];			/* subsystem table */
#endif

#undef EXT
#define DPIPEH

#endif

