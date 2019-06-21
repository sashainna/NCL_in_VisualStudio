/*********************************************************************
**    NAME         :  nclstack.h
**     MODULE NAME AND RELEASE LEVEL 
**       nclstack.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:36
*********************************************************************/

#include "nclfile.h"
#include "nclmplay.h"

#ifndef NSTACK
#ifdef EXT
#undef EXT
#endif
#ifdef NCLSTACK
#define EXT
#else
#define EXT extern
#endif
/*
.....Structures
*/
	typedef struct
	{
		UN_motseg *mbegin;
		UN_motseg *mend;
		UN_clstruc *cbegin;
		UN_clstruc *cend;
		UN_clstruc *crec;
		UU_REAL spt[6];
		UU_REAL cutr[20];
		UM_real8 tracut[12];
		UM_real8 invtra[12];
		UU_KEY_ID symkey[3];
		int cpt;
		int cfl[10];
		int trafl;
		UM_int2 rapid;
		UM_int2 multax;
		char symbol[3][MAXSYMLEN+2];
	} UN_mot_stack_struc;
/*
.....Motion stack variables
*/
	EXT int UN_mot_stack_size,UN_mot_stack_ptr,UN_mot_stack_num;
	EXT int UN_mot_stack_state;
	EXT UN_mot_stack_struc *UN_mot_stack;
#endif
#define NSTACK
#undef EXT
