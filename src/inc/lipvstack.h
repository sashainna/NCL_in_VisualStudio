/*********************************************************************
**    NAME         :  nclstack.h
**     MODULE NAME AND RELEASE LEVEL 
**       lipvstack.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:27
*********************************************************************/

#include "lipv.h"

#ifndef ISTACK
#ifdef EXT
#undef EXT
#endif
#ifdef IPVSTACK
#define EXT
#else
#define EXT extern
#endif

#define MAXPRIM 16

typedef enum
{
	LW_CUT_NONE,
	LW_CUT_POSITION,
	LW_CUT_5AXIS,
	LW_CUT_BLADE,
	LW_CUT_LATHE,
	LW_CUT_THREAD,
	LW_CUT_ARC,
	LW_CUT_VERTICAL_ARC,
	LW_CUT_LATHE_ARC,
	LW_CUT_CIRCLE,
	LW_CUT_CIREND
} LW_mot_stack_ctype;
/*
.....Cutter stack structure
*/
typedef struct
{
	int tool;
	int cutseg[3];
	int fr_mode;
	int spindle;
	UU_REAL cutr[9];
	UU_REAL translucency[3];
	UU_REAL toler;
	UU_REAL maxang;
	UU_REAL fr_val;
	UU_REAL sp_val;
	UU_REAL ofs;
} LW_cutter_stack_struc;
/*
.....Motion stack structure
*/
typedef struct
{
	LW_mot_stack_ctype ctype;
	int cutn;
	int cutter[LW_MAX_SPINDLE];
	int colors[4];
	int moving_part;
	int mach_type;
	int nval;
	int nprim[2];
	int nspin;
	LtSessionPrim prim[2][MAXPRIM];
	LtDouble fdata[LW_MAX_AXES];
	LtDouble rdata[LW_MAX_AXES];
} LW_mot_stack_struc;
/*
.....Motion stack variables
*/
	EXT int LW_mot_stack_size;
	EXT UU_LOGICAL LW_mot_stack_active,LW_mot_stack_fixture;
#endif
#define ISTACK
#undef EXT
