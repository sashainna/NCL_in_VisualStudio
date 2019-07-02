/*********************************************************************
**    NAME         :  mfeatcom.h
**       CONTAINS: Features include file.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       mfeatcom.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:30
*********************************************************************/

#ifndef UM_MFEATCOM

#include "usysdef.h"
#include "dasnog.h"

#ifdef UM_FEAINIT
#define EXT
#else
#define EXT extern
#endif


#define UM_FVECT 0
#define UM_FCOORD 1
#define UM_FREAL 2

#define UM_MAXFPICK 10  /* max. number of entities to put features on */
#define UM_NORDER 4     /* max. number of feature orders + 1 */

#define UM_MAXVECTF  100   /* max. number of vector features allowed */
#define UM_MAXCOORDF 100   /* max. number of coordinate features allowed */
#define UM_MAXREALF  100   /* max. number of real features allowed */

#define UM_VECTMIN 1
#define UM_VECTMAX UM_MAXVECTF
#define UM_COORDMIN (UM_MAXVECTF + 1)
#define UM_COORDMAX (UM_MAXVECTF + UM_MAXCOORDF)
#define UM_REALMIN (UM_MAXVECTF + UM_MAXCOORDF + 1)
#define UM_REALMAX (UM_MAXVECTF + UM_MAXCOORDF + UM_MAXREALF)

typedef struct {
	UD_PLOCREC ploc;				/* picked location on entity */
	int	used;						/* 0 if entry is free */
	int	seg;						/* DIGS segment id for features */
	int	view[UM_NORDER];		/* normtrans for feature */
	int	order[UM_NORDER];		/* 0 if not displayed, otherwise displayed */
	} UM_feature_struct;

EXT UM_feature_struct UM_Features[UM_MAXFPICK];

EXT int UM_F_vect_count;						/* number of vector features */
EXT UU_REAL UM_F_vect[UM_MAXVECTF][3];		/* vector feature table */

EXT int UM_F_coord_count;						/* number of coordinate features */
EXT UU_REAL UM_F_coord[UM_MAXCOORDF][3];	/* coordinate feature table */

EXT int UM_F_real_count;						/* number of real features */
EXT UU_REAL UM_F_real[UM_MAXREALF];			/*	real feature table */


#undef EXT
#define UM_MFEATCOM
#endif
