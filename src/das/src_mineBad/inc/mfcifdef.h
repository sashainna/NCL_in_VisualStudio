/*********************************************************************
**    NAME         : mfcifdef.h 
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       mfcifdef.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:30
*********************************************************************/

/* underbars are not needed on the Vax and Apollo to interface Fortran to 
C and vice versa -- this file removes those underbars */


#ifndef UM_FCIFDEF

#include "usysdef.h"

/*
	FORTRAN routines called by C
*/

#if UU_COMP==UU_PYRAMID || UU_COMP==UU_SUN || UU_COMP==UU_RIDGE || UU_COMP==UU_MASSCOMP || UU_COMP==UU_IRIS || UU_COMP==UU_VAXULTRIX || UU_COMP==UU_IRIS4D || UU_COMP==UU_TEK || UU_COMP==UU_DECUNIX

#define ublend ublend_
#define scurby scurby_
#define gtbycl gtbycl_
#define gtbyno gtbyno_
#define wsbycr wsbycr_
#define wsedbl wsedbl_
#define romain romain_
#define romini romini_
#define roinit roinit_
#define ugtcrv ugtcrv_
#define unmcby unmcby_
#define ugednm ugednm_
#define uedcby uedcby_
#define unewby unewby_
#define ugface ugface_
#define uplcrv uplcrv_
#define utweek utweek_
#define umomom umomom_
#define gplout gplout_
#define ufdlst ufdlst_
#define anoffa anoffa_
#define nofaby nofaby_
#define noanfa noanfa_
#define endvr endvr_
#define fwded fwded_
#define faofby faofby_
#define bxoffa bxoffa_
#define sfoffa sfoffa_
#define ugtsrf ugtsrf_

#endif

/*
C routines called by FORTRAN
*/

#if UU_COMP==UU_PYRAMID || UU_COMP==UU_SUN || UU_COMP==UU_RIDGE || UU_COMP==UU_MASSCOMP || UU_COMP==UU_IRIS || UU_COMP==UU_VAXULTRIX || UU_COMP==UU_IRIS4D || UU_COMP==UU_TEK || UU_COMP == UU_DECUNIX

#define utxrwp utxrwp_
#define ufiddl ufiddl_
#define uscrol uscrol_
#define upline upline_
#define utwktr utwktr_
#define utwksf utwksf_
#define umadby umadby_
#define umatch umatch_
#define oclash oclash_

#endif

#define UM_FCIFDEF
#endif
