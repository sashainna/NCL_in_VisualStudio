/*********************************************************************
**    NAME         :  ycom.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**     ycom.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**     04/29/15 , 15:07:23
*********************************************************************/
#ifndef YCOMDEF

#include "nclxmdl.h"
#include "nclxmot.h"

#ifdef YCOMGBL
#define EXT
int UY_nislands=0;
#else
#define EXT extern
extern int UY_nislands;
#endif

/*
.....PARSER values for geometry types
*/
#define NCLG_UNKNOWN	1
#define NCLG_SCALAR	2
#define NCLG_POINT	3
#define NCLG_VECTOR	4
#define NCLG_LINE		5
#define NCLG_PLANE	6
#define NCLG_CIRCLE	7
#define NCLG_CURVE	8
#define NCLG_SURF		9
#define NCLG_MATRIX	10
#define NCLG_SHAPE	18
#define NCLG_PATERN	20
#define NCLG_PNTVEC	21
#define NCLG_POLYLINE	23
#define NCLG_NETSF	27

EXT NCLX_mdl_struct *UY_ps, *UY_ds, *UY_cs[8], *UY_hldgeo, *UY_sf4;
EXT NCLX_mdl_struct *UY_hldps, *UY_hldds, *UY_hldcs, *UY_secps;
EXT NCLX_mdl_struct *UY_island[508];
EXT NCLX_mdl_point *UY_nearpt[504];
EXT int UY_nps, UY_nds, UY_ics, UY_ncs;
EXT NCLX_mdl_ptr **UY_netps, **UY_netds;
EXT char *UY_clstart;


#if (UU_COMP == UU_SUN) || (UU_COMP == UU_IRIS4D)
#ifndef UU_RS6000
#define isitwf isitwf_
#define gtclsd gtclsd_
#define evstup evstup_
#define smtset smtset_
#define smtrst smtrst_
#define pklild pklild_
#define pkllod pkllod_
#define fedout fedout_
#define pklsto pklsto_
#define poklst poklst_
#define gtrld  gtrld_
#define sftype sftype_
#define nclxini nclxini_
#define cutget cutget_
#define ygchk  ygchk_
#define ygchk1  ygchk1_
#define ymtool ymtool_
#define ymfwd  ymfwd_
#define ymaxan ymaxan_
#define ymaxdp ymaxdp_
#define ynumpt ynumpt_
#define ythick ythick_
#define ytoler ytoler_
#define yindpt yindpt_
#define yindvc yindvc_
#define yfgosf yfgosf_
#define ytlaxs ytlaxs_
#define ycntct ycntct_
#define yfdrat yfdrat_
#define ysfvct ysfvct_
#define yfdrcv yfdrcv_
#define yfgoxx yfgoxx_
#define yfgoto yfgoto_
#define cutset cutset_
#define ccvlen ccvlen_
#define uevsff uevsff_
#define uevsft uevsft_
#define uevcvv uevcvv_
#define uevcvt uevcvt_
#define tbdini tbdini_
#define tbdout tbdout_
#define tbdfin tbdfin_
#define ptinsf1 ptinsf1_
#define yautst yautst_
#define yautst1 yautst1_
#define yffrom yffrom_
#define ypscnd ypscnd_
#define yfscrub yfscrub_
#define yfrmill yfrmill_
#define yfapok yfapok_
#define yfapok yfapok_
#define yflrgh yflrgh_
#define yflfin yflfin_
#define putran putran_
#define getran getran_
#define yfnint yfnint_
#endif
#endif

#define YCOMDEF
#endif
