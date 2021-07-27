/*********************************************************************
**    NAME         :  nestubs.c
**       CONTAINS:  routines temporary stub routines
**    COPYRIGHT 1990 (c) Mills Data Systems Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nestubs.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:52
*********************************************************************/

#include "usysdef.h"

#if UU_COMP == UU_VAXVMS
#ifdef UU_ALPHA
gencev() {}
parevl() {}
usfevl() {}
#else
catiac() { printf("Called catiac"); }
catiae() { printf("Called catiae"); }
catiaq() { printf("Called catiaq"); }
comlod() { printf("Called comlod"); }
extcev() { printf("Called extcev"); }
gtaput() { printf("Called gtaput"); }
gtpthk() { printf("Called gtpthk"); }
magic()  { printf("Called magic");  }
prsevl() { printf("Called prsevl"); }
prslod() { printf("Called prslod"); }
reparm() { printf("Called reparm"); }
setbsk() { printf("Called setbsk"); }
thkadj() { printf("Called thkadj"); }
#endif
#else
int tig_max_cvlab;
int tig_max_sflab;
int UIG_from_sw;
int UIG_unmatch_sec;
int UIG_regfactor;
int UIG_checkkeys;
int UIG_matchkeys;
int UIG_regcount;
int UIG_regressive;
int UIG_sflist_keys;
int tig_unlabeled_keys;
int tig_unlabeled;
int UIG_match_tol;
/*crvevl() {}*/
gencev() {}
parevl() {}
usfevl() {}
uu_free_all() {}
uig_comp_circle() {}
uig_comp_compcrv() {}
uig_comp_line() {}
uig_comp_rbsplcrv() {}
uig_comp_trimsrf() {}
uig_create_sec_unmatch() {}
uig_match_updatts() {}
uig_match_circle() {}
uig_match_compcrv() {}
uig_match_line() {}
uig_match_point() {}
uig_match_rbsplcrv() {}
uig_match_trimsrf() {}
uig_secondary_unmatched() {}
uig_unused_sec() {}
#endif
