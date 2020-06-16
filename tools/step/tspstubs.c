
/*********************************************************************
**    NAME         :  tspstubs.c
**       CONTAINS:
**         (stub routines)
**
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       tspstubs.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:24
*********************************************************************/
#include "usysdef.h"
#include "tiges.h"

void uig_modal_config() {}
void uig_terminal_rec() {}
void uio_dir_par() {}
void uio_getdata() {}
void uio_term_sec() {}
void uio_trflst_init() {}

void uig_get_trans(ptr,t)
int ptr;
UU_REAL t[12];
{
	int i;
	for (i=0;i<12;i++) t[i] = 0.;
	t[0] = t[5] = t[10] = 1.;
}
