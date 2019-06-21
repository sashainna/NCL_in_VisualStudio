/*********************************************************************
**
**    NAME         :  diconm.h
**
**       CONTAINS:
**       	icon menu functions
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       diconm.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:13
**
*********************************************************************/

#ifndef DICONMH

#include "usysg.h"
#include "gi1.h"

#define UD_ICMU_UP(devnum) \
 	{ uu_dprint(UU_DTRC,(us, "in ud_icmu_up, devnum=%d", devnum)); \
	udm_icon_up(devnum); }
/*	gschoicemode(UD_ksws, devnum, UG_EVENT, UG_ECHO); }*/

#define UD_ICMU_DN(devnum) \
 	{ uu_dprint(UU_DTRC,(us, "in ud_icmu_dn, devnum=%d", devnum)); \
	udm_icon_down(devnum); }
/*	gschoicemode(UD_ksws, devnum, UG_REQUEST, UG_ECHO); }*/

#define DICONMH
#endif
