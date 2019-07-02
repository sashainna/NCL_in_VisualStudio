/************************************************************************
**   FILE NAME: wsntfuncid.h
**
**		function ID tables and some interface
**			function and data declarations
**
**
**     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**      MODULE NAME AND RELEASE LEVEL
**       %M% , %I%
**    DATE AND TIME OF LAST  MODIFICATION
**       %G% , %U%               
**
************************************************************************
*/
#ifndef NCLFUNCID_H
#define NCLFUNCID_H

#include "wsntres.h"
#include "wsntpmenu.h"
#include "wsntdlgitem.h"

#ifdef NCLWNT_CCMAIN
#ifdef EXT
#undef EXT
#endif
#define EXT 
#else
#define EXT	extern
#endif

typedef struct func_id
{
	UINT id;
	char func[40];
} NCL_Func_Id;

#define NCL_STATFUNC 8

EXT	CNCLMenu* NCL_menu[200];
EXT	int		NCL_menubar_count;

EXT void uw_ntinit_dlgtemp(char *dlgfile, DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20], int *itemnum, UINT &style, int size[2]);
EXT void uw_ntinit_dlgtemp1(char *string, int bar_size[2], UINT id, 
				  DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20]);
EXT void uw_ntinit_dlgtemp2(char *label, char *string, int bar_size[2], UINT pID, UINT eID,
					  DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20]);
EXT void uw_ntinit_dlgtemp3(char *string, int bar_size[2], UINT eID,
					  DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20]);
#undef EXT
#endif
