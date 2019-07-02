/*********************************************************************
**    NAME         :  version.h
**       CONTAINS:
**       Global version and link time variables.
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uversion.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:06
*********************************************************************/

#ifndef VERSIONH

#ifdef UU_VMAIN
char UU_version[9] = "unicad";
char UU_vtime[9] = ":T:";
char UU_vdate[9] = ":D:";
char UU_vdatime[45] = " Fri Sep 21 12:14:38  1990";
#else
extern char UU_version[9];
extern char UU_vtime[9];
extern char UU_vdate[9];
extern char UU_vdatime[45];
#endif

#define VERSIONH
#endif
