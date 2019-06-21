/*********************************************************************
**    NAME         :  ngeom.h
**       CONTAINS: curve/surface evaluator definitions
**    COPYRIGHT 2006 (c) NCCS Inc.  All Rights Reserved.
**       ngeom.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:39
*********************************************************************/
/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                             !!!!!!
!!!!!!   NOTE:                                                     !!!!!!
!!!!!!      Recompile routine NEGLOBAL.C to cause any changes      !!!!!!
!!!!!!      made in this include file to be reflected in the       !!!!!!
!!!!!!      executable.                                            !!!!!!
!!!!!!                                                             !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/
#ifndef NGEOM

#ifdef NCL_MPGM

int NCL_triansf = 0;

#else

extern int NCL_triansf;

#endif

#define NGEOM

#endif
