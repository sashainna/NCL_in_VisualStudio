
/*********************************************************************
**    NAME         :  jplot.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       jplot.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:26
*********************************************************************/

#ifdef   DEF
#define EXT
#else
#define EXT extern
#endif

#ifndef UJ_PLOT
/*
.....Are we plotting flag
*/
EXT UU_LOGICAL UJ_plotting;

#undef EXT
#define UJ_PLOT
#endif 
