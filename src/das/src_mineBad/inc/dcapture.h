/*********************************************************************
**    NAME         :  dcapture.h
**       CONTAINS:  Global Variables for DAS Capture Function. 
**    COPYRIGHT 1989 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       dcapture.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:13
*********************************************************************/

#ifndef UD_CAPTURE

struct UD_capture_def	{
	UU_LOGICAL state;       /* state of capture -- on/off */
	int cplun;			    	/* reference to open file table */
	};

/*************************************************************************
*
*	define external variables
*
*************************************************************************/

#ifdef  DPGM
#define EXT
   UU_LOGICAL UD_initialize_capture = UU_FALSE;
#else
#define EXT extern
   extern UU_LOGICAL UD_initialize_capture;
#endif

EXT struct  UD_capture_def	UD_capture;

#undef EXT

#define UD_CAPTURE
#endif
