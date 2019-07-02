
/*********************************************************************
**    NAME         :  tplot.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       tplot.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:57
*********************************************************************/
/*
......added for C++
......Yurong 1/20/00
*/
#include "xenv1.h"
#ifdef	DEF
#define EXT
#else
#ifdef __cplusplus
#define EXT extern "C"  
#else
#define EXT extern
#endif
#endif

#ifndef	TP_PLOT

/*
.....Added new command line arguments
.....Bobby  -  3/2/92
*/
#define BATCH 0x1
/*#define HELP 0x2*/
#define INTER 0x4
#define LIST 0x8

EXT UU_LOGICAL	P1P2_control;
EXT UU_LOGICAL	TP_interactive;
EXT UX_pathname DISK_fnm,TEMP_fnm;
/* added kathy */ 
EXT struct { struct{int      x,y;} ll, ur; } plot_bound;
EXT UU_LOGICAL DISK_file,TEMP_file;
EXT UU_REAL	PAPER_size[2];
EXT int PORT_id;
/*
.....Added to let the workstation drivers
.....know which size paper was being used
.....Especially for "A VERT"
.....Also added viewport variables
.....Bobby  -  7/16/91
*/
EXT char TP_size[10];
EXT UU_REAL DRAW_size[2],PLOT_size[2];
EXT int TP_nviews,TP_border[10];
EXT struct {Gnpoint llf;Gnpoint urb;} BORDER_pt[10];

/*
.....Changed to reflect new
.....command line arguments
.....Bobby  -  3/2/92
*/
typedef struct	argu
	{
		unsigned flag;
		int size;
		int bypass;
		int print;
		double linewt;
		UX_pathname diskfnm;
		UX_pathname port;
		char type[8];
		UX_pathname printque;
	}	TP_OPTIONS;

EXT TP_OPTIONS plotopts;
/*
.....added by Yurong
.....removed these variables
.....from serial files 8/25/97
*/
EXT int wsplotdx, wsplotdy;
EXT int  pts;
/*
.....added for MOTIF style interface
.....Yurong 7/31/98
*/
EXT int Motif_Plot;
/*
.....added for WNT window style interface
.....Yurong 1/20/00
*/
EXT int WNT_Plot;
#undef EXT

#define	TP_PLOT
#endif
