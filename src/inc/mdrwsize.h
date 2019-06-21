/*********************************************************************
**    NAME         :  mdrwsize.h
**       CONTAINS: size and names of drawings
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       mdrwsize.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**      04/29/15 , 15:06:30
*********************************************************************/

#ifndef UM_DRWSIZE

#undef EXT
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif

/*
.....change max size type to 16
.....Yurong 7/31/97
*/
#define DS_7475 0 
#define DS_7580 16
#define DS_PS 32
#define DS_1043 48
#define DS_GEN 60

#ifdef UM_MPGM 

/* drawing and border sizes */

/*  model/m2global.c must be recompiled to make changes here effective.
	Increased UM_drawing_size from [12][4] to [48][4] to support different
	plotter types and as a means of determining from the drwsize value,
	which plotter a drawing was made for.  Drawing->drwsize values
	0-11 are plotter HP 7580, 12-23 are HP 7574, 24-35 are Calcomp 1043
	and 36-47 are Original MPE values.  The original values are here to be
	backward compatible with Unibases created prior to the implementation
	of PLOTTER MODEL support.
						   R. Herring 10-3-90
*/

/*  The values for the horizontal and vertical (X & Y) physical limits
    are modified in the nclu_create_drawing_form routine in NUDRW.C.
	 The modifications are made based on the drawing size and plotter
	 model selected by the user in the Create Drawing form.  These 
	 modifications are made by using the values supplied in environmental
	 variables set by the run script.  The values are used in the 
	 um_get_drawing_extent routine in M9EDRW.C (model directory).
						  M. Gump  8-9-90
*/

/* 
.....jplotsz.c must be recompiled to make changes here effective.
.....Yurong
*/

/*
.....added 4 user define type, default to biggest size
.....Yurong 7/30/97
*/
UU_REAL UM_drawing_size[80][4] = 
		{/* horizontal  vertical   horz marg  vert marg   all in CM */

      /* HP 7475 */
	   25.654,   19.558,   0.0,    0.0,      /* A horizontal */
	   19.558,   25.654,   0.0,    0.0,      /* A vertical */
	   39.878,   25.654,   0.0,    0.0,      /* B */
	   49.8348,  39.9288,   0.0,    0.0,      /* C */
	   80.8288,  52.7558,   0.0,    0.0,      /* D */
	   105.4608, 83.2358 ,   0.0,    0.0,     /* E */
	   101.600,  71.1708,   0.0,    0.0,      /* F */
	 113.4      ,  81.0      ,  0.0,    0.0,       /* A0 */
	  78.6      ,  56.4      ,  0.0,    0.00,       /* A1 */
	  54.0      ,  39.0      ,  0.0,    0.0,       /* A2 */
	  40.055    ,  27.432      , 0.0,    0.0,      /* A3 */
	  19.05      , 27.305      ,  0.0,    0.00,       /* A4 */
	  210.9216,   82.0928     ,  0.0,    0.0,       /* user define */
	  316.3824     ,82.0928   ,  0.0,    0.0,       /* user define */
	  421.8432,      82.0928,  0.0,    0.0,       /* user define */
	  527.304,     82.0928,  0.0,    0.0,       /* user define */

	
	/* HP 7580 */
			  24.8158,  16.129,   0.0,    0.0,              /* A horizontal */
			  16.129,   24.8158,   0.0,    0.0,             /* A vertical */
			  37.5158,  24.8158,   0.0,    0.0,             /* B */
			  49.8348,  39.9288,   0.0,    0.0,             /* C */
			  80.8288,  52.7558,   0.0,    0.0,             /* D */
			  105.4608, 82.0928 ,   0.0,    0.0,            /* E */
			  101.600,  71.1708,   0.0,    0.0,             /* F */
			113.4      ,  81.0      , 0.0 ,    0.0,       /* A0 */
			 78.6      ,  56.4      ,  0.0,    0.0,       /* A1 */
			 54.0      ,  39.0      ,  0.0,    0.0,       /* A2 */
			 40.055    ,  24.71      ,  0.0,    0.0,      /* A3 */
			 27.25,       17.01      ,  0.0,    0.0,       /* A4 */
	  210.9216,   82.0928     ,  0.0,    0.0,       /* user define */
	  316.3824     ,82.0928   ,  0.0,    0.0,       /* user define */
	  421.8432,      82.0928,  0.0,    0.0,       /* user define */
	  527.304,     82.0928,  0.0,    0.0,       /* user define */
      
		/* PS */   
/*   
.....marg unit are pixel in PS
.....Yurong changed on 12/10/96
*/                                                      
		26.90,  20.50,  62.0  ,  62.0,              /* A horizontal */
			20.50,  26.90,  62.0  ,  62.0,              /* A vertical */
	   38.938,   25.146,    62.0,    62.0,      /* B */
	   48.8948,  39.4208,   62.0,    62,      /* C */
	   79.8828,  52.2478,   62.0,    62,      /* D */
	   105.4608, 82.0928 ,   62.0,    62,     /* E */
	   101.600,  71.1708,   62,    62,      /* F */
	 113.4      ,  81.0      ,  62,    62,       /* A0 */
	  78.6      ,  56.4      ,  62.0,    62.0,       /* A1 */
	  54.0      ,  39.0      ,  62,    62.0,       /* A2 */
	  40.055    ,  27.432      ,  62,    62.0,      /* A3 */
	  19.05      , 27.305      ,  62,    62.0,       /* A4 */
	  210.9216,   82.0928     ,  62.0,    62.0,       /* user define */
	  316.3824     ,82.0928   ,  62.0,    62.0,       /* user define */
	  421.8432,      82.0928,  62.0,    62.0,       /* user define */
	  527.304,     82.0928,  62.0,    62.0,       /* user define */
			

		/* Calcomp 1043 */
			  24.1300,     16.5100,   0.0,    0.0,          /* A horizontal */
			  18.0340,     22.8600,   0.0,    0.0,          /* A vertical */
			 38.1000,      24.1300,   0.0,    0.0,          /* B */
			 50.8000,      39.3700,   0.0,    0.0,          /* C */
			 81.2800,      51.6890,   0.0,    0.0,          /* D */
			 106.0450,     82.5500,   0.0,    0.0,          /* E */
			 101.6000,     71.1708,   0.,    0.0,           /* F */
			113.4      ,  81.0      ,  0.0,    0.0,       /* A0 */
			 78.6      ,  56.4      ,  0.0,   .00,       /* A1 */
			 54.0      ,  39.0      ,  0.0,   .00,       /* A2 */
			 36.6      ,  26.7      ,  0.0,    0.0,       /* A3 */
			 24.3      ,  18.0      ,  0.0,    0.0,       /* A4 */
	  210.9216,   82.0928     ,  0.0,    0.0,       /* user define */
	  316.3824     ,82.0928   ,  0.0,    0.0,       /* user define */
	  421.8432,      82.0928,  0.0,    0.0,       /* user define */
	  527.304,     82.0928,  0.0,    0.0,       /* user define */

		/* GENERIC CASE - ORIGINAL MPE VALUES */
	   25.654,   19.558,   0.0,    0.0,      /* A horizontal */
	   19.558,   25.654,   0.0,    0.0,      /* A vertical */
	   39.878,   25.654,   0.0,    0.0,      /* B */
	   49.8348,  39.9288,   0.0,    0.0,      /* C */
	   80.8288,  52.7558,   0.0,    0.0,      /* D */
	   105.4608, 83.2358 ,   0.0,    0.0,     /* E */
	   101.600,  71.1708,   0.0,    0.0,      /* F */
	 113.4      ,  81.0      , 0.0 ,   0.0 ,       /* A0 */
	  78.6      ,  56.4      ,  0.0,    0.0,       /* A1 */
	  54.0      ,  39.0      ,  0.0,    0.0,       /* A2 */
	  40.055    ,  27.432      ,  0.0,    0.0,      /* A3 */
	  19.05      , 27.305      ,  0.0,    0.0,       /* A4 */
	  210.9216,   82.0928     ,  0.0,    0.0,       /* user define */
	  316.3824     ,82.0928   ,  0.0,    0.0,       /* user define */
	  421.8432,      82.0928,  0.0,    0.0,       /* user define */
	  527.304,     82.0928,  0.0,    0.0,       /* user define */

		};

/* names for various drawing sizes */
char *UM_drawing_size_name[]  = {
		"AH", "AV", "B", "C", "D", "E", "F", "A0", "A1", "A2", "A3", 
		"A4" , "USER1", "USER2", "USER3", "USER4"};

/* names for supported plotter types */
/*
.....changed by Yurong
.....8/19/97
*/
char *UM_drawing_plotter_name[]  = {
		"HP7475", "HP7580", "PostScript", "Calcomp1043", "GENERIC"};
/*    "HP7580", "HP7475", "PostScript", "Calcomp1043", "GENERIC"}; */
/* map mcredraw.frm drawing size order to internal number */
int UM_form_to_drwsize[16] =
		{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};

#else
EXT UU_REAL UM_drawing_size[80][4];
EXT char *UM_drawing_size_name[];
EXT char *UM_drawing_plotter_name[];
EXT int UM_form_to_drwsize[16];
#endif

#define UM_DRWSIZE
#endif
