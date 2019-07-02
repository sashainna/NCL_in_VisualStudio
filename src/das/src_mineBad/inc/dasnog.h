/*********************************************************************
**    NAME         :  dasnog.h
**       CONTAINS:
**       	DAS common that has no gks constructs
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       dasnog.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:12
*********************************************************************/
/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                             !!!!!!
!!!!!!   NOTE:                                                     !!!!!!
!!!!!!      Recompile routine D1INITD.C (in .das directory)        !!!!!!
!!!!!!      to cause any changes made in this include file to be   !!!!!!
!!!!!!      reflected in the executable.                           !!!!!!
!!!!!!                                                             !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

#ifndef DASNOGKSH
#include "usysdef.h"
#include "alloc.h"
#include "derror.h"
#include "drubber.h"
#include "dtypes.h"

#ifdef DPGM
#define EXT    
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif

/* -- define section -- */

#define UD_CDONE     1				/* done with input sequence */
#define UD_CRLAST    2				/* reject last input */
#define UD_CRCOMMAND 3				/* reject last command */
#define UD_CRESTART  4				/* restart this input sequence */
#define UD_CHELP     5				/* display help stuff */
#define UD_CDEFAULT  6				/* use default for this input */
#define UD_CREJCHAR  7				/* reject last character */
#define UD_CALTACT   8				/* application alternate action */
#define UD_CALTACT1  9				/* subsystem alternate action */
#define UD_CRETROOT  10				/* return to root menu of top menu */
#define UD_CPANIC    11				/* panic stop */
#define UD_CUPFIELD  12				/* up field (forms navigation) */
#define UD_CDNFIELD  13				/* down field (forms navigation) */
#define UD_CCLFIELD  14				/* clear field (forms navigation) */
#define UD_CTGFIELD  15				/* toggle field (forms navigation) */
#define UD_CEXITSUB  16				/* exit subsystem (menu navigation) */

#define UD_NMENT 256					/* number of entities */
#define UD_NMBT_WD 32				/* number of bits per word */
#define UD_NMENTWD UD_NMENT/UD_NMBT_WD+1	/* number of wds in entity bit map */

#define UD_AUXMENU 5					/* auxiliary choice device */

#define UD_MAXPICKDEPTH 5			/* maximum pickpath depth */
#define UD_MAXENVDEPTH 3*STACK_MAX	/* maximum depth of nested environments */

#define UD_DMENU 0					/* in menu mode */
#define UD_COMMAND 1					/* in menu mode */
#define UD_DEFAULT UU_TRUE			/* default */
#define UD_NODEFAULT UU_FALSE		/* no default */
#define UD_PROMPTMSG UU_TRUE		/* prompt message */
#define UD_ERRORMSG UU_FALSE		/* error message */

/*	-- DAS help numbers -- */

#define UD_DPMTCART   18				/* cartesian coordinate input request */
#define UD_DPMTSELECT 19				/* select subsystem input request */
#define UD_DPMTVAL    20				/* distance value input request */
#define UD_DPMTDISTANCE 21				/* distance value input request */
#define UD_DPMTINT    22 				/* integer input request */
#define UD_DPMTVEC    23				/* vector input request */
#define UD_DPMTPICK   24				/* pick input request */
#define UD_DPMTPCKLOC 25				/* pick location input request */
#define UD_DPMTSTRING 26				/* string input request */
#define UD_DPMTCHOICE 27				/* choice input request */
#define UD_DPMTNDC    28				/* cartesian ndc coordinate input request */
#define UD_DPMTANGLE	 29				/* angle input */
#define UD_DPMTUNITLESS	30 			/* unitless real number input */

/*	-- max and min valuator values (default) -- */

#define UD_MINVALUATOR (UU_REAL) -360.0
#define UD_MAXVALUATOR (UU_REAL) 360.0

/*	-- menu device number start -- */

#define UD_APPLY_MAX_MENU UU_FALSE	/* if UU_TRUE, apply modulo algorithm when
													allocating choice device numbers */
#define UD_SUBMDEV 			20		/* start of request mode choice devices */
#define UD_POP_CHC_DEV_ST 	21		/* start choice dev number for popup menus */
#define UD_MENU_CHC_DEV_ST 120		/* start choice dev number for normal menus */
#define UD_MENU_MAX_NODE	14		/* max number of nodes per menu */

/*	-- window section -- */

#define UD_MAX_NUM_WIN 5			/* maximum number of active windows */

#define UD_WINDOWOFF  0
#define UD_WINDOWON   1
#define UD_WINDOWSUSP 2

#define UD_FREE 		 0				/* resource free */
#define UD_BUSY 		 1				/* resource busy */
#define UD_WINDOWERR -1				/* error creating window */

#define UD_NORETURN 0				/* system will handle busy situation */
#define UD_RETURN 1					/* caller will handle busy situation */

#define UD_NORESERVE 0				/* don't reserve window */
#define UD_RESERVE 1					/* reserve window */

/*	-- typedef section -- */

	typedef enum
	{
		DE_TRUE,
		DE_DONE,
		DE_RLAST,
		DE_RCOMMAND,
		DE_RESTART,
		DE_ALTACT,
		DE_ALTACT1,
		DE_DEFAULT,
		DE_AGAIN,
		DE_UPFIELD,
		DE_DNFIELD
	} UD_DASTAT;

	typedef enum
	{
		UD_ICONIF,
		UD_MENUIF,
		UD_CMDIF
	} UD_UISTATE;

	typedef struct
	{
		int depth;
		int pickpath[UD_MAXPICKDEPTH];
	} UD_PPICKREC;

	typedef struct
	{
		UU_REAL cord[3];
/*
.....added to save the cord input string if have any
*/
		char label[80];
	} UD_DASCORD;

	typedef struct
	{
		UU_REAL value;
/*
.....save the data input string if have any
*/
		char label[80];
	} UD_SCA_VALUE;
	
	typedef struct
	{
		int value;
/*
.....save the data input string if have any
*/
		char label[80];
	} UD_SCA_IVAL;

	typedef struct
	{
		UU_REAL org[3];				/* origin point */
		UU_REAL normal_vector[3];	/* normal vector */
		int transform;					/* normalization transform where vector is */
	} UD_PLANE_REC;

	typedef struct
	{
		UU_REAL cord[3];
		int transform;
		int choice;
		UU_REAL wndc3_mat[4][4];
		UU_REAL ndcw3_mat[4][4];
		char label[80];		/*** Added by Paul for "text" input. 07/15/92 ***/
	} UD_NDCLOCREC;

	typedef struct
	{
		UD_PPICKREC ppath;
		UD_NDCLOCREC pndc;
	} UD_PLOCREC;

	typedef struct
	{
		char *instring;			/* input string buffer */
		UD_DASTAT termcon;		/* string termination condition */
	} UD_STRREC;

	typedef union
	{
		int dint;
		UU_REAL dreal;
		UD_DASCORD cord;
		UD_PLANE_REC plane;
		UD_PPICKREC dpick;
		UD_PLOCREC dploc;
		UD_NDCLOCREC ndcloc;
		UD_STRREC strrec;
		char *dstr;
	} UD_DASIN;

	typedef struct
	{

/*		-- limit flags -- */

		UU_LOGICAL lsel;					/* limit geometry flag */
		UU_LOGICAL lpran;					/* limit parameter range */
		UU_LOGICAL lsint;					/* limit specific interaction */
		UU_LOGICAL lnumin;				/* limit number of inputs */
		UU_LOGICAL editable;				/* limit to editable entities */
	
/*	-- limit data -- */
	
		int lselbf[UD_NMENTWD];			/* geometry limit bit array */
		UU_REAL lpranlo;					/* low real value */
		UU_REAL lpranhi;					/* high real value */
		int fsint;							/* specific interaction */
		int fsdev;							/* specific device */
		int fsech;							/* specific echo type */
		int fntlo;							/* lower bound - number of interactions */
		int fnthi;							/* upper bound - number of interactions */
/*
........New: user defined local limit flag and bit array
.......-RAZ
*/
		int llsel;                  /* local limit geometry flag */
		int llselbf[UD_NMENTWD];    /* local geometry limit bit array */
		char llsel_name[24];        /* Name of local limit geo (for error msg) */
	} UD_LIMREC;

/*
.....Added to check if waiting for user Pick event
.....Yurong
*/
	EXT int UD_pickmode;

/*	-- limit section -- */

	EXT int UD_limbfptr;								/* DAS limit buffer pointer */
	EXT UD_LIMREC UD_limbf[UD_MAXENVDEPTH];	/* DAS limit buffer */
#define UD_LIMIT UD_limbf[UD_limbfptr]
#ifdef DPGM
	EXT UU_LOGICAL UD_dastkfl = UU_FALSE;		/* DAS init flag */
#else
	EXT UU_LOGICAL UD_dastkfl;						/* DAS init flag */
#endif

/* --  other data --  */

#ifdef DPGM
	UU_LOGICAL UD_commode = UD_DMENU;			/* menu or command mode flag */
	UU_REAL UD_lpnti[3] = {0., 0., 0.};		/* last point input */
	UU_REAL UD_lveci[3] = {0., 0., 0.};		/* last vector input */
	int UD_start_menu_num = UD_SUBMDEV;		/* start menu number */
	int UD_st_popup_num = UD_POP_CHC_DEV_ST;	/* start choice dev number 
																	for popup menus */
	int UD_st_menu_num = UD_MENU_CHC_DEV_ST;	/* start choice dev number 
																	for normal menus */
	UU_LOGICAL UD_menu_mod = UD_APPLY_MAX_MENU; /* apply modulo algorithm flag */
	int UD_max_node = UD_MENU_MAX_NODE;		/* max number of nodes per menu */
#else
	EXT UU_LOGICAL UD_commode;							/* menu or command mode flag */
	EXT UU_REAL UD_lpnti[3];							/* last point input */
	EXT UU_REAL UD_lveci[3];							/* last vector input */
	EXT int UD_start_menu_num;							/* start menu number */
	EXT int UD_st_popup_num;	/* start choice dev number for popup menus */
	EXT int UD_st_menu_num;		/* start choice dev number for normal menus */
	EXT UU_LOGICAL UD_menu_mod; /* apply modulo algorithm flag */
	EXT int UD_max_node;		/* max number of nodes per menu */
#endif

/*	-- default devices and echotypes -- */

#ifdef DPGM
	EXT int UD_chcint = UD_CHOICE;	/* current choice interaction */
	EXT int UD_chcdev = UD_SUBMDEV;	/* current choice device */
	/* default choice echotype = 22 = text menus with numbers displayed, 
		and allowing keyboard and mouse input */
	EXT int UD_chcech = 22;				

	EXT int UD_valint = UD_STRING;	/* current valuator interaction */
	EXT int UD_valdev = 1;				/* current valuator device number */
	EXT int UD_valech = 1;				/* current valuator echotype */

	EXT int UD_locint = UD_LOCATOR;	/* current locator interaction */
	EXT int UD_locdev = 1;				/* current locator device number */
	EXT int UD_locech = 1;				/* current locator echotype */

	EXT int UD_pckint = UD_PICK;		/* current pick interaction */
	EXT int UD_pckdev = 1;				/* current pick device number */
	EXT int UD_pckech = 1;				/* current pick echotype */
	EXT int UD_pckstr = UU_FALSE;		/* Allow string input without checking
												   for valid entity during pick mode */

	EXT int UD_strint = UD_STRING;	/* current string interaction */
	EXT int UD_strdev = 1;				/* current string device number */
	EXT int UD_strech = 1;				/* current string echotype */

	EXT int UD_vecint = UD_STRING;	/* current vector interaction */
	EXT int UD_vecdev = 1;				/* current vector device number */
	EXT int UD_vecech = 1;				/* current vector echotype */

	EXT int UD_stkint = UD_STROKE;	/* current stroke interaction */
	EXT int UD_stkdev = 1;				/* current stroke device number */
	EXT int UD_stkech = 1;				/* current stroke echotype */
#else
	EXT int UD_chcint;					/* current choice interaction */
	EXT int UD_chcdev;					/* current choice device */
	EXT int UD_chcech;					/* current choice echotype */

	EXT int UD_valint;					/* current valuator interaction */
	EXT int UD_valdev;					/* current valuator device number */
	EXT int UD_valech;					/* current valuator echotype */

	EXT int UD_locint;					/* current locator interaction */
	EXT int UD_locdev;					/* current locator device number */
	EXT int UD_locech;					/* current locator echotype */

	EXT int UD_pckint;					/* current pick interaction */
	EXT int UD_pckdev;					/* current pick device number */
	EXT int UD_pckech;					/* current pick echotype */
	EXT int UD_pckstr;					/* Allow string input without checking
												   for valid entity during pick mode */

	EXT int UD_strint;					/* current string interaction */
	EXT int UD_strdev;					/* current string device number */
	EXT int UD_strech;					/* current string echotype */

	EXT int UD_vecint;					/* current vector interaction */
	EXT int UD_vecdev;					/* current vector device number */
	EXT int UD_vecech;					/* current vector echotype */

	EXT int UD_stkint;					/* current stroke interaction */
	EXT int UD_stkdev;					/* current stroke device number */
	EXT int UD_stkech;					/* current stroke echotype */

#endif

/*	-- syntactic prompt save area -- */

	EXT char UD_synwc[30];			/* model coordinate */
	EXT char UD_synsel[30];			/* selection */
	EXT char UD_syndis[30];			/* distance */
	EXT char UD_synint[30];			/* integer */
	EXT char UD_synvec[30];			/* vector */
	EXT char UD_synplc[30];			/* pick by location */
	EXT char UD_synstr[30];			/* text string */
	EXT char UD_synchc[30];			/* choice */
	EXT char UD_synndc[30];			/* ndc coordinate */
	EXT char UD_synang[30];			/* angle */
	EXT char UD_synul[30];			/* unitless */
	EXT char UD_synstk[30];			/* stroke */

/*	-- lexical prompt save area -- */

	EXT char UD_chcpmt[40];				/* default choice prompt */
	EXT char UD_valpmt[40];				/* default valuator prompt */
	EXT char UD_locpmt[40];				/* default locator prompt */
	EXT char UD_pckpmt[40];				/* default pick prompt */
	EXT char UD_strpmt[40];				/* default string prompt */
	EXT char UD_stkpmt[40];				/* default vector prompt */

#ifdef DPGM
	EXT char *UD_chcpmtptr = UD_chcpmt;			/* pointer to choice prompt */
	UU_LOGICAL UD_promptsegflag = UU_FALSE;	/* GKS segment exists flag */
	UU_LOGICAL UD_errorsegflag = UU_FALSE; 	/* GKS segment exists flag */
#else
	EXT char *UD_chcpmtptr;							/* pointer to choice prompt */
	EXT UU_LOGICAL UD_promptsegflag;				/* GKS segment exists flag */
	EXT UU_LOGICAL UD_errorsegflag; 				/* GKS segment exists flag */
#endif

/*	-- Windowing Stuff -- */

#ifdef DPGM
	int UD_windev = 2;							/* window device number */
#else
	extern int UD_windev;						/* window device number */
#endif

	EXT int UD_winrow;							/*  number of rows in window */
	EXT int UD_wincol;							/*  number of columns in window */

/******** -- Icon Menu Section -- ********/

#ifdef DPGM
	char UD_iconfname[40] = "";		/* save icon name */
	UU_LOGICAL UD_iconrflg;				/* retain icon segment flag */
#else
	EXT char UD_iconfname[40];
	EXT UU_LOGICAL UD_iconrflg;		/* retain icon segment flag */
#endif

/********* -- User Interface Mode Section -- ***********/

#ifdef DPGM
	EXT UD_UISTATE UD_uimodefl = UD_MENUIF;	/* in menu input mode */
#else
	EXT UD_UISTATE UD_uimodefl;					/* in menu input mode */
#endif

/********* -- Input Pipe Section -- ***********/

#ifdef DPGM
	UU_LOGICAL UD_enabless = UU_TRUE;		/* enable subsystem flag */
	UU_LOGICAL UD_enableim = UU_TRUE;		/* enable input mapping flag */
#else
	EXT UU_LOGICAL UD_enabless;				/* enable subsystem flag */
	EXT UU_LOGICAL UD_enableim;				/* enable input mapping flag */
#endif

#undef EXT
#define DASNOGKSH

#endif
