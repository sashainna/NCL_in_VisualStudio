/*********************************************************************
**
**    NAME         :  dinput.h
**
**       CONTAINS:
**       	Global Data for input mechanism
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**			dinput.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:06:13
**
*********************************************************************/

#ifndef DINPUTH

#include "usysdef.h"
#include "gi1.h"
#include "gipick.h"
#include "gloc3.h"
#include "dtypes.h"
#include "udebug.h"

#ifdef DPGM
#define EXT    
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif

/***** String Event Termination Conditions *****/

#define UD_CRTERM		0		/* carriage return termination */
#define UD_PARTERM	1		/* partial string condition (something on event Q) */

/***** This section defines an input EVENT after conversion to 
			UU_REAL ******/

/* UD_DPICKS - PICK data for PHIGS -- has array of segments */

	typedef struct
	{
		Gpickid *pickpath;	/* pointer to array of segments and pickid at end  */
		Gstatus status;		/* pick status */
		Gint depth;				/* depth of pick path */
		Gint transform;		/* normalization transformation number */
		UU_REAL position[2];	/* locator position */
		Gint choice;			/* trigger used to invoke this pick */
	} UD_DPICKS;

/* UD_DLOC  -  locator data */

	typedef struct
	{
		UU_REAL position[2];	/* locator position */
		Gint transform;		/* normalization transformation number */
		Gint choice;			/* trigger used to invoke this pick */
	} UD_DLOC;


	typedef struct
	{
		Gnpoint *points;		/* points in stroke */
		Gint transform;		/* normalization transformation number */
		Gint	n_points;		/* locator position */
		Gint stktrig;			/* trigger used to invoke this pick */
	} UD_DSTROKE;

	typedef struct
	{		
		int evclass;		/* Event class. one of UD_NONE, UD_LOCATOR, UD_STROKE, 
								UD_VALUATOR, D_CHOICE, UD_PICK, UD_STRING, UD_VECTOR */
		int evdev;			/* Device number this input came from */
	
		union 
		{	
			Gint choicedata;			/* choice data if class=CHOICE */
			UD_DLOC locdata;			/* locator data if class=LOC */
			UD_DPICKS pickdata;		/* pick data if class=PICK */
			Gchar *stringdata;		/* string data if class=STRING */
			UU_REAL valdata;			/* valuator data if class=VALUATOR */
			UD_DSTROKE strokedata;	/* stroke data if class=STROKE */
			UU_REAL vecdata[3];		/* vector data from features */
		} indata;
	} UD_DEVENT;

/*	--------- This section defines the event record before it is converted
					to UU_REAL format ---------- */

/* UD_GKSPICKS - PICK data for PHIGS -- has array of segments */

	typedef struct
	{
		Gpickid *pickpath;	/* pointer to array of segments and pickid at end  */
		Gstatus status;		/* pick status */
		Gint depth;				/* depth of pick path */
		Gint transform;		/* normalization transformation number */
		Gwpoint	position;	/* locator position */
		Gint choice;			/* trigger used to invoke this pick */
	} UD_GKSPICKS;

/* --  locator event data structure -- */

	typedef struct
	{
		Gwpoint	position;	/* locator position */
		Gint transform;		/* normalization transformation number */
		Gint choice;			/* trigger used to invoke this pick */
	} UD_GKSLOC;

/* --  stroke event data structure -- */

	typedef struct
	{
		Gwpoint *points;		/* points in stroke */
		Gint transform;		/* normalization transformation number */
		Gint	n_points;		/* locator position */
		Gint stktrig;			/* trigger used to invoke this pick */
	} UD_GKSSTROKE;

	typedef struct
	{		
		int evclass;		/* Event class. one of UD_NONE, UD_LOCATOR, UD_STROKE, 
								UD_VALUATOR, D_CHOICE, UD_PICK, UD_STRING, UD_VECTOR */
		int evdev;			/* Device number this input came from */
	
		union 
		{	
			Gint choicedata;				/* choice data if class=CHOICE */
			UD_GKSLOC locdata;			/* locator data if class=LOC */
			UD_GKSPICKS pickdata;		/* pick data if class=PICK */
			Gchar *stringdata;			/* string data if class=STRING */
			Gfloat valdata;				/* valuator data if class=VALUATOR */
			UD_GKSSTROKE strokedata;	/* stroke data if class=STROKE */
			Gwpoint3 vecdata;				/* vector data from features */
		} indata;
	} UD_GKSEVENT;

	typedef enum
	{
		RECORD,
		PLAYBACK,
		RPOFF,
		RECSUSP,
		PLAYSUSP,
		RECSUSPAUTO
	} UD_RPFLTYPE;

	typedef struct
	{
		UD_RPFLTYPE flag;		/* R/P state flag */
		int rplun;				/* playback LUN */
	} UD_RPSTATE;

/*	-- input pipe input data definition and return structure */

	typedef struct
	{
		int strbfsz;			/* size of string input buffer */
		char *defstr;			/* pointer to default string */
		int termcon;			/* termination condition */
		Gloc3 rubloc;			/* WCS rubber band start location */
		int evtype;				/* event type */
		char *prompt;			/* prompt string */
		int number;				/* number of prompt strings (if choice ) */
		int device;				/* device to input from */
		int pet;					/* prompt and echo type */
		UU_LOGICAL atoveride; /* overide auto test flag */
	} UD_EVENTDEF;

/* -- input related common -- */

#ifdef DPGM
	Gfloat UD_Timeout = 300.;					/* timeout constant for await event */
#else
	EXT Gfloat UD_Timeout;						/* timeout constant for await event */
#endif

/******** -- Record and Playback Section -- ********/

#define UD_MAXPLAYB 	5		/* maximum number of nested playbacks */
#define UD_NUMPROC 10		/* number of lines procedural interface buffers */
#define UD_RPREC_LEN 200	/* RP record maximum length */

#ifdef DPGM
	UU_LOGICAL UD_firsttime = UU_TRUE;	/* first time flag for first playback */
	UU_LOGICAL UD_autotest = UU_FALSE;	/* auto-test mode flag */
	UD_RPSTATE UD_Rpstate[UD_MAXPLAYB] = {	/* save R/P state flag */
									RPOFF, 0,
									RPOFF, 0,
									RPOFF, 0,
									RPOFF, 0,
									RPOFF, 0
															};
	int UD_Rpstate_ptr = 0;							/* R/P state array pointer */
	int UD_prc_buf_ptr = 0;							/* procedure RP buffer ptr */
	char UD_prc_buf[UD_NUMPROC][UD_RPREC_LEN];	/* procedure RP buffer */
#else
	EXT UU_LOGICAL UD_firsttime;		/* first time flag for first playback */
	EXT UU_LOGICAL UD_autotest;		/* auto-test mode flag */
	EXT UD_RPSTATE UD_Rpstate[UD_MAXPLAYB];	/* save R/P state flag */
	EXT int UD_Rpstate_ptr;							/* R/P state array pointer */
	EXT int UD_prc_buf_ptr;
	EXT char UD_prc_buf[UD_NUMPROC][UD_RPREC_LEN];
#endif

/******** -- GKS section -- ********/

	EXT UU_REAL UD_aspect;				/* aspect ratio of graphic display */
	EXT int UD_rasteru;					/* number of rasters in u direction */
	EXT int UD_rasterv;					/* number of rasters in v direction */
	EXT UU_REAL UD_sizeu;				/* number of cm's in u direction */
	EXT UU_REAL UD_sizev;				/* number of cm's in v direction */
	EXT UU_REAL UD_rastprecm;			/* number of rasters per cm */

	EXT UU_LOGICAL UD_Eventm;			/* event mode on flag */

#undef EXT
#define DINPUTH

#endif

