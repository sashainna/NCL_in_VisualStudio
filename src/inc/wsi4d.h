/********************************************************************* 
**  NAME:  wsi4d.h 
**
**      Iris 4D workstation include file.
**
**  COPYRIGHT  1987  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL 
**       wsi4d.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:11
**
*********************************************************************/

#ifndef WSI4DH

/*
.....Added for RS6000 support
.....Bobby  -  1/14/92
*/
#ifndef UU_RS6000
#include <gl.h> 
#else
#include <gl/glport.h>
#endif

#include <device.h>
#include "gobas.h"
#include "gtbldef.h"

#ifdef UW_IRIS4DMAIN
#define EXT
#else
#define EXT extern
#endif

#define NOTRANS 15				/* number of xforms used 								*/

/************************* iris specific typedefs **************************/
typedef float Ifloat;

typedef struct {

		Gint wid;					/* workstation id of this workstation */

		struct {
			Gfloat sf;
			Gfloat dx,dy;
		} wsxform;					/* sf and dx,dy to map ws window to raster*/

		struct {
			Gfloat sfx;
			Gfloat sfy;
			Gfloat sfz;
			Gfloat dx,dy,dz;
		} xform;						/* sf, dx,dy to map current window to viewport */

		struct {
			int x;					/* current x position of alpha text		*/
			int y;					/* current y position of alpha text		*/
			int stx;					/* beginning x position of alpha text	*/
			int sty;					/* beginning y position of alpha text	*/
		} atext;

		int openseg;					/* number of open segment, or -1			*/
		int pick;						/* True, we're in pick mode				*/
		int icon;						/* True, we're creating icons 			*/
		int iclen;						/* Length of icon definition				*/
		int *icstr;						/* Points to beginning of icon def.		*/
		int scrollup;					/* no. scroll text areas that are up.  */
		int objopn;						/* iris open object number, or -1 		*/
		int pkident;					/* cnt of pik ids 							*/
		int redraw_flag;				/* 1 = need to redraw. 0=don't 			*/
		int erase;						/* 1 = erasing a segment, 0=not 			*/
		int iconflag;					/* 1 = drawing an icon. 0=not				*/
		int dbbfr;						/* double buffer flag						*/
		int rasmode;					/* True, identity matrix is on stack	*/
		int cur_surf;					/* current surface (POPUP or GRAPHIC)	*/
		int sav_surf;					/* previous current surface				*/
		Colorindex allmask;			/* write mask for all bit planes			*/
		Colorindex popmask;			/* write mask for POPUP surface			*/
		Colorindex graphmask;		/* write mask for GRAPHIC surface		*/
		int maxgcolors;				/* maximum # of GRAPHIC colors			*/
		int maxpcolors;				/* maximum # of POPUP colors				*/
		int ngraphplanes;				/* number of GRAPHIC planes allocated	*/
		int npopupplanes;				/* number of POPUP planes allocated		*/
		Matrix modxf;					/* current modeling xform 					*/
		Matrix ntran[NOTRANS];		/* Normtran matricies						*/

} UG_4ddat;

typedef enum{							/* this defines types of objects that	*/
			MENUTYP,						/* routine uw_4dmakobj is willing to	*/	
			TRANTYP,						/* make an object no for 					*/
			PRMTTYP,
			LOCTYP,
			OTHERTYP
		} UG_objtyp;


/********************** device specific defines ***************************/	
/* device coordinate space is from (0,0) to (DEVXMAX,DEVYMAX) */
/*
.....Changed from hard coded numbers to
.....Global variables which are set in uw_4dinit
.....For Indigo support
.....Bobby  -  9/11/91
*/
/*#define DEVXMAX 1279				/* maximum x coordinate */
/*#define DEVYMAX 1023				/* device maximum y coordinate */
EXT int DEVXMAX;
EXT int DEVYMAX;
EXT int DEVYUSE;

/* screen will hold ROWMAX rows of COLMAX  characters each, when using
	the standard size alpha characters */
/*
.....Changed from hard coded numbers to
.....Global variables which are set in uw_4dinit
.....for Indigo support
.....Bobby  -  10/4/91
*/
/*#define ROWMAX  64				/* max alpha rows @16 pixels baseline dist.		*/

/*#define COLMAX  142				/* max alpha columns*/

/*#define STRWDTH 9					/* width of one character in pixels					*/
/*#define XTPMIN  136				/* current default textport boundaries 			*/
/*#define XTPMAX  872
/*#define YTPMIN  68
/*#define YTPMAX  690*/
EXT int ROWMAX,COLMAX,STRWDTH,XTPMIN,XTPMAX,YTPMIN,YTPMAX,STRHGT;
EXT UU_REAL STRNDCW,STRNDCHW,STRNDCH,STRNDCHH;

/********************** magic numbers *****************************************/

#define NUMPETS 6					/* Number of available prompt-and-echo types		*/
#define NCHDEV 500				/* number of choice devices							*/
#define MAXCHOICES 50			/* max number of choices for any choice device	*/
#define MENUPRMPTSIZE 20		/* Max size of menu prompt								*/
#define MAXPRMPT 50				/* max no of prompts up at one time 				*/
#define MAXSSTK 10				/* max no of scrollareas on scroll stack 			*/
#define MATSIZ 4					/* coord matrix size 									*/
#define WS_ERR -1					/* error message 											*/
#define GRAPHIC 1					/* graphic surface flag									*/
#define POPUP 2					/* popup surface flag									*/
#define CLEAR -1					/* color to "clear" popup surfaces					*/
#define TAG_VPT 0					/* xform tag for setting viewport					*/
#define TAG_TRN 2					/* xform tag for view reference point				*/
#define TAG_VPN 3					/* xform tag for viewpoint normal					*/
#define TAG_VUP 4					/* xform tag for view up vector						*/
#define TAG_W2V 5					/* xform tag for window to viewport map			*/
#define TAG_KXF 6					/* xform tag for "K" matrix							*/
#define TAG_S1  7					/* xform tag for first scale							*/
#define TAG_T2  8					/* xform tag for second translation					*/
#define TAG_COLOR 1				/* Tag for first color command in object			*/
#define MAX_COLOR_TAGS 100		/* Highest color tag in an object					*/
#define TAG_MOD 101				/* Modeling Xform tag (TAG_COLOR+MAX_COLOR_TAGS */
#define MAXUSRCURSORS 10		/* Number of definable user-cursors					*/


/*********************  Codes for device dependent icons **********************/

#define UW_4D_COLORREP 1
#define UW_4D_SETPOPCLR 2
#define UW_4D_RASLINE 3 
#define UW_4D_FILLRAS 4
#define UW_4D_RASTEXT 5
#define UW_4D_PICKID 6
#define UW_4D_CELLRAS 7

/*********************  use of object numbers *********************************/

#define MASTEROBJ  0L					/* object to contain menus, prompts	*/

#define NO_MASTER 2						/* # of master objects */
#define NO_TRANS NOTRANS				/* # of transformation object */
#define NO_PRMPTS MAXPRMPT				/* # of prompts up at once (1 obj/prmt) */
#define NO_MENU NCHDEV					/* Max # of menus */
#define NO_LOCATORS MAXUSRCURSORS	/* # of locators we need objects for */

#define GTRANMIN (NO_MASTER+NO_MENU)	/* Lo object # for xform objects */

/***************** handy macros ***************************/
#define MAX(a,b) ((a>b)?a:b)		/* handy macros for clipping */
#define MIN(a,b) ((a<b)?a:b)

/******************* color redefinitions *********************************/
#undef BLACK						/* redefine color constants to GKS/UNICAD std */
/* NCL: set to be same as background - rah */
#define BLACK 0					/* these support the "remap" function call */
#undef RED							/* defined in wsi4d.c */
#define RED 6
#undef GREEN
#define GREEN 3
#undef YELLOW
#define YELLOW 7
#undef BLUE
#define BLUE 4
#undef MAGENTA
#define MAGENTA 8
#undef CYAN
#define CYAN 5
#undef WHITE
#define WHITE 1
#define BKRND 0					/* the backround color is black in remap() */

/******************** MULTIPLE ARCHs ****************************************/

/* RAH: In support of multiple architectures ... */
#define UW_4D20    0   /* for Personal Iris, and 4D25GT */
#define UW_4DGT    1   /* for 4D80 and 4D80GT */
#define UW_4DNOGT  2	/* for 4D70 ... */
/*
.....Added Indigo Support
.....Bobby  -  9/11/91
*/
#define UW_4DIGO   3   /* for Indigo */
/******************** useful constants ****************************************/
#define PI		(3.1415926454)
#define DTOR	(PI/180.0)
#define RTOD	(180.0/PI)

/****************** more macros ***********************************************/

#define uw_4dpopsurf()			uu_denter2(UU_GTRC,(us,"4dpopsurf"));		\
											uw_4d.sav_surf = uw_4d.cur_surf;			\
											writemask(uw_4d.popmask);					\
											uw_4d.cur_surf = POPUP;						\
										uu_dexit

#define uw_4dgraphsurf()		uu_denter2(UU_GTRC,(us,"4dgraphsurf"));		\
											uw_4d.sav_surf = uw_4d.cur_surf;				\
											writemask(uw_4d.graphmask);					\
											uw_4d.cur_surf = GRAPHIC;						\
										uu_dexit

#define uw_4drestoresurf()		uu_denter2(UU_GTRC,(us,"4drestoresurf"));	\
										uw_4d.cur_surf = uw_4d.sav_surf;				\
										if( uw_4d.cur_surf == GRAPHIC )				\
											writemask(uw_4d.graphmask);				\
										else													\
											writemask(uw_4d.popmask);					\
										uu_dexit

#define uw_4dsob()				uu_denter2(UU_GTRC,(us,"uw4dsob() %d",		\
											uw_4d.objopn)); 								\
											if( uw_4d.objopn != -1 )					\
												closeobj()

#define uw_4drob()				uu_dexit; uu_denter2(UU_GTRC,(us,				\
											"uw4drob() %d",									\
											uw_4d.objopn)); uu_dexit; 						\
											if( uw_4d.objopn != -1 )						\
												editobj( uw_4d.objopn )

#define uw_4dinitnames()		uu_denter2(UU_GITRC,(us,"initnames()"));		\
											initnames();										\
											uu_dexit

#define uw_4dpushname(a)		uu_denter2(UU_GITRC,(us,"pushname %d",a));	\
											pushname(a);										\
											uu_dexit

#define uw_4dloadname(a)		uu_denter2(UU_GITRC,(us,"loadname %d",a));	\
											loadname(a);										\
											uu_dexit

#define uw_4dpopname()		uu_denter2(UU_GITRC,(us,"popname"));				\
											popname();											\
											uu_dexit

#define uw_4dcallobj(a)		uu_denter2(UU_GITRC,(us,"editobj %d",a));			\
											editobj(a);											\
											uu_dexit

#define uw_4dcloseobj()		uu_denter2(UU_GITRC,(us,"closeobj"));				\
											closeobj();											\
											uu_dexit

#define uw_4deditobj(a)		uu_denter2(UU_GITRC,(us,"editobj %d",a));			\
											editobj(a);											\
											uu_dexit


#define uw_4dpushx() 		{ pushmatrix(); pushviewport(); }

#define uw_4dpopx() 			{ popmatrix(); popviewport(); }

#define uw_4d_matprt(a)		{ int i; char us[180];									\
											for (i=0; i<4; i++) {							\
												uu_denter2(UU_GITRC,(us,					\
												"     %g %g %g %g",							\
												a[i][0], a[i][1], a[i][2], a[i][3]));	\
												uu_dexit; }										\
									}

#define uw_4d_ident(a)		{ int i,j;													\
											uu_denter(UU_GITRC,(us,                   \
												"uw_4d_ident(a)"));                  	\
											for (i=0; i<4; i++) {                     \
												for (j=0; j<4; j++) a[i][j]=0.0;       \
											}                                         \
											for (i=0; i<4; i++) a[i][i]=1.0;          \
											uu_dexit;                                 \
									}



/*
.....Override GL 'cursoff' routine by
.....displaying a blank cursor instead of
.....turning the cursor off
.....Bobby  -  9/19/91
*/

#define cursoff()       setcursor(4,0,0);

/* Workstation variables */
EXT UG_4ddat uw_4d;
EXT short colorsav[4096][3];		/* save color map */

/* Array to hold up to 10 user defined cursors.  Uw_4dusrcursor[i] 
	corresponds to locator device i+1. If uw_4dusrcursor[i] <  0, a user 
	cursor for this device has not been defined yet. If uw_4dusrcursor[i] > 0, 
	its value is the iris 4d object number containing it. */
EXT int uw_4dusrcursor[MAXUSRCURSORS]
#ifdef  UW_IRIS4DMAIN
= {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}
#endif
;
EXT Gipoint uw_4dusrattach[MAXUSRCURSORS];

/* Intersection of current vport and ws_window */
EXT Gnrect3 uw_4dclip;			

/* Visible area of screen (apply uw_wsxform to clip) */
EXT Gipoint uw_4dscrnclip[2];

/*RAH: in support of multiple arch. */
EXT int UW_4DTYPE;
/*
.....'edyndraw' is used by the 'uw_4dviewsg' routine to determine whether
.....or not Dynamic Viewing is enabled when drawing 3dpolylines.
.....When Dynamic Viewing is active, then GL 'draw' is used instead of
.....'v3f' to draw lines, because of a bug in the RS/6000 OS.
.....Bobby  -  1/8/92
*/
EXT int edyndraw;

#undef EXT
#define WSI24H
#endif
