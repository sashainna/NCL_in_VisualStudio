/********************************************************************* 
**  NAME:  wsi24.h 
**
**      Iris workstation include file.
**
**  COPYRIGHT  1987  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       wsi24.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:10
**
*********************************************************************/

#ifndef WSI24H


#include <gl.h>
#include <device.h>
#include "gobas.h"
#include "gtbldef.h"

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

} UG_irisdat;

typedef enum{							/* this defines types of objects that	*/
			MENUTYP,						/* routine uw_irismakobj is willing to	*/	
			TRANTYP,						/* make an object no for 					*/
			PRMTTYP,
			LOCTYP,
			OTHERTYP
		} UG_objtyp;


/********************** device specific defines ***************************/	
/* device coordinate space is from (0,0) to (DEVXMAX,DEVYMAX) */
#define DEVXMAX 1023				/* maximum x coordinate */
#define DEVYMAX 767				/* device maximum y coordinate */

/* screen will hold ROWMAX rows of COLMAX  characters each, when using
	the standard size alpha characters */
#define ROWMAX  48				/* max alpha rows @16 pixels baseline dist.		*/
#define COLMAX  113				/* max alpha columns										*/
#define STRWDTH 9					/* width of one character in pixels					*/
#define XTPMIN  136				/* current default textport boundaries 			*/
#define XTPMAX  872
#define YTPMIN  68
#define YTPMAX  690

/********************** magic numbers *****************************************/

#define NUMPETS 6					/* Number of available prompt-and-echo types		*/
#define NCHDEV 350				/* number of choice devices							*/
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

#define UW_IRIS_COLORREP 1
#define UW_IRIS_SETPOPCLR 2
#define UW_IRIS_RASLINE 3 
#define UW_IRIS_FILLRAS 4
#define UW_IRIS_RASTEXT 5
#define UW_IRIS_PICKID 6
#define UW_IRIS_CELLRAS 7

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
#define BLACK 2					/* these support the "remap" function call */
#undef RED							/* defined in wsiris.c */
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

/******************** useful constants ****************************************/
#define PI		(3.1415926454)
#define DTOR	(PI/180.0)
#define RTOD	(180.0/PI)

/****************** more macros ***********************************************/

#define uw_irispopsurf()		uu_denter2(UU_GTRC,(us,"irispopsurf"));		\
											uw_iris.sav_surf = uw_iris.cur_surf;		\
											writemask(uw_iris.popmask);					\
											uw_iris.cur_surf = POPUP;						\
										uu_dexit

#define uw_irisgraphsurf()		uu_denter2(UU_GTRC,(us,"irisgraphsurf"));		\
											uw_iris.sav_surf = uw_iris.cur_surf;		\
											writemask(uw_iris.graphmask);					\
											uw_iris.cur_surf = GRAPHIC;					\
										uu_dexit

#define uw_irisrestoresurf()	uu_denter2(UU_GTRC,(us,"irisrestoresurf"));	\
										uw_iris.cur_surf = uw_iris.sav_surf;			\
										if( uw_iris.cur_surf == GRAPHIC )				\
											writemask(uw_iris.graphmask);					\
										else														\
											writemask(uw_iris.popmask);					\
										uu_dexit

#define uw_irissob()				uu_denter2(UU_GTRC,(us,"uwirissob() %d",		\
											uw_iris.objopn)); 								\
											if( uw_iris.objopn != -1 )						\
												closeobj()

#define uw_irisrob()				uu_dexit; uu_denter2(UU_GTRC,(us,				\
											"uwirisrob() %d",									\
											uw_iris.objopn)); uu_dexit; 					\
											if( uw_iris.objopn != -1 )						\
												editobj( uw_iris.objopn )

#define uw_irisinitnames()		uu_denter2(UU_GITRC,(us,"initnames()"));		\
											initnames();										\
											uu_dexit

#define uw_irispushname(a)		uu_denter2(UU_GITRC,(us,"pushname %d",a));	\
											pushname(a);										\
											uu_dexit

#define uw_irisloadname(a)		uu_denter2(UU_GITRC,(us,"loadname %d",a));	\
											loadname(a);										\
											uu_dexit

#define uw_irispopname()		uu_denter2(UU_GITRC,(us,"popname"));			\
											popname();											\
											uu_dexit

#define uw_iriscallobj(a)		uu_denter2(UU_GITRC,(us,"editobj %d",a));		\
											editobj(a);											\
											uu_dexit

#define uw_iriscloseobj()		uu_denter2(UU_GITRC,(us,"closeobj"));			\
											closeobj();											\
											uu_dexit

#define uw_iriseditobj(a)		uu_denter2(UU_GITRC,(us,"editobj %d",a));		\
											editobj(a);											\
											uu_dexit


#define uw_irispushx() 			{ pushmatrix(); pushviewport(); }

#define uw_irispopx() 			{ popmatrix(); popviewport(); }


#ifdef UW_IRISMAIN
#define EXT
#else
#define EXT extern
#endif

/* Workstation variables */
EXT UG_irisdat uw_iris;

/* Array to hold up to 10 user defined cursors.  Uw_irisusrcursor[i] 
	corresponds to locator device i+1. If uw_irisusrcursor[i] <  0, a user 
	cursor for this device has not been defined yet. If uw_irisusrcursor[i] > 0, 
	its value is the iris object number containing it. */
EXT int uw_irisusrcursor[MAXUSRCURSORS]
#ifdef  UW_IRISMAIN
= {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}
#endif
;
EXT Gipoint uw_irisusrattach[MAXUSRCURSORS];

/* Intersection of current vport and ws_window */
EXT Gnrect3 uw_irisclip;			

/* Visible area of screen (apply uw_wsxform to clip) */
EXT Gipoint uw_irisscrnclip[2];

#undef EXT
#define WSI24H
#endif
