
/*********************************************************************
**    NAME         :  ws747sec.c
**    CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ws747sec.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:04
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "zsysdep.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "ws7475.h"
#include "tplot.h"   /*added kathy */

extern Gs7475 uw_7475;					/* declare workstation local data */
static WSPTINFO	jpbuf[MAXJP];		/* information from pass 1 */
/* static jpind = 0;							/* index to jpbuf */
extern	int	uw_7475ind;				/* index to jpbuf */

/* extern UU_LOGICAL	P1P2_control;  declared in tplot.h kathy */

/* added kathy */
static WSPTINFO	ipbuf[MAXJP];		/* information from pass 1 */
extern	int	uw_7475ipt;				/* index to ipbuf */
extern  int pts;

/*********************************************************************
**    I_FUNCTION :  uw_7475wswind(prms)              
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uw_7475wswind(prms)              /* set workstation window. */
struct { Gint op; Gws ws; Gnrect3 wswind;} *prms;
{
	Gfloat winheight,nominal;

	uu_denter(UU_GITRC,(us,"uw_7475wswind"));

	/* reset nominal line width */
	winheight = prms->wswind.urb.y - prms->wswind.llf.y;
	nominal = winheight/ug_gksstli.wsopen[prms->ws].wdtptr->dspsize.raster.y;
	ug_gksstli.wsopen[prms->ws].wdtptr->outwdtpt->lnfac.nom = nominal;
	uu_dprint(UU_GITRC,(us,"new nominal line width = %g",nominal));

	zbytecp((*ug_gksstli.wsopen[(*prms).ws].outptr).curxform.w,(*prms).wswind);			/* structure assignment */
	uw_7475wsxform(&(*ug_gksstli.wsopen[(*prms).ws].outptr).curxform,
			&uw_7475.wsxform,(*prms).ws);

	uu_dprint(UU_GITRC,(us,"uw_7475wswind(%g %g,  %g %g)",(*prms).wswind.llf.x,
			(*prms).wswind.llf.y,(*prms).wswind.urb.x,(*prms).wswind.urb.y));
	uu_dexit;
}	/* uw_7475wswind */


/*********************************************************************
**    I_FUNCTION :  int uw_7475wsvport(prms)				
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uw_7475wsvport(prms)				/* set workstation viewport */
struct {Gint op; Gws ws; Gdrect3 wsvport;} *prms;

{

	uu_denter(UU_GITRC,(us," uw_7475wsvport(%g %g,  %g %g,%d,%d)",(*prms).wsvport.llf.x,
	(*prms).wsvport.llf.y,(*prms).wsvport.urb.x,(*prms).wsvport.urb.y,
	(*prms).ws,uw_7475.isrotate));
	zbytecp((*ug_gksstli.wsopen[(*prms).ws].outptr).curxform.v,(*prms).wsvport);				/* structure assignment */
	uw_7475wsxform(&(*ug_gksstli.wsopen[(*prms).ws].outptr).curxform,
			&uw_7475.wsxform,(*prms).ws);	/* update wsxform */

/*
.....Check for "A VERT" paper size, instead of X-axis travel greater
.....than a hard coded number, to handle rotation
.....Bobby  -  7/16/91
*/
	if (strcmp(TP_size,"AV") == 0) uw_7475.isrotate = UU_TRUE;
	else uw_7475.isrotate = UU_FALSE;


/*	if ((!uw_7475.isrotate)&&(prms->wsvport.urb.x < (10.5*2.54/100.0))&&(!P1P2_control))
		 uw_7475.isrotate = UU_TRUE;
	else
		if ((uw_7475.isrotate)&&(prms->wsvport.urb.x > (9*2.54/100.0)))
		 	uw_7475.isrotate = UU_FALSE;*/

	uu_dprint(UU_GITRC,(us," uw_7475wsvport(%g %g,  %g %g)",(*prms).wsvport.llf.x,
			(*prms).wsvport.llf.y,(*prms).wsvport.urb.x,(*prms).wsvport.urb.y));
	uu_dexit;
}	/* uw_7475wsvport */




/*********************************************************************
**    I_FUNCTION :  int uw_7475wsxform(wstran,wsxform,ws)	
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uw_7475wsxform(wstran,wsxform,ws)	
										/* calculate wsxform =scale and dx,dy to 
											perform the workstation transformation
											from window to vport to raster */
Gwstran3 *wstran;					/* workstation window and viewport */
struct {Gfloat scale; Gfloat dx,dy;} *wsxform;
Gws ws;
{
	char us[160];
	Gfloat sx,sy,sx2,sy2,s1,s2,s;
	int ll[2],ur[2];
	int devxmax;
	Gnrect3 *wp;
	Gdrect3 *vp;
	Gfloat y,waspect,wdx,wdy;
	UU_REAL	ratio;

	devxmax=(*ug_gksstli.wsopen[ws].wdtptr).dspsize.raster.x;
	wp= &(*wstran).w;						/* get pointers to ws window, viewport */
	vp= &(*wstran).v;
	uu_denter2(UU_GITRC,(us,"uw_7475wsxfm,wp=%g,%g,%g,%g,wv=%g,%g,%g,%g",
	(*wp).urb.x,(*wp).llf.x, (*wp).urb.y,(*wp).llf.y,
	(*vp).urb.x,(*vp).llf.x, (*vp).urb.y,(*vp).llf.y));
	uu_dexit;

	wdx=(*wp).urb.x-(*wp).llf.x;		/* ws window width */
	wdy=(*wp).urb.y-(*wp).llf.y;		/* ws window height */
	sx=((*vp).urb.x-(*vp).llf.x)/wdx;
	sy=((*vp).urb.y-(*vp).llf.y)/wdy;
	s1=(sx<sy) ? sx : sy;			/* use smaller of scale factors */
	ratio = devxmax/(*ug_gksstli.wsopen[ws].wdtptr).dspsize.device.x;	
	s=s1*ratio;
	/* s now scales window to pixels */
	(*wsxform).scale=s;	/* save composite scale factor */
	(*wsxform).dx=ratio*(*vp).llf.x-s*(*wp).llf.x;
	(*wsxform).dy=ratio*(*vp).llf.y-s*(*wp).llf.y;
	/*
	(*wsxform).dx=devxmax*(*vp).llf.x-s*(*wp).llf.x;
	(*wsxform).dy=devxmax*(*vp).llf.y-s*(*wp).llf.y;
	*/
	/* ll,ur = window in integer coords */
	ll[0]=(*wp).llf.x*devxmax; ll[1]=(*wp).llf.y*devxmax;
	ur[0]=(*wp).urb.x*devxmax; ur[1]=(*wp).urb.y*devxmax;
	uu_denter2(UU_GITRC,(us,
			"uw_7475wsxform. wswind=%g %g,%g %g, scale=%g, dx,dy=%g %g",
			(*wp).llf.x,(*wp).llf.y,(*wp).urb.x,
			(*wp).urb.y,(*wsxform).scale,(*wsxform).dx,(*wsxform).dy));
	uu_dexit;
	/* ll,ur= adjusted viewport in integer coords */
	ll[0]=(*wp).llf.x*s+(*wsxform).dx; 
	ll[1]=(*wp).llf.y*s+(*wsxform).dy;
	ur[0]=(*wp).urb.x*s+(*wsxform).dx; 
	ur[1]=(*wp).urb.y*s+(*wsxform).dy;
	uw_7475.vpy = ur[1];
	uu_denter2(UU_GITRC,(us,"uw_7475wsxform. adjusted wsvport=%d %d %d %d",
		ll[0],ll[1],ur[0],ur[1]));
	uu_dexit;
}	/* uw_7475wsxform */
		

/*********************************************************************
**    I_FUNCTION :  uw_7475ndctodev(f,i,ws)	
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uw_7475ndctodev(f,i,ws)	/* convert (fx,fy) from ndc to (ix,iy) 
											raster coords */
int i[2];
Gfloat f[2];
Gws ws;							/* workstation id */
{
	/* added kathy */
	extern int wsplotdx, wsplotdy;
	char us[180];
	i[0]=uw_7475.wsxform.scale*f[0]+uw_7475.wsxform.dx+wsplotdx+.5;
	i[1]=uw_7475.wsxform.scale*f[1]+uw_7475.wsxform.dy+wsplotdy+.5;
	uu_denter2(UU_GITRC,(us,"uw_7475ndctodev(%g %g, %d %d)",
		f[0],f[1],i[0],i[1]));
	uu_dexit;
}	/* uw_7475mdctodev */

		

/*********************************************************************
**    I_FUNCTION :  uw_7475devtondc(rast,ndc,ws)		
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uw_7475devtondc(rast,ndc,ws)		/* convert from raster to ndc coords */
int rast[2];
Gfloat ndc[2];
Gws ws;
{
	int y;
	char us[180];
	ndc[0]=(rast[0]-uw_7475.wsxform.dx)/uw_7475.wsxform.scale;
	ndc[1]=(rast[1]-uw_7475.wsxform.dy)/uw_7475.wsxform.scale;
	uu_denter2(UU_GITRC,(us,"uw_7475devtondc(%g %g, %d %d)",
		ndc[0],ndc[1],rast[0],rast[1]));
	uu_dexit;
}	/* uw_7475devtondc */

/*------------------------- local declarations --------------------------*/

/* secondary entries (lower level, called by wsdev.c simulation routines) */

/*-------------------------------------------------> UG_DAWAITCHOICE */
uw_7475awtch(wid,k)
Gws wid;						/* workstation id */
int *k;						/* dev select key no */
{
	/* Put up choice prompt for device curdevno if not already up by
		calling UG_DPROMPT or UG_DMENU depending 
		on echotype to put up prompt.
		Call UG_D1CHAR or UG_DTRK to wait for user to enter something,
				depending upon prompt and echo type.
		Take down prompt or menu by calling UG_DDNPROMPT or UG_DDNMENU.
		If user made a choice, update current choice measure.
		If user used keypad 2 key 1-6 (dev sel key), set devkey=number of
		the key used and return 0.
		If he used keypad key update choice device 2, put choice event on Q
		if in EVENT mode, and return 1.
		If he used tablet puck, update choice device 3, put choice event on Q
		if in EVENT mode, and return 1.
	*/	
	uu_denter(UU_GITRC,(us,"uw_7475awatch()"));
	uu_dexit;
}

/*------------------------------------------------> GDAWAITPIK */
uw_7475awtpik(wid,k)
Gws wid;							/* workstation id */
int *k;							/* dev select key number */
{
		/* Call UG_DPROMPT to put up pick prompt.
			Call UG_DPIK to wait for user to enter something.
			Call UG_DDNPROMPT to take down prompt.
			Update current pick measure, put on Q if EVENT mode.
			Update loc device 1 measure, put on Q if EVENT mode.
			If user used a device select key, set k=key number
			and return 0.  
			If user used normal kbd key, update choice dev 1, return 1.
			If user used keypad key, update choice device 2, return 1.
			If user used tablet puck, update choice device 3, return 1.
		*/	
	uu_denter(UU_GITRC,(us,"uw_7475awtpik()"));
	uu_dexit;
}

/*------------------------------------------------> UG_DAWAITLOC */
uw_7475awaitloc(wid,k)
Gws wid;						/* workstation id */
int *k;						/* device select key number */
{	
/* Call UG_DPROMPT to put up loc prompt for device number curdevno.
	Call UG_DTRK to await users input.
	Call UG_DDNPROMPT to take down prompt.
	Update current loc measure, put on Q if in EVENT mode.
	If he used keypad 2 key 1-6 (dev select keys), set k=number of the key used
	and return 0.  If other keypad 2 key, update choice device 4 and return 1.
	If he used a normal kbd key, keypad key, or tablet puck,  
	update choice 1 2, or 3 respectively and put on Q
	if in EVENT mode. Return 1.
*/
	uu_denter(UU_GITRC,(us,"uw_7475awaitloc()"));
	uu_dexit;
}

/*------------------------------------------------> UG_DAWAITVAL */
uw_7475awtval(wid,k)
Gws wid;							/* workstation id */
int *k;							/* dev select key number(1..6) */
{
/* Call UG_DPROMPT to put up prompt for valuator device curdevno. 
	If pet=1, call workstation's UG_DKBD entry to await users value. 
	If pet=21, call workstation's UG_DSTREAM entry to await users value.
	Call UG_DDNPROMPT to take down prompt.
	Update valuator measure and put val on Q if in EVENT mode.
	If he used keypad 2 key 1 to 6, set k=number of the key used
	and return 0. Else update choice device 4 and return 1.
	If he used a carriage return, put value event on Q 
	if in EVENT mode and not subject of a request, and return 1.
	If he hit keypad key update choice device 2
	put choice event on Q if in EVENT mode, and return 1.
	If he used tablet puck, update choice device 3,
	put choice device 3 event on Q if in EVENT mode, and return 1.
*/
	uu_denter(UU_GITRC,(us,"uw_7475awtval()"));
	uu_dexit;
}

/*----------------------------------------------> UG_DAWAITSTRING */
uw_7475awtstr(wid,k)
Gws wid;							/* workstation id */
int *k;							/* dev select key number */
{
/* Put up string prompt for device number curdevno if not already up
	by calling UG_DUPSCROLL if prompt is a scrolling text area, or calling
	UG_DMSG to put up prompt string.
	Call UG_DKBD to await users input.
	Take down scrolling text prompt if necessary by calling UG_DDNSCROLL.
	Update current string measure with whatever he typed.
	If he used keypad 2 (dev select keys), set k=number of the key used
	and return 0. 
	If he used a carriage return, put string event on Q 
	if in EVENT mode and not subject of a request, and return 1.
	If he hit keypad key update choice device 2
	put choice event on Q if in EVENT mode, and return 1.
	If he used tablet puck, put update choice device 3,
	put choice device 3 event on Q if in EVENT mode, and return 1.
*/
	uu_denter(UU_GITRC,(us,"uw_7475awtstr()"));
	uu_dexit;
}

/*-------------------------------------------------> UG_DAWAITSTROKE */
uw_7475awtstrk(prms,reply)		/* await stroke */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475awtstrk()"));
	uu_dexit;
}


/*------------------------------------------------> UG_DECHOOP */
uw_7475echoop(prms,reply)				/* echo */
int prms[],reply[];
{
	/* not implemented yet */
	uu_denter(UU_GITRC,(us,"uw_7475echoop()"));
	uu_dexit;
}

/*-------------------------------------------------> UG_DNDCDEV */
uw_7475ndcdev(f,i)	
int i[2];
Gfloat f[2];
{
	/* convert (fx,fy) from ndc to (ix,iy) raster coords */
	uu_denter(UU_GITRC,(us,"uw_7475ndcdev()"));
	uu_dexit;
}

/*------------------------------------------------> UGDDEVNDC */
uw_7475devndc(rast,ndc)			/* convert from device to NDC coords */
int rast[2];
Gfloat ndc[2];
{
	/* convert rast from device to NDC coords and put in ndc. */
	uu_denter(UU_GITRC,(us,"uw_7475devndc()"));
	uu_dexit;
}

/*------------------------------------------------> UG_DPAGOP */
uw_7475pagop()				/* page (clears the screen) */
{
	/* same as UG_DREDRAWWS. Only here for compatibility reasons */
	uu_denter(UU_GITRC,(us,"uw_7475pagop()"));
	uu_dexit;
}

/*------------------------------------------------> UG_DSAVSCROP */
uw_7475savscrop()			/* save screen */
{
	/*  now obsolete. Will not appear in future revisions */
	uu_denter(UU_GITRC,(us,"uw_7475savscrop()"));
	uu_dexit;
	
}

/*------------------------------------------------> UG_DRESSCROP */
uw_7475rstscrop()			/* restore screen */
{
	/*  now opsolete. Will not appear in future revisions */
	uu_denter(UU_GITRC,(us,"uw_7475rstscrop()"));
	uu_dexit;
}

/*------------------------------------------------> UG_DATEXT */
uw_7475atext(x,y,s)				/* alpha text */
int x,y;
char *s;
{
	/* put up the alpha text pointed to by s at x,y.  */
	uu_denter(UU_GITRC,(us,"uw_7475atext()"));
	uu_dexit;
}

/*------------------------------------------------> UG_DRASGET */
uw_7475rasget(rasll,rasur,savmem,rule) /* read a raster rectangle */ 
int rasll[2];					/* lower left of rectangle, raster coords */
int rasur[2];					/* upper right of rectangle */
int *savmem;					/* pointer to area big enough to hold rect */
int rule;						/* raster operation, one of: 
										UG_COPY UG_OR UG_XOR UG_AND UG_NOT UG_CLEAR UG_SEG */
{
		/* read into savmem the screen rectangle defined by rasll,rasur.
		Use operation "rule" when reading. */
	uu_denter(UU_GITRC,(us,"uw_7475rasget()"));
	uu_dexit;
}

/*------------------------------------------------> UG_DRASPUT */
uw_7475rasput(rasll,rasur,savmem,rule) 	/* put a raster rectangle */
int rasll[2];					/* lower left of rectangle, raster coords */
int rasur[2];					/* upper right of rectangle */
int *savmem;					/* pointer to area from which to get rect,or null */
int rule;						/* raster operation, one of: 
										UG_COPY UG_OR UG_XOR UG_AND UG_NOT UG_CLEAR UG_SEG */
{
	/* Put the raster rectangle pointed to by savmem onto the screen, in
	a rectangle defined by rasll, rasur. If savmem==NULL, the rule will
	be one which does not require a source operand (such as UG_CLEAR) */
	uu_denter(UU_GITRC,(us,"uw_7475rasput()"));
	uu_dexit;
}

/*------------------------------------------------> UG_DRASCPY */
uw_7475rascpy(lls,urs,lld,rule) /* copy a raster rectangle */
int lls[2],urs[2];			/* source rectangle */
int lld[2];						/* destination rectangle */
int rule;						/* raster operation, one of:
										UG_COPY UG_OR UG_XOR UG_AND UG_NOT UG_CLEAR UG_SEG */
{
	/* copy a rectangular area of the screen defined by (lls,urs) to 
	another rectangular area of the screen of the same size whose 
	lower left corner is (lld). "rule" is the raster-op rule to use
	for the copy. */
	uu_denter(UU_GITRC,(us,"uw_7475rascpy()"));
	uu_dexit;
}

/*------------------------------------------------> UG_DRASALLOC */
uw_7475rasaloc(ws,ll,ur,pt)		/* allocate memory for a raster rectangle */
Gws ws;							/* workstation id */
int ll[2];						/* lower left corner of raster rect */
int ur[2];						/* upper right corner of raster rect */
int **pt;							/* pointer to pointer to memory */
{
	/* allocate enough memory to hold the raster rectangle. Return
		a pointer to the memory in *pt. Actually, RASALLOC may return
		any single word it wishes in *pt. This word will not be
		used by the rest of the graphics package, but will be
		passed as an argument to RASGET, UG_DRASPUT, UG_DRASCPY and
		UG_DRASDEALLOC */
	uu_denter(UU_GITRC,(us,"uw_7475rasaloc()"));
	uu_dexit;
}

/*------------------------------------------------> UG_DRASDEALLOC */
uw_7475rasdaloc(ws,pt)			/* deallocate memory for a raster rect */
Gws ws;							/* workstation id */
int *pt;							/* pointer to memory */
{
	/* deallocate the memory pointed to by pt. Pt contains whatever
		was placed din it by UG_DRASALLOC. */
	uu_denter(UU_GITRC,(us,"uw_7475rasdaloc()"));
	uu_dexit;
}


/*------------------------------------------------> UG_DKBD */
uw_7475kbd(buf,k)		/* UG_DKBD table entry -- get string from kbd. */
unsigned char *buf;
int *k;
{
		/* Put chars in buf. Allow user to use backspace and line delete.
			Return codes:
		0 =a 2nd keypad key was hit, k contains the key number. 
		1= a carriage return ended kbd input. K not set.
		2=a keypad key was hit, k contains key number. 
		3=a tablet puck button was used, k contains number.
			In all 4 cases, buf contains the null terminated string entered
		on the ASCII keyboard before the ending key was hit.*/
		/* Note: it is not possible to return value 3 (puck) on 4115 hardware*/
	uu_denter(UU_GITRC,(us,"uw_7475kbd()"));
	uu_dexit;
}

/*------------------------------------------------> UG_D1CHAR */
uw_74751char(k)		/* UG_D1CHAR entry - get one char from kbd. */
int *k;								/* returned char */
{
	/* 	Do not
			recognize backspace, line erase, carriage return, etc. 
		Return values:
		0 =a 2nd keypad key was hit, k contains the key number. 
		1= An ASCII kbd key was hit, k contains ascii value.
		2=a keypad key was hit, k contains key number. 
		3=a tablet puck button was used, k contains number. 
			(this is not possible on 4115)  */
	uu_denter(UU_GITRC,(us,"uw_74751char()"));
	uu_dexit;
}

/*------------------------------------------------> UG_DKEYPAD */
uw_7475keypad(n,k)			/* UG_DKEYPAD entry - get keypad key.
int n;					/* keypad number (1 or 2) */
int *k;					/* which key number was struck */
{
	/*	Return values:
		0 =a keypad-2 key was hit, k contains the key number. 
		1= An ASCII kbd key was hit, k contains ascii value.
		2=a keypad-1 key was hit, k contains key number. 
		3=a tablet puck button was used, k contains number. 
			(this is not possible on 4115)  */
	uu_denter(UU_GITRC,(us,"uw_7475keypad()"));
	uu_dexit;
}

/*------------------------------------------------> UG_DTRK */
uw_7475trk(wid,xy,k,cursorno,locno,devno,usrc)	/* UG_DTRK entry - track a cursor */
Gws wid;						/* workstation id */
int xy[2];	 				/* input: initial cursor posn, dev coords*/
								/* output: final cursor posn, dev coords */
int *k;						/* ending key value */
int cursorno;				/* which style cursor to display:
									0=graphics picking cursor (PICK_CURSOR_SEG)
									1=menu picking cursor (MENU_CURSOR_SEG)
									2=loc cursor (LOC_CURSOR_SEG)   */
int locno;					/* 0=all locators available,
									>0= locator number locno. Locno 1 is thumbwheels
									or other keyboard manipulated locator. 
									Locno 2 is tablet if available. */
int devno;					/* If cursorno==21, devno==device number */
int *usrc;					/* For bitmaps, points to user-defined cursor */

{
	/* Track a cursor of type cursorno on the locator device specified by locno:
			0: use graphics picking cursor,
			1: use menu cursor,
			2: use locator cursor.
		Return values:
		0 =a keypad-2 key was hit, k contains the key number. 
		1= An ASCII kbd key was hit, k contains ascii value.
		2=a keypad-1 key was hit, k contains key number. 
		3=a tablet puck button was used, k contains number. 
		In all cases, x,y contain final coordinates.  */
	uu_denter(UU_GITRC,(us,"uw_7475trk()"));
	uu_dexit;
}

/*----------------------------------------------> UG_DPIK */
uw_7475pik(wid,seg,depth,xy,k,cursorno)
int seg[];					/* return segment number array picked, with pickid */
int *depth;					/* return length of seg here */
int xy[2];					/* on call, initial xy position of pick.
									on return x,y coordinates of pick */
int *k;						/* key that ended picking */
int cursorno;				/* number of the cursor to use:
									0=use graphics picking cursor,
									1=use menu picking cursor */
{

	/* UG_DPIK workstation table entry -- track a picking cursor.
		Return values:
		0 =a keypad-2 key was hit, k contains the key number. 
		1= An ASCII kbd key was hit, k contains ascii value.
		2=a keypad-1 key was hit, k contains key number. 
		3=a tablet puck button was used, k contains number. 
		In all cases, x,y contain final coordinates, segno and pkid
		contains segment and pickid of thing picked. If nothing picked,
		segno= -1. */
	uu_denter(UU_GITRC,(us,"uw_7475pik()"));
	uu_dexit;
}

/*-------------------------------------------> UG_DCHOICE */
uw_7475choice(devno,xy,k,choice)	/* UG_DCHOICE -- phys. "choice" device */
int devno;			/* choice device number */
int *k;				/* contains ending key or choice value */
int xy[2];			/* on return contains loc position in dev coords */
int *choice;		/* on return contains menu choice number, or zero */
{
	/* This gets called for menus echotype 22,23,24 which use the tablet.
		Track menu cursor. Set xy=cursor posn. Set choice=number of menu item 
		under cursor(1..n), or zero if cursor not over any item.
		Return values:
		0 =a keypad-2 key (dev sel) was hit, k contains the key number. 
		1= An ASCII kbd key was hit, k contains ascii value.
		2=a keypad-1 key was hit, k contains key number. 
		3=a tablet puck button was used, k contains number. 
		4= the expected device type was used, k contains choice number
		5= an inappropriate device was used, k contains device type */
	uu_denter(UU_GITRC,(us,"uw_7475choice()"));
	uu_dexit;
}

/*-------------------------------------------> UG_DBUTTON */
uw_7475button(devno,k,n)	/* UG_DBUTTON entry -- phys. button (switch) device */
int devno;			/* button device number */
int *k;				/* contains ending key or choice value */
unsigned long *n;	/* contains the "or" of the states of all the buttons */
{
	/* Get data from a physical button device.
		Return values:
		0 =a keypad-2 key was hit, k contains the key number. 
		1= An ASCII kbd key was hit, k contains ascii value.
		2=a keypad-1 key was hit, k contains key number. 
		3=a tablet puck button was used, k contains number. 
		4= the expected device type was used
		5= an inappropriate device was used, k contains device type */
	uu_denter(UU_GITRC,(us,"uw_7475button()"));
	uu_dexit;
}

/*--------------------------------------------> UG_DVAL */
uw_7475val(devno,k,v)	/* UG_DVAL entry -- phys. valuator device */
int devno;			/* logical valuator device number */
int *k;				/* contains ending key or choice value */
Gfloat *v;				/* contains value */
{
	/* Get data from a valuator device.
		Return values:
		0 =a keypad-2 key was hit, k contains the key number. 
		1= An ASCII kbd key was hit, k contains ascii value.
		2=a keypad-1 key was hit, k contains key number. 
		3=a tablet puck button was used, k contains number. 
		4= the expected device type was used
		5= an inappropriate device was used, k contains device type */
	uu_denter(UU_GITRC,(us,"uw_7475val()"));
	uu_dexit;
}

/*--------------------------------------------> UG_DSTREAM */
uw_7475stream(func,k,cursorno)	/* UG_DSTREAM entry -- tablet stream */
int (*func)();					/* call this with each x,y coordinate */
int *k;							/* contains ending key */
int cursorno;					/* which style stream cursor to use. Now
										have only one. */
{
	/* Enable tablet for stream input. On receiving each coordinate,
		call func(x,y) 
		Return values tell how stream ended:
		0 =a keypad-2 key was hit, k contains the key number. 
		1= An ASCII kbd key was hit, k contains ascii value.
		2=a keypad-1 key was hit, k contains key number. 
		3=a tablet puck button was used, k contains number. 
		NOTE: I believe only return possible on 4115 tablet is 3. Operator
			depresses puck to start stream and releases it to end stream. */
	uu_denter(UU_GITRC,(us,"uw_7475stream()"));
	uu_dexit;
}

/*---------------------------------------------> UG_DMOVOP */
/*********************************************************************
**    I_FUNCTION :  type name (parameters)
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_7475movop(x,y)				/* move to x,y */
int x,y;
{
	/* no longer used. should delete */
	uu_denter(UU_GITRC,(us,"uw_7475movop()"));
	uu_dexit;
}

/*---------------------------------------------> UG_DDRWOP */
/*********************************************************************
**    I_FUNCTION :  uw_7475drwop(prms)					
**		draw line from prms[2],prms[3] to [4],[5], NDC coordinates. Use
**		the current line color and linestyle. 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_7475drwop(prms)					/* line (2d) */
UG_line *prms;

{
	int	xy1[3], xy2[3], color; 
	int	tranlx,tranly,tmpx, tmpy;
	char	num[20];
	char	us[120];

	/*draw line from prms->p1 to prms->p2, NDC coordinates */
	uu_denter2(UU_GITRC,(us, "7475drwop(%g,%g,%g,%g)",(*prms).p1.x,(*prms).p1.y,(*prms).p2.x,(*prms).p2.y)); 
	uu_dexit; 
	uw_7475ndctodev(&(*prms).p1,xy1,uw_7475.wid);
	uw_7475ndctodev(&(*prms).p2,xy2,uw_7475.wid);
	if (uw_7475.isrotate)
	  {
		tmpx = -xy1[1] + uw_7475.vpy;
		tmpy = xy1[0];
		xy1[0] = tmpx;		xy1[1] = tmpy;
		tmpx = -xy2[1] + uw_7475.vpy;
		tmpy = xy2[0];
		xy2[0] = tmpx;		xy2[1] = tmpy;
	uu_denter2(UU_GITRC,(us, "7475drwopa,xy1=%d,%d,xy2=%d,%d",
		xy1[0],xy1[1],xy2[0],xy2[1]));
	uu_dexit; 
	  }
	xy1[0] = xy1[0] - DEVXHALF;
	xy1[1] = xy1[1] - DEVYHALF;
	xy2[0] = xy2[0] - DEVXHALF;
	xy2[1] = xy2[1] - DEVYHALF;
	uu_denter2(UU_GITRC,(us, "uw_7475drwop2(%d,%d,%d,%d)",xy1[0],xy1[1],xy2[0],xy2[1]));
	uu_dexit; 
									/* save the information to the buffer */
	color = ug_gksstli.curprats.lnbundl.color;
	uu_denter2(UU_GITRC,(us, "7475color,color=%d", color));
	uu_dexit;
	if (color > 6)   color = 1;
	jpbuf[uw_7475ind].color = color;
	jpbuf[uw_7475ind].pt1[0] = xy1[0];
	jpbuf[uw_7475ind].pt1[1] = xy1[1];
	jpbuf[uw_7475ind].pt2[0] = xy2[0];
	jpbuf[uw_7475ind].pt2[1] = xy2[1];
	uw_7475ind++;

	/* added kathy */
	/*if (uw_7475ind == MAXJP)*/
	if ((uw_7475ind+uw_7475ipt) == MAXJP)
		{
		 uw_7475dmpline();
		 uw_7475ind = 0;
		}

/* --------------------------------------------------------------*/
/*		 uu_ttput(uw_7475.ttfd,"SP ",3);			/* select pen color */
/*		 sprintf(num, "%d;", ug_gksstli.curprats.lnbundl.color);
		 uu_ttput(uw_7475.ttfd,num,strlen(num));			/* select pen color */
/*		 uu_ttflout(uw_7475.ttfd);
	uu_denter2(UU_GITRC,(us, "7475color,num=%s", num));
	uu_dexit;
 	uu_ttput(uw_7475.ttfd,"PU ",3);			/* pen up and move */
/*	sprintf(num, "%d,%d; ", xy1[0], xy1[1]);
 	uu_ttput(uw_7475.ttfd,num,strlen(num));
 	uu_ttput(uw_7475.ttfd,"PD ",3);			/* pen down and draw */
/*	sprintf(num, "%d,%d; ", xy2[0], xy2[1]);
 	uu_ttput(uw_7475.ttfd,num,strlen(num));
 	uu_ttput(uw_7475.ttfd,"PU;",3);			/* pen up and move */
/*---------------------------------------------------------------*/
}	/* uw_7475drwop */


/*------------------------------------------------> UG_DRASPNTS */
/*********************************************************************
**    I_FUNCTION :  uw_7475raspnts()
**       Plot the information in the buffer
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_7475raspnts()

{
	int	a, i, j, first;
	char	num[30];
	Gfloat	prept[3];			/* previous point */

	uu_denter(UU_GITRC,(us, "7475raspnts,uw_7475ipt=%d", uw_7475ipt));

	if (!pts)
		goto done;

	a = 0;
	prept[0] = ipbuf[0].pt1[0];
	prept[1] = ipbuf[0].pt1[1];
	for (i=1; i<9; i++)
	 {
	  first = UU_TRUE;
	  for (j=a; j<uw_7475ipt; j++)
		 {
		  if (ipbuf[j].color == i)
			 {
			  if (first)
				 {
				  first = UU_FALSE;
/*
.....Select pen color
.....Bobby  -  7/15/91
*/
					uw_7475pen(i);
/*
/*				  uu_ttput(uw_7475.ttfd,"SP ",3);			/* select pen color */
/*		 		  sprintf(num, "%d;", i);
/*		 		  uu_ttput(uw_7475.ttfd,num,strlen(num));	/* select pen color */

/*
.....This code causes symbol to be drawn twice
.....Bobby  -  7/23/91
*/
/*		 		  uu_ttflout(uw_7475.ttfd);
/* 			  	  uu_ttput(uw_7475.ttfd,"PU ",3);			/* pen up and move */
/*			  	  sprintf(num, "%d,%d; ", ipbuf[j].pt1[0], ipbuf[j].pt1[1]);
/*			  	  uu_ttput(uw_7475.ttfd,num,strlen(num));*/

				 }
			  if (a==j) 	a++;
/*
.....This code causes symbol to be drawn thrice
.....Bobby  -  7/23/91
*/
/*			  if ((prept[0]!=ipbuf[j].pt1[0])||(prept[1]!=ipbuf[j].pt1[1]))
/*				 {
/*			  	  uu_ttput(uw_7475.ttfd,"PU ",3);			/* pen up and move */
/*			  	  sprintf(num, "%d,%d; ", ipbuf[j].pt1[0], ipbuf[j].pt1[1]);
/*			  	  uu_ttput(uw_7475.ttfd,num,strlen(num));
/*				 }*/

/*
.....Use marker type from plot file
.....Bobby  -  7/23/91
*/
			sprintf (num,"SM%c ",ipbuf[j].type);
 		 	uu_ttput(uw_7475.ttfd,num,strlen(num));
/*
.....Changed PD to PU
.....Bobby  -  7/23/91
*/
 			  uu_ttput(uw_7475.ttfd,"PU ",3);			/* pen up and draw */
			  sprintf(num, "%d,%d; ", ipbuf[j].pt1[0], ipbuf[j].pt1[1]);
 			  uu_ttput(uw_7475.ttfd,num,strlen(num));

			  prept[0] = ipbuf[j].pt1[0];		/* save the last point  */
			  prept[1] = ipbuf[j].pt1[1];
 			  /*uu_ttput(uw_7475.ttfd,"PU;",3);			/* pen up and move */
			 }
		 }		/* for */
	 }	/* for */

   uu_ttput(uw_7475.ttfd,"SM;",3);			/* pen up and move */
   uu_ttput(uw_7475.ttfd,"PU;",3);			/* pen up and move */
   uu_ttflout(uw_7475.ttfd);

done:
	uu_dexit;
}	/* uw_7475raspnts  */


/*
.....Added new uw_7475rasline routine which
.....accepts the standard paramaters used to
.....call UG_DRASLINE.  Renamed old routine to
.....uw_7475dmpline
.....Bobby  -  7/24/91
*/
/*------------------------------------------------> UG_DRASLINE */
/*********************************************************************
**    I_FUNCTION :  uw_7475rasline(x1,y1,x2,y2)
**       Draw a raster line from x1,y1 to x2,y2
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_7475rasline(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
	int color;
	color = ug_gksstli.curprats.lnbundl.color;
	if (color > 6) color = 1;
	jpbuf[uw_7475ind].color = color;
	jpbuf[uw_7475ind].pt1[0] = x1;
	jpbuf[uw_7475ind].pt1[1] = y1;
	jpbuf[uw_7475ind].pt2[0] = x2;
	jpbuf[uw_7475ind].pt2[1] = y2;
	uw_7475ind++;
	if ((uw_7475ind+uw_7475ipt) == MAXJP)
	{
		uw_7475dmpline();
		uw_7475ind = 0;
	}
}

/*********************************************************************
**    I_FUNCTION :  uw_7475dmpline()
**       Plot the information in the buffer
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uw_7475dmpline()
{
	int	a, i, j, first;
	char	num[30];
	Gfloat	prept[3];			/* previous point */

	uu_denter(UU_GITRC,(us, "7475dmpline,uw_7475ind=%d", uw_7475ind));

	a = 0;
	prept[0] = jpbuf[0].pt1[0];
	prept[1] = jpbuf[0].pt1[1];
	for (i=1; i<9; i++)
	 {
	  first = UU_TRUE;
	  for (j=a; j<uw_7475ind; j++)
		 {
		  if (jpbuf[j].color == i)
			 {
			  if (first)
				 {
				  first = UU_FALSE;
/*
.....Select pen color
.....Bobby  -  7/15/91
*/
					uw_7475pen (i);

/*				  uu_ttput(uw_7475.ttfd,"SP ",3);			/* select pen color */
/*		 		  sprintf(num, "%d;", i);
/*		 		  uu_ttput(uw_7475.ttfd,num,strlen(num));	/* select pen color */
		 		  uu_ttflout(uw_7475.ttfd);
 			  	  uu_ttput(uw_7475.ttfd,"PU ",3);			/* pen up and move */
			  	  sprintf(num, "%d,%d; ", jpbuf[j].pt1[0], jpbuf[j].pt1[1]);
 			  	  uu_ttput(uw_7475.ttfd,num,strlen(num));
				 }
			  if (a==j) 	a++;
			  if ((prept[0]!=jpbuf[j].pt1[0])||(prept[1]!=jpbuf[j].pt1[1]))
				 {
 			  	  uu_ttput(uw_7475.ttfd,"PU ",3);			/* pen up and move */
			  	  sprintf(num, "%d,%d; ", jpbuf[j].pt1[0], jpbuf[j].pt1[1]);
 			  	  uu_ttput(uw_7475.ttfd,num,strlen(num));
				 }
 			  uu_ttput(uw_7475.ttfd,"PD ",3);			/* pen down and draw */
			  sprintf(num, "%d,%d; ", jpbuf[j].pt2[0], jpbuf[j].pt2[1]);
 			  uu_ttput(uw_7475.ttfd,num,strlen(num));

			  prept[0] = jpbuf[j].pt2[0];		/* save the last point  */
			  prept[1] = jpbuf[j].pt2[1];
 			  /*uu_ttput(uw_7475.ttfd,"PU;",3);			/* pen up and move */
			 }
		 }		/* for */
	 }	/* for */

   uu_ttput(uw_7475.ttfd,"PU;",3);			/* pen up and move */
	uu_dexit;
}	/* uw_7475dmpline  */



/*---------------------------------------------> UG_DPNTOP */
uw_7475pntop(prms)				/* marker */
UG_marker *prms;				/* defined in gksdidd.h as follows:
						typedef struct {
							Gint op;
							Gws id;
							Gnpoint p;
							Gint type;					/* marker type
						} UG_marker;	 */
{
/*
.....Added 'char'
.....Bobby  -  7/23/91
*/
	char *mtype = {".+*0X"};

	/* draw a marker of type (*prms).type at point (*prms).p in 
	NDC coordinates. Use the current marker color index. Use the
	marker type specified in (*prms).type. The predefined marker types
	are:
	1 = a small dot.
	2 = a plus sign.
	3 = an asterisk (*).
	4 = a zero (0).
	5 = a capital X.
		The driver's UG_DMKCINDEX entry will be called every time
	the marker color changes. Alternatively, the external variable
	ug_gksstli.curprats.mkbundl.color always contains the current
	marker color index. */

	/* added kathy */
	int	xy1[3],  color; 
	int	tmpx, tmpy;
	char	num[20];
	int     first;

	uu_denter(UU_GITRC,(us,"uw_7475pntop()"));
	uu_dexit;

	first = UU_TRUE;
	if (first)
		{
		pts = UU_TRUE;
		first = UU_FALSE;
		}

	uw_7475ndctodev(&(*prms).p,xy1,uw_7475.wid);
	if (uw_7475.isrotate)
	  {
		tmpx = -xy1[1] + uw_7475.vpy;
		tmpy = xy1[0];
		xy1[0] = tmpx;		xy1[1] = tmpy;
	  }

	xy1[0] = xy1[0] - DEVXHALF;
	xy1[1] = xy1[1] - DEVYHALF;
									/* save the information to the buffer */
	color = ug_gksstli.curprats.mkbundl.color;
	uu_denter2(UU_GITRC,(us, "7475color,color=%d", color));
	uu_dexit;
	if (color > 6)   color = 1;
	ipbuf[uw_7475ipt].color = color;
/*
.....Save marker type
.....Bobby  -  7/23/91
*/
	ipbuf[uw_7475ipt].type = mtype[(*prms).type-1];
	ipbuf[uw_7475ipt].pt1[0] = xy1[0];
	ipbuf[uw_7475ipt].pt1[1] = xy1[1];
	uw_7475ipt++;

	/* added kathy */
	if ((uw_7475ind+uw_7475ipt) == MAXJP)
		{
		 uw_7475raspnts();
		 uw_7475ipt = 0;
		}

}

/*---------------------------------------------> UG_DCHHTNDC */
uw_7475chhtndc(prms)				/* set character height NDC */
struct { Gint op; Gws id; Gfloat ndc_ht; } *prms;
{
	/*	(*prms).ndc_ht contains the char height between 0.0 and 1.0.
		Remember this and use it on all subsequent UG_DTEXT
		calls. If the device is able, use it on UG_DATEXT also. 
		Otherwise use "standard" height text on UG_DATEXT. */
	uu_denter(UU_GITRC,(us,"uw_7475chhtndc()"));
	uu_dexit;
}

/*---------------------------------------------> UG_DERASE */
uw_7475erase(prms)				/* set xor mode */
int prms[3];
{
	/* No longer used.  Will not appear in future revisions */
	uu_denter(UU_GITRC,(us,"uw_7475erase()"));
	uu_dexit;
}

/*----------------------------------------------> UG_DPROMPT */
uw_7475prompt(ws,prompt,loc,num)			
Gnpoint *loc;						/* where to put prompt */
char *prompt;						/* prompt string */
int num;								/* number of the prompt, 0<=num<=710 */
{
	/* pop-up prompt string at loc */
	/* NOTE: the hard part of this is making it possible for UG_DDNPROMPT
	  to restore whatever was behind the prompt before it was put up. */
	uu_denter(UU_GITRC,(us,"uw_7475prompt()"));
	uu_dexit;

}

/*---------------------------------------------> UG_DDNPROMPT */
uw_7475dnprmpt(ws,num)
Gws ws;
int num;								/* number of the prompt */
{
	/* take down prompt string, restoring whatever was behind it before
		it was put up. 
	  In general, this requires a full-blown window manager. If your
	  operating system has one, consider using it. Otherwise it is
	  usually necessary to clear the screen and re-draw the graphics and
	  whatever other prompts, menus, etc. were up. Some device hardware
	  can do this (for ex: Tek 4115). Because some device can't do this
	  and some I/O rates from host computer to device are too slow 
	  to erase and re-draw, UNICAD software currently operates under
	  the restriction that prompts will come up and down in stack (LIFO)
	  order. This allows the driver to use the following simpler 
	  algorithm:
		Before putting up a prompt, save the contents of the area of the
		screen covered by the prompt. To take down a prompt, simply replace
		that area of the screen with the saved contents. */
	uu_denter(UU_GITRC,(us,"uw_7475dnprmpt()"));
	uu_dexit;
}

/*----------------------------------------------> UG_DMENU */
uw_7475menu(wid,deviceno,prompt)	
Gws wid;									/* workstation id */
Gchar *prompt;							/* prompt */
int deviceno;							/* choice device number of this menu */
{
	/* popup a menu for choice device deviceno*/
	/* The hard part of this is to be able to take down the menu, replacing
		what was behind the menu */
	uu_denter(UU_GITRC,(us,"uw_7475menu()"));
	uu_dexit;
}

/*-----------------------------------------------> UG_DDNMENU */
uw_7475dnmenu(wid,devno)			
Gws wid;						/* workstation id */
int devno;					/* choice device number */
{
	/* take down menu  for device devno */
	/* The same comment applies here as applied for UG_DDNPROMPT above.
		The LIFO restriction is currently used by UNICAD software for
		menus (as well as prompts), so the same simpler algorithm may
		be used. */
	uu_denter(UU_GITRC,(us,"uw_7475dnmenu()"));
	uu_dexit;
}



