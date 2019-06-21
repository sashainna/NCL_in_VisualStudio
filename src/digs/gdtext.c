/**********************************************************************
**    NAME        :  gdtext.c - UNICAD fonts for workstation simulation.
**       CONTAINS:
**		ug_hershy(prms,raster) -- font chars.
**    static ug_fchinit(prms) -- init vars for stroking or text extent.
**		static ug_chxfm(chxfrm,gcurpos) -- calc font to ndc xform.
**		static ug_movch(c,chpos) -- update chpos for character c.
**		static ug_vcfnd(k) -- read kth font file.
**		static ug_vcdrw(wsid,pos,c,chxfrm) -- draw char c at pos.
**		static ug_vcdrw_ptadd(&ibuffpt,ipoints,points,p1,p2,raster)
**    static ug_fchfind(s,cur) -find char s[cur] in font char directory
**    ug_fchext(prms,concat,extrect) -- get text extent rect,concat pt
**		ug_text_capture(flag)
**		ug_text_iscaptured(flag)
**
**    COPYRIGHT 1990 (c) MILLS DATA SYSTEM Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gdtext.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:19
*********************************************************************/
#include <stdio.h>
#include "umath.h"
#include "usysdef.h"
#include "zsysdep.h"
#include "ginq.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "udebug.h"
#include "gfont.h"
#include "gerrorid.h"
#include "gmat4.h"
#include "ulist.h"
#include "xenv1.h"

char *ux_getenv();

#define UG_FUZZ (UU_REAL) 1.0e-10
/*------- default sizes for CHAR precision ------------------------*/
#define CGRIDSIZE		32  				/* Size of font grid	*/
#define CMID				16  				/* X center of font cell*/
#define CHALF		   16					/* Y center of font cell*/
#define CBASE		   11					/* Base line of characters	*/
#define CFONTHEIGHT 	22  				/* Height of capital letter*/
#define CWS			 	4 					/*White space included in font*/

#define LINECMD       0              /* stroke line command */
#define ARCCMD        1              /* stroke arc command */
#define LINEMOVE      0              /* line - move command */
#define LINEDRAW      1              /* line - move command */
#define ARCDIRCCW     0              /* arc - counterclockwise direct*/
#define ARCDIRCW      1              /* arc - clockwise direction */

#define TWOPI         (2*PI)
#define OVERLAP_TYPE  3
#define VCDRW_BUFFSIZE 50

static Gfloat fgridsize;             /* size of font grid */
static Gfloat fboxheight;            /* height of font grid */
static Gfloat fmid;                  /* X center of font cell */
static Gfloat fhalf;                 /* Y center of font cell */
static Gfloat fbase;                 /* base line of characters */
static Gfloat ffontheight;           /* height of capital letter */
static Gfloat fws;                   /* white space included */

static UG_FB   prchar_fb;           /* dummy font block for CHAR prec*/
static UG_FCDB dummy_fcdb;          /* dummy font char block */

static Gtxpath   path;					/* text path */
static Gtxhor    txal;					/* horiz alignment */
static Gtxver    tyal;					/* vertical alignment */
static Gtxprec   txpr;					/* text precision */
static Gfloat    extent;				/* extent of string in font coords */
static Gnpoint3  chpos;				/* font coord position of each char */
static Gfloat    chxfrm[4][4];		/* xlates font coords to out coords */
static Gfloat    spc;							/* character spacing */
static char      *str;					/* pointer to the char string */
static Gint      len;							/* length of text */
static int       fontlu;					/* xio logical unit */
/*static int       fontfd;					 font file descriptor */

/*---- shift index- use 1st char right, 2nd char left otype (1-6)---*/
/*     to get font oshift element index 0-2. -1 is NO oshift        */
static int       shift_index[7][7] = {  /* [1st right][2nd left] */
/*-2nd left otype--- 0  1  2  3  4  5  6----------------------------*/
							{-1,-1,-1,-1,-1,-1,-1},/* 1st right otype 0 */
							{-1,-1,-1,-1,-1,-1,-1},/* 1st right otype 1 */
							{-1,-1, 1,-1, 0, 1, 0},/* 1st right otype 2 */
							{-1,-1,-1, 1,-1,-1, 2},/* 1st right otype 3 */
							{-1,-1,-1, 0,-1,-1,-1},/* 1st right otype 4 */
							{-1,-1, 2, 0,-1, 2,-1},/* 1st right otype 5 */
							{-1,-1,-1, 1,-1,-1, 2} /* 1st right otype 6 */
						};

static int Scurpoly,Snpts;
static UU_LOGICAL Scapture=UU_FALSE;
static UU_LIST *Sptlist;

static void S_capture_point(),S_capture_poly();
static int ug_fchinit(),ug_chxfm(),ug_movch(),ug_vcfnd(),ug_vcdrw();
static int ug_vcdrw_ptadd(),ug_fchfind();

/*********************************************************************
**    E_FUNCTION   : int ug_hershy(prms,raster)
**			Calculate (and draw) CHAR and STROKE precision text.
**    PARAMETERS   
**       INPUT  : 
**				UG_dtext 	prms				location,stringlen and string
**				int			raster			1= stroke to raster coordinate
**													other= stroke to world coordinates
**       OUTPUT :  
**    RETURNS      : 0= font file found OK
**							1= font file not found (open error )
**							2= error reading font file
**							3= error allocating font block.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_hershy(prms,raster)
UG_dtext *prms;
int raster;							/* 1= raster, other=world coord */
{
		
	Glntype   oldlstyle;				/* copy of current line style */
	Glntype 	lntype;
	int		oldcolor;
	int       i;
	int       status;
	int       chlen,next_chlen;		/* 1 or 2(escape type) char*/
	UG_dtext  txprms;
	UG_FCDB   *prev_fcdb_p,*next_fcdb_p;

	uu_denter(UU_GTRC,(us,"ug_hershy(<%g,%g,%g> %s,%d)",
		(*prms).pos.x, (*prms).pos.y, (*prms).pos.z,
		(*prms).s,raster));
/*
.....Initialize variables for stroking text
*/
	status = ug_fchinit(prms);
	if (status!=0) goto exit;
	ug_gksstli.curprats.txbundl.fp.prec=UG_STRING;	/* temporarily */
/*
.....Save and set line style
*/
	if (!Scapture)
	{
		zbytecp(oldlstyle,*gqlinetype());
		lntype.typeno=1;
		lntype.npatn=0;
		gslinetype(&lntype);
	}
/*
.....String precision
*/
	if (txpr==UG_CHAR)
	{
		for (i=0; i<len; i++) {  /* for each char in string */
			txprms.slen=1;
			txprms.pos.x=chpos.x; 
			txprms.pos.y=chpos.y;
			txprms.s[0]=str[i];
			txprms.s[1]='\0';
			txprms.op=UG_DTEXT;
			ug_wkout(&txprms,sizeof(txprms));
			ug_movch(str[i],&chpos,path,
						0,0,0);  /* move chpos for this char */
		}             							/* for each char in string */
	}
/*
.....Stroke precision
*/
	else
	{
/*
........Save and set text color
*/
		if (!Scapture)
		{
			oldcolor = gqlinecolor();
			gslinecolor( gqtextcolor() );
		}
/*
........Loop through characters
*/
		UG_fcdb_p = UU_NULL;
		next_fcdb_p = UU_NULL;
		i = 0;
		while (i<len)                   /* find each character */
		{
			prev_fcdb_p = UG_fcdb_p;		/* save previous char */
/*
...........Already have current font character
*/
			if (next_fcdb_p!=UU_NULL)
			{
				chlen = next_chlen;
				UG_fcdb_p = next_fcdb_p;
			}
/*
...........Could not find character
...........Use blank character
*/
			else if ((chlen=ug_fchfind(str,i,&UG_fcdb_p))==0 )
			{
				chlen = 1;
				UG_fcdb_p = &dummy_fcdb;
			}
/*
...........Step over current character
*/
			i += chlen;
/*
...........Find next character
*/
			if ((i<len) && (next_chlen=ug_fchfind(str,i,&next_fcdb_p))==0)
			{
				next_chlen = 1;
				next_fcdb_p = &dummy_fcdb;
			}
			if ((path==UG_TP_LEFT)||(path==UG_TP_DOWN))  /* move before draw */
				ug_movch(str[i],&chpos,path,
						prev_fcdb_p,UG_fcdb_p,next_fcdb_p);
			ug_vcdrw((*prms).id,&chpos,str[i],chxfrm,raster);
			if ((path==UG_TP_RIGHT)||(path==UG_TP_UP))  /* move after draw */
				ug_movch(str[i],&chpos,path,
						prev_fcdb_p,UG_fcdb_p,next_fcdb_p);
		}
/*
........Reset line color
*/
		if (!Scapture) gslinecolor(oldcolor);
	}
/*
.....Restore precision & line style
*/
	ug_gksstli.curprats.txbundl.fp.prec=txpr;
	if (!Scapture) gslinetype(&oldlstyle);
/*
.....End of routine
*/
exit:
 uu_dexit;
	return( status);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_fchinit(prms) -- init for stroking chars.
**      Set various local variables for either stroking of text or
**      just computing text extent rectangle.
**      Variables set:  str        len          path           txpr
**                      txal       tyal         spc
**                      prchar_fb  UG_fb_p      dummy_fcdb
**                      fgridsize  fboxheight   fmid           fhalf
**                      fbase      ffontheight  fws
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : 0= font file found OK
**							1= font file not found (open error )
**							2= error reading font file
**							3= error allocating font block.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ug_fchinit(prms)				/* init to stroke string */
UG_dtext *prms;
{
	int       i;
	int       status;
	int       chlen;                 /* 1 or 2(escape type) char*/
	int       next_chlen;
	UG_FCDB   *next_fcdb_p,*prev_fcdb_p;

	uu_denter(UU_GITRC,(us,"ug_fchinit(%g %g,%s)",
		(*prms).pos.x,(*prms).pos.y,(*prms).s));

	status = 0;
	str  = (*prms).s;
	len  = strlen(str);
	path = ug_gksstli.curprats.txpath;							/* text path */
	txpr = ug_gksstli.curprats.txbundl.fp.prec;				/* text precision */
/*-------- get font block or fill in dummy for CHAR precision -------*/
	if (txpr==UG_STROKE)                  /* position to font block */
	{
		status = ug_vcfnd(ug_gksstli.curprats.txbundl.fp.font,&UG_fb_p);
		if (status!=0)
			goto exit;                   /* cannot continue */
	}
	else                              /* CHAR precision */
	{                                /* fill in dummy font block */
		prchar_fb.fh.boxheight = CGRIDSIZE;
		prchar_fb.fh.boxwidth = CGRIDSIZE;
		prchar_fb.fh.base_drop = CBASE;
		prchar_fb.fh.waist_drop = CFONTHEIGHT;
		prchar_fb.fh.width_type = 1;    /* fixed type font */
		UG_fb_p = &prchar_fb;          /* set current font block to dummy*/
	}
/*--------- fill in a dummy character block for char nofind ----------*/
	dummy_fcdb.fs = UU_NULL;          /* no strokes */
	dummy_fcdb.fc.nostroke = 0;
	dummy_fcdb.fc.width = UG_fb_p->fh.boxwidth/2;
/*--------- compute overall font coordinate values from font block ---*/
	fgridsize = (Gfloat)UG_fb_p->fh.boxwidth;
	fboxheight = (Gfloat)UG_fb_p->fh.boxheight;
	fmid = ((Gfloat)UG_fb_p->fh.boxwidth)/2.0;
	fhalf = ((Gfloat)UG_fb_p->fh.boxheight)/2.0;
	fbase = (Gfloat)UG_fb_p->fh.base_drop;
	ffontheight = (Gfloat)(UG_fb_p->fh.boxheight-UG_fb_p->fh.base_drop);
	fws = 0.0;                        /* no white space for now */

	txal = ug_gksstli.curprats.txalign.hor;					/* horiz alignment */
	tyal = ug_gksstli.curprats.txalign.ver;					/* vertical align */
	spc  = ug_gksstli.curprats.txbundl.space*ffontheight;	/* char spacing */

	/* Determine NORMAL values of txal and tyal */
	switch( path ){

		case UG_TP_RIGHT:
			if( txal == UG_TH_NORMAL ) txal = UG_TH_LEFT;
			if( tyal == UG_TV_NORMAL ) tyal = UG_TV_BASE;
			break;

		case UG_TP_LEFT:
			if( txal == UG_TH_NORMAL ) txal = UG_TH_RIGHT;
			if( tyal == UG_TV_NORMAL ) tyal = UG_TV_BASE;
			break;

		case UG_TP_UP:
			if( txal == UG_TH_NORMAL ) txal = UG_TH_CENTRE;
			if( tyal == UG_TV_NORMAL ) tyal = UG_TV_BOTTOM;
			break;

		case UG_TP_DOWN:
			if( txal == UG_TH_NORMAL ) txal = UG_TH_CENTRE;
			if( tyal == UG_TV_NORMAL ) tyal = UG_TV_TOP;
			break;

	}

	ug_chxfm(chxfrm,&(*prms).pos);	  /* calc xform from font to ndc space */

/*---- get max(min) x or y for extent computation ----------------------*/
	chpos.x = 0.0;
	chpos.y = 0.0;
	chpos.z = 0.0;
	if (txpr==UG_STROKE)							/* thru all char of string */
	{
		UG_fcdb_p = UU_NULL;
		next_fcdb_p = UU_NULL;
		i = 0;
		while (i<len)                   /* find each character */
		{
			prev_fcdb_p = UG_fcdb_p;		/* save previous char */
			if (next_fcdb_p!=UU_NULL) {   /* already have current */
				chlen = next_chlen;
				UG_fcdb_p = next_fcdb_p;
			}
			else if ( (chlen=ug_fchfind(str,i,&UG_fcdb_p))==0 ) { /*nofind*/
				chlen = 1;
				UG_fcdb_p = &dummy_fcdb;
			}
			i += chlen;							/* step over current char */
			if ( (i<len) && 					/* more char to go */
					(next_chlen=ug_fchfind(str,i,&next_fcdb_p))==0 ) {
				next_chlen = 1;
				next_fcdb_p = &dummy_fcdb;
			}
			ug_movch(str[i],&chpos,path,
						prev_fcdb_p,UG_fcdb_p,next_fcdb_p);
		}
	}

	/* Calculate extent, the length of the string in font units */
	if ((path==UG_TP_RIGHT)||(path==UG_TP_LEFT)) {
   	if (txpr==UG_CHAR)
			extent=fmid*len;
		else
			extent = fabs(chpos.x);        /* extent is width size */
	}
	else{					/* Vertical text */
   	if (txpr==UG_CHAR)
			extent = (fboxheight+spc) * len;
		else
			extent = fabs(chpos.y);        /* extent is heigth size */
	}

	chpos.x = 0.0;
	chpos.y = 0.0;
	chpos.z = 0.0;

	switch( path ){

	case( UG_TP_RIGHT ):
		switch( txal ){
			case UG_TH_LEFT:		break;
			case UG_TH_CENTRE:	chpos.x -= extent/2.0;		break;
			case UG_TH_RIGHT:		chpos.x -= extent;			break;
		}
		switch( tyal ){
			case UG_TV_TOP:		chpos.y -= fboxheight;		break;
			case UG_TV_HALF:		chpos.y -= fhalf;				break;
			case UG_TV_BASE:		chpos.y -= fbase;          break;
			case UG_TV_BOTTOM:	break;
		}
		break;

	case( UG_TP_LEFT ):
		switch( txal ){
			case UG_TH_RIGHT:		break;
			case UG_TH_CENTRE:	chpos.x += extent/2.0;		break;
			case UG_TH_LEFT: 		chpos.x += extent;			break;
		}
		switch( tyal ){
			case UG_TV_TOP:		chpos.y -= fboxheight;		break;
			case UG_TV_HALF:		chpos.y -= fhalf;				break;
			case UG_TV_BASE:		chpos.y -= fbase;          break;
			case UG_TV_BOTTOM:	break;
		}
		break;

	case( UG_TP_UP ):
		switch( txal ){
			case UG_TH_LEFT:		break;
			case UG_TH_CENTRE:	chpos.x -= fmid;				break;
			case UG_TH_RIGHT:		chpos.x -= fgridsize;			break;
		}
		switch( tyal ){
			case UG_TV_TOP:		chpos.y -= extent;			break;
			case UG_TV_HALF:		chpos.y -= extent/2.0;		break;
			case UG_TV_BASE:		chpos.y -= fbase;				break;
			case UG_TV_BOTTOM:	break;
		}
		break;

	case( UG_TP_DOWN ):
		switch( txal ){
			case UG_TH_LEFT:		break;
			case UG_TH_CENTRE:	chpos.x -= fmid;				break;
			case UG_TH_RIGHT:		chpos.x -= fgridsize;			break;
		}
		switch( tyal ){
			case UG_TV_TOP:		break;
			case UG_TV_HALF:		chpos.y += extent/2.0;		break;
			case UG_TV_BASE:		chpos.y += fbase;				break;
			case UG_TV_BOTTOM:	chpos.y += extent;         break;
		}
		break;

	}
/*------- function exit ----------------------------------------------*/
exit:
 uu_dexit;
	return( status);
}

/*********************************************************************
**    I_FUNCTION   :  ug_chxfm(chqfrm,gcurpos)
**			Calculate font to world/raster coord transformation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ug_chxfm(chqfrm,gcurpos)   /* calc font to out xform. account for
                 gcurpos, chplane, charup, chsize,ug_cxform[curvwindex].
                   		Origin of font coordinates is at gcurpos. */
Gfloat chqfrm[4][4];
Gnpoint3 *gcurpos;
{
	int i,j;
	Gfloat a[4][4];
	Gfloat b[4][4];
	Gfloat scl;
	static Gfloat origpt[3] = {0.,0.,0.};
	Gfloat theta;									/* rotation angle */
	Gfloat txpvec[3];							/* text plane normal vector */
	Gfloat txpvec_y[3];
	Gfloat txuv[3],txuv_proj[3];					/* text up vector */
	static Gfloat yaxis_def[3] = {0.,1.,0.};
	
	Gfloat ug_angle2p();

	uu_denter(UU_GITRC,(us,"ug_chxfm"));

	ug_ident(a);              /* a=identity */

	/* Scale text height into outcoord space */
	scl = ug_gksstli.curprats.txht/ffontheight;
	/* Scale taking char expansion into account for x-component of scale */
	ug_scale(scl * ug_gksstli.curprats.txbundl.expn, scl, scl, a);

	/*------- create b xform matrix- scaling and txplane rotation -----*/
	for (i=0; i<4; i++) {					/* start with scaled xform */
		for (j=0; j<4; j++)
			b[i][j] = a[i][j];
	}
	ug_unitvc( &ug_gksstli.curprats.txpvec, txpvec);
	ug_ztovec(b,txpvec);					/* add txplane rotation */
	/*------- Get txplane y axis ----------------------------------------*/
	ug_xform( yaxis_def[0],				/* txplane yaxis vector */
	          yaxis_def[1],
	          yaxis_def[2], txpvec_y, b);
	ug_unitvc(txpvec_y,txpvec_y);
	uu_dprint(UU_GITRC,
		(us,"ug_chxfm: txpvec=<%g,%g,%g>,txpvec_y=<%g,%g,%g>",
		txpvec[0],txpvec[1],txpvec[2],
		txpvec_y[0],txpvec_y[1],txpvec_y[2]));
	/*----- project text up vector onto text plane - use this as upvect */
	ug_vctovc(&ug_gksstli.curprats.txuv,txuv);
	ug_nptpln(txuv,origpt,txpvec,txuv_proj);
	ug_unitvc(txuv_proj,txuv_proj);
	uu_dprint(UU_GITRC,
		(us,"ug_chxfm: txuv=<%g,%g,%g>,txuv_proj=<%g,%g,%g>",
		txuv[0],txuv[1],txuv[2],
		txuv_proj[0],txuv_proj[1],txuv_proj[2]));
	/*------ get angle in text plane between regular y axis and text
	**       up vector (projected into plane above )- this is the angle
	**       we need to rotate plane about the zaxis. -----------------**/
	theta = ug_angle2p(txpvec_y,txuv_proj,txpvec);
	uu_dprint(UU_GITRC,
		(us,"ug_chxfm: theta=%g",theta));

	/******************
	theta = ug_atan2(ug_gksstli.curprats.txuv.y,
						  ug_gksstli.curprats.txuv.x) - 1.570796;
	***********************/

	/* Right-hand rotation! */
	ug_rot((UU_REAL) 0., (UU_REAL) 0., theta, a);			

	/* Apply rotation for character plane normal */
	ug_ztovec(a, &(ug_gksstli.curprats.txpvec));

	/* translate origin to gcurpos */
	ug_trans((*gcurpos).x,(*gcurpos).y,(*gcurpos).z,a); 
		
	/* Apply world to ndc xform -- Modeling should be included */
	/******ug_matmp(chqfrm,a,ug_cxform[ug_gksstli.curvwindex]);    */

	/* Since we took out the xform, let's copy */
	ug_mcopy(chqfrm,a);
	uu_dprint(UU_GITRC,(us,"ug_chxfm returns font to MC matrix:"));
	ug_matprt(chqfrm);
	uu_dprint(UU_GITRC,(us,"ug_chxfm. cxform[%d] matrix:",
			ug_gksstli.curvwindex)); 
	ug_matprt(ug_cxform[ug_gksstli.curvwindex]);
	uu_dexit;
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  ug_movch(c,chpos,path,prevch,curch,nextch) -- 
**										update chpos for char c.  Adjust if
**								have overlap type font and ajacent chars
**								are in shift_index array.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ug_movch(c,chpos,path,prevch,curch,nextch)
Gwpoint *chpos;
char c;
Gtxpath path;								/* text path */
UG_FCDB *prevch,*curch,*nextch;		/* previous,current,next char*/
{
	Gfloat fw,fh;
	int ishift;

	uu_denter(UU_GITRC,(us,"ug_movch(%c,%g %g)",c,(*chpos).x,(*chpos).y));

	if (ug_gksstli.curprats.txbundl.fp.prec==UG_CHAR) {
		fw=fmid; 
		fh=ffontheight;
	}
	else {                              /* STROKE precision */
		fh=fboxheight; 
		fw = UG_fcdb_p->fc.width - fws;
	}

	/* Take char spacing into account */
	fw += ug_gksstli.curprats.txbundl.space*ffontheight;
	fh += ug_gksstli.curprats.txbundl.space*ffontheight;

	switch (path) {
		case UG_TP_RIGHT:
			chpos->x += fw; 
			if ( UG_fb_p->fh.width_type==OVERLAP_TYPE && /* overlap font*/
				nextch!=UU_NULL &&				/* have next char */
				(ishift=shift_index[curch->fc.right_otype]
				 [nextch->fc.left_otype])>=0) /* 2 adjacents have overlap */
				chpos->x -= UG_fb_p->fh.oshift[ishift]; /* shift back */
			break;
		case UG_TP_LEFT:
			chpos->x -= fw;
			if ( UG_fb_p->fh.width_type==OVERLAP_TYPE && /* overlap font*/
				prevch!=UU_NULL &&				/* have previous char */
				(ishift=shift_index[curch->fc.right_otype]
				 [prevch->fc.left_otype])>=0) /* 2 adjacents have overlap */
				chpos->x += UG_fb_p->fh.oshift[ishift]; /* shift forward */
			break;
		case UG_TP_UP:		 chpos->y += fh; break;
		case UG_TP_DOWN:	 chpos->y -= fh; break;
	}

	uu_dexit;
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_vcfnd(k,&p) -- read kth font file and set
**      the font block address into p (if OK).
**      If font number (k) not in current list, read into memory
**      from font file.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : 0= font file found OK
**							1= font file not found (open error )
**							2= error reading font file
**							3= error allocating font block or char directory block or
**								strokes.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ug_vcfnd(k,fbp)				/* read kth font file */
int k;
UG_FB  *(*fbp);
{
	int        n;
	int        ic;
	int        isize;
	int        status;
	char      *uu_toolmalloc();
	UX_pathname filenv;	/* replaces char  filenv[UU_MAXPATHLEN] */
	UG_FCDB   (*cd)[];        /* pointer to char directory */
	UG_FCDB   *cdb;           /* individual char dir block */
	int ug_fontcmp();				/* Compare routine for sorting */

/*----------- begin function code -------------------------------*/
	 uu_denter(UU_GITRC,(us,"ug_vcfnd(%d)",k));
	status = 0;
/*--------- see if font already in memory -----------------------*/
	for ( UG_fb_p=UG_fb_h;               /* step thru font list */
			UG_fb_p!=UU_NULL; UG_fb_p=UG_fb_p->next)
	{
		if (k==UG_fb_p->fontnum)          /* this the fb we want */
			break;
	}
	if (UG_fb_p!=UU_NULL)  {             /* got a hit in loop */
		*fbp = UG_fb_p;                   /* set users pointer */
		goto exit;                        /* back we go */
	}
/*--------- get the font file name and open it -------------------*/
	sprintf(filenv,"FONT%d",k);         	/* make an env name for font*/

/***********************XIO******************************/
#if UU_COMP!=UU_WIN2K
	status = ux_open_to_data( filenv, "r", "STREAM",
					"BINARY", &fontlu, UX_PRTERRS);
#else
	status = ux_open_to_data( filenv, "rb", "STREAM",
					"BINARY", &fontlu, UX_PRTERRS);
#endif
	if (status!=0) {
		ug_errorhand( EFONTNF,"ug_vcfnd",0); /* give an error */
		status = 1;								/* set bad status */
		goto exit;                       /* cannot continue */
	}

/*	fontfd = open(fil,0,0);					 open fontfile readonly */

	/******NOTE: lseek NOT needed here when switching to XIO *********/
/*	lpos = lseek(fontfd,80,0);			 step over XIO header */
/*	if (lpos<0) goto rderror; */

/*--------- allocate font block and read info into it ------------*/
	UG_fb_p = (UG_FB *) uu_toolmalloc(sizeof(UG_FB)); /* get fb memory */
	if (UG_fb_p==NULL) {
		status = 3; goto exit;
	}
	if (UG_fb_h==UU_NULL)              /* 1st block in list */
		UG_fb_h = UG_fb_p;
	else
		UG_fb_t->next = UG_fb_p;        /* add to end(tail) of list */
	UG_fb_t = UG_fb_p;                 /* got new tail block */
	UG_fb_p->next = UU_NULL;           /* indicate this last block */
	UG_fb_p->fontnum = k;              /* set the font number */

/******************XIO**************************/
	n = 1;
	status = ux_read( fontlu, &UG_fb_p->fh, sizeof(UG_FONTHEAD), &n, UX_PRTERRS);
	if (status!=0) goto rderror;


/*--------- allocate character directory block and read in each ---*/
	isize = UG_fb_p->fh.nochar * sizeof(UG_FCDB);
	cd = UG_fb_p->cd = (UG_FCDB (*)[]) uu_toolmalloc(isize);
	if (UG_fb_p->cd == NULL) {
		status = 3; goto exit;
	}
	for (ic=0; ic<UG_fb_p->fh.nochar; ic++) /* through each character */
	{
	/*******************XIO******************************************/
		n = 1;
		status = ux_read( fontlu,
			&(*cd)[ic].fc, sizeof(UG_FCHARHEAD), &n, UX_PRTERRS );
		if (status!=0) goto rderror;
	}
/*--------- read in stroke info for each character -----------------*/
	for (ic=0; ic<UG_fb_p->fh.nochar; ic++) /* thru characters */
	{
		cdb = &(*cd)[ic];              /* current char dir block */
		isize = cdb->fc.nostroke*sizeof(UG_FSTROKE); /* size of strokes */
		cdb->fs =                      /* get memory for strokes */
			(UG_FSTROKE (*)[]) uu_toolmalloc(isize);
		if (cdb->fs == NULL) {
			status = 3; goto exit;
		}
/*******************XIO******************************/
		status = ux_read( fontlu, cdb->fs, isize, &n, UX_PRTERRS);
		if (status!=0) goto rderror;

	}
/*******************XIO*********	*************************/
	ux_close(fontlu);

/*	close(fontfd); */
	*fbp = UG_fb_p;                 /* set caller pointer to font blk*/

/*------------ Sort font entries	-----------------------------------*/
#if (UU_COMP != UU_WINNT) && (UU_COMP != UU_WIN2K)
	uu_qsort(UG_fb_p->cd , UG_fb_p->fh.nochar, sizeof(UG_FCDB), ug_fontcmp);
#endif

   for(ic=0;ic<256;ic++)
      UG_fb_p->ascii[ic] = NULL;

/*   while((*cd)[UG_fb_p->asc_inc].fc.fcharesc == '\0') */
/* Mills - Do not read more than number of characters read in to prevent random memory
			  corruption */
   for (ic=0; ic<UG_fb_p->fh.nochar && (*cd)[ic].fc.fcharesc == '\0'; ic++)
      {
      UG_fb_p->ascii[(int) (*cd)[ic].fc.fchar] = &((*cd)[ic]);
      }

   UG_fb_p->asc_inc = ic;
	
/*------------------ Exit ------------------------------------------*/
	goto exit;
/*------------ read error exit -------------------------------------*/
	rderror:
	status = 2;									/* set bad status */
	ug_errorhand( EFONTRERR,"ug_vcfnd",0); /* give an error */
/*******************XIO***********************************/
	ux_close(fontlu);
/*	close(fontfd); */
	
/*------------ function exit ---------------------------------------*/
	exit:
	uu_dprint(UU_GITRC,(us,"ug_vcfnd return status=%d",status));
	uu_dexit;
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  ug_vcdrw(wsid,pos,c,chqfrm,raster)
**     Draw char c at pos.  Current font character block is pointed to
**     by UG_fcdb_p. If raster=1 then output to ug_rasline
**    PARAMETERS   
**       INPUT  : 
**				wsid				workstation id
**				pos				current font coord position to draw c
**				c					character to draw
**				chqfrm			transformation to outcoord (raster/world)
**				raster			1=output is raster coord
**									other= world coord
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ug_vcdrw(wsid,pos,c,chqfrm,raster)
Gnpoint *pos;
char c;
Gws wsid;
Gfloat chqfrm[4][4];					/* font to ndc transformation */
int		raster;							/* 1=raster, other=world coord */
{
	int       i;
	int       itype;                 /* 0= line, 1= arc */
	int       icmd;                  /* move,draw or direction for arc */
	int		ibuffpt;						/* point buffer- 1st avail */
	int       is;
	int       nsides;                /* number sides of arc */
	Gfloat     x1,y1;
	Gfloat    angle;                 /* angle of arc- clockw or countercw*/
	Gfloat    dangle;                /* polygon angle for each side ofarc*/
	Gfloat    radius;                /* radius of arc */
	Gfloat    ap1ang,ap2ang;         /* start and end arc point angles*/
	Gfloat    arcxfrm[2][2];         /* rotate xform for arc delta */
	Gfloat	 ug_acos();
	Gnpoint3  p1,p2;
	Gwpoint3  points[VCDRW_BUFFSIZE];	/* Buffer for ug_polyln3 call */
	Gipoint	 ipoints[VCDRW_BUFFSIZE];	/* Buffer for ug_polylnras call */
	Gwpoint   ce;                    /* center of arc (font coord) */
	Gwpoint   curp;                  /* current stroke font coord */
	Gwpoint   ap1,ap2;               /* arc start and end points */
	UG_FSTROKE     (*fs)[];          /* char stroke array */
	UG_FSTROKE     *fsb;             /* char stroke array element */
/*------------- begin function code --------------------------------*/
	 uu_denter(UU_GITRC,(us,"ug_vcdrw(%d,%g %g,%c,chqfrm)",
		wsid,(*pos).x,(*pos).y,c));
/*
.....Store initial point
*/
	ibuffpt = 0;								/* init point buffer 1st avail */
	curp.x = curp.y = 0.0;           /* init current char font coord */
	ug_xform((*pos).x,(*pos).y,(UU_REAL) 0.0,&p1,chqfrm);
	if (Scapture) S_capture_point(&p1);
	else ug_vcdrw_ptadd(&ibuffpt,ipoints,points,&p1,raster);
	fs = UG_fcdb_p->fs;              /* get current chars stoke array*/
	for (is=0; is<UG_fcdb_p->fc.nostroke; is++) /* THRU strokes*/
	{
		fsb = &(*fs)[is];             /* current stroke */
		itype = fsb->x & 1;          /* get low bit- line or arc */
		x1 = (fsb->x >> 1);           /* shift x coord - rid bit flag */
		icmd = fsb->y & 1;           /* get low bit command */
		y1 = (fsb->y >> 1);           /* shift y coord - rid bit flag */
/*----- LINE command - either move or draw ------------------------*/
		if (itype==LINECMD)           /* line command- move or draw */
		{
			uu_dprint(UU_GITRC,(us,"ug_vcdrw line stroke cmd=%d,x1=%g,y1=%g",
							icmd,x1,y1));
			ug_xform((*pos).x+x1,(*pos).y+y1,(UU_REAL) 0.0,&p2,chqfrm);
			/*--- MOVE - draw what is in buffer and reset --------------*/
			if (icmd==LINEMOVE)       /* line move */
			{
				if (Scapture) S_capture_poly();
				else
				{
					if (ibuffpt>1)				/* something in buff- draw it */
					{
						if (raster==1)
							ug_polylnras(ibuffpt,ipoints);
						else
							ug_polyln3(ibuffpt,points);
					}
				}
				ibuffpt = 0;					/* reset buffer */
			}
			if (Scapture) S_capture_point(&p2);
			else ug_vcdrw_ptadd(&ibuffpt,ipoints,points,&p2,raster);
			zbytecp(p1,p2);							/* structure assignment */
		}
/*-----------------------------------------------------------------*/
/*        ARC command - clock or counterclock wise direction       */
/*  Draw arc as nsided polygon in arc center (origin) coordinate */
/*-----------------------------------------------------------------*/
		else if (itype==ARCCMD)       /* arc command- center and endpt*/
		{
			ce.x = x1; ce.y = y1;      /* save arc center */
/*------ get arc end point (next stroke x,y coord)------------------*/
			is++;                      /* to next stroke- arc endpt */
			fsb = &(*fs)[is];          /* current stroke */
			x1 = (fsb->x >> 1);        /* shift x coord - rid bit flag */
			icmd = fsb->y & 1;         /* get arc direction 0=CCW,1=CW */
			y1 = (fsb->y >> 1);        /* shift y coord - rid bit flag */
			uu_dprint(UU_GITRC,
				(us,"ug_vcdrw arc stroke cmd=%d,cx=%g,cy=%g,x1=%g,y1=%g",
				icmd,ce.x,ce.y,x1,y1));
/*------ change curp and x1,y1 relative to arc center- ap1,ap2 --*/
			ap1.x = curp.x-ce.x;  ap1.y = curp.y-ce.y;
			ap2.x = x1-ce.x;      ap2.y = y1-ce.y;
/*------ get angle between 2 points ------------------------------*/
			radius = sqrt(ap1.x*ap1.x + ap1.y*ap1.y); /* arc radius */
			ap1ang = ug_acos(ap1.x/radius);    /* start point angle */
			if (ap1.y<0.0)                  /* point in 3rd,4th quadrant*/
				ap1ang =  TWOPI - ap1ang;    /* ang btwn PI and TWOPI*/
			ap2ang = ug_acos(ap2.x/radius);    /* end point angle */
			if (ap2.y<0.0)
				ap2ang =  TWOPI - ap2ang;    /* ang btwn PI and TWOPI*/
			angle = ap2ang-ap1ang;
			if (angle==0.0)  {              /* beg=end: have full circle */
				icmd = ARCDIRCCW;            /* force counter clockwise */
				angle = TWOPI;               /* want full 2 PI angle */
			}
			if (icmd==ARCDIRCCW) {          /* counterclockwise: >0 ang*/
				if (angle<0.0)               /* ang p1->p2 is clockwise*/
					angle = TWOPI + angle;
			}
			else   {                        /* clockwise : <0 angle */
				if (angle>0.0)               /* ang p1->p2 is countercw*/
					angle = TWOPI - angle;
			}
			angle = fabs(angle);           /* make sure its positive*/
/*------- get delta angle and number sides --------------------*/
			dangle = 2.0 * ug_acos(1.0-UG_farcerr); /*ang for specified radius err*/
			nsides = angle/dangle;
			if (nsides<2)                   /* want a couple of sides atleast*/
				nsides = 2;
			dangle = angle/nsides;
			if (icmd==ARCDIRCW)           /* clockwise */
				dangle = -dangle;          /* clockwise is negative ang*/
			uu_dprint(UU_GITRC,
				(us,"ug_vcdrw arc rad=%g,p1ang=%g,p2ang=%g,ang=%g,nside=%d,dang=%g",
				radius,ap1ang,ap2ang,angle,nsides,dangle));
/*------- Compute xform matrix for incremental rotate by dangle */
/*        rotate x1 to x2 by angle:                             */
/*           x2 =  x1*cos(angle)  -  y1*sin(angle)              */
/*           y2 =  x1*sin(angle)  +  y1*cos(angle)             */
/*--------------------------------------------------------------*/
			arcxfrm[0][0] = cos(dangle);
			arcxfrm[1][1] = arcxfrm[0][0];   /* cos(dangle) also */
			arcxfrm[0][1] = sin(dangle);
			arcxfrm[1][0] = -arcxfrm[0][1];  /* - sin(dangle) */
/*--------------------------------------------------------------*/
/*     Rotate start point ap1 nsides times through angle dangle */
/*     p1 is current NDC coordinate. Xform to p2 like for line  */
/*     1) rotate ap1 to ap2 by dangle - in arc center coordinates*/
/*     2) add arc center to get back to font char coordinates */
/*     3) add font char coord to current font coordinate and  */
/*        transform to NDC- draw line */
/*--------------------------------------------------------------*/
			for( i=0; i<nsides; i++)         /* draw arc as nsides poly*/
			{
				ap2.x = ap1.x*arcxfrm[0][0] + ap1.y*arcxfrm[1][0];
				ap2.y = ap1.x*arcxfrm[0][1] + ap1.y*arcxfrm[1][1];
				x1 = ce.x + ap2.x;            /* back to font char coord */
				y1 = ce.y + ap2.y;
				ug_xform((*pos).x+x1,(*pos).y+y1,(UU_REAL) 0.0,&p2,chxfrm);
				ap1.x = ap2.x;  ap1.y = ap2.y; /* set new arc point */
				if (Scapture) S_capture_point(&p2);
				else ug_vcdrw_ptadd(&ibuffpt,ipoints,points,&p2,raster);
				zbytecp(p1,p2);					/* p2=p1 new current NDC pt*/
			}                                /* end draw nsides of arc */
		}												/* end arc command */
		curp.x = x1;  curp.y = y1;   /* save current char font coord */
	}                                /* end stroke loop */ 
	/*----------- purge point buffer if something still in it --------*/
	if (Scapture) S_capture_poly();
	else
	{
		if (ibuffpt>=1) {						/* something in point buffer */
			if (raster==1)
				ug_polylnras(ibuffpt,ipoints);
			else
				ug_polyln3(ibuffpt,points);
		}
	}
	uu_dexit;
	return (0);
}
/*********************************************************************
**    I_FUNCTION :  ug_vcdrw_ptadd(&ibuffpt,ipoints,points,p1,raster)
**       Add point p1  to ipoints/points buffer depending on 
**			raster type. Bump ibuffpt
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ug_vcdrw_ptadd(ibuffpt,ipoints,points,p1,raster)
int			*ibuffpt;						/* current buffer index */
Gipoint	ipoints[];						/* raster array of points */
Gwpoint3	points[];						/* world coord array of points */
Gnpoint3	*p1;								/* polyln point */
int			raster;							/* 1=raster,other=world */
{
	uu_denter(UU_GITRC,(us,"ug_vcdrw_ptadd(%d,,,<%g,%g,%g>,%d)",
		*ibuffpt, p1->x, p1->y, p1->z, raster));
	/*---------- raster coordinates here- add to ipoints ----------*/
	if (raster==1) {
		/*----- buffer full,draw,polyln continues with last point ------*/
		if (*ibuffpt>=VCDRW_BUFFSIZE) {	/* buffer full */
			ug_polylnras(*ibuffpt,ipoints);
			ipoints[0].x = ipoints[*ibuffpt-1].x;	/* last -> first */
			ipoints[0].y = ipoints[*ibuffpt-1].y;
			*ibuffpt = 1;
		}
		ipoints[*ibuffpt].x = p1->x;
		ipoints[*ibuffpt].y = p1->y;
		(*ibuffpt)++;
	}
	/*----------- world coordinates here - add to points -----------*/
	else {
		/*----- buffer full,draw,polyln continues with last point ------*/
		if (*ibuffpt>=VCDRW_BUFFSIZE) {
			ug_polyln3(*ibuffpt,points);
			points[0].x = points[*ibuffpt-1].x;	/* last -> first */
			points[0].y = points[*ibuffpt-1].y;
			points[0].z = points[*ibuffpt-1].z;
			*ibuffpt = 1;
		}
		points[*ibuffpt].x = p1->x;
		points[*ibuffpt].y = p1->y;
		points[*ibuffpt].z = p1->z;
		(*ibuffpt)++;
	}
	uu_dexit;
	return (0);
}

/*********************************************************************
**    I_FUNCTION :  int  ug_fchfind(s,cur,fcdbp)
**       Find character to be stroked s[cur] in the current font's
**       character directory. If find the current character, set
**       the char dir block pointer in fcdbp and return the length
**       of the character: 1 or 2(if escape code).
**    PARAMETERS   
**       INPUT  : 
**          char  s[] -    characters to be stroked.
**          int   cur -   current element in s.
**       OUTPUT :  
**    RETURNS      : 0= character not found
**                   1= single character found, fcdbp set.
**                   2= double(escape+char) found, fcdbp set.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ug_fchfind( s, cur, fcdbp)
/*$ INPUT */
char     s[];                    /* array of chars to be stroked */
int      cur;                   /* current element of s */
UG_FCDB  *(*fcdbp);              /* pointer for current char block*/
{
	int     ic;
	int     chlen;
	UG_FCDB (*cd)[];                /* temp to char directory */
	UG_FCDB *cdb;                   /* temp to char direct block */
	int lowlim, uplim;				 /* Search limits for binary search */
/*---------- begin function code ----------------------------------*/
	uu_denter(UU_GITRC,(us,"ug_fchfind(%x,%d),c=\"%c\"",
		s,cur,s[cur]));
	chlen = 0;                       /* init to char not found */
	cd = UG_fb_p->cd;                /* get current char directory ptr*/
	if(UG_fb_p->ascii[(int)s[cur]] != NULL)
		{
		cdb=UG_fb_p->ascii[(int)s[cur]];
		uu_dprint(UU_GITRC,(us,"ug_fchfind: Char in ascii array"));
		chlen = 1;
		}
/*
.....Do binary search on remaining chars
*/
	else if (UG_fb_p->asc_inc != UG_fb_p->fh.nochar)
		/* NOTE this may be made more efficient by comparing
			both normal and esc chars as shorts					*/
		{
		int saveic = -1;
		int saveic1 = -1;

		cdb = NULL;
		ic = (UG_fb_p->fh.nochar-UG_fb_p->asc_inc)/2 + UG_fb_p->asc_inc; 
		lowlim = UG_fb_p->asc_inc;
		uplim = UG_fb_p->fh.nochar;
		while(chlen == 0)
			{
			uu_dprint(UU_GITRC,(us,"ug_fchfind: char index = %d",ic));
			if((saveic == ic)||(saveic1 == ic)) break;
			saveic1 = saveic;
			saveic = ic;
			cdb = &(*cd)[ic];
			uu_dprint(UU_GITRC,(us,"srch char = %c  %c",
											cdb->fc.fcharesc,cdb->fc.fchar));
			if (cdb->fc.fcharesc==s[cur] &&
					cdb->fc.fchar==s[cur+1] )    /* found it! */
				{
				chlen = 2;
				}
			else
				{
				if(cdb->fc.fcharesc < s[cur])
					{
					lowlim=ic+1;
					ic = (uplim + lowlim)/2;
					}
				else 
					{
					if(cdb->fc.fcharesc > s[cur])
						{
						uplim = ic-1;
						ic = (uplim + lowlim)/2;
						}
					else
						{
						if(cdb->fc.fchar < s[cur+1])
							{
							lowlim=ic+1;
							ic = (uplim + lowlim)/2;
							}
						else
							{
							uplim = ic-1;
							ic = (uplim + lowlim)/2;
							}
						}
					}
				}
			} 
		} 
	if (chlen>0)                       /* character found */
	{
		*fcdbp = cdb;                /* set current char direct block*/
		uu_dprint(UU_GITRC,(us,"ug_fchfind nostr=%d,w=%d,lo=%d,ro=%d",
			cdb->fc.nostroke,cdb->fc.width,
			cdb->fc.left_otype,cdb->fc.right_otype));
	}
	else                               /* didnot find it */
	{
		*fcdbp = UU_NULL;
	}
/*---------- function exit ----------------------------------------*/
 uu_dprint(UU_GITRC,(us,"ug_fchfind return length=%d",chlen));
	uu_dexit;
	return(chlen);
}
/*********************************************************************
**    I_FUNCTION     :  ug_fchext(prms,concat,extrect)
**       Get text extent rectangle.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_fchext(prms,concat,extrect)
UG_dtext *prms;
Gwpoint3 *concat;
Gwrect3  *extrect;
{
		
	int       status;
	Gnrect3   chrectndc;             /* text rectangle in ndc coord */
	Gnpoint3  chposccndc;            /* concat point in ncd coord */
	Gwpoint3  chposur;               /* upper right in font coord */
	Gwpoint3  chposcc;					/* concat point in font coord */

	uu_denter(UU_GITRC,(us,"ug_fchext(%g %g,%s)",
		(*prms).pos.x,(*prms).pos.y,(*prms).s));

	status = ug_fchinit(prms);        /* go init vars for stroking */
	if (status!=0)
		goto exit;								/* cannot continue */

/*------ fix chpos to lower left for UP and DOWN paths --------------*/
	if (path == UG_TP_UP || path == UG_TP_DOWN) chpos.y -= extent;

 chposcc.x = 0.0;							/* init concat to origin */
	chposcc.y = 0.0;
	chposcc.z = 0.0;
	switch( path )								/* get upper right position */
	{
	 case UG_TP_RIGHT:							/* extent is x width */
	 case UG_TP_LEFT:
		chposur.x = chpos.x + extent;    /* right x font coord */
		chposur.y = chpos.y + fboxheight; /* upper y font coord */
		chposur.z = 0.0;
		chposcc.x -= extent;          /* adjust to the left */
		break;
	 case UG_TP_UP:								/* extent is y height */
	 case UG_TP_DOWN:
		chposur.x = chpos.x + fgridsize; /* width of vertical text */
		chposur.y = chpos.y + extent;    /* upper y font coord */
		chposur.z = 0.0;
		chposcc.y -= extent;
		break;
	}
/*-------- transform font coordinates to ndc coord ------------------*/
	ug_xform(chpos.x,chpos.y,chpos.z,&chrectndc.llf,chxfrm); /* lower left */
	ug_xform(chposur.x,chposur.y,chposur.z,&chrectndc.urb,chxfrm);
	ug_xform(chposcc.x,chposcc.y,chposcc.z,&chposccndc,chxfrm);

/* This transform now really goes from font to world...need to copy data */
extrect->llf.x = chrectndc.llf.x;
extrect->llf.y = chrectndc.llf.y;
extrect->llf.z = chrectndc.llf.z;
extrect->urb.x = chrectndc.urb.x;
extrect->urb.y = chrectndc.urb.y;
extrect->urb.z = chrectndc.urb.z;
concat->x = chposccndc.x;
concat->y = chposccndc.y;
concat->z = chposccndc.z;

/* Don't need this anymore */
/*-------- now transform back to world coordinates for caller --------*/
/*	gndcw3( &extrect->llf.x, &extrect->llf.y, &extrect->llf.z,*/
/*				chrectndc.llf.x, chrectndc.llf.y, chrectndc.llf.z);*/
/*	gndcw3( &extrect->urb.x, &extrect->urb.y, &extrect->urb.z,*/
/*				chrectndc.urb.x, chrectndc.urb.y, chrectndc.urb.z);*/
/*	gndcw3( &concat->x, &concat->y, &concat->z,*/
/*				chposccndc.x, chposccndc.y, chposccndc.z);*/

/*------- function exit -----------------------------------------------*/
exit:
 uu_dexit;
	return( status);
}

/*********************************************************************
**    I_FUNCTION     :  ug_fontcmp(p1,p2)
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_fontcmp(p1,p2)
UG_FCDB *p1,*p2;
{
	short *cmp1, *cmp2;
	int rtn;

#if ((UU_COMP == UU_VAXVMS)||(UU_COMP == UU_VAXULTRIX))
   if (p1->fc.fcharesc > p2->fc.fcharesc)
      {
      rtn = 1;
      goto ret;
      }
   if (p1->fc.fcharesc < p2->fc.fcharesc)
      {
      rtn = -1;
      goto ret;
      }
   if (p1->fc.fchar > p2->fc.fchar)
      {
      rtn = 1;
      goto ret;
      }
   if (p1->fc.fchar < p2->fc.fchar)
      {
      rtn = -1;
      goto ret;
      }
   rtn = 0;
#else

	cmp1 = (short *)&p1->fc.fcharesc;
	cmp2 = (short *)&p2->fc.fcharesc;

	if(*cmp1==*cmp2) rtn = 0;
	else 
		{
		if(*cmp1<*cmp2)  rtn = -1;
			else rtn = 1;
		}
#endif
ret:;
	return(rtn);
}

/*********************************************************************
**    E_FUNCTION     :  ug_text_capture(flag)
**       Causes the polylines of a drawn (stroked) text string to be
**       stored in a display list rather than being drawn to the
**       output device.
**    PARAMETERS   
**       INPUT  : 
**          flag    = UU_TRUE = Initiate capture, UU_FALSE = Terminate
**                    capture.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_text_capture(flag,ptlist)
UU_LOGICAL flag;
UU_LIST *ptlist;
{
	Gwpoint3 ptx;
/*
.....Initiate the text capture sequence
*/
	if (flag)
	{
		uu_list_init(ptlist,sizeof(Gwpoint3),200,50);
		Sptlist = ptlist;
		ptx.x = ptx.y = ptx.z = 0.;
		uu_list_push(Sptlist,&ptx);
		uu_list_push(Sptlist,&ptx);
		Scapture = UU_TRUE;
		Scurpoly = 1;
		Snpts = 0;
	}
/*
.....Terminate the text capture sequence
*/
	else
	{
		if (ptlist != UU_NULL) uu_list_free(ptlist);
		Scapture = UU_FALSE;
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     :  ug_text_iscaptured(flag)
**       Determines if text is being captured.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE when text is being captured.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ug_text_iscaptured()
{
	return(Scapture);
}

/*********************************************************************
**    I_FUNCTION     :  S_capture_poly()
**       Pushes the polyline count onto the capture point list.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void S_capture_poly()
{
	Gwpoint3 *pts,ptx;
/*
.....Store polyline count onto point list
*/
	if (Snpts != 0)
	{
		pts = (Gwpoint3 *)UU_LIST_ARRAY(Sptlist);
		pts[0].x = pts[0].x + 1;
		pts[Scurpoly].x = Snpts;
		Scurpoly = Scurpoly + Snpts + 1;
		ptx.x = ptx.y = ptx.z = 0.;
		uu_list_push(Sptlist,&ptx);
		Snpts = 0;
	}
}

/*********************************************************************
**    I_FUNCTION     :  S_capture_point(pt)
**       Pushes a point onto the capture point list.
**    PARAMETERS   
**       INPUT  : 
**          pt       = Point to store in list.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void S_capture_point(pt)
Gwpoint3 *pt;
{
/*
.....Store polyline count onto point list
*/
	uu_list_push(Sptlist,pt);
	Snpts++;
}

