/*********************************************************************
**    NAME         :  gviw.c -- DIGS "traverser" functions
**       CONTAINS:
**		gfindndc(mode,loc,epsx,epsy,fctn,vis) 
**		int ug_finxy(n,ats,nx,ny,eps,seg,seglen,pkid,dist) 
**		ug_viewws(n,ws) -- view segment n on workstation ws.
**		ug_view0(n,erase) -- view segment n with default seg atts.
**		ug_views(n,ats,erase) -- view segment n with seg attributes.
**		ug_viewsg(n,ats,erase) -- view or find segment n.
**		ug_viewsgpk(n,ats,erase)  
**  	gextrema(xform,rect) -- extrema NDC bounding box
**    int ug_segextrema(segptr,rect) - expand world coordinate rect for a seg.
**    ug_boxexpnt3(box,n,pts) -- expand box by pts.
**    ug_boxexpnt2(box,n,pts) -- expand box by pts.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       gviw.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:26
*********************************************************************/
#include "usysdef.h"
#include "zsysdep.h"
#include "udebug.h"
#define true 1
#define false 0
#define logical int
#include "umath.h"
#include "gtbl.h"
#include "g.h"
#include "ginq.h"
#include "gdidd.h"
#include "gvlib.h"
#include "gviw.h"
#include "gsegop.h"
#include "gsegac.h"
#include "gmat4.h"
#include "gconvert.h"

		/* used for conversion routines - to free used memory */
static Gwpoint3 *temp3;
static Gwpoint *temp;

extern int NCL_mot_seg;
extern int UN_override_geo_attr[2],UN_override_geo_mask;
extern UG_findit ug_find;
extern int ug_viwseg;	 /* segment number for output clip/exp routines */
UU_LOGICAL T_PLOT = UU_FALSE;
static Gint vlst[10];    /* list of active activations of ug_viewsg */
static Gint vlen=0;      /* length of vlst and mod xform stacks */
#if UU_DEBUG==1
char ug_ops[74][16]={		/* character op-codes, for debugging */
	/* opcodes 0-41, output primitives */
	"UG_NOOP","UG_PAGEOP"," "," "," "," "," "," "," "," "," "," "," "," ",
	"UG_PLYLNA3OP","UG_PLYLNA2OP","UG_PLYLNRASOP"," ","UG_PLYMKA3OP",
 	"UG_PLYMKA2OP","UG_PLYMKRASOP","UG_TEXTRASOP","UG_TEXTOP","UG_PROCOP",
 	"UG_CALLOP","UG_SNTRANOP","UG_FLAREAOP","UG_FLAREA3OP","UG_FLAREARASOP",
	"UG_CELL3OP","UG_CELLOP","UG_CELLRUNOP3","UG_CELLRUNOP","UG_CELLRASOP",
	"UG_CELLRUNRASOP"," "," "," "," "," "," "," ",
	/*  opcodes 42-73 attributes */
	"UG_DFATSOP","UG_COLOROP","UG_INTENSOP","UG_LSTYLOP","UG_LWIDOP",
 	"UG_PENOP","UG_FONTOP","UG_CHHGTOP","UG_CHPLANEOP","UG_CHUP2OP",
 	"UG_CHUP3OP","UG_CHPATHOP","UG_CHSPACEOP","UG_CHJUSTOP",
	"UG_CHPRECOP","UG_SYMBOLOP","UG_PICKIDOP","UG_LNCOLROP","UG_MKCOLROP",
	"UG_TXCOLROP","UG_FACOLROP","UG_CHEXPOP","UG_MTRANOP","UG_EDGEFLAGOP",
	"UG_LMTRANOP"," "," "," "," "," "," "," "
};
#endif

typedef struct {int id; Gws ws; Gseg n;} Stcallseg;

void ug_view0(),ug_viewsgpk(),ug_boxexpnt3(),ug_goxexpnt2(),ug_mctextbox();
void ug_unitvec(),ug_boxexpnt2();

/********************************************************************* 
**  E_FUNCTION:  gfindndc(mode,loc,epsx,epsy,fctn,vis) 
**
**      Find all graphics primitives within or without a rectangle
**			centered at loc, extending distances epsx,epsy. Each time
**			something found, call fctn.
**  PARAMETERS   
**      INPUT:  int mode -- 0=partially within rectangle, 1=at least
**							partially without, 2=wholly within, 
**							3=wholly without. Only modes 0 and 3
**							currently implemented.
**					 Gnpoint *loc -- center of search rectangle.
**					 int epsx,epsy -- half-lengths of search rectangle.
**					 int (*fctn)(len,segs,find) --  call this function
**							each time something found. 
**							len=length of array segs.
**							segs=integer array of segment numbers of the thing found.
**							pickid=pick id of the thing found.
**							find is a pointer to a structure defined as follows:
								typedef struct {
    								int find; 			 always 1
    								int found;			 always 1
    								Gfloat epsx,epsy;	 pick half-aperture 
    								Gfloat x,y;			  loc 
	 								Gfloat dist;			 dist from loc of thing found
	 								int pickid;			 pickid of thing found 
	 								int (*func)();		 function called 
	 								int pikmode;		 mode 
									int vis;				 0=vis, 1=invis, 2=both.
								} UG_findit;
**							Use this structure to obtain the pickid and distance
**							from loc of thing found. Also, the called function should
**							set find to 0 if it wishes the search to end.
**							By default, the search
**							rectangle will be decreased in size to the distance
**							of the thing found. Reseting epsx,epsy is required to keep
**							the search rectangle the same size for subsequent
**							items. 
**							
**									
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  Only mode 0 (partially within) implemented to date.
*********************************************************************/
void gfindndc(mode,loc,epsx,epsy,fctn,vis)
/*$ INPUT */
int mode;					/* 0=partially within, 1=partially without, 
									2=wholly within, 3=wholly without */
Gnpoint *loc;				/* NDC position */
Gfloat epsx,epsy;			/* half-distances away from loc */
int (*fctn)();				/* call this fctn upon finding something */
int vis;						/* 0=vis, 1=invis, 2=both */
{
	int i;
	int segno;				/* Segment number to search */
	UG_segstli *p;
	UG_findprms findprms;

	uu_denter(UU_GTRC,(us,"gfindndc(%d,%g,%g,%g,%g,%d,%d)",mode,(*loc).x,
					(*loc).y,epsx,epsy,fctn,vis));
	ug_find.x=(*loc).x; ug_find.y=(*loc).y;
	ug_find.find=1;
	ug_find.func=fctn;
	ug_find.pikmode=mode;
	ug_find.vis=vis;

	uu_dprint(UU_GITRC,(us,"vislistok=%d",ug_vislistok));
	for( ug_vislistok ? i=0 : ug_seginitscan();
		  ug_vislistok ? i<UU_IALEN(ug_vislist) : (p=ug_segscan())!=NULL;
		  i++ ) {

		if( ug_vislistok ) {
			segno = UU_IAVAL(ug_vislist, i);
		}
		else {
			segno = p->segid;
		}

		if (segno<UG_MAXSEGNO) {			/* don't look at "fake" icon segments */
			ug_find.epsx=epsx; ug_find.epsy=epsy;
			ug_find.found=0;
			findprms.curxform=ug_gksstli.curvwindex;
			findprms.pkid=ug_gksstli.curprats.pickid;
			zbytecp(findprms.prats,ug_gksstli.curprats);
			ug_ident(findprms.gmodxf);
			vlen=0;
			ug_findsgpk(segno,&findprms,vlst,&vlen);	/* search segment segno */
/* 
.............Improved picking - continue to search all segments to find closest one
.............instead of taking first one....RAZ
.............  if (ug_find.find==0) break;
*/
		}
	}
	ug_find.find=0;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_finxy(n,ats,nx,ny,eps,seg,seglen,pkid,dist) 
**			Find line or text near (nx,ny) in segment n.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT : int seg[10] -- array of segments found item is in.
**						int seglen -- size of seg.
**    RETURNS      : 1 if found, else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_finxy(n,ats,nx,ny,eps,seg,seglen,pkid,dist) /* find line or text within 
											 eps of (nx,ny) ndc coords in segment n.
                                  Return 1 if found, else 0. If found,
                                  return seg=array of instances of
                                  segments active when found.
											 pkid=current pick id, dist=distance */
int n,seg[10],*seglen,*pkid;
UG_segat *ats;
Gfloat nx,ny,eps,*dist;
{ 
	int i;
	UG_segstli *segptr;
	int inbox;
	UG_findprms findprms;
/*
.....Routine to call to verify pick as valid against active selection filter.
.....Located in das/d5filt.c.  - RAZ.
*/
	int pick_select_verify();
	
	uu_denter(UU_GITRC,(us,"ug_finxy(%d,ats,%g,%g,%g,seg,seglen,pkid)",
							n,nx,ny,eps));
	ug_find.epsx=eps; ug_find.epsy=eps;
	ug_find.x=nx; ug_find.y=ny;
	ug_find.find=1;					/* turn on switch to find stuff */
	vlen=0;
	ug_find.found=0;
/*
.....Define function pick_select_verify() to determine if we have a valid
.....Entity pick.  This is for improved pick - using the DAS select filter
.....to search through the near entities until a valid pick is found (if
.....possible.
.....WAS:
..........ug_find.func=NULL;
.....NOW:
*/
	ug_find.func = pick_select_verify;

	ug_find.pikmode=0;				/* find partially within */
	*seglen=0;
	if ((segptr=ug_segac(n))!=NULL) {
		if (segptr->segatts.gdtect==UG_DETECTABLE) {	
			/*  make sure segment has correct visibility,detectable  
				and within (without) ndcbox */
			if ((ug_find.vis!=2)&&((segptr->segatts).gvis!=(Gsegvis)ug_find.vis)) {
				goto rtn;
			}
			inbox=ug_notinbox(segptr,ug_find.x,ug_find.y,ug_find.epsx,ug_find.epsy);
			if ((ug_find.pikmode==0)||(ug_find.pikmode==2)) {	/* part or all in */
				if (inbox==1) {	/* out of box*/
					goto rtn;
				}
			}
			vlen=0;
			/* set up findsgpk parameter structure */
			findprms.curxform=ug_gksstli.curvwindex;
			findprms.pkid=ug_gksstli.curprats.pickid;
			zbytecp(findprms.prats,ug_gksstli.curprats);
			ug_ident(findprms.gmodxf);
			ug_findsgpk(n,&findprms,vlst,&vlen);
			uu_dprint(UU_GITRC,(us,"ug_finxy. vlen=%d",vlen)); 
  			for (i=0; i<vlen; i++) seg[i]=vlst[i];
  			(*seglen)=vlen;
  			if (vlen>0) *dist=ug_find.dist;
			*pkid=ug_find.pickid;
		}
	}
	rtn:ug_find.find=0;						/* turn off switch to find stuff */
	uu_dprint(UU_GITRC,(us,"ug_finxy(%d,ats,%g,%g,%g,%d,%d,%d) found=%d, dist=%g\n", 
				n,nx,ny,eps,seg[*seglen-1],*seglen,*pkid,
				ug_find.found,*dist));
	uu_dexit;
  return(ug_find.found);
}

/*********************************************************************
**    I_FUNCTION     :  ug_viewws(n,ws) -- view segment n on workstation ws.
**    PARAMETERS   
**       INPUT  : Gws ws -- workstation id.
**       			int n -- segment number to be viewed.
**						int erase -- 1=erasing segment n.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_viewws(ws,n,erase)						/* view segment n on workstation ws */
Gws ws;
int n;
int erase;
{
	uu_denter(UU_GITRC,(us,"ug_viewws(%d,%d,%d)", ws,n,erase));
	ug_wsoff(ws);						/* disable all but one ws */
	ug_view0(n,erase);				/* draw the segment. */
	ug_wson();							/* re-activate workstations */
	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION   :  ug_view0(n,erase) -- view segment n with default seg atts.
**    PARAMETERS   
**       INPUT  :  	int n -- segment to be viewed.
**							erase --	1=erasing view segment.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_view0(n,erase)            /* view segment n with default seg atts, xform */
int n;
{
	UG_segat ats;
	Gos systate;				/* save system state here */

	uu_denter(UU_GITRC,(us,"ug_view0(%d,%d)",n,erase));

	zbytecp(ats,ug_defsegat);					/* structure assignment */
	ug_find.find=0;
	systate=ug_gksos.sysstate;			/* save GKS system state */
	/* don't traverse in segment open state because we make calls to
	user callable GKS routines, which would add cmds to segments */
 	if (systate==UG_SGOP) ug_gksos.sysstate=UG_WSAC;
	/*gipat3(&pratts);              save current primitive attributes */
	vlen=0;
	ug_viewsgpk(n,&ats,erase);
	/*gspat3(&pratts);              restore primitive attributes */
	ug_gksos.sysstate=systate;		/* save GKS system state */
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_views(n,ats,erase)
**		View segment n with seg attributes.
**    PARAMETERS   
**       INPUT  : int n -- segment number to view.
**						UG_segat *ats -- segment attributes to use.
**						int erase -- 1=erasing segment n
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_views(n,ats,erase)            /* view segment n with given seg attributes */
int n;
UG_segat *ats;
int erase;
{
	Gos systate;				/* save system state here */

	uu_denter(UU_GITRC,(us,"ug_views(%d,ats,%d)",n,erase));
	ug_find.find=0;
	systate=ug_gksos.sysstate;			/* save GKS system state */
	/* don't traverse in segment open state because we make calls to
	user callable GKS routines, which would add cmds to segments */
 	if (systate==UG_SGOP) ug_gksos.sysstate=UG_WSAC;
	/*gipat3(&pratts);              save current primitive attributes */
	vlen=0;
	ug_viewsgpk(n,ats,erase);
	/*gspat3(&pratts);              restore primitive attributes */
	ug_gksos.sysstate=systate;		/* save GKS system state */
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_viewsg(n,ats,erase) -- view or find segment n.
**    PARAMETERS   
**       INPUT  : int n -- segment number to view or find.
**						UG_segat *ats -- segment attributes to use.
**						int erase -- 1=erasing segment n
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_viewsg(n,ats,erase)
int n; 
UG_segat *ats;
int erase;
{
	vlen=0;
	ug_viewsgpk(n,ats,erase);
}

/*********************************************************************
**    S_FUNCTION :  ug_dcallseg(prms) -- UG_DCALL simulation routine,
**							for workstations that don't maintain segment storage.
**    PARAMETERS   
**       INPUT  : struct {int id; Gws ws; Gseg n;} *prms;
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dcallseg(prms)					/* UG_DCALL simulation routine */
Stcallseg *prms;
{
	UG_segat ats;
	uu_denter(UU_GITRC,(us,"ug_dcallseg(%d)",prms->n));
	zbytecp(ats,ug_defsegat);					/* structure assignment */
	ug_find.find=0;
	vlen=1;					/* causes viewsgpk not to use raster segment */
	ug_viewsgpk(prms->n,&ats,0);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_viewsgpk(n,ats,erase)  
**			View segment n.
**    PARAMETERS   
**       INPUT  : n 					-- segment to view.
**						UG_segat *ats	-- segment attriutes to use.
**						erase 			--	1=erasing view segment.
**												(workstation may use this to erase a seg.)
**												0=not erasing.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_viewsgpk(n,ats,erase)                 
int n;
UG_segat *ats;          	/* dynamic segment attributes */
int erase;
{
  UG_plylna3op *cmd;			/* pointer to each cmd */
  int ncmd;						/* number of commands in a segment */
  int icmd;						/* ith command */
  UG_segat at;	 			   /* current seg atts and xform */
  int cmdlen;
  int i,m,len,opcode;
  logical ptr;
  Gfloat tmpxform[4][4];		/* holds global modelling xform temporarily */
  char *ug_lsielt();			/* function to return list element */
  UG_segstli *segptr;		/* pointer to segment n's header */
  int foundit;					/* false=didnt find anything */
  Gos systate;					/* save GKS state here */
  Gnrect3 nrect;				/* NDC text extent rectangle */
  Gstate state;				/* storage for xforms, atts, used on seg call */
  int oldviwseg;				/* save ug_viwseg here */
  UG_LSI *listp;				/* graphics command list for this seg */
  Gwpoint3 *Getpoints3();
  Gwpoint *Getpoints();
	int hilite,histyle,ln,mk,tx,fl,sty;

	uu_denter(UU_GITRC,(us,"ug_viewsgpk(%d,ats,%d) vlen=%d",
		n,erase,vlen));

	segptr=ug_segac(n);

	if( segptr == NULL ) {
		uu_dprint(UU_GITRC,(us,"ug_viewsgpk. seg %d doesn't exist",n));
		uu_dexit; return;
	}

	/* Don't traverse invisible segments */
	/* NCL: back out change Jonathan/UNICAD made in release 2.6 of MPE */
	/* if( (segptr->segatts.gvis == UG_INVISIBLE) ) { */
	if( !erase && (segptr->segatts.gvis == UG_INVISIBLE) ) {
		uu_dprint(UU_GITRC,(us,"ug_viewsgpk. segment is invisible"));
		uu_dexit; return;
	}

	/* If the ndcbox exists and is empty, everything in seg is clipped, so
		don't traverse it */
/*
.....If we are plotting we don't need to check the box JLS 9/9/99
*/
	if (segptr->wcboxok==1&&(!T_PLOT)) {
		if (((*segptr).wcbox.llf.x>(*segptr).wcbox.urb.x)||
			 ((*segptr).wcbox.llf.y>(*segptr).wcbox.urb.y) ||
			 ((*segptr).wcbox.llf.z>(*segptr).wcbox.urb.z)) {
			uu_dprint(UU_GITRC,(us,"ug_viewsgpk. ndcbox is null"));
			uu_dexit; return;
		}
	}
	vlst[vlen]=n;  vlen=vlen+1;  /* record this ug_viewsgpk activation */

	/* Push current transformations if neccessary.  Only required if this
	 * segment changes modelling xform. 
	 */
	uu_dprint(UU_GITRC,(us,"segptr->xforms = %x", segptr->xforms));
	uu_dprint(UU_GITRC,(us,"UG_SEGMODXF = %x", UG_SEGMODXF));
	if( segptr->xforms & (UG_SEGMODXF) )
		ug_savestate(&state);

	/* Concatenate local mod xform to global mod xform if it's not identity */
	uu_dprint(UU_GITRC,(us,"ug_lmodidnt %d",ug_lmodidnt));
#ifndef UU_DEBUGOFF
	uu_dprint(UU_GITRC,(us,"mod xform:"));
	ug_matprt(ug_modxform);
#endif

/*
.....If we're in the middle of Analyzation Playback
.....set all colors to requested geo color
.....Bobby  -  1/19/94
*/
	hilite = 0;
	histyle = 0;
	if (UN_override_geo_attr[0] != -1 && segptr->xforms & UN_override_geo_mask)
	{
		hilite = 1;
		/* Save current colors */
		ln = ug_gksstli.curprats.lnbundl.color;
		mk = ug_gksstli.curprats.mkbundl.color;
		tx = ug_gksstli.curprats.txbundl.color;
		fl = ug_gksstli.curprats.flbundl.color;
		/* Set colors to hilite index */
		ug_linecolor(UN_override_geo_attr[0]);
		ug_markcolor(UN_override_geo_attr[0]);
		ug_textcolor(UN_override_geo_attr[0]);
		ug_fillcolor(UN_override_geo_attr[0]);
	}
	if (UN_override_geo_attr[1] != -1 && segptr->xforms & UN_override_geo_mask)
	{
		Glntype lintyp;
		histyle = 1;
		sty = ug_gksstli.curprats.lnbundl.type.typeno;
		zbytecp(lintyp,*gqlinetype());
		lintyp.typeno = UN_override_geo_attr[1];
		ug_linetype(&lintyp);
	}

	if( !ug_lmodidnt ) {
		ug_matmp(tmpxform,ug_modxform,ug_lmodxform);
		ug_mcopy(ug_modxform,tmpxform);
		ug_ident(ug_lmodxform);			/* set local model xform to identity*/
		ug_lmodidnt = 1;					/* ug_lmodxform is now identity */
	}

  ug_find.found=false;
  foundit=false;
  systate=ug_gksos.sysstate;			/* save GKS system state */
  /* don't traverse in segment open state because we make calls to
	user callable GKS routines, which would add cmds to segments */
  if (systate==UG_SGOP) ug_gksos.sysstate=UG_WSAC;
  oldviwseg=ug_viwseg;			/* save ug_viwseg */
  ug_viwseg=n;						/* for clip/exp routines */

	/* if a raster copy of the seg exists and we are at a root seg, use it. */
	if ((segptr->rassegpt!=NULL)&&(vlen<=1)) {
		listp=segptr->rassegpt;
		uu_dprint(UU_GITRC,(us,"ug_viewsgpk using raster copy"));
	}
	else listp=(UG_LSI *)segptr->seglist;
	ncmd = ug_lsinelt((*listp));
	uu_dprint(UU_GITRC,(us,"ug_viewsgpk. ncmd=%d",ncmd));
 	/* for each command in seg n */
	for( icmd=0; icmd < ncmd; ++icmd ) {
	 cmd = (UG_plylna3op *)ug_lsielt(listp,icmd);
	 cmdlen = ug_lsilen(listp,icmd)/sizeof(int);
    opcode=(*cmd).elttype;
    ptr=false;
    if (opcode<=UG_OPPTR) {
      if ((opcode&UG_OPPTR)!=0) ptr=true;
      opcode=opcode&(UG_OPPTR-1);
    }
	if ((opcode&UG_OPPTR)!=0) ptr=true; else ptr=false;
	uu_dprint(UU_GITRC,(us,"ug_viewsgpk. opcode[%d]=%s ptr=%d cmdlen=%d",
				icmd,&ug_ops[opcode][0],ptr,cmdlen));
	if (opcode>UG_OPPTR) {       /* library command */
      ug_viwlib(cmd);                     /* call library proc */
    }
    else {                     /* normal command */
    switch (opcode) {
    case UG_NOOP: break;
    case UG_PAGEOP: ug_pag(); break;

    case UG_PLYLNA3OP: 
			len=(*(UG_plylna3op *)cmd).len;
			ug_polyln3(len,temp3=Getpoints3((*(UG_plylna3op *)cmd).pts,len));
			if ((ug_ndcseg>0)&&((*segptr).wcboxok==0)) 
				ug_boxexpln3(segptr,temp3,len);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp3);
#endif
			break;
    case UG_PLYLNA2OP: 
			len=(*(UG_plylna2op *)cmd).len;
			ug_polyln(len,temp=Getpoints((*(UG_plylna2op *)cmd).pts,len));
			if ((ug_ndcseg>0)&&((*segptr).wcboxok==0)) 
				ug_boxexpln2(segptr,temp3,len);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp);
#endif
			break;
	 case UG_PLYLNRASOP: 
			len=(*(UG_plylnrasop *)cmd).len;
			ug_polylnras(len,(*(UG_plylnrasop *)cmd).pts);
			break;
    case UG_PLYMKA3OP: 
			len=(*(UG_plymka3op *)cmd).len;
			ug_polymk3(len,temp3=Getpoints3((*(UG_plymka3op *)cmd).pts,len));
			if ((ug_ndcseg>0)&&((*segptr).wcboxok==0)) 
				ug_boxexpmk3(segptr,temp3,len);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp3);
#endif
			break;
    case UG_PLYMKA2OP: 
			len=(*(UG_plymka2op *)cmd).len;
			ug_polymk(len,temp=Getpoints((*(UG_plymka2op *)cmd).pts,len));
			if ((ug_ndcseg>0)&&((*segptr).wcboxok==0)) 
				ug_boxexpmk2(segptr,temp,len);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp);
#endif
         break;
	 case UG_PLYMKRASOP: 
			len=(*(UG_plymkrasop *)cmd).len;
			ug_polymkras(len,(*(UG_plymkrasop *)cmd).pts);
			break;
    case UG_TEXTOP: 
			len=(*(UG_textop *)cmd).len;
			gtext(temp3=Getpoints3(&(*(UG_textop *)cmd).position,1),
				(*(UG_textop *)cmd).string);
			if ((ug_ndcseg>0)&&((*segptr).wcboxok==0)) {
				/* expand the box by string extent rect */
				/* for now, estimate the string rectangle by using
					char height and estimating each char's width
					is .6 its height */
				ug_txrect(temp3, (*(UG_textop *)cmd).string,&nrect);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
				uu_toolfree(temp3);
#endif
/*				ug_ndcboxadd(segptr,&nrect.llf,&nrect.urb);	*/
			}
			break;
	 case UG_TEXTRASOP: 
			len=(*(UG_textrasop *)cmd).len;
			ug_textras(&(*(UG_textrasop *)cmd).position,
				(*(UG_textrasop *)cmd).string,(*segptr).userdata[0]);
			break;
	 case UG_FLAREA3OP: 
			len=(*(UG_flarea3op *)cmd).len;
			ug_fla3(len,temp3=Getpoints3((*(UG_flarea3op *)cmd).pts,len));
			if ((ug_ndcseg>0)&&((*segptr).wcboxok==0)) {
				ug_boxexpfa3(segptr,temp3,len);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp3);
#endif
			}
			break;
	 case UG_FLAREAOP: 
			len=(*(UG_flareaop *)cmd).len;
			ug_fla2(len,temp=Getpoints((*(UG_flareaop *)cmd).pts,len));
			if ((ug_ndcseg>0)&&((*segptr).wcboxok==0)) {
				ug_boxexpfa2(segptr,temp,len);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp);
#endif
			}
			break;
	 case UG_FLAREARASOP: 
			len=(*(UG_flarearasop *)cmd).len;
			ug_flaras(len,(*(UG_flarearasop *)cmd).pts,(*segptr).userdata[0]);
			break;
	case UG_CELLOP:	
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
/*......not used   */
/*		
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		{	Gwrect b;

			ug_ItoGpoint(b.ll,(*(UG_cellop *)cmd).rect.ll);
			ug_ItoGpoint(b.ur,(*(UG_cellop *)cmd).rect.ur);
			ug_dcellarray(&b,
			&(*(UG_cellop *)cmd).dims,(*(UG_cellop *)cmd).colorarray);
		}
#else
			ug_dcellarray(&(*(UG_cellop *)cmd).rect,
			&(*(UG_cellop *)cmd).dims,(*(UG_cellop *)cmd).colorarray);
#endif
*/			break;
	case UG_CELLRUNOP:
/*......not used   */
/*		{
				int dy,totalruns;
				dy=(*(UG_cellrunop *)cmd).dims.y;
				totalruns=(*(UG_cellrunop *)cmd).totalruns;
*/
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
/*#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			{	Gwrect b;

				ug_ItoGpoint(b.ll,(*(UG_cellrunop *)cmd).rect.ll);
				ug_ItoGpoint(b.ur,(*(UG_cellrunop *)cmd).rect.ur);
				ug_dcellrunarray(&b,
					&(*(UG_cellrunop *)cmd).dims,totalruns,
					&(*(UG_cellrunop *)cmd).arrays[0],
					&(*(UG_cellrunop *)cmd).arrays[dy],
					&(*(UG_cellrunop *)cmd).arrays[dy+totalruns]);
			}
#else
				ug_dcellrunarray(&(*(UG_cellrunop *)cmd).rect,
					&(*(UG_cellrunop *)cmd).dims,totalruns,
					&(*(UG_cellrunop *)cmd).arrays[0],
					&(*(UG_cellrunop *)cmd).arrays[dy],
					&(*(UG_cellrunop *)cmd).arrays[dy+totalruns]);
#endif
			}
*/			break;
	case UG_CELLRASOP:
/*.....not used */
/*			ug_dcellras((*(UG_cellrasop *)cmd).rect,
				&(*(UG_cellrasop *)cmd).dims,(*(UG_cellrasop *)cmd).colors);
*/			break;
	case UG_CELLRUNRASOP:
/*......not used   */
/*		{
			Gipoint rect[2];
			int *nrunrow,*lens;
			int aindx,lensindx,nrunrowindx;

			rect[0].x=(*(UG_cellrunrasop *)cmd).np.x;
			rect[0].y=(*(UG_cellrunrasop *)cmd).np.y;
			rect[1].x=(*(UG_cellrunrasop *)cmd).nq.x;
			rect[1].y=(*(UG_cellrunrasop *)cmd).nr.y;
			nrunrowindx=0;
			lensindx=(*(UG_cellrunrasop *)cmd).dims.y;
			aindx=lensindx+(*(UG_cellrunrasop *)cmd).totalruns;
			ug_dcellrunras(rect,&(*(UG_cellrunrasop *)cmd).dims,	
				&(*(UG_cellrunrasop *)cmd).arrays[nrunrowindx],
				&(*(UG_cellrunrasop *)cmd).arrays[lensindx],
				&(*(UG_cellrunrasop *)cmd).arrays[aindx]);
		}
*/		break;
	case UG_PROCOP: 
		{ 
			int (*func)();      /* p points to fctn returning int */
  	      int len;
        	func=(*(UG_procop *)cmd).func;   /* get pointer to fctn */
        	len=(*(UG_procop *)cmd).len;
			uu_denter2(UU_GITRC,(us,"gprocop. p=%x, len=%d, prms=",p,len));
			uu_dexit;
        	for (i=0; i<len; i++) {
				uu_dprint(UU_GITRC,(us,"%x ",(*(UG_procop *)cmd).prms[i]));
			}
        	(*func)(len,(*(UG_procop *)cmd).prms); 	/* call the user fctn */
  		} 
		break;
    case UG_CALLOP: 
			m=(*(UG_callop *)cmd).segno;   /* call seg m */
			zbytecp(at,ug_defsegat);		/* default seg atts to at */
         ug_viewsgpk(m,&at,erase);		/* recursive call */
			break;
	case UG_SNTRANOP: 
			ug_sntran((*(UG_sntranop *)cmd).xform); 
			break;
	case UG_MTRANOP:
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		{	Gtran a;

			ug_ItoGcopy(a,(*(UG_mtranop *)cmd).xf);
			ug_smodxf(a,(*(UG_mtranop *)cmd).type);
		}
#else
			ug_smodxf((*(UG_mtranop *)cmd).xf,(*(UG_mtranop *)cmd).type); 
#endif
			break;
	case UG_LMTRANOP:
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		{	Gtran a;

			ug_ItoGcopy(a,(*(UG_lmtranop *)cmd).xf);
			ug_slmodxf(a,(*(UG_lmtranop *)cmd).type);
		}
#else
			ug_slmodxf((*(UG_lmtranop *)cmd).xf, (*(UG_lmtranop *)cmd).type); 
#endif
			break;
   case UG_DFATSOP: ug_dfats(); break;
   case UG_INTENSOP: 
			/* gsint((*cmd).gcreal[1]); */ break;	/* un comment */
   case UG_LSTYLOP:
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		{	Glntype b;
			if (histyle != 1)
			{
				ug_ItoGlntype(b,&(*(UG_lstylop *)cmd).ls);
				ug_linetype(&b);
			}
		}
#else
			if (histyle != 1)
				ug_linetype(&(*(UG_lstylop *)cmd).ls);
#endif
			break;
   case UG_LWIDOP: ug_linewidth((Gscale)(*(UG_lwidop *)cmd).width); break;
   case UG_FONTOP: 
			ug_textfp(&(*(UG_fontop *)cmd).p); break;
   case UG_CHHGTOP: ug_charheight((Gchrht)(*(UG_chhgtop *)cmd).height); break;
	case UG_CHEXPOP: ug_charexp((Gchrexp)(*(UG_chexpop *)cmd).expn); break;
   case UG_CHPLANEOP: ug_txplane(temp3=Getpoints3(&(*(UG_chplaneop *)cmd).txpvc,1));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		uu_toolfree(temp3);
#endif
        break;
   case UG_CHUP3OP: ug_charup3(temp3=Getpoints3(&(*(UG_chup3op *)cmd).upvec,1));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp3);
#endif
			break;
   case UG_CHUP2OP: ug_charup(temp=Getpoints(&(*(UG_chup2op *)cmd).upvec,1));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp);
#endif
			break;
   case UG_CHPATHOP: 
			ug_textpath((*(UG_chpathop *)cmd).path);
			break;
   case UG_CHSPACEOP: ug_charspace((Gchrsp)(*(UG_chspaceop *)cmd).spacing); 
			break;
   case UG_CHJUSTOP: 
			ug_textalign(&(*(UG_chjustop *)cmd).align);
			break;
   case UG_SYMBOLOP: ug_marktype((*(UG_symbolop *)cmd).type); 
			break;
   case UG_PICKIDOP: ug_spickid((*(UG_pickidop *)cmd).pid); 
			break;
	case UG_LNCOLROP:
			if (erase == 0 && hilite == 0) 
				{
				ug_linecolor((*(UG_lncolrop *)cmd).color); break;
				}
			else if (hilite == 0)
				{
				ug_linecolor(ug_segerasecolor);
				}
		break;
	case UG_MKCOLROP: 
			if (erase == 0 && hilite == 0) 
				{
				ug_markcolor((*(UG_mkcolorop *)cmd).color); break;
				}
			else if (hilite == 0)
				{
				ug_markcolor(ug_segerasecolor);
				}
		break;
	case UG_TXCOLROP: 
			if (erase == 0 && hilite == 0) 
				{
				ug_textcolor((*(UG_txcolorop *)cmd).color); break;
				}
			else if (hilite == 0)
				{
				ug_textcolor(ug_segerasecolor);
				}
		break;
	case UG_FACOLROP: 
			if (erase == 0 && hilite == 0) 
				{
				ug_fillcolor((*(UG_facolorop *)cmd).color);
				}
			else if (hilite == 0)
				{
				ug_fillcolor(ug_segerasecolor);
				}
		break;
	case UG_EDGEFLAGOP: ug_sedgeflag((*(UG_edgeflagop *)cmd).f); break;
/*
.....those are not used by ug_viewsgpk (we only use those command when use openGL)
.....but it is not illegal opcode
*/
	case UG_MATERIALOP:
	case UG_SHDCOLROP:
	case UG_LUCENCYOP:
	case UG_SHADEAREAOP:
		break;
    default: 
/*		fprintf(ug_gksos.erfile,"ug_viewsgpk giewsg illegal opcode=%d\n",opcode);*/
		break;
    };                             /* case */
    }                              /* normal command */
	if (ug_find.found==true) {
		foundit=true;					/* remember we found something */
		ug_find.found=false;
		ug_find.epsx=ug_find.dist;		/* decrease distance. keep looking */
		ug_find.epsy=ug_find.dist;
		ug_find.pickid=ug_gksstli.curprats.pickid;	/* remember pkid of thing found*/
		if (ug_find.func!=NULL){ 		/* call a user supplied function */
			(*ug_find.func)(vlen,vlst,&ug_find);
			if (ug_find.find==false) break;	/* user func says stop looking */
			foundit=false;				/* keep looking */
		}
   } 
  }                                /* for each command in seg */

    /* Restore the colors */
	if (hilite == 1)
	{
		ug_gksstli.curprats.lnbundl.color = ln;
		ug_gksstli.curprats.mkbundl.color = mk;
		ug_gksstli.curprats.txbundl.color = tx;
		ug_gksstli.curprats.flbundl.color = fl;
	}
	if (histyle == 1)
	{
		ug_gksstli.curprats.lnbundl.type.typeno = sty;
	}

  /* If needed, update ndc box */
  if (ug_ndcseg>0 ) {

		(*segptr).wcboxok=1;

		/* Add box to visible list? */
		if( !(segptr->xforms & UG_SEGVISLIST) ) {	     
			uu_dprint(UU_GITRC,(us,"checking seg %d for visible list", n));

			/* Seg has something visible (not inside out) */
			uu_dprint(UU_GITRC,(us,"vislistok=%d", ug_vislistok));
			if( ug_vislistok && !erase ) {
				segptr->xforms |= UG_SEGVISLIST; /* We've checked this seg */
				if( !(segptr->wcbox.llf.x > segptr->wcbox.urb.x ||
						segptr->wcbox.llf.y > segptr->wcbox.urb.y ||
						segptr->wcbox.llf.z > segptr->wcbox.urb.z) ) {
					uu_dprint(UU_GITRC,(us,"adding segment %d to visible list", n));
					UU_IAADD(ug_vislist, n);
				}
			}
		}     
	}

	/* Restore digs state (xforms) */
	uu_dprint(UU_GITRC,(us,"segptr->xforms = %x", segptr->xforms));
	uu_dprint(UU_GITRC,(us,"UG_SEGMODXF = %x", UG_SEGMODXF));
	if( segptr->xforms & (UG_SEGMODXF) )
		ug_resstate(&state);

	/* reset NDC COORDS OK flag */
	(*segptr).xforms &= ~UG_SEGNDCBOX;	

	ug_find.found=foundit;
	if (foundit==false) 
		vlen=vlen-1;				/* remove this viewsg activation from list */
	ug_gksos.sysstate=systate;		/* restore GKS state */
	ug_viwseg=oldviwseg;			/* restore viwseg */
	
	uu_dprint(UU_GITRC,(us,"ug_viewsgpk returning."));
	uu_dexit;
}

/********************************************************************* 
**  I_FUNCTION:  gextrema(xform,rect) -- extrema NDC bounding box
**								for segs using normtran xform.
**  PARAMETERS   
**      INPUT:  int xform; -- normtran number.
**      OUTPUT: Gwrect3 *rect; -- result
**
**  RETURNS      :  boolean - true if a bounding box for the given
**										transform was found
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
gextrema(xform, rect)					/* extrema ndc bounding box */
int xform;
Gwrect3 *rect;
{
	int msk;
	UG_segstli *p;
	int found,i;
#define BIGCOORD (UU_REAL) 10.0e+20

	uu_denter(UU_GITRC,(us,"gextrema(%d)",xform));

	msk = (1<<xform)|UG_SEGINNTRAN;	/* this normtran or inherited normtran */
	
	found=0;
	/* set returned box to undefined. */
	(*rect).llf.x=BIGCOORD;
	(*rect).llf.y=BIGCOORD;
	(*rect).llf.z= -BIGCOORD;
	(*rect).urb.x= -BIGCOORD;
	(*rect).urb.y= -BIGCOORD;
	(*rect).urb.z=BIGCOORD;

	ug_seginitscan();
	while((p=ug_segscan())!=NULL) {
		uu_dprint(UU_GITRC,(us,"p->segid %d",p->segid));
		uu_dprint(UU_GITRC,(us,"p->xforms %x",p->xforms));
		uu_dprint(UU_GITRC,(us,"p->segatts.gvis %d",p->segatts.gvis));
		uu_dprint(UU_GITRC,(us,"p->segatts.gdtect %d",p->segatts.gdtect));
/*
.....Extrema zoom motion segment display
*/
		if (p->segid == NCL_mot_seg)
		{
			if (p->segatts.gvis == UG_VISIBLE)
			{
				i = ncl_motion_extrema(xform,rect);
				if (i == 1) found = 1;
			}
		}
		else if  (((p->xforms&msk)!=0)&&(p->segatts.gvis == UG_VISIBLE)
			&&(p->segatts.gdtect==UG_DETECTABLE || ncl_on_verify_segno(p->segid)))
		{
			/* found a visible detect segment that depends upon this normtran */
			i=ug_segextrema(p,rect);
			if (i==1) found=1;
		}
	}
	uu_dprint(UU_GITRC,(us,"%d=gextrema. result: %g %g %g  %g %g %g",found,
						(*rect).llf.x,(*rect).llf.y,(*rect).llf.z,
						(*rect).urb.x,(*rect).urb.y,(*rect).urb.z));
	uu_dexit;
	return(found);
}

/*********************************************************************
**    I_FUNCTION :  int ug_segextrema(segptr,rect) - expand world coordinate
**							extrema for a segment.  Ignores raster primitives.
**    PARAMETERS   
**       INPUT  : 	UG_segstli *segptr; -- segment ptr to find extrema for.
**							Gwrect3 *rect; -- extrema
**       OUTPUT :  	Gwrect3 *rect; -- extrema.
**    RETURNS      : 1 = found the extrema. 0=nothing in segment.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_segextrema(segptr,rect)
UG_segstli *segptr;
Gwrect3 *rect;
{
	int ncmd;					/* number of commands in segment */
	int icmd;					/* index of current command */
	int cmdlen;					/* length of current command */
	int  opcode;				/* opcode of current command */
	int len;						/* length of current command */
	Gfloat textheight;		/* char height */
	Gtxpath textpath;			/* text path */
	Gtxprec textprec;			/* text  precision */
	Gtxalign textalign;		/* text alignment(justification) */
	Gfloat textexp;			/* char expansion factor */
	Gfloat charspace;			/* character spacing */
	Gwpoint3 textup;			/* character up vector */
	Gwpoint3 textplane;		/* text plane normal */
   Gwpoint3 *Getpoints3();
   Gwpoint3 gpt[2];
	Gwpoint *Getpoints();
	UG_shadearea *shdcmd;
	Gwpoint3 *pts;

	char *ug_lsielt();
	int i;
	int irtn;
	int m;
	UG_plylna3op *cmd;
/*	char us[100];*/

	ncmd = ug_lsinelt(segptr->seglist);
	uu_denter2(UU_GITRC,(us,"ug_segextrema(seg=%d). ncmd=%d",
		segptr->segid,ncmd));

	irtn=0;
	/* start with current text attributes (prevents seg calls -- fix this.) */
	textheight=ug_gksstli.curprats.txht;
	textpath=ug_gksstli.curprats.txpath;
	textprec=ug_gksstli.curprats.txbundl.fp.prec;
	textexp=ug_gksstli.curprats.txbundl.expn;
	charspace=ug_gksstli.curprats.txbundl.space;
	zbytecp(textalign,ug_gksstli.curprats.txalign);
	zbytecp(textplane,ug_gksstli.curprats.txpvec);
	zbytecp(textup,ug_gksstli.curprats.txuv);

	if (segptr->wcboxok)
	{
		gpt[0].x = segptr->wcbox.llf.x;
		gpt[0].y = segptr->wcbox.llf.y;
		gpt[0].z = segptr->wcbox.llf.z;
		gpt[1].x = segptr->wcbox.urb.x;
		gpt[1].y = segptr->wcbox.urb.y;
		gpt[1].z = segptr->wcbox.urb.z;
		ug_boxexpnt3(rect,2,gpt);
		return(1);
	}
 	/* for each command in seg n */
	for( icmd=0; icmd < ncmd; ++icmd ){
		cmd = (UG_plylna3op *)ug_lsielt(segptr->seglist,icmd);
		cmdlen = ug_lsilen(segptr->seglist,icmd)/sizeof(int);
		opcode=(*cmd).elttype;
		uu_dprint(UU_GITRC,(us,"ug_segextrema. opcode[%d]=%s cmdlen=%d",
				icmd,&ug_ops[opcode][0],cmdlen));
		if (opcode>UG_OPPTR) {       /* library command */
			/* for now ignore library commands */
		}
		else {                     /* normal command */
		switch (opcode) {
		case UG_FLAREA3OP:
		case UG_PLYLNA3OP: 
		case UG_PLYMKA3OP:
			len=(*cmd).len;
			ug_boxexpnt3(rect,len,temp3=Getpoints3((*cmd).pts,len));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp3);
#endif
			irtn=1;
			break;
/*
.....added for shading
.....Yurong 3/3/99
*/
		case UG_SHADEAREAOP:
 			shdcmd= (UG_shadearea *)cmd;
			pts = shdcmd->pts;
			len=(*shdcmd).len;
			temp3 = (Gwpoint3 *)uu_toolmalloc(len*sizeof(Gwpoint3));
			for (i=0; i < shdcmd->len; i++)
			{
				temp3[i].x = pts[i].x;
				temp3[i].y = pts[i].y;
				temp3[i].z = pts[i].z;
			}
			ug_boxexpnt3(rect,len,temp3);
			uu_toolfree(temp3);
			irtn=1;
			break;
		case UG_FLAREAOP:
		case UG_PLYLNA2OP: 
		case UG_PLYMKA2OP:
			len=(*(UG_plymka2op *)cmd).len;
			ug_boxexpnt2(rect,len,temp=Getpoints((*(UG_plymka2op *)cmd).pts,len));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp);
#endif
			irtn=1;
			break;
	 /*case UG_PLYMKRASOP: 
	 	case UG_FLAREARASOP:
			len=(*(UG_plymkrasop *)cmd).len;
			irtn=1;
			break; */
		case UG_TEXTOP:  
			{	
				Gwrect3 txrect;			/* 3D text rectangle */
					
				ug_mctextbox(temp3=Getpoints3(&(*(UG_textop *)cmd).position,1),
					(*(UG_textop *)cmd).string,
					&textplane,&textup,textheight,
					textpath,charspace,&textalign,&textprec,&txrect);
				ug_boxexpnt3(rect,2,&txrect);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
				uu_toolfree(temp3);
#endif
				irtn=1;
			}
			break;						/* end of case UG_TEXTOP */
		/*case UG_TEXTRASOP: len=(*(UG_textrasop *)cmd).len;
			ug_textras(&(*(UG_textrasop *)cmd).position,
				(*(UG_textrasop *)cmd).string,(*segptr).userdata[0]);
			irtn=1;
			break; */
		case UG_CELLOP:	
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
/*......not used   */
/*		
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		{	Gwrect b;

			ug_ItoGpoint(b.ll,(*(UG_cellop *)cmd).rect.ll);
			ug_ItoGpoint(b.ur,(*(UG_cellop *)cmd).rect.ur);
			ug_boxexpnt2(rect,2,&b);
		}
#else
			ug_boxexpnt2(rect,2,&(*(UG_cellop *)cmd).rect);
#endif
*/			irtn=1;
			break;
		case UG_CELLRUNOP:
/*......not used   */
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
/*
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		{	Gwrect b;

			ug_ItoGpoint(b.ll,(*(UG_cellrunop *)cmd).rect.ll);
			ug_ItoGpoint(b.ur,(*(UG_cellrunop *)cmd).rect.ur);
			ug_boxexpnt2(rect,2,&b);
		}
#else
			ug_boxexpnt2(rect,2,&(*(UG_cellrunop *)cmd).rect);
#endif
*/			irtn=1;
			break;
	/*case UG_CELLRASOP: -- (*(UG_cellrasop *)cmd).rect has raster rect.
			irtn=1;
			break; */
	/*case UG_CELLRUNRASOP: 
			irtn=1;
			break; */
    case UG_CALLOP: 
			m=(*(UG_callop *)cmd).segno;   /* call seg m */
         i=ug_segextrema(ug_segac(m),rect);		/* recursive call */
			if (i==1) irtn=1;
			break;
   case UG_FONTOP: 
			textprec= (*(UG_fontop *)cmd).p.prec;
			break;
   case UG_CHHGTOP: textheight=(*(UG_chhgtop *)cmd).height; 
			uu_dprint(UU_GITRC,(us,"ug_segextrema textheight=%g",textheight));
			break;
	case UG_CHEXPOP: textexp=(*(UG_chexpop *)cmd).expn; break;
   case UG_CHPLANEOP: 
		/* Do ptr assignment if same precision, individual element copy if not. */
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			ug_ItoGpoint3(textplane,(*(UG_chplaneop *)cmd).txpvc);
#else
			zbytecp(textplane,(*(UG_chplaneop *)cmd).txpvc);
#endif
			break;
   case UG_CHUP3OP: 
		/* Do ptr assignment if same precision, individual element copy if not. */
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			ug_ItoGpoint3(textup,(*(UG_chup3op *)cmd).upvec);
#else
			zbytecp(textup,(*(UG_chup3op *)cmd).upvec); 
#endif
			break;
   case UG_CHUP2OP: 
			textup.x=(*(UG_chup2op *)cmd).upvec.x; 
			textup.y=(*(UG_chup2op *)cmd).upvec.y; 
			break;
   case UG_CHPATHOP:
			textpath=(*(UG_chpathop *)cmd).path;
			break;
   case UG_CHSPACEOP: charspace=(*(UG_chspaceop *)cmd).spacing; break;
   case UG_CHJUSTOP: 
			zbytecp(textalign,(*(UG_chjustop *)cmd).align);
			break;
	default: break;
	}                             /* switch */
	}										/* normal command */
	}										/* for each command */
rtn: uu_dprint(UU_GITRC,(us,"%d=ug_segextrema(%g %g %g %g %g %g)",irtn,
			(*rect).llf.x,(*rect).llf.y,(*rect).llf.z,
			(*rect).urb.x,(*rect).urb.y,(*rect).urb.z));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION :  ug_boxexpnt3(box,n,pts); expand box by pts.
**    PARAMETERS   
**       INPUT  : 	Gwrect3 *box;
**							int n;	-- length of pts.
**							Gwpoint3 pts[]; -- array of points.
**       OUTPUT :  	Gwrect *box;
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_boxexpnt3(box,n,pts)
Gwrect3 *box;
int n;
Gwpoint3 pts[];
{
	int i;
	for (i=0; i<n; i++) {
		(*box).llf.x = ((*box).llf.x<pts[i].x) ? (*box).llf.x : pts[i].x;
		(*box).llf.y = ((*box).llf.y<pts[i].y) ? (*box).llf.y : pts[i].y;
		(*box).llf.z = ((*box).llf.z>pts[i].z) ? (*box).llf.z : pts[i].z;
		(*box).urb.x = ((*box).urb.x>pts[i].x) ? (*box).urb.x : pts[i].x;
		(*box).urb.y = ((*box).urb.y>pts[i].y) ? (*box).urb.y : pts[i].y;
		(*box).urb.z = ((*box).urb.z<pts[i].z) ? (*box).urb.z : pts[i].z;
	}
}

/*********************************************************************
**    I_FUNCTION :  ug_boxexpnt2(box,n,pts); expand box by pts.
**    PARAMETERS   
**       INPUT  : 	Gwrect3 *box;
**							int n;	-- length of pts.
**							Gwpoint pts[]; -- array of points.
**       OUTPUT :  	Gwpoint *p;
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_boxexpnt2(box,n,pts)
Gwrect3 *box;
int n;
Gwpoint pts[];
{
	int i;
	for (i=0; i<n; i++) {
		(*box).llf.x = ((*box).llf.x<pts[i].x) ? (*box).llf.x : pts[i].x;
		(*box).llf.y = ((*box).llf.y<pts[i].y) ? (*box).llf.y : pts[i].y;
		(*box).urb.x = ((*box).urb.x>pts[i].x) ? (*box).urb.x : pts[i].x;
		(*box).urb.y = ((*box).urb.y>pts[i].y) ? (*box).urb.y : pts[i].y;
	}
}

/*********************************************************************
**    I_FUNCTION :  ug_mctextbox (strloc,str,textplane,textup,
**					textheight,textpath,charspace,textalign,textprec,txrect);
**        Calculate modelling coordinates of box surrounding text.
**    PARAMETERS   
**       INPUT  : 
**				Gwpoint3 *strloc;
**				char *str;
**				Gwpoint3 *textplane;
**				Gwpoint3 *textup;
**				Gfloat textheight;
**				Gtxpath *textpath;
**				Gfloat charspace;
**				Gtxalign *textalign;
**				Gtxprec *textprec;
**       OUTPUT :  
**				Gwrect3 *txrect;
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_mctextbox(strloc,str,textplane,textup,
	textheight,textpath,charspace,textalign,textprec,txrect)
Gwpoint3 *strloc;					/* starting loc of text */
char *str;							/* the text */
Gwpoint3 *textplane;				/* text plane normal */
Gwpoint3 *textup;					/* text up vector */
Gfloat textheight;				/* text height */
Gtxpath textpath;					/* text path */
Gfloat charspace;					/* character spacing */
Gtxalign *textalign;				/* text justification */
Gtxprec *textprec;				/* text precision */
Gwrect3 *txrect;					/* result text box */
{
	Gwpoint3 txvect;				/* vector in direction of text, size=width */
	Gwpoint3 txupvect;			/* vector in textup direction, size=height */
	Gwpoint3 txplvect;			/* unit vector in textplane direction */
	int slen;						/* string length */
	Gfloat width,height;			/* size of text box */
	Gfloat tmp;

	uu_denter(UU_GITRC,(us,
		"ug_mctextbox(%s loc=%g %g %g up=%g %g %g txplane=%g %g %g", 
		str,(*strloc).x,(*strloc).y,(*strloc).z,(*textup).x,(*textup).y,
		(*textup).z,(*textplane).x,(*textplane).y,(*textplane).z));
	uu_dprint(UU_GITRC,(us,"ug_mctextbox txrect=%x charspace=%g",
		txrect,charspace));
	slen=strlen(str);
	/* estimate the text rectangle width by using
		char height and estimating each char's width
		is .6 its height */
	switch(textpath) {
	case UG_TP_UP: case UG_TP_DOWN: 
		/* assume stroke precision */
		width= .6*textheight;
		height=textheight*(slen+(slen-1)*charspace);
		break;
	case UG_TP_LEFT: case UG_TP_RIGHT:
		/* assume stroke precision */
		height=textheight;
		width= .6*height*(slen+(slen-1)*charspace);
		break;
	default: uu_dprint(-1,(us,"ug_mctextbox bad textpath=%d",textpath));
	}
	uu_dprint(UU_GITRC,(us,"ug_mctextbox. width=%g height=%g",width,height));

	/* calc txplvect = unitvec(*textplane); */
	zbytecp(txplvect,(*textplane));
	ug_unitvec(&txplvect);
	
	/* calc txupvect = unitvec(textup)*height; */
	zbytecp(txupvect,(*textup));
	ug_unitvec(&txupvect);
	txupvect.x=txupvect.x*height;
	txupvect.y=txupvect.y*height;
	txupvect.z=txupvect.z*height;

	/*calc txvect = unitvec(txupvect cross txplvect)*width; */
	txvect.x = txupvect.y*txplvect.z - txupvect.z*txplvect.y;
	txvect.y = txupvect.z*txplvect.x - txupvect.x*txplvect.z;
	txvect.z = txupvect.x*txplvect.y - txupvect.y*txplvect.x;
	ug_unitvec(&txvect);
	txvect.x = txvect.x*width;
	txvect.y = txvect.y*width;
	txvect.z = txvect.z*width;

	/* move the text starting point to the lower left of the text */
	zbytecp((*txrect).llf,(*strloc));
	switch(textpath) {
	case UG_TP_DOWN:
		(*txrect).llf.x = (*txrect).llf.x - txupvect.x;
		(*txrect).llf.y = (*txrect).llf.y - txupvect.y;
		(*txrect).llf.z = (*txrect).llf.z - txupvect.z;
		break;
	case UG_TP_LEFT:
		(*txrect).llf.x = (*txrect).llf.x - txvect.x;
		(*txrect).llf.y = (*txrect).llf.y - txvect.y;
		(*txrect).llf.z = (*txrect).llf.z - txvect.z;
		break;
	}
	switch((*textalign).hor) {
	case UG_TH_RIGHT:
		if (textpath!=UG_TP_LEFT) {	/* if path left, already right justified*/
			(*txrect).llf.x = (*txrect).llf.x - txvect.x;
			(*txrect).llf.y = (*txrect).llf.y - txvect.y;
			(*txrect).llf.z = (*txrect).llf.z - txvect.z;
		}
		break;
	case UG_TH_CENTRE:
		if (textpath!=UG_TP_LEFT) {
			(*txrect).llf.x = (*txrect).llf.x - txvect.x/2.;
			(*txrect).llf.y = (*txrect).llf.y - txvect.y/2.;
			(*txrect).llf.z = (*txrect).llf.z - txvect.z/2.;
		}
		else {					/* text is drawn to left  and center justified */
			(*txrect).llf.x = (*txrect).llf.x + txvect.x/2.;
			(*txrect).llf.y = (*txrect).llf.y + txvect.y/2.;
			(*txrect).llf.z = (*txrect).llf.z + txvect.z/2.;
		}
		break;
	}
	/* calc txrect.urb = txrect.llf + (txupvect + txvect) */
	(*txrect).urb.x = (*txrect).llf.x + txupvect.x+txvect.x;
	(*txrect).urb.y = (*txrect).llf.y + txupvect.y+txvect.y;
	(*txrect).urb.z = (*txrect).llf.z + txupvect.z+txvect.z;

	/* now swap coordinates if needed */
	if ((*txrect).llf.x>(*txrect).urb.x) {
		tmp=(*txrect).llf.x; (*txrect).llf.x=(*txrect).urb.x;
		(*txrect).urb.x=tmp;
	}
	if ((*txrect).llf.y>(*txrect).urb.y) {
		tmp=(*txrect).llf.y; (*txrect).llf.y=(*txrect).urb.y;
		(*txrect).urb.y=tmp;
	}
	if ((*txrect).llf.z<(*txrect).urb.z) {
		tmp=(*txrect).llf.z; (*txrect).llf.z=(*txrect).urb.z;
		(*txrect).urb.z=tmp;
	}
	uu_dprint(UU_GITRC,(us,"ug_mctextbox returns %g %g %g %g %g %g",
		(*txrect).llf.x,(*txrect).llf.y,(*txrect).llf.z,
		(*txrect).urb.x,(*txrect).urb.y,(*txrect).urb.z));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_unitvec(v) -- unitize vector v.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_unitvec(v)
Gwpoint3 *v;
{
	Gfloat mag;
	uu_denter(-1,(us,"ug_unitvec(%g %g %g)",(*v).x,(*v).y,(*v).z));
	mag=sqrt((*v).x*(*v).x+(*v).y*(*v).y+(*v).z*(*v).z);
	uu_dprint(-1,(us,"ug_unitvec. mag=%g",mag));
	if (mag>.000001) {
		(*v).x=(*v).x/mag;
		(*v).y=(*v).y/mag;
		(*v).z=(*v).z/mag;
	}
	uu_dprint(UU_GITRC,(us,"ug_unitvec returns %g %g %g",(*v).x,(*v).y,(*v).z));
	uu_dexit;
}
