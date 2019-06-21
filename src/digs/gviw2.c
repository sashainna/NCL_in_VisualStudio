/*********************************************************************
**    NAME         :  gviw2.c -- DIGS "traverser" functions
**       CONTAINS:
**    	ug_dmakeras(ws,segptr) --  make raster copy in segment.
**		ug_drasseg(ws,oldseg,newseg,rect)		
**		ug_rasseg2(ws,oldlist,newlist,clipflag)		
**		ug_drasdrw(ws,seg,devrect) -- draw segment seg into devrect.
**		ug_3ras(n,points,sx,sy,dx,dy,raspts)	
**		ug_2ras(n,points,sx,sy,dx,dy,raspts)	
**		ug_nplyln3ras(ws,segptr,cmd,cliprect) -- convert polyline3 to raster in 
**			segptr->rassegpt.
**		ug_nplyln2ras(ws,segptr,cmd,cliprect) -- convert polyline2 to raster in 
**			segptr->rassegpt.
**		ug_nplymk3ras(ws,segptr,cmd,cliprect) -- convert polymarker3 to raster in 
**			segptr->rassegpt.
**		ug_nplymk2ras(ws,segptr,cmd,cliprect) -- convert polymarker2 to raster in 
**			segptr->rassegpt.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       gviw2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:26
*********************************************************************/
#include <stdio.h>
#include "zsysdep.h"
#include "udebug.h"
#define true 1
#define false 0
#define logical int
#include "gerrorst.h"
#include "gerrorid.h"
#include "ginq.h"
#include "gdidd.h"
#include "gvlib.h"
#include "gsegop.h"
#include "usysdef.h"
#include "gsegac.h"
#include "gmat4.h"
#include "ualloc.h"
#include"gconvert.h"
#include "umath.h"

/*
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gviw2.c 2.45 9/25/86 13:56:29 single"};
#else
static char uu_sccsident[]={"@(#) gviw2.c 2.45 9/25/86 13:56:29 double"};
#endif
*/
extern UG_findit ug_find;
extern UU_STORE *uu_toolstore;
static Gint vlst[10];    /* list of active activations of ug_viewsg */
static Gint vlen=0;      /* length of vlst and mod xform stacks */
static Gtran ug_modxfstk[10],ug_lmodxfstk[10];	/* mod xform stacks */
#if UU_DEBUG==1
extern char ug_ops[74][16];		/* character op-codes, for debugging */
#endif
		/* used for conversion routines - to free used memory */
static Gwpoint3 *temp3;
static Gwpoint *temp;

void ug_rasseg2(),ug_3ras(),ug_2ras(),ug_nplyln3ras(),ug_nplyln2ras();
void ug_nplymk3ras(),ug_nplymk2ras();

/*********************************************************************
**    I_FUNCTION :  ug_dmakeras(ws,segptr) --  make raster copy in segment.
**							Used by ug_dclosegras.
**    PARAMETERS   
**       INPUT  : 	Gws ws; -- workstation id.
**						UG_segstli *segptr -- segment header pointer.
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dmakeras(ws,segptr)
Gws ws;
UG_segstli *segptr;
{
	Gdrect rect;				/* map NDC unit square to this dev rect. */
	Gfloat s,sx,sy,dx,dy;		/* workstation xformation */
	Gnrect3 *wsw;
	Gdrect3 *wsvp;
	Gfloat oldmodxform[4][4];		/* save modelling xform here */

	uu_denter(UU_GITRC,(us,"ug_dmakeras(%d,%x)",ws,segptr));
	if (segptr->rassegpt==NULL) {	/* raster seg doesn't exist, make it */
		gqmodxf(oldmodxform);		/* save modelling xform since ug_rasseg2
												might change it */
		/* calculate DC image of NDC unit square under workstation xform*/
		wsw= &(*ug_gksstli.wsopen[ws].outptr).curxform.w;
		wsvp= &(*ug_gksstli.wsopen[ws].outptr).curxform.v;
		sx=(wsvp->urb.x-wsvp->llf.x)/(wsw->urb.x-wsw->llf.x);
		sy=(wsvp->urb.y-wsvp->llf.y)/(wsw->urb.y-wsw->llf.y);
		s=(sx<sy) ? sx : sy;		/* s=min(sx,sy) */
		dx=wsvp->llf.x-wsw->llf.x*s;
		dy=wsvp->llf.y-wsw->llf.y*s;
		uu_dprint(UU_GITRC,(us,
			"ug_dmakeras. sx,sy,s=%g %g %g, dx,dy=%g %g",sx,sy,s,dx,dy));
		rect.ll.x=dx; rect.ll.y=dy;
		rect.ur.x=s+dx; rect.ur.y=s+dy;
		uu_dprint(UU_GITRC,(us,"ug_dmakeras. rect=%g %g %g %g",
			rect.ll.x,rect.ll.y,rect.ur.x,rect.ur.y));
		segptr->rassegpt=(UG_LSI *)uu_toolmalloc(sizeof(UG_LSI));
		/* make a raster cmd list  */
		uu_alloc_push(uu_toolstore);
		ug_lsiinit(segptr->rassegpt);
		uu_alloc_pop();
		vlst[0]=segptr->segid; vlen=1;

		/* reset ndcbox if it doesn't already exist (just to be safe) */
/*		if (segptr->wcboxok == 0)*/
			UG_RESET_WCBOX(segptr);
		ug_rasseg2(ws,segptr,segptr->rassegpt,&rect,1);	

		/* now the ndcbox is completed (if it wasn't before) */
/*		segptr->wcboxok = 1;*/

		ug_smodxf(oldmodxform,UG_MODREPLACE);	/* restore modelling xform
										which ug_rasseg2 might have changed */
	}
	uu_dexit;
}
/*********************************************************************
**    S_FUNCTION     :  Gerror ug_drasseg(ws,oldseg,newseg,rect)		
**		Build a raster segment from an existing (world coord) segment.
**		This routine used by ug_dinitchoice simulation routine when pet==5
**		(graphic menu) to construct a menu segment in newseg.
**    PARAMETERS   
**       INPUT  : 
**				Gws ws -- workstation id (defines raster coordinates).
**				Gseg oldseg -- existing (world coord) segment.
**				Gseg newseg -- Raster segment number to create.
**				Gdrect *rect -- rectangle to map NDC unit square into.
**       OUTPUT :  
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror ug_drasseg(ws,oldseg,newseg,rect)		/* build a raster segment */
Gws ws;							/* workstation id */
Gseg oldseg,newseg;
Gdrect *rect;					/* device coord rectangle to map oldseg into */
{
	Gerror irtn;
	Gfloat oldmodxform[4][4];				/* save old mod xform here */
	UG_segstli *oldsegptr,*newsegptr;
	UG_wdt *wdtpt;
	Gnrect nrect;
	uu_denter(UU_GTRC,(us,"ug_drasseg(%d,%d,%d,%g %g, %g %g)",
		ws,oldseg,newseg,(*rect).ll.x,(*rect).ll.y,(*rect).ur.x,(*rect).ur.y));
	irtn=NCL_NO_ERROR;

	wdtpt=ug_gksstli.wsopen[ws].wdtptr;    /* get wdt pointer to ws */
	gqmodxf(oldmodxform);       	/* get current world coord matrix and save */
	oldsegptr=ug_segac(oldseg);
	newsegptr=ug_segadd(newseg);		/* create new segment */
	if (newsegptr==NULL) {
		irtn=EMEMSPAC; goto rtn;
	}
	newsegptr->segid=newseg;
	newsegptr->rassegpt=NULL;
	newsegptr->xforms=0;
	newsegptr->userdata[0]=ws;			/* save workstation id in segment hdr */

	/* copy dev rect into bounding box, changing to coords raster */
/*
	xras=(*wdtpt).dspsize.raster.x;
	yras=(*wdtpt).dspsize.raster.y;
	s=(xras>yras) ? xras : yras;
	(*newsegptr).ndcbox.ll.x=(*rect).ll.x*s;
	(*newsegptr).ndcbox.ll.y=(*rect).ll.y*s;
	(*newsegptr).ndcbox.ur.x=(*rect).ur.x*s;
	(*newsegptr).ndcbox.ur.y=(*rect).ur.y*s;
	newsegptr->ndcboxok=1;
*/

	/* Convert ndc box to device coordinates */
	ug_segndcbox(newseg,&nrect);
	(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV]) 
		(&rect->ll,&nrect.ll,ws);
	(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV]) 
		(&rect->ur,&nrect.ur,ws);
	/*newsegptr->ndcboxok=1;				 bounding box (dev coords) exists */
	UG_RESET_WCBOX(newsegptr);

	zbytecp(newsegptr->segatts,oldsegptr->segatts);	/* set seg atts */

	/* construct the raster seg list */
	vlst[0]=oldseg;  vlen=1;  /* record this ug_rasseg2 activation */
	ug_rasseg2(ws,oldsegptr,newsegptr->seglist,rect,0);	
	ug_smodxf(oldmodxform,UG_MODREPLACE);	/* restore old mod xform */

rtn:	
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  ug_rasseg2(ws,segptr,newlist,rect,clipflag)		
**		build a raster segment list.
**    PARAMETERS   
**       INPUT  : Gws ws;	-- workstation id 
**						UG_segstli *segptr; -- ptr to real segment
**						UG_LSI newlist;
**						Gdrect *rect; -- device coord rectangle to map oldlist NDC
**							unit square into 
**						int clipflag; --1=clip to current viewport, use current
**							workstation xform to map to raster.  Used by ug_dmakeras
**							to make raster copy of real segments.
**							Else no clip, map NDC unit square into rect. Used by
**							ug_drasseg to make a raster segment for a choice device
**							echo.
**									
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_rasseg2(ws,segptr,newlist,rect,clipflag)	/* build a raster segment list */
Gws ws;							/* workstation id */
UG_segstli *segptr;			/* pointer to real segment */
UG_LSI newlist;
Gdrect *rect;					/* device coord rectangle to map oldlist NDC
										unit square into */
int clipflag;					/* 1=clip to current viewport. Else no clip */
{
	struct UG_lsi *oldlist;		/* segptr->seglist */
	UG_plylna3op *cmd;         /* storage for a UG_cmd */
	int ncmd;						/* number of commands in oldlist */
	int icmd;						/* ith command of oldlist */
	char *ug_lsielt();			/* function to return next list element */
	int i,m,len,opcode;
	Gfloat dx,dy,sx,sy;			/* maps NDC unit square to raster rect */
	UG_wdt *wdtpt;
	logical ptr;
	Gfloat tmpxform[4][4];
	int ix;							/* workstation 's dev to raster scaling factor */
   Gwpoint3 *Getpoints3();
	Gwpoint *Getpoints();

	uu_denter(UU_GITRC,(us,"ug_rasseg2(%d,%x,%x,%g %g,%g %g)",
		ws,segptr,newlist,(*rect).ll.x,(*rect).ll.y,(*rect).ur.x,(*rect).ur.y));

	/* get pointer to oldlist */
	oldlist = segptr->seglist;

	/* concatenate local mod xform to global mod xform */
	ug_matmp(tmpxform,ug_modxform,ug_lmodxform);
	ug_mcopy(ug_modxform,tmpxform);
	ug_ident(ug_lmodxform);			/* set local model xform to identity*/

	wdtpt=ug_gksstli.wsopen[ws].wdtptr;    /* get wdt pointer to ws */

	/* Calc sx,sy,dx,dy to map NDC unit square to raster image of rect */
	ix=(*wdtpt).dspsize.raster.x;
	if ((*wdtpt).dspsize.raster.y>ix) ix=(*wdtpt).dspsize.raster.y;
	sx=((*rect).ur.x-(*rect).ll.x)*ix;
	sy=((*rect).ur.y-(*rect).ll.y)*ix;
	dx=(*rect).ll.x*ix-0.5;
	dy=(*rect).ll.y*ix-0.5;

	ncmd = ug_lsinelt(oldlist);
	uu_denter2(UU_GITRC,(us,"ug_rasseg2. ncmd=%d",ncmd));
	uu_dexit;

	for( icmd=0; icmd < ncmd; ++icmd ){
	 cmd = (UG_plylna3op *)ug_lsielt(oldlist,icmd);
    opcode=(*(UG_plylna3op *)cmd).elttype;
    ptr=false;
    if (opcode<=UG_OPPTR) {
      if ((opcode&UG_OPPTR)!=0) ptr=true;
      opcode=opcode&(UG_OPPTR-1);
    }
	if (((*(UG_plylna3op *)cmd).elttype&UG_OPPTR)!=0) ptr=true; else ptr=false;
	uu_denter2(UU_GITRC,(us,"ug_rasseg2. opcode[%d]=%s ptr=%d\n",
			icmd,ug_ops[opcode],ptr));
	uu_dexit;
	switch (opcode) {
   case UG_NOOP: break;
   case UG_PAGEOP: /*ug_lsins(newlist,(*cmd).gclist,1); */ break;
  
   case UG_PLYLNA3OP:
		if (!clipflag) {
			UG_plylnrasop *raspt;
			static int presize=sizeof(UG_plylnrasop)-sizeof(raspt->pts);
			raspt=(UG_plylnrasop *)uu_toolmalloc(presize 
				+ (*(UG_plylna3op *)cmd).len*sizeof(Gipoint));
			(*raspt).elttype=UG_PLYLNRASOP;
			(*raspt).len=(*(UG_plylna3op *)cmd).len;
			ug_3ras((*raspt).len,
				temp3=Getpoints3((*(UG_plylna3op *)cmd).pts,(*raspt).len),
				sx,sy,dx,dy,raspt->pts);
			ug_lsins(newlist,raspt,(presize+(*raspt).len*sizeof(Gipoint)
				+sizeof(int)-1)/sizeof(int));
			uu_toolfree(raspt);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp3);
#endif
		}
		else {						/* clipflag */
			/* convert polyline3 to add zero or more UG_PLYLNRASOP elements*/
			ug_nplyln3ras(ws,segptr,(UG_plylna3op *)cmd,
				&ug_gksstli.vtran[ug_gksstli.curvwindex].vport);
		}								/* end clipflag */
		break;
    case UG_PLYLNA2OP: 
			if (!clipflag) {
				UG_plylnrasop *raspt;
				static int presize=sizeof(UG_plylnrasop)-sizeof(raspt->pts);
				raspt=(UG_plylnrasop *)uu_toolmalloc(presize 
					+ (*(UG_plylna2op *)cmd).len*sizeof(Gipoint));
		 		(*raspt).len=(*(UG_plylna2op *)cmd).len;
				(*raspt).elttype=UG_PLYLNRASOP;
	uu_dprint(UU_GTRC,(us,"ug_rasseg2. points= %f %f ; %f %f",
(UU_REAL)(*(UG_plylna2op *)cmd).pts[0].x,(UU_REAL)(*(UG_plylna2op *)cmd).pts[0].y,
(UU_REAL)(*(UG_plylna2op *)cmd).pts[1].x,(UU_REAL)(*(UG_plylna2op *)cmd).pts[1].y));
				ug_2ras((*raspt).len,
					temp=Getpoints((*(UG_plylna2op *)cmd).pts,(*raspt).len),
					sx,sy,dx,dy, raspt->pts);
				ug_lsins(newlist,raspt,(presize + (*raspt).len*sizeof(Gipoint)
					+sizeof(int)-1)/sizeof(int));
				uu_toolfree(raspt);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
				uu_toolfree(temp);
#endif
			}
			else {
				/* convert polyline. add zero or more UG_PLYLNRASOP elements*/
				ug_nplyln2ras(ws,segptr,(UG_plylna2op *)cmd,
					&ug_gksstli.vtran[ug_gksstli.curvwindex].vport);
			}						/* end clipflag */
			break;
	 case UG_PLYLNRASOP: len=(*(UG_plylnrasop *)cmd).len;
			/* if clipping, expand ndcbox if it doesn't already exist */
/*
			if (clipflag && (segptr->ndcboxok == 0))
			{
				Gnpoint p1,p2;
				for (i=0; i < (len-1); i++) 
				{
					(*(ug_gksstli.wsopen[ws].connid)[UG_DDEVNDC])
						(&((*(UG_plylnrasop *)cmd).pts[i]),&p1,ws);
					(*(ug_gksstli.wsopen[ws].connid)[UG_DDEVNDC])
						(&((*(UG_plylnrasop *)cmd).pts[i+1]),&p2,ws);
					ug_ndcboxadd(segptr,&p1,&p2);
				}
			}
*/
			ug_lsins(newlist,(UG_plylnrasop *)cmd,
				(sizeof(UG_plylnrasop)-sizeof((*(UG_plylnrasop *)cmd).pts)
				+ len*sizeof(Gipoint)+sizeof(int)-1)/sizeof(int));
			break;
    case UG_PLYMKA3OP: len=(*(UG_plymka3op *)cmd).len;
		if (!clipflag) {
			UG_plymkrasop *raspt;
			static int presize=sizeof(UG_plymkrasop)-sizeof(raspt->pts);
			raspt=(UG_plymkrasop *)uu_toolmalloc(presize 
				+ (*(UG_plymka3op *)cmd).len*sizeof(Gipoint));
		 	(*raspt).len=(*(UG_plymka3op *)cmd).len;
			(*raspt).elttype=UG_PLYMKRASOP;
			ug_3ras((*raspt).len,
				temp3=Getpoints3((*(UG_plymka3op *)cmd).pts,(*raspt).len),
				sx,sy,dx,dy, (*raspt).pts);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp3);
#endif
			ug_lsins(newlist,raspt,(sizeof((*(UG_plymka3op *)cmd).elttype)
				+sizeof(int)+(*raspt).len*sizeof(Gipoint))/sizeof(int));
			uu_toolfree(raspt);
		}
		else {
			ug_nplymk3ras(ws,segptr,cmd,
				&ug_gksstli.vtran[ug_gksstli.curvwindex].vport);
		}
		break;
    case UG_PLYMKA2OP: len=(*(UG_plymka2op *)cmd).len;
		if (!clipflag) {
			UG_plymkrasop *raspt;
			static int presize=sizeof(UG_plymkrasop)-sizeof(raspt->pts);
			raspt=(UG_plymkrasop *)uu_toolmalloc(presize 
				+ (*(UG_plymka2op *)cmd).len*sizeof(Gipoint));
			(*raspt).elttype=UG_PLYMKRASOP;
		 	(*raspt).len=(*(UG_plymka2op *)cmd).len;
			ug_2ras((*raspt).len,
				temp=Getpoints((*(UG_plymka2op *)cmd).pts,(*raspt).len),sx,sy,dx,dy,
				(*raspt).pts);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp);
#endif
			ug_lsins(newlist,raspt,(presize + sizeof(int)-1
				+(*raspt).len*sizeof(Gipoint))/sizeof(int));
		}
		else {
			ug_nplymk2ras(ws,segptr,cmd,
				&ug_gksstli.vtran[ug_gksstli.curvwindex].vport);
		}
		break;
	 case UG_PLYMKRASOP: len=(*(UG_plymkrasop *)cmd).len;
			/* if clipping, expand ndcbox if it doesn't already exist */
/*
			if (clipflag && (segptr->ndcboxok == 0))
			{
				Gnpoint ctr,p1,p2;
				for (i=0; i < len; i++) 
				{
					(*(ug_gksstli.wsopen[ws].connid)[UG_DDEVNDC])
						(&((*(UG_plymkrasop *)cmd).pts[i]),&ctr,ws);
					p1.x = ctr.x - UG_MKRSIZ;
					p1.y = ctr.y - UG_MKRSIZ;
					p2.x = ctr.x + UG_MKRSIZ;
					p2.y = ctr.y + UG_MKRSIZ;
					ug_ndcboxadd(segptr,&p1,&p2);
				}
			}
*/
			ug_lsins(newlist,cmd,(sizeof((*(UG_plymkrasop *)cmd).elttype)
				+sizeof(int)+len*sizeof(Gipoint))/sizeof(int));
			break;
    case UG_TEXTOP: {
			UG_textrasop *raspt;
			int doit;
			Gnpoint3 np;
			static int presize=sizeof(UG_textrasop)-sizeof(raspt->string);
			raspt=(UG_textrasop *)uu_toolmalloc(presize+(*(UG_textop *)cmd).len);
			(*raspt).len=(*(UG_textop *)cmd).len;
			(*raspt).elttype=UG_TEXTRASOP;
			if (!clipflag) {
				doit=1;
				ug_3ras(1,temp3=Getpoints3(&(*(UG_textop *)cmd).position,1),
					sx,sy,dx,dy, &(*raspt).position);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			uu_toolfree(temp3);
#endif
			}
			else {				/* clipflag */
				ug_xform((Gfloat)(*(UG_textop *)cmd).position.x,
					(Gfloat)(*(UG_textop *)cmd).position.y,
					(Gfloat)(*(UG_textop *)cmd).position.z,
					&np,ug_cxform[ug_gksstli.curvwindex]);
				if (ug_clpntrect(&np,
					&ug_gksstli.vtran[ug_gksstli.curvwindex].vport)) {
					/* expand ndcbox if it doesn't already exist */
/*
					if (segptr->ndcboxok == 0)
					{
						Gnrect3 nrect;
						Gnpoint p1,p2;

						ug_txrect2ndc(&np,((*(UG_textop *)cmd).string),&nrect,
							ug_cxform[ug_gksstli.curvwindex],&(ug_gksstli.curprats));
						p1.x = nrect.llf.x;
						p1.y = nrect.llf.y;
						p2.x = nrect.urb.x;
						p2.y = nrect.urb.y;
						ug_ndcboxadd(segptr,&p1,&p2);
					}
*/
					(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])
						(&np,&(*raspt).position,ws);		/*convert to raster */
					doit=1;
				}
				else doit=0;
			}
			if (doit) {
				for (i=0; i<(*raspt).len; i++) 				/* copy the string */
					(*raspt).string[i]=(*(UG_textop *)cmd).string[i];
				(*raspt).string[(*raspt).len]='\0';
				ug_lsins(newlist,raspt,(presize+(*raspt).len
					+sizeof(int))/sizeof(int));
				}
			}
			break;
	 case UG_TEXTRASOP: len=(*(UG_textrasop *)cmd).len;
			/* if clipping, expand ndcbox if it doesn't already exist */
/*
			if (clipflag && (segptr->ndcboxok == 0))
			{
				Gnrect3 nrect;
				Gnpoint npos,p1,p2;

				(*(ug_gksstli.wsopen[ws].connid)[UG_DDEVNDC])
					(&((*(UG_textrasop *)cmd).position),&npos,ws);

				ug_txrect2ndc(&npos,((*(UG_textrasop *)cmd).string),&nrect,
					ug_cxform[ug_gksstli.curvwindex],&(ug_gksstli.curprats));
				p1.x = nrect.llf.x;
				p1.y = nrect.llf.y;
				p2.x = nrect.urb.x;
				p2.y = nrect.urb.y;
				ug_ndcboxadd(segptr,&p1,&p2);
			}
*/
			ug_lsins(newlist,cmd,(sizeof(UG_textrasop)
				-sizeof((*(UG_textrasop *)cmd).string)
				+len+1+sizeof(int)-1)/sizeof(int));
	 case UG_FLAREA3OP:  {
			UG_flarearasop *raspt;
			static int presize=sizeof(UG_flarearasop)-sizeof(raspt->pts);
			raspt=(UG_flarearasop *)uu_toolmalloc(presize
				+2*(*(UG_flarea3op *)cmd).len*sizeof(Gipoint));
			(*raspt).len=(*(UG_flarea3op *)cmd).len;
			(*raspt).elttype=UG_FLAREARASOP;
			if (!clipflag) {
				ug_3ras((*raspt).len,
					temp3=Getpoints3((*(UG_flarea3op *)cmd).pts,(*raspt).len),
					sx,sy,dx,dy,(*raspt).pts);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
				uu_toolfree(temp3);
#endif
				ug_lsins(newlist,raspt,(presize+(*raspt).len*sizeof(Gipoint)
					+sizeof(int)-1)/sizeof(int));
			}
			else {						/* clipflag */
				Gnpoint3 *ndcpts;		/* unclipped NDC  points */
				Gnpoint3 *clippts;	/* clipped NDC points */
				int nclip;				/* size of clippts */
				int i;
				ndcpts=(Gnpoint3 *)uu_toolmalloc(
					(*(UG_flarea3op *)cmd).len*sizeof(Gnpoint3));
				clippts=(Gnpoint3 *)uu_toolmalloc(2*
					(*(UG_flarea3op *)cmd).len*sizeof(Gnpoint3));
				for (i=0; i<(*(UG_flarea3op *)cmd).len; i++) {
					ug_xform((Gfloat)(*(UG_flarea3op *)cmd).pts[i].x,
						(Gfloat)(*(UG_flarea3op *)cmd).pts[i].y,
						(Gfloat)(*(UG_flarea3op *)cmd).pts[i].z,
						&ndcpts[i],ug_cxform[ug_gksstli.curvwindex]);
				}
				ug_facliprect((*(UG_flarea3op *)cmd).len,ndcpts,NULL,&nclip,clippts,
					NULL,&ug_gksstli.vtran[ug_gksstli.curvwindex].vport,1);
				uu_toolfree(ndcpts);		/* free unclipped, NDC points */
				if (nclip>0){
					(*raspt).len=nclip;			
					for (i=0; i<nclip; i++) 
					{
						/* expand ndcbox if it doesn't already exist */
/*
						if ((segptr->ndcboxok == 0) && (i < (nclip-1)))
						{
							p1.x = clippts[i].x;
							p1.y = clippts[i].y;
							p2.x = clippts[i+1].x;
							p2.y = clippts[i+1].y;
							ug_ndcboxadd(segptr,&p1,&p2);
						}
*/

						(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])
							(&clippts[i],&(*raspt).pts[i],ws);	/*convert to raster */
					}
					/* expand ndcbox by last edge */
/*
					if (segptr->ndcboxok == 0)
					{
						p1.x = clippts[nclip-1].x;
						p1.y = clippts[nclip-1].y;
						p2.x = clippts[0].x;
						p2.y = clippts[0].y;
						ug_ndcboxadd(segptr,&p1,&p2);
					}
*/
					ug_lsins(newlist,raspt,(presize+(*raspt).len*sizeof(Gipoint)
						+sizeof(int)-1)/sizeof(int));
				}
				uu_toolfree(clippts);		/* free clipped NDC  points */
			}										/* end clipflag */
			uu_toolfree(raspt);			/* free raster clipped fillarea */
			}									/* end case UG_FLAREA3OP */
			break;
	 case UG_FLAREAOP:  {
			UG_flarearasop *raspt;
			static int presize=sizeof(UG_flarearasop)-sizeof(raspt->pts);
			raspt=(UG_flarearasop *)uu_toolmalloc(presize
				+2*(*(UG_flareaop *)cmd).len*sizeof(Gipoint));
			(*raspt).len=(*(UG_flareaop *)cmd).len;
			(*raspt).elttype=UG_FLAREARASOP;
			if (!clipflag) {
				ug_2ras((*(UG_flareaop *)cmd).len,
					temp=Getpoints((*(UG_flareaop *)cmd).pts,(*raspt).len),
					sx,sy,dx,dy,(*raspt).pts);
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
				uu_toolfree(temp);
#endif
				ug_lsins(newlist,raspt,(presize+(*raspt).len*sizeof(Gipoint)
						+sizeof(int)-1)/sizeof(int));
			}
			else {						/* clipflag */
				Gnpoint3 *ndcpts;		/* unclipped NDC  points */
				Gnpoint3 *clippts;	/* clipped NDC points */
				int nclip;				/* size of clippts */
				int i;
				ndcpts=(Gnpoint3 *)uu_toolmalloc(
					(*(UG_flareaop *)cmd).len*sizeof(Gnpoint3));
				clippts=(Gnpoint3 *)uu_toolmalloc(2*
					(*(UG_flareaop *)cmd).len*sizeof(Gnpoint3));
				for (i=0; i<(*(UG_flareaop *)cmd).len; i++) {
					ug_xform((Gfloat)(*(UG_flareaop *)cmd).pts[i].x,
						(Gfloat)(*(UG_flareaop *)cmd).pts[i].y,(UU_REAL)0.0,
						&ndcpts[i],ug_cxform[ug_gksstli.curvwindex]);
				}
				ug_facliprect((*(UG_flareaop *)cmd).len,ndcpts,NULL,&nclip,clippts,
					NULL,&ug_gksstli.vtran[ug_gksstli.curvwindex].vport,1);
				uu_toolfree(ndcpts);		/* free unclipped, NDC points */
				if (nclip>0){
					(*raspt).len=nclip;			
					for (i=0; i<nclip; i++) 
					{
						/* expand ndcbox if it doesn't already exist */
/*
						if ((segptr->ndcboxok == 0) && (i < (nclip-1)))
						{
							p1.x = clippts[i].x;
							p1.y = clippts[i].y;
							p2.x = clippts[i+1].x;
							p2.y = clippts[i+1].y;
							ug_ndcboxadd(segptr,&p1,&p2);
						}
*/

						(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])
							(&clippts[i],&(*raspt).pts[i],ws);	/*convert to raster */
					}
					/* expand ndcbox by last edge */
/*
					if (segptr->ndcboxok == 0)
					{
						p1.x = clippts[nclip-1].x;
						p1.y = clippts[nclip-1].y;
						p2.x = clippts[0].x;
						p2.y = clippts[0].y;
						ug_ndcboxadd(segptr,&p1,&p2);
					}
*/
					ug_lsins(newlist,raspt,(presize+(*raspt).len*sizeof(Gipoint)
						+sizeof(int)-1)/sizeof(int));
				}
				uu_toolfree(clippts);		/* free clipped NDC  points */
			}										/* end clipflag */
			uu_toolfree(raspt);			/* free raster clipped fillarea */
			}									/* end case UG_FLAREAOP */
			break;
	 case UG_FLAREARASOP: len=(*(UG_flarearasop *)cmd).len;
			/* if clipping, expand ndcbox if it doesn't already exist */
/*
			if (clipflag && (segptr->ndcboxok == 0))
			{
				Gnpoint p1,p2;
				for (i=0; i < (len-1); i++) 
				{
					(*(ug_gksstli.wsopen[ws].connid)[UG_DDEVNDC])
						(&((*(UG_flarearasop *)cmd).pts[i]),&p1,ws);
					(*(ug_gksstli.wsopen[ws].connid)[UG_DDEVNDC])
						(&((*(UG_flarearasop *)cmd).pts[i+1]),&p2,ws);
					ug_ndcboxadd(segptr,&p1,&p2);
				}
				(*(ug_gksstli.wsopen[ws].connid)[UG_DDEVNDC])
					(&((*(UG_flarearasop *)cmd).pts[len-1]),&p1,ws);
				(*(ug_gksstli.wsopen[ws].connid)[UG_DDEVNDC])
					(&((*(UG_flarearasop *)cmd).pts[0]),&p2,ws);
				ug_ndcboxadd(segptr,&p1,&p2);
			}
*/
			ug_lsins(newlist,(UG_flarearasop *)cmd,(sizeof(UG_flarearasop)
				- sizeof((*(UG_flarearasop *)cmd).pts)
				+ (*(UG_flarearasop *)cmd).len*sizeof(Gipoint)
				+sizeof(int)-1)/sizeof(int));
			break;
	 case UG_CELLOP:	
			break;					/* end of case UG_CELLOP */
    case UG_PROCOP: 
			break;
    case UG_CALLOP: 
			m=(*(UG_callop *)cmd).segno;   /* call seg m */
			vlst[vlen]=m;  vlen++;  /* record this ug_rasseg2 activation */
         ug_rasseg2(ws,segptr,ug_segac(m)->seglist,rect,clipflag);
         break;
	case UG_SNTRANOP:  ug_sntran((*(UG_sntranop *)cmd).xform);
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
			ug_slmodxf(a,(*(UG_mtranop *)cmd).type);
		}
#else
			ug_slmodxf((*(UG_lmtranop *)cmd).xf, (*(UG_lmtranop *)cmd).type); 
#endif
			break;
	case UG_DFATSOP: ug_ndfat(newlist); break;
   case UG_LNCOLROP: 
			ug_nlncolr(newlist,(*(UG_lncolrop *)cmd).color); break;
   case UG_MKCOLROP: 
			ug_nmkcolr(newlist,(*(UG_mkcolorop *)cmd).color); break;
   case UG_TXCOLROP: 
			ug_ntxcolr(newlist,(*(UG_txcolorop *)cmd).color); break;
   case UG_LSTYLOP: 
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		{	Glntype b;
			ug_ItoGlntype(b,&(*(UG_lstylop *)cmd).ls);
			ug_nlstl(newlist,&b);
		}
#else
			ug_nlstl(newlist,&(*(UG_lstylop *)cmd).ls);
#endif
			break;
   case UG_LWIDOP: 
			ug_nlwid(newlist,(Gfloat)(*(UG_lwidop *)cmd).width); break;
   case UG_PENOP:  
			/*	ug_npen(newlist,(*cmd).gcint[1]); */ break;
   case UG_FONTOP: 
			ug_nfont(newlist,&(*(UG_fontop *)cmd).p); break;
   case UG_CHHGTOP: {
		Gfloat rasscl;
		Gfloat uvlen;
		Gfloat unitup[3], ndcup[3],zero[3];
		int raszero[2],rasup[2];
		/* unitize char up vector */
		uvlen=sqrt(ug_gksstli.curprats.txuv.x*ug_gksstli.curprats.txuv.x
					+ ug_gksstli.curprats.txuv.y*ug_gksstli.curprats.txuv.y
					+ ug_gksstli.curprats.txuv.z*ug_gksstli.curprats.txuv.z);
		unitup[0]=ug_gksstli.curprats.txuv.x/uvlen;
		unitup[1]=ug_gksstli.curprats.txuv.y/uvlen;
		unitup[2]=ug_gksstli.curprats.txuv.z/uvlen;
		/* convert unitized char up vec to raster */
		gwndc3(&zero[0],&zero[1],&zero[2],(UU_REAL) 0.,(UU_REAL) 0.,(UU_REAL) 0.);
		/* zero in NDC */
		raszero[0]=zero[0]*sx+dx;
		raszero[1]=zero[1]*sy+dy;
		gwndc3(&ndcup[0],&ndcup[1],&ndcup[2],unitup[0],unitup[1],unitup[2]);
		rasup[0]=ndcup[0]*sx+dx;
		rasup[1]=ndcup[1]*sy+dy;
		rasup[0]=rasup[0]-raszero[0];
		rasup[1]=rasup[1]-raszero[1];
		/* char ht scale factor is size of raster char up vector */
		rasscl=rasup[0]*rasup[0]+rasup[1]*rasup[1];
		rasscl=sqrt(rasscl);
		uu_denter2(UU_GTRC,(us,"ug_rasseg2. vwindex=%d txuv=%g %g %g",
			ug_gksstli.curvwindex,ug_gksstli.curprats.txuv.x,
			ug_gksstli.curprats.txuv.y,ug_gksstli.curprats.txuv.z));
		uu_dexit;
		uu_denter2(UU_GTRC,(us,"ug_rasseg2. ndcup=%g %g %g, rasup=%d %d, rasscl=%g",
			ndcup[0],ndcup[1],ndcup[2],rasup[0],rasup[1],rasscl));
		uu_dexit;
		ug_nchhgt(newlist,(*(UG_chhgtop *)cmd).height*rasscl);
		}
		break;
	case UG_CHEXPOP: 
			ug_nchexp(newlist,(Gfloat)(*(UG_chexpop *)cmd).expn); break;
   case UG_CHPLANEOP: 
			ug_nchpl(newlist,(Gfloat)(*(UG_chplaneop *)cmd).txpvc.x,
				(Gfloat)(*(UG_chplaneop *)cmd).txpvc.y,
				(Gfloat)(*(UG_chplaneop *)cmd).txpvc.z);
       	break;
   case UG_CHUP3OP: 
			ug_nchup3(newlist,temp3=Getpoints3(&(*(UG_chup3op *)cmd).upvec,1));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		uu_toolfree(temp3);
#endif
        break;
   case UG_CHUP2OP:
			ug_nchup(newlist,
				temp=Getpoints(&(*(UG_chup2op *)cmd).upvec,1));
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		uu_toolfree(temp);
#endif
        break;
   case UG_CHPATHOP: ug_nchpa(newlist,(*(UG_chpathop *)cmd).path); break;
   case UG_CHSPACEOP: 
			uu_dprint(UU_GTRC,(us,"ug_rasseg2. CHSPACEOP(%g)",
				(*(UG_chspaceop *)cmd).spacing));
			ug_nchsp(newlist,(Gfloat)(*(UG_chspaceop *)cmd).spacing); 
			break;
   case UG_CHJUSTOP: ug_nchj(newlist,&(*(UG_chjustop *)cmd).align); break;
   case UG_SYMBOLOP: ug_nmksy(newlist,(*(UG_symbolop *)cmd).type); break;
   case UG_PICKIDOP: ug_npkid(newlist,(*(UG_pickidop *)cmd).pid); break;
			break;
	case UG_FACOLROP: ug_nfacolr(newlist,(*(UG_facolorop *)cmd).color); break;
	case UG_EDGEFLAGOP: ug_sedgeflag((*(UG_edgeflagop *)cmd).f); break;
    default:/* fprintf(ug_gksos.erfile,"ug_rasseg2 illegal opcode=%d\n",opcode);*/
         break;
    };                             /* case */
  }                                /* for each command in segment */
	vlen=vlen-1;
	uu_denter2(UU_GITRC,(us,"ug_rasseg2 returning."));
	uu_dexit;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_drasdrw(ws,seg,devrect) -- draw seg into devrect.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_drasdrw(wid,seg,rect)
/* draw segment seg mapping it into rect. Change all output primitives
	and character height, expansion, to raster coordinates */
Gws wid;
int seg;
Gdrect *rect;					/* device coord rectangle to map oldseg into */
{
  int ncmd;							/* number of commands in segment */
  int icmd;							/* ith command of a segment */
  char *ug_lsielt();				/* function to return next list element */
  int *p,cmdlen;
  int len,opcode;
  Gfloat dx,dy,sx,sy;			/* maps NDC unit square to raster rect */
  UG_wdt *wdtpt;
  logical ptr;
  Gtran savmodxf;
  Gfloat tmpxform[4][4];
  UG_segstli *oldsegptr;		/* pointer to segment header */
  Gipoint ipt;
  int ix;						/* workstation 's dev to raster scaling factor */

  uu_denter(UU_GITRC,(us,"ug_drasdrw(%d,%d,%g %g,%g %g)",
		wid,seg,(*rect).ll.x,(*rect).ll.y,(*rect).ur.x,(*rect).ur.y));
	gqmodxf(savmodxf);

	/* record this ug_drasdrw activation */
	vlst[vlen]=seg;  vlen=vlen+1;  
	/* push global and local xforms */
	ug_mcopy(ug_modxfstk[vlen-1],ug_modxform);
	ug_mcopy(ug_lmodxfstk[vlen-1],ug_lmodxform);
	/* concatenate local mod xform to global mod xform */
	ug_matmp(tmpxform,ug_modxform,ug_lmodxform);
	ug_mcopy(ug_modxform,tmpxform);
	ug_ident(ug_lmodxform);			/* set local model xform to identity*/

	oldsegptr=ug_segac(seg);		/* get pointer to seg */
	wdtpt=ug_gksstli.wsopen[wid].wdtptr;    /* get wdt pointer to ws */
	/* calc sx,sy,dx,dy to map NDC unit square to raster image of rect */
	ix=(*wdtpt).dspsize.raster.x;
	if ((*wdtpt).dspsize.raster.y>ix) ix=(*wdtpt).dspsize.raster.y;
	sx=((*rect).ur.x-(*rect).ll.x)*ix;
	sy=((*rect).ur.y-(*rect).ll.y)*ix;
	dx=(*rect).ll.x*ix-0.5;
	dy=(*rect).ll.y*ix-0.5;

	ncmd = ug_lsinelt(oldsegptr->seglist);
	uu_denter2(UU_GITRC,(us,"ug_drasdrw. ncmd=%d",ncmd));
	uu_dexit;

	/* for each command in seg n */
	for( icmd=0; icmd < ncmd; ++icmd ){
	 p = (int *)ug_lsielt(oldsegptr->seglist,icmd);	/* p points to command*/
	 cmdlen = ug_lsilen(oldsegptr->seglist,icmd)/sizeof(int);
	 opcode=(*(UG_plylna3op *)p).elttype;
    ptr=false;
    if (opcode<=UG_OPPTR) {
      if ((opcode&UG_OPPTR)!=0) ptr=true;
      opcode=opcode&(UG_OPPTR-1);
    }
	if ((opcode&UG_OPPTR)!=0) ptr=true; else ptr=false;
	uu_denter2(UU_GITRC,(us,"ug_drasdrw. opcode[%d]=%s ptr=%d\n",
			icmd,ug_ops[opcode],ptr));
	uu_dexit;
	switch (opcode) {
		struct {int id; Gws ws; int n; Gipoint *points;} prms;
   case UG_NOOP: break;
   case UG_PAGEOP: break;		/* shouldn't have a PAGEOP in an icon */
  
   case UG_PLYLNA3OP: {
			int reply[4];
			Gipoint *pt;
			prms.n=(*(UG_plylna3op *)p).len;
			prms.ws=wid;
			pt=(Gipoint *)uu_toolmalloc(prms.n*sizeof(Gipoint));
			prms.id=UG_DPOLYLNRAS;
			ug_3ras(prms.n,(*(UG_plylna3op *)p).pts,sx,sy,dx,dy,pt);
			prms.points=pt;
			(*(ug_gksstli.wsopen[wid].connid)[UG_DPOLYLNRAS])(&prms,reply);	
			uu_toolfree(pt);
		}
		break;
    case UG_PLYLNA2OP: {
			int reply[4];
			Gipoint *pt;
			prms.n=(*(UG_plylna2op *)p).len;
			prms.ws=wid;
			pt=(Gipoint *)uu_toolmalloc(prms.n*sizeof(Gipoint));
			prms.id=UG_DPOLYLNRAS;
			ug_2ras(prms.n,(*(UG_plylna2op *)p).pts,sx,sy,dx,dy,pt);
			prms.points=pt;
			(*(ug_gksstli.wsopen[wid].connid)[UG_DPOLYLNRAS])(&prms,reply);	
			uu_toolfree(pt);
		}
		break;
	 case UG_PLYLNRASOP: len=(*(UG_plylnrasop *)p).len;
			ug_polylnras(len,(*(UG_plylnrasop *)p).pts);
			break;
    case UG_PLYMKA3OP: {
			int reply[4];
			Gipoint *pt;
			prms.n=(*(UG_plymka3op *)p).len;
			prms.ws=wid;
			pt=(Gipoint *)uu_toolmalloc(prms.n*sizeof(Gipoint));
			prms.id=UG_DPOLYMKRAS;
			ug_3ras(prms.n,(*(UG_plymka3op *)p).pts,sx,sy,dx,dy,pt);
			prms.points=pt;
			(*(ug_gksstli.wsopen[wid].connid)[UG_DPOLYMKRAS])(&prms,reply);	
			uu_toolfree(pt);
		}
		break;
    case UG_PLYMKA2OP: {
			int reply[4];
			Gipoint *pt;
			prms.n=(*(UG_plymka2op *)p).len;
			prms.ws=wid;
			pt=(Gipoint *)uu_toolmalloc(prms.n*sizeof(Gipoint));
			prms.id=UG_DPOLYMKRAS;
			ug_2ras(prms.n,(*(UG_plymka2op *)p).pts,sx,sy,dx,dy,pt);
			prms.points=pt;
			(*(ug_gksstli.wsopen[wid].connid)[UG_DPOLYMKRAS])(&prms,reply);	
			uu_toolfree(pt);
		}
		break;
	 case UG_PLYMKRASOP: {
			int reply[4];
			prms.ws=wid;
			prms.n=(*(UG_plymkrasop *)p).len;
			prms.id=UG_DPOLYMKRAS;
			prms.points=(*(UG_plymkrasop *)p).pts;
			(*(ug_gksstli.wsopen[wid].connid)[UG_DPOLYMKRAS])(&prms,reply);	
		}
		break;
    case UG_TEXTOP: 
			len=(*(UG_textop *)p).len;
			uu_denter2(UU_GITRC,(us,"ug_drasdrw. len=%d ,text=%s",len,
				(*(UG_textop *)p).string));
			uu_dexit;
			ug_3ras(1,&(*(UG_textop *)p).position,sx,sy,dx,dy,&ipt);
			ug_textras(&ipt,(*(UG_textop *)p).string,wid);
			break;
	 case UG_TEXTRASOP:
			ug_textras(&(*(UG_textop *)p).position,(*(UG_textop *)p).string,wid);
			break;
	 case UG_FLAREA3OP: {
			int reply[4];
			Gipoint *pt;
			prms.n=(*(UG_flarea3op *)p).len;
			prms.ws=wid;
			pt=(Gipoint *)uu_toolmalloc(prms.n*sizeof(Gipoint));
			prms.id=UG_DFLAREARAS;
			ug_3ras(prms.n,(*(UG_flarea3op *)p).pts,sx,sy,dx,dy,pt);
			prms.points=pt;
			(*(ug_gksstli.wsopen[wid].connid)[UG_DFLAREARAS])(&prms,reply);	
			uu_toolfree(pt);
		}
		break;
	 case UG_FLAREAOP: {
			int reply[4];
			Gipoint *pt;
			prms.n=(*(UG_flareaop *)p).len;
			prms.ws=wid;
			pt=(Gipoint *)uu_toolmalloc(prms.n*sizeof(Gipoint));
			prms.id=UG_DFLAREARAS;
			ug_2ras(prms.n,(*(UG_flareaop *)p).pts,sx,sy,dx,dy,pt);
			prms.points=pt;
			(*(ug_gksstli.wsopen[wid].connid)[UG_DFLAREARAS])(&prms,reply);	
			uu_toolfree(pt);
		}
		break;
	 case UG_FLAREARASOP: {
			int reply[4];
			prms.n=(*(UG_flarearasop *)p).len;
			prms.ws=wid;
			prms.id=UG_DFLAREARAS;
			prms.points=(*(UG_flarearasop *)p).pts;
			(*(ug_gksstli.wsopen[wid].connid)[UG_DFLAREARAS])(&prms,reply);	
		}	
		break;
	 case UG_CELLOP:	
/*......not used   */
/*		
		 {
				Gipoint rasrect[2];
				ug_2ras(2,&(*(UG_cellop *)p).rect,sx,sy,dx,dy,rasrect);
				ug_dcellras(rasrect,&(*(UG_cellop *)p).dims,
					(*(UG_cellop *)p).colorarray);
			}
*/			break;					/* end of case UG_CELLOP */
    case UG_PROCOP: 
			break;
    case UG_CALLOP: 
         ug_drasdrw(wid,(*(UG_callop *)p).segno,rect);
         break;
	case UG_SNTRANOP:  ug_sntran((*(UG_sntranop *)p).xform);
			break;
	case UG_MTRANOP: 
			ug_smodxf((*(UG_mtranop *)p).xf, (*(UG_mtranop *)p).type); break;
	case UG_LMTRANOP: 
			ug_slmodxf((*(UG_lmtranop *)p).xf, (*(UG_lmtranop *)p).type); break;
	case UG_DFATSOP: ug_dfats(); break;		/* set default attributes */
   case UG_LNCOLROP: ug_linecolor((*(UG_lncolrop *)p).color); break;
   case UG_MKCOLROP: ug_markcolor((*(UG_mkcolorop *)p).color); break;
   case UG_TXCOLROP: ug_textcolor((*(UG_txcolorop *)p).color); break;
   case UG_LSTYLOP: ug_lineindex(&(*(UG_lstylop *)p).ls); break;
   case UG_LWIDOP: ug_linewidth((*(UG_lwidop *)p).width); break;
   case UG_PENOP:  /* gspen(p[1]); */ break;
   case UG_FONTOP: ug_textfp(&(*(UG_fontop *)p).p); break;
   case UG_CHHGTOP: 
		/* FIX: don't just multiply by sy, but account for charup*/
		ug_charheight((*(UG_chhgtop *)p).height*sy); break;
	case UG_CHEXPOP: ug_charexp((*(UG_chexpop *)p).expn); break;
   case UG_CHPLANEOP: ug_txplane(&(*(UG_chplaneop *)p).txpvc); break;
   case UG_CHUP3OP: ug_charup3(&(*(UG_chup3op *)p).upvec); break;
   case UG_CHUP2OP: ug_charup(&(*(UG_chup2op *)p).upvec); break;
   case UG_CHPATHOP: ug_textpath((*(UG_chpathop *)p).path); break;
   case UG_CHSPACEOP:  
		/* FIX: next line shouldn't just multiply by sx, but account for charup*/
		ug_charspace((*(UG_chspaceop *)p).spacing*sx); break;
   case UG_CHJUSTOP: ug_textalign(&(*(UG_chjustop *)p).align); break;
   case UG_SYMBOLOP: ug_marktype((*(UG_symbolop *)p).type); break;
   case UG_PICKIDOP: ug_spickid((*(UG_pickidop *)p).pid); break;
	case UG_FACOLROP: ug_fillcolor((*(UG_facolorop *)p).color); break;
	case UG_EDGEFLAGOP: ug_sedgeflag((*(UG_edgeflagop *)p).f); break;
    default: /*fprintf(ug_gksos.erfile,"ug_drasdrw illegal opcode=%d\n",opcode);*/
         break;
    };                             /* case */
  }                                /* for each command in segment */
	vlen=vlen-1;
	gsmodxf(savmodxf,UG_MODREPLACE);
	uu_denter2(UU_GITRC,(us,"ug_drasdrw returning."));
	uu_dexit;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_3ras(n,points,sx,sy,dx,dy,raspts)	
**	   convert 3d world coord points to raster.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_3ras(n,points,sx,sy,dx,dy,raspts)	
/* convert 3d world coord points to raster, map unit NDC square to rasrect */
int n;						/* number of points */
Gwpoint3 points[];		/* 3d points */
Gfloat sx,sy,dx,dy;		/* use to convert ndc to raster */
Gipoint raspts[];
{
	Gnpoint3 ndcp;
	int i;
	uu_denter(UU_GITRC,(us,"ug_3ras(%d,%g %g %g,sx,sy=%g %g, dx,dy=%g %g",
			n,points[0].x,points[0].y,points[0].z,sx,sy,dx,dy));
	for (i=0; i<n; i++) {
		ug_xform(points[i].x,points[i].y,points[i].z,&ndcp,
				ug_cxform[ug_gksstli.curvwindex]);
		raspts[i].x=ndcp.x*sx+dx+0.5;
		raspts[i].y=ndcp.y*sy+dy+0.5;
		uu_denter2(UU_GITRC,(us,"ug_3ras. raspts[%d]=%d %d",i,raspts[i].x,
				raspts[i].y));
		uu_dexit;
	}
	uu_dexit;
}
/*********************************************************************
**    I_FUNCTION     :  ug_2ras(n,points,sx,sy,dx,dy,raspts)	
**	   convert 2d world coord points to raster.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_2ras(n,points,sx,sy,dx,dy,raspts)	
/* convert 2d world coord points to raster, map unit NDC square to rasrect */
int n;						/* number of points */
Gwpoint points[];			/* 2d points */
Gfloat sx,sy,dx,dy;		/* use to convert ndc to raster */
Gipoint raspts[];
{
	Gnpoint3 ndcp;
	int i;
	uu_denter(UU_GITRC,(us,"ug_2ras(%d,%g %g,sx,sy=%g %g, dx,dy=%g %g",
			n,points[0].x,points[0].y,sx,sy,dx,dy));
	for (i=0; i<n; i++) {
		ug_xform(points[i].x,points[i].y,(UU_REAL)0.0,&ndcp,
				ug_cxform[ug_gksstli.curvwindex]);
		raspts[i].x=ndcp.x*sx+dx+0.5;
		raspts[i].y=ndcp.y*sy+dy+0.5;
		uu_denter2(UU_GITRC,(us,"ug_2ras. raspts[%d]=%d %d",i,raspts[i].x,
				raspts[i].y));
		uu_dexit;
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_nplyln3ras(ws,segptr,cmd,cliprect) -- convert 
**								a 3D polyline into zero or more PLYLNRASOP elements 
**								in newlist.  Clip to cliprect.
**    PARAMETERS   
**       INPUT  : 	Gws ws; -- workstation id.
**							UG_segstli *segptr; -- segment being converted
**							UG_plylna3op *cmd; -- polyline3 to convert to raster.
**							Gnrect3 *cliprect; -- clip to this rectangle.
**								
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nplyln3ras(ws,segptr,cmd,cliprect)
Gws ws;							/* workstation id */
UG_segstli *segptr;			/* segment being converted */
UG_plylna3op *cmd; 			/*polyline3 to convert to raster.*/
Gnrect3 *cliprect; 			/* clip to this rectangle.  */
{
	UG_plylnrasop *rasptr;
	UG_LSI *newlist;			/* where to put raster polyline */
	int rasn;					/* number of raster points in each raster polyline*/
	int i,onscr,chg2;
	Gnpoint3 np1,np2;			/* unclipped, NDC points */
	Gnpoint3 pos1,pos2;		/* clipped NDC points */
				/* presize = size of a UG_plylnrasop element less the pts*/
	static int presize=sizeof(UG_plylnrasop)-sizeof(rasptr->pts);

	uu_denter(UU_GITRC,(us,
		"ug_nplyln3ras(%x,len=%d cliprect: %g %g %g %g %g %g)",
		segptr,(*cmd).len,cliprect->llf.x,cliprect->llf.y,
		cliprect->llf.z,cliprect->urb.x,cliprect->urb.y,cliprect->urb.z));

	/* get newlist */
	newlist = segptr->rassegpt;

	rasptr=(UG_plylnrasop *)uu_toolmalloc(presize+(*cmd).len*sizeof(Gipoint));
	(*rasptr).elttype=UG_PLYLNRASOP;
	/* xform 1st point to NDC to get started */
	ug_xform((Gfloat)(*cmd).pts[0].x,(Gfloat)(*cmd).pts[0].y,
		(Gfloat)(*cmd).pts[0].z,&np1,
		ug_cxform[ug_gksstli.curvwindex]);
	rasn=0;
	for (i=1; i<(*cmd).len; i++) {		/* for each line in the polyline */
		ug_xform((Gfloat)(*cmd).pts[i].x,(Gfloat)(*cmd).pts[i].y,
				(Gfloat)(*cmd).pts[i].z,&np2,
				ug_cxform[ug_gksstli.curvwindex]);		/* xform to ndc */
		ug_cliprect(&np1,&np2,&pos1,&pos2,&onscr,&chg2,cliprect,
			ug_gksstli.vtran[ug_gksstli.curvwindex].winclip);
		if (onscr!=0) {			/* at least partially on scrn */
			/* expand ndcbox if it doesn't already exist */
/*
			if (segptr->ndcboxok == 0)
			{
				p1.x = pos1.x;
				p1.y = pos1.y;
				p2.x = pos2.x;
				p2.y = pos2.y;
				ug_ndcboxadd(segptr,&p1,&p2);
			}
*/

			/* add another raster point (or two) */
			if (rasn==0) {				/* pick up 1st point also */
				(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])
					(&pos1,&(*rasptr).pts[0],ws);	/*convert to raster */
				rasn=1;
			}
			(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])
				(&pos2,&(*rasptr).pts[rasn],ws);	/*convert to raster */
			rasn++;					/* bump number raster points so far */
		}
		if (chg2==1) {		/* pos2!=np2, i.e. line went off screen, must start
									a new raster polyline.  flush buffer 1st. */
			if (rasn>0) {
				(*rasptr).len=rasn;
				ug_lsins(newlist,rasptr,(presize + (*rasptr).len*sizeof(Gipoint)
					+sizeof(int)-1)/sizeof(int));
			}
			rasn=0;
		}						/* end of flush the raster polyline buffer */
		np1=np2;				/* roll the NDC points */
	}							/* for i=1 to n */
	if (rasn>0) {			/* flush the polyline raster buffer */
		(*rasptr).len=rasn;
		ug_lsins(newlist,rasptr,(presize + (*rasptr).len*sizeof(Gipoint)
				+sizeof(int)-1)/sizeof(int));
	}
	uu_toolfree(rasptr);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_nplyln2ras(ws,segptr,cmd,cliprect) -- convert polyline2 
**								into zero or more PLYLNRASOP elements in newlist.
**								clip to cliprect.
**    PARAMETERS   
**       INPUT  : 	Gws ws;	--  workstation id.
**						UG_segstli *segptr; -- segment being converted
**						UG_plylna2op *cmd; -- polyline2 to convert to raster.
**						Gnrect3 *cliprect; -- clip to this rectangle.
**								
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nplyln2ras(ws,segptr,cmd,cliprect)
Gws ws;						/* workstation id */
UG_segstli *segptr;			/* segment to convert */
UG_plylna2op *cmd; 		/* polyline2 to convert to raster */
Gnrect3 *cliprect; 		/* clip to this rectangle.  */
{
	UG_LSI *newlist;			/* where to put raster polyline */
	UG_plylnrasop *rasptr;
	int rasn;					/* number of raster points in each raster polyline*/
	int i,onscr,chg2;
	Gnpoint3 np1,np2;			/* unclipped, NDC points */
	Gnpoint3 pos1,pos2;		/* clipped NDC points */

				/* presize = size of a UG_plylnrasop element less the pts*/
	static int presize=sizeof(UG_plylnrasop)-sizeof(rasptr->pts);

	uu_denter(UU_GITRC,(us,
		"ug_nplyln2ras(%x,len=%d cliprect: %g %g %g %g %g %g)",
		segptr,(*cmd).len,cliprect->llf.x,cliprect->llf.y,
		cliprect->llf.z,cliprect->urb.x,cliprect->urb.y,cliprect->urb.z));
	
	newlist = segptr->rassegpt;
	rasptr=(UG_plylnrasop *)uu_toolmalloc(presize+(*cmd).len*sizeof(Gipoint));
	(*rasptr).elttype=UG_PLYLNRASOP;
	/* xform 1st point to NDC to get started */
	ug_xform((Gfloat)(*cmd).pts[0].x,(Gfloat)(*cmd).pts[0].y,(UU_REAL)0.0,&np1,
		ug_cxform[ug_gksstli.curvwindex]);
	rasn=0;
	for (i=1; i<(*cmd).len; i++) {		/* for each line in the polyline */
		ug_xform((Gfloat)(*cmd).pts[i].x,(Gfloat)(*cmd).pts[i].y,(UU_REAL)0.0,
		&np2, ug_cxform[ug_gksstli.curvwindex]);		/* xform to ndc */
		ug_cliprect(&np1,&np2,&pos1,&pos2,&onscr,&chg2,cliprect,
			ug_gksstli.vtran[ug_gksstli.curvwindex].winclip);
		if (onscr!=0) {			/* at least partially on scrn */
			/* expand ndcbox if it doesn't already exist */
/*
			if (segptr->ndcboxok == 0)
			{
				p1.x = pos1.x;
				p1.y = pos1.y;
				p2.x = pos2.x;
				p2.y = pos2.y;
				ug_ndcboxadd(segptr,&p1,&p2);
			}
*/

			/* add another raster point (or two) */
			if (rasn==0) {				/* pick up 1st point also */
				(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])
					(&pos1,&(*rasptr).pts[0],ws);	/*convert to raster */
				rasn=1;
			}
			(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])
				(&pos2,&(*rasptr).pts[rasn],ws);	/*convert to raster */
			rasn++;					/* bump number raster points so far */
		}
		if (chg2==1) {		/* pos2!=np2, i.e. line went off screen, must start
									a new raster polyline.  flush buffer 1st. */
			if (rasn>0) {
				(*rasptr).len=rasn;
				ug_lsins(newlist,rasptr,(presize+(*rasptr).len*sizeof(Gipoint)
					+sizeof(int)-1)/sizeof(int));
			}
			rasn=0;
		}						/* end of flush the raster polyline buffer */
		np1=np2;				/* roll the NDC points */
	}							/* for i=1 to n */
	if (rasn>0) {			/* flush the polyline raster buffer */
		(*rasptr).len=rasn;
		ug_lsins(newlist,rasptr,(presize + (*rasptr).len*sizeof(Gipoint)
			+sizeof(int)-1)/sizeof(int));
	}
	uu_toolfree(rasptr);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_nplymk3ras(ws,segptr,cmd,cliprect) -- convert
**							UG_plymka3op in cmd to raster. Clip to cliprect.
**							Add to newlist.
**    PARAMETERS   
**       INPUT  : 	Gws ws; -- workstation id.
**						UG_segstli *segptr; -- segment to convert		
**						UG_plymka3op *cmd; -- polymarker 3D to be converted.
**						Gnrect3 *cliprect; -- clip to here.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nplymk3ras(ws,segptr,cmd,cliprect)
Gws ws;
UG_segstli *segptr;
UG_plymka3op *cmd;		/* 3D polymarker to be converted */
Gnrect3 *cliprect;		/* rectangle to clip to */
{
	/* convert polymarker 3D to a UG_PLYMKRASOP element */
	UG_LSI *newlist;		/* where to put raster polymarker */
	UG_plymkrasop *raspt;
	Gnpoint3 np;
	int n,i;
	int rasn;
	static int presize=sizeof(UG_plymkrasop)-sizeof(raspt->pts);

	uu_denter(UU_GITRC,(us,"ug_nplymk3ras(%d,%x, len=%d)",
		ws,segptr,(*cmd).len));

	newlist = segptr->rassegpt;
	n=(*cmd).len;
	rasn=0;
	raspt=(UG_plymkrasop *)uu_toolmalloc(presize + n*sizeof(Gipoint));
	for (i=0; i<n; i++) {
		ug_xform((Gfloat)(*cmd).pts[i].x,(Gfloat)(*cmd).pts[i].y,(Gfloat)(*cmd).pts[i].z,
		&np, ug_cxform[ug_gksstli.curvwindex]); 			/* xform to ndc */
		if (ug_clpntrect(&np,cliprect)) {
/*
			if (segptr->ndcboxok == 0)
			{
				p1.x = np.x - UG_MKRSIZ;
				p1.y = np.y - UG_MKRSIZ;
				p2.x = np.x + UG_MKRSIZ;
				p2.y = np.y + UG_MKRSIZ;
				ug_ndcboxadd(segptr,&p1,&p2);
			}
*/
			(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])
				(&np,&(*raspt).pts[rasn],ws);	/*convert to raster */
			rasn++;
		}
	}
	if (rasn>0) {
		(*raspt).elttype=UG_PLYMKRASOP;
		(*raspt).len=rasn;
		ug_lsins(newlist,raspt,(presize + (*raspt).len*sizeof(Gipoint)
			+sizeof(int)-1)/sizeof(int));
	}
	uu_toolfree(raspt);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_nplymk2ras(ws,segptr,cmd,cliprect) -- convert
**							UG_plymka2op in cmd to raster. Clip to cliprect.
**							Add to newlist.
**    PARAMETERS   
**       INPUT  : 	Gws ws; -- workstation id.
**						UG_segstli *segptr; -- segment to convert	
**						UG_plymka2op *cmd; -- polymarker 2D to be converted.
**						Gnrect3 *cliprect; -- clip to here.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nplymk2ras(ws,segptr,cmd,cliprect)
Gws ws;
UG_segstli *segptr;	/* segment to convert */
UG_plymka2op *cmd;		/* 2D polymarker to be converted */
Gnrect3 *cliprect;		/* rectangle to clip to */
{
	/* convert polymarker 3D to a UG_PLYMKRASOP element */
	UG_LSI *newlist;		/* where to put raster polymarker */
	UG_plymkrasop *raspt;
	Gnpoint3 np;
	int n,i;
	int rasn;
	static int presize=sizeof(UG_plymkrasop)-sizeof(raspt->pts);

	uu_denter(UU_GITRC,(us,"ug_nplymk2ras(%d,%x, len=%d)",
		ws,segptr,(*cmd).len));

	newlist = segptr->rassegpt;
	n=(*cmd).len;
	rasn=0;
	raspt=(UG_plymkrasop *)uu_toolmalloc(presize + n*sizeof(Gipoint));
	for (i=0; i<n; i++) {
		ug_xform((Gfloat)(*cmd).pts[i].x,(Gfloat)(*cmd).pts[i].y,(UU_REAL)0.0,&np,
			ug_cxform[ug_gksstli.curvwindex]);		/* xform to ndc */
		if (ug_clpntrect(&np,cliprect)) {
/*
			if (segptr->ndcboxok == 0)
			{
				p1.x = np.x - UG_MKRSIZ;
				p1.y = np.y - UG_MKRSIZ;
				p2.x = np.x + UG_MKRSIZ;
				p2.y = np.y + UG_MKRSIZ;
				ug_ndcboxadd(segptr,&p1,&p2);
			}
*/
			(*(ug_gksstli.wsopen[ws].connid)[UG_DNDCDEV])
				(&np,&(*raspt).pts[rasn],ws);	/*convert to raster */
			rasn++;
		}
	}
	if (rasn>0) {
		(*raspt).elttype=UG_PLYMKRASOP;
		(*raspt).len=rasn;
		ug_lsins(newlist,raspt,(presize + (*raspt).len*sizeof(Gipoint)
			+sizeof(int)-1)/sizeof(int));
	}
	uu_toolfree(raspt);
	uu_dexit;
}
