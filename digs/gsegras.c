/*********************************************************************
**    NAME         :  gsegras.c -- raster seg simulation routines for
**								workstations that want a raster segment copy kept.
**       CONTAINS:
**			ug_dclosegras(prms,reply) -- close seg, make raster copy.
**			ug_ddelsegras(prms) -- delete segment sim routine.
**			ug_dpikras() -- sumulates UG_DPIK using UG_DTRK.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gsegras.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:24
*********************************************************************/
#include "zsysdep.h"
#include <stdio.h>
#include "gtbl.h"
#include "g.h"
#include "udebug.h"
#include "gsegac.h"
#include "gdidd.h"
#include "gviw.h"
#include "gsegop.h"
#include "ginqdsiz.h"
#include "ualloc.h"

#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gsegras.c 3.2 4/13/88 10:13:45 single"};
#else
static char uu_sccsident[]={"@(#) gsegras.c 3.2 4/13/88 10:13:45 double"};
#endif

/* extern char ug_ops[74][16]; */
extern UU_STORE *uu_toolstore;

/*********************************************************************
**    S_FUNCTION :  ug_dclosegras(prms,reply) -- UG_DCLOSEG sim routine for 
**			device drivers that want a raster segment made on seg close.			
**    PARAMETERS   
**       INPUT  : 	int prms[3] -- prms[1] is workstation id.
**       OUTPUT :  	int reply[3] -- not used.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_dclosegras(prms,reply)
int prms[3],reply[3];
{
	UG_segstli *segptr;

	uu_denter(UU_GITRC,(us,"ug_dclosegras() opnseg=%d",ug_gksstli.opnseg));	
	if (ug_gksstli.opnseg<UG_MAXSEGNO) {
		segptr=ug_segac(ug_gksstli.opnseg);
		if (segptr!=NULL) {						/* segment exists */
			if (segptr->rassegpt==NULL) {	/* raster seg doesn't exist, make it */
				ug_dmakeras(prms[1],segptr);
			}
		}											/* end segment exists */
	}
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION :  ug_ddelsegras(prms) -- UG_DDELSEG sim for integer segs.
**    PARAMETERS   
**       INPUT  : 			int prms[]; -- prms[2] contains seg number.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_ddelsegras(prms)
int prms[];
{
	UG_segstli *segptr;
	uu_denter(UU_GITRC,(us,"ug_ddelsegras(%d)",prms[2]));
	if (prms[2]<UG_MAXSEGNO) {
		segptr=ug_segac(prms[2]);
		if (segptr!=NULL) {
			if (segptr->rassegpt!=NULL) {
				uu_alloc_push(uu_toolstore);
				ug_lsidel((*(segptr->rassegpt)));	/* delete raster list */
				uu_alloc_pop();
				segptr->rassegpt=NULL;
			}
		}
	}
	uu_dexit;
}

/*********************************************************************
**    S_FUNCTION     :  int ug_dpikras(wid,seg,depth,xy,k,cursorno)
**			Simulates UG_DPIK in terms of UG_DTRK. Like ug_dpik but 
**			uses raster segs.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_dpikras(wid,seg,depth,xy,k,cursorno)	
/* Simulates UG_DPIK in terms of UG_DTRK. Use raster segs. */
Gws wid;										/* workstation id */
int seg[];									/* for segment nos, pickid */
int *depth;									/* length of seg */
int xy[2];									/* on call init location of pick.
													on return, final location of pick */
int *k;										/* char that ended pick tracking */
int cursorno;								/* which cursor to track:
													0=graphics picking cursor,
													1=menu picking cursor.   */
{
#define APER (UU_REAL) .005
#define PICKTRIGLEN 6
	extern struct {					/* pick triggers that cause picking */
		int dev;							/* device, 0=keypad 2, 1=kbd, 2=fctn keys,
												3=tablet or mouse, 4=tablet square, 5=arrow
												key */
		int min,max;					/* range of values that cause picking */
	} ug_piktrig[PICKTRIGLEN];		/* defined in gddev.c */
	extern int ug_picktriglen;		/* length of piktrig */
	int done,i;
	int aperras;						/* raster aperture corresponding to APER */

	uu_denter(UU_GITRC,(us,"ug_dpikras(wid=%d)",wid));
	/* call workstation entry to track locator cross */
	done=(*(ug_gksstli.wsopen[wid].connid)[UG_DTRK])(wid,xy,k,cursorno,0,0,NULL,NULL);
	aperras=APER*(gqdisplaysize((&wid))->raster.x);
	for (i=0; i<ug_picktriglen; i++) {
		if ((done==ug_piktrig[i].dev)&&(*k>=ug_piktrig[i].min)
			&&(*k<=ug_piktrig[i].max))
			break;						/* legal pick trigger */
	}
	if (i<ug_picktriglen) {				/* pick */
		/* scan raster segment storage for closest seg to xy. */
		ug_dfindpkras(xy,seg,depth,aperras,-1,wid,
				0,0);						/* visible segs, partially within */
	}
	else {								/* don't pick */
		*depth=0;
	}
	uu_dprint(UU_GITRC,(us,
			"ug_dpikras returns %d, seg=%d %d, xy=%d %d, k=%d, depth=%d",
			done,seg[0],seg[1],xy[0],xy[1],*k,*depth));
	uu_dexit;
	return(done);
}

/********************************************************************* 
**  I_FUNCTION:  ug_dfindpkras(loc,seg,depth,aperras,xform,ws,vis,mode) 
**      Workstation routine to find closest segment to loc.
**  PARAMETERS   
**      INPUT:  Gipoint *loc -- point.
**					 Gint aper -- pick aperture.
**      OUTPUT: int seg[] -- segment id list, with pickid at end.
**					 int *depth -- length of seg.
**
**  RETURNS      :  0 if nothihg found close to loc, else 1.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int ug_dfindpkras(rasloc,seg,depth,aperras,xform,ws,vis,mode)	/* find closest graphics seg to loc,
							Return seg=segment ids and pickid, depth=length of seg.
							If not found, return depth=0. If xform>=0,
							Only look thru segs that depend on normtran xform. */
Gipoint *rasloc;
int seg[];
int *depth;
Gint aperras;						/* pick aperture */
Gint xform;				/* normtran to look thru, or -1 */
Gws ws;
Gsegvis vis;			/* visibility, or 2 */
int mode;				/* all in, in, allout, out */
{
	UG_segstli *p;
	int i,j,pkid,irtn;
	int prms[4];
	UG_segat ats;
	int closeseg,newseg[14],newdepth,segno;
	Gint closedist,newdist;
	int mask;
	Gnpoint3 ndcloc;
	int inbox;
	Gfloat gdist;
	char buf[80];					/* for debug */

	uu_denter(UU_GITRC,(us,"ug_dfindpkras(rasloc=%d %d, xform=%d, aper=%d)",
		(*rasloc).x,(*rasloc).y,xform,aperras));
	/* convert to NDC for testing against ndcboxes */
	(*(ug_gksstli.wsopen[ws].connid)[UG_DDEVNDC])(rasloc,&ndcloc,ws);	
	uu_dprint(UU_GITRC,(us,"ug_dfindpkras(ndcloc=%g %g)",ndcloc.x,ndcloc.y));
	/* find item picked */
	zbytecp(ats,ug_defsegat);					/* default atts */
	irtn=0;
	if (xform<0) mask= -1;
	else mask=(1<<xform)|UG_SEGINNTRAN;	/* this xform, or any xform */
	closedist=aperras;
	*depth=0;
	ug_seginitscan();
	while ((p=ug_segscan())!=NULL) {
		i=(*p).segid;
		if (i<=UG_MAXSEGNO) {		/* look thru only graphics segments */
			if ((xform<0)||(((*p).xforms&mask)!=0)) {
				if (p->segatts.gdtect==UG_DETECTABLE) {	
					/*  make sure segment has correct visibility,detectable  
						and within (without) ndcbox */
					if ((vis!=2)&&((p->segatts).gvis!=vis)) 
						break;
					gdist=closedist; gdist=gdist/gqdisplaysize(&ws)->raster.x;
					inbox=ug_notinbox(p,ndcloc.x,ndcloc.y,gdist,gdist);
					if ((mode==0)||(mode==2)) {	
						/* part or all in */
						if (inbox==1) continue;		/* out of box*/
					}
					if (p->rassegpt==NULL) {	/* raster seg list doesn't exist */
						ug_dmakeras(ws,p); 			/* create raster seg. */
					}
					if (ug_finxyras(p,&ats,rasloc,
							closedist,newseg,&newdepth,&pkid,&newdist,mode)) {
						closedist=newdist;
						*depth=newdepth;
						irtn=1;
						segno=i;
						newseg[*depth]=pkid;		/* add pkid to pickpath */
						(*depth)++;
						for (j=0; j<(*depth); j++) seg[j]=newseg[j];
					}
				}		/* end of if detectable */
			}			/* end of if mask */
		}				/* end of if graphics segment */
	}					/* end while */
	uu_dprint(UU_GITRC,(us,"ug_dfindpkras returns. depth=%d, seg=%d %d",
							*depth,seg[0],seg[1]));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_finxyras(segptr,ats,rasloc,eps,seg,seglen,pkid,
**								dist,mode) -- Find line or text near rasloc in segment
**								pointed to by segptr.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT : int seg[10] -- array of segments found item is in.
**						int seglen -- size of seg.
**    RETURNS      : 1 if found,something, 2 if found everything, else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_finxyras(segptr,ats,rasloc,eps,seg,seglen,pkid,dist,mode) 
									/* find line or text 
										within eps of (nx,ny) ndc coords in segment n.
                              Return 1 if found, else 0. If found,
                              return seg=array of instances of
                              segments active when found.
										pkid=current pick id, dist=distance */
UG_segstli *segptr;
int seg[10],*seglen,*pkid;
UG_segat *ats;
Gint eps,*dist;
Gipoint *rasloc;				/* point to find */
Gint mode;						/* pick mode, in, allin, out, allout */
{ 
	int i;
	int inbox;
	UG_findprms findprms;
	int vlen;
	int vlst[20];
	int found;

	uu_denter(UU_GITRC,(us,
		"ug_finxyras(%x,ats,loc=%d,%d,eps=%d,seg,seglen,pkid)",
			segptr,rasloc->x,rasloc->y,eps)); 
	vlen=0;
	*seglen=0;
	/* set up findsgpk parameter structure */
	findprms.curxform=ug_gksstli.curvwindex;
	findprms.pkid=ug_gksstli.curprats.pickid;
	zbytecp(findprms.prats,ug_gksstli.curprats);
	ug_ident(findprms.gmodxf);
	found=ug_findsgpkras(segptr,&findprms,vlst,&vlen,rasloc,eps,eps,NULL,
		mode,pkid,dist);
	uu_dprint(UU_GITRC,(us,"ug_finxyras. vlen=%d",vlen)); 
  	if (found) {
		for (i=0; i<vlen; i++) seg[i]=vlst[i];
  		(*seglen)=vlen;
	}
	
	uu_dprint(UU_GITRC,(us,
			"ug_finxyras(%x,ats,loc=%d %d,%d,%d,%d,*pkid=%d) found=%d, dist=%d", 
			segptr,rasloc->x,rasloc->y,eps,seg[*seglen-1],*seglen,*pkid,
			found,*dist));
	uu_dexit;
	return(found);
}

