/*********************************************************************
**    NAME         :   gseg.c -- DIGS segment functions.
**       CONTAINS:
**		Gerror gcreateseg(segno) 
**		int ug_creaseg(segno)--create segment, no range check.
**      Close the open GKS segment. 
**		Gerror grenameseg(old,new) -- Rename segment. 
**		Gerror gdelallsegs() -- delete all segments.
**		Gerror gdeleteseg(seg) -- Delete segment. 
**		ug_deleseg(n)-- delete seg, no range checking.
**		Gerror gopenseg(seg)  -- re-open segment.
**		Gerror Gassocsegws(seg,ws) -- Associate segment with workstation. 
**		Gerror gcopysegws(seg,ws) -- Copy segment to workstation. 
**		Gerror gcopyseg(seg)  -- insert segment.
**		Gerror gseteltptr(n) -- set element pointer.
**		Gerror gdelelt() -- delete current element
**		Gerror gssegvis(seg,visibility) -- seg segment visibility.
**		Gerror gsseghilite(seg,highlighting) -- Set highlighting. 
**		ug_dseghilite(prms) -- segment highlight workstation simulation.
**		Gerror gssegpri(seg,priority) -- Set segment priority. 
**		Gerror gssegdet(seg,detectability) -- Set detectability. 
**		int gnseg() -- Generate a unused segment id.
**		gsegrud(segno,buf) -- read user segment data.
**		gsegwud(segno,buf) -- write user segment data.
**		gsegerasecolor(color) -- set erase color.
**		int gqvisseg() -- number of visible segs.
**		int gqvissegnos(a) -- identifiers of visible segs.
**		ug_ndcboxdel() -- delete all NDC bounding boxes.
**		ug_ndcntranboxdel(xform) -- delete NDC bounding boxes for a normtran.
**		int ug_notinbox(segptr,x,y)  -- see if xy within box.
**		ug_ndcboxadd(segptr,p1,p2) -- expand ndc box to rect.
**		ug_ndcboxadd(segptr,p1,p2) -- expand ndc box to rect. for wide lines
**		ug_calcbox(segno) -- calculate ndc box.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gseg.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       05/01/17 , 12:40:47
*********************************************************************/

#include "zsysdep.h"
#include <stdio.h>
#include "gtbl.h"
#include "g.h"
#include "gerror.h"
#include "ginq.h"
#include "gdidd.h"
#include "gdidd2.h"
#ifdef RIDGE
#include "/usr/local/include/graf.h"
#endif
#include "udebug.h"
#include "gsegac.h"
#include "uiarray.h"
#include "ualloc.h"

struct {
	int mod;				/* 0=no REPLACE modelling xform in open seg yet */
	int ntran;			/* 0=no GSNORMTRAN in open seg yet */
}	ug_xfseg;	

extern UU_STORE *uu_segstore;
int ug_viwseg= -1;			/* no  segment is being traversed */

/*MILLS: for all platforms, store highest segment number for
		 screen layout segments for Unibase resets.
*/
int NCL_MAXWINSEGNO;
extern int NCLHOST;	/* MILLS:define terminal type on hosts with more that one (SUN) */

void ug_deleseg();
void ug_delvislist();
char *ug_lsielt();

/********************************************************************* 
**  E_FUNCTION:  Gerror gcreateseg(segno) 
**      Create a segment.
**  PARAMETERS   
**      INPUT:  Gseg segno -- id of segment to create.
**
**  RETURNS      : NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gcreateseg(segno)		/* create a segment. Return NCL_NO_ERROR if all OK. */
/*$ INPUT */
Gseg segno;							/* segment number to create */										
{
	Gerror irtn=NCL_NO_ERROR;

	uu_denter(UU_GTRC,(us,"gcreateseg(%d)",segno));

	if((ug_gksos.sysstate!=UG_WSOP)&&(ug_gksos.sysstate!=UG_WSAC)) irtn=ENOTOPAC;
	else if ((segno<UG_MINSEGNO)||(segno>UG_MAXSEGNO)) irtn=EBADNAME;
	else if (ug_segac(segno)!=NULL) irtn=EBADNAME;
	if (irtn==NCL_NO_ERROR) 
		irtn=ug_creaseg(segno);
	else
		ug_errorhand(irtn,"gcreateseg");

	uu_dexit;
	return(irtn);

}

/*********************************************************************
**    I_FUNCTION     :  Gerror ug_creaseg(segno)--create segment, no range check.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : NCL_NO_ERROR if all went OK. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror ug_creaseg(segno)
int segno;
{
	int i, prms[3];
	UG_segstli *p;
	Gerror irtn;
	uu_denter(UU_GITRC,(us,"ug_creaseg(%d)",segno));
	irtn=NCL_NO_ERROR;
/*
.....Update all workstations
*/
/*
	if (ug_getredrwflag()==1)  {
		prms[0]=UG_DUPDATE;
		prms[2]=(int)UG_SUPPRESS;
		ug_wkout(prms,3);
	}
*/
	p=ug_segadd(segno); 
	if (p==NULL) {
		irtn=EMEMSPAC; goto rtn;
	}
	p->segid=segno; 
	UG_RESET_WCBOX(p);				/* resets box & flag */
	(*p).xforms=0;						/* doesn't have output in any xform */
	ug_xfseg.mod=0;					/* no REPLACE modelling in seg yet */
	ug_xfseg.ntran=0;					/* no GSNORMTRAN in seg yet */
	/*p->nowspost=0;*/
	p->segatts=ug_defsegat;		/* set default segment attributes*/
	ug_gksstli.opnseg=segno;		/* remember this segment is open */
	ug_viwseg=segno;					/* for clip/box expansion routines */
	ug_gksstli.curelptr=0;			/* set current element pointer to 
												before 1st elt */
	p->rassegpt=NULL;					/* no raster copy of segment */
/*
....initial user data
....Yurong
*/
	for (i=0;i<UG_UDATA_SIZE;i++)
		p->userdata[i] = 0;
	ug_gksos.sysstate=UG_SGOP;
	/* call device driver's open segment routine */
	prms[0]=UG_DCRESEG;
	prms[2]=segno;
	ug_wkout(prms,3);
rtn:	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gcloseseg() 
**      Close the open GKS segment. 
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gcloseseg()			/* close the open segment */
{
	Gerror irtn;
	int prms[3];

	uu_denter(UU_GTRC,(us,"gcloseseg(seg=%d)", ug_viwseg));

	if (ug_gksos.sysstate!=UG_SGOP) {	/* error, no open segment */
		ug_errorhand(ENOTSGOP,"gcloseseg"); irtn=ENOTSGOP;
	}
	else {
		ug_segcomp(ug_gksstli.opnseg);
		ug_gksos.sysstate=UG_WSAC;
		irtn=NCL_NO_ERROR;

		/* call workstation driver */
		prms[0]=UG_DCLOSEG;
		prms[2]=ug_viwseg;
		ug_wkout(prms,2);

		ug_viwseg= -1;					/* no segment being traversed */
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror grenameseg(old,new) 
**      Rename segment. 
**  PARAMETERS   
**      INPUT:  Gseg old -- old segment id. 
**					 Gseg new -- new segment id.
**
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror grenameseg(old,new)		/* rename segment */
/*$ INPUT */
Gseg old;
Gseg new;
{
	Gerror irtn;
	UG_segstli *p,*q;
	 uu_denter(UU_GTRC,(us,"grenameseg(%d,%d)",old,new));
	irtn=NCL_NO_ERROR;
	if ((p=ug_segac(old))==NULL) irtn=EWHATSEG;	/* error, old seg doesn't exist */
	else if ((new<UG_MINSEGNO)||(new>UG_MAXSEGNO)) irtn=EBADNAME;
	else if (ug_segac(new)!=NULL) irtn=EBADNAME;
	if (irtn!=NCL_NO_ERROR) ug_errorhand(irtn,"grenameseg",NULL);
	else {
		q=ug_segadd(new);
		if (q==NULL) {				/* no memory for new segment header */
			irtn=EMEMSPAC; ug_errorhand(irtn,"grenameseg",NULL);
			goto rtn;
		}
		zbytecp(*q, *p);
		/* (*p).cmdptr=NULL;		 keeps ug_segdel from deleting old segment */
		ug_segdel(old);
	}
rtn:	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gdelallsegs()
**      Delete all segments. This is a PHIGS extension, not in 
**			standard GKS.
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void gdelallsegs()		/* PHIGS delete all segments */
{
	int prms[3],segno;
	UG_segstli *p;

	 uu_denter(UU_GTRC,(us,"gdelallsegs()"));

/* NCL: test to prevent window boarder segments from being deleted when Unibase is
   reset. PSB#7 rel7.5 - rah */
	 ug_seginitscan();
	 while ((p=ug_segscan())!=NULL) {
			 segno=(*p).segid;

		 if (segno > NCL_MAXWINSEGNO)
			 ug_segdel(segno);
	 }
	prms[0]=UG_DREDRAWWS;
/*MILLS: handle redraws on tek terminals from host after deleting most 
		 but not all segs - unibase reset - psb#26 V7.794 */
	prms[1] = 0;
	if (NCLHOST != 0)
		prms[2] = -2;	/* special flag to tek from this routine only !!! */
	else 
		prms[2] = 0;

	ug_wkout(prms,3);		/* call workstations */
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gdeleteseg(seg) -- Delete segment. 
**  PARAMETERS   
**      INPUT:  Gseg seg -- segment id. 
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gdeleteseg(n)		/* delete segment n */
/*$ INPUT */
Gseg n;						/* segment id */
{
	Gerror irtn;

	uu_denter(UU_GTRC,(us,"gdeleteseg(%d)",n));

	irtn=NCL_NO_ERROR;
	if ((n<UG_MINSEGNO)||(n>UG_MAXSEGNO)) irtn=EBADNAME;
	else { if (ug_segac(n)==NULL) irtn=EBADNAME; }
	if (irtn==NCL_NO_ERROR)
		ug_deleseg(n);

	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  ug_deleseg(n)-- delete seg, no range checking.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_deleseg(n)					/* delete seg, no range checking */
Gseg n;
{
	int hiseg;
	UG_segstli *p;
	int prms[3];
	p=ug_segac(n);
	if (p!=NULL) {			/* if segment exists */
		prms[0]=UG_DDELSEG;
		if (p->segatts.ghilit==UG_HIGHLIGHTED) {
			/* first delete the highlighting segment for this seg */
			hiseg=n+UG_MAXSEGNO+1;
			/* because seg n is HIGHLIGHTED, segment hiseg will exist, provided
				the workstation uses the highlighting simulation routine. Thus
				we must make sure the highlighting exists in case the driver 
				(such as IRIS) doesn't use highlighting simulation. */
			if (ug_segac(hiseg)!=NULL) {	/* highlighted segment exists */
				prms[2]=hiseg;
				ug_wkout(prms,3);		/* tell workstation to delete hilit seg*/
				ug_segdel(hiseg);
			}
		}
		prms[2]=n;
		ug_wkout(prms,3);			/* tell workstation to delete segment n */
		ug_segdel(n);
	}
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gopenseg(seg) 
**      Re-open segment. PHIGS extension, not in GKS. 
**  PARAMETERS   
**      INPUT:  Gseg seg -- segment id. 
**
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gopenseg(n)					/* open segment -- not in GKS
											but in PHIGS extension */
/*$ INPUT */
Gseg n;
{
	Gerror irtn;
	int prms[3];
	UG_segstli *segptr;		/* pointer to segment n's header */
	int ncmd;
	
	uu_denter(UU_GTRC,(us,"gopenseg(%d)",n));
	if ((segptr=ug_segac(n))==NULL) {			/* error, seg n doesn't exist */
		ug_errorhand(EBADNAME,"gopenseg"); irtn=EBADNAME;
	}
	else {						/* valid existing segment id */
		uu_dprint(UU_GTRC,(us,"boxok %d, xforms x%x", 
			segptr->wcboxok, segptr->xforms));
		/* access last cmd in segment to make it the current cmd so
			that ug_lsins will add stuff after it. */
  		ncmd = ug_lsinelt(segptr->seglist);	/* number of cmds so far */
		ug_lsielt(segptr->seglist,ncmd-1);	/* access last cmd */
		uu_dprint(UU_GITRC,(us,"gopenseg. seg %d now has %d cmds",n,ncmd));
		/* delete raster copy of seg if it exists */
		if (segptr->rassegpt!=NULL) {			
			uu_alloc_push(uu_segstore);
			ug_lsidel((*(segptr->rassegpt)));
			uu_alloc_pop();
			segptr->rassegpt=NULL;
		}
		prms[0] = UG_DOPNSEG;
		prms[2] = n;
		ug_wkout(prms,3);
		ug_gksstli.opnseg=n;			/* remember this segment is open */
		ug_gksstli.curelptr=ncmd;	/* set current element ptr to last cmd */
		ug_gksos.sysstate=UG_SGOP;
		ug_viwseg=n;
		segptr->wcboxok = 2;						/* Force ndcbox to undefined */
		segptr->xforms &= ~UG_SEGVISLIST;		/* Recheck for visible list */
		irtn=NCL_NO_ERROR;
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror Gassocsegws(seg,ws) 
**      Associate segment with workstation. 
**  PARAMETERS   
**      INPUT:  Gseg seg -- segment id, returned by gcreateseg. 
**					 Gws *ws -- pointer to workstation, returned by gopenws.
**
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gassocsegws(seg,ws)				/* associate seg with workstation */
/*$ INPUT */
Gseg seg;
Gws *ws;
{
	int prms[3],reply[3];
/*	fprintf(ug_gksos.erfile,"**gassocsegws(%d,ws)\n",seg);*/
	prms[0] = UG_DASSSEG;
	prms[2] = seg;
	ug_wkcal(*ws,prms,reply);
	return(reply[0]);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gcopysegws(seg,ws) 
**      Copy segment to workstation. 
**  PARAMETERS   
**      INPUT:  Gseg seg -- segment id, returned by gcreateseg.
**					 Gws *ws -- workstation pointer, returned by gopenws.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gcopysegws(seg,ws)			/* copy segment to workstation */
/*$ INPUT */
Gseg seg;
Gws *ws;
{
	int prms[3],reply[3];
/*	fprintf(ug_gksos.erfile,"**gcopysegws(%d,ws)\n",seg);*/
	prms[0] = UG_DCOPYSEG;
	prms[2] = seg;
	ug_wkcal(*ws,prms,reply);
	return(reply[0]);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gcopyseg(seg) 
**      Insert segment. The contents of the specified segment are
**			inserted into the open segment.
**  PARAMETERS   
**      INPUT:  Gsegtran *transform -- transformation to use to transform seg.
**					 Gseg seg -- segment id, returned by gcreateseg.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gcopyseg(seg)		/* insert segment */
/*$ INPUT */
Gseg seg;
{
	Gerror irtn;
	int opnseg;
	struct { Gint op; Gws id; Gseg seg; } prms;
	UG_segstli *pt1,*pt2;

	uu_denter(UU_GTRC,(us,"gcopyseg(%d)\n",seg));
	irtn=NCL_NO_ERROR;
	opnseg=ug_gksstli.opnseg;
	prms.op = UG_DINSSEG;
	prms.seg=seg;
	ug_wkout ( &prms, sizeof(prms)/sizeof(int) );
	pt1=ug_segac(opnseg);
	pt2=ug_segac(seg);
	if ((pt1!=NULL)&&(pt2!=NULL)) {
		uu_alloc_push(uu_segstore);
		ug_lsicpy(pt1->seglist,pt2->seglist);
		uu_alloc_pop();
  		ug_gksstli.curelptr = 
			ug_lsinelt(pt1->seglist);	/* number of cmds so far */
		UG_RESET_WCBOX(pt1);			/* resets ndcbox & flag */
		pt1->wcboxok=2;					/* make closeseg set boxok=0 */
	}
	else {
  		irtn=EBADNAME;
		uu_denter2(-1,(us,"gcopyseg(%d) error. opnseg(%d) pt=%x, seg pt=%x",
			seg,opnseg,pt1,pt2));
		uu_dexit;
		ug_errorhand(irtn,"gcopyseg",NULL);
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gseteltptr(n)  -- set element pointer to n.
**						There must be a segment open. If n<0, set to 0.
**						If n>number of elements in open structure, set to number
**						of elements.
**  PARAMETERS   
**      INPUT:  int n -- new element pointer (0 thru length of segment,
**								0 is in front of 1st element).
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gseteltptr(n)		/* set element pointer */
/*$ INPUT */
Gint n;
{
	Gerror irtn;
	UG_segstli *sp;
	Gint ncmd;
	int prms[3];

	uu_denter(UU_GTRC,(us,"gseteltptr(%d)",n));
	irtn=NCL_NO_ERROR;
	if (ug_gksos.sysstate!=UG_SGOP) {
		irtn=ENOTSGOP; ug_errorhand(irtn,"gseteltptr",&n);
	}
	else {
		ug_gksstli.curelptr=n;
		if (ug_gksstli.curelptr<0) ug_gksstli.curelptr=0;
		/* get pointer to segment n's header info */
  		sp = ug_segac(ug_gksstli.opnseg);	
  		ncmd = ug_lsinelt(sp->seglist);	/* number of element in segment */
		if (ug_gksstli.curelptr>ncmd) ug_gksstli.curelptr=ncmd;
		/* tell workstation of set element pointer */
		prms[0]=UG_DSETEPT;
		ug_wkout(prms,2);
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gdelelt()  -- delete current element.
**						There must be a segment open.
**  PARAMETERS   
**      INPUT:  none
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gdelelt()		/* delete current element */
{
	Gerror irtn;
	UG_segstli *sp;
	int prms[3];

	uu_denter(UU_GTRC,(us,"gdelelt()"));
	irtn=NCL_NO_ERROR;
	if (ug_gksos.sysstate!=UG_SGOP) {
		irtn=ENOTSGOP; ug_errorhand(irtn,"gdelelt",NULL);
	}
	else {
		sp=ug_segac(ug_gksstli.opnseg);	/* pointer to segment's header info */
		if (ug_gksstli.curelptr>0) {
			prms[0]=UG_DELELT;
			ug_wkout(prms,2); 			/* tell workstation of element delete */
			ug_lsidele(sp->seglist,ug_gksstli.curelptr-1);
			ug_gksstli.curelptr--;
			UG_RESET_WCBOX(sp);			/* resets ndcbox & flag */
			sp->wcboxok=2;				/* causes gcloseseg to reset ndcbox */
		}
  	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gssegvis(seg,visibility) 
**      Set segment visibility.  The visibility of the specified
**			segment is set.
**  PARAMETERS   
**      INPUT:  Gseg seg -- segment id.
**					 Gsegvis visibility -- either UG_VISIBLE or UG_INVISIBLE.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**							EWHATSEG if segment doesn't exist.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gssegvis(seg,v)			/* set segment visibility */
/*$ INPUT */
Gseg seg;
Gsegvis v;
{
	Gerror irtn;
	int prms[4];
	UG_segstli *segptr;
	int hiseg;

	uu_denter(UU_GTRC,(us,"gssegvis(%d,%d)",seg,v));

	irtn=NCL_NO_ERROR;
	segptr=ug_segac(seg);
#ifdef UU_CHECK
	if (segptr==NULL) {
		irtn=EWHATSEG; ug_errorhand(irtn,"gssegvis",NULL);
	}
	if (irtn==NCL_NO_ERROR) {
#endif
	

		if (segptr->segatts.gvis!=v)  {

			/* Delete visible list if setting segment visible...this is
			 * required since the segment may have a low priority, simply
			 * adding it to the end of the visible list results in an
			 * incorrect ordering.
			 */
			ug_delvislist();

			if (v != segptr->segatts.gvis)
			{
				segptr->segatts.gvis=v;
				prms[0] = UG_DSEGVIS;
				prms[2] = seg;
				prms[3] = (int)v;
				ug_wkout(prms,4);

			/* if the segment has a HILITE segment, set its visibility also */
				hiseg=seg+UG_MAXSEGNO+1;
				if (ug_segac(hiseg)!=NULL) {
					prms[2]=hiseg;
					ug_wkout(prms,4);
				}
			}

			/* if setting invisible, set ndcboxok to flag the need to reset box */
/*			if ((int)v == UG_INVISIBLE)
				segptr->ndcboxok = 2;*/
		}

#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsseghilite(seg,highlighting) 
**      Set highlighting. 
**  PARAMETERS   
**      INPUT:  Gseg seg -- segment id.
**					 Gseghi highlighting -- either NORMAL or HIGHLIGHTED.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsseghilite(seg,h)		/* set segment highlighting */
/*$ INPUT */
Gseg seg;
Gseghi h;
{
	UG_segstli *sp;
	int prms[4];
	uu_denter(UU_GTRC,(us,"gsseghilite(%d,%d)",seg,h));
	if ((sp=ug_segac(seg))&&(sp->segatts.ghilit!=h))	{
		/* hilighting is changing */
		prms[0] = UG_DHILITE;
		prms[2] = seg;
		prms[3] = (int)h;
		ug_wkout(prms,4);
		sp->segatts.ghilit=h;
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  S_FUNCTION:  ug_dseghilite(prms) -- segment highlight.
**		 Workstation simulation routine for  segment highlighting using a marker.
**			Marker is placed in another segment with same name
**			as orig segment, but first letter set to 'h'.
**			Hilite at midpoint of middle line or marker of first
**			polyline or polymarker in segment. Only works if
**			no segment is open.
**  PARAMETERS   
**      INPUT:  int prms[4] -- prms[2] contains segment number to hilite.
**										 prms[3] contains either HIGHLIGHTED or NORMAL.
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_dseghilite(prms)			/* simulate segment highlighting using a marker */
							/* marker is placed in another segment with same name
								as orig segment, but first letter set to 'h'
								hilite at midpoint of middle line or marker of first
								polyline or polymarker in segment. Only works if
								no segment is open */
int prms[];
{
	Gseghi h;
	int segno;
	int xf;			/* xformation that seg is drawn in */
	int tmp;			/* temporarily store current xform here */
	Gws ws;
/*	char us[120];*/
	int type,n,m,path[20],depth;
	Gfloat (*coords)[3];
	Gwpoint3 loc;							/* marker location */
	Gcolor oldcolor;
	Gindex oldindx;
	Gseg hsegno;
	UG_segstli *p;
	Gmodmode oldmodmode;
	UG_segstli *segptr;
	int i;

	ws=prms[1];
	segno=prms[2];
	h=(Gseghi)prms[3];

	uu_denter2(UU_GITRC,(us,"ug_dseghilite(ws=%d, seg=%d, hilit=%d)",
		ws,segno,h));

	hsegno=segno+UG_MAXSEGNO+1;
	segptr=ug_segac(segno);
	if (h==UG_HIGHLIGHTED) {				/* turn on highlighting */

		/* calculate x,y position to put highlighting marker */
		gtrav(segno);
		xf=ug_travnx2(&type,&n,&coords,path,&depth);	/* find polyline or marker */
		for(i=0; i<n; i++)
			uu_dprint(UU_GTRC,(us,"coords[%d] = %f %f %f",
				i, coords[i][0], coords[i][1], coords[i][2]));
		if (type!= -1) {				/* found something */
			m=(n+1)/2;					/* use mth coordinate */
			switch(type) {
			case 0:						/* polymarker */
				loc.x=coords[m-1][0]; loc.y=coords[m-1][1];
				loc.z=coords[m-1][2];
				break;
			case 1:				/* polyline, use midpoint  of mth line */
			case 3:				/* fillarea, use midpoint of mth edge */
				loc.x=(coords[m][0]+coords[m-1][0])/2.;
				loc.y=(coords[m][1]+coords[m-1][1])/2.;
				loc.z=(coords[m][2]+coords[m-1][2])/2.;
				break;
			case 2:				/* Text */
				loc.x = coords[0][0];
				loc.y = coords[0][1];
				loc.z = coords[0][2];
				break;
			}

			if ((p=ug_segac(hsegno))==NULL) {	/* make sure hilite seg
														doesn't already exist */
				ug_creaseg(hsegno);				/* assumes no open segment */
				/* if the segment being highlighted is INVISIBLE, make the
					hilite marker INVISIBLE also */
				if (segptr->segatts.gvis==UG_INVISIBLE) 
					gssegvis(segno,UG_INVISIBLE);
				tmp=ug_gksstli.curvwindex;		/* save current xform */
				gsnormtran(xf);
				oldindx=ug_gksstli.curprats.mkbundl.type;
				gsmarktype(7);				/* set mark index */
				oldcolor=ug_gksstli.curprats.mkbundl.color;
				gsmarkcolor(3);				/* set mark color */
				gpolymarker3(1,&loc);			/* draw the marker */
				gcloseseg();
				gssegdet(hsegno,UG_UNDETECTABLE);
				gsmarkcolor(oldcolor);		/* restore marker color */
				gsmarktype(oldindx);		/* restore marker index */
				gsnormtran(tmp);			/* restore current xform */
			}
		}		/* End found something */
	}

	else {	/* Unhilite segno */
		/* hilite seg might not exist, if user tells to make a
			segment UG_NORMAL that is already UG_NORMAL */
		if ((p=ug_segac(hsegno))!=NULL){		/* if hilite segment exists */
			p->segatts.ghilit=UG_NORMAL;		/* keeps deleseg from trying to
														1st delete highlight */
			/* modificate mode is UG_UQUM for deleting the hilite symbol to
				insure that raster devices delete it individually, not
				block delete */
			oldmodmode=(*ug_gksstli.wsopen[prms[1]].outptr).modmode;
			(*ug_gksstli.wsopen[prms[1]].outptr).modmode=UG_UQUM;
			ug_deleseg(hsegno);		/* delete hilite segment */
			(*ug_gksstli.wsopen[prms[1]].outptr).modmode=oldmodmode;
		}
	}

	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gssegpri(seg,priority) -- Set segment priority. 
**						Not implemented yet.
**  PARAMETERS   
**      INPUT:  Gseg seg -- segment id, returned by gcreateseg.
**					 Gsegpri priority -- segment priority, between 0 and 1.
**
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gssegpri(seg,pri)		/* set segment priority */
/*$ INPUT */
Gseg seg;
Gfloat pri;
{
	Gerror irtn;
	UG_segstli *segptr;
	struct { Gint op; Gws id; Gseg sg; Gsegpri pr; } prms;

	uu_denter(UU_GTRC,(us,"gssegpri(%d,%g)",seg,pri));

	irtn=NCL_NO_ERROR;
	if ((segptr=ug_segac(seg))==NULL)  {
		irtn=EBADNAME; ug_errorhand(irtn,"gssegpri",NULL);
	}
	else {
		/* FIX: next line should be workstation dependent maximum prio */
		prms.pr=(pri*(UG_MAXSEGPRIO-1))+1;
		prms.op = UG_DSEGPRI;
		prms.sg = seg;
		ug_wkout(&prms,sizeof(prms)/sizeof(int));
		segptr->segatts.prio=prms.pr;

		/* Delete the visible list, this function changes its order. */
		ug_delvislist();

	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gssegdet(seg,detectability) 
**      Set detectability. 
**  PARAMETERS   
**      INPUT:  Gseg  seg -- segment id. 
**					 Gsegdet detectability -- either UG_DETECTABLE or UG_UNDETECTABLE.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gssegdet(seg,detect)		/* set segment detectability */
/*$ INPUT */
Gseg seg;
Gsegdet detect;
{
	int prms[4];
	UG_segstli *segptr;
	 uu_denter(UU_GTRC,(us,"gssegdet(%d,%d)",seg,detect));
	prms[0] = UG_DSEGDET;
	prms[2] = seg;
	prms[3] = (int)detect;
	ug_wkout(prms,4);
	if (segptr=ug_segac(seg)) segptr->segatts.gdtect=detect;
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  int gnseg() 
**      Generate a unused segment id. UNICAD extension. Not in GKS.
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**
**  RETURNS      :  unused non negative segment id.
**  SIDE EFFECTS :  none
**  WARNINGS     :  if no space available, -1 is returned.
**						  gcreateseg will flag this as an error.
*********************************************************************/
int gnseg()					/* generate unused non-negative segment id */
{
	int i=0;
	static int nxtsegid=UG_MINSEGNO;

	uu_denter(UU_GTRC,(us,"gnseg()"));

	while (ug_segac(nxtsegid)!=NULL) {
		i++;
		nxtsegid++;
		if (nxtsegid>UG_MAXSEGNO) nxtsegid=UG_MINSEGNO;
		if (i > UG_MAXSEGNO) {
			uu_dexit;
			return(-1);
		}
	}

	uu_dexit;
	return(nxtsegid);
}

/********************************************************************* 
**  E_FUNCTION:  gsegrud(segno,buf) -- read user segment data.
**    Read 2 words of user data associated with the specified 
**		segment. UNICAD extension, not in GKS or PHIGS.
**  PARAMETERS   
**    INPUT:  	int segno -- segment number of an existing segment.
**		OUTPUT:	int buf[2] -- two words of user data to be read.
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int gsegrud(segno,buf)			/* read user segment data */
/*$ INPUT */
int segno;
/*$ OUTPUT */
long buf[];
{
	UG_segstli *p;
	int i,status;
	p=ug_segac(segno);
	if (p!=NULL) {
		for (i=0; i<UG_UDATA_SIZE; i++) buf[i]=(*p).userdata[i];
		status = 0;
	}
	else
		status = -1;
	return(status);
}
/********************************************************************* 
**  E_FUNCTION:  gsegwud(segno,buf) -- write user segment data.
**    Write 2 words of user data associated with the specified 
**		segment. UNICAD extension, not in GKS or PHIGS.
**  PARAMETERS   
**    INPUT:  		int segno -- segment number of an existing segment.
**						int buf[2] -- two words of user data to be written.
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void gsegwud(segno,buf)			/* write user segment data */
/*$ INPUT */
int segno;
long buf[];
{
	UG_segstli *p;
	int i;
	p=ug_segac(segno);
	if (p!=NULL) {
		for (i=0; i<UG_UDATA_SIZE; i++) (*p).userdata[i]=buf[i];
	}
}

/*********************************************************************
**    E_FUNCTION :  gsegearsecolor(color) -- segment erase color.
**    PARAMETERS   
**       INPUT  : 	int color -- erase color.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gsegerasecolor(color)
/*$ INPUT */
int color;
{
	uu_denter(UU_GTRC,(us,"gsegerasecolor(%d)",color));
	ug_segerasecolor=color;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  int gqvisseg() -- number of visible segments.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : number of visible segments.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gqvisseg()						/* return number of visible segments */
{
	int irtn;
	UG_segstli *p;
/*	char us[100];*/
	irtn=0;
	ug_seginitscan();
	while ((p=ug_segscan())!=NULL) {
		if (((p->segatts).gvis==UG_VISIBLE)&&((p->segid)<UG_MAXSEGNO)) irtn++;
	}
	uu_denter2(UU_GTRC,(us,"%d=gqvisseg()",irtn));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  int gqvissegnos(segnos) -- visible segment numbers.
**       description
**    PARAMETERS   
**       INPUT  : 	int segnos[] -- array to hold visible segment numbers.
**       OUTPUT :  
**    RETURNS      : length of segnos.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int gqvissegnos(segnos)
/*$ OUTPUT */
int segnos[];
{
	int irtn;
	UG_segstli *p;
/*	char us[100];*/

	irtn=0;
	ug_seginitscan();
	while ((p=ug_segscan())!=NULL) {
		if (((p->segatts).gvis==UG_VISIBLE)&&((p->segid)<UG_MAXSEGNO)) {
			segnos[irtn++]=p->segid;
		}
	}
	uu_denter2(UU_GTRC,(us,"%d=gqvissegnos(%x)",irtn,segnos));
	uu_dexit;
	return(irtn);
}

			/********** below here not user callable ********/
/********************************************************************* 
**  I_FUNCTION:  ug_ndcboxdel() -- delete all NDC bounding boxes.
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_ndcboxdel()					/* delete all ndc bounding boxes */
{
	UG_segstli *p;

	uu_denter(UU_GITRC,(us,"ug_ndcboxdel()"));
	ug_seginitscan();
	while ((p=ug_segscan())!=NULL) 
		if ((*p).segid<2*UG_MAXSEGNO)	{		/* don't do it for raster segments */
/*		UG_RESET_NDCBOX(p);			 resets ndcbox & flag */
/*		(*p).ndcboxok=0;
		(*p).ndcbox.ll.x=10e+30;
		(*p).ndcbox.ll.y=10e+30;
		(*p).ndcbox.ur.x= -10e+30;
		(*p).ndcbox.ur.y= -10e+30; */
	}

	uu_dexit;
}

/********************************************************************* 
**  I_FUNCTION:  ug_ndcntranboxdel(xform) -- delete NDC bounding boxes
**								for segs using normtran xform.
**  PARAMETERS   
**      INPUT:  int xform -- normtran number.
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_ndcntranboxdel(xform)					/* delete ndc bounding boxes */
int xform;
{
	int msk;
	UG_segstli *p;

	uu_denter(UU_GITRC,(us,"ug_ndcntranboxdel(%d)",xform));

	uu_dprint(UU_GITRC,(us,"ug_ntranbox %x", ug_ntranbox));

	if( ug_ntranbox & (1<<xform) ) { /* There are boxes to be deleted */
		if( UU_IALEN(ug_vislist) > 0 ) {	/* Started vislist, can't maintain */
			uu_dprint(UU_GITRC,(us,"setting vislist=0 (out of date)"));
			ug_vislistok = 0;
		}
		UU_IADEL(ug_vislist);				/* Delete the picking list */
		msk=(1<<xform)|(UG_SEGINNTRAN);	/* This normtran or inherited normtran */
		ug_seginitscan();
		while ((p=ug_segscan())!=NULL) {
			if ((*p).segid<2*UG_MAXSEGNO)	{	/* don't do it for raster segments */
				if (((*p).xforms&msk)!=0) {	/* seg uses this or inherited ntran */
/*					UG_RESET_NDCBOX(p);			 resets ndcbox & flag */
					/* delete this seg's raster copy if there is one */
					if (p->rassegpt!=NULL) {
						uu_alloc_push(uu_segstore);
						ug_lsidel((*(p->rassegpt)));
						uu_alloc_pop();
						p->rassegpt=NULL;
					}
					/* remember this seg's ndc coords have changed */ 
/*					(*p).xforms |= UG_SEGNDCBOX;*/
					uu_dprint(UU_GITRC,(us,"ug_ndcntranboxdel. seg(%d).xforms=%x",
						(*p).segid,(*p).xforms));
				}

			 	/* All segs no longer in vis list */
				p->xforms &= (~UG_SEGVISLIST);
				uu_dprint(UU_GITRC,(us,"seg %d xforms %x",p->segid,
					p->xforms));
			}
		}
	
		/* No longer have any ndc boxes for this normtran (clear xform bit) */
		ug_ntranbox &= ~(1<<(xform));
		uu_dprint(UU_GITRC,(us,"ug_ntranbox %x", ug_ntranbox));
	}
	else
		uu_dprint(UU_GITRC,(us,"no boxes to delete"));

	uu_dexit;
}

/********************************************************************* 
**  I_FUNCTION:  int ug_notinbox(segptr,x,y,epsx,epsy) 
**		See if x,y is within(without) segment's bounding box.
**  PARAMETERS   
**      INPUT:  Gegstli *setptr -- pointer to segment header.
**					 Gfloat x,y -- NDC coordinate point.
**					 Gfloat epsx,epsy -- distance from box.
**      OUTPUT: none
**
**  RETURNS      :  0 if box not defined.
**						  1 if box defined and x,y is not within epsx,epsy of the box.
**						  2 if box defined and x,y is within epsx,epsy of the box.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int ug_notinbox(segptr,x,y,epsx,epsy)	/* return 1 if box defined and x,y is
										not within distance epsx,epsy of the box, 
										2 if defined and within, 0 if box not defined */
UG_segstli *segptr;
Gfloat x,y,epsx,epsy;
{
	int irtn;
/*	char us[150];*/
	Gnrect nrect;
	irtn=0;
	if ((*segptr).wcboxok==1) {		/* box defined */
	ug_segndcbox(segptr->segid,&nrect);
		if ((nrect.ll.x>(x+epsx))||(nrect.ll.y>(y+epsy))||
				(nrect.ur.x<(x-epsx))||(nrect.ur.y<(y-epsy)))
		irtn=1;						/* not in box */
		else irtn=2;				/* in box */
	}
	uu_denter2(UU_GITRC,(us,"%d=ug_notinbox(seg=%d,xy=%g,%g, epsx,y=%g %g).",
		irtn,segptr->segid,x,y,epsx,epsy));
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  I_FUNCTION:  ug_ndcboxadd(segptr,p1,p2) -- expand ndc box.
**      Expand ndc box to  rect defined by p1,p2.
**  PARAMETERS   
**      INPUT:  UG_segstli *segptr -- segment header pointer.
**					 Gnpoint *p1,*p2 -- two opposite corners of NDC rect.
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_wcboxadd(segptr,p1,p2)			/* expand ndc box surrounding seg */
UG_segstli *segptr;					/* pointer to segment header */
Gwpoint3 *p1,*p2;					/* opposite corners of NDC rect */
{
	Gwpoint3 llf,urb;
	uu_denter(UU_GITRC,(us,"ug_ndcboxadd(seg=%d,%g %g, %g %g)",(*segptr).segid,
			(*p1).x,(*p1).y,(*p2).x,(*p2).y));
	/* set ll,ur to lower left, upper right of NDC rect */
	llf.x=((*p1).x<(*p2).x) ? (*p1).x : (*p2).x;
	llf.y=((*p1).y<(*p2).y) ? (*p1).y : (*p2).y;
	llf.z=((*p1).z<(*p2).z) ? (*p1).z : (*p2).z;
	urb.x=((*p1).x>=(*p2).x) ? (*p1).x : (*p2).x;
	urb.y=((*p1).y>=(*p2).y) ? (*p1).y : (*p2).y;
	urb.z=((*p1).z>=(*p2).z) ? (*p1).z : (*p2).z;
	/* expand box */
	(*segptr).wcbox.llf.x= 
			(llf.x<(*segptr).wcbox.llf.x) ? llf.x : (*segptr).wcbox.llf.x;
	(*segptr).wcbox.llf.y= 
			(llf.y<(*segptr).wcbox.llf.y) ? llf.y : (*segptr).wcbox.llf.y;
	(*segptr).wcbox.llf.z= 
			(llf.z<(*segptr).wcbox.llf.z) ? llf.z : (*segptr).wcbox.llf.z;
	(*segptr).wcbox.urb.x= 
			(urb.x>=(*segptr).wcbox.urb.x) ? urb.x : (*segptr).wcbox.urb.x;
	(*segptr).wcbox.urb.y= 
			(urb.y>=(*segptr).wcbox.urb.y) ? urb.y : (*segptr).wcbox.urb.y;
	(*segptr).wcbox.urb.z= 
			(urb.z>=(*segptr).wcbox.urb.z) ? urb.z : (*segptr).wcbox.urb.z;

	/* Current normtran now has at least one ndc box */
	ug_ntranbox |= (1<<(ug_gksstli.curvwindex));
	uu_dprint(UU_GITRC,(us,"ug_ntranbox %x", ug_ntranbox));

	uu_dexit;
}


/********************************************************************* 
**  I_FUNCTION:  ug_ndcboxaddln(segptr,p1,p2) -- expand ndc box.
**					same as ndcboxadd exept expands box by 
**					offset * line weight / 2 in order to handle 
**					weighted lines.
**  PARAMETERS   
**      INPUT:  UG_segstli *segptr -- segment header pointer.
**					 Gnpoint *p1,*p2 -- two opposite corners of NDC rect.
**					 Gfloat offset  - usually nominal line width.
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_ndcboxaddln(segptr,p1,p2,offset)	/* expand ndc box surrounding seg */
UG_segstli *segptr;					/* pointer to segment header */
Gnpoint *p1,*p2;					/* opposite corners of NDC rect */
Gfloat offset;
{
/*	Gnpoint ll,ur;
	Gfloat width;*/

	uu_denter(UU_GITRC,(us,"ug_ndcboxaddln(seg=%d,%g %g, %g %g)",(*segptr).segid,
			(*p1).x,(*p1).y,(*p2).x,(*p2).y));
	/* set ll,ur to lower left, upper right of NDC rect */
/*
	ll.x=((*p1).x<(*p2).x) ? (*p1).x : (*p2).x;
	ll.y=((*p1).y<(*p2).y) ? (*p1).y : (*p2).y;
	ur.x=((*p1).x>=(*p2).x) ? (*p1).x : (*p2).x;
	ur.y=((*p1).y>=(*p2).y) ? (*p1).y : (*p2).y;
	if((width = gqlinewidth()) >= 2.0)
		{
		Gfloat diff;

		diff = offset*(width - 1.0) /2.0;
		uu_dprint(UU_GITRC,(us,"ug_ndcboxaddln: diff = %g",diff));
		ll.x -= diff;
		ll.y -= diff;
		ur.x += diff;
		ur.y += diff;
		}
*/
	/* expand box */
/*
	(*segptr).ndcbox.ll.x= 
			(ll.x<(*segptr).ndcbox.ll.x) ? ll.x : (*segptr).ndcbox.ll.x;
	(*segptr).ndcbox.ll.y= 
			(ll.y<(*segptr).ndcbox.ll.y) ? ll.y : (*segptr).ndcbox.ll.y;
	(*segptr).ndcbox.ur.x= 
			(ur.x>=(*segptr).ndcbox.ur.x) ? ur.x : (*segptr).ndcbox.ur.x;
	(*segptr).ndcbox.ur.y= 
			(ur.y>=(*segptr).ndcbox.ur.y) ? ur.y : (*segptr).ndcbox.ur.y;

*/
	/* Current normtran now has at least one ndc box */
/*
	ug_ntranbox |= (1<<(ug_gksstli.curvwindex));
	uu_dprint(UU_GITRC,(us,"ug_ntranbox %x", ug_ntranbox));
*/
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_calcbox(segno) -- calculate ndc box.
**    PARAMETERS   
**       INPUT  : 	int segno; -- segment to calculate box for.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_calcbox(segno)
int segno;
{
	int wkstns[UG_MAXOPWS];
	int i,j;
	uu_denter(UU_GITRC,(us,"ug_calcbox(%d)",segno));
	/* Deactivate all active workstations */
	for(i=0, j=0; i<ug_gksdesctbl.maxopws; i++) { /* for each ws */
		if(ug_gksstli.wsopen[i].connid!=NULL &&
			ug_gksstli.wsopen[i].state==UG_ACTIVE) {
			wkstns[j++] = i;
			gdeactivatews(&i);
   	}
   }
		
	ug_view0(segno,0);		/* this calculates ndc box */

	/* Now reactivate the workstations */
	for(i=0; i<j ; i++) {
		gactivatews(&(wkstns[i]));
	}
	uu_dexit;
}


/********************************************************************* 
**  I_FUNCTION:  ug_delvislist() -- delete the visible list.
**  PARAMETERS   
**      INPUT:  none
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_delvislist()
{
	UG_segstli *p;

	uu_denter(UU_GITRC,(us,"ug_delvislist()"));

	if( ug_vislistok ) {

		/* Delete the picking list */
		ug_vislistok = 0;
		UU_IADEL(ug_vislist);
	
		ug_seginitscan();
		while ((p=ug_segscan())!=NULL) {

		 	/* All segs no longer in vis list */
			p->xforms &= (~UG_SEGVISLIST);
			uu_dprint(UU_GITRC,(us,"seg %d xforms %x",p->segid,
				p->xforms));
		}
	}
	
	uu_dexit;
}
