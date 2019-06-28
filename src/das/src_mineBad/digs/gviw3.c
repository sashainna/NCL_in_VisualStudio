/*********************************************************************
**    NAME         :  gviw3.c
**       CONTAINS:
**
**		ug_boxexpln3
**		ug_boxexpln2
**		ug_boxexpmk3
**		ug_boxexpmk2
**		ug_boxexpfa3
**		ug_boxexpfa2
**		ug_txrect
**		ug_txrect2
**		ug_txrect2ndc
**		ug_txbox3
**    ug_savestate
**    ug_resstate
**    ug_boxexpshade
**
**    ug_get_bundle_colors
**    ug_set_bundle_colors
**		ug_box_exp
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gviw3.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:57:12
*********************************************************************/
#include <stdio.h>
#include "zsysdep.h"
#include "usysg.h"
#include "udebug.h"
#include "gtbl.h"
#include "g.h"
#include "ginq.h"
#include "gdidd.h"
#include "usysdef.h"
#include "gmat4.h"
#include "mdcoord.h"
#include "modef.h"
#include "gviw.h"

#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gviw3.c 3.4 5/4/88 16:11:09 single"};
#else
static char uu_sccsident[]={"@(#) gviw3.c 3.4 5/4/88 16:11:09 double"};
#endif
void ug_getmarker_size(int, double *);
void ug_txrect2(Gwpoint3 *,char *,Gnrect3 *,Gtran,UG_prat3 *);
void ug_txrect2ndc(Gnpoint3 *,char *,Gnrect3 *,Gtran,UG_prat3 *);	
/*********************************************************************
**    I_FUNCTION     :  ug_boxexpln3(segptr,points,len)		
**		expand box by polyline 3.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_boxexpln3(segptr,points,len)		/* expand box by polyline 3 */
UG_segstli *segptr;
Gwpoint3 points[];
int len;
{
	int i;
	Gwpoint3 npos1,npos2;
	uu_denter(UU_GITRC,(us,"ug_boxexpln3(seg=%d,points,%d)",(*segptr).segid,len));
#ifdef UU_CHECK
	if (segptr!=NULL) {
#endif
	npos1.x = points[0].x;
	npos1.y = points[0].y;
	npos1.z = points[0].z;
	for (i=1; i<len; i++) {
		npos2.x = points[i].x;
		npos2.y = points[i].y;
		npos2.z = points[i].z;
		ug_wcboxadd(segptr,&npos1,&npos2);
		npos1.x=npos2.x; npos1.y=npos2.y; npos1.z=npos2.z;
	}
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_boxexpln2(segptr,points,len)		
**		expand box by polyline 2.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_boxexpln2(segptr,points,len)		/* expand box by polyline 2 */
UG_segstli *segptr;
Gwpoint points[];
int len;
{
	int i;
	Gwpoint3 npos1,npos2;
	uu_denter(UU_GITRC,(us,"ug_boxexpln2(seg=%d,points,%d)",(*segptr).segid,len));
#ifdef UU_CHECK
	if (segptr!=NULL) {
#endif
	npos1.x = points[0].x;
	npos1.y = points[0].y;
	npos1.z = 0.;
	npos2.z = 0.;
	for (i=1; i<len; i++) {
		npos2.x = points[i].x;
		npos2.y = points[i].y;
		ug_wcboxadd(segptr,&npos1,&npos2);
		npos1.x=npos2.x; npos1.y=npos2.y;
	}
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_boxexpmk3(segptr,points,len)		
**		expand box by polymarker 3.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_boxexpmk3(segptr,points,len)		/* expand box by polymarker 3 */
UG_segstli *segptr;
Gwpoint3 points[];
int len;
{
	int i, mtype;
	double msize;
	Gnpoint3 npos;
	uu_denter(UU_GITRC,(us,"ug_boxexpmk3(seg=%d,points,%d)",(*segptr).segid,len));
#ifdef UU_CHECK
	if (segptr!=NULL) {
#endif
	mtype = gqmarktype();
	ug_getmarker_size(mtype, &msize);
	for (i=0; i<len; i++) {
		npos.x = points[i].x;
		npos.y = points[i].y;
		npos.z = points[i].z;
		(*segptr).wcbox.llf.x=
				(npos.x-msize)<(*segptr).wcbox.llf.x ? 
				(npos.x-msize) : (*segptr).wcbox.llf.x;
		(*segptr).wcbox.llf.y=
			(npos.y-msize)<(*segptr).wcbox.llf.y ? 
			(npos.y-msize) : (*segptr).wcbox.llf.y;
		(*segptr).wcbox.llf.z=
			(npos.z-msize)<(*segptr).wcbox.llf.z ? 
			(npos.z-msize) : (*segptr).wcbox.llf.z;
		(*segptr).wcbox.urb.x=
			(npos.x+msize)>=(*segptr).wcbox.urb.x ? 
			(npos.x+msize) : (*segptr).wcbox.urb.x;
		(*segptr).wcbox.urb.y=
			(npos.y+msize)>=(*segptr).wcbox.urb.y ? 
			(npos.y+msize) : (*segptr).wcbox.urb.y;
		(*segptr).wcbox.urb.z=
			(npos.z+msize)>=(*segptr).wcbox.urb.z ? 
			(npos.z+msize) : (*segptr).wcbox.urb.z;
	}
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_boxexpmk2(segptr,points,len)		
**		expand box by polymarker 2.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_boxexpmk2(segptr,points,len)		/* expand box by polymarker 2 */
UG_segstli *segptr;
Gwpoint points[];
int len;
{
	int i, mtype;
	double msize;
	Gwpoint3 npos;
	uu_denter(UU_GITRC,(us,"ug_boxexpmk2(seg=%d,points,%d)",(*segptr).segid,len));
#ifdef UU_CHECK
	if (segptr!=NULL) {
#endif
	mtype = gqmarktype();
	ug_getmarker_size(mtype, &msize);
	npos.z = 0.;
	for (i=0; i<len; i++) {
		npos.x = points[i].x;
		npos.y = points[i].y;
		(*segptr).wcbox.llf.x=
			(npos.x-msize)<(*segptr).wcbox.llf.x ? 
			(npos.x-msize) : (*segptr).wcbox.llf.x;
		(*segptr).wcbox.llf.y=
			(npos.y-msize)<(*segptr).wcbox.llf.y ? 
			(npos.y-msize) : (*segptr).wcbox.llf.y;
		(*segptr).wcbox.llf.z=
			(npos.z-msize)<(*segptr).wcbox.llf.z ? 
			(npos.z-msize) : (*segptr).wcbox.llf.z;
		(*segptr).wcbox.urb.x=
			(npos.x+msize)>=(*segptr).wcbox.urb.x ? 
			(npos.x+msize) : (*segptr).wcbox.urb.x;
		(*segptr).wcbox.urb.y=
			(npos.y+msize)>=(*segptr).wcbox.urb.y ? 
			(npos.y+msize) : (*segptr).wcbox.urb.y;
		(*segptr).wcbox.urb.z=
			(npos.z+msize)>=(*segptr).wcbox.urb.z ? 
			(npos.z+msize) : (*segptr).wcbox.urb.z;
	}
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_boxexpfa3(segptr,points,len)		
**		expand box for fill area 3.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_boxexpfa3(segptr,points,len)		/* expand box for fill area 3 */
UG_segstli *segptr;							/* pointer to seg's state info */
Gwpoint3 points[];						/* vertices of fill area */
int len;										/* number of vertices */
{
	Gwpoint3 npos1,npos2;
#ifdef UU_CHECK
	if (segptr!=NULL) {
#endif
	ug_boxexpln3(segptr,points,len);	/* do all but closing line of fill area*/
	npos1.x = points[0].x;
	npos1.y = points[0].y;
	npos1.z = points[0].z;
	npos2.x = points[len-1].x;
	npos2.y = points[len-1].y;
	npos2.z = points[len-1].z;
	ug_wcboxadd(segptr,&npos1,&npos2);	/* do closing line of fill area */
#ifdef UU_CHECK
	}
#endif
}

/*********************************************************************
**    I_FUNCTION     :  int ug_boxexpfa2(segptr,points,len)		
**		expand box for fill area 2D.
**    PARAMETERS   
**       INPUT  : 		UG_segstli *segptr -- pointer to seg's state info.
**								Gwpoint points[] -- vertices of fill area.
**								int len -- number of vertices.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_boxexpfa2(segptr,points,len)		/* expand box for fill area 2D */
UG_segstli *segptr;							/* pointer to seg's state info */
Gwpoint points[];						/* vertices of fill area */
int len;										/* number of vertices */
{
	Gnpoint3 npos1,npos2;
#ifdef UU_CHECK
	if (segptr!=NULL) {
#endif
	ug_boxexpln2(segptr,points,len);	/* do all but closing line of fill area*/
	npos1.x = points[0].x;
	npos1.y = points[0].y;
	npos1.z = 0.;
	npos2.x = points[len-1].x;
	npos2.y = points[len-1].y;
	npos2.z = 0.;
	ug_wcboxadd(segptr,&npos1,&npos2);	/* do closing line of fill area */
#ifdef UU_CHECK
	}
#endif
}

/*********************************************************************
**    I_FUNCTION     :  ug_txrect(pos,s,nrect)	
**		determine nrect= text rect for string s.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_txrect(pos,s,nrect)	
/* determine nrect= text rect for string s, starting at
								world coord position pos. */
Gwpoint3 *pos;			/* starting point of text */
char *s;					/* text */
Gnrect3 *nrect;			/* NDC rectangle containing text */
{
	uu_denter(UU_GITRC,(us,"ug_txrect(%g %g %g,%s)",
		(*pos).x,(*pos).y,(*pos).z,s));
	ug_txrect2(pos,s,nrect,ug_cxform[ug_gksstli.curvwindex],
		&(ug_gksstli.curprats));
	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     :  ug_txrect2(pos,s,nrect,cxform,prats)	
**		determine nrect= text rect for string s.
**    PARAMETERS   
**       INPUT  : 
**						Gwpoint3 *pos;				/* starting point of text 
**						char *s;						/* text 
**						Gnrect3 *nrect;			/* NDC rectangle containing text 
**						Gtran cxform;				/* model->NDC transformation 
**						UG_prat3 *prats;			/* primitive attributes
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_txrect2(pos,s,nrect,cxform,prats)	
/* determine nrect= text rect for string s, starting at
									world coord position pos. */
Gwpoint3 *pos;				/* starting point of text */
char *s;						/* text */
Gnrect3 *nrect;				/* NDC rectangle containing text */
Gtran cxform;				/* 4x4 composite (model->NDC) transformation */
UG_prat3 *prats;			/* primitive attributes, only text atts used */
{
	Gnpoint3 npos;				/* NDC starting point of text */
/*	char buf[120];*/

	uu_denter(UU_GITRC,(us,"ug_txrect2(%g %g %g,%s)",
		(*pos).x,(*pos).y,(*pos).z,s));

	/* convert starting point to ndc */
	ug_xform((*pos).x,(*pos).y,(*pos).z,&npos,cxform);

	ug_txrect2ndc(&npos,s,nrect,cxform,prats);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_txrect2ndc(pos,s,nrect,cxform,prats)	
**		determine nrect= text rect for string s.
**    PARAMETERS   
**       INPUT  : 
**						Gnpoint3 *pos;				/* starting point of text 
**						char *s;						/* text 
**						Gnrect3 *nrect;			/* NDC rectangle containing text 
**						Gtran cxform;				/* model->NDC transformation 
**						UG_prat3 *prats;			/* primitive attributes
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_txrect2ndc(pos,s,nrect,cxform,prats)	
/* determine nrect= text rect for string s, starting at
									world coord position pos. */
Gnpoint3 *pos;				/* starting point of text */
char *s;						/* text */
Gnrect3 *nrect;			/* NDC rectangle containing text */
Gtran cxform;				/* 4x4 composite (model->NDC) transformation */
UG_prat3 *prats;			/* primitive attributes, only text atts used */
{
#define EPSZ (UU_REAL) .001
	Gfloat height,width;	/* size of text in NDC units */
	Gfloat ug_ndclen();
	Gtxprec prec;
	Gtxbundl *p;
	UG_wdt *wdtpt;
	Gwstran3 *wsxfmpt;
	Gfloat dx;
	Gtxhor halign;

	uu_denter(UU_GITRC,(us,"ug_txrect2ndc(%g %g %g,%s)",
		(*pos).x,(*pos).y,(*pos).z,s));

	/* calculate text rectangle height, width */
	/* for now assume width = .75*height for each character */

	zbytecp(nrect->llf,*pos);		/* start with pos at llf corner of nrect */
	p= &((*prats).txbundl);
	prec=(*p).fp.prec;					/* current text precision */
	switch((*prats).txpath) {
	case UG_TP_UP: case UG_TP_DOWN: 
		if (prec==UG_STROKE) {		/* use text height if stroke precision */
			width=ug_ndclen((UU_REAL) 0.,(UU_REAL) .75*(*prats).txht,(UU_REAL) 0.,cxform);
			height=ug_ndclen((UU_REAL) 0.,(*prats).txht*(strlen(s)+prats->txbundl.space),
				0.,cxform);
			/* now convert to NDC height,width */
			wsxfmpt= &(*ug_gksstli.wsopen[0].outptr).curxform;
			height=height*((*wsxfmpt).w.urb.y-(*wsxfmpt).w.llf.y)/
						((*wsxfmpt).v.urb.y-(*wsxfmpt).v.llf.y);
			width=width*((*wsxfmpt).w.urb.x-(*wsxfmpt).w.llf.x)/
						((*wsxfmpt).v.urb.x-(*wsxfmpt).v.llf.x);
		}
		else {						/* use hardware size of 1st open workstation */
			wdtpt=ug_gksstli.wsopen[0].wdtptr;    /* get wdt pointer to 1st ws */
			width=(*wdtpt).dspsize.device.x/(*wdtpt).colmax;
			height=strlen(s)*(*wdtpt).dspsize.device.y/(*wdtpt).rowmax;
		}
		if ((*prats).txpath==UG_TP_DOWN) 
				(*nrect).llf.y=(*nrect).llf.y-height;
		break;
	case UG_TP_LEFT: case UG_TP_RIGHT:
		if (prec==UG_STROKE) {
			height=ug_ndclen((*prats).txht,(UU_REAL) 0.,(UU_REAL) 0.,cxform);	/* get ndc height */
			width=(.75*height*(strlen(s)+prats->txbundl.space));	/* ndc width*/
		}
		else {						/* use hardware text size of 1st workstation */
			wdtpt=ug_gksstli.wsopen[0].wdtptr;    /* get wdt pointer to 1st ws */
			height=(*wdtpt).dspsize.device.y/(*wdtpt).rowmax;
			width=strlen(s)*(*wdtpt).dspsize.device.x/(*wdtpt).colmax;
			/* now convert to NDC height,width */
			wsxfmpt= &(*ug_gksstli.wsopen[0].outptr).curxform;
			height=height*((*wsxfmpt).w.urb.y-(*wsxfmpt).w.llf.y)/
						((*wsxfmpt).v.urb.y-(*wsxfmpt).v.llf.y);
			width=width*((*wsxfmpt).w.urb.x-(*wsxfmpt).w.llf.x)/
						((*wsxfmpt).v.urb.x-(*wsxfmpt).v.llf.x);
		}
		/* account for horizontal alignment */
		halign=(*prats).txalign.hor;
		if (halign==UG_TH_NORMAL) {
			if ((*prats).txpath==UG_TP_RIGHT) halign=UG_TH_LEFT;
			else halign=UG_TH_RIGHT;	/* normal for left path is right aligned*/
		}
		switch (halign) {
		case UG_TH_LEFT: dx=0.; break;
		case	UG_TH_CENTRE: dx=width/2.; break;
		case UG_TH_RIGHT: dx=width; break;
		}
		(*nrect).llf.x=(*nrect).llf.x-dx;
		break;
	}						 /* end switch on text path */
	(*nrect).urb.x=(*nrect).llf.x+width; 
	(*nrect).urb.y=(*nrect).llf.y+height;
	/* give the text rect some depth, centered at starting posn */
	(*nrect).urb.z=(*nrect).llf.z-EPSZ;
	(*nrect).llf.z += EPSZ;
	uu_denter2(UU_GITRC,(us,"ug_txrect2ndc returns nrect=%g %g %g, %g %g %g",
		(*nrect).llf.x,(*nrect).llf.y,(*nrect).llf.z,
		(*nrect).urb.x,(*nrect).urb.y,(*nrect).urb.z));
	uu_dexit;
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  ug_txbox3(pos, s, corn)
**       Calculates a 3-D WC box around a text primitive.
**    PARAMETERS   
**       INPUT  : 
**				pos					Starting position of text.
**				s	 					Text string.
**       OUTPUT :  
**				corn					Corners of 3-D WC box.
**    RETURNS      : UU_FAILURE if problem, otherwise UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_txbox3(pos,s,corn)
Gwpoint3 *pos;
char *s;
Gwpoint3 *corn;
{
	UU_REAL len,um_dot(),fabs();
	UM_vector xaxis,vec;
	Gwpoint3 *yaxis,*zaxis,concat;
	Gwrect3 extent;
	Gtxbundl *txp;
	Gtxprec prec;
/*
.....Get the text attributes
*/
	txp = &ug_gksstli.curprats.txbundl;
	prec=(*txp).fp.prec;					/* current text precision */
	if (prec==UG_STROKE)
	{
		zaxis = gqtxplane();
		yaxis = (Gwpoint3 *)gqcharup();
		um_cross(yaxis,zaxis,xaxis);
		um_unitvc(xaxis,xaxis);
	}
/*
.....Get the text extents
*/
	gqtextextent3(UD_ksws,pos,s,&concat,&extent);
/*
.....Create corner points
*/
	if (prec==UG_STROKE)
	{
		um_vctovc(&extent.llf,&corn[0]);
		um_vcmnvc(&extent.urb,&extent.llf,vec);
		len = fabs(um_dot(vec,xaxis));
		um_translate_point(&corn[0],len,xaxis,&corn[1]);
		um_vctovc(&extent.urb,&corn[2]);
		um_translate_point(&corn[2],-len,xaxis,&corn[3]);
	}
	else
	{
		um_vctovc(&extent.llf,&corn[0]);
		corn[1].x = extent.urb.x;
		corn[1].y = extent.llf.x;
		corn[1].z = extent.llf.z;
		um_vctovc(&extent.urb,&corn[2]);
		corn[3].x = extent.llf.x;
		corn[3].y = extent.urb.x;
		corn[3].z = extent.llf.z;
	}
	return;
}	

/*********************************************************************
**    I_FUNCTION     :  ug_savestate( state )
**			Save the current transformation states of gks.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  Gstate *state -- A structure containing values of 
**									all current states of transforms.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_savestate( state )
Gstate *state;
{
	int i;

	uu_denter(UU_GITRC,(us,"ug_savestate(state)"));

	/* Save current normalization transformation number */

	/* Save the (UG_MAXNTRAN) normalization transforms,etc */
	for (i=0; i<UG_MAXNTRAN; i++){
		zbytecp((*state).vtran[i],ug_gksstli.vtran[i]);
		ug_mcopy((*state).cxform[i],ug_cxform[i]);
		ug_mcopy((*state).cxinv[i],ug_cxinv[i]);
		(*state).cxchg[i]=ug_cxchg[i];
	}	


	/* Save the current global modeling transform */
	gqmodxf((*state).modxform); 

	/* Save the current local modeling transform */
	gqlmodxf(state->lmodxform);
	state->lmodidnt = ug_lmodidnt;
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_resstate( state )
**			Restore the state of gks to that of "state".
**    PARAMETERS   
**			INPUT   : Gstate *state -- A structure containing values of 
**									 the requested states (attributes, transforms).
**			OUTPUT  : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_resstate( state )
Gstate *state;
{
	int i;

	uu_denter(UU_GITRC,(us,"ug_reststate()"));

	
	/* Restore the (UG_MAXNTRAN) normalization transforms,etc. */
	/* NOTE: should tell workstations about these */
	for (i=0; i<UG_MAXNTRAN; i++){
		zbytecp(ug_gksstli.vtran[i],(*state).vtran[i]);
		ug_mcopy(ug_cxform[i],(*state).cxform[i]);
		ug_mcopy(ug_cxinv[i],(*state).cxinv[i]);
		ug_cxchg[i]=(*state).cxchg[i];
	}	

	/* Restore the current modeling transform if needed */
	if (ug_matcmp(ug_modxform,(*state).modxform)==1)
		ug_smodxf((*state).modxform,UG_MODREPLACE); 

	if (ug_matcmp(ug_lmodxform,(*state).lmodxform)==1)
		ug_slmodxf((*state).lmodxform,UG_MODREPLACE); 

	ug_lmodidnt = state->lmodidnt;

	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_boxexpshade(segptr,points,len)		
**		expand box for shade area.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_boxexpshade (segptr,points,len)		
UG_segstli *segptr;							/* pointer to seg's state info */
Gwpoint3 points[];						/* vertices/normals of shade area */
int len;										/* number of vertices */
{
	int i;
/*
... do all but closing line of fill area
*/
#ifdef UU_CHECK
	if (segptr!=NULL) {
#endif
/*
.....points[0] to points[len-1] is point Vertex
.....point[len] to points[2*len-1] is for glNormal3f
.....changed by Yurong 5/19/01
*/
/*		for (i=0; i<2*(len-1); i+=2) 
		{
			ug_wcboxadd(segptr,&points[i],&points[i+2]);
		}
*/
		for (i=0; i<(len-1); i+=1) 
		{
			ug_wcboxadd(segptr,&points[i],&points[i+1]);
		}
#ifdef UU_CHECK
	}
#endif

/*
... do closing line of shade area
*/
/*
	ug_wcboxadd(segptr,&points[0],&points[2*len -2]);
*/
	ug_wcboxadd(segptr,&points[0],&points[len-1]);
#ifdef UU_CHECK
	}
#endif
}
/*********************************************************************
**    FUNCTION     :  int ug_get_bundle_colors (ln,mk,tx,fl)		
**		Gets current bundle colors
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_get_bundle_colors (ln,mk,tx,fl)		
int *ln,*mk,*tx,*fl;
{
	*ln = ug_gksstli.curprats.lnbundl.color;
	*mk = ug_gksstli.curprats.mkbundl.color;
	*tx = ug_gksstli.curprats.txbundl.color;
	*fl = ug_gksstli.curprats.flbundl.color;
} 
/*********************************************************************
**    FUNCTION     :  int ug_set_bundle_colors (ln,mk,tx,fl)		
**		Sets current bundle colors
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_set_bundle_colors (ln,mk,tx,fl)		
int ln,mk,tx,fl;
{
	if (ln >= 0) ug_gksstli.curprats.lnbundl.color = ln;
	if (mk >= 0) ug_gksstli.curprats.mkbundl.color = mk;
	if (tx >= 0) ug_gksstli.curprats.txbundl.color = tx;
	if (fl >= 0) ug_gksstli.curprats.flbundl.color = fl;
} 

void ug_getmarker_size(mtype, msize)
int mtype;
double *msize;
{
	(*(ug_gksstli.wsopen[0].connid)[UW_MARKER_SIZE])(mtype, msize);
}

/********************************************************************* 
**  I_FUNCTION:  S_wcboxadd(wcbox,p1,p2) -- expand wc box.
**      Expand wc box to rect defined by p1,p2.
**  PARAMETERS   
**      INPUT:  wcbox: current WC box
**				Gnpoint *p1,*p2 -- two opposite corners of wc rect.
**      OUTPUT: wcbox: expanded box
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
static void S_wcboxadd(wcbox, p1, p2)
Gwrect3 *wcbox;
Gwpoint3 *p1,*p2;					
{
	Gwpoint3 llf,urb;
	
	llf.x=((*p1).x<(*p2).x) ? (*p1).x : (*p2).x;
	llf.y=((*p1).y<(*p2).y) ? (*p1).y : (*p2).y;
	llf.z=((*p1).z<(*p2).z) ? (*p1).z : (*p2).z;
	urb.x=((*p1).x>=(*p2).x) ? (*p1).x : (*p2).x;
	urb.y=((*p1).y>=(*p2).y) ? (*p1).y : (*p2).y;
	urb.z=((*p1).z>=(*p2).z) ? (*p1).z : (*p2).z;
	
	wcbox->llf.x= 
			(llf.x<wcbox->llf.x) ? llf.x : wcbox->llf.x;
	wcbox->llf.y= 
			(llf.y<wcbox->llf.y) ? llf.y : wcbox->llf.y;
	wcbox->llf.z= 
			(llf.z<wcbox->llf.z) ? llf.z : wcbox->llf.z;
	wcbox->urb.x= 
			(urb.x>=wcbox->urb.x) ? urb.x : wcbox->urb.x;
	wcbox->urb.y= 
			(urb.y>=wcbox->urb.y) ? urb.y : wcbox->urb.y;
	wcbox->urb.z= 
			(urb.z>=wcbox->urb.z) ? urb.z : wcbox->urb.z;
}

/*********************************************************************
**    I_FUNCTION     :  ug_box_exp(wcbox, opcode, points,len)		
**		expand box by points of operation.
**    PARAMETERS   
**       INPUT  : 
**          wcbox: current WC box
**			opcode: operation code
**			points: point to expend
**			len: length of points
**       OUTPUT :  
**          wcbox: expanded WC box
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_box_exp(wcbox, opcode, points,len)		/* expand box by polyline 3 */
Gwrect3 *wcbox;
Gwpoint3 points[];
int len, opcode;
{
	int i, mtype;
	Gwpoint3 npos1,npos2;
	double msize;
	Gnpoint3 npos;

	switch (opcode)
	{
    case UG_PLYLNA3OP: 
	{
		npos1.x = points[0].x;
		npos1.y = points[0].y;
		npos1.z = points[0].z;
		for (i=1; i<len; i++) 
		{
			npos2.x = points[i].x;
			npos2.y = points[i].y;
			npos2.z = points[i].z;
			S_wcboxadd(wcbox,&npos1,&npos2);
			npos1.x=npos2.x; npos1.y=npos2.y; npos1.z=npos2.z;
		}
		break;
	}
	case UG_PLYLNA2OP: 
	{
		npos1.x = points[0].x;
		npos1.y = points[0].y;
		npos1.z = 0.;
		npos2.z = 0.;
		for (i=1; i<len; i++) 
		{
			npos2.x = points[i].x;
			npos2.y = points[i].y;
			S_wcboxadd(wcbox,&npos1,&npos2);
			npos1.x=npos2.x; npos1.y=npos2.y;
		}
		break;
	}
    case UG_PLYMKA3OP: 
	{
		mtype = gqmarktype();
		ug_getmarker_size(mtype, &msize);
		for (i=0; i<len; i++) 
		{
			npos.x = points[i].x;
			npos.y = points[i].y;
			npos.z = points[i].z;
			wcbox->llf.x=
					(npos.x-msize)< wcbox->llf.x ? 
					(npos.x-msize) : wcbox->llf.x;
			wcbox->llf.y=
				(npos.y-msize)<wcbox->llf.y ? 
				(npos.y-msize) : wcbox->llf.y;
			wcbox->llf.z=
				(npos.z-msize)<wcbox->llf.z ? 
				(npos.z-msize) : wcbox->llf.z;
			wcbox->urb.x=
				(npos.x+msize)>=wcbox->urb.x ? 
				(npos.x+msize) : wcbox->urb.x;
			wcbox->urb.y=
				(npos.y+msize)>=wcbox->urb.y ? 
				(npos.y+msize) : wcbox->urb.y;
			wcbox->urb.z=
				(npos.z+msize)>=wcbox->urb.z ? 
				(npos.z+msize) : wcbox->urb.z;
		}
		break;
	}
	case UG_PLYMKA2OP: 
	{
		mtype = gqmarktype();
		ug_getmarker_size(mtype, &msize);
		npos.z = 0.;
		for (i=0; i<len; i++) 
		{
			npos.x = points[i].x;
			npos.y = points[i].y;
			wcbox->llf.x=
				(npos.x-msize)<wcbox->llf.x ? 
				(npos.x-msize) : wcbox->llf.x;
			wcbox->llf.y=
				(npos.y-msize)<wcbox->llf.y ? 
				(npos.y-msize) : wcbox->llf.y;
			wcbox->llf.z=
				(npos.z-msize)<wcbox->llf.z ? 
				(npos.z-msize) : wcbox->llf.z;
			wcbox->urb.x=
				(npos.x+msize)>=wcbox->urb.x ? 
				(npos.x+msize) : wcbox->urb.x;
			wcbox->urb.y=
				(npos.y+msize)>=wcbox->urb.y ? 
				(npos.y+msize) : wcbox->urb.y;
			wcbox->urb.z=
				(npos.z+msize)>=wcbox->urb.z ? 
				(npos.z+msize) : wcbox->urb.z;
		}
		break;
	}
	case UG_FLAREA3OP: 
	{
		npos1.x = points[0].x;
		npos1.y = points[0].y;
		npos1.z = points[0].z;
		for (i=1; i<len; i++) 
		{
			npos2.x = points[i].x;
			npos2.y = points[i].y;
			npos2.z = points[i].z;
			S_wcboxadd(wcbox, &npos1, &npos2);
			npos1.x=npos2.x; npos1.y=npos2.y; npos1.z=npos2.z;
		}
		npos1.x = points[0].x;
		npos1.y = points[0].y;
		npos1.z = points[0].z;
		npos2.x = points[len-1].x;
		npos2.y = points[len-1].y;
		npos2.z = points[len-1].z;
		S_wcboxadd(wcbox, &npos1, &npos2);
		break;
	}
	case UG_SHADEAREAOP: 
	{
		for (i=0; i<(len-1); i+=1) 
		{
			S_wcboxadd(wcbox,&points[i],&points[i+1]);
		}
		break;
	}
	case UG_FLAREAOP:
	{
		npos1.x = points[0].x;
		npos1.y = points[0].y;
		npos1.z = 0.;
		npos2.z = 0.;
		for (i=1; i<len; i++) 
		{
			npos2.x = points[i].x;
			npos2.y = points[i].y;
			S_wcboxadd(wcbox,&npos1,&npos2);
			npos1.x=npos2.x; npos1.y=npos2.y;
		}
		break;
		npos1.x = points[0].x;
		npos1.y = points[0].y;
		npos1.z = 0.;
		npos2.x = points[len-1].x;
		npos2.y = points[len-1].y;
		npos2.z = 0.;
		S_wcboxadd(wcbox,&npos1,&npos2);
		break;
	}
	}
}
