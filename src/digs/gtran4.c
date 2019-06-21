/*********************************************************************
**    NAME         :  gtran4.c --  DIGS viewing pipeline(cont).
**       CONTAINS:
**		ug_clip(gcurpos,npos,pos1,pos2,onscr,chg2)
**		ug_clipexp(gcurpos,npos,pos1,pos2,onscr,chg2)
**		ug_clipexpln(gcurpos,npos,pos1,pos2,onscr,chg2, line_offset)
**		ug_cliprect(gcurpos,npos,pos1,pos2,onscr,chg2,rect,clipflag)
**		int ug_lineinrect2(gcurpos,npos,rect)
**		ug_icliprect(gcurpos,npos,pos1,pos2,onscr,chg2,rect)
**		Logical ug_clpnt(pos) -- UG_TRUE iff pos is within viewport.
**		Logical ug_clpntexp(pos) -- UG_TRUE iff pos is within viewport.
**		Logical ug_clpntrect(pos,rect) -- UG_TRUE iff pos is within rect.
**		ug_facliprect(n,points,norms,&nclip,pclip,nclip
**			ug_gksstli.vtran[ug_gksstli.curvwindex].vport); -- fill area clip. 
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**       gtran4.c , 25.1
**    MODULE NAME AND RELEASE LEVEL
**       04/29/15 , 15:05:25
*********************************************************************/
#include <stdio.h>
#include "umath.h"
#include "go1.h"
#include "go2.h"
#include "go3.h"
#include "gobas.h"
#include "goseg.h"
#include "gomisc.h"
#include "goatt.h"
#include "gobndl.h"
#include "gofac.h"
#include "gows.h"
#include "gi1.h"
#include "gichoice.h"
#include "giloc.h"
#include "gistr.h"
#include "gistroke.h"
#include "gipick.h"
#include "gival.h"
#include "gimisc.h"
#include "gtblopst.h"
#include "gtbldesc.h"
#include "gtblws.h"
#include "gtblseg.h"
#include "gtblst.h"
#include "gtbluni.h"
#include "gtblvar2.h"
#include "gtblvar4.h"
#include "gtblvar6.h"
#include "gtblvar7.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gviw.h"
#include "udebug.h"
#include "gsegac.h"
#include "jplot.h"
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gtran4.c 1.33 3/24/87 14:53:42 single"};
#else
static char uu_sccsident[]={"@(#) gtran4.c 1.33 3/24/87 14:53:42 double"};
#endif
#define UG_TRUE 1
#define UG_FALSE 0
#define Logical int
static Gfloat vwport[3][2];		/* temporary copy for loops */
extern int ug_viwseg;
/*********************************************************************
**    I_FUNCTION     :  ug_clip(gcurpos,npos,pos1,pos2,onscr,chg2)
**       Clip the line from gcurpos to npos to the current viewport
**			if ug_clip2flag==0. Else clip to current ug_clip2rect
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_clip(gcurpos,npos,pos1,pos2,onscr,chg2)
/* clip line from gcurpos to npos. return 2 (possibly different)
   points in pos1, pos2. onscr=1 if anything is within current vport. chg2=1
   if npos!=pos2
  gcurpos and npos in ndc coords, like vport.
*/
Gfloat gcurpos[3],npos[3],pos1[3],pos2[3];
int *onscr, *chg2;
{
	int i;
	char us[120];
	i=ug_gksstli.curvwindex;
	if (ug_clip2flag!=1) {
		ug_cliprect(gcurpos,npos,pos1,pos2,onscr,chg2,
			&ug_gksstli.vtran[i].vport,ug_gksstli.vtran[i].winclip);
	}
	else
  {
		uu_denter2(UU_GITRC,(us,"ug_clip(clip2null[%d]=%d",i,ug_clip2null[i]));
		uu_dexit;
		if (ug_clip2null[i]==1) {			/* ug_clip2rect is null */
			*onscr=0; *chg2=1;
		}
		else
			ug_cliprect(gcurpos,npos,pos1,pos2,onscr,chg2,
				&ug_clip2rect[i],ug_gksstli.vtran[i].winclip);
	}
}

/*********************************************************************
**    I_FUNCTION     :  ug_clipexp(gcurpos,npos,pos1,pos2,onscr,chg2)
**			Just like ug_clip, but also expands ndcbox.
**       Clip the line from gcurpos to npos to the current viewport
**			if ug_clip2flag==0. Else clip to current ug_clip2rect.
**			If a segment open and ndcseg>0, expand
**			ndcbox.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_clipexp(gcurpos,npos,pos1,pos2,onscr,chg2)
Gfloat gcurpos[3],npos[3],pos1[3],pos2[3];
int *onscr, *chg2;
{
	UG_segstli *sp;
	uu_denter(UU_GITRC,(us,"ug_clipexp(..)"));
	ug_clip(gcurpos,npos,pos1,pos2,onscr,chg2);
	if ((ug_viwseg>=0)&&(ug_ndcseg>0)) {
		sp=ug_segac(ug_viwseg);
		if (*onscr) {
/*			ug_ndcboxadd(sp,pos1,pos2);	/* Expand ndc box */
		}
		else  {
			/* One seg in this normtran */
			ug_ntranbox |= (1<<(ug_gksstli.curvwindex));
		}
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_clipexpln(gcurpos,npos,pos1,pos2,onscr,chg2,offset)
**			Just like ug_clipexp, but calls ug_ndcboxaddln
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_clipexpln(gcurpos,npos,pos1,pos2,onscr,chg2, offset)
Gfloat gcurpos[3],npos[3],pos1[3],pos2[3];
int *onscr, *chg2;
Gfloat offset;
{
	UG_segstli *sp;
	uu_denter(UU_GITRC,(us,"ug_clipexp(..)"));
	ug_clip(gcurpos,npos,pos1,pos2,onscr,chg2);
	if ((ug_viwseg>=0)&&(ug_ndcseg>0)) {
		sp=ug_segac(ug_viwseg);
		if (*onscr) {
/*			ug_ndcboxaddln(sp,pos1,pos2,offset);	/* Expand ndc box */
		}
		else  {
			/* One seg in this normtran */
			ug_ntranbox |= (1<<(ug_gksstli.curvwindex));
		}
	}
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_cliprect(gcurpos,npos,pos1,pos2,onscr,chg2,rect,
**								clipflag)
**       Clip the line from gcurpos to npos if clipflag==UG_CLIP. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_cliprect(gcurpos,npos,pos1,pos2,onscr,chg2,rect,clipflag)
/* clip line from gcurpos to npos. return 2 (possibly different)
   points in pos1, pos2. onscr=1 if anything is within rect. chg2=1
   if npos!=pos2
  gcurpos and npos in same coord system as rect.
*/
Gfloat gcurpos[3],npos[3],pos1[3],pos2[3];
int *onscr, *chg2;
Gnrect3 *rect;								/* rectangle to clip to */
Gclip clipflag;
{ 
	int i,j,ic1,ic2,ibit,ibit2;
  Gfloat a;
  Gfloat vpeps;				/* viewport fuzz amount to insure line ends are 
										within viewport */
  static Gfloat eps=0.00001;

  uu_denter(UU_GITRC,(us,"ug_cliprect( rect=%g %g %g %g %g %g,%d)",
	(*rect).llf.x,(*rect).llf.y,(*rect).llf.z,
	(*rect).urb.x,(*rect).urb.y,(*rect).urb.z,(int)clipflag));
  for (i=0; i<3; i++) {
    pos2[i]=npos[i];
	 pos1[i]=gcurpos[i];
  }
	if (clipflag==UG_NOCLIP) {		/* if we don't have to clip, return now */
		*onscr=1; *chg2=0;
		goto rtn;
	}
  /* make a temporary copy of rect in an array for loops */
	vwport[0][0]=(*rect).llf.x;
	vwport[0][1]=(*rect).urb.x;
	vwport[1][0]=(*rect).llf.y;
	vwport[1][1]=(*rect).urb.y;

	/* copy the front and back z values backwards, since front z is
		larger than back z in a right handed system. The code that
		follows requires vport[*][1] to be the larger value. */
	vwport[2][1]=(*rect).llf.z;
	vwport[2][0]=(*rect).urb.z;
	/* make viewport 1/1000th smaller to insure we push endpoints onto it */
	/* dont shrink in z so we wont clip stuff on front or back face of vwport*/
/*
.....Don't shrink viewport when plotting
.....otherwise borders will not be displayed
.....Yurong  -  8/7/97
*/
	if (UJ_plotting != UU_TRUE)
	{
/*
.....if clip within viewport, don't shrink because may not display border
.....Yurong 3/23/99
*/
		if (ug_clip2flag==1) 
		{    
			for (i=0; i<2; i++) {
				vpeps=(vwport[i][1]-vwport[i][0])/2000.;
				vwport[i][0]=vwport[i][0]+vpeps;
				vwport[i][1]=vwport[i][1]-vpeps;
			} 
		}
	}
  /* calculate clip flags ic1 and ic2. */
  ic1=0; ic2=0; ibit=1; ibit2=2;
  for (i=0; i<3; i++) {
    if (pos1[i]<vwport[i][0]) ic1=ic1|ibit;
    if (pos1[i]>vwport[i][1]) ic1=ic1|ibit2;
    if (pos2[i]<vwport[i][0]) ic2=ic2|ibit;
    if (pos2[i]>vwport[i][1]) ic2=ic2|ibit2;
    ibit=ibit*4;  ibit2=ibit2*4;
  }
  /* see if within screen */
  if ((ic1+ic2)==0) {       /* all on screen. calc chg2 */
    if ((npos[0]!=pos2[0])||(npos[1]!=pos2[1]))
                     (*chg2)=1;
    else (*chg2)=0;
    (*onscr)=1;
    goto rtn;
  }
  /* see if all off screen */
  if ((ic1&ic2)!=0) {       /* all off screen */
    (*onscr)=0; (*chg2)=1;
    goto rtn;
	}
	/* line is part on and part off, or all off */
	/* see if pos1 is off */
	if (ic1!=0) {             /* push pos1 to screen edge */
    for (i=0; i<3; i++) {
      /* check min value on this axis */
      if (pos1[i]<vwport[i][0]) {
        /* push pos1 toward higher value on this axis */
        if (fabs(pos2[i]-pos1[i])>eps) {
          a=(vwport[i][0]-pos1[i])/(pos2[i]-pos1[i]); 
          for (j=0; j<3; j++) pos1[j]=a*(pos2[j]-pos1[j])+pos1[j];
        }
      }
      else {
        /* check max value on this axis */
        if (pos1[i]>=vwport[i][1]) {
          /* push pos1 toward lower value on this axis */
          if (fabs(pos2[i]-pos1[i])>eps) {
            a=(vwport[i][1]-pos1[i])/(pos2[i]-pos1[i]);
            for (j=0; j<3; j++) pos1[j]=a*(pos2[j]-pos1[j])+pos1[j];
          }
        }
      }
    }                        /* for i */
  }                          /* if ic1!=0 */
  /* see if pos2 is on screen */
  if (ic2==0) {              /* pos2 on screen. calc chg2 */
    (*chg2)=0;
    if ((npos[0]!=pos2[0])||(npos[1]!=pos2[1]))
                            (*chg2)=1;
    (*onscr)=1;
    goto rtn;
  }
  /* push pos2 to screen edge. */
  for (i=0; i<3; i++) {
    /* check min value on this axis */
    if (pos2[i]<vwport[i][0]) {
      /* push pos2 toward higher value on this axis */
      if (fabs(pos2[i]-pos1[i])>eps) {
        a=(vwport[i][0]-pos1[i])/(pos2[i]-pos1[i]);
        for (j=0; j<3; j++) pos2[j]=a*(pos2[j]-pos1[j])+pos1[j];
      }
    }
    else {
      /* check max value on this axis */
      if (pos2[i]>vwport[i][1]) {
        /* push pos2 toward lower value on this axis */
        if (fabs(pos2[i]-pos1[i])>eps) {
          a=(vwport[i][1]-pos1[i])/(pos2[i]-pos1[i]);
          for (j=0; j<3; j++) pos2[j]=a*(pos2[j]-pos1[j])+pos1[j];
        }
      }
    }
	}                            /* for i */
	/* pos2 now  on screen. calc chg2 */
	if ((npos[0]!=pos2[0])||(npos[1]!=pos2[1]))
                       (*chg2)=1;
	else (*chg2)=0;
	/* see if line is all off screen */
	/* first  expand view volume to make sure don't clip stuff on a face */
	for (i=0; i<3; i++) {
		vpeps=(vwport[i][1]-vwport[i][0])/2000.;
		vwport[i][0]=vwport[i][0]-vpeps;
		vwport[i][1]=vwport[i][1]+vpeps;
	}
	/* see if both points are within expanded view volume */
	for (i=0; i<3; i++) {
		if ((pos1[i]<vwport[i][0])||(pos1[i]>vwport[i][1])||
				(pos2[i]<vwport[i][0])||(pos2[i]>vwport[i][1])) {
			/* one of the points is outside view volume. dont draw it */
			(*onscr)=0; goto rtn;
		}
	}
  (*onscr)=1;
  rtn:
	uu_dprint(UU_GITRC,(us,
	"ug_cliprect((%3g %3g %3g, %3g %3g %3g),(%3g %3g %3g, %3g %3g %3g),%d,%d)",
      gcurpos[0],gcurpos[1],gcurpos[2],npos[0],npos[1],npos[2],
      pos1[0],pos1[1],pos1[2],pos2[0],pos2[1],pos2[2],
      *onscr,*chg2));
	uu_dexit;
	return;
}

/*********************************************************************
**    I_FUNCTION     :  ug_lineinrect2(gcurpos,npos,rect)
**			see if line from gcurpos to npos goes within rect. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : 1 if line goes in rect, else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_lineinrect2(gcurpos,npos,rect)
/* return 1 if line from gcurpos to npos goes within rect. Else return 0 */
Gfloat gcurpos[2],npos[2];
Gnrect *rect;								/* rectangle to clip to */
{ 
	int irtn;						/* return value */
	Gfloat pos1[2],pos2[2];
	int i,j,ic1,ic2,ibit,ibit2;
  Gfloat a;
  Gfloat vwport[2][2];				/* local copy */
  Gfloat vpeps;
  static Gfloat eps=0.00001;

  uu_denter(UU_GITRC,(us,"ug_lineinrect2(%g %g, %g %g rect=%g %g %g %g)",
	gcurpos[0],gcurpos[1],npos[0],npos[1],
	(*rect).ll.x,(*rect).ll.y, (*rect).ur.x,(*rect).ur.y));
  for (i=0; i<2; i++) {
    pos2[i]=npos[i];
	 pos1[i]=gcurpos[i];
  }
  /* make a local copy of clip rectangle */
  vwport[0][0]=(*rect).ll.x;
  vwport[0][1]=(*rect).ur.x;
  vwport[1][0]=(*rect).ll.y;
  vwport[1][1]=(*rect).ur.y;
	/* make viewport 1/1000th smaller to insure we push endpoints onto it */
	for (i=0; i<2; i++) {
		vpeps=(vwport[i][1]-vwport[i][0])/2000.;
		vwport[i][0]=vwport[i][0]+vpeps;
		vwport[i][1]=vwport[i][1]-vpeps;
	}
  /* calculate clip flags ic1 and ic2. */
  ic1=0; ic2=0; ibit=1; ibit2=2;
  for (i=0; i<2; i++) {
    if (pos1[i]<vwport[i][0]) ic1=ic1|ibit;
    if (pos1[i]>vwport[i][1]) ic1=ic1|ibit2;
    if (pos2[i]<vwport[i][0]) ic2=ic2|ibit;
    if (pos2[i]>vwport[i][1]) ic2=ic2|ibit2;
    ibit=ibit*4;  ibit2=ibit2*4;
  }
  /* see if within screen */
  if ((ic1+ic2)==0) {       /* all on screen */
    irtn=1;
    goto rtn;
  }
  /* see if all off rect */
  if ((ic1&ic2)!=0) {       /* all off rect */
    irtn=0;
    goto rtn;
	}
	uu_dprint(UU_GITRC,(us,"lineinrect2.  ic1=%d ic2=%d"));
	/* line is part on and part off, or all off */
	/* see if pos1 is off */
	if (ic1!=0) {             /* push pos1 to rect edge */
		uu_dprint(UU_GITRC,(us,"push pos1 to edge"));
    for (i=0; i<2; i++) {
      /* check min value on this axis */
      if (pos1[i]<vwport[i][0]) {
        /* push pos1 toward higher value on this axis */
        if (fabs(pos2[i]-pos1[i])>eps) {
          a=(vwport[i][0]-pos1[i])/(pos2[i]-pos1[i]); 
          for (j=0; j<2; j++) pos1[j]=a*(pos2[j]-pos1[j])+pos1[j];
        }
      }
      else {
        /* check max value on this axis */
        if (pos1[i]>=vwport[i][1]) {
          /* push pos1 toward lower value on this axis */
          if (fabs(pos2[i]-pos1[i])>eps) {
            a=(vwport[i][1]-pos1[i])/(pos2[i]-pos1[i]);
            for (j=0; j<2; j++) pos1[j]=a*(pos2[j]-pos1[j])+pos1[j];
          }
        }
      }
    }                        /* for i */
  }                          /* if ic1!=0 */
  /* see if pos2 is on screen */
  if (ic2==0) {              /* pos2 in rect */
    irtn=1;
    goto rtn;
  }
  /* push pos2 to screen edge. */
  uu_dprint(UU_GITRC,(us,"push pos2 to edge"));
  for (i=0; i<2; i++) {
    /* check min value on this axis */
    if (pos2[i]<vwport[i][0]) {
      /* push pos2 toward higher value on this axis */
      if (fabs(pos2[i]-pos1[i])>eps) {
        a=(vwport[i][0]-pos1[i])/(pos2[i]-pos1[i]);
        for (j=0; j<2; j++) pos2[j]=a*(pos2[j]-pos1[j])+pos1[j];
      }
    }
    else {
      /* check max value on this axis */
      if (pos2[i]>vwport[i][1]) {
        /* push pos2 toward lower value on this axis */
        if (fabs(pos2[i]-pos1[i])>eps) {
          a=(vwport[i][1]-pos1[i])/(pos2[i]-pos1[i]);
          for (j=0; j<2; j++) pos2[j]=a*(pos2[j]-pos1[j])+pos1[j];
        }
      }
    }
	}                            /* for i */
	/* pos2 now  in rect. */
	/* see if line is all off screen */
	/* first  expand view volume to make sure don't clip stuff on a face */
	for (i=0; i<2; i++) {
		vpeps=(vwport[i][1]-vwport[i][0])/2000.;
		vwport[i][0]=vwport[i][0]-vpeps;
		vwport[i][1]=vwport[i][1]+vpeps;
	}
	uu_dprint(UU_GITRC,(us,"lineinrect2 pos1=%g %g pos2=%g %g",
		pos1[0],pos1[1],pos2[0],pos2[1]));
	uu_dprint(UU_GITRC,(us,"lineinrect2 vport=%g %g  %g %g",
		vwport[0][0],vwport[0][1],vwport[1][0],vwport[1][1]));
	/* see if both points are within expanded view volume */
	for (i=0; i<2; i++) {
		if ((pos1[i]<vwport[i][0])||(pos1[i]>vwport[i][1])||
				(pos2[i]<vwport[i][0])||(pos2[i]>vwport[i][1])) {
			/* one of the points is outside view volume. */
			irtn=0; goto rtn;
		}
	}
  irtn=1;
  rtn:
	uu_dprint(UU_GITRC,(us,"%d=ug_lineinrect2(..)",irtn));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  ug_icliprect(gcurpos,npos,pos1,pos2,onscr,chg2,rect)
**       Clip the line from gcurpos to npos. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_icliprect(gcurpos,npos,pos1,pos2,onscr,chg2,rect)
/* clip line from gcurpos to npos. return 2 (possibly different)
   points in pos1, pos2. onscr=1 if anything is within rect. chg2=1
   if npos!=pos2
  gcurpos and npos in same coord system as rect.
*/
Gint gcurpos[2],npos[2],pos1[2],pos2[2];
int *onscr, *chg2;
Gint rect[2][2];								/* rectangle to clip to */
{ int i,j,ic1,ic2,ibit,ibit2;
  Gfloat a;
  Gfloat vpeps;				/* viewport fuzz amount to insure line ends are 
										within viewport */
  char us[160];
  static Gfloat eps=0.00001;
  for (i=0; i<3; i++) {
    pos2[i]=npos[i];
	 pos1[i]=gcurpos[i];
  }
  
	/* calculate clip flags ic1 and ic2. */
  ic1=0; ic2=0; ibit=1; ibit2=2;
  for (i=0; i<2; i++) {
    if (pos1[i]<rect[0][i]) ic1=ic1|ibit;
    if (pos1[i]>rect[1][i]) ic1=ic1|ibit2;
    if (pos2[i]<rect[0][i]) ic2=ic2|ibit;
    if (pos2[i]>rect[1][i]) ic2=ic2|ibit2;
    ibit=ibit*4;  ibit2=ibit2*4;
  }
  /* see if within screen */
  if ((ic1+ic2)==0) {       /* all on screen. calc chg2 */
    if ((npos[0]!=pos2[0])||(npos[1]!=pos2[1]))
                     (*chg2)=1;
    else (*chg2)=0;
    (*onscr)=1;
    goto rtn;
  }
  /* see if all off screen */
  if ((ic1&ic2)!=0) {       /* all off screen */
    (*onscr)=0; (*chg2)=1;
    goto rtn;
	}
	/* line is part on and part off, or all off */
	/* see if pos1 is off */
	if (ic1!=0) {             /* push pos1 to screen edge */
    for (i=0; i<2; i++) {
      /* check min value on this axis */
      if (pos1[i]<rect[0][i]) {
        /* push pos1 toward higher value on this axis */
        if (pos2[i]!=pos1[i]) {
          a=(Gfloat)(rect[0][i]-pos1[i])/(Gfloat)(pos2[i]-pos1[i]); 
          for (j=0; j<2; j++) pos1[j]=a*(pos2[j]-pos1[j])+pos1[j];
        }
      }
      else {
        /* check max value on this axis */
        if (pos1[i]>=rect[1][i]) {
          /* push pos1 toward lower value on this axis */
          if (pos2[i]!=pos1[i]) {
            a=(Gfloat)(rect[1][i]-pos1[i])/(Gfloat)(pos2[i]-pos1[i]);
            for (j=0; j<2; j++) pos1[j]=a*(pos2[j]-pos1[j])+pos1[j];
          }
        }
      }
    }                        /* for i */
  }                          /* if ic1!=0 */
  /* see if pos2 is on screen */
  if (ic2==0) {              /* pos2 on screen. calc chg2 */
    (*chg2)=0;
    if ((npos[0]!=pos2[0])||(npos[1]!=pos2[1]))
                            (*chg2)=1;
    (*onscr)=1;
    goto rtn;
  }
  /* push pos2 to screen edge. */
  for (i=0; i<2; i++) {
    /* check min value on this axis */
    if (pos2[i]<rect[0][i]) {
      /* push pos2 toward higher value on this axis */
      if (pos2[i]!=pos1[i]) {
        a=(Gfloat)(rect[0][i]-pos1[i])/(Gfloat)(pos2[i]-pos1[i]);
        for (j=0; j<2; j++) pos2[j]=a*(pos2[j]-pos1[j])+pos1[j];
      }
    }
    else {
      /* check max value on this axis */
      if (pos2[i]>rect[1][i]) {
        /* push pos2 toward lower value on this axis */
        if (pos2[i]!=pos1[i]) {
          a=(Gfloat)(rect[1][i]-pos1[i])/(Gfloat)(pos2[i]-pos1[i]);
          for (j=0; j<2; j++) pos2[j]=a*(pos2[j]-pos1[j])+pos1[j];
        }
      }
    }
	}                            /* for i */
	/* pos2 now  on screen. calc chg2 */
	if ((npos[0]!=pos2[0])||(npos[1]!=pos2[1]))
                       (*chg2)=1;
	else (*chg2)=0;
	/* see if line is all off screen */
	/* see if both points are within view volume */
	for (i=0; i<2; i++) {
		if ((pos1[i]<rect[0][i])||(pos1[i]>rect[1][i])||
				(pos2[i]<rect[0][i])||(pos2[i]>rect[1][i])) {
			/* one of the points is outside view volume. dont draw it */
			(*onscr)=0; goto rtn;
		}
	}
  (*onscr)=1;
  rtn:
	uu_denter2(UU_GITRC,(us,
	"ug_icliprect((%d %d, %d %d),(%d %d, %d %d),%d,%d)",
      gcurpos[0],gcurpos[1],gcurpos[2],npos[0],npos[1],npos[2],
      pos1[0],pos1[1],pos1[2],pos2[0],pos2[1],pos2[2],
      *onscr,*chg2));
	uu_dexit;
	return;
}

/*********************************************************************
**    I_FUNCTION   :  Logical ug_clpnt(pos) -- TRUE iff pos is within viewport
**							if ug_clip2flag==1. Else TRUE iff pos within
**							current ug_clip2rect.
**    PARAMETERS   
**       INPUT  :  Gfloat pos[3] -- point to be clipped.
**       OUTPUT :  
**    RETURNS      : UG_TRUE if pos is within viewport.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Logical ug_clpnt(pos)          /* UG_TRUE iff pos is within viewport */
Gfloat pos[3];
{
	Logical ug_clpntrect();
	if (ug_clip2flag!=1)
		return(ug_clpntrect(pos,&ug_gksstli.vtran[ug_gksstli.curvwindex].vport));
	else {
		if (ug_clip2null[ug_gksstli.curvwindex]==1) return(UG_FALSE);	/*null*/
		return(ug_clpntrect(pos,&ug_clip2rect[ug_gksstli.curvwindex]));
	}
}

/*********************************************************************
**    I_FUNCTION   :  Logical ug_clpntexp(pos) -- like ug_clpnt but expands box.
**							TRUE iff pos is within viewport
**							if ug_clip2flag==1. Else TRUE iff pos within
**							current ug_clip2rect.
**							If a segment open and ndcseg>0, expand
**							this seg's ndcbox.
**    PARAMETERS   
**       INPUT  :  Gfloat pos[3] -- point to be clipped.
**       OUTPUT :  
**    RETURNS      : UG_TRUE if pos is within viewport.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Logical ug_clpntexp(npos)          /* UG_TRUE iff pos is within viewport */
Gnpoint3 *npos;
{
/* ndc half height of a marker */
	UG_segstli *segptr;
	Logical irtn;

	uu_denter(UU_GITRC,(us,"ug_clpntexp(%g %g %g)",
		(*npos).x,(*npos).y,(*npos).z));

	irtn=ug_clpnt(npos);
	if((irtn==UG_TRUE) &&
		((ug_viwseg>=0)&&(ug_ndcseg>0))) {
		segptr=ug_segac(ug_viwseg);
/*		(*segptr).ndcbox.ll.x=
/*			((*npos).x-UG_MKRSIZ)<(*segptr).ndcbox.ll.x ?
/*			((*npos).x-UG_MKRSIZ) : (*segptr).ndcbox.ll.x;
/*		(*segptr).ndcbox.ll.y=
/*			((*npos).y-UG_MKRSIZ)<(*segptr).ndcbox.ll.y ? 
/*			((*npos).y-UG_MKRSIZ) : (*segptr).ndcbox.ll.y;
/*		(*segptr).ndcbox.ur.x=
/*			((*npos).x+UG_MKRSIZ)>=(*segptr).ndcbox.ur.x ? 
/*			((*npos).x+UG_MKRSIZ):(*segptr).ndcbox.ur.x;
/*		(*segptr).ndcbox.ur.y=
/*			((*npos).y+UG_MKRSIZ)>=(*segptr).ndcbox.ur.y ? 
/*			((*npos).y+UG_MKRSIZ):(*segptr).ndcbox.ur.y;
/**/
		/* Current normtran now has at least one ndc box */
		ug_ntranbox |= (1<<(ug_gksstli.curvwindex));
		uu_dprint(UU_GITRC,(us,"ug_ntranbox %x", ug_ntranbox));
	}

	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION   :  Logical ug_clpntrect(pos,rect) -- TRUE iff pos within rect.
**    PARAMETERS   
**       INPUT  :  Gfloat pos[3] -- point to be clipped.
**						 Gnrect *rect -- 3D (right handed) rectangle to clip to.
**									(llf.z > urb.z)
**       OUTPUT :  
**    RETURNS      : UG_TRUE if pos is within rect.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Logical ug_clpntrect(pos,rect)   /* UG_TRUE iff pos is within rect */
Gfloat pos[3];							/* point to be clipped */
Gnrect3 *rect;							/* rect to clip to */
{ 
	if (pos[0]<(*rect).llf.x) return(UG_FALSE);
	if (pos[0]>(*rect).urb.x) return(UG_FALSE);
	if (pos[1]<(*rect).llf.y) return(UG_FALSE);
	if (pos[1]>(*rect).urb.y) return(UG_FALSE);
	if (pos[2]>(*rect).llf.z) return(UG_FALSE);
	if (pos[2]<(*rect).urb.z) return(UG_FALSE);
	
	return(UG_TRUE);
}

/*********************************************************************
**    I_FUNCTION   : Logical ug_iclpntrect(pos,rect)--TRUE iff pos within rect.
**    PARAMETERS   
**       INPUT  :  Gint pos[2] -- point to be clipped.
**						 Gint rect[2][2] -- 2D rectangle to clip to.
**       OUTPUT :  
**    RETURNS      : UG_TRUE if pos is within rect.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Logical ug_iclpntrect(pos,rect)   /* UG_TRUE iff pos is within rect */
Gint pos[2];							/* point to be clipped */
Gint rect[2][2];						/* rect to clip to */
{ 
	if (pos[0]<rect[0][0]) return(UG_FALSE);
	if (pos[0]>rect[1][0]) return(UG_FALSE);
	if (pos[1]<rect[0][1]) return(UG_FALSE);
	if (pos[1]>rect[1][1]) return(UG_FALSE);
	
	return(UG_TRUE);
}

/*********************************************************************
**    I_FUNCTION :  ug_facliprect(n,points,norms,nclip,clippoints,
**											clipnorms,rect)
**				Fill area clip.  Use Sutherland-Hodgman reentrant clip algorithm.
**    PARAMETERS   
**       INPUT  : 	int n; -- number of vertices in fill area.
**							Gnpoint3 points[]; -- vertices to clip.
**							Gnpoint3 norms[]; -- normals at each vertex.
**							Gnrect3 *rect; -- clipping volume.
**							int zclipflag; -- 1=z-clip, 0=don't.
**       OUTPUT :  	int *nclip; -- number of clipped vertices.
**							Gnpoint3 clippoints[]; -- clipped vertices.
**							Gnpoint3 clipnorms[]; -- normals at clipped vertices.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_facliprect(n,points,norms,nclip,clippoints,clipnorms,rect,zclipflag)
/*$ INPUT */
int n; 						/* number of vertices in fill area. */
Gnpoint3 points[]; 		/* vertices to clip. */
Gnpoint3 norms[]; 		/* normals at each vertex. */
Gnrect3 *rect; 			/* clipping volume.*/
int zclipflag;				/* 1=clip in z axis 0=don't */
/*$ OUTPUT */
int *nclip; 				/* number of clipped vertices. */
Gnpoint3 clippoints[]; 	/* clipped vertices. */
Gnpoint3 clipnorms[]; 	/* normals at clipped vertices. */
{
	/* Fill area clip.  Use Sutherland-Hodgman reentrant clip algorithm.
		See Rogers, David, "Procedureal Elements for Computer Graphics", 
		Pp. 169-179. */
	Gnpoint3 clippts2[40];	/* temp copy of clipped vertices */
	Gnpoint3 clipnms2[40];	/* temp copy of clipped normals */
	Gnpoint3 *clipp;			/* pointer to clipts2 */
	Gnpoint3 *clipn;			/* pointer to clipnms2 */
	int nclipp;					/* size of clipp */
	int nclipflag;				/* 1=clip normals, 0=don't */

	uu_denter(UU_GITRC,(us,"ug_facliprect(%d,%g %g %g, %g %g %g, %g %g %g,%d)",
		n,points[0].x,points[0].y,points[0].z,rect->llf.x,rect->llf.y,
		rect->llf.z,rect->urb.x,rect->urb.y,rect->urb.z,zclipflag));

	/* Set up local buffers, if we're not clipping normals (clipnorms, norms
	 * are NULL) then the local buffer clipn is NULL.
	 */
	if( clipnorms == NULL ) 
		nclipflag = 0;
	else 
		nclipflag = 1;

	if (n>20) {
		clipp=(Gnpoint3 *)uu_toolmalloc(2*n*sizeof(Gnpoint3));
		if( nclipflag )
			clipn=(Gnpoint3 *)uu_toolmalloc(2*n*sizeof(Gnpoint3));
		else
			clipn=NULL;
	}
	else {
		clipp=clippts2;
		if( nclipflag )
			clipn=clipnms2;
		else
			clipn=NULL;
	}

	/* clip to min x-axis */
	ug_faclipplane(n,points,norms,&nclipp,clipp,
						clipn,rect->llf.x,0,0,nclipflag);
	/* clip to max x-axis */
	ug_faclipplane(nclipp,clipp,clipn,nclip,clippoints,
						clipnorms,rect->urb.x,1,0,nclipflag); 
	/* clip to min y-axis */
	ug_faclipplane(*nclip,clippoints,clipnorms,&nclipp,
						clipp,clipn,rect->llf.y,0,1,nclipflag);
	/* clip to max y-axis */
	ug_faclipplane(nclipp,clipp,clipn,nclip,clippoints,
						clipnorms,rect->urb.y,1,1,nclipflag);
	if (zclipflag) {
		/* clip to min z-axis */
		ug_faclipplane(*nclip,clippoints,clipnorms,&nclipp,
							clipp,clipn,rect->urb.z,0,2,nclipflag);
		/* clip to max z-axis */
		ug_faclipplane(nclipp,clipp,clipn,nclip,
							clippoints,clipnorms,rect->llf.z,1,2,nclipflag);
	}
	if (n>20) {
		uu_toolfree(clipp);
		if( nclipflag )
			uu_toolfree(clipn);
	}
	uu_dexit;
}

#define PTOK(pt,v,mm,ax) (mm==0) ? (pt[ax]>=v) : (pt[ax]<=v)
#define PTCOPY(out,in) out[0]=in[0]; out[1]=in[1]; out[2]=in[2]
/*********************************************************************
**    I_FUNCTION :  ug_faclipplane(n,points,norms,nclip,clippoints,
**												clipnorms,val,minmax,axis)
**								Clip a fill area to a plane.  Part of 
**								Sutherland-Hodgman clipping algorithm.
**    PARAMETERS   
**       INPUT  : int n; -- number of vertices in points 
**						Gnpoint3 points[]; -- unclipped fill area vertices
**							the next 3 arguments define the plane, 
**								which is parallel to two axes 
**						Gfloat val;	--  value of one axis defining the plane 
**						int minmax;	-- 0 if clip below val, 1 if clip above val 
**						int axis;	-- 0: clip plane is x=val, 1: y=val, 2: z=val.
**       OUTPUT : int *nclip;	--  number of vertices in clippoints 
**						Gnpoint3 clippoints[];	--  clipped vertices 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : Assumes Gfloat points[][3] is same as Gnpoint3 points[];
*********************************************************************/
ug_faclipplane(n,points,norms,nclip,clippoints,clipnorms,
												val,minmax,axis,nclipflag)
int n;						/* number of vertices in points */
Gfloat points[][3];		/* unclipped fill area vertices */
Gfloat norms[][3];		/* normals at points, or NULL */
int *nclip;					/* on return, number of vertices in clippoints */
Gfloat clippoints[][3];	/* on return, clipped vertices */
Gfloat clipnorms[][3];	/* If != NULL clipped vertices */

/* The next 3 arguments define the plane, which is parallel to two axes */
Gfloat val;					/*  value of one axis defining the plane */
int minmax;					/* 0 if clip below val, 1 if clip above val */
int axis;					/* 0: plane is x=val, 1: y=val, 2: z=val */
int nclipflag;				/* 1=clip normals, 0=don't */
{	
	int i;
	Gfloat pt[3];			/* 1st point */
	Gfloat norm[3];		/* 1st normal */
	Gfloat a;
	int startvis,vis0;

	uu_denter(UU_GITRC,(us,"ug_faclipplane(%d,%g %g %g,%g %d %d)",
		n,points[0][0],points[0][1],points[0][2],val,minmax,axis));
	*nclip=0;
	if (n>0) {
		/* do first point/normal as a special case */
		if (PTOK(points[0],val,minmax,axis)) {		/* 1st point is visible */
			PTCOPY(clippoints[0],points[0]);
			if( nclipflag ) {
				PTCOPY(clipnorms[0],norms[0]);
			}
			*nclip=1; startvis=1;
		}
		else {												/* 1st point invisible */
			*nclip=0; startvis=0;
		}
		vis0=startvis;			/* remember if 1st point is visible */
		if( nclipflag ) {		/* clip the normals */
			for (i=1; i<n; i++) {
				/* clip the edge from points[i-1] to points[i] */
				ug_lnclipplane(points[i-1],points[i],norms[i-1],norms[i],
					&startvis,nclip,clippoints,clipnorms,val,minmax,axis);
			}								/* end for i=1; i<n;  */
		}
		else {		/* no normals to clip */
			for (i=1; i<n; i++) {
				/* clip the edge from points[i-1] to points[i] */
				ug_lnclipplane(points[i-1],points[i],NULL,NULL,
					&startvis,nclip,clippoints,NULL,val,minmax,axis);
			}								/* end for i=1; i<n;  */
		}

		/* now handle the closing edge */
		if( nclipflag ) {
			ug_lnclipplane(points[n-1],points[0],norms[n-1],norms[0],
				&startvis,nclip,clippoints,clipnorms,val,minmax,axis);
		}
		else {
			ug_lnclipplane(points[n-1],points[0],NULL,NULL,
				&startvis,nclip,clippoints,NULL,val,minmax,axis);
		}

		/* if 1st and last point are visible, they are same point.
			Remove 2nd copy */
		if ((vis0==1)&&(startvis==1)) (*nclip)--;

	}									/* end if n>0 */

	uu_dprint(UU_GITRC,(us,"ug_faclipplane returns. *nclip=%d, %g %g %g",
		*nclip,clippoints[0][0],clippoints[0][1],clippoints[0][2]));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_lnclipplane(s,e,snorm,enorm,startvis,nclip,
**							clippoints,clipnorms,val,
**								minmax,axis) -- clip one fa edge to a plane.
**								Part of Sutherland -Hodgman polygon clipping.
**    PARAMETERS   
**       INPUT  : 	Gfloat s[3]; -- start point of line.
**							Gfloat e[3]; -- end point of line.
**							Gfloat snorm[3] -- normal at s.
**							Gfloat enorm[3] -- normal at e.
**							int *startvis; -- 0=s not visible. 1=s is visible.
**							int *nclip; -- length of clippoints.
**							Gfloat val; -- value of plane.
**							int minmax; 0=clip above plane. 1=clip below.
**							int axis; 0=clip plane is x=val. 1: y=val. 2: z=val.
**       OUTPUT :  	Gfloat clippoints[][3]; -- array of clipped vertices.
**       				Gfloat clipnorms[][3]; -- array of normals at clippoints.
**							int *nclip; -- length of clippoints.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_lnclipplane(s,e,snorm,enorm,startvis,nclip,clippoints,
				clipnorms,val,minmax,axis)
Gfloat s[3],e[3];				/* start end points of line */
Gfloat snorm[3],enorm[3];			/* normals at s,e */
int *startvis;					/* 1=s is visible, 0=not.
										on return, 1=e is visible */
int *nclip;						/* length of clippoints */
Gfloat clippoints[][3];		/* clipped vertices */
Gfloat clipnorms[][3];		/* normals at clipped vertices */
Gfloat val;
int minmax;
int axis;
{
	Gfloat a;
	int j;

	uu_denter(UU_GITRC,(us,"ug_lnclipplane(%g %g %g, %g %g %g,%d,%d, %g %d %d)",
		s[0],s[1],s[2],e[0],e[1],e[2],*startvis,*nclip,val,minmax,axis));

	if (clipnorms!=NULL)  {
	uu_dprint(UU_GITRC,(us,"ug_lnclipplane.snorm=%g %g %g, enorm=%g %g %g",
		snorm[0],snorm[1],snorm[2],enorm[0],enorm[1],enorm[2]));
	}
	if (PTOK(e,val,minmax,axis)) {	/* endpoint is visible */
		if (*startvis) {		/* edge entirely visible, output end point */
			PTCOPY(clippoints[*nclip],e);
			if (clipnorms!=NULL) {
				PTCOPY(clipnorms[*nclip],enorm);
			}
			(*nclip)++;
		}
		else {		/* entering visible, output intersect and end pt. */
			/* calc a = fraction of line that is visible */
			a= (e[axis]-val)/(e[axis]-s[axis]);
			for (j=0; j<3; j++) {
				clippoints[*nclip][j]=e[j] + a*(s[j]-e[j]);
				if (clipnorms!=NULL) 
					clipnorms[*nclip][j]=enorm[j] + a*(snorm[j]-enorm[j]);
			}
			(*nclip)++;
			PTCOPY(clippoints[*nclip],e);
			if (clipnorms!=NULL) {
				PTCOPY(clipnorms[*nclip],enorm);
			}
			(*nclip)++;
		}
		*startvis=1;			/* remember this point is visible */
	}
	else {								/* endpoint is invisible */
		if (*startvis) {				/* leaving visible, output intersect */
			/* calc a = fraction of line that is visible */
			a= (e[axis]-val)/(e[axis]-s[axis]);
			for (j=0; j<3; j++) {
				clippoints[*nclip][j]=e[j] + a*(s[j]-e[j]);
				if (clipnorms!=NULL)
					clipnorms[*nclip][j]=enorm[j] + a*(snorm[j]-enorm[j]);
			}
			(*nclip)++;
		}
		*startvis=0;			/* remember end point is invisible */
	}
#ifdef UU_DEBUG
	if (*nclip>0) {
		uu_dprint(UU_GITRC,(us,
		"ug_lnclipplane returns. *startvis=%d clipponts[%d]=%g %g %g", 
		*startvis,*nclip-1,clippoints[*nclip-1][0],clippoints[*nclip-1][1],
		clippoints[*nclip-1][2]));
	}
	else {
		uu_dprint(UU_GITRC,(us,"ug_lnclipplane returns. *startvis=%d *nclip=%d",
			*startvis,*nclip));
	}
#endif
	uu_dexit;
}
