/*********************************************************************
**    NAME         :  gpickras.c -- ordinary (not area) picking routines
**									Like gpick.c but for raster segments.
**       CONTAINS:
**    ug_findsgpkras(n,ats)  
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gpickras.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:23
*********************************************************************/
#include <math.h>
#include "zsysdep.h"
#include "udebug.h"
#include "gtbl.h"
#include "gsegac.h"
#include "gviw.h"
#include "gdidd.h"
#include "gsegop.h"
#include "gconvert.h"
#define false 0
#define true 1
#define UG_FALSE 0
#define UG_TRUE 1
/* extern char ug_ops[74][16]; */
/*********************************************************************
**    I_FUNCTION     :  int ug_findsgpkras(segptr,findprms,vlst,vlen,loc,epsx,
**							epsy, mode,pkid,newdist) -- Find seg pointed to by segptr. 
**    PARAMETERS   
**							We need all these parameters to make ug_findsgpkras
**							recursively callable for heirarchial segments.
**       INPUT: 
**						UG_segstli *segptr -- segment to find.
**						UG_findprms *findprms;
**						typedef struct {
**							UG_prat3 prats; 		/* primitive atts (only text used). 
**							int curxform;			/*current viewing xform.
**							int pkid; 				/*current pick id.
**							Gtran gmodxf;			/* global modelling xform.
**						} UG_findprms;
**						Gipoint *loc; -- center of find location.
**						Gint epsx,epsy; -- size of find aperture, centered at loc.
**						int (*func()); -- call func when find something.
**						int mode; -- pick mode (in, all in, out, allout).
**			OUTPUT:
**						int vlst[]; -- list of segment nos.
**						int vlen; -- length of vlst.
**						int *pkid -- pick id.
**						int *newdist -- new distance object is from loc.
**    RETURNS      : 1 if found something, 0 if didn't, 2 if found everything.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_findsgpkras(segptr,findprms,vlst,vlen,loc,epsx,epsy,func,mode,
	pkid,newdist)
/*$ INPUT */
UG_segstli *segptr;
UG_findprms *findprms;
Gipoint *loc;
Gint epsx,epsy;
int (*func)();
int mode;
/*$ OUTPUT */
int vlst[10];								/* segment id list of seg found */
int *vlen;									/* length of vlst */
int *pkid;
int *newdist;
{
  UG_plylna2op *cmd;  					/* storage for a UG_cmd */
  int ncmd;								/* number of commands in a segment */
  int icmd;								/* ith command */
  int *p,cmdlen;
  int i,j,k,n,m,len,opcode;
  Gfloat x,y,z;
  Gfloat (*q)[];           	/* q is pointer to float array */
  char *s;
  char *ug_lsielt();			/* function to return list element */
  /*Gwpoint3 pt;*/
  int foundit;					/* false=didnt find anything */
  int found;
  Gnrect3 nrect;				/* NDC text extent rectangle */
  int inbox;
  int foundall;				/* 1 if everything found */
  char us[150];
#define EPS (UU_REAL) .0001 

	n=segptr->segid;
	uu_denter2(UU_GITRC,(us,
		"ug_findsgpkras(%x segid=%d) vlen=%d epsx,y=%d %d func=%x",
			segptr,n,*vlen,epsx,epsy,func));
	vlst[*vlen]=n;  (*vlen)++; 	/* record this ug_findsgpk activation */
	found=false;
	*newdist=epsx;						
	ncmd = ug_lsinelt((*(segptr->rassegpt)));
	uu_dprint(UU_GITRC,(us,"ug_findsgpkras. ncmd=%d",ncmd));

	foundall=1;				/* assume everything found until something fails */
 	/* for each command in seg n */
	for( icmd=0; icmd < ncmd; ++icmd ){
	cmd = (UG_plylna2op *)ug_lsielt(segptr->rassegpt,icmd);
	cmdlen = ug_lsilen(segptr->seglist,icmd)/sizeof(int);
	opcode=(*cmd).elttype;
/*	uu_dprint(UU_GITRC,(us,"ug_findsgpkras. opcode[%d]=%s cmdlen=%d",
				icmd,&ug_ops[opcode][0],cmdlen)); */
	if (opcode>UG_OPPTR) {       /* library command */
      /*ug_viwlib(cmd);                     /* call library proc */
		uu_dprint(-1,(us,"ug_findsgpkras can't handle library command"));
	}
	else {                     /* normal command */
		foundit=false;
    switch (opcode) {
	 case UG_PLYLNRASOP: len=(*(UG_plylnrasop *)cmd).len;
			foundit=ug_findpolylnras(len,(*(UG_plylnrasop *)cmd).pts,loc,
				epsx,epsy,newdist,mode);
			if (foundit!=true) foundall=0;
			break;
	 case UG_PLYMKRASOP: len=(*(UG_plymkrasop *)cmd).len;
			foundit=ug_findpolymkras(len,(*(UG_plymkrasop *)cmd).pts,loc,
				epsx,epsy,newdist,mode);
			if (foundit!=true) foundall=0;
			break;
	 case UG_TEXTRASOP: len=(*(UG_textrasop *)cmd).len;
			foundit=ug_findtextras(&(*(UG_textrasop *)cmd).position,
			(*(UG_textrasop *)cmd).string,&(*findprms).prats,loc,epsx,epsy,
				newdist,mode,0);
			if (foundit!=true) foundall=0;
			break;
	 case UG_FLAREARASOP: len=(*(UG_flarearasop *)cmd).len;
			/*ug_flaras(len,&(*cmd).gcint[2],(*segptr).userdata[0]); */
			/*if (foundit!=true) foundall=0;*/
			break;
	case UG_CELLRASOP:
			/*ug_dcellras(&(*cmd).gcint[1],&(*cmd).gcint[5],&(*cmd).gcint[7]);*/
			/*if (foundit!=true) foundall=0;*/
			break;
	case UG_CELLRUNRASOP:
			/*if (foundit!=true) foundall=0;*/
			break;
	case UG_PROCOP: 
			/*if (foundit!=true) foundall=0;*/
			break;
    case UG_CALLOP: 
	 	{
			int irtn2;
			UG_findprms newprms;
			UG_segstli *newsegptr;
			m=(*(UG_callop *)cmd).segno; 					/* call seg m */
			newsegptr=ug_segac(m);
			if (newsegptr!=NULL)  {
				zbytecp(newprms,(*findprms));
				/* recursive call */
         	irtn2=ug_findsgpkras(newsegptr,&newprms,vlst,vlen,loc,epsx,epsy,
					func,mode,pkid,newdist);
				if (irtn2!=2) foundall=0;
			}
		}
		break;
   case UG_FONTOP:
			zbytecp((*findprms).prats.txbundl.fp,(*(UG_fontop *)cmd).p);
			break;
   case UG_CHHGTOP: (*findprms).prats.txht=(*(UG_chhgtop *)cmd).height; 
			break;
	case UG_CHEXPOP: 
			(*findprms).prats.txbundl.expn=(*(UG_chexpop *)cmd).expn; break;
   case UG_CHPLANEOP: 
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			ug_ItoGpoint3((*findprms).prats.txpvec,(*(UG_chplaneop *)cmd).txpvc);
#else
			zbytecp((*findprms).prats.txpvec,(*(UG_chplaneop *)cmd).txpvc);
#endif
  			break;
   case UG_CHUP3OP: 
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
		ug_ItoGpoint3((*findprms).prats.txuv,(*(UG_chup3op *)cmd).upvec);
#else
			zbytecp((*findprms).prats.txuv,(*(UG_chup3op *)cmd).upvec);
#endif
        break;
   case UG_CHUP2OP:
			(*findprms).prats.txuv.x=(*(UG_chup2op *)cmd).upvec.x;
			(*findprms).prats.txuv.y=(*(UG_chup2op *)cmd).upvec.y;
			(*findprms).prats.txuv.z=0.;
			break;
   case UG_CHPATHOP: 
			(*findprms).prats.txpath=(*(UG_chpathop *)cmd).path;
			break;
   case UG_CHSPACEOP:
			(*findprms).prats.txbundl.space=(*(UG_chspaceop *)cmd).spacing;
			break;
   case UG_CHJUSTOP:
			zbytecp((*findprms).prats.txalign,(*(UG_chjustop *)cmd).align);
			break;
   case UG_SYMBOLOP:
			(*findprms).prats.mkbundl.type=(*(UG_symbolop *)cmd).type;
			break;
   case UG_PICKIDOP: (*findprms).pkid=(*(UG_pickidop *)cmd).pid;
			break;
    }											/* switch(opcode)  */
	}                 					/* normal command */
	if (foundit==true) {				/* found something */
		epsx= *newdist; epsy= *newdist;	/* make aperture smaller */
		found=true; 
		*pkid=(*findprms).pkid;		/* remember pkid of found*/
		if (func!=NULL) {
			UG_findit fnd;
			/* copy finding parms into fnd structure */
			fnd.find=true; fnd.found=true;
			fnd.epsx=epsx;  
			/* FIX next line to use current ws (not ws 0) and to go thru ws xform*/
			fnd.epsx=fnd.epsx/(*(ug_gksstli.wsopen[0].wdtptr)).dspsize.raster.x;
			fnd.epsy=epsy;
			fnd.epsy=fnd.epsy/(*(ug_gksstli.wsopen[0].wdtptr)).dspsize.raster.x;
			/* FIX next line to use current ws (not ws 0) */
			(*(ug_gksstli.wsopen[0].connid)[UG_DDEVNDC])(loc,&fnd.x,0);	
			fnd.dist= *newdist;
			fnd.dist=fnd.dist/(*(ug_gksstli.wsopen[0].wdtptr)).dspsize.raster.x;
			fnd.pickid= *pkid;
			fnd.pikmode=mode;
			fnd.vis=ug_find.vis;				/* FIX this */
			(*func)(*vlen,vlst,&fnd);		/* call user supplied function */
			/* FIX next 2 lines to use current ws and to go thru ws xform*/
			fnd.epsx=fnd.epsx * (*(ug_gksstli.wsopen[0].wdtptr)).dspsize.raster.x;
			fnd.epsy=fnd.epsy * (*(ug_gksstli.wsopen[0].wdtptr)).dspsize.raster.x;
			epsx=fnd.epsx; epsy=fnd.epsy;	/* in case user fctn changed aperture*/
			if (!fnd.find) break;		/* user func says stop looking */
		}										/* end if func!=NULL */
   }											/* end if foundit==true */
  }										/* for each command in seg */
	if (!found) 
		*vlen= *vlen-1;				/* remove this viewsg activation from list */
	
	if ((found==1)&&(foundall==1)) found=2;
	uu_dprint(UU_GITRC,(us,"%d=ug_findsgpkras(..)",found));
	uu_dexit;
	return(found);
}

/*********************************************************************
**    I_FUNCTION :  int ug_findpolylnras(n,points,rasloc,epsx,epsy,dist,mode) 
**								find polyline raster.
**    PARAMETERS   
**       INPUT  : 
**							Gipoint points[]; -- endpoints of line.
**							int n; -- size of points array.
**							Gipoint *loc; -- find location.
**							Gint epsx,epsy; -- half size of find aperture.
**							Gint mode;
**       INPUT/OUTPUT :   int *dist; -- on input, find half aperture.
**									on output, dist polyline is from rasloc if found.
**    RETURNS      : 0 if didn't find, 1 if found.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_findpolylnras(n,points,rasloc,epsx,epsy,dist,mode)
int n;							/* number of points in polyline */
Gipoint points[];
Gipoint *rasloc;
Gint epsx,epsy;				/* half size of find aperture */
int *dist;
Gint mode;
{
	int i;
	int found,foundit;
	
	uu_denter(UU_GITRC,(us,
		"ug_findpolylnras(%d, %d %d,%d %d epsx,y=%d %d, mode=%d)",
		n,points[0].x,points[0].y,rasloc->x,rasloc->y,epsx,epsy,mode));
	foundit=0;
	for (i=1; i<n; i++) { 
		found=ug_closlnras(&points[i-1],&points[i],rasloc,
			epsx,epsy,dist,mode);
		if (found) {
			foundit=1;
			epsx= *dist; epsy= *dist;		/* make aperture smaller */
			/* if looking for part in or part out this line passing ends it*/
			if ((mode==0)||(mode==1)) break;
		}
	}
	uu_dprint(UU_GITRC,(us,"%d=ug_findpolylnras. *dist=%d",foundit,*dist));
	uu_dexit;
	return(foundit);
}

/*********************************************************************
**    I_FUNCTION :  int ug_closlnras(p1,p2,loc,epsx,epsy,dist,mode)
**       Find out if raster line from p1 to p2 is within (without,etc) 
**			epsx,epsy distance of loc.
**    PARAMETERS   
**       INPUT  : 		Gipoint *p1,*p2; -- endpoints of line.
**								Gipoint *loc; -- find location.
**								Gint epsx,epsy; -- size of find aperture.
**								Gint mode; -- in, allin, out, allout.
**       OUTPUT :  		Gint *dist; -- distance from line if found.
**    RETURNS      :    1 if found, 0 if didn't.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_closlnras(p1,p2,loc,epsx,epsy,dist,mode)
Gipoint *p1,*p2;					/* ndc endpoints of line */
Gipoint *loc;						/* find point */
Gint epsx,epsy;					/* half find aperture */
Gint *dist;							/* if found, distance */
Gint mode;							/* find mode */
{
	Gint found;						/* return value */
	Gint in;
	Gipoint clospt;				/* closest point on line to loc. */

	Gint dx,dy;
	Gint xmin,xmax,ymin,ymax;		/* box surrounding line seg */
	Girect prect;							/* rect centerd at p epsx,epsy half size*/

	uu_denter(UU_GITRC,(us,"ug_closlnras(%d %d, %d %d, %d %d,%d,%d,%d,%d)",
		(*p1).x,(*p1).y,(*p2).x,(*p2).y,loc->x,loc->y,epsx,epsy,*dist,mode));

	/* see if the line from p1 to p2 is within (without, part in/out) 
		epsx,epsy of loc.*/

	/* first calculate rectangle surrounding the line segment */
	xmin = ((p1->x<p2->x) ? p1->x:p2->x);	/* xmin=min(p1->x,p2->x) */
	xmax = ((p1->x>p2->x) ? p1->x:p2->x);	/* xmax=max(p1->x,p2->x) */
	ymin = ((p1->y<p2->y) ? p1->y:p2->y);	/* ymin=min(p1->y,p2->y) */
	ymax = ((p1->y>p2->y) ? p1->y:p2->y);	/* ymax=max(p1->y,p2->y) */
	switch (mode) {
	case 0:									/* want at least part in  */
		/* if loc-box outside of rectangle bounding the line.. */
		if ((loc->x<(xmin-epsx))||(loc->x>(xmax+epsx))||(loc->y<(ymin-epsy))
			||(loc->y>(ymax+epsy))) {
				found=0; break;
		}
		prect.ll.x=loc->x-epsx;
		prect.ll.y=loc->y-epsy;
		prect.ur.x=loc->x+epsx;
		prect.ur.y=loc->y+epsy;
		found=ug_lineinrectras(p1,p2,&prect);
		if (found==1) {						/* line goes within pbox */
			/* dist from p to line seg */
			*dist=ug_ptlndistras(loc,p1,p2,&dx,&dy);		
		}
		break;							/* end of case 0 (at least part in) */
	case 1:								/* want at least part out */
		/* line is part out iff box surrounding line is part out of loc-box */
		if ((xmin<(loc->x-epsx))||(ymin<(loc->y-epsy))||
			 		(xmax>(loc->x+epsx))||(ymax>(loc->y+epsy))) {
			found=1;						/* line part out */
			*dist=ug_ptlndistras(loc,p1,p2,&dx,&dy);	/* calc dist from p to line seg */
		}
		else  found=0;
		break;
	case 2:								/* want all in */
		/* line is all in iff box surrounding  line is all in loc-box */
		if ((xmin<(loc->x-epsx))||(ymin<(loc->y-epsy))||
			 		(xmax>(loc->x+epsx))||(ymax>(loc->y+epsy))) {
			found=0;			/* line is not all in */
		}
		else {				/* line is all in */
			found=1;
			/* calc dist from p to line seg */
			*dist=ug_ptlndistras(loc,p1,p2,&dx,&dy);	
		}
		break;
	case 3:								/* want all out */
		prect.ll.x=loc->x-epsx;
		prect.ll.y=loc->y-epsy;
		prect.ur.x=loc->x+epsx;
		prect.ur.y=loc->y+epsy;
		if (ug_lineinrectras(p1,p2,&prect)==0) {		/* line is all out */
			found=1;
			/* calc dist from loc to line seg */
			*dist=ug_ptlndistras(loc,p1,p2,&dx,&dy);	
		}
		else found=0;
		break;
	}										/* end of switch(mode) */
	uu_dprint(UU_GITRC,(us,"%d=ug_closlnras(*dist=%d).",found,*dist));
	uu_dexit;
	return(found);
}	

/*********************************************************************
**    I_FUNCTION :  ug_findpolymkras(n,points,rasloc,epsx,epsy,dist,mode) -- 
**						find polymarker-raster
**    PARAMETERS   
**       INPUT  : 
**							int n;
**							Gipoint points[];
**							Gipoint *rasloc; -- center of find aperture.
**							Gint epsx,epsy; -- half size of find aperture.
**							int mode; -- part in, part out, all in, all out.
**							Gint *dist; -- find half aperture.
**       OUTPUT :  
**							Gint *dist; -- horiz or vert distance from rasloc.
**    RETURNS      : 1 if found, 0 if not.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_findpolymkras(n,points,rasloc,epsx,epsy,dist,mode)
int n;							/* number of points */
Gipoint points[];
Gipoint *rasloc;				/* location to find */
Gint epsx,epsy;				/* half size of find aperture */
Gint *dist;						/* on output if found, distance  */
int mode;						/* find mode: in, out, all in, all out */
{
	int i;
	int in,wantin;
	int thisdist;				/* distance of each point found */
	int found;
	int dx,dy;
#define EPSMRK (UU_REAL) .0001

	uu_denter(UU_GITRC,(us,
		"ug_findpolymkras(%d, %d %d, %d %d, epsx,y=%d %d,%d)",n,
			points[0].x,points[0].y,rasloc->x,rasloc->y,epsx,epsy,mode));
	found=0;
	/* see if marker point is withhin (without, etc.),*/
	if ((mode==0)||(mode==2)) wantin=1;			/* all or part in */
	else wantin=0;
	for (i=0; i<n; i++) { 
		dx=abs(rasloc->x-points[i].x);
		dy=abs(rasloc->y-points[i].y);
		if ((dx<epsx)&&(dy<epsy)) in=1;
		else in=0;
		if (wantin==in) {						/* found one, either in or out*/
			thisdist=(dx>dy)?dx:dy;			/* max(dx,dy) */
			if (thisdist<*dist) *dist=thisdist;
			found=1;								/* remember we found at least 1 */
			epsx= *dist; epsy= *dist;		/* make aperture smaller */
			/*  if looking for part in or part out, this point passing ends it */
			if ((mode==0)||(mode==1)) break;
		}											/* end found one */
	}												/* end for (i=0; i<n) */
	uu_dprint(UU_GITRC,(us,"%d=ug_findpolymkras(dist=%d)",found,*dist));
	uu_dexit;
	return(found);
}

/*********************************************************************
**    I_FUNCTION :  ug_findtextras(posn,str,prats,rasloc,epsx,epsy,dist,
**								mode,ws) -- find raster text.
**    PARAMETERS   
**       INPUT  : 
**							Gipoint *posn; -- start posn of text
**							char *str; -- the text.
**							UG_prat3 *prats; -- use only text attributes.
**							Gipoint *rasloc; -- center of find aperture.
**							Gint epsx,epsy; -- half size of find aperture.
**							int mode; -- part in, part out, all in, all out.
**							Gws ws; -- workstation.
**       OUTPUT :  
**							Gint *dist; -- dist from rasloc to text rect, or 0.
**												if rasloc is within text rectangle.
**          output
**    RETURNS      : 1 if found, else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_findtextras(posn,str,prats,rasloc,epsx,epsy,dist,mode,ws)
Gipoint *posn; 				/* start posn of text */
char *str; 						/* the text. */
UG_prat3 *prats;				/* prim atts (only text attributes used) */
Gipoint *rasloc;				/* center of find aperture */
Gint epsx,epsy;				/* half size of find aperture */
Gint *dist;						/* On output, distance from rasloc to text rect,
										or 0 if rasloc within text rectangle. */
int mode;						/* part in, part out, all in, all out */
Gws ws;
{
	int found;
	Girect rect;				/* text rectangle */
	Gint dx,dy;
	Gint dxl,dyl,dxr,dyu;

	uu_denter(UU_GITRC,(us,
		"ug_findtextras(%d %d,%s,prats,%d %d,%d %d,%d,%d)", 
		(*posn).x,(*posn).y,str,rasloc->x,rasloc->y,epsx,epsy,mode,ws));
	/* get rect=text raster rectangle */
	ug_txrectras(posn,str,&rect,prats,ws);	
	dxl=rect.ll.x-rasloc->x;
	dxr=rasloc->x-rect.ur.x;
	dx=(dxl>dxr)?dxl:dxr;	/* dx=distance ug_find.x is from text rect, 
										negative if within rectangle */
	dyl=rect.ll.y-rasloc->y;
	dyu=rasloc->y-rect.ur.y;
	dy=(dyl>dyu)?dyl:dyu;	/* dy=distance ug_find.y is from text rect 
												or negative if within rectangle */
	uu_dprint(UU_GITRC,(us, "ug_findtextras. text rect=%d %d, %d %d",
			rect.ll.x,rect.ll.y,rect.ur.x,rect.ur.y));
	switch(mode) {
	case 0:							/* part or all in */
		/* see if ug_find posn is within epsx,epsy of text rectangle */
		if ((dx<epsx)&&(dy<epsy)) {
			found=1;
			*dist=(dx>dy)?dx:dy;
			if (*dist<0) *dist=0;
			epsx= *dist; epsy= *dist;	/* make aperture smaller */
		}
		else found=0;
		break;
	case 1:										/* all or part out */
		if ((rect.ll.x>(rasloc->x+epsx))||
			(rect.ll.y>(rasloc->y+epsy))||
			(rect.ur.x<(rasloc->x-epsx))||
			(rect.ur.y<(rasloc->y-epsy))) {
				found=1;
				*dist=(dx>dy)?dx:dy;
				if (*dist<0) *dist=0;
				epsx= *dist; epsy= *dist;
			}
			break;
	case 2:									/* text is all within pick aperture */
		if ((rect.ll.x>(rasloc->x-epsx))&&
			(rect.ll.y>(rasloc->y-epsy))&&
			(rect.ur.x<(rasloc->x+epsx))&&
			(rect.ur.y<(rasloc->y+epsy))) {
			found=1;
			*dist=(dx>dy)?dx:dy;
			if (*dist<0) *dist=0;
			epsx= *dist; epsy= *dist;
		}
		break;
	case 3:										/* all out */
		if ((rect.ll.x>(rasloc->x+epsx))||
			(rect.ll.y>(rasloc->y+epsy))||
			(rect.ur.x<(rasloc->x-epsx))||
			(rect.ur.y<(rasloc->y-epsy))) {
				found=1;
				*dist=(dx>dy)?dx:dy;
				if (*dist<0) *dist=0;
				epsx= *dist; epsy= *dist;
		}
		break;
	}												/* end switch(mode) */
	uu_dprint(UU_GITRC,(us,"%d=ug_findtextras(*dist=%d)",found,*dist));
	uu_dexit;
	return(found);
}

/*********************************************************************
**    I_FUNCTION     :  ug_txrectras(pos,s,rect,prats,ws)	
**		determine nrect= text rect for string s.
**    PARAMETERS   
**       INPUT  : 
**						Gipoint *pos;				/* starting point of text 
**						char *s;						/* text 
**						UG_prat3 *prats;			/* primitive attributes
**						Gws ws;						/* workstation 
**       OUTPUT :  
**						Girect *rect;				/* raster rectangle containing text 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_txrectras(pos,s,rect,prats,ws)	
/* determine rect= text rect for string s, starting at raster position pos. */
Gipoint *pos;				/* starting point of text */
char *s;						/* text */
Girect *rect;				/* raster rectangle containing text */
UG_prat3 *prats;			/* primitive attributes, only text atts used */
Gws ws;						/* workstation */
{
	Gint height,width;	/* size of text rectangle in raster units */
	Gtxprec prec;
	Gtxbundl *p;
	UG_wdt *wdtpt;
	Gint dx;
	Gtxhor halign;

	uu_denter(UU_GITRC,(us,
		"ug_txrectras(%d %d,%s,rect,%d)",(*pos).x,(*pos).y,s,ws));
	p= &((*prats).txbundl);
	prec=(*p).fp.prec;					/* current text precision */
	uu_dprint(UU_GITRC,(us,"ug_txrectras. txht=%g prec=%d path=%d",
		prats->txht,prec,prats->txpath));
	(*rect).ll.x=pos->x; (*rect).ll.y=pos->y;
	/* calculate text rectangle height, width */
	/* for now assume width = .6*height for each character */
	switch((*prats).txpath) {
	case UG_TP_UP: case UG_TP_DOWN: 
		if (prec==UG_STROKE) {		/* use text height if stroke precision */
			width= .6*(*prats).txht;
			height=(*prats).txht*strlen(s);
		}
		else {						/* use hardware size of workstation */
			wdtpt=ug_gksstli.wsopen[ws].wdtptr;    /* get wdt pointer to ws */
			width=(*wdtpt).dspsize.device.x/(*wdtpt).colmax;
			height=strlen(s)*(*wdtpt).dspsize.device.y/(*wdtpt).rowmax;
		}
		if ((*prats).txpath==UG_TP_DOWN) 
				(*rect).ll.y=(*rect).ll.y-height;
		break;
	case UG_TP_LEFT: case UG_TP_RIGHT:
		if (prec==UG_STROKE) {
			height=(*prats).txht;						/* get raster height */
			width=(.6*(*prats).txht*strlen(s));		/* get raster width */
		}
		else {						/* use hardware text size of workstation */
			wdtpt=ug_gksstli.wsopen[ws].wdtptr;    /* get wdt pointer to ws */
			height=(*wdtpt).dspsize.device.y/(*wdtpt).rowmax;
			width=strlen(s)*(*wdtpt).dspsize.device.x/(*wdtpt).colmax;
		}
		/* account for horizontal alignment */
		halign=(*prats).txalign.hor;
		if (halign==UG_TH_NORMAL) {
			if ((*prats).txpath==UG_TP_RIGHT) halign=UG_TH_LEFT;
			else halign=UG_TH_RIGHT;	/* normal for left path is right aligned*/
		}
		switch (halign) {
		case UG_TH_LEFT: dx=0; break;
		case	UG_TH_CENTRE: dx=width/2; break;
		case UG_TH_RIGHT: dx=width; break;
		}
		(*rect).ll.x=(*rect).ll.x-dx;
		break;
	}						 /* end switch on text path */
	(*rect).ur.x=(*rect).ll.x+width; 
	(*rect).ur.y=(*rect).ll.y+height;
	uu_dprint(UU_GITRC,(us,"ug_txrectras returns rect=%d %d %d %d",
		(*rect).ll.x,(*rect).ll.y,(*rect).ur.x,(*rect).ur.y));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  int ug_ptlndistras(p,s,e,&dx,&dy) - dist from pt to line.
**       Return max of horiz and vert dist from point p to line segment 
**			defined by  s and e.
**    PARAMETERS   
**       INPUT  :  Gfloat p[2]; -- point near the line segment.
**						 Gfloat s[2],e[2]; -- points defining the line segment.
**       OUTPUT :  float *dx,*dy -- horiz, vert distance from p to line seg.
**    RETURNS      : max(*dx,*dy)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_ptlndistras(p,s,e,dx,dy)
Gint p[2],s[2],e[2];
int *dx,*dy;
{
	Gint q[2];
	Gint ems[2];					/* e-s */
	Gint pms[2];					/* p-s */
	Gint emq[2],smq[2];
	Gint magems;					/* magnitude(e-s)**2 */
	Gint dot;						/* (p-s)dot(e-s) */
	Gint dmax;
	Gint dists,diste;
	Gint dist;					/* return value */
	Gint dx1,dy1;
	char us[120];

	/* calculate q = point on infinite line
	passing thru s and e nearest to point p:
	q = s + (p-s)dot(e-s)*(e-s)/magnitude(e-s)  */
	ems[0]=e[0]-s[0]; ems[1]=e[1]-s[1];		/* (e-s) */
	pms[0]=p[0]-s[0]; pms[1]=p[1]-s[1];		/* (p-s) */
	magems = (ems[0]*ems[0]+ems[1]*ems[1]); /* magnitude(e-s)**2 */
	if (magems==0) {					/* check for zero length line */
		/* line is zero length, just return dist between p and s */
		*dx=abs(pms[0]); *dx=abs(pms[1]);
		dist=(*dx > *dy) ? *dx : *dy;		/* dist=max(*dx,*dy) */
	}
	else {										/* line not zero length */
		dot = pms[0]*ems[0]+pms[1]*ems[1];	/*(p-s)dot(e-s) */
		q[0]=s[0]+dot*ems[0]/magems;
		q[1]=s[1]+dot*ems[1]/magems;
		*dx=p[0]-q[0]; *dy=p[1]-q[1];
		*dx=abs(*dx); *dy=abs(*dy);
		uu_denter2(UU_GITRC,(us,"ug_ptlndistras. q=%d %d, dx,dy=%d %d",
			q[0],q[1],*dx,*dy));
		uu_dexit;
		/* see if q is on the line segment (i.e. between e and s) */
 		/* if (e-q)dot(s-q)>0, q is not in middle */
 		emq[0]=e[0]-q[0]; emq[1]=e[1]-q[1];
 		smq[0]=s[0]-q[0]; smq[1]=s[1]-q[1];
 		if ((emq[0]*smq[0]+emq[1]*smq[1])>0) {
			/*  gets here if point q is not between s and e. Therefore either s 
				or e is nearest point on line segment to point p. */
			dx1=s[0]-p[0]; dy1=s[1]-p[1];
			dx1=abs(dx1); dy1=abs(dy1);
			dists = dx1*dx1+dy1*dy1;
			*dx=e[0]-p[0]; *dy=e[1]-p[1];
			*dx=abs(*dx); *dy=abs(*dy);
			diste = (*dx)*(*dx)+(*dy)*(*dy);
			if (dists<diste) {					/* point s is closest */
				*dx=dx1; *dy=dy1;
			}
		}												/* end q not between s and e */
		dist=(*dx > *dy) ? *dx : *dy;			/* dist=max(*dx,*dy) */
	}													/* end line is not zero length */
	uu_denter2(UU_GITRC,(us,
		"%d=ug_ptlndistras(p=%d %d, line=%d %d, %d %d, *dx,*dy=%d %d",
		dist,p[0],p[1],s[0],s[1],e[0],e[1],*dx,*dy));
	uu_dexit;
	return(dist);
}

/*********************************************************************
**    I_FUNCTION     :  ug_lineinrectras(gcurpos,npos,rect)
**			see if line from gcurpos to npos goes within rect. 
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : 1 if line goes in rect, else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_lineinrectras(gcurpos,npos,rect)
/* return 1 if line from gcurpos to npos goes within rect. Else return 0 */
Gint gcurpos[2],npos[2];
Girect *rect;								/* rectangle to clip to */
{ 
	int irtn;						/* return value */
	Gint pos1[2],pos2[2];
	int i,j,ic1,ic2,ibit,ibit2;
  Gfloat a;
  Gint vwport[2][2];				/* local copy */
  Gint vpeps;

  uu_denter(UU_GITRC,(us,"ug_lineinrectras(%d %d, %d %d rect=%d %d %d %d)",
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
	/* make viewport 1 smaller to insure we push endpoints onto it */
	for (i=0; i<2; i++) {
		vpeps=1;
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
	uu_dprint(UU_GITRC,(us,"lineinrectras.  ic1=%d ic2=%d"));
	/* line is part on and part off, or all off */
	/* see if pos1 is off */
	if (ic1!=0) {             /* push pos1 to rect edge */
		uu_dprint(UU_GITRC,(us,"push pos1 to edge"));
    for (i=0; i<2; i++) {
      /* check min value on this axis */
      if (pos1[i]<vwport[i][0]) {
        /* push pos1 toward higher value on this axis */
        if (pos2[i]!=pos1[i]) {
          a=(vwport[i][0]-pos1[i]); a=a/(pos2[i]-pos1[i]); 
          for (j=0; j<2; j++) pos1[j]=a*(pos2[j]-pos1[j])+pos1[j];
        }
      }
      else {
        /* check max value on this axis */
        if (pos1[i]>=vwport[i][1]) {
          /* push pos1 toward lower value on this axis */
          if (pos2[i]!=pos1[i]) {
            a=(vwport[i][1]-pos1[i]); a=a/(pos2[i]-pos1[i]);
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
      if (pos2[i]!=pos1[i]) {
        a=(vwport[i][0]-pos1[i]); a=a/(pos2[i]-pos1[i]);
        for (j=0; j<2; j++) pos2[j]=a*(pos2[j]-pos1[j])+pos1[j];
      }
    }
    else {
      /* check max value on this axis */
      if (pos2[i]>vwport[i][1]) {
        /* push pos2 toward lower value on this axis */
        if (pos2[i]!=pos1[i]) {
          a=(vwport[i][1]-pos1[i]); a=a/(pos2[i]-pos1[i]);
          for (j=0; j<2; j++) pos2[j]=a*(pos2[j]-pos1[j])+pos1[j];
        }
      }
    }
	}                            /* for i */
	/* pos2 now  in rect. */
	/* see if line is all off screen */
	/* first  expand view volume to make sure don't clip stuff on a face */
	for (i=0; i<2; i++) {
		vpeps=1;
		vwport[i][0]=vwport[i][0]-vpeps;
		vwport[i][1]=vwport[i][1]+vpeps;
	}
	uu_dprint(UU_GITRC,(us,"lineinrectras pos1=%d %d pos2=%d %d",
		pos1[0],pos1[1],pos2[0],pos2[1]));
	uu_dprint(UU_GITRC,(us,"lineinrectras vport=%d %d  %d %d",
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
	uu_dprint(UU_GITRC,(us,"%d=ug_lineinrectras(..)",irtn));
	uu_dexit;
	return(irtn);
}
