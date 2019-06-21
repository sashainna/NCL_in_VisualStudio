/*********************************************************************
**    NAME         :  ginq.c -- DIGS inquiry functions.
**       CONTAINS:
**		Gwpoint3 *gqtxplane() -- inquire character text plane.
**		Gwpoint *gqcharup() -- inquire character up vector. 
**		Gtxalign *gqtextalign() -- inquire text alignment
**		Gwpoint * gqpatsize() -- inquire pattern size
**		Gwpoint *gqpatrefpoint() -- inquire pattern reference point.
**		Gasfs *gqasfs() -- inquire aspect source flags.
**		Gerror gqtextextent3(ws,position,string,concat,extent)
**		Gcobundl *gqcolorrep(ws,index,type) -- inquire color rep.
**    Gint gqmaxcolor(ws) -- inquire maximum colors.
**		Gerror giwswindow3(win) -- Inquire workstation window 3D.
**		Gerror givpn3(xform,vpn) -- Inquire view plane normal.
**		Gerror givup3(xform,vpn) -- Inquire view plane up vector.
**		Gerror givref3(xform,vref)-- Inquire view reference point.
**		Gerror gqopenstr(typ,segno) -- inquire open structure.
**		int gqndev(ws,nos) -- inquire number of logical input devices.
**		gqmenutextsize(ws,ht,wid) -- inquire menu text size on ws.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       ginq.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:20
*********************************************************************/
#include <stdio.h>
#include "gtbl.h"
#include "g.h"
#include "gerror.h"
#include "ginq.h"
#include "gdidd.h"
#include "gdidd2.h"
#include <math.h>
#include "udebug.h"
#include "gsegac.h"
#include "zsysdep.h"

/* the simple inquiry functions are defined in gksinq.h */

/********************************************************************* 
**  E_FUNCTION:  Gwpoint *gqcharup() -- inquire character up vector. 
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**  RETURNS      :  pointer to current character up vector.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gwpoint *gqcharup()
{
	static Gwpoint3 upvec;
	upvec.x=ug_gksstli.curprats.txuv.x;
	upvec.y=ug_gksstli.curprats.txuv.y;
	upvec.z=ug_gksstli.curprats.txuv.z;
	return((Gwpoint *)&upvec);
}

/********************************************************************* 
**  E_FUNCTION:  Gwpoint3 *gqtxplane() -- inquire text plane normal
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**  RETURNS      :  pointer to current text plane normal
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gwpoint3 *gqtxplane()
{
	static Gwpoint3 normvec;
	normvec.x=ug_gksstli.curprats.txpvec.x;
	normvec.y=ug_gksstli.curprats.txpvec.y;
	normvec.z=ug_gksstli.curprats.txpvec.z;
	return(&normvec);
}

/********************************************************************* 
**  E_FUNCTION:  Gtxalign *gqtextalign() -- inquire text alignment
**      Returns a pointer to the current text alignment structure.
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**
**  RETURNS      :  pointer to current text alignment structure.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gtxalign *gqtextalign()
{
	static Gtxalign txa;
	zbytecp(txa,ug_gksstli.curprats.txalign);		/* structure assignment */
	return(&txa);
}

/********************************************************************* 
**  E_FUNCTION:  Gwpoint * gqpatsize() -- inquire pattern size
**					***** not implemented yet ******
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**  RETURNS      :  Pointer to current pattern size.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gwpoint *gqpatsize()
{
	static Gwpoint p;
	p.x=ug_gksstli.curprats.patsize.x;
	p.y=ug_gksstli.curprats.patsize.y;
	return(&p);
}

/********************************************************************* 
**  E_FUNCTION:  Gwpoint *gqpatrefpoint() -- inquire pattern reference point.
**				**** not implemented yet ****
**  PARAMETERS   
**      INPUT:  none
**      OUTPUT: none
**
**  RETURNS      :  pointer to current pattern reference point.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gwpoint *gqpatrefpoint()
{
	static Gwpoint p;
	p.x=ug_gksstli.curprats.patrefpt.x;
	p.y=ug_gksstli.curprats.patrefpt.y;
	return(&p);
}

/********************************************************************* 
**  E_FUNCTION:  Gasfs *gqasfs() -- inquire aspect source flags.
**					**** not implemented yet ******
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**  RETURNS      :  pointer to asf structure.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gasfs *gqasfs()
{
	static Gasfs f;
	zbytecp(f,ug_gksstli.curprats.asfs);		/* structure assignment */
	return(&f);
}

/*********************************************************************
**    E_FUNCTION:  Gerror gqtextextent3(ws,position,string,concat,extent)
**       Returns in extent the bounding rectangle of string, starting at
**			position. Extent is a function of char height, char up vector,
**			text path, text alignment of current text font and precision.
**        Call ug_fchext to get info for CHAR and STROKE precision-
**        this is part of the wstext routines that do the text stroking.
**    PARAMETERS   
**       INPUT  :  Gws *ws -- Pointer to workstation.
**						 Gwpoint3 *position -- starting position of string.
**						 Gchar *string -- character string.
**       OUTPUT :  Gwpoint3 *concat -- concatenation position.
**						 Gwrect3 *extent -- text extent rectangle.
**    RETURNS      : NCL_NO_ERROR if all was OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gqtextextent3(ws,position,string,concat,extent)
/*$ INPUT */
Gws *ws;					/* workstation */
Gwpoint3 *position;		/* start position of string */
Gchar *string;			/* text string, null terminated */
/*$ OUTPUT */
Gwpoint3 *concat;		/* returned concatenation point */
Gwrect3 *extent;		/* returned text extent rectangle */
{
	Gerror irtn;
	Gndc ht,wid;	/* height and width of string from workstation */
	Gnpoint3 ndcpos;		/* text start position in ndc */
	UG_dtext prms;							/* parms to ug_fchext if needed */
	uu_denter(UU_GTRC,(us,"gqtextextent3(%d,%g %g %g,%s,concat,extent)",
							*ws,(*position).x,(*position).y,(*position).z,
							string));
/*
.....Batch mode
.....Call Hershey text routine directly
*/
	if ((ug_gksos.sysstate!=UG_WSOP)&&(ug_gksos.sysstate!=UG_WSAC)
					&&(ug_gksos.sysstate!=UG_SGOP))
	{
		if (ug_gksstli.curprats.txbundl.fp.prec==UG_STROKE)
		{
			prms.pos.x = position->x;
			prms.pos.y = position->y;
			prms.pos.z = position->z;
			prms.slen = strlen(string);
			strcpy(prms.s,string);
			ug_fchext(&prms,concat,extent);
			irtn=NCL_NO_ERROR;
		}
		else
		{
			ug_errorhand(ENOTWSOP,"gqtextextent",&ug_gksos.sysstate); 
			irtn=ENOTWSOP;
		}
	}
	else if ((*ws<0)||(*ws>=UG_MAXOPWS)) {
		ug_errorhand(EWSIDINV,"gqtextextent",ws);
		irtn=EWSIDINV;
	}
	else if (*ws>=ug_gksstli.nowsopen) {			/* workstation not open */
		ug_errorhand(EWSNOTOP,"gqtextextent",ws);
		irtn=EWSNOTOP;
	}
	else if (((*ug_gksstli.wsopen[*ws].wdtptr).category!=UG_OUTPUT)&&
				((*ug_gksstli.wsopen[*ws].wdtptr).category!=UG_OUTIN)) {
		ug_errorhand(EWSNOTOO,"gqtextextent",ws);
		irtn=EWSNOTOO;
	}
	else {												/* no errors */
		/* Call workstation's entry to get text height and width. If stroke
		 * precision, the entry also calculates concat and extent. 
		 */
		(*(ug_gksstli.wsopen[*ws].connid)[UG_DTXTEXT])
					(*ws, string, &ht, &wid, position, concat, extent);

		/* For STRING, CHAR precison, calculate concat and extent */
		if (ug_gksstli.curprats.txbundl.fp.prec!=UG_STROKE)
		{
			/* get position in ndc */
			gwndc3(&ndcpos.x,&ndcpos.y,&ndcpos.z,(*position).x,
						(*position).y,(*position).z);
			/* here should take into account text plane, char up vec, etc. */
			switch (ug_gksstli.curprats.txalign.hor) {
			case UG_TH_NORMAL:
			case UG_TH_LEFT:
				break;
			case UG_TH_CENTRE:
				ndcpos.x =ndcpos.x-wid/2;
				break;
			case UG_TH_RIGHT:
				ndcpos.x=ndcpos.x-wid;
				break;
			}
			/* ndcpos is now ll corner of text extent rect in ndc */
			/* transform back into wc */
			ndcpos.y = ndcpos.y - ht/4;
			gndcw3(&(*extent).llf.x,&(*extent).llf.y,&(*extent).llf.z,
						ndcpos.x,ndcpos.y,ndcpos.z);
			/* now set ndcpos = upper right of text extent rect in ndc */
			ndcpos.x=ndcpos.x+wid; ndcpos.y=ndcpos.y+ht+ht/4;
			gndcw3(&(*extent).urb.x,&(*extent).urb.y,&(*extent).urb.z,
						ndcpos.x,ndcpos.y,ndcpos.z);
			/* set concat position to lower right of extent */
			(*concat).x=(*extent).urb.x;
			(*concat).y=(*extent).llf.y;
			(*concat).z=(*extent).urb.z;
		}
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gcobundl *gqcolorrep(ws,index,type) -- inquire color rep.
**      Inquire color representation. Returns reg,green,blue components
**			of the specified color index on the specified workstation.
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id.
**					 Gindex index -- color index.
**					 Gqtype type -- either SET or REALIZED. If SET, return
**								bundle from GKS state table. If REALIZED, return
**								bundle from workstation. Not used now. Always
**								return bundle from GKS state table.
**		  OUTPUT: none
**
**  RETURNS      :  pinter to red,green,blue color rep.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gcobundl *gqcolorrep(ws,index,type)	/* inquire color rep */
/*$ INPUT */
Gws *ws;					/* workstation id */
Gindex index;			/* color index */
Gqtype type;			/* not used now */
{
	UG_outwdt *wdtoutpt;
	int n;
	Gcobundl *irtn;
/*	char us[100];*/
	wdtoutpt= (*ug_gksstli.wsopen[*ws].wdtptr).outwdtpt;	
	n=(*wdtoutpt).cofac.colors;
	uu_denter2(UU_GTRC,(us,"gqcolorrep(ws=%d, index=%d, type=%d) maxcolri=%d",
					*ws,index,type,n));
	if ((index<0)||(index>=n))  {
		ug_errorhand(EBADCOLX,"gqcolorrep");
		irtn=NULL;
	}
	else {
		irtn= &(*wdtoutpt).cobundl[index];
		uu_denter2(UU_GTRC,(us,"gqcolorrep returns rgb=%g %g %g",
				(*irtn).red,(*irtn).green,(*irtn).blue));
		uu_dexit;
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gint gqmaxcolor(ws) -- inquire maximum colors.
**      Inquire maximum number of colors on workstation.
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id.
**		  OUTPUT: none
**
**  RETURNS      :  Maximum number of "graphic" colors.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gint gqmaxcolor(ws)
/*$ INPUT */
Gws *ws;					/* workstation id */
{
	int n;
	UG_outwdt *wdtoutpt;
/*	char us[100];*/

	wdtoutpt= (*ug_gksstli.wsopen[*ws].wdtptr).outwdtpt;	
	n=(*wdtoutpt).cofac.colors;

	uu_denter2(UU_GTRC,(us,"gqmaxcolor(ws=%d) maxcolors=%d",*ws,n));
	uu_dexit;

	return(n);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror giwswindow3(win) -- Inquire workstation window 3D.
**  PARAMETERS   
**      OUTPUT: Gnrect3 *win -- workstation window (ndc space).
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror giwswindow3(win)    /* inquire workstation window  (ndc space) */
/*$ OUTPUT */
Gnrect3 *win;
{
	 
	uu_denter(UU_GTRC,(us,"giwswindow3(%3g %3g %3g, %3g %3g %3g)",
			ug_gksstli.vtran[ug_gksstli.curvwindex].vport.llf.x,
			ug_gksstli.vtran[ug_gksstli.curvwindex].vport.llf.y,
			ug_gksstli.vtran[ug_gksstli.curvwindex].vport.llf.z,
			ug_gksstli.vtran[ug_gksstli.curvwindex].vport.urb.x,
			ug_gksstli.vtran[ug_gksstli.curvwindex].vport.urb.y,
			ug_gksstli.vtran[ug_gksstli.curvwindex].vport.urb.z));
	zbytecp(*win,ug_gksstli.vtran[ug_gksstli.curvwindex].vport);			/* structure assignment */
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror givpn3(xform,vpn) -- Inquire view plane normal.
**  PARAMETERS   
**      INPUT:  Gint xform -- transformation number.
**      OUTPUT: Gwpoint3 *vpn -- view plane normal.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror givpn3(xform,vpn)					/* inquire view plane normal */
/*$ OUTPUT */
Gwpoint3 *vpn;
/*$ INPUT */
Gint xform;
{
	 uu_denter(UU_GTRC,(us,"givpn3(%d,vpn)",xform));
	(*vpn).x=ug_gksstli.vtran[xform].vpnorm.x; 
	(*vpn).y=ug_gksstli.vtran[xform].vpnorm.y; 
	(*vpn).z=ug_gksstli.vtran[xform].vpnorm.z;
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror givup3(xform,vup) -- Inquire view plane up vector.
**  PARAMETERS   
**      INPUT:  Gint xform -- transformation number.
**      OUTPUT: Gwpoint3 *vup -- view plane up vector.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror givup3(xform,vup)					/* inquire view plane up vector */
/*$ OUTPUT */
Gwpoint3 *vup;
/*$ INPUT */
Gint xform;
{
	 uu_denter(UU_GTRC,(us,"givup3(%d,vup)",xform));
	(*vup).x=ug_gksstli.vtran[xform].vup.x; 
	(*vup).y=ug_gksstli.vtran[xform].vup.y; 
	(*vup).z=ug_gksstli.vtran[xform].vup.z;
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror givref3(xform,vref)-- Inquire view reference point.
**  PARAMETERS   
**      INPUT:  Gint xform -- transformation number.
**		  OUTPUT: Gwpoint3 *vref -- view reference point.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror givref3(xform,vref)					/* inquire view ref point */
/*$ OUTPUT */
Gwpoint3 *vref;
/*$ INPUT */
Gint xform;
{
	 uu_denter(UU_GTRC,(us,"givref3(vref)"));
	(*vref).x=ug_gksstli.vtran[xform].vrefpt.x; 
	(*vref).y=ug_gksstli.vtran[xform].vrefpt.y; 
	(*vref).z=ug_gksstli.vtran[xform].vrefpt.z;
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    E_FUNCTION :  Gerror gqopenstr(typ,segno) -- inquire open structure.
**    PARAMETERS   
**       INPUT  :  none
**       OUTPUT :  int *typ -- type of structure (NIL, NON_RETAINED,STRUCTURE)
**						 int *segno -- id of open structure.
**    RETURNS      : NCL_NO_ERROR if all went ok, ENOTSGOP of no struct open.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gqopenstr(typ,segno)				/* inquire open structure */
/*$ OUTPUT */
int *typ;
int *segno;
{
	Gerror irtn;
/*	char us[100];*/

	irtn=NCL_NO_ERROR;
	if (ug_gksos.sysstate!=UG_SGOP)  {
		irtn=ENOTSGOP; 
		*typ=0; *segno= -1;
	}
	else {
		*typ=2; *segno=ug_gksstli.opnseg;
	}
	uu_denter2(UU_GITRC,(us,"%d=gqopenstr(%d,%d)",irtn,*typ,*segno));
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  int gqndev(ws,nos) inquire no. logical input devices.
**    PARAMETERS   
**       INPUT  : 	Gws *ws -- workstation id.
**       OUTPUT :  	int nos[6] -- number of loc, stroke, val,
**												choice,pick, string devices.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gqndev(ws,nos)				/* inquire no. of logical input devices*/
/*$ INPUT */
Gws *ws;
/*$ OUTPUT */
int nos[];							/* number loc, stroke,val,choice,pick,string*/
{
	UG_inwdt *p;
/*	char us[120];*/

	p= (*ug_gksstli.wsopen[*ws].wdtptr).inwdtpt;
	nos[0]=(*p).nloc;
	nos[1]=(*p).nstroke;
	nos[2]=(*p).nval;
	nos[3]=(*p).nchoice;
	nos[4]=(*p).npick;
	nos[5]=(*p).nstring;
	uu_denter2(UU_GTRC,(us,
		"gqndev(%d,nloc=%d nstroke=%d nval=%d nchoice=%d npick=%d nstring=%d)",
		*ws,nos[0],nos[1],nos[2],nos[3],nos[4],nos[5]));
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  gqmenutextsize(ws,ht,width) -- inquire menu text size.
**    PARAMETERS   
**       INPUT  : 	Gws *ws -- workstation.
**       OUTPUT :  	Gfloat *ht,*width; -- height and width, in DC.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gqmenutextsize(ws,ht,width)
/*$ INPUT */
Gws *ws;
/*$ OUTPUT */
Gfloat *ht,*width;
{
	int prms[4];
	Gfloat reply[3];
/*	char us[80];*/
	(*(ug_gksstli.wsopen[*ws].connid)[UG_DMENUTEXTSIZE])
					(prms,reply);
	*ht=reply[0]; *width=reply[1];
	uu_denter2(UU_GTRC,(us,"gqmenutextsize(%d, ht=%g, width=%g)",
		*ws,*ht,*width));
	uu_dexit;
}

