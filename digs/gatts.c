/*********************************************************************
**    NAME         :  gatts.c
**       CONTAINS:	DIGS user callable attribute routines.
** 	ug_dfats() -- set prim attributes to default values.
** 	gspat3(ats) -- set primitive attributes 3D.
** 	gipat3(ats) -- inquire primitive attributes 3D
** 	Gerror gslineindex(index) -- Set line bundle index.
** 	Gerror gslinetype(typ)  -- Set line type.
** 	Gerror gslinewidth(width) -- Set linewidth.
** 	Gerror gslinecolor(color) -- Set polyline color index.
** 	Gerror gsmarkindex(index) -- Set marker bundle index.
** 	Gerror gsmarktype(type) -- Set marker type (style).
** 	Gerror gsmarksize(size) -- Set marker size.
** 	Gerror gsmarkcolor(color) -- Set marker color index.
** 	Gerror gstextindex(index) -- Set text bundle index.
** 	Gerror gstextfp(fp) -- Set text font and precision.
** 	Gerror gscharexp(expn) -- Set character expansion factor.
** 	Gerror gscharspace(spacing) -- Set character spacing.
** 	Gerror gstextcolor(color) -- Set text color index.
** 	Gerror gscharheight(height) -- Set character height.
** 	Gerror ug_dcharheight(p)
**		Gfloat ug_ndcchht(ht) -- convert char ht to ndc.
** 	Gerror gstxplane(txpvec) -- set text plane normal vector.
** 	Gerror gscharup3(p) -- set character up vector 3D.
** 	Gerror gscharup(up) -- Set char up vector 2D.
** 	Gerror gstextpath(path) -- Set character path.
** 	Gerror gstextalign(align) -- Set text alignment.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gatts.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:17
*********************************************************************/
#include "zsysdep.h"
#include <stdio.h>
#include "gtbl.h"
#include "ginq.h"
#include "gerror.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gviw.h"
#include "gtbldef.h"
#include "umath.h"
#include "gsegac.h"
#include "udebug.h"
#include "mrender.h"

static int current_lucency = 100;
static int current_material = 0;
/*temp yurong static unsigned char current_stipple[128]; */

char ug_alignnames[2][5][10]={	/* names of text align, for debugging */
				{"TH_NORMAL","TH_LEFT","TH_CENTRE","TH_RIGHT"," "},
				{"TV_NORMAL","TV_TOP","TV_HALF","TV_BASE","TV_BOTTOM"}
		  };
char ug_txpathnames[4][9]={		/* names of text path for debugging */
			"TP_RIGHT", "TP_LEFT", "TP_UP", "TP_DOWN"};
Gfloat ug_chhtscl[UG_MAXNTRAN];
Gfloat ug_chhtsclflag[UG_MAXNTRAN]={
			0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};	/* no char ht scales up to date */

typedef struct { Gint op; Gws id; Gchrht ht; } S_UG_charhgt;

/*********************************************************************
**    I_FUNCTION     :  ug_dfats() -- set prim attributes to default values.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_dfats()					/* set primitive attributes to default values */
{
	zbytecp(ug_gksstli.curprats, ug_gksdesctbl.defprats);
}

/*********************************************************************
**    E_FUNCTION     :  gipat3(ats) -- inquire primitive attributes 3D.
**    PARAMETERS   
**       INPUT  :
**       OUTPUT :		UG_prat3 *ats; -- put attributes here.  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gipat3(ats)								/* inquire primitive attributes 3 */
/*$ OUTPUT */
UG_prat3 *ats;
{ 
	extern int UU_debmask;
	uu_denter(UU_GTRC,(us,"gipat3(ats)"));

	zbytecp((*ats),ug_gksstli.curprats);

	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_lineindex(index) -- set line bundle index.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**		RETURNS:			none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_lineindex(index)
Gindex index;
{
	int prms[3];
	uu_denter(UU_GITRC,(us,"ug_lineindex(%d)",index));

	/* tell workstations of lineindex chg */
	prms[2]=index;
	prms[0]=UG_DLNINDEX; 
	ug_wkout(prms,3);
	ug_gksstli.curprats.lnindex=index;

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gslineindex(index) -- Set line bundle index.
**    PARAMETERS   
**       INPUT  :  Gindex index -- new line bundle index.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gslineindex (index)					/* set line bundle index */
/*$ INPUT */
Gindex	index;
{
	uu_denter(UU_GTRC,(us,"gslineindex(%d)",index));
	ug_lineindex(index);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nlindex(ug_segac(ug_gksstli.opnseg)->seglist,index);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    I_FUNCTION        :  ug_cp_linetype(typ1,typ2) copy line type struct.
**    PARAMETERS   
**       INPUT  :  Glntype *typ2 -- old line type.
**       OUTPUT :  Glntype *typ1 -- new line type.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ug_cp_linetype (typ1,typ2)
Glntype	*typ1,*typ2;
{
	int i,n;
	typ1->typeno = typ2->typeno;
	typ1->npatn = typ2->npatn;
	typ1->patnlen = typ2->patnlen;
	n = typ2->npatn;
	if (n > 100) n = 100;
	for (i = 0; i < n; i++)
		typ1->typepatn[i] = typ2->typepatn[i];
}

/*********************************************************************
**    I_FUNCTION        :  ug_linetype(typ) - Set workstation line type.
**    PARAMETERS   
**       INPUT  :  Glntype *typ -- new line type.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_linetype (typ) /* set line type (style) */
Glntype	*typ;
{
	int i,j;
	struct  {
		int op;
		int wsid;
		Glntype type;
	} prms;

	uu_denter(UU_GITRC,(us,"ug_linetype(%d)",(*typ).typeno));

	if( typ->typeno==21) {		/* user defined patn'n */
		uu_dprint(UU_GITRC,(us,"ug_linetype npatn=%d, patn len=%g, patn:",
				typ->npatn, typ->patnlen));

		for(i=0; i < typ->npatn; i++) {
			j = typ->typepatn[i];
			uu_dprint(UU_GITRC,(us,"%d",j));
		}
	}										/* end user defined pat'n */

	/* tell workstations of linetype chg
	zbytecp(prms.type,(*typ)); */
	ug_cp_linetype (&(prms.type),typ);

	prms.op=UG_DLINETYPE;
	ug_wkout(&prms,i=sizeof(prms)/sizeof(int));
/*
	zbytecp(ug_gksstli.curprats.lnbundl.type,(*typ));
*/
	ug_cp_linetype (&(ug_gksstli.curprats.lnbundl.type),typ);

	if( ug_gksos.sysstate == UG_SGOP )
		ug_nlstl(ug_segac(ug_gksstli.opnseg)->seglist,typ);

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION         :  Gerror gslinetype(typ)  -- Set line type.
**    PARAMETERS   
**       INPUT  :  Glntype *typ -- new line type.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gslinetype (typ)					/* set line type (style) */
/*$ INPUT */
Glntype	*typ;
{
	Gerror irtn;

	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK	
	irtn=ug_chkad(typ,"gslinetype");
	if (irtn==NCL_NO_ERROR) if ((*typ).typeno<1) {
		irtn=ELINELEZ; ug_errorhand(irtn,"gslinetype",NULL);
	}
#endif
	if (irtn==NCL_NO_ERROR) {
	 	uu_denter2(UU_GTRC,(us,"gslinetype(%d)",(*typ).typeno));
		ug_linetype(typ);
		uu_dexit;
	}
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION :  ug_linewidth(width) -- internal set linewidth.
**    
**    PARAMETERS   
**       INPUT  : 	 Gscale width -- new linewidth.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_linewidth(width)							/* set linewidth */
Gscale width;
{
	struct { Gint op; Gws id; Gscale wid; } prms;

	uu_denter(UU_GITRC,(us,"ug_linewidth(%g)",width));

	prms.wid=width;
	prms.op=UG_DLINEWIDTH;
	ug_wkout(&prms,sizeof(prms)/sizeof(int));
	ug_gksstli.curprats.lnbundl.width=width;

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION         :  Gerror gslinewidth(width) -- Set linewidth.
**    PARAMETERS   
**       INPUT  :  Gscale width -- new linewidth.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gslinewidth (width)					/* set linewidth */
/*$ INPUT */
Gscale	width;
{
	uu_denter(UU_GTRC,(us,"gslinewidth(%g)",width));
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nlwid (ug_segac(ug_gksstli.opnseg)->seglist,width);
	}
	else
		ug_linewidth(width);
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gsshdlucency(lucency) -- Set shade translucency
**    PARAMETERS   
**       INPUT  : int -- new shade translucency
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added translucency for shading
.....Yurong
*/
Gerror gsshdlucency(lucency) 
int lucency;
{
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gslinecolor(%d)",color));
	irtn=NCL_NO_ERROR;
	if ((lucency<0) || (lucency>100))
		lucency = 100 ;
	if ( ug_gksos.sysstate == UG_SGOP ) 
		{
			ug_nshdlucency (ug_segac(ug_gksstli.opnseg)->seglist,lucency);
		}
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gsshdstipple(mask) -- Set shade stipple
**    PARAMETERS   
**       INPUT  : int -- new shade stipple
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added stipple for shading
.....Yurong
*/
/*temp yurong Gerror gsshdstipple(mask)
unsigned char *mask;
{
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gslinecolor(%d)",color));
	irtn=NCL_NO_ERROR;
	if ( ug_gksos.sysstate == UG_SGOP ) 
		{
			ug_nshdstipple (ug_segac(ug_gksstli.opnseg)->seglist,mask);
		}
	uu_dexit;
	return(irtn);
}
*/
/*********************************************************************
**    E_FUNCTION   :  Gerror gsshdcolor(color) -- Set shade base color index.
**    PARAMETERS   
**       INPUT  : Gcolor color -- new shade base color index.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added color for shading
.....Yurong
*/
Gerror gsshdcolor (color)				
Gcolor	color;
{
	Gerror irtn;
	 uu_denter(UU_GTRC,(us,"gslinecolor(%d)",color));
	 irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	if (color<0){
		irtn=ECINDXLZ; ug_errorhand(irtn,"gslinecolor",&color);
	}
	if (color>32767) {
		irtn=EBADCOLX; ug_errorhand(irtn,"gslinecolor",&color);
	}
	if (irtn==NCL_NO_ERROR) {
#endif
		if ( ug_gksos.sysstate == UG_SGOP ) 
		{
			ug_nshdcolr (ug_segac(ug_gksstli.opnseg)->seglist,color);
		}
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION   :  ug_linecolor(color) -- Set workstation polyline color index.
**    PARAMETERS   
**       INPUT  : Gcolor color -- new line color index.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_linecolor (color)					/* set line color */
Gcolor	color;
{
	int prms[3];
	 uu_denter(UU_GITRC,(us,"ug_linecolor(%d)",color));
	/* tell workstations of linecolor chg */
	prms[0]=UG_DLNCINDEX;
	prms[2]=color;
	ug_wkout(prms,3);
	ug_gksstli.curprats.lnbundl.color=color;	/* save the new line color*/
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gslinecolor(color) -- Set polyline color index.
**    PARAMETERS   
**       INPUT  : Gcolor color -- new line color index.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gslinecolor (color)					/* set line color */
/*$ INPUT */
Gcolor	color;
{
	Gerror irtn;
	 uu_denter(UU_GTRC,(us,"gslinecolor(%d)",color));
	 irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	if (color<0){
		irtn=ECINDXLZ; ug_errorhand(irtn,"gslinecolor",&color);
	}
	if (color>32767) {
		irtn=EBADCOLX; ug_errorhand(irtn,"gslinecolor",&color);
	}
	if (irtn==NCL_NO_ERROR) {
#endif
		ug_linecolor(color);
		if ( ug_gksos.sysstate == UG_SGOP ) {
			ug_nlncolr (ug_segac(ug_gksstli.opnseg)->seglist,color);
		}
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION   :  ug_markindex(index) -- Set marker bundle index.
**    PARAMETERS   
**       INPUT  :  Gindex index -- new marker bundle index.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_markindex (index)					/* set marker bundle index */
/*$ INPUT */
Gindex	index;
{
	int prms[3];
	uu_denter(UU_GITRC,(us,"ug_markindex(%d)",index));

	/* tell workstations of markindex chg */
	prms[2]=index;
	prms[0]=UG_DMKINDEX;
	ug_wkout(prms,3);
	ug_gksstli.curprats.mkindex=index;

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gsmarkindex(index) -- Set marker bundle index.
**    PARAMETERS   
**       INPUT  :  Gindex index -- new marker bundle index.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gsmarkindex (index)					/* set marker bundle index */
/*$ INPUT */
Gindex	index;
{
	 uu_denter(UU_GTRC,(us,"gsmarkindex(%d)",index));
	 ug_markindex(index);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nmkindex(ug_segac(ug_gksstli.opnseg)->seglist,index);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    I_FUNCTION   :  ug_marktype(type) -- Set marker type (style).
**    PARAMETERS   
**       INPUT  : Gmktype type -- new marker type.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_marktype (type)					/* set mark type (style) */
Gmktype	type;
{
	int prms[3];

	uu_denter(UU_GTRC,(us,"ug_marktype(%d)",type));

	/* tell workstations of marktype chg */
	prms[2]=type;
	prms[0]=UG_DMKTYPE;
	ug_wkout(prms,3);
	ug_gksstli.curprats.mkbundl.type=type;

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gsmarktype(type) -- Set marker type (style).
**    PARAMETERS   
**       INPUT  : Gmktype type -- new marker type.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gsmarktype (type)					/* set mark type (style) */
/*$ INPUT */
Gmktype	type;
{
	uu_denter(UU_GTRC,(us,"gsmarktype(%d)",type));
	ug_marktype(type);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nmksy(ug_segac(ug_gksstli.opnseg)->seglist,type);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    I_FUNCTION   :  gsmarksize(size) -- Set marker size.
**    PARAMETERS   
**       INPUT  : Gscale size -- new marker size.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_marksize (size)					/* set marker size */
Gscale	size;
{
	struct { Gint op; Gws id; Gscale size; } prms;

	uu_denter(UU_GITRC,(us,"ug_marksize(%g)",size));
	
	/* tell workstations of marksize chg */
	prms.size=size;
	prms.op=UG_DMKSIZE;
	ug_wkout ( &prms, sizeof(prms)/sizeof(int) );
	ug_gksstli.curprats.mkbundl.size=size;

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gsmarksize(size) -- Set marker size.
**    PARAMETERS   
**       INPUT  : Gscale size -- new marker size.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gsmarksize (size)					/* set marker size */
/*$ INPUT */
Gscale	size;
{
/* struct { Gint op; Gws id; Gscale size; } prms; */
	uu_denter(UU_GTRC,(us,"gsmarksize(%g)",size));
	ug_marksize(size);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nmksiz (ug_segac(ug_gksstli.opnseg)->seglist,size);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    I_FUNCTION   :  ug_markcolor(color) -- tell ws of marker color index.
**    PARAMETERS   
**       INPUT  : Gcolor color -- new marker color index.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_markcolor (color)					/* set marker color */
Gcolor	color;
{
	int prms[3];

	uu_denter(UU_GITRC,(us,"ug_markcolor(%d)",color));

	/* tell workstations of markcolor chg */
	prms[2]=color;
	prms[0]=UG_DMKCINDEX;
	ug_wkout(prms,3);
	ug_gksstli.curprats.mkbundl.color=color;

	uu_dexit;
	return;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gsmarkcolor(color) -- Set marker color index.
**    PARAMETERS   
**       INPUT  : Gcolor color -- new marker color index.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gsmarkcolor (color)					/* set marker color */
/*$ INPUT */
Gcolor	color;
{
	uu_denter(UU_GTRC,(us,"gsmarkcolor(%d)",color));
	ug_markcolor(color);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nmkcolr(ug_segac(ug_gksstli.opnseg)->seglist,color);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    I_FUNCTION :  ug_textindex(index) -- set text index.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_textindex(index)
Gindex index;
{
	int prms[4];

	uu_denter(UU_GITRC,(us,"ug_textindex(%d)",index));

	/* tell workstations of textindex chg */
	prms[2]=index;
	prms[0]=UG_DTEXTINDEX;
	ug_wkout(prms,3);
	ug_gksstli.curprats.txindex=index;
	
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gstextindex(index) -- Set text bundle index.
**    PARAMETERS   
**       INPUT  :  Gindex index -- new text bundle index.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gstextindex (index)					/* set texter bundle index */
/*$ INPUT */
Gindex	index;
{
	uu_denter(UU_GTRC,(us,"gstextindex(%d)",index));
	 ug_textindex(index);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_ntxindex (ug_segac(ug_gksstli.opnseg)->seglist,index);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    I_FUNCTION :  ug_textfp(fp) -- tell ws of text font/precision.
**    PARAMETERS   
**       INPUT  : 	Gtxfp *fp -- text font/precision.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_textfp(fp)						/* set text font/precision */
Gtxfp *fp;
{
	int prms[4];

	uu_denter(UU_GITRC,(us,"ug_textfp(%d,%d)",fp->font,fp->prec));
		
	ug_gksstli.curprats.txbundl.fp.font=fp->font;
	ug_gksstli.curprats.txbundl.fp.prec=fp->prec;
	/* tell workstations of textfont chg */
	prms[2]=(int)fp->font;
	prms[3]=(int)fp->prec;
	prms[0]=UG_DTEXTFP;
	ug_wkout(prms,4);
	
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gstextfp(fp) -- Set text font and precision.
**    PARAMETERS   
**       INPUT  :  Gtxfp *fp -- new text font and precision.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gstextfp (fp)							/* set text font and precision */
/*$ INPUT */
Gtxfp	*fp;
{
	uu_denter(UU_GTRC,(us,"gstextfp(%d,%d)",fp->font,fp->prec));
	ug_textfp(fp);
	if ( ug_gksos.sysstate == UG_SGOP ) 
		ug_nfont (ug_segac(ug_gksstli.opnseg)->seglist,fp);
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    I_FUNCTION :  ug_charexp(expn) -- set char expansion factor.
**    PARAMETERS   
**       INPUT  : 	Gchrexp	expn;
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_charexp(expn)							/* set char expansion factor */
Gchrexp expn;
{
	struct { Gint op; Gws id; Gchrexp xp; } prms;

	uu_denter(UU_GITRC,(us,"ug_charexp(%g)",expn));

	/* tell workstations of new char expansion factor */
	prms.xp=expn;
	prms.op=UG_DCHAREXP;
	ug_wkout(&prms,sizeof(prms)/sizeof(int));
	ug_gksstli.curprats.txbundl.expn=expn;
	
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gscharexp(expn) -- Set character expansion factor.
**    PARAMETERS   
**       INPUT  : Gchrexp expn -- new character expansion factor.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gscharexp(expn)						/* set character expansion factor */
/*$ INPUT */
Gchrexp	expn;
{
	uu_denter(UU_GTRC,(us,"gscharexp(%g)",expn));
	ug_charexp(expn);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nchexp(ug_segac(ug_gksstli.opnseg)->seglist,expn);
	}
	uu_dexit;
	return (0);
}

/*********************************************************************
**    I_FUNCTION   :  ug_charspace(spacing) -- Set character spacing.
**    PARAMETERS   
**       INPUT  : Gchrsp spacing -- new character spacing.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_charspace (spacing)			/* set char spacing */
Gchrsp	spacing;
{
	struct { Gint op; Gws id; Gchrsp sp; } prms;

	uu_denter(UU_GITRC,(us,"ug_charspace(%g)",spacing));

	/* tell workstations of char spacing chg */
	prms.sp=spacing;
	prms.op=UG_DCHARSPACE;
	ug_wkout(&prms,sizeof(prms)/sizeof(int));
	ug_gksstli.curprats.txbundl.space=spacing;

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gscharspace(spacing) -- Set character spacing.
**    PARAMETERS   
**       INPUT  : Gchrsp spacing -- new character spacing.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gscharspace (spacing)			/* set char spacing */
/*$ INPUT */
Gchrsp	spacing;
{
	uu_denter(UU_GTRC,(us,"gscharspace(%g)",spacing));
	ug_charspace(spacing);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nchsp(ug_segac(ug_gksstli.opnseg)->seglist,spacing);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    I_FUNCTION   :  ug_textcolor(color) - Set work station text color.
**    PARAMETERS   
**       INPUT  :  Gcolor color -- new text color.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_textcolor (color)					/* set text color */
Gcolor	color;
{
	int prms[3];
	
	uu_denter(UU_GITRC,(us,"ug_textcolor(%d)",color));

	/* tell workstations of textcolor chg */
	prms[2]=color;
	prms[0]=UG_DTXCINDEX;
	ug_wkout(prms,3);
	ug_gksstli.curprats.txbundl.color=color;

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gstextcolor(color) -- Set text color index.
**    PARAMETERS   
**       INPUT  :  Gcolor color -- new text color.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gstextcolor (color)					/* set text color */
/*$ INPUT */
Gcolor	color;
{
	int irtn;
	 uu_denter(UU_GTRC,(us,"gstextcolor(%d)",color));
	 irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	if ((color<0)||(color>10000)) {
		ug_errorhand(EBADTXTX,"gstextcolor",&color);
		irtn=EBADTXTX;
	}
	if (irtn==NCL_NO_ERROR) {
#endif

	/* tell workstations of textcolor chg */
	ug_textcolor(color);

	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_ntxcolr (ug_segac(ug_gksstli.opnseg)->seglist,color);
	}
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  ug_charheight(height) -- set char height.
**    PARAMETERS   
**       INPUT  : 	Gchrht	height -- char height.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_charheight(height)
/*$ INPUT */
Gchrht height;
{
	struct { Gint op; Gws id; Gchrht ht; } prms;
	uu_denter(UU_GITRC,(us,"ug_charheight(%g)",height));

	/* tell workstations of new char height */
	prms.ht=height;
	prms.op=UG_DCHARHT;
	ug_wkout ( &prms, sizeof(prms)/sizeof(int) );
	ug_gksstli.curprats.txht=height;

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gscharheight(height) -- Set character height.
**    PARAMETERS   
**       INPUT  : Gchrht height -- new character height.
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gscharheight (height)						/* set character height */
/*$ INPUT */
Gchrht	height;
{
	uu_denter2(UU_GTRC,(us,"gscharheight(%g)",height));
	ug_charheight(height);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nchhgt(ug_segac(ug_gksstli.opnseg)->seglist,height);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  S_FUNCTION:  Gerror ug_dcharheight(p)
**      Workstation simulation routine to set charheight. 
**			Calls workstation's GDCHHT entry after translating the world
**			coordinate system charheight into NDC height.
**  PARAMETERS   
**      INPUT:  struct { Gint op; Gws id; Gchrht ht; } *p;
**      OUTPUT: none
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror ug_dcharheight(p)
S_UG_charhgt *p;
{
	Gfloat ug_ndcchht();
	struct { Gint op; Gws id; Gfloat ndc_ht; } prms;
	int reply[3];
	uu_denter(UU_GITRC,(us,"ug_dcharheight(%g)",(*p).ht));
	/* calc ndc char height by multiplying world height by pre calculated
		scale factor for the current normtran. */
	prms.ndc_ht=ug_ndcchht((*p).ht,ug_gksstli.curprats.txbundl.fp.prec);
	prms.id=(*p).id;
	prms.op=UG_DCHHTNDC;
	ug_wkcal((*p).id,&prms,reply);
	uu_dexit;
	return (0);
}

/*********************************************************************
**    I_FUNCTION :  ug_charhtscl(xform) - calc charheight scale factor
**														for a normtran.
**    PARAMETERS   
**       INPUT  : 	int xform -- which normtran.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_charhtscl(xform)
int xform;
{
	Gfloat zero[3],unitup[3],ndcup[3];
	Gfloat ndcscl,uvlen;

	/* unitize char up vector */
	uvlen=sqrt(ug_gksstli.curprats.txuv.x*ug_gksstli.curprats.txuv.x
				+ ug_gksstli.curprats.txuv.y*ug_gksstli.curprats.txuv.y
				+ ug_gksstli.curprats.txuv.z*ug_gksstli.curprats.txuv.z);
	unitup[0]=ug_gksstli.curprats.txuv.x/uvlen;
	unitup[1]=ug_gksstli.curprats.txuv.y/uvlen;
	unitup[2]=ug_gksstli.curprats.txuv.z/uvlen;
	/* convert unitized char up vec to ndc */
	/* calculate origin in NDC */
	ug_xform((UU_REAL) 0.,(UU_REAL) 0.,(UU_REAL) 0.,zero,ug_cxform[xform]);		
	ug_xform(unitup[0],unitup[1],unitup[2],ndcup,ug_cxform[xform]);
	ndcup[0]=ndcup[0]-zero[0];
	ndcup[1]=ndcup[1]-zero[1];
	ndcup[2]=ndcup[2]-zero[2];
	/* char ht scale factor is length of ndc char up vector */
	ndcscl=ndcup[0]*ndcup[0]+ndcup[1]*ndcup[1]+ndcup[2]*ndcup[2];
	ug_chhtscl[xform]=sqrt(ndcscl);
	ug_chhtsclflag[xform]=1;			/* remember this scale factor up to date*/
	uu_denter2(UU_GITRC,(us,"ug_charhtscl(%d) new scale=%g",
		xform,ug_chhtscl[xform]));
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION :  Gfloat ug_ndcchht(ht,prec) -- convert char ht to NDC.
**    PARAMETERS   
**       INPUT  : 		Gfloat ht -- wc char height
**								Gtxprec prec -- text precision.
**       OUTPUT :  
**    RETURNS      : ndc char ht.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gfloat ug_ndcchht(ht,prec)
/*$ INPUT */
Gfloat ht;
Gtxprec prec;
{
		Gfloat ndcht;
		int n;

		n=ug_gksstli.curvwindex;
		if (prec==UG_STROKE) {
			/* first calculate char height scale for this normtran if needed */
			if (ug_chhtsclflag[n]==0) ug_charhtscl(n);	
			ndcht=ug_chhtscl[n]*ht;
		}									/* end if prec==UG_STROKE */
		else {							/* not stroke precision */
			/* just use ratio of window to viewport height and modelling 
				xform's  y scale factor. */
/*			ndcscl=((ug_gksstli.vtran[n].vport.urb.y-
					ug_gksstli.vtran[n].vport.llf.y) /
					(ug_gksstli.vtran[n].window.urb.y -
					ug_gksstli.vtran[n].window.llf.y))
					* ug_modxform[1][1]; */
/* NCL changed so the character sizes would remain reasonable on VMS systems. kathy */
			/* ndcht=ndcscl*ht; */
			ndcht=0.0109;
/* end NCL */
		}
		uu_denter2(UU_GTRC,(us,"%g=ug_ndcchht(%g). vwindex=%d txuv=%g %g %g",
			ndcht,ht,ug_gksstli.curvwindex,ug_gksstli.curprats.txuv.x,
			ug_gksstli.curprats.txuv.y,ug_gksstli.curprats.txuv.z));
		uu_dexit;
		return(ndcht);
}

/*********************************************************************
**    I_FUNCTION :  ug_txplane(txpvc) -- set text plane vector.
**    PARAMETERS   
**       INPUT  : 	Gwpoint3 *txpvc -- text plane normal vector.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_txplane(txpvc)
Gwpoint3 *txpvc;
{
	struct {Gint op; Gws id; Gwpoint3 p;} prms;

	uu_denter(UU_GITRC,(us,"ug_txplane(%g,%g,%g)",
		(*txpvc).x,(*txpvc).y,(*txpvc).z));
	
	/* tell workstation of new charplane vector */
	prms.op = UG_DTXPLANE;
	zbytecp (prms.p, *txpvc);
	ug_wkout ( &prms, sizeof(prms)/sizeof(int) );
	zbytecp(ug_gksstli.curprats.txpvec, *txpvc);
		
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gstxplane(txpvec) -- set text plane normal vector.
**       All subsequent text will be drawn on the specified plane.
**    PARAMETERS   
**       INPUT  : Gwpoint3 *txpvec -- char plane vector (world coordinates)
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gstxplane(txpvc)							/* set charplane vector */
/*$ INPUT */
Gwpoint3 *txpvc;
{
	uu_denter(UU_GTRC,(us,"gstxplane(%g,%g,%g)",
		(*txpvc).x,(*txpvc).y,(*txpvc).z));
	ug_txplane(txpvc);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nchvec (ug_segac(ug_gksstli.opnseg)->seglist, txpvc);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    I_FUNCTION :  ug_charup3(upvec) -- set char up vector 3D.
**    PARAMETERS   
**       INPUT  :    Gwpoint3 *upvec -- char up vector.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_charup3(upvec)							/* set char up vector 3D */
Gwpoint3 *upvec;
{
	struct {Gint op; Gws id; Gwpoint3 p;} prms;
	int i;
	uu_denter(UU_GITRC,(us,"ug_chup3(%g,%g,%g)",
		(*upvec).x,(*upvec).y,(*upvec).z));
	
	/* remember char ht scales not up to date */
	for (i=0; i<UG_MAXNTRAN; i++) ug_chhtsclflag[i]=0;	
	/* tell workstation of new char up vector */
	prms.op = UG_DCHARUP3;
	zbytecp (prms.p, *upvec);
	ug_wkout ( &prms, sizeof(prms)/sizeof(int) );
	zbytecp( ug_gksstli.curprats.txuv, *upvec);
		
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION	  :  Gerror gscharup3(p) -- set character up vector 3D.
**  PARAMETERS   
**      INPUT:  Gwpoint3 *p -- 3D character up vector.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gscharup3(upvec)							/* set charup 3d */
/*$ INPUT */
Gwpoint3 *upvec;
{
	uu_denter(UU_GTRC,(us,"gschup3(%g,%g,%g)",(*upvec).x,(*upvec).y,(*upvec).z));
	ug_charup3(upvec);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nchup3(ug_segac(ug_gksstli.opnseg)->seglist,upvec);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    I_FUNCTION :  ug_charup(up) -- set char up vector 2D.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_charup(up)							/* set char up vector 2D. */
Gwpoint *up;
{
	struct {Gint op; Gws id; Gwpoint p;} prms;
	int i;
	uu_denter(UU_GITRC,(us,"ug_charup(%g,%g)",up->x,up->y));
		
	/* remember char ht scales not up to date */
	for (i=0; i<UG_MAXNTRAN; i++) ug_chhtsclflag[i]=0;	
	/* tell workstation of new char up vector */
	prms.op = UG_DCHARUP;
	zbytecp ( prms.p, *up);
	ug_wkout ( &prms, sizeof(prms)/sizeof(int) );
	ug_gksstli.curprats.txuv.x = (*up).x;
	ug_gksstli.curprats.txuv.y = (*up).y;
	ug_gksstli.curprats.txuv.z = 0.0;

	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION	  :  Gerror gscharup(up) -- Set char up vector 2D.
**  PARAMETERS   
**      INPUT:  Gwpoint *up -- 2D character up vector.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gscharup (up)							/* set char up vector */
/*$ INPUT */
Gwpoint	*up;
{
	uu_denter(UU_GTRC,(us,"gscharup2(%g,%g)",up->x,up->y));
	ug_charup(up);
	if (ug_gksos.sysstate == UG_SGOP) 
	{
		ug_nchup(ug_segac(ug_gksstli.opnseg)->seglist,&ug_gksstli.curprats.txuv);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  I_FUNCTION	  :  ug_textpath(path) -- Set character path.
**  PARAMETERS   
**      INPUT:  Gtxpath path -- new text path, which may have values
**					 TP_RIGHT, TP_LEFT, TP_UP, TP_DOWN.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_textpath (path)						/* set char path */
Gtxpath	path;
{
	int prms[3];

	uu_denter(UU_GTRC,(us,"ug_textpath(%s)",
			&ug_txpathnames[(int)path][0]));
	
	/* tell workstations of textpath chg */
	prms[2]=(int)path;
	prms[0]=UG_DTEXTPATH;
	ug_wkout(prms,3);
	ug_gksstli.curprats.txpath=path;
	
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION	  :  Gerror gstextpath(path) -- Set character path.
**  PARAMETERS   
**      INPUT:  Gtxpath path -- new text path, which may have values
**					 TP_RIGHT, TP_LEFT, TP_UP, TP_DOWN.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gstextpath (path)						/* set char path */
/*$ INPUT */
Gtxpath	path;
{
	uu_denter(UU_GTRC,(us,"gstextpath(%s)",
			&ug_txpathnames[(int)path][0]));
	ug_textpath(path);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_ntxpath (ug_segac(ug_gksstli.opnseg)->seglist,path);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  I_FUNCTION:  ug_textalign(align) -- Set text alignment.
**  PARAMETERS   
**      INPUT:   Gtxalign *align -- new text alignment, defined as:
**							typefef struct {
**									Gtxhor hor;
**									Gtxver ver;
**							} Gtxalign;
**					  Gtxhor may have values TH_NORMAL, TH_LEFT, TH_CENTRE, TH_RIGHT.
**					  Gtxver may have values TV_NORMAL, TV_TOP, TV_HALF, TV_BASE,
**								and TV_BOTTOM.
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
void ug_textalign (align)						/* set text alignment */
Gtxalign	*align;
{
	int prms[4];
	uu_denter(UU_GITRC,(us,"ug_textalign(%s,%s)",
			&ug_alignnames[0][(int)align->hor][0],
			&ug_alignnames[1][(int)align->ver][0]));

	/* tell workstations of textalignment chg */
	prms[2]=(int)align->hor;
	prms[3]=(int)align->ver;
	prms[0]=UG_DTEXTALIGN;
	ug_wkout(prms,4);
	ug_gksstli.curprats.txalign.hor = align->hor;
	ug_gksstli.curprats.txalign.ver = align->ver;
	
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gstextalign(align) -- Set text alignment.
**  PARAMETERS   
**      INPUT:   Gtxalign *align -- new text alignment, defined as:
**							typefef struct {
**									Gtxhor hor;
**									Gtxver ver;
**							} Gtxalign;
**					  Gtxhor may have values TH_NORMAL, TH_LEFT, TH_CENTRE, TH_RIGHT.
**					  Gtxver may have values TV_NORMAL, TV_TOP, TV_HALF, TV_BASE,
**								and TV_BOTTOM.
**
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**						  ENOTGWWS if the operating state was not UG_GKOP, UG_WSOP, UG_WASC,
**								or UG_SGOP.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gstextalign (align)						/* set text alignment */
/*$ INPUT */
Gtxalign	*align;
{
	uu_denter(UU_GTRC,(us,"gstextalign(%s,%s)",
			&ug_alignnames[0][(int)align->hor][0],
			&ug_alignnames[1][(int)align->ver][0]));
	ug_textalign(align);
	if (ug_gksos.sysstate==UG_SGOP) 
	{
		ug_nchj(ug_segac(ug_gksstli.opnseg)->seglist,align);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}
/******************************************************************
**    FUNCTION     : ug_set_lucency(lucency) 
**				save current lucency
**    PARAMETERS
**       INPUT  :
**          lucency: lucency to save to currect lucency.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_set_lucency(lucency)
int lucency;
{
	current_lucency = lucency;
}

/******************************************************************
**    FUNCTION     : ug_get_lucency() 
**				get current lucency
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_get_lucency()
{
	return current_lucency;
}

/******************************************************************
**    FUNCTION     : ug_set_stipple(stipple) 
**				save current stipple
**    PARAMETERS
**       INPUT  :
**          stipple: stripple to save.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*temp yurong void ug_set_stipple(stipple)
unsigned char *stipple;
{
	int i;
	for (i=0; i<128; i++)
		current_stipple[i] = stipple[i];
}
*/
/******************************************************************
**    FUNCTION     : ug_get_stipple(stripple) 
**				get current stipple
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*temp yurong void ug_get_stipple(stipple)
unsigned char *stipple;
{
	int i;
	for (i=0; i<128; i++)
		stipple[i] = current_stipple[i];
}
*/
/*********************************************************************
**    E_FUNCTION     :  gspat3(ats) -- set primitive attributes 3D.
**    PARAMETERS   
**       INPUT  : 	UG_prat3 *ats -- attributes to be set.
**       OUTPUT :  	none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gspat3(ats)						/* set primitive attributes 3 */
/*$ INPUT */
UG_prat3 *ats;
{
	uu_denter(UU_GTRC,(us,"gspat3(ats)"));

	ug_lineindex (ats->lnindex);
	ug_linetype (&ats->lnbundl.type);
	ug_linewidth (ats->lnbundl.width);
	ug_linecolor (ats->lnbundl.color);
	ug_markindex (ats->mkindex);
	ug_marktype (ats->mkbundl.type);
	ug_marksize (ats->mkbundl.size);
	ug_markcolor (ats->mkbundl.color);
	ug_textindex (ats->txindex);
	ug_textfp (&ats->txbundl.fp);
	ug_charexp (ats->txbundl.expn);
	ug_charspace (ats->txbundl.space);
	ug_textcolor (ats->txbundl.color);
	ug_charheight (ats->txht);
	ug_charup3 (&ats->txuv);
	ug_textpath (ats->txpath);
	ug_textalign (&ats->txalign);
	ug_fillindex (ats->flindex);
	ug_fillintstyle (ats->flbundl.inter);
	ug_fillstyleindex (ats->flbundl.style);
	ug_fillcolor (ats->flbundl.color);
	ug_patsize (&ats->patsize);
	ug_patrefpoint (&ats->patrefpt);
	ug_asfs (&ats->asfs);
	ug_dspickid (ats->pickid);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION   :  Gerror gsmaterial(material) -- Set material
**    PARAMETERS   
**       INPUT  : int -- new material
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gsmaterial(material) 
int material;
{
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gsmaterial(%d)",material));
	irtn=NCL_NO_ERROR;
	if ((material<0) || (material>NMATRLDEFAULTS))
		material = 0;
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nmaterial (ug_segac(ug_gksstli.opnseg)->seglist, material);
	}
	uu_dexit;
	return(irtn);
}
/******************************************************************
**    FUNCTION     : ug_set_material(material) 
**				save current material
**    PARAMETERS
**       INPUT  :
**          material: material to save to currect material.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_set_material(material)
int material;
{
	current_material = material;
}

/******************************************************************
**    FUNCTION     : ug_get_material() 
**				get current material
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_get_material()
{
	return current_material;
}
