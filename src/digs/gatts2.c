/*********************************************************************
**    NAME         :  gksatts2.c -- DIGS attribute fctns (cont).
**       CONTAINS:
**		Gerror gsfillindex(index) --  Set fill area index.
**		Gerror gsfillintstyle(style) -- Set fill area interior style.
**		Gerror gsfillstyleindex(style) --  Set fill area style index.
**		Gerror gsedgeflag(f) -- set (fill area) edge flag.
**		Gerror gsfillcolor(color) --  Set fill area color index.
**		int ug_fillcolor(color) -- set fill area color.
**		Gerror gspatsize(size) --  Set pattern size.
**		Gerror gspatrefpoint(point) --  Set pattern reference point.
**		Gerror gsasfs(asfs). --  Set aspect source flags.
**		Gerror gspickid(id) -- Set the current pick identifier.
**		ug_dspickid(id) -- inform workstation of pickid change.
**		Gerror gslinerep(ws,index,rep) -- Set polyline representation. 
**		Gerror gsmarkrep(ws,index,rep)--  Set polymarker rep.
**		Gerror gstextrep(ws,index,rep) -- Set text representation.
**		Gerror gsfillrep(ws,index,rep) --  Set fill area rep.
**		Gerror gspatrep(ws,index,rep) -- Set pattern representation.
**		Gerror gscolorrep(ws,index,rep)-- Set color representation.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
** 
**    MODULE NAME AND RELEASE LEVEL
**       gatts2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:17
*********************************************************************/
#include "zsysdep.h"
#include <stdio.h>
#include "gtbl.h"
#include "gerror.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gviw.h"
#include <math.h>
#include "gsegac.h"
#include "udebug.h"
/*
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gatts2.c 3.2 8/27/87 17:31:16 single"};
#else
static char uu_sccsident[]={"@(#) gatts2.c 3.2 8/27/87 17:31:16 double"};
#endif
*/

/********************************************************************* 
**  E_FUNCTION:  Gerror gsfillindex(index) --  Set fill area index.
**  PARAMETERS   
**      INPUT:  	Gindex index;
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsfillindex (index)
/*$ INPUT */
Gindex	index;
{
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gsfillindex(%d)",index));
	ug_fillindex(index);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nfaindex (ug_segac(ug_gksstli.opnseg)->seglist,index);
	}
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  I_FUNCTION:  ug_fillindex(index) --  Set fill area index.
**  PARAMETERS   
**      INPUT:  Gindex index.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror ug_fillindex (index)
Gindex	index;
{
	Gerror irtn;
	int prms[3];
	uu_denter(UU_GITRC,(us,"ug_fillindex(%d)",index));
	if (ug_gksstli.curprats.flindex!=index)
	{
		/* tell workstations of fill index chg */
		prms[2]=index;
		prms[0]=UG_DFAINDEX;
		ug_wkout(prms,3);
		ug_gksstli.curprats.flindex=index;
	}
	uu_dexit;
	irtn=NCL_NO_ERROR;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsfillintstyle(style) -- Set fill area interior style.
**							Not implemented yet.
**  PARAMETERS   
**      INPUT:  Gflinter	interior;
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsfillintstyle (interior)
/*$ INPUT */
Gflinter	interior;
{
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gsfillintstyle(%d)",interior));
	ug_fillintstyle(interior);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		/*generate fill area interior style command */
	}
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  I_FUNCTION: ug_fillintstyle(interior) -- Set fill area interior style.
**  PARAMETERS   
**      INPUT:  Gflinter interior.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror ug_fillintstyle (interior)
Gflinter	interior;
{
	Gerror irtn;
	int prms[3];
	uu_denter(UU_GTRC,(us,"ug_fillintstyle(%d)",interior));
	if (ug_gksstli.curprats.flbundl.inter!=interior)
	{
		/* tell workstations of fill style chg */
		prms[2]=(int)interior;
		prms[0]=UG_DFAINTSTYLE;
		ug_wkout(prms,3);
		ug_gksstli.curprats.flbundl.inter=interior;
	}
	uu_dexit;
	irtn=NCL_NO_ERROR;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsfillstyleindex(style) --  Set fill area style index.
**						Not implemented yet.
**  PARAMETERS   
**      INPUT:  Gflstyle	style;
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsfillstyleindex (style)
/*$ INPUT */
Gflstyle	style;
{
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gsfillstyleindex(%d)",style));
	ug_fillstyleindex(style);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		/*generate fill area style command */
	}
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  I_FUNCTION:  ug_fillstyleindex(style) --  Set fill area style index.
**  PARAMETERS   
**      INPUT:  none 
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror ug_fillstyleindex (style)
Gflstyle	style;
{
	Gerror irtn;
	int prms[3];
	uu_denter(UU_GTRC,(us,"ug_fillstyleindex(%d)",style));
	if (ug_gksstli.curprats.flbundl.style!=style)
	{
		/* tell workstations of fill style index chg */
		prms[2]=style;
		prms[0]=UG_DFASTYLEINDEX;
		ug_wkout(prms,3);
		ug_gksstli.curprats.flbundl.style=style;
	}
	uu_dexit;
	irtn=NCL_NO_ERROR;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  Gerror gsedgeflag(f) -- set edge flag.
**    PARAMETERS   
**       INPUT  :  Gtoggle f; -- OFF or ON.
**       OUTPUT :  
**    RETURNS      : NCL_NO_ERROR if all went OK.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror gsedgeflag(f)						/* set edge flag */
/*$ INPUT */
Gtoggle f;									/* OFF or ON */
{
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gsedgeflag(%d)",f));
	ug_sedgeflag(f);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		/*generate edge flag command */
		ug_nedgeflag(ug_segac(ug_gksstli.opnseg)->seglist,f);			
	}
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION :  ug_sedgeflag(f) --	set edge flag 
**    PARAMETERS   
**       INPUT  : 	Gtoggle f; -- edgeflag, OFF or ON.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gerror ug_sedgeflag(f)
/*$ INPUT */
Gtoggle f;
{
	int prms[4];
	uu_denter(UU_GTRC,(us,"ug_sedgeflag(%d)",f));
	ug_gksstli.curprats.flbundl.edgeflag=f;
	/* tell workstations of fill style index chg */
	prms[2]=(int)f;
	prms[0]=UG_DEDGEFLAG;
	ug_wkout(prms,3);
	return(NCL_NO_ERROR);
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsfillcolor(color) --  Set fill area color index.
**  PARAMETERS   
**      INPUT:  Gcolor	color;
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsfillcolor (color)
/*$ INPUT */
Gcolor	color;
{
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gsfillcolor(%d)",color));
	ug_fillcolor(color);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		ug_nfacolr(ug_segac(ug_gksstli.opnseg)->seglist,color);
	}
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_fillcolor(color) -- set fill area color.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_fillcolor(color)
Gcolor color;
{
	int prms[3];

	uu_denter(UU_GITRC,(us,"ug_fillcolor(%d)",color));

	/* tell workstations of fill color chg */
	prms[2]=color;
	prms[0]=UG_DFACINDEX;
	ug_wkout(prms,3);
	ug_gksstli.curprats.flbundl.color=color;

	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsfillcolorrep(color) --  
**		Set fill area color representation for rendering.
**  PARAMETERS   
**      INPUT:  Gcobundl color 
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsfillcolorrep(color)
/*$ INPUT */
Gcobundl	*color;
{
	Gerror irtn;

	uu_denter(UU_GTRC,(us,"gsfillcolorrep(%f %f %f)",
		color->red, color->green, color->blue));

	ug_fillcolorrep(color);
	if ( ug_gksos.sysstate == UG_SGOP ) {
		ug_nfacolrrep(ug_gksstli.opnseg,color);
	}
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_fillcolorrep(color) -- 
**		Set fill area color representation.
**    PARAMETERS   
**       INPUT  : 
**          Gcobunl *color
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_fillcolorrep(color)
Gcobundl *color;
{
	uu_denter(UU_GITRC,(us,"ug_fillcolorrep(%f %f %f)",
		color->red, color->green, color->blue));

	ug_gksstli.curprats.flbundl.colorrep.red   = color->red;
	ug_gksstli.curprats.flbundl.colorrep.green = color->green;
	ug_gksstli.curprats.flbundl.colorrep.blue  = color->blue;

	return(NCL_NO_ERROR);
	uu_dexit;
}

 /********************************************************************* 
**  E_FUNCTION:  Gerror gspatsize(size) --  Set pattern size.
**						Not implemented yet.
**  PARAMETERS   
**      INPUT:  Gwpoint	*size;
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gspatsize (size)
/*$ INPUT */
Gwpoint	*size;
{
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gspatsize(%g,%g)",(*size).x,(*size).y));
	ug_patsize(size);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		/* generate pattern size command */
	}
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  I_FUNCTION:  ug_patsize(size) --  Set pattern size.
**  PARAMETERS   
**      INPUT:  Gwpoint  *size.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
ug_patsize(size)
Gwpoint	*size;
{
	struct { Gint op; Gws id; Gwpoint sz;} prms;
	int prmsiz;
	uu_denter(UU_GTRC,(us,"ug_patsize(%g,%g)",(*size).x,(*size).y));
	if (	(ug_gksstli.curprats.patsize.x != (*size).x) ||
			(ug_gksstli.curprats.patsize.y != (*size).y)   )
	{
		/* tell workstations of pattern size chg */
		prms.sz.x=(*size).x;
		prms.sz.y=(*size).y;
		prms.op=UG_DPATSIZE;
		ug_wkout ( &prms, prmsiz=sizeof(prms)/sizeof(int) );
		zbytecp ( ug_gksstli.curprats.patsize, *size );
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gspatrefpoint(point) --  Set pattern reference point.
**  PARAMETERS   
**      INPUT:  none 
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gspatrefpoint (point)
/*$ INPUT */
Gwpoint	*point;
{
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gspatrefpoint(%g,%g)",(*point).x,(*point).y));
	ug_patrefpoint(point);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		/* generate pat ref pt command */
	}
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  I_FUNCTION:  ug_patrefpoint(point) --  Set pattern reference point.
**  PARAMETERS   
**      INPUT:  Gwpoint *point;
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
ug_patrefpoint (point)
Gwpoint	*point;
{
	struct { Gint op; Gws id; Gwpoint pt;} prms;
	int prmsiz;
	uu_denter(UU_GITRC,(us,"ug_patrefpoint(%g,%g)",(*point).x,(*point).y));
	if (	(ug_gksstli.curprats.patrefpt.x != (*point).x) ||
			(ug_gksstli.curprats.patrefpt.y != (*point).y)   )
	{
		/* tell workstations of pattern reference point chg */
		prms.pt.x=(*point).x;
		prms.pt.y=(*point).y;
		prms.op=UG_DPATREFPT;
		ug_wkout ( &prms, prmsiz=sizeof(prms)/sizeof(int) );
		zbytecp ( ug_gksstli.curprats.patrefpt, *point );
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsasfs(asfs). --  Set aspect source flags.
**							Not implemented yet.
**  PARAMETERS   
**      INPUT:  Gasfs		*asfs;
**  RETURNS      :  NCL_NO_ERROR if all went OK
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsasfs (asfs)
/*$ INPUT */
Gasfs		*asfs;
{
	Gerror irtn;
	uu_denter(UU_GTRC,(us,"gsasfs(asfs) called; too many asfs to print"));
	ug_asfs(asfs);
	if ( ug_gksos.sysstate == UG_SGOP ) 
	{
		/* generate asfs command */
	}
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  I_FUNCTION:  ug_asfs(asfs). --  Set aspect source flags.
**  PARAMETERS   
**      INPUT:  Gasfs *asfs.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
ug_asfs (asfs)
Gasfs		*asfs;
{
	struct { Gint op; Gws id; Gasfs flags; } prms;
	int prmsiz;
	uu_denter(UU_GITRC,(us,"ug_asfs(asfs) called; too many asfs to print"));
	prms.op = UG_DASFS;
	zbytecp ( prms.flags, *asfs );
	ug_wkout ( &prms, prmsiz=sizeof(prms)/sizeof(int) );
	zbytecp ( ug_gksstli.curprats.asfs, *asfs );
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gspickid(id) -- Set the current pick identifier.
**  PARAMETERS   
**      INPUT:  Gpickid id --new pick id.
**      OUTPUT: none
**
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**						  ENOTGWWS if the operating state was not GKOP, UG_WSOP,
**						  		UG_WSAC, or UG_SGOP.
**						  EBADPICK if the id was invalid.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gspickid (id)								/* set pick id */
/*$ INPUT */
Gpickid	id;
{
	uu_denter(UU_GTRC,(us,"gspickid(%d). old id=%d, find=%d",id,
					ug_gksstli.curprats.pickid,ug_find.find));
	ug_spickid(id);
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  I_FUNCTION:  ug_spickid(id) -- Set the current pick identifier.
**  PARAMETERS   
**      INPUT:  Gpickid id --new pick id.
**      OUTPUT: none
**  RETURNS      : 	none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror ug_spickid (id)								/* set pick id */
Gpickid	id;
{
	uu_denter(UU_GTRC,(us,"ug_spickid(%d). old id=%d, find=%d",id,
					ug_gksstli.curprats.pickid,ug_find.find));
	if ((ug_gksstli.curprats.pickid!=id)&&(ug_find.find==0))
	{
		ug_dspickid(id); 		/* tell workstations of pick-id chg */
	}
	ug_gksstli.curprats.pickid=id;					/* remember this pick id */
	if (ug_gksos.sysstate==UG_SGOP) 
	{
		ug_npkid(ug_segac(ug_gksstli.opnseg)->seglist,id);
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/*********************************************************************
**    I_FUNCTION     :  ug_dspickid(id) --  inform ws of pickid change.
**    PARAMETERS   
**       INPUT  : 
**          int id -- new pickid.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_dspickid(id)
int id;
{
	int prms[3];
		prms[2]=id;
		prms[0]=UG_DPICKID;
		ug_wkout(prms,3);
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gslinerep(ws,index,rep) -- Set polyline representation. 
**				Define the contents of the polyline bundle.
**				Not implemented yet.
** PARAMETERS   
**      INPUT:  Gws *ws -- workstation.
**					 Gindex index -- the entry in the bundle to be defined.
**					 Glnbundl *rep -- pointer to line bundle containing the
**							new values for the bundle table entry.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gslinerep(ws,index,rep)
/*$ INPUT */
Gws *ws;
Gindex index;
Glnbundl *rep;
{
	Gerror irtn;
	struct { Gint op; Gws id; Gindex ndx; Glnbundl lnrep; } prms;
	int reply[3];
	uu_denter(UU_GTRC,(us,"gslinerep(%d,%d,rep)",*ws,index));
	/* tell specific workstation of line-representation chg */
	prms.id = *ws;
	prms.ndx=index;
	zbytecp ( prms.lnrep, *rep );
	prms.op=UG_DLNREP;
	ug_wkcal(*ws,&prms,reply);
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsmarkrep(ws,index,rep)--  Set polymarker rep.
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id.
**					 Gindex index -- the entry in the bundle table to be defined.
**					 Gmkbundl *rep -- pointer to bundle of new values.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsmarkrep(ws,index,rep)
/*$ INPUT */
Gws *ws;
Gindex index;
Gmkbundl *rep;
{
	Gerror irtn;
	struct { Gint op; Gws id; Gindex ndx; Gmkbundl mkrep; } prms;
	int reply[3];
	uu_denter(UU_GTRC,(us,"gsmarkerp(%d,%d,rep)",*ws,index));
	/* tell specific workstation of marker-representation chg */
	prms.id = *ws;
	prms.ndx=index;
	zbytecp ( prms.mkrep, *rep );
	prms.op=UG_DMKREP;
	ug_wkcal(*ws,&prms,reply);
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gstextrep(ws,index,rep) -- Set text representation.
**							Not implemented yet.
**  PARAMETERS   
**      INPUT:  	Gws *ws;
**						Gindex index;
**						Gtxbundl *rep;
**      OUTPUT: none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gstextrep(ws,index,rep)
/*$ INPUT */
Gws *ws;
Gindex index;
Gtxbundl *rep;
{
	Gerror irtn;
	struct { Gint op; Gws id; Gindex ndx; Gtxbundl txrep; } prms;
	int reply[3];
	uu_denter(UU_GTRC,(us,"gstextrep(%d,%d,rep)",*ws,index));
	/* tell specific workstation of text-representation chg */
	prms.id = *ws;
	prms.ndx=index;
	zbytecp ( prms.txrep, *rep );
	prms.op=UG_DTEXTREP;
	ug_wkcal(*ws,&prms,reply);
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsfillrep(ws,index,rep) --  Set fill area rep.
**							Not implemented yet.
**  PARAMETERS   
**      INPUT:  	Gws *ws;
**						Gindex index;
**						Gflbundl *rep;
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsfillrep(ws,index,rep)		/* set fill area representation */
/*$ INPUT */
Gws *ws;
Gindex index;
Gflbundl *rep;
{
	Gerror irtn;
	struct { Gint op; Gws id; Gindex ndx; Gflbundl flrep; } prms;
	int reply[3];

	uu_denter(UU_GTRC,(us,"gsfillrep(%d,%d,rep)",*ws,index));

	prms.id = *ws;
	prms.ndx=index;
	zbytecp ( prms.flrep, *rep );
	prms.op=UG_DFAREP;
	ug_wkcal(*ws,&prms,reply);
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gspatrep(ws,index,rep) -- Set pattern representation.
**							Not implemeneted yet.
**  PARAMETERS   
**      INPUT:  	Gws *ws;
**						Gindex index;
**						Gptbundl *rep;
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gspatrep(ws,index,rep)
/*$ INPUT */
Gws *ws;
Gindex index;
Gptbundl *rep;
{
	Gerror irtn;
	struct { Gint op; Gws id; Gindex ndx; Gptbundl ptrep; } prms;
	int reply[3];
	uu_denter(UU_GTRC,(us,"gspatrep(%d,%d,rep)",*ws,index));
	/* tell specific workstation of fill-area-representation chg */
	prms.id = *ws;
	prms.ndx=index;
	zbytecp ( prms.ptrep, *rep );
	prms.op=UG_DPATREP;
	ug_wkcal(*ws,&prms,reply);
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gscolorrep(ws,index,rep)-- Set color representation.
**  PARAMETERS   
**      INPUT:  Gws *ws -- workstation id.
**					 Gindex index -- color index.
**					 Gcobundl *rep -- color (RGB) to go with index.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  Rgb values are specified in the range 0.0 to 1.0.
**						  The index is 0 through the maximum supported by
**						  the workstation.  This maximum value is returned by
**						  gqmaxcolor(). 
*********************************************************************/
Gerror gscolorrep(ws,index,rep)
/*$ INPUT */
Gws *ws;
Gindex index;
Gcobundl *rep;
{
	Gcobundl *p;
	UG_outwdt *outwdtpt;
	Gerror irtn;
	struct {int op; int wsid; int indx; Gfloat rgb[3];} prms;
	int reply[2];
	uu_denter(UU_GTRC,(us,"gscolorrep(%d,%d,%g %g %g ))",*ws,index,
			(*rep).red,(*rep).green,(*rep).blue));
	outwdtpt=(*ug_gksstli.wsopen[*ws].wdtptr).outwdtpt;
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	if ((*ws<0)||(*ws>4))
	{
		ug_errorhand(EWSIDINV,"gscolorrep",*ws);
		irtn=EWSIDINV;
	}
	if ((ug_gksos.sysstate!=UG_WSAC)&&(ug_gksos.sysstate!=UG_WSOP)
		&&(ug_gksos.sysstate!=UG_SGOP))
	{
		ug_errorhand(ENOTWSOP,"gscolorrep",NULL); irtn=ENOTWSOP;
	}
	if (ug_gksstli.wsopen[*ws].connid==NULL)
	{															/*workstation is closed */
		ug_errorhand(EWSNOTOP,"gscolorrep",NULL); irtn=EWSNOTOP;
	}
	if ((*outwdtpt).cofac.coavail!=UG_COLOUR) {
		ug_errorhand(EBADCOLX,"gscolorrep",NULL); irtn=EBADCOLX;
	}
	if (index<0) {
		ug_errorhand(ECINDXLZ,"gscolorrep",&index); irtn=ECINDXLZ;
	}
	if (index>=(*outwdtpt).cofac.colors) {
		ug_errorhand(EBADCOLX,"gscolorrep",&index); irtn=EBADCOLX;
	}
	if (((*rep).red<0.)||((*rep).red>1.0)||((*rep).green<0.)||((*rep).green>1.0)
		||((*rep).blue<0.)||((*rep).blue>1.0)) {
		ug_errorhand(ECOLRNGE,"gscolorrep",rep); irtn=ECOLRNGE;
	}
	if (irtn==NCL_NO_ERROR) {
#endif
		/* update color bundles array in WDT */
		p=(*outwdtpt).cobundl;
		p[index].red=(*rep).red;
		p[index].green=(*rep).green;
		p[index].blue=(*rep).blue;
		prms.op=UG_DCOLORREP;
		prms.wsid= *ws;
		prms.indx=index;
		prms.rgb[0]=(*rep).red;
		prms.rgb[1]=(*rep).green;
		prms.rgb[2]=(*rep).blue;
		ug_wkcal(*ws,&prms,reply);
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}
