#include "zsysdep.h"
#include <stdio.h>
#include "g.h"
#include "gviw.h"
#include "umath.h"
#include "udebug.h"
/*#include "gsegop.h" */
#include "gconvert.h"

#define INTSIZ(x) ((x+sizeof(int)-1)/sizeof(int))
#define INTSIZEOF(x) ((sizeof(x)+sizeof(int)-1)/sizeof(int))

/*
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gatts3.c 2.10 7/18/86 16:18:38 single"};
#else
static char uu_sccsident[]={"@(#) gatts3.c 2.10 7/18/86 16:18:38 double"};
#endif
*/

/*********************************************************************
**    NAME         :  gatts3.c -- not user callable attribute routines.
**       CONTAINS: Segment command generating routines.
**		ug_ndfat(n)	-- gen a default attribute command.
**		ug_nlncolr(n,c) -- gen a gslinecolor command.
**		ug_nmkcolr(n,c) -- gen a gslinecolor command.
**		ug_ntxcolr(n,c) -- gen a gstextcolor command.
**		ug_nfacolr(n,c) -- gen a fillarea color cmd.
**		ug_nlstl(n,lstyl) -- gen a linestyle command.
**		ug_nlwid(n,w) -- gen a linewidth command.
**		ug_npen(n,p) -- gen a pen command.
**		ug_nfont(n,f) -- gen a font command.
**		ug_nchhgt(n,h) -- gen a char height command.
**		ug_nchpl(n,dx,dy,dz) -- gen a charplane command.
**		ug_nchup(n,up) -- gen a charup 2 command.
**		ug_nchup3(n,up) -- gen a charup 3 command.
**		ug_nchpa(n,p) -- gen a charpath command.
**		ug_nchsp(n,sp) -- gen a charspace command.
**		ug_nchj(n,align) -- gen a charjust command.
**		ug_nmksy(n,s) -- gen a marker symbol command.
**		ug_npkid(n,id) -- gen a pickid command.
**		ug_nlindex(n,index) -- gen a polyline index.
**		ug_nmkindex(n,index) -- gen a marker index cmd.
**		ug_nmksiz(n,siz) -- gen marker size command.
**		ug_ntxindex(n,index) -- gen a text index cmd.
**		ug_nchexp(n,expn) -- gen a char expansion fact cmd.
**		ug_nchvec (n,vec) -- gen a char vec cmd.
**		ug_ntxpath(n,p) -- gen a text path cmd.
**		ug_nfaindex(n,ndx) -- gen a fill area index cmd.
**		ug_nedgeflag(n,f) -- gen an edgeflag cmd.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
** 
**    MODULE NAME AND RELEASE LEVEL
**       gatts3.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:17
*********************************************************************/

/*********************************************************************
**    I_FUNCTION     :  ug_ndfat(n) -- gen a default att command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_ndfat(n)										/* gen a default att command */
UG_LSI n;
{
	UG_dfatsop cmd;
	uu_denter(UU_GITRC,(us,"ug_ndfat(%d)",n));
	cmd.elttype=UG_DFATSOP;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}
/*********************************************************************
**    I_FUNCTION     :  ug_nshdlucency(n,lucency) -- gen a shade
translucency command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added stipple for shading
.....Yurong 2/1/99
*/
/*temp yurong ug_nshdstipple(n,stipple)
UG_LSI n; unsigned char *stipple;
{
	UG_stippleop cmd;
	int i;
	uu_denter(UU_GITRC,(us,"ug_nshdcolr(%d,%d)",n,c));
	cmd.elttype=UG_STIPPLEOP;
	for (i=0; i<128; i++)
		cmd.stipple[i] = stipple[i];
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}
*/
/*********************************************************************
**    I_FUNCTION     :  ug_nshdlucency(n,lucency) -- gen a shade
translucency command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added translucency for shading
.....Yurong 2/1/99
*/
void ug_nshdlucency(n,lucency)
UG_LSI n; int lucency;
{
	UG_lucencyop cmd;
	uu_denter(UU_GITRC,(us,"ug_nshdcolr(%d,%d)",n,c));
	cmd.elttype=UG_LUCENCYOP;
	cmd.lucency = lucency;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}
/*********************************************************************
**    I_FUNCTION     :  ug_nshdcolr(n,c) -- gen a gslinecolor command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added color for shading
.....Yurong
*/
void ug_nshdcolr(n,c)							/* gen a gsshdcolor command */
UG_LSI n; Gcolor c;
{
	UG_lncolrop cmd;
	uu_denter(UU_GITRC,(us,"ug_nshdcolr(%d,%d)",n,c));
	cmd.elttype=UG_SHDCOLROP;
	cmd.color=c;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nlncolr(n,c) -- gen a gslinecolor command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nlncolr(n,c)							/* gen a gslinecolor command */
UG_LSI n; Gcolor c;
{
	UG_lncolrop cmd;
	uu_denter(UU_GITRC,(us,"ug_nlncolr(%d,%d)",n,c));
	cmd.elttype=UG_LNCOLROP;
	cmd.color=c;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nmkcolr(n,c) -- gen a gslinecolor command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nmkcolr(n,c)							/* gen a gslinecolor command */
UG_LSI n; Gcolor c;
{
	UG_mkcolorop cmd;
	uu_denter(UU_GITRC,(us,"ug_nmkcolr(%d,%d)",n,c));
	cmd.elttype=UG_MKCOLROP;
	cmd.color=c;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_ntxcolr(n,c) -- gen a gstextcolor command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_ntxcolr(n,c)							/* gen a gstextcolor command */
UG_LSI n; Gcolor c;
{
	UG_txcolorop cmd;
	uu_denter(UU_GITRC,(us,"ug_ntxcolr(%d,%d)",n,c));
	cmd.elttype=UG_TXCOLROP;
	cmd.color=c;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;

}

/*********************************************************************
**    I_FUNCTION     :  ug_nfacolr(n,c) -- gen a fillarea color cmd.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nfacolr(n,c)							/* gen a fillarea color cmd */
UG_LSI n; Gcolor c;
{
	UG_facolorop cmd;
	uu_denter(UU_GITRC,(us,"ug_nfacolr(%d,%d)",n,c));
	cmd.elttype=UG_FACOLROP;
	cmd.color=c;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nfacolrrep(n,c) -- gen a fillarea color rep cmd.
**    PARAMETERS   
**       INPUT  : 
**          n			Segment command goes into.
**				c			Color representation.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nfacolrrep(n,c)							/* gen a fillarea color rep cmd */
UG_LSI n;
Gcobundl c;
{
	UG_facolorrepop cmd;

	uu_denter(UU_GITRC,(us,"ug_nfacolr(%d,%f,%f,%f)",n,c.red,c.green,c.blue));
	cmd.elttype=UG_FACOLRREPOP;
	zbytecp(cmd.color,c);
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nlstl(n,lstyl) -- gen a linestyle command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nlstl(n,lstyl)							/* gen a linestyle command */
UG_LSI n;
Glntype *lstyl;
{
/*	char us[100];*/
	UG_lstylop cmd;
	int siz;

#ifdef UU_CHECK
	ug_chkad(n,"ug_nlstl 1st arg");
	ug_chkad(lstyl,"ug_nlstl 2nd arg");
#endif
	uu_denter2(UU_GITRC,(us,"ug_nlstl(%x,typeno=%d)",n,(*lstyl).typeno));
	cmd.elttype=UG_LSTYLOP;
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
	ug_GtoIlntype(cmd.ls,lstyl);
#else
	zbytecp(cmd.ls,(*lstyl));
#endif

	if ((*lstyl).typeno!=21) 
		siz=sizeof(cmd)-sizeof(cmd.ls)+sizeof(cmd.ls.typeno);
	else 								/* user defined linestyle */
		siz=sizeof(cmd);
	uu_dprint(UU_GITRC,(us,"ug_nlstl siz=%d",siz));
	ug_lsins(n,&cmd,INTSIZ(siz));
		
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nlwid(n,w) -- gen a linewidth command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nlwid(n,w)									/* gen a linewidth command */
UG_LSI n;
Gfloat w;
{
	UG_lwidop cmd;
	uu_denter(UU_GITRC,(us,"ug_nlwid(%d,%g)",n,w));
	cmd.elttype=UG_LWIDOP;
	cmd.width=w;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nfont(n,fp) -- gen a font command.
**    PARAMETERS   
**       INPUT  : 	Gtxfp *fp -- pointer to font/precision pair.
**							int n -- segment number.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nfont(n,fp)									/* gen a font/prec command */
UG_LSI n;
Gtxfp *fp;
{
	UG_fontop cmd;
	uu_denter(UU_GITRC,(us,"ug_nfont(%d,font=%d,prec=%d)",
			n,fp->font,fp->prec));
	cmd.elttype=UG_FONTOP;
	zbytecp(cmd.p,*fp);					/* structure assignment */
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nchhgt(n,h) -- gen a char height command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nchhgt(n,h)								/* gen a char height command */
UG_LSI n;
Gfloat h;
{
	UG_chhgtop cmd;
	uu_denter(UU_GITRC,(us,"ug_nchhgt(%d,%g)",n,h));
	cmd.elttype=UG_CHHGTOP;
	cmd.height=h;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nchpl(n,dx,dy,dz) -- gen a charplane command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nchpl(n,dx,dy,dz)						/* gen a charplane command */
UG_LSI n;
Gfloat dx,dy,dz;
{
	UG_chplaneop cmd;
	uu_denter(UU_GITRC,(us,"ug_nchpl(%d,%g,%g,%g)",n,dx,dy,dz));
	cmd.elttype=UG_CHPLANEOP;
	cmd.txpvc.x=dx;
	cmd.txpvc.y=dy;
	cmd.txpvc.z=dz;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nchup(n,up) -- gen a charup 2 command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nchup(n,up)							/* gen a charup 2 command */
UG_LSI n;
Gwpoint *up;
{
	UG_chup2op cmd;
	uu_denter(UU_GITRC,(us,"ug_nchup(%d,%g,%g)",n,(*up).x,(*up).y));
	cmd.elttype=UG_CHUP2OP;

	/* Do ptr assignment if same precision, individual element copy if not. */
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
	ug_GtoIpoint(cmd.upvec,(*up));
#else
	zbytecp(cmd.upvec,(*up));
#endif
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nchup3(n,up) -- gen a charup 3 command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nchup3(n,up)					/* gen a charup 3 command */
UG_LSI n;
Gwpoint3 *up;
{
	UG_chup3op cmd;
	uu_denter(UU_GITRC,(us,"ug_nchup3(%d,%g,%g,%g)",n,(*up).x,(*up).y,(*up).z));
	cmd.elttype=UG_CHUP3OP;

	/* Do ptr assignment if same precision, individual element copy if not. */
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
	ug_GtoIpoint3(cmd.upvec,(*up));
#else
	zbytecp(cmd.upvec,(*up));
#endif
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nchpa(n,p) -- gen a charpath command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nchpa(n,p)									/* gen a charpath command */
UG_LSI n;
Gtxpath p;
{
	UG_chpathop cmd;
	uu_denter(UU_GITRC,(us,"ug_nchpa(%d,%d)",n,p));
	cmd.elttype=UG_CHPATHOP;
	cmd.path=p;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nchsp(n,sp) -- gen a charspace command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nchsp(n,sp)									/* gen a charspace command */
UG_LSI n;
Gchrsp sp;
{
	UG_chspaceop cmd;
	uu_denter(UU_GITRC,(us,"ug_nchsp(%d,%g)",n,sp));
	cmd.elttype=UG_CHSPACEOP;
	cmd.spacing=sp;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nchj(n,align) -- gen a charjust command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nchj(n,align)								/* gen a charjust command */
UG_LSI n;
Gtxalign *align;
{
	UG_chjustop cmd;
	extern char ug_alignnames[2][5][10];	/* initialized in gksatts.c */
	uu_denter(UU_GITRC,(us,"ug_nchj(%d,hor=%s ver=%s)",
			n,&ug_alignnames[0][(int)(*align).hor][0],
			&ug_alignnames[1][(int)(*align).ver][0]));
	cmd.elttype=UG_CHJUSTOP;
	zbytecp(cmd.align,*align);
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nmksy(n,s) -- gen a marker symbol command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nmksy(n,s)									/* gen a marker symbol command */
UG_LSI n;
Gmktype s;
{
	UG_symbolop cmd;
	uu_denter(UU_GITRC,(us,"ug_nmksy(%d,%d)",n,s));
	cmd.elttype=UG_SYMBOLOP;
	cmd.type=s;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_npkid(n,id) -- gen a pickid command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_npkid(n,id)								/* gen a pickid command */
UG_LSI n;
int id;
{
	UG_pickidop cmd;
	uu_denter(UU_GITRC,(us,"ug_npkid(%d,%d)",n,id));
	cmd.elttype=UG_PICKIDOP;
	cmd.pid=id;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nlindex(n,index) -- gen a polyline index.
**       *** not implemented yet ***
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nlindex(n,index)
UG_LSI n,index;
{
}

/*********************************************************************
**    I_FUNCTION     :  ug_nmkindex(n,index) -- gen a marker index cmd.
**       *** not implemented yet ***
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nmkindex(n,index)
UG_LSI n,index;
{
}

/*********************************************************************
**    I_FUNCTION     :  ug_nmksiz(n,siz) -- gen marker size command.
**       *** not implemented yet ***
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nmksiz(n,siz)
UG_LSI n;
Gscale siz;
{
}

/*********************************************************************
**    I_FUNCTION     :  ug_ntxindex(n,index) -- gen a text index cmd.
**			*** not implemented yet ***
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_ntxindex(n,index)
UG_LSI n,index;
{
}

/*********************************************************************
**    I_FUNCTION     :  ug_nchexp(n,w,h) -- gen a char expansion fact cmd.
**       *** not implemented yet ***
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nchexp(n,expn)
UG_LSI n;
Gfloat expn;
{
	UG_chexpop cmd;
	uu_denter(UU_GITRC,(us,"ug_nchexp(%d,%g)",n,expn));
	cmd.elttype=UG_CHEXPOP;
	cmd.expn=expn;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nchvec (n,vec) -- gen a char vec cmd.
**       *** not implemented yet ***
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nchvec (n,vec)
UG_LSI n;
Gwpoint3 *vec;
{
	UG_chplaneop cmd;
	uu_denter(UU_GITRC,(us,"ug_nchvec(%d,%g, %g, %g)",n,vec->x,vec->y,vec->z));
	cmd.elttype=UG_CHPLANEOP;

	/* Do ptr assignment if same precision, individual element copy if not. */
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
	ug_GtoIpoint3(cmd.txpvc,(*vec));
#else
	zbytecp(cmd.txpvc,(*vec));
#endif
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_ntxpath(n,p) -- gen a text path cmd.
**       *** not implemented yet ***
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_ntxpath(n,p)
UG_LSI n;
Gtxpath p;
{
	UG_chpathop cmd;
	extern char ug_txpathnames[4][9];		/* initialized in gksatts.c */
	uu_denter(UU_GITRC,(us,"ug_ntxpath(%d,%s)",n,&ug_txpathnames[(int)p][0]));
	cmd.elttype=UG_CHPATHOP;
	cmd.path=p;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_nfaindex(n,ndx) -- gen a fill area index cmd.
**       *** not implemented yet ***
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nfaindex(n,ndx)
UG_LSI n,ndx;
{
}

/*********************************************************************
**    I_FUNCTION     :  ug_nedgeflag(n,f) -- gen a edgeflag cmd.
**    PARAMETERS   
**       INPUT  :  int n; -- segment number.
**						 Gtoggle f; -- OFF or ON;
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nedgeflag(n,f)
UG_LSI n;
Gtoggle f;
{
	UG_edgeflagop cmd;
/*	static char sedge[2][4]={"OFF","ON"};*/
	uu_denter(UU_GITRC,(us,"ug_nedgeflag(%d,%s)",n,&sedge[(int)f][0]));
	cmd.elttype=UG_EDGEFLAGOP;
	cmd.f=f;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}
/*********************************************************************
**    I_FUNCTION     :  ug_nmaterial(n,material) -- gen a material command.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ug_nmaterial(n,material)
UG_LSI n; int material;
{
	UG_materialop cmd;
	uu_denter(UU_GITRC,(us,"ug_nmaterial(%d,%d)",n,material));
	cmd.elttype=UG_MATERIALOP;
	cmd.material = material;
	ug_lsins(n,&cmd,INTSIZEOF(cmd));
	uu_dexit;
}
