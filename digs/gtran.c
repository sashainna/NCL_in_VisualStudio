/*********************************************************************
**    NAME         :  gtran.c -- DIGS viewing pipeline.
**       CONTAINS:
**		Gerror gswindow3(transform,window) -- Set window 3D.
**		Gerror Gsview3(transform,viewport) -- Set viewport 3D.
**		Gerror gsview(transform,viewport) -- Set viewport 2D.
**		Gerror gsviewpri(transform,reference,priority)
**		Gerror gsnormtran(n) -- Select normalization transform.
**		Gerror gsmodxf(xf,type) -- set modelling xform.
**		Gerror gslmodxf(xf,type) -- set localmodelling xform.
**		int ug_sntran(xform) -- set normalization xform.
**  	Gerror gsviewchar(ws,n,winclip,frontclip,backclip,priority)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       gtran.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:25
*********************************************************************/
#include "zsysdep.h"
#include <math.h>
#include "gtbl.h"
#include "gerror.h"
#include "gdidd.h"
#include "gviw.h"
#include "gmat4.h"
#include "gsegop.h"
#include "gsegac.h"
#include "udebug.h"
#include "gconvert.h"		/* needed for double/single conversion */

#define UG_TRUE 1
#define UG_FALSE 0
#define Logical int
#define INTSIZ(x) ((x+sizeof(int)-1)/sizeof(int))
#define INTSIZEOF(x) ((sizeof(x)+sizeof(int)-1)/sizeof(int))

extern struct {
	int mod;				/* 0=no REPLACE modelling xform in open seg yet */
	int ntran;			/* 0=no GSNORMTRAN in open seg yet */
}	ug_xfseg;	
extern Gfloat ug_chhtsclflag[UG_MAXNTRAN];

static char stype[3][16]={"PRECONCATENATE","POSTCONCATENATE","MODREPLACE"};

/********************************************************************* 
**  E_FUNCTION:  Gerror gswindow3(transform,window) -- Set window 3D.
**  PARAMETERS   
**      INPUT:  Gint transform -- xform number.
**					 Gwrect3 *window -- 3D window.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gswindow3(xform,win)
/*$ INPUT */
Gint xform;								/* transform index */
Gwrect3 *win;							/* pointer to world coord rectangle*/
{
	struct { Gint op; Gws id; Gint xf; Gwrect3 wn; } prms;
	Gerror irtn;
	Gos st;

	 uu_denter(UU_GTRC,(us,"gswindow3(%d,%3g,%3g,%3g,%3g,%3g,%3g)",xform,
					(*win).llf.x,(*win).llf.y,(*win).llf.z,
					(*win).urb.x,(*win).urb.y,(*win).urb.z));
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	st=ug_gksos.sysstate;
	if ((st!=UG_GKOP)&&(st!=UG_WSOP)&&(st!=UG_WSAC)&&(st!=UG_SGOP)) {
		ug_errorhand(ENOTGWWS,"gswindow3",&ug_gksos.sysstate);
		irtn=ENOTGWWS;
	}
	else if ((xform<1)||(xform>=UG_MAXNTRAN)) {
		ug_errorhand(EBADXFRM,"gswindow3",&xform);
		irtn=EBADXFRM;
	}
	else if (((*win).llf.x>=(*win).urb.x)||
				 ((*win).llf.y>=(*win).urb.y)||
				 ((*win).llf.z<=(*win).urb.z)) {	/* <= is correct for right hand*/
		 irtn=EBADRCTD;
		ug_errorhand(irtn,"gswindow3",win);
	}
	else  {
#endif
  		ug_gksstli.vtran[xform].window=(*win);
		ug_winviw(xform);
		/* delete segment ndc bounding boxes for those segs using this normtran */
		ug_ndcntranboxdel(xform);	
		prms.op=UG_DWIND;
		prms.xf=xform;
		zbytecp ( prms.wn, *win );
		ug_wkout ( &prms, sizeof(prms)/sizeof(int) );
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror Gsview3(transform,viewport) -- Set viewport 3D.
**  PARAMETERS   
**      INPUT:  Gint transform -- transformation number.
**					 Gnrect3 *viewport -- new viewport for this transformation.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsview3(xform,vpt)
/*$ INPUT */
Gint xform;					/* transformation index */
Gnrect3 *vpt;				/* viewport NDC rectangle */
{
	struct { Gint op; Gws id; Gint xf; Gnrect3 vp; } prms;
	Gerror irtn;
	Gos st;

	uu_denter(UU_GTRC,(us,"gsview3(%3g,%3g,%3g,%3g,%3g,%3g)",(*vpt).llf.x,
						(*vpt).llf.y,(*vpt).llf.z,(*vpt).urb.x,(*vpt).urb.y,
						(*vpt).urb.z));

	st=ug_gksos.sysstate;
	irtn=NCL_NO_ERROR;
#ifdef UU_CHECK
	if( ((*vpt).llf.x<0.) || ((*vpt).llf.y<0.) || 
		 ((*vpt).urb.x>ug_ndcmax[0]) || ((*vpt).urb.y>ug_ndcmax[1]) ) {
   	ug_errorhand(EBDVIEWP);
		irtn=EBDVIEWP;
	}
	else if ((st!=UG_GKOP)&&(st!=UG_WSOP)&&(st!=UG_WSAC)&&(st!=UG_SGOP)) {
		ug_errorhand(ENOTGWWS,"gsview3",&ug_gksos.sysstate);
		irtn=ENOTGWWS;
	}
	else if ((xform<1)||(xform>=UG_MAXNTRAN)) {
		ug_errorhand(EBADXFRM,"gsview3",&xform);
		irtn=EBADXFRM;
	}
	else if (((*vpt).llf.x>=(*vpt).urb.x)||
				 ((*vpt).llf.y>=(*vpt).urb.y)||
				 ((*vpt).llf.z<=(*vpt).urb.z)) {	/* <= is correct for right hand*/
		irtn=EBADRCTD;
		ug_errorhand(irtn,"gsview3",vpt);
	}
	else { 
#endif
		/* no errors so far */
  		zbytecp(ug_gksstli.vtran[xform].vport,(*vpt));		/* structure assign */
   	ug_ndcset=UG_TRUE;
   	ug_winviw(xform);
		ug_ndcntranboxdel(xform);	/* delete segment ndc bounding boxes for 
												those segs using this normtran */
		prms.op=UG_DVPORT;
		prms.xf=xform;
		zbytecp ( prms.vp, *vpt );
		ug_wkout ( &prms, sizeof(prms)/sizeof(int) );
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsview(transform,viewport) -- Set viewport 2D.
**  PARAMETERS   
**      INPUT:  Gint transform -- transformation number.
**					 Gnrect *viewport -- new viewport for this transformation.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsview(xform,vpt)			/* set 2d viewport. unchanged in z */
/*$ INPUT */
Gint xform;
Gnrect *vpt;
{
	Gnrect3 vp3;
	Gos st;
	Gerror irtn;

	uu_denter(UU_GTRC,(us,"gview2(%d,%3g,%3g,%3g,%3g)",xform,
  								(*vpt).ll.x, (*vpt).ll.y,(*vpt).ur.x,(*vpt).ur.y));

	st=ug_gksos.sysstate;
	if ((st!=UG_GKOP)&&(st!=UG_WSOP)&&(st!=UG_WSAC)&&(st!=UG_SGOP)) {
		ug_errorhand(ENOTGWWS,"gsview",&ug_gksos.sysstate);
		irtn=ENOTGWWS;
	}
	else if ((xform<1)||(xform>=UG_MAXNTRAN)) {
		ug_errorhand(EBADXFRM,"gsview",&xform);
		irtn=EBADXFRM;
	}
	else {
		vp3.llf.x=(*vpt).ll.x; vp3.llf.y=(*vpt).ll.y; 
		vp3.urb.x=(*vpt).ur.x; vp3.urb.y=(*vpt).ur.y;
		vp3.llf.z=ug_gksstli.vtran[xform].vport.llf.z; 
		vp3.urb.z=ug_gksstli.vtran[xform].vport.urb.z;
		irtn=gsview3(xform,&vp3);		/* shouldn't call a user callable */
	}
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsviewpri(transform,reference,priority)
**      Set viewport input priority
**  PARAMETERS   
**      INPUT:  Gint transform -- transformation number.
**					 Gint reference -- reference transformation number.
**					 Gvpri priority -- new priority. Either HIGHER or LOWER.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsviewpri(xform,ref,pri)
/*$ INPUT */
Gint xform;
Gint ref;
Gvpri pri;
{
	struct { Gint op; Gws id; Gint xf; Gint rf; Gvpri pr; } prms;
	Gerror irtn;

	uu_denter(UU_GTRC,(us,"gsviewpri(%d,%d,%d)",xform,ref,pri));

	prms.op=UG_DVPPRI;
	prms.xf=xform;
	prms.rf=ref;
	zbytecp ( prms.pr, pri );
	ug_wkout ( &prms, sizeof(prms)/sizeof(int) );
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsnormtran(n) -- Select normalization transform.
**  PARAMETERS   
**      INPUT:  Gint n -- number of transform to be selected.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsnormtran(xform)
/*$ INPUT */
Gint xform;
{
	Gerror irtn;
	Gos st;

	uu_denter(UU_GTRC,(us,"gsnormtran(%d)",xform));

	irtn=NCL_NO_ERROR;
	st=ug_gksos.sysstate;
	if ((st!=UG_GKOP)&&(st!=UG_WSOP)&&(st!=UG_WSAC)&&(st!=UG_SGOP)) {
		ug_errorhand(ENOTGWWS,"gswindow3",&ug_gksos.sysstate);
		irtn=ENOTGWWS;
	}
	else if ((xform<0)||(xform>=UG_MAXNTRAN)) {
		ug_errorhand(EBADXFRM,"gswindow3",&xform);
		irtn=EBADXFRM;
	}
	else 
	{
		ug_sntran(xform);
		if (st==UG_SGOP) {				/* put in currently open segment */
			ug_nntran(ug_segac(ug_gksstli.opnseg)->seglist,xform);
		}
	}
	uu_dexit;
	return(irtn);
}


/********************************************************************* 
**  E_FUNCTION:  Gerror gsmodxf(xf,type) -- Set modeling transform.
**
**  PARAMETERS   
**      INPUT:  Gtran xf 		-- Transform to be set.
**					 Gmodtran type -- How to apply the transform (Replace,  
**											Postconcatenate, Preconcatenate)
**
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsmodxf(xf,typ)
/*$ INPUT */
	Gtran xf;
	Gmodtran typ;
{
	Gerror irtn;											/* Return state */
	Gos st;													/* Current operation state */
	char us[140];

	irtn=NCL_NO_ERROR;

	uu_denter2(UU_GTRC,(us,"gsmodxf(xf,%d)",typ));

	st=ug_gksos.sysstate;
#ifdef UU_CHECK
	irtn=ug_chkwsac("gsmodxf");
	if (irtn==NCL_NO_ERROR) {
#endif
		ug_smodxf(xf,typ);
		if( st == UG_SGOP ){
			UG_mtranop cmd;
			UG_segstli *sp;

			/* Put in currently open segment */
			cmd.elttype = UG_MTRANOP;

		/* Do ptr assignment if same precision, individual element copy if not. */
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			ug_GtoIcopy(cmd.xf,xf);
#else
			ug_mcopy(cmd.xf,xf);
#endif
			cmd.type= typ;
			sp=ug_segac(ug_gksstli.opnseg);
			ug_lsins(sp->seglist, &cmd,INTSIZEOF(UG_mtranop));
			if (typ==UG_MODREPLACE) ug_xfseg.mod=1;	/* modelling cmd in seg */
			sp->xforms |= UG_SEGMODXF;				/* Modelling cmd in seg header */
		}
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_smodxf(xf,typ) -- set modeling transform.
**    PARAMETERS   
**       INPUT  :  Gtran xf -- 4x4 transform.
**						 Gmodtype typ -- how to apply the transformation.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_smodxf(xf,typ)
Gtran xf;
Gmodtran typ;					/* PRE, POSTCONCATENATE, or MODREPLACE */
{
	int i;
	Gfloat a[4][4];
	struct { Gint op; Gws id; Gtran mat; Gmodtran typ; } prms;

	uu_denter(UU_GITRC,(us,"ug_smodxf(typ=%d, xf:",typ));

#if UU_DEBUG==1
	if (UU_debmask&UU_GITRC) ug_matprt(xf);
#endif

	/* If modeling matrix is not identity, or type UG_MODREPLACE */
	if(( ug_mident(xf) != 1 )||(typ==UG_MODREPLACE)) {

		switch(typ) {

		case UG_PRECONCATENATE:
			ug_matmp(a,xf,ug_modxform);
			ug_mcopy(ug_modxform,a);
			break;
			
		case UG_POSTCONCATENATE:
			ug_matmp(a,ug_modxform,xf);
			ug_mcopy(ug_modxform,a);
			break;

		case UG_MODREPLACE:
			ug_mcopy(ug_modxform,xf);
			break;

		default: uu_denter2(UU_GITRC,(us,"ug_smodxf bad typ=%d",typ));
			uu_dexit;
			break;
		}

		for (i=0; i<UG_MAXNTRAN; i++) {		/* update all ug_cxform's */
			/*ug_winviw(i);	/* update all xforms */
			ug_mat3mp(ug_cxform[i],ug_modxform,ug_lmodxform,ug_vxform[i]);
			ug_cxchg[i]=UG_TRUE;  				/* remember we changed ug_cxform */
			ug_chhtsclflag[i]=0;					/* rememberchar height scale
															factor not up to date */
			/*uu_dprint(UU_GITRC,(us,"ug_smodxf. cxform[%d]=",i));
			/*ug_matprt(ug_cxform[i]);				/* print ug_cxform[xform] */
			/*uu_dprint(UU_GITRC,(us,"ug_smodxf. vxform[%d]=",i));
			/*ug_matprt(ug_vxform[i]);				/* print ug_vxform[xform] */
		}
	
		prms.op =UG_DMOD3;
		ug_mcopy(prms.mat,xf);
		prms.typ = typ;
		ug_wkout(&prms,sizeof(prms)/sizeof(int));
	}								/* end if not identity */
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gslmodxf(xf,type) -- Set modeling transform.
**
**  PARAMETERS   
**      INPUT:  Gtran xf 		-- Transform to be set.
**					 Gmodtran type -- How to apply the transform (Replace,  
**											Postconcatenate, Preconcatenate)
**
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gslmodxf(xf,typ)
/*$ INPUT */
	Gtran xf;
	Gmodtran typ;
{
	Gerror irtn;											/* Return state */
	Gos st;													/* Current operation state */
	
	uu_denter(UU_GTRC,(us,"gslmodxf(xf,%d)",typ));
	irtn=NCL_NO_ERROR;
	st=ug_gksos.sysstate;
#ifdef UU_CHECK
	irtn=ug_chkwsac("gslmodxf");
	if (irtn==NCL_NO_ERROR) {
#endif
		ug_slmodxf(xf,typ);
		if( st == UG_SGOP ){
			UG_lmtranop cmd;
			UG_segstli *sp;

			/* Put in currently open segment */
			cmd.elttype = UG_LMTRANOP;

		/* Do ptr assignment if same precision, individual element copy if not. */
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
			ug_GtoIcopy(cmd.xf,xf);
#else
			ug_mcopy(cmd.xf,xf);
#endif
			cmd.type= typ;
			sp=ug_segac(ug_gksstli.opnseg);
			ug_lsins(sp->seglist, &cmd,INTSIZEOF(UG_lmtranop));
			if (typ==UG_MODREPLACE) ug_xfseg.mod=1;	/* modelling cmd in seg */
			sp->xforms |= UG_SEGMODXF;				/* Modelling cmd in seg header */
		}
#ifdef UU_CHECK
	}
#endif
	uu_dexit;
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     :  int ug_slmodxf(xf,typ) -- set local modeling transform.
**    PARAMETERS   
**       INPUT  :  Gtran xf -- 4x4 transform.
**						 Gmodtype typ -- how to apply the transformation.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_slmodxf(xf,typ)
Gtran xf;
Gmodtran typ;					/* PRE, POSTCONCATENATE, or MODREPLACE */
{
	int i;
	Gfloat a[4][4];
	struct { Gint op; Gws id; Gtran mat; Gmodtran typ; } prms;

	uu_denter(UU_GITRC,(us,"ug_slmodxf(typ=%d, xf:",typ));

#if UU_DEBUG==1
	if (UU_debmask&UU_GITRC) ug_matprt(xf);
#endif

	/* No longer no if ug_lmodxform is identity */
	uu_dprint(UU_GITRC,(us,"ug_lmodidnt=0"));
	ug_lmodidnt = 0;

	/* If modeling matrix is not identity, or type UG_MODREPLACE */
	if(( ug_mident(xf) != 1 )||(typ==UG_MODREPLACE)) {

		switch(typ) {

		case UG_PRECONCATENATE:
			ug_matmp(a,xf,ug_lmodxform);
			ug_mcopy(ug_lmodxform,a);
			break;
			
		case UG_POSTCONCATENATE:
			ug_matmp(a,ug_lmodxform,xf);
			ug_mcopy(ug_lmodxform,a);
			break;

		case UG_MODREPLACE:
			ug_mcopy(ug_lmodxform,xf);
			break;

		default: uu_denter2(UU_GITRC,(us,"ug_slmodxf bad typ=%d",typ));
			uu_dexit;
			break;
		}

		for (i=0; i<UG_MAXNTRAN; i++) {
			/*ug_winviw(i);	/* update all xforms */
			ug_mat3mp(ug_cxform[i],ug_modxform,ug_lmodxform,ug_vxform[i]);
			ug_cxchg[i]=UG_TRUE;  			/* remember we changed ug_cxform */
			ug_chhtsclflag[i]=0;				/* rememberchar height scale
														factor not up to date */
		}
	
		prms.op =UG_DMOD3;
		ug_mcopy(prms.mat,xf);
		prms.typ = typ;
		ug_wkout(&prms,sizeof(prms)/sizeof(int));
	}								/* end if not identity */
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_nntran(no,xform) -- gen normtran cmd.
**    PARAMETERS   
**       INPUT  : 	UG_LSI no -- list to gen cmd into.
**							int xform -- number of the normalization xform.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_nntran(no,xform)			/* gen normtran cmd */
UG_LSI no;
int xform;
{
	UG_sntranop cmd;
	uu_denter(UU_GITRC,(us,"ug_nntran(%d,%d)",no,xform));
	cmd.elttype=UG_SNTRANOP;
	cmd.xform=xform;
	ug_lsins(no,&cmd,INTSIZEOF(cmd));
	ug_xfseg.ntran=1;				/* a normtran cmd is in open seg */
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  int ug_sntran(xform) -- set normalization xform.
**    PARAMETERS   
**       INPUT  :  int xnorm -- which xform to use.
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_sntran(xform)
Gint xform;
{
	int prms[3];

	uu_denter(UU_GITRC,(us,"ug_sntran(%d)",xform));

	ug_gksstli.curvwindex=xform;
	prms[0]=UG_DNORMTRAN;
	prms[2]=xform;
	ug_wkout(prms,3);
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION: gsviewchar(ws,n,winclip,frontclip,backclip,priority)
**		Set view characteristics.
**  PARAMETERS   
**      INPUT: Gws *ws	Workstation identifier.
**					int n		normalization transformation number.
**					Gclip winclip,frontclip,backclip -- new clipping state, 
**									UG_CLIP or UG_NOCLIP.
**					int priority	Input priority reference number.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsviewchar(ws,n,winclip,frontclip,backclip,priority)
/*$ INPUT */
int n;
Gclip winclip,backclip,frontclip;
int priority;
{
	Gerror irtn;
	int prms[7];

	uu_denter(UU_GTRC,(us,"gsviewchar(ws,%d,%d,%d,%d,%d)",
		n,winclip,frontclip,backclip,priority));

	/* save new clipping states in DIGS state list */
	ug_gksstli.vtran[n].winclip=winclip;
	ug_gksstli.vtran[n].backclip=backclip;
	ug_gksstli.vtran[n].frontclip=frontclip;
	ug_gksstli.vtran[n].inputprio=priority;
	/* notify workstation of clipping state change */
	prms[0]=UG_DCLIP;
	prms[2]=n;
	prms[3]=(int)winclip;
	prms[4]=(int)backclip;
	prms[5]=(int)frontclip;
	prms[6]=priority;
	ug_wkout (prms,sizeof(prms)/sizeof(int));
	irtn=NCL_NO_ERROR;
	uu_dexit;
	return(irtn);
}
