/*********************************************************************
**    NAME         :  gtran3.c --  DIGS viewing pipeline(cont).
**       CONTAINS:
**		ug_winviw(xform) -- update ug_cxform[xform].
**		ug_rotup(v,a) -- rotate so that v is (0,1,0).
**    ug_mat3mp(r,a,b,c) -- r = a x b x c.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gtran3.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:25
*********************************************************************/
#include "zsysdep.h"
#include "umath.h"
#include <stdio.h>
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
#include "gerrorxf.h"
#include "ginqst.h"
#include "ginqatt.h"
#include "ginqatt2.h"
#include "ginqatt3.h"
#include "ginqxf.h"
#include "gerrorid.h"
#include "ginqdsiz.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gviw.h"
#include "gmat4.h"
#include "gtbldef.h"
#include "udebug.h"
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gtran3.c 25.1 04/29/15 15:05:25 single"};
#else
static char uu_sccsident[]={"@(#) gtran3.c 25.1 04/29/15 15:05:25 double"};
#endif
#define UG_TRUE 1
#define UG_FALSE 0
#define Logical int
static Gfloat vwport[3][2],wind[3][2];	/* temporary copy for loops */
Gfloat ug_atan2();
extern Gfloat ug_chhtsclflag[UG_MAXNTRAN];
/*********************************************************************
**    I_FUNCTION     :  ug_winviw(xform) -- update ug_cxform[xform].
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_winviw(xform) 			/* update ug_cxform for modelling, viewing parms */
Gint xform;
{
/* EPS used in y-axis rotation for viewport normal  and in check for len=0*/
#define EPS (UU_REAL) 1.0e-5
	int i,j;
  Gfloat len,r,eps;
  Gfloat ang,cosang,sinang,s[3],t[3];
  Gwpoint3 vup2;
  Gfloat a[4][4],b[4][4],c[4][4];
  Gfloat dx,dy,dz,sx,sy,sz;	/* scale, xlate factors */
  Gfloat dwx,dwy,dwz;			/* size of window */
  Gfloat dvx,dvy,dvz;			/* size of viewport */
  UG_vtran3 *viewp;				/* pointer to the this view xform */

	uu_denter(UU_GITRC,(us,"ug_winviw(%d)",xform));
	if ((xform<0)||(xform>=UG_MAXNTRAN)) {
		ug_errorhand(EBADXFRM,"ug_winviw",&xform);
		goto rtn;
	}
	viewp= &(ug_gksstli.vtran[xform]);		/* get pointer to this view xform */
	dwx=(viewp->window.urb.x - viewp->window.llf.x)/2.;
	dwy=(viewp->window.urb.y - viewp->window.llf.y)/2.;
	dwz=(viewp->window.urb.z - viewp->window.llf.z)/2.;
	dvx=(viewp->vport.urb.x - viewp->vport.llf.x)/2.;
	dvy=(viewp->vport.urb.y - viewp->vport.llf.y)/2.;
	dvz=(viewp->vport.urb.z - viewp->vport.llf.z)/2.;
	if ((dwx<EPS)||(dwy<EPS)||(-dwz<EPS)||(dvx<EPS)||(dvy<EPS)||(-dvz<EPS)) {
		/* window or viewport has a zero dimension. error. */
		uu_dprint(-1,(us,
			"ug_winviw(%d) bad window or vport size: %g %g %g %g %g %g",
			 xform,dwx,dwy,dwz,dvx,dvy,dvz));
		ug_errorhand(EBADXFRM,"ug_winviw",&xform);
		goto rtn;
	}
	for (i=0; i<4; i++) {
		for (j=0; j<4; j++) {
			a[i][j]=0.0; b[i][j]=0.0;
		}
	}
	ug_ident(ug_vxform[xform]);			/* ug_vxform[xform] = identity */

  /* translate so that view ref point is at origin */
  ug_trans(-viewp->vrefpt.x,-viewp->vrefpt.y,
			-viewp->vrefpt.z,ug_vxform[xform]);
  /* rotate so that view plane normal is (0,0,1) */
  len=sqrt(viewp->vpnorm.x*viewp->vpnorm.x
			+ viewp->vpnorm.y*viewp->vpnorm.y
			+ viewp->vpnorm.z
			* viewp->vpnorm.z);		/*length of vpnorm */
	if (len<EPS) {
		uu_denter2(-1,(us,"ug_winviw error. vpnorm[%d]=%g %g %g. set to (0,0,1)",
			xform,viewp->vpnorm.x, viewp->vpnorm.y, viewp->vpnorm.z));
		uu_dexit;
		viewp->vpnorm.x=0.;
		viewp->vpnorm.y=0.;
		viewp->vpnorm.z=1.;
		len=1.0;
	}
	r=sqrt(len*len
			- viewp->vpnorm.y * viewp->vpnorm.y);		/* proj len in xz plane*/
	/* calc y-axis rotation angle */
	if (((fabs(viewp->vpnorm.x)>EPS)||(viewp->vpnorm.z<0.))&&r>0.) {	
		cosang = viewp->vpnorm.z/r;
		sinang = - viewp->vpnorm.x/r;	/* neg to rotate cw*/
		/* rotate about y-axis, pos 90 deg angle is such that, when viewed
			from pos y axis toward origin, pos z axis is rotated ccw into the
			pos x axis. */
		b[1][1]=1.0;
		b[3][3]=1.0;
		b[0][0]=cosang;
		b[2][2]=cosang;
		b[2][0]=sinang;
		b[0][2] = -sinang;
		ug_matmp(c,ug_vxform[xform],b);				/* c= ug_cxform*b */
	}
	else ug_mcopy(c,ug_vxform[xform]);			/* didnt need to y-axis rotate*/
	/* calc x-axis rotation angle */
	cosang=r/len;
	sinang = viewp->vpnorm.y/len;
	/* rotate about x-axis, right handed rotation */
	a[0][0]=1.0; a[3][3]=1.0;
	a[1][1]=cosang;
	a[2][2]=cosang;
	a[1][2]=sinang;
	a[2][1]= -sinang;
	ug_matmp(b,c,a);						/* b = c * a */
	/* apply a rotation for vup */
	ang=0.0;
	/* 1st apply xform b to vup vector giving vup2 */
	ug_vecxform(&vup2,&(viewp->vup),b);
	ang=ug_atan2(vup2.x,vup2.y);
	if (ang!=0.0) {
		grotz(ang,c);
		ug_matmp(ug_vxform[xform],b,c);
	}
	else ug_mcopy(ug_vxform[xform],b);		/* didnt need a z rotation for vup */
	
	/* at this point matrix ug_cxform[xform] xforms modelling coords so that
		view refpt is at origin, view plane normal is down the z axis,
		view up vector is the y axis. This defines a new viewing
		coordinate system. From here on, work window and prp which
		are defined in that system.*/
	if (viewp->vtype==UG_PERSPECTIVE) {	/* perspective? */
		/* translate center of window to origin. */
		ug_trans(-(viewp->prp.x),-(viewp->prp.y),-(viewp->prp.z));
		/* shear in x,y so that prp is on the z axis. */
		sx=viewp->prp.x/viewp->prp.z;
		sy=viewp->prp.y/viewp->prp.z;
		ug_ident(a); a[0][2]= -sx; a[1][2]= -sy;
		ug_matmp(b,ug_vxform[xform],a);	/* b=ug_vxform*a */
		/* shear so that truncated pyramid view volume is a rectangular solid*/
		sx=(dwx*dwx)/(dwz*dwz);
		sy=(dwy*dwy)/(dwz*dwz);
		ug_ident(a);
		a[0][2]=sx; a[1][2]=sy;
		ug_matmp(ug_vxform[xform],b,a);	/* ug_vxform=b*a */
		/* now viewing pyramid is mapped into the rectangular window  centerd
			at the origin */
		/* apply a scale and translation to map window to viewport. Window
			is now centered about origin of UVN. */
		sx=(viewp->vport.urb.x - viewp->vport.llf.x) /
				 (viewp->window.urb.x - viewp->window.llf.x);
		sy=(viewp->vport.urb.y - viewp->vport.llf.y) /
				 (viewp->window.urb.y - viewp->window.llf.y);
		sz=(viewp->vport.urb.z - viewp->vport.llf.z) /
				 (viewp->window.urb.z - viewp->window.llf.z);
		ug_scale(sx,sy,sz,ug_vxform[xform]);
		/* since window is centered at origin, just xlate to center of vport. */
		dx=(viewp->vport.llf.x+viewp->vport.urb.x)/2.;
		dy=(viewp->vport.llf.y+viewp->vport.urb.y)/2.;
		dz=(viewp->vport.llf.z+viewp->vport.urb.z)/2.;
		ug_trans(dx,dy,dz,ug_vxform[xform]);
	}
	else {						/* PARALLEL projectin */
		sx=dvx/dwx; sy=dvy/dwy; sz=dvz/dwz;
		dx=viewp->vport.llf.x-viewp->window.llf.x*sx;
		dy=viewp->vport.llf.y-viewp->window.llf.y*sy;
		dz=viewp->vport.llf.z-viewp->window.llf.z*sz;
		ug_scale(sx,sy,sz,ug_vxform[xform]);
		ug_trans(dx,dy,dz,ug_vxform[xform]);
	}										/* PARALLEL */
  /* calc ug_cxform = global modxform x local modxform  x ug_vxform[xform] */
	ug_mat3mp(ug_cxform[xform],ug_modxform,ug_lmodxform,ug_vxform[xform]);
	ug_cxchg[xform]=UG_TRUE;            /* remember we changed ug_cxform */
	ug_chhtsclflag[xform]=0;				/* rememberchar height scale
														factor not up to date */
	uu_dprint(UU_GITRC,(us,"ug_winviw. xform[%d]:",xform));
	ug_matprt(ug_cxform[xform]);				/* print ug_cxform[xform] */
rtn:	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_mat3mp(r,a,b,c) -- r = a x b x c.
**        Multiply matrices a x b x c, checking if a or b is identity.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_mat3mp(r,a,b,c)
Gtran r;							/* result r=a x b x c */
Gtran a,b,c;					/* matrices to multiply */
{
	Gtran tmp;
	int i;
	i=2*ug_mident(a) + ug_mident(b);
	switch(i) {
	case 0:						/* neither a nor b is identity */
		ug_matmp(tmp,a,b);
		ug_matmp(r,tmp,c);
		break;
	case 1:						/* b is identitiy */
		ug_matmp(r,a,c);
		break;
	case 2:						/* a is identity */
		ug_matmp(r,b,c);
		break;
	case 3:						/* both a and b are identity */
		ug_mcopy(r,c);
		break;
  }
}	
