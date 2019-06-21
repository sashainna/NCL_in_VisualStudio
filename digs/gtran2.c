/*********************************************************************
**    NAME         :  gkstran2.c --  DIGS viewing pipeline(cont).
**       CONTAINS:
**		gqmodxf(a) -- Inquire world coordinate matrix 3D.
**		Gerror gwndc3(nx,ny,nz,x,y,z) -- Map world to NDC coords.
**		gndcw3(x,y,z,nx,ny,nz) -- Map NDC to world coordinate (3D).
**		ug_sviewp(xform,v) -- set current viewing parameters.
**		ug_iview(v) -- inquire viewing parameters.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gtran2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:25
*********************************************************************/
#include "zsysdep.h"
#include "usysdef.h"
#include <math.h>
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
#include "dinput.h"
#include "udebug.h"
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gtran2.c 2.10 10/10/86 15:24:46 single"};
#else
static char uu_sccsident[]={"@(#) gtran2.c 2.10 10/10/86 15:24:46 double"};
#endif
#define UG_TRUE 1
#define UG_FALSE 0
#define Logical int
static Gfloat vwport[3][2],wind[3][2];	/* temporary copy for loops */

/********************************************************************* 
**  E_FUNCTION		:  gqmodxf(a) -- Inquire modelling xform.
**		PARAMETERS   
**      OUTPUT: Gfloat[4][4] a -- modelling xform.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
gqmodxf(a)                       /* inquire modelling xform. */
/*$ OUTPUT */
Gfloat a[4][4];
{ 
	int i,j;
	 uu_denter(UU_GTRC,(us,"gqmodxf(:"));

#if UU_DEBUG==1
	if (UU_debmask&UU_GITRC) {
		ug_matprt(ug_modxform);
	}
#endif

	for (i=0; i<4; i++) {
    for (j=0; j<4; j++) a[i][j]=ug_modxform[i][j];
  }
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION		:  gqlmodxf(a) -- Inquire modelling xform.
**		PARAMETERS   
**      OUTPUT: Gfloat[4][4] a -- modelling xform.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
gqlmodxf(a)                       /* inquire modelling xform. */
/*$ OUTPUT */
Gfloat a[4][4];
{ 
	int i,j;
	 uu_denter(UU_GTRC,(us,"gqlmodxf(:"));

#if UU_DEBUG==1
	if (UU_debmask&UU_GITRC) {
		ug_matprt(ug_lmodxform);
	}
#endif

	for (i=0; i<4; i++) {
    for (j=0; j<4; j++) a[i][j]=ug_lmodxform[i][j];
  }
	uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gwndc3(nx,ny,nz,x,y,z) -- Map world to NDC coords.
**  PARAMETERS   
**      INPUT:  Gfloat x,y,z -- world coordinate.
**      OUTPUT: Gfloat *nx,*ny,*nz -- NDC coordinate.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
gwndc3(nx,ny,nz,x,y,z)   /* map world to ndc 3d */
/*$ OUTPUT */
Gfloat *nx,*ny,*nz;
/*$ INPUT */
Gfloat x,y,z;
{
  Gfloat npos[3];

   uu_denter(UU_GTRC,(us,"gwndc3(nx,ny,nz,%3g %3g %3g)",x,y,z));

	ug_xform(x,y,z,npos,ug_cxform[ug_gksstli.curvwindex]);  /* npos=(x,y,z,1)*ug_cxform */
  (*nx)=npos[0]; (*ny)=npos[1]; (*nz)=npos[2];
  uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  gndcw3(x,y,z,nx,ny,nz) -- Map NDC to world coordinate (3D).
**  PARAMETERS   
**      INPUT:  Gfloat nx,ny,nz -- NDC coordinate.
**      OUTPUT: Gfloat *x,*y,*z -- world coordinate.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
gndcw3(x,y,z,nx,ny,nz)    /* map ndc to world 3d */
/*$ OUTPUT */
Gfloat *x,*y,*z;
/*$ INPUT */
Gfloat nx,ny,nz;
{
  uu_denter(UU_GTRC,(us,"gndcw3(x,y,z,%3g %3g %3g) curvwindex=%d",
	nx,ny,nz,ug_gksstli.curvwindex));
	ug_ndcw3(ug_gksstli.curvwindex,x,y,z,nx,ny,nz);
  uu_dexit;
}

/*********************************************************************
**    I_FUNCTION :  ug_ndcw3(n,x,y,z,nx,ny,nz)
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_ndcw3(n,x,y,z,nx,ny,nz)
int n;								/* xformation number */
/*$ OUTPUT */
Gfloat *x,*y,*z;
/*$ INPUT */
Gfloat nx,ny,nz;
{
  Gfloat npos[3];
  Logical ok;
  static char msg[] = {"gndcw3 can't invert ug_cxform.\n"};

	uu_denter(UU_GITRC,(us,"ug_ndcw3(%d,%d,%d,%d,nx,ny,nz)",n,x,y,z));
  if (ug_cxchg[n]==UG_TRUE) {
	 /* invert ug_cxform */
    ok=ug_invrt(ug_cxinv[n], ug_cxform[n]);   
    if (ok==UG_FALSE) {          /* ug_cxform not invertible */
		uu_dprint(-1,(us,"gndcw3 error. can't invert ug_cxform %d",n));
      uu_dexit; return;
    }

	 /* remember we inverted ug_cxform */
    ug_cxchg[n]=UG_FALSE;
  }

  ug_xform(nx,ny,nz,npos,ug_cxinv[n]);
  (*x)=npos[0]; (*y)=npos[1]; (*z)=npos[2];
  uu_dexit;
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gwndcvec3( newvec, vec ) -- 
**			Map world vector (not point) to NDC coords.
**  PARAMETERS   
**      INPUT:  Gfloat vec[3]    -- world coordinate vector.
**      OUTPUT: Gfloat newvec[3] -- NDC coordinate vector.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
gwndcvec3(newvec, vec)
/*$ OUTPUT */
Gfloat newvec[];
/*$ INPUT */
Gfloat vec[];
{
	int i, j;
	Gtran *a;

	uu_denter(UU_GTRC,(us,"gwndcvec3()"));

/* 	ug_vecxform(newvec,vec,ug_cxform[ug_gksstli.curvwindex]); */

	a = (Gtran *)ug_cxform[ug_gksstli.curvwindex];

	ug_matprt(a);

	ug_vecxform(newvec, vec, a);
   uu_dprint(UU_GTRC,(us,"gwndcvec3(%3g %3g %3g to %3g %3g %3g)",
		vec[0],vec[1],vec[2],newvec[0],newvec[1],newvec[2]));

/* Why don't we just do this commented loop? */
/*	for(i=0; i<3; i++) {
/*		newvec[i] = 0.0;
/*		for( j=0; j<4; j++ ) {
/*			newvec[i] += (*a)[i][j] * vec[i];
/*		}
/*	}
/*			
/*   uu_dprint(UU_GTRC,(us,"gwndcvec3(%3g %3g %3g to %3g %3g %3g)",
/*		vec[0],vec[1],vec[2],newvec[0],newvec[1],newvec[2]));
*/
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_sviewp(xform,v) -- set current viewing parameters.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_sviewp(xform,v)                 /* set current viewing parameters */
									/* not user callable */
UG_vtran3 *v;
Gint xform;
{
	 uu_denter(UU_GITRC,(us,"ug_sviewp(%d,v)",xform));

	zbytecp(ug_gksstli.vtran[xform],(*v));	/* structure assignment */ 
	ug_winviw(xform);
	ug_ndcntranboxdel(xform);	/* delete segment ndc bounding boxes */
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  ug_iview(v) -- inquire viewing parameters.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_iview(v)                     /* inquire viewing parameters */
										/* not user callable */
UG_vtran3 *v;
{
	 uu_denter(UU_GITRC,(us,"ug_iview(v)"));

	zbytecp(*v,ug_gksstli.vtran[ug_gksstli.curvwindex]);	/* structure assignment */
	uu_dexit;
}
