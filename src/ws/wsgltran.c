#include "zsysdep.h"
#ifdef UU_OPENGL

/*********************************************************************
**  NAME:  wsgltran.c
**
**      GKS OpenGL workstation.
**
**       CONTAINS:
**				
**    uw_glwswind(prms)
**    uw_glwsvport(prms)
**    uw_glnormtran(prms,reply)
**    uw_glntran(n)
**    uw_glintersect(c,a,b)
**    uw_glrest(n)
**    uw_glvref3(prms,reply) 
**    uw_glvpn3(prms,reply)
**    uw_glcvpn( vpn, phix, phiy )
**    uw_glvup3(prms,reply)
**    uw_glmodxf(prms,reply)
**    uw_glwsxform(wstran,wsxform,ws)
**    uw_glndectowc(ndc,wc)
**    uw_glwind3(prms,reply) 
**    uw_glvport3(prms,reply)
**    uw_glw2v( wndw, vprt, s, t )
**		uw_glndctodev(f,i)
**		uw_gldevtondc(rast,ndc)
**		uw_gldevxtondc(rast)
**		uw_gldevytondc(rast)
**	
**     MODULE NAME AND RELEASE LEVEL
**       wsgltran.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:09
*********************************************************************/

#ifdef VMS
#include <decw$include:Xlib.h>
#else
#if UU_COMP != UU_WIN2K 
#include <X11/Xlib.h>
#endif
#endif


#include <math.h>
#include "wsgl.h"
#include "udebug.h"					/* GKS/PHIGS pkg defs */
#include "gobas.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "gmat4.h"
#include "xenv1.h"
#include "ws.h"
#if UU_COMP == UU_WIN2K
#include "wsntglfunc.h"
#endif
#include "gtblvar6.h"
#include "lipv.h"
extern UG_wdt glwdt;
void uw_glntran();
void uw_glintersect();
void uw_glrest();
void uw_glcvpn();
void uw_glwsxform();
void uw_glw2v();
void  uw_gldevtondc();
void uw_glndctodev();
extern int IPV_resize;

/*********************************************************************
**    I_FUNCTION     :   uw_glwswind(prms)
**       Sets workstation window.
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

typedef struct { Gint op; Gws ws; Gnrect3 wswind;} Swindrect;
void uw_glwswind(prms)
Swindrect *prms;
{
	int i;

	uu_denter(UU_GITRC,(us,"uw_4dwswind(%g %g %g %g)",(*prms).wswind.llf.x,
				(*prms).wswind.llf.y,(*prms).wswind.urb.x,(*prms).wswind.urb.y));

	zbytecp((*ug_gksstli.wsopen[(*prms).ws].outptr).curxform.w,(*prms).wswind);

	/* update wsxform, the workstation transformation*/
	uw_glwsxform(&(*ug_gksstli.wsopen[(*prms).ws].outptr).curxform,
				  &uw_gl.wsxform,(*prms).ws);
/*
........call uw_glntran for each xform to calculate:
........clip=intersection of current viewport and ws_window;
........scrn_clip=result of applying uw_wsxform to clip;
........VIEWPORT(scrn_clip);
*/
	if (IPV_resize)
	{
		uw_glntran(LW_vport.xform);
		return;
	}
	for( i = 0 ;  i < NOTRANS ; ++i )
		uw_glntran( i );
	ug_setredrwflag(1);
	uu_dexit;

}

/*********************************************************************
**    I_FUNCTION     :  uw_glwsvport(prms)
**       Sets workstation viewport.
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

typedef struct {Gint op; Gws ws; Gdrect3 wsvport;} Svport;
void uw_glwsvport(prms)
Svport *prms;
{
	int i;

	uu_denter(UU_GITRC,(us,"uw_glwsvport(%g %g %g %g)",(*prms).wsvport.llf.x,
				(*prms).wsvport.llf.y,(*prms).wsvport.urb.x,(*prms).wsvport.urb.y));


	zbytecp((*ug_gksstli.wsopen[(*prms).ws].outptr).curxform.v,(*prms).wsvport);
         /* structure assignment */
	uw_glwsxform(&(*ug_gksstli.wsopen[(*prms).ws].outptr).curxform,
				  &uw_gl.wsxform,(*prms).ws);
/*
........call uw_glntran to calculate:
........clip=intersection of current viewport and ws_window;
........scrn_clip=result of applying uw_wsxform to clip;
........VIEWPORT(scrn_clip);
*/
	for(i=0; i<NOTRANS; ++i)
		uw_glntran(i);
	ug_setredrwflag(1);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  uw_glnormtran(prms,reply)
**       Selects normalization transformation.
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          reply
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

typedef struct { Gint op; Gws id; Gint xf; Gnrect3 vp; } Snvp;
void uw_glnormtran(prms,reply)			/* select normalization transform */
Snvp *prms;
int reply[];									/* no reply parameters */
{
	/* Change the current normalization transformation */

	uu_denter(UU_GITRC,(us,"uw_glnormtran(%d)",prms->xf));
	if( uw_gl.rasmode == 1 ) {
		uu_dprint(UU_GITRC,(us,"rasmode escape, no matrix called"));
		uu_dexit;
		return;
	}
/*
.....Store the current matrix
*/
	uw_glload_matrix(prms->xf,UU_TRUE,uw_gl.modxf);
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  uw_glntran(n)
**       Set up a workstation window.
**    PARAMETERS   
**       INPUT  : 
**          n:   window number
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_glntran(n)
int n;									/* n= transformation # to use */

/*	calculate clip=intersection of current viewport and ws_window;
	uw_glscrnclip=result of applying uw_wsxform to clip;
	VIEWPORT(uw_glscrnclip);
	LOADMATRIX(current viewing xform) */

{
	GLint t,b,l,r;
	
	if(ug_clip2flag==1)
		uw_glintersect(&uw_glclip,&ug_gksstli.vtran[n].vport,
			&ug_clip2rect[n]);
	else
		uw_glintersect(&uw_glclip,&ug_gksstli.vtran[n].vport,
			&(*ug_gksstli.wsopen[uw_gl.wid].outptr).curxform.w);
	uu_dprint(UU_GITRC,(us,"uw_glntran(%d) clip=llf=%g%g %g urb=%g %g %g",n,
			uw_glclip.llf.x,uw_glclip.llf.y,uw_glclip.llf.z,
			uw_glclip.urb.x,uw_glclip.urb.y,uw_glclip.urb.z));
	uw_glndctodev(&uw_glclip.llf,&uw_glscrnclip[0]);
	uw_glndctodev(&uw_glclip.urb,&uw_glscrnclip[1]);

	t=uw_glscrnclip[1].y;
	b=uw_glscrnclip[0].y;
	l=uw_glscrnclip[0].x;
	r=uw_glscrnclip[1].x;

	uw_gltag_vpt(n,l,b,r-l,t-b);
	uw_glscrn_vport[n].x = l;
	uw_glscrn_vport[n].y = b;
	uw_glscrn_vport[n].width = r-l; 
	uw_glscrn_vport[n].height = t-b;   
	uu_dprint(UU_GITRC,(us,"uw_glntran viewport(ll=%d %d ur=%d %d)",
			uw_glscrnclip[0].x,uw_glscrnclip[0].y,
			uw_glscrnclip[1].x,uw_glscrnclip[1].y));

	uw_glrest(n);					/* loadmatrix with current viewing xform */

}

/*********************************************************************
**    I_FUNCTION     :  uw_glintersect(c,a,b)
**       Calculate c = intersection of a and b.
**    PARAMETERS   
**       INPUT  : 
**          a, b
**       OUTPUT :  
**          c
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_glintersect(c,a,b)		/* calculate c = intersection of a and b */
Gwrect3 *c,*a,*b;
{
	(*c).llf.x= MAX((*a).llf.x,(*b).llf.x);
	(*c).urb.x= MIN((*a).urb.x,(*b).urb.x);
	(*c).llf.y= MAX((*a).llf.y,(*b).llf.y);
	(*c).urb.y= MIN((*a).urb.y,(*b).urb.y);
	(*c).llf.z= MAX((*a).llf.z,(*b).llf.z);
	(*c).urb.z= MIN((*a).urb.z,(*b).urb.z);
	
}


/*********************************************************************
**    I_FUNCTION     :  uw_glrest(n)
**	 Calculate K=matrix to map clip to 2-unit square.
**    PARAMETERS   
**       INPUT  : 
**          n		matrix that changes
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glrest(n)
int n;
{
	/* calculate K=matrix to map clip to 2-unit square. This is almost
		the matrix ORTHO would have calculated */

	Gfloat left,right,bottom,top,near1,far1;

	uu_denter(UU_GITRC,(us,"uw_glrest wind=%d",n)); uu_dexit;

	left   = uw_glclip.llf.x;		right = uw_glclip.urb.x;
	bottom = uw_glclip.llf.y; 		top   = uw_glclip.urb.y;
	near1   = -uw_glclip.llf.z;		far1   = -uw_glclip.urb.z;
	uw_gltag_ortho(n,left,right,bottom,top,near1,far1);
	ug_setredrwflag(1);	/* Redraw at next interaction */
}


/*********************************************************************
**    I_FUNCTION     :  uw_glvref3(prms,reply) 
**       Set view reference point 3D
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
typedef struct { Gint op; Gws id; Gint xf; Gwpoint3 pt; } Svref;
void uw_glvref3(prms,reply)
Svref *prms;
int reply[];						/* no reply parameters */
{

	uu_denter(UU_GITRC,(us,"uw_glvref3(%g %g %g)",(*prms).pt.x,(*prms).pt.y,
				(*prms).pt.z));

	uu_dprint(UU_GITRC,(us,"translate %f %f %f", -prms->pt.x, -prms->pt.y,
		-prms->pt.z));



	uw_gltag_trn(prms->xf,-prms->pt.x,-prms->pt.y,-prms->pt.z);
/*	ug_setredrwflag(1);*/
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  uw_glvpn3(prms,reply)
**       set view plane (3D)
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

typedef struct { Gint op; Gws id; Gint xf; Gwpoint3 pl; } Svpn;
void uw_glvpn3(prms,reply)				
Svpn *prms;
int reply[];						/* no reply parameters */
{

	Gfloat phix, phiy;
	GLdouble  x, y;

	uu_denter(UU_GITRC,(us,"uw_glvpn3(%g %g %g xf=%d)",
		prms->pl.x, prms->pl.y, prms->pl.z, prms->xf));

	/* Save the current object */
	/* Calculate the necessary rotation angles */
	uw_glcvpn(&(prms->pl), &phix, &phiy);
	x = phix * RTOD  ;
	y = phiy * RTOD  ;

	uu_dprint(UU_GITRC,(us,"rotate x, y %f %f (deg)",phix*RTOD,phiy*RTOD));

	/* Edit these rotations into appropriate normalization transform */
	uw_gltag_vpn(prms->xf,x,y);
/*	ug_setredrwflag(1);*/

	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  uw_glcvpn( vpn, phix, phiy )
**       Calculates the rotation angles phix, phiy which place a
**			vector vpn onto the z-axis:
**			<vpn>[rotate y by phiy][rotate x phix] = <0,0,1>
**			normal vpn.
**    PARAMETERS   
**       INPUT  : 
**          vpn		View plane normal
**       OUTPUT :  
**          phix		X-axis rotation value
**				phiy		Y-axis rotation value
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_glcvpn( vpn, phix, phiy )
Gwpoint3 *vpn;
Gfloat *phix, *phiy;
{

Gfloat len, r;
Gfloat ug_atan2();

	uu_denter(UU_GITRC,(us,"uw_glcvpn %f %f %f",vpn->x,vpn->y,vpn->z));

	/* calculate angles phix, phiy... rotations to set vpn */
	len = sqrt((vpn->x)*(vpn->x) +
			  	  (vpn->y)*(vpn->y) +
				  (vpn->z)*(vpn->z));				/* length of vp normal */

	r = sqrt(len*len - vpn->y*vpn->y);		/* proj len in xz plane*/

	uu_dprint(UU_GITRC,(us,"len= %f r = %f",len, r));

	/* calc y-axis rotation angle phiy */
	*phiy = -ug_atan2( vpn->x, vpn->z );

	/* calc x-axis rotation angle */
	*phix = ug_atan2( vpn->y, r );

	uu_dprint(UU_GITRC,(us,"cvpn returns x, y %f %f (deg)",
		(*phix)*RTOD,(*phiy)*RTOD));

	uu_dexit;

}

/*********************************************************************
**    I_FUNCTION     :  uw_glvup3(prms,reply)
**       Set view up-vector.
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

typedef struct { Gint op; Gws id; Gfloat dxup, dyup, dzup; int xf; } Svup;
void uw_glvup3(prms,reply)
Svup *prms;
int reply[];
{
	GLdouble phiz;					/* Up vector rotation value */
	Gfloat vup[3], vup2[3];	/* Temporary view up vectors */
	Gtran a;						/* Temporary rotation matrix */
	Gfloat ug_atan2();

	uu_denter(UU_GITRC,(us,"uw_glvup3(%g %g %g xf=%d)",(*prms).dxup,
				(*prms).dyup,(*prms).dzup,prms->xf));


	/* Calculate the matrix which rotates view plane normal to z-axis */
	ug_ident(a);
	ug_vectoz(a, &(ug_gksstli.vtran[prms->xf].vpnorm));

	/* Rotate vup though these angles */
	vup[0] = prms->dxup;
	vup[1] = prms->dyup;
	vup[2] = prms->dzup;

	uu_dprint(UU_GITRC,(us,"matrix is:"));
	uu_dprint(UU_GITRC,(us,"vup is %g %g %g",vup[0],vup[1],vup[2]));
	ug_vecxform(vup2, vup, a);
	uu_dprint(UU_GITRC,(us,"new vup2 is %g %g %g",vup2[0],vup2[1],vup2[2]));

	/* Now rotate vup into the y-axis */
	phiz =  RTOD * ug_atan2(vup2[0], vup2[1]);

	uu_dprint(UU_GITRC,(us,"rotation about z of %d",phiz));
	
	uw_gltag_vup(prms->xf,phiz);
/*	ug_setredrwflag(1); */
	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  uw_glmodxf(prms,reply)
**       Set modeling matrix.
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmodxf()
{
	int i, j;
	Gfloat a[4][4];

	uu_denter(UU_GITRC,(us,"uw_glmodxf()"));

/* 
........Get the new modeling matrix 
*/
	gqmodxf(a);

/*
........Copy the gks matrix into the iris matrix
*/
	for(i=0; i<4; i++) 
	{
		for(j=0; j<4; j++) 
		{
			uw_gl.modxf[ j+(i*4) ] = a[i][j];
		}
	}

/*
........Call the current normtran
*/
	uw_glload_matrix(ug_gksstli.curvwindex,UU_TRUE,uw_gl.modxf);
	uu_dexit;
					
}


/*********************************************************************
**    I_FUNCTION     :  static uw_glwsxform(wstran,wsxform,ws)
**       Perform the window to viewport map.
**    PARAMETERS   
**       INPUT  : 
**          wstran, ws
**       OUTPUT :  
**          wsxform
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : This is just a window to viewport map.
**							The lower left corner of ndc space (0.0, 0.0) 
**							maps to the lower left corner of screen
**							space (-0.5, -0.5)!  The upper right corner of
**							ndc (1.0, aspect) maps to the upper right
**							corner of screen space (DEVXMAX+.5, DEVYMAX+.5).
**							
*********************************************************************/

typedef struct {Gfloat sf; Gfloat dx,dy;} Sxform;
void uw_glwsxform(wstran,wsxform,ws)	
Gwstran3 *wstran;					/* workstation window and viewport */
Sxform *wsxform;
Gws ws;
{
	Gfloat s1,s;
	int ssize;
	UU_REAL r1,r2;
	ssize = MAX(glwdt.dspsize.raster.x,glwdt.dspsize.raster.y);
	s1 = MAX((*ug_gksstli.wsopen[ws].wdtptr).dspsize.device.x, (*ug_gksstli.wsopen[ws].wdtptr).dspsize.device.y);
	s = ssize/s1;
/*
   sx=((*wstran).v.urb.x-(*wstran).v.llf.x)/
                                 ((*wstran).w.urb.x-(*wstran).w.llf.x);
   sy=((*wstran).v.urb.y-(*wstran).v.llf.y)/
                                 ((*wstran).w.urb.y-(*wstran).w.llf.y);

   if (sx<sy) s1=sx;
   else s1=sy;
	s=s1*(glwdt.dspsize.raster.x)/(*ug_gksstli.wsopen[ws].wdtptr).dspsize.device.x;
*/
	(*wsxform).sf=s;	/* save composite scale factor */
	(*wsxform).dx=0;
	(*wsxform).dy=0;
/*
.......Set up the new font sizes
.......in comparison the the new window size
*/
#if UU_COMP != UU_WIN2K
   uw_glfont.chrwid = (float)uw_glfont.chrpwid / (float)uw_gl.xpixels;
   uw_glfont.chrhgt = (float)uw_glfont.chrphgt / (float)uw_gl.ypixels;
   uw_glfont.offset = (float)(uw_glfont.poffset) / uw_gl.xpixels;
   r1 = uw_gl.ypixels ; r2 = uw_glfont.chrphgt;
	glwdt.rowmax = r1 / r2;
	r1 = uw_gl.xpixels ; r2 = uw_glfont.chrpwid;
	glwdt.colmax = r1 / r2;
#else
   uw_glfont.chrwid = (float)uw_glfont.chrpwid / (float)uw_gl.xpixels;
   uw_glfont.chrhgt = (float)uw_glfont.chrphgt / (float)uw_gl.ypixels;
   uw_glfont.offset = (float)(uw_glfont.poffset) / uw_gl.xpixels;
   r1 = uw_gl.ypixels ; r2 = uw_glfont.chrphgt;
	glwdt.rowmax = (int)(r1 / r2);
	r1 = uw_gl.xpixels ; r2 = uw_glfont.chrpwid;
	glwdt.colmax = (int)(r1 / r2);
#endif
	uu_denter2(UU_GITRC,(us," uw_glwsxform. dx,dy=%g %g, scale=%g",
					(*wsxform).dx,(*wsxform).dy,(*wsxform).sf));
	uu_dexit;
}
/*********************************************************************
**    I_FUNCTION     :   uw_glwind3(prms,reply) 
**       Set window 3D.
**    PARAMETERS   
**       INPUT  : 
**          prms: store info about window
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

typedef struct { Gint op; Gws id; Gint xf; Gwrect3 wn; } Swind;
void uw_glwind3(prms,reply)
Swind *prms;
int reply[];						/* no reply parameters */
{
	int xf;
	float s[3], t[3];
	uu_denter(UU_GITRC,(us,"uw_glwind3(%d, %g %g %g, %g %g %g)",(*prms).xf,
			(*prms).wn.llf.x,(*prms).wn.llf.y,(*prms).wn.llf.z,
			(*prms).wn.urb.x,(*prms).wn.urb.y,(*prms).wn.urb.z));
		
	xf = prms->xf;

/* 
........calculate window and viewport parameters
*/
	uw_glw2v( &(prms->wn) , &(ug_gksstli.vtran[xf].vport) , s , t );

	uu_dprint(UU_GITRC,(us,"translate %f %f %f",t[0],t[1],t[2]));
	uu_dprint(UU_GITRC,(us,"scale     %f %f %f",s[0],s[1],s[2]));


	uw_gltag_w2v(xf,t[0],t[1],t[2]);
	uw_gltag_s1(xf,s[0],s[1],s[2]);
/*	ug_setredrwflag(1); */

	uu_dexit;
}

/*********************************************************************
**    I_FUNCTION     :  uw_glvport3(prms,reply)
**       Set viewport 3D.
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

typedef struct { Gint op; Gws id; Gint xf; Gnrect3 vp; } Svprect;
void uw_glvport3(prms,reply)
Svprect *prms;
int reply[];									/* no reply parameters */
{
	int xf;
	float s[3], t[3];
	uu_denter(UU_GITRC,(us,"uw_glvport3(%d, %g %g %g, %g %g %g)",(*prms).xf,
			(*prms).vp.llf.x,(*prms).vp.llf.y,(*prms).vp.llf.z,
			(*prms).vp.urb.x,(*prms).vp.urb.y,(*prms).vp.urb.z));


	xf = prms->xf;
		  
	/* calculate the new scale and translation */
	uw_glw2v( &(ug_gksstli.vtran[xf].window),&(prms->vp) , s, t );

	uu_dprint(UU_GITRC,(us,"translate %f %f %f",t[0],t[1],t[2]));
	uu_dprint(UU_GITRC,(us,"scale     %f %f %f",s[0],s[1],s[2]));

	ug_setredrwrect(&(*prms).vp);

	uw_gltag_w2v(xf,t[0],t[1],t[2]);
	uw_gltag_s1(xf,s[0],s[1],s[2]);

	uw_glntran(xf);		/* load matrix K with current composite xform */
	ug_setredrwflag(1); 
	uu_dexit;
	
}

/*********************************************************************
**    I_FUNCTION     :  uw_glw2v
**       Calculates the scale and translation necessary to do the
**			window to viewport mapping.
**    PARAMETERS   
**       INPUT  : 
**          wndw			The 3D window.
**				vprt			The 3D viewprt.
**       OUTPUT :  
**          s				The necessary scale factors.
**				t				The necessary translations.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_glw2v( wndw, vprt, s, t )
Gnrect3 *vprt;
Gwrect3 *wndw;
float s[3], t[3];					
{

	int i;
	Gfloat dv, dw;
	Gfloat ug_atan2();
	Gfloat vwport[3][3], wind[3][3];

	uu_denter(UU_GITRC,(us,"uw_glw2v"));

	/* make a temporary copy of viewport and window in an array for loops */
	vwport[0][0] = vprt->llf.x;
	vwport[0][1] = vprt->urb.x;
	vwport[1][0] = vprt->llf.y;
	vwport[1][1] = vprt->urb.y;
	vwport[2][0] = vprt->llf.z;
	vwport[2][1] = vprt->urb.z;
	wind[0][0]   = wndw->llf.x;
	wind[0][1]   = wndw->urb.x;
	wind[1][0]   = wndw->llf.y;
	wind[1][1]   = wndw->urb.y;
	wind[2][0]   = wndw->llf.z;
	wind[2][1]   = wndw->urb.z;

	/* apply a scale and translation to map window to viewport */
	for (i=0; i<3; i++) {
		dv=vwport[i][1]-vwport[i][0];
		dw=wind[i][1]-wind[i][0];
		if ((fabs(dw)<0.0001)||(fabs(dv)<0.0001)) {		/* zero width */
			uu_dprint(UU_GITRC,(us,"ug_winviw error. window=%g %g %g, %g %g %g",
				wind[0][0],wind[1][0],wind[2][0],wind[0][1],wind[1][1],
				wind[2][1]));
			uu_dprint(UU_GITRC,(us," ug_winviw. viewport=%g %g %g, %g %g %g",
				vwport[0][0],vwport[1][0],vwport[2][0],vwport[0][1],vwport[1][1],
				vwport[2][1]));
		}
		s[i]= (float)(dv/dw);
		t[i]= (float)(vwport[i][0]-wind[i][0]*s[i]);
	}
	s[0]=s[1];
	uu_dexit;

}
/*********************************************************************
**    I_FUNCTION     :  uw_gldevytondc(rast)
**       Translate raster cord to normalized cord
**    PARAMETERS   
**       INPUT  : 
**          rast
**       OUTPUT :  
**          ndc
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

Gfloat uw_gldevytondc(rast)
int rast;
{

Gfloat ndcy;
	ndcy = (rast - uw_gl.wsxform.dy) / uw_gl.wsxform.sf;
	return ndcy;
}

/*********************************************************************
**    I_FUNCTION     :  uw_gldevxtondc(rast,ndc)
**       Translate raster cord to normalized cord
**    PARAMETERS   
**       INPUT  : 
**          rast
**       OUTPUT :  
**          ndc
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

Gfloat uw_gldevxtondc(rast)
int rast;
{
Gfloat ndcx;
	ndcx = (rast - uw_gl.wsxform.dx) / uw_gl.wsxform.sf;
	return ndcx;
}

/*********************************************************************
**    I_FUNCTION     :  uw_gldevtondc(rast,ndc)
**       Translate raster cord to normalized cord
**    PARAMETERS   
**       INPUT  : 
**          rast
**       OUTPUT :  
**          ndc
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void  uw_gldevtondc(rast,ndc)
int rast[2];
Gfloat ndc[2];
{
	ndc[0] = (rast[0] - uw_gl.wsxform.dx) / uw_gl.wsxform.sf;
	ndc[1] = (rast[1] - uw_gl.wsxform.dy) / uw_gl.wsxform.sf;
	uu_denter2(UU_GITRC,(us,"uw_gldevtondc(%d %d to %g %g)",
		rast[0], rast[1], ndc[0], ndc[1]));
	uu_dexit;

}

/*********************************************************************
**    I_FUNCTION     :  uw_glndctodev(f,i)
**       Convert fx,fy from ndc to ix,iy device coordinates.
**    PARAMETERS  
**       INPUT  :
**          f
**       OUTPUT :
**          i
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glndctodev(f,i)
int i[2];
Gfloat f[2];
{
   Gfloat tmp[2];

   /* Apply the workstation transformation to f */
   tmp[0]=uw_gl.wsxform.sf*f[0]+uw_gl.wsxform.dx;
   tmp[1]=uw_gl.wsxform.sf*f[1]+uw_gl.wsxform.dy;

   /* Round to the nearest pixel.  At least at some positions on the
    * screen, the iris rounds x.5 to x, not x+1.  So we add .499
    * instead of .5.
    */
/*
.....Changed to use actual resolution
.....instead of hard coded numbers
.....Bobby  -  9/11/91
*/
   i[0] = (int)(tmp[0] + .499); /* i[0] = i[0] > DEVXMAX ? DEVXMAX : i[0]; */
   i[1] = (int)(tmp[1] + .499); /* i[1] = i[1] > DEVYMAX ? DEVYMAX : i[1]; */


   uu_denter2(UU_GITRC,(us,"uw_glndctodev(%g %g to %d %d)",
      f[0],f[1],i[0],i[1]));
   uu_dexit;

}
#endif
