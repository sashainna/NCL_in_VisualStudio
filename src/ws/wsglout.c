#include "usysdef.h"
#ifdef UU_OPENGL

/*********************************************************************
**  NAME:  wsglout.c
**
**      GKS workstation: output functions section.
**
**    CONTAINS:
**          uw_glpolyln
**          uw_glpolyln3
**          uw_glrasline
**          uw_glline
**          uw_glpolymk
**          uw_glpolymk3
**          uw_glmarker
**          uw_glmarkerras
**          uw_glvis
**          uw_gltext
**          uw_glprintString
**          uw_glrastext
**          uw_glfillarea
**          uw_glfillarea3
**          uw_glfillras
**          uw_glshadearea
**				uw_getchrwdht
**				uw_gllabelbackground           
**				uw_glleaderline
**				uw_gllabelsz
**				uw_gloverlapdis
**
**    MODULE NAME AND RELEASE LEVEL 
**			wsglout.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:07
**    
*********************************************************************/

#if UU_COMP != UU_WIN2K 
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <Xm/Xm.h>
#endif
#include <math.h>

#include "udebug.h"
#include "zsysdep.h"
#include "driver.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "go1.h"
#include "gsegac.h"
#include "mdattr.h"
#if UU_COMP != UU_WIN2K 
#include "wsxw.h"
#include "wsmf.h"
#endif
#include "ginqatti.h"
#include "gmat4.h"
#include "mpocket.h"
#include "view.h"
#include "uims.h"
#include "wsgl.h"
#include "nccs.h"
#include "nclfc.h"
#include "lcom.h"
#include "wsglfun.h"
#include "nclmodals.h"
#if UU_COMP == UU_WIN2K 
#include "wsntglfunc.h"
#else
extern UWS_MF uw_mf;
#endif

typedef struct
{
	int id; Gws ws; int n; int type; Gwpoint3 *points;Gwpoint3 *norms;
} shade_struct;

typedef struct
{
	int id; Gws ws; Gipoint p; char *str;
} char_struct;

typedef struct
{
	int id; Gws ws; int n; Gipoint *points;
} ipoint_struct;

typedef struct
{
	int id; Gws ws; int n; Gwpoint *points;
} wpoint_struct;

typedef struct
{
	int id; Gws ws; int n; Gwpoint3 *points;
} w3point_struct;

extern UG_wdt glwdt;
extern int NCL_mot_seg;
extern int *UD_ksws;

void uw_glmarker();
void uw_gllabelbackground();
void uw_glprintString();
extern UU_LIST NCL_ldr_list;
extern int NCL_dyn_ldr ;

/*********************************************************************
**    E_FUNCTION : uw_glpolyln(prms,reply) -----> UG_DPOLYLN
**     
**    DESCRIPTION:
**            Draw the polyline specified in (*prms). 
**            The number of 2D world coordinates (untransformed) 
**            is contained in (*prms).points.
**
**    PARAMETERS   
**       INPUT  : 
**          prms: store info about the line
**       OUTPUT :  
**                              none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glpolyln(prms,reply)
wpoint_struct *prms;
int reply[];                    
{
	int i;

	if( ug_gksos.sysstate != UG_SGOP ) 
	{
/*
...set attributes
*/
		uw_gllighting(UU_FALSE);
		uw_glcolor(ug_gksstli.curprats.lnbundl.color);
		glBegin_d(GL_LINE_STRIP);
		for (i=0;i<(prms->n);i++)
		{
/*
.......Draw the line
*/
			uw_glmark_dirty_pt2(&(prms->points[i]));
			glVertex2f_d(prms->points[i].x,prms->points[i].y);
		}
		glEnd_d();
	}
}

/*********************************************************************
**    E_FUNCTION     :  uw_glpolyln3(prms,reply)
**    
**    DESCRIPTION:
**            Draw the 3D polyline specified in *prms.  (*prms).n is
**            the number of 3D world coordinates (untransformed)
**            contained in (*prms).points.
**
**    PARAMETERS   
**       INPUT  : 
**          prms: store info about the line
**       OUTPUT :  
**          reply
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_glpolyln3(prms)
w3point_struct *prms;
{
	int i;
	if( ug_gksos.sysstate != UG_SGOP ) 
	{ 
		uw_gllighting(UU_FALSE);
		uw_glcolor(ug_gksstli.curprats.lnbundl.color);
		glBegin_d(GL_LINE_STRIP);
		for (i=0; i<(*prms).n; i++)
		{
			uw_glmark_dirty_pt3(&(prms->points[i]));
			glVertex3f_d(prms->points[i].x,prms->points[i].y, prms->points[i].z);
		}
		glEnd_d();
	} 
}       

/*********************************************************************
**    E_FUNCTION : uw_glrasline(x1,y1,x2,y2)  -----> UG_DRASLINE
**       
**    DESCRIPTION:
**            draw a line between given raster coordinates
**
**    PARAMETERS   
**       INPUT  : 
**         x1,y1,x2,y2: dev cord of two points 
**       OUTPUT :  
**         none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glrasline(x1,y1,x2,y2)
int x1,y1,x2,y2;
{
	Gdpoint pt;
/*
.....Draw the line
*/
	if( ug_gksos.sysstate != UG_SGOP )
	{
		uw_gllighting(UU_FALSE);
		uw_glcolor(ug_gksstli.curprats.lnbundl.color);
		glBegin_d(GL_LINES);
			glVertex2i_d( x1 , y1 );
			glVertex2i_d( x2 , y2 );
		glEnd_d();        
		pt.x = x1; pt.y = y1;
		uw_glmark_dirty_dpt(&pt);
		pt.x = x2; pt.y = y2;
		uw_glmark_dirty_dpt(&pt);
	}
}

/*********************************************************************
**    E_FUNCTION : uw_glline(prms) ------------> UG_DDRWOP
**      
**    DESCRIPTION:
**            draw line from prms[2],prms[3] to [4],[5], NDC coordinates. 
**      
**    PARAMETERS   
**       INPUT  : 
**          prms: store info about the line
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glline(prms)
UG_line *prms;
{
   Gfloat p1[2],p2[2];
   int ip1[2],ip2[2];
	GLint viewport[4];
	GLdouble ortho[6];
	if( ug_gksos.sysstate != UG_SGOP )
	{
/*
.....Convert from NDC to device coordinates
*/
		p1[0] = prms->p1.x;     p1[1] = prms->p1.y;
		p2[0] = prms->p2.x;     p2[1] = prms->p2.y;
		uw_glndctodev(p1, ip1);
		uw_glndctodev(p2, ip2);
		uw_glpushx();

/*		
		glGetIntegerv_d( GL_VIEWPORT , viewport );
		vpt[0] = 0 ; vpt[1] = 0 ; vpt[2] = viewport[2];
		vpt[3] = viewport[3];
*/
		uw_glget_vport(viewport);
		uw_glViewport(viewport);
		ortho[0] = 0. ; ortho[1] = viewport[2] ; ortho[2] = 0.;
		ortho[3] = viewport[3] ; ortho[4] = -1000. ; ortho[5] = 1000.;
		uw_glOrtho(ortho);

		glLoadIdentity_d();
/*
.....Draw the line
*/
		uw_gllighting(UU_FALSE);
		uw_glcolor(ug_gksstli.curprats.lnbundl.color);
		glBegin_d(GL_LINES);
			glVertex2i_d( ip1[0] , ip1[1] );
			glVertex2i_d( ip2[0] , ip2[1] );
		glEnd_d();        
		uw_glmark_dirty_dpt(ip1);
		uw_glmark_dirty_dpt(ip2);
		uw_glpopx();
	}
						
}

/*********************************************************************
**    E_FUNCTION : uw_glpolymk(prms,reply) ------> UG_DPOLYMK
**      
**     DESCRIPTION:
**             draw the polymarker specified in (*prms). Use the current
**             polymarker attributes.
**             If a segment is open ( i.e. UG_DCRESEG or UG_DOPNSEG 
**             has been called), save the polymarker in the segment 
**             and don't actually draw it unless the segment is VISIBLE. 
**             (*prms).n is the number of 2D world coordinates (untransformed) 
**             contained in (*prms).points.
**
**    PARAMETERS   
**       INPUT  : 
**                              prms: store info about the marker
**       OUTPUT :  
**                                      none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glpolymk(prms,reply)
wpoint_struct *prms;
int reply[];
{
	int i,n;
	int mtype;
/*
.....Draw the marker if a segment
.....is not currently open
*/
	if (ug_gksos.sysstate != UG_SGOP)
	{
/*
.......Store number of markers &
.......Marker type
*/
		n=(*prms).n;
		mtype = ug_gksstli.curprats.mkbundl.type;
/*
.......Loop to draw the markers
*/
		for (i=0; i<n; i++)
		{
			uw_glmarker( prms->points[i].x, prms->points[i].y, 0.0, mtype );
			uw_glmark_dirty_pt2(&(prms->points[i]));
		}
	}
}

/*********************************************************************
**    E_FUNCTION :  uw_glpolymk3(prms,reply)  -------> UG_DPOLYMK3
**      
**    DESCRIPTION:
**            Draw the 3D polymarker specified in (*prms). Use the current
**            polymarker attributes.
**            If a segment is open ( i.e. UG_DCRESEG or UG_DOPNSEG 
**            has been called), save the polymarker in the segment 
**            and don't actually draw it unless the segment is VISIBLE. 
**            (*prms).n is the number of 3D world coordinates (untransformed) 
**            contained in (*prms).points. 
**
**    PARAMETERS   
**       INPUT  : 
**                              prms: store info about the marker
**       OUTPUT :  
**                              none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glpolymk3(prms,reply)
w3point_struct *prms;
int reply[];                            
{
	int i,n;
	int mtype;
/*
.....Draw the marker if a segment
.....is not currently open
*/
	if (ug_gksos.sysstate != UG_SGOP)
	{
/*
.......Store number of markers &
.......Marker type
*/
		n=(*prms).n;
		mtype = ug_gksstli.curprats.mkbundl.type;
/*
.......Loop to draw the markers
*/
		for (i=0; i<n; i++)
		{
/*
.........Draw the marker
*/
				uw_glmarker(prms->points[i].x, prms->points[i].y,
					   prms->points[i].z, mtype ); 
				uw_glmark_dirty_pt3(&(prms->points[i]));
		}
	}
}

/*********************************************************************
**    E_FUNCTION : uw_glmarker(x, y, z, type) ---------> UG_DPNTOP
**                      
**    DESCRIPTION:
**      draw a marker of type (*prms).type centered at point (*prms).p in 
**      world coordinates. Use the current marker color index. Use the
**      marker type specified in (*prms).type. The predefined marker types
**      are:
**              1 = a small dot.
**              2 = a plus sign.
**              3 = an asterisk (*).
**              4 = a zero (0).
**              5 = a capital X.
**              7 = a diamond.
**              8 = a square.
**
**    PARAMETERS   
**       INPUT  : 
**          x,y,z          World Coordinates to place marker at.
**          type           Type of marker to draw.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmarker(x,y,z,type)                 
Gfloat x, y, z;                              /* 3-D position of marker */
int type;                                    /* Type of marker */
{
	static GLubyte dot[] = {0x80};
	static GLubyte plus[] = {0x04, 0x00, 0x04, 0x00, 0x04,0x00, 0x04, 0x00, 
									 0x04, 0x00, 0xFF, 0xE0, 0x04, 0x00,0x04, 0x00,
									 0x04, 0x00, 0x04, 0x00, 0x04, 0x00};
	static GLubyte star[] = {0x04, 0x00, 0x04, 0x00, 0x24, 0x80, 0x15, 0x00,
									 0x0E, 0x00, 0xFF, 0xE0, 0x0E, 0x00, 0x15, 0x00,
									 0x24, 0x80, 0x04, 0x00, 0x04, 0x00};
	static GLubyte circle[] = {0x0E,0x00, 0x31,0x80, 0x40,0x40, 0x40,0x40,
							   	   0x80,0x20, 0x84,0x20, 0x80,0x20, 0x40,0x40, 
										0x40,0x40, 0x31,0x80, 0x0E,0x00};
	static GLubyte cross[] = {0x10,0x10, 0x08,0x20, 0x04,0x40,
										0x02,0x80, 0x01,0x00, 0x02,0x80, 0x04,0x40,
										0x08,0x20, 0x10,0x10, 0x00,0x00, 0x00,0x00, 0x00,0x00};
	static GLubyte triangle[] = {0x3F,0xF8, 0x20,0x08, 0x10,0x10, 0x10,0x10,
									 	0x08,0x20, 0x09,0x20, 0x04,0x40, 0x04,0x40, 
										0x02,0x80, 0x02,0x80, 0x01,0x00};								
	static GLubyte diamond[] = {0x04,0x00, 0x0A,0x00, 0x11,0x00, 0x20,0x80,
									 	0x40,0x40, 0x84,0x20, 0x40,0x40, 0x20,0x80, 
										0x11,0x00, 0x0A,0x00, 0x04,0x00};
	static GLubyte square[] = {0x00,0x00, 0x1F,0xF0, 0x10,0x10, 0x10,0x10,
										0x10,0x10, 0x11,0x10, 0x10,0x10, 0x10,0x10,
										0x10,0x10, 0x1F,0xF0, 0x00,0x00};
	static GLubyte dblcir[] = {0x07,0x80, 0x18,0x60, 0x20,0x10, 0x23,0x10,
										0x44,0x88, 0x48,0x48, 0x48,0x48, 0x44,0x88,
										0x23,0x10, 0x20,0x10, 0x18,0x60, 0x07,0x80};
	static GLubyte lrgdot[] = {0x00,0x00, 0x00,0x00, 0x07,0x00, 0x0F,0x80,
										0x1F,0xC0, 0x1F,0xC0, 0x1F,0xC0, 0x0F,0x80,
										0x07,0x00, 0x00,0x00, 0x00,0x00};
	static GLubyte cube[] = {0x00,0x00, 0x00,0x00, 0x3F,0x80, 0x3F,0x80,
										0x3F,0x80, 0x3F,0x80, 0x3F,0x80, 0x3F,0x80,
										0x3F,0x80, 0x00,0x00, 0x00,0x00};

	uu_denter(UU_GITRC,(us,"glmarker %f %f %f, type %d", x,y,z,type));
	uw_gllighting(UU_FALSE);
	uw_glcolor( ug_gksstli.curprats.mkbundl.color );
/*
.....Move to marker position and
.....select marker font
*/
	glRasterPos3f_d(x, y, z );
	
	switch( type-1 ){         
/*
......don't know why glCallList (uw_glprintString) is not works in marker's case,
......the marker with  glCallList (uw_glprintString) is much smaller and with wrong
......offset, maybe we has better way to fix this, but for now, I just using the 
......glBitmap call
*/
		case 0:  /* dot */
			glBitmap_d((GLsizei)1,(GLsizei)1,(GLfloat)0,
					(GLfloat)0,(GLfloat)1,(GLfloat)0, dot, 0);
			break;

		case 1: /* plus */
			glBitmap_d((GLsizei)11,(GLsizei)11,(GLfloat)5, 
					(GLfloat)6,(GLfloat)11,(GLfloat)0, plus, 1);
			break;

		case 2: /* star */
			glBitmap_d((GLsizei)11,(GLsizei)11,(GLfloat)5,
					(GLfloat)6,(GLfloat)11,(GLfloat)0, star, 2);
			break;

		case 3: /* circle */
			glBitmap_d((GLsizei)11,(GLsizei)11,(GLfloat)5,
					(GLfloat)6,(GLfloat)11, (GLfloat)0,circle, 3);
			break;

		case 4:  /* cross */
			glBitmap_d((GLsizei)13,(GLsizei)11,(GLfloat)7,
					(GLfloat)5,(GLfloat)0,(GLfloat)11, cross, 4);
			break;

		case 5:  /* triangle */
			glBitmap_d((GLsizei)13,(GLsizei)11,(GLfloat)7,
					(GLfloat)6,(GLfloat)0,(GLfloat)11,triangle, 5);
			break;
		
		case 6:  /* diamond */
			glBitmap_d((GLsizei)11,(GLsizei)11,(GLfloat)5,
					(GLfloat)6,(GLfloat)0,(GLfloat)11,diamond, 6);
			break;
		
		case 7:  /* square */
			glBitmap_d((GLsizei)13,(GLsizei)11,(GLfloat)7,
					(GLfloat)6,(GLfloat)0,(GLfloat)11, square, 7);
			break;

		case 8:  /* double circle */
			glBitmap_d((GLsizei)13,(GLsizei)13,(GLfloat)7,
					(GLfloat)6,(GLfloat)11,(GLfloat)0,dblcir, 8);
			break;
		
		case 9:  /* large dot */
			glBitmap_d((GLsizei)11,(GLsizei)11,(GLfloat)6,
					(GLfloat)6,(GLfloat)11,(GLfloat)0,lrgdot, 9);
			break;
		
		case 10:  /* cube */
			glBitmap_d((GLsizei)11,(GLsizei)11,(GLfloat)5,
					(GLfloat)6,(GLfloat)11,(GLfloat)0,cube, 10);
			break;
		
		default:
			glBitmap_d((GLsizei)1,(GLsizei)1,(GLfloat)0,
					(GLfloat)0,(GLfloat)1,(GLfloat)0, dot, 0);
			break;
	}
}

/*********************************************************************
**    E_FUNCTION : uw_glmarkerras(x,y,mkbundl) ----> UG_DMARKERRAS
**       
**    DESCRIPTION :
**            Draws a "raster" marker.
**    PARAMETERS   
**       INPUT  : 
**          x,y      =     raster coordinates of marker
**          *mkbundl =     marker bundle
**       OUTPUT :  
**              output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmarkerras(rasx,rasy,mkbundl)
int rasx,rasy;
Gmkbundl *mkbundl;      
{
	GLint vpt[4];
	GLdouble ortho[6];
	Gdpoint pt;
/*
.....Set line thickness
.....Bobby  -  5/19/94
*/
	if( ug_gksos.sysstate != UG_SGOP )
	{
/*
..... Push an identity matrix if not in rasmode 
*/
		if( uw_gl.rasmode != 1 )
		{
			uw_glpushx();

			vpt[0] = 0 ; vpt[1] = 0 ;
			vpt[2] = glwdt.dspsize.raster.x ; vpt[3] = glwdt.dspsize.raster.y;
			uw_glViewport(vpt);
			ortho[0] = 0. ; ortho[1] = glwdt.dspsize.raster.x ; ortho[2] = 0.;
			ortho[3] = glwdt.dspsize.raster.y ; ortho[4] = -1000. ; ortho[5] = 1000.;
			uw_glOrtho(ortho);
			glLoadIdentity_d(); 
		}
/*
.....need cast to diff variable
.....Yurong 9/30/97
*/
		uw_glmarker((Gfloat)rasx, (Gfloat)rasy, 0.0, (int)mkbundl->type );      
		pt.x = rasx; pt.y = rasy;
		uw_glmark_dirty_dpt(&pt);
		if( uw_gl.rasmode != 1 ) uw_glpopx();
	}
}

/*********************************************************************
**    E_FUNCTION     :  uw_glvis(prms,reply)
**
**    DESCRIPTION :
**       Set visibility.
**    PARAMETERS   
**       INPUT  : 
**          prms[2]: segment number
**       OUTPUT :  
**                              none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glvis(prms,reply)
int prms[],reply[];
{
/*
.....Visible motion segment
*/
	if( prms[3] == (int)UG_VISIBLE )
	{
		if (prms[2] == NCL_mot_seg)
		{
			ncl_display_motion(-1,0,0,0,UU_TRUE,UU_FALSE,UU_NULL);
		}
		else
		{
			uw_glviewsg(prms[2]);
		}
	}
/*
.....Invisible segment
*/
	else 
	{
		uw_glinvseg(prms[2]);
	}
}

/*********************************************************************
**    E_FUNCTION     :  uw_glrasput(rasll,rasur,savmem,rule)
**       Display a raster rectangle. Here only clear a raster rectangle.
**    PARAMETERS
**       INPUT  :
**          prms
**       OUTPUT :
**          reply
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glrasput(rasll,rasur,savmem,rule)
int rasll[2];
int rasur[2];
int *savmem;
int rule;
{
/*
.....Clear rectangle
*/
	if(savmem ==NULL)
	{
		uw_glclear_rect(rasll,rasur);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     :  uw_gltext(prms)
**       Put up text at 3D position.
**    PARAMETERS   
**       INPUT  : 
**          prms
**       OUTPUT :  
**          reply
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gltext(prms)     
UG_dtext *prms;
{
	int color;
	Gfloat xh, yh, zh;
	Gfloat xv, yv, zv;
	Gfloat x0, y0, z0;
	Gdpoint pt;
	Gtxhor txal;
	Gtxver tyal; 
	uu_denter(UU_GITRC,(us,"uw_gltext(%g %g %g,%s)",(*prms).pos.x,
	(*prms).pos.y, (*prms).pos.z,(*prms).s));

	if( ug_gksos.sysstate == UG_SGOP ) 
	{
		uu_dexit;
		return;
	}
/*
.....Display background box for labels
*/
	if(ug_gksstli.curprats.txbundl.fp.prec == UG_STRING ||
			ug_gksstli.curprats.txbundl.fp.prec == UG_CHAR) 
	{
		if (UW_bkg_on!=2 && NCL_labeldisp )
			uw_gllabelbackground((Gfloat)prms->pos.x, (Gfloat)prms->pos.y,
				(Gfloat)prms->pos.z,prms->s); 
	}
/*
.....Turn off lighting
*/
	uw_gllighting(UU_FALSE);
/*
.....Hardware text
*/
	if(ug_gksstli.curprats.txbundl.fp.prec == UG_STRING ||
			ug_gksstli.curprats.txbundl.fp.prec == UG_CHAR) 
	{

		/*  Find offset x or y for text alignment */
		gndcw3(&x0, &y0, &z0, 0.0, 0.0, 0.0);
		txal = ug_gksstli.curprats.txalign.hor;
		tyal = ug_gksstli.curprats.txalign.ver;
		xh = yh = 0.0;
		xv = yv = 0.0;
		if( txal == UG_TH_CENTRE ) 
		{
			gndcw3(&xh, &yh, &zh, strlen(prms->s) * (uw_glfont.chrwid/2), 0.0, 0.0);
			xh -= x0;
		}
		else if( txal == UG_TH_RIGHT ) 
		{
			gndcw3(&xh, &yh, &zh, strlen(prms->s) * (uw_glfont.chrwid), 0.0, 0.0);
			xh -= x0;
		}
		if( tyal == UG_TV_TOP ) 
		{
			gndcw3(&xv, &yv, &zv, 0.0, uw_glfont.chrhgt, 0.0);
			yv -= y0;
		}
		else if( tyal == UG_TV_HALF ) 
		{
			gndcw3(&xv, &yv, &zv, 0.0, uw_glfont.offset/2, 0.0);
			yv -= y0;
		}
		color = ug_gksstli.curprats.txbundl.color;
		uw_glcolor(color);

		glRasterPos3f_d(prms->pos.x-xh, prms->pos.y-yv, prms->pos.z );
		uw_glprintString(prms->s, uw_gltext_base);              /* Print string */
		
		pt.x = prms->pos.x; pt.y = prms->pos.y;
		uw_glmark_dirty_dpt(&pt);
		pt.x = pt.x + uw_glfont.chrpwid * strlen(prms->s);
		pt.y = pt.y + UW_label_size[0];
		uw_glmark_dirty_dpt(&pt);
	}
/*
.....Hershey text
*/
	else 
		ug_hershy(prms,0);
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :  uw_glprintString(s)
**       Put up text at current position.
**    PARAMETERS   
**       INPUT  : 
**          string s
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glprintString(s, font_base)
char *s;
GLuint font_base;
{
	glPushAttrib_d(GL_LIST_BIT);
	glListBase_d(font_base);
	glCallLists_d(strlen(s), GL_UNSIGNED_BYTE, (unsigned char *)s);
	glPopAttrib_d();
}
/*********************************************************************
**    E_FUNCTION     :  uw_glrastext(prms)
**       Display graphics text at raster coordinates.
**    PARAMETERS   
**       INPUT  : 
**          prms->id        Opcode of this command.
**          prms->ws        Workstation
**          prms->Gipoint   Raster coordinates of lower left corner of text.
**          str             Character string to display.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_glrastext(prms, reply)
char_struct *prms;
int reply[];
{
	int x,y;
	UG_dtext hershyprms;
	Gtxhor txal;
	Gtxver tyal;
	GLint vpt[4];
	GLdouble ortho[6];
	Gdpoint pt;
	
	uu_denter(UU_GITRC,(us,"uw_glrastext(%d %d, %s)",
				prms->p.x,prms->p.y,prms->str));

	if( ug_gksos.sysstate == UG_SGOP ) {
		uu_dexit;
		return 0;
	}

	x=prms->p.x; y=prms->p.y;                                       /* Text position */

	if(ug_gksstli.curprats.txbundl.fp.prec == UG_STRING) {
	
		if( uw_gl.rasmode != 1 )
		{
			uw_glpushx();

			vpt[0] = 0 ; vpt[1] = 0 ; vpt[2] = glwdt.dspsize.raster.x;
			vpt[3] =  glwdt.dspsize.raster.y;
			uw_glViewport(vpt);
			ortho[0] = 0. ; ortho[1] = glwdt.dspsize.raster.x ; ortho[2] = 0.;
			ortho[3] = glwdt.dspsize.raster.y ; ortho[4] = -1000. ; ortho[5] = 1000.;
			uw_glOrtho(ortho);

			glLoadIdentity_d(); 
		}

		uw_glcolor( ug_gksstli.curprats.txbundl.color );
	
		/*  Adjust xy for text alignment */
		txal = ug_gksstli.curprats.txalign.hor;
		tyal = ug_gksstli.curprats.txalign.ver;
		if( txal == UG_TH_CENTRE ) {
			x -= strlen(prms->str) *(uw_glfont.chrwid/2); 
			uu_dprint(UU_GITRC,(us,"adjusted for horizontal center"));
		}
		else if( txal == UG_TH_RIGHT ) {
			x -= strlen(prms->str) * uw_glfont.chrwid;
			uu_dprint(UU_GITRC,(us,"adjusted for horizontal right"));
		}
		if( tyal == UG_TV_TOP ) {
			y -= uw_glfont.chrhgt;
			uu_dprint(UU_GITRC,(us,"adjusted for vertical top"));
		}
		else if( tyal == UG_TV_HALF ) {
			y = y - uw_glfont.offset/2;
			uu_dprint(UU_GITRC,(us,"adjusted for vertical half"));
		}
	
		glRasterPos2i_d(x,y);
		uw_glprintString(prms->str, uw_gltext_base);
		if( uw_gl.rasmode != 1 )
			uw_glpopx();     /* restore orig. matrix */
	
		pt.x = prms->p.x; pt.y = prms->p.y;
		uw_glmark_dirty_dpt(&pt);
		pt.x = pt.x + uw_glfont.chrpwid * strlen(prms->str);
		pt.y = pt.y + UW_label_size[0];
		uw_glmark_dirty_dpt(&pt);
	}

	else {
		/**** Call simulation routine to do stroked text */
		strcpy(hershyprms.s, prms->str);
		hershyprms.slen  = strlen(prms->str);
		hershyprms.op    = prms->id;
		hershyprms.id    = prms->ws;
		hershyprms.pos.x = prms->p.x;
		hershyprms.pos.y = prms->p.y;
		hershyprms.pos.z = 0.0;
		ug_hershy(&hershyprms,1);                               /* calls DRASLINE */
	}
	uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     :  uw_glfillarea(prms)
**       Fill area 2D.
**    PARAMETERS
**       INPUT  :
**          prms
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uw_glfillarea(prms)
wpoint_struct *prms;
{
	int i;
	if( ug_gksos.sysstate != UG_SGOP ) 
	{
		i = prms->n;
		uw_glcolor( ug_gksstli.curprats.flbundl.color );
		glBegin_d(GL_POLYGON);
		for (i=0; i<(*prms).n; i++)
		{
			glVertex2f_d(prms->points[i].x,prms->points[i].y);
			uw_glmark_dirty_pt2(&(prms->points[i]));
		}
		glEnd_d();
	}
}

/*********************************************************************
**    E_FUNCTION     :  uw_glfillarea3(prms)
**       Fill area 3D.
**    PARAMETERS
**       INPUT  :
**          prms
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glfillarea3(prms)
w3point_struct *prms;
{
	int i;
	if( ug_gksos.sysstate != UG_SGOP ) 
	{
		i = prms->n;
		uw_gllighting(UU_FALSE);
		uw_glcolor( ug_gksstli.curprats.flbundl.color );
		glBegin_d(GL_POLYGON);
		for (i=0; i<(*prms).n; i++)
		{
			glVertex3f_d(prms->points[i].x,prms->points[i].y, prms->points[i].z);
			uw_glmark_dirty_pt3(&(prms->points[i]));
		}
		glEnd_d();
	}
}
	
/*********************************************************************
**    E_FUNCTION     :  uw_glfillras(prms)
**       Fill raster area.
**    PARAMETERS
**       INPUT  :
**          prms
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glfillras(prms)
ipoint_struct *prms;
{
	int i;
	GLint vpt[4];
	GLdouble ortho[6];
	if( ug_gksos.sysstate != UG_SGOP )
	{
		if( uw_gl.rasmode != 1 )
		{
			uw_glpushx();

			vpt[0] = 0 ; vpt[1] = 0 ; vpt[2] = glwdt.dspsize.raster.x;
			vpt[3] =  glwdt.dspsize.raster.y;
			uw_glViewport(vpt);
			ortho[0] = 0. ; ortho[1] = glwdt.dspsize.raster.x ; ortho[2] = 0.;
			ortho[3] = glwdt.dspsize.raster.y ; ortho[4] = -1000. ; ortho[5] = 1000.;
			uw_glOrtho(ortho);

			glLoadIdentity_d(); 
		}
		uw_glcolor( ug_gksstli.curprats.flbundl.color );
		glBegin_d(GL_POLYGON);
		for (i=0; i<(*prms).n; i++)
		{
			glVertex2i_d(prms->points[i].x,prms->points[i].y);
			uw_glmark_dirty_dpt(&(prms->points[i]));
		}
		glEnd_d();
		if( uw_gl.rasmode != 1)
			uw_glpopx();
	}
}

/*********************************************************************
**    E_FUNCTION     :  uw_glshadearea (prms)
**       Fill area 3D.
**    PARAMETERS
**       INPUT  :
**          prms
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glshadearea (prms)
shade_struct *prms;
{
	if( ug_gksos.sysstate != UG_SGOP ) 
	{
		uw_glshade_poly(prms->n,prms->points,prms->norms,prms->type);
	}
}

/*********************************************************************
**    E_FUNCTION : uw_gllabelbackground(x,y,z,s)
**                      
**      draw a rectangle centered at point (x,y,z) in world coordinates, for 
**		  label background. Use the current label background color.
**
**    PARAMETERS   
**       INPUT  : 
**          x,y,z    Coordinate to position label.
**          s        Label string.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gllabelbackground(x,y,z,s)
Gfloat x, y, z;                              /* 3-D position */
char *s;
{
	static GLubyte *Sfarea = UU_NULL;
	static int Sfsize = 0;
	int i, wid,size;
	GLfloat rectwt,rectht;

#if(UU_COMP==UU_WIN2K)
	uw_ntget_labelwid(s, &wid);
#else
	uw_mfget_labelwid(s, &wid);
#endif
	rectwt = wid;
	rectht = uw_glfont.chrphgt;
	size = rectwt*rectht/8 + 100;
	if (size > Sfsize)
	{
		if (Sfarea != UU_NULL) uu_free(Sfarea);
		Sfarea = (GLubyte*) uu_malloc (size*(sizeof (GLubyte)));
		Sfsize = size;
		for (i=0; i<size; i++) Sfarea[i] = 0xff;
	}
	uw_gllighting(UU_FALSE);
/*
.....Move to  position and
.....select marker font
*/
	if(NCL_toggle_off || NCL_alter )
		uw_glcolor(0);
	else
		uw_glcolor(ug_gksstli.curprats.txbundl.color);
	glRasterPos3f_d(x, y, z );
#if(UU_COMP==UU_WIN2K)
	glBitmap_d((GLsizei)rectwt+1,(GLsizei)rectht+1,(GLfloat)1,
				(GLfloat)2,(GLfloat)0,(GLfloat)0, Sfarea, 8);
#else
	glBitmap_d((GLsizei)rectwt,(GLsizei)rectht+3,(GLfloat)-3,
				(GLfloat)3,(GLfloat)0,(GLfloat)0, Sfarea, 8);
#endif
	if(NCL_toggle_off || NCL_alter)
		uw_glcolor(0);
	else
		uw_glcolor(UW_bkg_clr);
/*
.....Move to  position and
.....select marker font
*/
	glRasterPos3f_d(x, y, z );

#if(UU_COMP==UU_WIN2K)
	glBitmap_d((GLsizei)rectwt-1,(GLsizei)rectht-1,(GLfloat)0,
				(GLfloat)1,(GLfloat)0,(GLfloat)0, Sfarea, 8);
#else
	glBitmap_d((GLsizei)rectwt-2,(GLsizei)rectht+1,(GLfloat)-4,
				(GLfloat)2,(GLfloat)0,(GLfloat)0, Sfarea, 8);
#endif
}

/*********************************************************************
**    E_FUNCTION     : uw_glleaderline(key,labpos,ldrpos,label)
**       Draw a leader line from the closest point on the label box to 
**			the point on the entity.
**    PARAMETERS   
**       INPUT  : 
**			   key     key of entity to add to NCL_ldr_list
**          labpos  position of the label
**          ldrpos  point on the entity
**          label   string for label
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_glleaderline(key,labpos,ldrpos,label)
UU_KEY_ID key;
UM_coord labpos,ldrpos;
char *label;
{
	UM_coord npt,labpt[8],wpt,nldr;
	int found =0,nkey,i,index=0,n;

	UU_REAL labwd,labht,hflabwd,hflabht;
	int wd,ht;
	Gfloat offset=0,wid,hgt;
	UM_vector vec;
	UU_KEY_ID *keys;
	Gwpoint3 cpts[2];
	Gfloat uw_gldevxtondc();
	Gfloat uw_gldevytondc();

	wd = uw_glfont.chrpwid * (strlen(label));
#if(UU_COMP==UU_WIN2K)
	ht = UW_label_size[0];
	offset=uw_gldevytondc(1);
#else
	ht = uw_glfont.chrphgt; 
#endif
	wid=uw_gldevxtondc(wd);
	hgt=uw_gldevytondc(ht);
	
/*
.....Project label and leader start locations onto view plane
*/
	gwndc3(&npt[0],&npt[1],&npt[2],
			labpos[0],labpos[1],labpos[2]);
	gwndc3(&nldr[0],&nldr[1],&nldr[2],
			ldrpos[0],ldrpos[1],ldrpos[2]);

	if (NCL_dyn_ldr)
	{
		if(um_dcccc(nldr,npt)<(hgt*.5))
			return 0;
		um_vctovc(labpos,wpt);
		goto display;
	}

/*
.....Find the closest point on the label box to the start position 
.....of the leader line
*/
	npt[1] -= offset;

	for(i=0;i<8;i++)
		um_vctovc(npt,labpt[i]);

	labwd = npt[0] + wid;
	labht = npt[1] + hgt;

	hflabwd =npt[0] + wid * 0.5; 
	hflabht =npt[1]+ hgt * 0.5; 

	labpt[1][0] = labpt[5][0] = hflabwd ;
	labpt[3][1] = labpt[7][1] = hflabht ;
	labpt[2][0] = labpt[3][0] = labpt[4][0] = labwd ;
	labpt[4][1] = labpt[5][1] = labpt[6][1] = labht ;
	
	ncl_lblpt(labpt,nldr,&index);
	if(um_dcccc(labpt[index],nldr)<(hgt*.5))
		return 0;
	
/*
.....Project this point back to world coordinate space. and draw the leader
.....line ending at this point
*/
	
	gndcw3(&wpt[0],&wpt[1],&wpt[2],labpt[index][0],labpt[index][1],labpt[index][2]);
display:;
/*
......set leader line width and color
*/
	gslinewidth (1.0);
	gslinecolor (UW_ldr_clr);
	
	if(UW_ldr_arrow==2)
	{
		cpts[0].x =ldrpos[0];
		cpts[0].y =ldrpos[1];
		cpts[0].z =ldrpos[2];
		cpts[1].x =wpt[0];
		cpts[1].y =wpt[1];
		cpts[1].z =wpt[2];
		n =2;
		gpolyline3(n,cpts);
	}
	else
	{
		um_vcmnvc(ldrpos,wpt,vec);
		dstptv (wpt,vec,UM_idmat);
	}
/*
.....add this entity to the list of entities with leader lines displayed.
*/
	nkey = NCL_ldr_list.cur_cnt;
	keys = (UU_KEY_ID *) UU_LIST_ARRAY (&NCL_ldr_list);
	for(i=0;i<nkey;i++)
	{
		if(keys[i]==key) 
		{	
			found =1;
			break;
		}
	}
	
	if(!found)
		uu_list_push(&NCL_ldr_list,&key);
done:;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : uw_gllabelsz(label,wid,hgt)
**       get label box size
**    PARAMETERS   
**       INPUT  : 
**          label     label string
**          wid       width
**          hgt       height
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_gllabelsz(label,wid,hgt)
char *label;
Gfloat *wid,*hgt;
{
	int wd,ht;
	Gfloat uw_gldevxtondc();
    Gfloat uw_gldevytondc();

	wd = uw_glfont.chrpwid * (strlen(label));
	ht = uw_glfont.chrphgt ;
	*wid = uw_gldevxtondc(wd);
	*hgt = uw_gldevytondc(ht);
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : uw_gloverlapdis(hgt)
**       get label overlap distance
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          hgt      height
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_gloverlapdis(hgt)
Gfloat *hgt;
{
    Gfloat uw_gldevytondc();

	int ht=0;
	ht = UW_overlap_dis;
	*hgt = uw_gldevytondc(ht);
	return 0;
}
#endif
