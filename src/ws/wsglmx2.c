#include "zsysdep.h"
#ifdef UU_OPENGL

/*********************************************************************
**  NAME:  wsglmx2.c
**
**      GKS OpenGL workstation.
**
**       CONTAINS:
**				
**				uw_glload_matrix(xform,merge,mmx)
**				uw_glpkload_matrix
**				uw_glOrtho(ortho)
**				uw_glViewport(vpt)
**				uw_glLoadIdentity()
**    			uw_glpopx()
**    			uw_glpushx()
**    			uw_glflushx()
**				uw_glget_vport()
**	
**     MODULE NAME AND RELEASE LEVEL
**       wsglmx2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:07
*********************************************************************/

#include <math.h>
#include "mdmatrix.h"
#include "ws.h"
#include "wsgl.h"
#if UU_COMP==UU_WIN2K
#include "wsntglfunc.h"
#endif
#include "wsglfun.h"
#include  "lipv.h"
#include "gtblvar6.h"
UW_glmatrix ul_ipv_matrix;
static int Slast_xform=-1;
static int last_xform=-1;
static int Snmx = 0;
static int ortho_chg = 0;
static GLdouble ortho_save[6];
static GLint vpt_save[4];
extern int UW_light_reset; 
extern int MSLite;
void uw_glmultmx();
void uw_glViewport();

void uw_glreset_xform()
{
	last_xform=-1;
}

uw_glget_last_xform()
{
	return last_xform;
}
void uw_setlast_xform(xform)
int xform;
{
	last_xform = xform;
}
/*********************************************************************
**    I_FUNCTION     :   uw_glload_matrix(xform);
**       Loads the specified matrix as the current GL matrix.
**    PARAMETERS   
**       INPUT  : 
**          xform   = Matrix to load.
**				merge   = UU_TRUE - merge 'mmx' with current xform matrix.
**				mmx     = Matrix to multiply with the xform matrix.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glload_matrix(xform,merge,mmx)
int xform;
UU_LOGICAL merge;
GLdouble mmx[16];
{
	int i,j;
	UU_LOGICAL merg,uw_glisident();
	GLdouble mx[16],ang;
/*	GLdouble mx1[16];TEMP*/
/*
.....Load ortho and viewport matrices
*/
/*
.....added global flag
.....if ortho changed vport need reset
.....Yurong 2/16/99
*/
	if (MSLite==0)
	{
		if (xform!=LW_vport.xform)
		{
			ortho_chg = uw_glOrtho(uw_gl_matrix[xform].ortho,UU_FALSE);
			uw_glViewport(uw_gl_matrix[xform].vpt);
		}
		ortho_chg = 0;
	}
/*
.....See if merged matrix exists and
.....is not an identity matrix
*/
	merg  = merge;
	if (merg)
	{
		if (uw_glisident(mmx)) merg = UU_FALSE;
	}
/*
.....Calculate new matrix if needed
*/
	if (xform != last_xform || uw_gl_matrix[xform].changed == 1)
	{
		uw_gl_ident(uw_gl_matrix[xform].matrix);
/*
........Window to viewport
*/
		uw_gl_ident(mx);
	
		mx[12] = uw_gl_matrix[xform].w2v[0];
		mx[13] = uw_gl_matrix[xform].w2v[1];
		mx[14] = uw_gl_matrix[xform].w2v[2];
		uw_glmultmx(uw_gl_matrix[xform].matrix,mx);
/*
........Scale matrix
*/
		uw_gl_matrix[xform].matrix[0] = uw_gl_matrix[xform].s1[0];
		uw_gl_matrix[xform].matrix[5] = uw_gl_matrix[xform].s1[1];
		uw_gl_matrix[xform].matrix[10] = uw_gl_matrix[xform].s1[2];
/*
........Translate matrix
*/
		uw_gl_ident(mx);
		mx[12] = uw_gl_matrix[xform].t2[0];
		mx[13] = uw_gl_matrix[xform].t2[1];
		mx[14] = uw_gl_matrix[xform].t2[2];
		uw_glmultmx(uw_gl_matrix[xform].matrix,mx);
/*
........View up rotation matrix
*/
		ang = uw_gl_matrix[xform].vup * DTOR;
		uw_gl_ident(mx);
		mx[0] = cos(ang) ; mx[1] = sin(ang);
		mx[4] = sin(ang) * -1. ; mx[5] = cos(ang);
		uw_glmultmx(uw_gl_matrix[xform].matrix,mx);
/*
........View normal rotation matrix
*/
		ang = uw_gl_matrix[xform].vpn[0] * DTOR;
		uw_gl_ident(mx);
		mx[5] = cos(ang) ; mx[6] = sin(ang);
		mx[9] = sin(ang) * -1. ; mx[10] = cos(ang);
		uw_glmultmx(uw_gl_matrix[xform].matrix,mx);

		ang = uw_gl_matrix[xform].vpn[1] * DTOR;
		uw_gl_ident(mx);
		mx[0] = cos(ang) ; mx[8] = sin(ang);
		mx[2] = sin(ang) * -1. ; mx[10] = cos(ang);
		uw_glmultmx(uw_gl_matrix[xform].matrix,mx);
/*
........Reference point matrix
*/
		uw_gl_ident(mx);
		mx[12] = uw_gl_matrix[xform].trn[0];
		mx[13] = uw_gl_matrix[xform].trn[1];
		mx[14] = uw_gl_matrix[xform].trn[2];
		uw_glmultmx(uw_gl_matrix[xform].matrix,mx);
		last_xform = xform;
		uw_gl_matrix[xform].changed = 1;
}
/*
.....Merge matrices
*/
	if (merg)
	{
		for (i=0;i<16;i++) mx[i] = uw_gl_matrix[xform].matrix[i];
		uw_glmultmx(mx,mmx);
		uw_gl_matrix[xform].changed = 1;
	}
	else if (uw_gl_matrix[xform].changed == 1)
	{
		for (i=0;i<16;i++) mx[i] = uw_gl_matrix[xform].matrix[i];
	}
/*
.....Load new matrix
*/
	if (uw_gl_matrix[xform].changed == 1)
	{
		if (MSLite)
		{
			UW_light_reset = 1; 
			for (i=0;i<4;i++)
			{
				for (j=0;j<4;j++)
				{
					ug_cxform[xform][i][j] = mx[4*i+j];
				}		
			}
			for (i=0;i<16;i++) ul_ipv_matrix.matrix[i] = mx[i];
		}			
		else
		{
			if (xform!=LW_vport.xform)
			{
				glLoadMatrixd_d(mx);
				uw_gllight_pos(UU_FALSE);
			}
		}
		uw_gl_matrix[xform].changed = 0;
		last_xform = xform;
	}
}

/*********************************************************************
**    I_FUNCTION     :   uw_glpkload_matrix(xform);
**       Loads the specified matrix as the current GL matrix During pick.
**	don't save anything about projection.
**    PARAMETERS   
**       INPUT  : 
**          xform   = Matrix to load.
**				merge   = UU_TRUE - merge 'mmx' with current xform matrix.
**				mmx     = Matrix to multiply with the xform matrix.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glpkload_matrix(xform,merge,mmx)
int xform;
UU_LOGICAL merge;
GLdouble mmx[16];
{
	int i;
	UU_LOGICAL merg,uw_glisident();
	GLdouble mx[16],ang;
/*	GLdouble mx1[16]; TEMP*/
/*
.....Load ortho and viewport matrices
*/
	glMatrixMode_d(GL_PROJECTION);
	glLoadIdentity_d();
	glOrtho_d(uw_gl_matrix[xform].ortho[0],
		uw_gl_matrix[xform].ortho[1],
		uw_gl_matrix[xform].ortho[2],
		uw_gl_matrix[xform].ortho[3],
		uw_gl_matrix[xform].ortho[4],
		uw_gl_matrix[xform].ortho[5]);
	glDepthRange_d(uw_gl_matrix[xform].ortho[4],uw_gl_matrix[xform].ortho[5]);
	glMatrixMode_d(GL_MODELVIEW);
	glViewport_d(uw_gl_matrix[xform].vpt[0],
			uw_gl_matrix[xform].vpt[1],
			uw_gl_matrix[xform].vpt[2],
			uw_gl_matrix[xform].vpt[3]);
/*
.....See if merged matrix exists and
.....is not an identity matrix
*/
	merg  = merge;
	if (merg)
	{
		if (uw_glisident(mmx)) merg = UU_FALSE;
	}
/*
.....Calculate new matrix if needed
*/
	if (xform != last_xform || uw_gl_matrix[xform].changed == 1)
	{
		uw_gl_ident(uw_gl_matrix[xform].matrix);
/*
........Window to viewport
*/
		uw_gl_ident(mx);
	
		mx[12] = uw_gl_matrix[xform].w2v[0];
		mx[13] = uw_gl_matrix[xform].w2v[1];
		mx[14] = uw_gl_matrix[xform].w2v[2];
		uw_glmultmx(uw_gl_matrix[xform].matrix,mx);
/*
........Scale matrix
*/
		uw_gl_matrix[xform].matrix[0] = uw_gl_matrix[xform].s1[0];
		uw_gl_matrix[xform].matrix[5] = uw_gl_matrix[xform].s1[1];
		uw_gl_matrix[xform].matrix[10] = uw_gl_matrix[xform].s1[2];
/*
........Translate matrix
*/
		uw_gl_ident(mx);
		mx[12] = uw_gl_matrix[xform].t2[0];
		mx[13] = uw_gl_matrix[xform].t2[1];
		mx[14] = uw_gl_matrix[xform].t2[2];
		uw_glmultmx(uw_gl_matrix[xform].matrix,mx);
/*
........View up rotation matrix
*/
		ang = uw_gl_matrix[xform].vup * DTOR;
		uw_gl_ident(mx);
		mx[0] = cos(ang) ; mx[1] = sin(ang);
		mx[4] = sin(ang) * -1. ; mx[5] = cos(ang);
		uw_glmultmx(uw_gl_matrix[xform].matrix,mx);
/*
........View normal rotation matrix
*/
		ang = uw_gl_matrix[xform].vpn[0] * DTOR;
		uw_gl_ident(mx);
		mx[5] = cos(ang) ; mx[6] = sin(ang);
		mx[9] = sin(ang) * -1. ; mx[10] = cos(ang);
		uw_glmultmx(uw_gl_matrix[xform].matrix,mx);

		ang = uw_gl_matrix[xform].vpn[1] * DTOR;
		uw_gl_ident(mx);
		mx[0] = cos(ang) ; mx[8] = sin(ang);
		mx[2] = sin(ang) * -1. ; mx[10] = cos(ang);
		uw_glmultmx(uw_gl_matrix[xform].matrix,mx);
/*
........Reference point matrix
*/
		uw_gl_ident(mx);
		mx[12] = uw_gl_matrix[xform].trn[0];
		mx[13] = uw_gl_matrix[xform].trn[1];
		mx[14] = uw_gl_matrix[xform].trn[2];
		uw_glmultmx(uw_gl_matrix[xform].matrix,mx);
		last_xform = xform;
		uw_gl_matrix[xform].changed = 1;
	}
/*
.....Merge matrices
*/
	if (merg)
	{
		for (i=0;i<16;i++) mx[i] = uw_gl_matrix[xform].matrix[i];
		uw_glmultmx(mx,mmx);
/*	glMultMatrixd(mmx);
	glGetDoublev(GL_MODELVIEW_MATRIX,mx1);*/
		uw_gl_matrix[xform].changed = 1;
	}
	else if (uw_gl_matrix[xform].changed == 1)
	{
		for (i=0;i<16;i++) mx[i] = uw_gl_matrix[xform].matrix[i];
	}
/*
.....Load new matrix
*/
	if (uw_gl_matrix[xform].changed == 1)
	{
		glLoadMatrixd_d(mx);
		uw_gl_matrix[xform].changed = 0;
		last_xform = xform;
	}
}
/*********************************************************************
**    I_FUNCTION     :   uw_glLoadIdentity();
**       Loads the identity matrix.
**    PARAMETERS   
**       INPUT  : 
**          vpt     = Viewport parameters.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glLoadIdentity()
{
	glLoadIdentity_d();
	last_xform = -1;
}
/*********************************************************************
**    I_FUNCTION     : uw_glpopx()
**     Pop Matrix and Atribute out of the stack
**    PARAMETERS  
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glpopx()
{
	if (Snmx > 0)
	{
		glMatrixMode_d( GL_PROJECTION);
		glPopMatrix_d();
		glMatrixMode_d( GL_MODELVIEW );
		glPopMatrix_d();
		glPopAttrib_d();
		Snmx--;
		last_xform = Slast_xform;
	}
	ortho_save[0] = -9999.9999;
}
/*********************************************************************
**    I_FUNCTION     : uw_glpushx()
**     Push Matrix and Atribute on the stack
**    PARAMETERS  
**       INPUT  :none
**       OUTPUT :none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glpushx()					
{
	glMatrixMode_d( GL_PROJECTION);
	glPushMatrix_d();
	glMatrixMode_d( GL_MODELVIEW );
	glPushMatrix_d();
	glPushAttrib_d( GL_VIEWPORT_BIT );
	Snmx++;
	Slast_xform = last_xform;
}

/*********************************************************************
**    I_FUNCTION     : uw_glflushx()
**     Remove all Matrix and Atribute from the stack
**    PARAMETERS  
**       INPUT  :none
**       OUTPUT :none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glflushx()					
{
	int i;
	int nmx=Snmx;
	for (i=0;i<nmx;i++) uw_glpopx();
}
/*********************************************************************
**    I_FUNCTION     :   uw_glViewport(vpt);
**       Sets the requested Viewport.
**    PARAMETERS   
**       INPUT  : 
**          vpt     = Viewport parameters.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glViewport(vpt)
GLint vpt[4];
{
	int i,j;
	for (i=0;i<4;i++)
	{
		if ((vpt[i] != vpt_save[i])||(ortho_chg))
		{
/*
.....adjust 1 pixel to include border line
.....Yurong 3/18/99
*/
			glViewport_d(vpt[0],vpt[1],vpt[2]-1,vpt[3]-1);
/*
			glViewport(vpt[0],vpt[1],vpt[2],vpt[3]);
*/
/*
	printf("Accepted VPORT = %d %d %d %d \n",vpt[0],vpt[1],vpt[2],
		vpt[3]);   
*/
			for (j=0;j<4;j++) vpt_save[j] = vpt[j];
			return;
		}  
	}
}

/*********************************************************************
**    I_FUNCTION     :   uw_glOrtho(ortho);
**       Loads the specified Ortho matrix as the current GL_PROJECTION
**			 matrix.
**    PARAMETERS   
**       INPUT  : 
**          ortho   = Ortho parameters.
**       OUTPUT :  
**          none
.....added return flag
.....if ortho changed vport need reset
.....Yurong 2/16/99
**    RETURNS      : 0: ortho not changed
**							1: ortho changed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_glOrtho(ortho)
GLdouble ortho[6];
{
	int i,j;
	for (i=0;i<6;i++)
	{
		if (ortho[i] != ortho_save[i])
		{
			glMatrixMode_d(GL_PROJECTION);
			glLoadIdentity_d();
			glOrtho_d(ortho[0],ortho[1],ortho[2],ortho[3],ortho[4],ortho[5]);
/*
.....set depth range
.....Yurong 3/24/99
*/
			if ((ortho[4] != ortho_save[4]) ||
					(ortho[5] != ortho_save[5]))
				glDepthRange_d(ortho[4],ortho[5]);
			glMatrixMode_d(GL_MODELVIEW);
			for (j=0;j<6;j++) ortho_save[j] = ortho[j];
			return 1;
		}     
	}
	return 0;
/*	printf("Rejected = %f %f %f %f %f %f\n",ortho[0],ortho[1],ortho[2],
		ortho[3],ortho[4],ortho[5]);*/
}

void uw_glget_vport(viewport)
GLint viewport[4];
{
	int j;
	for (j=0;j<4;j++) viewport[j] = vpt_save[j];
}

#endif
