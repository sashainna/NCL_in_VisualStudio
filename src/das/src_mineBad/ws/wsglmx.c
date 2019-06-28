#include "zsysdep.h"
#ifdef UU_OPENGL

/*********************************************************************
**  NAME:  wsglmx.c
**
**      GKS OpenGL workstation.
**
**       CONTAINS:
**				
**				uw_gltag_ortho(xform,l,r,b,t,n,f)
**				uw_gltag_vpt(xform,x,y,w,h)
**				uw_gltag_w2v(xform,x,y,z)
**				uw_gltag_s1(xform,x,y,z)
**				uw_gltag_t2(xform,x,y,z)
**				uw_gltag_vup(xform,z)
**				uw_gltag_vpn(xform,x,y)
**				uw_gltag_trn(xform,x,y,z)
**				uw_glisident(matrix)
**				uw_glmultmx(mx1,mx2)
**				uw_glsave_matrix(xform,merge,mmx,smx)
**	
**     MODULE NAME AND RELEASE LEVEL
**       wsglmx.c , 25.1
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

/*********************************************************************
**    I_FUNCTION     :   uw_gltag_ortho(xform,l,r,b,t,n,f)
**       Defines ortho portion of matrix (GL_PROJECTION)
**    PARAMETERS   
**       INPUT  : 
**          xform   = Matrix to define.
**				l       = Left clipping plane.
**				r       = Right clipping plane.
**				b       = Bottom clipping plane.
**				t       = Top clipping plane.
**				n       = Near clipping plane.
**				f       = Far clipping plane.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gltag_ortho(xform,l,r,b,t,n,f)
int xform;
GLdouble l,r,b,t,n,f;
{
	uw_gl_matrix[xform].ortho[0] = l;
	uw_gl_matrix[xform].ortho[1] = r;
	uw_gl_matrix[xform].ortho[2] = b;
	uw_gl_matrix[xform].ortho[3] = t;
	uw_gl_matrix[xform].ortho[4] = n;
	uw_gl_matrix[xform].ortho[5] = f;
}

/*********************************************************************
**    I_FUNCTION     :   uw_gltag_vpt(xform,x,y,w,h);
**       Defines viewport portion of matrix.
**    PARAMETERS   
**       INPUT  : 
**          xform   = Matrix to define.
**				x       = Lower left of viewport.
**				y       = Lower left of viewport.
**				w       = Width of viewport.
**				h       = Height of viewport.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gltag_vpt(xform,x,y,w,h)
int xform;
GLint x,y,w,h;
{
	uw_gl_matrix[xform].vpt[0] = x;
	uw_gl_matrix[xform].vpt[1] = y;
	uw_gl_matrix[xform].vpt[2] = w;
	uw_gl_matrix[xform].vpt[3] = h;
}

/*********************************************************************
**    I_FUNCTION     :   uw_gltag_w2v(xform,x,y,z);
**       Defines window to viewport translation portion of matrix.
**    PARAMETERS   
**       INPUT  : 
**          xform   = Matrix to define.
**				x       = X translation.
**				y       = Y translation.
**				z       = Z translation.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gltag_w2v(xform,x,y,z)
int xform;
GLdouble x,y,z;
{
	uw_gl_matrix[xform].w2v[0] = x;
	uw_gl_matrix[xform].w2v[1] = y;
	uw_gl_matrix[xform].w2v[2] = z;
	uw_gl_matrix[xform].changed = 1;
}

/*********************************************************************
**    I_FUNCTION     :   uw_gltag_s1(xform,x,y,z);
**       Defines scale portion of matrix.
**    PARAMETERS   
**       INPUT  : 
**          xform   = Matrix to define.
**				x       = X scale factor.
**				y       = Y scale factor.
**				z       = Z scale factor.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gltag_s1(xform,x,y,z)
int xform;
GLdouble x,y,z;
{
	uw_gl_matrix[xform].s1[0] = x;
	uw_gl_matrix[xform].s1[1] = y;
	uw_gl_matrix[xform].s1[2] = z;
	uw_gl_matrix[xform].changed = 1;
}

/*********************************************************************
**    I_FUNCTION     :   uw_gltag_t2(xform,x,y,z);
**       Defines translation portion of matrix.
**    PARAMETERS   
**       INPUT  : 
**          xform   = Matrix to define.
**				x       = X translation.
**				y       = Y translation.
**				z       = Z translation.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gltag_t2(xform,x,y,z)
int xform;
GLdouble x,y,z;
{
	uw_gl_matrix[xform].t2[0] = x;
	uw_gl_matrix[xform].t2[1] = y;
	uw_gl_matrix[xform].t2[2] = z;
	uw_gl_matrix[xform].changed = 1;
}

/*********************************************************************
**    I_FUNCTION     :   uw_gltag_vup(xform,z);
**       Defines view up rotation portion of matrix.
**    PARAMETERS   
**       INPUT  : 
**          xform   = Matrix to define.
**				z       = View up rotation.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gltag_vup(xform,z)
int xform;
GLdouble z;
{
	uw_gl_matrix[xform].vup = z;
	uw_gl_matrix[xform].changed = 1;
}

/*********************************************************************
**    I_FUNCTION     :   uw_gltag_vpn(xform,x,y,z);
**       Defines view normal rotation portion of matrix.
**    PARAMETERS   
**       INPUT  : 
**          xform   = Matrix to define.
**				x       = X view normal rotation.
**				y       = Y view normal rotation.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gltag_vpn(xform,x,y)
int xform;
GLdouble x,y;
{
	uw_gl_matrix[xform].vpn[0] = x;
	uw_gl_matrix[xform].vpn[1] = y;
	uw_gl_matrix[xform].changed = 1;
}

/*********************************************************************
**    I_FUNCTION     :   uw_gltag_trn(xform,x,y,z);
**       Defines translation to reference point portion of matrix.
**    PARAMETERS   
**       INPUT  : 
**          xform   = Matrix to define.
**				x       = X translation.
**				y       = Y translation.
**				z       = Z translation.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gltag_trn(xform,x,y,z)
int xform;
GLdouble x,y,z;
{
	uw_gl_matrix[xform].trn[0] = x;
	uw_gl_matrix[xform].trn[1] = y;
	uw_gl_matrix[xform].trn[2] = z;
	uw_gl_matrix[xform].changed = 1;
}
/*********************************************************************
**    I_FUNCTION     :   uw_glisident(matrix);
**       Determines if the input matrix is an identity matrix.
**			 matrix.
**    PARAMETERS   
**       INPUT  : 
**          matrix   = Input matrix.
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if matrix is an identity matrix.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL uw_glisident(matrix)
GLdouble matrix[16];
{
	int i,stat;
	GLdouble ident[16]={1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,0,1};
	stat = UU_TRUE;
	for (i=0;i<16;i++)
	{
		if (matrix[i] != ident[i])
		{
			stat = UU_FALSE;
			break;
		}
	}
	return(stat);
}

/*********************************************************************
**    I_FUNCTION     :   uw_glmultmx(mx1,mx2);
**       Multiplies mx1 by mx2 and puts the resultant matrix in mx1.
**    PARAMETERS   
**       INPUT  : 
**          mx1   = 1st matrix to multiply.
**          mx2   = 2nd matrix to multiply.
**       OUTPUT :  
**          mx1   = Resultant matrix.
**    RETURNS      : UU_TRUE if matrix is an identity matrix.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glmultmx(mx1,mx2)
GLdouble mx1[16],mx2[16];
{
	double mx[16];
	int i,j,k,ij;

	for (i=0, ij = 0; i<4; i++)
		for (j=0;j<4; j++, ij++)
			for (k=0, mx[ij] = 0. ; k<4; k++) mx[ij] += mx1[i+4*k]*mx2[k+4*j];

	for (i=0;i<4;i++)
		for (j=0;j<4;j++)
			mx1[i+4*j] = mx[j+4*i];

}

/*********************************************************************
**    I_FUNCTION     :   uw_glsave_matrix(xform,merge,mmx,smx);
**       Loads the specified matrix as the current GL matrix and then
**			saves it in 'mx'.
**    PARAMETERS   
**       INPUT  : 
**          xform   = Matrix to load.
**				merge   = UU_TRUE - merge 'mmx' with current xform matrix.
**				mmx     = Matrix to multiply with the xform matrix.
**				smx     = Matrix to receive calculated matrix.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glsave_matrix(xform,merge,mmx,smx)
int xform;
UU_LOGICAL merge;
GLdouble smx[16],mmx[16];
{
	int i;
	uw_glload_matrix(xform,merge,mmx);
	for (i=0;i<16;i++) smx[i] = uw_gl_matrix[xform].matrix[i];
}
#endif
