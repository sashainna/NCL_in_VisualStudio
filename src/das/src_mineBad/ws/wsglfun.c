#include "zsysdep.h"
#ifdef UU_OPENGL
/*#define OPENGL_DEBUG 1*/
extern int UW_project_depth;
static int project_depth_count = 1;
static int matrix_pushed = 0;
static int current_matrix_mode;
extern int UW_print_screen, UW_print_back;
/*********************************************************************
**    NAME         :  wsglfun.c
**
**       CONTAINS:
**		GKS openGL workstation routines. All routines are just call
**		openGL library routine plus generate journal file if debug
**
**    MODULE NAME AND RELEASE LEVEL 
**       wsglfun.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:06
*********************************************************************/

#include <stdio.h>
#include "wsgl.h"
#include <math.h>

#ifdef OPENGL_DEBUG
static FILE *GLjou_fd;
#endif

#ifdef OPENGL_DEBUG
static int Slist_begin = 0;
#endif

static Matrix *Spjmatrix_ptr = UU_NULL;
static int stack_count = 0;

/*********************************************************************
**    E_FUNCTION     : uw_glpush_pjmatrix()
**          This function put a current projection matrix into
**			a matrix stack, using this function instead of
**			glPushMatrix() when the matrix stack is excess the
**			maxinum stack depth openGL support. Otherwise, it will
**			have problem (lock the computer or set font not working)
**    PARAMETERS
**       INPUT  :none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_glpush_pjmatrix()
{
	Matrix *p1;
	int n1;
	n1 = sizeof(Matrix);
	if (Spjmatrix_ptr==NULL)
		Spjmatrix_ptr = (Matrix *) uu_lsnew();
	if (stack_count != project_depth_count-UW_project_depth)
		return -1;
	p1 = Spjmatrix_ptr;
	if (p1 != UU_NULL) p1 = (Matrix *) uu_lsend (p1);
	if (p1 == UU_NULL) p1 = Spjmatrix_ptr;
	if (p1 != UU_NULL) p1 = (Matrix *) uu_lsinsrt(p1,n1);
	if (p1 == UU_NULL)
		return -1;
	else
	{
		glGetDoublev(GL_PROJECTION_MATRIX,(GLdouble *)p1);
	}
	stack_count++;
	return 0;
}	
int uw_glpop_pjmatrix()
{
	Matrix *p1;
	int n1;
	n1 = sizeof(Matrix);
	if (Spjmatrix_ptr==NULL)
		return 0;
	if (stack_count != project_depth_count-UW_project_depth)
		return -1;
	p1 = Spjmatrix_ptr;
	if (p1 != UU_NULL) p1 = (Matrix *) uu_lsend (p1);
	if (p1 == UU_NULL) p1 = Spjmatrix_ptr;
	if (p1 == UU_NULL)
		return -1;
	else
	{
		glLoadMatrixd((const GLdouble *)p1);
		p1 = (Matrix *) uu_lsdele(p1);
	}
	stack_count--;
	return 0;
}

void wglUseFontBitmaps_test(x,y)
int x,y;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"wglUseFontBitmaps\t%d,%d\n",x,y);	
#endif
}
void uw_open_gldebug_file()
{
#ifdef OPENGL_DEBUG
	GLjou_fd= fopen("ncl.jgl","w");
#endif
}

void uw_close_gldebug_file()
{
#ifdef OPENGL_DEBUG
	if (GLjou_fd!=NULL)
		fclose(GLjou_fd);
#endif
}

void glBegin_d(mode)
GLenum mode;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glBegin\t%d\n", mode);	
#endif
	glBegin(mode);
}
 
void glEnd_d()
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glEnd\n");	
#endif
	glEnd();
}
void glBitmap_d(width, height, xorig, yorig, xmove, ymove, bitmap, mtyp)  
  GLsizei width;           
  GLsizei height;          
  GLfloat xorig;           
  GLfloat yorig;           
  GLfloat xmove;           
  GLfloat ymove;   
  int mtyp;
/*  GLint xorig;           
  GLint yorig;           
  GLint xmove;           
  GLint ymove;
*/
	GLubyte * bitmap;   
{
#ifdef OPENGL_DEBUG
/*
.....bitmap is output parameter, we are not save the address, useless
*/
	fprintf(GLjou_fd,"glBitmap\t%d,%d,%f,%f,%f,%f %d\n",
					width, height, xorig, yorig, xmove, ymove, mtyp);
#endif
	glBitmap(width, height, xorig, yorig, xmove, ymove, bitmap);
}

void glBlendFunc_d(sfactor, dfactor)
GLenum sfactor, dfactor;  
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glBlendFunc\t%d,%d\n", sfactor, dfactor);
#endif
	glBlendFunc(sfactor, dfactor);
}

void glCallList_d(list)
GLuint list;
{
	if (list<=0) return;
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glCallList\t%d\n", list);
#endif
	glCallList(list);
}

void glCallLists_d(n,type,lists)
GLuint n;
GLenum type;           
GLvoid * lists;   
{
#ifdef OPENGL_DEBUG
/*
.....save lists as character strings
*/
	fprintf(GLjou_fd,"glCallLists\t%d,%d,%s\n", n,type,lists);
#endif
	glCallLists(n,type,lists);
}
 
void glClear_d(mask)
GLbitfield mask;
{
#ifdef OPENGL_DEBUG
/*
.....save lists as character strings
*/
	fprintf(GLjou_fd,"glClear\t%d\n", mask);
#endif
	glClear(mask);
}

void glClearColor_d(red, green, blue, alpha)
	GLclampf red, green, blue, alpha; 
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glClearColor\t%f,%f,%f,%f\n", red, green, blue, alpha);
#endif
	glClearColor(red, green, blue, alpha);
}
 
void glClearDepth_d(depth)
GLclampd depth;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glClearDepth\t%f\n", depth);
#endif
	glClearDepth(depth);
} 

void glClearStencil_d(s)
GLint s;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glClearStencil\t%d\n", s);
#endif
	glClearStencil(s);
}

void glColor3f_d(red, green, blue)  
GLfloat red, green, blue;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glColor3f\t%f,%f,%f\n", red, green, blue);
#endif
	if ((UW_print_screen==1)&&(UW_print_back==0))
	{
		if ((red==1)&&(green==1)&&(blue==1))
		{
			red = blue = green = 0;
		}
	}
	glColor3f(red, green, blue);
}

void glColor4f_d(red, green, blue, alpha)
GLfloat red, green, blue, alpha;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glColor3f\t%f,%f,%f,%f\n", red, green, blue, alpha);
#endif
	glColor4f(red, green, blue, alpha);
}

void glColorMask_d(red, green, blue, alpha) 
GLboolean red, green,blue, alpha;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glColorMask\t%d,%d,%d,%d\n", red, green, blue, alpha);
#endif
	glColorMask(red, green, blue, alpha) ;
}
 
void glCopyPixels_d(x,y,width,height, type)
GLint x, y;         
GLsizei width, height; 
GLenum type;     
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glCopyPixels\t%d,%d,%d,%d,%d\n", x,y,width,height, type);
#endif
	glCopyPixels(x,y,width,height, type);
}
 
void glDeleteLists_d(list,  range)
GLuint list;
GLsizei range;
{
#ifdef OPENGL_DEBUG
	if (Slist_begin>0)
	{
		ud_printmsg("Call glDeleteLists within a openGL calling list, not allowed"); 
		return;
	}
	fprintf(GLjou_fd,"glDeleteLists\t%d,%d\n", list, range);
#endif
	glDeleteLists(list,  range);
}

void glDepthFunc_d(func)
GLenum func;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glDepthFunc\t%d\n", func);
#endif
	glDepthFunc(func);
} 

void glDepthMask_d(flag)
GLboolean flag;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glDepthMask\t%d\n", flag);
#endif
	glDepthMask(flag);
}


void glDepthRange_d(znear,zfar)
GLclampd znear, zfar;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glDepthRange\t%f,%f\n", znear,zfar);
#endif
	glDepthRange(znear,zfar);
}
 
void glEnable_d(cap)
GLenum cap;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glEnable\t%d\n", cap);
#endif
	glEnable(cap);
}

void glDisable_d(cap)
GLenum cap;  
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glDisable\t%d\n", cap);
#endif
	glDisable(cap);
}
 

void glDrawBuffer_d(mode)
GLenum mode;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glDrawBuffer\t%d\n", mode);
#endif
	glDrawBuffer(mode);
}

void glDrawPixels_d(width, height, format,  type, pixels)
GLsizei width, height;       
GLenum format,  type;           
GLvoid *pixels;  
{
#ifdef OPENGL_DEBUG
/*
.....not save pixel now, maybe later
.....when testing , using the privious saved readpixel value
*/
	fprintf(GLjou_fd,"glDrawPixels\t%d,%d,%d,%d\n", width, height, format,  type);
#endif
	glDrawPixels(width, height, format,  type, pixels);
}
 
void glNewList_d(list, mode)
GLuint list;  
GLenum mode;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glNewList\t%d,%d\n", list, mode);
	Slist_begin++;
#endif
	glNewList(list, mode);
}
 
void glEndList_d()
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glEndList\n");
	Slist_begin--;
#endif
	glEndList();
}
 
void glFeedbackBuffer_d(size, type,  buffer)
GLsizei size;
GLenum type;       
GLfloat * buffer;   
{
#ifdef OPENGL_DEBUG
	if (Slist_begin>0)
	{
		ud_printmsg("Call glFeedbackBuffer within a openGL calling list, not allowed"); 
		return;
	}
/*
.....don't save the pointer
*/
	fprintf(GLjou_fd,"glFeedbackBuffer\t%d,%d\n", size, type);
#endif
	glFeedbackBuffer(size, type,  buffer);
}

void glFinish_d()
{
#ifdef OPENGL_DEBUG
	if (Slist_begin>0)
	{
		ud_printmsg("Call glFinish within a openGL calling list, not allowed"); 
		return;
	}
	fprintf(GLjou_fd,"glFinish\n");
#endif
	glFinish();  
}

void glFlush_d()
{
#ifdef OPENGL_DEBUG
	if (Slist_begin>0)
	{
		ud_printmsg("Call glFlush within a openGL calling list, not allowed"); 
		return;
	}
	fprintf(GLjou_fd,"glFlush\n");
#endif
	glFlush();
}

void glFrontFace_d(mode)
GLenum mode;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glFrontFace\t%d\n", mode);
#endif
	glFrontFace(mode);
}
 
GLuint glGenLists_d(range)
GLsizei range;   
{
#ifdef OPENGL_DEBUG
	if (Slist_begin>0)
	{
		ud_printmsg("Call glGenLists within a openGL calling list, not allowed"); 
		return 0;
	}
	fprintf(GLjou_fd,"glGenLists\t%d\n", range);
#endif
	return glGenLists(range);
}
  
void glGetDoublev_d(pname, params)
GLenum pname;       
GLdouble * params;   
{
#ifdef OPENGL_DEBUG
	if (Slist_begin>0)
	{
		ud_printmsg("Call glGetDoublev within a openGL calling list, not allowed"); 
		return;
	}
/*
.....don't save the pointer now
*/
	fprintf(GLjou_fd,"glGetDoublev\t%d\n", pname);
#endif
	glGetDoublev(pname, params);
}
  
void glGetIntegerv_d(pname, params)
GLenum pname;  
GLint * params;  
{
#ifdef OPENGL_DEBUG
	if (Slist_begin>0)
	{
		ud_printmsg("Call glGetIntegerv within a openGL calling list, not allowed"); 
		return;
	}
/*
.....don't save the pointer now
*/
	fprintf(GLjou_fd,"glGetIntegerv\t%d\n", pname);
#endif
	glGetIntegerv(pname, params);
}
       
GLboolean glIsEnabled_d(cap)
GLenum cap;   
{
#ifdef OPENGL_DEBUG
	if (Slist_begin>0)
	{
		ud_printmsg("Call glIsEnabled within a openGL calling list, not allowed"); 
		return 0;
	}
	fprintf(GLjou_fd,"glIsEnabled\t%d\n", cap);
#endif
	return glIsEnabled(cap);
}
 
GLboolean glIsList_d(list)
GLuint list;   
{
#ifdef OPENGL_DEBUG
	if (Slist_begin>0)
	{
		ud_printmsg("Call glIsList within a openGL calling list, not allowed"); 
		return 0;
	}
	fprintf(GLjou_fd,"glIsList\t%d\n", list);
#endif
	return glIsList(list);
}

void glLightf_d(light,pname, param)
GLenum light, pname;   
GLfloat param;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glLightf\t%d,%d,%f\n", light,pname, param);
#endif
	glLightf(light,pname, param);
}
  
void glLightfv_d(light,pname, param)
GLenum light, pname;   
GLfloat *param;  
{
#ifdef OPENGL_DEBUG
	if ((pname==GL_AMBIENT) || (pname==GL_DIFFUSE)
			|| (pname==GL_SPECULAR) || (pname==GL_POSITION))
		fprintf(GLjou_fd,"glLightfv\t%d,%d,%f,%f,%f,%f\n", light,pname, 
					param[0], param[1], param[2], param[3]);
	else if (pname==GL_SPOT_DIRECTION)
		fprintf(GLjou_fd,"glLightfv\t%d,%d,%f,%f,%f\n", light,pname, 
					param[0], param[1], param[2]);
	else if ((pname==GL_SPOT_EXPONENT) || (pname==GL_SPOT_CUTOFF ))
		fprintf(GLjou_fd,"glLightfv\t%d,%d,%f\n", light,pname,param[0]);
	else
		fprintf(GLjou_fd,"glLightfv\t%d,%d,%f\n", light,pname,param[0]);
#endif
	glLightfv(light,pname, param);
}
 
void glLightModeli_d(pname, param)
GLenum pname;   
GLint param;     
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glLightModeli\t%d,%d\n", pname, param);
#endif
	glLightModeli(pname, param);
}
 
void glLineStipple_d(factor, pattern)
GLint factor;     
GLushort pattern;  
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glLineStipple\t%d,%d\n", factor, pattern);
#endif
	glLineStipple(factor, pattern);
}

void glLineWidth_d(width)
GLfloat width;   
{
	if (width<=0) width = 0.01;
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glLineWidth\t%f\n", width);
#endif
	glLineWidth(width);
}

void glListBase_d(base)
GLuint base;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glListBase\t%d\n", base);
#endif
	glListBase(base);
}

void glLoadIdentity_d()
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glLoadIdentity\n");
#endif
	glLoadIdentity();
}

void glLoadMatrixd_d(m)
GLdouble *m;   
{
/*
//	if ((m[0]==m[1]==m[2]==m[3]==m[4]==m[5]==m[6]==m[7]==m[8]==0.0)
//		&&(m[9]==m[10]==m[11]==m[12]==m[13]==m[14]==m[15]==0.0))
//		return;
*/
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glLoadMatrixd\t%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f\n",
				m[0],m[1],m[2],m[3], m[4],m[5],m[6],m[7],m[8],
				m[9],m[10],m[11],m[12],m[13],m[14],m[15]);
#endif
	glLoadMatrixd(m);
}
  
void glMateriali_d(face, pname, param)
GLenum face, pname;  
GLint param;    
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glMateriali\t%d,%d,%d\n", face, pname, param);
#endif
	glMateriali(face, pname, param);
}
 
void glMaterialfv_d(face, pname, param)
GLenum face, pname;  
GLfloat *param;
{  
#ifdef OPENGL_DEBUG
	if ((pname==GL_AMBIENT) || (pname==GL_DIFFUSE)
			|| (pname==GL_SPECULAR) || (pname==GL_EMISSION)
			|| (pname==GL_AMBIENT_AND_DIFFUSE ))
		fprintf(GLjou_fd,"glMaterialfv\t%d,%d,%f,%f,%f,%f\n", face,pname, 
					param[0], param[1], param[2], param[3]);
	else if (pname==GL_COLOR_INDEXES)
		fprintf(GLjou_fd,"glMaterialfv\t%d,%d,%f,%f,%f\n", face,pname, 
					param[0], param[1], param[2]);
	else if (pname==GL_SHININESS)
		fprintf(GLjou_fd,"glMaterialfv\t%d,%d,%f\n", face,pname,param[0]);
	else
		fprintf(GLjou_fd,"glMaterialfv\t%d,%d,%f\n", face,pname,param[0]);
#endif
	glMaterialfv(face, pname, param);
}

void glMatrixMode_d(mode)
GLenum mode;   
{
	current_matrix_mode = mode;
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glMatrixMode\t%d\n", mode);
#endif
	glMatrixMode(mode);
}
 

void glMultMatrixd_d(m)
GLdouble *m;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glMultMatrixd\t%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f\n",
				m[0],m[1],m[2],m[3], m[4],m[5],m[6],m[7],m[8],
				m[9],m[10],m[11],m[12],m[13],m[14],m[15]);
#endif
	glMultMatrixd(m);
}
	
void glNormal3f_d(nx,ny,nz)
GLfloat nx, ny, nz;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glNormal3f\t%f,%f,%f\n", nx, ny, nz);
#endif
	glNormal3f(nx,ny,nz);
}
 
void glOrtho_d(left1, right1, bottom1, top1, near1, far1)
GLdouble left1, right1, bottom1, top1, near1, far1;     
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glOrtho\t%f,%f,%f,%f,%f,%f\n", 
					left1, right1, bottom1, top1, near1, far1);
#endif
	glOrtho(left1, right1, bottom1, top1, near1, far1);
}
  
void glPixelStorei_d(pname, param)  
GLenum pname;   
GLint param;     
{
#ifdef OPENGL_DEBUG
	if (Slist_begin>0)
	{
		ud_printmsg("Call glPixelStorei within a openGL calling list, not allowed"); 
		return;
	}
	fprintf(GLjou_fd,"glPixelStorei\t%d,%d\n", pname, param);
#endif
	glPixelStorei(pname, param);
}

void glPixelTransferf_d(pname, param)  
GLenum pname;   
GLfloat param;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glPixelTransferf\t%d,%f\n", pname, param);
#endif
	glPixelTransferf(pname, param);
}
  
void glPolygonMode_d(face, mode)
GLenum face, mode;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glPolygonMode\t%d,%d\n", face, mode);
#endif
	glPolygonMode(face, mode);
}
  
void glPushAttrib_d(mask)
GLbitfield mask;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glPushAttrib\t%d\n", mask);
#endif
	glPushAttrib(mask);
}
 
void glPopAttrib_d()
{   
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glPopAttrib\n");
#endif
	glPopAttrib();
}
 
void glPushMatrix_d()
{
	matrix_pushed++;

	if ((project_depth_count>=UW_project_depth) && (current_matrix_mode==GL_PROJECTION))
	{
/*
.....don't use glPushMatrix function because it excess the max depth of the projection
.....matrix stack of openGL, using our own matrix stack
*/
		uw_glpush_pjmatrix();
		project_depth_count++;
		return;
	}
	if (current_matrix_mode==GL_PROJECTION)
		project_depth_count++;
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glPushMatrix\n");
#endif
	glPushMatrix();
}

void glPopMatrix_d()
{
	if ((project_depth_count>UW_project_depth) && (current_matrix_mode==GL_PROJECTION))
	{
/*
.....don't use glPopMatrix function because it excess the max depth of the projection
.....matrix stack of openGL, using our own matrix stack
*/
		uw_glpop_pjmatrix();
		project_depth_count--;
		return;
	}
	if (current_matrix_mode==GL_PROJECTION)
		project_depth_count--;
	matrix_pushed--;
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glPopMatrix\n");
#endif
	glPopMatrix();
}

void glRasterPos2f_d(x,y)
GLfloat x, y; 
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glRasterPos2f\t%f,%f\n", x,y);
#endif
	glRasterPos2f(x,y);
}

void glRasterPos2i_d(x,y)
GLint x,y;  
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glRasterPos2i\t%d,%d\n", x,y);
#endif
	glRasterPos2i(x,y);
}
 
void glRasterPos3f_d(x,y,z)
GLfloat x,y,z;  
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glRasterPos3f\t%f,%f,%f\n", x,y,z);
#endif
	glRasterPos3f(x,y,z);
}

void glRasterPos3i_d(x,y,z)
GLint x,y,z;  
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glRasterPos3i\t%d,%d,%d\n", x,y,z);
#endif
	glRasterPos3i(x,y,z);
}

void glReadBuffer_d(mode)
GLenum mode;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glReadBuffer\t%d\n", mode);
#endif
	glReadBuffer(mode);
}
void glReadPixels_d(x,y,width, height,format,type,pixels)
GLint x, y;        
GLsizei width, height;  
GLenum format, type;
GLvoid *pixels;   
{
#ifdef OPENGL_DEBUG
	if (Slist_begin>0)
	{
		ud_printmsg("Call glReadPixels within a openGL calling list, not allowed"); 
		return;
	}
	fprintf(GLjou_fd,"glReadPixels\t%d,%d,%d,%d,%d,%d\n", 
		x,y,width, height,format,type);
#endif
	glReadPixels(x,y,width, height,format,type,pixels);
}
 

void glRecti_d(x1,y1,x2,y2)
GLint x1, y1,x2,y2;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glRecti\t%d,%d,%d,%d\n", x1, y1,x2,y2);
#endif
	glRecti(x1,y1,x2,y2);
}
 
GLint glRenderMode_d(mode)
GLenum mode;   
{
#ifdef OPENGL_DEBUG
	if (Slist_begin>0)
	{
		ud_printmsg("Call glDeleteLists within a openGL calling list, not allowed"); 
		return 0;
	}
	fprintf(GLjou_fd,"glRenderMode\t%d\n", mode);
#endif
	return glRenderMode(mode);
}
 
void glRotated_d(angle, x,y,z)
GLdouble angle, x,y,z;  
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glRotated\t%f,%f,%f,%f\n", angle, x,y,z);
#endif
	glRotated(angle, x,y,z);
}
 
void glScaled_d(x,y,z)
GLdouble x,y,z;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glScaled\t%f,%f,%f\n", x,y,z);
#endif
	glScaled(x,y,z);
}
  
void glScissor_d(x,y,width,height)
GLint x, y;       
GLsizei width, height; 
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glScissor\t%d,%d,%d,%d\n", x,y,width,height);
#endif
	glScissor(x,y,width,height);
}
 
void glShadeModel_d(mode)
GLenum mode;  
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glShadeModel\t%d\n", mode);
#endif
	glShadeModel(mode);
}
 
void glStencilFunc_d(func, ref, mask)
GLenum func; 
GLint ref;   
GLuint mask; 
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glStencilFunc\t%d,%d,%d\n", func, ref, mask);
#endif
	glStencilFunc(func, ref, mask);
}
 
void glStencilMask_d(mask)
GLuint mask;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glStencilMask\t%d\n", mask);
#endif
	glStencilMask(mask);
}
 
void glStencilOp_d(fail,zfail, zpass)
GLenum fail,zfail, zpass;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glStencilOp\t%d,%d,%d\n", fail,zfail, zpass);
#endif
	glStencilOp(fail,zfail, zpass);
}
  
void glTranslatef_d(x,y,z)
GLfloat x,y,z;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glTranslatef\t%f,%f,%f\n", x,y,z);
#endif
	glTranslatef(x,y,z);
}
 
void glVertex2f_d(x,y)
GLfloat x, y;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glVertex2f\t%f,%f\n", x,y);
#endif
	glVertex2f(x,y);
}

void glVertex2i_d(x,y)
GLint x, y;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glVertex2i\t%d,%d\n", x,y);
#endif
	glVertex2i(x,y);
}
 
 
void glVertex3f_d(x,y,z)
GLfloat x, y,z; 
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glVertex3f\t%f,%f,%f\n", x,y,z);
#endif
	glVertex3f(x,y,z);
}
 
void glVertex3i_d(x,y,z)
GLint x, y,z; 
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glVertex3i\t%d,%d,%d\n", x,y,z);
#endif
	glVertex3i(x,y,z);
}

void glViewport_d(x,y,width,height)
GLint x, y;       
GLsizei width,  height;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glViewport\t%d,%d,%d,%d\n", x,y,width,height);
#endif
	glViewport(x,y,width,height);
}
 
void gluPickMatrix_d(x,y,width,height,viewport)
GLdouble x;       
GLdouble y;       
GLdouble width;   
GLdouble height;  
GLint viewport[4]; 
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"gluPickMatrix\t%f,%f,%f,%f,d,%d,%d,%d\n", 
		x,y,width,height,viewport[0],viewport[1],viewport[2],viewport[3] );
#endif
	gluPickMatrix(x,y,width,height,viewport);
}
 
void glPassThrough_d(token)
GLfloat token;   
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glPassThrough\t%f\n", token);
#endif
	glPassThrough(token);
}
#if UU_COMP == UU_WIN2K 
BOOL SwapBuffers_d(hdc)
HDC  hdc;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"SwapBuffers\twglGetCurrentDC()\n");
#endif
	return SwapBuffers(hdc);
}
#else
void glXSwapBuffers_d(disp, wid)
Display *disp;
Window wid;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glXSwapBuffers\n");
#endif
	glXSwapBuffers(disp, wid);
}
#endif

void glGetDoublev_dd(params)
GLdouble * params;   
{
	glGetDoublev_d(GL_MODELVIEW_MATRIX, params);
}

void glPolygonOffset_d (a,b)
GLfloat a,b;
{
#ifdef OPENGL_DEBUG
	fprintf(GLjou_fd,"glPolygonOffset\t%f, %f\n", a,b);
#endif
	glPolygonOffset (a,b);
}

#endif
