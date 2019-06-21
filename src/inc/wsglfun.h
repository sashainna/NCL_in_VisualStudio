#include "zsysdep.h"
#ifdef UU_OPENGL
/*********************************************************************
**    NAME         :  wsglfun.h
**
**       CONTAINS:
**		GKS openGL workstation routines. All routines are just call
**		openGL library routine plus generate journal file if debug
**		Not Include 'wglxxxx" functions
**
**    MODULE NAME AND RELEASE LEVEL 
**       wsglfun.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:10
*********************************************************************/

#ifndef WSGLFUN_H
#define WSGLFUN_H

#include <stdio.h>
#include "wsgl.h"


#ifdef __cplusplus 
	#define EXT extern "C"
/*
#else
	#define EXT
#endif
*/
EXT void uw_glenable(GLenum num, int flag, int force);
EXT void glBegin_d(GLenum mode);
EXT void glEnd_d();
EXT void glBitmap_d(  
	GLsizei width,           
	GLsizei height,          
	GLfloat xorig,           
	GLfloat yorig,           
	GLfloat xmove,           
	GLfloat ymove,           
	GLubyte * bitmap, int mtyp);   
EXT void glBlendFunc_d(GLenum sfactor, GLenum dfactor);
EXT void glCallList_d(GLuint list);
EXT void glCallLists_d(GLuint list, GLenum type, GLvoid * lists); 
EXT void glClear_d(GLbitfield mask);
EXT void glClearColor_d(
	GLclampf red,    
	GLclampf green,  
	GLclampf blue,   
	GLclampf alpha);
 
EXT void glClearDepth_d(
  GLclampd depth   
); 

EXT void glClearStencil_d(
  GLint s   
);

EXT void glColor3f_d(
  GLfloat red,     
  GLfloat green,   
  GLfloat blue     
);
 
EXT void glColorMask_d(
  GLboolean red,    
  GLboolean green,  
  GLboolean blue,   
  GLboolean alpha   
);
  
EXT void glCopyPixels_d(
  GLint x,         
  GLint y,         
  GLsizei width,   
  GLsizei height,  
  GLenum type      
);
 
EXT void glDeleteLists_d(
  GLuint list,   
  GLsizei range  
);

EXT void glDepthFunc_d(
  GLenum func   
);
 

EXT void glDepthMask_d(
  GLboolean flag   
);

EXT void glDepthRange_d(
  GLclampd znear,   
  GLclampd zfar     
);
 
EXT void glEnable_d(
  GLenum cap   
);
EXT void glDisable_d(
  GLenum cap   
);
 

EXT void glDrawBuffer_d(
  GLenum mode   
);

EXT void glDrawPixels_d(
  GLsizei width,         
  GLsizei height,        
  GLenum format,         
  GLenum type,           
  const GLvoid *pixels   
);
 
EXT void glNewList_d(
  GLuint list,  
  GLenum mode   
);
 
EXT void glEndList_d(
  void   
);
 
EXT void glFeedbackBuffer_d(
  GLsizei size,      
  GLenum type,       
  GLfloat * buffer   
);

EXT void glFinish_d(
  void   
);

 
EXT void glFlush_d(
  void   
);

EXT void glFrontFace_d(
  GLenum mode   
);
 
EXT GLuint glGenLists_d(
  GLsizei range   
);
  
EXT void glGetDoublev_d(
  GLenum pname,       
  GLdouble * params   
);
  
EXT void glGetIntegerv_d(
  GLenum pname,   
  GLint * params  
);
        
EXT GLboolean glIsEnabled_d(
  GLenum cap   
);
 
EXT GLboolean glIsList_d(
  GLuint list   
);

EXT void glLightf_d(
  GLenum light,   
  GLenum pname,   
  GLfloat param   
);
  
EXT void glLightfv_d(
  GLenum light,           
  GLenum pname,           
  const GLfloat *params   
);
 
EXT void glLightModeli_d(
  GLenum pname,   
  GLint param     
);
 
EXT void glLineStipple_d(
  GLint factor,      
  GLushort pattern   
);
 
EXT void glLineWidth_d(
  GLfloat width   
);

EXT void glListBase_d(
  GLuint base   
);

EXT void glLoadIdentity_d(
  void   
);

EXT void glLoadMatrixd_d(
  const GLdouble *m   
);
   
EXT void glMateriali_d(
  GLenum face,   
  GLenum pname,  
  GLint param    
);
 
EXT void glMaterialfv_d(
  GLenum face,            
  GLenum pname,           
  const GLfloat *params   
);
  
EXT void glMatrixMode_d(
  GLenum mode   
);
 

EXT void glMultMatrixd_d(
  const GLdouble *m   
);
 
EXT void glNormal3f_d(
  GLfloat nx,  
  GLfloat ny,  
  GLfloat nz   
);
 
EXT void glOrtho_d(
  GLdouble left,    
  GLdouble right,   
  GLdouble bottom,  
  GLdouble top,     
  GLdouble near,    
  GLdouble far      
);
  
EXT void glPixelStorei_d(
  GLenum pname,   
  GLint param     
);
EXT void glPixelTransferf_d(
  GLenum pname,   
  GLfloat param   
);
  
EXT void glPolygonMode_d(
  GLenum face,  
  GLenum mode   
);
  
EXT void glPushAttrib_d(
  GLbitfield mask   
);
 
EXT void glPopAttrib_d(
  void   
);
 
EXT void glPushMatrix_d(
  void   
);
 
EXT void glPopMatrix_d(
  void   
);

EXT void glRasterPos2f_d(
  GLfloat x,  
  GLfloat y   
);
 
EXT void glRasterPos2i_d(
  GLint x,  
  GLint y   
);
  
EXT void glRasterPos3f_d(
  GLfloat x,  
  GLfloat y,  
  GLfloat z   
);
 
EXT void glRasterPos3i_d(
  GLint x,  
  GLint y,  
  GLint z   
);
 
EXT void glReadBuffer_d(
  GLenum mode   
);
 
EXT void glReadPixels_d(
  GLint x,         
  GLint y,         
  GLsizei width,   
  GLsizei height,  
  GLenum format,   
  GLenum type,     
  GLvoid *pixels   
);
 

 
EXT void glRecti_d(
  GLint x1,  
  GLint y1,  
  GLint x2,  
  GLint y2   
);
  
EXT GLint glRenderMode_d(
  GLenum mode   
);
 
EXT void glRotated_d(
  GLdouble angle,   
  GLdouble x,       
  GLdouble y,       
  GLdouble z        
);
  
EXT void glScaled_d(
  GLdouble x,   
  GLdouble y,   
  GLdouble z    
);
  
EXT void glScissor_d(
  GLint x,        
  GLint y,        
  GLsizei width,  
  GLsizei height  
);
 
EXT void glShadeModel_d(
  GLenum mode  
);
 
EXT void glStencilFunc_d(
  GLenum func, 
  GLint ref,   
  GLuint mask  
);
 
EXT void glStencilMask_d(
  GLuint mask  
);
 
EXT void glStencilOp_d(
  GLenum fail,  
  GLenum zfail, 
  GLenum zpass  
);
  
EXT void glTranslatef_d(
  GLfloat x, 
  GLfloat y, 
  GLfloat z  
);
 
EXT void glVertex2f_d(
  GLfloat x, 
  GLfloat y  
);
 
EXT void glVertex2i_d(
  GLint x, 
  GLint y  
);
 
 
EXT void glVertex3f_d(
  GLfloat x,  
  GLfloat y,  
  GLfloat z   
);
 
EXT void glVertex3i_d(
  GLint x, 
  GLint y, 
  GLint z  
);
 
EXT void glViewport_d(
  GLint x,        
  GLint y,        
  GLsizei width,  
  GLsizei height  
);
 
EXT void gluPickMatrix_d(
  GLdouble x,       
  GLdouble y,       
  GLdouble width,   
  GLdouble height,  
  GLint viewport[4] 
);
 
EXT void glPassThrough_d(
  GLfloat token   
);
  
#if UU_COMP == UU_WIN2K
EXT BOOL SwapBuffers_d(
  HDC  hdc 
);
#else
EXT void glXSwapBuffers_d(Display *disp, Window wid);
#endif
#endif
#undef EXT

#endif
#endif
