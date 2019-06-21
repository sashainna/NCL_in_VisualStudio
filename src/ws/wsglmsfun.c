#include "zsysdep.h"
/*********************************************************************
**  NAME:  wsglmsfun.c
**
**      GKS OpenGL workstation. similar/same the NCL function
**
**       CONTAINS:
**          uw_glget_depthmask
**          uw_gl_depth_mask
**          uw_gl_depth_func
**				uw_gllight_on
**				uw_gllight_off          
**				uw_glflush              
**				uw_glset_scissor        
**				uw_glget_scissor        
**				uw_glgetbuffer          
**				uw_gldrawbuffer        
**				uw_glswapbuffer         
**				uw_glenable_light       
**				uw_restore_area         
**				uw_gllighting           
**				uw_gllighting_reset     
**	
**    MODULE NAME AND RELEASE LEVEL
**       wsglmsfun.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:06
*********************************************************************/

#include <math.h>
#include "mdmatrix.h"
#if UU_COMP != UU_WIN2K
#include <X11/Xlib.h>
#endif
#include "ws.h"
#include "wsxw.h"
#include "wsgl.h"
#if UU_COMP==UU_WIN2K
#include "wsntglfunc.h"
#endif
#include "gtblws.h"
#include "wsglfun.h"
#include "lcom.h"

int NCL_light_on = 0;
extern int UW_signon;
extern UG_wdt glwdt;
static void S_buffer_swap();
static void S_buffer_copy(int, Gnrect *);
static GLboolean uw_glmask = 1;
static int Scur_clip[4];
static Gbuffer Sdraw_buffer=UG_BOTH_BUFFER;
int UW_light_reset = 0;
extern int UM_material_reset;
extern int MSLite;
extern int UZ_nclipv_view;
static UU_LOGICAL Slighting=UU_FALSE, Slighting_force=UU_FALSE;
typedef struct
{
	int ll[2];
	int ur[2];
} TRK_box;

int UW_print_screen = 0;
int UW_print_back = 1;
/*********************************************************************
**    E_FUNCTION     : uw_glget_depthmask() 
**       Returns the active setting of the OpenGL depth mask.
**    PARAMETERS  
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : 0 = Depth buffer is not being written to,
**                   1 = Depth buffer is being written to.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uw_glget_depthmask()
{
	return uw_glmask;
}

/*********************************************************************
**    E_FUNCTION     : uw_gldepth_mask(flag) 
**       Sets the active OpenGL depth buffer mask.
**    PARAMETERS  
**       INPUT  :
**          flag     = 0 = Don't write to depth buffer, 1 = Do.
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gldepth_mask(flag)
GLboolean flag;
{
	if (uw_glmask==flag)
		return;
	if (flag)
		glDepthMask_d(GL_TRUE);
	else	
		glDepthMask_d(GL_FALSE);
	uw_glmask = flag;
}

/*********************************************************************
**    E_FUNCTION     : uw_gldepth_func(flag) 
**       Sets the active depth buffer comparison mode.
**    PARAMETERS  
**       INPUT  :
**          flag     - 0 = Always display graphics.
**                     1 = Display only graphics that pass depth buffer
**                         test.  This is the normal mode.
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gldepth_func(flag)
int flag;
{
	if (flag == 0) glDepthFunc_d(GL_ALWAYS);
	else glDepthFunc_d(GL_LEQUAL);
}

/*********************************************************************
**    FUNCTION     :  uw_gllight_on (shade)
**
**       Sets up light conditions
**
**    PARAMETERS   
**       INPUT  : shade: not used now
**          
**       OUTPUT :  
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_gllight_on (shade)
int shade;
{
	int i,blanked;
	GLfloat angle;
	int save_active = 1;
	GLfloat diffuseLight[] = {1.0,1.0,1.0,1.0};
	GLfloat pos[] = {1.0,1.0,1.0,1.0};
	GLfloat dir[] = {1.0,1.0,1.0,1.0};
	GLfloat ambLight[] = {1.0,1.0,1.0,1.0};

	GLfloat specular[] = {0.01,0.01,0.01,1.0};
	GLfloat st_rgb[3];
	GLfloat spos[] = {0, 0, 1.0, 0};
	i = 0;
/*
... use Phong shading, not Gourard (GL_FLAT);
*/
	glShadeModel_d(GL_SMOOTH);
/*
... CCW poligons are front ones
*/
	glFrontFace_d(GL_CCW);
/*
... initialize light model
*/
	glLightModeli_d(GL_LIGHT_MODEL_LOCAL_VIEWER, GL_FALSE);
	glLightModeli_d(GL_LIGHT_MODEL_TWO_SIDE, GL_TRUE);
	glEnable_d(GL_LIGHTING);
	glDisable_d(GL_LIGHT0);
	glDisable_d(GL_LIGHT1);
	glDisable_d(GL_LIGHT2);
	glDisable_d(GL_LIGHT3);
	glDisable_d(GL_LIGHT4);
/*		color = 4;
		st_rgb[0] = uw_color_table[color][0]/255.0;
		st_rgb[1] = uw_color_table[color][1]/255.0;
		st_rgb[2] = uw_color_table[color][2]/255.0;
/*
... initialize light # 0
*/
/*		diffuseLight[0] = st_rgb[0];
		diffuseLight[1] = st_rgb[1];
		diffuseLight[2] = st_rgb[2];
*/
/*		specular[0] = st_rgb[0];
		specular[1] = st_rgb[1];
		specular[2] = st_rgb[2];
*/
		pos[0] = 0;
		pos[1] = 0;
		pos[2] = 0;

		dir[0] = 0;
		dir[1] = 0;
		dir[2] = 1;
		angle = 30;
		ambLight[0] = 0.2;
		ambLight[1] = 0.2;
		ambLight[2] = 0.2;
		ambLight[3] = 1.0;
		switch (i)
		{
		case 0:
			{
				glLightfv_d(GL_LIGHT0, GL_AMBIENT, ambLight);
				glLightfv_d(GL_LIGHT0, GL_DIFFUSE, diffuseLight);
				glLightfv_d(GL_LIGHT0, GL_SPECULAR, specular);
/*
.....screen
*/
					glMatrixMode_d( GL_PROJECTION);
					glPushMatrix_d();
					glMatrixMode_d( GL_MODELVIEW );
					glPushMatrix_d();
/*
....Set the Matrix of
....the original screen in NC 
*/
					glMatrixMode_d(GL_PROJECTION);
					glLoadIdentity_d();
					glMatrixMode_d(GL_MODELVIEW);
					glLoadIdentity_d();
					spos[0] = dir[0];
					spos[1] = dir[1];
					spos[2] = dir[2];
					glLightfv_d(GL_LIGHT0, GL_POSITION, spos);
					glLightf_d(GL_LIGHT0, GL_SPOT_CUTOFF, (GLfloat)180.0);
					glMatrixMode_d( GL_PROJECTION);
					glPopMatrix_d();
					glMatrixMode_d( GL_MODELVIEW );
					glPopMatrix_d();
					glEnable_d(GL_LIGHT0);
				break;
			}
		case 1:
			{
				glLightfv_d(GL_LIGHT1, GL_AMBIENT, ambLight);
				glLightfv_d(GL_LIGHT1, GL_DIFFUSE, diffuseLight);
				glLightfv_d(GL_LIGHT1, GL_SPECULAR, specular);
/*
.....screen
*/
					glMatrixMode_d( GL_PROJECTION);
					glPushMatrix_d();
					glMatrixMode_d( GL_MODELVIEW );
					glPushMatrix_d();
/*
....Set the Matrix of
....the original screen in NC 
*/
					glMatrixMode_d(GL_PROJECTION);
					glLoadIdentity_d();
					glMatrixMode_d(GL_MODELVIEW);
					glLoadIdentity_d();
					spos[0] = dir[0];
					spos[1] = dir[1];
					spos[2] = dir[2];
					glLightfv_d(GL_LIGHT1, GL_POSITION, spos);
					glLightf_d(GL_LIGHT1, GL_SPOT_CUTOFF, (GLfloat)180.0);
					glMatrixMode_d( GL_PROJECTION);
					glPopMatrix_d();
					glMatrixMode_d( GL_MODELVIEW );
					glPopMatrix_d();
					glEnable_d(GL_LIGHT1);
				break;
			}
		case 2:
			{
				glLightfv_d(GL_LIGHT2, GL_AMBIENT, ambLight);
				glLightfv_d(GL_LIGHT2, GL_DIFFUSE, diffuseLight);
				glLightfv_d(GL_LIGHT2, GL_SPECULAR, specular);
/*
.....screen
*/
					glMatrixMode_d( GL_PROJECTION);
					glPushMatrix_d();
					glMatrixMode_d( GL_MODELVIEW );
					glPushMatrix_d();
/*
....Set the Matrix of
....the original screen in NC 
*/
					glMatrixMode_d(GL_PROJECTION);
					glLoadIdentity_d();
					glMatrixMode_d(GL_MODELVIEW);
					glLoadIdentity_d();
					spos[0] = dir[0];
					spos[1] = dir[1];
					spos[2] = dir[2];
					glLightfv_d(GL_LIGHT2, GL_POSITION, spos);
					glLightf_d(GL_LIGHT2, GL_SPOT_CUTOFF, (GLfloat)180.0);
					glMatrixMode_d( GL_PROJECTION);
					glPopMatrix_d();
					glMatrixMode_d( GL_MODELVIEW );
					glPopMatrix_d();
					glEnable_d(GL_LIGHT2);
				break;
			}
		case 3:
			{
				glLightfv_d(GL_LIGHT3, GL_AMBIENT, ambLight);
				glLightfv_d(GL_LIGHT3, GL_DIFFUSE, diffuseLight);
				glLightfv_d(GL_LIGHT3, GL_SPECULAR, specular);
/*
.....screen
*/
					glMatrixMode_d( GL_PROJECTION);
					glPushMatrix_d();
					glMatrixMode_d( GL_MODELVIEW );
					glPushMatrix_d();
/*
....Set the Matrix of
....the original screen in NC 
*/
					glMatrixMode_d(GL_PROJECTION);
					glLoadIdentity_d();
					glMatrixMode_d(GL_MODELVIEW);
					glLoadIdentity_d();
					spos[0] = dir[0];
					spos[1] = dir[1];
					spos[2] = dir[2];
					glLightfv_d(GL_LIGHT3, GL_POSITION, spos);
					glLightf_d(GL_LIGHT3, GL_SPOT_CUTOFF, (GLfloat)180.0);
					glMatrixMode_d( GL_PROJECTION);
					glPopMatrix_d();
					glMatrixMode_d( GL_MODELVIEW );
					glPopMatrix_d();
					glEnable_d(GL_LIGHT3);
				break;
			}
		case 4:
			{
				glLightfv_d(GL_LIGHT4, GL_AMBIENT, ambLight);
				glLightfv_d(GL_LIGHT4, GL_DIFFUSE, diffuseLight);
				glLightfv_d(GL_LIGHT4, GL_SPECULAR, specular);
/*
.....screen
*/
					glMatrixMode_d( GL_PROJECTION);
					glPushMatrix_d();
					glMatrixMode_d( GL_MODELVIEW );
					glPushMatrix_d();
/*
....Set the Matrix of
....the original screen in NC 
*/
					glMatrixMode_d(GL_PROJECTION);
					glLoadIdentity_d();
					glMatrixMode_d(GL_MODELVIEW);
					glLoadIdentity_d();
					spos[0] = dir[0];
					spos[1] = dir[1];
					spos[2] = dir[2];
					glLightfv_d(GL_LIGHT4, GL_POSITION, spos);
					glLightf_d(GL_LIGHT4, GL_SPOT_CUTOFF, (GLfloat)180.0);
					glMatrixMode_d( GL_PROJECTION);
					glPopMatrix_d();
					glMatrixMode_d( GL_MODELVIEW );
					glPopMatrix_d();
					glEnable_d(GL_LIGHT4);
				break;
			}
		}
/*
... unitize normals when calling glNormal ()
*/
	glEnable_d(GL_NORMALIZE);
	NCL_light_on = 1;
	return (0);
}
/*********************************************************************
**    FUNCTION     :  uw_gllight_off()
**
**       turn off light conditions
**
**    PARAMETERS   
**       INPUT  : 
**          
**       OUTPUT :  
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_gllight_off ()
{
	if (NCL_light_on == 0) return 0;

	glShadeModel_d(GL_FLAT);

	glDisable_d(GL_LIGHT0);
	glDisable_d(GL_LIGHT1);
	glDisable_d(GL_LIGHT2);
	glDisable_d(GL_LIGHT3);
	glDisable_d(GL_LIGHT4);
	glDisable_d(GL_LIGHTING);
	glDisable_d(GL_NORMALIZE);
	NCL_light_on = 0;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : uw_glflush()
**       Flushes the graphics.
**       
**    PARAMETERS  
**       INPUT  :
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glflush()
{
	glFlush_d();
}
/*********************************************************************
**    E_FUNCTION     : uw_glset_scissor(clip) 
**       Sets the active graphics clipping (scissor) area.
**    PARAMETERS  
**       INPUT  :
**          clip     = Clipping rectangle in pixels.
**       OUTPUT :
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glset_scissor(clip)  
int clip[];
{
	int i, j;
	for (i=0; i<4; i++)
	{
		if (clip[i]!=Scur_clip[i])
		{
			glScissor_d(clip[0], clip[1], clip[2], clip[3]);
			for (j=0;j<4; j++)
				Scur_clip[j] = clip[j];
			return;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : uw_glget_scissor(clip) 
**       Returns the active graphics clipping (scissor) area.
**    PARAMETERS  
**       INPUT  :
**       OUTPUT :
**          clip     = Clipping rectangle in pixels.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glget_scissor(clip)
int clip[];
{
	int i;
	for (i=0; i<4; i++)
		clip[i] = Scur_clip[i];
}

/*********************************************************************
**    E_FUNCTION     : uw_glgetbuffer()
**       Returns the active drawing buffer.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT : 
**          none.
**    RETURNS      :
**          UG_BACK, UG_FRONT, UGBOTH.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gbuffer uw_glgetbuffer()
{
/*
.....Return the active drawing buffer
*/
	return(Sdraw_buffer);
}

/*********************************************************************
**    E_FUNCTION     : uw_gldrawbuffer(buffer)
**       Sets the appropriate drawing buffer.
**    PARAMETERS
**       INPUT  :
**          buffer   = UG_BACK_BUFFER, UG_FRONT_BUFFER, UG_BOTH_BUFFER.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gldrawbuffer(buffer)
Gbuffer buffer;
{
/*
.....Set correct drawing buffer
*/
	if (buffer != Sdraw_buffer)
	{
		switch (buffer)
		{
		case UG_BACK_BUFFER:;
			glDrawBuffer_d(GL_BACK);
			break;
		case UG_FRONT_BUFFER:;
			glDrawBuffer_d(GL_FRONT);
			break;
		case UG_BOTH_BUFFER:;
			glDrawBuffer_d(GL_FRONT_AND_BACK);
			break;
		}
		Sdraw_buffer = buffer;
	}
}

/*********************************************************************
**    E_FUNCTION     : uw_glswapbuffer(rect)
**       Copies an area of the back buffer to the front buffer, using
**       the default method (SwapBuffers or CopyPixel).
**    PARAMETERS
**       INPUT  :
**          rect     = Area of back buffer to copy to front buffer.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glswapbuffer(rect)
Gnrect *rect;
{
/*
......if it is print screen, don't do the swap
*/
	if (UW_print_screen==1)
		return;
/*
.....Copy back buffer area to front buffer
*/
	if (UW_disp_buf == 1)
		S_buffer_copy(UU_TRUE,rect);
/*
.....Use swap buffer
*/
	else
		S_buffer_swap();
/*
.....Flush the graphics
*/
	glFlush_d();
}

/*********************************************************************
**    I_FUNCTION     : S_buffer_swap()
**       Swaps the back buffer with the front buffer when in double
**       buffer mode.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_buffer_swap()
{
#if UU_COMP!=UU_WIN2K
	glXSwapBuffers_d(uw_xw.disp, uw_xw.wd_id);
#else
	SwapBuffers_d(wglGetCurrentDC());
#endif
}

/*********************************************************************
**    I_FUNCTION     : S_buffer_copy(flag, rect)
**       Copy the back buffer graphic the front buffer 
**    PARAMETERS
**       INPUT  :
**          flag: UU_FALSE = Copy all screen, 'rect' is ignored.
**                UU_TRUE  = Copy rectangle area.
**			   rect: Area to copy when 'flag' = UU_TRUE.
**       OUTPUT : 
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_buffer_copy(flag,segrect)
int flag;
Gnrect *segrect;
{
	int llras[2],urras[2],wid,hgt;
	GLint t,b;
	Gbuffer d;
/*
.....Graphics area is not ready yet
*/
	if (UW_signon==0) return;
/*
.....Store current settings
*/
	glGetIntegerv_d(GL_DEPTH_TEST,&t);

	d = uw_glgetbuffer();

	glGetIntegerv_d(GL_BLEND, &b);
	if (t) glDisable_d(GL_DEPTH_TEST);
	if (b) glDisable_d(GL_BLEND);
	glReadBuffer_d(GL_BACK);
	uw_gldrawbuffer(UG_FRONT_BUFFER);
/*
.....Set up the proper matrix
*/
	uw_glpushx();		
	glMatrixMode_d(GL_PROJECTION);
	glLoadIdentity_d();
	glOrtho_d((GLdouble)0,(GLdouble)(glwdt.dspsize.raster.x),
				(GLdouble)0,(GLdouble)(glwdt.dspsize.raster.y),
				(GLdouble)(-1000),(GLdouble)1000);
	glViewport_d(0,0,glwdt.dspsize.raster.x,glwdt.dspsize.raster.y);
	glMatrixMode_d(GL_MODELVIEW);
	glLoadIdentity_d();
/*
.....Define the rectangle
*/
	if (flag)
	{
		uw_glndctodev(&(*segrect).ll,llras);
		uw_glndctodev(&(*segrect).ur,urras);
		llras[0] -= 2;
		llras[1] -= 2;
		urras[0] += 2;
		urras[1] += 2;
	}
	else
	{
		llras[0] = llras[1] = 0;
		urras[0] = glwdt.dspsize.raster.x;
		urras[1] = glwdt.dspsize.raster.y;
	}
	if (llras[0]<=1)  llras[0] = 0;
	if (llras[1]<=1)  llras[1] = 0;
	if (urras[0] > glwdt.dspsize.raster.x) urras[0] = glwdt.dspsize.raster.x;
	if (urras[1] > glwdt.dspsize.raster.y) urras[1] = glwdt.dspsize.raster.y;
	wid = urras[0] - llras[0] + 1;
	hgt = urras[1] - llras[1] + 1;
/*
.....Copy the rectangle
*/
	if (wid > 0 && hgt > 0)
	{
		glRasterPos2i_d(llras[0],llras[1]);
		glCopyPixels_d(llras[0],llras[1],wid,hgt,GL_COLOR);
	}
/*
.....Restore the settings
*/
	if (t) glEnable_d(GL_DEPTH_TEST);
	if (b) glEnable_d(GL_BLEND);
	uw_gldrawbuffer(d);
	uw_glpopx();
}
/*********************************************************************
**    FUNCTION     :  uw_glenable_light(num, flag, force)
**
**       Enable/Disable light variable
**
**    PARAMETERS   
**       INPUT  : num: A symbolic constant indicating an OpenGL light capability. 
**					flag: 1: enable the capability
**							0: Disable the capability
**					force: 1: force to do it
**							0: if it is same as before, do not force to enable/disable it
**         
**
**       OUTPUT :  
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glenable_light(num, flag, force)
GLenum num;
int flag, force;
{
	static GLenum cur_light0 = 0;
	static GLenum cur_light1 = 0;
	static GLenum cur_light2 = 0;
	static GLenum cur_light3 = 0;
	static GLenum cur_light4 = 0;
	static GLenum cur_lighting = 0;
	static GLenum cur_normal = 0;
	if (num==GL_LIGHT0)
	{
		if (flag==1)
		{
			if ((cur_light0==0) || (force==1) || (UW_light_reset))
			{
				cur_light0 = num;
				glEnable_d(num);
			}
		}
		else if (flag==0)
		{
			glDisable_d(num);
			cur_light0 = 0;
		}
		return;
	}
	if (num==GL_LIGHT1)
	{
		if (flag==1)
		{
			if ((cur_light1==0) || (force==1) || (UW_light_reset))
			{
				cur_light1 = num;
				glEnable_d(num);
			}
		}
		else if (flag==0)
		{
			glDisable_d(num);
			cur_light1 = 0;
		}
		return;
	}
	if (num==GL_LIGHT2)
	{
		if (flag==1)
		{
			if ((cur_light2==0) || (force==1) || (UW_light_reset))
			{
				cur_light2 = num;
				glEnable_d(num);
			}
		}
		else if (flag==0)
		{
			glDisable_d(num);
			cur_light2 = 0;
		}
		return;
	}
	if (num==GL_LIGHT3)
	{
		if (flag==1)
		{
			if ((cur_light3==0) || (force==1) || (UW_light_reset))
			{
				cur_light3 = num;
				glEnable_d(num);
			}
		}
		else if (flag==0)
		{
			glDisable_d(num);
			cur_light3 = 0;
		}
		return;
	}
	if (num==GL_LIGHT4)
	{
		if (flag==1)
		{
			if ((cur_light4==0) || (force==1) || (UW_light_reset))
			{
				cur_light4 = num;
				glEnable_d(num);
			}
		}
		else if (flag==0)
		{
			glDisable_d(num);
			cur_light4 = 0;
		}
		return;
	}
	if (num==GL_LIGHTING)
	{
		if (flag==1)
		{
			if ((cur_lighting==0) || (force==1) || (UW_light_reset))
			{
				cur_lighting = num;
				glEnable_d(num);
			}
		}
		else if (flag==0)
		{
			glDisable_d(num);
			cur_lighting = 0;
		}
		return;
	}
	if (num==GL_NORMALIZE)
	{
		if (flag==1)
		{
			if ((cur_normal==0) || (force==1) || (UW_light_reset))
			{
				cur_normal = num;
				glEnable_d(num);
			}
		}
		else if (flag==0)
		{
			glDisable_d(num);
			cur_normal = 0;
		}
		return;
	}
}

/*********************************************************************
**    I_FUNCTION     : uw_restore_area(box)
**         Restores the area defined by the box to a previous state.
**         Depending on the setting, the area is either copied from the
**         back buffer or the entire back buffer is copied to the front
**         buffer.
**
**    PARAMETERS
**       INPUT  :
**          box    = Area to restore.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_restore_area(box)
TRK_box *box;
{
	Gnrect segrect;
/*
.....IPV is active
.....restore area behind box
*/
	if ((UZ_nclipv_view == 1)||(MSLite))
	{
		ul_ipv_view_rect (box->ll[0],box->ll[1],box->ur[0],box->ur[1]);
	}
/*
.....Restore area by
.....Copying from the back buffer
.....To the front buffer
*/
	else
	{
		uw_glpushx();
		uw_gldevtondc(box->ll,(Gfloat *)&(segrect.ll));
		uw_gldevtondc(box->ur,(Gfloat *)&(segrect.ur));
		uw_glmark_dirty_rect(&segrect);
		uw_glupdate_front(UG_SUPPRESS);
		uw_glpopx();
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    FUNCTION     :  uw_gllighting(flag)
**
**       Enables/Disables lighting.
**
**    PARAMETERS   
**       INPUT  :
**          flag   = UU_FALSE = Disable lighting, UU_TRUE = Enable.
**       OUTPUT : none
**          
**    RETURNS      : Current lighting state prior to changing it.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL uw_gllighting(flag)
{
	UU_LOGICAL iret=Slighting;
/*
.....Don't do anything if lights are not initialized
*/
	if ((MSLite==0)&&(!um_is_light_init())) return(iret);
/*
.....Enable lighting
*/
	if (flag)
	{
		if (!Slighting || Slighting_force) glEnable_d(GL_LIGHTING);
		Slighting = UU_TRUE;
	}
/*
.....Disable lighting
*/
	else
	{
		if (Slighting || Slighting_force) glDisable_d(GL_LIGHTING);
		Slighting = UU_FALSE;
/*
........When light is off
........material is not off too
........Next time when we call material function,
........we need to reset material
*/
		UM_material_reset = 1;
	}
/*
.....End of routine
*/
	Slighting_force = UU_FALSE;
	return(iret);
}

/*********************************************************************
**    FUNCTION     :  uw_gllighting_reset()
**
**       Forces the output of the lighting state on the next call.
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gllighting_reset()
{
/*
.....Set the current lighting mode
*/
	Slighting_force = UU_TRUE;
}

