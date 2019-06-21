#include "usysdef.h"
#ifdef UU_OPENGL

/*********************************************************************
**  NAME:  wsglshade.c
**
**    CONTAINS:
**        uw_glinit_shading
**        uw_gllight_define
**        uw_gllight_pos
**        uw_gllighting
**        uw_gldefine_material
**        uw_glget_material
**        uw_glrestore_material
**        uw_glget_wsshade 
**	       uw_glset_stencil
**	       uw_glget_stencil
**
**    MODULE NAME AND RELEASE LEVEL 
**       wsglshade.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:12:08
**    
*********************************************************************/

#ifdef VMS
#include <decw$include:Xlib.h>
#include <decw$include:Xutil.h>
#else
#if UU_COMP != UU_WIN2K 
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#endif
#endif

#include "wsgl.h"
#include "udebug.h"
#include "zsysdep.h"
#include "driver.h"
#include "gviw.h"
#include "gsegop.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "gsegac.h"
#if UU_COMP != UU_WIN2K 
#include "wsxw.h"
#else
#include "wsntglfunc.h"
#endif
#include "ginqatti.h"
#include "gmat4.h"
#include "mconst.h"
#include "mdattr.h"
#include "mrender.h"
#include "mattrddl.h"
#include "mdcoord.h"
#include "mrenddl.h"
#include "mdrel.h"
#include "wsglfun.h"

#define DBG UU_FALSE

extern int UM_material_reset;
extern int UM_light_keys[];
extern UG_wdt glwdt;
extern int UR_active;
extern int UW_print_screen, UW_print_back;

static int Current_clrinx = -1, Current_mat = -1, Current_lucency = -1;
static UU_LOGICAL Slighting=UU_FALSE, Slighting_force=UU_FALSE;
static UU_LOGICAL Stencil_on=UU_TRUE;
static int Slights_on[] = {-1,-1,-1,-1,-1};
static GLenum Slights[5] = {GL_LIGHT0,GL_LIGHT1,GL_LIGHT2,GL_LIGHT3,GL_LIGHT4};

void uw_gllight_pos(),uw_glenable_light();

/*********************************************************************
**    FUNCTION     :  uw_glinit_shading()
**
**       Initializes lighting and shading parameters.
**
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glinit_shading()
{
	glLightModeli_d(GL_LIGHT_MODEL_LOCAL_VIEWER,GL_FALSE);
	glLightModeli_d(GL_LIGHT_MODEL_TWO_SIDE,GL_TRUE);
	glShadeModel_d(GL_SMOOTH);
	glFrontFace_d(GL_CCW);
	glEnable_d(GL_NORMALIZE);
}

/*********************************************************************
**    FUNCTION     :  uw_gllight_define()
**
**       Defines the active lights.
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
void uw_gllight_define()
{
	int i, color,blanked;
	int save_active=1;
	UU_LOGICAL pushed=UU_FALSE;
	struct UM_attrdata_rec attr;
	struct UM_light_rec e;
	GLfloat diffuseLight[4],ambLight[4],specular[4],st_rgb[3],dir[3],angle;
	GLfloat pos[4];
/*
.....Don't do anything if lights are not initialized
*/
	if (!um_is_light_init()) return;
/*
.....When setting up lighting
.....we will always using the working Unibase atrributes
.....because the lights are not loaded in the secondary Unibase
*/
	if (UR_active == 2)
	{
		save_active = UR_active;
		ur_getu_work();
	}
/*
.....Loop through the lights
*/
	for (i=0;i<5;i++)
	{
		e.key = UM_light_keys[i];
		ur_retrieve_blanked(e.key, &blanked);
/*
........Disable inactive lights
*/
		if (blanked)
		{
			if (Slights_on[i] != 2) glDisable_d(Slights[i]);
			Slights_on[i] = 0;
			continue;
		}
/*
........Get the light from the Unibase
*/
		e.rel_num = UM_LIGHT_REL;
		uc_retrieve_data(&e, sizeof(struct UM_light_rec));
		um_get_disp_attr(e.key, &attr);	
/*
........Initialize the light properties
*/
/*		color = attr.color-1;  
		color = ncl_set_color(color); */
		color = attr.color;
		if (color<0) color = 1;
#if UU_COMP == UU_WIN2K
		if (color >= 64) color = 63;
#else
		if (color >= 16) color = 15;
#endif
		st_rgb[0] = uw_color_table[color][0]/255.0;
		st_rgb[1] = uw_color_table[color][1]/255.0;
		st_rgb[2] = uw_color_table[color][2]/255.0;

		diffuseLight[0] = st_rgb[0]*(e.intens/100.0);
		diffuseLight[1] = st_rgb[1]*(e.intens/100.0);
		diffuseLight[2] = st_rgb[2]*(e.intens/100.0);
		diffuseLight[3] = 1.;

		specular[0] = st_rgb[0]*(e.intens/100.0);
		specular[1] = st_rgb[1]*(e.intens/100.0);
		specular[2] = st_rgb[2]*(e.intens/100.0);
		specular[3] = 1.;

		ambLight[0] = e.ambient[0];
		ambLight[1] = e.ambient[1];
		ambLight[2] = e.ambient[2];
		ambLight[3] = e.ambient[3];
/*
.....Define the light properties
*/
		glLightfv_d(Slights[i], GL_AMBIENT, ambLight);
		glLightfv_d(Slights[i], GL_DIFFUSE, diffuseLight);
		glLightfv_d(Slights[i], GL_SPECULAR, specular);
/*
........Set the Screen Coordinate Matrix
*/
		if (e.space == UM_SCREEN_POS)
		{
			glMatrixMode_d( GL_PROJECTION);
			glPushMatrix_d();
			glLoadIdentity_d();
			glMatrixMode_d( GL_MODELVIEW );
			glPushMatrix_d();
			glLoadIdentity_d();
			pushed = UU_TRUE;
		}
/*
........Set light direction for Directional light
*/

		if (e.type == UM_EYE_LIGHT)
		{
			dir[0] = e.direction[0];
			dir[1] = e.direction[1];
			dir[2] = e.direction[2];
			dir[3] = 0.;
			glLightfv_d(Slights[i], GL_POSITION, dir);
		}
/*
.....Set spot direction
*/
		else
		{
			if (e.type == UM_SPOT_LIGHT)
			{
				dir[0] = e.direction[0];
				dir[1] = e.direction[1];
				dir[2] = e.direction[2];
				glLightfv_d(Slights[i],GL_SPOT_DIRECTION,dir);
				angle = e.cone_angle;
			}
			else
				angle = 180.0;
			glLightf_d(Slights[i],GL_SPOT_CUTOFF,angle);
/*
........Set light position for Point & Spot Lights
*/
			pos[0] = e.position[0];
			pos[1] = e.position[1];
			pos[2] = e.position[2];
			pos[3] = 1.;
			glLightfv_d(Slights[i], GL_POSITION, pos);
		}
/*
........Reset viewing matrices
*/
		if (pushed)
		{
			glMatrixMode_d( GL_PROJECTION);
			glPopMatrix_d();
			glMatrixMode_d( GL_MODELVIEW );
			glPopMatrix_d();
			pushed = UU_FALSE;
		}
/*
........Enable the light
*/
		if (Slights_on[i] != 2) glEnable_d(Slights[i]);
		Slights_on[i] = 1;
	}
/*
.....Restore secondary Unibase
*/
	if (save_active == 2) ur_getu_second();
	return;
}

/*********************************************************************
**    FUNCTION     :  uw_gllight_pos(flag)
**
**       Positions and sets the directions for all lights.
**
**    PARAMETERS   
**       INPUT  :
**          flag    = UU_TRUE = Position both screen and world coordinate
**                    lights.  UU_FALSE = Position only world coordinate
**                    lights.
**          
**       OUTPUT :  none
**          
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_gllight_pos(flag)
UU_LOGICAL flag;
{
	int i,blanked,save_active=1;
	UU_LOGICAL pushed=UU_FALSE;
	struct UM_attrdata_rec attr;
	struct UM_light_rec e;
	GLfloat pos[4];
/*
.....Don't do anything if lights are not initialized
*/
	if (!um_is_light_init()) return;
/*
.....When setting up lighting
.....we will always using the working Unibase atrributes
.....because the lights are not loaded in the secondary Unibase
*/
	if (UR_active == 2)
	{
		save_active = UR_active;
		ur_getu_work();
	}
/*
.....Loop through lights
*/
	for (i=0;i<5;i++)
	{
		e.key = UM_light_keys[i];
		ur_retrieve_blanked(e.key, &blanked);
		if (blanked) continue;
		e.rel_num = UM_LIGHT_REL;
		uc_retrieve_data(&e, sizeof(struct UM_light_rec));
		um_get_disp_attr(e.key, &attr);	
/*
........Not positioning in Screen Coordinates
*/
		if (e.space == UM_SCREEN_POS && !flag) continue;
/*
........Position in Screen Coordinates
........Set the Screen Coordinate Matrix
*/
		if (e.space == UM_SCREEN_POS)
		{
			glMatrixMode_d( GL_PROJECTION);
			glPushMatrix_d();
			glLoadIdentity_d();
			glMatrixMode_d( GL_MODELVIEW );
			glPushMatrix_d();
			glLoadIdentity_d();
			pushed = UU_TRUE;
		}
/*
........Set light position for Point & Spot Lights
*/
		if (e.type != UM_EYE_LIGHT)
		{
			pos[0] = e.position[0];
			pos[1] = e.position[1];
			pos[2] = e.position[2];
			pos[3] = 1.;
			glLightfv_d(Slights[i], GL_POSITION, pos);
		}
/*
........Reset viewing matrices
*/
		if (pushed)
		{
			glMatrixMode_d( GL_PROJECTION);
			glPopMatrix_d();
			glMatrixMode_d( GL_MODELVIEW );
			glPopMatrix_d();
			pushed = UU_FALSE;
		}
	}
/*
.....Restore secondary Unibase
*/
	if (save_active == 2) ur_getu_second();
}

/*********************************************************************
**    FUNCTION     :  uw_gldefine_material(color,lucency,shaded)
**
**       Sets the active material.
**		
**    PARAMETERS   
**       INPUT  : 
**          indx     = Color index.
**          lucency  = Translucency of material.
**          shaded   = 1 = Material is being defined for shaded entity.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_gldefine_material (indx,lucency,shaded)
int indx, lucency;
{
	GLfloat colormap[4], tmp[4];
	GLfloat st_rgb[3], end_rgb[3], mid_rgb[3], ambient, diffuse,
		specular, spec_clr[3], exp;
	int color, material;
	color = indx;
#if UU_COMP == UU_WIN2K
	if (color >= 64) color = 63;
#else
	if (color >= 16) color = 15;
#endif

	if ((color==1)&&(UW_print_screen==1)&&(UW_print_back==0))
	{
		color = 0;
	}
	material = ug_get_material();
	if (UM_material_reset)
	{
		Current_mat = -11111;
	}
/*
......if it is same material, same indx, lucency, just return
*/
	if ((Current_mat==material)&&(Current_lucency==lucency)
			&&(Current_clrinx==indx))
		return(0);
	Current_clrinx = indx;
	Current_lucency = lucency;
	Current_mat = material;
	if ((material >= 0 ) && (material<NMATRLDEFAULTS))
	{
		ambient = UM_mtrlmdl.ka[material];
		diffuse = UM_mtrlmdl.kd[material];
		specular = UM_mtrlmdl.ks[material];
		exp = UM_mtrlmdl.spec_exp[material];
		spec_clr[0] = UM_mtrlmdl.ks_r[material];
		spec_clr[1] = UM_mtrlmdl.ks_g[material];
		spec_clr[2] = UM_mtrlmdl.ks_b[material];
	}
	else
	{
		ambient = UM_mtrlmdl.ka[0];
		diffuse = UM_mtrlmdl.kd[0];
		specular = UM_mtrlmdl.ks[0];
		exp = UM_mtrlmdl.spec_exp[0];
		spec_clr[0] = UM_mtrlmdl.ks_r[0];
		spec_clr[1] = UM_mtrlmdl.ks_g[0];
		spec_clr[2] = UM_mtrlmdl.ks_b[0];
	}
	st_rgb[0] = uw_color_table[color][0]/255.0;
	st_rgb[1] = uw_color_table[color][1]/255.0;
	st_rgb[2] = uw_color_table[color][2]/255.0;
	end_rgb[0] = (uw_color_table[color][0]/255.0)/15;
	end_rgb[1] = (uw_color_table[color][1]/255.0)/15;
	end_rgb[2] = (uw_color_table[color][2]/255.0)/15;
	mid_rgb[0] = (uw_color_table[color][0]/255.0)/2;
	mid_rgb[1] = (uw_color_table[color][1]/255.0)/2;
	mid_rgb[2] = (uw_color_table[color][2]/255.0)/2;
	if (shaded)
	{
		colormap[0] = st_rgb[0];
		colormap[1] = st_rgb[1];
		colormap[2] = st_rgb[2];
		colormap[3] = (lucency/100.0);

		tmp[0] = colormap[0] * ambient; 
		tmp[1] = colormap[1] * ambient; 
		tmp[2] = colormap[2] * ambient; 
		tmp[3] = colormap[3] * ambient; 

		glMaterialfv_d(GL_FRONT_AND_BACK,GL_AMBIENT, tmp);

		tmp[0] = colormap[0] * diffuse; 
		tmp[1] = colormap[1] * diffuse; 
		tmp[2] = colormap[2] * diffuse; 
		tmp[3] = colormap[3] * diffuse; 
		glMaterialfv_d(GL_FRONT_AND_BACK,GL_DIFFUSE, tmp);

		if ((colormap[0]==0) && (colormap[1]==0) && (colormap[2]==0))
		{
			tmp[0] = tmp[1] = tmp[2] = 0.0;
		}
		else
		{
			tmp[0] = spec_clr[0] * specular; 
			tmp[1] = spec_clr[1] * specular; 
			tmp[2] = spec_clr[2] * specular; 
			tmp[3] = specular;
		}
		glMaterialfv_d(GL_FRONT_AND_BACK,GL_SPECULAR, tmp);
		glMateriali_d(GL_FRONT_AND_BACK, GL_SHININESS,exp);
	}
	else
	{
		colormap[0] = st_rgb[0];
		colormap[1] = st_rgb[1];
		colormap[2] = st_rgb[2];
		colormap[3] = 1.0;
		glMaterialfv_d(GL_FRONT_AND_BACK,GL_AMBIENT, colormap);
		glMaterialfv_d(GL_FRONT_AND_BACK,GL_DIFFUSE, colormap);
		glMaterialfv_d(GL_FRONT_AND_BACK,GL_SPECULAR, colormap);
		glMateriali_d(GL_FRONT_AND_BACK, GL_SHININESS,128);
	}
	UM_material_reset = 0;
	return (0);
}

/*********************************************************************
**    FUNCTION     :  uw_glget_material(indx,material,lucency)
**
**       Returns the active material settings.
**		
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          indx     = Color index of active material.
**          material = Active material index.
**          lucency  = Translucency of material.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glget_material(indx,material,lucency)
int *indx,*material,*lucency;
{
	*indx = Current_clrinx;
	*material = Current_mat;
	*lucency = Current_lucency;
}

/*********************************************************************
**    FUNCTION     :  uw_glrestore_material(indx,material,lucency)
**
**       Restores the active material settings.
**		
**    PARAMETERS   
**       INPUT  :
**          indx     = Color index of active material.
**          material = Active material index.
**          lucency  = Translucency of material.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_glrestore_material(indx,material,lucency)
int indx,material,lucency;
{
	Current_clrinx = indx;
	Current_mat = material;
	Current_lucency = lucency;
}

/*********************************************************************
**    FUNCTION     :  uw_glget_wsshade()
**
**       return shade mode (always shade on now)
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
uw_glget_wsshade()
{
	return 1;
}

/*********************************************************************
**    FUNCTION     :  uw_glset_stencil(flag)
**
**       Turn On/Off Stencil buffer
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
void uw_glset_stencil(flag)
UU_LOGICAL flag;
{
	if (flag==Stencil_on) return;
	Stencil_on = flag;
	if (flag)
	{
		glEnable_d(GL_STENCIL_TEST);
		glClearStencil_d(0);
		glClear_d(GL_STENCIL_BUFFER_BIT);
		glStencilFunc_d(GL_ALWAYS, 0, 1);
		glStencilOp_d(GL_ZERO, GL_ZERO, GL_ZERO);
	}
	else
		glDisable_d(GL_STENCIL_TEST);  
}

/*********************************************************************
**    FUNCTION     :  uw_glget_stencil()
**
**       Returns the setting of the stencil buffer.
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
UU_LOGICAL uw_glget_stencil()
{
	return(Stencil_on);
}
#endif


