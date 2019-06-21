/*********************************************************************
**    NAME :  neshade.c
**
**    CONTAINS: auxiliary shading routines
**
**        ncl_set_tess_parms
**        ncl_get_tess_parms
**        ncl_set_hidline
**        ncl_is_hidline
**        ncl_getdepth_mask
**        ncl_setdepth_mask
**        ncl_set_shademode
**        ncl_set_wireframe
**        ncl_get_wireframe
**        ncl_setcrt_lucency
**        ncl_getcrt_lucency
**        ncl_shade_triangles
**        ncl_shading_mode
**        ncl_shading_switch
**        ncl_shade_surf
**        ncl_unshade_surf
**        ncl_display_shaded_surf
**        ncl_shade_tessellation
**        ncl_is_shaded
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       neshade.c , 25.1
**     DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:49
*********************************************************************/

#include "mgeom.h"
#include "udebug.h"
#include "gobas.h"
#include "gviw.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "gerror.h"
#include "gmat4.h"
#include "xenv1.h"
#include "gtblvar6.h"
#include "uhep.h"
#include "msrf.h"
#include "mdrel.h"
#include "nccs.h"
#include "nclfc.h"
#include "wsgl.h"
#include "gsegac.h"
#include "class.h"
#include "vsegbf.h"
#include "view.h"
#include "driver.h"
#include "ginqatt.h"
#include "ginqatti.h"

int DEBUG_SHOW_TESS;
int DEBUG_SHOW_TESS_UV;
static int NCL_SHADING_MODE = 0;
static int NCL_WIREFRAME_MODE = 1;
static int NCL_HIDE_LINE = 0;
static int current_lucency = 100;
static int current_material = 0;
static UM_tess_setting UM_TESS_SETTING;

/*********************************************************************
**    FUNCTION : void ncl_set_tess_parms (typ,tol,kupts,kvpts)
**
** Set tessellation parameters. If the type is UM_TESS_TOLER, the 
** tessellation is done by tolerance only. If the type is UM_TESS_GRID,
** the surface is subdivided into rectangular panels according to the specified
** grid parameters, and each panel is then tessellated as flat. If the typ is
** UM_TESS_BOTH, the tessellation is done by tolerance, but with the minimal
** panel size given by the grid parameters.
**
**    PARAMETERS   
**       INPUT : 
**                typ   - UM_TESS_TOLER, UM_TESS_GRID, or UM_TESS_BOTH
**                tol   - tolerance
**                kupts,kvpts - surface grid parameters
**       OUTPUT : none
**    RETURNS      : none 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_set_tess_parms (typ,tol,kupts,kvpts)
UM_tess_settype typ;
UU_REAL tol;
int kupts,kvpts;
{
	UM_TESS_SETTING.typ = typ;
	UM_TESS_SETTING.tol = tol;
	UM_TESS_SETTING.kupts = kupts;
	UM_TESS_SETTING.kvpts = kvpts;
}
	
/*********************************************************************
**    FUNCTION : void ncl_get_tess_parms (typ,tol,kupts,kvpts)
**
** Get current tessellation parameters.
**
**    PARAMETERS
**       INPUT : none
**       OUTPUT :
**                typ   - UM_TESS_TOLER, UM_TESS_GRID, or UM_TESS_BOTH
**                tol   - tolerance
**                kupts,kvpts - surface grid if UM_TESS_GRID, or minimal
**                kvpts -
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_tess_parms (typ,tol,kupts,kvpts)
UM_tess_settype *typ;
UU_REAL *tol;
int *kupts,*kvpts;
{
	*typ = UM_TESS_SETTING.typ;
	*tol = UM_TESS_SETTING.tol;
	*kupts = *kvpts = 0;

	if (UM_TESS_SETTING.typ == UM_TESS_GRID || UM_TESS_SETTING.typ == UM_TESS_BOTH)
	{
		if (UM_TESS_SETTING.kupts >= 2)
			*kupts = UM_TESS_SETTING.kupts;
		if (UM_TESS_SETTING.kvpts >= 2)
			*kvpts = UM_TESS_SETTING.kvpts;
	}
}

/*********************************************************************
*********************************************************************/
int ncl_set_hidline(mode)
int mode;
{
	NCL_HIDE_LINE = mode;
	return (0);
}

/*********************************************************************
*********************************************************************/
int ncl_is_hidline()
{
	return NCL_HIDE_LINE;
}

/*********************************************************************
*********************************************************************/
int ncl_getdepth_mask()
{
	int mask;
	mask = ug_getdepth_mask();
	return mask;
}

/*********************************************************************
*********************************************************************/
int ncl_setdepth_mask(flag)
int flag;
{
	ug_setdepth_mask(flag);
	return (0);
}

/*********************************************************************
*********************************************************************/
int ncl_set_shademode(mode)
int mode;
{
	int shade;
	shade = ug_get_wsshade();
	if (shade == 0)
		NCL_SHADING_MODE = 0;
	else
		NCL_SHADING_MODE = mode;
	return (0);
}

/*********************************************************************
*********************************************************************/
int ncl_set_wireframe(flag)
int flag;
{
	NCL_WIREFRAME_MODE = flag;
	return (0);
}

/*********************************************************************
*********************************************************************/
int ncl_get_wireframe()
{
	return NCL_WIREFRAME_MODE ;
}

/*********************************************************************
*********************************************************************/
int ncl_setcrt_lucency(lucency)
int lucency;
{
	current_lucency = lucency;
	return (0);
}

/*********************************************************************
*********************************************************************/
int ncl_getcrt_lucency()
{
	return current_lucency;
}

/*********************************************************************
*********************************************************************/
int ncl_setcrt_material(material)
int material;
{
	current_material = material;
	return (0);
}

/*********************************************************************
*********************************************************************/
int ncl_getcrt_material()
{
	return current_material;
}

/*********************************************************************
**   FUNCTION : int ncl_shade_triangles (triangles,flag)
**     
**  Creates a DIGS segment representing shaded set of triangles
**
**    PARAMETERS   
**       INPUT :  
**          triangles - list of triangles 
**				flag:  0: hid_line removal
**						 1: shading
**       OUTPUT : none 
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#ifdef UU_OPENGL
int ncl_shade_triangles (triangles, flag)
UU_LIST *triangles;
int flag;
{
	Gwpoint3 *points, *norms, *pt,*norm, *pt0, *norm0;
	UM_triangle *trian;
/*	GLubyte mask[128];*/
	int status=UU_SUCCESS, n,i, color, np,count, lucency,material;
	int NMAX = 500;
	
	if (flag)
	{
/*
.....set material before color
*/
		material = ncl_getcrt_material();
		gsmaterial(material);
/*
.....use current display color
.....already set in display function (ncl_disp_surf....)
.....and the color can't retrive from tess->key, it should
.....retrieve from eptr->key, color wrong here.
.....Yurong 2/15/99
*/
		color = gqlinecolor ();
		gsshdcolor(color);

		lucency = ncl_getcrt_lucency();
		gsshdlucency(lucency);
/*temp yurong 
		if ((lucency < 97) && (lucency > 0))
		{
			ug_get_lucency_mask(lucency, mask);
			gsshdstipple(mask);
		}
*/
	}
	n = UU_LIST_LENGTH (triangles);
	trian = (UM_triangle *) UU_LIST_ARRAY (triangles);

	if (NMAX > n) NMAX = n;
	np = 3*n;

	points = (Gwpoint3 *) uu_malloc (np*sizeof (Gwpoint3));
	norms = (Gwpoint3 *) uu_malloc (np*sizeof (Gwpoint3));
	if (!points || !norms)
	{
		status = UU_FAILURE; goto Done;
	}
/*
... output bunches of NMAX triangles at a time as triplets of vertex-norm:
... (v1,n1,v2,n2,v3,n3), (v4,n4,v5,n5,v6,n6), ...
*/
	pt = pt0 = points; norm = norm0 = norms;

	for (i=0, count = 0, np = 0; i<n; i++,trian++)
	{
		pt->x = trian->p1[0]; pt->y = trian->p1[1]; pt->z = trian->p1[2];
			pt++;
		pt->x = trian->p2[0]; pt->y = trian->p2[1]; pt->z = trian->p2[2];
			pt++;
		pt->x = trian->p3[0]; pt->y = trian->p3[1]; pt->z = trian->p3[2];
			pt++;

		np += 3;

		norm->x=trian->norm1[0]; norm->y=trian->norm1[1]; norm->z=trian->norm1[2];
			norm++;
		norm->x=trian->norm2[0]; norm->y=trian->norm2[1]; norm->z=trian->norm2[2];
			norm++;
		norm->x=trian->norm3[0]; norm->y=trian->norm3[1]; norm->z=trian->norm3[2];
			norm++;

		count++;
/*
... output bunch of NMAX triangles
*/
		if (count == NMAX || i == n-1)
		{
			gshadearea ( np, pt0, norm0, 0);
			count = np = 0;
			pt0 = pt; norm0 = norm;
		}
	}

Done:;
	if (points) uu_free (points);
	if (norms)  uu_free (norms);

	return (status);
}
#endif

/*********************************************************************
**    FUNCTION : int ncl_shading_mode ()
**
** returns current shading mode 
**
**    PARAMETERS   
**       INPUT : none 
**       OUTPUT : none
**    RETURNS      : 
**             0/1 if shading is off/on
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_shading_mode ()
{
	int shade;
	shade = ug_get_wsshade();
	if (shade == 0) return 0;
	return (NCL_SHADING_MODE);
} 

/*********************************************************************
**    FUNCTION : int ncl_shading_switch ()
**
** if shading mode is ON, turns it OFF, erases all shaded segments
** and restores wireframe representation of shaded surfaces;
** if shading mode is OFF, turns the shading ON.
**
**    PARAMETERS   
**       INPUT : none 
**       OUTPUT : none
**    RETURNS      : 
**       UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_shading_switch (vp, mode)
UV_vport  *vp;
int mode;
{
/*
.....we don't use this function now
*/
/*

#ifdef UU_OPENGL
	vp->disp_mode = mode;
	uv_updatevp(vp, 1);
#endif
*/
	return(0);
} 

/*********************************************************************
**  FUNCTION : int ncl_unshade_surf (keys, nsf)
**
**  	Unshade a set of surfaces
**
**  PARAMETERS   
**     INPUT :  
**          keys - array of surface keys
**          nsf  - number of surfaces
**     OUTPUT : none 
**  RETURNS      : 
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int ncl_unshade_surf (keys, nsf)
UU_KEY_ID *keys;
int nsf;
{
	struct NCL_fixed_databag ent;
	int isf,status = UU_SUCCESS;
	UU_KEY_ID *key;

	if (!keys || nsf <= 0) return (UU_FAILURE);
	for (isf = 0, key = keys; isf < nsf; isf++, key++)
	{
		if (!ncl_is_shaded(*key))
			continue;
		else
		{
/*
.....set shaded attribute = UU_FALSE
.....and display this entity again
*/
			ent.key = *key;
			if (ncl_retrieve_data_fixed (&ent) != UU_SUCCESS) 
				return (UU_FAILURE);
			ncl_setent_shaded(&ent, UU_FALSE);
			uv_disp_entity(&ent);	
		}
	}
	return (status);
}

/*********************************************************************
**  FUNCTION : int ncl_disp_tess (ent,tess)
**
**  Use a tessellation to shade a surface
**
**  PARAMETERS   
**     INPUT :  
**          ent  - surface struct
**          tess - tessellation
**     OUTPUT : none
**  RETURNS      : 
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int ncl_disp_tess (ent,tess)
struct NCL_fixed_databag *ent;
UM_tessellation *tess;
{
	int status,lucency,material;
/*
.....set current lucency here
.....Yurong 3/2/99
*/
	ncl_retrieve_lucency(ent, &lucency);
	ncl_setcrt_lucency(lucency);

	ncl_retrieve_material(ent, &material);
	ncl_setcrt_material(material);
	
	status = ncl_shade_tessellation (tess, 1);

	if (status != UU_SUCCESS)
		ncl_lst_delete1 (TESSELLATION_LIST,ent);

	return (status);
}

/*********************************************************************
**  FUNCTION : int ncl_display_shaded_sf (ent)
**
**  Dispatcher routine to tessellate and shade a surface
**
**  PARAMETERS   
**     INPUT :  
**          ent  - surface struct
**          tol  - tolerance
**     OUTPUT : none
**  RETURNS      : 
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : a successfully calculated tessellation is stored as a
**                 surface list (if wasn't already there)
**  WARNINGS     : none
*********************************************************************/
int ncl_display_shaded_sf (ent,tol)
struct NCL_fixed_databag *ent;
UU_REAL tol; 
{
	int status;
	UM_tessellation tess;

	um_set_tess_toler (tol);
	ncl_set_boundary_toler (tol);

	um_init_tess (&tess);
	status = ncl_get_tessellation (ent,&tess);
	if (status == UU_SUCCESS)
		status = ncl_disp_tess (ent,&tess);

	um_free_tess (&tess);

	return (status);
}

/*********************************************************************
**  FUNCTION : int ncl_display_shaded_surf(key)
**
**  Dispatcher routine to tessellate and shade a surface
**
**  PARAMETERS   
**     INPUT :  
**          key  - surface key
**     OUTPUT : none
**  RETURNS      : 
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : a successfully calculated tessellation is stored as a
**                 surface list (if wasn't already there)
**  WARNINGS     : none
*********************************************************************/
int ncl_display_shaded_surf (key)
UU_KEY_ID key;
{
	struct NCL_fixed_databag ent;
	int status;
	UU_REAL tol; 
	UM_int2 idx;

	if (!ncl_shading_mode()) return (UU_FAILURE);
	if (key == NULLKEY) return (UU_FAILURE);
/*
... surface & boundary tolerance of tessellation
*/
	idx = 175;
	getsc (&idx,&tol);

	ent.key = key;
	status = ncl_retrieve_data_fixed (&ent);
	if (status == UU_SUCCESS)
	status = ncl_display_shaded_sf (&ent,tol);

	return (status);
}

/*********************************************************************
**  FUNCTION : int ncl_shade_surf (keys, nsf)
**
**  Dispatcher routine to tessellate and shade a set of surfaces
**
**  PARAMETERS   
**     INPUT :  
**          keys - array of surface keys
**          nsf  - number of surfaces
**     OUTPUT : none 
**  RETURNS      : 
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int ncl_shade_surf (keys, nsf)
UU_KEY_ID *keys;
int nsf;
{
	struct NCL_fixed_databag ent;
	int isf;
	UU_KEY_ID *key;
	UU_REAL tol; 
	UM_int2 idx;

	if (!keys || nsf <= 0) return (UU_FAILURE);
/*
... surface & boundary tolerance of tessellation
*/
	idx = 175;
	getsc (&idx,&tol);

	ncl_set_boundary_toler (tol);

	for (isf = 0, key = keys; isf < nsf; isf++, key++)
	{
		if (!ncl_is_shaded(*key))
		{
/*
.....set shaded attribute = UU_TRUE
.....and display this entity again
*/
			ent.key = *key;
			if (ncl_retrieve_data_fixed (&ent) != UU_SUCCESS) 
				return (UU_FAILURE);

			ncl_setent_shaded(&ent, UU_TRUE);
			uv_disp_entity(&ent);	
		}
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**    FUNCTION : int ncl_shade_tessellation (tess,flag)
**     Shades a triangular tessellation
**    PARAMETERS   
**       INPUT :  
**          tess - tessellation
**          flag:  0: hid_line removal
**                 1: shading
**       OUTPUT : none 
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_shade_tessellation (tess, flag)
UM_tessellation *tess;
int flag;
{
	int status = UU_SUCCESS;
	UU_LIST triangles;

#ifdef UU_OPENGL

	triangles.cur_cnt = triangles.max_cnt = triangles.exp_cnt = 0;
	triangles.data = UU_NULL;
/*
if (DEBUG_SHOW_TESS_UV)
	status = ncl_get_tess_triangles_uv (tess,&triangles);
else
*/
	status = ncl_get_tess_triangles (tess,&triangles,1,0);

	if (status == UU_SUCCESS) ncl_shade_triangles (&triangles, flag);

	if (triangles.max_cnt > 0) uu_list_free (&triangles);
#endif

	return (status);
}

/*********************************************************************
**    FUNCTION : UU_LOGICAL ncl_is_shaded (key)
**
**     Determines whether a surface "shaded" flag is set
**
**    PARAMETERS
**       INPUT :
**          key - surface key
**       OUTPUT : none
**    RETURNS      :
**       UU_TRUE if shaded; UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_is_shaded (key)
UU_KEY_ID key;
{
	UU_LOGICAL found, shaded;
	struct NCL_fixed_databag ent;

	found = UU_FALSE;

	ent.key = key;
	if (ncl_retrieve_data_fixed (&ent) != UU_SUCCESS) 
		return (found);

	ncl_retrieve_shaded(&ent, &shaded);
	found = shaded;
	return (found);
}

/*********************************************************************
**  FUNCTION : int ncl_display_hid_surf(key);
**
**  Shade a surface for hidden line removal.
**
**  PARAMETERS   
**     INPUT :  
**          keys - array of surface keys
**          nsf  - number of surfaces
**     OUTPUT : none 
**  RETURNS      : 
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
int ncl_display_hid_surf(key)
UU_KEY_ID key;
{
	struct NCL_fixed_databag ent;
	UM_tessellation tess;
	int status;
	UU_REAL tol; 
	UM_int2 idx;
	static int time = 0;
	time++;

	status = UU_SUCCESS;

#ifdef UU_OPENGL

	if (!key) return (UU_FAILURE);
/*
... surface & boundary tolerance of tessellation
*/
	idx = 175;
	getsc (&idx,&tol);
	um_set_tess_toler (5.*tol);
	ncl_set_boundary_toler (tol);

	ent.key = key;
	if (ncl_retrieve_data_fixed (&ent) != UU_SUCCESS)  return UU_FAILURE;

	um_init_tess(&tess);

	status = ncl_get_tessellation (&ent,&tess);
	if (status == UU_SUCCESS)
		status = ncl_shade_tessellation (&tess, 0);

	um_free_tess (&tess);

#endif

	return (status);
}
