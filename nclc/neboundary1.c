/*********************************************************************
**    NAME         :  neboundary1.c
**       CONTAINS:  Routines to visualize cv on sf by its normals to 
**                  surface and common boundary of two (adjacent) surfaces. 
**
**           ncl_2sfbnry_disp
**           ncl_cvonsf_disp
**           ncl_box_disp
**           ncl_get_box_f
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       neboundary1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:24
*********************************************************************/

#include "ncldef.h"
#include "nccs.h"
#include "mattr.h"
#include "mdattr.h"
#include "view.h"
#include "dasnog.h"
#include "dselmask.h"
#include "ginqatt.h"
#include "gobas.h"
#include "gtbl.h"
#include "mdpick.h"
#include "modef.h"
#include "mdeval.h"
#include "mdcpln.h"
#include "zsysdep.h"
#include "mgeom.h"

extern UU_LIST cbxyz, cbnum, *NCL_uvlst;
extern int NCL_cpn, NCL_ubcopy;
extern struct NCL_cvonsf_rec *NCL_cvonsf[3];

/*********************************************************************
**    E_FUNCTION     : ncl_cvonsf_disp (key1,key2,told,tolm)
**       Displays curve on surface.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cvonsf_disp (key,kolor)
int *kolor, *key;
{
   UM_coord *bnpt;
	struct NCL_cvonsf_rec *cvps;
   int n, i, ix, k, j, *knpt, isav, col;
   Glntype line_style, lintyp;
   Gcolor lincol;
   Gwpoint3 *pts;
/*
...save current display attributes
*/
   isav = ug_gksstli.curvwindex;
   zbytecp(lintyp,*gqlinetype());
   lincol = gqlinecolor();
/*
...set display attributes for boundary curve
*/
   if (*kolor < 0 || *kolor > 64) col = 1;
   else col = *kolor;
   gsfillcolor(col);
   gslinecolor(col);
   line_style.typeno = UM_SOLID_LINE;
   line_style.npatn = 0;
   gslinetype(&line_style);

/*
.....this is temporary to visualize normal to the boundary curve.
.....assume that all points in NCL_uvlst are used and use key to
.....evaluate base surface at uv points.
*/
	if (*key != 0)
	{
		struct NCL_trimsf_rec tsf;
		UM_transf tfmat;
		struct UM_evsrfout evsrf;
		UM_coord *uvp;
		tsf.key = *key;
		if (ncl_retrieve_data_fixed (&tsf) == 0)
		{
			if (tsf.rel_num == NCL_TRIMSF_REL)
			{
				UM_coord pt1,pt2;
				struct UM_line_rec e1;
				UU_REAL *t1, *t2, *uv;
				t1 = (UU_REAL *) pt1;
				t2 = (UU_REAL *) pt2;
				uc_retrieve_transf(tsf.key, tfmat);
				uc_init_evsrfout (&tsf,&evsrf);
				cvps = (struct NCL_cvonsf_rec *) NCL_cvonsf[0];
				uvp  = (UM_coord *) UU_LIST_ARRAY (cvps->uvlst);
				n    = UU_LIST_LENGTH (cvps->uvlst);
				for (i=0; i<n; i++)
				{
					uv = (UU_REAL *) uvp;
					uc_evsrf (UM_NORM,uv[0],uv[1],&tsf,tfmat,&evsrf);
					um_vctovc (evsrf.sp,e1.spt);
					um_unitvc (evsrf.snorm,pt2);
					um_vcplvc (e1.spt,pt2,e1.ept);	
/*
.....temporary create lines in unibase
*/
					e1.no_displst = 0;
					e1.key = 0;
					e1.rel_num = UM_LINE_REL;
					um_create_geom (&e1,tfmat,UM_CURRENT_ATTR);
					ncl_retrieve_data_fixed(&e1);
					uc_display (&e1);
					uvp++;
				}
			}
		}
	}
	cvps = (struct NCL_cvonsf_rec *) NCL_cvonsf[0];
	bnpt  = (UM_coord *) UU_LIST_ARRAY (cvps->xylst);
	n    = UU_LIST_LENGTH (cvps->xylst);
   pts  = (Gwpoint3 *) bnpt;
/*
...display segments in all views
*/
	for (j=1;j<=UV_act_screen[0].nvports;j++)
	{
		ug_gksstli.curvwindex = j;
		ug_plna3(n,pts);
	}
/*
...reset display attributes
*/
   ug_gksstli.curvwindex = isav;
   gslinetype(&lintyp);
   gslinecolor(lincol);

   return(0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_2sfbnry_disp (key1,key2,told,tolm)
**       Displays common boundary of two surfaces.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_2sfbnry_disp (kolor)
int *kolor;
  {
   UM_coord *bnpt;
   int n, i, ix, k, j, *knpt, isav, col;
   Glntype line_style, lintyp;
   Gcolor lincol;
   Gwpoint3 *pts;
/*
...save current display attributes
*/
   isav = ug_gksstli.curvwindex;
   zbytecp(lintyp,*gqlinetype());
   lincol = gqlinecolor();
/*
...set display attributes for boundary curve
*/
   if (*kolor < 0 || *kolor > 64) col = 1;
   else col = *kolor;
   gsfillcolor(col);
   gslinecolor(col);
   line_style.typeno = UM_SOLID_LINE;
   line_style.npatn = 0;
   gslinetype(&line_style);

   bnpt = (UM_coord *) UU_LIST_ARRAY (&cbxyz);
   knpt = (int *) UU_LIST_ARRAY (&cbnum);
   pts  = (Gwpoint3 *) bnpt;
/*
...display segments in all views
*/
   for (j=1;j<=UV_act_screen[0].nvports;j++)
     {
      ug_gksstli.curvwindex = j;
      for (k=0, ix=0; k<NCL_cpn; k++)
        {
         n = knpt[k];
         ug_plna3(n,&pts[ix]);
         ix += n;
        }
     }
/*
...reset display attributes
*/
   ug_gksstli.curvwindex = isav;
   gslinetype(&lintyp);
   gslinecolor(lincol);

   return(0);
  }

/*********************************************************************
**    E_FUNCTION : ncl_box_disp (ver, edge, color)
**       Displays 3d-box
**    PARAMETERS
**       INPUT  :
**                 ver - array of 8 vertices of a box
**                 edge - array of 12 edges of the box
**                 color - display color
**       OUTPUT : 
**          none
**    RETURNS      :
**         UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_box_disp (ver,color)
UM_coord *ver;
int *color;
{
   struct UM_line_rec e1;
   int i,j1,j2;
/*
...write box into database and display it edge by edge
*/
   for (i=0; i< UM_3DBOX_NUM_EDGES; i += 2)
   {
      j1 = UM_3DBOX_EDGES[i];
      j2 = UM_3DBOX_EDGES[i+1];
      um_vctovc (ver[j1],e1.spt);
      um_vctovc (ver[j2],e1.ept);
      e1.no_displst = 0;
      e1.key = 0;
      e1.rel_num = UM_LINE_REL;
      um_create_geom(&e1,UM_DEFAULT_TF,UM_CURRENT_ATTR);
      ncl_retrieve_data_fixed1(&e1);
      uc_display (&e1);
   }

   return(0);
}

/******************************************************************
**   FUNCTION : int ncl_get_box_f (key, ver, edge)
**
**      calls ncl_get_box to construct a box around surface specified by key
**      provides interface to FORTRAN to display the box
**
**    PARAMETERS
**       INPUT  :
**                 key - database key of a surface
**       OUTPUT :
**                 ver - array of 8 vertices of the box around sf.
**                 edge - array of 12 edges of the box.
**    RETURNS :
**               UU_SUCCESS if O.K., UU_FAILURE if problem
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_box_f (key,ver,edge)
UU_KEY_ID *key;
UM_coord *ver;
int *edge;
{
   struct NCL_fixed_databag sf;
   UM_3D_box box;
   int i, status;

   if (*key <= 0) return (UU_FAILURE);
   sf.key = *key;
   if (ncl_retrieve_data_fixed (&sf))  return (UU_FAILURE);


   status = ncl_get_box(&sf, &box);

   if(status != UU_SUCCESS) return UU_FAILURE;

   for(i=0;i<UM_3DBOX_NUM_VERT;i++) um_vctovc (box.ver[i],ver[i]);

   for(i=0;i<UM_3DBOX_NUM_EDGES;i++) edge[i] = UM_3DBOX_EDGES[i];

   return (UU_SUCCESS);
}

