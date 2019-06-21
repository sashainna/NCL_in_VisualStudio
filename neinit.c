/*********************************************************************
**    NAME         :  neinit.c
**       CONTAINS: routines to initialize NCL specific relations
**                 into the Unibase
**       NOTE: when adding entities to the unibase, a routine
**       should be added here called ncl_uni_init_XXX where XXX is
**       the new entity added.  The routine 
**       ncl_init_XXX() should be added to neirel.c when the entity is 
**       being added.  A call to ncl_uni_init_XXX should then
**       be made from ncl_uni_init_all() in this module as well
**       as from ncl_init_XXX() in neirel.c.  
**   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**     NOTE: When changing ddl file by addind new variable list make
**           sure that atom_size and list_size is initialized in ass-
**           otiate routine here AND in nclc/neinit.c file if exists
**           in both. There is better way (future) to fix it by using
**           dictionary data.
**     vp 2/10/98 fixed all atom_size[#displst] to match size
**                allocated in dictionary.
**   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**
**       ncl_uni_init_all
**       ncl_uni_init_attr
**       ncl_uni_init_surfattr
**       ncl_uni_init_attr_rec
**       ncl_uni_init_vector
**       ncl_uni_init_point
**       ncl_uni_init_line
**       ncl_uni_init_circle
**       ncl_uni_init_plane
**       ncl_uni_init_matrix
**       ncl_uni_init_scalar
**       ncl_uni_init_labloc
**       ncl_uni_init_curve
**       ncl_uni_init_surf
**       ncl_uni_init_revsurf
**       ncl_uni_init_panel
**       ncl_uni_init_meshsf
**       ncl_uni_init_quiltsf
**       ncl_uni_init_patern
**       ncl_uni_init_netsf
**       ncl_uni_init_shape
**       ncl_uni_init_evalcv
**       ncl_uni_init_evalsf
**       ncl_uni_init_rbsf
**       ncl_uni_init_pntvec
**       ncl_uni_init_trimsf
**       ncl_uni_init_datast
**       ncl_uni_init_textvar
**       ncl_uni_init_solid
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       neinit.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:35
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mattr.h"
#include "mdrel.h"
#include "msrf.h"
#include "msol.h"
#include "class.h"
#include "nccs.h"
#include "msrf.h"

#define NCL_MAXVARLISTS 9
extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_all()
**      Initializes all of the NCL specific UNIBASE relations.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_all()
	{
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"ncl_uni_init_all()"));

	ncl_uni_init_attr();
	ncl_uni_init_surfattr();
	ncl_uni_init_vector();
	ncl_uni_init_point();
	ncl_uni_init_line();
	ncl_uni_init_pntvec();
	ncl_uni_init_circle();
	ncl_uni_init_plane();
	ncl_uni_init_matrix();
	ncl_uni_init_scalar();
	ncl_uni_init_labloc();
	ncl_uni_init_curve();
	ncl_uni_init_surf();
	ncl_uni_init_revsurf();
	ncl_uni_init_panel();
	ncl_uni_init_meshsf();
	ncl_uni_init_quiltsf();
	ncl_uni_init_patern();
	ncl_uni_init_netsf();
	ncl_uni_init_shape();
	ncl_uni_init_evalcv();
	ncl_uni_init_evalsf();
	ncl_uni_init_rbsf();
	ncl_uni_init_trimsf();
	ncl_uni_init_datast();
	ncl_uni_init_textvar();
	ncl_uni_init_solid();

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_attr()
**      Initializes NCL attribute UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_attr()
   {

   uu_denter(UU_MTRC,(us,"ncl_uni_init_attr()"));

   /* NCL attribute relation */
   um_init_attr(NCL_ATTR_REL, "nclattr", 10, sizeof(struct NCL_nclattr_rec));

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_surfattr()
**      Initializes NCL surface/solid attribute UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_uni_init_surfattr()
{
/*
.....Initialize surface attribute record
*/
   um_init_attr(UM_SURFATTR_REL, "surfattr", 10,sizeof(struct UM_surfattr_rec));

   return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_vector()
**      Initializes NCL vector UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_vector()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_vector()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   fixed_size = sizeof(struct  NCL_vector_rec) - sizeof(char)*NCL_VECTOR_BUFSZ;
   atom_size[0] = sizeof(UU_REAL);
   list_size[0] = 10;
   um_init_rel(NCL_VECTOR_REL, "vector",  60, fixed_size, 1,
      atom_size, list_size);
   umi_init_rel_label(NCL_VECTOR_REL, "VE", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_point()
**      Initializes NCL point UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_point()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_point()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   fixed_size = sizeof(struct  NCL_nclpt_rec) - sizeof(char)*NCL_NCLPT_BUFSZ;
   atom_size[0] = sizeof(UU_REAL);
   list_size[0] = 10;
   um_init_rel(NCL_POINT_REL, "nclpt",  60, fixed_size, 1,
      atom_size, list_size);
   umi_init_rel_label(NCL_POINT_REL, "PT", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_line()
**      Initializes NCL line UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_line()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_line()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   fixed_size = sizeof(struct  NCL_nclln_rec) - sizeof(char)*NCL_NCLLN_BUFSZ;
   atom_size[0] = sizeof(UU_REAL);
   list_size[0] = 10;
   um_init_rel(NCL_LINE_REL, "nclln",  60, fixed_size, 1,
      atom_size, list_size);
   umi_init_rel_label(NCL_LINE_REL, "LN", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_pntvec()
**      Initializes NCL pntvec UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_pntvec()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_pntvec()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   fixed_size = sizeof(struct  NCL_nclpv_rec) - sizeof(char)*NCL_NCLPV_BUFSZ;
   atom_size[0] = sizeof(UU_REAL);
   list_size[0] = 10;
   um_init_rel(NCL_POINTVEC_REL, "nclpv",  60, fixed_size, 1,
      atom_size, list_size);
   umi_init_rel_label(NCL_POINTVEC_REL, "PV", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_circle()
**      Initializes NCL circle UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_circle()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_circle()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }


   fixed_size = sizeof(struct  NCL_nclci_rec) - sizeof(char)*NCL_NCLCI_BUFSZ;
   atom_size[0] = sizeof(UU_REAL);
   list_size[0] = 10;
   um_init_rel(NCL_CIRCLE_REL, "nclci",  60, fixed_size, 1,
      atom_size, list_size);
   umi_init_rel_label(NCL_CIRCLE_REL, "CI", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_plane()
**      Initializes NCL plane UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_plane()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_plane()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   fixed_size = sizeof(struct  NCL_nclpl_rec) - sizeof(char)*NCL_NCLPL_BUFSZ;
   atom_size[0] = sizeof(UU_REAL);
   list_size[0] = 10;
   um_init_rel(NCL_PLN_REL, "nclpl",  60, fixed_size, 1,
      atom_size, list_size);
   umi_init_rel_label(NCL_PLN_REL, "PL", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_matrix()
**      Initializes NCL matrix UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_matrix()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_matrix()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   fixed_size = sizeof(struct  NCL_matrix_rec) - sizeof(char)*NCL_MATRIX_BUFSZ;
   atom_size[0] = sizeof(UU_REAL);
   list_size[0] = 10;
   um_init_rel(NCL_MATRIX_REL, "matrix",  60, fixed_size, 1,
      atom_size, list_size);
   umi_init_rel_label(NCL_MATRIX_REL, "MX", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_scalar()
**      Initializes NCL scalar UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_scalar()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_scalar()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   fixed_size = sizeof(struct  NCL_scalar_rec);
   um_init_rel(NCL_SCALAR_REL, "scalar",  60, fixed_size, 0,
      atom_size, list_size);
   umi_init_rel_label(NCL_SCALAR_REL, "UN", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_labloc()
**      Initializes NCL labloc UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_labloc()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_labloc()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   /* NCL altered label location table relation */
   atom_size[0] = sizeof(struct NCL_labloc_rec);
   list_size[0] = 10;
   fixed_size = sizeof(struct  NCL_labtbl_rec) - sizeof(char) * NCL_LABTBL_BUFSZ;
   um_init_rel(NCL_LABTBL_REL, "labtbl",  60, fixed_size, 1,
      atom_size, list_size);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_curve()
**      Initializes NCL curve UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_curve()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_curve()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   atom_size[0] = sizeof(float);
   list_size[0] = 10;
   atom_size[1] = sizeof(struct NCL_segment_rec);
   list_size[1] = 10;
   atom_size[2] = sizeof(UU_REAL);
   list_size[2] = 10;
   fixed_size = sizeof(struct  NCL_curve_rec) - sizeof(char) * NCL_CURVE_BUFSZ;
   um_init_rel(NCL_CURVE_REL, "curve",  60, fixed_size, 3,
      atom_size, list_size);
   umi_init_rel_label(NCL_CURVE_REL, "CV", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_surf()
**      Initializes NCL surf UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_uni_init_surf()
{
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;
   int num_lists;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_surf()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
   {
      list_size[j] = 0;
      atom_size[j] = 0;
   }
/*
... panelkey
*/
   atom_size[0] = sizeof(UU_KEY_ID);
   list_size[0] = 10;
/*
... sskey
*/
   atom_size[1] = sizeof(UU_KEY_ID);
   list_size[1] = 10;
/*
... displst
*/
   atom_size[2] = sizeof(UU_REAL)*3;
   list_size[2] = 10;
/*
... tesslst
*/
   atom_size[3] = sizeof(UU_REAL);
   list_size[3] = 10;
/*
... boxlst
*/
   atom_size[4] = sizeof(UU_REAL);
   list_size[4] = 24;
/*
... xyzbylst
*/
   atom_size[5] = sizeof(UU_REAL);
   list_size[5] = 32;

	num_lists = 6;
   fixed_size = sizeof(struct  NCL_surface_rec)-sizeof(char)*NCL_SURFACE_BUFSZ;
   um_init_rel(NCL_SURF_REL, "surface",  60, fixed_size, num_lists,
      atom_size, list_size);
   umi_init_rel_label(NCL_SURF_REL, "SF", 1);

   uu_dexit;
   return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_revsurf()
**      Initializes NCL surface of revolution UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_uni_init_revsurf()
{
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;
   int num_lists;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_revsurf()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
   {
      list_size[j] = 0;
      atom_size[j] = 0;
   }
/*
... sskey
*/
   atom_size[0] = sizeof(UU_KEY_ID);
   list_size[0] = 10;
/*
... displst
*/
   atom_size[1] = sizeof(UU_REAL)*3;
   list_size[1] = 10;
/*
... tesslst
*/
   atom_size[2] = sizeof(UU_REAL);
   list_size[2] = 10;
/*
... boxlst
*/
   atom_size[3] = sizeof(UU_REAL);
   list_size[3] = 24;
/*
... xyzbylst
*/
   atom_size[4] = sizeof(UU_REAL);
   list_size[4] = 32;

	num_lists = 5;
   fixed_size = sizeof(struct  NCL_revsurf_rec)-sizeof(char)*NCL_REVSURF_BUFSZ;
   um_init_rel(NCL_REVSURF_REL, "revsurf",  60, fixed_size, num_lists,
      atom_size, list_size);
   umi_init_rel_label(NCL_REVSURF_REL, "SF", 1);

   uu_dexit;
   return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_panel()
**      Initializes NCL panel UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_panel()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_panel()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   atom_size[0] = sizeof(struct NCL_patch_rec);
   list_size[0] = 10;
   fixed_size = sizeof(struct  NCL_panel_rec) - sizeof(char) * NCL_PANEL_BUFSZ;
   um_init_rel(NCL_PANEL_REL, "panel",  60, fixed_size, 1,
      atom_size, list_size);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_meshsf()
**      Initializes NCL meshsf UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_meshsf()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;
   int num_lists;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_meshsf()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   atom_size[0] = sizeof(struct NCL_mpatch_rec);
   list_size[0] = 10;
   atom_size[1] = sizeof(UU_REAL)*3;
   list_size[1] = 10;
/*
... tesslst
*/
   atom_size[2] = sizeof(UU_REAL);
   list_size[2] = 10;

   atom_size[3] = sizeof(UU_REAL);
   list_size[3] = 24;

   atom_size[4] = sizeof(UU_REAL);
   list_size[4] = 32;

	num_lists = 5;
   fixed_size = sizeof(struct  NCL_meshsf_rec) - sizeof(char)*NCL_MESHSF_BUFSZ;
   um_init_rel(NCL_MESHSURF_REL, "meshsf",  60, fixed_size, num_lists,
      atom_size, list_size);
   umi_init_rel_label(NCL_MESHSURF_REL, "SF", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_quiltsf()
**      Initializes NCL quiltsf UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_uni_init_quiltsf()
{
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;
   int num_lists;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_quiltsf()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
   {
      list_size[j] = 0;
      atom_size[j] = 0;
   }

   atom_size[0] = sizeof(struct NCL_qpatch_rec);
   list_size[0] = 10;
   atom_size[1] = sizeof(UU_REAL)*3;
   list_size[1] = 10;
/*
... tesslst
*/
   atom_size[2] = sizeof(UU_REAL)*3;
   list_size[2] = 10;

   atom_size[3] = sizeof(UU_REAL);
   list_size[3] = 24;

   atom_size[4] = sizeof(UU_REAL);
   list_size[4] = 32;

	num_lists = 5;
   fixed_size = sizeof(struct  NCL_quiltsf_rec)-sizeof(char)*NCL_QUILTSF_BUFSZ;
   um_init_rel(NCL_QUILTSURF_REL, "quiltsf",  60, fixed_size, num_lists,
      atom_size, list_size);
   umi_init_rel_label(NCL_QUILTSURF_REL, "SF", 1);

   uu_dexit;
   return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_patern()
**      Initializes NCL patern UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_patern()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_patern()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   atom_size[0] = sizeof(UU_REAL);
   list_size[0] = 10;
   atom_size[1] = sizeof(UU_REAL);
   list_size[1] = 10;
   fixed_size = sizeof(struct  NCL_patern_rec) - sizeof(char)*NCL_PATERN_BUFSZ;

   um_init_rel(NCL_PATERN_REL, "patern",  60, fixed_size, 2,
      atom_size, list_size);
   umi_init_rel_label(NCL_PATERN_REL, "PN", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_netsf()
**      Initializes NCL netsf UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_uni_init_netsf()
{
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_netsf()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   atom_size[0] = sizeof(UU_KEY_ID);
   list_size[0] = 10;
   atom_size[1] = sizeof(UU_KEY_ID);
   list_size[1] = 10;
   atom_size[2] = sizeof(UU_REAL)*3;
   list_size[2] = 10;
/*
... tesslst
*/
   atom_size[3] = sizeof (UU_REAL);
   list_size[3] = 10;

   fixed_size = sizeof(struct  NCL_netsf_rec) - sizeof(char)*NCL_SURFACE_BUFSZ;
   um_init_rel(NCL_NETSF_REL, "netsf",  60, fixed_size, 4,
      atom_size, list_size);
   umi_init_rel_label(NCL_NETSF_REL, "SF", 1);

   uu_dexit;
   return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_shape()
**      Initializes NCL shape UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_shape()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_shape()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   atom_size[0] = sizeof(double);
   list_size[0] = 10;
   atom_size[1] = sizeof(UU_REAL);
   list_size[1] = 10;
/*
... tesslst
*/
   atom_size[2] = sizeof(UU_REAL);
   list_size[2] = 10;
   fixed_size = sizeof(struct  NCL_shape_rec) - sizeof(char)*NCL_SHAPE_BUFSZ;
   um_init_rel(NCL_SHAPE_REL, "shape",  60, fixed_size, 3,
      atom_size, list_size);
   umi_init_rel_label(NCL_SHAPE_REL, "SH", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_evalcv()
**      Initializes NCL evaluated curve UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_evalcv()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_evalcv()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   /* NCL evalcv relation */
   atom_size[0] = sizeof(double);
   list_size[0] = 10;
   atom_size[1] = sizeof(UU_REAL);
   list_size[1] = 10;
   fixed_size = sizeof(struct  NCL_evalcv_rec) - sizeof(char)*NCL_EVALCV_BUFSZ;
   um_init_rel(NCL_EVALCV_REL, "evalcv",  60, fixed_size, 2,
      atom_size, list_size);
   umi_init_rel_label(NCL_EVALCV_REL, "CV", 1);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_evalsf()
**      Initializes NCL evaluated surface UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_uni_init_evalsf()
{
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_evalsf()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
   {
      list_size[j] = 0;
      atom_size[j] = 0;
   }

   /* NCL evalsf relation */
   atom_size[0] = sizeof(double);
   list_size[0] = 10;
   atom_size[1] = sizeof(UU_REAL)*3;
   list_size[1] = 10;
/*
... tesslst
*/
   atom_size[2] = sizeof(UU_REAL)*3;
   list_size[2] = 10;

   fixed_size = sizeof(struct  NCL_evalsf_rec) - sizeof(char)*NCL_EVALSF_BUFSZ;
   um_init_rel(NCL_EVALSF_REL, "evalsf",  60, fixed_size, 3,
      atom_size, list_size);
   umi_init_rel_label(NCL_EVALSF_REL, "SF", 1);

   uu_dexit;
   return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_rbsf()
**      Initializes NCL rational bspline surface UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_uni_init_rbsf()
{
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;
   int num_lists;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_rbsf()"));

   /* NCL rbsf relation */
   atom_size[0] = sizeof(double);
   list_size[0] = 10;

   atom_size[1] = sizeof(double);
   list_size[1] = 10;

   atom_size[2] = sizeof(double)*3;
   list_size[2] = 10;

   atom_size[3] = sizeof(double);
   list_size[3] = 10;
/*
... sskey
*/
   atom_size[4] = sizeof(UU_KEY_ID);
   list_size[4] = 10;
/*
... displst
*/
   atom_size[5] = sizeof(UU_REAL)*3;
   list_size[5] = 10;
/*
... tesslst
*/
   atom_size[6] = sizeof(UU_REAL);
   list_size[6] = 10;
/*
... boxlst
*/
   atom_size[7] = sizeof(UU_REAL);
   list_size[7] = 24;
/*
... xyzbylst
*/
   atom_size[8] = sizeof(UU_REAL);
   list_size[8] = 32;

   num_lists = 9;
   fixed_size = sizeof(struct  UM_rbsplsrf_rec) - sizeof(char)*UM_RBSPLSRF_BUFSZ;
   um_init_rel(UM_RBSPLSRF_REL, "rbsplsrf",  60, fixed_size, num_lists,
      atom_size, list_size);
   umi_init_rel_label(UM_RBSPLSRF_REL, "SF", 1);

   uu_dexit;
   return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_trimsf()
**      Initializes NCL trimsf UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_uni_init_trimsf()
{
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;
   int num_lists;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_trimsf()"));

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   atom_size[0] = sizeof(UU_KEY_ID);
   list_size[0] = 10;

   atom_size[1] = sizeof(double)*3;
   list_size[1] = 10;
/*
... tesslst
*/
   atom_size[2] = sizeof(UU_REAL);
   list_size[2] = 10;
/*
... boxlst
*/
   atom_size[3] = sizeof(UU_REAL);
   list_size[3] = 24;

   atom_size[4] = sizeof(UU_REAL);
   list_size[4] = 32;

   atom_size[5] = sizeof(UU_REAL);
   list_size[5] = 32;

   atom_size[6] = sizeof(UU_REAL);
   list_size[6] = 32;

	num_lists = 7;
   fixed_size = sizeof(struct  NCL_trimsf_rec) - sizeof(char)*NCL_TRIMSF_BUFSZ;
   um_init_rel(NCL_TRIMSF_REL, "trimsf",  60, fixed_size, num_lists,
      atom_size, list_size);
   umi_init_rel_label(NCL_TRIMSF_REL, "SF", 1);

   uu_dexit;
   return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_datast()
**      Initializes NCL datast UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_datast()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   atom_size[0] = sizeof(struct NCL_datael_rec);
   list_size[0] = 10;
   fixed_size = sizeof(struct  NCL_datast_rec) - sizeof(char)*NCL_DATAST_BUFSZ;
   um_init_rel(NCL_DATAST_REL, "datast",  60, fixed_size, 1,
      atom_size, list_size);
   umi_init_rel_label(NCL_DATAST_REL, "UN", 1);

   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_textvar()
**      Initializes NCL textvar UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_textvar()

   {
   int j;
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;

   for (j=0; j<NCL_MAXVARLISTS; j++)
      {
      list_size[j] = 0;
      atom_size[j] = 0;
      }

   fixed_size = sizeof(struct  NCL_textvar_rec);
   um_init_rel(NCL_TEXTVAR_REL, "textvar",  60, fixed_size, 0,
      atom_size, list_size);
   umi_init_rel_label(NCL_TEXTVAR_REL, "UN", 1);

   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_attr_rec()
**      Initializes NCL attribute UNIBASE relation into the Unibase.
**    PARAMETERS   
**       INPUT  : 
**          key         key of geometry.
**          attr        attribute bundle
**          update_flag	= 0, called from NCL502, = 1 call from a tool.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_init_attr_rec(key, attr, update_flag)
UU_KEY_ID key;
struct NCL_nclattr_rec *attr;
int   update_flag;
	{
	UU_LOGICAL blanked = UU_FALSE;

	uu_denter(UU_MTRC,(us,"ncl_uni_init_attr_rec()"));

	attr->key = key;
	attr->rel_num = NCL_ATTR_REL;
	attr->use_count = 0;
	attr->layer = ur_get_attrmdl_layer();
	attr->pen = ur_get_attrmdl_pen();
	attr->line_style = ur_get_attrmdl_line_style();
	attr->line_weight = ur_get_attrmdl_line_weight();
	attr->line_width = ur_get_attrmdl_line_width();
	attr->displayable = ur_get_attrmdl_displayable();
	attr->selectable = ur_get_attrmdl_selectable();

	ur_update_blanked(key, blanked);

	if (update_flag)	/* called from tool, use built in defaults and update */
		{
		attr->label_on = 0;
		attr->color = ur_get_attrmdl_color();

		ur_update_attr(attr);
		}
	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : ncl_uni_init_solid()
**      Initializes Visual Solid UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_uni_init_solid()
{
   int atom_size[NCL_MAXVARLISTS];
   int list_size[NCL_MAXVARLISTS];
   int fixed_size;
   int num_lists;

   uu_denter(UU_MTRC,(us,"ncl_uni_init_rbsf()"));
/*
.....Solid canonical data
*/
   atom_size[0] = sizeof(UU_REAL);
   list_size[0] = 10;
/*
.....Display list
*/
   atom_size[1] = sizeof(UU_REAL)*3;
   list_size[1] = 10;
/*
.....Tessellation list
*/
   atom_size[2] = sizeof(UU_REAL);
   list_size[2] = 10;
/*
.....Surface key list
*/
   atom_size[3] = sizeof(UU_KEY_ID);
   list_size[3] = 10;
/*
.....Initialize structure
*/
   num_lists = 4;
   fixed_size = sizeof(struct  UM_solid_rec) - sizeof(char)*UM_SOLID_BUFSZ;
   um_init_rel(UM_SOLID_REL, "solid",  60, fixed_size, num_lists,
      atom_size, list_size);
   umi_init_rel_label(UM_SOLID_REL, "SO", 1);

   uu_dexit;
   return(UU_SUCCESS);
}
