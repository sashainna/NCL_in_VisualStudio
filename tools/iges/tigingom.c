/*********************************************************************
**    NAME         :  tigingom.c
**       CONTAINS:
**                  uig_in_line
**                  uig_in_plane
**                  uig_in_point
**                  uig_in_arc
**                  uig_in_conic
**                  uig_in_comp
**                  uig_in_poly2
**                  uig_in_poly3
**                  uig_in_poly6
**                  uig_in_spline
**                  uig_in_rspline
**                  uig_in_rsplsrf
**                  uig_in_ruledsrf
**                  uig_in_revsrf
**                  uig_in_tbcysrf
**                  uig_in_group
**                  uig_in_viewvs
**                  uig_in_plnassoc
**                  uig_in_offstsrf
**                  uig_in_crvonsrf
**                  uig_in_trimsrf
**                  uig_in_bdsrf
**                  uig_in_solid
**                  uig_in_verlist
**                  uig_in_edglist
**                  uig_in_loop
**                  uig_in_face 
**                  uig_in_shell
**                  int uig_copy_bndy(old, new)
**                  int uig_load_current_matrix(mptr,t)
**                  int uig_pop_current_matrix()
**                  int uig_check_current_matrix_list(dblk)
**    				  int uig_shift_plane(e1ptr, e2ptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigingom.c , 25.3
**     DATE AND TIME OF LAST  MODIFICATION
**       11/22/17 , 11:40:48
*********************************************************************/

#include "tiges.h"
#include "tigdefs.h"
#include "mdattr.h"
#include "mdeval.h"
#include "udebug.h"
#include "nccs.h"
#include "mdrel.h"
#include "modef.h"
#include "msrf.h"
#include "mcrv.h"
#include "mattrddl.h"
#include "nclfc.h"
#include "mgeom.h"

/* NCL: roberta - handle mapping to ncl curves of parametric splines */
extern int mapncl; 	
int srf_number = 0;	/* cpp */
UU_KEY_ID NCL_savekey, NCL_savekey1;
UU_REAL NCL_t0, NCL_t1;
extern int MAX_PARA_REC;
extern UU_LOGICAL drw_flag, uvcv_flag, bump_u, bump_v;
extern UU_REAL starting_ang_u, starting_ang_v, period_u, period_v;
UU_LOGICAL posangle = UU_FALSE;
extern UU_LOGICAL instance_trans;
UU_KEY_ID tig_uvcvonsf_bskey;
extern int tig_unlabeled;
extern UU_KEY_ID *tig_unlabeled_keys;
static int UIG_splitccrv_wrk;
int UIG_inner_error = 0;
extern UU_REAL instance_tf[4][3];

static UU_LOGICAL IG_trans = UU_TRUE;
static UU_REAL *IG_matrix_list_ptr;
static UU_REAL IG_idmatrix[12] =
	{1.0,0.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,0.0,1.0,0.0};

int uig_check_poly_uv();
int uig_check_uv();
int uig_check1_uv();
int uig_copy_bndy();
int uig_get_rulsrf_parm();
int uig_get_srf_uv();
int uig_in_arc_prm();
int uig_in_poly2();
int uig_in_poly3();
int uig_in_rspline();
int uig_in_spline();

extern int create_label();
extern UU_LOGICAL uig_check_range();
extern int uig_create_comp();
extern int uig_create_rbsp();
extern void uig_error();
extern int uig_evcrv_to_uvcv();
extern int uig_get_data();
extern int uig_get_directory();
extern int uig_get_trans();
extern void uig_in_dispat();
extern void uig_list_out();
extern int uig_map_arc();
extern int uig_map_bdsrf();
extern int uig_map_conic();
extern int uig_map_line();
extern int uig_map_offsetsrf();
extern int uig_map_plane();
extern int uig_map_poly2();
extern int uig_map_poly3();
extern int uig_map_poly6();
extern int uig_map_pt();
extern void uig_map_rbspl();
extern int uig_map_rbsplsrf();
extern int uig_map_revsrf();
extern int uig_map_ruledsrf();
extern int uig_map_spline();
extern void uig_map_splinesrf();
extern int uig_map_spline_to_nclcrv();
extern int uig_map_tabcyl();
extern int uig_map_trimsrf();
extern int uig_match_surface();
extern int uig_match_trimsrf();
extern int uig_ncl_evcrv_to_pts();
extern int uig_ncl_interp_rbsp1();
extern int uig_transform();
extern int uig_transf_entity();
extern void uig_trans_comp();
extern void uig_tran_coor(); 
extern void uig_update_attr();
extern int uig_vctovc();
extern void check_color();
extern void init_label();
extern void update_counts();

extern int ncl_plane_to_sf();
extern int ncl_retrieve_data_fixed();
extern int ncl_update_sskey();
extern int uc_evcrv();
extern int uc_init_evcrvout();
extern int uc_retrieve_transf();
extern void ul_strip_blanks();
extern void um_cctmtf();
extern int um_chgcstf();
extern void um_disptf();
extern void um_tftmtf();
extern int um_unitvc();
extern int um_vcmnvc();
extern int um_vctmsc();
extern int um_vctovc();
extern int ur_delete_all();
extern int ur_retrieve_data();
extern int ur_update_data_fixed();
extern int ur_update_data_varlist();
extern int ur_update_displayable();
extern void uu_toolfree();
extern char *uu_toolmalloc();

char *uu_lsinsrt(), *uu_lsend(), *uu_lsnext(), *uu_lsnew(), *uu_lsdele();

/*********************************************************************
**    I_FUNCTION     :  uig_in_line(gblk,dblk,pblk,key)
**				Create a line record from an iges line.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_line(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_igesline_rec *pblk;			/* iges line parameter record */
UU_KEY_ID *key;
{
	UU_REAL *t;
	UU_KEY_ID e_key;

	uig_load_current_matrix(dblk->matrix_ptr, &t);
	uig_map_line(dblk,pblk,t,&e_key);
	uig_pop_current_matrix();
	*key = e_key;

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_plane(gblk,dblk,pblk,key)
**			Create a plane record from an iges plane.
**    PARAMETERS   
**       INPUT  : 
**				gblk				global block
**				dblk 				directory block
**				pblk				parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_plane(gblk,dblk,pblk,key)
struct global_rec *gblk;		/* global record sturcture */
struct dir_rec *dblk;			/* directory record */
struct IG_igespln_rec *pblk;	/* iges plane parameter record */
UU_KEY_ID *key;
{
	struct dir_rec ldir;
	int irec;
	UU_REAL *t;
	UU_KEY_ID e_key;

	uig_load_current_matrix(dblk->matrix_ptr, &t);
	uig_map_plane(dblk,pblk,t,&e_key);
	uig_pop_current_matrix();
	*key = e_key;
	if ((irec = pblk->b_crv) > 0 && uig_get_data(irec,0,&ldir,pblk) == 0)
	{           /* translate plane boundry curve */
		init_label(ldir.rel_type, irec);
		check_color(&ldir);
		current_dir_number = irec;
		uig_in_dispat(gblk,&ldir,pblk,&e_key);
		update_counts (e_key, irec);
	}

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_point(gblk,dblk,pblk,key)
**			Create a data base point from an iges point.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_point(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_igespt_rec *pblk;			/* iges point parameter record */
UU_KEY_ID *key;
{
	UU_REAL *t;
	UU_KEY_ID e_key;

	uig_load_current_matrix(dblk->matrix_ptr, &t);
	uig_map_pt(dblk,pblk,t,&e_key);
	uig_pop_current_matrix();
	*key = e_key;

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_arc(gblk,dblk,pblk,key)
**				Create a data base arc from an iges arc.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_arc(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_igesarc_rec *pblk;			/* iges arc parameter record */
UU_KEY_ID *key;
{
	UU_REAL *t;
	UU_KEY_ID e_key;

	uig_load_current_matrix(dblk->matrix_ptr, &t);
	uig_map_arc(dblk,pblk,t,&e_key);
	uig_pop_current_matrix();
	*key = e_key;

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_conic(gblk,dblk,pblk,key)
**				Create a data base conic from an iges conic.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_conic(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_igescon_rec *pblk;		/* iges conic parameter record */
UU_KEY_ID *key;
{
	UU_REAL *t;
	UU_KEY_ID e_key;

	uig_load_current_matrix(dblk->matrix_ptr, &t);
	uig_map_conic(dblk,pblk,t,&e_key);
	uig_pop_current_matrix();
	*key = e_key;

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_comp(gblk,dblk,pblk,key)
**				Create a data base comp curve from an iges comp curve.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_comp(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_igescomp_rec *pblk;		/* iges comp curve parameter record */
UU_KEY_ID *key;
{
	char p_buff[80], *c;
	int i,irec,status,num;
	int lerr = 0, isvdsp, splitccrv = 0;
	UU_LOGICAL lsvlab;
	UU_KEY_ID e_key, lkey, *p, *key_list = UU_NULL;
	struct dir_rec ldir;

	c = uu_toolmalloc(MAX_PARA_REC);
	isvdsp = DDC_displayable_flag;
	lsvlab = label_comp_element;
	DDC_displayable_flag = UM_NEVERDISPLAYABLE;		/*jkd38 */
	label_comp_element = UU_TRUE;
	p = pblk->cid;
	num = pblk->no_cid;
	if (!uvcv_flag) splitccrv = UIG_splitccrv_wrk;
	if (!(splitccrv%2))
	{
		key_list = (UU_KEY_ID *)uu_toolmalloc(num*sizeof(UU_KEY_ID));

		for(i=0;i<num;i++)
		{
			irec = p[i];
			status = uig_get_data(irec,0,&ldir,c);
			lkey = 0;		/*jkd38 */
			if(status == 0)
			{
				switch(ldir.rel_type)
				{
					case GLINE:
						uig_in_line(gblk,&ldir,(struct IG_igesline_rec *)c,&lkey);
						break;
					case GARC:
						uig_in_arc(gblk,&ldir,(struct IG_igesarc_rec *)c,&lkey);
						break;
					case GCONIC:
						uig_in_conic(gblk,&ldir,(struct IG_igescon_rec *)c,&lkey);
						break;
					case GSPLINE:
					/*jkd10: handle parametric spline curve */
						uig_in_spline(gblk,&ldir,(struct IG_igesplin_rec *)c,&lkey);
						break;
					case GRSPLINE:
					/*cpp: handle rational spline curve */
						uig_in_rspline(gblk,&ldir,(struct IG_igesrspl_rec *)c,&lkey);
						break;
					case GPOLY:
						uig_in_poly2(gblk,&ldir,(struct IG_poly2d_rec *)c,&lkey);
						break;
					case GPOLY3D:
						uig_in_poly3(gblk,&ldir,(struct IG_poly3d_rec *)c,&lkey);
						break;
					default:
						lkey = 0;
						break;
				}
				update_counts(lkey, irec);	/*jkd5: update counts */
				/*jkd10: notify user if part of curve not tranlsated */
				if (lkey == 0) 
				{
					sprintf(p_buff, "(DREC = %d) Part of composite curve not translated; type=%d\n",
						irec, ldir.rel_type);
					uig_error (p_buff);
				}
			}
			if (lkey == 0) 
				lerr = 1;
			key_list[i] = lkey;				/*jkd38 */
		}
	}

	if (splitccrv || lerr)
	{
		*key = 1;
		if (lerr)
		{
			sprintf(p_buff, "(DREC = %d) Composite curve not translated, but subcurves are translated.\n",
				dblk->drec_num);
			uig_error (p_buff);
			*key = 0;
			label_comp_element = lsvlab;
			DDC_displayable_flag = isvdsp;
		}
		else
		{
			label_comp_element = UU_FALSE;
			DDC_displayable_flag = UM_DISPLAYABLE;
		}
		for(i=0;i<num;i++) /* Translate individual elements */
		{
			irec = p[i];
			if (uig_get_data(irec,0,&ldir,c) == 0)
			{
				init_label(ldir.rel_type, irec);
				check_color(&ldir);
				current_dir_number = irec;
				uig_in_dispat(gblk,&ldir,c,&lkey);
			}
		}
	}
	if (!(splitccrv%2) && !lerr)
	{
		if (!splitccrv)
		{
			label_comp_element = lsvlab;
			DDC_displayable_flag = isvdsp;
		}
		else
		{
			label_comp_element = UU_FALSE;
			DDC_displayable_flag = UM_DISPLAYABLE;
		}
		uig_update_attr(dblk);
		uig_create_comp(dblk, pblk, num, key_list, UU_FALSE, NULL, &e_key);
		*key = e_key; 
	}
	if (c) uu_toolfree(c);
	if (key_list) uu_toolfree(key_list);
	label_comp_element = lsvlab;
	DDC_displayable_flag = isvdsp;

	return (0);
}

/********************************************************************
**    I_FUNCTION     :  uig_in_poly2(gblk,dblk,pblk,key)
**				Create a data base poly2 curve from an iges poly2 curve.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_poly2(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_poly2d_rec *pblk;		/* iges poly2 curve parameter record */
UU_KEY_ID *key;
{
	UU_REAL *t;
	UU_KEY_ID e_key;

	uig_load_current_matrix(dblk->matrix_ptr, &t);
	uig_map_poly2(dblk,pblk,t,&e_key);
	uig_pop_current_matrix();
	*key = e_key;

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_poly3(gblk,dblk,pblk,key)
**				Create a data base poly3 curve from an iges poly3 curve.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_poly3(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_poly3d_rec *pblk;		/* iges poly3 curve parameter record */
UU_KEY_ID *key;
{
	UU_REAL *t;
	UU_KEY_ID e_key;

	uig_load_current_matrix(dblk->matrix_ptr, &t);
	if(dblk->form_no ==12 && entity_mask[36]==1)
		 uig_map_poly3crv(dblk,pblk,t,&e_key);
	else
		uig_map_poly3(dblk,pblk,t,&e_key);
	uig_pop_current_matrix();
	*key = e_key;

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_poly6(gblk,dblk,pblk,key)
**				Create a data base poly6 curve from an iges poly6 curve.
**          vp 5-may-3 routine brought to live from dummy in order
**          to map IGES copious data Form 3 to NCL patern or pointvector.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_poly6(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_poly6d_rec *pblk;		/* iges poly6 curve parameter record */
UU_KEY_ID *key;
{
	UU_REAL *t;
	UU_KEY_ID e_key;

	uig_load_current_matrix(dblk->matrix_ptr, &t);
   if(dblk->form_no ==13 && entity_mask[36]==1)
       uig_map_poly6crv(dblk,pblk,t,&e_key);
   else
      uig_map_poly6(dblk,pblk,t,&e_key);
	uig_pop_current_matrix();
	*key = e_key;

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_spline(gblk,dblk,pblk,key)
**				Create a data base spline curve from an iges spline curve.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_spline(gblk,dblk,pblk,key)
struct global_rec *gblk;	/* global record sturcture */
struct dir_rec *dblk;		/* directory record */
struct IG_igesplin_rec *pblk;	/* iges spline parameter record */
UU_KEY_ID *key;
{
	UU_REAL *t;
	UU_KEY_ID e_key;

	uig_load_current_matrix(dblk->matrix_ptr, &t);
	if (mapncl)
		uig_map_spline_to_nclcrv(dblk,pblk,t,&e_key);
	else
		uig_map_spline(dblk,pblk,t,&e_key);
	uig_pop_current_matrix();
	*key = e_key;

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_splinesrf(gblk,dblk,pblk,key)
**		Create a data base spline surface from an iges spline surface.
**    PARAMETERS   
**       INPUT  : 
**			gblk			global block
**			dblk			directory block
**			pblk			parameter block
**       OUTPUT :  
**			none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_splinesrf(gblk,dblk,pblk,key)	/*jkd14: p-spline surface */
struct global_rec *gblk;	/* global record sturcture */
struct dir_rec *dblk;		/* directory record */
struct IG_igesplsf_rec *pblk;	/* iges spline srf parameter record */
UU_KEY_ID *key;
{
	UU_REAL *t;
	UU_KEY_ID e_key;

	uig_load_current_matrix(dblk->matrix_ptr, &t);
	uig_map_splinesrf(dblk,pblk,t,&e_key);
	uig_pop_current_matrix();
	*key = e_key;

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_rspline(gblk,dblk,pblk,key)
**			Create a data base rational spline curve from an iges 
**          rational spline curve.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_rspline(gblk,dblk,pblk,key)
struct global_rec *gblk;    /* global record sturcture */
struct dir_rec *dblk;        /* directory record */
struct IG_igesrspl_rec *pblk;    /* iges spline parameter record */
UU_KEY_ID *key;
{
	char p_buff[80];
	UU_REAL *t;
	int i;
/*
.....If the rational spline curve has negative weights , it is not translated
*/
	if (!uvcv_flag)
		for (i = 0; i < pblk->no_w ; i++)
			if (pblk->w[i] <= 0.0) 
			{
				sprintf(p_buff,
				"(DREC = %d) B-Spline has negative weights,not translated.\n",
				dblk->drec_num);
				uig_error(p_buff);
				*key = 0;
				return (0);
			}

	if (pblk->degree > 24)
	{
		sprintf (p_buff, "(DREC = %d) B-spline higher than 24th degree, not translated.\n",
							  dblk->drec_num);
		uig_error(p_buff);
		*key = 0;
	}
	else
	{
		uig_load_current_matrix(dblk->matrix_ptr, &t);
		uig_map_rbspl(dblk,pblk,t,key);
		uig_pop_current_matrix();
	}

	return (0);
}
/*********************************************************************
**    I_FUNCTION     :  uig_in_rsplsrf(gblk,dblk,pblk,key)
**			Create a data base rational spline surface from an iges 
**          rational spline surface.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_rsplsrf(gblk,dblk,pblk,key)
struct global_rec *gblk;	/* global record sturcture */
struct dir_rec *dblk;        /* directory record */
struct IG_igesrssf_rec *pblk;/* iges spline surface parameter record */
UU_KEY_ID *key;
{
	char p_buff[80];
	UU_REAL *t;

	if (pblk->degree1 > 24 || pblk->degree2 > 24)
	{
		sprintf (p_buff, "(DREC = %d) B-spline surf higher than 24th degree, not translated.\n",
							  dblk->drec_num);
		uig_error(p_buff);
		*key = 0;
	}
	else
	{
		uig_load_current_matrix(dblk->matrix_ptr, &t);
		uig_map_rbsplsrf(dblk,pblk,t,key);
		uig_pop_current_matrix();
	}

	return (0);
}
/*********************************************************************
**    I_FUNCTION     :  uig_in_ruledsrf(gblk,dblk,pblk,key)
**				Create a data base rational spline surface from an iges 
**          ruled surface.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_ruledsrf(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_igesrlsf_rec *pblk;		/* iges ruled surface parameter record */
UU_KEY_ID *key;
{
	UU_REAL *t;
	UU_KEY_ID e_key, lkey, crv_ptr[2];  /* cpp: increase size from 500 to 1500 */
	int i,irec,status,reverse, crv_num[2];
	char *c;
	struct dir_rec ldir;
	UU_REAL vec[3], vecx[3] ,vecy[3] ,vecz[3], origin[3], tfmat[4][3];
	UU_REAL modx[3], mody[3], modz[3], modorg[3], temp_mat[4][3];
	UU_LOGICAL hld_drw_flg;

	crv_ptr[0] = 0;
	crv_ptr[1] = 0;
	uig_load_current_matrix(dblk->matrix_ptr, &t);

	for(i=0;i<3;i++)
	{
		modx[i] = mody[i] = modz[i] = modorg[i] = 0.0;
	}
	modx[0] = mody[1] = modz[2] = 1.0;
	for(i=1;i<5;i++)
	{
		uig_trans_comp(t,i,vec);
		switch(i)
		{
			case 1:
				uig_vctovc(vecx, vec);
				break;
			case 2:
				uig_vctovc(vecy, vec);
				break;
			case 3:
				uig_vctovc(vecz, vec);
				break;
			case 4:
				uig_vctovc(origin, vec);
				um_vctmsc(origin, unit_scale, origin);
				break;
		}
	}
	um_chgcstf(modorg, modx, mody, modz, modorg, vecx, vecy, vecz, tfmat);
	um_disptf(origin, temp_mat);
	um_tftmtf(tfmat, temp_mat, tfmat);

	c = uu_toolmalloc(MAX_PARA_REC);

	crv_num[0] = pblk->crv1;
	crv_num[1] = pblk->crv2;
	reverse = pblk->dirflg;

	hld_drw_flg = drw_flag;
	drw_flag = UU_FALSE;

	for(i=0;i<2;i++)
	{
		irec = crv_num[i];
		status = uig_get_data(irec,0,&ldir,c);
		if(status == 0)
		{
			init_label(ldir.rel_type, irec);	/*jkd15a: unique label names */
			check_color(&ldir);		/*jkd34: set default colors */
			current_dir_number = irec;
			switch(ldir.rel_type)
			{
				case GLINE:
					uig_in_line(gblk,&ldir,(struct IG_igesline_rec *)c,&lkey);
					break;
				case GPOINT:
					uig_in_point(gblk,&ldir,(struct IG_igespt_rec *)c,&lkey);
					break;
				case GARC:
					uig_in_arc(gblk,&ldir,(struct IG_igesarc_rec *)c,&lkey);
					break;
				case GCONIC:
					uig_in_conic(gblk,&ldir,(struct IG_igescon_rec *)c,&lkey);
					break;
				case GCOMPOSITE:
					uig_in_comp(gblk,&ldir,(struct IG_igescomp_rec *)c,&lkey);
					break;
				case GPOLY:
					uig_in_poly2(gblk,&ldir,(struct IG_poly2d_rec *)c,&lkey);
					break;
				case GPOLY3D:
					uig_in_poly3(gblk,&ldir,(struct IG_poly3d_rec *)c,&lkey);
					break;
				case GSPLINE:
					uig_in_spline(gblk,&ldir,(struct IG_igesplin_rec *)c,&lkey);
					break;
				case GRSPLINE:
					uig_in_rspline(gblk,&ldir,(struct IG_igesrspl_rec *)c,&lkey);
					break;
				default:
					break;
			}
			if((int)lkey > 0)
				crv_ptr[i] = lkey;
	   		update_counts(lkey, irec);	/*jkd5: update translated counts */
		}
	}

	NCL_savekey = crv_ptr[0];
	NCL_savekey1 = crv_ptr[1];
	drw_flag = hld_drw_flg;
	uig_map_ruledsrf(crv_ptr, reverse, tfmat,  dblk, pblk, &e_key);
	*key = e_key;
	uu_toolfree(c);
	uig_pop_current_matrix();

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_revsrf(gblk,dblk,pblk,key)
**          Create a NCL surface of revolution from an IGES 
**          surface of revolution.
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_revsrf(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_igesrvsf_rec *pblk;		/* iges surface of revolution record */
UU_KEY_ID *key;
{
	int i,isvdsp,irec,status;
	char *c;
	UU_REAL vec[3], vecx[3] ,vecy[3] ,vecz[3], origin[3], tfmat[4][3];
	UU_REAL modx[3], mody[3], modz[3], modorg[3], temp_mat[4][3];
	UU_REAL pta[3], vca[3], *t, *t2;
	UU_LOGICAL hld_drw_flg, lsvlab, store = UU_TRUE;
	UU_KEY_ID e_key, lkey, cvkey;
	struct dir_rec ldir;
	struct IG_igesline_rec *axis;
	struct NCL_fixed_databag e1;

	e_key = 0;

	uig_load_current_matrix(dblk->matrix_ptr, &t);

	for(i=0;i<3;i++)
	{
		modx[i] = mody[i] = modz[i] = modorg[i] = 0.0;
	}
	modx[0] = mody[1] = modz[2] = 1.0;
	for(i=1;i<5;i++)
	{
		uig_trans_comp(t,i,vec);
		switch(i)
		{
			case 1:
				uig_vctovc(vecx, vec);
				break;
			case 2:
				uig_vctovc(vecy, vec);
				break;
			case 3:
				uig_vctovc(vecz, vec);
				break;
			case 4:
				uig_vctovc(origin, vec);
				um_vctmsc(origin, unit_scale, origin);
				break;
		}
	}
	um_chgcstf(modorg, modx, mody, modz, modorg, vecx, vecy, vecz, tfmat);
	um_disptf(origin, temp_mat);
	um_tftmtf(tfmat, temp_mat, tfmat);

	c = uu_toolmalloc(MAX_PARA_REC);
/*
..... obtain the rotation axis
*/
	irec = pblk->axis;
	status = uig_get_data(irec,0,&ldir,c);
	if (status == 0)
	{
		current_dir_number = irec;

		uig_load_current_matrix(ldir.matrix_ptr, &t2);
		axis = (struct IG_igesline_rec *) c;

		uig_tran_coor(&axis->spt[0],t2,pta); 
		um_vctmsc(pta,unit_scale,pta);     

		uig_tran_coor(&axis->ept[0],t2,vca);
		um_vctmsc(vca,unit_scale,vca);    
		um_vcmnvc (vca,pta,vca);
		update_counts(1, irec);	/*jkd5: update translated counts */
		uig_pop_current_matrix();
	}
/*
..... translate the generatrix curve
*/
	hld_drw_flg = drw_flag;
	drw_flag = UU_FALSE;

	irec = pblk->crv;
	status = uig_get_data(irec,0,&ldir,c);
	if (status == 0)
	{
		isvdsp = DDC_displayable_flag;
		lsvlab = label_comp_element;
		DDC_displayable_flag = UM_NEVERDISPLAYABLE;
		label_comp_element = UU_TRUE;

		current_dir_number = irec;
		switch(ldir.rel_type)
		{
			case GLINE:
				uig_in_line(gblk,&ldir,(struct IG_igesline_rec *)c,&lkey);
				break;
			case GPOINT:
				uig_in_point(gblk,&ldir,(struct IG_igespt_rec *)c,&lkey);
				break;
			case GARC:
				posangle = UU_TRUE;
				uig_in_arc(gblk,&ldir,(struct IG_igesarc_rec *)c,&lkey);
				uig_in_arc_prm(c,&NCL_t0,&NCL_t1);
				break;
			case GCONIC:
				uig_in_conic(gblk,&ldir,(struct IG_igescon_rec *)c,&lkey);
				break;
			case GCOMPOSITE:
				uig_in_comp(gblk,&ldir,(struct IG_igescomp_rec *)c,&lkey);
				break;
			case GPOLY:
				uig_in_poly2(gblk,&ldir,(struct IG_poly2d_rec *)c,&lkey);
				break;
			case GPOLY3D:
				uig_in_poly3(gblk,&ldir,(struct IG_poly3d_rec *)c,&lkey);
				break;
			case GSPLINE:
				uig_in_spline(gblk,&ldir,(struct IG_igesplin_rec *)c,&lkey);
				break;
			case GRSPLINE:
				uig_in_rspline(gblk,&ldir,(struct IG_igesrspl_rec *)c,&lkey);
				break;
			default:
				break;
		}
		if ((int)lkey > 0)
			cvkey = lkey;
   		update_counts(lkey, irec);	/*jkd5: update translated counts */
	}

    e1.key = cvkey;
    status = ncl_retrieve_data_fixed (&e1);

    if (status == UU_SUCCESS)
        status = uig_transform (&e1, tfmat, store);
    if (status != UU_SUCCESS) goto done;

	NCL_savekey = cvkey;
	drw_flag = hld_drw_flg;
	DDC_displayable_flag = isvdsp;
	label_comp_element = lsvlab;

	uig_map_revsrf(cvkey,pta,vca,pblk->sa,pblk->ea,tfmat,dblk,pblk, &e_key);

done:;
	*key = e_key;
	uu_toolfree(c);
	uig_pop_current_matrix();

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_tbcysrf(gblk,dblk,pblk,key)
**				Create a data base rational spline surface from an iges 
**          tabulated cylinder .
**    PARAMETERS   
**       INPUT  : 
**				gblk							global block
**				dblk 							directory block
**				pblk							parameter block
**       OUTPUT :  
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_tbcysrf(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_igestbcy_rec *pblk;			/* iges tabulated cylinder record */
UU_KEY_ID *key;
{
	int i,irec,status, crv_num;
	char *c;
	UU_REAL vec[3], vecx[3] ,vecy[3] ,vecz[3], origin[3], tfmat[4][3];
	UU_REAL modx[3], mody[3], modz[3], modorg[3], temp_mat[4][3];
	UU_REAL *t, lpoint[3];
	UU_LOGICAL hld_drw_flg;
	UU_KEY_ID lkey; 
	struct dir_rec ldir;

	uig_load_current_matrix(dblk->matrix_ptr, &t);

	for(i=0;i<3;i++)
	{
		modx[i] = mody[i] = modz[i] = modorg[i] = 0.0;
	}
	modx[0] = mody[1] = modz[2] = 1.0;
	for(i=1;i<5;i++)
	{
		uig_trans_comp(t,i,vec);
		switch(i)
		{
			case 1:
				uig_vctovc(vecx, vec);
				break;
			case 2:
				uig_vctovc(vecy, vec);
				break;
			case 3:
				uig_vctovc(vecz, vec);
				break;
			case 4:
				uig_vctovc(origin, vec);
				um_vctmsc(origin, unit_scale, origin);
				break;
		}
	}
	um_chgcstf(modorg, modx, mody, modz, modorg, vecx, vecy, vecz, tfmat);
	um_disptf(origin, temp_mat);
	um_tftmtf(tfmat, temp_mat, tfmat);

/*
	uig_tran_coor(&pblk->d_pt[0],t,lpoint);	
*/
	um_vctovc(&pblk->d_pt[0], lpoint);

	um_vctmsc(lpoint,unit_scale,lpoint);

	c = uu_toolmalloc(MAX_PARA_REC);

	crv_num = pblk->crv;

	*key = 0;
	irec = crv_num;
	status = uig_get_data(irec,0,&ldir,c);
	if(status == 0)
	{
		init_label(ldir.rel_type, irec);	/*jkd15a: unique label names */
		check_color(&ldir);		/*jkd34: set default colors */
		current_dir_number = irec;
		hld_drw_flg = drw_flag;
		drw_flag = UU_FALSE;
		switch(ldir.rel_type)
		{
			case GLINE:
				uig_in_line(gblk,&ldir,(struct IG_igesline_rec *)c,&lkey);
				break;
			case GPOINT:
				uig_in_point(gblk,&ldir,(struct IG_igespt_rec *)c,&lkey);
				break;
			case GARC:
				uig_in_arc(gblk,&ldir,(struct IG_igesarc_rec *)c,&lkey);
				break;
			case GCONIC:
				uig_in_conic(gblk,&ldir,(struct IG_igescon_rec *)c,&lkey);
				break;
			case GCOMPOSITE:
				uig_in_comp(gblk,&ldir,(struct IG_igescomp_rec *)c,&lkey);
				break;
			case GPOLY:
				uig_in_poly2(gblk,&ldir,(struct IG_poly2d_rec *)c,&lkey);
				break;
			case GPOLY3D:
				uig_in_poly3(gblk,&ldir,(struct IG_poly3d_rec *)c,&lkey);
				break;
			case GSPLINE:
				uig_in_spline(gblk,&ldir,(struct IG_igesplin_rec *)c,&lkey);
				break;
			case GRSPLINE:
				uig_in_rspline(gblk,&ldir,(struct IG_igesrspl_rec *)c,&lkey);
				break;
			default:
				break;
		}
		update_counts(lkey, irec);	/*jkd5: update translated counts */
		drw_flag = hld_drw_flg;
		if((int)lkey > 0) uig_map_tabcyl(lkey, lpoint, tfmat,  dblk, pblk, key);
	}
	uu_toolfree(c);
	uig_pop_current_matrix();

	return (0);
}
/*********************************************************************
**    I_FUNCTION     :  uig_in_group(gblk,dblk,pblk,key)
**				translate individual entities in the group
**				and ignore the group entity itself
**    PARAMETERS   
**       INPUT  : 
**			gblk			global block
**			dblk 			directory block
**			pblk			parameter block
**       OUTPUT :  
**			none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_group(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_igesgrp_rec *pblk;			/* iges group record */
UU_KEY_ID *key;
{
	UU_KEY_ID lkey, *p, *grp_ptr;
	struct dir_rec ldir;
	int i,irec,status,num, num_grp;
	char *c;

	c = UU_NULL;
	c = uu_toolmalloc(MAX_PARA_REC);

/* 
..... cpp: translate individual entities in the group
..... and ignore the group entity itself

	DDC_displayable_flag = UM_NEVERDISPLAYABLE;
*/

	p = pblk->cid;
	num = pblk->num;
	grp_ptr = UU_NULL;
	grp_ptr = (UU_KEY_ID *)uu_toolmalloc(num*sizeof(UU_KEY_ID));
	num_grp = 0;
	for(i=0;i<num;i++)
	{
		lkey = 0;
		irec = p[i];
		status = uig_get_data(irec,0,&ldir,c);
		if(status == 0)
		{
			init_label(ldir.rel_type, irec);		
			check_color(&ldir);		
			current_dir_number = irec;

			if( uig_check_range(&ldir))
				uig_in_dispat(gblk,&ldir,c,&lkey);
			if ((int)lkey > 0)
			{
				grp_ptr[num_grp++] = lkey;
			}
			update_counts(lkey, irec);	
		}
	}

/*
..... cpp: do not create a group entity

	DDC_displayable_flag = UM_DISPLAYABLE;
	uig_map_group(num_grp, grp_ptr, dblk, &e_key);
	*key = e_key;
*/
	*key = 1;
	if (c != 0) uu_toolfree(c);
	if (grp_ptr != 0) uu_toolfree(grp_ptr);
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_viewvs(gblk,dblk,pblk,key)
**				Translate a view association entity.
**				Translate every individual entity
**    PARAMETERS   
**       INPUT  : 
**			gblk			global block
**			dblk 			directory block
**			pblk			parameter block
**       OUTPUT :  
**			none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_viewvs(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_viewvs_rec *pblk;				/* iges view assoc  record */
UU_KEY_ID *key;
{
	UU_KEY_ID lkey, *p, *grp_ptr;
	struct dir_rec ldir;
	int i,irec,status,num, num_grp;
	char *c;

	c = UU_NULL;
	c = uu_toolmalloc(MAX_PARA_REC);
	
	p = pblk->cid;
	num = pblk->no_cid;
	grp_ptr = UU_NULL;
	grp_ptr = (UU_KEY_ID *)uu_toolmalloc(num*sizeof(UU_KEY_ID));
	num_grp = 0;
	for(i=0;i<num;i++)
	{
		lkey = 0;
		irec = p[i];
		status = uig_get_data(irec,0,&ldir,c);
		if(status == 0)
		{
			init_label(ldir.rel_type, irec);		
			check_color(&ldir);		
			current_dir_number = irec;

			if( uig_check_range(&ldir))
				uig_in_dispat(gblk,&ldir,c,&lkey);
			if ((int)lkey > 0)
			{
				grp_ptr[num_grp++] = lkey;
			}
			update_counts(lkey, irec);	
		}
	}
	*key = 1;
	if (c != 0) uu_toolfree(c);
	if (grp_ptr != 0) uu_toolfree(grp_ptr);

	return (0);
}
/*********************************************************************
**    I_FUNCTION     :  uig_in_plnassoc(gblk,dblk,pblk,key)
**				Translate a view association entity.
**				Translate every individual entity
**    PARAMETERS   
**       INPUT  : 
**			gblk			global block
**			dblk 			directory block
**			pblk			parameter block
**       OUTPUT :  
**			none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_plnassoc(gblk,dblk,pblk,key)
struct global_rec *gblk;				/* global record sturcture */
struct dir_rec *dblk;					/* directory record */
struct IG_plnassoc_rec *pblk;				/* iges view assoc  record */
UU_KEY_ID *key;
{

	UU_KEY_ID lkey, *p, *grp_ptr;
	struct dir_rec ldir;
	int i,irec,status,num, num_grp;
	char *c;
	UU_REAL *t;

	c = UU_NULL;
	c = uu_toolmalloc(MAX_PARA_REC);
	uig_load_current_matrix(pblk->trans_id, &t);
	p = pblk->cid;
	num = pblk->no_cid;
	grp_ptr = UU_NULL;
	grp_ptr = (UU_KEY_ID *)uu_toolmalloc(num*sizeof(UU_KEY_ID));
	num_grp = 0;
	for(i=0;i<num;i++)
	{
		lkey = 0;
		irec = p[i];
		status = uig_get_data(irec,0,&ldir,c);
		if(status == 0)
		{
			init_label(ldir.rel_type, irec);		
			check_color(&ldir);		
			current_dir_number = irec;

			if( uig_check_range(&ldir))
				uig_in_dispat(gblk,&ldir,c,&lkey);
			if ((int)lkey > 0)
			{
				grp_ptr[num_grp++] = lkey;
			}
			update_counts(lkey, irec);	
		}
	}
	uig_pop_current_matrix();
	*key = 1;
	if (c != 0) uu_toolfree(c);
	if (grp_ptr != 0) uu_toolfree(grp_ptr);

	return (0);
}


/*********************************************************************
**    I_FUNCTION     :  uig_in_offstsrf(gblk,dblk,pblk,key)
**				Create an offset surface from an iges offset surface.
**    PARAMETERS   
**       INPUT  : 
**			gblk			global block
**			dblk 			directory block
**			pblk			parameter block
**       OUTPUT :  
**			key				key of surface created
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_offstsrf(gblk,dblk,pblk,key)
struct global_rec *gblk;                /* global record sturcture */
struct dir_rec *dblk;                   /* directory record */
struct IG_igesofsf_rec *pblk;           /* iges offset srf record */
UU_KEY_ID *key;
{
	UU_REAL *t;

	uig_load_current_matrix(dblk->matrix_ptr, &t);
	uig_map_offsetsrf(gblk, dblk, pblk, t, key);
	uig_pop_current_matrix();

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_crvonsrf(gblk,dblk,pblk,key)
**              Create a curve from an iges curve on surface.
**              If it is uv curve on surface create NCL uvcvonsf as a
**              separate entity.
**    PARAMETERS   
**       INPUT  : 
**          gblk            global block
**          dblk            directory block
**          pblk            parameter block
**       OUTPUT :  
**          key             key of XYZ curve created or
**                          key of UV curve if it is defined
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_crvonsrf(gblk,dblk,pblk,key)
struct global_rec *gblk;                /* global record sturcture */
struct dir_rec *dblk;                   /* directory record */
struct IG_igescvsf_rec *pblk;           /* iges cirve on srf record */
UU_KEY_ID *key;
{
	char p_buff[80], *c;
	struct dir_rec ldir;
	struct UM_rbsplcrv_rec *xyz_crv;
	UU_LOGICAL lsvlab, label_comp_element_sav, hldtrans;
	UU_LIST *cvpts;
	UU_KEY_ID keyuv=0, keycv=0, keysf=0;
	UU_REAL uv[6], *knots, *control_pts;
	int irec, status, isvdsp, form, ixsf;
	int crv_on_srf_filtered, num_control_pts, num_pts_tessel;
	int pre_exist_surf=0, displayable_sav;
	struct NCL_fixed_databag e1;

	crv_on_srf_filtered = entity_mask[18];
	label_comp_element_sav = label_comp_element;
	displayable_sav = DDC_displayable_flag;

	if (crv_on_srf_filtered)
	{
		label_comp_element = UU_FALSE;
		DDC_displayable_flag = UM_DISPLAYABLE;
	}
	c = uu_toolmalloc(MAX_PARA_REC);
/*
.....translate XYZ curve if defined
*/
	irec = pblk->crv;
/*
.....Only try and tanslate the XYZ curve if there is one.  JLS 6/29/99
*/
	if (irec != 0)
	{
		status = uig_get_data(irec,0,&ldir,c);
		if(status == 0)
		{
			init_label(ldir.rel_type, irec);    /*jkd15a: unique label names */
			check_color(&ldir);                 /*jkd34: set default colors */
			current_dir_number = irec;

			uig_in_dispat(gblk,&ldir,c,&keycv);
			keyuv = keycv;
			update_counts(keycv, irec);    /*jkd5: update translated counts */
		}
		if ((keycv == 0) && (crv_on_srf_filtered))
		{
			sprintf(p_buff, "(DREC = %d) Curve-on-surface not translated.\n", dblk->drec_num);
			uig_error (p_buff);
		}
	}
	isvdsp = DDC_displayable_flag;
	lsvlab = label_comp_element;
/*
.....translate UV curve on surface, (same as in trimmed srf)
.....only rbsplines curves are translated for now,
.....we may include in future polylines also!
*/
/*
..... make sure that the uv curve is present
.....(some IGES files do not contain the uv curve)
*/
	if(pblk->b_ptr == 0)
	{
		status = UU_FAILURE;
		keyuv = 0;
	}
	else
	{
		status = uig_get_directory (pblk->b_ptr,&ldir);
		if (ldir.rel_type == GRSPLINE || ldir.rel_type == GCOMPOSITE)
		{
			irec = pblk->srf;
			status = uig_get_data(irec,0,&ldir,c);
		}
		else status = UU_FAILURE;
	}
	if (status == 0)
	{
		ixsf = irec/2;
		form  = ldir.form_no;
		if (xlated_key[ixsf] == 0)
		{
			DDC_displayable_flag = UM_NEVERDISPLAYABLE;
			label_comp_element = UU_TRUE;
			init_label(ldir.rel_type, irec);
			check_color(&ldir);
			current_dir_number = irec;
			uig_in_dispat(gblk,&ldir,c,&keysf);
			if (!crv_on_srf_filtered)
				update_counts(keysf, irec);
			DDC_displayable_flag = isvdsp;
			label_comp_element = lsvlab;
			pre_exist_surf = 0;
		}
		else 
		{
			keysf = xlated_key[ixsf];
			pre_exist_surf = 1;
		}

		if (keysf != 0) 
		{
/*
.....If label_type is equal to 8 we are going to need to know what
.....keysf when we map the uvcvonsf.  JLS 12/13/99
*/
			tig_uvcvonsf_bskey = keysf;
			status = uig_get_srf_uv (ldir.rel_type, form, c, uv);
			irec = pblk->b_ptr;
			status = (irec == 0)? -2: uig_get_data(irec,0,&ldir,c);
			if (status == 0)
			{
 				keyuv = 0;
				init_label(ldir.rel_type, irec);
				check_color(&ldir);
				current_dir_number = irec;
				uvcv_flag = UU_TRUE;
				hldtrans = IG_trans;
				IG_trans = UU_FALSE;
				uig_in_dispat(gblk,&ldir,c,&keyuv);
				IG_trans = hldtrans;
				DDC_displayable_flag = isvdsp;
				uvcv_flag = UU_FALSE;
				if (keyuv == 0)
					status = -2;
				else
					status = 0;
/*
.....Fix unibase entities now
*/
				if (status == 0)
				{
					struct UM_rbsplsrf_rec *bs;
					struct UM_uvcvonsf_rec *uvcv; 
					struct NCL_fixed_databag ent;
					uvcv = (struct UM_uvcvonsf_rec *) &ent;
					uvcv->key = keyuv;
					if (ncl_retrieve_data_fixed (uvcv) == UU_SUCCESS)
					{
/*
.....Add bs key reference to the curve entity
.....Only uv_on_sf splines are supported now. We will add composite curves
.....in future so each subcurve will have bs key in it. If subcurve 
.....is not uv_on_sf spline (LN, CN & CI) it should be converted first
*/
						if (uvcv->rel_num == UM_UVCVONSF_REL)
						{
							uvcv->bskey = keysf;
							ur_update_data_fixed (uvcv);	
						}
					}
/*
.....Update cv key reference in surface entity
*/
					bs = (struct UM_rbsplsrf_rec *) &ent;
					bs->key = keysf;
					if (ncl_retrieve_data_fixed (bs) == UU_SUCCESS)
					{
						status = ncl_update_sskey (bs,keyuv,1);
						if (!crv_on_srf_filtered)
							update_counts(keyuv, irec);
					}	
				}
				else
				{
/*
.....If it was a bad surface and or a bad uv-curve, delete the information
.....in the unibase since it won't be needed. Set keyuv = 0 to indicate that
.....this particular entity has not been translated. JLS 5/29/99
*/
					if((keysf != 0) && (!pre_exist_surf))
					{
						ur_delete_all(keysf);
						xlated_key[ixsf] = 0;
						keysf = 0;
					}
					if (keyuv !=0)
						ur_delete_all(keyuv);
					keyuv = 0;
				}
			}
		}	
		*key = keyuv;
	}
/*
..... If curves_on_surfaces (IGES number 142) are filtered out of the 
..... translation, just bring in the 3D, spatial curve.  Don't bring 
..... in the uv_curve.  The entity_mask is defined in igesmfmain.c/
..... uig_translate_filter_form.  If entity_mask[i] == 1, translate; 
..... however, if == 0, don't translate, filter.  I thought the local 
..... name made more sense talking about filtering instead of doing
..... translations.
*/
	if (crv_on_srf_filtered)
	{
		if (keycv == 0)
		{
/*
..... First, make sure that I have the uv curve and the surface.
..... Next, generate some XYZ points along the uv curve.
..... Feed the points into uig_ncl_interp_rbsp1(), which will make knot
..... vector and control points.  After that, give that stuff to 
..... uig_create rbsp().  This actually makes the curve.  Finally, clean up
..... all the extra stuff.  Get rid of the uv curve and base surface.
*/
			if ((keyuv == 0) || (keysf == 0))
			{
				*key = 0;
				goto xit;
			}
/*
..... Tessellate curve_on_surf into a set of points.
*/
			cvpts = 0;
			status = uig_ncl_evcrv_to_pts (&keyuv, &num_pts_tessel, cvpts);
/*
..... Get knot vector and control points.
*/
			num_control_pts = 0;
			knots = 0;
			if (status == UU_SUCCESS)
				status = uig_ncl_interp_rbsp1(num_pts_tessel, cvpts, 1, \
                       num_control_pts, knots, &control_pts);
/*
..... Make actual curve.
*/
			xyz_crv = 0;
			if (status == UU_SUCCESS)
				status = uig_create_rbsp(num_control_pts, knots, \
				 							control_pts, xyz_crv);
			if (status == UU_SUCCESS)
				keycv = xyz_crv->key;
			else
				keycv = 0;
		}
/*
..... Delete the Unibase entries for the uv_curve and if the surface was
..... created for this entity, delete it also. 
*/
		if (keyuv != 0)
		{
		 	ur_delete_all(keyuv);
			if (tig_unlabeled != 0 && keyuv == tig_unlabeled_keys[tig_unlabeled-1])
			{
				tig_unlabeled_keys[tig_unlabeled-1]=0;
				tig_unlabeled--;
			}
		}
		if((keysf != 0) && (!pre_exist_surf))
		{
			ur_delete_all(keysf);
			xlated_key[ixsf] = 0;
		}
		else if (keysf && keyuv != 0)
		{
			e1.key = keysf;
			status = ncl_retrieve_data_fixed(&e1);
			ncl_update_sskey (&e1,keyuv,0);
		}
		
		*key = keycv;
	}
xit:;
	uu_toolfree(c);
	
	label_comp_element = label_comp_element_sav;
   DDC_displayable_flag = displayable_sav;

	if (*key != 0)
		return UU_SUCCESS;
	else
		return UU_FAILURE; 
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_trimsrf(gblk,dblk,pblk,key)
**          Translate member surface and curve(s) of an IGES trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**          gblk            global block
**          dblk            directory block
**          pblk            parameter block
**       OUTPUT :  
**          key             Set to 1 to give illusion sf was translated.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_trimsrf(gblk,dblk,pblk,key)
struct global_rec *gblk;         /* global record sturcture */
struct dir_rec *dblk;            /* directory record */
struct IG_igestrsf_rec *pblk;    /* iges trimmed srf record */
UU_KEY_ID *key;
{
	struct dir_rec ldir;
	struct IG_igescvsf_rec *cvonsfp;
	struct NCL_trimsf_rec eptr;
	struct NCL_fixed_databag  e1, e2, e3;
	UU_REAL save_unit_scale, tol, *t, uv[6];
	UU_KEY_ID keysf, keycv, keyuv, *ikeys, *ikeyp;
	UU_KEY_ID keycv_indep, inner_keycv, inner_keyuv;
	int i, k, irec,num,status, statin, isvdsp, sftyp, form, stattf;
	int istat,irec_cv;
	int outer_spatial_cv_dnum;
	UM_transf tt,tfmat;
	UU_LOGICAL lsvlab, savtrans, sav_draw, store=UU_TRUE, hldtrans;
	char *c, clabel[64], p_buff[100];
	int uig_match_surface();
	UU_LOGICAL offset;

	offset = UU_FALSE;
	um_evlset();
	UIG_from_trimsrf = 1;
	status = 0;
	posangle = UU_FALSE;
	*key   = keyuv = keycv = 0;
	save_unit_scale = unit_scale;
	if (gblk->units==1) tol = 0.0002;
	else tol = 0.005;
	isvdsp = DDC_displayable_flag;
	lsvlab = label_comp_element;
	if (pblk->n1 == 1 || pblk->crv > 0)
	{
		DDC_displayable_flag = UM_NEVERDISPLAYABLE;
		label_comp_element = UU_TRUE;
	}
	c = UU_NULL;
	c = uu_toolmalloc(MAX_PARA_REC);
	ikeys = 0;
	if ((num = pblk->n2) > 0)
		ikeys = (UU_KEY_ID *)uu_toolmalloc(2*num*sizeof(UU_KEY_ID));
	ikeyp = ikeys;
	irec = pblk->srf;
	status = uig_get_data(irec,0,&ldir,c);
	if (status == 0)
	{
		form  = ldir.form_no;
		sftyp = ldir.rel_type;
		if (sftyp == GOFFSTSRF) offset = UU_TRUE;
		init_label(ldir.rel_type, irec);
		check_color(&ldir);
		current_dir_number = irec;
		uig_in_dispat(gblk,&ldir,c,&keysf);

		if (keysf < 1)
		{
			sprintf (p_buff, 
				"(DREC = %d) Base surface of trimmed surface failed to translate.\n",
				dblk->drec_num);
			uig_error(p_buff);
		}
		update_counts(keysf, irec);
		*key = keysf;
/*
.....If this trim surface is a part of an instance, then use the transformation 
.....matrix from the instance.
*/
		if(instance_trans== UU_TRUE)
		{
			t = (UU_REAL *)uu_toolmalloc(12*sizeof(UU_REAL));
			um_tftotf(instance_tf, tt);
			um_vctmsc(tt[3],unit_scale,tt[3]);
			uig_tf_to_igtf(tt,t);
		}
		else
			uig_load_current_matrix(dblk->matrix_ptr, &t);
		if (((int)keysf > 0) && (pblk->crv > 0))
		{
/*
..... get surface parameters min and max
*/
			status = uig_get_srf_uv (ldir.rel_type, form, c, uv);
/*
..... Translate outer boundary curve.
*/
			irec = pblk->crv;
			status = uig_get_data(irec,0,&ldir,c);
			if (status == 0)
			{
				cvonsfp = (struct IG_igescvsf_rec *)c;
/*
........Don't use XYZ boundary curves
........if surface is transformed
........because IGES transforms the curves
........prior to storing the curves while
........NCL does not transform the boundary curves
........FSR 61754
........Bobby - 120215
*/
				if (!um_is_idmat(t)) cvonsfp->pref = 1;
/*
..... If curves-on-surfaces are filtered, make an independent curve
..... entity.  Otherwise, just update the statistics.
*/
   			if (entity_mask[18] == 1)
				{
					init_label(ldir.rel_type, irec);
					check_color(&ldir);
					hldtrans = IG_trans;
					IG_trans = UU_TRUE;
					UIG_splitccrv_wrk = UIG_splitccrv;
					uig_in_dispat(gblk,&ldir,c,&keycv_indep);
					UIG_splitccrv_wrk = 0;
					IG_trans = hldtrans;
					update_counts(keycv_indep, irec);
				}
				else
				{
					check_color(&ldir);
					k = 1;
					update_counts(k, irec);
				}
/*
.....If label type is equal to 6 and there is a property
.....label entity associate with the crvonsrf, updates counts
.....as though the property was processed.  JLS 1/27/99
*/
				if ((label_type==6||label_type == 10) && (cvonsfp->no_prop > 0)
						&&(cvonsfp->prop[0] <= 150000))
					update_counts(1,cvonsfp->prop[0]);
				irec = cvonsfp->b_ptr;
				i = cvonsfp->crv;
                outer_spatial_cv_dnum = cvonsfp->crv;
				if (i == 0 && sftyp == GPLANE) status = UU_FAILURE;
/*
..... if the preference is to use the UV-curve, and the curve is present,
..... do not translate the XYZ curve since it could be off the surface
..... QAR 92080
*/
 				if (irec > 0 && cvonsfp->pref == 1) i = 0;
				if (i>0)
				{
/*
.....vp 5-aug-97 make sure that directory has this entry
.....(some IGES files contain deleted entity from directory)
*/
					status = uig_get_data(i,0,&ldir,c);
					if (status == 0)
					{
						init_label(ldir.rel_type, i);
						check_color(&ldir);
						current_dir_number = i;
						uig_in_dispat(gblk,&ldir,c,&keycv);
						update_counts(keycv, i);
					}
					if ((sftyp == GPLANE) && ((int)keycv > 0))
					{
						e1.key = keysf;
						status = ncl_retrieve_data_fixed(&e1);
						if (status == UU_SUCCESS)
						{
							e2.key = keycv;
							status = ncl_retrieve_data_fixed(&e2);
						}
/*
..... removed this fix since the plane generating this error did not have
..... a unit normal.

..... If the base surface is a plane and it does not lie at 
..... the same level as the boundary curve which is planar, then the base 
..... plane is shifted to the level of the curve, provided the plane
..... normal is parallel to the normal of the plane on which the curve lies.
						if (status == UU_SUCCESS)
						{	
							status = uig_shift_plane(&e1,&e2);
						}
*/
						if (status == UU_SUCCESS)
						{
							strcpy(e3.label,"@UN      ");
							e3.subscr = 0;
							status = ncl_plane_to_sf(&e1,&e2,&e3);
						}
						if (status == UU_SUCCESS)
							keysf = e3.key;
					}
				}
				status = (irec == 0)? -2: uig_get_data(irec,0,&ldir,c);
			}
			if (status == 0)
			{
				init_label(ldir.rel_type, irec);
				check_color(&ldir);
				current_dir_number = irec;
				unit_scale = 1.0;
/*
.....Since we are working with uv curves, we don't want the
.....curve to be translated, it needs to stay right where it is
.....so save instance_trans, then set it to UU_False, this will
.....result in the identity matrix being the transformation matrix.
.....After calling uig_in_in_dispat set instance_trans back to 
.....what it was. JLS 1/18/99
*/
/*
..... Outer curve as uv-curve.

..... Save the drawing flag and then pretend that there is no drawing.
..... This allows given uv-curve to be translated correctly, even if
..... it is a part of a drawing.
*/
				savtrans=instance_trans;
				instance_trans=UU_FALSE;
				sav_draw = drw_flag;
				drw_flag = 0;
/*
..... bump_u is 'true' means u-coordinates are angles, and we translate angles
..... as positive
*/

				bump_u = UU_TRUE; 
				bump_v = UU_TRUE;
				starting_ang_u = uv[0];
				starting_ang_v = uv[2];
				period_u = uv[4];
				period_v = uv[5];

				hldtrans = IG_trans;
				IG_trans = UU_FALSE;
				uig_in_dispat(gblk,&ldir,c,&keyuv);
				IG_trans = hldtrans;

				bump_u = UU_FALSE;
				bump_v = UU_FALSE;
				starting_ang_u = 0.0;
				starting_ang_v = 0.0;
				period_u = 0.0;
				period_v = 0.0;

				drw_flag = sav_draw;
				instance_trans=savtrans;
				update_counts(keyuv, irec);
				unit_scale = save_unit_scale;
				if (keyuv == 0) 
					status = -2;
/*
...check validity of outer boundry parameter curve
*/
				else 
					status = uig_check_uv (uv,keyuv);
			}
/*
..... Translate inner boundary curves.
*/
			statin = 0;
			for (i=0; i<num; i++) 
			{
				irec = pblk->cid[i];
				istat = uig_get_data(irec,0,&ldir,c);
				if (istat == 0)
				{
					cvonsfp = (struct IG_igescvsf_rec *)c;
/*
..... If curves-on-surfaces are filtered, make an independent curve
..... entity.  Otherwise, just update the statistics.
*/
					if (entity_mask[18] == 1)
					{
						init_label(ldir.rel_type, irec);
						check_color(&ldir);
						hldtrans = IG_trans;
						IG_trans = UU_TRUE;
						uig_in_dispat(gblk,&ldir,c,&keycv_indep);
						IG_trans = hldtrans;
						update_counts(keycv_indep, irec);
					}
					else
					{
						k = 1;
						check_color(&ldir);
						update_counts(k, irec);
					}
/*
.....If label type is equal to 6 and there is a property
.....label entity associate with the crvonsrf, updates counts
.....as though the property was processed.  JLS 1/27/99
*/
					if((label_type==6) && (cvonsfp->no_prop>0)
						&&(cvonsfp->prop[0]<=150000))
						update_counts(1,cvonsfp->prop[0]);
					irec = cvonsfp->b_ptr;
					irec_cv = k = cvonsfp->crv;
					if (irec > 0 && cvonsfp->pref == 1) 
						istat = -1;
					else
/*
.....vp 5-aug-97 make sure that directory has this entry
.....(some IGES files contain deleted entity from directory)
*/
						istat = uig_get_data(k,0,&ldir,c);
					if (istat == 0)
					{
						init_label(ldir.rel_type, k);
						check_color(&ldir);
						current_dir_number = k;
						*ikeyp = 0;
/*
.....Save current value of instance_trans and
.....set it to UU_FALSE, return it to its original value after
.....calling uig_in_dispat. JLS 1/18/99
*/
						if (entity_mask[18] == 1)
						{
							savtrans=instance_trans;
							instance_trans=UU_FALSE;
							hldtrans = IG_trans;
							IG_trans = UU_FALSE;
							uig_in_dispat(gblk,&ldir,c,ikeyp);
							IG_trans = hldtrans;
							instance_trans=savtrans;
						}
						else
							uig_in_dispat(gblk,&ldir,c,ikeyp);
						update_counts(*ikeyp, k);
						ikeyp++;
					}
/*
.....When 141 and 143 entities are translated back into iges
.....as trims surfaces, and then the translation back to a 
.....unibase file, the inner boundaries are loaded without 
.....xyz curves, so we need to set ikeyp to 0 and increment.
.....It does not matter that there was not an xyz curve because
.....in the trim surface display routine it will just skip over the 
.....zero, but we do need it as a place holder.
.....JLS 2/4/99
*/
					else
					{
						*ikeyp=0;
						ikeyp++;
					}
					istat = (irec == 0)? -2: uig_get_data(irec,0,&ldir,c);
				}
				if (istat == 0)
				{
					init_label(ldir.rel_type, irec);
					check_color(&ldir);
					current_dir_number = irec;
					*ikeyp = 0;
/*
.....Also same as above, make sure that the uv curve isn't
.....being transformed. JLS 1/18/99
*/
					if (entity_mask[18] == 1)
					{
						savtrans=instance_trans;
						instance_trans=UU_FALSE;
						hldtrans = IG_trans;
						IG_trans = UU_FALSE;
					}
					unit_scale = 1.0;

					bump_u = UU_TRUE; 
					bump_v = UU_TRUE;
					starting_ang_u = uv[0];
					starting_ang_v = uv[2];
					period_u = uv[4];
					period_v = uv[5];

					uig_in_dispat(gblk,&ldir,c,ikeyp);

					bump_u = UU_FALSE;
					bump_v = UU_FALSE;
					starting_ang_u = 0.0;
					starting_ang_v = 0.0;
					period_u = 0.0;
					period_v = 0.0;

					if (entity_mask[18] == 1)
					{
						IG_trans = hldtrans;
						instance_trans=savtrans;
					}
					unit_scale = save_unit_scale;
					update_counts(*ikeyp, irec);
					if (*ikeyp == 0 && ikeyp == ikeys+2*i+1)
					{
						inner_keycv = ikeys[2*i];
						if (inner_keycv == 0 && irec_cv > 0)
						{
							istat = uig_get_data(irec_cv,0,&ldir,c);
							if (istat == 0)
							{
								check_color(&ldir);
								current_dir_number = irec_cv;
/*
.....Save current value of instance_trans and
.....set it to UU_FALSE, return it to its original value after
.....calling uig_in_dispat. JLS 1/18/99
*/
								if (entity_mask[18] == 1)
								{
									savtrans=instance_trans;
									instance_trans=UU_FALSE;
									hldtrans = IG_trans;
									IG_trans = UU_FALSE;
									uig_in_dispat(gblk,&ldir,c,&inner_keycv);
									IG_trans = hldtrans;
									instance_trans=savtrans;
								}
								else
									uig_in_dispat(gblk,&ldir,c,&inner_keycv);
							}
						}
						if (inner_keycv != 0)
						{
							istat = uig_evcrv_to_uvcv(&inner_keycv, &keysf, &inner_keyuv,uv,tol);
							ur_delete_all(inner_keycv);

							if ((istat == 0) && (inner_keyuv != 0))
							{
								istat = uig_check_uv(uv,inner_keyuv);

								if (istat == 0)
								{
									*ikeyp = inner_keyuv;
									update_counts(*ikeyp, irec);
								}

							}
						}

						if (*ikeyp == 0) istat = -2;
					}
					else
/*
...check validity of inner boundry parameter curve
*/
						istat = uig_check_uv (uv,*ikeyp);
					ikeyp++;
				}
				else if(istat == -2)
				{
					*ikeyp = 0;
					ikeyp ++;
				}

				if (istat != 0 && statin == 0) statin = istat;
			}
/*
..... jingrong 11/9/98 build uv trimming curve based on cv boundary 
..... check if uv-curves were created.
*/
			if (status == -2 || statin == -2)
			{
				if (status == -2)
				{
                    if ((keycv < 1) && (outer_spatial_cv_dnum != 0))
                    {
                        status = uig_get_data(outer_spatial_cv_dnum,0,&ldir,c);
                        if (status == 0)
                        {
                            init_label(ldir.rel_type, outer_spatial_cv_dnum);
                            check_color(&ldir);
                            current_dir_number = outer_spatial_cv_dnum;
                            uig_in_dispat(gblk,&ldir,c,&keycv);
                            update_counts(keycv, outer_spatial_cv_dnum);
                        }
                        if ((sftyp == GPLANE) && ((int)keycv > 0))
                        {
                            e1.key = keysf;
                            status = ncl_retrieve_data_fixed(&e1);
                            if (status == UU_SUCCESS)
                            {
                                e2.key = keycv;
                                status = ncl_retrieve_data_fixed(&e2);
                            }
                            if (status == UU_SUCCESS)
                            {
                                strcpy(e3.label,"@UN      ");
                                e3.subscr = 0;
                                status = ncl_plane_to_sf(&e1,&e2,&e3);
                            }
                            if (status == UU_SUCCESS)
                                keysf = e3.key;
                        }
                        if (keycv < 1)
                        {
                            sprintf (p_buff,
        "            Parametric and spatial outer trimming curves bad!\n");
                            uig_error (p_buff);
                        }
                    }
					UIG_change_uv = 0;
					status = uig_evcrv_to_uvcv(&keycv,&keysf,&keyuv,uv,tol);
					if ((int)keyuv <= 0) status = -1;
					if (status == 0)
						status = uig_check_uv(uv,keyuv);
/*
..... If the outer boundary curve does not lie within the uv boundary
..... of a surface of revolution then we shift the start and end angle 
..... of revolution.
*/  					
					if (status != 0 && sftyp == GREVSRF)
					{
						UIG_change_uv = 1;
						status =uig_evcrv_to_uvcv(&keycv,&keysf,&keyuv,uv,tol);
						if ((int)keyuv <= 0) status = -1;
						if (status == 0)
						{ 
  				 			status = uig_check_uv(uv,keyuv);
						}
					}
				}
				if (status == 0 && statin == -2)
				{
					for (i = 1; i <= num; i++)
					{
						inner_keyuv = ikeys[2*i-1];
						if (inner_keyuv == 0)
						{

						inner_keycv = ikeys[2*i-2];
						if (inner_keycv != 0)
						{
						statin = uig_evcrv_to_uvcv(&inner_keycv, &keysf, &inner_keyuv,uv,tol);
						}
						if ((statin == 0) && ((int)inner_keyuv > 0))
							statin = uig_check_uv(uv,inner_keyuv);
						if (statin != 0)
						{
							UIG_inner_error = 1;
							num = i-1;
							if (num > 0) statin = 0;
							break;
						}
						ikeys[2*i-1] = inner_keyuv;
						}
					}
				}
			}
/*			uig_get_trans(dblk->matrix_ptr,t);*/
			if (status == 0)
			{
/*
..... map trimsf only if there a valid outer boundary
*/
				DDC_displayable_flag = isvdsp;
				label_comp_element = lsvlab; 
				if (statin != 0)  num = 0; 
				uig_map_trimsrf(dblk,pblk,t,keysf,keyuv,keycv,uv,num,ikeys,key);
			   	
			}
			else
			{
				struct NCL_fixed_databag *bs; 
				DDC_displayable_flag = isvdsp;
				label_comp_element = lsvlab;
				bs = (struct NCL_fixed_databag *) c;
				bs->key = keysf;
				ncl_retrieve_data_fixed (bs);
				stattf = UU_SUCCESS;
				uig_igtf_to_tf(t, tfmat);
				if (!um_is_idmat(tfmat)) stattf = uig_transform(bs,tfmat,store);
				um_save_active_label(NCL_TRIMSF_REL);
				create_label(dblk, pblk->no_prop, pblk->prop, clabel, &bs->subscr);
				strcpy(bs->label, clabel);
				i = 8;
				ul_strip_blanks (clabel,&i);
				sprintf (p_buff,
		"(DREC = %d) Trimmed surf %s has a boundary problem. Imported as untrimmed.\n",
						dblk->drec_num, clabel);
				uig_error (p_buff);
				if (keyuv < 1)
				{
					sprintf (p_buff,
			"            Failed to project outer, spatial boundary curve onto surface.\n");
					uig_error (p_buff);
				}
				if (status == -2)
				{
					sprintf (p_buff,
			"            Outer, parametric curve went outside of surface u,v boundaries.\n");
					uig_error (p_buff);
				}
				if (stattf != UU_SUCCESS)
				{
					sprintf (p_buff,
			"            Base surf could not be transformed.\n");
					uig_error (p_buff);
				}
				ur_update_data_fixed(bs);
				ur_update_displayable(keysf, DDC_displayable_flag);
				uig_transf_entity (bs, dblk);
				*key = keysf;
			}

			eptr.key = *key;
/*
..... Call uig_match_surface instead of uig_match_trimsrf, since the base
..... surface is used .
*/
			if (UIG_nodups && !label_comp_element &&
				ncl_retrieve_data_fixed(&eptr) == UU_SUCCESS &&
				uig_match_surface(&eptr,0) == UU_SUCCESS)
			{
				uig_remove_dup(&eptr,key);
			}
			else
			{

				if (label_type == 8)
				{
/*
.....Label matching. Determine if exact match.
*/
					uig_exact_match(&eptr,uig_match_surface);
				}
			}
/*
.....Check the base surface offset distance
*/
			if (offset) 
				uig_check_offset_dist(&eptr,keysf);
		}
	}
	DDC_displayable_flag = isvdsp;
	label_comp_element = lsvlab; 
	if (c != 0) uu_toolfree(c);
	if (ikeys != 0) uu_toolfree(ikeys); 
	if (instance_trans== UU_TRUE && t != 0) uu_toolfree(t); 
	if(instance_trans== UU_FALSE)uig_pop_current_matrix();
	UIG_from_trimsrf = 0;
	um_evlrst();

	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_get_srf_uv (sftype, form, c, uv)
**      Return the maximum & minimum u & v extents of a surface.
**    PARAMETERS   
**       INPUT  : 
**            sftype             Surface type.
**            c                  Surface data.
**       OUTPUT :  
**            uv[0]              U min.
**            uv[1]              U max.
**            uv[2]              V min.
**            uv[3]              V max.
**            uv[4]              U period ( = 0.0 if not periodic ).
**            uv[5]              V period ( = 0.0 if not periodic ).
**    RETURNS      : UU_FAILURE if unknown surface; else UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : May change the data in input array c.
*********************************************************************/

int uig_get_srf_uv (sftype, form, c, uv)
int sftype, form;
char *c;
UU_REAL uv[6];
{
	int status, irec;
	struct dir_rec ldir;
	struct NCL_fixed_databag e;
	UU_REAL diffu, diffv, maxdiff;

	status = UU_SUCCESS;
	switch (sftype)
	{
		case GSPLSURF:
		{
			struct IG_igesplsf_rec *sfp;
			sfp = (struct IG_igesplsf_rec *)c;
			uv[0] = sfp->tu[0];
			uv[1] = sfp->tu[sfp->no_tu-1];
			uv[2] = sfp->tv[0];
			uv[3] = sfp->tv[sfp->no_tv-1];
			uv[4] = 0.0;
			uv[5] = 0.0;
			break;
		}
		case GRSPLSRF:
		{
			struct IG_igesrssf_rec *rbsrf;
			rbsrf = (struct IG_igesrssf_rec *)c;
			if (rbsrf->period1 == 0)
			{
				uv[0] = rbsrf->t1[0];
				uv[1] = rbsrf->t1[rbsrf->no_t1-1];
			}
			else
			{
				uv[0] = rbsrf->rspara->u0;
				uv[1] = rbsrf->rspara->u1;
			}
			if (rbsrf->period2 == 0)
			{
				uv[2] = rbsrf->t2[0];
				uv[3] = rbsrf->t2[rbsrf->no_t2-1];
			}
			else
			{
				uv[2] = rbsrf->rspara->v0;
				uv[3] = rbsrf->rspara->v1;
			}
			uv[4] = 0.0;
			uv[5] = 0.0;
			break;
		}
		case GOFFSTSRF:
		{
			struct IG_igesofsf_rec *offsrf;
			offsrf = (struct IG_igesofsf_rec *)c;
			irec = offsrf->srf;
			status = uig_get_data(irec,0,&ldir,c);
			if (status == 0)
			status = uig_get_srf_uv (ldir.rel_type, ldir.form_no, c, uv);
			break;
		}
		case GREVSRF:
		{
			struct IG_igesrvsf_rec *revsrf;
			revsrf = (struct IG_igesrvsf_rec *) c;
			e.key = NCL_savekey;
			status = ncl_retrieve_data_fixed (&e);
/* 
.....vp 6-jan-97 if generatrix is a circle use parametrization
.....of that circle as u boundaries of srf (set by uig_in_arc_prm)
*/
			if (e.rel_num == UM_CIRCLE_REL)
			{
				uv[0] = NCL_t0;	
				uv[1] = NCL_t1;
				uv[4] = UM_TWOPI;
			}	
			else
			{
				uv[0] = 0.0;
				uv[1] = 1.0;
				uv[4] = 0.0;
			}
			uv[2] = revsrf->sa;
			uv[3] = revsrf->ea;
			while(uv[2] < -UM_FUZZ)
			{
				uv[2] += UM_TWOPI;
				uv[3] += UM_TWOPI;
			}
			while (uv[2] > (UM_TWOPI + UM_FUZZ))
			{
				uv[2] -= UM_TWOPI;
				uv[3] -= UM_TWOPI;
			}
			uv[5] = UM_TWOPI;
			break;
		}
		case GRULEDSRF:
		{
			status = UU_SUCCESS; 
			uv[0] = 0.0;
			uv[1] = 1.0;
			uv[2] = 0.0;
			uv[3] = 1.0;
			uv[4] = 0.0;
			uv[5] = 0.0;
			if (form == 1) status = uig_get_rulsrf_parm (c,uv);
			break;
		}
		case GTBCYSRF:
		default:
			status = UU_FAILURE;
			uv[0] = 0.0;
			uv[1] = 1.0;
			uv[2] = 0.0;
			uv[3] = 1.0;
			uv[4] = 0.0;
			uv[5] = 0.0;
			break;
	}
/*
..... If this routine is called as a part of a trimmed surface adjust 
..... tolerance for creating composite curves according to the size of
..... the uv boundary of the surface. For the surface of revolution and plane
..... used a fixed tolerance.
*/
	if (UIG_from_trimsrf)
	{
		if (sftype == GREVSRF)
			UIG_comp_tol = 0.02;
		else if (sftype == GPLANE)
			UIG_comp_tol = 0.02;
		else
		{
			diffu = fabs(uv[0] - uv[1]);
			diffv = fabs(uv[2] - uv[3]);
			if (diffu < diffv)
				maxdiff = diffv;
			else
				maxdiff = diffu;
	 		if (maxdiff <= 1.0)
				UIG_comp_tol = 0.02;
			else if (maxdiff < 10.0)
   			UIG_comp_tol = 0.05;
			else if (maxdiff < 20.0)
   			UIG_comp_tol = 0.05;
			else if (maxdiff < 30.0)
   			UIG_comp_tol = 0.06;
			else
   			UIG_comp_tol = 0.02;
		}
	}	
	return (status); 
}

/*********************************************************************
** E_FUNCTION: uig_get_rulsrf_parm (c,uv)
**       Gets UV parameters span for ruled surface. 
**    PARAMETERS
**       INPUT  : c   - pointer to the ruled surface IGES record
**       OUTPUT : uv  - min, max, and periods of u and v
**                        uv[0] = u min
**                        uv[1] = u max
**                        uv[2] = v min
**                        uv[3] = v max
**                        uv[4] = u period ( = 0.0 if not periodic)
**                        uv[5] = u period ( = 0.0 if not periodic)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/

int uig_get_rulsrf_parm (c,uv)
struct IG_igesrlsf_rec *c;		
UU_REAL uv[6];
{
	int i, irec[2];
	struct dir_rec ldir;
	char *crv, *uu_toolmalloc();
	UU_REAL t0, t1;

	uv[0] = 0.;
	uv[1] = 1.;
	uv[4] = 0.0;
	uv[5] = 0.0;
	crv = uu_toolmalloc (MAX_PARA_REC);
	irec[0] = c->crv1;
	irec[1] = c->crv2;
	for (i=0; i<2; i++)
	{
		uig_get_data (irec[i],0,&ldir,crv);  
		switch (ldir.rel_type)
		{
			case GPOINT:
			case GLINE:
			case GARC:
			case GCONIC:
			case GCOMPOSITE:
			case GPOLY:
			case GPOLY3D:
				break;
			case GSPLINE:
			{
				struct IG_igesplin_rec *pblk;
				pblk = (struct IG_igesplin_rec *) crv;
				t0 = pblk->t[0];
				t1 = pblk->t[pblk->no_t-1];
				if (uv[0] < t0) uv[0] = t0;
				if (uv[1] < t1) uv[1] = t1;
				break;
			}
			case GRSPLINE:
			{
				struct IG_igesrspl_rec *pblk;
				struct IG_rpara_rec *parm;
				pblk = (struct IG_igesrspl_rec *) crv;
				parm = (struct IG_rpara_rec *) pblk->rpara;
				t0 = parm->t0;
				t1 = parm->t1;
				if (uv[0] < t0) uv[0] = t0;
				if (uv[1] < t1) uv[1] = t1;
				break;
			}
		}
	} 
	if (crv != 0) uu_toolfree(crv);
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_arc_prm(pblk,t0,t1)
**				Get parametrization of the iges arc (eq. arc angles).
**    PARAMETERS   
**       INPUT  : 
**				pblk							iges circle data block
**       OUTPUT :  
**				t0,t1                   arc start & end parameter values
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_arc_prm(igcir,t0,t1)
struct IG_igesarc_rec *igcir;			/* iges arc parameter record */
UU_REAL *t0, *t1;
{
	UU_REAL pts[3], pte[3], ptc[3], alpha;
	UM_vector vecs, vece;
	UM_vector xas;
	UM_vector nv;

	xas[0] = 1.; xas[1] = 0.; xas[2] = 0.;
	nv[0] = 0.; nv[1] = 0.; nv[2] = 1.;
	ptc[0] = igcir->cpt[0];	
	ptc[1] = igcir->cpt[1];	
	ptc[2] = 0;	
	pts[0] = igcir->spt[0];	
	pts[1] = igcir->spt[1];	
	pts[2] = 0;	
	pte[0] = igcir->ept[0];	
	pte[1] = igcir->ept[1];	
	pte[2] = 0;	
	um_vcmnvc (pts,ptc,vecs);
	um_unitvc (vecs,vecs);
/*
.....get start angle of arc
*/
	if (um_cceqcc(vecs,xas) == UU_TRUE)
		*t0 = 0.;
	else
		*t0 = um_angle2p (xas,vecs,nv);
/*
.....get end angle of arc
*/
	if (um_cceqcc(pts,pte) == UU_TRUE)
		alpha = UM_TWOPI;
	else
	{
		um_vcmnvc (pte,ptc,vece);
		um_unitvc (vece,vece);
		alpha = um_angle2p (vecs,vece,nv);
	}
	*t1 = *t0 + alpha;

	return(0);
}
/*********************************************************************
**    I_FUNCTION     :  uig_in_crvonsrf_out (gblk,ldir,crv,keylst)
**				Map 'xyz' curve on surface used to trimm SF as independant
**          entity.  This is temporary solution used when trimmed SF
**          does not have 'uv' curve supplied with.
**    PARAMETERS   
**       INPUT  : 
**				gblk		global block
**				ldir 		directory block
**				crv		iges curve data block
**       OUTPUT :  
**				keylst	key of the created unibase curve.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_crvonsrf_out (gblk,ldir,crv,keylst)
struct global_rec *gblk;
struct dir_rec *ldir;
struct IG_igescvsf_rec *crv;
UU_KEY_ID *keylst;
{
	int isvdsp;
	UU_KEY_ID key;
	UU_LOGICAL lsvlab;

	isvdsp = DDC_displayable_flag;
	lsvlab = label_comp_element;

	DDC_displayable_flag = UM_DISPLAYABLE; 
	label_comp_element = UU_FALSE;
	uig_in_dispat(gblk,ldir,crv,&key); 
	if (key != 0) *keylst = key;

	DDC_displayable_flag = isvdsp;
	label_comp_element = lsvlab; 
	return(0);
}
/*********************************************************************
**    I_FUNCTION     :  uig_in_bdsrf(gblk,dblk,pblk,key)
**          Translate member surface and curve(s) of an IGES trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**          gblk            global block
**          dblk            directory block
**          pblk            parameter block
**       OUTPUT :  
**          key             Set to 1 to give illusion sf was translated.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_bdsrf(gblk,dblk,pblk,key)
struct global_rec *gblk;         /* global record sturcture */
struct dir_rec *dblk;            /* directory record */
struct IG_igesbdsf_rec *pblk;    /* iges bounded srf record */
UU_KEY_ID *key;
{
	struct dir_rec ldir;
	struct IG_igesbndy_rec *bndyptr, newbndy;
	struct NCL_trimsf_rec trimsrf;
	struct UM_compcrv_rec newkeycv;
	UU_REAL tol, *t, uv[6];
	UU_KEY_ID keysf, keycv, keyuv, *ikeys, *ikeyp, *trimkeys, *keybl;
	UU_KEY_ID *new_comp=NULL, *uvcomp=NULL, *big_uv_comp=NULL;
	int num_uvcomp, numhld, *sense = NULL, counter,counter2;
	int i, j, k, bcnt, irec, num, status, statin, isvdsp, form;
	int i2,j2,n1,numcrvs, return_status;
	char *c, clabel[64], mlabel[64], p_buff[100];
	UU_LOGICAL lsvlab;
	int uig_match_trimsrf();
/*
.....Initialize the counters and flags.
*/
	counter=0;
	status = 0;
	tol = 0.001;
	return_status = UU_SUCCESS;
	bcnt   = 0;
	*key   = keyuv = keycv = 0;
	isvdsp = DDC_displayable_flag;
	lsvlab = label_comp_element;
	numhld = 0;
	if (pblk->no_cid > 0)
	{
		DDC_displayable_flag = UM_NEVERDISPLAYABLE;
		label_comp_element = UU_TRUE;
	}
	c = UU_NULL;
	c = uu_toolmalloc(MAX_PARA_REC);
	ikeys = trimkeys = 0;
	keybl = uvcomp = new_comp = big_uv_comp = sense = 0;
  	newbndy.crvptr = 0;
/*
.....num is the number of the innerboundaries, no_cid is the number of
.....innerboundaries plus the outer boundary, so subtract 1 from
.....no_cid to get num.
*/
	if ((num =((pblk->no_cid)-1)) > 0)
	{
		ikeys = (UU_KEY_ID *)uu_toolmalloc(2*num*sizeof(UU_KEY_ID));
		trimkeys = (UU_KEY_ID *)uu_toolmalloc(2*num*sizeof(UU_KEY_ID));
	}
	keybl = (UU_KEY_ID *)uu_toolmalloc(2*(num+1)*sizeof(UU_KEY_ID));
	ikeyp = ikeys;
/*
.....This is the first pointer get the info on it and process it.
*/
	irec = pblk->srf;
	status = uig_get_data(irec,0,&ldir,c);
	if (status == 0)
	{
		form  = ldir.form_no;
		init_label(ldir.rel_type, irec);
		check_color(&ldir);
		current_dir_number = irec;
		uig_in_dispat(gblk,&ldir,c,&keysf);
		update_counts(keysf, irec);
		*key = keysf;
		if (((int)keysf > 0) && (pblk->no_cid > 0))
		{
			status = uig_get_srf_uv (ldir.rel_type, form, c, uv);
/*
.....Now at the second pointer, get the info and process it.
*/
			irec = pblk->cid[0];
			status = uig_get_data(irec,0,&ldir,c);
			if (status == 0)
			{
/*
.....bndyptr has the information associated with the first Boundary
.....entity, process the information associated with it.
*/
				bndyptr = (struct IG_igesbndy_rec *)c;
/*
.....numcrvs is the number of curves that make up the boundary.
*/
				numcrvs = bndyptr->no_crvptr;
				if (numcrvs > 0)
				{
					sense = (int *)uu_toolmalloc(numcrvs*sizeof(int));
					big_uv_comp = (UU_KEY_ID *)uu_toolmalloc(numcrvs*sizeof(UU_KEY_ID));
					new_comp = (UU_KEY_ID *)uu_toolmalloc(numcrvs*sizeof(UU_KEY_ID));
					newbndy.crvptr = (struct IG_crvptr_rec *)
						uu_toolmalloc(numcrvs*sizeof(struct IG_crvptr_rec));
					for (i=0; i<numcrvs; i++)
						newbndy.crvptr[i].cid =
							(UU_KEY_ID *)uu_toolmalloc(10*sizeof(UU_KEY_ID));
				}
				numhld = numcrvs;
/*
.....since the information in bndyptr changes below when c changes,
.....we want to save the info in bndyptr to another structure that
.....won't change.
*/
				uig_copy_bndy(bndyptr, &newbndy);
				k = 1;
				update_counts(k, irec);
				for (j =0; j<numcrvs; j++)
				{
					i = newbndy.crvptr[j].crvptr;
/*
.....Keep track of whether or not this curve needs to be reversed.
*/
					sense[j] = newbndy.crvptr[j].sense;
					if (i>0)
					{
/*
.....vp 5-aug-97 make sure that directory has this entry
.....(some IGES files contain deleted entity from directory)
*/
						status = uig_get_data(i,0,&ldir,c);
						if (status == 0)
						{
							init_label(ldir.rel_type, i);
							check_color(&ldir);
							current_dir_number = i;
							if (irec == 0)
								uig_in_crvonsrf_out (gblk,&ldir,(struct IG_igescvsf_rec *)c,&keybl[bcnt++]);
							else
								uig_in_dispat(gblk,&ldir,c,&keycv);
							update_counts(keycv, i);
						}
/*
.....If numcrvs is greater than 1, new_comp is going to be used to make
.....a composite curve to be used as the boundary curve, if numcrvs is
.....equal to 1, then keycv will be processed
*/
						new_comp[j] = keycv;
						counter=counter +1;
						counter2 = 0;
/*
.....The members of newbndy.crvptr.cid if there are any, are in parameter
.....space.  If they do exist, then we won't need to convert the xyz
.....curve to uv.
*/
            		n1 = newbndy.crvptr[j].no_cid;
						if (n1>0)
						{
							uvcomp = (UU_KEY_ID *)uu_toolmalloc(n1*sizeof(UU_KEY_ID));
						}
						num_uvcomp = n1;
            		for (j2=0;j2<n1;j2++)
						{
							i = newbndy.crvptr[j].cid[j2];
							if (i>0)
							{
								status = uig_get_data(i,0,&ldir,c);
								if (status == 0)
								{
									bump_u = UU_TRUE; 
									bump_v = UU_TRUE;
									starting_ang_u = uv[0];
									starting_ang_v = uv[2];
									period_u = uv[4];
									period_v = uv[5];

									init_label(ldir.rel_type, i);
									check_color(&ldir);
									current_dir_number = i;
									if (irec == 0)
										uig_in_crvonsrf_out (gblk,&ldir,(struct IG_igescvsf_rec *)c,&keybl[bcnt++]);
									else
										uig_in_dispat(gblk,&ldir,c,&keyuv);
									counter2=counter2+1;
									uvcomp[j2]=keyuv;
									update_counts(&keyuv, i);

									bump_u = UU_FALSE;
									bump_v = UU_FALSE;
									starting_ang_u = 0.0;
									starting_ang_v = 0.0;
									period_u = 0.0;
									period_v = 0.0;
								}
							}
						}
/*
.....If there were parameter curves, create a composite curve out of them
*/
						if (counter2 >1)
							uig_create_comp(dblk,&newkeycv,counter2,uvcomp,
							  UU_TRUE, NULL, &keyuv);
/*
.....If numcrvs is greater than 1, then this is just a portion of the over all
.....uv curve, so save it into big_uv_comp
*/
						if (counter2!=0)
							big_uv_comp[j] =keyuv;
					}
				}
				if (pblk->type==0)
/*
.....Need to create the uv curve by a conversion of the keycv curve.
.....This is the parameter space of the surface. If type is equal
.....to 1 then use the information from above to create a comp crv 
.....that serves this purpose.
*/
				{
/*
.....If numcrvs is greater than one, create the composite curve before
.....processing the xyz curve into an uv curve
*/
					if (numcrvs > 1)
						uig_create_comp(dblk,&newkeycv,counter,new_comp,
							UU_TRUE, sense, &keycv);
					newkeycv.key=keycv;
					ur_retrieve_data(&newkeycv,sizeof(struct UM_compcrv_rec));
					uig_evcrv_to_uvcv(&newkeycv,key,&keyuv,uv,tol);
				}
				else
				{
/*
.....If parameter space curves were given, we may use those for keyuv.  If numcrvs
.....is equal to 1, then the correct ID is already in keyuv, if it is greater, we
.....need to create a composite curve first.
*/
					if (numcrvs >1)
						uig_create_comp(dblk,&newkeycv,counter,big_uv_comp,
							UU_TRUE,NULL,&keyuv);
					if (keyuv == 0)
					{
						if (numcrvs > 1)
							uig_create_comp(dblk,&newkeycv,counter,new_comp,
								UU_TRUE, sense, &keycv);
						newkeycv.key=keycv;
						if ((int)keycv > 0)
						{
							ur_retrieve_data(&newkeycv,sizeof(struct UM_compcrv_rec));
							uig_evcrv_to_uvcv(&newkeycv,key,&keyuv,uv,tol);
						}
					}
				}
/*
..... If failed to make a trimming curve, just translate the untrimmed
..... base surface.
*/
				if (keyuv == 0)
				{
					struct NCL_fixed_databag *bs;

					sprintf (p_buff, "(DREC = %d) Can't make trimming curve.\n",
						dblk->drec_num);
					uig_list_out (p_buff, UU_TRUE);
					DDC_displayable_flag = isvdsp;
					label_comp_element = lsvlab;
					bs = (struct NCL_fixed_databag *) c;
					bs->key = keysf;
					ncl_retrieve_data_fixed (bs);
					create_label(dblk, pblk->no_prop, pblk->prop, clabel, &bs->subscr);
					strcpy(bs->label, clabel);
					i = 8;
					ul_strip_blanks (clabel,&i);
					ur_update_data_fixed(bs);
					ur_update_displayable(keysf, DDC_displayable_flag);
					uig_transf_entity (bs, dblk);
					*key = keysf;
					return_status = UU_FAILURE;
					goto done;
				}
				else
				{
/*
.....Make sure the keyuv is compatible with uv.
*/
					status = uig_check_uv (uv,keyuv);
        			statin = 0;
					DDC_displayable_flag = isvdsp;
        			label_comp_element = lsvlab;
					uig_load_current_matrix(dblk->matrix_ptr, &t);
/*
.....Create the trimsrf, if there are inner boundaries, the will be 
.....processed afterwards and then update the already created trimsrf.
*/
					status=uig_map_bdsrf(dblk,pblk,t,keysf,keyuv,keycv,uv,0,ikeys,key);
					uig_pop_current_matrix();
					DDC_displayable_flag = UM_NEVERDISPLAYABLE;
					trimsrf.key=*key;
					label_comp_element = UU_TRUE;
					ur_retrieve_data(&trimsrf,sizeof(struct NCL_trimsf_rec));
					counter=0;
				}
/*
.....The following is for the inner boundaries of the trimsrf.
*/
				for (i2=0; i2<num && statin == 0; i2++)
				{
					*ikeyp =0;
					irec=pblk->cid[i2+1];
					statin = uig_get_data(irec,0,&ldir,c);
/*
.....This should be a 141 entity.
*/
					update_counts(1,irec);
					bndyptr = (struct IG_igesbndy_rec *)c;
					numcrvs = bndyptr->no_crvptr;
					if (numcrvs > numhld)
					{
						if (new_comp) uu_toolfree(new_comp);
						if (big_uv_comp) uu_toolfree(big_uv_comp);
						if (sense) uu_toolfree(sense);
						if (numhld>0)
						{
							for (i = 0; i<numhld; i++)
  								if (newbndy.crvptr[i].cid) uu_toolfree(newbndy.crvptr[i].cid);
  							if (newbndy.crvptr) uu_toolfree(newbndy.crvptr);
						}
						big_uv_comp = (UU_KEY_ID *)uu_toolmalloc(numcrvs*sizeof(UU_KEY_ID));
						new_comp = (UU_KEY_ID *)uu_toolmalloc(numcrvs*sizeof(UU_KEY_ID));
						sense = (int *)uu_toolmalloc(numcrvs*sizeof(int));
						newbndy.crvptr = (struct IG_crvptr_rec *)
							uu_toolmalloc(numcrvs*sizeof(struct IG_crvptr_rec));
						for (i=0; i<numcrvs; i++)
							newbndy.crvptr[i].cid =
								(UU_KEY_ID *)uu_toolmalloc(10*sizeof(UU_KEY_ID));
						numhld = numcrvs;
					}
/*
.....Copy bndyptr since it will change when c changes.
*/
					uig_copy_bndy(bndyptr,&newbndy);
					for (j=0;j<numcrvs; j++)
					{
						i=newbndy.crvptr[j].crvptr;
						if (i>0)
						{
/*
.....vp 5-aug-97 make sure that directory has this entry
.....(some IGES files contain deleted entity from directory)
*/
							status = uig_get_data(i,0,&ldir,c);
							if (status == 0)
							{
								init_label(ldir.rel_type, i);
  	               		check_color(&ldir);
  	               		current_dir_number = i;
								uig_in_dispat(gblk,&ldir,c,&keycv);
/*
.....Put this curve into the array to create a composite curve
.....if there is only one curve, then it will be processed as is
*/
								new_comp[j] = keycv;
								sense[j] = newbndy.crvptr[j].sense;
								update_counts(&keycv,i);
/*
.....If there is parameter curves, process them.
*/
								counter2=0;
            				n1 = newbndy.crvptr[j].no_cid;
								if (n1>num_uvcomp)
								{
									if (uvcomp) uu_toolfree(uvcomp);
									uvcomp = (UU_KEY_ID *)uu_toolmalloc(n1*sizeof(UU_KEY_ID));
									num_uvcomp = n1;
								}
								for (j2=0;j2<n1;j2++)
								{
									i = newbndy.crvptr[j].cid[j2];
									if (i>0)
									{
/*
.....vp 5-aug-97 make sure that directory has this entry
.....(some IGES files contain deleted entity from directory)
*/
										status = uig_get_data(i,0,&ldir,c);
										if (status == 0)
										{
											bump_u = UU_TRUE; 
											bump_v = UU_TRUE;
											starting_ang_u = uv[0];
											starting_ang_v = uv[2];
											period_u = uv[4];
											period_v = uv[5];

											init_label(ldir.rel_type, i);
											check_color(&ldir);
											current_dir_number = i;
											if (irec == 0)
												uig_in_crvonsrf_out (gblk,&ldir,(struct IG_igescvsf_rec *)c,&keybl[bcnt++]);
											else
												uig_in_dispat(gblk,&ldir,c,&keyuv);
											uvcomp[j2]=keyuv;
											update_counts(&keyuv, i);
											counter2=counter2+1;

											bump_u = UU_FALSE;
											bump_v = UU_FALSE;
											starting_ang_u = 0.0;
											starting_ang_v = 0.0;
											period_u = 0.0;
											period_v = 0.0;
										}
									}
								}
/*
.....Create the uv composite curve
*/
								if (counter2>1)
									uig_create_comp(dblk,&newkeycv,counter2,uvcomp,
										UU_TRUE, NULL, &keyuv);
								if (counter2!=0)
									big_uv_comp[j] =keyuv;
							}
						}
					}

/*
.....This curve is in normal x,y,z space, for now it is being saved
.....in trimkeys, but it isn't really needed in there, for it gets skipped
.....over in the display routine.  What is needed is this curve in
.....uv space. So create it.
*/
					if(pblk->type==0)
					{
						if (numcrvs>1)
							uig_create_comp(dblk,&newkeycv,numcrvs,new_comp,
								UU_TRUE, sense, ikeyp);
						else
							*ikeyp = keycv;
						newkeycv.key=*ikeyp;
						trimkeys[counter]=*ikeyp;
						counter=counter+1;
						ur_retrieve_data(&newkeycv,sizeof(struct UM_compcrv_rec));
/*
.....Convert the curve into a uv space curve, this curve is the
.....important one when it comes to the display routine.
*/
						uig_evcrv_to_uvcv(&newkeycv,&trimsrf,ikeyp,uv,tol);
					}
/*
.....As mentioned up above, if type is equal to 1, then the uv 
.....parameter curves were given and just need to put them into
.....one curve
*/
					else
						uig_create_comp(dblk,&newkeycv,numcrvs,
											big_uv_comp,UU_TRUE, NULL, ikeyp);
					trimkeys[counter]=*ikeyp;
					status = uig_check_uv (uv,*ikeyp);
					counter=counter+1;
				}
/*
......Assign to ikeyp the elements of trimkeys.  Since
......only every other element is basically junk, just assign
......zero to it. The order of the elements in ikeyp should be
......the reverse of trimkeys (in groups of two) Not exactly
......sure why, but the display of the entity is much cleaner
......this way.
*/
				for(i=0;i<counter;i++)
				{
					*ikeyp=0;
					ikeyp++;
					*ikeyp=trimkeys[counter-(1+i)];
					ikeyp++;
				}
/*
.....If there are inner boundaries to the trimsrf, update the trimsrf
.....with the boundaries.
*/
				if (num>0)
				{
					ur_update_data_varlist(trimsrf.key,1,ikeys,1,num*2);
				}
			}
/*
.....outer boundary bad
.....or inner boundary bad
*/
        	if (status != 0 || statin != 0)
        	{
               struct NCL_fixed_databag *bs;
           	   DDC_displayable_flag = isvdsp;
           	   label_comp_element = lsvlab;
           	   bs = (struct NCL_fixed_databag *) c;
           	   bs->key = keysf;
           	   ncl_retrieve_data_fixed (bs);
					um_save_active_label(NCL_TRIMSF_REL);
           	   create_label(dblk, pblk->no_prop, pblk->prop, clabel, &bs->subscr);
           	   strcpy(bs->label, clabel);
           	   i = 8;
           	   ul_strip_blanks (clabel,&i);
/*
..... Print error messages.
*/
               sprintf (p_buff,
               "(DREC = %d) UV cv for trimmed surf %s is incorrect. Imported as untrimmed.\n",
                    dblk->drec_num, clabel);
               uig_error (p_buff);
               if( statin == -1)
                  sprintf (p_buff,
              	    "            Can't read inner boundary curve info.\n");
               if( status == -1)
                  sprintf (p_buff,
              	    "            Coulcn't map boundary surface.\n");
               if( status == 1)
                  sprintf (p_buff,
              	    "            Couldn't get data for outer curves or any parametric curves.\n");
           	   uig_error (p_buff);

           	   ur_update_data_fixed(bs);
           	   ur_update_displayable(keysf, DDC_displayable_flag);
           	   uig_transf_entity (bs, dblk);
           	   *key = keysf;
/*
.....merge labels of trimming curves with surface name
*/
               for (i=0; i<bcnt; i++)
               {
                  strcpy (mlabel,clabel);
                  bs->key = keybl[i];
                  ncl_retrieve_data_fixed (bs);
                  strcat (mlabel,"_");
                  strcat (mlabel,bs->label);
                  strcpy (bs->label,mlabel);
                  ur_update_data_fixed(bs);
               }
			}
			trimsrf.key = *key;
			if (UIG_nodups && !label_comp_element &&
				ncl_retrieve_data_fixed(&trimsrf) == UU_SUCCESS &&
				uig_match_trimsrf(&trimsrf,0) == UU_SUCCESS)
			{
				uig_remove_dup(&trimsrf,key);
			}
			else
			{
				if(label_type == 8)
				{
/*
.....Label matching. Determine if exact match.
*/
					uig_exact_match(&trimsrf,uig_match_trimsrf);
				}
			}
		}
	}
done:;

  	DDC_displayable_flag = isvdsp;
  	label_comp_element = lsvlab;
  	if (c != 0) uu_toolfree(c);
  	if (ikeys != 0) uu_toolfree(ikeys);
  	if (trimkeys) uu_toolfree(trimkeys);
  	if (keybl != 0) uu_toolfree(keybl);
	if (uvcomp) uu_toolfree(uvcomp);
	if (new_comp) uu_toolfree(new_comp);
	if (big_uv_comp) uu_toolfree(big_uv_comp);
	if (sense) uu_toolfree(sense);
	for (i = 0; i<numhld; i++)
  		if (newbndy.crvptr[i].cid) uu_toolfree(newbndy.crvptr[i].cid);
  	if (newbndy.crvptr) uu_toolfree(newbndy.crvptr);
  	return (return_status);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_solid(gblk,dblk,pblk,key)
**          Translates a Solid entity,this function does not perform any tasks.
**    PARAMETERS
**       INPUT  :
**          gblk                    global block
**          dblk                    directory block
**          pblk                    parameter block
**				key							key id
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_solid(gblk,dblk,pblk,key)
struct global_rec *gblk;            /* global record sturcture */
struct dir_rec *dblk;               /* directory record */
struct IG_igesolid_rec *pblk;       /* iges solid parameter record */
UU_KEY_ID *key;
{
   return(0);
}	

/*********************************************************************
**		I_FUNCTION     :  uig_in_verlist(gblk,dblk,pblk,key)
**          Translate a Vertex List entity.This function performs no function.
**    PARAMETERS
**       INPUT  :
**          gblk                    global block
**          dblk                    directory block
**          pblk                    parameter block
**				key							key id
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_verlist(gblk,dblk,pblk,key)
struct global_rec *gblk;            /* global record sturcture */
struct dir_rec *dblk;               /* directory record */
struct IG_igesvlst_rec *pblk;       /* iges vertex list parameter record */
UU_KEY_ID *key;
{
   return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_edglist(gblk,dblk,pblk,key)
**          Translate an Edge List entity.This function does not perform any
**				task
**    PARAMETERS
**       INPUT  :
**          gblk                    global block
**          dblk                    directory block
**          pblk                    parameter block
**				key							key id
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_edglist(gblk,dblk,pblk,key)
struct global_rec *gblk;            /* global record sturcture */
struct dir_rec *dblk;               /* directory record */
struct IG_igeselst_rec *pblk;       /* iges edge list parameter record */
UU_KEY_ID *key;
{
   return (0);
}

/*********************************************************************
**    I_FUNCTION     : uig_in_loop(gblk,dblk,pblk,keyuvcv,uv,tol,uvcvflag)
**          Translate the parameteric and the spatial curves of the loop entity
**    PARAMETERS
**       INPUT  :
**          gblk                    global block
**          dblk                    directory block
**          pblk                    parameter block
**			keyuvcv					key id of the composite parametric/spaltial curve
**          uv[6]					min, max, and periods of u and v
**          tol						tolerance
**			uvcvflag				0: spaltial curve 1:parametric curve
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_loop(gblk,dblk,pblk,keyuvcv,uv,tol,uvcvflag)
struct global_rec *gblk;            /* global record sturcture */
struct dir_rec *dblk;               /* directory record */
struct IG_igesloop_rec *pblk;       /* iges loop parameter record */
UU_KEY_ID *keyuvcv;
UU_REAL tol,uv[6];
int uvcvflag;
{
	UU_KEY_ID lkeyuv,lkeycv, e_keyuv =0,e_keycv=0 ,*crvlistuv = NULL, *crvlistcv = NULL;
	UU_KEY_ID ikey;
	struct IG_edge_rec *z;
	struct dir_rec ldir;
	int crv_on_srf_filtered,no_cv = 0 ,no_uv = 0,index1,i,irec,status,num, numpar,isvdsp;
	int inc;
	UU_LOGICAL lsvlab;
	char p_buff[80], *c;
	struct IG_sp_crv_rec modelcrv;
	struct IG_igeselst_rec *edgelist;
	struct IG_par_sp_rec *parsp;
	UU_LOGICAL savtrans,sav_draw,hldtrans;
	UU_REAL save_unit_scale;
	int  *ireclistcv = NULL;
	save_unit_scale = unit_scale;
	c = UU_NULL;
	c = uu_toolmalloc(MAX_PARA_REC);
	isvdsp = DDC_displayable_flag;
	lsvlab = label_comp_element;
	crv_on_srf_filtered = entity_mask[18];
	DDC_displayable_flag = UM_NEVERDISPLAYABLE;		
	label_comp_element = UU_TRUE;
	z = pblk->edge;
	num = pblk->n;
	ireclistcv = (int *)uu_toolmalloc(num*sizeof(int));
	crvlistuv = (UU_KEY_ID *)uu_toolmalloc(num*sizeof(UU_KEY_ID));
	crvlistcv = (UU_KEY_ID *)uu_toolmalloc(num*sizeof(UU_KEY_ID));
	inc = 0;
	for(i=0;i<num;i++)
	{	
/*
.....execute this edge use only if it is spatial curve is an entry in the edge
..... list
*/
		if(z[i].type == 0)
		{
			if(!uvcvflag)
			{
				irec = z[i].list;
				index1 = z[i].index;
				status = uig_get_data(irec,0,&ldir,c);
				if(status == 0)
				{
/*
.....Extract the spatial xyz curve from the edge list and translate it.
*/
					edgelist = (struct IG_igeselst_rec *)c; 
					modelcrv = edgelist->sp_crv[index1-1];
					irec = modelcrv.crv;
					ireclistcv[i] = irec;
					lkeycv=0;
					status =  uig_get_data(irec,0,&ldir,c);
					if(status == 0)
					{
						init_label(ldir.rel_type, irec);	
						check_color(&ldir);		
						current_dir_number = irec;
						if( uig_check_range(&ldir))
							uig_in_dispat(gblk,&ldir,c,&lkeycv);
						update_counts(lkeycv, irec);	
					}
				}
				if(lkeycv==0) no_cv =1;
				crvlistcv[inc] = lkeycv;	
			}
/*
.....Extract the parametric uv curve and translate it, 
..... only if # of parametric curves is not 0
*/
			if(uvcvflag)
			{
				numpar = z[i].num_par;
				if(numpar)
				{
					parsp = z[i].par_sp;
					irec = parsp[0].par_crv;
					lkeyuv = 0;
					status =  uig_get_data(irec,0,&ldir,c);
					if(status == 0)
					{
						init_label(ldir.rel_type, irec);
						check_color(&ldir);
						current_dir_number = irec;
						unit_scale = 1.0;
	/*
	..... Save the drawing flag and then pretend that there is no drawing.
	..... This allows given uv-curve to be translated correctly, even if
	..... it is a part of a drawing.
	*/
						savtrans=instance_trans;
						instance_trans=UU_FALSE;
						sav_draw = drw_flag;
						drw_flag = 0;
	/*
	..... bump_u is 'true' means u-coordinates are angles, and we translate angles
	..... as positive
	*/
						bump_u = UU_TRUE; 
						bump_v = UU_TRUE;
						starting_ang_u = uv[0];
						starting_ang_v = uv[2];
						period_u = uv[4];
						period_v = uv[5];
						hldtrans = IG_trans;
						IG_trans = UU_TRUE;
						if( uig_check_range(&ldir))
							uig_in_dispat(gblk,&ldir,c,&lkeyuv);
						IG_trans = hldtrans;
						bump_u = UU_FALSE;
						bump_v = UU_FALSE;
						starting_ang_u = 0.0;
						starting_ang_v = 0.0;
						period_u = 0.0;
						period_v = 0.0;
						drw_flag = sav_draw;
						instance_trans=savtrans;
						unit_scale = save_unit_scale;
						update_counts(lkeyuv, irec);
						if (lkeyuv == 0) 
						{
							sprintf(p_buff, "(DREC = %d) Part of loop not translated; type=%d\n",
							irec, ldir.rel_type);
							uig_error (p_buff);
						}
					} 
				}
				else
				{
					lkeyuv = 0;
					if(!no_uv) no_uv =1;
					if (lkeycv == 0) 
						{
							sprintf(p_buff, "(DREC = %d) Part of loop not translated; type=%d\n",
							irec, ldir.rel_type);
							uig_error (p_buff);
						}
				}
				crvlistuv[inc] = lkeyuv;
			}
			inc++;
		}		
	}	

	label_comp_element = lsvlab;
	DDC_displayable_flag = isvdsp;
	uig_update_attr(dblk);
	
/*
.....create composite curves from the individual curves of the loop
*/
	if(!uvcvflag)
	{
		if((crv_on_srf_filtered && UIG_splitccrv==1)||!crv_on_srf_filtered)
		{
			DDC_displayable_flag = UM_NEVERDISPLAYABLE;		
			label_comp_element = UU_TRUE;
		}
		if(!no_cv)
			uig_create_comp(dblk,pblk,inc,crvlistcv,UU_FALSE,NULL,&e_keycv);
		*keyuvcv = e_keycv;
	}
	if(uvcvflag)
	{
		if(!no_uv)
			uig_create_comp(dblk,pblk,inc,crvlistuv,UU_FALSE,NULL,&e_keyuv);
		*keyuvcv = e_keyuv;
	}
	label_comp_element = lsvlab;
	DDC_displayable_flag = isvdsp;
/*
.....Translate the individual components of the loop if the filter is on
*/
	if(!uvcvflag&&crv_on_srf_filtered && UIG_splitccrv!=0)
	{
		DDC_displayable_flag = UM_DISPLAYABLE;		
		label_comp_element = UU_FALSE;
		for(i=0;i<num;i++) /* Translate individual elements */
		{
			irec = ireclistcv[i];
			if (uig_get_data(irec,0,&ldir,c) == 0)
			{
				init_label(ldir.rel_type, irec);
				check_color(&ldir);
				current_dir_number = irec;
				uig_in_dispat(gblk,&ldir,c,&ikey);
			}
		}
	}
	if (ireclistcv) uu_toolfree(ireclistcv);
	if (crvlistuv) uu_toolfree(crvlistuv);
	if (crvlistcv) uu_toolfree(crvlistcv);
	if (c != 0) uu_toolfree(c);
	label_comp_element = lsvlab;
	DDC_displayable_flag = isvdsp;
	return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_face(gblk,dblk,pblk,key)
**          Translate a face entity.
**    PARAMETERS
**       INPUT  :
**          gblk                    global block
**          dblk                    directory block
**          pblk                    parameter block
**				key							key id
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_face(gblk,dblk,pblk,key)
struct global_rec *gblk;            /* global record sturcture */
struct dir_rec *dblk;               /* directory record */
struct IG_igesface_rec *pblk;       /* iges face parameter record */
UU_KEY_ID *key;
{
	UU_KEY_ID *ikeys,*ikeyp,ikeyuv,ikeycv,keyuv,keycv ,keysf;
	UU_KEY_ID z,*z1,inner_keyuv, inner_keycv;
	struct dir_rec ldir;
	UU_REAL *t,tol,uv[6];
	int sftyp,form,i,irec,isvdsp,status,statin,num;
	char clabel[64],*c,p_buff[100];
	struct IG_igesloop_rec  *loop;
	UU_LOGICAL store=UU_TRUE,lsvlab;
	struct NCL_trimsf_rec eptr;
	UM_transf tfmat;
	int crv_on_srf_filtered,stattf,ikeyerr =0 ;
/*
.....Here the iges face is broken down into individual components to correspond
..... to the iges trimed surface components, so as to translate the iges face.
*/
		um_evlset();
	UIG_from_trimsrf = 1;
	status = 0;
	posangle = UU_FALSE;
	*key   = keyuv = keycv= 0;
	if (gblk->units==1) tol = 0.0002;
	else tol = 0.005;
	crv_on_srf_filtered = entity_mask[18];
	isvdsp = DDC_displayable_flag;
	lsvlab = label_comp_element;

	DDC_displayable_flag = UM_NEVERDISPLAYABLE;
	label_comp_element = UU_TRUE;
	c = UU_NULL;
	c = uu_toolmalloc(MAX_PARA_REC);
	
   z = pblk->srf;
	irec=z;
	keysf = 0;
	ikeys = 0;
	status = uig_get_data(irec,0,&ldir,c);
	if(status == 0)
	{
		form  = ldir.form_no;
		sftyp = ldir.rel_type;
	  	init_label(ldir.rel_type, irec);	
		check_color(&ldir);		
	  	current_dir_number = irec;
/*
..... the base surface could be any kind of surface, not just spline surface
*/
	  	uig_in_dispat(gblk,&ldir,c,&keysf);
		if(keysf<1)
		{
			sprintf (p_buff, "(DREC = %d) Base surface of trimmed surface failed to translate.\n",dblk->drec_num);
			uig_error(p_buff);
		}
		update_counts(keysf, irec);
		*key = keysf;
		uig_load_current_matrix(dblk->matrix_ptr, &t);
		if (((int)keysf > 0))
		{
/*
..... get surface parameters min and max
*/
			status = uig_get_srf_uv (ldir.rel_type, form, c, uv);
			num = (pblk->n);
			z1 = pblk->cid;
/*
.....Translate Outermost curve. 
.....the first loop is assumed as the outermost curve of the 
.....iges face. IGNORE THE OUTER LOOP FLAG

			if(pblk->out)
*/
			irec = z1[0];
			status = uig_get_data(irec,0,&ldir,c);
			if(status == 0)
			{
				loop = (struct IG_igesloop_rec *)c;
				init_label(ldir.rel_type, irec);
				check_color(&ldir);
				current_dir_number = irec;
				if (crv_on_srf_filtered)
				{
					label_comp_element = UU_FALSE;
					DDC_displayable_flag = UM_DISPLAYABLE;
				}
/*
......Translate the Saptial curves of the loop
*/
				uig_in_loop(gblk,&ldir,(struct IG_igesloop_rec *)c,&keycv,uv,tol,0);
				update_counts(keycv, irec);
				DDC_displayable_flag = UM_NEVERDISPLAYABLE;
				label_comp_element = UU_TRUE;
/*
......Translate the Parametric curves of the loop
*/
				uig_in_loop(gblk,&ldir,(struct IG_igesloop_rec *)c,&keyuv,uv,tol,1);
				update_counts(keyuv, irec);
				if (keyuv == 0) 
					status = -2;
				
			}
			if ((num=pblk->n-1) > 0)
				ikeys = (UU_KEY_ID *)uu_toolmalloc(2*(num)*sizeof(UU_KEY_ID));
			ikeyp = ikeys;
/*
.....Translate inner boundary.
*/
			num=pblk->n;
			ikeyerr =0;
			statin = 0;
			for(i=1;i<=num-1;i++)
			{
				irec = z1[i];
				statin = uig_get_data(irec,0,&ldir,c);
				if(statin == 0)
				{
					init_label(ldir.rel_type, irec);
					check_color(&ldir);
					current_dir_number = irec;
					*ikeyp = 0;
					ikeyuv=ikeycv=0;
					if (crv_on_srf_filtered)
					{
						label_comp_element = UU_FALSE;
						DDC_displayable_flag = UM_DISPLAYABLE;
					}
/*
......Translate the Spatial curves of the inner loop
*/
					uig_in_loop(gblk,&ldir,(struct IG_igesloop_rec *)c,&ikeycv,uv,tol,0);
					DDC_displayable_flag = UM_NEVERDISPLAYABLE;
					label_comp_element = UU_TRUE;
					/*
......Translate the Parametric curves of the inner loop
*/
					uig_in_loop(gblk,&ldir,(struct IG_igesloop_rec *)c,&ikeyuv,uv,tol,1);
					if (ikeyuv == 0 && ikeycv == 0) 
					{
						sprintf(p_buff, "(DREC = %d) Inner loop not translated; type=%d\n",
						irec, ldir.rel_type);
						uig_error (p_buff);
						ikeyerr ++;
						continue;
					}
					*ikeyp = ikeycv;
					ikeyp++;
					*ikeyp = ikeyuv;
					if (*ikeyp == 0) statin = -2;
					else
						statin = uig_check_uv (uv,*ikeyp);
					ikeyp++;
				}
				else if(statin == -2)
				{
					*ikeyp = 0;
					ikeyp ++;
					*ikeyp = 0;
					ikeyp++;
				}			
			}
/*
..... build uv trimming curve based on cv boundary 
..... check if uv-curves were created.
*/
			if (status == -2 || statin == -2)
			{
				if (status == -2)
				{
					if (keycv > 1)
					{
						UIG_change_uv = 0;
						status = uig_evcrv_to_uvcv(&keycv,&keysf,&keyuv,uv,tol);
						if ((int)keyuv <= 0) status = -1;
						if (status == 0)
							status = uig_check_uv(uv,keyuv);
/*
..... If the outer boundary curve does not lie within the uv boundary
..... of a surface of revolution then we shift the start and end angle 
..... of revolution.
*/  					
						if (status != 0 && sftyp == GREVSRF)
						{
							UIG_change_uv = 1;
							status =uig_evcrv_to_uvcv(&keycv,&keysf,&keyuv,uv,tol);
							if ((int)keyuv <= 0) status = -1;
							if (status == 0)
							{ 
  				 				status = uig_check_uv(uv,keyuv);
							}
						}
					}

					if(!keycv && !keyuv)
					{
						sprintf (p_buff,
        "            Parametric and spatial outer trimming curves bad!\n");
						uig_error (p_buff);
                    }
				}
				if (status == 0 && statin == -2)
				{
					for(i=1;i<=num-1;i++)
					{
						inner_keycv = ikeys[2*i-2];
						if(inner_keycv)
						{
						statin = uig_evcrv_to_uvcv(&inner_keycv, &keysf, &inner_keyuv,uv,tol);
						if (statin != 0)
						{
							UIG_inner_error = 1;
							num = i-1;
							if (num > 0) statin = 0;
							break;
						}
						ikeys[2*i-1] = inner_keyuv;
						}
					}
				}
			}
			if(status ==0)
			{
/*
..... map trimsf only if there a valid outer boundary
*/
				DDC_displayable_flag = isvdsp;
				label_comp_element = lsvlab;
				if (statin != 0)  num = 0;
				uig_map_trimsrf(dblk,pblk,t,keysf,keyuv,keycv,uv,num-1,ikeys,key);
			}
			else
			{
				struct NCL_fixed_databag *bs; 
				DDC_displayable_flag = isvdsp;
				label_comp_element = lsvlab;
				bs = (struct NCL_fixed_databag *) c;
				bs->key = keysf;
				ncl_retrieve_data_fixed (bs);
				stattf = UU_SUCCESS;
				uig_igtf_to_tf(t, tfmat);
				if (!um_is_idmat(tfmat)) stattf = uig_transform(bs,tfmat,store);
				um_save_active_label(NCL_TRIMSF_REL);
				create_label(dblk, 0, 0, clabel, &bs->subscr);
				strcpy(bs->label, clabel);
				i = 8;
				ul_strip_blanks (clabel,&i);
				sprintf (p_buff,
		"(DREC = %d) Trimmed surf %s has a boundary problem. Imported as untrimmed.\n",
						dblk->drec_num, clabel);
				uig_error (p_buff);
				if (keyuv < 1)
				{
					sprintf (p_buff,
			"            Failed to project outer, spatial boundary curve onto surface.\n");
					uig_error (p_buff);
				}
				if (status == -2)
				{
					sprintf (p_buff,
			"            Outer, parametric curve went outside of surface u,v boundaries.\n");
					uig_error (p_buff);
				}
				if (stattf != UU_SUCCESS)
				{
					sprintf (p_buff,
			"            Base surf could not be transformed.\n");
					uig_error (p_buff);
				}
				ur_update_data_fixed(bs);
				ur_update_displayable(keysf, DDC_displayable_flag);
				uig_transf_entity (bs, dblk);
				*key = keysf;
			}
			eptr.key = *key;
			if (UIG_nodups && !label_comp_element &&
				ncl_retrieve_data_fixed(&eptr) == UU_SUCCESS &&
				uig_match_surface(&eptr,0) == UU_SUCCESS)
			{
				uig_remove_dup(&eptr,key);
			}
		}	
	}
Done:;
DDC_displayable_flag = isvdsp;
label_comp_element = lsvlab;
if (c != 0) uu_toolfree(c);
if (ikeys != 0) uu_toolfree(ikeys); 
uig_pop_current_matrix();
UIG_from_trimsrf = 0;
um_evlrst();
return (0);
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_shell(gblk,dblk,pblk,key)
**          Translate a shell entity.
**    PARAMETERS
**       INPUT  :
**          gblk                    global block
**          dblk                    directory block
**          pblk                    parameter block
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_shell(gblk,dblk,pblk,key)
struct global_rec *gblk;            /* global record sturcture */
struct dir_rec *dblk;               /* directory record */
struct IG_igeshell_rec *pblk;       /* iges shell parameter record */
UU_KEY_ID *key;
{
	return (0);
}
static int IsColorNameExist(name)
char *name;
{
	int i;
	for (i=0; i<UIG_MAXCOLOR;i++)
	{
		if (stricmp(uw_color_name[i], name)==0)
			return 1;
	}
	return 0;
}

/*********************************************************************
**    I_FUNCTION     :  uig_in_color(gblk,dblk,pblk,key,set_color)
**          Translate a color entity.
**    PARAMETERS
**       INPUT  :
**          pblk                    parameter block
**			indx: custom color index
**       OUTPUT :
**          set_color			returned color
**			indx: next custom color index
**    RETURNS      : 0 if this is a new color and is added to the
**                   color table.  -1 if this is an existing color.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uig_in_color(pblk, indx, icol)
struct IG_igesclr_rec *pblk;
int *indx, *icol;
{
	int i,j,len,tcol;
	double dx,dy,dz,d,dsav;	
	char tmpstr[96], numstr[40];
	int cus_colors;
/*
.....If system only supports standard colors
.....then find closest match
*/
	if (UIG_MAXCOLOR == UIG_STDCOLOR)
	{
   		tcol = 15;
   		dsav = 1000.;
   		for (i=0;i<UIG_MAXCOLOR;i++)
   		{
      		dx = (pblk->red*2.55)-uw_color_table[i][0];
      		dy = (pblk->green*2.55)-uw_color_table[i][1];
      		dz = (pblk->blue*2.55)-uw_color_table[i][2];
      		d = dx*dx + dy*dy + dz*dz;
      		if (d < dsav)
      		{
         		dsav = d;
         		tcol = i;
      		}
   		}
		*icol = tcol;
		*indx = UIG_MAXCOLOR;
		return (0);
	}
/*
.....pblk->name is in the form of '5HBLACK', need remove the 5H
*/
	strcpy(tmpstr, pblk->name);
	len = strlen(tmpstr);
	for (j=0; j<len; j++)
	{
		if (tmpstr[j] == 'H') 
		{
			strcpy(pblk->name, &(tmpstr[j+1]));
			break;
		}
	}	
/*
......see if this color is the standard color, if yes, don't save
......into custom color
*/
	for (i=0;(i<*indx+UIG_STDCOLOR)&&(i<UIG_MAXCOLOR);i++)
	{
		dx = pblk->red*2.55 - 1.0*uw_color_table[i][0];
		dy = pblk->green*2.55 - 1.0*uw_color_table[i][1];
		dz = pblk->blue*2.55 - 1.0*uw_color_table[i][2];
		if ((abs(dx)<=1.0)&&(abs(dy)<=1.0)&&(abs(dz)<=1.0))
		{
			*icol = i;
			return (-1);
		}
	}
/*
......if the custom color index is excess maxinum colors,
......try to find the close match from the current color table
*/
	cus_colors = UIG_MAXCOLOR - UIG_STDCOLOR;
	if (*indx>=cus_colors)
	{
		*icol = -1;
		dsav = 1000.;
		for (i=0;i<UIG_MAXCOLOR;i++)
		{
			dx = (pblk->red*2.55 - 1.0*uw_color_table[i][0])/255.0;
			dy = (pblk->green*2.55 - 1.0*uw_color_table[i][1])/255.0;
			dz = (pblk->blue*2.55 - 1.0*uw_color_table[i][2])/255.0;
			d = dx*dx + dy*dy + dz*dz;
			if (d < dsav)
			{
				dsav = d;
				*icol = i;
			}
		}
		return -1;
	}
	if (pblk->name[0]!='\0')
	{
/*
......if there is a name there, check if this name is exist,
......if not, copy this name and save it, if they do, use
......IGES_COLOR_indxnum
*/
		if (IsColorNameExist(pblk->name)==0)
			strcpy(uw_color_name[*indx+UIG_STDCOLOR], pblk->name);
		else
			sprintf(uw_color_name[*indx+UIG_STDCOLOR], "IGES_COLOR_%d", *indx+1);
	}
	else
		sprintf(uw_color_name[*indx+UIG_STDCOLOR], "IGES_COLOR_%d", *indx+1);
	uw_color_table[*indx+UIG_STDCOLOR][0] = pblk->red*2.55 + 0.5;
	uw_color_table[*indx+UIG_STDCOLOR][1] = pblk->green*2.55 + 0.5;
	uw_color_table[*indx+UIG_STDCOLOR][2] = pblk->blue*2.55 + 0.5;
	*icol = *indx+UIG_STDCOLOR;
	*indx = *indx + 1;
	return (0);
}

/*****************************************************************
**
**     FUNCTION:     uig_copy_bndy
**
**     PURPOSE:      To copy one IG_igesbndy_rec into another.
**
*******************************************************************/

int uig_copy_bndy(old, new)
struct IG_igesbndy_rec *old;
struct IG_igesbndy_rec *new;
{
	int i,j;

	new->key = old->key;
	new->rel_num = old->rel_num;
	new->type = old->type;
	new->pref = old->pref;
	new->srf = old->srf;
	new->no_crvptr = old->no_crvptr;
	for (i=0; i<old->no_crvptr; i++)
	{
		new->crvptr[i].crvptr = old->crvptr[i].crvptr;
		new->crvptr[i].sense = old->crvptr[i].sense;
		new->crvptr[i].no_cid = old->crvptr[i].no_cid;
		for (j=0;j<old->crvptr[i].no_cid;j++)
		{
			new->crvptr[i].cid[j] = old->crvptr[i].cid[j];
		}
	}
	return (0);
}

/*********************************************************************
**    I_FUNCTION     : int uig_load_current_matrix(mptr)
**       Set the transformation matrix for an entity by combining
**       the matrix for this entity with the matrix currently in
**       effect.
**    PARAMETERS
**       INPUT  :
**          mptr     IGES directory pointer to entity transform.
**       OUTPUT :
**          t      - pointer to combined transformation.
**    RETURNS      : UU_SUCCESS if no error, otherwise UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_load_current_matrix(mptr,t)
int mptr;
UU_REAL **t;
{
	int j;
	UU_REAL t1[12],  *tp1, *tp2, *tpe;
	UM_transf tf1, tf2, tf3;
/*
.....Initialize the matrix list.
*/
	if (!IG_matrix_list_ptr)
	{
		IG_matrix_list_ptr = (UU_REAL *)uu_lsnew();
		if (!IG_matrix_list_ptr)
		{
			*t = IG_idmatrix;
			return(UU_FAILURE);
		}
	}

/*
.....Get the matrix for this entity.
*/
	if (mptr)
	{
		uig_get_trans(mptr,t1);
		tp1 = t1;
	}
	else
		tp1 = IG_idmatrix;
/*
.....If a matrix was already in effect, combine it with the
.....current matrix.
*/
	tpe = (UU_REAL *)uu_lsend(IG_matrix_list_ptr);
	if (!tpe)
	{
		tpe = IG_matrix_list_ptr;
	}
	else if (IG_trans)
	{
		uig_igtf_to_tf(tp1, tf1);
		uig_igtf_to_tf(tpe, tf2);
		um_tftmtf(tf1, tf2, tf3);
		uig_tf_to_igtf(tf3, t1);
		tp1 = t1;
	}
/*
.....Push the matrix onto the list and return a pointer to it.
*/
	tp2 = (UU_REAL *)uu_lsinsrt((char *)tpe,sizeof(UU_REAL)*12);
	if (!tp2)
	{
		*t = IG_idmatrix;
		return(UU_FAILURE);
	}
	for (j=0;j<12;j++) tp2[j] = tp1[j];
	*t = tp2;

	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : int uig_pop_current_matrix()
**       Pop a matrix off the matrix list.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_pop_current_matrix()
{
	UU_REAL *tpe;

	if (IG_matrix_list_ptr)
	{
		tpe = (UU_REAL *)uu_lsend(IG_matrix_list_ptr);
		if (tpe) uu_lsdele(tpe);
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : int uig_check_current_matrix_list(dblk)
**       If number of matrices in the matrix list is not zero,
**       empty it and give a warning.
**    PARAMETERS
**       INPUT  :
**          dblk     - Current directory block.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_check_current_matrix_list(dblk)
struct dir_rec *dblk;
{
	char p_buff[80];

	if (IG_matrix_list_ptr && uu_lsnext(IG_matrix_list_ptr))
	{
		sprintf (p_buff,
			"(DREC = %d) System warning - Matrix list not empty.\n",
			dblk->drec_num);
		uig_error (p_buff);
		uu_lsempty(IG_matrix_list_ptr);
	}

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : int uig_shift_plane(e1ptr, e2ptr)
**************** NOT USED **************
**       Shift the base plane to the level of the boundary curve, if the
**			curve is planar and its normal lies on the normal of the plane.
**    PARAMETERS
**       INPUT  :
**          e1ptr    - Pointer to plane.
**          e2ptr    - Pointer to xyz curve.
**       OUTPUT :
**				none
**    RETURNS      :
**       UU_SUCCESS iff no error, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uig_shift_plane(e1ptr, e2ptr)
struct NCL_fixed_databag *e1ptr, *e2ptr;
{
	int i,status,npts;
   UU_REAL plane[4];
   UM_coord ptnew, *p1;
   UM_vector xaxis,yaxis;
   UU_LIST ptlist;
   struct NCL_nclpl_rec *plptr;
   UM_transf tfmat;
	UM_real8 d1, disp,diff,tol;

	status = UU_SUCCESS;
	gettol(&tol);
   uu_list_init (&ptlist, sizeof(UM_coord), 200, 200);
/*
.....Evolve points on curve.
*/
   status = uc_retrieve_transf(e2ptr->key, tfmat);
   if (status == UU_SUCCESS)
   {
     	npts = ncl_evolve_curve (e2ptr,tfmat,tol,&ptlist,UU_NULL,UU_NULL,0);
     	if (npts < 4) status = UU_FAILURE;
   }
   if (status == UU_SUCCESS)
   {
     	p1 = (UM_coord *)UU_LIST_ARRAY(&ptlist);
     	plptr = (struct NCL_nclpl_rec *)e1ptr;
/*
.....Determine if the boundary curve is planar.
*/
		if(um_planar_curve(p1,npts,plane,xaxis,yaxis))
		{
/*
.....DEtermine if the plane normal and the normal to the plane on which 
.....the curve lies are parallel.
*/
			d1 = UM_DOT(plane,plptr->nvec);
			if ( (1.0 - fabs(d1)) < UM_DFUZZ) 
			{
				disp = UM_DOT(plptr->pt,plptr->nvec);		
				diff = disp - plane[3];
				if (d1 > 0) diff = -1.0 * diff;
/*
..... if they do not lie on the same level translate the plane along the normal
..... to the level on the curve.
*/
				if ((fabs(diff)) > UM_DFUZZ)
				{
					um_translate_point(plptr->pt,diff,plptr->nvec,ptnew);
					for(i=0;i<3;i++)plptr->pt[i]=ptnew[i];
				}
			}
		}
   }
	return (status);
}

/*********************************************************************
**    I_FUNCTION     : int uig_check_offset_dist(dblk)
**     Check to make sure the sign of the offset distance is correct
**     for the base surface of a trimmed surface. The offset depends
**     on the distance and the direction vector. In uig_map_offsetsrf
**     the uv parameters are set to 0.5, which may not be in the
**     boundary of the trimed surface, when the normal vector is
**     found. This will cause a problem if the normal vector at 
**     (0.5,0.5) is not pointing in the same direction as the normal
**     vector for the trimmed surface at its center point. This
**     routine checks to make sure the normal vector used to check
**     the sign of the offset distance was appropriate by finding
**     the normal vector of the trimmed surface in the center of its
**     boundary and setting if the distance if it should not have
**     been changed.
**
**    PARAMETERS
**       INPUT  :
**          eptr - trim surface entity
**          bkey - base surface key
**       OUTPUT :
**          none
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uig_check_offset_dist(eptr,bkey)
struct NCL_trimsf_rec *eptr;
UU_KEY_ID bkey;
{
	int status;
	UM_srf_boundary b;
	UU_REAL u0,u1,v0,v1;
   struct NCL_fixed_databag srf;
   struct UM_evsrfout evsrf, evsrf2;
   UM_param u, v, uchk, vchk;
   UM_transf tfmat;
	UU_LOGICAL changedis;
	UU_REAL dis;
	
	changedis = UU_FALSE;
	status = ncl_get_boundary (UV_BOX_LIST,eptr,&b);
	if (status == UU_SUCCESS)
	{
		u0 = b.ummx[0][0]; u1 = b.ummx[0][1];
		v0 = b.vmmx[0][0]; v1 = b.vmmx[0][1];
		um_free_boundary (&b);
		srf.key = bkey;
		status = ncl_retrieve_data_fixed (&srf);
/*
.....Get the distance currently set for the base surface
*/
		if (srf.rel_num == NCL_MESHSURF_REL)
			dis = ((struct NCL_meshsf_rec *)&srf)->offdist;
		else if (srf.rel_num == UM_RBSPLSRF_REL)
			dis = ((struct UM_rbsplsrf_rec *)&srf)->offdist;
		else
			return(UU_FAILURE);
/*
.....The normal vector for the trimmed surface at the center of the
.....trimmed surface will be an approximate match to the direction
.....vector for the offset distance
*/
		if (status == UU_SUCCESS) 
		{ 
			uc_init_evsrfout (&srf, &evsrf);
			status = uc_retrieve_transf(srf.key,tfmat);
			u = (u0 + u1)/2.;
			v = (v0 + v1)/2.;
			status = uc_evsrf(UM_NORM, u, v, &srf, tfmat, &evsrf);
		}
		if (status == UU_SUCCESS) 
		{ 
			uc_init_evsrfout (&srf, &evsrf2);
			u = .5;
			v = .5;
			status = uc_evsrf(UM_NORM, u, v, &srf, tfmat, &evsrf2);
		}
/*
.....If the two vectors have a negative angle between them, then the
.....sign was changed for the distance in uig_map_offsetsrf and so
.....the sign needs to be set back to the original way. This is
.....handled in the dot product check in uig_set_offset_dist
*/
		if (status == UU_SUCCESS) 
		{
			status = uig_set_offset_dist(&srf,evsrf.snorm,evsrf2.snorm,dis,
				UU_FALSE,tfmat);
		}
	}
	return (status);
}
