/*********************************************************************
**
**    NAME         :  cuinit.c
**
**       CONTAINS:
**					uc_uni_init_all()
**					uc_uni_init_point()
**					uc_uni_init_line()
**					uc_uni_init_circle()
**					uc_uni_init_conic()
**					uc_uni_init_compcrv()
**					uc_uni_init_rspl()
**					uc_uni_init_body()
**					uc_uni_init_txt()
**					uc_uni_init_txtatt()
**					uc_uni_init_tran()
**					uc_uni_init_modatt()
**					uc_uni_init_polygon()
**					uc_uni_init_polyline()
**					uc_uni_init_corsys()
**				 	uc_uni_init_light();
**					uc_uni_init_group()
**					uc_uni_init_draw()
**					uc_uni_init_layer()
**					uc_uni_init_lindim()
**					uc_uni_init_xhatch()
**					uc_uni_init_msym()
**					uc_uni_init_instance()
**					uc_uni_init_attsym()
**					uc_uni_init_connector()
**					uc_uni_init_box()
**					uc_uni_init_attbun()
**					uc_uni_init_view73()
**					uc_uni_init_view74()
**					uc_uni_init_view75()
**					uc_uni_init_calc()
**					uc_uni_init_unistat()
**   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**     NOTE: When changing ddl file by addind new variable list make
**           sure that atom_size and list_size is initialized in ass-
**           otiate routine here AND in nclc/neinit.c file if exists 
**           in both. There is better way (future) to fix it by using 
**           dictionary data.
**   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**     MODULE NAME AND RELEASE LEVEL 
**       cuinit.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:00
**
*********************************************************************/

#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "msol.h"
#include "minitrel.h"
#include "mattr.h"
#include "mxxx.h"
#include "mrenddl.h"
#include "adraft.h"
#include "adrf.h"
#include "bsym.h"
#include "calcom.h"
#include "view.h"
#include "udebug.h"
#include "atext.h"
#include "ncolorddl.h"
#include "riddle.h"

#define UM_MAX_NBR_VAR_LISTS 4

#define VARLISTS 6

#define INITSIZE 31
#define UA_NE_HATCHLINE 20	/* initial # of entries in rel */
#define UA_NV_HATCHLINE 1	/* # of varlists in UA_hatchlin_rec */
#define UA_LS_HATCHLINE 200 /* initial varlist size */

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_all()
**       initialize all relations
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_all()
	{
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_uni_init_all()"));

	uc_uni_init_point();
	uc_uni_init_line();
	uc_uni_init_circle();
	uc_uni_init_conic();
	uc_uni_init_compcrv();
	uc_uni_init_rspl();
	uc_uni_init_cvsf();
	uc_uni_init_agcrv();
	uc_uni_init_agsrf();
	uc_uni_init_agshell();
	uc_uni_init_body();
	uc_uni_init_txt();
	uc_uni_init_txtatt();
	uc_uni_init_tran();
	uc_uni_init_modatt();
	uc_uni_init_polygon();
	uc_uni_init_polyline();
	uc_uni_init_corsys();
 	uc_uni_init_light();
	uc_uni_init_group();
	uc_uni_init_draw();
	uc_uni_init_layer();
	uc_uni_init_lindim();
	uc_uni_init_xhatch();
	uc_uni_init_msym();
	uc_uni_init_instance();
	uc_uni_init_attsym();
	uc_uni_init_connector();
	uc_uni_init_box();
	uc_uni_init_attbun();
	uc_uni_init_view73();
	uc_uni_init_view74();
	uc_uni_init_view75();
	uc_uni_init_calc();
	uc_uni_init_color();
	uc_uni_init_unistat();

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_point()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_point()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];

	uu_denter(UU_MTRC,(us,"uc_uni_init_point()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}
   fixed_size = sizeof(struct UM_point_rec)-sizeof(char)*UM_POINT_BUFSZ;
        atom_size[0] = sizeof(UU_REAL);
        list_size[0] = 10;
	um_init_rel(UM_POINT_REL, "point", 
		UM_NE_POINT, fixed_size, UM_NV_POINT, atom_size, list_size);
	umi_init_rel_label(UM_POINT_REL, "PT", 1);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_line()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_line()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];

	uu_denter(UU_MTRC,(us,"uc_uni_init_line()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}
   fixed_size = sizeof(struct UM_line_rec)-sizeof(char)*UM_LINE_BUFSZ;
        atom_size[0] = sizeof(UU_REAL);
        list_size[0] = 10;
	um_init_rel(UM_LINE_REL, "line", 
		UM_NE_LINE, fixed_size, UM_NV_LINE, atom_size, list_size);
	umi_init_rel_label(UM_LINE_REL, "LN", 1);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_circle()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_circle()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];

	uu_denter(UU_MTRC,(us,"uc_uni_init_circle()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}
   fixed_size = sizeof(struct UM_circle_rec)-sizeof(char)*UM_CIRCLE_BUFSZ;
        atom_size[0] = sizeof(UU_REAL);
        list_size[0] = 10;
	um_init_rel(UM_CIRCLE_REL, "circle",  UM_NE_CIRCLE, 
		fixed_size, UM_NV_CIRCLE, atom_size, list_size);
	umi_init_rel_label(UM_CIRCLE_REL, "CI", 1);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_conic()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_conic()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];

	uu_denter(UU_MTRC,(us,"uc_uni_init_conic()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}
   fixed_size = sizeof(struct UM_conic_rec)-sizeof(char)*UM_CONIC_BUFSZ;
        atom_size[0] = sizeof(UU_REAL);
        list_size[0] = 10;
	um_init_rel(UM_CONIC_REL, "conic", UM_NE_CONIC, 
		fixed_size, UM_NV_CONIC, atom_size, list_size);
	umi_init_rel_label(UM_CONIC_REL, "CN", 1);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_compcrv()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_compcrv()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_compcrv()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	atom_size[0] = sizeof(struct UM_cid_rec);
	fixed_size = sizeof(struct  UM_compcrv_rec) 
					- sizeof(char) * UM_COMPCRV_BUFSZ;
	list_size[0] = 20;
        atom_size[1] = sizeof(UU_REAL);
        list_size[1] = 10;
	um_init_rel(UM_COMPCRV_REL, "compcrv", UM_NE_COMPCRV,
		fixed_size, UM_NV_COMPCRV, atom_size, list_size);
	umi_init_rel_label(UM_COMPCRV_REL, "CV", 1);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_rspl()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_rspl()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_rspl()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	atom_size[0] = sizeof(UU_REAL);
	atom_size[1] = 3 * sizeof(UU_REAL);
	atom_size[2] = sizeof(UU_REAL);
        atom_size[3] = sizeof(UU_REAL);
	list_size[0] = 50;
	list_size[1] = 50;
	list_size[2] = 50;
        list_size[3] = 10;
	fixed_size = sizeof(struct UM_rbsplcrv_rec)
						- sizeof(char) * UM_RBSPLCRV_BUFSZ;
	um_init_rel(UM_RBSPLCRV_REL, "rbsplcrv", 
		UM_NE_RBSPLCRV, fixed_size, UM_NV_RBSPLCRV, 
		atom_size, list_size);
	umi_init_rel_label(UM_RBSPLCRV_REL, "CV", 1);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_cvsf()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_cvsf()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_cvsf()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	atom_size[0] = sizeof(UU_REAL);
	atom_size[1] = 3 * sizeof(UU_REAL);
	atom_size[2] = sizeof(UU_REAL);
        atom_size[3] = sizeof(UU_REAL);
	list_size[0] = 50;
	list_size[1] = 50;
	list_size[2] = 50;
        list_size[3] = 10;
	fixed_size = sizeof(struct UM_uvcvonsf_rec)
						- sizeof(char) * UM_UVCVONSF_BUFSZ;
	um_init_rel(UM_UVCVONSF_REL, "uvcvonsf", 
		UM_NE_UVCVONSF, fixed_size, UM_NV_UVCVONSF, 
		atom_size, list_size);
	umi_init_rel_label(UM_UVCVONSF_REL, "SS", 1);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_uni_init_agcrv()
**	     Initialize AG curve relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_uni_init_agcrv()

	{
	int j;
	int atom_size[UM_MAX_NBR_VAR_LISTS];
	int list_size[UM_MAX_NBR_VAR_LISTS];
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_uni_init_agcrv()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	um_init_rel(UM_AGCRV_REL, "agcrv", 50, sizeof(struct UM_agcrv_rec), 0, 
					atom_size, list_size);
	umi_init_rel_label(UM_AGCRV_REL, "CV", 1);

	uu_dexitstatus("uc_uni_init_agcrv", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_uni_init_agsrf()
**	     Initialize AG surface relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_uni_init_agsrf()

	{
	int j;
	int atom_size[UM_MAX_NBR_VAR_LISTS];
	int list_size[UM_MAX_NBR_VAR_LISTS];
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_uni_init_agsrf()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	um_init_rel(UM_AGSRF_REL, "agsrf", 50, sizeof(struct UM_agsrf_rec), 0, 
					atom_size, list_size);
	umi_init_rel_label(UM_AGSRF_REL, "SF", 1);

	uu_dexitstatus("uc_uni_init_agsrf", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_uni_init_agshell()
**	     Initialize AG shell relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_uni_init_agshell()

	{
	int j;
	int atom_size[UM_MAX_NBR_VAR_LISTS];
	int list_size[UM_MAX_NBR_VAR_LISTS];
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_uni_init_agshell()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	um_init_rel(UM_AGSHELL_REL, "agshell", 50, sizeof(struct UM_agshell_rec), 0, 
					atom_size, list_size);
	umi_init_rel_label(UM_AGSHELL_REL, "SL", 1);

	uu_dexitstatus("uc_uni_init_agshell", status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_body()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_body()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_body()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	atom_size[0] = sizeof(int);
	fixed_size = sizeof(struct UM_body_rec)
						- sizeof(char) * UM_BODY_BUFSZ;
	list_size[0] = 100;
	um_init_rel(UM_BODY_REL, "body", UM_NE_BODY, 
		fixed_size, UM_NV_BODY, atom_size, list_size);
	umi_init_rel_label(UM_BODY_REL, "BY", 1);

	uu_dexit;
	return(status);
	}


/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_txt()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_txt()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_txt()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	atom_size[0] = sizeof(char);
	fixed_size = sizeof(struct UA_txt_rec) -
						(sizeof(char)*UA_TXT_BUFSZ);
	list_size[0] = 100;
	atom_size[1] = sizeof(UM_covec);
	list_size[1] = 100;
	ur_init_rel(UA_TEXT_REL, "txt", UM_NE_TEXT, 
		fixed_size, UM_NV_TEXT, atom_size, list_size);
	umi_init_rel_label(UA_TEXT_REL,"AN");
	uu_dexit; 
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_txtatt()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output 
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_txtatt()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int list_size[UM_MAX_NBR_VAR_LISTS];

	uu_denter(UU_MTRC,(us,"uc_uni_init_txtatt()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	ur_init_attr(UA_TEXTATTR_REL, "txtattr", UM_NE_TEXTATTR,
		sizeof(struct UA_txtattr_rec) );

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_tran()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_tran()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int list_size[UM_MAX_NBR_VAR_LISTS];
	struct UM_transf_rec transfpacket;

	uu_denter(UU_MTRC,(us,"uc_uni_init_tran()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	um_init_transf(UM_TRANSFORM_REL, "transf", UM_NE_TRANSFORM, 
		sizeof(struct UM_transf_rec));

/* set default transformation */

	um_tftotf(UM_idmat, transfpacket.tfmat);
	ur_set_default_transf(&transfpacket);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_modatt()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_modatt()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int list_size[UM_MAX_NBR_VAR_LISTS];

	uu_denter(UU_MTRC,(us,"uc_uni_init_modatt()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	um_init_attr(UM_ATTR_REL, "attrdata", UM_NE_ATTR,
					sizeof(struct UM_attrdata_rec));

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_polygon()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_polygon()
	{
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];
	int j;


	uu_denter(UU_MTRC,(us,"uc_uni_init_polygon()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}
   fixed_size = sizeof(struct UM_poly_rec)-sizeof(char)*UM_POLY_BUFSZ;
	um_init_rel(UM_POLY_REL, "poly", 
 		UM_NE_POLY, fixed_size, UM_NV_POLY, atom_size, list_size);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_polyline()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_polyline()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_polyline()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	atom_size[0] =  3 * sizeof(UU_REAL);
	fixed_size = sizeof(struct  UM_polyline_rec)
						- sizeof(char) * UM_POLYLINE_BUFSZ;
	list_size[0] = 60;
 	um_init_rel(UM_POLYLINE_REL, "polyline", 
 		UM_NE_POLYLINE, fixed_size, UM_NV_POLYLINE, 
 		atom_size, list_size);
	umi_init_rel_label(UM_POLYLINE_REL, "CV", 1);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_corsys()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_corsys()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int list_size[UM_MAX_NBR_VAR_LISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_corsys()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	um_init_rel(UM_COORDSYS_REL, "coordsys", 
					UM_NE_POINT, sizeof(struct UM_coordsys_rec), 0, 
					atom_size, list_size);


	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_light()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_light()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int list_size[UM_MAX_NBR_VAR_LISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_light()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	um_init_rel(UM_LIGHT_REL, "light", 
					30, sizeof(struct UM_light_rec), 0, 
					atom_size, list_size);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_group()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_group()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_group()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	fixed_size= sizeof(struct UM_grouper_rec) -
			(UM_GROUPER_BUFSZ*sizeof(char));
	atom_size[0] = sizeof(UU_KEY_ID);
	list_size[0] = 100;

	um_init_rel(UM_GROUP_REL, "grouper", UM_NE_GROUP, fixed_size,
		UM_NV_GROUP, atom_size, list_size);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_draw()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_draw()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_draw()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	atom_size[0] = sizeof(UU_KEY_ID);
	fixed_size = sizeof(struct  UM_drawing_rec) 
					- sizeof(char) * UM_DRAWING_BUFSZ;
	list_size[0] = UM_DRAWING_BUFSZ / sizeof(UU_KEY_ID);
	um_init_rel(UM_DRAWING_REL, "drawing", UM_NE_DRAWING, fixed_size,
		/*UM_NV_DRAWING*/1, atom_size, list_size);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_layer()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_layer()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int list_size[UM_MAX_NBR_VAR_LISTS];

	uu_denter(UU_MTRC,(us,"uc_uni_init_layer()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}
/*
.....Added new layer attributes
.....Bobby 1/10/03
*/
	atom_size[0] = sizeof(int);
	list_size[0] = 10;

	ur_init_data_rel(UM_LAYER_REL, "layer", UM_NE_LAYER, 
				sizeof(struct UM_layer_rec), UM_NV_LAYER, atom_size, list_size);

	uu_dexit;
	return(status);
	}

/* --	Non Geometric Stuff -- */

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_lindim()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_lindim()
	{
	int  status,fixed_size;
	struct UA_draft_rec fred;

	static int list_sizes[VARLISTS];
	static int atom_sizes[VARLISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_lindim()"));

	atom_sizes[0] = sizeof(struct UA_txtblk_rec);
	list_sizes[0] = 10;
	atom_sizes[1] = sizeof(struct UA_arcblk_rec);
	list_sizes[1] = 5;
	atom_sizes[2] = sizeof(struct UA_lineblk_rec);
	list_sizes[2] = 5;
	atom_sizes[3] = sizeof(struct UA_arrowblk_rec);
	list_sizes[3] = 10;
	atom_sizes[4] = sizeof(struct UA_assoblk_rec);
	list_sizes[4] = 50;
/*------------------------
	fixed_size = sizeof(struct UA_draft_rec) -
					sizeof(char) * UA_DRAFT_BUFSZ;
--------------------------*/

	fixed_size = (int )fred.varlistbuf - (int)&fred;

	/* drafting relation	*/
	status = ur_init_rel(UA_LINEAR_DIMS_REL, "draft", INITSIZE, 
				fixed_size , 5, atom_sizes, list_sizes);


	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_xhatch()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_xhatch()
	{
	int  status,fixed_size;

	static int list_sizes[VARLISTS];
	static int atom_sizes[VARLISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_xhatch()"));

	/* now initialize UA_HATCHING_REL */

	list_sizes[0] = UA_LS_HATCHLINE;
	atom_sizes[0] = sizeof(UU_REAL);
	fixed_size = sizeof(struct UA_hatchlin_rec) - sizeof(char) *
		UA_HATCHLIN_BUFSZ;
 status = ur_init_rel(UA_HATCHING_REL, "hatchlin", UA_NE_HATCHLINE,
	fixed_size, UA_NV_HATCHLINE, atom_sizes, list_sizes);


	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_msym()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT :  none.
**    RETURNS      : UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_uni_init_msym()
	{
	int status = UU_SUCCESS;
	uu_denter((UU_MTRC|UU_BTRC),(us,"uc_uni_init_msym()"));

	if (ub_init_msym_rel() != UU_SUCCESS) goto failed;
	umi_init_rel_label(UB_SYMBOL_REL, "SY", 1);
	goto done;
failed: status = UU_SUCCESS;
done:;
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_instance()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT :  none.
**    RETURNS      : UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_uni_init_instance()
	{
	int status = UU_SUCCESS;
	uu_denter((UU_MTRC|UU_BTRC),(us,"uc_uni_init_instance()"));

	if (ub_init_instance_rel() != UU_SUCCESS) goto failed;
	umi_init_rel_label(UB_INSTANCE_REL, "[]", 1);
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_attsym()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none. 
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_uni_init_attsym()
	{
	int status = UU_SUCCESS;
	uu_denter((UU_MTRC|UU_BTRC),(us,"uc_uni_init_attsym()"));

	/* in the next call, 20 = expansion factor */
	um_init_attr(UB_SYMATTR_REL, "symattr", 20, sizeof(struct UB_symattr_rec));

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_connector()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS      : UU_SUCCESS or UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_uni_init_connector()
	{
	int status = UU_SUCCESS;
	uu_denter((UU_MTRC|UU_BTRC),(us,"uc_uni_init_connector()"));

	if (ub_con_init_relations() != UU_SUCCESS) goto failed;
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_box()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_box()
	{
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uv_init_attbox()"));

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_attbun()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_attbun()
	{
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_uni_init_attbun()"));

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_view73()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_view73()
	{
	int atom_size[1];					/* size of atom on variable length lists */
	int list_size[1];					/* initial size/expansion factor for lists */

	uu_denter(UU_MTRC,(us,"uv_init_viewing_rel73()"));

/* -- init view relation -- */

	atom_size[0] = 0;
	ur_init_data_rel(UV_VIEW_REL, "viewdef", 25,
				   sizeof(UV_view), 0, atom_size, list_size);

	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_view74()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_view74()
	{
	int atom_size[1];					/* size of atom on variable length lists */
	int list_size[1];					/* initial size/expansion factor for lists */

	uu_denter(UU_MTRC,(us,"uv_init_viewing_rel74()"));

/* init viewport relation */

	atom_size[0] = 0;
	ur_init_data_rel(UV_VPORT_REL, "vport", 25,
				   sizeof(UV_vport), 0, atom_size, list_size);

	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_view75()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_view75()
	{
	int atom_size[1];					/* size of atom on variable length lists */
	int list_size[1];					/* initial size/expansion factor for lists */

	uu_denter(UU_MTRC,(us,"uv_init_viewing_rel75()"));

/* init screen relation */
	atom_size[0] = 0;
	ur_init_data_rel(UV_SCREEN_REL, "screen", 10,
				   sizeof(UV_screen), 0, atom_size, list_size);

	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_calc()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_calc()
	{
	int	atom_size[2], list_size[2];
	int	status;

	uu_denter(UU_MTRC,(us,"uc_uni_init_calc()"));

	atom_size[0] = sizeof(UQ_func);
	list_size[0] = 1;
	status = ur_init_data_rel(UQ_CALC_REL, "qstb", 25,
			  sizeof(UQ_qstb)-UQ_QSTB_BUFSZ*sizeof(char), 1, atom_size,list_size); 


	uu_dexit;
	return (status);
	}


/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_color()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_uni_init_color()
	{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int list_size[UM_MAX_NBR_VAR_LISTS];

	uu_denter(UU_MTRC,(us,"uc_uni_init_color()"));

	atom_size[0] = 0;
	ur_init_data_rel(NCL_COLOR_REL, "color", 1,
				   sizeof(struct NCL_color_rec), 0, atom_size, list_size);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_uni_init_unistat()
**       initialize this relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_uni_init_unistat()
{
	int j;									/* indices */
	int status = UU_SUCCESS;
	int atom_size[UM_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int fixed_size;
	int list_size[UM_MAX_NBR_VAR_LISTS];


	uu_denter(UU_MTRC,(us,"uc_uni_init_txt()"));

	for (j=0; j<UM_MAX_NBR_VAR_LISTS; j++)
 		{
 		list_size[j] = 0;
 		atom_size[j] = 0;
 		}

	atom_size[0] = sizeof(char);
	fixed_size = sizeof(struct UR_unistat_rec) -
						(sizeof(char)*UR_UNISTAT_BUFSZ);
	list_size[0] = 100;
	ur_init_rel(UR_UNISTAT_REL, "unistat", UM_NE_STAT, 
		fixed_size, UM_NV_STAT, atom_size, list_size);
	uu_dexit; 
	return(status);
}

