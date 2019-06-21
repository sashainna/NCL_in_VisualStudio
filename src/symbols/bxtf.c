/*********************************************************************
**    NAME:  bxtf.c
**       CONTAINS:
**				ub_transform_sym
**				ub_transform_msubents
**				ub_transform_isubents
**				ubi_get_tf_for_new_basis
**				ubi_orient_symbol
**				ubi_transform_instance_geom
**				ubi_transform_instance_snodes
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bxtf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:07
*********************************************************************/
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "umoveb.h"
#include "mdcoord.h"		/* for UM_BOX */
#include "mdrel.h"		/* for define relation numbers */
#include "mattr.h"		/* for definition of UM_transform_rec */
#include "class.h"		/* for "UC_" data types */
#include "mcrv.h"			/* for UM_point_rec */
#include "mdcpln.h"		/* for construction plane */
#include "bsym.h"
#include "nccs.h"

#define TRACE UU_TRUE	/* for debugging only */
/*********************************************************************
**    E_FUNCTION : int ub_transform_sym(symptr, tfmat, store) 
**       This function transforms a symbol entity (MASTER or INSTANCE) according**			to the transform,	"tfmat"; if "store" is UU_TRUE then all subentities
**			of the symbol pointed to by "symptr" are also tranformed and all 
**			transformed entities are stored in UNIBASE.  If "store" is 
**			UU_FALSE then only the fields pointed to by "symptr" (that may 
**			have a tranformation applied to them) will be changed and 
**			nothing whatsoever in UNIBASE will be changed.
**			NOTE, WE ASSUME BOTH MASTER SYMBOLS AND SYMBOL INSTANCES HAVE THE
**			IDENTITY TRANSFORMATION AS THEIR ASSOCIATED TRANSFORM.
**    PARAMETERS   
**       INPUT  : 
**          symptr			pointer to a symbol to be transformed.
**				tfmat				transformation to be applied.
**				store				if UU_TRUE then UNIBASE entities are changed, 
**									otherwise not.
**       OUTPUT :  
**          symptr			pointer to the transformed symbol entity; no 
**									subentities are included.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise. 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ub_transform_sym(symptr, tfmat, store)
	struct UB_symbol_rec *symptr;
	UU_REAL tfmat[4][3];
	UU_LOGICAL store;
{
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us, "ub_transform_sym(symptr->key:%d,tfmat:%x,store:%d)",
							symptr->key, tfmat, store));
#if (TRACE)
	ubi_pscroll("from ub_transform_sym, input transform:");
	umi_print_transformation(tfmat);
#endif

	if (store)	
	{	/* then tranform all subentities of the symbol, otherwise do nothing */
		if (symptr->rel_num == UB_SYMBOL_REL)
		{
			if (ub_transform_msubents(symptr, tfmat) != UU_SUCCESS) goto failed;
		}
		else if (ub_transform_isubents(symptr, tfmat) != UU_SUCCESS) goto failed;
	}/* end store */
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION : int ub_transform_msubents(symptr, tfmat)
**			This function transforms the subentities of a master symbol.			
**    PARAMETERS   
**       INPUT  : 
**				symptr		Pointer to the master symbol to have its subentities 
**								transformed.
**				tfmat			Transformation associated with the master symbol.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: Subentities can't not be hierarchical.
*********************************************************************/
int ub_transform_msubents(symptr, tfmat)
	struct UB_symbol_rec *symptr;
	UM_transf tfmat;
{
	struct UC_entitydatabag ent;
	UM_transf temptf; 
	int i, status = UU_SUCCESS;

	uu_denter(UU_BTRC, (us, "ub_transform_msubents(symptr->key:%d, tfmat:%x)",
			symptr->key, tfmat));
	for (i=0; i<symptr->no_geom; i++)
	{
		ent.key = symptr->geom[i];
		if (uc_retrieve_data(&ent, sizeof(ent)) != UU_SUCCESS) 
			goto failed;
		/* orient "ent" in instance space */
		if (uc_retrieve_transf(ent.key, temptf) != UU_SUCCESS)
			goto failed;
		um_tftmtf(temptf, tfmat, temptf);

#if (TRACE)
		ubi_pscroll("from ub_transform_msubents before geom transform:");
		umi_print_transformation(temptf);
#endif
		/* Note, the following usually transforms the underlying geometry
		 * but this should always be the case final parameter is UU_TRUE. */
		if (uc_transform(&ent, temptf, UU_TRUE) != UU_SUCCESS)
					goto failed;
	}/* end transform geom */

	for (i=0; i<symptr->no_snap_nod; i++)
	{
		ent.key = symptr->snap_nod[i].snap_key;
		if (uc_retrieve_data(&ent, sizeof(struct UC_entitydatabag))
			!= UU_SUCCESS) goto failed;
		if (uc_retrieve_transf(ent.key, temptf) != UU_SUCCESS)
			goto failed;
		um_tftmtf(temptf, tfmat, temptf);
		if (uc_transform(&ent, temptf, UU_TRUE) != UU_SUCCESS)
			goto failed;
	}
	for (i=0; i<symptr->no_text_nod; i++)
	{
		ent.key = symptr->text_nod[i].text_key;
		if (uc_retrieve_data(&ent, sizeof(struct UC_entitydatabag))
			!= UU_SUCCESS) goto failed;
		if (uc_retrieve_transf(ent.key, temptf) != UU_SUCCESS)
			goto failed;
		um_tftmtf(temptf, tfmat, temptf);
		if (uc_transform(&ent, temptf, UU_TRUE) != UU_SUCCESS)
			goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION : int ub_transform_isubents(symptr, tfmat)
**			This function transforms the subentities of a symbol instance.			
**    PARAMETERS   
**       INPUT  : 
**				symptr		Pointer to the instance to have its subentities 
**								transformed.
**				tfmat			Transformation associated with the instance.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: Subentities can't not be hierarchical.
*********************************************************************/
int ub_transform_isubents(symptr, tfmat)
	struct UB_instance_rec *symptr;
	UM_transf tfmat;
{
	struct UC_entitydatabag ent;
	UM_transf temptf; 
	int i;
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ub_transform_isubents(symptr->key:%d, tfmat:%x)",
			symptr->key, tfmat));

	if (ubi_transform_instance_geom(symptr, tfmat) != UU_SUCCESS)
			goto failed;

	if (ubi_transform_instance_snodes(symptr, tfmat) != UU_SUCCESS)
			goto failed;

	for (i=0; i<symptr->no_text_nod; i++)
	{
		ent.key = symptr->text_nod[i].text_key;
		if (uc_retrieve_data(&ent, sizeof(struct UC_entitydatabag))
			!= UU_SUCCESS) goto failed;
		if (uc_retrieve_transf(ent.key, temptf) != UU_SUCCESS)
			goto failed;
		um_tftmtf(temptf, tfmat, temptf);
		if (uc_transform(&ent, temptf, UU_TRUE) != UU_SUCCESS)
			goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    I_FUNCTION : int ubi_get_tf_for_new_basis(whichbasis, origin, tfmat) 
**       This function calculates the transformation that will change the
**			origin to "origin" and either change construction coordinates into
**			model coordinates, or, change model coordinates into construction
**			coordinates.
**    PARAMETERS   
**       INPUT  : 
**          whichbasis		if this is "UB_MODEL_TO_CPLANE" then we calculate the
**								transformation that will orient the symbol with 
**								respect to "origin" and the construction 
**								plane axises in model coordinates.
**								if this is "UB_CPLANE_TO_MODEL" then we 
**								calculate the transformation that will change the 
**								coordinates of the (master) symbol so that the 
**								(master) symbol has a) its origin at "origin" 
**								and b) its coordinates in terms of the construction 
**								plane axises.
**				origin		coordinates of the origin in MODEL COORDINATES.
**       OUTPUT :  
**          tfmat			transformation that will change origin and basis.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_get_tf_for_new_basis(whichbasis, origin, tfmat)
	int whichbasis;
	UU_REAL origin[3];
	UM_transf tfmat;
{
	UM_transf translationtf;
	int i, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubi_get_tf_for_new_basis(whichbasis:%d,%x,%x)",
								whichbasis, origin, tfmat));
#if (TRACE)
	sprintf(UB_sbuf,"origin:<%g,%g,%g>",origin[0],origin[1],origin[2]);
	ubi_pscroll(UB_sbuf);
#endif
	/* copy identity transformation into "tfmat" */
	uu_move_byte(UM_idmat, tfmat, sizeof(UM_transf));

	/* copy identity transformation into "translationtf" */
	uu_move_byte(UM_idmat, translationtf, sizeof(UM_transf));

	if (whichbasis == UB_MODEL_TO_CPLANE)
	{
		/* translate new symbol origin to <0,0,0> so that the master symbol
		 * subentities can be relativized with respect to <0,0,0> */
		um_vctovc(origin, translationtf[3]);
		um_vctmsc(translationtf[3], (UU_REAL) -1.0, translationtf[3]);
		 
		/* Get transformation to change basis to construction plane axises from 
		 * model space. Since both coordinate systems have orthogonal axises, the
		 * change of basis matrix from model coordinates to construction plane
		 * coordinates is created by having the columns of the matrix be the unit
		 * direction vectors for the construction plane axises.
		 * This is due to the fact that for an orthogonal matrix, A, 
		 * inverse(A) = transpose(A).
		 */
		for (i=0; i<3; i++)
		{
			tfmat[i][0] = UM_cpln.xaxis[i];
			tfmat[i][1] = UM_cpln.yaxis[i];
			tfmat[i][2] = UM_cpln.zaxis[i];
		}
		/* In translating from model coordinates to construction plane 
		 * coordinates, we first translate to model space origin, then apply the
		 * matrix, "tfmat", then translate back to the point via the new 
		 * coordinate system. */
		/* this gets us to the origin and a change of basis */
		um_tftmtf(translationtf, tfmat, tfmat);
		/*
		{
			* this is a check on the matrix *
			static UM_vector xaxis = { 1.0, 0.0, 0.0};
			static UM_vector yaxis = { 0.0, 1.0, 0.0};
			static UM_vector zaxis = { 0.0, 0.0, 1.0};
			static UM_coord oldorigin = {0.0, 0.0, 0.0};
			UM_transf newtfmat;
			um_chgcstf(oldorigin,xaxis,yaxis,zaxis,origin,UM_cpln.xaxis,
						UM_cpln.yaxis, UM_cpln.zaxis, newtfmat);
			ubi_pscroll("from ubi_get_tf_for_new_basis, check tf");
			umi_print_transformation(newtfmat);
		}
		*/
	}
	else if (whichbasis == UB_CPLANE_TO_MODEL)
	{
		/* translate <0,0,0> to new symbol instance origin so that it can 
		 * be placed appropriately. */
		um_vctovc(origin, translationtf[3]);
		 
		/* matrix rows are construction plane direction vectors */
		um_vctovc(UM_cpln.xaxis, tfmat[0]);
		um_vctovc(UM_cpln.yaxis, tfmat[1]);
		um_vctovc(UM_cpln.zaxis, tfmat[2]);
		/* convert to model coordinates then translate */
		um_tftmtf(tfmat, translationtf, tfmat);
		/*
		{
			* this is a check on the matrix * 
			static UM_vector xaxis = { 1.0, 0.0, 0.0};
			static UM_vector yaxis = { 0.0, 1.0, 0.0};
			static UM_vector zaxis = { 0.0, 0.0, 1.0};
			static UM_coord oldorigin = {0.0, 0.0, 0.0};
			UM_transf newtfmat;
			um_chgcstf(oldorigin,UM_cpln.xaxis,UM_cpln.yaxis,
				UM_cpln.zaxis,origin,xaxis,yaxis,zaxis,newtfmat);
			ubi_pscroll("from ubi_get_tf_for_new_basis, check tf");
			umi_print_transformation(newtfmat);
		}
		*/
	}
	else goto failed;

#if (TRACE)
	ubi_pscroll("from ubi_get_tf_for_new_basis, real tf");
	umi_print_transformation(tfmat);
#endif
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION : int ubi_orient_symbol(symptr, symorigin)
**       Fix the geometry for the master symbol so that it is relativized
**		 	to the symbol origin and construction plane coordinate axises; 
**			note, 1) we TRANSFORM ALL THE SUBENTITIES so that the transforms
**						associated with them are the USUALLY THE IDENTITY TRANSFORM.
**    PARAMETERS   
**       INPUT  : 
**       		symptr	pointer to the master symbol record that is to have its
**								geometry relativized.
**				symorigin	coordinates of the master symbol origin in model 
**								coordinates.
**       OUTPUT :  
**          output
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_orient_symbol(symptr, symorigin)
	struct UB_symbol_rec *symptr;
	UU_REAL symorigin[3];
{
	struct UC_entitydatabag e;
	UU_REAL tfmat[4][3];
	UU_REAL etfmat[4][3];
	struct UC_attributedatabag attr;
	int i, stat, status = UU_SUCCESS;
	
	uu_denter(UU_BTRC,(us,"ubi_orient_symbol(symptr:%x,symorigin:%g,%g,%g)",
					symptr, symorigin[0],symorigin[1],symorigin[2]));

	/* get transformation that will change the coordinates of the symbol so that
	 * the (master) symbol has a) its origin at "symorigin" and b) its 
	 * coordinates in terms of the construction plane axises as basis vectors.
	 */
	if (ubi_get_tf_for_new_basis(UB_MODEL_TO_CPLANE, symorigin, tfmat)
			!= UU_SUCCESS) goto failed;
	/* for each geometric entity in the master symbol, relativize it to the
	 * symbol origin */
	for (i=0; i<symptr->no_geom; i++)
	{
		e.key = symptr->geom[i];
		if (uc_retrieve_transf(e.key, etfmat) != UU_SUCCESS)
			goto failed;
		um_tftmtf(etfmat, tfmat, etfmat);

		if (uc_retrieve_data(&e, sizeof(e)) != UU_SUCCESS)
		{
			uu_uerror1(UB_SYMBOL, 22, e.key);
			/* message is: Can not retrieve data for key = %d 
			 * (ubi_orient_symbol). */
			goto failed;
		}
		/* store transformed record of the entity, with identity transform */
		if (uc_transform(&e, etfmat, UU_TRUE) != UU_SUCCESS) goto failed;
/*
.....also make symbol entity label off
*/
		attr.key = e.key;
		stat = ur_retrieve_attr(&attr);
		if (stat == 0)
		{
			attr.label_on = ncl_label_off(attr.label_on);
			ur_update_attr(&attr);
		}
	}/* end for */
	for (i=0; i<symptr->no_snap_nod; i++)
	{
		e.key = symptr->snap_nod[i].snap_key;
		if (um_get_transformation(e.key, etfmat) != UU_SUCCESS)
			goto failed;
		um_tftmtf(etfmat, tfmat, etfmat);

		if (uc_retrieve_data(&e, sizeof(e)) != UU_SUCCESS)
		{
			uu_uerror1(UB_SYMBOL, 22, e.key);
			/* message is: Can not retrieve data for key = %d 
			 * (ubi_orient_symbol). */
			goto failed;
		}
		/* store transformed record of the entity, with identity transform */
		if (uc_transform(&e, etfmat, UU_TRUE) != UU_SUCCESS)
		{
			uu_uerror2(UB_SYMBOL, 87, e.key, "ubi_orient_symbol");
			/* error is: Can't transform snap node with key: %d  (%s). */
			goto failed;
		}
	}/* end for */

	for (i=0; i<symptr->no_text_nod; i++)
	{
		e.key = symptr->text_nod[i].text_key;
		if (um_get_transformation(e.key, etfmat) != UU_SUCCESS)
			goto failed;
		um_tftmtf(etfmat, tfmat, etfmat);

		if (uc_retrieve_data(&e, sizeof(e)) != UU_SUCCESS)
		{
			uu_uerror1(UB_SYMBOL, 22, e.key);
			/* message is: Can not retrieve data for key = %d 
			 * (ubi_orient_symbol). */
			goto failed;
		}
		/* store transformed record of the entity, with identity transform */
		if (uc_transform(&e, etfmat, UU_TRUE) != UU_SUCCESS)
				goto failed;
	}/* end for */

	goto success;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
success:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION : int ubi_transform_instance_geom(symptr, tfmat)
**			This function transforms instance geometry of a symbol instance.
**    PARAMETERS   
**       INPUT  : 
**				symptr		Pointer to the instance to have its subentities 
**								transformed.
**				tfmat			Transformation associated with the instance.
**       OUTPUT :  .
**				symptr		Pointer to the instance with transformed geometry.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: Subentities can't not be hierarchical.
*********************************************************************/
int ubi_transform_instance_geom(symptr, tfmat)
	struct UB_instance_rec *symptr;
	UM_transf tfmat;
{
	struct UC_entitydatabag ent;
	UM_transf temptf; 
	int i, status = UU_SUCCESS;

	uu_denter(UU_BTRC, (us,"ubi_transform_instance_geom(symptr>key:%d,tfmat:%x)",
			symptr->key, tfmat));
	for (i=0; i<symptr->no_geom; i++)
	{
		ent.key = symptr->geom[i];
		if (uc_retrieve_data(&ent, sizeof(ent)) != UU_SUCCESS) 
			goto failed;
		/* orient "ent" in instance space */
		if (uc_retrieve_transf(ent.key, temptf) != UU_SUCCESS)
			goto failed;
/*
.....tmat will be multiplied with the transformation matrix of the trim
.....surface in the trim surface transformation routine so multiplying here
.....causes a second multiplication, which we don't want - Andrew 10/29/12
*/
		if (ent.rel_num != NCL_TRIMSF_REL) um_tftmtf(temptf, tfmat, temptf);
		else um_tftotf(tfmat,temptf);

#if (TRACE)
		ubi_pscroll("from ubi_transfrom_instance_geom before geom uc_transform:");
		umi_print_transformation(temptf);
#endif

		/* Note, the following usually transforms the underlying geometry
		 * but this is not always the case.
		 */
		if (uc_transform(&ent, temptf, UU_TRUE) != UU_SUCCESS)
			goto failed;
	}/* end transform geom */

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION : int ubi_transform_instance_snodes(symptr, tfmat)
**			This function transforms instance snap nodes of a symbol instance.
**    PARAMETERS   
**       INPUT  : 
**				symptr		Pointer to the instance to have its subentities 
**								transformed.
**				tfmat			Transformation associated with the instance.
**       OUTPUT :  .
**				symptr		Pointer to the instance with transformed snap nodes.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: Subentities can't not be hierarchical.
*********************************************************************/
int ubi_transform_instance_snodes(symptr, tfmat)
	struct UB_instance_rec *symptr;
	UM_transf tfmat;
{
	struct UM_point_rec ent;
	UM_transf temptf; 
	int i, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubi_transform_instance_snodes(symptr>key:%d,tfmat:%x)"
			,symptr->key, tfmat));
	for (i=0; i<symptr->no_snap_nod; i++)
	{
		ent.key = symptr->snap_nod[i].snap_key;
		if (uc_retrieve_data(&ent, sizeof(struct UM_point_rec))
			!= UU_SUCCESS) goto failed;
		if (uc_retrieve_transf(ent.key, temptf) != UU_SUCCESS)
			goto failed;
		um_tftmtf(temptf, tfmat, temptf);
		if (uc_transform(&ent, temptf, UU_TRUE) != UU_SUCCESS)
			goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}
