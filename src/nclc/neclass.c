/*********************************************************************
**    NAME         :  neclass.c
**       CONTAINS: class dispatchers for NCL entities
**
**       int ncl_display
**       int ncl_draw
**       int ncl_uni_draw
**       int ncl_uni_copy
**       int ncl_retrieve_data
**       int ncl_print
**       int ncl_delete
**       int ncl_proj_to_drawing
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       neclass.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:26
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mattr.h"
#include "mcrv.h"
#include "msrf.h"
#include "mfort.h"
#include "mdebug.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : int ncl_display(eptr)
**       Call the viewing system to display an NCL entity.
**    PARAMETERS   
**       INPUT  : 
**          eptr						NCL entity 
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			The only data which must be initialized in the entity 
**			are the UNIBASE key and relation number.
*********************************************************************/
int
ncl_display(eptr)
struct UC_entitydatabag *eptr;

{
	int status;

	uu_denter(UU_MTRC,(us,"ncl_display(key=%x)", eptr->key));
	status = uv_disp_entity(eptr);
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_draw(eptr, tfmat, attrptr)
**       Call the appropriate routine to draw an NCL entity (EPTR)
**			which is transformed by a transformation matrix (TFMAT) and
**			drawn with the specified display attribute bundle (ATTRPTR).
**			Both the transformation matrix and attribute bundle may be
**			UU_NULL indicating that no transformation matrix or display
**			attributes are to be applied in drawing (i.e. stroking) the
**			entity.
**    PARAMETERS   
**       INPUT  : 
**          eptr						entity data
**				tfmat						transforamtion matrix
**				attptr					display attribute bundle
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			At present, the transformation matrix is not applied.
*********************************************************************/
int
ncl_draw(eptr, tfmat, attrptr)
struct UC_entitydatabag *eptr;
UM_transf tfmat;
struct UC_attributedatabag *attrptr;

{
	int status;
	UM_int4 nclkey;
	UM_int2 ncltype;

	uu_denter(UU_MTRC,(us,"ncl_draw(key=%x, tfmat=%x, attrptr=%x)",
		eptr->key, tfmat, attrptr));
	nclkey = eptr->key;
	status = UU_SUCCESS;
	um_set_disp_attr(attrptr);
/*
.....In case ncl_draw() is called to draw a circle which is not in the
.....Unibase or RANFIL.  UC_DRAW for circles is now ncl_draw() to correct
.....the use of *SET/ADISPLY.  For DRAFTING DIMENSION, arcs are created and 
.....displayed without being created into the Unibase and RANFILE.
.....
.....The correct fix would be to modify um_set3_circle() to consider
.....the*set/adisply and then change class/ccrvin.c: uc_init_circle() 
.....so that UC_DRAW is um_drw3_circle() once again.
*/
	if ((eptr->key == 0) && (eptr->rel_num == UM_CIRCLE_REL))
	{
		status = um_drw3_circle(eptr, tfmat, attrptr);
	}
	else if (eptr->rel_num == NCL_EVALCV_REL)
 	{
	 	status = ncl_disp_evalcv (eptr, tfmat, attrptr);
	}
	else if (eptr->rel_num == NCL_EVALSF_REL)
 	{
		status = ncl_disp_evalsf (eptr, tfmat, attrptr);
	}
	else
	{
		status = ncl_get_type(eptr->rel_num, &ncltype);
		if (status == UU_SUCCESS) drwent(&nclkey, &ncltype, tfmat);
	}

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_uni_draw(eptr, tfmat, attrptr)
**       Call the unicad draw routine & then draw a label if necessary.
**    PARAMETERS   
**       INPUT  : 
**          eptr                 entity data
**          tfmat                transforamtion matrix
**          attptr               display attribute bundle
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* NCL: this module made obsolete with a change to class/c1class.c:uc_draw().
	uc_draw() now call ncl_draw_unilabel() directly.
int
ncl_uni_draw(eptr, tfmat, attrptr)
struct UC_entitydatabag *eptr;
UM_transf tfmat;
struct UC_attributedatabag *attrptr;

{
	int status;

	uu_denter(UU_MTRC,(us,"ncl_uni_draw(key=%x, tfmat=%x, attrptr=%x)",
		eptr->key, tfmat, attrptr));

	if (uc_draw(eptr, tfmat, attrptr) != 0)
		status = UU_FAILURE;
	else status = ncl_draw_unilabel(eptr, tfmat);

	uu_dexit;
	return (status);
}
*/

/*********************************************************************
**    E_FUNCTION     : int ncl_uni_copy(e1ptr, e2ptr, entsize)
**       Call the unicad copy routine & then update label of entity.
**    PARAMETERS   
**       INPUT  : 
**          e1ptr             old entity data
**          e2ptr             new entity data
**          entsize           size of entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uni_copy(e1ptr, e2ptr, entsize)
struct UC_entitydatabag *e1ptr;
struct UC_entitydatabag *e2ptr;
int entsize;

{
	UM_transf tfmat;
	int status;
	UM_int2 nclstatus=0;

	uu_denter(UU_MTRC,(us,"ncl_uni_copy(key=%x)", e1ptr->key));

	status = UU_FAILURE;

	if (uc_copy(e1ptr, e2ptr, entsize) == 0)
	{
		struct UM_point_rec *e;
		e = (struct UM_point_rec *) e2ptr;
/* Added key and status for new geom name feature. */
		ncl_label_wf(e->rel_num, e->label, &e->subscr, e->key, &nclstatus);
		ncl_store_wf1(e->key);
		if (um_get_transformation(e->key, tfmat) == 0)
			if (um_update_geom(e2ptr,tfmat) == 0) status = UU_SUCCESS;
	}

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_retrieve_data(eptr, entsize)
**       Retrieve the data for an NCL relation. It calls 
**       ncl_retrieve_data_fixed() which sets the pointers to any
**       variable lists to their memory location in the unibase.
**    PARAMETERS   
**       INPUT  : 
**          eptr                 entity structure with KEY field set
**          entsize              size of entity structure (not used)
**       OUTPUT :  
**          eptr                 Relation data
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : Care should be taken in modifying variable list
**                   data directly after calling this routine.
*********************************************************************/
int
ncl_retrieve_data(eptr, entsize)
struct UC_entitydatabag *eptr;
int entsize;

{
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"ncl_retrieve_data(key=%x, entsize=%d)",
		eptr->key, entsize));

	if (ncl_retrieve_data_fixed(eptr) != 0) status = UU_FAILURE;

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_print(eptr)
**       Call the appropriate routine to print an entity in an 
**			NCL relation.
**    PARAMETERS   
**       INPUT  : 
**          eptr							an NCL entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_print(eptr)
struct UC_entitydatabag *eptr;

{
	int status;

	uu_denter(UU_MTRC,(us,"ncl_print(key=%x)", eptr->key));
	status = uc_retrieve_mtuple_data(eptr, sizeof(struct UC_entitydatabag));
	if (status == UU_SUCCESS)
	{
		switch(eptr->rel_num)
		{
			case NCL_POINT_REL:
				ncl_p88_point(eptr);
				break;
			case NCL_LINE_REL:
				ncl_p89_line(eptr);
				break;
			case NCL_CIRCLE_REL:
				ncl_p90_circle(eptr);
				break;
			case NCL_PLN_REL:
				status = UU_FAILURE;
				break;
			case NCL_VECTOR_REL:
				ncl_p86_vector(eptr);
				break;
			case NCL_MATRIX_REL:
				ncl_p87_matrix(eptr);
				break;
			case NCL_CURVE_REL:
				ncl_p80_curve(eptr);
				break;
			case NCL_SURF_REL:
				ncl_p81_surf(eptr);
				break;
			case NCL_PANEL_REL:
				ncl_p82_panel(eptr);
				break;
			case NCL_MESHSURF_REL:
				ncl_p83_meshsurf(eptr);
				break;
			case NCL_QUILTSURF_REL:
				ncl_p86_quiltsurf(eptr);
				break;
			case NCL_SHAPE_REL:
			default:
				status = UU_FAILURE;
				break;
		}
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_delete(key)
**       Delete an NCL entity (KEY) from UNIBASE, DIGS, and the NCL
**			ranfile.
**    PARAMETERS   
**       INPUT  : 
**          key					UNIBASE key of entity to delete
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_delete(key)
UU_KEY_ID key;

{
	int status;
	int rel_num;
	struct NCL_trimsf_rec trmsf;
	struct UC_attributedatabag attr;
	int dsegid;
	int i;

	uu_denter(UU_MTRC,(us,"ncl_delete(key=%d)", key));

	status = UU_SUCCESS;
	ur_retrieve_data_relnum(key, &rel_num);
	switch (rel_num)
	{
		case NCL_POINT_REL:
		case NCL_LINE_REL:
		case NCL_CIRCLE_REL:
		case NCL_PLN_REL:
		case NCL_VECTOR_REL:
		case NCL_MATRIX_REL:
		case NCL_CURVE_REL:
		case NCL_SURF_REL:
		case NCL_REVSURF_REL:
		case NCL_MESHSURF_REL:
		case NCL_QUILTSURF_REL:
		case NCL_NETSF_REL:
		case NCL_SHAPE_REL:
		case NCL_PATERN_REL:
		case NCL_SCALAR_REL: /*NCL: ian's fix for unibase growing problem */
		case NCL_POINTVEC_REL:
		case NCL_EVALCV_REL:
		case NCL_EVALSF_REL:
		case NCL_TRIMSF_REL:
		case UM_RBSPLSRF_REL:
		case NCL_DATAST_REL:
		case NCL_TEXTVAR_REL:
		case UM_SOLID_REL:
			if (ur_retrieve_disp_segid(key, &dsegid) != 0)
				status = UU_FAILURE;
			else
			{
				if (dsegid >= 0) uv_delsegs(dsegid);
				ur_update_blanked(key, UU_TRUE);
				attr.key = key;
				ur_retrieve_attr(&attr);
/*
.....Delete label location if label is altered
*/
				if (ncl_get_label_alter(attr.label_on))
					ncl_delete_labloc(&attr.label_on);

				switch (rel_num)
				{
					case UM_RBSPLSRF_REL:
						status = ncl_delete_rbsf_assoc (key);
						break;
					case NCL_SURF_REL:
						status = ncl_delete_surf_assoc (key);
						break;
					case NCL_REVSURF_REL:
						status = ncl_delete_revsurf_assoc (key);
						break;
					case NCL_NETSF_REL:
						status = ncl_delete_netsf_assoc (key);
						break;
					case NCL_TRIMSF_REL:
						trmsf.key = key;
						status = ncl_retrieve_data_fixed(&trmsf, sizeof(trmsf));
						if (trmsf.uv_key > 0 && status == UU_SUCCESS)
							status = uc_delete (trmsf.uv_key);
						if (trmsf.cv_key > 0 && status == UU_SUCCESS)
							status = uc_delete (trmsf.cv_key);
						if (trmsf.bs_key > 0 && status == UU_SUCCESS)
							status = uc_delete (trmsf.bs_key);
						for (i=0; i<trmsf.no_ibndykey && status == UU_SUCCESS; i++)
							if (trmsf.ibndykey[i]>0) status=uc_delete(trmsf.ibndykey[i]);
						break;
					default:
						break;
				}
				 ur_delete_all(key);
			}
			break;
		default:
			status = UU_FAILURE;
			break;
	}

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_delete_rbsf_assoc (key)
**       Delete an rbspline surface associate entities from UNIBASE, DIGS, 
**       and the NCL ranfile if applicable.
**    PARAMETERS   
**       INPUT  : 
**          key	UNIBASE key of rbspline surface entity to delete
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_delete_rbsf_assoc (key)
UU_KEY_ID key;
{
	int status, i;
	struct UM_rbsplsrf_rec e;

	e.key = key;
	status = ncl_retrieve_data_fixed (&e);
	if (status == UU_SUCCESS)
	{
/*
.....delete curves on surface as full entity (use uc_delete removes
.....also associate key from surface data e.sskey and since it is
.....pointed to data in unibase next sskey is always in [0] after
.....deleting previous curve). Do NOT change index unless using other
.....function than uc_delete.
*/
		for (i=0; i<e.no_sskey; i++)
			if (e.sskey[0] != key)
				uc_delete (e.sskey[0]);
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_delete_surf_assoc (key)
**       Delete an NCL surface associate entities from UNIBASE, DIGS, 
**       and the NCL ranfile if applicable.
**    PARAMETERS   
**       INPUT  : 
**          key	UNIBASE key of NCL surface entity to delete
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_delete_surf_assoc (key)
UU_KEY_ID key;
{
	int status, i;
	struct NCL_surface_rec e;

	e.key = key;
	status = ncl_retrieve_data_fixed (&e);
	if (status == UU_SUCCESS)
	{
/*
.....delete panels (old logic)
*/
		for (i=0; i<e.no_panelkey; i++)
			ur_delete_all(e.panelkey[i]);
/*
.....delete curves on surface as full entity (use uc_delete removes
.....also associate key from surface data e.sskey and since it is
.....pointed to data in unibase next sskey is always in [0] after
.....deleting previous curve). Do NOT change index unless using other
.....function than uc_delete.
*/
		for (i=0; i<e.no_sskey; i++)
			uc_delete(e.sskey[0]);
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_delete_revsurf_assoc (key)
**       Delete an NCL surface of revolution associate entities from 
**       UNIBASE, DIGS, and the NCL ranfile if applicable.
**    PARAMETERS   
**       INPUT  : 
**          key	UNIBASE key of NCL surface entity to delete
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_delete_revsurf_assoc (key)
UU_KEY_ID key;
{
	int status, i;
	struct NCL_revsurf_rec e;

	e.key = key;
	status = ncl_retrieve_data_fixed (&e);
	if (status == UU_SUCCESS)
	{
/*
.....delete curves on surface as full entity (use uc_delete removes
.....also associate key from surface data e.sskey and since it is
.....pointed to data in unibase next sskey is always in [0] after
.....deleting previous curve). Do NOT change index unless using other
.....function than uc_delete.
*/
		for (i=0; i<e.no_sskey; i++)
			uc_delete(e.sskey[0]);
	}
	if (status == UU_SUCCESS)
		ur_delete_all(e.cvkey);
	
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_delete_netsf_assoc (key)
**       Delete an NCL surface associate entities from UNIBASE, DIGS, 
**       and the NCL ranfile if applicable.
**    PARAMETERS   
**       INPUT  : 
**          key	UNIBASE key of NCL surface entity to delete
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_delete_netsf_assoc (key)
UU_KEY_ID key;
{
	int status, i;
	struct NCL_netsf_rec e;

	e.key = key;
	status = ncl_retrieve_data_fixed (&e);
	if (status == UU_SUCCESS)
	{
/*
.....delete subsurfaces (old logic)
*/
		for (i=0; i<e.no_netkey; i++)
			uc_delete(e.netkey[i]);
/*
.....delete curves on surface as full entity (use uc_delete removes
.....also associate key from surface data e.sskey and since it is
.....pointed to data in unibase next sskey is always in [0] after
.....deleting previous curve). Do NOT change index unless using other
.....function than uc_delete.
*/
		for (i=0; i<e.no_sskey; i++)
			uc_delete(e.sskey[0]);
	}
	return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_proj_to_drawing(eptr, tfmat, attrptr, 
**									drwmat, vrefpt, vpnorm,
**									keylist, render_solids)
**			Create a list of keys (KEYLIST) of entities to be included
**			in a drawing for the given entity (EPTR, TFMAT, ATTRPTR).
**			The transformation (DRWMAT) will position the viewplane
**			(VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**			scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**				eptr						pointer to entity data
**				tfmat						matrix to position entity in MCS
**				attrptr					pointer to attribute bundle 
**				drwmat					transformation to convert MCS to DCS
**				vrefpt					reference point of viewing plane
**				vpnorm					normal of viewing plane
**				render_solids			UU_TRUE => render solid entities
**       OUTPUT :  
**				keylist					keys of projected entities are pushed
**											onto this list of keys
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_proj_to_drawing(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
		keylist, render_solids)
struct UC_entitydatabag *eptr;
UM_transf tfmat;
struct UC_attributedatabag *attrptr;
UM_transf drwmat;
UM_coord vrefpt;
UM_vector vpnorm;
UU_LIST *keylist;
UU_LOGICAL render_solids;

{
	struct UM_crvdatabag unientity;
	struct UM_attrdata_rec uniattr;
	int status;
	UM_int2 mdsys, mm;

	uu_denter(UU_MTRC,(us,"ncl_proj_to_drawing(key=%x)", eptr->key));

	status = UU_SUCCESS;
/*
..... aak 20-apr-1998: commented this out. Reads in wrong label
..... if a curve is passed in as a part of a composite curve
.....
	status = uc_retrieve_mtuple_data(eptr, sizeof(struct UC_entitydatabag));
*/
	switch (eptr->rel_num)
	{
		/* convert point, line, and circle NCL entities to UNICAD entities; use
			the UNICAD supplied routines to project them onto a drawing */
		case NCL_POINT_REL:
		case NCL_LINE_REL:
		case NCL_CIRCLE_REL:
/* added vector for drawing. kathy */
/*    case NCL_VECTOR_REL:vp 3-may-93 removed     */ 
			switch (eptr->rel_num)
			{
				case NCL_POINT_REL:
					ncl_nclpt_to_unipt(eptr, &unientity);
					break;
				case NCL_LINE_REL:
					ncl_nclln_to_uniln(eptr, &unientity);
					break;
				case NCL_CIRCLE_REL:
					ncl_nclci_to_unici(eptr, &unientity);
					break;

				/* added vector for drawing. kathy  */
/*
				case NCL_VECTOR_REL:
					ncl_nclve_to_unive(eptr, &unientity);
					break;   
.....  vp 3-may-93 removed and assigned method same as for PV 
*/

			}
			ncl_nclattr_to_uniattr((struct NCL_nclattr_rec *)attrptr, &uniattr);
			status = uc_proj_to_drawing(&unientity, tfmat, &uniattr,
							drwmat, vrefpt, vpnorm,
							keylist, render_solids);
			break;
		case NCL_CURVE_REL:
			status = ncl_proj_curve_to_drawing(eptr, tfmat, attrptr,
							drwmat, vrefpt, vpnorm,
							keylist, render_solids);
			break;
		case NCL_PLN_REL:  /* add for drawing. kathy */
			status = ncl_proj_plane_to_drawing(eptr, tfmat, attrptr,
							drwmat, vrefpt, vpnorm,
							keylist, render_solids);
			break;
/*
.....Project pattern onto drawing
.....Bobby  -  7/25/91
*/
		case NCL_PATERN_REL:
			status = ncl_proj_pn_to_drawing (eptr,tfmat,attrptr,drwmat,
							vrefpt,vpnorm,keylist,render_solids);
			break;
		case NCL_SURF_REL:
		case NCL_REVSURF_REL:
		case NCL_MESHSURF_REL:
			gtmsmm (&mdsys, &mm);
			status = ncl_proj_rbsf_to_drawing(eptr, tfmat, attrptr,
							drwmat, vrefpt, vpnorm,
							keylist, render_solids);
			stmsmm (&mdsys, &mm);
			break;
		case NCL_QUILTSURF_REL:
			status = ncl_proj_surf_to_drawing(eptr, tfmat, attrptr,
							drwmat, vrefpt, vpnorm,
							keylist, render_solids);
			break;

		/* these entities will probably never be projected onto a drawing */
		case NCL_PANEL_REL:
		/* case NCL_VECTOR_REL: commented for drawing. kathy */
		case NCL_MATRIX_REL:
		/* case NCL_PLN_REL:  commented for drawing. kathy */
		case NCL_SHAPE_REL:
		default:
			status = UU_FAILURE;
			break;
	}

	uu_dexit;
	return (status);
}
int ncl_retrieve_transf(key, tfmat)
UU_KEY_ID key;
UM_transf tfmat;
{
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"ncl_retrieve_transf(key=%x)", key));
	if (uc_retrieve_mtuple_transf (key, tfmat) != 0) status = UU_FAILURE;
	uu_dexit;
	return(status);
}
int ncl_retrieve_attr(key, attrptr)
UU_KEY_ID key;
struct UC_attributedatabag *attrptr;
{
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"ncl_retrieve_attr(key=%x)", key));
	attrptr->key = key;
	if (ur_retrieve_attr(attrptr)!=0) status = UU_FAILURE;
	uu_dexit;
	return(status);
}

int ncl_translate(eptr, offset)
struct UM_entitydatabag *eptr;
UM_vector offset;
{
	int status;

	uu_denter(UU_MTRC,(us,"ncl_translate(key=%x)", eptr->key));

	status = UU_SUCCESS;
	switch (eptr->rel_num)
	{
		case NCL_POINT_REL:
			ncl_point_transl(eptr,offset);
			break;
		case NCL_LINE_REL:
			ncl_line_transl(eptr,offset);
			break;
		case NCL_PLN_REL:
			ncl_plane_transl(eptr,offset);
			break;
		case NCL_CIRCLE_REL:
			ncl_circle_transl(eptr,offset);
			break;
		case NCL_CURVE_REL:
/*         ncl_curve_transl(eptr,offset); */
/*         break; */
		case NCL_SURF_REL:
		case NCL_REVSURF_REL:
		case NCL_MESHSURF_REL:
		case NCL_QUILTSURF_REL:
		case NCL_SHAPE_REL:
/*         ncl_surf_transl(eptr,offset); */
/*         break; */
		case NCL_VECTOR_REL:
		default:
			status = UU_FAILURE;
			break;
	}
	uu_dexit;
	return(status);
}
int ncl_rotate(eptr, pt, dir, angle, rotmat)
{
/*	um_pscroll("ncl_rotate stub"); */
	return(UU_FAILURE);
}
int ncl_transform(eptr, tfmat, store)
{
/*	um_pscroll("ncl_transform stub"); */
	return(UU_FAILURE);
}
int ncl_copy(e1ptr, e2ptr, entsize)
{
/*	um_pscroll("ncl_copy stub"); */
	return(UU_FAILURE);
}
int ncl_mirror(eptr, mirrpt, normal, mirrmat)
{
/*	um_pscroll("ncl_mirror stub"); */
	return(UU_FAILURE);
}
int ncl_scale(eptr, scalpt, scalmat, scale)
{
/*	um_pscroll("ncl_scale stub"); */
	return(UU_FAILURE);
}
int ncl_create(eptr, tfmat, attrptr)
{
/*	um_pscroll("ncl_create stub"); */
	return(UU_FAILURE);
}
