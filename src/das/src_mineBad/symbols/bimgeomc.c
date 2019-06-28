/*********************************************************************
**	 NAME:  bimgeomc.c
**		 CONTAINS:
**			 ubi_add_geom_and_msyms
**			 ubi_disolve_composite
**			 ubi_delete_composites
**			 ubi_calc_symbol_box
**	 COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**	 MODULE NAME AND RELEASE LEVEL 
**       bimgeomc.c , 25.1
**	 DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:03
*********************************************************************/
#include "usysdef.h"	 		/* for UU_REAL, etc. */
#include "uhep.h"		 		/* for error system */
#include "mdrel.h"			/* for relation numbers */
#include "class.h"			/* for "UC_" data types */
#include "mattr.h"			/* for text attributes */
#include "mcrv.h"		 		/* for text definition */
#include "mdcpln.h"	  		/* for construction plane definition */
#include "mdclass.h"	 		/* for UM_POINT_CLASS, etc */
#include "bsym.h"	 
#include "udebug.h"	  		/* for debugging trace facility */

#define TRACE UU_FALSE /* for debugging only */
/*********************************************************************
**	 I_FUNCTION : int ubi_add_geom_and_msyms(key, symptr,	delgeomlist, 
**								nbronglistptr, delinstlist, nbronilistptr)
**		 This function adds a single geometry entity to a master symbol.
**		 Note, the entity could be a composite curve.
**	 PARAMETERS	
**		 INPUT  :
**		 	key				UNIBASE key for the entity to added.
**		 	symptr			Pointer to master symbol entity being constructed.
**			delgeomlist		Array of geometry keys to delete from UNIBASE, 
**								since their subentities are put into the master
**								symbol.
**			nbronglistptr	Pointer to the number of valid entries in
**								"delgeomlist".
**			delinstlist		Array of symbol instance keys to delete from
**								UNIBASE, since their subentities are put into the
**								master symbol.
**			nbronilistptr	Pointer to the number of valid entries in 
**								"delinstlist".
**		 OUTPUT :  
**			delgeomlist		Array of geometry keys to delete from UNIBASE, 
**								since their subentities are put into the master
**								symbol.
**			nbronglistptr	Pointer to the number of valid entries in
**								"delgeomlist".
**			delinstlist		Array of symbol instance keys to delete from
**								UNIBASE, since their subentities are put into the
**								master symbol.
**			nbronilistptr	Pointer to the number of valid entries in 
**								"delinstlist".
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubi_add_geom_and_msyms(key,symptr,delgeomlist,nbronglistptr,
				 delinstlist, nbronilistptr)
	UU_KEY_ID key;
	struct UB_symbol_rec *symptr;
	UU_KEY_ID delgeomlist[];
	int *nbronglistptr;
	/* THE FOLLOWING AREN'T USED YET */
	UU_KEY_ID delinstlist[];
	int *nbronilistptr;
{
	int rel_num;
	char rel_name[20];
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
	"ubi_add_geom_and_msyms(key:%d,symptr:%x,%x,nbrgeom:%d,%x,nbrinst:%d)",
			key, symptr, delgeomlist, *nbronglistptr, delinstlist,*nbronilistptr));
	
  status = UU_SUCCESS;  /* assume success */
	ur_retrieve_data_relnum(key, &rel_num);
	ur_retrieve_rel_name(key, rel_name);

	if (rel_num == UM_COMPCRV_REL)
	{
		if (ubi_disolve_composite(key, symptr) != UU_SUCCESS) 
					goto failed;
		/* put on list to delete */
		delgeomlist[*nbronglistptr] = key;
		(*nbronglistptr)++;
	}
	else if(rel_num == UM_POINT_REL || rel_num == UA_TEXT_REL)
	{
		/* put the geometry key into the variable list */
		if (ubi_update_app_varlist(symptr,UB_MGEOM_LIST,&key,
				symptr->no_geom+1, 1) != UU_SUCCESS) goto failed;
	}
	else
	{
		/* error is: Entity type, %s, not allowed in master 
		 * symbol; master not created (%s). */
		uu_uerror2(UB_SYMBOL, 50, rel_name, "ubi_add_geom_and_msyms");
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
**	 I_FUNCTION: int ubi_disolve_composite(key, symptr)
**		 This function distributes the composite curve transformation
**		 in to the transformations associated with the constituent 
**		 curves, and adds the subentities to the master symbol.
**	 PARAMETERS	
**		 INPUT  : 
**		 	key				key of composite curve.
**			symptr			Pointer to the master symbol being created.
**		 OUTPUT :  
**			symptr			Pointer to the master symbol being created with
**								the keys of the subentities of the composite curve
**								associated with "key" added to the geometry of
**								"symptr".
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE 
**				 otherwise.
**	 SIDE EFFECTS : changes the transformations of constituents of the
**						 composite curve.
**	 WARNINGS	  : none
*********************************************************************/
int ubi_disolve_composite(key, symptr)
	UU_KEY_ID key;
	struct UB_symbol_rec *symptr;
{
	struct UM_compcrv_rec comp;
	UU_REAL comptfmat[4][3];
	UU_REAL constfmat[4][3];
	UU_KEY_ID *dummylist = UU_NULL;
	int dummyint = -999; /* dummy argument for call to ubi_add_geom_and_msyms */
	int i, status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,"ubi_disolve_composite(key:%d,%x,)",key, symptr));

	/* find constitutents and concatenate their transforms
	 * with the composite's transform */
	if (um_get_transformation(key, comptfmat) != UU_SUCCESS)
		goto failed;
	comp.key = key;
	if (um_get_all_geom(&comp, sizeof(struct UM_compcrv_rec)) != UU_SUCCESS)
		goto failed;

	/* concatenate transforms, and put subentities in symbol */
	i = 0;
	while (i<comp.no_cid)
	{
		if (um_get_transformation(comp.cid[i].crvid, constfmat) != UU_SUCCESS)
			goto failed;
		um_tftmtf(constfmat, comptfmat, constfmat);
		/* store new transform */
		if (um_update_transformation(comp.cid[i].crvid, constfmat) != UU_SUCCESS)
			goto failed;

		/* put subentities into master symbol list */
		if (ubi_add_geom_and_msyms(comp.cid[i].crvid, symptr,	dummylist, 
				&dummyint, dummylist, &dummyint) != UU_SUCCESS) goto failed;
		i++;
	}/* end while */

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}


/*********************************************************************
**	 I_FUNCTION :int ubi_delete_composites(deletelist, nbronlist)
**		This function deletes the subentities whose keys are in the array
**	 "deletelist" from both UNIBASE and the display.
**	 PARAMETERS	
**		 INPUT  :
**		 deletelist	  array of keys whose geometry is to be deleted.
**		 nbronlist		the number of keys on the list.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no difficulties with forms; UB_FAILURE otherwise.
**	 SIDE EFFECTS : none
**	 WARNINGS	  : none
*********************************************************************/
int ubi_delete_composites(deletelist, nbronlist)
	UU_KEY_ID deletelist[];
	int nbronlist;
{
	int dsegid;
	int status;
	int i;

	uu_denter(UU_BTRC,(us,"ubi_delete_composites(%x,nbronlist:%d)",
								deletelist, nbronlist));
	status = UU_SUCCESS;

	/* now delete the geometry in the master symbol form the display */
	for (i=0; i<nbronlist; i++)
	{
		/* delete the display segments for these subentities */
		ur_retrieve_disp_segid(deletelist[i], &dsegid);
		if (dsegid >= 0) uv_delsegs(dsegid);											
		/* ur_update_disp_segid(deletelist[i], -1); */
		if (ur_delete_all(deletelist[i]) != 0)
		{
			uu_uerror2(UB_SYMBOL, 16, deletelist[i], "ubi_delete_composites");
			/* error is: Can't delete entity record, %d, from UNIBASE  (%s). */
			goto failed;
		}
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**	 E_FUNCTION : ubi_calc_symbol_box(eptr,box)
**		This function calculates the bounding box of a symbol master or
**    symbol instance.
**	 PARAMETERS	
**		 INPUT  :
**        eptr         Symbol master or instance to calculate box for.
**		 OUTPUT :
**        box          Bounding box of symbol.
**	 RETURNS: UU_SUCCESS if no errors, UU_FAILURE otherwise.
**	 SIDE EFFECTS : none
**	 WARNINGS	  : none
*********************************************************************/
int ubi_calc_symbol_box(eptr,box)
struct UC_entitydatabag  *eptr;
UU_REAL *box;
{
	int i,status,ngeo;
	UU_KEY_ID *geo;
	UU_REAL geobox[6];
	struct UB_symbol_rec *sym;
	struct UB_instance_rec *inst;
/*
.....Determine type of symbol
*/
	if (eptr->rel_num == UB_SYMBOL_REL)
	{
		sym = (struct UB_symbol_rec *)eptr;
		ngeo = sym->no_geom;
		geo = sym->geom;
	}
	else if (eptr->rel_num == UB_INSTANCE_REL)
	{
		inst = (struct UB_instance_rec *)eptr;
		ngeo = inst->no_geom;
		geo = inst->geom;
	}
	else
	{
		status = UU_FAILURE;
		goto done;
	}
/*
.....Loop through symbol geometry
*/
	box[0] = box[1] = box[2] = 100000.;
	box[3] = box[4] = box[5] = -100000.;
	for (i=0;i<ngeo;i++)
	{
/*
........Calculate box for individual entity
*/
		status = ncl_geo_box(geo[i],geobox);
/*
........Merge boxes
*/
		if (status == UU_SUCCESS)
		{
			ncl_update_box(&geobox[0],box);
			ncl_update_box(&geobox[3],box);
		}
	}
	status = UU_SUCCESS;
	if (box[3] < box[0]) status = UU_FAILURE;
/*
.....End of routine
*/
done:
	return(status);
}
