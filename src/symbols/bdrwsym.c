/*********************************************************************
**    NAME:  bdrwsym.c
**       CONTAINS:
**			ub_display_sym
**			ub_draw_sym
**			ubu_change_instance_color
**			ubu_change_instance_visibility
**			ub_get_keys_for_drawing
**			ub_proj_to_drawing
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bdrwsym.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:03
*********************************************************************/
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "dmark.h"		/* for UD_MARK and UD_UNMARK */
#include "dasnog.h"		/* for UD_RUBBER used in dragging */
#include "class.h"		/* for "UC_" data types */
#include "dasnog.h"		/* for UD_DASCART */
#include "dselmask.h"	/* for UD_symbol in limit DAS */
#include "mdrel.h"		/* for define relation numbers */
#include "mdattr.h"		/* for UM_DISPLAYABLE */
#include "mattr.h"		/* for definition of UM_transform_rec */
#include "mcrv.h"			/* for UM_point_rec */
#include "mdcpln.h"		/* for construction plane */
#include "mdcoord.h"		/* for UM_zerovec */
#include "ulist.h"		/* for UU_LIST data type */
#include "bsym.h"

#include "umath.h"		/* these files brough over with "ub_proj_to_drawing" */
#include "mdrel.h"
#include "mdclass.h"
#include "mdgenent.h"
#include "msol.h"
#include "mxxx.h"
#include "mdraw.h"
#include "mdebug.h"

#include "nccs.h"
extern int UR_active;

#define TRACE UU_FALSE	/* for debugging only */
/*********************************************************************
**    E_FUNCTION :  int ub_display_sym(eptr)
**       This function displays the symbol pointed to by "eptr".
**    PARAMETERS   
**       INPUT  : 
**          eptr			pointer to the symbol entity to be displayed.
**       OUTPUT :  		none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_display_sym(eptr)
	struct UB_instance_rec *eptr;
{
	int ub_draw_sym();
	int status;
	struct UB_symattr_rec attr;

	uu_denter(UU_BTRC,(us,"ub_display_sym(eptr->key:%d)",eptr->key));
	status = UU_SUCCESS;	/* assume success */
	/* uc_retrieve_attr(eptr->key,&attr); */

	/* set view key so that symbol instances are displayed in all views */
	/*
	if (ur_update_view_key(eptr->key, 0) != 0)
	{
		uu_uerror1(UB_SYMBOL,46, "ub_display_sym");
		goto failed;
	}
	*/
	/* The following call sets up the view, gets transformations and attributes
	 * then calls "ub_draw_sym".
	 */
/*
.....if it's second unibase, don't draw symbol
.......Removed so symbols are displayed in two sided display when a secondary
.......unibase is opened - Andrew 10/26/12
	if (UR_active == 2) 
		goto done;
*/
	if (uv_disp_entity(eptr) != UU_SUCCESS)
		goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :  int ub_draw_sym(symptr, tfmat, attrptr)
**       This function draws the (instance) symbol pointed to by "eptr".
**    PARAMETERS   
**       INPUT  : 
**        symptr		Pointer to the symbol entity to be displayed.
**			 tfmat 		Transformation to orient symbol.
**			 attrptr		Pointer to the attribute bundle for the symbol.
**       OUTPUT :  		none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_draw_sym(symptr, tfmat, attrptr)
	struct UB_instance_rec *symptr;
	UU_REAL tfmat[4][3];
	struct UB_symattr_rec *attrptr;	/* note not being used yet */
{
	struct UC_entitydatabag ent;
	struct NCL_fixed_databag *nclent;
	struct UC_attributedatabag subent_attr;
	UU_REAL etfmat[4][3];
	int offset;
	struct UM_point_rec *ptptr;
	int i, status = UU_SUCCESS;
	UU_LOGICAL uj_miplotting();

	uu_denter(UU_BTRC,(us,"ub_draw_sym(symptr->key:%d, tfmat:%x, attrptr:%x)",
				symptr->key, tfmat, attrptr));

	/*************** draw snap nodes *******************/
	if (attrptr->see_snod)
	{
		offset = symptr->no_geom + 1;	/* pickid offset for snap nodes */
		ptptr = (struct UM_point_rec *)&ent;

		/* set marker type for snap nodes */
		if (ubi_set_marker_type(UB_snap_node_marker_type) != UU_SUCCESS)
			goto failed;

		for (i=0; i<symptr->no_snap_nod; i++)
		{
			ptptr->key = symptr->snap_nod[i].snap_key;
			if (uc_retrieve_data(ptptr, sizeof(struct UC_entitydatabag))
				!= UU_SUCCESS)	goto failed;
			/* assume snap nodes have the default transform */
			/* get display attributes */
			if (uc_retrieve_attr(ptptr->key, &subent_attr) != UU_SUCCESS)
				goto failed;
			/* set snap node draw attributes to draw */
/*
.....We are plotting
.....Set Color to Logical Pen Number
.....of symbol
.....Bobby  -  7/22/91
*/
			if (uj_miplotting())
			{
				subent_attr.color = attrptr->pen;
			}
			else
			{
				subent_attr.color = UB_snap_node_color;
				if (ur_update_attr(&subent_attr) != 0)
					goto failed;
			}

			ptptr->markertype = UB_snap_node_marker_type;

#if (TRACE)
			ubi_pscroll("from ub_draw_sym: snap node for instance:");
			um_p_geometry(ptptr);
			umi_print_transformation(tfmat);
#endif
			/* set pick id for subentities and draw symbol subentity */
			gspickid(i+offset);	/* set pick id */
			if (uc_draw(ptptr, tfmat, &subent_attr) != UU_SUCCESS)
				goto failed;
		}/* end drawing snap nodes */

	/********** draw symbol geometry *************/
	for (i=0; i<symptr->no_geom; i++) 
	{
		ent.key = symptr->geom[i];
		if (uc_retrieve_data(&ent, sizeof(struct UC_entitydatabag))
			!= UU_SUCCESS)	goto failed;
		/* orient "ent" in instance space */
		if (uc_retrieve_transf(ent.key, etfmat) != UU_SUCCESS)
			goto failed;
		um_tftmtf(etfmat, tfmat, etfmat);

		/* get display attributes */
		if (uc_retrieve_attr(ent.key, &subent_attr) != UU_SUCCESS)
			goto failed;
		/* change color symbol subentities are drawn in 
		subent_attr.color = attrptr->color;
		 */
/*
.....We are plotting
.....Set Color to Logical Pen Number
.....of sub-entity
.....Bobby  -  7/22/91
*/
		if (uj_miplotting()) subent_attr.color = subent_attr.pen;
/*
.....Some entities won't display
.....if the label is @UN
.....(surfaces for example)
.....Bobby  -  3/21/03
*/
		nclent = (struct NCL_fixed_databag *)&ent;
		if (strncmp(nclent->label,"@UN",3) == 0) strcpy(nclent->label,"@UNKNOWN");
	
#if (TRACE)
			ubi_pscroll("from ub_draw_sym: geom for instance:");
			um_p_geometry(&ent);
			umi_print_transformation(etfmat);
#endif
		/* set pick id for subentities and draw symbol subentity */
		gspickid(i+1);	/* set pick id */
/*
.....Continue drawing entities after failure
.....Some symbols have bad surfaces that can't be displayed
.....Bobby - 07/30/09
*/
/*		if (uc_draw(&ent, etfmat, &subent_attr) != UU_SUCCESS)
			goto failed;*/
		uc_draw(&ent, etfmat, &subent_attr);
	}/* end drawing geometry */


		if (ubi_reset_marker_type() != UU_SUCCESS)
			goto failed;
	}

	/*************** draw text nodes ****************/
	if (  (attrptr->see_tnod == UB_ALL_TEXT_NODES_VISIBLE)
		|| (attrptr->see_tnod == UB_ONLY_GRAPHIC_TEXT_NODES))
	{
		/* cycle thru text nodes displaying the ones desired */
		offset = offset + symptr->no_snap_nod;
		for (i=0; i<symptr->no_text_nod; i++)
		{
			switch (symptr->text_nod[i].visibility)
			{
				case UB_NONGRAPHIC_TEXT_NODE:
					if (UB_text_node_visibility == UB_ONLY_GRAPHIC_TEXT_NODES)
						break;
				case UB_GRAPHIC_TEXT_NODE:/* display these in either case */
					/* then got a text node to display */
					ent.key = symptr->text_nod[i].text_key;
					if (uc_retrieve_data(&ent, sizeof(struct UC_entitydatabag))
						!= UU_SUCCESS)	goto failed;
					/* assume text nodes have the default transform */
					/* get display attributes */
					if (uc_retrieve_attr(ent.key, &subent_attr) != UU_SUCCESS)
						goto failed;
#if (TRACE)
					ubi_pscroll("from ub_draw_sym: text node for instance:");
					um_p_geometry(&ent);
					umi_print_transformation(tfmat);
#endif  	
/*
.....We are plotting
.....Set Color to Logical Pen Number
.....of symbol
.....Bobby  -  7/22/91
*/
					if (uj_miplotting()) subent_attr.color = attrptr->pen;

					/* set pick id for subentities and draw symbol subentity */
					gspickid(i+offset);	/* set pick id */
					if (uc_draw(&ent, tfmat, &subent_attr) != UU_SUCCESS)
						goto failed;
					break;
				default:
					uu_uerror2(UB_SYMBOL, 72, symptr->text_nod[i].visibility, 
									"ub_drw_sym");
					/* error is: Bad text node visibility number, %d, (%s) */
					break;
			}/*end switch */
		}/* end text node for loop */
	}/* end drawing text nodes */

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION : int ubu_change_instance_color(symptr, color, firstimptr); 
**			This function changes the color of an instance in the instance
**			attribute bundle. This function does not display instances.
**    PARAMETERS   
**       INPUT  : 
**          symptr		Point to the instance to have its color changed.
**				color			Color of new instance.
**				firstimptr	UU_TRUE iff the subentities to change in the instance,
**								"symptr", (i.e. the geometry, text nodes, and/or snap
**								nodes) have NOT been specified in a previous call to 
**								this function.
**       OUTPUT :  
**          symptr		Point to instance in new color.
**				firstimptr  Pointer to UU_FALSE.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: No long jumps here; long jumps should be in calling function.
*********************************************************************/
int ubu_change_instance_color(symptr, color, firstimptr)
	struct UB_instance_rec *symptr;
	int color;
	UU_LOGICAL *firstimptr;
{
	struct UC_attributedatabag attr;
	static UU_LOGICAL change_geom;
	static UU_LOGICAL change_snap;
	static UU_LOGICAL change_text;
	char *index();
	int old_snap_node_color;
	int i;
	int numint;
	static char string[4] = "a";
	int status;			/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, 
		"ubu_change_instance_color(symptr->key:%d,color:%d,firstim:%d)",
							symptr->key, color, *firstimptr));
	status = UU_SUCCESS; /* assume success */
	
	if (*firstimptr)	/* then need to ask about things to change on instances */
	{
		ud_ldas(UD_DASSTRING, UB_SYMBOL, 24, string, 3, &numint, UD_DEFAULT);
		/* message is: Change symbol color of: 
		 * geometry (g), snap nodes (s), text (t), all (a). */
		if (index(string, 'a') != UU_NULL)
		{
			change_geom = UU_TRUE;
			change_snap = UU_TRUE;
			change_text = UU_TRUE;
		}
		else
		{
			if (index(string, 'g') != UU_NULL)
				change_geom = UU_TRUE;
			else
				change_geom = UU_FALSE;

			if (index(string, 's') != UU_NULL)
				change_snap = UU_TRUE;
			else
				change_snap = UU_FALSE;

			if (index(string, 't') != UU_NULL)
				change_text = UU_TRUE;
			else 
				change_text = UU_FALSE;
		}
	}
#if (TRACE)
	sprintf(UB_sbuf,"from ubu_change_instance_color: string:%s", string);
	ubi_pscroll(UB_sbuf);
	sprintf(UB_sbuf,"		change_geom:%d,change_snap:%d,change_text:%d",
					change_geom, change_snap, change_text);
	ubi_pscroll(UB_sbuf);
#endif

	if (change_geom)
		for (i=0; i<symptr->no_geom; i++)
		{
			if (uc_retrieve_attr(symptr->geom[i], &attr) != UU_SUCCESS)
				goto failed;
			attr.color = color;
			if (ur_update_attr(&attr) != 0)
				goto failed;
		}
	old_snap_node_color = UB_snap_node_color;
	if (change_snap)
		UB_snap_node_color = color;
		/*
		for (i=0; i<symptr->no_snap_nod; i++)
		{
			if (uc_retrieve_attr(symptr->snap_nod[i].snap_key, &attr) 
						!= UU_SUCCESS) goto failed;
			attr.color = color;
			if (ur_update_attr(&attr) != 0)
				goto failed;
		}
		*/
	if (change_text)
		for (i=0; i<symptr->no_text_nod; i++)
		{
			if (uc_retrieve_attr(symptr->text_nod[i].text_key, &attr) 
				!= UU_SUCCESS) goto failed;
			attr.color = color;
			if (ur_update_attr(&attr) != 0)
				goto failed;
		}
	if (uc_display(symptr) != UU_SUCCESS)
		goto failed;
	
	UB_snap_node_color = old_snap_node_color; /* reset snap node color */
	if (*firstimptr) *firstimptr = UU_FALSE;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION : int ubu_change_instance_visibility()
**			This function changes the visibility of symbol instances.
**			Snap nodes and text nodes can be either blanked or unblanked.
**			This function both changes the attribute bundles and
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none. 
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_change_instance_visibility()
{
	struct UB_instance_rec sym;
	struct UB_symattr_rec attr;
	int cmdreject;	/* command reject flag */
	static char string[4] = "a";	/* storage for user input */
	UU_LOGICAL startover;
	char *index();
	int level;
	int snapvis, textvis; /* snap and text node visibility */
	int dsegid, nbrpicks, numint, status = UU_SUCCESS;

	uu_denter(UU_BTRC, (us, "ubu_change_instance_visibility()"));
	status = UU_SUCCESS; /* assume success */
	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE);	
	if (cmdreject)	/* then command reject encountered */
		goto done;

	/* now temporarily reset the global modals for snap and text node
	 * visibility so that the instances picked above can be drawn as
	 * desired. */
	if (ubu_reset_instance_vis_form("binstvis.frm", &snapvis, &textvis) 
				!= UU_SUCCESS) goto failed;

	/* limit das to symbol instances only */
	ud_lgeo(UU_TRUE, UD_symbol);
		
	ud_ldas(UD_DASSELECT, UB_SYMBOL, 25, UU_NULL, 0, &nbrpicks, UD_NODEFAULT);
	/* prompt is: Pick symbol instances to change visibility. */
	if (nbrpicks <= 0)
		goto done;

	startover = UU_TRUE;		
	level = 1;
	/* redraw picked instances */
	while (ud_gnxt(startover, UU_NULL, &sym.key, level)) 
	{
		startover = UU_FALSE;
		/* delete the display segment for the current symbol */
		if (ur_retrieve_disp_segid(sym.key, &dsegid) != 0) goto failed;
		if (dsegid >= 0) uv_delsegs(dsegid);
		if (ur_update_disp_segid(sym.key, -1) != 0) goto failed;
		if (ub_retrieve_sym(&sym, sizeof(sym)) != 0) goto failed;
		if (ub_get_sym_attr(sym.key, &attr) != 0) goto failed;
		attr.see_snod = snapvis;
		attr.see_tnod = textvis;
		if (ur_update_attr(&attr) != 0)
		{
			uu_uerror1(UB_SYMBOL, 15, sym.key);
			/* message is: Can not store attribute bundle for entity that
			 * has key: %d. */
			goto failed;
		}
		if (uc_display(&sym) != UU_SUCCESS) goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	ud_lgeo(UU_FALSE, UD_symbol);/* second arg unnecessary */
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :int ub_get_keys_for_drawing(keyptr, listptr)
**			This function puts the subentity keys for a symbol instance into
**			the list pointed to by "listptr".  Note, only the geometry keys
**			and the keys for the graphic text nodes are put in the list.
**    PARAMETERS   
**       INPUT  : 
**          keyptr		pointer to the key of the symbol instance.
**				listptr		pointer to the list on which to put the keys.
**       OUTPUT :  
**          listptr		pointer to the list with the new keys.
**    RETURNS: UU_SUCC
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_get_keys_for_drawing(keyptr, listptr)
	UU_KEY_ID *keyptr;
	UU_LIST *listptr;
{
	struct UB_instance_rec sym;
	struct UB_symattr_rec attr;
	int i;
	int status;			/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ub_get_keys_for_drawing(*keyptr:%d, listptr:%x)",
								*keyptr, listptr));
	status = UU_SUCCESS; /* assume success */
	sym.key = *keyptr;
	if (ub_retrieve_sym(&sym, sizeof(sym)) != UU_SUCCESS)
		goto failed;
	
	for (i=0; i<sym.no_geom; i++) /* get geometry keys */
		uu_list_push(listptr, &(sym.geom[i]));

	if (ub_get_sym_attr(sym.key, &attr) != UU_SUCCESS)
		goto failed;

	/* determine which text nodes to put in drawing */
	switch (attr.see_tnod)
	{
		case UB_ALL_TEXT_NODES_VISIBLE:
			for (i=0; i<sym.no_text_nod; i++)
				uu_list_push(listptr, &(sym.text_nod[i].text_key));
			break;
		case UB_ONLY_GRAPHIC_TEXT_NODES:
			for (i=0; i<sym.no_text_nod; i++)
				if (sym.text_nod[i].visibility == UB_GRAPHIC_TEXT_NODE)
					uu_list_push(listptr, &(sym.text_nod[i].text_key));
			break;
		case UB_NO_TEXT_NODES_VISIBLE:
			break;
	}

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**    E_FUNCTION     : int ub_proj_to_drawing(eptr, tfmat, attrptr, 
**									drwmat, vrefpt, vpnorm,
**									keylist, render_solids)
**       Routine to project UNICAD symbol entities to a drawing.
**			Create a list of keys (KEYLIST) of entities to be included
**			in a drawing for the given symbol (EPTR, TFMAT, ATTRPTR).
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
int
ub_proj_to_drawing(sym, tfmat, attrptr, drwmat, vrefpt, vpnorm,
		keylist, render_solids)
	struct UB_instance_rec *sym;
	UM_transf tfmat;
	struct UB_symattr_rec *attrptr;
	UM_transf drwmat;
	UM_coord vrefpt;
	UM_vector vpnorm;
	UU_LIST *keylist;
	UU_LOGICAL render_solids;

	{
	struct UC_entitydatabag edata;
/*
.....UB_symattr_rec structure is not big enough
.....Bobby  -  5/19/94
*/
/*	struct UB_symattr_rec subent_attr;*/
	struct UC_attributedatabag subent_attr;

	int i;
	int status;

	uu_denter(UU_BTRC,(us,"ub_proj_to_drawing(key=%x)", sym->key));

	status = UU_SUCCESS; /* assume success */
	
	for (i=0; i<sym->no_geom; i++) /* get geometry keys */
		{
		edata.key = sym->geom[i];
		if (uc_retrieve_data(&edata, sizeof(edata)) != UU_SUCCESS)
			goto failed;
		if (uc_retrieve_attr(edata.key, &subent_attr) != UU_SUCCESS)
			goto failed;
		uc_proj_to_drawing(&edata, tfmat, &subent_attr, drwmat, vrefpt, vpnorm,
			keylist, render_solids);
		}

	/* determine which text nodes to put in drawing */
	switch (attrptr->see_tnod)
		{
		case UB_ALL_TEXT_NODES_VISIBLE:
			for (i=0; i<sym->no_text_nod; i++)
				{
				edata.key = sym->text_nod[i].text_key;
				if (uc_retrieve_data(&edata, sizeof(edata)) != UU_SUCCESS)
					goto failed;
				uc_proj_to_drawing(&edata, tfmat, attrptr, drwmat, vrefpt, vpnorm,
					keylist, render_solids);
				}
			break;
		case UB_ONLY_GRAPHIC_TEXT_NODES:
			for (i=0; i<sym->no_text_nod; i++)
				if (sym->text_nod[i].visibility == UB_GRAPHIC_TEXT_NODE)
					{
					edata.key = sym->text_nod[i].text_key;
					if (uc_retrieve_data(&edata, sizeof(edata)) != UU_SUCCESS)
						goto failed;
					uc_proj_to_drawing(&edata, tfmat, attrptr, drwmat, vrefpt,
						vpnorm, keylist, render_solids);
					}
			break;
		case UB_NO_TEXT_NODES_VISIBLE:
			break;
		}

	goto done;
failed: status = UB_FAILURE;
#if (TRACE) 
	UB_IF_FAILURE_PRINT_IT
#endif
done:;
	uu_dexit;
	return(status);
	}	
