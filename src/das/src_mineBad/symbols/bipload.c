/*********************************************************************
**    NAME:  bipload.c
**       CONTAINS:
**    		int ubi_use_old_tnodes(oltnodes2use,oldsymptr,newmasterptr,
**    		int ubi_loc_inst2_change(oldsymptr, nbr, newmasterptr, 
**    		int ubi_get_instance_orientation(instdataptr, tfmat)
**    		int ubi_get_namelist(retrieve_funct,namelist, nbreltsptr)
**    		int ubi_fix_names(namelist, nbr)
**	 		int ubi_update_inst_connectors
**			int ubi_reloadMasters
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bipload.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:04
*********************************************************************/
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "class.h"		/* for "UC_" types */
#include "mdrel.h"		/* for relation numbers */
#include "mdattr.h"		/* for definition of colors and UM_DISPLAYABLE*/
#include "mdcoord.h"		/* for UM_transf */
#include "mcrv.h"			/* for UM_text_rec */
#include "bsym.h"
#include "atext.h"		/* for text data types */

#define TRACE UU_FALSE	/* for debugging only */
/*********************************************************************
**    I_FUNCTION: int ubi_use_old_tnodes(maxNbrOldTnodes2Use,oldsymptr,nbr,
**							newmasterptr, instanceptr, nbrtnodesptr)
**		This function puts "maxNbrOldTnodes2Use" number of text nodes from 
**		the current instance whose key is "oldsymptr->inst[nbr]" into the
**		new instance that is to replace this current instance.
**    PARAMETERS   
**		INPUT: 
**			maxNbrOldTnodes2Use	Number of text nodes that the new instance is to
**								inherit from the current one.
**			oldsymptr		Pointer to the current (i.e. old) master symbol whose
**								instances are to be replaced.
**			nbr				Index of instance of the old master, "oldsymptr";
**								we will attempt to use text nodes from this 
**								instance.
**			newmasterptr	Pointer to the new master symbol definition.
**			instanceptr		Pointer to the new instance to have old text nodes
**								put in it.
**		OUTPUT:  
**			oldsymptr		Some text nodes may be deleted from this instance.
**			instanceptr		Pointer to the new instance with old text nodes
**								inherited.
**			nbrtnodesptr	Number of text nodes actually put in the instance.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_use_old_tnodes(maxNbrOldTnodes2Use,oldsymptr,nbr,newmasterptr,
								instanceptr, nbrtnodesptr)
	int maxNbrOldTnodes2Use;
	struct UB_symbol_rec *oldsymptr;
	int nbr;
	struct UB_symbol_rec *newmasterptr;
	struct UB_instance_rec *instanceptr;
	int *nbrtnodesptr;
{
	struct UB_instance_rec oldinst;
	struct UA_txt_rec mastertext, instext;
	int i,j, masterTextNodeIndx, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
	"ubi_use_old_tnodes(maxNbrOldTnodes2Use:%d,oldsym>key:%d,nbr:%d,newsym>key:%d,?,?)",
				maxNbrOldTnodes2Use,oldsymptr->key,nbr,newmasterptr->key));
	/* inherit "maxNbrOldTnodes2Use" nodes text from "olsymptr"; any text nodes 
	 * in the new master remaining will be processed as if a new instance is 
	 * being created. */
	oldinst.key = oldsymptr->inst[nbr].inst_key;
	if (ub_retrieve_sym(&oldinst, sizeof(oldinst)) != UU_SUCCESS)
			goto failed;
	j = 0; /* the index of the next text node in the old instance that is 
			  * to be put into the new instance */
	/* note, the loop below is needed because the old instance may not have
	 * the same number of text nodes as the old master. */
	while ((j<oldinst.no_text_nod) && /*make sure have valid index into oldinst*/
			 (j<maxNbrOldTnodes2Use) && /*see if it is possible for another to be*/
			 (oldinst.text_nod[j].masterindx < maxNbrOldTnodes2Use))
				/* This final check assures us that we are only using the first
				 * "maxNbrOldTnodes2Use" of text nodes that COULD be in the 
				 * instance. */
	{	/* "masterindx" has the index of the text node in the master */

		if (ubi_update_app_varlist(instanceptr,UB_ITEXT_LIST,
				&(oldinst.text_nod[j]),instanceptr->no_text_nod+1, 1) 
			!= UU_SUCCESS) goto failed;

		/* now reposition this old text node as it should be in the new 
		 * instance */

		masterTextNodeIndx = oldinst.text_nod[j].masterindx; 
		/* "masterTextNodeIndx" should be the same for both the old master and the
		 * new master */
		mastertext.key = newmasterptr->text_nod[masterTextNodeIndx].text_key;
		if (uc_retrieve_data(&mastertext, sizeof(mastertext)) != UU_SUCCESS)
				goto failed;
		instext.key = oldinst.text_nod[j].text_key;
		if (uc_retrieve_data(&instext, sizeof(instext)) != UU_SUCCESS)
				goto failed;
		um_vctovc(mastertext.position, instext.position);
		if (ur_update_data(&instext) != 0)
		{
			uu_uerror2(UB_SYMBOL, 95, instext.key, oldinst.key, 
									"ubi_use_old_tnodes");
			/* error is:Can't update position of text entity %d in old instance %d
			 * (%s) */
			sleep(5);
			goto failed;
		}
		j++;
	}
	/* delete text entities from old instance list now so that they don't get 
	 * deleted from UNIBASE later; we just put them on the new instance's list. 
	 * so we don't want to delete them. */
	if (ur_delete_varlist_atoms(oldinst.key, UB_ITEXT_LIST, 1, j) != 0)
	{
		uu_dprint(UU_BTRC,(us,"failed on ur_delete_varlist_atom"));
		uu_uerror3(UB_SYMBOL,93,UB_ITEXT_LIST, oldinst.key, "ubi_use_old_tnodes");
		/* error is:Failure on UNIBASE deletion from list: %d of key:%d  (%s). */
		goto failed;
	}

	if (ur_free_app_data(&oldinst) != 0)
	{
		uu_dprint(UU_BTRC,(us,"ur_free_app_data failed"));
		uu_uerror1(UB_SYMBOL, 100, oldinst.key);
		/* error is: Error in freeing dynamic storage for instance with key:%d. */
		goto failed;
	}
	uu_dprint(UU_BTRC,(us,"got past ur_free_app_data"));
	*nbrtnodesptr = j;
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ubi_loc_inst2_change(oldsymptr, nbr, newmasterptr, 
**									instanceptr)
**		This function locates the instance to be changed; it does this by
**		drawing the geometry of the new instance in either CYAN or MAGENTA.
**    PARAMETERS   
**		INPUT: 
**			oldsymptr		Pointer to the current (i.e. old) master symbol
**								to be replaced.
**			nbr				Index indicating which instance is to be located.
**			newmasterptr	Pointer to the new master symbol definition.
**		OUTPUT:  
**			instanceptr		Pointer to the instance containing the geometry used 
**								to locate the new instance to be created.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_loc_inst2_change(oldsymptr, nbr, newmasterptr, instanceptr)
	struct UB_symbol_rec *oldsymptr;
	int nbr;
	struct UB_symbol_rec *newmasterptr;
	struct UB_instance_rec *instanceptr;
{
	struct UB_inst_rec dummyinstdata;
	struct UC_attributedatabag attr;
	UU_LOGICAL notify;
	UM_transf tfmat;
	int dsegid, j, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
	"ubi_loc_inst2_change(oldsym>key:%d,instnbr:%d,newmaster>key:%d,?)",
				oldsymptr->key, nbr, newmasterptr->key));
	if (ub_retrieve_disp_segid(oldsymptr->inst[nbr].inst_key, &dsegid) != 0)
			goto failed;
	if (dsegid >= 0) uv_delsegs(dsegid);

	/* fillin new master's key into new instance */
	/* instanceptr->master_key = newmasterptr->key; */
	/* get a copy of the geometry of the new master in an instance */
	if (ubi_get_instance_geom(newmasterptr, instanceptr) != UU_SUCCESS)
			goto failed;

	if (ub_create_instance_tuple(newmasterptr->key,instanceptr,UB_DEFAULT_TF,
			&UB_symattr_default_bundle, &dummyinstdata) != UU_SUCCESS) goto failed;

	for (j=0; j<instanceptr->no_geom; j++)
	{		
		if (uc_retrieve_attr(instanceptr->geom[j],&attr) != UU_SUCCESS)
			goto failed;
		/* set attributes to change the color */
		if (attr.color == UM_CYAN) /* then change it */
			attr.color = UM_MAGENTA;
		else 
			attr.color = UM_CYAN;
		if (ub_update_sym_attr(instanceptr->geom[j], &attr) != UU_SUCCESS)
				goto failed;
	}

	/* get the transformation to orient this instance */
	if (ubi_get_instance_orientation(&(oldsymptr->inst[nbr]), tfmat) 
			!= UU_SUCCESS) goto failed;

	/* now transform the instance */
	notify = UU_FALSE;
	if (ur1_notifyAssocOfUpdates(instanceptr->key, &notify) != UU_SUCCESS)
			goto failed;
	if (ub_transform_sym(instanceptr, tfmat, UU_TRUE)	!= UU_SUCCESS) 
			goto failed;
	if (ur1_notifyAssocOfUpdates(instanceptr->key, &notify) != UU_SUCCESS)
			goto failed;

	if (ub_display_sym(instanceptr) != UU_SUCCESS)
			goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ubi_get_instance_orientation(instdataptr, tfmat)
**		This function gets the orientation transformation for the symbol
**		instance whose key is "key".
**    PARAMETERS   
**		INPUT: 
**			instdataptr	Instance data packet associated with master.
**		OUTPUT:  
**			tfmat			Tranformation giving the orientation of the instance.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_get_instance_orientation(instdataptr, tfmat)
	struct UB_inst_rec *instdataptr;
	UM_transf tfmat;
{
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubi_get_instance_orientation(instdataptr:%x,?)", 
					instdataptr));
	um_tftotf(instdataptr->tfmat, tfmat);
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION:int ubi_get_namelist(retrieve_funct,namelist, nbreltsptr)
**			This function returns a list of names of master symbols that
**			are retrieved by the function "retrieve_funct".
**    PARAMETERS   
**		INPUT: 
**			retrieve_funct		Function indicating which master symbols are to be
**									retrieved.
**		OUTPUT:  
**			namelist				List of names of master symbols retrieved.
**			nbreltsptr			Number of valid entries in "namelist".
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_get_namelist(retrieve_funct,namelist, nbreltsptr)
	int (*retrieve_funct)();
	UB_list *namelist;
	int *nbreltsptr;
{
	struct UB_symbol_rec sym;
	int nxtuple, i;
	int status;

	uu_denter(UU_BTRC,(us,"ubi_get_namelist(funct,%x,?)", namelist));
	status = UU_SUCCESS; /* assume success */
	nxtuple = 1;
	i = 0;
	while ((*retrieve_funct)(UB_SYMBOL_REL,&nxtuple,&sym.key)==0)
	{
		if (ub_retrieve_sym(&sym, sizeof(sym)) != UU_SUCCESS) 
				goto failed;
		strcpy(namelist[i].name, sym.label);
		namelist[i].subscr = sym.subscr;
		namelist[i].key = sym.key;
		/*uu_dprint(UU_BTRC,(us,"in ubi_get_namelist, i=%d",i));*/
		/*uu_dprint(UU_BTRC,(us,"in ubi_get_namelist, nxtuple=%d",nxtuple));*/
		i++;
		if (i==UB_LIST_LEN) /* too many masters to deal with */
		{
			uu_uerror1(UB_SYMBOL, 86, "ubi_get_namelist");
			/* error is:Too many master symbols to process during load/merge (%s)*/
			break;
		}
		nxtuple++;
		uu_dprint(UU_BTRC,(us,"in ubi_get_namelist, nxtuple=%d",nxtuple));
	}
	if (ubi_fix_names(namelist, i) != UU_SUCCESS)
			goto failed;
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	*nbreltsptr = i;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ubi_fix_names(namelist, nbr)
**			This function fixes the name list.
**    PARAMETERS   
**		INPUT: 
**			namelist		List of names of master symbols.
**			nbr			Number of valid entries in "namelist".
**		OUTPUT:  
**			namelist		Modified name list.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_fix_names(namelist, nbr)
	UB_list *namelist;
	int nbr;
{
	char tempstr[UB_SYMBOL_NAME_LEN_PLUS];
	UU_KEY_ID tempkey;
	int i,j,stat,tempsub;
	int status;

	uu_denter(UU_BTRC,(us,"ubi_fix_names(namelist:%x, nbr:%d)",
				namelist, nbr));
	status = UU_SUCCESS; /* assume success */
	for (i=0; i<nbr; i++)
	{
		for (j=i+1; j<nbr; j++)
		{
			stat = strcmp(namelist[i].name,namelist[j].name);
			if (stat < 0 || (stat == 0 && namelist[i].subscr < namelist[j].subscr))
			{
				strcpy(tempstr, namelist[i].name);
				tempsub = namelist[i].subscr;
				tempkey = namelist[i].key;

				strcpy(namelist[i].name, namelist[j].name);
				namelist[i].subscr = namelist[j].subscr;
				namelist[i].key = namelist[j].key;

				strcpy(namelist[j].name, tempstr);
				namelist[j].subscr = namelist[i].subscr;
				namelist[j].key = tempkey;
			}
		}
	}
#if (TRACE)
	sprintf(UB_sbuf,"from ubi_fix_names:");
	ubi_pscroll(UB_sbuf);
	for (i=0; i<nbr; i++)
	{
		sprintf(UB_sbuf,"    %s, key:%d",namelist[i].name, namelist[i].key);
		ubi_pscroll(UB_sbuf);
	}
#endif
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ubi_update_inst_connectors(instdataptr, instanceptr)
**			This function a) updates the connector list in the instance
**			pointed to by "instanceptr" so that it has the same connectors
**			as the old instance; b) updates the instance list of each connector
**			that connects to the old instance so that it now contains
**			the new instance instead; c) updates the old instance connector
**			list to have no connections. We assume the new instance has
**			already been created in UNIBASE.
**    PARAMETERS   
**		INPUT: 
**			instdataptr	Pointer to the instance data packet indicating the
**							key of the old instance.
**			instanceptr	Pointer to the new instance.
**		OUTPUT:  none.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_update_inst_connectors(instdataptr, instanceptr)
	struct UB_inst_rec *instdataptr;
	struct UB_instance_rec *instanceptr;
{
	struct UB_instance_rec oldinstance;
	UU_KEY_ID *keysptr;
	int nbrKeys, i, status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,
		"ubi_update_inst_connectors(instdataptr:%x,inst>key:%d)",
		instdataptr, instanceptr->key));

	oldinstance.key = instdataptr->inst_key;

	/* retrieve the connector list; dynamic storage allocated */
	ubi_get_app_spec_assoc_list(oldinstance.key,UB_CONECTOR_REL,
			UB_CON_INST_FLD,&nbrKeys,&keysptr);

	/* switch newinstance for oldinstance on the connectors list */
	for (i=0; i<nbrKeys; i++)
		if (ub_switch_inst_on_connect(keysptr[i],oldinstance.key,
						instanceptr->key) != UU_SUCCESS) goto failed;

	/* @@@ should get rid of dynamic storage allocated here */

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ubi_reloadMasters(doReloadptr)
**			This function determines whether master symbols in UNIBASE are to 
**			be checked against their associated files.
**    PARAMETERS   
**       INPUT: none.
**       OUTPUT :  
**          doReloadptr		Pointer to either UU_TRUE or UU_FALSE. If UU_TRUE
**									then a check for reloading of masters should be
**									done.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubi_reloadMasters(doReloadptr)
	UU_LOGICAL *doReloadptr;
{
	char *index(), *ux_getenv();
	char *chk4reload, *t, *f;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubi_reloadMasters(?)"));
	chk4reload = ux_getenv("UB_UPDATE_LOAD_SYMS");
	if (chk4reload == UU_NULL)
	{
		uu_uerror1(UB_SYMBOL, 94, "ubi_reloadMasters");
		/* error is: Mandatory symbol UB_UPDATE_LOAD_SYMS not defined (%s). */
		goto failed;
	}
	uu_dprint(UU_BTRC,(us,"do reload of master(s)? %s", chk4reload));
	t = index(chk4reload,'T');
	f = index(chk4reload,'F');
	if (t == UU_NULL) 
		*doReloadptr = UU_FALSE;
	else if ((f != UU_NULL) && (f <= t)) 
		*doReloadptr = UU_FALSE;
	else /* either f is UU_NULL or f >= t; so reload */
		*doReloadptr = UU_TRUE;

	*doReloadptr = UU_FALSE;	/* short circuit the auto load capability */
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}
