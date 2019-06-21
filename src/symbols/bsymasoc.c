/*********************************************************************
**	 NAME:  bsymasoc.c
**		 CONTAINS:
**				int ubu_ReplaceMaster(oldsymptr,newsymptr)
**				int ubi_FixMsymsAfterMsymLoad(area,libname,symname,
**				int ubu_assoc_old_inst_new_msym(oldsymptr, oltnodes2use, 
**				int ubu_RedoInstsForNewMaster(olinstdataptr,tnodeindx,
**	 COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**	 MODULE NAME AND RELEASE LEVEL 
**       bsymasoc.c , 25.1
**	 DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:05
*********************************************************************/
#include "usysdef.h"	/* for UU_REAL, etc. */
#include "uhep.h"		/* for error system */
#include "udebug.h"	/* for debugging trace facility */
#include "dasnog.h"	/* for DAS */
#include "mdrel.h"	/* for UB_SYMBOL_REL */
#include "xenv1.h"	/* for UX_PRTERRS */
#include "mcrv.h"		/* for UM_text_rec */
#include "mattr.h"	/* for UM_textattr_rec */
#include "bsym.h"

#define TRACE UU_TRUE /* for debugging only */
/*********************************************************************
**    E_FUNCTION:  int ubu_ReplaceMaster(oldsymptr,newsymptr)
**			This function reassociates the instances of the master pointed to
**			by "oldsymptr" so that they are now instances of the master pointed
**			to by, "newsymptr".
**    PARAMETERS   
**       INPUT: 
**				oldsymptr		Pointer to the old symbol master to have its
**									instances associated to a new symbol master.
**				newsymptr		Poiinter to the new symbol master that is to 
**									receive the instances from "oldsymptr". 
**       OUTPUT:  
**				oldsymptr		Pointer to the old symbol master without any 
**									instances.
**				newsymptr		Pointer to the new symbol master with the new
**									instances added.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: No marks/unmarks done here.
*********************************************************************/
int ubu_ReplaceMaster(oldsymptr,newsymptr)
	struct UB_symbol_rec *oldsymptr;
	struct UB_symbol_rec *newsymptr;
{
	int oltnodes2use;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
			"ubu_ReplaceMaster(oldsymptr>key:%d(%s),newsymptr>key:%d(%s))",
			oldsymptr->key,oldsymptr->label,newsymptr->key,newsymptr->label));
	/* associate any instances of any old master symbol of the same name */
	if (oldsymptr->no_inst > 0) /* then prompt about text nodes */
		/* prompt is: Updating instances of %s; shall current text nodes be 
		 * reused? */
		if (ud_yesno(0, uu_uprompt1(UB_SYMBOL, 35, oldsymptr->label), "Question?"))
			if (oldsymptr->no_text_nod >= newsymptr->no_text_nod)
				oltnodes2use = newsymptr->no_text_nod;
			else
				oltnodes2use = oldsymptr->no_text_nod;
		else  oltnodes2use = 0;
	/* associate any instances of any old master symbol of the same name */
	if (ubu_assoc_old_inst_new_msym(oldsymptr,oltnodes2use,newsymptr) 
			!= UU_SUCCESS) goto failed;
	/* old instances are now deleted; so delete old master */
	if (ubu_del_symmaster(oldsymptr->key, UU_FALSE,UU_TRUE) != UU_SUCCESS)
			goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**	 I_FUNCTION: int ubi_FixMsymsAfterMsymLoad(area,libname,symname,
**												option,symptr)
**			This function updates the master symbols in UNIBASE after a master
**			symbol file has been loaded.
**	 PARAMETERS	
**		 INPUT  : 
**				area			User's local file area; note, this can be a symbolic
**								name from the Unicad initialization file; this can
**								also be UU_NULL.
**				libname		Library (i.e. directory) name; can be a symbolic
**								name, can NOT be UU_NULL; note, this can not be the 
**								full path name up to the file name (or including the
**								file name) unless the parameter, "area" is
**								UU_NULL.
**				symname		Name of the symbol loaded; this must be the symbol
**								as it appears in UNIBASE.
**				option		If "WHOLESYM" then the entire master will be 
**								returned; otherwise, only the fixed part will be 
**								returned
**		 OUTPUT :			
**				symptr		If not UU_NULL on input, then this is 
**								the pointer to the master symbol loaded.
**	 RETURNS: UU_SUCCESS if no problems encountered; otherwise UB_FAILURE.
**	 SIDE EFFECTS : none
**	 WARNINGS	  : none
*********************************************************************/
int ubi_FixMsymsAfterMsymLoad(area,libname,symname,option,symptr)
	char *area;
	char libname[UB_MAX_PATH_LEN];
	char symname[UB_SYMBOL_NAME_LEN];
	char *option;
	struct UB_symbol_rec *symptr;
{
	struct UB_symbol_rec sym;
	char libpath[UB_MAX_PATH_LEN];
	int nxtuple = 1;
	int found = UU_FALSE;
	int *lstat = UU_NULL;
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, 
		"ubi_FixMsymsAfterMsymLoad(area:%x,lib:%x,symname:%s,%s,%x)", 
						area,libname,symname,option,symptr));
#ifdef UU_DEBUGON
	if (area != UU_NULL)
		uu_dprint(UU_BTRC,(us,"area:%s", area));
	if (libname != UU_NULL)
		uu_dprint(UU_BTRC,(us,"libname:%s", libname));
#endif
	/* The following will probably not work if there are masters inside of
	 * masters. */
	while (ur_get_next_new_data_key(UB_SYMBOL_REL,&nxtuple,&sym.key)==0)
	{
		if (ur_retrieve_data_fixed(&sym) != UU_SUCCESS)
				goto failed;
		if (strcmp(sym.label, UB_ROOTMASTER) == 0)
		{
			found = UU_TRUE;
			ncl_parse_label(symname,sym.label,&sym.subscr);
			if (ubi_mk_libname(area,libname,libpath,&lstat,UX_PRTERRS) != UU_SUCCESS)
				goto failed;
#ifdef UU_DEBUGON
		uu_dprint(UU_BTRC,(us,"Return from ubi_mk_libname."));
#endif
			/* make sure the right path name is in the path field */
			if (ux_mk_chk_syspath(UU_NULL,libpath,symname, UU_NULL,
					"UB_SYM_SUFFIX",UU_NULL,sym.path,UX_PRTERRS) != UU_SUCCESS)
					goto failed;
			if (ux_get_fversion(sym.path, &(sym.version), UX_PRTERRS) 
					!= UU_SUCCESS) goto failed;
			if (ubi_update_data_fixed(&sym) != UU_SUCCESS) 
					goto failed;
			break;
		}
		nxtuple++;
	}
	if (found) 
	{
		if (symptr != UU_NULL) 
			if (strcmp(option,"WHOLESYM") == 0)
			{
				symptr->key = sym.key;
				if (ub_retrieve_sym(symptr, sizeof(struct UB_symbol_rec)) 
						!= UU_SUCCESS) goto failed;
			}
			else uu_move_byte(&sym, symptr, sizeof(sym));
	}
	else /* something's wrong */
	{
		uu_uerror3(UB_SYMBOL, 84, symname, libname, "ubi_FixMsymsAfterMsymLoad");
		/* error is:Can't find master symbol, %s, loaded from lib %s, (%s). */
		nxtuple = 1; 
		while(ur_get_next_new_data_key(UB_SYMBOL_REL,&nxtuple,&sym.key)==0)
		{
			if (ubu_del_symmaster(sym.key, UU_FALSE,UU_TRUE) != UU_SUCCESS) 
					goto failed;
			nxtuple++;
		}
		goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
#if (TRACE)
	uu_dprint(UU_MTRC,(us,"END OF ubi_FixMsymsAfterMsymLoad, symptr:%x",symptr));
	if (symptr != UU_NULL)
		if (ub_print_sym(symptr) != UU_SUCCESS) goto failed;
#endif
	uu_dexit;
	return(status);
}	

/*********************************************************************
**	 I_FUNCTION:int ubu_assoc_old_inst_new_msym(oldsymptr, maxNbrOldTnodes2Use, 
**								newmasterptr)
**			This function associates the instances of an old version of a
**			master symbol with a new version; i.e. the instances are changed
**			to reflect the definition of the new version.
**	 PARAMETERS	
**		INPUT: 
**			oldsymptr		Pointer to the current (i.e. old) master symbol.
**			maxNbrOldTnodes2Use	Number of text nodes from the current instance to
**								use.
**			newmasterptr	Pointer to the newly loaded master symbol entity
**								(may only be fixed data).
**		OUTPUT:  none.
**	 RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubu_assoc_old_inst_new_msym(oldsymptr, maxNbrOldTnodes2Use, newmasterptr)
	struct UB_symbol_rec *oldsymptr;
	int maxNbrOldTnodes2Use;
	struct UB_symbol_rec *newmasterptr;
{
	struct UB_instance_rec locinst; /* instance used to locate next old instance
												* to be changed interactively */
	struct UB_instance_rec instance;
	UM_transf tfmat;
	int tnodesleft;
	int i, nxtTnodeIndx;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
	"ubu_assoc_old_inst_new_msym(old>key:%d,oltnod2use:%d,newmaster>key:%d)",
			oldsymptr->key, maxNbrOldTnodes2Use, newmasterptr->key));
	uu_dprint(UU_BTRC,(us,"old>name:%s, newmaster>name:%s", oldsymptr->label,
					newmasterptr->label));
	if (ub_retrieve_sym(newmasterptr, sizeof(*newmasterptr)) != UU_SUCCESS)
				goto failed;
	tnodesleft = newmasterptr->no_text_nod - maxNbrOldTnodes2Use;
	/* associate instances of previous old master with the new one. */
	for (i=0; i<oldsymptr->no_inst; i++)
	{
		UB_SETUP_DATA(UB_INSTANCE_REL, &instance, sizeof(instance), status);
		if (status != UU_SUCCESS) goto failed;
#if (TRACE)
		sprintf(UB_sbuf,
			"REDOING THE OLD INSTANCE WITH INDEX %d OUT OF %d INSTANCES",
						i,oldsymptr->no_inst);
		ubi_pscroll(UB_sbuf);
#endif
		/* "maxNbrOldTnodes2Use" is the max nbr of possible text nodes from the 
		 * old instance that could be placed in the new instance; any text nodes
		 * inherited by the new instance will be removed from the old instance. */
		if (ubi_use_old_tnodes(maxNbrOldTnodes2Use,oldsymptr,i,newmasterptr,
						&instance, &nxtTnodeIndx) != UU_SUCCESS) goto failed;
		if (tnodesleft > 0)
		{	/* display the new geometry as an indicator of which instance is to be
			 * changed; hiliting instance filled in here also. */

			/* shouldn't create a location instance if there is no text for the
			 * user to enter; currently this instance is always created if
			 * "tnodesleft" > 0 */
			uu_dprint(UU_BTRC,(us,"CREATING LOCATION INSTANCE, FOR EXTRA TEXT"));

			UB_SETUP_DATA(UB_INSTANCE_REL, &locinst, sizeof(locinst), status);
			if (status != UU_SUCCESS) goto failed;
			if (ubi_loc_inst2_change(oldsymptr, i, newmasterptr, &locinst)
					!= UU_SUCCESS) goto failed;
		}
		if (ubu_RedoInstsForNewMaster(&(oldsymptr->inst[i]),nxtTnodeIndx,
					newmasterptr, &instance) != UU_SUCCESS) goto failed;

		/**** DELETE OLD INSTANCE ****/
#if (TRACE)
		sprintf(UB_sbuf,"DELETING INSTANCE (KEY:%d) OF OLD MASTER (KEY:%d)",
					oldsymptr->inst[i].inst_key, oldsymptr->key);
		ubi_pscroll(UB_sbuf);
#endif
		/* this deletes subentities also */
		if (uc_delete(oldsymptr->inst[i].inst_key) != UU_SUCCESS)
					goto failed;
		
		if (tnodesleft > 0) /* then get rid of location instance */
		{
#if (TRACE)
			ubi_pscroll("DELETING HILITING INSTANCE");
#endif
			if (uc_delete(locinst.key) != UU_SUCCESS)
				goto failed;
			ur_free_app_data(&locinst);			
		}

		/**** DISPLAY THE NEW INSTANCE ****/
		if (ub_display_sym(&instance) != UU_SUCCESS)
				goto failed;
	}/* end with reassociation */
	oldsymptr->no_inst = 0;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**	 I_FUNCTION:int ubu_RedoInstsForNewMaster(olinstdataptr,tnodeindx,
**								newmasterptr,instanceptr)
**		This function creates an instance of the new master symbol in the 
**		correct orientation to replace a current instance of the old master
**		symbol. Note any old text nodes are already filled in and are properly
**		placed positioned with respect master symbol origin.
**	 PARAMETERS	
**		INPUT: 
**			olinstdataptr	Pointer to the instance data packet from the old
**								master symbol.
**			tnodeindx		Index into the text node list of the next text to get.
**			newmasterptr	Pointer to the new master symbol.
**		OUTPUT:  
**			instanceptr		Pointer to a new master's instance.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubu_RedoInstsForNewMaster(olinstdataptr,tnodeindx,newmasterptr,instanceptr)
	struct UB_inst_rec *olinstdataptr;
	int tnodeindx;
	struct UB_symbol_rec *newmasterptr;
	struct UB_instance_rec *instanceptr;
{
	/* struct UA_text_rec text;
	struct UA_textattr_rec textattr; */
	UU_LOGICAL redo;
	int cmdreject;
	int nbrtnodes;
	UU_LOGICAL textreqd, label;
	UM_transf tfmat, temptf;
	int desegid;
	UU_KEY_ID savoldkey;
	int i, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
	"ubu_RedoInstsForNewMaster(olinstdataptr,tnodeindx:%d,newmaster>key:%d,?)",
			tnodeindx,newmasterptr->key));
	/* instanceptr->master_key = newmasterptr->key; */

	/* update instance and make it displayable */
	if (ubi_get_instance_geom(newmasterptr, instanceptr) != UU_SUCCESS)
			goto failed;
	if (ubi_get_instance_snodes(newmasterptr, instanceptr) != UU_SUCCESS)
			goto failed;

	nbrtnodes = instanceptr->no_text_nod; /* number of current text nodes */
	redo = UU_TRUE;
	while (redo)
	{
		if (ubu_get_instance_tnodes(newmasterptr,tnodeindx,instanceptr,&cmdreject)
				!= UU_SUCCESS) goto failed;
		if (cmdreject) /* see what user wants to do */
		{
			if (ubu_process_cmdreject(newmasterptr,instanceptr, &redo) 
						!= UU_SUCCESS) goto failed;
		}
		else /* no cmdreject */
			redo = UU_FALSE;
	}/* while */

	/* get the transformation to orient this instance */
	if (ubi_get_instance_orientation(olinstdataptr, tfmat) != UU_SUCCESS) 
				goto failed;

	savoldkey = olinstdataptr->inst_key;
	if (ub_create_instance_tuple(newmasterptr->key,instanceptr,UB_DEFAULT_TF,
			&UB_symattr_default_bundle, olinstdataptr) != UU_SUCCESS) goto failed;
	olinstdataptr->inst_key = savoldkey; /* restore original key */

	/* fix the connector lists to use the new instance */
	if (ubi_update_inst_connectors(olinstdataptr, instanceptr) != UU_SUCCESS)
				goto failed;

	/* now transform the instance and UNIBASE record */
#if (TRACE)
	ubi_pscroll("TRANSFORMING INSTANCE CREATED FROM OLD INSTANCE");
	umi_print_transformation(tfmat);
#endif
	if (ub_transform_sym(instanceptr, tfmat, UU_TRUE) != UU_SUCCESS)
				goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}
