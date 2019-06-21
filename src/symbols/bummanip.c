/*********************************************************************
**    NAME:  bummanip.c
**       CONTAINS:
**      		ubu_list_unibase_symmaster
**				ubu_rename_symmaster
**				ubu_delete_symmaster
**				ubu_del_symmaster
**				ub_del_symmaster_tuple
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			bummanip.c , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			01/20/17 , 11:38:02
*********************************************************************/
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "dmark.h"		/* for UD_MARK */
#include "dasnog.h"		/* for DAS */
#include "dinput.h"		/* for DAS input types; e.g. UD_STRING */
#include "mdrel.h"		/* for relation numbers */
#include "class.h"		/* for "UC_" data types */
#include "xenv1.h"
#include "xfsys1.h"	
#include "bsym.h"
#include "udforms.h"

#define TRACE UU_FALSE /* for debugging only */


/*********************************************************************
**    E_FUNCTION :  int ubu_rename_symmaster()
**       This function renames a master symbol in UNIBASE.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none. 
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_rename_symmaster()
{
	struct UB_symbol_rec sym, sym2replace;
	UU_LOGICAL cmdreject; /* flag to indicate whether command reject hit */
	UU_LOGICAL found;	/* UU_TRUE iff the master symbol was found in UNIBASE */
/*
.....added for use list/form
.....Yurong 9/23/98
*/
	int *ans[2], len;
	UD_LIST symname_list;
	char **ub_get_symmaster_name();
	static char sym_name[UB_SYMBOL_NAME_LEN] = " ";
	static char new_sym[UB_SYMBOL_NAME_LEN] = "";
	int status = UU_SUCCESS; /* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_rename_symmaster()"));
	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE);
	if (cmdreject) goto done;	
/*
......use form with symbol list instead of
......prompt
......Yurong changed 9/22/98
*/
	len = strlen(sym_name);
	symname_list.answer = (char *) uu_malloc(UB_SYMBOL_NAME_LEN * sizeof(char));
	strcpy(symname_list.answer, sym_name);
	symname_list.item = (char **) 
								ub_get_symmaster_name(&(symname_list.num_item));
	if ((symname_list.item==NULL) || (symname_list.num_item==0))
	{
		ud_wrerr("No symbols in the UNIBASE");
		goto done;
	}
	ans[0] = (int *)&symname_list;
	ans[1] = (int *)new_sym;
	status = ud_form("brensym.frm", ans, ans);
	if (status==-1)
		goto done;
	ncl_parse_label(symname_list.answer,sym.label,&sym.subscr);
	if (ub_get_symmaster_by_name(&sym, &found,1,1) != UU_SUCCESS)
		goto failed;
  	if (!found) 
	{
		uu_uerror2(UB_SYMBOL, 25, sym.label, "ubu_rename_symmaster");
		/* error is: Can not find master symbol %s (%s). */
		goto done;
	}
	ncl_parse_label(new_sym,sym2replace.label,&sym2replace.subscr);
	if (ub_get_symmaster_by_name(&sym2replace, &found,1,1) != UU_SUCCESS)
		goto failed;
	if (found)
	{
/* 
.....prompt is:Master symbol, %s, is already in the data base;
.....overwrite? 
*/
		if (ud_yesno(0, uu_uprompt1(UB_SYMBOL, 21, new_sym), "Rename Symbol"))
		{
			if (ubu_ReplaceMaster(&sym2replace, &sym) != UU_SUCCESS)
				goto failed;
		}
		else 
			goto failed; 
	}
	uu_uerror0(UB_SYMBOL,98);
	/* error is: Warning, rename does not change symbol file names in libraries.
	 */
	/* name gotten, so do copy and save */
	strcpy(sym.label, new_sym);
	if (ur_update_data_fixed(&sym) != 0)
	{
		uu_uerror2(UB_SYMBOL, 42, sym.key, "ubu_rename_symmaster"); 
		/* error message: Can't update UNIBASE master symbol record having
		 * key = %d  (%s). */
		goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	ud_free_flist(&symname_list);
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :  int ubu_delete_symmaster()
**       This function deletes a master symbol from UNIBASE.
**			This is currently only called by the menu item "unload symbol".
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_delete_symmaster()
{
	struct UB_symbol_rec sym;
	UU_LOGICAL cmdreject;
	UU_LOGICAL found;	/* UU_TRUE iff master found in UNIBASE */
	int status;			/* return status; either UU_SUCCESS or UB_FAILURE */
/*
.....added for use list/form
.....Yurong 9/22/98
*/
	int *ans[1], len;
	UD_LIST symname_list;
	char **ub_get_symmaster_name();
	static char sym_name[UB_SYMBOL_NAME_LEN] = " ";

	uu_denter(UU_BTRC, (us, "ubu_delete_symmaster(numintptr)"));
	status = UU_SUCCESS; /* assume success */

	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)	/* then no command reject encountered */
	{
/*
......use form with symbol list instead of
......prompt
......Yurong changed 9/22/98
*/
		len = strlen(sym_name);
		symname_list.answer = (char *) uu_malloc(UB_SYMBOL_NAME_LEN *
			sizeof(char));
		strcpy(symname_list.answer, sym_name);
		symname_list.item = (char **) 
									ub_get_symmaster_name(&(symname_list.num_item));
		if ((symname_list.item==NULL) || (symname_list.num_item==0))
		{
			ud_wrerr("No symbols in the UNIBASE");
			goto done;
		}
		ans[0] = (int *)&symname_list;
		status = ud_form("bunldsym.frm", ans, ans);
		if (status==-1)
			goto done;
		ncl_parse_label(symname_list.answer,sym.label,&sym.subscr);

		/* name of master symbol gotten */
		if (ub_get_symmaster_by_name(&sym, &found,1,1) != UU_SUCCESS)
			goto failed;
		if (!found) 
		{
			uu_uerror2(UB_SYMBOL, 25, sym.label, "ubu_delete_symmaster");
			/* error is: Can not find master symbol %s (%s). */
			goto done;
		}

		if (ubu_del_symmaster(sym.key, UU_FALSE,UU_TRUE) != UU_SUCCESS)
			goto failed;

	}/* end no command reject */
	else
		goto done;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	ud_free_flist(&symname_list);
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :int ubu_del_symmaster(key, delSymFile,ask)
**       This function deletes a master symbol from UNIBASE, and all 
**			subentities are also deleted.
**    PARAMETERS   
**       INPUT  : 
**          key			The key of the master symbol UNIBASE record to be
**								deleted; note, all subentities are also deleted.
**				delSymFile  UU_TRUE iff the master symbol library file is also
**								to be deleted.
**				ask         UU_TRUE if user should be asked to delete instances.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems are encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_del_symmaster(key, delSymFile,ask)
	UU_KEY_ID key;
	UU_LOGICAL delSymFile,ask;
{
	struct UB_symbol_rec sym;
	char fullpath[UB_MAX_PATH_LEN];
	char descname[UB_MAX_PATH_LEN];
	char ag_name[UB_MAX_PATH_LEN];
	int mode, hdrfound,stat;
	int i, status = UU_SUCCESS;

	uu_denter(UU_BTRC, (us, "ubu_del_symmaster(key:%d,delSymFile:%d)", 
							key, delSymFile));
	sym.key = key;
	if (ub_retrieve_sym(&sym, sizeof(sym)) != UU_SUCCESS)
		goto failed;
	if (sym.no_inst > 0)
	{
		if (!ask) stat = UU_TRUE;
		else
		/* prompt is: To remove master, %s, from UNIBASE all instances 
		 * will be removed; ok? */
			stat = ud_yesno(0, uu_uprompt1(UB_SYMBOL,104,sym.label), "Question?");
		if (stat)
		{
			for (i=0; i<sym.no_inst; i++)
				if (uc_delete(sym.inst[i].inst_key) != UU_SUCCESS)
							goto failed;
			sym.no_inst = 0;
		}
		else goto failed;

	}
 	if (ub_del_symmaster_tuple(&sym) != UU_SUCCESS)
			goto failed;

	if (!delSymFile) goto done;

	/* now delete master symbol from library */

	mode = UX_READ | UX_WRITE; /* default check for read and write access */
	if (ux_file_inquire(UU_NULL, UU_NULL, sym.path, "UB_SYM_EXTEN",
		"UB_SYM_SUFFIX", &mode, &hdrfound, fullpath, UX_PRTERRS) 
		!= UU_SUCCESS) goto failed;

	if ((mode == (mode | UX_NEXISTS))  /* then the file not does not exist */
		|| (mode == (mode | UX_FAREA)))/* then the file is a directory */
	{
		uu_uerror2(UB_SYMBOL, 43, fullpath, "ubu_del_symmaster");
		/* error is: Master symbol does not exist in %s (%s). */
		goto failed;
	}
	if (ux_delete(fullpath, UX_PRTERRS) != UU_SUCCESS)
	{  /* then can't delete old version */
		uu_uerror2(UB_SYMBOL, 30, fullpath, "ubu_del_symmaster");
		/* error message: Can't delete version of master sym at: %s,
		 * (%s). */
		goto failed;
	}

	/* after successful delete on the symbol master file, remove
	the associated APPLIED GEOMETRY file */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, UU_NULL, sym.path, "UB_SYM_EXTEN",
		"UR_AG_FILE", &mode, &hdrfound, ag_name, UX_PRTERRS) 
		!= UU_SUCCESS) goto desc;
	if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
	{
		uu_dprint(UU_BTRC,(us,"This master has no associated AG file."));
	}
	else
		ux_delete(ag_name, UX_PRTERRS);

	/* after successful delete on the symbol master file, remove
	the associated description file if there is one */
desc:;
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, UU_NULL, sym.path, "UB_SYM_EXTEN",
		"UB_DESC_SUFFIX", &mode, &hdrfound, descname, UX_PRTERRS) 
		!= UU_SUCCESS) goto failed;
	if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
	{
		uu_dprint(UU_BTRC,(us,"This master has no associated description."));
	}
	else
		if (ux_delete(descname, UX_PRTERRS) != UU_SUCCESS) goto failed;
	goto done;

failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :int ub_del_symmaster_tuple(symptr)
**       This function deletes a master symbol from UNIBASE only.
**    PARAMETERS   
**       INPUT  : 
**          symptr		Pointer to the master symbol UNIBASE record to be
**								deleted; note, all subentities are also deleted.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems are encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_del_symmaster_tuple(symptr)
	struct UB_symbol_rec *symptr;
{
	int i, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ub_del_symmaster_tuple(symptr>key:%d)",symptr->key));
	if (symptr->no_inst != 0) /* don't delete if there are still instances */
	{
		uu_uerror2(UB_SYMBOL,99,symptr->label, "ub_del_symmaster_tuple");
		/* error is:Can't delete master, %s, instances of it exist, (%s) */
		goto failed;
	}
	/* delete the master symbol subentities */
	for (i=0; i<symptr->no_geom; i++)
		if (uc_delete_noseg(symptr->geom[i]) != UU_SUCCESS)
			goto failed;
	for (i=0; i<symptr->no_text_nod; i++)
		if (uc_delete_noseg(symptr->text_nod[i].text_key) != UU_SUCCESS)
			goto failed;
	for (i=0; i<symptr->no_snap_nod; i++)
		if (uc_delete_noseg(symptr->snap_nod[i].snap_key) != UU_SUCCESS)
			goto failed;
	/* delete master symbol record */
	if (ur_delete_all(symptr->key) != 0)
	{
		uu_uerror2(UB_SYMBOL, 37, symptr->key, "ub_del_symmaster_tuple");
		/* error message: Unable to delete master symbol with key=%d (%s). */
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
**    E_FUNCTION         :  ub_get_symmaster_name
**       get all the symbol names in UNIBASE
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT : number: number of symbol name
**    RETURNS      : a list of symbol name
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added by Yurong
.....9/22/98
*/
char **ub_get_symmaster_name(number)
int *number;
{
	int len;
	int nxtuple;
	struct UB_symbol_rec master;		/* master symbol record */
	char **symbol_name;

	*number = 0;
	nxtuple = 1;
	symbol_name = (char **) uu_malloc(UB_LIST_LEN*sizeof(char *));
	master.rel_num = UB_SYMBOL_REL;
	while (ur_get_next_data_key(master.rel_num, &nxtuple, &master.key) == 0)
	{
		nxtuple++;
		if (ub_retrieve_sym(&master, sizeof(struct UB_symbol_rec)) 
				!= UU_SUCCESS) break;
		if (master.label[0]!='@')
		{
			len = strlen(master.label);
			symbol_name[*number] =
				(char *)uu_malloc(UB_SYMBOL_NAME_LEN_PLUS*sizeof(char));
			ncl_get_label(&master,symbol_name[*number]);
			(*number)++;
			if (*number == UB_LIST_LEN) break;
		}
	}
	return symbol_name;
}

/*********************************************************************
**    E_FUNCTION         :  ub_get_symmaster_name_path
**       get all the symbol names (with path) in UNIBASE 
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT : number: number of symbol name
**    RETURNS      : a list of symbol name
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char **ub_get_symmaster_name_path(number)
int *number;
{
	int len;
	int nxtuple;
	char label[80];
	struct UB_symbol_rec master;		/* master symbol record */
	char **symbol_name, *position;

	*number = 0;
	nxtuple = 1;
	symbol_name = (char **) uu_malloc(UB_LIST_LEN*sizeof(char *));
	master.rel_num = UB_SYMBOL_REL;
	while (ur_get_next_data_key(master.rel_num, &nxtuple, &master.key) == 0)
	{
		nxtuple++;
		if (ub_retrieve_sym(&master, sizeof(struct UB_symbol_rec)) 
				!= UU_SUCCESS) break;
		len = strlen(master.label);
		symbol_name[*number] =
			(char *)uu_malloc(500*sizeof(char));
		ul_remove_quotes(master.path);
		strcpy(symbol_name[*number], master.path);
		position = rindex(symbol_name[*number], '.');
		if (position != UU_NULL) *position = '\0';
		(*number)++;
		if (*number == UB_LIST_LEN) break;
	}
	return symbol_name;
}

