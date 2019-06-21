/*********************************************************************
**    NAME:  bupload.c
**       CONTAINS:
**			ubu_chk_mastersyms()
**			ubu_chk2_reload(loadkey)
**			ubu_chk4_reload(oldkey,loadkey)
**			ubu_reloadMsym
**			ubu_load_and_update
**			ubu_process_cmdreject(newmasterptr,instanceptr, redoptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bupload.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:07
*********************************************************************/
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "dmark.h"		/* for UD_MARK and UD_UNMARK */
#include "mdrel.h"		/* for relation numbers */
#include "mdattr.h"		/* for definition of colors and UM_DISPLAYABLE*/
#include "mattr.h"		/* for geometry attributes, UM_textattr_rec */
#include "mdcoord.h"		/* for UM_transf */
#include "mcrv.h"			/* for UM_text_rec */
#include "xenv1.h"		/* used by "ux_" calls for UX_PRTERRS value */
#include "xfsys1.h"		/* for UX_R */
#include "bsym.h"
#include "atext.h"

#define TRACE UU_TRUE	/* for debugging only */
/*********************************************************************
**    E_FUNCTION: ubu_chk_mastersyms()
**			This function checks the master symbols in UNIBASE to see if
**       they are the most recent versions; those that are not are
**       reloaded and their instances are regenerated.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_chk_mastersyms()
{
	char *ux_getenv(), *index();
	int ur_get_next_new_data_key();
	int ur_get_next_old_data_key();
	UB_list loadednames[UB_LIST_LEN], oldnames[UB_LIST_LEN];
	UU_LOGICAL reload;
	struct UB_symbol_rec oldmaster, newmaster;
	int nbrloaded, nbrold, nxtlded, nxtold, stat, status = UU_SUCCESS;
	
	uu_denter(UU_BTRC,(us,"ubu_chk_mastersyms()"));
	if (ubi_reloadMasters(&reload) != UU_SUCCESS) goto failed;

	/* get newly loaded master symbols */
	if (ubi_get_namelist(ur_get_next_new_data_key,loadednames,&nbrloaded)
				!= UU_SUCCESS) goto failed;
	if (nbrloaded == 0) /* no master symbols loaded */
		goto done;

	if (ubi_get_namelist(ur_get_next_old_data_key,oldnames,&nbrold)
				!= UU_SUCCESS) goto failed;

	/* have both old and new master symbols; see if there are any in common */
	nxtlded = nxtold = 0;
	while (nxtlded < nbrloaded) /* check all newly loaded masters with old ones*/
	{
		if (nxtold < nbrold) /* there are BOTH loaded and old symbols to chk */
		{
			stat = strcmp(loadednames[nxtlded].name, oldnames[nxtold].name);
			if (stat > 0 ||		/* loaded name bigger */
			 (stat == 0 && loadednames[nxtlded].subscr > oldnames[nxtold].subscr))
					nxtold++; /* get next old index */
			else if (stat < 0 ||		/* old name bigger */
			 (stat == 0 && loadednames[nxtlded].subscr < oldnames[nxtold].subscr))
			{
				if (reload)
					if (ubu_chk2_reload(loadednames[nxtlded].key) != UU_SUCCESS)
							goto failed;
				nxtlded++;
			}
			else /* same symbol name in both old and new list */
			{
				/* see if both the old and the new master are identical and whether
				 * to reload. */
				if (reload)
				{
					if (ubu_chk4_reload(oldnames[nxtold].key,
							loadednames[nxtlded].key) != UU_SUCCESS) goto failed;
				}
				else /* don't reload; but find out which one to use, and use it */
				{
					oldmaster.key = oldnames[nxtold].key;
					newmaster.key = loadednames[nxtlded].key;
					if (ub_retrieve_sym(&oldmaster, sizeof(oldmaster)) != UU_SUCCESS)
						goto failed;
					if (ub_retrieve_sym(&newmaster, sizeof(newmaster)) != UU_SUCCESS)
						goto failed;
					if (ubu_2msymsSameNameNoAutoLoad(&oldmaster, &newmaster)
						!= UU_SUCCESS) goto failed;
				}
				nxtold++; 
				nxtlded++;
			}
		}
		else /* there is only newly loaded symbols to check */
		{
			if (reload)
				if (ubu_chk2_reload(loadednames[nxtlded].key) != UU_SUCCESS)
							goto failed;
			nxtlded++;
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
**    I_FUNCTION: int ubu_chk2_reload(loadkey)
**			This function checks to determine if a master symbol needs
**			to be reloaded from it's master symbol file. If so, the
**			reloading is done here.
**    PARAMETERS   
**		INPUT: 
**			loadkey		Key of the master symbol that has just been loaded.
**		OUTPUT:  none.
**    RETURNS: UU_SUCESS if no problems encountered; otherwise UB_FAILURE.
**    SIDE EFFECTS: Potentially substantial rearrangements in UNIBASE.
**    WARNINGS: none
*********************************************************************/
int ubu_chk2_reload(loadkey)
	UU_KEY_ID loadkey;
{
	struct UB_symbol_rec loadedsym;
	char libpath[UB_MAX_PATH_LEN]; /* currently not used */
	int cmdreject;
	int mode, whatToDo, version, status = UU_SUCCESS; 

	uu_denter(UU_BTRC,(us,"ubu_chk2_reload(loadkey:%d)",loadkey));
	UD_MARK(cmdreject, UU_FALSE); /* this allows command reject to be caught */

	if (cmdreject < 0) /* this test is for the "uu_sys_err_recovery" at
		 * at the end of this function; that function call will cause 
		 * execution to begin at the last "MARK" which probably will be the
		 * above "UD_MARK" call; thus an infinite loop!! */
		/* error is: Error updating master sym, %s; data base inconsistency 
		 * req'd a reset; NOTE WON'T COME BACK TO HERE EVER!! */
		uu_sys_err_recovery(-1,UB_SYMBOL,104, UB_MSYMNAME(&loadedsym),0);
	else if (cmdreject > 0) /* then got a normal command reject */
			goto done;

	loadedsym.key = loadkey;
	if (ub_retrieve_sym(&loadedsym, sizeof(loadedsym)) != UU_SUCCESS)
			goto failed;

	/* get path to master symbol and/or what to do if path not given */
	mode = UX_READ;
	uu_dprint(UU_BTRC,(us,"before &whatToDo:%x", (&whatToDo)));
	if (ubu_updateMsymPaths(UB_MSYMNAME(&loadedsym), &mode, libpath, 
				UB_MSYMPATH(&loadedsym), &whatToDo) != UU_SUCCESS) 
	{
		uu_dprint(UU_BTRC,(us,"going to failed"));
		goto failed;
	}
	uu_dprint(UU_BTRC,(us,"not going to failed"));
	uu_dprint(UU_BTRC,(us,"loadedsym.label:%s", loadedsym.label));
	uu_dprint(UU_BTRC,(us,"mode:%d", mode));
	uu_dprint(UU_BTRC,(us,"libpath:%s", libpath));
	uu_dprint(UU_BTRC,(us,"path:%s", loadedsym.path));
	uu_dprint(UU_BTRC,(us,"after &whatToDo:%x", (&whatToDo)));
	uu_dprint(UU_BTRC,(us,"after whatToDo:%d", whatToDo));
	/* loadedsym now has the correct path */
	switch (whatToDo)
	{
		case UB_CHKLOADEDMSYM:
			if (ubu_1Msym2ChkReload(&loadedsym) != UU_SUCCESS)
					goto failed;
			break;
		case UB_USEMSYMFROMPART:
			uu_dprint(UU_BTRC,(us,"use msym in part file regardless of libpath"));
			break;
		case UB_DONTUSE:
			uu_dprint(UU_BTRC,(us,"don't use this msym at all"));
		default:
			uu_dprint(UU_BTRC,(us,"unknown return flag"));
			goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
	/* error is: Error updating master sym, %s; data base inconsistency 
	 * req'd a reset. */
	uu_sys_err_recovery(-1,UB_SYMBOL,104, UB_MSYMNAME(&loadedsym),0);
done:;
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION: ubu_1Msym2ChkReload(loadedsymptr) 
**			This function checks to see if the master pointed to by "loadedsymptr"
**			needs to be reloaded.
**    PARAMETERS   
**       INPUT  : 
**				loadedsymptr		Pointer to the master symbol to be checked.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubu_1Msym2ChkReload(loadedsymptr) 
	struct UB_symbol_rec *loadedsymptr; 
{
	UU_LOGICAL ud_lyesno();
	struct UB_symbol_rec *msym2replace[1]; /* array of pointers */
	struct UB_symbol_rec newmaster;
	int version, status = UU_SUCCESS; 

	uu_denter(UU_BTRC, (us,"ubu_1Msym2ChkReload(loadedsymptr>key:%d [%s])",
								loadedsymptr->key, loadedsymptr->label));
	if (ux_get_fversion(UB_MSYMPATH(loadedsymptr), &version, UX_NPRTERRS) 
							!= UU_SUCCESS)
	{
		uu_uerror2(UB_SYMBOL,78,UB_MSYMNAME(loadedsymptr),
							UB_MSYMPATH(loadedsymptr));
		/* error is:Can't get version of file for master, %s, at %s; no auto 
		 * load done. */
		if (ud_lyesno(UB_SYMBOL,115))
			/* prompt is: Use currently loaded master version without 
			 * updating? */
			goto done;
		else goto failed;
	}
#if (TRACE)
	sprintf(UB_sbuf,"version in master just loaded:%d, file version:%d",
					loadedsymptr->version, version);
	ubi_pscroll(UB_sbuf);
#endif
	if (loadedsymptr->version != version) /* then update "loadedsym" instances */
	{
		msym2replace[0] = loadedsymptr;
		/* don't prompt for lib */
		if (ubu_reloadMsym(msym2replace, 1, UU_FALSE, &newmaster) != UU_SUCCESS)
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
**    I_FUNCTION: int ubu_chk4_reload(oldkey,loadkey)
**			This function checks to determine if a master symbol needs
**			to be reloaded from it's master symbol file. If so, the
**			reloading is done here.
**    PARAMETERS   
**		INPUT: 
**			oldkey		Key of the master symbol currently being used
**							in UNIBASE.
**			loadkey		Key of the master symbol that has just been loaded.
**		OUTPUT:  none.
**    RETURNS: UU_SUCESS if no problems encountered; otherwise UB_FAILURE.
**    SIDE EFFECTS: Potentially substantial rearrangements in UNIBASE.
**    WARNINGS: none
*********************************************************************/
int ubu_chk4_reload(oldkey,loadkey)
	UU_KEY_ID oldkey;
	UU_KEY_ID loadkey;
{
	char *uu_uprompt1();
	struct UB_symbol_rec oldsym;
	struct UB_symbol_rec loadedsym;
	struct UB_symbol_rec *msym2replace[2]; /* array of pointers */
	char libpath[UB_MAX_PATH_LEN]; /* currently not used */
	UU_LOGICAL inheritext;
	int mode, whatToDo, version, cmdreject, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubu_chk4_reload(oldkey:%d, loadkey:%d)",
					oldkey,loadkey));
	UD_MARK(cmdreject, UU_FALSE); /* this allows command reject to be caught */

	if (cmdreject < 0) /* this test is for the "uu_sys_err_recovery" at
		 * at the end of this function; that function call will cause 
		 * execution to begin at the last "MARK" which probably will be the
		 * above "UD_MARK" call; thus an infinite loop!! */
		/* error is: Error updating master sym, %s; data base inconsistency 
		 * req'd a reset; NOTE WON'T COME BACK TO HERE EVER!! */
		uu_sys_err_recovery(-1,UB_SYMBOL,104, UB_MSYMNAME(&loadedsym),0);
	else if (cmdreject > 0) /* then got a normal command reject */
			goto done;

	oldsym.key = oldkey;
	loadedsym.key = loadkey;
	if (ub_retrieve_sym(&oldsym, sizeof(oldsym)) != UU_SUCCESS)
			goto failed;
	if (ub_retrieve_sym(&loadedsym, sizeof(loadedsym)) != UU_SUCCESS)
			goto failed;
	mode = UX_READ;
	if (ubu_updateMsymPaths(UB_MSYMNAME(&loadedsym), &mode, libpath, 
				UB_MSYMPATH(&loadedsym), &whatToDo) != UU_SUCCESS) goto failed;
	/* "libpath" and "loadedsym.path" are now correct */
	switch (whatToDo)
	{
		case UB_CHKLOADEDMSYM:
			if (ubu_2SameNameChk2Reload(&oldsym, &loadedsym) != UU_SUCCESS)
					goto failed;
			break;
		case UB_USEMSYMFROMPART: 
			/* we are going to use the master symbol in the part file; see what
			 * user wants to do with the text nodes of current instances of the
			 * old master of the same name. */
			msym2replace[0] = &oldsym;
			/* prompt is: Updating instances of %s; shall current text nodes be
			 * reused? */
			if (ud_yesno(0, uu_uprompt1(UB_SYMBOL, 35, msym2replace[0]->label), "Question?"))
				inheritext = UU_TRUE;
			else inheritext = UU_FALSE;
			if (ubu_reassociateInstances(msym2replace, 1, &loadedsym, 
							inheritext) != UU_SUCCESS) goto failed;
			break;
		case UB_DONTUSE:
		default:
			goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
	/* error is: Error updating master sym, %s; data base inconsistency 
	 * req'd a reset. */
	uu_sys_err_recovery(-1, UB_SYMBOL,104, UB_MSYMNAME(&loadedsym),0);
done:;
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION :  int ubu_2SameNameChk2Reload(oldsymptr, loadedsymptr)
**			This function checks to if the master symbols, "oldsymptr" and 
**			"loadedsymptr", each having the same name are in deed the same.
**			If not then a new version maybe loaded from a master symbol file.
**    PARAMETERS   
**       INPUT  : 
**				oldsymptr		Pointer to the master symbol that was in Unibase
**									prior to the load.
**				loadedsymptr	Pointer to the master symbol that is in the part
**									file.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : instances of these masters are likely to be reassociated
**							with a different master.
**    WARNINGS     : none
*********************************************************************/
int ubu_2SameNameChk2Reload(oldsymptr, loadedsymptr)
	struct UB_symbol_rec *oldsymptr;
	struct UB_symbol_rec *loadedsymptr;
{
	struct UB_symbol_rec *msym2replace[2]; /* array of pointers */
	struct UB_symbol_rec newmaster;
	char libpath[UB_MAX_PATH_LEN]; /* currently not used */
	UU_LOGICAL found, inheritext;
	int version, status = UU_SUCCESS;

	uu_denter(UU_BTRC, (us, 
	 "ubu_2SameNameChk2Reload(oldsym>key:%d,loadedsym>key:%d, [%s])",
		oldsymptr->key, loadedsymptr->key, oldsymptr->label));

	if (ubu_getOldMsymFileVersion(oldsymptr,loadedsymptr,&found,
				&version) != UU_SUCCESS) goto failed;
	if (!found) goto done; /* everything already taken care of */

	if (strcmp(oldsymptr->path, loadedsymptr->path) != 0)
	{	/* then we assume these 2 master symbols are not the same; we
		 * assume "oldsym" has the correct path. */
		if (oldsymptr->version != version) 
		{	/* then must replace both old and new sym with version in
			 * library */
			msym2replace[0] = oldsymptr;
			msym2replace[1] = loadedsymptr;
			/* ask for the symbol library to use */
			if (ubu_reloadMsym(msym2replace, 2, UU_TRUE, &newmaster) != UU_SUCCESS)
					goto failed;
		}
		else /* "oldsym" is up-to-date; so just replace "loadedsym" */
		{
			msym2replace[0] = loadedsymptr;
			inheritext = UU_TRUE;
			if (ubu_reassociateInstances(msym2replace, 1, oldsymptr, inheritext)
					!= UU_SUCCESS) goto failed;
		}
	}
	else /* the paths for "oldsym" and "loadedsym" are the same; make
			* sure both are current versions */
	{
		if (oldsymptr->version != version) /* then update "oldsym" instances */
			if (loadedsymptr->version == version) /* then reassociate */
			{
				msym2replace[0] = oldsymptr;
				inheritext = UU_TRUE;
				if (ubu_reassociateInstances(msym2replace,1,loadedsymptr,inheritext)
						!= UU_SUCCESS) goto failed;
			}
			else /* both are incorrect versions */
			{
				msym2replace[0] = oldsymptr;
				msym2replace[1] = loadedsymptr;
				/* don't ask for symbol library */
				if (ubu_reloadMsym(msym2replace, 2, UU_FALSE, &newmaster) 
						!= UU_SUCCESS) goto failed;
			}
		else /* no need to update "oldsym"; just give "oldsym" new instances */
		{
			msym2replace[0] = loadedsymptr;
			inheritext = UU_TRUE;
			if (ubu_reassociateInstances(msym2replace, 1, oldsymptr, inheritext)
					!= UU_SUCCESS) goto failed;
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
**    I_FUNCTION:int ubu_reloadMsym(msym2replace, nbr, ask4Lib, newmasterptr)
**			This function reloads master symbols from it's library file.
**    PARAMETERS   
**		INPUT: 
**			msym2replace	Array of pointers to master symbol entities
**								with the same name that are to be replaced by
**								the master symbol definition in the library file.
**			nbr				Number of valid entries in "msym2replace".
**								Currently is no more than 2.
**		OUTPUT:  
**			newmasterptr	Pointer to the new master.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_reloadMsym(msym2replace, nbr, ask4Lib, newmasterptr)
	struct UB_symbol_rec *msym2replace[];
	int nbr;
	UU_LOGICAL ask4Lib;
	struct UB_symbol_rec *newmasterptr;
{
	char libpath[UB_MAX_PATH_LEN];
	char *prompt, *uu_uprompt1();
	UU_LOGICAL found;
	int i, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
			"ubu_reloadMsym(msym2replace,nbr:%d,ask4Lib:%d,?)",nbr,ask4Lib));
	if (nbr <= 0) goto done;
	/* make sure the master symbols in "msym2replace" are marked "old" in
	 * UNIBASE so that we don't mistake them for the new master we are going to
	 * load. */
	for (i=0; i<nbr; i++)
		if (ur_mark_old((msym2replace[0])->key) != 0)
		{
			uu_uerror1(UB_SYMBOL, 83, "ubu_reloadMsym");
			/* error is:Can't mark currently loaded master symbols (%s). */
			goto failed;
		}

	/***** load a new version of the master symbol *****/
	if (ask4Lib)
	{
		prompt = uu_uprompt1(UB_SYMBOL, 107, (msym2replace[0])->label);
		/* prompt is: Must reload master sym, %s; give lib (for sys area, 
		 * prefix \"sys\") */
		if (ubu_ask4Libpath(prompt, libpath, &found) != UU_SUCCESS) goto failed;
		if (!found) goto failed;
	}
	else /* take libpath from master to update */
	{
		char oldName[UB_SYMBOL_NAME_LEN_PLUS]; /* not used */
		if (ux_decompose_path(UB_MSYMPATH((msym2replace[0])),libpath,oldName,
						UX_PRTERRS | UX_QUOTES) != UU_SUCCESS) goto failed;
	}
 	if (ubu_load_and_update(UU_NULL, libpath, msym2replace, nbr, "FIXEDONLY", 
						newmasterptr) != UU_SUCCESS) goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION :ubu_load_and_update(area,libname,msym2replace,nbr,
**								option,newmasterptr)
**			This function attempts to load a master symbol into UNIBASE given
**			the file area, library name, and any currently loaded master symbols
**			of the same name as the one to load.  The currently loaded masters are
**			in "msym2replace". Note the UNIBASE bit map is NOT reset here.
**    PARAMETERS   
**       INPUT  : 
**				area				Designation of which file area to use; this is
**									either "local", "system", or can be UU_NULL.
**									In the later case, the area name resides in 
**									"libname".
**				libname			Symbol Library name.
**				msym2replace	Array of pointers to master symbol entities
**									with the same name that are to be replaced by
**									the master symbol definition in the library file.
**				nbr				Number of valid entries in "msym2replace".
**									Currently is no more than 2.
**				option			If "WHOLESYM" then the entire new master symbol is
**									returned; otherwise, only the fixed part is returned.
**       OUTPUT : 
**				newmasterptr	Pointer to the master symbol loaded.
**    RETURNS: UU_SUCCESS if no problems encountered; otherwise UB_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : Note, if the root master symbol can't be found in the
**							symbol file, then all master symbols marked "new" in
**							UNIBASE will be deleted.
**							The pointer "newmasterptr" can not be a pointer in
**							"msym2replace".
*********************************************************************/
int ubu_load_and_update(area, libname, msym2replace, nbr, option, newmasterptr)
	char *area;
	char *libname;
	struct UB_symbol_rec *msym2replace[];
	int nbr;
	char *option;
	struct UB_symbol_rec *newmasterptr;
{
	int ur_lp02b();
	char *uu_uprompt1(), *strcat();
	char templib[UB_MAX_PATH_LEN];
	UU_LOGICAL inheritext;
	int i,status = UU_SUCCESS;
	
	uu_denter(UU_BTRC,(us,"ubu_load_and_update(%x,%s,msym2replace,nbr:%d,%s,?)",
					area, libname, nbr, option));
	if (area != UU_NULL)
		if (strcmp(area, "system") == 0)/* then add "sys:" to lib path for note */
		{
			strcpy(templib, "sys:");
			strcat(templib, libname);
		}
		else strcpy(templib, libname);
	uu_uerror2(UB_SYMBOL, 105, msym2replace[0]->label, templib);
	/* message is (this is not an error): Note, reloading %s from lib %s. */

	/* prompt is: Updating instances of %s; shall current text nodes be 
	 * reused? */
	if (ud_yesno(0, uu_uprompt1(UB_SYMBOL, 35, msym2replace[0]->label), "Question?"))
		inheritext = UU_TRUE;
	else inheritext = UU_FALSE;
	/* note, the function "ur_lp02b" does not reset the UNIBASE bit map */
	if (ubi_load_file(area, libname, (msym2replace[0])->label, ur_lp02b, option,
						newmasterptr,UX_PRTERRS) != UU_SUCCESS) goto failed;

	uu_dprint(UU_MTRC,(us,"newmaster: key=%d, name=%s",newmasterptr->key,
								newmasterptr->label));

	/* newly loaded master symbol whose instances are to replace those
	 * of the master symbols in "msym2replace" has been retrieved */
	if (ubu_reassociateInstances(msym2replace, nbr, newmasterptr, inheritext)
			!= UU_SUCCESS) goto failed;
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION : ubu_reassociateInstances(msym2TakeFrom, nbr, msym2Give2ptr, 
**											inheritext)
**				This function reassociates the instances that are currently 
**				associated with the master symbols in "msym2TakeFrom" so that
**				these instances will be associated with "msym2Give2ptr".
**    PARAMETERS   
**       INPUT  : 
**				msym2TakeFrom		Array of pointers of master symbol entities to
**										have their instances reassociated.
**				nbr					Number of valid entries in "msym2TakeFrom".
**				msym2Give2ptr		Pointer to the master symbol the instances are to
**										be reassociated with.
**				inheritext			UU_TRUE iff the text node data from the instances
**										being reassociated should be inherited.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubu_reassociateInstances(msym2TakeFrom, nbr, msym2Give2ptr, inheritext)
	struct UB_symbol_rec *msym2TakeFrom[];
	int nbr;
	struct UB_symbol_rec *msym2Give2ptr;
	UU_LOGICAL inheritext;
{
	int oltnodes2use[2];
	int i, status = UU_SUCCESS;	

	uu_denter(UU_BTRC, (us, 
		"ubu_reassociateInstances(msym2TakeFrom,msym2Give2ptr,inheritext:%d)",
			inheritext));

	if (inheritext)
		for (i=0; i<nbr; i++)
			if ((msym2TakeFrom[i])->no_text_nod >= msym2Give2ptr->no_text_nod)
				/* then fill in all the text nodes in the new instances with the
				 * first portion of those in the current instances */
				oltnodes2use[i] = msym2Give2ptr->no_text_nod;
			else /* the new master has more text nodes than the old master */
				oltnodes2use[i] = (msym2TakeFrom[i])->no_text_nod;
	else /* don't inherit any text */
		for (i=0; i<nbr; i++)
			oltnodes2use[i] = 0;

	for (i=0; i<nbr; i++) /* change old instance's to those of new master */
	{
		if (ubu_assoc_old_inst_new_msym(msym2TakeFrom[i], oltnodes2use[i], 
						msym2Give2ptr) != UU_SUCCESS) goto failed;
		/* old instances are now deleted; so delete old master */
		uu_dprint(UU_BTRC,(us,
					"DELETE MASTER WHOSE INSTANCES HAVE BEEN REASSOC'ED"));
		msym2TakeFrom[i]->no_inst = 0; /* should be no instances by now */
		if (ub_del_symmaster_tuple(msym2TakeFrom[i]) != UU_SUCCESS)
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
**    I_FUNCTION:int ubu_process_cmdreject(newmasterptr,instanceptr, redoptr)
**		This function processes any command reject from ubu_RedoInstsForNewMaster.
**    PARAMETERS   
**		INPUT: 
**			newmasterptr	Pointer to the new master symbol.
**		OUTPUT:  
**			instanceptr		Pointer to a new master's instance.
**			redoptr			Pointer to UU_TRUE iff the text should be redone in
**								"ubu_RedoInstsForNewMaster".
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_process_cmdreject(newmasterptr,instanceptr, redoptr)
	struct UB_symbol_rec *newmasterptr;
	struct UB_instance_rec *instanceptr;
	UU_LOGICAL *redoptr;
{
	char *uu_uprompt0();
/*--- new text
	struct UM_text_rec text;
	struct UM_textattr_rec textattr;
---*/
	struct UA_txt_rec text;
	struct UA_txtattr_rec textattr;
	int nbrtnodes=0;
	UU_LOGICAL textreqd, label;
	int cmdreject, i, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubu_process_cmdreject(newmasterptr>key:%d,?,?)",
					newmasterptr->key));
	status = UU_SUCCESS; /* assume success */
		if (!ud_yesno(0, uu_uprompt0(UB_SYMBOL, 34), "Question?"))
		/* prompt is:Redo this instance? Otherwise, required text will be 
		 * \"***\" in this instance. */
		{
			*redoptr = UU_FALSE;
			instanceptr->no_text_nod = 0;
			for (i=0; i<newmasterptr->no_text_nod; i++)
			{
				if (ubi_get_textnode_type(&(newmasterptr->text_nod[i]),
						&textreqd, &label) != UU_SUCCESS) goto failed;
				if (textreqd) /* then fill-in with "***" */
				{
					ur_setup_data(UA_TEXT_REL, &text, sizeof(text));
					ur_setup_data(UA_TEXTATTR_REL, &textattr, sizeof(textattr));
					if (ubi_setup_textnode(&(newmasterptr->text_nod[i]),&text,
								&textattr) != UU_SUCCESS) goto failed;
					strcpy(text.tchar, "***");
					
					if (ubi_create_text_node(newmasterptr,&text,&textattr,i,
								instanceptr) != UU_SUCCESS) goto failed;
				}						
			}/* end for */
		}/* end ud_yesno */
		else /* redo the text nodes of this instance */
		{
			instanceptr->no_text_nod = nbrtnodes;
			*redoptr = UU_TRUE;
			/* ought to get rid of current text node list for instance */
		}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION : int ubu_getOldMsymFileVersion(oldsymptr, loadedsymptr, 
**									foundptr, versionptr) 
**			This function gets the file version of the master symbol file,
**			if it can obtain it.
**    PARAMETERS   
**       INPUT  : 
**				oldsymptr		Pointer to the master symbol in Unibase when load
**									began.
**				loadedsymptr	Pointer to the master symbol in the part file just
**									loaded.
**       OUTPUT :  
**				foundptr			Pointer to UU_TRUE iff the version of the file was
**									obtained.
**				versionptr		Pointer to the version number if found.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : Instances maybe reassociated here.
**    WARNINGS     : none
*********************************************************************/
int ubu_getOldMsymFileVersion(oldsymptr, loadedsymptr, foundptr, versionptr) 
	struct UB_symbol_rec *oldsymptr;
	struct UB_symbol_rec *loadedsymptr;
	UU_LOGICAL *foundptr;
	int *versionptr;
{
	char *uu_uprompt1();
	struct UB_symbol_rec *msym2replace[2];
	UU_LOGICAL inheritext;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC, (us, 
		"ubu_getOldMsymFileVersion(oldsym>key:%d,loadedsym:%d,?,?)"));

	if (ux_get_fversion(oldsymptr->path, versionptr, UX_PRTERRS) != UU_SUCCESS)
	{
		*foundptr = UU_FALSE;
		uu_uerror2(UB_SYMBOL,78,UB_MSYMNAME(oldsymptr),UB_MSYMPATH(oldsymptr));
		/* error is:Can't get version of file for master, %s, at %s; no auto 
		 * load done. */
		if (ud_yesno(0, uu_uprompt1(UB_SYMBOL,116,UB_MSYMNAME(oldsymptr)), "Question?"))
			/* prompt is: 2 masters named %s loaded; use newest loaded?*/
		{
			msym2replace[0] = oldsymptr;
			inheritext = UU_TRUE;
			if (ubu_reassociateInstances(msym2replace,1,loadedsymptr,inheritext)
					!= UU_SUCCESS) goto failed;
		}
		else
		{
			msym2replace[0] = loadedsymptr;
			inheritext = UU_TRUE;
			if (ubu_reassociateInstances(msym2replace, 1, oldsymptr, inheritext)
					!= UU_SUCCESS) goto failed;
		}
		goto done;
	}
	else *foundptr = UU_TRUE;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    I_FUNCTION: ubu_2msymsSameNameNoAutoLoad(oldmasterptr, newmasterptr)
**			This function determines which master symbol to use if 2 of the same
**			name are loaded and auto load is NOT enabled.
**    PARAMETERS   
**       INPUT  : 
**				oldmasterptr	Pointer to the old master symbol with the same
**									name as the new one.
**				newmasterptr	Pointer to the newly loaded master symbol.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : Reassociates the instances of one of the master symbols
**							to the other.
**    WARNINGS     : none
*********************************************************************/
int ubu_2msymsSameNameNoAutoLoad(oldmasterptr, newmasterptr)
	struct UB_symbol_rec *oldmasterptr;
	struct UB_symbol_rec *newmasterptr;
{
	char *uu_uprompt1(), *strcat();
	struct UB_symbol_rec *msym2replace[2], *finalMsymptr;
	char promptStrg[UB_SYMBOL_NAME_LEN+10];
	UU_LOGICAL inheritext;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
		"ubu_2msymsSameNameNoAutoLoad(oldmastr>key:%d,newmastr>key:%d [%s])",
					oldmasterptr->key, newmasterptr->key,UB_MSYMNAME(newmasterptr)));
	if (oldmasterptr->version == newmasterptr->version)
	{  /* merely reassociate the new master instances with the old master */
		msym2replace[0] = newmasterptr;
		inheritext = UU_TRUE;
		if (ubu_reassociateInstances(msym2replace,1,oldmasterptr,inheritext)
				!= UU_SUCCESS) goto failed;
		goto done;
	}
	if (ud_yesno(0, uu_uprompt1(UB_SYMBOL,116,UB_MSYMNAME(oldmasterptr)), "Question?"))
		/* prompt is: 2 masters named %s loaded; use newest loaded? */
	{
		msym2replace[0] = oldmasterptr;
		finalMsymptr = newmasterptr;
		strcpy(promptStrg, "old ");
	}
	else /* use old master */
	{
		msym2replace[0] = newmasterptr;
		finalMsymptr = oldmasterptr;
		strcpy(promptStrg, "new ");
	}
	strcat(promptStrg, msym2replace[0]->label);
	/* prompt is: Updating instances of %s; shall current text nodes be 
	 * reused? */
	if (ud_yesno(0, uu_uprompt1(UB_SYMBOL, 35, promptStrg), "Question?"))
		inheritext = UU_TRUE;
	else inheritext = UU_FALSE;

	if (ubu_reassociateInstances(msym2replace,1,finalMsymptr,inheritext)
				!= UU_SUCCESS) goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

