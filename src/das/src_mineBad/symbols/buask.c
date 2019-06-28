/*********************************************************************
**    NAME:  buask.c
**       CONTAINS:
**			 ubu_ask_for_symmaster
**			 ubu_ask_to_overwrite
**			 ubu_ask_for_target_sym
**			 ubu_ask4LibOrMsym2Use
**			 ubu_ask4Libpath(prompt, libpath, foundptr)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       buask.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:05
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) buask.c 3.9 9/27/88 16:38:03 single"};
#else
static char uu_sccsident[]={"@(#) buask.c 3.9 9/27/88 16:38:03 double"};
#endif
#endif
#include "usysdef.h"	 /* for UU_REAL, etc. */
#include "uhep.h"		 /* for error system */
#include "udebug.h"	  /* for debugging trace facility */
#include "dmark.h"		/* for UD_MARK */
#include "dasnog.h"	  /* for DAS */
#include "dinput.h"	  /* for DAS input types; e.g. UD_STRING */
#include "mdrel.h"		/* for UB_SYMBOL_REL */
#include "xenv1.h"
#include "xfsys1.h"
#include "xfsys2.h"
#include "bsym.h"

#define TRACE UU_TRUE  /* for debugging only */
/*********************************************************************
**	 E_FUNCTION:int ubu_ask_for_symmaster(symname, numintptr)
**		 This function gets a master symbol name from the user. 
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT :
**		 symname	  	Name of master symbol.
**		 numintptr	Pointer to the number of appropriate user interactions;
**						 either 0 or 1 in this case.
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: No checking is done as to whether the path name is valid.
*********************************************************************/
int ubu_ask_for_symmaster(symname, numintptr)
	char symname[UB_SYMBOL_NAME_LEN];	/* retruns the master symbol name */
	int *numintptr;	/* number of user interactions */
{
	int cmdreject;
	UU_LOGICAL nameok;
	int status = UU_SUCCESS; /* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_ask_for_symmaster(%x,numintptr)", symname));
	status = UU_SUCCESS; /* assume success */
	*numintptr = 0;	/* no user interactions yet to return */
	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)	/* then no command reject encountered */
	{
		ud_ldas(UD_DASSTRING, UB_SYMBOL, 1, symname, UB_SYMBOL_NAME_LEN,
					numintptr, UD_NODEFAULT);
		/* prompt is: Enter symbol name. */
		if (*numintptr <= 0) goto done;
		if (ubu_symmaster_name_ok(symname, &nameok) != UU_SUCCESS)
					goto failed;
		if (nameok) 
			/* name of master symbol gotten */
			*numintptr = 1;	/* we now have an appropriate user response */
	}/* end no command reject */
	else /* command reject hit */
	{}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}  

/*********************************************************************
**	 E_FUNCTION : int ubu_ask_to_overwrite(fullname)
**		 This function determines if there is a master symbol already in
**		 UNIBASE that has the same name as the master symbol stored in the
**		 save/load file whose path name is "fullname"; if there is then
**		 the user is asked if the current UNIBASE master symbol should
**		 be overwritten.
**	 PARAMETERS	
**		 INPUT  : 
**			 fullname	 Path name to the save/load file containing the master
**							 symbol.
**		 OUTPUT : none. 
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubu_ask_to_overwrite(fullname)
	char *fullname;
{
	int mode;
	char *uu_uprompt1();
	struct UB_symbol_rec sym;
	UU_LOGICAL found;
	char fname[UB_SYMBOL_NAME_LEN_PLUS],symname[UB_SYMBOL_NAME_LEN_PLUS];
	char farea[UB_MAX_PATH_LEN];
	char *rindex(), *position;
	int status = UU_SUCCESS; /* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_ask_to_overwrite(%s)",fullname));
	if (ux_decompose_path(fullname,farea,fname,UX_PRTERRS|UX_NQUOTES)
		!= UU_SUCCESS) goto failed;
	strcpy(symname, fname);
	/* take off any suffix */
	position = rindex(symname, '.');
	if (position != UU_NULL)	/* strip everything following */
		*position = '\0';
	ncl_parse_label(symname,sym.label,&sym.subscr);

#if (UU_COMP == UU_VAXVMS)
	UB_TO_UPPER(sym.label)
#endif
	if (ub_get_symmaster_by_name(&sym, &found, 1,1) != UU_SUCCESS) 
		goto failed;

	if (found) /* "sym" has old key now */
	{	/* then a master symbol by this name already exists, so ask if it 
		 * should be over written. */
		if (!ud_yesno(0, uu_uprompt1(UB_SYMBOL, 21, sym.label), "Overwrite master symbol?"))
			/* prompt is: UNIBASE already contains a master symbol named:
			 * %s; overwrite? */
			goto done;
		if (ubu_del_symmaster(sym.key, UU_FALSE,UU_TRUE) != UU_SUCCESS) 
				goto done;	/* delete current one */
	}
	if (ur_lp02(fullname) != 0)
	{
		uu_uerror2(UB_SYMBOL, 42, fullname, "ubu_ask_to_overwrite");
		/* error message: Error in loading master symbol: %s into 
		 * UNIBASE (%s). */
		goto failed;
	}

	/* handle APPLIED GEOMETRY file here for now */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
/*	ux_file_inquire(UU_NULL, farea, sym.label, UU_NULL,
		"UR_AG_FILE", &mode, &hdrfound, ag_name, UX_PRTERRS); 
	um_load_appgeo(ag_name, UU_FALSE);
	um_post_load_appgeo();*/

	if (ubi_FixMsymsAfterMsymLoad(UU_NULL,farea,sym.label,"NOOUTPUT",UU_NULL)
			!= UU_SUCCESS) goto failed;
/*
.....Update entity attributes
.....after intial symbol load
*/
	ubi_update_symbol(symname);
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  

/*********************************************************************
**	 E_FUNCTION:int ubu_ask_for_target_sym(symname, numintptr)
**		 This function gets a master symbol name from the user. 
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT :
**		 symname	  	Name of master symbol.
**		 numintptr	Pointer to the number of appropriate user interactions;
**						 either 0 or 1 in this case.
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: No checking is done as to whether the path name is valid.
*********************************************************************/
int ubu_ask_for_target_sym(symname, numintptr)
	char symname[UB_SYMBOL_NAME_LEN];	/* returns the master symbol name */
	int *numintptr;	/* number of user interactions */
{
	int cmdreject;
	UU_LOGICAL nameok;
	int status = UU_SUCCESS; /* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_ask_for_target_sym(%x,numintptr)", symname));
	status = UU_SUCCESS; /* assume success */
	*numintptr = 0;	/* no user interactions yet to return */
	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)	/* then no command reject encountered */
	{
		ud_ldas(UD_DASSTRING, UB_SYMBOL, 106, symname, UB_SYMBOL_NAME_LEN,
					numintptr, UD_NODEFAULT);
		/* prompt is: Enter the new symbol file name. */
		if (*numintptr <= 0) goto done;
		if (ubu_symmaster_name_ok(symname, &nameok) != UU_SUCCESS)
					goto failed;
		if (nameok) 
			/* name of master symbol gotten */
			*numintptr = 1;	/* we now have an appropriate user response */
	}/* end no command reject */
	else /* command reject hit */
	{}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}  
/*********************************************************************
**    I_FUNCTION : int ubu_ask4LibOrMsym2Use(prompt, libpath, whatToDoptr)
**			This function gets the path name to the symbol library during a post
**			load operation, or, it determines what other action the user wishes
**			to be done if he doesn't want to give a library path name.
**    PARAMETERS   
**       INPUT  : 
**				prompt		Prompt to display to the user.
**       OUTPUT :  
**				libpath		Path to the symbol library (if UB_CHKLOADEDMSYM returned**								below).
**				whatToDoptr	Pointer to one of the following values:
**									UB_CHKLOADEDMSYM		check version number of newly
**																loaded master.
**									UB_USEMSYMFROMPART	use the newly load master 
**																regardless of lib path.
**									UB_DONTUSE				don't use any master.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubu_ask4LibOrMsym2Use(prompt, libpath, whatToDoptr)
	char *prompt;
	char *libpath;
	int *whatToDoptr;
{
	UU_LOGICAL ud_lyesno();
	UU_LOGICAL found;
	int cmdreject, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubu_ask4LibOrMsym2Use(%s,?,?)", prompt));
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)
	{	 
		if (ubu_ask4Libpath(prompt, libpath, &found) != UU_SUCCESS) goto failed;

		if (!found) /* no legal path given; see what user wants to do */
			if (ud_lyesno(UB_SYMBOL,112))
				/* prompt is: Do you want to use the master in the part
				 * file regardless of lib path? */
				*whatToDoptr = UB_USEMSYMFROMPART;
			else *whatToDoptr = UB_DONTUSE;
	}/* end no command reject */
	else/* command reject */
		;
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
#if (TRACE)
	switch (*whatToDoptr)
	{
		case UB_CHKLOADEDMSYM: 
				sprintf(UB_sbuf,"check to reload, libpath:%s",libpath); break;
		case UB_USEMSYMFROMPART: sprintf(UB_sbuf,
				"use newly loaded master regardless of path"); break;
		case UB_DONTUSE: sprintf(UB_sbuf, "don't use any master"); break;
		default: sprintf(UB_sbuf,"something's wrong"); break;
	}
	uu_dprint(UU_BTRC,(us,"%s", UB_sbuf));
#endif
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}
	
/*********************************************************************
**    I_FUNCTION :  ubu_ask4Libpath(prompt, libpath, foundptr)
**			This function asks for the symbol library from the user and checks
**			to determine if the path is legitimate and can be found and is a
**			directory.
**    PARAMETERS   
**       INPUT  : 
**				prompt		Prompt to use in prompting user.
**       OUTPUT :  
**				libpath		Library path to be returned.
**				foundptr		Pointer to UU_TRUE iff a library path was found.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubu_ask4Libpath(prompt, libpath, foundptr)
	char *prompt;
	char *libpath;
	UU_LOGICAL *foundptr;
{
	char *uu_uprompt1();
	UU_LOGICAL ask4path = UU_TRUE;
	int numint, fileStatus, status = UU_SUCCESS;

	uu_denter(UU_BTRC, (us, "ubu_ask4Libpath(%s,?,?)", prompt));
	*foundptr = UU_FALSE;
	while (ask4path)
	{
		/* the following prints errors of syntax error in path, or,
		 * lib not found. */
		if (uxu_ask_for_lib(&UB_libdata_rec, prompt, libpath, &numint, 
					&fileStatus) != UU_SUCCESS) goto failed;
		if (numint <= 0)
			break;
		if (fileStatus == UB_D) /* library found */
		{
			*foundptr = UU_TRUE;
			ask4path = UU_FALSE;	
		}
		else 
			prompt = uu_uprompt1(UB_SYMBOL,111,libpath);
			/* %s doesn't correspond to a library; enter new lib. */
	}

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
