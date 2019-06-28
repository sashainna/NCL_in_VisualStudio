/*********************************************************************
**	 NAME:  bumlib.c
**		 CONTAINS:
**			 ubu_load_symmaster()
**			 ubu_load_symmaster_lib()
**			 ubu_load_archive
**			 ubu_archive_symmaster()
**			 ubu_delete_lib_symmaster()
**			 ubu_rename_lib_symmaster()
**        ubu_delete_sym_file
**			 ubu_copy_sym_file
**			 ubu_rename_sym_file
**			 ubu_updateMsymPaths
**	 COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**       bumlib.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       12/01/15 , 08:11:00
*********************************************************************/
#include "usysdef.h"	 /* for UU_REAL, etc. */
#include "uhep.h"		 /* for error system */
#include "udebug.h"	  /* for debugging trace facility */
#include "dmark.h"		/* for UD_MARK */
#include "dasnog.h"	  /* for DAS */
#include "dinput.h"	  /* for DAS input types; e.g. UD_STRING */
#include "mdrel.h"		/* for UB_SYMBOL_REL */
#include "xfsys2.h"
#include "xfsys1.h"
#include "xenv1.h"
#include "bsym.h"
#include "udforms.h"
#include "udfdata.h"

#define TRACE UU_FALSE  /* for debugging only */
/*********************************************************************
**	 E_FUNCTION :  int ubu_load_symmaster(symbol, path)
**		 This function loads master symbol into UNIBASE.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT :
**              symbol  = Name of symbol loaded.
**              path  = path of symbol loaded.
**
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubu_load_symmaster(symbol, path)
char *symbol, *path;
{
	struct UB_symnam_frm_rec sym_frm_out;
	char fullname[UB_MAX_PATH_LEN];
	char libname[UB_MAX_PATH_LEN];
			/* symbol library from which to load the master symbol */
	char symname[UB_SYMBOL_NAME_LEN_PLUS];
	char path_prefix[UB_MAX_PATH_LEN];
	char dir[UB_MAX_PATH_LEN];
	char bname[UB_MAX_PATH_LEN];
			/* name of master symbol to be loaded */
	char *uu_uprompt0(), *uu_uprompt1();
	struct UB_symbol_rec sym;
	int nameok, mode, hdrfound;
	UU_LOGICAL found; /* UU_TRUE iff master name found in UNIBASE */
	int status = UU_SUCCESS;
	
	uu_denter(UU_BTRC, (us, "ubu_load_symmaster()"));
/*
......use form interface
.....Yurong 9/21/98
*/
	symname[0] = '\0';
	libname[0] = '\0';
	sym_frm_out.name = symname;
	sym_frm_out.lib = libname;
	if (UB_libdata_rec.default_firstim)
		sym_frm_out.libdir = 0;
	else
	{
		if (strcmp(UB_libdata_rec.default_area, "system") == 0)
			sym_frm_out.libdir = 1;
		else
			sym_frm_out.libdir = 0;
		strcpy(sym_frm_out.lib, UB_libdata_rec.default_lib);
	}
	status = ubi_symnam_form("Load Symbol", &(sym_frm_out), &UB_libdata_rec);
	if (status!=0) goto failed;
	if (ubu_symmaster_name_ok(sym_frm_out.name, &nameok) != UU_SUCCESS)
		goto failed;
	ncl_parse_label(sym_frm_out.name,sym.label,&sym.subscr);
	if (ub_get_symmaster_by_name(&sym, &found,1,1) != UU_SUCCESS)
		goto failed;
	if (found)
	{
		if (!ud_yesno(0, uu_uprompt1(UB_SYMBOL, 21, sym.label),
					"Load Symbols"))
			goto done;
		else
/*
......delete current UNIBASE symbol
*/
		if (ubu_del_symmaster(sym.key, UU_FALSE,UU_TRUE) != UU_SUCCESS)
			goto failed;
	}
	if (sym_frm_out.libdir == 0)
		strcpy(path_prefix, UB_libdata_rec.loc_farea);
	else
		strcpy(path_prefix, UB_libdata_rec.sys_farea);
	mode = UX_READ | UX_WRITE; /* check for read and write access */
/*
......before we cat path_prefix to libname, we need check if
......libname already have a path
......Yurong changed 9/17/98
*/
	ul_break_fname(libname, dir, bname);
	if (dir[0]!='\0')
	{
		status = ux_file_inquire(UU_NULL, libname, UU_NULL, "UB_SYM_EXTEN",
						"UB_SYM_SUFFIX", &mode, &hdrfound, libname, UX_PRTERRS);
	}
	else
	{
		status = ux_file_inquire(path_prefix, libname, UU_NULL, "UB_SYM_EXTEN",
						"UB_SYM_SUFFIX", &mode, &hdrfound, libname, UX_PRTERRS);
	}
	if (status!= UU_SUCCESS) goto failed;
	if (mode != (mode | UX_FAREA))
	{
		uu_uerror3(UX_UDOS,26,UB_libdata_rec.pstring, fullname, 
						"ubu_load_symmaster");
		goto failed;
	}
	if (ubi_load_file(UU_NULL,libname,symname,UU_NULL,"NOOUTPUT",UU_NULL,
		UX_PRTERRS) != UU_SUCCESS) goto failed;
	if (symbol != UU_NULL) 
	{
		strcpy(symbol,symname);
		ul_remove_quotes(libname);
		strcpy(path, libname);
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  

/*********************************************************************
**	 E_FUNCTION :  int ubu_load_symmaster_lib()
**		 This function loads an entire master symbol library archive into UNIBASE.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none. 
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubu_load_symmaster_lib()
{
	char libname[UB_MAX_PATH_LEN];
			/* symbol library from which to load the master symbol */
	char *prompt, *uu_uprompt0();
	int numint, file_status;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC, (us, "ubu_load_symmaster_lib()"));
	prompt = uu_uprompt0(UB_SYMBOL, 15);
	if (uxu_ask_for_lib(&UB_libdata_rec, prompt, libname, &numint, &file_status) 
				!= UU_SUCCESS) goto failed;
	if (numint <= 0) 
		goto done;
	if (file_status != UX_FAREA) /* we did not get a directory */
	{
		uu_uerror1(UB_SYMBOL, 71, "ubu_load_symmaster_lib");
		/* error is: No symbol library by this name exists (%s). */
		goto failed;
	}

#if (TRACE)
	if (libname != UU_NULL)
	{
		uu_dprint(UU_BTRC,(us,"libname:%s", libname));
	}
	else
		uu_dprint(UU_BTRC,(us,"libname is UU_NULL"));
#endif

	/* removed this code with new udos subsystem:
	 * remove the symbol area suffix from "libname", since it will
	 * be put on again below. */

	if (ubu_load_archive(UU_NULL, libname) != UU_SUCCESS)
				goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  

/*********************************************************************
**    I_FUNCTION: int ubu_load_archive(area, libname)
**       This function loads a master symbol archive and adds a version numbers
**			to the UNIBASE records of each master loaded.
**    PARAMETERS   
**       INPUT  : 
**				area				User's local file area; note, this can be a symbolic
**									name from the Unicad initialization file; this can
**									also be UU_NULL.
**				libname			Library (i.e. directory) name; can not be a symbolic
**									name, but can be UU_NULL.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubu_load_archive(area, libname)
	char *area;
	char *libname;
{
	int ubu_ask_to_overwrite();
	struct UB_symbol_rec sym;
	int nxtuple;
	int ok;
	int status;			/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_load_archive(%x,%x)",area,libname));
#ifdef UU_DEBUGON
	if (area != UU_NULL)
		uu_dprint(UU_BTRC,(us,"area:%s", area));
	if (libname != UU_NULL)
		uu_dprint(UU_BTRC,(us,"libname:%s", libname));
#endif
	status = UU_SUCCESS; /* assume done */

	if (uxu_load_archive(area, libname, UU_NULL, "UB_SYM_SUFFIX",
				ubu_ask_to_overwrite, &ok) != UU_SUCCESS) goto failed;
	if (ok != UU_SUCCESS) goto failed;

	nxtuple = 1;
	while (ur_get_next_new_data_key(UB_SYMBOL_REL,&nxtuple,&sym.key)==0)
	{
		if (ub_retrieve_sym(&sym,sizeof(sym)) != UU_SUCCESS)
				goto failed;
		if (ux_get_fversion(sym.path, &(sym.version), UX_PRTERRS) 
					!= UU_SUCCESS)	goto failed; 
		if (ubi_update_data_fixed(&sym) != UU_SUCCESS) 
				goto failed;
		nxtuple++;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
/*********************************************************************
**	 E_FUNCTION :  int ubu_archive_symmaster()
**		 This function archives master symbols into master symbol library.
**			"Save As"
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none. 
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
/*
.....function changed to "Save As"
.....Yurong 9/24/98
*/
int ubu_archive_symmaster()
{
	int status;
	char symname[UB_MAX_PATH_LEN];
	char new_sym[UB_MAX_PATH_LEN];

	uu_denter(UU_BTRC, (us, "ubu_archive_symmaster()"));
/*
......use form with symbol list instead of
......prompt
......Yurong changed 9/22/98
*/
	status = ubi_savsym_form("bsavsym.frm", symname, new_sym);
	if (status!=0) goto done; 
/*
.....Moved saving code to a new routine so the routine for the "Save All"
.....button could reuse the existing code - ASF 8/5/13.
*/
	ubi_archive_symmaster(symname, new_sym);
done:;
	uu_dexit;
	return(status);
}  	
/*********************************************************************
**	 E_FUNCTION :  int ubi_archive_symmaster()
**		 This function archives master symbols into master symbol library.
**			"Save As"
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none. 
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubi_archive_symmaster(symname,new_sym)
char *symname,*new_sym;
{
	struct UB_symbol_rec sym;
	char libname[UB_MAX_PATH_LEN];
	char tmp_sym[UB_MAX_PATH_LEN];
	char *uu_uprompt0();
	UU_LOGICAL found; /* UU_TRUE iff master name in UNIBASE */
	int mode, hdrfound, libdir;
	int status = UU_SUCCESS; /* return status; either UU_SUCCESS or UB_FAILURE */
	char olddesc[UB_MAX_PATH_LEN];
	char descname[UB_MAX_PATH_LEN];
	char path_prefix[UB_MAX_PATH_LEN];
	char dir[UB_MAX_PATH_LEN];
	char bname[UB_MAX_PATH_LEN];
	char env[14] = "UB_SYM_EXTEN";
	char *fenv;

	ncl_parse_label(symname,sym.label,&sym.subscr);
	if (ub_get_symmaster_by_name(&sym, &found,1,1) != UU_SUCCESS)
		goto failed;
	if (!found)
	{
/*
.....error is: Can not find master symbol %s (%s).
*/
		uu_uerror2(UB_SYMBOL, 25, sym.label, "ubu_archive_symmaster");
		goto done;
	}
	strcpy(olddesc, sym.path);    /* replace the suffix */
	if (ux_add_ftype("UB_DESC_SUFFIX", olddesc, UX_PRTERRS) == UX_FAILURE)
		goto failed;
	if (strlen(new_sym) > 0)
	{
		ubi_breaklib_sym(new_sym, libname, tmp_sym);
/*		ul_break_fname(new_sym, libname, bname);*/
		ncl_parse_label(tmp_sym,sym.label,&sym.subscr);
	}
	else
	{
		libname[0] = '\0';
	}
	if (libname[0] == '\0')
	{
		ul_get_full_dir(sym.path, libname, bname);
	}
	if (strcmp(UB_libdata_rec.default_area, "local") == 0)
	{
		libdir = 0;
		strcpy(path_prefix, UB_libdata_rec.loc_farea);
	}
	else
	{
		libdir = 1;
		strcpy(path_prefix, UB_libdata_rec.sys_farea);
	}
	mode = UX_READ | UX_WRITE; /* check for read and write access */
/*
......before we cat path_prefix to libname, we need check if
......libname already have a path
......Yurong changed 9/17/98
*/
/*	ul_break_fname(libname, dir, bname);*/
/*
.....Check if the symbol is being saved to thte local or system directory. If
.....so, do not add "_S" or the save will fail - ASF 8/5/13.
*/
	if (ubi_compare_defaultpath(libname,libdir))
		fenv = UU_NULL;
	else
		fenv = env;
	if (libname[0]!='\0')
	{
		status = ux_file_inquire(UU_NULL, libname, UU_NULL, fenv,
						"UB_SYM_SUFFIX", &mode, &hdrfound, sym.path, UX_PRTERRS);
/*		strcpy(sym.path,libname);*/
	}
	else
	{
		if (libname[0] == '\0') strcpy(libname,"symlib");
		status = ux_file_inquire(path_prefix, libname, UU_NULL, fenv,
						"UB_SYM_SUFFIX", &mode, &hdrfound, sym.path, UX_PRTERRS);
	}
	if (status!= UU_SUCCESS) goto failed;

	/* compose full description file name that will associate with
	this saved master */
	/* the names can differ: different libraries, int. rename, etc. */


	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, sym.path, sym.label, "UB_SYM_EXTEN",
		"UB_DESC_SUFFIX", &mode, &hdrfound, descname, UX_PRTERRS) 
		!= UU_SUCCESS) goto failed;

	uu_dprint(UU_BTRC,(us,
		"Desc file associated with saved master named %s.", olddesc));

	/* library and symbol gotten; so do archive; if there is an old master 
	 * ask to delete it */
	if (ubu_savsym(&sym, sym.path, UU_TRUE) != UU_SUCCESS)
		goto failed;

	/* after the successful save, handle the description file business */
	if ( strcmp(descname, olddesc) != 0)
	{
		ux_copy(olddesc, descname, UU_NULL, UX_NPRTERRS);
		/* there may not be any description file, this is not an error */
		/* fix up the name found in the header of this file */
		ux_fix_ds_text(descname);						
	}

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**	 E_FUNCTION :  int ubu_delete_lib_symmaster()
**		 This function deletes a master symbol from a master symbol library,
**		 deleting it from Unibase if it is found to be loaded.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none. 
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubu_delete_lib_symmaster()
{
	struct UB_symbol_rec sym;
	struct UB_symnam_frm_rec sym_frm_out;
	char symname[UB_SYMBOL_NAME_LEN_PLUS];
	char libname[UB_MAX_PATH_LEN];
	char fullname[UB_MAX_PATH_LEN];
	char descname[UB_MAX_PATH_LEN];
	char ag_name[UB_MAX_PATH_LEN];
	char farea[UB_MAX_PATH_LEN];
	char larea[UB_MAX_PATH_LEN];
	char fname[UB_MAX_PATH_LEN];
	char bname[UB_MAX_PATH_LEN];
	char dir[UB_MAX_PATH_LEN];
	char path_prefix[UB_MAX_PATH_LEN];
	char *uu_uprompt0();
	int mode, hdrfound, nameok;
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */
	UU_LOGICAL found;

	uu_denter(UU_BTRC, (us, "ubu_delete_lib_symmaster()"));
	status = UU_SUCCESS; /* assume success */
/*
......changed to use form interface
......Yurong 9/22/98
*/
	symname[0] = '\0';
	libname[0] = '\0';
	sym_frm_out.name = symname;
	sym_frm_out.lib = libname;
	if (UB_libdata_rec.default_firstim)
		sym_frm_out.libdir = 0;
	else
	{
		if (strcmp(UB_libdata_rec.default_area, "system") == 0)
			sym_frm_out.libdir = 1;
		else
			sym_frm_out.libdir = 0;
		strcpy(sym_frm_out.lib, UB_libdata_rec.default_lib);
	}
	status = ubi_symnam_form("Delete Master Symbol", &(sym_frm_out), &UB_libdata_rec);
	if (status!=0) goto failed;
	if (ubu_symmaster_name_ok(sym_frm_out.name, &nameok) != UU_SUCCESS)
		goto failed;
	ncl_parse_label(sym_frm_out.name,sym.label,&sym.subscr);
	if (ub_get_symmaster_by_name(&sym, &found,1,1) != UU_SUCCESS) 
		goto failed;
	if ((found) && (sym.version == UB_NOLIB))
/*
.....A UB_NOLIB type of master so do a Unibase-only delete 
*/
	{
		if (ubu_del_symmaster(sym.key, UU_FALSE,UU_TRUE) != UU_SUCCESS)
			goto failed;
		goto done;
	}
	if (strcmp(UB_libdata_rec.default_area, "local") == 0)
		strcpy(path_prefix, UB_libdata_rec.loc_farea);
	else
		strcpy(path_prefix, UB_libdata_rec.sys_farea);
	mode = UX_READ | UX_WRITE; /* check for read and write access */
/*
......before we cat path_prefix to libname, we need check if
......libname already have a path
......Yurong changed 9/17/98
*/
	ul_break_fname(libname, dir, bname);
	if (dir[0]!='\0')
	{
		status = ux_file_inquire(UU_NULL, libname, UU_NULL, "UB_SYM_EXTEN",
						"UB_SYM_SUFFIX", &mode, &hdrfound, libname, UX_PRTERRS);
	}
	else
	{
		status = ux_file_inquire(path_prefix, libname, UU_NULL, "UB_SYM_EXTEN",
						"UB_SYM_SUFFIX", &mode, &hdrfound, libname, UX_PRTERRS);
	}
	if (status!= UU_SUCCESS) goto failed;
	mode = UX_READ | UX_WRITE; 
	if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
			"UB_SYM_SUFFIX", &mode, &hdrfound, fullname, UX_PRTERRS)
			!= UU_SUCCESS) goto failed;
	if ((mode == (mode | UX_NEXISTS))  
		|| (mode == (mode | UX_FAREA)))
	{
		uu_uerror2(UB_SYMBOL, 43, libname, "ubu_delete_lib_symmaster");
		/* error is: Master symbol does not exist in %s (%s). */
		goto failed;
	}
	else 
	{
		if (found)	
/* 
.....then delete it from Unibase first 
*/
		{
/*
.....check that it is from the same lib  
*/
			if (sym.version != UB_NOLIB)
			{
				if (ux_decompose_path(sym.path, farea, fname, UX_PRTERRS)
					!= UU_SUCCESS)	goto failed;
				if (ux_decompose_path(fullname, larea, fname, UX_PRTERRS)
					!= UU_SUCCESS)	goto failed;
				if (strcmp(farea,larea) == 0)
				{
					if (ubu_del_symmaster(sym.key, UU_FALSE,UU_TRUE) != UU_SUCCESS)
						goto failed;
				}
/* 
.....else the loaded symbol is from a diff. library anyway 
*/
			}
		}
/* 
.....delete the master symbol file from the library 
*/
		if (ux_delete(fullname, UX_PRTERRS) != UU_SUCCESS)
			goto failed;

/* 
.....after successful delete on the symbol master file, 
.....remove the associated APPLIED GEOMETRY file 
*/
		mode = UX_READ | UX_WRITE; /* check for read and write access */
		if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
				"UR_AG_FILE", &mode, &hdrfound, ag_name, UX_PRTERRS) 
				!= UU_SUCCESS) goto desc;
		if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
		{
			uu_dprint(UU_BTRC,(us,"This master has no associated AG file."));
		}
		else
			ux_delete(ag_name, UX_PRTERRS);

/* 
.....after successful delete on the symbol master file, 
.....remove the associated description file if there is one 
*/
desc:;
		mode = UX_READ | UX_WRITE; /* check for read and write access */
		if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
			"UB_DESC_SUFFIX", &mode, &hdrfound, descname, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;
		if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
		{
			uu_dprint(UU_BTRC,(us,"This master has no associated description."));
		}
		else
			if (ux_delete(descname, UX_PRTERRS) != UU_SUCCESS) goto failed;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  
/*********************************************************************
**	 E_FUNCTION :  int ubu_rename_lib_symmaster()
**		 This function renames a master symbol from a master symbol library.
**		 If either name is found to be loaded in Unibase, further checking
**		 on the association to a library master and prompts to overwrite
**		 in Unibase are used.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none. 
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubu_rename_lib_symmaster()
{
	struct UB_symbol_rec sym;
	struct UB_symbol_rec sym2;
	char symname[UB_SYMBOL_NAME_LEN_PLUS];
	char symname2[UB_SYMBOL_NAME_LEN_PLUS];
	char libname[UB_MAX_PATH_LEN];
	char libname2[UB_MAX_PATH_LEN];
	char fullname[UB_MAX_PATH_LEN];
	char fullname2[UB_MAX_PATH_LEN];
	char descname[UB_MAX_PATH_LEN];
	char descname2[UB_MAX_PATH_LEN];
	char ag_name[UB_MAX_PATH_LEN];
	char ag_name2[UB_MAX_PATH_LEN];
	char tempname1[UB_MAX_PATH_LEN];
	char tempname2[UB_MAX_PATH_LEN];
	char path_prefix[UB_MAX_PATH_LEN];
	char dir[UB_MAX_PATH_LEN];
	char bname[UB_MAX_PATH_LEN];
	char *uu_uprompt0(), *uu_uprompt1();
	int mode, hdrfound;
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */
	int rstat;
	int cmdreject;
	UU_LOGICAL found;
	UU_LOGICAL sym2found;
	UU_LOGICAL urename = UU_FALSE;

	uu_denter(UU_BTRC, (us, "ubu_rename_lib_symmaster()"));
	status = UU_SUCCESS; /* assume success */

	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE);
	if (cmdreject) goto done;
	tempname1[0] = '\0';
	tempname2[0] = '\0';
	status = ubi_rensym_form("brensymf.frm", tempname1, tempname2);
	if (status == -1) goto done;
	
	ubi_breaklib_sym(tempname1, libname, symname);
	ubi_breaklib_sym(tempname2, libname2, symname2);
	if (strlen(libname2) == 0) strcpy(libname2,libname);

	if (strcmp(UB_libdata_rec.default_area, "local") == 0)
		strcpy(path_prefix, UB_libdata_rec.loc_farea);
	else
		strcpy(path_prefix, UB_libdata_rec.sys_farea);
	mode = UX_READ | UX_WRITE; /* check for read and write access */
/*
......before we cat path_prefix to libname, we need check if
......libname already have a path
......Yurong changed 9/17/98
*/
	ul_break_fname(libname, dir, bname);
	if (dir[0]!='\0')
	{
		status = ux_file_inquire(UU_NULL, libname, UU_NULL, "UB_SYM_EXTEN",
						"UB_SYM_SUFFIX", &mode, &hdrfound, libname, UX_PRTERRS);
	}
	else
	{
		status = ux_file_inquire(path_prefix, libname, UU_NULL, "UB_SYM_EXTEN",
						"UB_SYM_SUFFIX", &mode, &hdrfound, libname, UX_PRTERRS);
	}
	if (status!= UU_SUCCESS) 
	{
/*
.....check for path without "_S"
*/
		if (dir[0]!='\0')
		{
			status = ux_file_inquire(UU_NULL, libname, UU_NULL, NULL,
							NULL, &mode, &hdrfound, libname, UX_PRTERRS);
		}
		if (status!=UU_SUCCESS) 
			goto failed;
	}
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	ul_break_fname(libname2, dir, bname);
	if (dir[0]!='\0')
	{
		status = ux_file_inquire(UU_NULL, libname2, UU_NULL, "UB_SYM_EXTEN",
						"UB_SYM_SUFFIX", &mode, &hdrfound, libname2, UX_PRTERRS);
	}
	else
	{
		status = ux_file_inquire(path_prefix, libname2, UU_NULL, "UB_SYM_EXTEN",
						"UB_SYM_SUFFIX", &mode, &hdrfound, libname2, UX_PRTERRS);
	}
	if (status!= UU_SUCCESS) 
	{
/*
.....check for path without "_S"
*/
		if (dir[0]!='\0')
		{
			status = ux_file_inquire(UU_NULL, libname2, UU_NULL, NULL,
							NULL, &mode, &hdrfound, libname2, UX_PRTERRS);
		}
		if (status!=UU_SUCCESS) 
			goto failed;
	}
	/* compose full target name and check for writeable subdirectory */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname2, symname2, NULL,
			"UB_SYM_SUFFIX", &mode, &hdrfound, fullname2, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	/* compose full file name and check for its existence in library */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname, symname, NULL,
			"UB_SYM_SUFFIX", &mode, &hdrfound, fullname, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
	{
		uu_uerror2(UB_SYMBOL, 43, libname, "ubu_rename_lib_symmaster");
		/* error is: Master symbol does not exist in %s (%s). */
		goto failed;
	}

	/* else  master symbol file exists so try to rename it */
	/* master symbol name gotten; try to retrieve the master symbol */
	ncl_parse_label(symname,sym.label,&sym.subscr);
	if (ub_get_symmaster_by_name(&sym, &found,1,1) != UU_SUCCESS)
		goto failed;
	/* check that the symbol is from same library */
	if (found)
	{
		if (sym.version != UB_NOLIB)
		{
			/* may differ by quotes only though ?? */
			if (strcmp(fullname, sym.path) == 0)
			/* then the libraries and names are the same */
			{
				/* set the flag to do the Unibase rename also */
				urename = UU_TRUE;
			}
			/* else the Unibase master got its name from a recent 
			internal rename operation or is from a different library */
		}
		/* else the Unibase symbol is not associated with the library */
	}

	ncl_parse_label(symname2,sym2.label,&sym2.subscr);
	if (ub_get_symmaster_by_name(&sym2, &sym2found,1,1) != UU_SUCCESS)
		goto failed;

	/* will test on sym2found and compare fullname2 to sym2.path
	to see if there's a loaded master of same name and library as the
	renamed master will be, or a loaded target master symbol name that
	is not same as would be loaded from the target master symbol file */

	if ((rstat=ux_rename(fullname, fullname2, UX_PRTERRS)) != UU_SUCCESS)
	{
		if	(rstat == UX_BAD_TARGET)
			/* ask to overwrite target file */
			if (ud_yesno(0, uu_uprompt1(UB_SYMBOL,117,fullname2), "Overwrite target file?"))
			/* prompt: target file name exists, overwrite file? */
			{
				if (ux_delete(fullname2, UX_PRTERRS) != UU_SUCCESS)
					goto failed;
				if (ux_rename(fullname, fullname2, UX_PRTERRS) != UU_SUCCESS)
					goto failed;
				if (sym2found)
				{
					if ( (strcmp(fullname2, sym2.path) == 0) && 
							(sym2.version != UB_NOLIB) )
					{
						if (ubu_del_symmaster(sym2.key, UU_FALSE,UU_TRUE) != UU_SUCCESS)
							goto failed;
						if (!found)
						{
							if (ubi_load_file(UU_NULL,libname,symname,UU_NULL,
								"NOOUTPUT",UU_NULL,UX_PRTERRS) != UU_SUCCESS)
									goto failed;
							ub_retrieve_sym(&sym);
						}
						urename = UU_TRUE;
					}
					else	/* not from the same lib or name as the target file */
					{
						if (urename)
						{
							if (ud_yesno(0, uu_uprompt1(UB_SYMBOL,119,fullname2), "Overwrite symbol"))
							/* UNIBASE already contains symbol named: %s; overwrite? */
							{
								if (ubu_del_symmaster(sym2.key, UU_FALSE,UU_TRUE) != UU_SUCCESS)
									goto failed;
								urename = UU_TRUE;
							}
							else
								urename = UU_FALSE;
						}
					}
				}	/* end of the if (sym2found) */	
			}
			else	/* user replies no to overwriting */
				goto done;	/* no need to handle Unibase overwriting either */
		else
			goto failed;
	}
	else /* ux_rename was successful (and the target file didn't exist */
	{
		if (sym2found)
		{
			if ((strcmp(fullname2, sym2.path) == 0) && (sym2.version != UB_NOLIB))
			{	/* there's a loaded master of same name and library as the
				renamed master will be, and the target file didn't exist */
				/* ask to rename old new (requires loading it, if not there */
				if (ud_yesno(0, uu_uprompt1(UB_SYMBOL,21,fullname2), "Overwrite master symbol"))
				/* UNIBASE already contains master symbol named: %s; overwrite? */
				{
					if (ubu_del_symmaster(sym2.key, UU_FALSE,UU_TRUE) != UU_SUCCESS)
						goto failed;
					if (!found)
					{
						if (ubi_load_file(UU_NULL,libname,symname,UU_NULL,
							"NOOUTPUT",UU_NULL,UX_PRTERRS) != UU_SUCCESS)
								goto failed;
						ub_retrieve_sym(&sym);
					}
					urename = UU_TRUE;
				}
				else
					urename = UU_FALSE;
			}
			else	/* not from the same target lib or name */
			{
				if (urename) 
					if (ud_yesno(0, uu_uprompt1(UB_SYMBOL,119,fullname2), "Overwrite symbol?"))
					/* UNIBASE already contains symbol named: %s; overwrite? */
					{
						if (ubu_del_symmaster(sym2.key, UU_FALSE,UU_TRUE) != UU_SUCCESS)
							goto failed;
						urename = UU_TRUE;
					}
					else
						urename = UU_FALSE;
			}
	
		}	/* end of the if sym2found */
	}
	
	if (urename == UU_TRUE)  /* do the Unibase rename also */
	{
		strcpy(sym.label, symname2);
		if (ur_update_data_fixed(&sym) != 0)
		{
			uu_uerror2(UB_SYMBOL, 42, sym.key, "ubu_rename_symmaster"); 
			/* error message: Can't update UNIBASE master symbol record having
			 * key = %d  (%s). */
			goto failed;
		}
	}

	/* after the rename of the symbol file, rename the associated APPLIED GEOMETRY file */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
		"UR_AG_FILE", &mode, &hdrfound, ag_name, UX_PRTERRS) 
		!= UU_SUCCESS) goto desc;
	if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
		{
		uu_dprint(UU_BTRC,(us,"This master has no associated AG file."));
		}
	else
	{
		/* compose the full target name */
		mode = UX_READ | UX_WRITE; /* check for read and write access */
		if (ux_file_inquire(UU_NULL, libname2, symname2, "UB_SYM_EXTEN",
			"UR_AG_FILE", &mode, &hdrfound, ag_name2, UX_PRTERRS) 
			!= UU_SUCCESS) goto desc;

		if ((rstat = ux_rename(ag_name, ag_name2, UX_PRTERRS)) != UU_SUCCESS)
		{
			if (rstat == UX_BAD_TARGET)
			{
				/* then overwrite the target file */
				if (ux_delete(ag_name2, UX_PRTERRS) != UU_SUCCESS)
					goto desc;
				if (ux_rename(ag_name, ag_name2, UX_PRTERRS) != UU_SUCCESS)
					goto desc;
			}
			else
				goto desc;
		}
	}

	/* after the rename of the symbol file, rename the associated description
	file if one is found to exist */
desc:;
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
		"UB_DESC_SUFFIX", &mode, &hdrfound, descname, UX_PRTERRS) 
		!= UU_SUCCESS) goto failed;
	if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
	{
		uu_dprint(UU_BTRC,(us,"This master has no associated description."));
	}
	else
	{
		/* compose the full target name */
		mode = UX_READ | UX_WRITE; /* check for read and write access */
		if (ux_file_inquire(UU_NULL, libname2, symname2, "UB_SYM_EXTEN",
			"UB_DESC_SUFFIX", &mode, &hdrfound, descname2, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

		if ((rstat = ux_rename(descname, descname2, UX_PRTERRS)) != UU_SUCCESS)
		{
			if (rstat == UX_BAD_TARGET)
			{
				/* then overwrite the target file */
				if (ux_delete(descname2, UX_PRTERRS) != UU_SUCCESS)
					goto failed;
				if (ux_rename(descname, descname2, UX_PRTERRS) != UU_SUCCESS)
					goto failed;
			}
			else
				goto failed;
		}
		ux_fix_ds_text(descname2);						
	}
	
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	UD_UNMARK(cmdreject);
	uu_dexit;
	return(status);
}  
/*********************************************************************
**	 E_FUNCTION :  int ubu_delete_sym_file()
**		 This function deletes a master symbol from a master symbol library.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none. 
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubu_delete_sym_file()
{
	struct UB_symnam_frm_rec sym_frm_out;
	char symname[UB_SYMBOL_NAME_LEN_PLUS];
	char libname[UB_MAX_PATH_LEN];
	char fullname[UB_MAX_PATH_LEN];
	char descname[UB_MAX_PATH_LEN];
	char ag_name[UB_MAX_PATH_LEN];
	char *uu_uprompt0();
	int mode, hdrfound, nameok;
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_delete_sym_file()"));
	symname[0] = '\0';
	libname[0] = '\0';
	sym_frm_out.name = symname;
	sym_frm_out.lib = libname;
	sym_frm_out.libdir = 0;
	status = ubi_symnam_form("Delete Symbol File", &(sym_frm_out));
	if (status!=0) goto failed;
	if (ubu_symmaster_name_ok(sym_frm_out.name, &nameok) != UU_SUCCESS)
		goto failed;
		
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
		"UB_SYM_SUFFIX", &mode, &hdrfound, fullname, UX_PRTERRS) 
		!= UU_SUCCESS) goto failed;
	if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
	{
		uu_uerror2(UB_SYMBOL, 43, libname, "ubu_delete_sym_file");
		/* error is: Master symbol does not exist in %s (%s). */
		goto failed;
	}
	else /* master symbol file exists so delete it */
	{
     if (ux_delete(fullname, UX_PRTERRS) != UU_SUCCESS)
				goto failed;

		/* after successful delete on the symbol master file, remove
		the associated APPLIED GEOMETRY file */
		mode = UX_READ | UX_WRITE; /* check for read and write access */
		if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
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
		if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
			"UB_DESC_SUFFIX", &mode, &hdrfound, descname, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;
		if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
		{
			uu_dprint(UU_BTRC,(us,"This master has no associated description."));
		}
		else
			if (ux_delete(descname, UX_PRTERRS) != UU_SUCCESS) goto failed;
	}

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  

/*********************************************************************
**	 E_FUNCTION :  int ubu_copy_sym_file()
**		 This function copies a master symbol from a master symbol library.
**		to a new name. File management only, not manipulated in Unibase.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none. 
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
/*
.....this function changed to be called from general "File Copy" function
.....Yurong 9/24/98
*/
int ubu_copy_sym_file(symfile, newsymfile)
char symfile[UB_MAX_PATH_LEN], newsymfile[UB_MAX_PATH_LEN];
{
	char symname[UB_SYMBOL_NAME_LEN_PLUS];
	char libname[UB_MAX_PATH_LEN];
	char fullname[UB_MAX_PATH_LEN];
	char symname2[UB_SYMBOL_NAME_LEN_PLUS];
	char libname2[UB_MAX_PATH_LEN];
	char fullname2[UB_MAX_PATH_LEN];
	char descname[UB_MAX_PATH_LEN];
	char descname2[UB_MAX_PATH_LEN];
	char ag_name[UB_MAX_PATH_LEN];
	char ag_name2[UB_MAX_PATH_LEN];
	char *uu_uprompt0();
	int mode, hdrfound;
	int status = UU_SUCCESS; /* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_copy_sym_file()"));
	ul_remove_quotes(symfile);
	ul_remove_quotes(newsymfile);
	ubi_breaklib_sym(symfile, libname, symname);
	ubi_breaklib_sym(newsymfile, libname2, symname2);

	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname2, symname2, "UB_SYM_EXTEN",
			"UB_SYM_SUFFIX", &mode, &hdrfound, fullname2, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	/* compose full file name and check for its existence in library */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
			"UB_SYM_SUFFIX", &mode, &hdrfound, fullname, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	if ((mode == (mode | UX_NEXISTS))  /* then the file not does not exist */
		|| (mode == (mode | UX_FAREA)))/* then the file is a directory */
	{
		uu_uerror2(UB_SYMBOL, 43, libname, "ubu_copy_sym_file");
		/* error is: Master symbol does not exist in %s (%s). */
		goto failed;
	}
	else	/* master symbol file exists so copy it */
		if (ux_copy(fullname, fullname2, UU_NULL, UX_PRTERRS) != UU_SUCCESS)
			goto failed;
		else	/* successful copy, so copy associated APPLIED GEOMETRY file */
		{
			mode = UX_READ | UX_WRITE; /* check for read and write access */
			if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
				"UR_AG_FILE", &mode, &hdrfound, ag_name, UX_PRTERRS) 
				!= UU_SUCCESS) goto desc;
			if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
			{
			uu_dprint(UU_BTRC,(us,"This master has no associated AG file."));
			}
			else
			{
				/* compose the full target name */
				if (ux_file_inquire(UU_NULL, libname2, symname2, "UB_SYM_EXTEN",
					"UR_AG_FILE", &mode, &hdrfound, ag_name2, UX_PRTERRS) 
					!= UU_SUCCESS) goto desc;
				ux_copy(ag_name, ag_name2, UU_NULL, UX_PRTERRS);
			}

			/* now copy associated description file */
desc:;
			mode = UX_READ | UX_WRITE; /* check for read and write access */
			if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
				"UB_DESC_SUFFIX", &mode, &hdrfound, descname, UX_PRTERRS) 
				!= UU_SUCCESS) goto failed;
			if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
			{
			uu_dprint(UU_BTRC,(us,"This master has no associated description."));
			}
			else
			{
				/* compose the full target name */
				if (ux_file_inquire(UU_NULL, libname2, symname2, "UB_SYM_EXTEN",
					"UB_DESC_SUFFIX", &mode, &hdrfound, descname2, UX_PRTERRS) 
					!= UU_SUCCESS) goto failed;
				if (ux_copy(descname, descname2, UU_NULL, UX_PRTERRS) != UU_SUCCESS)
					goto failed;
				ux_fix_ds_text(descname2);						
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
**	 E_FUNCTION :  int ubu_rename_sym_file()
**		 This function renames a master symbol from a master symbol library.
**		to a new name. File management only, not manipulated in Unibase.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none. 
**	 RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
/*
.....this function changed to be called from general "File Rename" function
.....Yurong 9/24/98
*/
int ubu_rename_sym_file(symfile, newsymfile)
char newsymfile[UB_MAX_PATH_LEN], symfile[UB_MAX_PATH_LEN];
{
	char symname[UB_SYMBOL_NAME_LEN_PLUS];
	char libname[UB_MAX_PATH_LEN];
	char fullname[UB_MAX_PATH_LEN];
	char symname2[UB_SYMBOL_NAME_LEN_PLUS];
	char libname2[UB_MAX_PATH_LEN];
	char fullname2[UB_MAX_PATH_LEN];
	char descname[UB_MAX_PATH_LEN];
	char descname2[UB_MAX_PATH_LEN];
	char ag_name[UB_MAX_PATH_LEN];
	char ag_name2[UB_MAX_PATH_LEN];
	char *uu_uprompt0(), *uu_uprompt1();
	int mode, hdrfound;
	int rstat;
	int status = UU_SUCCESS; /* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_rename_sym_file()"));
	ul_remove_quotes(symfile);
	ul_remove_quotes(newsymfile);
	ubi_breaklib_sym(symfile, libname, symname);
	ubi_breaklib_sym(newsymfile, libname2, symname2);

	/* compose full target name and check for writeable subdirectory */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname2, symname2, "UB_SYM_EXTEN",
			"UB_SYM_SUFFIX", &mode, &hdrfound, fullname2, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	/* compose full file name and check for its existence in library */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
			"UB_SYM_SUFFIX", &mode, &hdrfound, fullname, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	if ((mode == (mode | UX_NEXISTS))  /* then the file not does not exist */
		|| (mode == (mode | UX_FAREA)))/* then the file is a directory */
	{
		uu_uerror2(UB_SYMBOL, 43, libname, "ubu_rename_sym_file");
		/* error is: Master symbol does not exist in %s (%s). */
		goto failed;
	}
	else	/* master symbol file exists so rename it */
	{
		if ((rstat = ux_rename(fullname, fullname2, UX_PRTERRS)) != UU_SUCCESS)
		{
			if	(rstat == UX_BAD_TARGET)
				/* ask to overwrite target file */
				if (ud_yesno(0, uu_uprompt1(UB_SYMBOL,117,fullname2), "Overwrite target file?"))
				/* prompt: target file name exists, overwrite file? */
				{
					if (ux_delete(fullname2, UX_PRTERRS) != UU_SUCCESS)
						goto failed;
					if (ux_rename(fullname, fullname2, UX_PRTERRS) != UU_SUCCESS)
						goto failed;
				}
			else
				goto failed;
		}

		/* after successful rename, rename the associated APPLIED GEOMETRY file */
		mode = UX_READ | UX_WRITE; /* check for read and write access */
		if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
			"UR_AG_FILE", &mode, &hdrfound, ag_name, UX_PRTERRS) 
			!= UU_SUCCESS) goto desc;
		if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
		{
			uu_dprint(UU_BTRC,(us,"This master has no associated AG file."));
		}
		else
		{
			/* compose the target name */
			mode = UX_READ | UX_WRITE; /* check for read and write access */
			if (ux_file_inquire(UU_NULL, libname2, symname2, "UB_SYM_EXTEN",
				"UR_AG_FILE", &mode, &hdrfound, ag_name2, UX_PRTERRS) 
				!= UU_SUCCESS) goto desc;
			if ((rstat = ux_rename(ag_name, ag_name2, UX_PRTERRS)) != UU_SUCCESS)
				if (rstat == UX_BAD_TARGET)
				{
					/* then overwrite the target file */
					if (ux_delete(ag_name2, UX_PRTERRS) != UU_SUCCESS) goto desc;
					ux_rename(ag_name, ag_name2, UX_PRTERRS);
				}
		}

		/* after successful rename, rename the associated descriptin file */
desc:;
		mode = UX_READ | UX_WRITE; /* check for read and write access */
		if (ux_file_inquire(UU_NULL, libname, symname, "UB_SYM_EXTEN",
			"UB_DESC_SUFFIX", &mode, &hdrfound, descname, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;
		if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
		{
			uu_dprint(UU_BTRC,(us,"This master has no associated description."));
		}
		else
		{
			/* compose the target name */
			mode = UX_READ | UX_WRITE; /* check for read and write access */
			if (ux_file_inquire(UU_NULL, libname2, symname2, "UB_SYM_EXTEN",
				"UB_DESC_SUFFIX", &mode, &hdrfound, descname2, UX_PRTERRS) 
				!= UU_SUCCESS) goto failed;
			if ((rstat = ux_rename(descname, descname2, UX_PRTERRS)) != UU_SUCCESS)
				if (rstat == UX_BAD_TARGET)
				{
					/* then overwrite the target file */
					if (ux_delete(descname2, UX_PRTERRS) != UU_SUCCESS)
						goto failed;
					if (ux_rename(descname, descname2, UX_PRTERRS) != UU_SUCCESS)
						goto failed;
				}
				else
					goto failed;
			ux_fix_ds_text(descname2);
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
**    I_FUNCTION : int ubu_updateMsymPaths(name, modeptr, libpath, fullpath,
**									what2doptr)
**			This function creates the path name to the symbol library and the
**			the full path to the master symbol.  
**    PARAMETERS   
**       INPUT  : 
**				name			Name of the master symbol.
**				modeptr		UU_NULL, or,
**								Pointer to the bitwise "OR" of the following values: 
**								the pointer "modeptr" may be UU_NULL if no access
**								checking is desired
**								UX_CREATE: can the file designated by "pathname"
**									be created; if the file can be created we 
**									assume it can be read and written;
**								UX_EXISTS: does the designated file already exist;
**								UX_READ: can the file be read;
**								UX_WRITE: can the file be written to;
**								UX_EXECUTE: can the file be executed;
**								UX_DELETE: can the file be deleted;
**       OUTPUT :  
**				modeptr		If not UU_NULL, the one of the following values:
**								UX_NEXIST: If "pathname" corresponds to a file 
**									(not a file area) and the path to the last file 
**									area in "pathname" exists and can be searched 
**									but "pathname" does not exist.
**									If "pathname" designates a file area (not a file)
**									and its parent file area exists and can be 
**									searched but "pathname" does not exist. 
**								UX_NEXIST | UX_CREATE: same as above but the file
**									(area) maybe created. This assumes UX_CREATE was
**									specified as input in "modeptr".
**								UX_FAREA: "pathname" is a file area (that exists).
**								UX_LOCKED: the file is locked.
**								A bitwise "OR" of the above input mode values that 
**								are satisfied, plus, 
**								if the file is currently open
**								then the type of open is bitwise "OR"ed in:
**									UX_R: open for reading;
**									UX_W: open for writing;
**									UX_A: open for appending.
**				libpath		Path to symbol library.
**				fullpath		Path to master symbol.
**				what2doptr	Pointer that retruns an indication of what the user 
**								wants to do.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubu_updateMsymPaths(name, modeptr, libpath, fullpath, what2doptr)
	char name[];
	int *modeptr;
	char libpath[];
	char fullpath[];
	int *what2doptr;
{
	char oldName[UB_SYMBOL_NAME_LEN_PLUS];
	char *prompt, *uu_uprompt2(), *uu_uprompt1();
	UU_LOGICAL chkingPath = UU_TRUE;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC, (us,"ubu_updateMsymPaths(name=%s,mode:%d,?,fullpath:%s)",
						name, *modeptr, fullpath));
	/* get symbol library path */
	switch(ux_decompose_path(fullpath,libpath,oldName,UX_PRTERRS | UX_QUOTES))
	{
		case UU_SUCCESS:
			break;
		case UX_BAD_SUBJECT:/* in this case libpath should have an empty string */
			uu_dprint(UU_BTRC,(us,"illegal path found, but we will continue"));
			break;
		case UU_FAILURE:
		default:
			goto failed;
	}
	*what2doptr = UB_CHKLOADEDMSYM; /* assume lib path will be given correctly */
	while (chkingPath)
	{
		switch (ux_mk_chk_syspath(UU_NULL, libpath, name, UU_NULL,
			"UB_SYM_SUFFIX",modeptr,fullpath,UX_PRTERRS) )
		{
			case UU_SUCCESS: 
				goto done;
			case UX_NO_ACCESS:
				prompt = uu_uprompt2(UB_SYMBOL, 110, name,fullpath);
				/* prompt is: No access to get status of master, %s, at %s;
				 * enter new lib. */
				break;
			case UX_BAD_SUBJECT:
				prompt = uu_uprompt1(UB_SYMBOL, 108, name);
				/* prompt is: Can't construct library path to master, %s; enter lib 
				 * name.  */
				break;
			case UX_BAD_ENV:
			case UX_FAILURE:
				/* can't read master file */
				prompt = uu_uprompt2(UB_SYMBOL, 109, name,fullpath);
				/* prompt is: Master symbol file for %s isn't readable at %s;
				 * enter new lib. */
				break;
			default:
				goto failed;
		}/* end switch */
		if (ubu_ask4LibOrMsym2Use(prompt, libpath, what2doptr) 
				!= UU_SUCCESS) goto failed;
		if (*what2doptr != UB_CHKLOADEDMSYM) /* no lib path was given */
			break;
	}/* end while chkingPath */	

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
#if (TRACE)
	if (*what2doptr == UB_CHKLOADEDMSYM)
	{
		if (modeptr != UU_NULL)
		{
			uu_dprint(UU_BTRC,(us,"mode:%d,libpath:%s,fullpath:%s", 
						*modeptr,libpath, fullpath));
		}
		else 
			uu_dprint(UU_BTRC,(us,"libpath:%s,fullpath:%s", libpath, fullpath));
	}
#endif
	uu_dexit;
	return(status);
}

