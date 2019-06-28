/*******************************************************************
**    NAME:bspl.c
**       CONTAINS:  
**    	ubu_get_spl_name(name, fullpath, libokptr)
**	 		ubu_delete_spart_file()
**	 		ubu_copy_spart_file()
**	 		ubu_rename_spart_file()
**	 		ubi_mk_spl(area,spl,fulspl, statptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**           bsplx.c , 3.1
**    MODULE NAME AND RELEASE LEVEL 
**       bsplx.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:05
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "dselmask.h"
#include "bsym.h"		
#include "xfsys1.h"
#include "xenv1.h"
extern UU_LOGICAL UR_load_env;
#define TRACE UU_FALSE /* for debugging only */

/*********************************************************************
**    I_FUNCTION: int ubu_get_spl_name(name, fullpath, libokptr)
**		This function determines whether "name" is a legal standard part
**		library name and determines whether "name" is already the name
**		of a local library.
**    PARAMETERS   
**       INPUT  : 
**				name			Proposed name of the standard parts library.
**       OUTPUT :  
**				name			If "*statusptr" = UU_TRUE then this is the name of 
**								an existing library to be returned.
**				fullpath		Full path name to the library.
**				libokptr		Pointer to the return status; UU_TRUE iff the returned
**								name is ok.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE 
**				 otherwise.
**    SIDE EFFECTS: none.
**    WARNINGS: NO marks or unmarks in here.
*********************************************************************/
int ubu_get_spl_name(name, fullpath, libokptr)
	char name[];
	char fullpath[UB_MAX_PATH_LEN];
	UU_LOGICAL *libokptr;
{
	char path_prefix[UB_MAX_PATH_LEN];
	int file_status;
	int mode, hdrfound;
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC, (us, "ubu_get_spl_name(%s,?,?)",name));

/*
......remove because we allow path in the library name
......but '/' is not a legal char in uxu_lib_name_ok
......Yurong 9/18/98
*/
/*
	if (uxu_lib_name_ok(&UB_spl_libdata_rec, name, libokptr) != UU_SUCCESS) 
		goto failed;            
	if (!(*libokptr))
			goto done;
*/
	if (strcmp(UB_spl_libdata_rec.default_area, "local") == 0)
		strcpy(path_prefix, "UB_LOC_SPLDIR");
	else
		strcpy(path_prefix, "UB_SYS_SPLDIR");


	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(path_prefix, name, UU_NULL, "UB_SPL_EXTEN",
			"UB_SP_SUFFIX", &mode,&hdrfound, fullpath, UX_PRTERRS)
			!= UU_SUCCESS)
	{
		uu_uerror2(UB_SYMBOL, 58, "ub_get_spl_name");
		/* error is: Error in path name to archive,  (%s). */
		goto failed;
	}
	if ((mode != (mode|UX_FAREA)) && (mode != (mode|UX_NEXISTS)))
	{  
		uu_uerror2(UB_SYMBOL, 120, fullpath, "ub_get_spl_name");
		/* error message: No standard part library found at %s  (%s). */
		goto failed;
	}
	/* reset default library name for subsequent library references */
	UB_spl_libdata_rec.default_firstim = UU_FALSE;
	strcpy(UB_spl_libdata_rec.default_lib, name);/* need only what user supplied*/

	/* then either we have a directory or nothing */
	if ( mode != (mode|UX_FAREA)) 
	{
		uu_uerror2(UB_SYMBOL, 121, name, "ubu_get_spl_name");
		/* error is: No standard part library by this name exists (%s). */
		*libokptr = UU_FALSE;
		goto done;
	}

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubu_get_spl_name",status);
	return(status);
}	
/*********************************************************************
**	 E_FUNCTION :  int ubu_delete_spart_file()
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none. 
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubu_delete_spart_file()
{
	char spartname[UB_SYMBOL_NAME_LEN_PLUS];
	char libname[UB_MAX_PATH_LEN];
	char fullname[UB_MAX_PATH_LEN]; 
	char solname[UB_MAX_PATH_LEN]; 
	char *prompt, *uu_uprompt0();
	int mode, hdrfound;
	int file_status;
	UU_LOGICAL nameok;
	int numint = 0;		 /* number of appropriate user interactions */
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UU_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_delete_spart_file()"));

	ud_ldas(UD_DASSTRING, UB_SYMBOL, 1, spartname, UB_SYMBOL_NAME_LEN,
		&numint, UD_NODEFAULT);
		/* prompt is: Enter standard part name. */
	if (numint<= 0) goto done;
	if (ubu_symmaster_name_ok(spartname, &nameok) != UU_SUCCESS)
		goto failed;
	if (nameok) 
		numint = 1;	/* we now have an appropriate user response */
	if (numint<= 0) 
		goto done;

	prompt = uu_uprompt0(UB_SYMBOL, 120);
	/* Prompt is: Give %s lib. name; for system area, prefix with \"sys:\". */
	if (uxu_ask_for_lib(&UB_spl_libdata_rec, prompt, libname, &numint, &file_status) 
				!= UU_SUCCESS) goto failed;
	if (numint <=0)
		goto done;
	if (file_status != UX_FAREA) /* we did not get a directory */
		goto failed;
		
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname, spartname, "UB_SPL_EXTEN",
		"UB_SP_SUFFIX", &mode, &hdrfound, fullname, UX_PRTERRS) 
		!= UU_SUCCESS) goto failed;
	if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
	{
		uu_uerror2(UB_SYMBOL, 124, libname, "ubu_delete_spart_file");
		/* error is: Standard part does not exist in %s (%s). */
		goto failed;
	}
	else /* standard part file exists so delete it */
	{
		mode = UX_READ | UX_WRITE; /* check for read and write access */
		if (ux_file_inquire(UU_NULL, libname, spartname, "UB_SPL_EXTEN",
			"UB_SP_SOL_SUFFIX", &mode, &hdrfound, solname, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;
		if ( (mode == (mode|UX_NEXISTS)) || (mode == (mode|UX_FAREA)) )
		{
			uu_uerror2(UB_SYMBOL, 124, libname, "ubu_delete_spart_file");
			/* error is: Standard part does not exist in %s (%s). */
			goto failed;
		}
 		if (ux_delete(fullname, UX_PRTERRS) != UU_SUCCESS)
				goto failed;
 		if (ux_delete(solname, UX_PRTERRS) != UU_SUCCESS)
				goto failed;
	}

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus(" ubu_delete_spart_file",status);
	return(status);
}  
/*********************************************************************
**	 E_FUNCTION :  int ubu_copy_spart_file()
**		 This function copies a standard part from a standard part library
**		to a new name.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none. 
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubu_copy_spart_file()
{
	char spartname[UB_SYMBOL_NAME_LEN_PLUS];
	char libname[UB_MAX_PATH_LEN];
	char fullname[UB_MAX_PATH_LEN];
	char fullsol[UB_MAX_PATH_LEN];
	char spartname2[UB_SYMBOL_NAME_LEN_PLUS];
	char libname2[UB_MAX_PATH_LEN];
	char fullname2[UB_MAX_PATH_LEN];
	char fullsol2[UB_MAX_PATH_LEN];
	char *prompt, *uu_uprompt0();
	int mode, hdrfound;
	int file_status;
	UU_LOGICAL copyit, nameok;
	int numint = 0;		 /* number of appropriate user interactions */
	int status = UU_SUCCESS; /* return status; either UU_SUCCESS or UU_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_copy_spart_file()"));

	ud_ldas(UD_DASSTRING, UB_SYMBOL, 1, spartname, UB_SYMBOL_NAME_LEN,
		&numint, UD_NODEFAULT);
		/* prompt is: Enter standard part name. */
	if (numint<= 0) goto done;
	if (ubu_symmaster_name_ok(spartname, &nameok) != UU_SUCCESS)
		goto failed;
	if (nameok) 
		numint = 1;	/* we now have an appropriate user response */
	if (numint<= 0) 
		goto done;

	prompt = uu_uprompt0(UB_SYMBOL, 120);
	/* Prompt is: Give spart lib. name; for system area, prefix name with 
	 * \"sys:\". */
	if (uxu_ask_for_lib(&UB_spl_libdata_rec, prompt, libname, &numint, &file_status) 
				!= UU_SUCCESS) goto failed;
	if (numint <=0)
		goto done;
	if (file_status != UX_FAREA) /* we did not get a directory */
		goto failed;
		
	numint = 0;	/* no user interactions yet to return */
	ud_ldas(UD_DASSTRING, UB_SYMBOL, 123, spartname2, UB_SYMBOL_NAME_LEN,
		&numint, UD_NODEFAULT);
	/* prompt is: Enter the new spart file name. */
	if (numint <= 0) goto done;
	if (ubu_symmaster_name_ok(spartname2, &nameok) != UU_SUCCESS)
		goto failed;
	if (nameok) 
		numint = 1;	/* we now have an appropriate user response */
	if (numint<= 0) 
		goto done;

	if (uxu_ask_for_target_lib(&UB_spl_libdata_rec, libname2, &numint, &file_status)
		!= UU_SUCCESS) goto failed;
	if (numint <=0)
		goto done;
	if (file_status != UX_FAREA) /* we did not get a directory */
		goto failed;

	/* compose full target name and check for writeable subdirectory */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname2, spartname2, "UB_SPL_EXTEN",
			"UB_SP_SUFFIX", &mode, &hdrfound, fullname2, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	/* compose full target name and check for writeable subdirectory */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname2, spartname2, "UB_SPL_EXTEN",
			"UB_SP_SOL_SUFFIX", &mode, &hdrfound, fullsol2, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	/* compose full file name and check for its existence in library */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname, spartname, "UB_SPL_EXTEN",
			"UB_SP_SUFFIX", &mode, &hdrfound, fullname, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	if ((mode == (mode | UX_NEXISTS))  /* then the file does not exist */
		|| (mode == (mode | UX_FAREA)))/* then the file is a directory */
	{
		uu_uerror2(UB_SYMBOL, 124, libname, "ubu_copy_spart_file");
		/* error is: Standard part does not exist in %s (%s). */
		goto failed;
	}

	/* compose full file name and check for its existence in library */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname, spartname, "UB_SPL_EXTEN",
			"UB_SP_SOL_SUFFIX", &mode, &hdrfound, fullsol, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	if ((mode == (mode | UX_NEXISTS))  /* then the file does not exist */
		|| (mode == (mode | UX_FAREA)))/* then the file is a directory */
	{
		uu_uerror2(UB_SYMBOL, 124, libname, "ubu_copy_spart_file");
		/* error is: Standard part does not exist in %s (%s). */
		goto failed;
	}
	else	/* both parts of the standard part file exist, so copy them */
	{
		if (ux_copy(fullname, fullname2, UU_NULL, UX_PRTERRS) != UU_SUCCESS)
			goto failed;
		if (ux_copy(fullsol, fullsol2, UU_NULL, UX_PRTERRS) != UU_SUCCESS)
			goto failed;
	}

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubu_copy_spart_file",status);
	return(status);
}  

/*********************************************************************
**	 E_FUNCTION :  int ubu_rename_spart_file()
**		 This function renames a standard part from a standard part library.
**		to a new name. File management only, not manipulated in Unibase.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none. 
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubu_rename_spart_file()
{
	char spartname[UB_SYMBOL_NAME_LEN_PLUS];
	char libname[UB_MAX_PATH_LEN];
	char fullname[UB_MAX_PATH_LEN];
	char fullsol[UB_MAX_PATH_LEN];
	char spartname2[UB_SYMBOL_NAME_LEN_PLUS];
	char libname2[UB_MAX_PATH_LEN];
	char fullname2[UB_MAX_PATH_LEN];
	char fullsol2[UB_MAX_PATH_LEN];
	char *prompt, *uu_uprompt0(), *uu_uprompt1();
	int mode, hdrfound;
	int file_status, rstat;
	UU_LOGICAL rename,nameok;
	int numint = 0;		 /* number of appropriate user interactions */
	int status = UU_SUCCESS; /* return status; either UU_SUCCESS or UU_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_rename_spart_file()"));

	ud_ldas(UD_DASSTRING, UB_SYMBOL, 1, spartname, UB_SYMBOL_NAME_LEN,
		&numint, UD_NODEFAULT);
		/* prompt is: Enter spart name. */
	if (numint<= 0) goto done;
	if (ubu_symmaster_name_ok(spartname, &nameok) != UU_SUCCESS)
		goto failed;
	if (nameok) 
		numint = 1;	/* we now have an appropriate user response */
	if (numint<= 0) 
		goto done;

	prompt = uu_uprompt0(UB_SYMBOL, 120);
	/* Prompt is: Give spart lib. name; for system area, prefix name with 
	 * \"sys:\". */
	if (uxu_ask_for_lib(&UB_spl_libdata_rec, prompt, libname, &numint, &file_status) 
				!= UU_SUCCESS) goto failed;
	if (numint <=0)
		goto done;
	if (file_status != UX_FAREA) /* we did not get a directory */
		goto failed;
		
	numint = 0;	/* no user interactions yet to return */
	ud_ldas(UD_DASSTRING, UB_SYMBOL, 123, spartname2, UB_SYMBOL_NAME_LEN,
		&numint, UD_NODEFAULT);
	/* prompt is: Enter the new spart file name. */
	if (numint <= 0) goto done;
	if (ubu_symmaster_name_ok(spartname2, &nameok) != UU_SUCCESS)
		goto failed;
	if (nameok) 
		numint = 1;	/* we now have an appropriate user response */
	if (numint<= 0) 
		goto done;

	if (uxu_ask_for_target_lib(&UB_spl_libdata_rec, libname2, &numint, &file_status)
		!= UU_SUCCESS) goto failed;
	if (numint <=0)
		goto done;
	if (file_status != UX_FAREA) /* we did not get a directory */
		goto failed;

	/* compose full target name and check for writeable subdirectory */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname2, spartname2, "UB_SPL_EXTEN",
			"UB_SP_SUFFIX", &mode, &hdrfound, fullname2, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	/* compose full target name and check for writeable subdirectory */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname2, spartname2, "UB_SPL_EXTEN",
			"UB_SP_SOL_SUFFIX", &mode, &hdrfound, fullsol2, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	/* compose full file name and check for its existence in library */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname, spartname, "UB_SPL_EXTEN",
			"UB_SP_SUFFIX", &mode, &hdrfound, fullname, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	if ((mode == (mode | UX_NEXISTS))  /* then the file does not exist */
		|| (mode == (mode | UX_FAREA)))/* then the file is a directory */
	{
		uu_uerror2(UB_SYMBOL, 124, libname, "ubu_rename_spart_file");
		/* error is: Standard part does not exist in %s (%s). */
		goto failed;
	}

	/* compose full file name and check for its existence in library */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname, spartname, "UB_SPL_EXTEN",
			"UB_SP_SOL_SUFFIX", &mode, &hdrfound, fullsol, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	if ((mode == (mode | UX_NEXISTS))  /* then the file does not exist */
		|| (mode == (mode | UX_FAREA)))/* then the file is a directory */
	{
		uu_uerror2(UB_SYMBOL, 124, libname, "ubu_rename_spart_file");
		/* error is: Standard part does not exist in %s (%s). */
		goto failed;
	}
	else	/* both types of the standard part file exists so rename them */
	{
		rstat = ux_rename(fullname, fullname2, UX_PRTERRS);
		if (rstat != UU_SUCCESS)
		{
			if	(rstat == UX_BAD_TARGET)
				/* ask to overwrite target file */
				if (ud_yesno(uu_uprompt1(UB_SYMBOL,125,fullname2)))
				/* prompt: target file name exists, overwrite file? */
				{
					if (ux_delete(fullname2, UX_PRTERRS) != UU_SUCCESS)
						goto failed;
					if (ux_rename(fullname, fullname2, UX_PRTERRS) != UU_SUCCESS)
						goto failed;
					rename = UU_TRUE;
				}
				else
					rename = UU_FALSE;
			else
				goto failed;
		}
		else 
			rename = UU_TRUE;
		if (rename == UU_TRUE)
		{
			if ((rstat = ux_rename(fullsol, fullsol2, UX_PRTERRS)) != UU_SUCCESS)
			{
				if	(rstat == UX_BAD_TARGET)
				{
					if (ux_delete(fullsol2, UX_PRTERRS) != UU_SUCCESS)
						goto failed;
					if (ux_rename(fullsol, fullsol2, UX_PRTERRS) != UU_SUCCESS)
						goto failed;
				}
				else
					goto failed;
			}
		}
	}

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus(" ubu_rename_spart_file",status);
	return(status);
}  
/*********************************************************************
**	 I_FUNCTION :int ubi_mk_spl(area,spl,fulspl, statptr)
**		This function attempts to make the path name to a standard parts library.
**		This also checks to make sure the library exists if requested to.
**	 PARAMETERS	
**		 INPUT  : 
**				area		   Designation of which file area to use; this is
**								either "local", or "system"; can also be UU_NULL, in
**								this case, the path name must be in "spl".
**				spl	   Symbol Library name; if "area" is UU_NULL, then this
**								must be the system dependent path name.
**				statptr		Pointer to the value on which to print errors.
**								Currently statptr can be UU_NULL, in which case
**								no errors are printed, or, *statptr can be UB_NE
**								indicating that errors should be printed if the
**								library does not exist.
**		 OUTPUT :  
**				fulspl  Full path name to the symbol library.
**				statptr		Pointer to the status of library at "fulspl".
**	 RETURNS: UU_SUCCESS if no problems encountered; otherwise UU_FAILURE.
**	 SIDE EFFECTS : This function prints errors if the "fulspl" to be created
**						 1) doesn't correspond to a directory or 2) doesn't exist.
**						 Depending on "statptr" errors maybe printed for (2) as well.
**	 WARNINGS	  : none
*********************************************************************/
int ubi_mk_spl(area, spl,fulspl, statptr)
	char *area;
	char *spl;
	char *fulspl;
	int *statptr;
{
	int status = UU_SUCCESS;
	int mode, hdrfound;
	char errorstrg[80]; /* buffer to print strings in error messages */
	uu_denter(UU_MTRC,(us,"ubi_mk_spl(%x,%s,?,statptr:%x)",
						area, spl, statptr));
#ifdef UU_DEBUGON
	if (area != UU_NULL)
		uu_dprint(UU_BTRC,(us,"area:%s",area));
#endif
	if (area == UU_NULL)
		if (spl == UU_NULL)	goto failed;
		else 
		{
			mode = UX_READ | UX_WRITE; /* check for read and write access */
			if (ux_file_inquire(UU_NULL, UU_NULL, spl, UU_NULL, UU_NULL, 
					&mode, &hdrfound, fulspl, UX_PRTERRS) != UU_SUCCESS) 
			{
				uu_uerror2(UB_SYMBOL, 58, "ubi_mk_spl");
				/* error is: Error in path name to archive,  (%s). */
				goto failed;
			}
			strcpy(errorstrg, "The");
		}
	else if (strcmp(area,"local")==0)
	{
		mode = UX_READ | UX_WRITE; /* check for read and write access */
		if (ux_file_inquire("UB_LOC_SPLDIR",spl,UU_NULL, "UB_SPL_EXTEN",
				"UB_SP_SUFFIX", &mode, &hdrfound, fulspl, UX_PRTERRS) 
				!= UU_SUCCESS) 
		{
			uu_uerror2(UB_SYMBOL, 58, "ubi_mk_spl");
			/* error is: Error in path name to archive,  (%s). */
			goto failed;
		}
		strcpy(errorstrg, "Local");
	}
	else if (strcmp(area, "system") == 0)/* use system symbol file area */
	{
		mode = UX_READ | UX_WRITE; /* check for read and write access */
		if (ux_file_inquire("UB_SYS_SPLDIR",spl,UU_NULL, "UB_SPL_EXTEN",
				"UB_SP_SUFFIX", &mode, &hdrfound, fulspl, UX_PRTERRS) 
				!= UU_SUCCESS) 
		{
			uu_uerror2(UB_SYMBOL, 58, "ubi_mk_spl");
			/* error is: Error in path name to archive,  (%s). */
			goto failed;
		}
		strcpy(errorstrg, "System");
		/* reset default spart library name for subsequent library references */
		UB_spl_libdata_rec.default_firstim = UU_FALSE;
		strcpy(UB_spl_libdata_rec.default_lib,spl);/* only what user supplied*/
	}
	else 
	{
		uu_dprint(UU_MTRC,(us,
			"spart area specification is bad: %s, (ubi_mk_spl)", area));
		goto failed;
	}

	if ( (mode != (mode |UX_FAREA)) && ( mode != (mode|UX_NEXISTS) ) )
	{
		uu_uerror2(UB_SYMBOL, 120, fulspl, "ubi_mk_spl");
		/* No standrd parts library found at %s (%s). */
		goto failed;
	}

	if (statptr != UU_NULL)
		if ((*statptr == UB_NE) && ( mode == (mode | UX_NEXISTS)))
		{
			uu_uerror3(UB_SYMBOL, 74, errorstrg,spl, "ubi_mk_spl");
			/* error is:%s symbol area, %s, doesn't exist (%s). */
			goto failed;
		}

	/* reset default symbol library name for subsequent library references */
	UB_spl_libdata_rec.default_firstim = UU_FALSE;
	strcpy(UB_spl_libdata_rec.default_lib, spl);/* need only what user supplied*/

	if (mode == (mode | UX_FAREA)) /* then the path name is to a directory */
		*statptr = UB_D;
	else /* not a directory */
		if (mode == (mode | UX_NEXISTS))
			*statptr = UB_NE;
		else /* file exists */
			if (hdrfound == UX_FOUND) 
				*statptr = UB_E_X;
			else
				*statptr = UB_E_NX;

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubi_mk_spl",status);
	return(status);
}  
/*********************************************************************
**    I_FUNCTION: int ubu_chk_place_files(formptr)
**    PARAMETERS   
**       INPUT  : 
**				formptr
**       OUTPUT :  
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE 
**				 otherwise.
**    SIDE EFFECTS: none.
**    WARNINGS: NO marks or unmarks in here.
*********************************************************************/
int ub_chk_place_files(formptr, fnameu, fnames)
	struct UB_sym_instance_frm_rec *formptr;
	char *fnameu, *fnames;
{
	UX_pathname libpath;
	int hstat, stat, file_status;
	int status = UU_SUCCESS;		/* return status */

	uu_denter(UU_BTRC, (us, "ubu_chk_place_files()"));

	/* make the full library pathname */
	if (ubi_mk_spl(formptr->area, formptr->lib, libpath, &stat) 
		 != UU_SUCCESS) goto failed;
	ux_strip_quotes(libpath);

	/* load in the file that is this standard part */

	/* initialize mode to test status */
	file_status = UX_EXISTS;
	/* get full unicad part path name and check file's status */
	if (ux_file_inquire(UU_NULL, libpath, formptr->name, UU_NULL,
		"UB_SP_SUFFIX", &file_status, &hstat, fnameu, UX_PRTERRS)
		!= UU_SUCCESS) goto failed;
	if ((file_status == (file_status | UX_NEXISTS)) ||
		(file_status == (file_status | UX_FAREA)) || (hstat == UX_NFOUND) )
	{
		/* then file does not exist, or not xio compatible, can't load file */
		uu_uerror2(UB_SYMBOL, 48, fnameu, "");
		/* error is: File %s does not exist or is not compatible (%s). */
		goto failed;
	} 

	file_status = UX_EXISTS;		/* initialize mode to test status */
	/* get full romulus solids part path name and check file's status */
	if (ux_file_inquire(UU_NULL, libpath, formptr->name, UU_NULL,
		"UB_SP_SOL_SUFFIX", &file_status, &hstat, fnames, UX_PRTERRS)
		!= UU_SUCCESS) goto failed;
	if ((file_status == (file_status | UX_NEXISTS)) ||
		(file_status == (file_status | UX_FAREA))  )
	{
		/* then file does not exist, can't load file */
		uu_uerror2(UB_SYMBOL, 48, fnames, "");
		/* error is: File %s does not exist (%s). */
		goto failed;
	} 

	goto done;

failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("", status);
	return(status);
}
