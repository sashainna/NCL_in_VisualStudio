/*********************************************************************
**	 NAME:  bmlib.c
**		 CONTAINS:
**				ubi_mk_libname
**				ubi_load_file
**	 COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**	 MODULE NAME AND RELEASE LEVEL 
**       bmlib.c , 25.3
**  DATE AND TIME OF LAST MODIFICATION
**       01/20/17 , 11:36:15
*********************************************************************/
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "ustdio.h"
#include "nclver.h"
#include "uhep.h"	  		/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "xenv1.h"
#include "xfsys1.h"
#include "xfsys2.h"
#include "xfsys0.h"
#include "bsym.h"
#define TRACE UU_FALSE	/* for debugging only */

void ubi_update_symbol();

/*********************************************************************
**	 I_FUNCTION :int ubi_mk_libname(area,libname,fullibname, statptr)
**		This function attempts to make the path name to a symbol library.
**		This also checks to make sure the library exists if requested to.
**	 PARAMETERS	
**		 INPUT  : 
**				area		   Designation of which file area to use; this is
**								either "local", or "system"; can also be UU_NULL, in
**								this case, the path name must be in "libname".
**				libname	   Symbol Library name; if "area" is UU_NULL, then this
**								must be the system dependent path name.
**				statptr		Pointer to the value on which to print errors.
**								Currently statptr can be UU_NULL, in which case
**								no errors are printed, or, *statptr can be UB_NE
**								indicating that errors should be printed if the
**								library does not exist.
**				prterr		Flag to output error messages UX_PRTERRS or
**							UX_NPRTERRS.
**		 OUTPUT :  
**				fullibname  Full path name to the symbol library.
**				statptr		Pointer to the status of library at "fullibname".
**	 RETURNS: UU_SUCCESS if no problems encountered; otherwise UB_FAILURE.
**	 SIDE EFFECTS : This function prints errors if the "fullibname" to be created
**						 1) doesn't correspond to a directory or 2) doesn't exist.
**						 Depending on "statptr" errors maybe printed for (2) as well.
**	 WARNINGS	  : none
*********************************************************************/
int ubi_mk_libname(area, libname,fullibname, statptr,prterr)
	char *area;
	char *libname;
	char *fullibname;
	int *statptr,prterr;
{
	int stat, status = UU_SUCCESS;
	int mode, hdrfound;
	char dir[UB_MAX_PATH_LEN], bname[UB_MAX_PATH_LEN], fullstr[UB_MAX_PATH_LEN];
	char errorstrg[80]; /* buffer to print strings in error messages */
	uu_denter(UU_MTRC,(us,"ubi_mk_libname(%x,%s,?,statptr:%x)",
						area, libname, statptr));
#ifdef UU_DEBUGON
	if (area != UU_NULL)
		uu_dprint(UU_BTRC,(us,"area:%s",area));
#endif
/*
.....if libname is a path name, ignore area and copy libname to
.....fullibname and return
*/
	stat = ul_get_full_dir (libname, fullstr);
	if (stat==UU_SUCCESS)
	{
		strcpy(fullibname, fullstr);
		return UU_SUCCESS;
	}
	if (area == UU_NULL)
		if (libname == UU_NULL)	goto failed;
		else 
		{
			mode = UX_READ | UX_WRITE; /* check for read and write access */
			if (ux_file_inquire(UU_NULL, UU_NULL, libname, UU_NULL, UU_NULL, 
					&mode, &hdrfound, fullibname, UX_PRTERRS) != UU_SUCCESS) 
			{
				if (prterr == UX_PRTERRS)
					uu_uerror2(UB_SYMBOL, 58, "ubi_mk_libname");
				/* error is: Error in path name to symbol archive,  (%s). */
				goto failed;
			}
			strcpy(errorstrg, "The");
		}
	else if (strcmp(area,"local")==0)
	{
		mode = UX_READ | UX_WRITE; /* check for read and write access */
/*
.....if symlib already has a path, ignore symbol area
.....Yurong changed 9/18/98
*/
		ul_break_fname(libname, dir, bname);
		if (dir[0]!='\0')
		{
			status = ux_file_inquire(UU_NULL, libname,UU_NULL, "UB_SYM_EXTEN",
						"UB_SYM_SUFFIX", &mode, &hdrfound, fullibname, UX_PRTERRS);
		}
		else
		{
			status = ux_file_inquire("UB_LOC_M_SYMDIR",libname,UU_NULL, 
							"UB_SYM_EXTEN", "UB_SYM_SUFFIX", &mode, &hdrfound,
							fullibname, UX_PRTERRS);
		}
/*
		if (ux_file_inquire("UB_LOC_M_SYMDIR",libname,UU_NULL, "UB_SYM_EXTEN",
				"UB_SYM_SUFFIX", &mode, &hdrfound, fullibname, UX_PRTERRS) 
				!= UU_SUCCESS) 
*/
		if (status != UU_SUCCESS)
		{
			if (prterr == UX_PRTERRS)
				uu_uerror2(UB_SYMBOL, 58, "ubi_mk_libname");
			/* error is: Error in path name to symbol archive,  (%s). */
			goto failed;
		}
		strcpy(errorstrg, "Local");
	}
	else if (strcmp(area, "system") == 0)/* use system symbol file area */
	{
		mode = UX_READ | UX_WRITE; /* check for read and write access */
/*
.....if symlib already has a path, ignore symbol area
.....Yurong changed 9/18/98
*/
		ul_break_fname(libname, dir, bname);
		if (dir[0]!='\0')
		{
			status = ux_file_inquire(UU_NULL, libname,UU_NULL, "UB_SYM_EXTEN",
						"UB_SYM_SUFFIX", &mode, &hdrfound, fullibname, UX_PRTERRS);
		}
		else
		{
			status = ux_file_inquire("UB_SYS_M_SYMDIR",libname,UU_NULL, 
							"UB_SYM_EXTEN", "UB_SYM_SUFFIX", &mode, &hdrfound,
							fullibname, UX_PRTERRS);
		}
/*
		if (ux_file_inquire("UB_SYS_M_SYMDIR",libname,UU_NULL, "UB_SYM_EXTEN",
				"UB_SYM_SUFFIX", &mode, &hdrfound, fullibname, UX_PRTERRS) 
				!= UU_SUCCESS) 
*/
		if (status != UU_SUCCESS)
		{
			if (prterr == UX_PRTERRS)
				uu_uerror2(UB_SYMBOL, 58, "ubi_mk_libname");
			/* error is: Error in path name to symbol archive,  (%s). */
			goto failed;
		}
		strcpy(errorstrg, "System");
		/* reset default symbol library name for subsequent library references */
		UB_libdata_rec.default_firstim = UU_FALSE;
		strcpy(UB_libdata_rec.default_lib,libname);/* only what user supplied*/
	}
	else 
	{
		uu_dprint(UU_MTRC,(us,
			"symbol area specification is bad: %s, (ubi_mk_libname)", area));
		goto failed;
	}

	if ( (mode != (mode |UX_FAREA)) && ( mode != (mode|UX_NEXISTS) ) )
	{
		if (prterr == UX_PRTERRS)
			uu_uerror2(UB_SYMBOL, 44, fullibname, "ubi_mk_libname");
		/* No master symbol library found at %s (%s). */
		goto failed;
	}

	if (statptr != UU_NULL)
		if ((*statptr == UB_NE) && ( mode == (mode | UX_NEXISTS)))
		{
			if (prterr == UX_PRTERRS)
				uu_uerror3(UB_SYMBOL, 74, errorstrg,libname, "ubi_mk_libname");
			/* error is:%s symbol area, %s, doesn't exist (%s). */
			goto failed;
		}

	/* reset default symbol library name for subsequent library references */
	UB_libdata_rec.default_firstim = UU_FALSE;
	strcpy(UB_libdata_rec.default_lib, libname);/* need only what user supplied*/

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
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}  
/*********************************************************************
**	 I_FUNCTION: int ubi_load_file(area, libname, symname, loadfunct,option,
**												symptr)
**			This function attempts to load a master symbol into UNIBASE;
**			afterwhich the master symbols in UNIBASE are updated; this 
**			includes checking for duplicates; updating
**			version, name, and path fields. Note, the UNIBASE bit map MAY NOT
**			be reset here; resetting depends on the load function used.
**	 PARAMETERS	
**		 INPUT  : 
**				area			User's local file area; note, this can be a symbolic
**								name from the Unicad initialization file; this can
**								also be UU_NULL.
**				libname		Library (i.e. directory) name; can be a symbolic
**								name, can be UU_NULL; note, this can not be the 
**								full path name up to the file name (or including the
**								file name) unless the parameter, "area" is
**								UU_NULL.
**				symname		Base name of file to load; can not be symbolic;
**								can not be UU_NULL. This should be the name of the
**								symbol as it appears in UNIBASE.
**				loadfunct	Function to use to load the files in a directory;
**								if nothing peculiar needs to be done, then this 
**								parameter may be UU_NULL; in this case the UNIBASE 
**								load (i.e. merge) function will be called and an 
**								error message will be printed if the UNIBASE load
**								function fails; if this is not what you want, then
**								supply our own function; note, this function MUST
**								RETURN UU_SUCCESS IF NOTHING WENT WRONG. This 
**								function should have a single parameter, the 
**								fullpath name to the file. No error messages are
**								printed in response to YOUR "loadfunct" if it does 
**								not return UU_SUCCESS.
**				option		If "WHOLESYM" then the entire master will be 
**								returned; otherwise, only the fixed part will be 
**								returned
**				prterr		Flag to output error messages UX_PRTERRS or
**							UX_NPRTERRS.
**		 OUTPUT :			
**				symptr		If not UU_NULL on input, then this is 
**								the pointer to the master symbol loaded.
**	 RETURNS: UU_SUCCSS if no problems encountered; UB_FAILURE otherwise.
**	 SIDE EFFECTS : none
**	 WARNINGS	  : Note, if the root master symbol can't be found in the
**						symbol file, then all master symbols marked "new" in
**						UNIBASE will be deleted.
*********************************************************************/
int ubi_load_file(area, libname, symname, loadfunct,option,symptr,prterr)
	char *area;
	char *libname;
	char *symname;
	int (*loadfunct)();
	char *option;
	int prterr;
	struct UB_symbol_rec *symptr;
{
	char libpath[UB_MAX_PATH_LEN], direc[UB_MAX_PATH_LEN], name[UB_MAX_PATH_LEN];
	char ag_libpath[UB_MAX_PATH_LEN];
	int mode, hdrfound;
	int stat = UB_NE;
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC,(us,
	"ubi_load_file(area:%x,lib:%x,symname:%s,loadfunct:%x,%s,symptr:%x)",
				area,libname,symname,loadfunct,option,symptr));
#ifdef UU_DEBUGON
	if (area != UU_NULL)
		uu_dprint(UU_BTRC,(us,"area:%s", area));
	if (libname != UU_NULL)
		uu_dprint(UU_BTRC,(us,"libname:%s", libname));
#endif
/*
.....check if the symname have the path, if it is, replace the default libpath
*/
	ul_break_fname(symname, direc, name);
	if (direc[0]!='\0')
	{
		strcpy(libpath, direc);
		strcpy(symname, name);
		if (area!=NULL)
			area[0] = '\0';
		if (libname!=NULL)
			strcpy(libname, direc);
	}
	else
	{
		if (ubi_mk_libname(area,libname,libpath,&stat,prterr) != UU_SUCCESS)
		goto failed;
	}
	if ((stat = uxu_load_file(UU_NULL, libpath, symname, UU_NULL,
		"UB_SYM_SUFFIX", loadfunct)) == UX_FAILURE) goto failed;
	if (stat == UX_BAD_FILE)
	{
		/* use previously mentioned library name */
		if (prterr == UX_PRTERRS)
			uu_uerror2(UB_SYMBOL, 25, symname, "ubi_load_file");
		/* error is: Can not find master symbol %s (%s). */
		goto failed;
	}

	/* handle APPLIED GEOMETRY data here for now */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	ux_file_inquire(UU_NULL, libpath, symname, UU_NULL,
		"UR_AG_FILE", &mode, &hdrfound, ag_libpath, UX_PRTERRS); 
	um_load_appgeo(ag_libpath, UU_FALSE);
	um_post_load_appgeo();
	if (ubi_FixMsymsAfterMsymLoad(area,libname,symname,option,symptr) 
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
**	 E_FUNCTION: ubi_update_symbol(symname)
**			This function updates a symbol's entity's attributes after
**			loading the symbol from an external Unibase.
**	 PARAMETERS	
**		 INPUT  : 
**				symname		Name of symbol to update.
**		 OUTPUT :			
**	 RETURNS: none
**	 SIDE EFFECTS : none
**	 WARNINGS	  : none
*********************************************************************/
void ubi_update_symbol(symname)
char *symname;
{
	int rel_num,i;
	UU_LOGICAL found;
	struct UB_symbol_rec symbol;
/*
.....Get the symbol structure
*/
	ncl_parse_label(symname,symbol.label,&symbol.subscr);
	if (ub_get_symmaster_by_name(&symbol,&found,1,1) == UU_SUCCESS)
	{
/*
.....Update symbol attributes
*/
		if (found && NCL_infile_version != NCL_version)
		{
			for (i=0;i<symbol.no_geom;i++)
			{
				ur_retrieve_data_relnum(symbol.geom[i],&rel_num);
				ur_update_attribut(symbol.geom[i],rel_num);
			}
		}
	}
}

