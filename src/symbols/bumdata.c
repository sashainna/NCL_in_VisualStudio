/*********************************************************************
**    NAME:  bumdata.c
**       CONTAINS:
**				ubu_get_symmaster_data
**				ubu_symmaster_name_ok
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bumdata.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:06
*********************************************************************/
#include "usysdef.h"
#include "uhep.h"     	/* for error system */
#include "udebug.h"
#include "umoveb.h"
#include "dasnog.h"		/* for DAS input types; e.g. UD_DASSTRING */
#include "mdattr.h"		/* for modelling colors */
#include "mdcpln.h"		/* for construction plane definition */
#include "mdcoord.h"		/* for UM_zerovec */
#include "xenv1.h"
#include "xfsys1.h"
#include "bsym.h"

#define TRACE UU_FALSE	/* for debugging only */
/*********************************************************************
**    E_FUNCTION : int ubu_get_symmaster_data(formptr, fullpath)
**			This function gets the data for a master symbol form and makes
**			sure the symbol name and library name is appropriate.
**    PARAMETERS   
**       INPUT  : 
**				formptr		Pointer to the form record to be filled in.
**       OUTPUT :  
**				formptr		Pointer to the filled in form record.
**				fullpath		Full path name.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_get_symmaster_data(formptr, fullpath)
	struct UB_master_sym_frm_rec *formptr;
	char fullpath[UB_MAX_PATH_LEN];
{
	UU_LOGICAL ud_lyesno();
	static char name[NCL_MAX_LABEL_AND_SUBSCRIPT];
	static char lib[UB_MAX_PATH_LEN] = "******";
	static UU_REAL symorigin[3];	/* origin for orienting symbol */
	struct UB_symbol_rec sym;
	char fullname[UB_MAX_PATH_LEN];
	UU_LOGICAL nameok;
	UU_LOGICAL libok;
	UU_LOGICAL found;
	int mode, hdrfound;
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_get_symmaster_data(%x)", formptr));
	/* if not first time through, use established sym. lib. name as default */
	if ( UB_libdata_rec.default_firstim == UU_FALSE)
		strcpy(lib,UB_libdata_rec.default_lib);

	um_vctovc(UM_zerovec, symorigin);/* zero out "symorigin" */
	/* make sure there is a default symbol library */
	if ((strcmp(lib,"******")==0) || (strcmp(lib,"symlib")==0)
		|| (strcmp(lib,"SYMLIB")==0)) /* get a library from user */
	{
		/* see if UB_LOC_M_SYMDIR.symlib exists, if not ask to create it */
		mode = UX_READ | UX_WRITE; /* check for read and write access */
		if (ux_file_inquire("UB_LOC_M_SYMDIR", "symlib", UU_NULL, "UB_SYM_EXTEN",
			"UB_SYM_SUFFIX", &mode,&hdrfound, fullname, UX_PRTERRS)
			!= UU_SUCCESS)
		{
			uu_uerror2(UB_SYMBOL, 58, "ubu_get_symmaster_data");
			/* error is: Error in path name to symbol archive,  (%s). */
			goto failed;
		}
		if (mode == (mode|UX_NEXISTS))  /* doesn't exist */
		{
		 	/* then ask user if it should be created and set to the default 
			 * library */
			if (ud_lyesno(UB_SYMBOL, 29)) /* create it? */
			{
				mode = 0777;
				if (ux_mk_dir(fullname,mode,UX_PRTERRS) != UU_SUCCESS) goto failed;
				strcpy(lib, "symlib");
				/* reset default sym lib name for subsequent library references */
				UB_libdata_rec.default_firstim = UU_FALSE;
				strcpy(UB_libdata_rec.default_lib, lib);
				/* need only what user supplied*/
			}
		}
		else
		{
			if (mode == (mode | UX_FAREA))
			{
				strcpy(lib, "symlib");
				/* reset default symbol lib name for subsequent lib references */
				UB_libdata_rec.default_firstim = UU_FALSE;
				strcpy(UB_libdata_rec.default_lib, lib);
				/* need only what user supplied*/
			}
			else
			{  
				uu_uerror2(UB_SYMBOL, 44, fullname, "ubu_get_symmaster_data");
				/* error message: No master symbol library found at %s  (%s). */
				goto failed;
			}
		}
	}

	strcpy(name, "******");
	/* get storage for master symbol form */
	formptr->name = name;
	formptr->lib = lib;

	/* get symbol origin; default symbol origin is cp origin */
	um_vctovc(UM_cpln.origin, symorigin);
	formptr->origin = symorigin;

	if (ubi_master_sym_form("bmaster.frm", formptr)	!= UU_SUCCESS) 
		goto failed;

	if (ubu_symmaster_name_ok(formptr->name, &nameok) != UU_SUCCESS)
		goto failed;

	if (nameok) /* look for lib name */
	{
		ncl_parse_label(formptr->name,sym.label,&sym.subscr);
		if (ub_get_symmaster_by_name(&sym, &found,1,1) != UU_SUCCESS)
			goto failed;
		if (found)
/* 
.....prompt is:Master symbol, %s, is already 
.....in the data base; overwrite? 
*/
		{
			if (!ud_yesno(0, uu_uprompt1(UB_SYMBOL, 21, sym.label), 
						"Create Symbol"))			
					goto failed; /* don't continue */
		}

		if (ubu_get_lib_name(formptr->lib, fullpath, &libok) != UU_SUCCESS) 						goto failed;
		if (!libok)
			goto failed;
	}
	else /* sym name not ok */
		goto failed;	
				
#if (TRACE)
	uu_dprint(UU_BTRC,(us," form data: symname:%s, lib:%s, origin:%g,%g,%g",
			formptr->name, formptr->lib, 
			formptr->origin[0], formptr->origin[1], formptr->origin[2]));
#endif
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION : int ubu_symmaster_name_ok(name, statusptr)
**			This function determines whether "name" is a legal master
**			symbol name.
**    PARAMETERS   
**       INPUT  : 
**          name			Proposed name of the symbol master.
**       OUTPUT :  
**			 name				If "*statusptr" = UU_TRUE then this is the 
**								name to be used for the master symbol.
**        statusptr		Pointer to the return status; UU_TRUE iff
**								the returned name is ok.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE 
**				 otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_symmaster_name_ok(name, statusptr)
	char name[];
	UU_LOGICAL *statusptr;
{
	UU_LOGICAL nameok;
	UU_LOGICAL modified;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubu_symmaster_name_ok(%s,?)",name));

	/* remove beginning and ending blanks and check for illegal 
	 * characters */
	if (uxi_get_legal_name(name, &modified) != UU_SUCCESS) 
	{
		uu_uerror3(UB_SYMBOL, 67, "Symbol ", name, "ubu_symmaster_name_ok");
		/* error is: %s name, %s, is illegal  (%s). */
		nameok = UU_FALSE;
		goto done;
	}
	if (modified)
		/* ask user if new name is ok */
		if (ud_yesno(0, uu_uprompt2(UB_SYMBOL, 30, "symbol", name), "Question?"))
		{
			nameok = UU_TRUE;
		}
		else /* name is not ok */
		{
			nameok = UU_FALSE;
			goto done;
		}
	else /* name not modified and it's ok */
	{
		nameok = UU_TRUE;
	}

done:;
	*statusptr = nameok;
	uu_dprint(UU_BTRC,(us,"ubu_symmaster_name_ok returns:%s,%d", 
					name,*statusptr));
	uu_dexit;
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ubu_get_lib_name(name, fullpath, libokptr)
**		This function determines whether "name" is a legal master symbol
**		library name and determines whether "name" is already the name
**		of a local symbol library.
**    PARAMETERS   
**       INPUT  : 
**				name			Proposed name of the symbol library.
**       OUTPUT :  
**				name			If "*statusptr" = UU_TRUE then this is the name of 
**								an existing symbol library to be returned.
**				fullpath		Full path name to the symbol library.
**				libokptr		Pointer to the return status; UU_TRUE iff the returned
**								name is ok.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE 
**				 otherwise.
**    SIDE EFFECTS: none.
**    WARNINGS: NO marks or unmarks in here.
*********************************************************************/
int ubu_get_lib_name(name, fullpath, libokptr)
	char name[];
	char fullpath[UB_MAX_PATH_LEN];
	UU_LOGICAL *libokptr;
{
	char path_prefix[UB_MAX_PATH_LEN];
	char dir[UB_MAX_PATH_LEN], bname[UB_MAX_PATH_LEN];
	int mode, hdrfound;
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC, (us, "ubu_get_lib_name(%s,?,?)",name));
/*
......remove because we allow path in the library name
......but '/' is not a legal char in uxu_lib_name_ok
......Yurong 9/18/98
*/
/*
	if (uxu_lib_name_ok(&UB_libdata_rec, name, libokptr) != UU_SUCCESS) 
				goto failed;            
	if (!(*libokptr))
			goto done;
*/
	UB_GET_FAREA_IDENT(UB_libdata_rec.default_area, path_prefix);

	mode = UX_READ | UX_WRITE; /* check for read and write access */
/*
.....if symlib already has a path, ignore symbol area
.....Yurong changed 9/18/98
*/
	ul_break_fname(name, dir, bname);
	if (dir[0]!='\0')
	{
		status = ux_file_inquire(UU_NULL, name, UU_NULL, "UB_SYM_EXTEN",
						"UB_SYM_SUFFIX", &mode,&hdrfound, fullpath, UX_PRTERRS);
	}
	else
	{
		status = ux_file_inquire(path_prefix, name, UU_NULL, "UB_SYM_EXTEN",
						"UB_SYM_SUFFIX", &mode,&hdrfound, fullpath, UX_PRTERRS);
	}
	if (status != UU_SUCCESS)
/*
	if (ux_file_inquire(path_prefix, name, UU_NULL, "UB_SYM_EXTEN",
			"UB_SYM_SUFFIX", &mode,&hdrfound, fullpath, UX_PRTERRS)
			!= UU_SUCCESS)
*/
	{
		uu_uerror2(UB_SYMBOL, 58, "ub_get_lib_name");
		/* error is: Error in path name to symbol archive,  (%s). */
		goto failed;
	}
	if ((mode != (mode|UX_FAREA)) && (mode != (mode|UX_NEXISTS)))
	{  
		uu_uerror2(UB_SYMBOL, 44, fullpath, "ub_get_lib_name");
		/* error message: No master symbol library found at %s  (%s). */
		goto failed;
	}
	/* reset default symbol library name for subsequent library references */
	UB_libdata_rec.default_firstim = UU_FALSE;
	strcpy(UB_libdata_rec.default_lib, name);/* need only what user supplied*/

	/* then either we have a directory or nothing */
	if ( mode != (mode|UX_FAREA)) 
	{
		uu_uerror2(UB_SYMBOL, 71, name, "ubu_get_lib_name");
		/* error is: No symbol library by this name exists (%s). */
		*libokptr = UU_FALSE;
		goto done;
	}

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	*libokptr = UU_TRUE;
	uu_dexit;
	return(status);
}	

