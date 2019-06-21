
/*******************************************************************
**    NAME:bspl.c
**       CONTAINS:  
**			ubu_get_spart_data
**			ubu_get_place_data
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**           bspfrm.c , 3.1
**    MODULE NAME AND RELEASE LEVEL 
**       bspfrm.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:04
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "mdcpln.h"
#include "bsym.h"		
#include "xfsys1.h"		/* for ux calls and UX_NEXISTS */
#include "xenv1.h"		/* for ux calls and UX_PRTERRS */
extern UU_LOGICAL UR_load_env;
#define TRACE UU_FALSE /* for debugging only */

/*********************************************************************
**    E_FUNCTION : int ubu_get_spart_data(formptr, fullpath)
**			This function gets the data for a standard part form and makes
**			sure the standard part name and s.p. library name is appropriate.
**    PARAMETERS   
**       INPUT  : 
**				formptr		Pointer to the form record to be filled in.
**       OUTPUT :  
**				formptr		Pointer to the filled in form record.
**				fullpath		Full path name.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_get_spart_data(formptr, fullpath)
	struct UB_master_sym_frm_rec *formptr;
	char fullpath[UB_MAX_PATH_LEN];
{
	UU_LOGICAL ud_lyesno();
	static char name[UB_SYMBOL_NAME_LEN_PLUS];/*storage for form */
	static char lib[UB_MAX_PATH_LEN] = "******";
	static UU_REAL splorigin[3];	/* origin for orienting spart */
	struct UB_spart_rec spart;
	char fullname[UB_MAX_PATH_LEN];
	char tempname[UB_MAX_PATH_LEN];
	UU_LOGICAL modified;
	UU_LOGICAL nameok;
	UU_LOGICAL libok;
	UU_LOGICAL found;
	int mode;
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UU_FAILURE */

	uu_denter(UU_BTRC, (us, "ubu_get_spart_data(%x)", formptr));
	/* if not first time through, use established spl name as default */

	um_vctovc(UM_zerovec, splorigin);/* zero out "splorigin" */
	/* make sure there is a default spl */
	if ((strcmp(lib,"******")==0) || (strcmp(lib,"splib")==0)
		|| (strcmp(lib,"SPLIB")==0)) /* get a library from user */
	{
		/* see if library exists, if not ask to create it */
		mode = UX_EXISTS;
		if (ux_mk_chk_syspath("UB_LOC_SPLDIR","splib", UU_NULL, "UB_SPL_EXTEN",
			"UB_SP_SUFFIX", &mode, fullname, UX_PRTERRS) != UU_SUCCESS)
		{
			uu_uerror2(UB_SYMBOL, 58, "ub_get_spart_data");
			/* error is: Error in path name to archive,  (%s). */
			goto failed;
		}

		if (mode == (mode|UX_NEXISTS))	 /* doesn't exist */
		{
		 	/* then ask user if it should be created and set to the default 
			 * library */
			if (ud_lyesno(UB_SYMBOL, 126)) /* create it? */
			{
				mode = 0777;
				if (ux_mk_dir(fullname,mode,UX_PRTERRS) != UU_SUCCESS) goto failed;
				strcpy(lib, "splib");
			}
		}
		else strcpy(lib, "splib");
	}

	strcpy(name, "******");
	/* get storage for form */
	formptr->name = name;
	formptr->lib = lib;

	/* get spart origin; default spart origin is cp origin */
	um_vctovc(UM_cpln.origin, splorigin);
	formptr->origin = splorigin;

	nameok = UU_FALSE;
	if (ubi_master_sym_form("bspart.frm", formptr)	!= UU_SUCCESS) 
		goto failed;
/*
......remove this function
......Yurong 9/21/98
*/
/*
	if (uxu_lib_name_ok(&UB_spl_libdata_rec, formptr->name, &nameok)
			!= UU_SUCCESS) goto failed;
*/
	strcpy(spart.name, formptr->name);
	if (ubu_get_spl_name(formptr->lib, fullpath, &libok) != UU_SUCCESS) 						goto failed;
	if (!libok)
		strcpy(formptr->lib, "******");

	/* note we have an appropriate spart name and spl name; also,
	 * there is no spart currently in UNIBASE with the returned name,
	 * and there exists a sp library with the returned library name. */


	/* check to see if user will overwrite existing spart file and ask */
	mode = UX_EXISTS;

	/* note that work is done only in the local area. make full path */
	if (ux_mk_chk_syspath("UB_LOC_SPLDIR",formptr->lib,
			formptr->name,"UB_SPL_EXTEN","UB_SP_SUFFIX",&mode,
			fullpath, UX_PRTERRS) != UU_SUCCESS) goto failed;

	uu_dprint(UU_BTRC,(us,"mode is: %d, for %s", mode, fullpath));
	if (mode != (mode|UX_NEXISTS))	/* then the named file already exists */
	{	
		if (ud_yesno(uu_uprompt1(UB_SYMBOL,127,fullpath)))
		{
			if (ux_delete((fullpath), UX_PRTERRS) != UU_SUCCESS)
			{  
				uu_uerror2(UB_SYMBOL, 122, fullpath, "ubu_get_spart_data");
				/* error message: Can't delete standard part %s (%s). */
				goto failed;
			}
		}
		else goto failed;
	}


#if (TRACE)
	uu_dprint(UU_BTRC,(us," form data: spart name:%s, lib:%s, origin:%g,%g,%g",
			formptr->name, formptr->lib, 
			formptr->origin[0], formptr->origin[1], formptr->origin[2]));
#endif
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubu_get_spart_data",status);
	return(status);
}


/*********************************************************************
**    E_FUNCTION : int ubu_get_place_data(formptr)
**			This function gets the data for a standard part place form.
**    PARAMETERS   
**       INPUT  : 
**				formptr		Pointer to the form record to be filled in.
**       OUTPUT :  
**				formptr		Pointer to the filled in form record.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_get_place_data(formptr)
	struct UB_sym_instance_frm_rec *formptr;
{
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC, (us, "ubu_get_place_data()"));

	formptr->name = UB_default_spart_name;
	formptr->lib = UB_spl_libdata_rec.default_lib;
	formptr->area = UB_spl_libdata_rec.default_area;
	formptr->scale = UB_spl_default_scale;
	formptr->angle = UB_spl_default_angle;

	/* put up the form and accept the responses */
	if (ubi_sym_instance_form("bplacesp.frm", formptr)
		!= UU_SUCCESS) goto failed;


	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubu_get_place_data",status);
	return(status);


}
