/*********************************************************************
**    NAME:  bsavsym.c
**       CONTAINS:
**    		int ubu_mk_master_sym(symptr, libname, tfmat, attrptr)
**				ubu_savsym
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       bsavsym.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:04
**      
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdcoord.h"
#include "dmark.h"
#include "dasnog.h"
#include "mdclass.h"
#include "mdrel.h"
#include "ribase.h"
#include "bsym.h"
#include "xenv1.h"
#include "xfsys1.h"

#define TRACE UU_FALSE	/* for debugging only */
/*********************************************************************
**    I_FUNCTION: int ubu_mk_master_sym(symptr, libname, tfmat, attrptr)
**       This function tries to save a master symbol. In addition, any current
**			symbol by the same name in UNIBASE will have its instances associated
**			with the new master symbol.
**    PARAMETERS   
**       INPUT  : 
**          symptr		Pointer to the master symbol to be saved.
**				libname		The final directory in which the master
**								symbol definition goes.
**				tfmat			Transformation for making the master symbol.
**				attrptr		Pointer to the master symbol attribute bundle.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered; UB_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ubu_mk_master_sym(symptr, libname, tfmat, attrptr)
struct UB_symbol_rec *symptr;
char libname[];
UM_transf tfmat;
struct UB_symattr_rec *attrptr;
{
	struct UB_symbol_rec oldsym;
	UU_LOGICAL found;
	int oltnodes2use;
	int numint;
	char dummy[3];
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubu_mk_master_sym(symptr>key:%d,%s,tfmat,attrptr)",
						symptr->key, libname));
	strcpy(oldsym.label, symptr->label);
	oldsym.subscr = symptr->subscr;
	if (ub_get_symmaster_by_name(&oldsym, &found, 1,1) != UU_SUCCESS)
			goto failed;
	/* note, no instances are referenced by the master symbol yet */
	if (ub_create_symbol_tuple(symptr,tfmat,attrptr) != UU_SUCCESS) 
			goto failed;
	/* now save the master symbol away; don't ask about any current version of
	 * of the file; this function prints errors */
	if (ubu_savsym(symptr, symptr->path, UU_FALSE) != UU_SUCCESS)
	{
		ud_ldas(UD_DASSTRING, UB_SYMBOL, 32, dummy, sizeof(dummy), &numint,
							UD_NODEFAULT);
		/* prompt is: hit any key to continue. */
		uu_uerror1(UB_SYMBOL, 70, "ubu_mk_master_sym");
		/* Error is: Master symbol not created  (%s). */
		if (ur_delete_all(symptr->key) != UU_SUCCESS)
		{
			uu_dprint(UU_BTRC,(us,"ur_delete_all failed"));
			goto failed;
		}
		goto failed;
	}
	if (found)
		if (ubu_ReplaceMaster(&oldsym, symptr) != UU_SUCCESS)
					goto failed;
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION: int ubu_savsym(symptr, libname, askuser) 
**			Save the master symbol definition. Assume no solids in master symbols.
**			Note, the symbol is saved under its "name" in the library.
**			We also fill in the path name, modification date of the file, and
**			initialize the instance pointer field (to UU_NULL).
**    PARAMETERS   
**       INPUT  : 
**				symptr		Pointer to the master symbol to be saved.
**				libname		The final directory in which the master
**								symbol definition goes.
**				askuser		UU_TRUE iff we are to ask user whether the old version
**								should really be deleted.
**       OUTPUT :   none
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE
**					 otherwise.
**    SIDE EFFECTS : does a selective save of entities.
**    WARNINGS     : none
*********************************************************************/

int ubu_savsym(symptr, libname, askuser)
struct UB_symbol_rec *symptr;
char libname[];
UU_LOGICAL askuser;
{
	UU_LOGICAL delfile;
	char tempath[UB_MAX_PATH_LEN];
	char ag_tempath[UB_MAX_PATH_LEN];
	char ag_libname[UB_MAX_PATH_LEN];
	char tempname[UB_SYMBOL_NAME_LEN];
	char label[UB_SYMBOL_NAME_LEN_PLUS];
	struct UB_symbol_rec tempmaster;
	struct UB_symattr_rec attr;
	UM_transf tfmat;
	UU_LOGICAL copymade;
	int mode, hdrfound;
	int file_status, i, status = UU_SUCCESS;	

	uu_denter(UU_BTRC, (us, "ubu_savsym(symptr->key:%d,%s)",
						symptr->key, libname));

	if (!ur_unibase_used())	/* if nothing in data base, tell'em and exit */
	{ 
		/* error message is: Error, nothing in UNIBASE to save (%s). */
		uu_uerror1(UB_SYMBOL, 29, "ubu_savsym");
		goto failed;
	}
/*
.....Create filename with optional subscript
*/
	ncl_get_label(symptr,label);

	/* get full path name and check to see if it exists and its status */
	strcpy(ag_libname, libname); /* save symbol name */
	mode = UX_READ | UX_WRITE; /* check for read and write access */
	if (ux_file_inquire(UU_NULL, libname, label,UU_NULL,
		"UB_SYM_SUFFIX", &mode, &hdrfound, symptr->path, UX_PRTERRS) 
		!= UU_SUCCESS) goto failed;

	/* Handle APPLIED GEOMETRY file name here */
	ux_file_inquire(UU_NULL, ag_libname, label, UU_NULL,
			"UR_AG_FILE", &mode, &hdrfound, ag_tempath, UX_PRTERRS); 

	delfile = UU_FALSE; /* assume any file should not be deleted */
	if (mode!=(mode|UX_NEXISTS)) /* then file exists */
	{
		if (askuser)
			/* prompt: master symbol exists; delete old version? (y/n). */
			if (ud_lyesno(UB_SYMBOL,12))
					delfile = UU_TRUE;
			else goto failed;
		else /* assume the file should be deleted */
			delfile = UU_TRUE;
		if (delfile) /* delete the file */
		{
			if (ux_delete((symptr->path), UX_PRTERRS) != UU_SUCCESS)
			{  /* then can't delete old version */
				uu_uerror2(UB_SYMBOL, 30, symptr->path, "ubu_savsym");
				/* error message: Can't delete old version of master symbol
				 * (%s). */
				goto failed;
			}
			/* Handle APPLIED GEOMETRY file delete here */
			ux_delete(ag_tempath, UX_PRTERRS);
		}
	}
	strcpy(tempname, symptr->label); /* save symbol name */
	/* put in special name for identification in symbol file */
	strcpy(symptr->label, UB_ROOTMASTER); 

	/* strip off quotes on the full pathname before sending it on */
	strcpy(tempath,symptr->path);
	ux_strip_quotes(tempath);

	/* save away full path name in the path field */
	if (ubi_update_data_fixed(symptr) != UU_SUCCESS)
			goto failed;

	uu_move_byte(symptr, &tempmaster, sizeof(tempmaster));

	/* if there are instances of this master, then make a copy and save the
	 * copy; don't want instances in master symbol file. */
	if (symptr->no_inst != 0)
	{
		uu_dprint(UU_BTRC,(us,"making copy of master for save"));
		tempmaster.no_inst = 0;
		if (ub_get_sym_attr(symptr->key, &attr) != UU_SUCCESS) goto failed;
		if (ub_get_sym_transf(symptr->key, tfmat) != UU_SUCCESS) goto failed;
		if (ub_create_symbol_tuple(&tempmaster, tfmat, &attr) != UU_SUCCESS)
			goto failed;
		copymade = UU_TRUE;
	}
	else copymade = UU_FALSE;
	/* clear all bits indicating a tuple is to be saved */
	if (ur_svld_clear() != 0)
	{
		uu_uerror1(UB_SYMBOL, 77, "ubu_savsym");
		/* error is:Can't clear save/load bit map (%s) */
		goto failed;
	}
	/* set bit to indicate the master symbol tuple is to be saved */
	if (ur_save_set(tempmaster.key) != 0)
	{
		uu_uerror2(UB_SYMBOL, 31, tempmaster.key, "ubu_savsym");
		/* error message: Can't set flag to save master symbol (sub)entity
		 * with key=%d (%s).
		 */
		goto failed;
	}
	/* go do Unibase save */
	/* if (ur_sp02(symptr->path) != 0)	replaced with not-quoted name */
	if (ur_sp02(tempath) != 0)
	{
		uu_uerror2(UB_SYMBOL, 32, symptr->path, "ubu_savsym");
		/* error message: Error in archiving master symbol: %s (%s). */
		goto failed;
	}

	/* now save APPLIED GEOM */
/*	um_selectivesave_appgeo(ag_tempath);*/

	if (copymade)
	{
		uu_dprint(UU_BTRC,(us,"DELETING TEMPORARY COPY OF MASTER"));
		if (ur_delete_all(tempmaster.key) != UU_SUCCESS) goto failed;
	}
	/* now fill in modification date */
	if (ux_get_fversion(symptr->path, &(symptr->version), UX_PRTERRS)
		!= UU_SUCCESS) goto failed;
	strcpy(symptr->label, tempname); /* restore symbol name */
	/* update UNIBASE record with file version number */
	if (ubi_update_data_fixed(symptr) != UU_SUCCESS)
			goto failed;
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
