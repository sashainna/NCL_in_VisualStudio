/*********************************************************************
**    NAME         :	m4esavld.c
**       CONTAINS: routines to save/load/merge AG entities
**			int um_save_appgeo(fname)
**			int um_selectivesave_appgeo(fname)
**			int um_save_agcrv(fd)
**			int um_save_agsrf(fd)
**			int um_save_agshell(fd)
**			int um_load_appgeo(fname, load)
**			int um_post_load_appgeo()
**			int um_reset_appgeo()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4esavld.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:03
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "ustdio.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "msrf.h"
#include "msol.h"
#include "mdebug.h"
#include "mderror.h"
#include "xfsys0.h"
#include "xenv1.h"
#include "rebus.h"

static UX_pathname UM_appgeo_fname;
static UU_LOGICAL UM_appgeo_load;

/*********************************************************************
**    E_FUNCTION     : int um_load_appgeo(fname, load)
**			Read AG entities from the specified file (FNAME).
**    PARAMETERS   
**       INPUT  : 
**          fname					name of file containing AG data
**				load					UU_TRUE => load operation
**										UU_FALSE => merge operation
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_load_appgeo(fname, load)
	char *fname;
	UU_LOGICAL load;

	{

	uu_denter(UU_MTRC, (us,"um_load_appgeo(fname=%s, load=%d)",
		fname, load));

	/* save file name and load flag */
	strcpy(UM_appgeo_fname, fname);
	UM_appgeo_load = load;

	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_post_load_appgeo()
**			Link up AG database to UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_post_load_appgeo()

	{
	int status;
	int crvnum;
	int srfnum;
	int shellnum;
	FILE *fd;
/*	FILE *fopen(); */
	struct UM_agcrv_rec crv;
	struct UM_agsrf_rec srf;
	struct UM_agshell_rec shell;
	struct UM_rbsplcrv_rec bcv;
	struct UM_rbsplsrf_rec bsf;
	char enttype[256];
	UX_pathname name;
	UU_LOGICAL editState;	/* editability state of a master tuple */
	char *ux_getenv(), *str;
	UU_LOGICAL lagcnv;

	uu_denter(UU_MTRC, (us,"um_post_load_appgeo()"));

	/* remove leading and trailing quotes */
	strcpy(name, UM_appgeo_fname);
	status = ux_strip_quotes(name);
	if (status != UU_SUCCESS) goto done;

	/* open file */
	fd = fopen(name, "r");
	if (fd == NULL)
		{
		uu_dprint(-1, (us,"error opening AG part file"));
		status = UU_FAILURE;
		goto done;
		}

/*
...  Determine if we need to convert AG geometry .
*/
   lagcnv = UU_TRUE; 
/* 	str = ux_getenv("UU_CONVERT_AG", UX_NPRTERRS); */
/* 	if (str == UU_NULL) str = "YES"; */
/* 	lagcnv = strcmp(str, "NO"); */
	/* initialize all AG databags */
	um_agcrv_setup_data(UM_AGCRV_REL, &crv, sizeof(crv));
	um_agsrf_setup_data(UM_AGSRF_REL, &srf, sizeof(srf));
/*	um_agshell_setup_data(UM_AGSHELL_REL, &shell, sizeof(shell));*/

	/* read data from file */
	crvnum = 0;
	srfnum = 0;
	shellnum = 0;
	while (fscanf(fd, "%s", enttype) != EOF)
		{
		if (strcmp(enttype, "agcrv") == 0)
			{
			crvnum++;
			status = ur_get_next_new_data_key(UM_AGCRV_REL, &crvnum, &crv.key);
			if (status < 0) goto done;
			status = uc_retrieve_mtuple_data(&crv, sizeof(crv));
			if (status != UU_SUCCESS) goto done;
			if (lagcnv)
			  {
			  crv.crvaddr = 0;
			  status = ncl_agcrv_conv (fd, &crv, &bcv);
			  }
			}
		else if (strcmp(enttype, "agsrf") == 0)
			{
			srfnum++;
			status = ur_get_next_new_data_key(UM_AGSRF_REL, &srfnum, &srf.key);
			if (status < 0) goto done;
			status = uc_retrieve_mtuple_data(&srf, sizeof(srf));
			if (status != UU_SUCCESS) goto done;
			/*make tuple momentarily editable to update or delete */
			ur_retrieve_editability(srf.key, &editState);
			ur_update_editability(srf.key,UU_TRUE);
			if (lagcnv)
			  {
			  srf.srfaddr = 0;
			  status = ncl_agsrf_conv (fd, &srf, &bsf);
/*			  umi_agsrf_delete (&srf); */
			  ur_delete_all (srf.key);
			  }
			ur_update_editability(srf.key,editState);
			}
/*		else if (strcmp(enttype, "agshell") == 0)
/*			{
/*			shellnum++;
/*			status = ur_get_next_new_data_key(UM_AGSHELL_REL, &shellnum, &shell.key);
/*			if (status < 0) goto done;
/*			status = uc_retrieve_mtuple_data(&shell, sizeof(shell));
/*			if (status != UU_SUCCESS) goto done;
/*			shell.shelladdr = (int) ag_fr_sh(fd, "AG_ASCIII");
			/*make tuple momentarily editable to update UNIBASE with shell.shelladdr*/
/*			ur_retrieve_editability(shell.key, &editState);
/*			ur_update_editability(shell.key,UU_TRUE);
/*			ur_update_data(&shell);
/*			ur_update_editability(shell.key,editState);
/*			} */
		else
			{
			uu_dprint(-1, (us,"error reading AG part file"));
			status = UU_FAILURE;
			goto done;
			}	
		}

	/* for any remaining tuples, simply NULL out AG address for now */
	while (UU_TRUE)
		{
		crvnum++;
		status = ur_get_next_new_data_key(UM_AGCRV_REL, &crvnum, &crv.key);
		if (status < 0) break;
		status = uc_retrieve_mtuple_data(&crv, sizeof(crv));
		if (status != UU_SUCCESS) goto done;
		crv.crvaddr = (int) NULL;
		ur_update_data(&crv);
		}

	while (UU_TRUE)
		{
		srfnum++;
		status = ur_get_next_new_data_key(UM_AGSRF_REL, &srfnum, &srf.key);
		if (status < 0) break;
		status = uc_retrieve_mtuple_data(&srf, sizeof(srf));
		if (status != UU_SUCCESS) goto done;
		srf.srfaddr = (int) NULL;
		ur_update_data(&srf);
		}

	while (UU_TRUE)
		{
		shellnum++;
		status = ur_get_next_new_data_key(UM_AGSHELL_REL, &shellnum, &shell.key);
		if (status < 0) break;
		status = uc_retrieve_mtuple_data(&shell, sizeof(shell));
		if (status != UU_SUCCESS) goto done;
		shell.shelladdr = (int) NULL;
		ur_update_data(&shell);
		}

	status = UU_SUCCESS;
	
done:
	if (fd != NULL) fclose(fd);
	uu_dexitstatus("um_load_appgeo", status);
	return (status);
	}

