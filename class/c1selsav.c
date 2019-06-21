/*********************************************************************
**    NAME:  c1selsav.c
**       CONTAINS:
**          ucu_doSelSav()
**          ucu_getFilePaths()
**          uci_deleteItIfItExists()
**          uci_saveMySet()
**          uci_selSavInstance()
**          uci_save_srf_parts()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       c1selsav.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:57
*********************************************************************/
#include "usysdef.h"    /* basic data types */
#include "uhep.h"               /* for error system */
#include "udebug.h"     /* for debugging trace facility */
#include "dmark.h" 
#include "dasnog.h"     /* for das data types */
#include "uhash.h"      /* for hash table for copies of master symbols */
#include "xenv1.h"
#include "xfsys1.h"
#include "mdrel.h"      /* for relation number definitions */
#include "mdattr.h"     /* for UM_DISPLAYABLE */
#include "mxxx.h"               /* for UM_grouper_rec */
#include "bsym.h"               /* for symbol definitions */
#include "mcrv.h"               /* for compcrv definition */
#include "atext.h"      /* for text definition */
#include "adraft.h"     /* for drafting entity definition */
#include "rbase.h"      /* for UR_REL_NAME */
#include "nccs.h"

static UU_HASH_TBL masterHashTbl;
static UU_LOGICAL mustInitHashTbl = UU_TRUE;
#define _(funct) { if (funct != UU_SUCCESS) goto failed; }

#define TRACE  UU_FALSE /* for debugging only */
/*********************************************************************
**    E_FUNCTION: int ucu_doSelSav()
**                      This function does selective save.
**    PARAMETERS   
**       INPUT: none.
**       OUTPUT:  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ucu_doSelSav()
{
	char    partPath[UX_MAX_PATH_LEN];      /* unibase filename to use      */
	char    solidsPath[UX_MAX_PATH_LEN];    /* solid filename to use        */
	char    appgeoPath[UX_MAX_PATH_LEN];    /* applied geometry filename to use     */
	extern UU_LOGICAL       UR_changed;     /* Unibase change flag */
	int rel_num, initialize = UU_TRUE;
	UU_KEY_ID key;
	char *listHead = UU_NULL;
	int numint, status = UU_SUCCESS;
	uu_denter(UU_MTRC, (us, "ucu_doSelSav()"));

	_(ucu_getFilePaths(partPath, solidsPath, appgeoPath)); 
	ud_ldas(UD_DASSELECT, /*pick entities to save*/UM_MODEL, 328, UU_NULL, 1, 
				&numint,UD_NODEFAULT);
	if (numint != 0)
	{
		/* clear all bits indicating a tuple is to be saved */
		if (ur_svld_clear() != UU_SUCCESS)
		{
			uu_uerror1(UM_MODEL, 283, "ucu_doSelSav");
			/* error is:Can't clear save/load bit map (%s) */
			goto failed;
		}
		while (ud_gnxt(initialize, UU_NULL, &key, 1))
		{
			initialize = UU_FALSE;
			_(um_retrieve_data_relnum(key, &rel_num));
			if (rel_num == UM_DRAWING_REL)
			{
				uu_uerror0(UM_MODEL, 292);
				/* Error: can't selectively save a drawing. */
				/* fix things up as if for selective save */
				_(uci_fixUp4SelSav(&listHead)); 
				/* fix things up as if selective save had been done */
				_(uci_fixUpAfterSelSav(listHead));
				goto failed;
			}
			_(uci_saveMySet(key));
		}
	}
	/* go do solids pre-processing, and some other stuff with borders 
	 * note, currently the path names aren't used. */
	_(ur_sp01(solidsPath, appgeoPath, UU_FALSE));

	/* fix things up for selective save */
	_(uci_fixUp4SelSav(&listHead)); 

	/* mark modals to save */
	_(ur_set_save_modals()); 

	/* now save the Unibase stuff */
	if (ur_sp02(partPath) != UU_SUCCESS) 
	{
		uu_dprint(UU_MTRC,(us,"Unibase selective save failed, file:%s",partPath));
		/* fix things up as if selective save had been done */
		_(uci_fixUpAfterSelSav(listHead));
		goto failed;
	}

	/* now fix up views */
	um_post_save();

	goto done;
failed: status = UU_FAILURE;
done:;
	if (listHead != UU_NULL)
		uu_lsdel(listHead); /* delete list */
	uu_dexitstatus("ucu_doSelSav", status);
	return(status);
}       

/*********************************************************************
**    E_FUNCTION: int ucu_getFilePaths(partPath, solidsPath, appgeoPath)
**                      This functions gets the full part, solids, and applied geometry
**                      path names.
**    PARAMETERS   
**       INPUT: just the buffers.
**       OUTPUT:  
**                              partPath                Full path to the part file.
**                              solidsPath      Full path to the solids file.
**                              appgeoPath      Full path to the applied geometry file.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ucu_getFilePaths(partPath, solidsPath, appgeoPath)
	char            partPath[UX_MAX_PATH_LEN];              /* unibase filename to use      */
	char            solidsPath[UX_MAX_PATH_LEN];    /* solid filename to use        */
	char            appgeoPath[UX_MAX_PATH_LEN];    /* applied geometry filename to use     */
{
	char            fname[UX_MAX_PATH_LEN]; /* base filename to use */
	char            filename[UX_MAX_PATH_LEN], dir[UX_MAX_PATH_LEN];        
	char            pathNoSuffix[UX_MAX_PATH_LEN];  /* unibase filename to use      */
	int             length; /* returned lenof filename      */
	UU_LOGICAL startOver = UU_FALSE;
	UX_pathname bname;
	char *p, *ux_getenv(), ext[UX_MAX_PATH_LEN], ext1[UX_SUFFIX_LEN];
	char descrip[UX_MAX_PATH_LEN];
	int status = UU_SUCCESS;
	uu_denter(UU_MTRC,(us,
		"ucu_getFilePaths(partPath:%x,solidPath:%x,appgeoPath:%x",
							partPath, solidsPath, appgeoPath)) ;
/*
.....use Motif interface
......Yurong 9/11/98
*/
	fname[0] = '\0';
	strcpy(ext,"*.");
	strcpy(descrip, "Unibase File (");
	p = ux_getenv("UR_PART_SUFFIX");
	if (p != UU_NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(ext,ext1);
	}       
	else 
		strcat(ext,"u");

	strcat(descrip, ext);
	strcat(descrip, ")|Textual Unibase Files (*.");
	
	p = ux_getenv("UR_ASCII_PART");
	strcat(ext,"|*.");
	if (p != UU_NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(ext,ext1);
		strcat(descrip, ext1);
	}
	else
	{
		strcat(ext,"ud");
		strcat(descrip, "ud");
	}
	strcat(descrip, ")");
	ud_get_filename("Enter Filename", "Enter Filename",
							ext, fname, &length, descrip, 0, UU_FALSE);
	if (ux_get_base_fname(fname, bname, (UX_NPRTERRS | UX_NCHK)) !=
	       UU_SUCCESS)
	{
		uu_uerror0(UX_UDOS,24);
		goto failed;
	}
	if (length <= 0) 
		goto failed;
	 
/*
.....if filename already have a path, we don't need
.....added it
.....Yurong 9/11/98
*/
	ul_break_fname(fname, dir, filename);
	if (dir[0]!='\0')
		status = ux_mk_chk_syspath(UU_NULL, NULL, fname, UU_NULL,
						UU_NULL, UU_NULL, pathNoSuffix, UX_PRTERRS);
	else
		status = ux_mk_chk_syspath(UU_NULL, "^UR_PART_AREA", fname, UU_NULL,
						UU_NULL, UU_NULL, pathNoSuffix, UX_PRTERRS);
	switch(status)
	{
		case UU_SUCCESS:
			break;
		case UX_FAILURE:
		case UX_BAD_SUBJECT:
		case UX_BAD_ENV:
		case UX_NO_ACCESS:
			uu_dprint(-1,(us,"bad status of %d from ux_mk_chk_syspath",status));
			uu_uerror0(UU_UBASE, 3);
		default:
			uu_dprint(-1,(us,"Illegal return from UDOS"));
			goto failed;
	}
	strcpy(partPath, pathNoSuffix);
	status = ux_add_ftype("UR_UNB_FILE",partPath,UX_PRTERRS);
	switch(status)
	{
		case UU_SUCCESS:
		case UX_FIXED_EXT: /* had an extension that was overwritten */
/*
.....set status = UU_SUCCESS
.....Yurong 9/14/98
*/
			status = UU_SUCCESS;
			break;
		case UX_BAD_ENV:
		case UX_FAILURE:
			uu_dprint(-1,(us,"bad status %d from ux_add_ftype",status));
			uu_uerror0(UU_UBASE, 3);
			/* error message: can not open specified save file */
		default:
			goto failed;
	}
	strcpy(solidsPath, pathNoSuffix);
	status = ux_add_ftype("UR_SOL_FILE",solidsPath,UX_PRTERRS);
	switch(status)
	{
		case UU_SUCCESS:
		case UX_FIXED_EXT:
/*
.....set status = UU_SUCCESS
.....Yurong 9/14/98
*/
			status = UU_SUCCESS;
			break;
		case UX_FAILURE:
		case UX_BAD_ENV:
			uu_dprint(-1,(us,"bad status %d from ux_add_ftype",status));
			uu_uerror0(UU_UBASE, 3);
		default:
			goto failed;
	}
	strcpy(appgeoPath, pathNoSuffix);
	status = ux_add_ftype("UR_AG_FILE",appgeoPath,UX_PRTERRS);
	switch(status)
	{
		case UU_SUCCESS:
		case UX_FIXED_EXT:
/*
.....set status = UU_SUCCESS
.....Yurong 9/14/98
*/
			status = UU_SUCCESS;
			break;
		case UX_FAILURE:
		case UX_BAD_ENV:
			uu_dprint(-1,(us,"bad status %d from ux_add_ftype",status));
			uu_uerror0(UU_UBASE, 3);
		default:
			goto failed;
	}
	_(uci_deleteItIfItExists(partPath, &startOver));
	if (startOver) goto failed;
/*
.....if successfully deleted the Unibase portion, check as to the 
.....existence of the Solids portion and if it exists, delete it too
*/
	_(uci_deleteItIfItExists(solidsPath, &startOver));
	if (startOver) goto failed;
/* 
.....now check on applied geometry file and delete it if it 
.....exists 
*/
	_(uci_deleteItIfItExists(appgeoPath, &startOver));
	if (startOver) goto failed;
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ucu_getFilePaths", status);
	return(status);
}       

/*********************************************************************
**    I_FUNCTION: int uci_deleteItIfItExists(fullPath, startOverptr)
**                      This function deletes the file residing at "fullPath" if it exists.
**    PARAMETERS   
**       INPUT  : 
**                              fullPath                Full path to the file to delete.
**       OUTPUT :  
**                              startOverptr    Pointer to either UU_TRUE or UU_FALSE. UU_TRUE is
**                                                                      is output if we must start over in obtaining the
**                                                                      file information for selective save.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uci_deleteItIfItExists(fullPath, startOverptr)
	char fullPath[UX_MAX_PATH_LEN]; /* full filename to use */
	UU_LOGICAL *startOverptr;
{
	char dummyPath[UX_MAX_PATH_LEN];        /* only a place holder */
	char msg[100];
	int hstat, mode, status = UU_SUCCESS;
	uu_denter(UU_MTRC,(us,"uci_deleteItIfItExists(%s)", fullPath));

	mode = 0;
	status = ux_file_inquire(UU_NULL, UU_NULL, fullPath, UU_NULL, 
					UU_NULL, &mode, &hstat, dummyPath, UX_PRTERRS);
	switch(status)
	{
		case UU_SUCCESS:
			break;
		case UX_FAILURE:
		case UX_BAD_SUBJECT:
		case UX_BAD_ENV:
		case UX_NO_ACCESS:
			uu_dprint(-1,(us,"bad status %d from ux_file_inquire", status));
			uu_uerror0(UU_UBASE, 3);
			*startOverptr = UU_TRUE;
			break;
		default:
			uu_dprint(-1,(us,"Illegal return from UDOS"));
			goto failed;
	}
	if ((!(*startOverptr)) && (!(mode & UX_NEXISTS)))
/*
.....added question box for "Yes/No"
.....Yurong 9/15/98
*/
	{
		sprintf(msg, "File %s Exist, Delete it?", fullPath);
		if (ud_yesno(0, msg, "File Exist"))
		{
			_(ux_delete(fullPath, UX_PRTERRS));
		}
		else
			*startOverptr = UU_TRUE;
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("uci_deleteItIfItExists", status);
	return(status);
}       

/*********************************************************************
**    I_FUNCTION: int uci_saveMySet(key2Sav) 
**                      This function saves (tries to) the appropriate set of entities
**                      given that the entity associated with "key2Sav" is to be saved.
**    PARAMETERS   
**       INPUT  : 
**                              key2Sav         Key of entity to be saved.  We also save associated
**                                                              entities depending on the semantics of the entity.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uci_saveMySet(key2Sav)
	UU_KEY_ID       key2Sav;
{
	int i, relNum, status = UU_SUCCESS;
	UR_REL_NAME             name;
	struct NCL_fixed_databag e;
	uu_denter(UU_MTRC,(us,"uci_saveMySet(key2Sav:%d)",key2Sav));

	/* mark only the entity requested */
#if (TRACE)
	_(ur_retrieve_rel_name(key2Sav, name));
	uu_dprint(UU_MTRC,(us,"marking %s for selective save", name));
#endif
	_(ur_mark_to_save(key2Sav));

	_(ur_retrieve_data_relnum(key2Sav, &relNum));
	switch (relNum)
	{
		case UM_BODY_REL:
			break;

		case UM_AGCRV_REL:
		case UM_AGSRF_REL:
		case UM_AGSHELL_REL:
			break;

		case UM_COMPCRV_REL:
		{
			struct UM_compcrv_rec cmpcrv;
			cmpcrv.key = key2Sav;
			_(uc_retrieve_data(&cmpcrv, sizeof(cmpcrv)));
			for (i=0; i<cmpcrv.no_cid; i++)
				_(uci_saveMySet(cmpcrv.cid[i].crvid)); 
			break;
		}
		case UM_GROUP_REL:
		{
			struct UM_grouper_rec group;
			group.key = key2Sav;
			_(uc_retrieve_data(&group, sizeof(group)));

			for (i=0; i<group.no_member; i++)
				_(uci_saveMySet(group.member[i]));
			break;
		}
		case UA_LINEAR_DIMS_REL:        /* linear dimension */
		{
			struct UA_generic_draft drft;
			drft.key = key2Sav;
			_(uc_retrieve_data(&drft, sizeof(drft)));

			for (i=0; i<drft.asso_blk_use; i++)
				_(uci_saveMySet(drft.asso_blk[i].key));
			break;
		}
		case UB_SYMBOL_REL:
		{
			struct UB_symbol_rec msym;
			UB_SETUP_DATA(UB_SYMBOL_REL,&msym,sizeof(struct UB_symbol_rec),status);
			if (status != UU_SUCCESS) goto failed;
			msym.key = key2Sav;
			_(uc_retrieve_data(&msym, sizeof(msym)));

			for (i=0; i<msym.no_geom; i++)
				_(uci_saveMySet(msym.geom[i]));
			for (i=0; i<msym.no_text_nod; i++)
				_(uci_saveMySet(msym.text_nod[i].text_key));
			for (i=0; i<msym.no_snap_nod; i++)
				_(uci_saveMySet(msym.snap_nod[i].snap_key));
			break;
		}
		case UB_INSTANCE_REL:
			_(uci_selSavInstance(key2Sav));
			break;
		case UA_TEXT_REL:
		{
			struct UA_txt_rec text;
			text.key = key2Sav;
			_(uc_retrieve_data(&text, sizeof(text)));
			if (text.arckey != 0)
				_(uci_saveMySet(text.arckey));
			break;
		}
		case NCL_TRIMSF_REL:
		{
/*
.....It is a trimmed surface so save the base surface, xyz curve, uv curve
.....and any innner-boundary curves that may exist.
*/
			struct NCL_trimsf_rec *tsfp;
			tsfp = (struct NCL_trimsf_rec *)&e;
			tsfp->key = key2Sav;
			if (ncl_retrieve_data_fixed(tsfp) != UU_SUCCESS) goto failed;
			if (tsfp->bs_key == 0) goto failed;
			if (uci_saveMySet(tsfp->bs_key) != UU_SUCCESS) goto failed;
			if (tsfp->uv_key > 0)
			{
				if (uci_saveMySet(tsfp->uv_key) != UU_SUCCESS) goto failed;
			}
			if (tsfp->cv_key > 0)
			{
				if (uci_saveMySet(tsfp->cv_key) != UU_SUCCESS) goto failed;
			}
/*
......Get the inner boundary keys if there are any.
*/
			for (i=0; i<tsfp->no_ibndykey; i++)
			{
				if (tsfp->ibndykey[i] > 0)
					if (uci_saveMySet(tsfp->ibndykey[i]) != UU_SUCCESS) goto failed;
			}
			break;
		}
		case NCL_SURF_REL:
		{
/*
.....It is a NCL surface, so save the panels and the cv on surface, if 
.....they exist.
*/
			struct NCL_surface_rec *nsfp;
			nsfp = (struct NCL_surface_rec *)&e;
			nsfp->key = key2Sav;
			if (ncl_retrieve_data_fixed(nsfp) != UU_SUCCESS) goto failed;
/*
.....Save NCL surface panel keys.
*/
			for (i=0; i<nsfp->no_panelkey; i++)
			{
				if (uci_saveMySet(nsfp->panelkey[i]) != UU_SUCCESS) goto failed;
			}
			for (i=0; i<nsfp->no_sskey; i++)
			{
				uci_saveMySet(nsfp->sskey[i]);
			}
			break;
		}
		case NCL_NETSF_REL:
		{
/*
.....Net surface. Mark member surface and their components for saving.
*/
			struct NCL_netsf_rec *netsfp;
			netsfp = (struct NCL_netsf_rec *)&e;
			netsfp->key = key2Sav;
			if (ncl_retrieve_data_fixed(netsfp) != UU_SUCCESS) goto failed;
			for (i=0; i<netsfp->no_netkey; i++)
			{
				if (uci_saveMySet(netsfp->netkey[i]) != UU_SUCCESS) goto failed;
			}
			for (i=0; i<netsfp->no_sskey; i++)
			{
				uci_saveMySet(netsfp->sskey[i]);
			}
			break;
		}
		case NCL_REVSURF_REL:
		{
/*
.....Surface of revolution
*/
			struct NCL_revsurf_rec *revsfp;
			revsfp = (struct NCL_revsurf_rec *)&e;
			revsfp->key = key2Sav;
			if (ncl_retrieve_data_fixed(revsfp)) goto failed;
			if (revsfp->cvkey > 0)
				if (uci_saveMySet(revsfp->cvkey) != UU_SUCCESS) goto failed;
			for (i=0; i<revsfp->no_sskey; i++)
			{
				uci_saveMySet(revsfp->sskey[i]);
			}
			break;
		}
		default:
			break;
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("uci_saveMySet", status);
	return(status);
}       

/*********************************************************************
**    I_FUNCTION :  int uci_fixUp4SelSav(listHeadptr)
**                      This function fixes things up for a selective save; i.e. it:
**                              a. marks for selective saving:
**                                              the default transformation, lights, layers, views, view ports,
**                                              screens, calculator stuff, coordinate systems.
**                              b. unmarks master symbols that have had copies of themselves made
**                                      since the copies will be selectively saved in their place.
**                              c. sets subentities of composite curves to UM_DISPLAYABLE if
**                                      the subentities are going to be selectively saved but the 
**                                      composite cuvre is not.
**                      Note, must call this function if uci_fixUpAfterSelSav is to be
**                      called.
**    PARAMETERS   
**       INPUT :  none.
**       OUTPUT :  
**                              listHeadptr             Pointer to the head of the list of entities that
**                                                                      have their displayability changed.
**    RETURNS      : UU_SUCCESS if no problems encountered; UU_FAILURE 
**                                                      otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uci_fixUp4SelSav(listHeadptr)
	char **listHeadptr;
{
	char *uu_lsnew();
	UU_KEY_ID key;
	struct UA_generic_draft drft;
	struct UA_txt_rec text;
	char *listEnd;
	UU_KEY_ID *bufptr;
	int i, nxtuple, status = UU_SUCCESS;
	uu_denter(UU_MTRC, (us, "uci_fixUp4SelSav(*listHeadptr:%x)", *listHeadptr));

	/* used below, but init the list here so that failures don't cause crashes 
	 * when an attempt to delete the list is made. */
	listEnd = *listHeadptr = uu_lsnew();

	/* MARK DEFAULT TRANSFORMATION */
	uu_dprint(UU_MTRC,(us,"mark default transformation"));
	nxtuple = 1;
	if (ur_get_next_data_key(UM_TRANSFORM_REL, &nxtuple, &key) == 0)
	{
		_(ur_mark_to_save(key));
	}
	else goto failed;

	/* MARK LIGHTS FOR SAVE */
	uu_dprint(UU_MTRC,(us,"mark lights"));
	nxtuple = 1;
	while (ur_get_next_data_key(UM_LIGHT_REL, &nxtuple, &key) == 0)
	{
		/* mark only the entity requested */
		_(ur_mark_to_save(key));
		nxtuple++;
	}

	/* MARK LAYERS FOR SAVE */
	uu_dprint(UU_MTRC,(us,"mark layers"));
	nxtuple = 1;
	while (ur_get_next_data_key(UM_LAYER_REL, &nxtuple, &key) == 0)
	{
		/* mark only the entity requested */
		_(ur_mark_to_save(key));
		nxtuple++;
	}

	/* MARK VIEWS FOR SAVE */
	uu_dprint(UU_MTRC,(us,"mark views"));
	nxtuple = 1;
	while (ur_get_next_data_key(UV_VIEW_REL, &nxtuple, &key) == 0)
	{
		/* mark only the entity requested */
		_(ur_mark_to_save(key));
		nxtuple++;
	}

	/* MARK VIEW PORTS FOR SAVE */
	uu_dprint(UU_MTRC,(us,"mark view ports"));
	nxtuple = 1;
	while (ur_get_next_data_key(UV_VPORT_REL, &nxtuple, &key) == 0)
	{
		/* mark only the entity requested */
		_(ur_mark_to_save(key));
		nxtuple++;
	}

	/* MARK SCREENS FOR SAVE */
	uu_dprint(UU_MTRC,(us,"mark screens"));
	nxtuple = 1;
	while (ur_get_next_data_key(UV_SCREEN_REL, &nxtuple, &key) == 0)
	{
		/* mark only the entity requested */
		_(ur_mark_to_save(key));
		nxtuple++;
	}

	/* MARK CALCULATOR STUFF FOR SAVE */
	uu_dprint(UU_MTRC,(us,"mark calculator stuff"));
	nxtuple = 1;
	while (ur_get_next_data_key(UQ_CALC_REL, &nxtuple, &key) == 0)
	{
		/* mark only the entity requested */
		_(ur_mark_to_save(key));
		nxtuple++;
	}

	/* MARK COORD SYS'S FOR SAVE */
	uu_dprint(UU_MTRC,(us,"mark corrd sys's"));
	nxtuple = 1;
	while (ur_get_next_data_key(UM_COORDSYS_REL, &nxtuple, &key) == 0)
	{
		/* mark only the entity requested */
		_(ur_mark_to_save(key));
		nxtuple++;
	}

	/* MUST UNMARK ORIGINAL MASTERS HERE; A COPY HAS BEEN MADE AND MARKED */
	while ((bufptr = (UU_KEY_ID *) uu_hash_scan(masterHashTbl)) != UU_NULL)
	{
		uu_dprint(UU_MTRC,(us,"clearing mark on master: %d", bufptr[1]));
		_(ur_save_clr(bufptr[1]));
	}

	/* for each drafting and text entity, see if any associated geometry is
	 * in a composite curve. */
	uu_dprint(UU_MTRC,(us,"now set displayability drafting associated geom"));
	nxtuple = 1;
	while (ur_get_next_new_data_key(UA_LINEAR_DIMS_REL, &nxtuple, &(drft.key)) 
					== UU_SUCCESS)
	{       
		_(ur_setup_data(UA_LINEAR_DIMS_REL, &drft, 
						sizeof(struct UA_generic_draft)));
		if (uc_retrieve_data(&drft, sizeof(struct UA_generic_draft)) 
				!= UU_SUCCESS)
		{
			uu_dprint(UU_MTRC,(us,"can not retrieve drafting with key:%d", 
									drft.key)); 
			goto failed;
		}
		for (i=0; i<drft.asso_blk_use; i++)
		{
			/* Test to see if "drft.asso_blk[i].key" is in a composite; if so, it's
			 * displayability must be changed so that the drafted entity is 
			 * displayable; the following function returns with a list of 
			 * entities that have had their displayability changed. */
			_(uci_mkCompSubentDisplayable(drft.asso_blk[i].key,listEnd));
		}
		nxtuple++;
	}

	uu_dprint(UU_MTRC,(us,"now set displayability of text associated geom"));
	nxtuple = 1;
	while (ur_get_next_new_data_key(UA_TEXT_REL, &nxtuple, &(text.key)) 
					== UU_SUCCESS)
	{       
		_(ur_setup_data(UA_TEXT_REL, &text, sizeof(struct UA_txt_rec))); 
		if (uc_retrieve_data(&text, sizeof(struct UA_txt_rec)) 
				!= UU_SUCCESS)
		{
			uu_dprint(UU_MTRC,(us,"can not retrieve text with key:%d", 
									text.key)); 
			goto failed;
		}
		if (text.arckey != 0)
		{
			/* Test to see if "text.arckey" is in a composite; if so, it's
			 * displayability must be changed so that the arc is displayable;
			 * the following function returns with a list of entities that have had
			 * their displayability changed. */
			_(uci_mkCompSubentDisplayable(text.arckey,listEnd));
		}
		nxtuple++;
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("uci_fixUp4SelSav", status);
	return(status);
}       

/*********************************************************************
**    I_FUNCTION: int uci_fixUpAfterSelSav(listHead) 
**                      This function:
**                              a. Resets the displayability of subentities of composite curves 
**                                      that have been selectively saved without their composites.  
**                                      The subentities are reset to "UM_NEVERDISPLAYABLE".
**                              b. Deletes any master symbols that are marked for selective save;
**                                      these should be temporary copies of the actual masters.
**                      Should have called "uci_fixUp4SelSav" before calling this function.
**    PARAMETERS   
**       INPUT: 
**                              listHead                List of the entities to have their displayability reset.
**       OUTPUT : none. 
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uci_fixUpAfterSelSav(listHead)
	char *listHead;
{
	char *tmp;
	UU_KEY_ID key;
	int nxtuple, status = UU_SUCCESS;
	uu_denter(UU_MTRC, (us,"uci_fixUpAfterSelSav(listHead:%x)", listHead));
	
	tmp = listHead;
	while ((tmp = (char *)uu_lsnext(tmp)) != UU_NULL)
	{
		key = *((UU_KEY_ID *)tmp);
		if (ur_update_displayable(key, UM_NEVERDISPLAYABLE) != UU_SUCCESS) 
		{
			uu_dprint(UU_MTRC,(us,"can't reset displayability of entity, key:%d",
					key));
			goto failed;
		}
	}

	/* delete any master symbols that are marked */ 
	nxtuple = 1;
	while (ur_get_next_new_data_key(UB_SYMBOL_REL, &nxtuple, &key) == UU_SUCCESS)
	{
		if (ur_delete_all(key) != UU_SUCCESS) 
		{
			uu_dprint(UU_MTRC,(us,"error on delete of master sym copy, key:%d",
					key));
			goto failed;
		}
		nxtuple++;
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("uci_fixUpAfterSelSav", status);
	return(status);
}       

/*********************************************************************
**    I_FUNCTION : int uci_mkCompSubentDisplayable(associate,listEnd)
**                      If "associate" is a subentity of a composite curve and the composite
**                      is not going to be saved, then this function sets the displayability
**                      of the subentity so that it is displayable when the selective save
**                      file is reloaded.
**    PARAMETERS   
**       INPUT: 
**                              associate               Key of the entity to test to see if it is in a 
**                                                                      composite not to be saved.
**                              listend                 List of entity keys that have had their 
**                                                                      displayability changed.
**       OUTPUT:  
**                              listend                 List of entity keys that have had their 
**                                                                      displayability changed; need this to change them
**                                                                      back after the selective save.  "associate" may now
**                                                                      be on this list.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uci_mkCompSubentDisplayable(associate,listEnd)
	UU_KEY_ID associate;
	char *listEnd;
{
	char *uu_lsinsrt();
	UU_KEY_ID key;
	int nxtuple, status = UU_SUCCESS;
	uu_denter(UU_MTRC,(us,"uci_mkCompSubentDisplayable(key:%d,listEnd:%x)",
						associate, listEnd));

	nxtuple = 1;
	if (ur_get_next_spec_assoc_key(associate, &nxtuple, UM_COMPCRV_REL,
						0 /* any field */, &key) == UU_SUCCESS) 
		/* composite found containing "associate"; see if composite to be saved */
		if (!ur_svld_test(key)) /* then compcrv is not to be saved */ 
		{       /* so set the subentity to be displayable */ 
			if (ur_update_displayable(associate, UM_DISPLAYABLE) != UU_SUCCESS) 
			{ 
				uu_dprint(UU_MTRC,(us, 
				"can't set displayability on drafting subentity,key:%d",associate));
				goto failed;
			}
			/* insert on list so that we can change it back after save */
			listEnd = uu_lsinsrt(listEnd, sizeof(UU_KEY_ID));
			if (listEnd == UU_NULL) 
			{
				uu_uerror0(UU_UBASE, 1111); /* out of dynamic memory */
				goto failed;
			}
			*((UU_KEY_ID *)listEnd) = associate;
		}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("uci_mkCompSubentDisplayable",status);
	return(status);
}       

/*********************************************************************
**    I_FUNCTION : int uci_selSavInstance(key2Ssav) 
**                      This function saves the subentities of a symbol instance, and
**                      checks to make sure an appropriate master symbol is saved.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uci_selSavInstance(instKey)
	UU_KEY_ID instKey;
{
	struct UB_symbol_rec msym, mcopy;
	struct UB_instance_rec inst;
	UU_KEY_ID *bufptr;
	UM_transf tfmat;
	struct UB_symattr_rec attr;
	UU_LOGICAL firstim = UU_FALSE;
	int nxtuple, i, status = UU_SUCCESS;
	uu_denter(UU_MTRC,(us,"uci_selSavInstance(instKey:%d)", instKey));

	/* save the instance's subentities */
	UB_SETUP_DATA(UB_INSTANCE_REL,&inst,sizeof(inst),status);
	if (status != UU_SUCCESS) goto failed;
	inst.key = instKey;
	_(uc_retrieve_data(&inst, sizeof(struct UB_instance_rec)));
	uu_dprint(UU_MTRC,(us,"saving instance's subentities"));
	for (i=0; i<inst.no_geom; i++)
		_(uci_saveMySet(inst.geom[i]));
	for (i=0; i<inst.no_text_nod; i++)
		_(uci_saveMySet(inst.text_nod[i].text_key));
	for (i=0; i<inst.no_snap_nod; i++)
		_(uci_saveMySet(inst.snap_nod[i].snap_key));
		
	uu_dprint(UU_MTRC,(us,"fix-up master for instance selective save"));
	UB_SETUP_DATA(UB_SYMBOL_REL,&msym,sizeof(msym),status);
	if (status != UU_SUCCESS) goto failed;
	nxtuple = 1;
	_(ur_get_next_spec_assoc_key(instKey,&nxtuple,UB_SYMBOL_REL,
				6/* field in ddl master def */, &(msym.key))); 
	_(uc_retrieve_data(&msym, sizeof(msym)));
	if (!ur_svld_test(msym.key)) /* then master has not been seen before */
	{
		uu_dprint(UU_MTRC,(us,"the master, %s, has not been seen before", 
					msym.name));
		firstim = UU_TRUE; /* first time to see this master */

		/* copy the fixed part of the original master into the copy */
		uu_move_byte(&msym, &mcopy, sizeof(struct UB_symbol_rec));

		/* mark the master to indicate that we have SEEN it (this mark is not to
		 * indicate that we are going to save it) */
		_(ur_mark_to_save(msym.key));

		UB_SETUP_DATA(UB_SYMBOL_REL,&mcopy,sizeof(mcopy),status);
		if (status != UU_SUCCESS) goto failed;

		/* attach the original master's geom, text nodes and snap nodes
		 * to copy */
		for (i=0; i<msym.no_geom; i++)
		{
			/* put the geometry keys into the variable list */
			_(ubi_update_app_varlist(&mcopy,UB_MGEOM_LIST,&(msym.geom[i]),
					mcopy.no_geom+1, 1));
		}
		for (i=0; i<msym.no_text_nod; i++)
		{
			/* put the text keys into the variable list */
			_(ubi_update_app_varlist(&mcopy,UB_MTEXT_LIST,
				&(msym.text_nod[i]), mcopy.no_text_nod+1, 1));
		}
		for (i=0; i<msym.no_snap_nod; i++)
		{
			/* put the snap node keys into the variable list */
			_(ubi_update_app_varlist(&mcopy,UB_MSNAP_LIST,
				&(msym.snap_nod[i]), mcopy.no_snap_nod+1, 1));
		}
		_(uc_retrieve_transf(msym.key, tfmat));
		_(uc_retrieve_attr(msym.key, &attr));
		uu_dprint(UU_MTRC,(us,"CREATING MASTER SYM COPY"));
		_(ub_create_symbol_tuple(&mcopy,tfmat,&attr));

		/* mark the copy of the master for selective save; this is really for 
		 * saving. */
		_(uci_saveMySet(mcopy.key));

		/* put key of copy of master in hash table, indexed on master key */
		if (mustInitHashTbl) /* initialize the hash table */
		{
			uu_hash_init(masterHashTbl, 100 /* nbr of buckets */);
			mustInitHashTbl = UU_FALSE;
		}
		bufptr = (UU_KEY_ID *)uu_hash_add(masterHashTbl, msym.key, 
														sizeof(UU_KEY_ID) * 2);
		if (bufptr == UU_NULL) goto failed;
		bufptr[0] = mcopy.key; /* store in hash table */
		bufptr[1] = msym.key; /* store original in hash table */
	}
	if (!firstim) /* must retrieve master's copy */
	{
		if ((bufptr = (UU_KEY_ID *)uu_hash_get(masterHashTbl, msym.key))
				== UU_NULL) goto failed;
		UB_SETUP_DATA(UB_SYMBOL_REL,&mcopy,sizeof(mcopy),status);
		if (status != UU_SUCCESS) goto failed;
		mcopy.key = *bufptr;
		_(uc_retrieve_data(&mcopy, sizeof(mcopy)));
	}
	/* look for this instance record on master's list */
	for (i=0; i<msym.no_inst; i++)
		if (instKey == msym.inst[i].inst_key) break;
	if (i == msym.no_inst) /* didn't find instance, something's wrong */
	{
		uu_dprint(UU_MTRC,(us,
			"error, didn't find instance on original master's list!!"));
			goto failed;
	}
	/* add this instance to master's copy */
	_(ubi_update_varlist(mcopy.key,UB_INST_LIST, &(msym.inst[i]), 
			mcopy.no_inst+1, 1));

	ur_free_app_data(&msym);                /* free varlist space */
	ur_free_app_data(&mcopy);               /* free varlist space */
	ur_free_app_data(&inst);                /* free varlist space */

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("uci_selSavInstance",status);
	return(status);
}
