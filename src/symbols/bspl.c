/*******************************************************************
**    NAME:bspl.c
**       CONTAINS:  
**			ubu_create_spart
**			ubu_mk_spart
**			ubu_place_spart
**			ub_make_spart_copies
**	 		ub_mk_disp_copy
**			ub_get_spart_geom
**			ubi_orient_spart
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**           bspl.c , 3.7
**    MODULE NAME AND RELEASE LEVEL 
**       bspl.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:05
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "uhep.h"     	/* for error system */
#include "udebug.h"		/* for debugging trace facility */
#include "dselmask.h"
#include "dmark.h"		/* for UD_MARK */

#include "view.h"
#include "mdrel.h"
#include "msol.h"
#include "mdcpln.h"		/* for constrcuction plane */
#include "mattr.h"
#include "mdattr.h"		/* for UM_ (NEVER) DISPLAYABLE */
#include "modef.h"		/* UM_ZILCH */
#include "class.h"
#include "bsym.h"		

#include "xenv1.h"		/* xio */
extern UU_LOGICAL UR_load_env;
#define TRACE UU_FALSE /* for debugging only */

/*********************************************************************
**    E_FUNCTION : int ubu_create_spart()
**       Create a standard part.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS   : UU_SUCCESS if no problems encountered, UU_FAILURE
**                otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ubu_create_spart()
{
	struct UB_spart_rec spart;
	struct UB_master_sym_frm_rec spl_frm_out;
	char fullpath[UB_MAX_PATH_LEN];
	char *uu_uprompt1();
	int cmdreject;		/* command reject variable, TRUE iff a command reject has
							 * occurred */
	int numint;			/* number of DAS interactions */
	int i, status = UU_SUCCESS;
	UM_vector temp_orig;
	uu_denter(UU_BTRC, (us, "ubu_create_spart()"));

	spart.no_geom = 0;

	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)	/* then no command reject encountered, go on */
	{
		/* get the spare part data from the form */
		if (ubu_get_spart_data(&spl_frm_out, spart.path) != UU_SUCCESS) 
						goto failed;
		strcpy(spart.name, spl_frm_out.name);

		/* get the geometry to be in the spart */
		if (ubu_get_spart_geom(&spart) != UU_SUCCESS)
			goto failed;

	  	/* Fix the geometry for the spart so that it is relativized
	 	 * to the origin.  */
		um_vctmsc(spl_frm_out.origin, (UU_REAL) -1.0, temp_orig);
		if (ubi_orient_spart(&spart, temp_orig) != UU_SUCCESS)
			goto failed;

		/* save the part file */
		if (ubu_mk_spart(&spart,spl_frm_out.lib,UB_DEFAULT_TF,
						UB_CURRENT_ATTR) != UU_SUCCESS) goto failed;

		/* reset the geometry origin for current use */
		if (ubi_orient_spart(&spart, spl_frm_out.origin) != UU_SUCCESS)
			goto failed;

		/* clear the save load bit map */
		ur_svld_clear();
		
	}/* end no command reject */
	else /* command reject */
	{
	}

	goto done;
failed: status = UU_FAILURE;
done:;
	UD_UNMARK(cmdreject);
	uu_dexitstatus("ubu_create_spart",status);
	return(status);
}	

/*********************************************************************
**    I_FUNCTION: int ubu_mk_spart(splptr, libname)
**    PARAMETERS   
**       INPUT  : 
**          splptr		Pointer to the standard part to be saved.
**				libname		The final directory in which the definition goes.
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : does a selective save of entities.
**    WARNINGS     : none
*********************************************************************/
int ubu_mk_spart(splptr, libname)
struct UB_spart_rec *splptr;
char libname[];
{
	UU_LOGICAL found;
	char tempath[UB_MAX_PATH_LEN];
	char solidsPath[UB_MAX_PATH_LEN];
	int fmode, i;	
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ubu_mk_spart(splptr>key:%d,%s,)",
						splptr->key, libname));

	/* set up the unibase path/filename */
	fmode = UX_EXISTS;
	if (ux_mk_chk_syspath(UU_NULL, libname, splptr->name, "UB_SPL_EXTEN",
			"UB_SP_SUFFIX",&fmode, tempath, UX_PRTERRS) != UU_SUCCESS) 
			goto failed;
	if (fmode != (fmode|UX_NEXISTS)) /* then file exists */
	{
		if (ux_delete((tempath), UX_PRTERRS) != UU_SUCCESS)
		{  
			uu_uerror2(UB_SYMBOL, 122, tempath, "ubu_mk_spart");
			/* error message: Can't delete standard part %s (%s). */
			goto failed;
		}
	}

	/* set up the ROMULUS database filename */
	fmode = UX_EXISTS;
	if (ux_mk_chk_syspath(UU_NULL, libname, splptr->name, "UB_SPL_EXTEN",
			"UB_SP_SOL_SUFFIX",&fmode, solidsPath, UX_PRTERRS) 
			!= UU_SUCCESS) goto failed;

	/* go do solids pre-processing */
	ur_sp01(solidsPath, UU_NULL, UU_TRUE);

	/* mark modals to save */
	ur_set_save_modals(); 

	/* now save the Unibase stuff */
	if (ur_sp02(tempath) != UU_SUCCESS)
	{
		uu_uerror2(UB_SYMBOL, 32, tempath, "ubu_mk_spart");
		/* error message: Error in archiving standard part: %s (%s). */
		goto failed;
	}
	/* now selectively save the solids */

	if (um_selectivesave_romulus(solidsPath) != UU_SUCCESS)
	{
		uu_dprint(UU_MTRC,(us,"Romulus selective save failed, file:%s",
					solidsPath));
		goto failed;
	}

	/* now fix up views */
	um_post_save();


	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubu_mk_spart",status);
	return(status);
}	
/*********************************************************************
**	 E_FUNCTION : int ubu_place_spart() 
**		 This function places copies of a spart, creating the entity in
**			Unibase.
**	 PARAMETERS	
**		 INPUT  : none. 
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS of no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ubu_place_spart( )
{
	struct UB_sym_instance_frm_rec /* copy forms and defaults */
		spart_place_frm_out; 
	int cmdreject; /* command reject flag */
	struct UB_spart_rec spart_rec; /* the rec for spart to be copied */
	struct UM_body_rec body;
	struct UC_attributedatabag attr;
	UX_pathname fnameu, fnames, fnamea;
	int solnum;
	int status = UU_SUCCESS;
	int dragsegid, entsegid;
	UM_transf tfmat;
	uu_denter(UU_BTRC,(us,"ubu_place_spart()"));
	
	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE); 
	if (!cmdreject)	/* then no command reject encountered, go on */
	{
		/* get all the form data */
		ubu_get_place_data(&spart_place_frm_out);

		/* check the files indicated by the response to the form */
		status = ub_chk_place_files(&spart_place_frm_out, fnameu, fnames);
		if (status == UU_FAILURE) goto failed;

		/* do special pre-processing --  this is a merge */
		status = ur_lp01(fnames, "", UU_FALSE);

		/* do the unibase load */
		UR_load_env = UU_TRUE;
		status = ur_lp02(fnameu);
		if (status != UU_SUCCESS) goto failed;
		UR_load_env = UU_FALSE;

		/* do special post-processing */
		status =	ur_lp03(fnames, "", UU_FALSE);
		if (status != UU_SUCCESS) goto failed;

		/* create a segment to draw copies of the solids into for use
		in dragging (all in one segment) */
		dragsegid = gnseg();
		gcreateseg(dragsegid);
		/* dragging uses its own copy, so this one is actually set INVIS */
		gssegvis(dragsegid, UG_INVISIBLE);

		/* fill in the geometry list with the newly loaded solids entities,
		so spart.no_geom and spart.geom are filled in, in our "local" record
		which is the list of "subentities" */
		solnum = 1;
		spart_rec.no_geom = 0;
		while (ur_get_next_new_data_key(UM_BODY_REL, &solnum,
			&spart_rec.geom[spart_rec.no_geom]) == UU_SUCCESS)
			{
				solnum++;

				body.key = spart_rec.geom[spart_rec.no_geom];
				if (uc_retrieve_data(&body, sizeof(struct UM_body_rec))
					!= UU_SUCCESS)	goto failed;
				/* orient each sub-entity in the standard part template 
				to be scaled, rotated about the part origin */ 
				/* note that "tfmat" isn't used */
				if (!UM_ZILCH(spart_place_frm_out.angle))
					um_rt31_rotbody(&body,UM_cpln.origin,
							UM_cpln.zaxis,spart_place_frm_out.angle,tfmat);
				if(!UM_ZILCH(spart_place_frm_out.scale - 1.0))
					um_sc31_scalbody(&body,UM_cpln.origin,
							tfmat,spart_place_frm_out.scale);
				if(!um_cceqcc(UM_cpln.origin, UM_zerovec))
					um_tr31_tranbody(&body,UM_cpln.origin);

				/* get the seg no. of this geom ent and delete the original seg */
				/*
				if (ur_retrieve_disp_segid(spart_rec.geom[spart_rec.no_geom],
					&entsegid) != UU_SUCCESS)
					goto failed;
				*/
				/* delete the original display segment */
				/* can't do this here, the origional unibase bodies were never
					displayed, therefore they have no DIGS segements (at least
					not a valid DIGS segement #)
					gdeleteseg(entsegid);
				*/

				/* get display attributes */
				/* note: tfmat is not used	*/
				if (uc_retrieve_attr(body.key, &attr) != UU_SUCCESS)
					goto failed;
				uc_retrieve_transf(body.key, tfmat);

				if (uc_draw(&body, tfmat, &attr) != UU_SUCCESS)
					goto failed;

				/* make the original entity non-displayable, so repaint
				and such doesn't cause it to be drawn again */
		 		ur_update_displayable(body.key, UM_NEVERDISPLAYABLE);	
				spart_rec.no_geom++;
			}
		gcloseseg(dragsegid); /* close up the dragging segment */

		/* now create and display copies */
		if (ub_make_spart_copies(&spart_rec, dragsegid) != UU_SUCCESS)
			goto failed;
		um_repaint();

	}/* end no cmdreject */
	else /* command reject hit */
		goto done;

	goto done;
failed: status = UU_FAILURE;
done:;
	UD_UNMARK(cmdreject);	/* take mark off long jump stack */
	uu_dexitstatus(" ubu_place_spart",status);
	return(status);
}  
/*********************************************************************
**	 E_FUNCTION : int ub_make_spart_copies(spartptr, dragsegid)
**		 A dragging copy appears if appropriate.
**		 This function prompts the user for locations to place an copy
**		 of a standard part, creates and displays the copies. 
**	 PARAMETERS	
**		 INPUT  : 
**		 	spartptr		Pointer to the standard part record to copy.
**			dragsegid	Segment that is the drag copy.
**		 OUTPUT :  none.
**	 RETURNS: UU_SUCCESS if no problems encoutered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: displays copies and puts them in UNIBASE.
**	 WARNINGS: none
*********************************************************************/
int ub_make_spart_copies(spartptr, dragsegid)
	struct UB_spart_rec *spartptr;/* pointer to the symbol */
	int dragsegid;
{
	UU_LOGICAL more_copies;
/**	UM_coord origin; **/
    UD_NDCLOCREC origin;

	UD_RUBBER draginfo;  /* data structure for dragging */
	UU_LOGICAL dragon;	/* UU_TRUE iff we are dragging a segment */
	UU_LOGICAL	ud_qlocpet();
	int cmdreject;		 /* command reject flag */
	int i,numint, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
			"ub_make_spart_copies(spartptr:%x,)", spartptr));
			
	/* set mark for long jump if there is a command reject */
	UD_MARK(cmdreject, UU_FALSE); 
	if (!cmdreject)	/* then no command reject encountered, go on */
	{
		/* determine if device supports dragging */
		if(UV_act_screen[0].nvports>1)
			dragon = UU_FALSE;
		else
		{
			if (ud_qlocpet(2, 21)) /* 2=device, 21=dragging */
			{  /* supports dragging */
				dragon = UU_TRUE;
				/* now make sure we are in locator mode */
				ud_dtcord(UD_LOCATOR, 1, 1);
			}
			else
				dragon = UU_FALSE;
		}
		if (dragon)
		{
			/* now set up to drag a copy of the solids */
			/* UM_cpln.origin is used since Romulus is relative to it */
			DRAG_GRAPHICS((&draginfo), dragsegid, UM_cpln.origin, 1); 
			/* allow dragging of segment */
			DRAG_ON(&(draginfo));
		}

		/* for each location (origin) picked, place a copy of the spart */
		more_copies = UU_TRUE;
		while (more_copies)
		{
			ud_ldas(UD_DASCART, UB_SYMBOL, 122, &origin, 1, &numint, UD_NODEFAULT);
			/* message is: Enter location of copy origin. */
			if (numint <= 0) /* then done */
			{
				more_copies = UU_FALSE;
				break;
			}
			/* make one copy of the standard part and display it */
			um_vcmnvc(&origin, UM_cpln.origin, &origin);
			if (ub_mk_disp_copy(spartptr, &origin) != UU_SUCCESS)
				goto failed;
		}/* end more_copies while */

		if (dragon) /* get rid of dragging segment */
		{
			DRAG_OFF(&draginfo);  /* delete dragging copy */
			/* delete the drag segment, if not already done */
		}
	}/* end no command reject */
	else /* command reject hit */
	{ }

	/* when done using these solids for copying, delete the
	original from unibase */
	for (i=0; i<spartptr->no_geom; i++)
	{
		if (uc_delete_noseg(spartptr->geom[i]) != UU_SUCCESS)
			goto failed;
	}

	goto done;
failed: status = UU_FAILURE;
done:;
	UD_UNMARK(cmdreject);	/* take mark off long jump stack */
	uu_dexitstatus("ub_make_spart_copies",status);
	return(status);
}  
/*********************************************************************
**    I_FUNCTION:int ubu_get_spart_geom(spartptr)
**			This function determines the geometry that is to be put in a
**				standard part.
**    PARAMETERS   
**       INPUT  : 
**				spartptr			Pointer to the standard part entity being created.
**       OUTPUT :  
**				spartptr			Pointer to the standard part entity being created
**									with the geometry lists filled in.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubu_get_spart_geom(spartptr)
	struct UB_spart_rec *spartptr;
{
	struct UC_entitydatabag ent;
	int i, rel_num;
	UU_LOGICAL startover;
	UU_KEY_ID key;
	int level, nbrpicks, stat, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
	"ubu_get_spart_geom(spartptr:%x)", spartptr));

	spartptr->no_geom = 0;
	/* clear all bits indicating a tuple is to be saved */
	if (ur_svld_clear() != 0)
	{
		uu_uerror1(UB_SYMBOL, 77, "ubu_get_spart_geom");
		/* error is:Can't clear save/load bit map (%s) */
		goto failed;
	}

	ud_lgeo(UU_TRUE, UD_body);
	ud_ldas(UD_DASSELECT, UB_SYMBOL, 124, UU_NULL, 100, &nbrpicks, UD_NODEFAULT);
	/* prompt: Pick entities to be put in standard part. */
	if (nbrpicks <= 0)
		goto done;

	level = 1;	/* picking level */
	startover = UU_TRUE;	/* get all keys for entities picked */	
	while ((status == UU_SUCCESS) 
			&& (ud_gnxt(startover, UU_NULL, &key, level)))
	{
		startover = UU_FALSE;
		/* put the geometry key into the geometry key list */
		spartptr->geom[spartptr->no_geom] = key;
		spartptr->no_geom++;
		ur_mark_to_save(key);
	}/* end while */

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubu_get_spart_geom",status);
	return(status);
}
/*********************************************************************
**    I_FUNCTION : int ubi_orient_spart(spartptr, spartorigin)
**       Fix the geometry for the spart so that it is relativized
**		 	to the spart origin and construction plane coordinate axises; 
**    PARAMETERS   
**       INPUT  : 
**      		spartptr	pointer to the record that is to be relativized.
**				spartorigin	coordinates of the origin in model coordinates.
**       OUTPUT :  
**          output
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_orient_spart(spartptr, spartorigin)
	struct UB_spart_rec *spartptr;
	UM_vector spartorigin;
{
	struct UC_entitydatabag e;
	int i, status = UU_SUCCESS;
	
	uu_denter(UU_BTRC,(us,"ubi_orient_spart(spartptr:%x,spartorigin:%g,%g,%g)",
					spartptr, spartorigin[0],spartorigin[1],spartorigin[2]));

	/* for each geometric entity in the spart, relativize it to the
	 * spart origin */
	for (i=0; i<spartptr->no_geom; i++)
	{
		e.key = spartptr->geom[i];

		if (uc_retrieve_data(&e, sizeof(e)) != UU_SUCCESS)
		{
			uu_uerror1(UB_SYMBOL, 22, e.key);
			/* message is: Can not retrieve data for key = %d 
			 * (ubi_orient_spart). */
			goto failed;
		}
		um_tr31_tranbody(&e,spartorigin);
	}/* end for */

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ubi_orient_spart",status);
	return(status);
}
/*********************************************************************
**	 E_FUNCTION :int ub_mk_disp_copy(spartptr, origin)
**		Given an origin to locate the copy at, and the part info, this 
**		is the routine that creates one copy of the part.
**	 PARAMETERS	
**		 INPUT  : 
**			 spartptr		pointer to the standardf part record.
**			 origin			coordinate picked  for translation of this copy
**		 OUTPUT :  
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ub_mk_disp_copy(spartptr, origin)
	struct UB_spart_rec *spartptr;
	UM_coord origin;
{
	struct UC_entitydatabag *ent;
	struct UC_entitydatabag *newent;
	int i, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,
	"ub_mk_disp_copy(spart->no_geom:%d, origin:%g, %g, %g)",
	spartptr->no_geom, origin[0],origin[1],origin[2]));

	ent = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));
	newent = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));

	/* for each solid entity */
	for (i=0; i<spartptr->no_geom; i++) 
	{
		/* retrieve the data and make a new copy of the entity */
		ent->key = spartptr->geom[i];
		if (uc_retrieve_data(ent, sizeof(struct UC_entitydatabag))
			!= UU_SUCCESS)	goto failed;
		if (uc_copy(ent, newent, sizeof(struct UC_entitydatabag)) 
			!= UU_SUCCESS) goto failed;
		/* set displayable, since the one we used to copy from wasn't */
		ur_update_displayable(newent->key, UM_DISPLAYABLE);
		/* translate the new body using the "picked" origin */
		um_tr31_tranbody(newent,origin);
		if (uc_display(newent) != UU_SUCCESS)
			goto failed;
	}/* end of  creating and drawing  the copy of the  geometry */

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_free(ent);
	uu_free(newent);
	uu_dexitstatus("ub_mk_disp_copy",status);
	return(status);
}  
