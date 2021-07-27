/*********************************************************************
**    NAME         :  m9udrw3.c
**       CONTAINS: user interface routines for drawings
**                      umu_46_archive_drawing()
**                      umu_46_retrieve_drawing()
**                      int um_archive_drawing(drawing, fnameu, fnamea)
**                      int um_retrieve_drawing(drawname, fnameu, fnamea)
**                      int um_open_archive_file(rw, filename, fnameu, fnamea)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       m9udrw3.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:13
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mxxx.h"
#include "mdebug.h"
#include "mdraw.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "udforms.h"
#include "udfdata.h"

/*********************************************************************
**    S_FUNCTION     :  static um_browseo_dwfile(fieldno, val, stat)
**       Method called at 'drawing file browser' toggle field for
**		"Retrieve drawing form".
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT um_browseo_dwfile(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	char filename[UX_MAX_PATH_LEN], ext[UX_MAX_FILE_LEN], ext1[UX_SUFFIX_LEN],*p;
	char *ux_getenv(),descrip[UX_MAX_PATH_LEN];
	int len;
	filename[0] = '\0';
	strcpy(descrip, "Drawing Files (");
	strcpy(ext,"*.");
	p = ux_getenv("UM_DRAWING_SUFFIX");
	if (p != UU_NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(ext,ext1);
	}       
	else 
	{
		strcat(ext,"dw");
	}
	strcat(descrip, ext);
	strcat(descrip, ")");
	ud_get_filename("Drawing Files", "Drawing Files", ext,
							filename, &len,descrip, 1, UU_FALSE) ;
	if (filename[0]!='\0')
	{
		ud_update_answer(*fieldno-1, (int*)filename);
	}
	else
/*
.....canceled
*/
	*fieldno = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  static um_browses_dwfile(fieldno, val, stat)
**       Method called at 'drawing file browser' toggle field for
**		"Archieve drawing form".
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form display mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT um_browses_dwfile(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	char filename[UX_MAX_PATH_LEN], ext[UX_MAX_FILE_LEN], ext1[UX_SUFFIX_LEN],*p;
	char *ux_getenv(),descrip[UX_MAX_PATH_LEN];
	int len;
	filename[0] = '\0';
	strcpy(descrip, "Drawing Files (");
	strcpy(ext,"*.");
	p = ux_getenv("UM_DRAWING_SUFFIX");
	if (p != UU_NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(ext,ext1);
	}       
	else 
	{
		strcat(ext,"dw");
	}
	strcat(descrip, ext);
	strcat(descrip, ")");
	ud_get_filename("Drawing Files", "Drawing Files", ext,
							filename, &len,descrip, 0, UU_FALSE) ;
	if (filename[0]!='\0')
	{
		ud_update_answer(*fieldno-1, (int*)filename);
	}
	else
/*
.....canceled
*/
	*fieldno = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : umu_46_archive_drawing()
**       Prompt the user for the file to archive a drawing.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_46_archive_drawing()

	{
	char fnameu[UX_MAX_PATH_LEN];
	char fnamea[UX_MAX_PATH_LEN], *str;
	UU_KEY_ID drawkey;
	int status, len, option;
	UD_LIST drawing_name_list;
	static char filename[UX_MAX_PATH_LEN] = "";
	static char drawname[16] = " ";
	UD_FSTAT uj_noop();
	int *ans[3];
	static UD_METHOD methods[3] = {
							uj_noop, uj_noop, um_browses_dwfile };           
	static char called[] = { 6,6,6};
	uu_denter(UU_MTRC,(us,"umu_46_archive_drawing()"));

/*
.....change form structure to
.....display drawing in the form
.....Yurong 8/18/97
*/
	drawing_name_list.item = 
					(char **) um_get_drawing_name(&(drawing_name_list.num_item));
	if (drawing_name_list.num_item == 0)
	{
		uu_uerror1(/* no drawing exist */ UM_MODEL, 319);
		return;
	}
	len = strlen(drawname);
/*
......the answer should hold max_label + subnum chars. The drawname here
......could be only default to " "
*/
	if (len<80) len = 80;
	drawing_name_list.answer = (char *) uu_malloc((len+1) * sizeof(char));
	strcpy(drawing_name_list.answer, drawname);
/*
.....added browse button
.....Yurong 9/15/98
*/
	option = 0;
	ans[0] = (int *)&drawing_name_list;
	ans[1] = (int *)filename;
	ans[2] = (int *)&option;
/*      status = ud_form("marcdraw.frm", ans, ans);    */
	status = ud_form1("marcdraw.frm", ans, ans, methods, called, NULL, NULL);
	if (status != UU_SUCCESS) goto done;
/*
.....added code for check filename
.....and remove empty space before filename
.....Yurong 8/19/98
*/
	if (filename[0]=='\0') goto done;
/*
   str = filename;
   str = (char*)strtok(filename, " \n\t");
	if (str!=NULL)
		strcpy(filename, str);
*/
	strcpy(drawname, drawing_name_list.answer);
	status = um_key_from_drawing_name(drawname, &drawkey);
	status = um_open_archive_file(UX_WRITE, filename, fnameu, fnamea);
	if (status != UU_SUCCESS) return;

	um_archive_drawing(drawkey, fnameu, fnamea);

done:
	ud_free_flist(&drawing_name_list);
	uu_dexit;
}
/*********************************************************************
**    E_FUNCTION     : umu_46_retrieve_drawing()
**       Prompt the user for the drawing archive file to read.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_46_retrieve_drawing()

{
	char drawname[16];
	char filename[UX_MAX_PATH_LEN];
	char fnameu[UX_MAX_PATH_LEN];
	char fnamea[UX_MAX_PATH_LEN];
	int status, option;
	UU_LOGICAL change_drwscale;
	int *ans[3];
	UD_FSTAT uj_noop();
	static UD_METHOD methods[3] = {
							uj_noop, uj_noop, um_browseo_dwfile };           
	static char called[] = { 6,6,6};
	uu_denter(UU_MTRC,(us,"umu_46_retrieve_drawing()"));

	strcpy(drawname, "");
	strcpy(filename, "");
	option  = 0;
	ans[0] = (int *) drawname;
	ans[1] = (int *) filename;
	ans[2] = (int *) &option;

	status = ud_form1("mretdraw.frm", ans, ans, methods,called,NULL,NULL); 
	if (status != UU_SUCCESS) goto done;
	status = um_open_archive_file(UX_READ, filename, fnameu, fnamea);
	if (status != UU_SUCCESS) goto done;

	change_drwscale = ud_yesno(0, 
								"change drawing scale for retrieved drawing?", 
								"Retrieve Drawing");
	if (change_drwscale) umu_set_drwscale();
	um_retrieve_drawing(drawname, fnameu, fnamea, change_drwscale);
done:
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : int um_archive_drawing(drawkey, fnameu, fnamea)
**       Archive the specified drawing (KEY).
**    PARAMETERS   
**       INPUT  : 
**          drawkey                                     UNIBASE key of drawing to archive
**                              fnameu                                  full name of file to archive drawing
**                                                                                      to
**                              fnamea                                  full name of file to archive drawing
**                                                                                      to
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
um_archive_drawing(drawkey, fnameu, fnamea)
	UU_KEY_ID drawkey;
	char fnameu[];
	char fnamea[];

	{
	int status;
	UU_KEY_ID active_drawing;

	uu_denter(UU_MTRC,(us,"um_archive_drawing(key=%d, fnameu=%s)",
		drawkey, fnameu));

	/* set status to assume failure */
	status = UU_FAILURE;

	/* if this is the current active drawing, set displayable flag to
		UM_UNDISPLAYABLE (for subsequent load) */
	active_drawing = ur_get_drwmdl_curdrw();
	if (active_drawing == drawkey)
		{
		um_update_active_drawing();
		um_fordrawing_set_disp(drawkey, UM_UNDISPLAYABLE);
		}

	/* clear all bits indicating which tuples are to be saved */
	if (ur_svld_clear() != 0)
		{
		uu_dprint(-1,(us,"..FAILURE um_archive_drawing: from ur_svld_clear"));
		goto done;
		}

	/* set bit to indicate the drawing tuple is to be saved */
	if (ur_save_set(drawkey) != 0)
		{
		uu_dprint(-1,(us,"..FAILURE um_archive_drawing: from ur_set_save drawing"));
		goto done;
		}

	/* set bit to indicate the geom associated with each key is to be saved */
/*
	for (i=0; i<drawing->no_member; i++) 
		if (ur_save_set(drawing->member[i]) != 0) 
			{
			uu_dprint(-1,(us,"..FAILURE um_archive_drawing: ur_set_save member=%x",
				drawing->member[i]));
			goto done;
			}
*/
	
	/* save all marked tuples */
	if (ur_sp02(fnameu) != 0) 
		{
		uu_dprint(-1,(us,"..FAILURE um_archive_drawing: from ur_sp02"));
		goto done;
		}
/*
      status = um_selectivesave_appgeo(fnamea);
      if (status != UU_SUCCESS)
              {
              uu_dprint(-1,(us,"..FAILURE um_archive_drawing: from um_selectivesave_appgeo"));
              goto done;
              }
*/

	/* set status for success */
	status = UU_SUCCESS;

done:
	/* finally, reset displayable flag if necessary */
	if (active_drawing == drawkey)
		um_fordrawing_set_disp(drawkey, UM_DISPLAYABLE);

	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_retrieve_drawing(drawname, fnameu, fnamea,
**                                                                      change_drwscale)
**       Read the specified drawing archive file.
**    PARAMETERS   
**       INPUT  : 
**          newname                                             name of retrieved drawing
**                              fnameu                                          full name of unibase file to retrieve
**                              fnamea                                          full name of ag file to retrieve
**                              change_drwscale                 UU_TRUE => use current drawing scale
**                                                                                              UU_FALSE => use stored drawing scale
**       OUTPUT :  
**          none
**    RETURNS      : 
**                      UU_SUCCESS if file read correctly
***             UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
um_retrieve_drawing(drawname, fnameu, fnamea, change_drwscale)
	char drawname[];
	char fnameu[];
	char fnamea[];
	UU_LOGICAL change_drwscale;
	{
	int status;
	int next_tupleid;
	int i,numint;
	UU_KEY_ID key;
	UU_LOGICAL found;
	struct UM_drawing_rec newdrawing;
	struct UM_drawing_rec olddrawing;
	UU_KEY_ID drawing_view;

	uu_denter(UU_MTRC,(us,"um_retrieve_drawing(name=%s, file=%s)",
		drawname, fnameu));

	/* set status to assume failure */
	status = UU_FAILURE;

	/* read the archive file */
	status = ur_lp02(fnameu, 1);
/*
	status = ur_lp02(fnameu);
*/
	if (status != UU_SUCCESS) 
	{
		goto done;
	}
	um_load_appgeo(fnamea, UU_FALSE);
	um_post_load_appgeo();

	/* get the data for the single drawing just read */
	next_tupleid = 1;
	ur_get_next_new_data_key(UM_DRAWING_REL, &next_tupleid, &newdrawing.key);
	um_get_all_geom(&newdrawing, sizeof(newdrawing));

	/* determine the name to store with the newly read drawing */
/*
.....remove trailing spaces and limit to 16chars because the drawing name
.....inside "struct UM_drawing_rec" only defined as char[16]
*/
	for (i=strlen(drawname); i>0; i--)
	{
		if (drawname[i-1]==' ')
			drawname[i-1] = '\0';
		else
			break;
	}
	drawname[15] = '\0';
	if (strcmp(drawname, "") == 0)
		strcpy(drawname, newdrawing.name);

	/* check to see if a drawing with the same name already exists */
checkname:
	found = UU_FALSE;
	next_tupleid = 1;
	while (ur_get_next_data_key(UM_DRAWING_REL, &next_tupleid, &key) > -1)
		{
		next_tupleid++;
		if (key != newdrawing.key)
			{
			olddrawing.key = key;
			um_get_all_geom(&olddrawing, sizeof(olddrawing));
			found = (strcmp(olddrawing.name, drawname) == 0);
			}
		if (found) break;
		}

	/* if a drawing with the specified name already exists, see what the
		user wants to do about it */
	if (found)
		{
		if (ud_yesno(NULL,"Delete old drawing?",
			"Drawing Exists"))
			uc_delete(olddrawing.key);
		else
			{
			ud_ldas(UD_DASSTRING,/* enter drawing name */ UM_MODEL, 227, drawname,
					16, &numint, UD_DEFAULT);
			if (numint > 0) goto checkname;
			uc_delete(newdrawing.key);
			goto done;
			}
		}

	/* finally, have found a unique name for the retrieved drawing so
		update UNIBASE */
	strcpy(newdrawing.name, drawname);
	if (change_drwscale)
		{
		newdrawing.drwscale = ur_get_drwmdl_drwscale();
		newdrawing.modscale = ur_get_drwmdl_modscale();
		newdrawing.modunits = ur_get_drwmdl_modunits();
		newdrawing.drwunits = ur_get_drwmdl_drwunits();
		}
	um_update_geom(&newdrawing, UM_DEFAULT_TF);

	/* set the view key to be the current drawing view key for all
		entities read in */
	drawing_view = ur_get_drwmdl_drwvw();
	next_tupleid = 1;
	while (ur_get_next_new_data_key(0, &next_tupleid, &key) == 0)
		{
		ur_update_view_key(key, drawing_view);
		next_tupleid++;
		}

	/* successful load */
	status = UU_SUCCESS;

done:
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_open_archive_file(rw, filename, fnameu,
**                                                                              fnamea)
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_open_archive_file(rw, filename, fnameu, fnamea)
	int rw;
	char filename[];
	char fnameu[];
	char fnamea[];

	{
	int status;
	char msg[UX_MAX_PATH_LEN+40];
	char fname[UX_MAX_PATH_LEN];
	char dir[UX_MAX_PATH_LEN];
	UU_LOGICAL fileexists;
	UU_LOGICAL readaccess;
	UU_LOGICAL writeaccess;
	int mode;

	uu_denter(UU_MTRC,(us,"um_open_archive_file(rw=%d,name=%s)",rw,filename));

	mode = rw;
/*
.....the filename may include path already
.....Yurong changed 9/15/98
*/
/*
	ux_mk_chk_syspath(UU_NULL, "^UM_DRW_ARCHIVE_LIB", filename, UU_NULL,
		"UM_DRAWING_SUFFIX", &mode, fnameu, UX_PRTERRS); 
*/
	ul_break_fname(filename, dir, fname);
	if (dir[0]!='\0')
	{
		status = ux_mk_chk_syspath(UU_NULL, NULL, filename, UU_NULL,
						"UM_DRAWING_SUFFIX", &mode, fnameu, UX_PRTERRS);
	}
	else
	{
		status = ux_mk_chk_syspath(UU_NULL, "^UM_DRW_ARCHIVE_LIB", filename,
						UU_NULL,"UM_DRAWING_SUFFIX", &mode, fnameu, UX_PRTERRS);
	}
	status = UU_FAILURE;

	fileexists = !(mode & UX_NEXISTS);
	if (rw == UX_READ)
		{
		if (!fileexists)
			{
			uu_uerror1(/* file does not exist */ UM_MODEL, 234, fnameu);
			goto done;
			}
		readaccess = (mode & UX_READ);
		if (!readaccess)
			{
			uu_uerror1(/* don't have read access */ UM_MODEL, 235, fnameu);
			goto done;
			}
		}
	else if (rw == UX_WRITE)
		{
		if (fileexists)
			{
			sprintf(msg, "drawing file %s exists, overwrite?", fnameu);
			if (ud_yesno(NULL, msg, "File Exists"))
				{
				ux_delete(fnameu, UX_PRTERRS);
				}
			else
				goto done;
			writeaccess = (mode & UX_WRITE);
			if (!writeaccess)
				{
				uu_uerror1(/* don't have write access */ UM_MODEL, 236, fnameu);
				goto done;
				}
			}
		}
	else goto done;

	/* get AG file name */
	if (dir[0]!='\0')
	{
		status = ux_mk_chk_syspath(UU_NULL, NULL, filename, UU_NULL,
						"UM_DRAWING_SUFFIX", &mode, fnamea, UX_PRTERRS);
	}
	else
	{
		status = ux_mk_chk_syspath(UU_NULL, "^UM_DRW_ARCHIVE_LIB", filename,
						UU_NULL,"UM_DRAWING_SUFFIX", &mode, fnamea, UX_PRTERRS);
	}
	status = ux_add_ftype("UM_DRW_ASUFFIX", fnamea, UX_PRTERRS);

	status = UU_SUCCESS;

done:
	uu_dexit;
	return(status);
}

