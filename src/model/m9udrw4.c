/*********************************************************************
**    NAME         :  m9udrw4.c
**       CONTAINS: user interface routines for drawings
**			umu_46_load_drawing()
**			umu_46_rename_drawing()
**    COPYRIGHT 2000 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m9udrw4.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 16:36:02
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mxxx.h"
#include "mdebug.h"
#include "xenv1.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "mpocket.h"

static int newload;

/*********************************************************************
**
**			load_drawing
**
*********************************************************************/
static UD_FSTAT load_drawing(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UX_pathname fname,fullname,fnamea,fnameu,descrip;
	char paths[UX_MAX_PATH_LEN*20], path_des[UX_MAX_PATH_LEN*20];
	char sufx[UX_SUFFIX_LEN],*p;
	int nc,mode,status,file_status;
	char *ux_getenv();
/*
.....Get the file to load
*/
	fname[0] = '\0' ; nc = 0;
	strcpy(descrip, "Drawing Files (");
	p = ux_getenv("UM_DRAWING_SUFFIX",UX_NPRTERRS);
	if (p != UU_NULL)
	{
		strcpy(fnamea,p);
		ul_remove_quotes(fnamea);
		strcpy(sufx,"*."); strcat(sufx,fnamea);
	}
	else
	{
		strcat(sufx,"dw");
	}
	strcat(descrip, sufx);
	strcat(descrip, ")");

	strcpy(paths, "UM_DRW_ARCHIVE_LIB;NCL_INCDIR");
	strcpy(path_des, "System;Include");
	ud_get_filename1(NULL, "Load Drawing", sufx, fname,&nc, descrip, 1, UU_FALSE, paths, path_des);
	if (nc == 0) goto done;
/*
.....Verify the drawing file exists
*/
	mode = UX_EXISTS|UX_READ;
	status = ux_file_inquire(UU_NULL,UU_NULL,fname,UU_NULL,UU_NULL,&mode,
		&file_status,fullname,UX_NPRTERRS);
	if (status != UU_SUCCESS || mode == (mode|UX_NEXISTS))
	{
		sprintf(fnamea,"Could not find Drawing File '%s'.",fullname);
		ud_wrerr(fname);
		status = UU_FAILURE;
		goto done;
	}
/*
.....Open the drawing file
*/
	ul_remove_quotes(fullname);
	status = um_open_archive_file(UX_READ, fullname, fnameu, fnamea);
	if (status != UU_SUCCESS) goto done;
/*
.....Close the form so that the main routine can
.....redisplay it with any newly defined macros
*/
	newload = 1;
	ud_mfclose_form(1);
/*
.....Retrieve the drawing
*/
	sufx[0] = '\0';
	um_retrieve_drawing(sufx,fnameu,fnamea,UU_FALSE);
done:;
	return(UD_FLDOK);
}
/*********************************************************************
**
**			view_drawing
**
*********************************************************************/
static UD_FSTAT view_drawing(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int nc,status;
	char name[20],msg[80];
	UD_DDATA data;
	struct UM_drawing_rec drawing;
/*
.....Get the drawing name
*/
	data.frmstr = name;
	ud_get_field(0,data);

	nc = 20;
	ul_strip_blanks(name,&nc);
/*
.....Verify the drawing exists
*/
	if (um_key_from_drawing_name(name,&drawing.key) == -1)
	{
		sprintf(msg,"The requested drawing '%s' does not exist.",name);
		ud_wrerr(msg);
	}
/*
.....View the drawing
*/
	else
	{
/*
........Load the drawing information
*/
		status = um_get_all_geom(&drawing,sizeof(struct UM_drawing_rec));
		if (status != UU_SUCCESS) goto done;
/*
.....Open the Pocket Window
*/
		status = um_pocket_window(NULL, &drawing,UM_DRAWING_WINDOW);
	}
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : umu_46_load_drawing()
**       Prompt the user for a drawing to make active.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_46_load_drawing()

	{
	int len;
	struct UM_drawing_rec drawing;
	int status;
	UD_LIST drawing_name_list;
	static char draw_name[16] = " ";
	UU_LOGICAL cmdreject;

/*
.....Form fields
*/
   static UD_METHOD methods[] = {NULL,load_drawing,view_drawing};
   static char called[] = {6,6,6,6};
   static char traverse[] = {1,1,1,1};
   static char display[] = {1,1,1,1};
   int *ans[4];

	uu_denter(UU_MTRC,(us,"umu_46_load_drawing()"));
/*
.....Command Reject
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)
	{
		status = UU_FAILURE;
		goto done;
	}
/*
.....Use form instead of prompt
.....Yurong 8/16/97
*/
repeat:;
	drawing_name_list.item =
					 (char **) um_get_drawing_name(&(drawing_name_list.num_item));
/*
	if (drawing_name_list.num_item == 0)
	{
		uu_uerror1(UM_MODEL, 319);
		return;
	}
*/
	len = strlen(draw_name);
/*
......the answer should hold max_label + subnum chars. The drawname here
......could be only default to " "
*/
	if (len<80) len = 80;
	drawing_name_list.answer = (char *) uu_malloc((len+1) * sizeof(char));
	strcpy(drawing_name_list.answer, draw_name);
/*
.....Get form input
*/
	ans[0] = (int *)&drawing_name_list;
	ans[1] = UU_NULL; ans[2] = UU_NULL; ans[3] = UU_NULL;
	newload = 0;
	status = ud_form1("mviewdraw.frm", ans, ans,methods,called,display,traverse);
	if (status==-1)
	{
		ud_free_flist(&drawing_name_list);
		goto done;
	}
/*
.....The user loaded an external drawing
.....Redisplay the form
*/
	if (newload == 1)
	{
		uu_free(drawing_name_list.item);
		goto repeat;
	}
/*
.....View the drawing
*/
	strcpy(draw_name, drawing_name_list.answer);
	strcpy(drawing.name, drawing_name_list.answer);
	status = um_key_from_drawing_name(drawing.name, &drawing.key);
	um_get_all_geom(&drawing, sizeof(drawing));
	um_view_drawing(&drawing);
done:;
	ud_free_flist(&drawing_name_list);
	um_close_pocket_window(UM_DRAWING_WINDOW);
	UD_UNMARK(cmdreject);
}
/*********************************************************************
**    E_FUNCTION     : umu_46_rename_drawing()
**       Prompt the user for a drawing to rename.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_46_rename_drawing()
{
	int len;
	int *ans[2];
	struct UM_drawing_rec drawing;
	char newname[16];
	int status;
	UU_KEY_ID key;
	UX_pathname bname;
	UD_LIST drawing_name_list;
	static char draw_name[16] = " ";

	uu_denter(UU_MTRC,(us,"umu_46_load_drawing()"));
/*
.....Use form instead of prompt
.....Yurong 8/16/97
*/
	strcpy(newname, "");
	drawing_name_list.item =
					 (char **) um_get_drawing_name(&(drawing_name_list.num_item));
	if (drawing_name_list.num_item == 0)
	{
		uu_uerror1(/* no drawing exist */ UM_MODEL, 319);
		return(UU_FAILURE);
	}
	len = strlen(draw_name);
/*
......the answer should hold max_label + subnum chars. The drawname here
......could be only default to " "
*/
	if (len<80) len = 80;
	drawing_name_list.answer = (char *) uu_malloc((len+1) * sizeof(char));
	strcpy(drawing_name_list.answer, draw_name);
	ans[0] = (int *)&drawing_name_list;
	ans[1] = (int *)newname;
	status = ud_form("mrename.frm", ans, ans);
	if (status==-1)
	{
		ud_free_flist(&drawing_name_list);
		return(UU_FAILURE);
	}
	strcpy(draw_name, drawing_name_list.answer);
	strcpy(drawing.name, drawing_name_list.answer);

	status = um_key_from_drawing_name(drawing.name, &drawing.key);
	if (ux_get_base_fname(newname, bname, (UX_NPRTERRS | UX_NCHK))
						!= UU_SUCCESS)
	{
		uu_uerror0(UX_UDOS,24);
		return(UU_FAILURE);
	}
	status = um_key_from_drawing_name(newname, &key);
	if (status == 0 )
	{
		uu_uerror1(/* drawing already exists */ UM_MODEL, 204, newname);
		return(UU_FAILURE);
	}
	else
	{
		um_get_all_geom(&drawing, sizeof(drawing));
		strcpy(drawing.name, newname);
		um_update_geom(&drawing, UM_DEFAULT_TF);
	}
	ud_free_flist(&drawing_name_list);
	return(UU_SUCCESS);
	uu_dexit;
}
