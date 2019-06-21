/*********************************************************************
**    NAME         : vuscreen.c
**		CONTAINS: user interface routines to handle changing screen format
**			int uvu_screen_format_form(newscreen)
**			uvu_select_screen_format(sc_name)
**          uvu_view_chk(token,viewkey)
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       vuscreen.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 12:06:40
*********************************************************************/
#include "usysdef.h"
#include "uhep.h"
#include "udebug.h"
#include "view.h"
#include "nclfc.h"
#include "mfort.h"
#include "driver.h"
#include "udforms.h"
#include "ulist.h"
#include "mxxx.h"

#define NO_DEFAULT 0
#define DEFAULT 1
/*
......for all views
*/
static UD_LIST view_list[UV_NVPORTS];
/*
.....for 12 view + more
*/
static UD_LIST fview_list[UV_NVPORTS];
/*
......for all screen
*/
static UD_LIST screen_list;
/*
......for 12 screen  + more
*/
static UD_LIST fscreen_list;

static UD_LIST vport_list, cview_list;
static int mainmarkval=0;
static int frm_choice[120];
static int 	cur_sel_vport = 0;
static char vport_name[UV_NVPORTS][20];
static char screen_name[20];
static int vport_disp = 0;
static int save_frm_data = 1;
static int choice[36];
static int srn_frm = 0;
static char SelView[40], SelScreen[40];
static int save_vindx = -1;

static int Sviewfrm_cancel=0, Sscreenfrm_cancel=0, Sviewfrm=-1, Sscreenfrm=-1;

extern UD_METHOD UD_initfrm_intry;

char **uv_get_screenname();
char **uv_get_viewname();

/*********************************************************************
**   I_FUNCTION: uv_add_layer_list(list)	
**      Added the layer list at the end of current list
**   PARAMETERS
**       INPUT  : list  : list to add the layer list
**       OUTPUT : list  : list that has added the layer list
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void uv_add_layer_list(list)	
UD_LIST *list;
{
	int i,j,k,status, ii, add;
	UU_LIST layer;
	int nlayer, cur_active_layer;
	UD_LIST temp_list;
	struct UM_layer_rec *sptr;
/*
.....Added "layer list" as end of the view
*/		
	status = umu_load_layers(&layer,&nlayer,&cur_active_layer);
	if (nlayer<1)
		return;
	temp_list.num_item = list->num_item + nlayer;
	temp_list.answer = (char *) uu_malloc(80 * sizeof(char));
	strcpy(temp_list.answer, list->answer);

	temp_list.item = (char **) uu_malloc(temp_list.num_item *sizeof(char *)); 
	for (i=0; i<list->num_item; i++)
	{
		temp_list.item[i] = (char *)uu_malloc(sizeof(char)*40);
		strcpy(temp_list.item[i], list->item[i]);
	}
	sptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&layer);
	for (j=0, k=list->num_item;j<nlayer;j++, k++)
	{
/*
.....if there is the layer number before, don't put it again
*/
		add = 1;
		for (ii=0; ii<j; ii++)
		{
			if (sptr[j].num==sptr[ii].num)
			{
				add = 0;
				k--;
				temp_list.num_item--;
				break;
			}
		}
		if (add)
		{
			temp_list.item[k] = (char *)uu_malloc(sizeof(char)*40);
			sprintf(temp_list.item[k], "Layer%d", sptr[j].num);
		}
	}
	ud_free_flist(list);
/*
.....copy temp_list to list
*/
	list->num_item = k;
	list->item = (char **) uu_malloc(list->num_item *sizeof(char *)); 
	for (j=0; j<list->num_item; j++)
	{
		list->item[j] = (char *) uu_malloc(40 * sizeof(char));
		strcpy(list->item[j], temp_list.item[j]);
	}
	list->answer = (char *) uu_malloc(80 * sizeof(char));
	strcpy(list->answer, temp_list.answer);
	ud_free_flist(&temp_list);
}

static UD_FSTAT uv_inits_selfrm(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(UD_FLDOK);
}

static UD_FSTAT uv_initv_selfrm(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char label[80];
	sprintf(label, "Select a View For the \"%s\" Viewport.", vport_name[0]);
	ud_dispfrm_update_label(*fieldno, 0, label);
	return(UD_FLDOK);
}


/*********************************************************************
**   I_FUNCTION: OnSelectView(fieldno,val,stat)
**      Callback function for a list Selecton from the Select View form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSelectView(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (val->frmstr[0]!='\0')
		strcpy(SelView, val->frmstr);
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnCancelView(fieldno,val,stat)
**      Callback function for the Cancel button on View Selection form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnCancelView(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....User rejected the form
*/
	Sviewfrm_cancel = -1;
	ud_close_dispfrm(Sviewfrm);
	*fieldno = -1;
	Sviewfrm = -1;
	return(UD_FRMCLOSE);
}

/*********************************************************************
**   I_FUNCTION: OnCloseView()
**      Callback function for the Close button on View Selection form..
**   PARAMETERS
**       INPUT  : none.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnCloseView()
{
/*
.....Mark the form as closed
*/
	Sviewfrm = -1;
	return(UD_FLDOK);
}
/*********************************************************************
**   E_FUNCTION: uv_view_select(vlist)
**      This function controls the View Selection form.  It displays
**      a list of Views and returns the user selection 
**     
**   PARAMETERS
**       INPUT  : vlist   = UD_list structure of view list.
**                        
**       OUTPUT : vlist  = UD_list structure of view list.
**						the answer field include the select answer
**   RETURNS: -1 if the user did not select any item.
**   SIDE EFFECTS: none 
**   WARNINGS: none
*********************************************************************/
int uv_view_select(vlist)
UD_LIST vlist;
{
	UD_METHOD save_entry;

	int *ans[2];
	static UD_METHOD methods[] = {OnSelectView,OnCancelView,OnCloseView};
	static char called[]       = {6,6};
	static char traverse[]     = {1,1};
	static char disp[]         = {1,1,1};

/*
......only allow one view form exist
*/
	if (Sviewfrm!=-1)
		return;
/*
.....Setup the default answers
*/
	ans[0] = (int *)&vlist;
	ans[1] = UU_NULL;
	strcpy(SelView, vlist.answer);
/*
.....Display the form
*/
	save_entry = UD_initfrm_intry;
	UD_initfrm_intry = uv_initv_selfrm;

	Sviewfrm = ud_form_display1("viewsel.frm", ans, ans, methods, called, disp,
		traverse);
	UD_initfrm_intry = save_entry;

	if (Sviewfrm == -1) goto nofrm;
/*
.....Wait for the user to select a view
.....or cancel the form
*/
	Sviewfrm_cancel = 0;
	uw_dispfrm_wait(Sviewfrm);
	goto done;
nofrm:
	ud_wrerr("Could not load 'viewsel.frm'.");
	Sviewfrm_cancel = -1;
	goto done;
/*
.....End of routine
*/
done:
	if (Sviewfrm_cancel!=-1)
		strcpy(vlist.answer, SelView);
	return(Sviewfrm_cancel);
}

/*********************************************************************
**   I_FUNCTION: OnSelectScreen(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Screen form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSelectScreen(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (val->frmstr[0]!='\0')
		strcpy(SelScreen, val->frmstr);
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnCancelScreen(fieldno,val,stat)
**      Callback function for the Cancel button on Screen Selection form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnCancelScreen(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....User rejected the form
*/
	Sscreenfrm_cancel = -1;
	ud_close_dispfrm(Sscreenfrm);
	*fieldno = -1;
	Sscreenfrm = -1;
	return(UD_FRMCLOSE);
}

/*********************************************************************
**   I_FUNCTION: OnCloseScreen()
**      Callback function for the Close button on Screen Selection form..
**   PARAMETERS
**       INPUT  : none.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnCloseScreen()
{
/*
.....Mark the form as closed
*/
	Sscreenfrm = -1;
	return(UD_FLDOK);
}
/*********************************************************************
**   E_FUNCTION: uv_screen_select(slist)
**      This function controls the View Selection form.  It displays
**      a list of Screens and returns the user selection 
**     
**   PARAMETERS
**       INPUT  : slist   = UD_list structure of view list.
**                        
**       OUTPUT : slist  = UD_list structure of view list.
**						the answer field include the select answer
**   RETURNS: -1 if the user did not select any item.
**   SIDE EFFECTS: none 
**   WARNINGS: none
*********************************************************************/
int uv_screen_select(slist)
UD_LIST slist;
{
	UD_METHOD save_entry;

	int *ans[2];
	static UD_METHOD methods[] = {OnSelectScreen,OnCancelScreen,OnCloseScreen};
	static char called[]       = {6,6};
	static char traverse[]     = {1,1};
	static char disp[]         = {1,1,1};
/*
......only allow one view form exist
*/
	if (Sscreenfrm!=-1)
		return;
/*
.....Setup the default answers
*/
	ans[0] = (int *)&slist;
	ans[1] = UU_NULL;
	strcpy(SelScreen, slist.answer);
/*
.....Display the form
*/
	save_entry = UD_initfrm_intry;
	UD_initfrm_intry = uv_inits_selfrm;
	Sscreenfrm = ud_form_display1("screensel.frm", ans, ans, methods, called, disp,
		traverse);
	UD_initfrm_intry = save_entry;
	if (Sscreenfrm == -1) goto nofrm;
/*
.....Wait for the user to select a screen
.....or cancel the form
*/
	Sscreenfrm_cancel = 0;
	uw_dispfrm_wait(Sscreenfrm);
	goto done;
/*
.....Could not allocate memory
*/
nofrm:
	ud_wrerr("Could not load 'screensel.frm'.");
	Sscreenfrm_cancel = -1;
	goto done;
/*
.....End of routine
*/
done:
	if (Sscreenfrm_cancel!=-1)
		strcpy(slist.answer, SelScreen);
	return(Sscreenfrm_cancel);
}

static void uv_save_frm_data(indx)
int indx;
{
	int j, defans;
	if (indx<0)
		indx = 0;
	frm_choice[indx*6] = choice[0];
	frm_choice[indx*6+1] = choice[1];
	frm_choice[indx*6+2] = choice[2];
	frm_choice[indx*6+3] = choice[3];
	frm_choice[indx*6+4] = choice[4];
	frm_choice[indx*6+5] = choice[5];

	if (cview_list.num_item>12)
	{
/*
......make sure the answer is within the 12 items
*/	
		defans = 0;
		for (j=0; j<cview_list.num_item; j++)
		{
			if (strcmp(cview_list.answer, cview_list.item[j])==0)
			{
				defans = j;
				break;
			}
		}
		if (defans>=12)
		{
			strcpy(fview_list[indx].item[11], cview_list.answer);
			strcpy(cview_list.item[11], cview_list.answer);
		}
	}
	strcpy(view_list[indx].answer, cview_list.answer);
	strcpy(fview_list[indx].answer, cview_list.answer);
}
/*********************************************************************
**    S_FUNCTION     :  static uv_vport_selectCB(filedno, val, stat)
**       Method called at when "View..." button
**                      is pushed
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT uv_vport_selectCB(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	char vname[20];
	if (val->frmstr!=NULL)
	{
/*
......save the previous data
*/
		if (save_frm_data)
			uv_save_frm_data(save_vindx);
		else
			save_vindx = -1;
		strcpy(vname, val->frmstr);
		for (i=0; i<vport_list.num_item; i++)
		{
			if (strcmp(vname, vport_name[i])==0)
			{
				save_vindx = i;
				cur_sel_vport = i;
				break;
			}
		}
/*
.....update the first display vport and values
*/
		strcpy(cview_list.answer, fview_list[cur_sel_vport].answer);
		if (cview_list.num_item>12)
		{
/*
......make sure the answer is within the 12 items
*/	
			strcpy(cview_list.item[11], fview_list[cur_sel_vport].item[11]);
		}
		strcpy(cview_list.answer, fview_list[save_vindx].answer);
		ud_update_answer(0+srn_frm, (int*)&cview_list);
		ud_update_answer(1+srn_frm, (int*)&frm_choice[save_vindx*6]);
		ud_update_answer(2+srn_frm, (int*)&frm_choice[save_vindx*6+1]);
		ud_update_answer(3+srn_frm, (int*)&frm_choice[save_vindx*6+2]);
		ud_update_answer(4+srn_frm, (int*)&frm_choice[save_vindx*6+3]);
		ud_update_answer(5+srn_frm, (int*)&frm_choice[save_vindx*6+4]);
		ud_update_answer(6+srn_frm, (int*)&frm_choice[save_vindx*6+5]);
		if (srn_frm)
			ud_dispfrm_update_label(0, 0, vname);
		else
			ud_dispfrm_update_label(0, 1, vname);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  static uv_view_selectCB(filedno, val, stat)
**       Method called at when "View..." button
**                      is pushed
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT uv_view_selectCB(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int j, defans, status;
	if (val->frmstr!=NULL)
	{
		if (vport_disp!=1)
		{
			if (*fieldno==0)
				cur_sel_vport = 0;
			else if (*fieldno==7)
				cur_sel_vport = 1;
			else if (*fieldno==14)
				cur_sel_vport = 2;
			else if (*fieldno==21)
				cur_sel_vport = 3;
			else if (*fieldno==28)
				cur_sel_vport = 4;
			else if (*fieldno==35)
				cur_sel_vport = 5;
		}
		if (strcmp(val->frmstr, "More...")!=0)
		{
			strcpy(fview_list[cur_sel_vport].answer, val->frmstr);
			strcpy(view_list[cur_sel_vport].answer, val->frmstr);
			if (cview_list.answer!=NULL)
				strcpy(cview_list.answer, val->frmstr);
		}
		if (strcmp(val->frmstr, "More...")==0)
		{
			status = uv_view_select(view_list[cur_sel_vport]);
			if (status!=-1)
			{
/*
.....if the answer is not within 12 items
*/
				defans = -1;
				for (j=0; j<12; j++)
				{
					if (strcmp(view_list[cur_sel_vport].answer, fview_list[cur_sel_vport].item[j])==0)
					{
						defans = j;
						break;
					}
				}
				if ((defans==-1) && (view_list[cur_sel_vport].answer[0]!='\0'))
				{
					strcpy(fview_list[cur_sel_vport].item[11], view_list[cur_sel_vport].answer);
					if ((cview_list.item!=NULL) && cview_list.item[11]!=NULL)
						strcpy(cview_list.item[11], view_list[cur_sel_vport].answer);
				}
				if (view_list[cur_sel_vport].answer[0]!='\0')
				{
					strcpy(fview_list[cur_sel_vport].answer, view_list[cur_sel_vport].answer);
					if (cview_list.answer!=NULL)
						strcpy(cview_list.answer, view_list[cur_sel_vport].answer);
				}
			}
			if (cview_list.answer!=NULL)
				ud_update_answer(0+srn_frm, (int*)&cview_list);
			else
			{
/*
......if cancel, reset the original pick
*/
				if (status==-1)
					strcpy(fview_list[cur_sel_vport].answer, view_list[cur_sel_vport].answer);
				ud_update_answer(cur_sel_vport*7+srn_frm, (int*)&fview_list[cur_sel_vport]);
			}
		}
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  static uv_screen_selectCB(filedno, val, stat)
**       Method called at when "select screen list"
**                     
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT uv_screen_selectCB(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status,i,j, defans;
	char sname[20];
	UV_screen newscreen;
	UV_vport	vport;
	UV_view	view;

	if (val->frmstr!=NULL)
	{
		if (strcmp(val->frmstr, "More...")!=0)
		{
			strcpy(screen_list.answer, val->frmstr);
			strcpy(fscreen_list.answer, val->frmstr);
		}
		if (strcmp(val->frmstr, "More...")==0)
		{
			if (uv_screen_select(screen_list)!=-1)
			{
/*
.....if the answer is not within 12 items
*/
				defans = -1;
				for (j=0; j<12; j++)
				{
					if (strcmp(screen_list.answer, fscreen_list.item[j])==0)
					{
						defans = j;
						break;
					}
				}
				if ((defans==-1) && (screen_list.answer[0]!='\0'))
				{
					strcpy(fscreen_list.item[11], screen_list.answer);
				}
				if (screen_list.answer[0]!='\0')
					strcpy(fscreen_list.answer, screen_list.answer);
			}
			ud_update_answer(0, (int*)&fscreen_list);
			return(UD_FLDOK);
		}

		strcpy(sname, val->frmstr);
		status = uv_getscnm(sname, &newscreen);
		if (status != UU_SUCCESS)
			return (UD_FSTAT)-1;

		save_frm_data = 0;
		save_vindx = -1;
		ud_free_flist(&vport_list);
		vport_list.item = (char **)uu_malloc(newscreen.nvports*sizeof(char*));
		vport_list.answer = (char*)uu_malloc(81*sizeof(char));
		vport_list.num_item = newscreen.nvports;

		for (i=0;i<newscreen.nvports;i++)
		{
			status = uv_getvpid(newscreen.vports[i], &vport);
			status = uv_getvid(vport.cur_view, &view);
			strcpy(view_list[i].answer, view.name);

			if (fview_list[i].num_item<=12)
				strcpy(fview_list[i].answer, view.name);
			else
			{
/*
......make sure the answer is within the 12 items
*/	
				defans = 0;
				for (j=0; j<view_list[i].num_item; j++)
				{
					if (strcmp(view.name, view_list[i].item[j])==0)
					{
						defans = j;
						break;
					}
				}
				if (defans>=12)
					strcpy(fview_list[i].item[11], view.name);
				strcpy(fview_list[i].answer, view.name);
			}

			vport_list.item[i] = (char*)uu_malloc(20*sizeof(char));
			strcpy(vport_list.item[i], vport.name);
			strcpy(vport_name[i], vport.name);
			frm_choice[6*i] = vport.motion;
			frm_choice[6*i+1] = vport.bord_on;
			frm_choice[6*i+2] = vport.v_axis_on;
			frm_choice[6*i+3] = vport.name_on;
			frm_choice[6*i+4] = vport.aperture_on;
			
			if ((vport.disp_mode==2)&&(vport.wireframe==0))
				frm_choice[i*6+5] = 3;
			else	if ((vport.disp_mode>=1)&&(vport.disp_mode<=3))
				frm_choice[i*6+5] = vport.disp_mode-1;
			else
				frm_choice[i*6+5] = 1;
		}
		strcpy(vport_list.answer, vport_list.item[0]);
/*
.....update vport list
*/
		save_vindx = -1;
		ud_update_answer(8, (int*)&vport_list);
	}
	save_frm_data = 1;
	return(UD_FLDOK);
}

static UD_FSTAT uv_init_scrfrm(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_dispfrm_update_label(0, 0, vport_name[0]);
	return(UD_FLDOK);
}

static UD_FSTAT uv_init_viewfrm(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	char label[80];
	sprintf(label, "Screen: %s", screen_name);
	ud_dispfrm_update_label(0, 0, label);
	for (i=0; i<vport_disp; i++)
		ud_dispfrm_update_label(0, i+1, vport_name[i]);
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : int uvu_screen_format_form(newscreen,
**										draw_vp_border, draw_vp_axis,
**										display_motion,
**										draw_vp_name, draw_vp_aperture,
**										new_vp_cur_view)
**			Display the form associated with the given SCREEN and enable
**			the user to manipulate any of the fields. Check each of the
**			view names for legality, and return UU_FAILURE if any error
**			occurs.
**    PARAMETERS   
**       INPUT  : 
**				newscreen			data defining the new screen
**       OUTPUT :  
**				display_motion		UU_TRUE => display motion in viewport
**				draw_vp_border		UU_TRUE => draw border in corresponding viewport
**				draw_vp_axis		UU_TRUE => draw axis in corresponding viewport
**				draw_vp_name		UU_TRUE => draw name in corresponding viewport
**				draw_vp_aperture	UU_TRUE => draw aperture in corresponding viewport
**				new_vp_cur_view	key of view to place in corresponding viewport
**    RETURNS      : 
**			UU_SUCCESS  iff no user errors in filling form
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
.....added display_mod for shading/wire-frame
.....Yurong 1/13/98
*/
int
uvu_screen_format_form(sc_name, newscreen, display_motion, draw_vp_border, draw_vp_axis,
	draw_vp_name, draw_vp_aperture, display_mod, new_vp_cur_view)
	char *sc_name;
	UV_screen *newscreen;
	UU_LOGICAL display_motion[UV_NVPORTS];
	UU_LOGICAL draw_vp_border[UV_NVPORTS];
	UU_LOGICAL draw_vp_axis[UV_NVPORTS];
	UU_LOGICAL draw_vp_name[UV_NVPORTS];
	UU_LOGICAL draw_vp_aperture[UV_NVPORTS];
	int display_mod[UV_NVPORTS];
	UU_KEY_ID new_vp_cur_view[UV_NVPORTS];
	
	{
	UU_KEY_ID layer;
	UD_METHOD save_entry;
	int	item_num, defans;
	char	form_name[256];				/* name of the form associated with
												new screen */
	char	view_name[UV_NVPORTS][15];			/* view name for this entry */
	int	status;						/* status from get view (port) id */
	int	i, j, len;							/* loop counter */
	UV_vport	vport;					/* view port structure */
	UV_view	view;						/* view structure */
/*
.....added Display mode (shading/wire-frame) for every view
.....Yurong 1/13/98
*/
	int *ans[42];
	static char traverse[43] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
	static UD_METHOD methods[43] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
									0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
									0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
									0, 0, 0, 0, 0, 0, 0, 0, 0, 0};
	static char called[43] = {6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
								6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6};
/*
.....Added motion display flag for viewport record
.....Bobby  -  11/30/92
*/

	uu_denter(UU_MTRC,(us,"uvu_screen_format_form(sc_name = %s)"
						  , newscreen->name));

	save_entry = UD_initfrm_intry;

	save_vindx = -1;
	for (i=0; i<UV_NVPORTS; i++)
	{
		view_list[i].num_item = 0;
		view_list[i].item = NULL;
		view_list[i].answer = NULL;
		fview_list[i].num_item = 0;
		fview_list[i].item = NULL;
		fview_list[i].answer = NULL;
	}
	screen_list.num_item = 0;
	screen_list.item = NULL;
	screen_list.answer = NULL;
	fscreen_list.num_item = 0;
	fscreen_list.item = NULL;
	fscreen_list.answer = NULL;
	vport_list.num_item = 0;
	vport_list.item = NULL;
	vport_list.answer = NULL;
	cview_list.num_item = 0;
	cview_list.item = NULL;
	cview_list.answer = NULL;

	if ((sc_name==NULL) || (sc_name[0] == '\0'))
	{
		strcpy(form_name, "scrselvf.frm");
		vport_disp = 1;
		srn_frm = 1;
	}
	else
	{
		strcpy(screen_name, sc_name);
		status = uv_getscnm(sc_name, newscreen);
		if (status != UU_SUCCESS)
		{
			uu_uerror1(/* illegal screen name */ UM_MODEL, 228, sc_name);
			return -1;
		}
		if (newscreen->nvports==1)
		{
			vport_disp = 1;
			strcpy(form_name, "onevf.frm");
		}
		else if (newscreen->nvports==2)
		{
			vport_disp = 2;
			strcpy(form_name, "twovf.frm");
		}
		else if (newscreen->nvports==3)
		{
			vport_disp = 3;
			strcpy(form_name, "threevf.frm");
		}
		else if (newscreen->nvports==4)
		{
			vport_disp = 4;
			strcpy(form_name, "fourvf.frm");
		}
		else if (newscreen->nvports==5)
		{
			vport_disp = 5;
			strcpy(form_name, "fivevf.frm");
		}
		else if (newscreen->nvports==6)
		{
			vport_disp = 6;
			strcpy(form_name, "sixvf.frm");
		}
		else
		{
			vport_disp = 1;
			strcpy(form_name, "mutivf.frm");
		}
		srn_frm = 0;
	}
	if ((sc_name==NULL) || (sc_name[0] == '\0'))
	{
		screen_list.item = (char**)uv_get_screenname(&(screen_list.num_item));
		if (screen_list.num_item==0)
			return -1;
		ud_list_sort(&screen_list);
		screen_list.answer = (char *) uu_malloc(80 * sizeof(char));
		strcpy(screen_list.answer, screen_list.item[0]);
		for (i=0; i<screen_list.num_item; i++)
		{
			status = uv_getscnm(screen_list.item[i], newscreen);
			if (status != UU_SUCCESS)
				return -1;
			if (newscreen->active == UV_ACTIVE)
			{
				strcpy(screen_list.answer, screen_list.item[i]);
				break;
			}
		}
		if (screen_list.num_item>=13)
			len = 13;
		else
			len = screen_list.num_item;
		fscreen_list.num_item = len;
		fscreen_list.item = (char **) uu_malloc(len *sizeof(char *));
		for (j=0; j<len; j++)
		{
			fscreen_list.item[j] = (char *) uu_malloc(40 * sizeof(char));
			strcpy(fscreen_list.item[j], screen_list.item[j]);
		}
		if (len==13)
		{
			strcpy(fscreen_list.item[12], "More...");
		}
		fscreen_list.answer = (char *) uu_malloc(80 * sizeof(char));
		strcpy(fscreen_list.answer, screen_list.answer);
/*
......make sure the answer is within the 12 items
*/
		defans = 0;
		for (j=0; j<screen_list.num_item; j++)
		{
			if (strcmp(screen_list.answer, screen_list.item[j])==0)
			{
				defans = j;
				break;
			}
		}
		if (defans>=12)
			strcpy(fscreen_list.item[11], screen_list.answer);	
	}
	if ((sc_name==NULL) || (sc_name[0] == '\0') || (newscreen->nvports>6))
	{
		vport_list.item = (char **)uu_malloc(newscreen->nvports*sizeof(char*));
		vport_list.answer = (char*)uu_malloc(81*sizeof(char));
		vport_list.answer[0] = '\0';
		vport_list.num_item = newscreen->nvports;
		for (i=0;i<newscreen->nvports;i++)
		{
			status = uv_getvpid(newscreen->vports[i], &vport);
			vport_list.item[i] = (char*)uu_malloc(20*sizeof(char));
			strcpy(vport_list.item[i], vport.name);
		}
		strcpy(vport_list.answer, vport_list.item[0]);
	}
/*
.....Command Reject
*/
	UD_MARK (mainmarkval,UU_FALSE);
	if (mainmarkval != 0)
	{
		for (i=0; i<UV_NVPORTS; i++)
		{
			ud_free_flist(&(view_list[i]));
			ud_free_flist(&(fview_list[i]));
		}
		if (strcmp(form_name, "scrselvf.frm")==0)
		{
			ud_free_flist(&screen_list);
			ud_free_flist(&fscreen_list);
			ud_free_flist(&vport_list);
		}
		UD_initfrm_intry = save_entry;
		UD_UNMARK (mainmarkval);
		return -1;
	}
	for (i=0; i<UV_NVPORTS; i++)
	{
		view_list[i].item = (char**)uv_get_viewname(&(view_list[i].num_item), 1);
		if (view_list[i].num_item==0)
			return -1;
		view_list[i].answer = (char *) uu_malloc(80 * sizeof(char));
/*
.....initialized answer to a null character to avoid UMR in purify
*/
		view_list[i].answer[0] = '\0';
/*
.....Added "layer list" as end of the view
*/		
		uv_add_layer_list(&(view_list[i]));	
		if (view_list[i].num_item>=13)
			len = 13;
		else
			len = view_list[i].num_item;
		fview_list[i].num_item = len;
		fview_list[i].item = (char **) uu_malloc(len *sizeof(char *));
		for (j=0; j<len; j++)
		{
			fview_list[i].item[j] = (char *) uu_malloc(40 * sizeof(char));
			strcpy(fview_list[i].item[j], view_list[i].item[j]);
		}
		if (len==13)
		{
			strcpy(fview_list[i].item[12], "More...");
		}
		fview_list[i].answer = (char *) uu_malloc(80 * sizeof(char));
/*
.....initialized answer to a null character to avoid UMR in purify
*/
		fview_list[i].answer[0] = '\0';
	}
	item_num = 0;
	if (strcmp(form_name, "scrselvf.frm")==0)
	{
		ans[item_num++] = (int*)&fscreen_list;
		methods[0] = uv_screen_selectCB;
	}

	for (i = 0; i < newscreen->nvports; i++)
	{
		status = uv_getvpid(newscreen->vports[i], &vport);
		status = uv_getvid(vport.cur_view, &view);
		strcpy(view_list[i].answer, view.name);
		if (fview_list[i].num_item<=12)
			strcpy(fview_list[i].answer, view.name);
		else
		{
/*
......make sure the answer is within the 12 items
*/
			defans = 0;
			for (j=0; j<view_list[i].num_item; j++)
			{
				if (strcmp(view.name, view_list[i].item[j])==0)
				{
					defans = j;
					break;
				}
			}
			if (defans>=12)
				strcpy(fview_list[i].item[11], view.name);
			strcpy(fview_list[i].answer, view.name);
		}
		strcpy(vport_name[i], vport.name);

		if (i==0)
			nclu_update_pre_view(view);

		if (i<vport_disp)
			choice[i*6] = vport.motion;
		frm_choice[6*i] = vport.motion;

		if (i<vport_disp)
			choice[i*6+1] = vport.bord_on;
		frm_choice[6*i+1] = vport.bord_on;

		if (i<vport_disp)
			choice[i*6+2] = vport.v_axis_on;
		frm_choice[6*i+2] = vport.v_axis_on;

		if (i<vport_disp)
			choice[i*6+3] = vport.name_on;
		frm_choice[6*i+3] = vport.name_on;

		if (i<vport_disp)
			choice[i*6+4] = vport.aperture_on;
		frm_choice[6*i+4] = vport.aperture_on;
			
		if ((vport.disp_mode==2)&&(vport.wireframe==0))
			frm_choice[i*6+5] = 3;
		else	if ((vport.disp_mode>=1)&&(vport.disp_mode<=3))
			frm_choice[i*6+5] = vport.disp_mode-1;
		else
			frm_choice[i*6+5] = 1;
		if (i<vport_disp)
			choice[6*i+5] = frm_choice[i*6+5];
	}
	for (i = 0; i < vport_disp; i++)
	{
		if ((strcmp(form_name, "scrselvf.frm")==0)
			|| (strcmp(form_name, "mutivf.frm")==0))
		{
			cview_list.num_item = fview_list[i].num_item;
			cview_list.item = (char **) uu_malloc((cview_list.num_item) *sizeof(char *));
			for (j=0; j<cview_list.num_item; j++)
			{
				cview_list.item[j] = (char *) uu_malloc(40 * sizeof(char));
				strcpy(cview_list.item[j], fview_list[0].item[j]);
			}
			cview_list.answer = (char *) uu_malloc(80 * sizeof(char));
			strcpy(cview_list.answer, fview_list[0].answer);
			ans[item_num] = (int *) &cview_list;
		}
		else
			ans[item_num] = (int *) &(fview_list[i]);
		methods[item_num] = uv_view_selectCB;
		item_num++;

		ans[item_num] = (int *) &(choice[i*6]);
		item_num++;

		ans[item_num] = (int *) & (choice[i*6+1]);
		item_num++;

		ans[item_num] = (int *) &(choice[i*6+2]);
		item_num++;

		ans[item_num] = (int *) &(choice[i*6+3]);
		item_num++;

		ans[item_num] = (int *) &(choice[i*6+4]);
		item_num++;

		ans[item_num] = (int *) &(choice[i*6+5]);
		item_num++;
	}
	save_entry = UD_initfrm_intry;
	if (strcmp(form_name, "scrselvf.frm")==0)
	{
		ans[item_num] = (int*)&vport_list;
		methods[item_num] = uv_vport_selectCB;
		UD_initfrm_intry = uv_init_scrfrm;
	}
	else if (strcmp(form_name, "mutivf.frm")==0)
	{
		ans[item_num] = (int*)&vport_list;
		methods[item_num] = uv_vport_selectCB;
		UD_initfrm_intry = uv_init_viewfrm;
	}
	else
		UD_initfrm_intry = uv_init_viewfrm;

	cur_sel_vport = 0;
	status = ud_form1(form_name, ans, ans, methods, called, NULL,traverse);	
	UD_initfrm_intry = save_entry;
	if (status==-1)
	{
		for (i=0; i<UV_NVPORTS; i++)
		{
			ud_free_flist(&(view_list[i]));
			ud_free_flist(&(fview_list[i]));
		}
		if (strcmp(form_name, "scrselvf.frm")==0)
		{
			ud_free_flist(&screen_list);
			ud_free_flist(&fscreen_list);
			ud_free_flist(&vport_list);
		}
		ud_free_flist(&cview_list);
		UD_UNMARK(mainmarkval);
		return -1;
	}
	if (strcmp(form_name, "scrselvf.frm")==0)
	{
		status = uv_getscnm(fscreen_list.answer, newscreen);
		if (status != UU_SUCCESS)
			goto failure;
		frm_choice[6*cur_sel_vport] = choice[0];
		frm_choice[6*cur_sel_vport+1] = choice[1];
		frm_choice[6*cur_sel_vport+2] = choice[2];
		frm_choice[6*cur_sel_vport+3] = choice[3];
		frm_choice[6*cur_sel_vport+4] = choice[4];
		frm_choice[6*cur_sel_vport+5] = choice[5];
		ud_free_flist(&screen_list);
		ud_free_flist(&fscreen_list);
		ud_free_flist(&vport_list);
		strcpy(fview_list[cur_sel_vport].answer, cview_list.answer);
		ud_free_flist(&cview_list);
	}
	else
	{
		if (strcmp(form_name, "mutivf.frm")==0)
		{
			frm_choice[6*cur_sel_vport] = choice[0];
			frm_choice[6*cur_sel_vport+1] = choice[1];
			frm_choice[6*cur_sel_vport+2] = choice[2];
			frm_choice[6*cur_sel_vport+3] = choice[3];
			frm_choice[6*cur_sel_vport+4] = choice[4];
			frm_choice[6*cur_sel_vport+5] = choice[5];
			strcpy(fview_list[cur_sel_vport].answer, cview_list.answer);
			ud_free_flist(&cview_list);
		}
		else
		{
			for (i = 0; i < vport_disp; i++)
			{
				frm_choice[6*i] = choice[6*i];
				frm_choice[6*i+1] = choice[6*i+1];
				frm_choice[6*i+2] = choice[6*i+2];
				frm_choice[6*i+3] = choice[6*i+3];
				frm_choice[6*i+4] = choice[6*i+4];
				frm_choice[6*i+5] = choice[6*i+5];
			}
		}
	}
	for (i = 0; i < newscreen->nvports; i++)
	{
		strcpy(view_name[i], fview_list[i].answer);
		ud_free_flist(&(view_list[i]));
		ud_free_flist(&(fview_list[i]));
	}
	for (i = 0; i < newscreen->nvports; i++)
	{
		status = uv_getvnm(view_name[i], &view);
		if (status == UU_FAILURE) 
		{
/*
.....all the view should already exist except layer view
.....so if this layer view not exist, create it
*/
			uv_special_view(&layer, view_name[i], 2);
			status = uv_getvnm(view_name[i], &view);
		}
/*
.....added Disp_mod for every view
.....Yurong 1/13/98
*/
		display_motion[i] = frm_choice[(i*6) + 0];
		draw_vp_border[i] = frm_choice[(i*6) + 1];
		draw_vp_axis[i] = frm_choice[(i*6) + 2];
		draw_vp_name[i] = frm_choice[(i*6) + 3];
		draw_vp_aperture[i] = frm_choice[(i*6) + 4];
		display_mod[i] = frm_choice[(i*6) + 5];
		new_vp_cur_view[i] = view.key;
		}

	status = UU_SUCCESS;
failure:
	UD_UNMARK(mainmarkval);
	return (status);
}
/*********************************************************************
**    E_FUNCTION     : uvu_select_screen_format(sc_name)
**			Given the name of the screen (SC_NAME) to make active,
**			display the form defining the views and other display
**			parameters (e.g. view name, aperture etc.) for that 
**			screen and let the user modify them. Finally, display the
**			screen format.
**    PARAMETERS   
**       INPUT  : 
**				sc_name			name of screen to make active
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uvu_select_screen_format(sc_name)
	char	*sc_name;

	{
	int	status;		
	UV_screen	newscreen;
	UU_LOGICAL display_motion[UV_NVPORTS];
	UU_LOGICAL draw_vp_border[UV_NVPORTS];
	UU_LOGICAL draw_vp_axis[UV_NVPORTS];
	UU_LOGICAL draw_vp_name[UV_NVPORTS];
	UU_LOGICAL draw_vp_aperture[UV_NVPORTS];
	int display_mod[UV_NVPORTS];
	UU_KEY_ID new_vp_cur_view[UV_NVPORTS];


	uu_denter(UU_MTRC,(us,"uvu_select_screen_format(sc_name = %s)",sc_name));

	status = uvu_screen_format_form(sc_name, &newscreen, display_motion,
			draw_vp_border,
			draw_vp_axis, draw_vp_name, draw_vp_aperture, 
			display_mod, new_vp_cur_view);
	if (status == UU_SUCCESS) 
	{
		uv_swap_screen(&newscreen, display_motion, draw_vp_border,
			draw_vp_axis,
			draw_vp_name, draw_vp_aperture, 
			display_mod, new_vp_cur_view);
	}
	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : uvu_view_chk(view_name,view_key)      
**          Given the name of the view (view_name) this routine   
**          returns the view key if this view exists or 0 if it does not.
**    PARAMETERS
**       INPUT  :
**              sc_name         name of screen to to check   
**       OUTPUT :
**              view_key / 0
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uvu_view_chk(view_name,view_key)
UM_f77_str_ptr view_name;
/***char view_name[20];***/
int *view_key;
{
char *p;
UV_view view;
int status;

p = UM_cstr_of_f77_str(view_name);
/***status = uv_getvnm1(view_name, &view);***/
status = uv_getvnm1(p, &view);
if (status == UU_FAILURE)
  *view_key = 0;
else
  *view_key = view.key;
}

/*********************************************************************
**    E_FUNCTION     : uvu_select_screen_format1(n, new_vp_cur_view,
**                        draw_vp_border, draw_vp_axis, draw_vp_name,
**                        draw_vp_aperture, display_motion)
**          Given the name of the screen number (n) to make active, 
**          display the new  screen format.
**    PARAMETERS   
**       INPUT  : 
**              sc_name           -  number of the screen to make active
**              new_vp_cur_view   -  pointer to the list of the views
**                                   to associate with this screen
**              draw_vp_border    -  pointer to the list of the "border"
**                                   parameters for the specified views
**              draw_vp_axis      -  pointer to the list of the "axis"  
**                                   parameters for the specified views
**              draw_vp_name      -  pointer to the list of the "name"  
**                                   parameters for the specified views
**              draw_vp_aperture  -  pointer to the list of the "size"  
**                                   parameters for the specified views
**              display_motion    -  pointer to the list of the "motion"
**                                   parameters for the specified views
**       NOTE:
**          The values of the border,axis,name,size and motion are the
**          same for all views in the viewlist but I used these arrays
**          to pass all arguments because in the lower level routine
**          (uv_swap_screen) this sructure is already used.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*
....added disp_mode
.....wire_frame=0, shading=1, hiden line = 2, shade only = 3
....Yurong 4/15/99
*/
void uvu_select_screen_format1(sc_name,new_vp_cur_view, fborder, draw_vp_border,
                          faxis, draw_vp_axis, fname, draw_vp_name, fsize, draw_vp_aperture,
                          fmotion, display_motion, fmode, display_mod)
	char *sc_name;
    UU_LOGICAL display_motion[UV_NVPORTS];
    UU_LOGICAL draw_vp_border[UV_NVPORTS];
    UU_LOGICAL draw_vp_axis[UV_NVPORTS];
    UU_LOGICAL draw_vp_name[UV_NVPORTS];
    UU_LOGICAL draw_vp_aperture[UV_NVPORTS];
    UU_KEY_ID  new_vp_cur_view[UV_NVPORTS];
	int display_mod[UV_NVPORTS];
	int *fborder, *faxis, *fname, *fsize, *fmotion, *fmode;
    {
    int i, status;         
    UV_screen   newscreen;
	UV_vport	vport;

    status = uv_getscnm(sc_name, &newscreen);
    if (status != UU_SUCCESS)
    {
        uu_uerror1(/* illegal screen name */ UM_MODEL, 228, sc_name);
    }
    else 
    {
		for (i=0; i<newscreen.nvports;i++)
		{
/*
......if the value not asked for change, use the default one
*/			
			status = uv_getvpid(newscreen.vports[i], &vport);
			if (*fborder==0)
			{
				draw_vp_border[i] = vport.bord_on;
			}
			if (*fmotion==0)
			{
				display_motion[i] = vport.motion;
			}
			if (*faxis==0)
			{
				draw_vp_axis[i] = vport.v_axis_on;
			}
			if (*fname==0)
			{
				draw_vp_name[i] = vport.name_on;
			}
			if (*fsize==0)
			{
				draw_vp_aperture[i] = vport.aperture_on;
			}
			if (*fmode==0)
			{
/*
.....vport.disp_mode = 2 and wireframe = 0, then display_mod[i] = 3 (SHADE ONLY)
.....vport.disp_mode = 2 and wireframe = 1, then display_mod[i] = 1 (SHADED)
*/
				if ((vport.disp_mode==2) && (vport.wireframe))
/*
.....shaded
*/
					display_mod[i] = 1;
				else if ((vport.disp_mode==2) && (vport.wireframe==0))
/*
.....shaded only
*/
					display_mod[i] = 3;
				else
					display_mod[i] = vport.disp_mode;
			}
		}
      uv_swap_screen(&newscreen, display_motion, draw_vp_border,
                       draw_vp_axis,
                       draw_vp_name, draw_vp_aperture, 
								display_mod, new_vp_cur_view);
    }
    uu_dexit;
}  

void uv_screen_chk(in_screenname, out_screenname, iviewnum)
UM_f77_str_ptr in_screenname, out_screenname;
int *iviewnum;
{
    int status;         
    UV_screen   newscreen;
	char *innam,*outnam;

	innam = UM_cstr_of_f77_str(in_screenname);
	outnam = UM_cstr_of_f77_str(out_screenname);
	
	status = uv_getscnm1(innam, &newscreen);
    if (status != UU_SUCCESS)
    {
        uu_uerror1(/* illegal screen name */ UM_MODEL, 228, innam);
		*iviewnum = 0;
    }
	*iviewnum = newscreen.nvports;
	strcpy(outnam, newscreen.name);
}

