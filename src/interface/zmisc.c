/*********************************************************************
**
**    NAME         :  zmisc.c
**
**       CONTAINS:
**				uz_iface_modals
**				uz_pos_modals()
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       zmisc.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 10:19:32
**
*********************************************************************/
#include "usysdef.h"
#include "lcom.h"
#include "lumb.h"
#include <stdio.h>
#include "lipv.h"
#include "dmotif.h"
#include "wsgl.h"

/* This defines these for the world */
int (*UD_savefkb)(), (*UD_savekb)(), (*UD_savemouse)(); 
extern int NCL_clrs_changed;
/*********************************************************************
**    E_FUNCTION     : S_save_modfile
**       Save the interface properties into modals file.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_modfile()
{
	int stat;
	char msg[80];
	UX_pathname fname;
	FILE *fptr;
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	if (LW_nclipv!=LW_STANDALONE)
		strcpy(fname,"ncl_interface.mod");
	else
		strcpy(fname,"nclipv_interface.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,fname,
		3,&fptr);
	if (stat != UU_SUCCESS || fptr == UU_NULL) goto done;
/*
.....Store playback modals
*/
	ux_fputs0("#INTERFACE#\n", fptr);
/*
.....remove menu format from here
*/
/*
	if (UW_menu_fmt==0)
		sprintf(msg,"/MENU_FORMAT/ *ICON\n");
	else if (UW_menu_fmt==1)
		sprintf(msg,"/MENU_FORMAT/ *TEXT\n");
	else
		sprintf(msg,"/MENU_FORMAT/ *BOTH\n");
	ux_fputs0(msg, fptr);
*/
	if (UW_auto_cursor==0)
		sprintf(msg,"/AUTO_CURSOR/ *OFF\n");
	else
		sprintf(msg,"/AUTO_CURSOR/ *ON\n");
	ux_fputs0(msg, fptr);

	if (UW_icon_size==0)
		sprintf(msg,"/ICON_SIZE/ *16\n");
	else if (UW_icon_size==1)
		sprintf(msg,"/ICON_SIZE/ *24\n");
	else if (UW_icon_size==2)
		sprintf(msg,"/ICON_SIZE/ *32\n");
	else if (UW_icon_size==3)
		sprintf(msg,"/ICON_SIZE/ *40\n");
	else
		sprintf(msg,"/ICON_SIZE/ *48\n");
	ux_fputs0(msg, fptr);

	if (UW_browse_dir == 0)
		sprintf(msg,"/BROWSER/ *LOCAL\n");
	else
		sprintf(msg,"/BROWSER/ *SAVED\n");
	ux_fputs0(msg, fptr);

	sprintf(msg,"/TEXT_FONT/ %s\n", UW_com_font);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/TEXT_SIZE/ *%d\n", UW_com_size);
	ux_fputs0(msg, fptr);

	if (UW_text_cursor==0)
		sprintf(msg,"/TEXT_CURSOR/ *BEGIN\n");
	else
		sprintf(msg,"/TEXT_CURSOR/ *END\n");
	ux_fputs0(msg, fptr);

	if (UW_frmtext_select==0)
		sprintf(msg,"/FORM_HIGHLIGHT/ *OFF\n");
	else
		sprintf(msg,"/FORM_HIGHLIGHT/ *ON\n");
	ux_fputs0(msg, fptr);

	if (UW_text_select==0)
		sprintf(msg,"/TEXT_HIGHLIGHT/ *OFF\n");
	else
		sprintf(msg,"/TEXT_HIGHLIGHT/ *ON\n");
	ux_fputs0(msg, fptr);

	if (UW_keypad==0)
		sprintf(msg,"/COM_KEYPAD/ *NUMERIC\n");
	else
		sprintf(msg,"/COM_KEYPAD/ *FUNCTION\n");
	ux_fputs0(msg, fptr);

	sprintf(msg,"/FORM_HELP_FONT/ %s\n", UW_form_font);
	ux_fputs0(msg, fptr);
	
	sprintf(msg,"/FORM_HELP_SIZE/ *%d\n", UW_form_helpsize);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/FORM_SIZE/ *%d\n", UW_form_fontsize);
	ux_fputs0(msg, fptr);

	if (UW_picture_pos==0)
		sprintf(msg,"/FORM_PICTURE_POS/ *OFF\n");
	else
		sprintf(msg,"/FORM_PICTURE_POS/ *ON\n");
	ux_fputs0(msg, fptr);
	
	sprintf(msg,"/STATUS_TEXT_FONT/ %s\n", UW_status_font);
	ux_fputs0(msg, fptr);
	
	sprintf(msg,"/STATUS_TEXT_SIZE/ *%d\n", UW_status_fontsize);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/PLABEL_FONT/ %s\n", UW_prmpt_font);
	ux_fputs0(msg, fptr);
	
	sprintf(msg,"/PLABEL_SIZE/ *%d\n", UW_prmpt_size);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/ELABEL_FONT/ %s\n", UW_error_font);
	ux_fputs0(msg, fptr);
	
	sprintf(msg,"/ELABEL_SIZE/ *%d\n", UW_error_size);
	ux_fputs0(msg, fptr);
	if (LW_nclipv!=LW_STANDALONE)
	{
		if (UW_live_mouse==0)
			sprintf(msg,"/LIVE_MOUSE/ *OFF\n");
		else
			sprintf(msg,"/LIVE_MOUSE/ *ON\n");
		ux_fputs0(msg, fptr);

		sprintf(msg,"/ACTIVE_LINE/ *%s\n", uw_color_name[(UDM_layout.command_clr[0])]);
		ux_fputs0(msg, fptr);
		sprintf(msg,"/CURRENT_LINE/ *%s\n", uw_color_name[(UDM_layout.command_clr[1])]);
		ux_fputs0(msg, fptr);

		if (UW_stat_mode==0)
			sprintf(msg,"/STATUS_BUTTON/ *CHANGED\n");
		else
			sprintf(msg,"/STATUS_BUTTON/ *IDLE\n");
		ux_fputs0(msg, fptr);
	}
/*
.....Close modals file
*/
	ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
	return(stat);
}

/**************************************************************************
**
**  E_FUNCTION         :  uz_iface_modals()
**     Process the interface modals form.
**  PARAMETERS
**      INPUT  :
**          none
**      OUTPUT :
**          none
**
**  RETURNS      :  
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
uz_iface_modals()
{
#if UU_COMP == UU_WIN2K
	int upt_com, upt_status, upt_error, upt_prmpt;
#endif
	int *ans[23],status, old_size, old_fmt, keypad, browsop;
	int autoc[9], textc[6], highl[5], menufmt, iconsize, formhl, formpic,
		old_comsize, old_formsize, old_helpsize, old_statussize, old_prmpt_size, old_error_size,
		comsize, formsize, helpsize, statussize, prmpt_size, error_size, live_mouse,
		curr_color, act_color, status_mode;
	char formfont[20], 	comfont[20], statusfont[20], errorfont[20],
			prmptfont[20];
/*	static char traverse[] = { 1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0};
	static char display[] = { 1,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0};
	*/
	static char traverse[] = {1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1};
	static char display[] = { 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1,1,1, 1,1,1};
/*
...Set default values
*/
	autoc[0] = UW_auto_cursor;
	textc[0] = UW_text_cursor;
	highl[0] = UW_text_select;
	formhl = UW_frmtext_select;
	formpic = UW_picture_pos;

/*	menufmt = UW_menu_fmt; */
	iconsize = UW_icon_size;
	old_size = UW_icon_size;
	old_fmt = UW_menu_fmt; 
	browsop = UW_browse_dir;
	keypad = UW_keypad;
	live_mouse = UW_live_mouse;

	act_color = UDM_layout.command_clr[0];
	curr_color = UDM_layout.command_clr[1];

	if (UW_com_size==8)
		old_comsize = 0;
	else if (UW_com_size==10)
		old_comsize = 1;
	else if (UW_com_size==12)
		old_comsize = 2;
	else if (UW_com_size==14)
		old_comsize = 3;
	else if (UW_com_size==16)
		old_comsize = 4;
	else if (UW_com_size==18)
		old_comsize = 5;
	else if (UW_com_size==20)
		old_comsize = 6;
	else if (UW_com_size==24)
		old_comsize = 7;
	else if (UW_com_size==36)
		old_comsize = 8;
	else if (UW_com_size==48)
		old_comsize = 9;
	else
		old_comsize = 0;
	comsize = old_comsize;
	if (UW_form_fontsize==8)
		old_formsize = 0;
	else if (UW_form_fontsize==10)
		old_formsize = 1;
	else if (UW_form_fontsize==12)
		old_formsize = 2;
	else if (UW_form_fontsize==14)
		old_formsize = 3;
	else if (UW_form_fontsize==16)
		old_formsize = 4;
	else if (UW_form_fontsize==18)
		old_formsize = 5;
/*	
....
.... restrict form size not to exceed 18
....
    else if (UW_form_fontsize==20)
		old_formsize = 6;
	else if (UW_form_fontsize==24)
		old_formsize = 7;
	else if (UW_form_fontsize==36)
		old_formsize = 8;
	else if (UW_form_fontsize==48)
		old_formsize = 9;
*/
	else
		old_formsize = 0;
	formsize = old_formsize;

	if (UW_form_helpsize==8)
		old_helpsize = 0;
	else if (UW_form_helpsize==10)
		old_helpsize = 1;
	else if (UW_form_helpsize==12)
		old_helpsize = 2;
	else if (UW_form_helpsize==14)
		old_helpsize = 3;
	else if (UW_form_helpsize==16)
		old_helpsize = 4;
	else if (UW_form_helpsize==18)
		old_helpsize = 5;
	else if (UW_form_helpsize==20)
		old_helpsize = 6;
	else if (UW_form_helpsize==24)
		old_helpsize = 7;
	else if (UW_form_helpsize==36)
		old_helpsize = 8;
	else if (UW_form_helpsize==48)
		old_helpsize = 9;
	else
		old_helpsize = 0;
	helpsize = old_helpsize;

	strcpy(formfont, UW_form_font);
	strcpy(comfont, UW_com_font);

	if (UW_status_fontsize==8)
		old_statussize = 0;
	else if (UW_status_fontsize==10)
		old_statussize = 1;
	else if (UW_status_fontsize==12)
		old_statussize = 2;
	else if (UW_status_fontsize==14)
		old_statussize = 3;
	else if (UW_status_fontsize==16)
		old_statussize = 4;
	else if (UW_status_fontsize==18)
		old_statussize = 5;
	else if (UW_status_fontsize==20)
		old_statussize = 6;
	else if (UW_status_fontsize==24)
		old_statussize = 7;
	else if (UW_status_fontsize==36)
		old_statussize = 8;
	else if (UW_status_fontsize==48)
		old_statussize = 9;
	else
		old_statussize = 0;
	strcpy(statusfont, UW_status_font);
	statussize = old_statussize;
	if (UW_prmpt_size==8)
		old_prmpt_size = 0;
	else if (UW_prmpt_size==10)
		old_prmpt_size = 1;
	else if (UW_prmpt_size==12)
		old_prmpt_size = 2;
	else if (UW_prmpt_size==14)
		old_prmpt_size = 3;
	else if (UW_prmpt_size==16)
		old_prmpt_size = 4;
	else if (UW_prmpt_size==18)
		old_prmpt_size = 5;
	else if (UW_prmpt_size==20)
		old_prmpt_size = 6;
	else if (UW_prmpt_size==24)
		old_prmpt_size = 7;
	else if (UW_prmpt_size==36)
		old_prmpt_size = 8;
	else if (UW_prmpt_size==48)
		old_prmpt_size = 9;
	else
		old_prmpt_size = 0;
	prmpt_size = old_prmpt_size;
	strcpy(prmptfont, UW_prmpt_font);

	if (UW_error_size==8)
		old_error_size = 0;
	else if (UW_error_size==10)
		old_error_size = 1;
	else if (UW_error_size==12)
		old_error_size = 2;
	else if (UW_error_size==14)
		old_error_size = 3;
	else if (UW_error_size==16)
		old_error_size = 4;
	else if (UW_error_size==18)
		old_error_size = 5;
	else if (UW_error_size==20)
		old_error_size = 6;
	else if (UW_error_size==24)
		old_error_size = 7;
	else if (UW_error_size==36)
		old_error_size = 8;
	else if (UW_error_size==48)
		old_error_size = 9;
	else
		old_error_size = 0;
	error_size = old_error_size;
	strcpy(errorfont, UW_error_font);
	status_mode = UW_stat_mode;
#if UU_COMP!=UU_WIN2K
	ans[0] = (int *)autoc;
	ans[1] = (int *)textc;
	ans[2] = (int *)highl;
	ans[3] = (int *)&keypad;
/*
...Get input from user
*/
	status = ud_form1("ifaceunx.frm", ans, ans,UU_NULL,UU_NULL,display,traverse);
#else
	ans[0] = (int *)autoc;
/*	ans[1] = (int *)&menufmt; */
	ans[1] = (int *)&iconsize;
	ans[2] = (int *)&browsop;
	ans[3] = (int *)textc;
	ans[4] = (int *)highl;
	ans[5] = (int *)&keypad;
	ans[6] = (int *)comfont;
	ans[7] = (int *)&comsize;
	if (LW_nclipv!=LW_STANDALONE)
	{
		ans[8] = (int *)&act_color;
		ans[9] = (int *)&curr_color;

		ans[10] = (int *)&formhl;
		ans[11] = (int *)formfont;
		ans[12] = (int *)&formsize;
		ans[13] = (int *)&helpsize;
		ans[14] = (int *)&formpic;

		ans[15] = (int *)statusfont;
		ans[16] = (int *)&statussize;
		ans[17] = (int *)prmptfont;
		ans[18] = (int *)&prmpt_size;
		ans[19] = (int *)errorfont;
		ans[20] = (int *)&error_size;
		ans[21] = (int *)&live_mouse;
		ans[22] = (int *)&status_mode;
		status = ud_form1("iface.frm", ans, ans,UU_NULL,UU_NULL,display,traverse);
	}
	else
	{
		ans[8] = (int *)&formhl;
		ans[9] = (int *)formfont;
		ans[10] = (int *)&formsize;
		ans[11] = (int *)&helpsize;
		ans[12] = (int *)&formpic;

		ans[13] = (int *)prmptfont;
		ans[14] = (int *)&prmpt_size;
		ans[15] = (int *)errorfont;
		ans[16] = (int *)&error_size;
		status = ud_form1("ifaceipv.frm", ans, ans,UU_NULL,UU_NULL,display,traverse);
	}
#endif
	if (status == -1) return (status);

#if UU_COMP == UU_WIN2K
	upt_com = 0;
	if ((old_comsize != comsize) || 
		((strcmp(comfont, UW_com_font)!=0) && (comfont[0]!='\0')))
		upt_com = 1;
	if (comfont[0]!='\0')
		strcpy(UW_com_font, comfont);
	if (old_comsize != comsize)
	{
		if (comsize==0)
			UW_com_size = 8;
		else if (comsize==1)
			UW_com_size = 10;
		else if (comsize==2)
			UW_com_size = 12;
		else if (comsize==3)
			UW_com_size = 14;
		else if (comsize==4)
			UW_com_size = 16;
		else if (comsize==5)
			UW_com_size = 18;
		else if (comsize==6)
			UW_com_size = 20;
		else if (comsize==7)
			UW_com_size = 24;
		else if (comsize==8)
			UW_com_size = 36;
		else if (comsize==9)
			UW_com_size = 48;
	}
	if (upt_com)
	{
		uw_ntupd_comfont();
	}
	if (LW_nclipv!=LW_STANDALONE)
	{
		if ((act_color != UDM_layout.command_clr[0])|| (curr_color != UDM_layout.command_clr[1]))
		{
			UDM_layout.command_clr[0] = act_color;
			UDM_layout.command_clr[1] = curr_color;
		}
	}
	if (formsize==0)
		UW_form_fontsize = 8;
	else if (formsize==1)
		UW_form_fontsize = 10;
	else if (formsize==2)
		UW_form_fontsize = 12;
	else if (formsize==3)
		UW_form_fontsize = 14;
	else if (formsize==4)
		UW_form_fontsize = 16;
	else if (formsize==5)
		UW_form_fontsize = 18;
/*	
....
.... restrict form size not to exceed 18
....
    else if (formsize==6)
		UW_form_fontsize = 20;
	else if (formsize==7)
		UW_form_fontsize = 24;
	else if (formsize==8)
		UW_form_fontsize = 36;
	else if (formsize==9)
		UW_form_fontsize = 48;
*/
	if (helpsize==0)
		UW_form_helpsize = 8;
	else if (helpsize==1)
		UW_form_helpsize = 10;
	else if (helpsize==2)
		UW_form_helpsize = 12;
	else if (helpsize==3)
		UW_form_helpsize = 14;
	else if (helpsize==4)
		UW_form_helpsize = 16;
	else if (helpsize==5)
		UW_form_helpsize = 18;
	else if (helpsize==6)
		UW_form_helpsize = 20;
	else if (helpsize==7)
		UW_form_helpsize = 24;
	else if (helpsize==8)
		UW_form_helpsize = 36;
	else if (helpsize==9)
		UW_form_helpsize = 48;

	if (formfont[0]!='\0')
		strcpy(UW_form_font, formfont);
	UW_picture_pos = formpic;

	upt_status = 0;
	if ((old_statussize != statussize) || 
		((strcmp(statusfont, UW_status_font)!=0) && (statusfont[0]!='\0')))
		upt_status = 1;
	if (statusfont[0]!='\0')
		strcpy(UW_status_font, statusfont);
	if (old_statussize != statussize)
	{
		if (statussize==0)
			UW_status_fontsize = 8;
		else if (statussize==1)
			UW_status_fontsize = 10;
		else if (statussize==2)
			UW_status_fontsize = 12;
		else if (statussize==3)
			UW_status_fontsize = 14;
		else if (statussize==4)
			UW_status_fontsize = 16;
		else if (statussize==5)
			UW_status_fontsize = 18;
		else if (statussize==6)
			UW_status_fontsize = 20;
		else if (statussize==7)
			UW_status_fontsize = 24;
		else if (statussize==8)
			UW_status_fontsize = 36;
		else if (statussize==9)
			UW_status_fontsize = 48;
	}
	if (upt_status)
	{
		uw_ntupd_statusfont();
	}
	upt_prmpt = 0;
	if ((old_prmpt_size != prmpt_size) || 
		((strcmp(prmptfont, UW_prmpt_font)!=0) && (prmptfont[0]!='\0')))
		upt_prmpt = 1;
	if (prmptfont[0]!='\0')
		strcpy(UW_prmpt_font, prmptfont);
	if (UW_prmpt_size != prmpt_size)
	{
		if (prmpt_size==0)
			UW_prmpt_size = 8;
		else if (prmpt_size==1)
			UW_prmpt_size = 10;
		else if (prmpt_size==2)
			UW_prmpt_size = 12;
		else if (prmpt_size==3)
			UW_prmpt_size = 14;
		else if (prmpt_size==4)
			UW_prmpt_size = 16;
		else if (prmpt_size==5)
			UW_prmpt_size = 18;
		else if (prmpt_size==6)
			UW_prmpt_size = 20;
		else if (prmpt_size==7)
			UW_prmpt_size = 24;
		else if (prmpt_size==8)
			UW_prmpt_size = 36;
		else if (prmpt_size==9)
			UW_prmpt_size = 48;
	}
	if (upt_prmpt)
	{
		uw_ntupd_prmptfont();
	}

	upt_error = 0;
	if ((old_error_size != error_size) || 
		((strcmp(errorfont, UW_error_font)!=0) && (errorfont[0]!='\0')))
		upt_error = 1;
	if (errorfont[0]!='\0')
		strcpy(UW_error_font, errorfont);
	if (old_error_size != error_size)
	{
		if (error_size==0)
			UW_error_size = 8;
		else if (error_size==1)
			UW_error_size = 10;
		else if (error_size==2)
			UW_error_size = 12;
		else if (error_size==3)
			UW_error_size = 14;
		else if (error_size==4)
			UW_error_size = 16;
		else if (error_size==5)
			UW_error_size = 18;
		else if (error_size==6)
			UW_error_size = 20;
		else if (error_size==7)
			UW_error_size = 24;
		else if (error_size==8)
			UW_error_size = 36;
		else if (error_size==9)
			UW_error_size = 48;
	}
	if (upt_error)
	{
		uw_ntupd_errorfont();
	}
/*	if ((UW_menu_fmt!=menufmt)||(UW_icon_size!=iconsize))
	{
		UW_menu_fmt = menufmt;
*/
	if (UW_icon_size!=iconsize)
	{
		UW_icon_size = iconsize;
		uw_ntmenu_redisp(old_size, old_fmt);
	}
	if ((UW_live_mouse!=live_mouse)&&(LW_nclipv!=LW_STANDALONE))
	{
		uw_ntset_livemouse(live_mouse);
	}
#endif
	UW_auto_cursor = autoc[0];
	UW_text_cursor = textc[0];
	UW_text_select = highl[0];
	UW_frmtext_select = formhl;
	UW_browse_dir = browsop;
	UW_keypad = keypad;
	UW_stat_mode = status_mode;
	S_save_modfile();
	return(status);
}
/*********************************************************************
**    E_FUNCTION     : S_save_modfile2
**       Save the output modals properties into modals file.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_modfile2()
{
	int stat;
	char msg[80];
	UX_pathname fname;
	FILE *fptr;
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy(fname,"ncl_modals.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,fname,
		3,&fptr);
	if (stat != UU_SUCCESS || fptr == UU_NULL) goto done;

	ux_fputs0("#MODALS_FILES#\n", fptr);
		
	if (UW_Store_color==0)
		sprintf(msg,"/SAVE_COLORS/ *No\n");
	else
		sprintf(msg,"/SAVE_COLORS/ *Yes\n");
	ux_fputs0(msg, fptr);

	if (UW_Store_forms==0)
		sprintf(msg,"/SAVE_FORMS/ *No\n");
	else
		sprintf(msg,"/SAVE_FORMS/ *Yes\n");
	ux_fputs0(msg, fptr);
/*
.....Close modals file
*/
	ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
	return(stat);
}

/**************************************************************************
**
**  E_FUNCTION         :  uz_pos_modals()
**     Process the modals/positioning form.
**  PARAMETERS
**      INPUT  :
**          none
**      OUTPUT :
**          none
**
**  RETURNS      :  
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
int uz_pos_modals()
{
	int status; 
	static store_file, store_color;
	static int *ans[] = {(int *)&store_color, (int *)&store_file};

	store_color = UW_Store_color;
	store_file = UW_Store_forms;

	status = ud_form("ioutmodal.frm", ans, ans);
	if (status==-1)
		return -1;
	if ((store_color!=UW_Store_color)&&(store_color==1))
/*
......output the colors in the ncl_color now
*/
	{
		UW_Store_color = store_color;
/*
.....if the NCL color changed but not saved due to the UW_Store_color
.....updated the output file
*/
		if (NCL_clrs_changed)
			ncl_save_clrmod();
	}
	else
		UW_Store_color = store_color;
	UW_Store_forms = store_file;
	S_save_modfile2();
	return(0);
}

