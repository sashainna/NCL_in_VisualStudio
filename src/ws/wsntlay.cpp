#include "usysdef.h"
#if UU_COMP == UU_WIN2K
/*********************************************************************
**
**    NAME         :  wsntlay.c
**
**       CONTAINS:
**  			  uw_ntsave_layout
**  			  uw_ntload_layout
**				  uw_ntwrite_layout(fullname)
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntlay.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 16:53:59
*********************************************************************/
#include "wsntstdafx.h"
#include "uims.h"
#include "dinput.h"
#include "dasnog.h"
#include "dasg.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "wsntctl.h"
#include "wsntdoc.h"
#include "wsntview.h"
#include "wsntpktwin.h"
#include "wsntframe.h"
#include "wsgl.h"
#include "wsntform.h"
#include "wsntglfunc.h"
#include "wsntopt.h"
#include "wsntcfunc.h"
#include "wsntfuncid.h"
#include "zkeysym.h"
#include "dmotif.h"

extern CMainFrame *NCL_MainFrame;
extern CWnd *NCL_Main_View;
extern int frm_done;

extern void uw_ntadd_menuborder(CRect *rect);
extern CControlBar* uw_ntget_bars(int bar_pos, int indx);

extern "C" int uw_ntshow_rows(int menu_pos[3]);
extern "C" int uw_ntget_gwsize(int *wid, int *hgt);
extern "C" int uw_ntmenu(int kinc, int disp);
extern "C" int ud_get_areadockable(char *area_name, int *dockable);
extern "C" int uw_ntshow_dir(int menu_pos);
extern "C" int ud_find_menuarea(int pos, char *areaname);
extern "C" int ul_open_mod_file(char*, char*, char*, char*, char*, int, FILE**);
extern "C" UU_LOGICAL ul_session_active();
extern int UW_reset_menu;
extern HWND NCL_cmdwin;

/*
.....added for temperate area name
*/
static char temp_area[20][80];
static int temp_areanum = 0;
extern "C" int ncl_set_statact(int);
extern "C" int uw_ntcommand_areabar(int kdis);
extern "C" int uw_ntprompt_area(int kdis);
extern "C" int uw_nterror_area(int kdis);
extern "C" int uw_cmd_extend();
/*************************************************************************
**
**  E_FUNCTION         :  uw_clear_temparea_list()
**     Clear temperate areaname list 
**
**  PARAMETERS   
**      INPUT  : 
**          none
**      OUTPUT :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
static void uw_clear_temparea_list()
{
	int i;
	for (i=0; i<20; i++)
	{
		temp_area[i][0] = '\0';
	}
	temp_areanum = 0;
}

/*************************************************************************
**
**  E_FUNCTION         :  uw_addin_temparea(char *area)
**     add a earaname into temperate areaname list 
**
**  PARAMETERS   
**      INPUT  : 
**          area: araename to be added
**      OUTPUT :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
static void uw_addin_temparea(char *area)
{
	strcpy_s(temp_area[temp_areanum], sizeof(temp_area[temp_areanum]), area);
	temp_areanum++;
}

/*************************************************************************
**
**  E_FUNCTION         :  uw_ntmatch_temparea(char *area)
**     check if a areaname is in the temperate areaname list 
**
**  PARAMETERS   
**      INPUT  : 
**          area: areaname to be checked
**      OUTPUT :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
static int uw_ntmatch_temparea(char *area)
{
	int i;
	for (i=0; i<temp_areanum; i++)
	{
		if (strcmp(temp_area[i], area)==0)
			return 1;
	}
	return 0;
}

/***********************************************************************
**
**   FUNCTION: uw_getnext_area(char *next_area)
**
**            Get the next avilable area name
**
**   INPUT:  None
**
**   OUTPUT :   next_area: next avilable area name
**   RETURN:    None
**
**********************************************************************/
void uw_getnext_area(char *next_area)
{
	char area[80];
	int i, k;

	k = 1;
	while (k!=200)
	{
		sprintf_s(area, sizeof(area), "Area%d", k);
		for (i=0; i<UDM_run_layout.nmenu; i++)
		{
			if (UDM_run_layout.menu_area[i][0]!='\0')
			{
				if (strcmp(UDM_run_layout.menu_area[i], area)==0)
				{
					k++;
					break;
				}
			}
		}
		if (i==UDM_run_layout.nmenu)
/*
.....search temp list
*/
		{
			if (!(uw_ntmatch_temparea(area)))
			{
				break;
			}
			k++;
		}
	}
	if (k!=200)
		strcpy_s(next_area, 80, area);
	else
		next_area[0] = '\0';
}
/***********************************************************************
**
**   FUNCTION: uw_ntis_statwinbar(CNCLDialogBar* tempbar)
**
**           Check if this is the statusbar
**
**   INPUT:  tempbar: dialogbar to be check
**
**   OUTPUT :   none
**   RETURN:    1: Yes
**				0: No
**
**********************************************************************/
static int uw_ntis_statwinbar(CNCLDialogBar* tempbar)
{					
	if ((NCL_MainFrame->m_statusBar!=NULL)
		&& ((NCL_MainFrame->m_statusBar)->m_visible==1)
		&& (NCL_MainFrame->m_statusBar==tempbar))
		return 1;
	return 0;			
}

/***********************************************************************
**
**   FUNCTION: uw_ntis_cmdbar(CNCLDialogBar* tempbar)
**
**           Check if this is the command line bar
**
**   INPUT:  tempbar: dialogbar to be check
**
**   OUTPUT :   none
**   RETURN:    1: Yes
**				0: No
**
**********************************************************************/
static int uw_ntis_cmdbar(CNCLDialogBar* tempbar)
{					
	if ((NCL_MainFrame->m_commandBar!=NULL)
		&& ((NCL_MainFrame->m_commandBar)->m_visible==1)
		&& (NCL_MainFrame->m_commandBar==tempbar))
		return 1;
	return 0;			
}

/***********************************************************************
**
**   FUNCTION: uw_ntis_promptbar(CNCLDialogBar* tempbar)
**
**           Check if this is the prompt bar
**
**   INPUT:  tempbar: dialogbar to be check
**
**   OUTPUT :   none
**   RETURN:    1: Yes
**				0: No
**
**********************************************************************/
static int uw_ntis_promptbar(CNCLDialogBar* tempbar)
{
	if ((NCL_MainFrame->m_promptBar!=NULL)
		&& ((NCL_MainFrame->m_promptBar)->m_visible==1)
		&& (NCL_MainFrame->m_promptBar==tempbar))
		return 1;
	return 0;								
}
/***********************************************************************
**
**   FUNCTION: uw_ntis_errorbar(CNCLDialogBar* tempbar)
**
**           Check if this is the error bar
**
**   INPUT:  tempbar: dialogbar to be check
**
**   OUTPUT :   none
**   RETURN:    1: Yes
**				0: No
**
**********************************************************************/
static int uw_ntis_errorbar(CNCLDialogBar* tempbar)
{
	if ((NCL_MainFrame->m_errorBar!=NULL)
		&& ((NCL_MainFrame->m_errorBar)->m_visible==1)
		&& (NCL_MainFrame->m_errorBar==tempbar))
		return 1;
	return 0;			
}

/***********************************************************************
**
**   FUNCTION: uw_ntfind_menunum(CToolmenu *tempbar, int *menunum)
**
**           find menu index number from menu array
**
**   INPUT:  tempbar: menubar to be check
**
**   OUTPUT :   menunum: menubar index numberzz
**   RETURN:    1: find it
**				0: not find
**
**********************************************************************/
static int uw_ntfind_menunum(CToolmenu *tempbar, int *menunum)
{
	int i;
	for( i = 0; i< UDM_menu_count; i++)
	{
		if ((NCL_MainFrame->NCL_menubar[i]!=NULL)
			&&((NCL_MainFrame->NCL_menubar[i])->m_visible==1)
			&& (NCL_MainFrame->NCL_menubar[i] == tempbar))
		{
			*menunum = i;
			return 1;
		}
	}
	return 0;
}

/***********************************************************************
**
**   FUNCTION: uw_isarea_empty(char *menu_area, int menunum)
**
**          Check if the input menu area is empty
**
**   INPUT:  menu_area: input menu area to be checked
**				menunum: menu to be excluded
**   OUTPUT :   none
**   RETURN:    1: is empty
**				0: not empty
**
**********************************************************************/
static int uw_isarea_empty(char *menu_area, int menunum)
{
	int i;
	for( i = 0; i< UDM_menu_count; i++)
	{
		if ((NCL_MainFrame->NCL_menubar[i]!=NULL)
			&&((NCL_MainFrame->NCL_menubar[i])->m_visible==1)
			&& (strcmp(UDM_menu[i].menu_area, menu_area)==0))
		{
			if (i!=menunum)
				return 0;
		}
	}
	return 1;
}

/**********************************************************************
**    I_FUNCTION : uw_ntsave_menu(type, UDM_MENU menu_struct)
**    Save the menu of UDM_menu
**    PARAMETERS   
**       INPUT  : 
**			type: menu type:
**						UDM_MTYPE_MENU
**						UDM_MTYPE_ICON
**						UDM_MTYPE_PULLDOWN
**						UDM_MTYPE_POPUP
**						UDM_MTYPE_MENUBAR
**			menu_struct: menu structure to save
**       OUTPUT :  
**         		none 
**    RETURNS      : status
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uw_ntsave_menu(int type, UDM_MENU menu_struct)
{
	int i, mode, status;
	UX_pathname fullname,file1,ptr1;
	char buffer[256];
	float number1, number2;
	int menu_file;
	char *p, *ptr,*file;
	FILE *fd;						/* os dependent file descriptor */
	ptr = ptr1;
	file = file1;

	mode = 0;
	status = 0;
	strcpy_s(file1, sizeof(file1), menu_struct.file);
	p = strstr(menu_struct.file, "_F.menu");
	if (p!=NULL) 
	{
		status = ux_mk_chk_syspath(0, 0, file1, 0,
			     "UD_MENU_SUFFIX", &mode, fullname, UX_PRTERRS);
		if (!(mode & UX_NEXISTS)) 
			status = ux_delete(menu_struct.file);
		else
			status =0;
	}
	else
	{
		status = ux_mk_chk_syspath(0, 0, file1, 0,
			     "UD_MENU_SUFFIX", &mode, fullname, UX_PRTERRS);
		if (!(mode & UX_NEXISTS)) 
			status = ux_delete(menu_struct.file);
		else
			status =0;
	}
	if (status != 0) return status;
	
	status = ux_create_file(menu_struct.file, 0666, 0, "STREAM", "ASCII",
		"UX_NOHEADER", &menu_file, UX_PRTERRS);

	if (status == 0)
	{
/*
...put menu data into menu file
*/
		if ((type==UDM_MTYPE_MENU) || (type==UDM_MTYPE_PULLDOWN)
			|| (type==UDM_MTYPE_POPUP) 
			|| (type==UDM_MTYPE_MENUBAR))
		{
			ux_get_os_filedesc(menu_file, &fd, UX_PRTERRS);
			ux_fputs0("#DESCRIPTOR#\n", fd);
			sprintf_s(buffer, sizeof(buffer), "/NAME/ %s\n", menu_struct.name);
			ux_fputs0(buffer, fd);
			number1 = (float)menu_struct.pos[0]/(float)uw_gl.dev_xmax;
			number2 = (float)menu_struct.pos[1]/(float)uw_gl.dev_ymax;
			if (number1<0) number1 = 0;
			if (number2<0) number2 = 0;
			sprintf_s(buffer, sizeof(buffer), "/POSITION/ %f,%f\n", number1, number2);
			ux_fputs0(buffer, fd);
			number1 = (float)(menu_struct.size[0]);
			number2 = (float)(menu_struct.size[1]);
			number1 = number1/(float)uw_gl.dev_xmax;
			number2 = number2/(float)uw_gl.dev_ymax;
			sprintf_s(buffer, sizeof(buffer), "/SIZE/ %f,%f\n", number1, number2);
			ux_fputs0(buffer, fd);
			sprintf_s(buffer, sizeof(buffer), "/ROWS/ %d\n", menu_struct.rows);
			ux_fputs0(buffer, fd);
			sprintf_s(buffer, sizeof(buffer), "/COLS/ %d\n", menu_struct.cols);
			ux_fputs0(buffer, fd);
			if(type == UDM_MTYPE_MENU)
				sprintf_s(buffer, sizeof(buffer), "/TYPE/ MENU\n");
			else if(type == UDM_MTYPE_MENUBAR)
				sprintf_s(buffer, sizeof(buffer), "/TYPE/ MENUBAR\n");
			else if(type == UDM_MTYPE_PULLDOWN)
				sprintf_s(buffer, sizeof(buffer), "/TYPE/ PULLDOWN\n");
			else
				sprintf_s(buffer, sizeof(buffer), "/TYPE/ POPUP\n");
			ux_fputs0(buffer, fd);
/*
.....Bitmap
*/
			if (menu_struct.bmpfile[0]!='\0')
			{
				sprintf_s(buffer, sizeof(buffer), "/BITMAP/ %s\n", menu_struct.bmpfile);
				ux_fputs0(buffer, fd);
			}
			ux_fputs0("\n#MENUS#\n", fd);
			for(i = 0; i < menu_struct.num; i++)
			{
				if (menu_struct.menus[i].chcfile[0]=='\0')
				{
					if (menu_struct.menus[i].separator==1)
					{
						ux_fputs0("#BAR#\n", fd);
						continue;
					}
					if (menu_struct.menus[i].name[0]=='\0')
						strcpy_s(menu_struct.menus[i].name, sizeof(menu_struct.menus[i].name), " ");
					if ((menu_struct.menus[i].params==NULL)||(menu_struct.menus[i].params[0]=='\0'))
					{
						if ((menu_struct.menus[i].bmpfile[0]=='\0') 
							&& (menu_struct.menus[i].bmpnum==-1))
							sprintf_s(buffer, sizeof(buffer), "/%s/ %s\n", menu_struct.menus[i].name,
											menu_struct.menus[i].file);
						else if (strcmp (menu_struct.bmpfile, menu_struct.menus[i].file)==0)
						{
							sprintf_s(buffer, sizeof(buffer), "/%s/ %s, %d\n", menu_struct.menus[i].name,
											menu_struct.menus[i].file,
											menu_struct.menus[i].bmpnum);
						}
						else 
						{
							sprintf_s(buffer, sizeof(buffer), "/%s/ %s, %s, %d\n", menu_struct.menus[i].name,
											menu_struct.menus[i].file,
											menu_struct.menus[i].bmpfile,
											menu_struct.menus[i].bmpnum);
						}
					}
					else
					{
						if ((menu_struct.menus[i].bmpfile[0]=='\0')
								&& (menu_struct.menus[i].bmpnum==-1))
							sprintf_s(buffer, sizeof(buffer), "/%s/ %s, \"%s\"\n",menu_struct.menus[i].name,
											menu_struct.menus[i].file, 
											menu_struct.menus[i].params);
						else if (strcmp (menu_struct.bmpfile, menu_struct.menus[i].file)==0)
						{
							sprintf_s(buffer, sizeof(buffer), "/%s/ %s, \"%s\", %d\n",menu_struct.menus[i].name,
											menu_struct.menus[i].file, 
											menu_struct.menus[i].params,
											menu_struct.menus[i].bmpnum);
						}
						else 
						{
							sprintf_s(buffer, sizeof(buffer), "/%s/ %s, \"%s\", %s, %d\n",menu_struct.menus[i].name,
											menu_struct.menus[i].file, 
											menu_struct.menus[i].params,
											menu_struct.menus[i].bmpfile,
											menu_struct.menus[i].bmpnum);
						}
					}
				}
				else
				{
/*
.....choice menu
*/
					sprintf_s(buffer, sizeof(buffer), "#CHOICE# %s,%d\n",
						menu_struct.menus[i].chcfile, menu_struct.menus[i].chcdef);

				}
				ux_fputs0(buffer, fd);
			}
		}
		else
		{
			ux_get_os_filedesc(menu_file, &fd, UX_PRTERRS);
			ux_fputs0("#DESCRIPTOR#\n", fd);
			sprintf_s(buffer, sizeof(buffer), "/NAME/ %s\n", menu_struct.name);
			ux_fputs0(buffer, fd);
			if (menu_struct.statflag!=0)
			{
				number1 = (float)menu_struct.pos[0]/(float)uw_gl.dev_xmax;
				number2 = (float)menu_struct.pos[1]/(float)uw_gl.dev_ymax;
				if (number1<0) number1 = 0;
				if (number2<0) number2 = 0;
				sprintf_s(buffer, sizeof(buffer), "/POSITION/ %f,%f\n", number1, number2);
				ux_fputs0(buffer, fd);
				number1 = (float)(menu_struct.size[0]);
				number2 = (float)(menu_struct.size[1]);
				number1 = number1/(float)uw_gl.dev_xmax;
				number2 = number2/(float)uw_gl.dev_ymax;
				sprintf_s(buffer, sizeof(buffer), "/SIZE/ %f,%f\n", number1, number2);
				ux_fputs0(buffer, fd);
			}
			sprintf_s(buffer, sizeof(buffer), "/AREA/ %s\n", menu_struct.menu_area);
			ux_fputs0(buffer, fd);
			sprintf_s(buffer, sizeof(buffer), "/ROWS/ %d\n", menu_struct.rows);
			ux_fputs0(buffer, fd);
			sprintf_s(buffer, sizeof(buffer), "/COLS/ %d\n", menu_struct.cols);
			ux_fputs0(buffer, fd);
			sprintf_s(buffer, sizeof(buffer), "/TYPE/ ICON\n");
			ux_fputs0(buffer, fd);
			if (menu_struct.justify)
				ux_fputs0("/JUSTIFY/ RIGHT\n", fd);
			int stat_flag = -1;
			for(i = 0; i < menu_struct.num; i++)
			{
				if ((menu_struct.menus[i].statname[0]!='\0')&&(stat_flag!=1))
				{
					stat_flag = 1;
					ux_fputs0("\n#BUTTONS#\n", fd);
				}
				else if ((stat_flag!=0)&&(menu_struct.menus[i].statname[0]=='\0'))
				{
					stat_flag = 0;
					ux_fputs0("\n#MENUS#\n", fd);
				}
				if (menu_struct.menus[i].chcfile[0]=='\0')
				{
					if (menu_struct.menus[i].separator==1)
					{
						ux_fputs0("#BAR#\n", fd);
						continue;
					}
					if (menu_struct.menus[i].name[0]=='\0')
						strcpy_s(menu_struct.menus[i].name, sizeof(menu_struct.menus[i].name), " ");
					if ((menu_struct.menus[i].params==NULL)||(menu_struct.menus[i].params[0]=='\0'))
					{
						if ((menu_struct.menus[i].bmpfile[0]=='\0') 
							&& (menu_struct.menus[i].bmpnum==-1))
						{
							if (menu_struct.menus[i].statname[0]!=0)
							{
								if (menu_struct.menus[i].descrip[0]!=0)
								{
									sprintf_s(buffer, sizeof(buffer), "/%s/ %s, <%s>, %s\n", 
											menu_struct.menus[i].name,
											menu_struct.menus[i].statname,
											menu_struct.menus[i].descrip,
											menu_struct.menus[i].file);
								}
								else
									sprintf_s(buffer, sizeof(buffer), "/%s/ %s %s\n", 
											menu_struct.menus[i].name,
											menu_struct.menus[i].statname,
											menu_struct.menus[i].file);
							}
							else
								sprintf_s(buffer, sizeof(buffer), "/%s/ %s\n", menu_struct.menus[i].name,
											menu_struct.menus[i].file);
						}
						else if (strcmp (menu_struct.bmpfile, menu_struct.menus[i].file)==0)
						{
							sprintf_s(buffer, sizeof(buffer), "/%s/ %s, %d\n", menu_struct.menus[i].name,
											menu_struct.menus[i].file,
											menu_struct.menus[i].bmpnum);
						}
						else 					
						{
							if (menu_struct.menus[i].statname[0]!=0)
							{
								if (menu_struct.menus[i].descrip[0]!=0)
								{
									sprintf_s(buffer, sizeof(buffer), "/%s/ %s, <%s>, %s\n", menu_struct.menus[i].name,
											menu_struct.menus[i].statname, menu_struct.menus[i].descrip, 
											menu_struct.menus[i].file);
								}
								else
									sprintf_s(buffer, sizeof(buffer), "/%s/ %s %s\n", menu_struct.menus[i].name,
											menu_struct.menus[i].statname, menu_struct.menus[i].file);
							}
							else
							{
								sprintf_s(buffer, sizeof(buffer), "/%s/ %s, %s, %d\n", menu_struct.menus[i].name,
											menu_struct.menus[i].file,
											menu_struct.menus[i].bmpfile,
											menu_struct.menus[i].bmpnum);
							}
						}
					}
					else
					{
						if ((menu_struct.menus[i].bmpfile[0]=='\0')
								&& (menu_struct.menus[i].bmpnum==-1))
							sprintf_s(buffer, sizeof(buffer), "/%s/ %s, \"%s\"\n",menu_struct.menus[i].name,
											menu_struct.menus[i].file, 
											menu_struct.menus[i].params);
						else if (strcmp (menu_struct.bmpfile, menu_struct.menus[i].file)==0)
						{
							sprintf_s(buffer, sizeof(buffer), "/%s/ %s, \"%s\", %d\n",menu_struct.menus[i].name,
											menu_struct.menus[i].file, 
											menu_struct.menus[i].params,
											menu_struct.menus[i].bmpnum);
						}
						else 
						{
							sprintf_s(buffer, sizeof(buffer), "/%s/ %s, \"%s\", %s, %d\n",menu_struct.menus[i].name,
											menu_struct.menus[i].file, 
											menu_struct.menus[i].params,
											menu_struct.menus[i].bmpfile,
											menu_struct.menus[i].bmpnum);
						}
					}
				}
				else
				{
/*
.....choice menu
*/
					sprintf_s(buffer, sizeof(buffer), "#CHOICE# %s,%d\n",
						menu_struct.menus[i].chcfile, menu_struct.menus[i].chcdef);

				}
				ux_fputs0(buffer, fd);
			}
		}
		ux_close(menu_file, UX_PRTERRS);
	}
	else 
	{
/*
...cannot open file
*/
		uw_nterror("Cannot create output menu file.");
	}
	return status;
}

/*************************************************************************
**
**  E_FUNCTION         :  uw_ntwrite_layout()
**     save the layout into a layout file 
**
**  PARAMETERS   
**      INPUT  : 
**          none
**      OUTPUT :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

int uw_ntwrite_layout(char* fullname)
{
	int layout, i, k, x,y,len;
	char areaname[80];
	int rows, cols, cx, cy;
	FILE *fd;
	char *ptr2;
	char num[100];
	UX_pathname dir1,dir,filename,ptr1;
	float temp1, temp2, temp3, temp4;
	int status, ans, statwin_saved, cmdwin_saved;
	char msg[UX_MAX_PATH_LEN+40], *p, next_area[256];
	int menu_pos[5], menu_size[2];
	CRect rect, rectW;
	RECT *rect1;
	int first = 1;
	int def_first = 1;

	status = ux_create_file(fullname, 0666, 0, "STREAM", "ASCII",
		"UX_NOHEADER", &layout, UX_PRTERRS);
	if (status != 0)
	{
		sprintf_s(msg, sizeof(msg), "Can not create file %s!", fullname);
		uw_nterror(msg);
		return (UU_FALSE);
	}
	statwin_saved = 0;
	cmdwin_saved = 0;
	uw_clear_temparea_list();
	if (status == 0)
	{
		ux_get_os_filedesc(layout, &fd, UX_PRTERRS);
/*
.....save #TITLE# section
*/
		ux_fputs0("\n#TITLE#\n", fd);
		sprintf_s(dir, sizeof(dir), "/NAME/ %s\n", UD_window_title);
		ux_fputs0(dir, fd);
/*
.....save the menu directory
.....default to NCL_MENU
*/
		ux_fputs0("\n#DIRECTORY#\n", fd);
		ux_search_for_path("NCL_MENU", dir1, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
		sprintf_s(dir, sizeof(dir), "/MENU/ \"%s\"\n", dir1);
		ux_fputs0(dir, fd);
/*
.....save the bitmap directory
.....default to NCL_BITMAP
*/
		ux_search_for_path("NCL_BITMAP", dir1, UX_PRTERRS|UX_NCHK|UX_NQUOTES);
		sprintf_s(dir, sizeof(dir), "/BITMAP/ \"%s\"\n", dir1);
		ux_fputs0(dir, fd);
/*
.....save StatusBar on the bottom
*/
		if (UDM_layout.statbar_no>0)
		{
			ux_fputs0("\n#STATUSBAR#\n", fd);
/*
.....statflag==2
*/
			if (NCL_MainFrame->m_bStatusBar.m_changed)
			{
				NCL_MainFrame->m_bStatusBar.SaveChange();
			}
			for (i=0; i<UDM_run_layout.statbar_no&&i<4; i++)
			{
				sprintf_s(num, sizeof(num), "/MENU/ %s\n", UDM_layout.statbar_file[i]);
				ux_fputs0(num, fd);
			}
		}
/*
...save the menu layout
*/
/*
.....save menubar first
*/
		ux_fputs0("\n#MENUBAR#\n", fd);
		sprintf_s(num, sizeof(num), "/MENU/ %s\n", UDM_run_layout.menubar);
		ux_fputs0(num, fd);
/*
......First, save the floating menus
*/
		for( i = 0; i< UDM_menu_count; i++)
		{
			if ((NCL_MainFrame->NCL_menubar[i]!=NULL)
				&&((NCL_MainFrame->NCL_menubar[i])->m_visible==1)
				&&((NCL_MainFrame->NCL_menubar[i])->IsFloating()!=0))
			{
				if (UDM_menu[i].type == UDM_MTYPE_ICON)
				{
					if (first)
					{
						first = 0;
						sprintf_s(msg, sizeof(msg), "%s\r\n%s\r\n%s\r\n\r\n%s\r\n%s\r\n%s",
							"You have changed a Menu/Status Bar type from floating to",
							"docked or vice versa. The menu file will have to be",
							"overwritten to reflect this change.",
							"Click ‘Yes’ to overwrite the menu file,",
							"‘No’ to save the layout without the menu changes, or",
							"‘Cancel’ to not save the layout changes");
						ans = uw_ntyesnocancel(NULL, msg, "Save Menu Changes");
						if (ans==-1)
/*
.....Canceled, jump out of this save routine
*/
						{
							return (UU_FALSE);
						}
						else if (ans==0)
						{
/*
......if "No", we not only don't save the menu, we can't save this menu into layout file too 
......since it won't display it anyway or even wrong
*/
							continue;
						}
					}
				}

/*
.....Get the floating menu position and size
*/
				(NCL_MainFrame->NCL_menubar[i])->GetWindowRect(&rect);			
				menu_size[0] = rect.Width() -8;
				menu_size[1] = rect.Height() -8;
/*
.....we need extra 7+7 for bar edge because when we deal with statusbar
.....we set the button size instead of menu size (use size/cols) so we need
.....consider that
*/
				if (UDM_menu[i].statflag==1)
					menu_size[0] = menu_size[0]-14;
				uw_ntadd_menuborder(&rect);
				rect1 = (LPRECT)&rect;
				menu_pos[0] = rect1->left;
				menu_pos[1] = rect1->top;
				ux_fputs0("\n#MENUS#\n", fd);
				temp1 = (float)(menu_pos[0]);
				temp2 = (float)(menu_pos[1]);
				temp1 = temp1/(float)uw_gl.dev_xmax;
				temp2 = temp2/(float)uw_gl.dev_ymax;
				temp3 = (float)(menu_size[0]);
				temp4 = (float)(menu_size[1]);			
				temp3 = temp3/(float)uw_gl.dev_xmax;
				temp4 = temp4/(float)uw_gl.dev_ymax;
/*
.....only save the menu file name, exclude whole path
*/
				ptr2 = ptr1;
				if((ptr2 = strrchr(UDM_menu[i].file,'\\'))!=NULL)
					strcpy_s(filename, sizeof(filename), ptr2+1) ;
				else
					strcpy_s(filename, sizeof(filename), UDM_menu[i].file);
/*
......we save status bar as menu now
*/
/*				if (UDM_menu[i].statflag==0) */
				{
					sprintf_s(num, sizeof(num), "/MENU/ %s,%f,%f,%f,%f\n",filename,
								temp1, temp2, temp3, temp4);
					ux_fputs0(num, fd);
				}
/*
......if type changed, save this menu
*/
				if (UDM_menu[i].type == UDM_MTYPE_ICON)
				{
					UDM_menu[i].pos[0] = menu_pos[0];
					UDM_menu[i].pos[1] = menu_pos[1];
					UDM_menu[i].size[0] = menu_size[0];
					UDM_menu[i].size[1] = menu_size[1];
					uw_ntsave_menu(UDM_MTYPE_MENU, UDM_menu[i]);
				}

				p = strstr(UDM_menu[i].file, "_F.menu");
				if (p!=NULL) 
				{
					UDM_menu[i].pos[0] = menu_pos[0];
					UDM_menu[i].pos[1] = menu_pos[1];
					UDM_menu[i].size[0] = menu_size[0];
					UDM_menu[i].size[1] = menu_size[1];
					UDM_menu[i].type = UDM_MTYPE_MENU;
					uw_ntsave_menu(UDM_MTYPE_MENU, UDM_menu[i]);
				}
				if (((NCL_MainFrame->NCL_menubar[i])->m_defchg==1)
					&& (def_first))
				{

					def_first = 0;
					sprintf_s(msg, sizeof(msg), "%s\n%s\n%s\n%s\n%s\n",
							"Some of menu's default choice is changed. We have to overwrite",
							"the existing menu file to keep the layout as displayed, otherwise,",
							"the layout display will not show as now. Click 'Yes' to overwrite",
							"the existing menu files. Click 'No' to ignore default menu change",
							"Click 'Cancel' to cancel the layout saving");				
					ans = uw_ntyesnocancel(NULL, msg, "Menu type change");
					if (ans==1)
					{
						UDM_menu[i].pos[0] = menu_pos[0];
						UDM_menu[i].pos[1] = menu_pos[1];
						UDM_menu[i].size[0] = menu_size[0];
						UDM_menu[i].size[1] = menu_size[1];
						UDM_menu[i].type = UDM_MTYPE_MENU;
						uw_ntsave_menu(UDM_MTYPE_MENU, UDM_menu[i]);
					}
					else if (ans==-1)
/*
.....Canceled, jump out of this save routine
*/
					{
							return (UU_FALSE);
					}
				}
			}
		}
/*
.....save empty menu area (menu area without menu inside) before save other bars
.....because the other bar (such as status window bar) may using the meny area defined here
*/
		for (i=0; i<UDM_run_layout.nmenu; i++)
		{
			if ((UDM_run_layout.menu_area[i][0]!='\0')
				&& (UDM_run_layout.menu_name[i][0]=='\0'))
			{
/*
.....see if this menuarea have already display a menu and saved
*/
				if (uw_isarea_empty(UDM_run_layout.menu_area[i], -1))
				{
					ux_fputs0("\n#MENUAREA#\n", fd);
					sprintf_s(num, sizeof(num), "/NAME/ %s\n",UDM_run_layout.menu_area[i]);
					ux_fputs0(num, fd);
				
					if (UDM_run_layout.area_dockable[i]==UDM_AREA_FRONTDOCK)
						ux_fputs0("/DOCKABLE/ FRONT\n", fd);
					else if (UDM_run_layout.area_dockable[i]==UDM_AREA_ENDDOCK)
						ux_fputs0("/DOCKABLE/ END\n", fd);
					else if (UDM_run_layout.area_dockable[i]==UDM_AREA_NODOCK)
						ux_fputs0("/DOCKABLE/ NO\n", fd);
					else
						ux_fputs0("/DOCKABLE/ FRONT\n", fd);

					ud_getpos_from_bararea(1, UDM_run_layout.menu_name[i], UDM_run_layout.menu_area[i], menu_pos, menu_size);
					if (menu_pos[2]==1)
						sprintf_s(num, sizeof(num), "/POSITION/ TOP\n");
					else if (menu_pos[2]==2)
						sprintf_s(num, sizeof(num), "/POSITION/ BOTTOM\n");
					else if (menu_pos[2]==3)
						sprintf_s(num, sizeof(num), "/POSITION/ LEFT\n");
					else if (menu_pos[2]==4)
						sprintf_s(num, sizeof(num), "/POSITION/ RIGHT\n");
					ux_fputs0(num, fd);
					ux_fputs0("/MENU/ \n", fd);
				}
			}
		}
/*
.....save menu area with menu inside
.....and save error/prompt/command bar
*/
/*
.....save menuarea from "TOP", "BOTTOM","LEFT", "RIGHT"
.....now, because we may need create new menu area because 
.....floating menu may be docked into ICON menu
*/
		CControlBar* tempbar;
		int indx, menunum, dockable;
		for (k=1; k<=4;k++)
		{
			indx = 0;
			tempbar = (CControlBar* )uw_ntget_bars(k, indx);
			while (tempbar!=NULL)
			{
				if(tempbar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
/*
......menubar and statusbar
*/
				{
					uw_ntfind_menunum((CToolmenu*)tempbar, &menunum);
					if (UDM_menu[menunum].type == UDM_MTYPE_MENU)
					{
						if (first)
						{
							first = 0;
							sprintf_s(msg, sizeof(msg), "%s\r\n%s\r\n%s\r\n\r\n%s\r\n%s\r\n%s",
								"You have changed a Menu/Status Bar type from floating to",
								"docked or vice versa. The menu file will have to be",
								"overwritten to reflect this change.",
								"Click ‘Yes’ to overwrite the menu file,",
								"‘No’ to save the layout without the menu changes, or",
								"‘Cancel’ to not save the layout changes");
							ans = uw_ntyesnocancel(NULL, msg, "Save Menu Changes");
							if (ans==-1)
							{
/*
......Canceled, do not save layour file at all
*/
								return (UU_FALSE);
							}
							else if (ans==0)
							{
/*
......save layout file without the menu changes
......if "No", we not only don't save the menu, we can't save this menu into layout file too 
......since it won't display it anyway or even wrong
*/
								goto next;
							}
						}
					}
					ud_getpos_from_bararea(0, UDM_menu[menunum].file, UDM_menu[menunum].menu_area, menu_pos, menu_size);
					if (menu_pos[2]!=k)
/*
.....this menu draged from original position
.....create a new menu area and save the original
.....menu area (if it is empty) with empty menu inside
*/
					{
						if (uw_isarea_empty(UDM_menu[menunum].menu_area, menunum))
						{
							ux_fputs0("\n#MENUAREA#\n", fd);

							sprintf_s(num, sizeof(num), "/NAME/ %s\n",UDM_menu[menunum].menu_area);
							ux_fputs0(num, fd);
							if (menu_pos[2]==1)
								sprintf_s(num, sizeof(num), "/POSITION/ TOP\n");
							else if (menu_pos[2]==2)
								sprintf_s(num, sizeof(num), "/POSITION/ BOTTOM\n");
							else if (menu_pos[2]==3)
								sprintf_s(num, sizeof(num), "/POSITION/ LEFT\n");
							else if (menu_pos[2]==4)
								sprintf_s(num, sizeof(num), "/POSITION/ RIGHT\n");
							ux_fputs0(num, fd);
							ux_fputs0("/DOCKABLE/ FRONT\n", fd);
							if (UDM_menu[menunum].statflag==1)
								ux_fputs0("/SIZE/ -1, -1\n", fd);
							ux_fputs0("/MENU/ \n", fd);
						}
						ux_fputs0("\n#MENUAREA#\n", fd);
						uw_getnext_area(next_area);
						uw_addin_temparea(next_area);
						sprintf_s(num, sizeof(num), "/NAME/ %s\n",next_area);
						ux_fputs0(num, fd);
						ux_fputs0("/DOCKABLE/ FRONT\n", fd);
						strcpy_s(UDM_menu[menunum].menu_area, sizeof(UDM_menu[menunum].menu_area), next_area);
					}
					else
					{
						ux_fputs0("\n#MENUAREA#\n", fd);
						sprintf_s(num, sizeof(num), "/NAME/ %s\n",UDM_menu[menunum].menu_area);
						ux_fputs0(num, fd);
						ud_get_areadockable(UDM_menu[menunum].menu_area, &dockable);
						if (dockable==UDM_AREA_FRONTDOCK)
							ux_fputs0("/DOCKABLE/ FRONT\n", fd);
						else if (dockable==UDM_AREA_ENDDOCK)
							ux_fputs0("/DOCKABLE/ END\n", fd);
						else if (dockable==UDM_AREA_NODOCK)
							ux_fputs0("/DOCKABLE/ NO\n", fd);
						else
							ux_fputs0("/DOCKABLE/ FRONT\n", fd);
					}
					if (k==1)
						sprintf_s(num, sizeof(num), "/POSITION/ TOP\n");
					else if (k==2)
						sprintf_s(num, sizeof(num), "/POSITION/ BOTTOM\n");
					else if (k==3)
						sprintf_s(num, sizeof(num), "/POSITION/ LEFT\n");
					else if (k==4)
						sprintf_s(num, sizeof(num), "/POSITION/ RIGHT\n");
					ux_fputs0(num, fd);
					if (UDM_menu[menunum].statflag==1)
					{
/*
.....put in SIZE
*/
						SIZE bsiz;
 						(NCL_MainFrame->NCL_menubar[menunum])->GetButSize(bsiz);
						temp3 = (float)(bsiz.cx);
						temp4 = (float)(bsiz.cy);
						temp3 = temp3/(float)uw_gl.dev_xmax;
						temp4 = temp4/(float)uw_gl.dev_ymax;
						sprintf_s(num, sizeof(num), "/SIZE/ %f, %f\n", temp3, temp4);
						ux_fputs0(num, fd);
					}
					ptr2 = ptr1;
					if((ptr2 = strrchr(UDM_menu[menunum].file,'\\'))!=NULL)
						strcpy_s(filename, sizeof(filename), ptr2+1) ;
					else
						strcpy_s(filename, sizeof(filename), UDM_menu[menunum].file);

					sprintf_s(num, sizeof(num), "/MENU/ %s\n",filename);
					ux_fputs0(num, fd);
/*
......if type changed, save this menu
*/
					if (UDM_menu[menunum].type != UDM_MTYPE_ICON)
					{
						UDM_menu[menunum].pos[0] = -1;
						UDM_menu[menunum].pos[1] = -1;
						UDM_menu[menunum].size[0] = -1;
						UDM_menu[menunum].size[1] = -1;
						uw_ntsave_menu(UDM_MTYPE_ICON, UDM_menu[menunum]);
					}
					if (((NCL_MainFrame->NCL_menubar[menunum])->m_defchg==1)
						&& (def_first))
					{
						def_first = 0;
						sprintf_s(msg, sizeof(msg), "%s\n%s\n%s\n%s\n%s\n",
							"Some of menu's default choice is changed. We have to overwrite",
							"the existing menu file to keep the layout as displayed, otherwise,",
							"the layout display will not show as now. Click 'Yes' to overwrite",
							"the existing menu files. Click 'No' to ignore default menu change",
							"Click 'Cancel' to cancel the layout saving");				
						ans = uw_ntyesnocancel(NULL, msg, "Menu type change");
						if (ans==1)
							uw_ntsave_menu(UDM_MTYPE_ICON, UDM_menu[menunum]);
						else if (ans==-1)
						{
								return (UU_FALSE);
						}
					}
				}
				else if(tempbar->IsKindOf(RUNTIME_CLASS(CNCLDialogBar)))
/*
......command bar, prompt bar and error bar
*/
				{
					if (uw_ntis_cmdbar((CNCLDialogBar*)tempbar))
					{
/*
						ux_fputs0("\n#COMMAND#\n",fd);
						ux_fputs0("/TYPE/ ICON\n",fd);
						if (k==1)
							sprintf(num, "/POSITION/ TOP\n");
						else if (k==2)
							sprintf(num, "/POSITION/ BOTTOM\n");
						else if (k==3)
							sprintf(num, "/POSITION/ LEFT\n");
						else if (k==4)
							sprintf(num, "/POSITION/ RIGHT\n");
						ux_fputs0(num, fd);
						temp1 = (float)(UDM_run_layout.command_size[0])/(float)uw_gl.dev_xmax;
						temp2 = (float)(UDM_run_layout.command_size[1])/(float)uw_gl.dev_ymax;
						sprintf(num, "/SIZE/ %f,%f\n", temp1, temp2);
						ux_fputs0(num, fd);
*/
						cmdwin_saved = 1;
						ux_fputs0("\n#COMMAND#\n",fd);
/*						if (UDM_run_layout.command_active==1)
							sprintf_s(num, sizeof(num), "/ACTIVE/ MULTI\n");
						else
							sprintf_s(num, sizeof(num), "/ACTIVE/ SINGLE\n");
*/
						if (uw_cmd_extend())
							sprintf_s(num, sizeof(num), "/ACTIVE/ MULTI\n");
						else
							sprintf_s(num, sizeof(num), "/ACTIVE/ SINGLE\n");
						ux_fputs0(num, fd);
						if (UDM_run_layout.command_att[0]==0)
							sprintf_s(num, sizeof(num), "/ATTACH/ TOP,");
						else
							sprintf_s(num, sizeof(num), "/ATTACH/ BOTTOM,");
						if (UDM_run_layout.command_att[1]==0)
							strcat_s(num, "LEFT\n");
						else
							strcat_s(num, "RIGHT\n");
						ux_fputs0(num, fd);
/*
.....find the menuarea name, first match
*/
						ud_find_menuarea(k, areaname);
						sprintf_s(num, sizeof(num), "/POSITION/ %s\n", areaname);
						ux_fputs0(num, fd);
						
						((CNCLCmdBar*)(NCL_MainFrame->m_commandBar))->GetWindowMultiSize(cx, cy, rows,0);			
						temp1 = (float)(cx)/(float)uw_gl.dev_xmax;
						temp2 = (float)(cy)/(float)uw_gl.dev_ymax;
						sprintf_s(num, sizeof(num), "/SIZE/ %f,%f,%d\n", temp1, temp2, rows);

						ux_fputs0(num, fd);
						if ((UDM_run_layout.command_doc[0]==0)
							&& (UDM_run_layout.command_doc[1]==0)
							&& (UDM_run_layout.command_doc[2]==0)
							&& (UDM_run_layout.command_doc[3]==0))
							sprintf_s(num, sizeof(num), "/DOCKABLE/ NO\n");
						else if ((UDM_run_layout.command_doc[0]==1)
							&& (UDM_run_layout.command_doc[1]==1)
							&& (UDM_run_layout.command_doc[2]==1)
							&& (UDM_run_layout.command_doc[3]==1))
							sprintf_s(num, sizeof(num), "/DOCKABLE/ YES\n");
						else
						{
							sprintf_s(num, sizeof(num), "/DOCKABLE/ ");
							if (UDM_run_layout.command_doc[0]==1)
								strcat_s(num, " TOP,");
							if (UDM_run_layout.command_doc[1]==1)
								strcat_s(num, " BOTTOM,");
							if (UDM_run_layout.command_doc[2]==1)
								strcat_s(num, " LEFT,");
							if (UDM_run_layout.command_doc[3]==1)
								strcat_s(num, " RIGHT");
							len = strlen(num);
							if ((len>1)&&(num[len-1]==','))
								num[len-1] = '\0';
							strcat_s(num, "\n");
						}
						ux_fputs0(num, fd);
					}
					else if (uw_ntis_promptbar((CNCLDialogBar*)tempbar))
					{
						ux_fputs0("\n#PROMPTS#\n",fd);
						ux_fputs0("/TYPE/ ICON\n",fd);
						if (k==1)
							sprintf_s(num, sizeof(num), "/POSITION/ TOP\n");
						else if (k==2)
							sprintf_s(num, sizeof(num), "/POSITION/ BOTTOM\n");
						else if (k==3)
							sprintf_s(num, sizeof(num), "/POSITION/ LEFT\n");
						else if (k==4)
							sprintf_s(num, sizeof(num), "/POSITION/ RIGHT\n");
						ux_fputs0(num, fd);
						temp1 = (float)(UDM_run_layout.prompt_size[0])/(float)uw_gl.dev_xmax;
						temp2 = (float)(UDM_run_layout.prompt_size[1])/(float)uw_gl.dev_ymax;
						sprintf_s(num, sizeof(num), "/SIZE/ %f,%f\n", temp1, temp2);
						ux_fputs0(num, fd);
					}
					else if (uw_ntis_errorbar((CNCLDialogBar*)tempbar))
					{
						ux_fputs0("\n#ERROR#\n",fd);
						ux_fputs0("/TYPE/ ICON\n",fd);
						if (k==1)
							sprintf_s(num, sizeof(num), "/POSITION/ TOP\n");
						else if (k==2)
							sprintf_s(num, sizeof(num), "/POSITION/ BOTTOM\n");
						else if (k==3)
							sprintf_s(num, sizeof(num), "/POSITION/ LEFT\n");
						else if (k==4)
							sprintf_s(num, sizeof(num), "/POSITION/ RIGHT\n");
						ux_fputs0(num, fd);
						temp1 = (float)(UDM_run_layout.error_size[0])/(float)uw_gl.dev_xmax;
						temp2 = (float)(UDM_run_layout.error_size[1])/(float)uw_gl.dev_ymax;
						sprintf_s(num, sizeof(num), "/SIZE/ %f,%f\n", temp1, temp2);
						ux_fputs0(num, fd);
					}
					else if (uw_ntis_statwinbar((CNCLDialogBar*)tempbar))
					{
						statwin_saved = 1;
						ux_fputs0("\n#WINDOW#\n",fd);
						ux_fputs0("/ACTIVE/ YES\n",fd);
						if (UDM_run_layout.statwin_att[0]==0)
							sprintf_s(num, sizeof(num), "/ATTACH/ TOP,");
						else
							sprintf_s(num, sizeof(num), "/ATTACH/ BOTTOM,");
						if (UDM_run_layout.statwin_att[1]==0)
							strcat_s(num, "LEFT\n");
						else
							strcat_s(num, "RIGHT\n");
						ux_fputs0(num, fd);
/*
.....find the menuarea name, first match
*/
						ud_find_menuarea(k, areaname);
						sprintf_s(num, sizeof(num), "/POSITION/ %s\n", areaname);
						ux_fputs0(num, fd);
						((CNCLDialogBar*)(NCL_MainFrame->m_statusBar))->GetWindowSize(cols, rows);			
						sprintf_s(num, sizeof(num), "/SIZE/ %d,%d\n", cols, rows);
						ux_fputs0(num, fd);
						if ((UDM_run_layout.statwin_doc[0]==0)
							&& (UDM_run_layout.statwin_doc[1]==0)
							&& (UDM_run_layout.statwin_doc[2]==0)
							&& (UDM_run_layout.statwin_doc[3]==0))
							sprintf_s(num, sizeof(num), "/DOCKABLE/ NO\n");
						else if ((UDM_run_layout.statwin_doc[0]==1)
							&& (UDM_run_layout.statwin_doc[1]==1)
							&& (UDM_run_layout.statwin_doc[2]==1)
							&& (UDM_run_layout.statwin_doc[3]==1))
							sprintf_s(num, sizeof(num), "/DOCKABLE/ YES\n");
						else
						{
							sprintf_s(num, sizeof(num), "/DOCKABLE/ ");
							if (UDM_run_layout.statwin_doc[0]==1)
								strcat_s(num, " TOP,");
							if (UDM_run_layout.statwin_doc[1]==1)
								strcat_s(num, " BOTTOM,");
							if (UDM_run_layout.statwin_doc[2]==1)
								strcat_s(num, " LEFT,");
							if (UDM_run_layout.statwin_doc[3]==1)
								strcat_s(num, " RIGHT");
							len = strlen(num);
							if ((len>1)&&(num[len-1]==','))
								num[len-1] = '\0';
							strcat_s(num, "\n");
						}
						ux_fputs0(num, fd);
						if (UDM_run_layout.statwin_type==1)
							sprintf_s(num, sizeof(num), "/STATIC/ NO\n");
						if (UDM_run_layout.statwin_type==0)
							sprintf_s(num, sizeof(num), "/STATIC/ YES\n");
						ux_fputs0(num, fd);
					}
				}
next:;
				indx++;
				tempbar = (CToolmenu* )uw_ntget_bars(k, indx);
			}
		}
/*
.....Save the floating error/command/prompt bar 
*/
		if ((NCL_MainFrame->m_errorBar!=NULL)&&((NCL_MainFrame->m_errorBar)->IsFloating()))
		{
			(NCL_MainFrame->m_errorBar)->GetWindowRect(&rect);			
			menu_size[0] = rect.Width();
			menu_size[1] = rect.Height() + 1;
			uw_ntadd_menuborder(&rect);
			rect1 = (LPRECT)&rect;
			menu_pos[0] = rect1->left;
			menu_pos[1] = rect1->top;
			ux_fputs0("\n#ERROR#\n",fd);
			ux_fputs0("/TYPE/ FLOATING\n",fd);
			temp1 = (float)(menu_pos[0]);
			temp2 = (float)(menu_pos[1]);
			temp1 = temp1/(float)uw_gl.dev_xmax;
			temp2 = temp2/(float)uw_gl.dev_ymax;
			temp3 = (float)(menu_size[0]);
			temp4 = (float)(menu_size[1]);			
			temp3 = temp3/(float)uw_gl.dev_xmax;
			temp4 = temp4/(float)uw_gl.dev_ymax;
			sprintf_s(num, sizeof(num), "/POSITION/ %f,%f\n", temp1, temp2);
			ux_fputs0(num, fd);
			sprintf_s(num, sizeof(num), "/SIZE/ %f,%f\n", temp3, temp4);
			ux_fputs0(num, fd);
		}
		if ((NCL_MainFrame->m_promptBar!=NULL)&&((NCL_MainFrame->m_promptBar)->IsFloating()))
		{
			(NCL_MainFrame->m_promptBar)->GetWindowRect(&rect);			
			menu_size[0] = rect.Width();
			menu_size[1] = rect.Height() + 1;
			uw_ntadd_menuborder(&rect);
			rect1 = (LPRECT)&rect;
			menu_pos[0] = rect1->left;
			menu_pos[1] = rect1->top;
			ux_fputs0("\n#PROMPTS#\n",fd);
			ux_fputs0("/TYPE/ FLOATING\n",fd);
			temp1 = (float)(menu_pos[0]);
			temp2 = (float)(menu_pos[1]);
			temp1 = temp1/(float)uw_gl.dev_xmax;
			temp2 = temp2/(float)uw_gl.dev_ymax;
			temp3 = (float)(menu_size[0]);
			temp4 = (float)(menu_size[1]);			
			temp3 = temp3/(float)uw_gl.dev_xmax;
			temp4 = temp4/(float)uw_gl.dev_ymax;
			sprintf_s(num, sizeof(num), "/POSITION/ %f,%f\n", temp1, temp2);
			ux_fputs0(num, fd);
			sprintf_s(num, sizeof(num), "/SIZE/ %f,%f\n", temp3, temp4);
			ux_fputs0(num, fd);
		}
		if ((NCL_MainFrame->m_commandBar!=NULL)&&((NCL_MainFrame->m_commandBar)->IsFloating()))
		{
			cmdwin_saved = 1;
			ux_fputs0("\n#COMMAND#\n",fd);
/*
.....ACTIVE should refer to current command window, not layout value
*/
/*			if (UDM_run_layout.command_active==1)
				sprintf_s(num, sizeof(num), "/ACTIVE/ MULTI");
			else
				sprintf_s(num, sizeof(num), "/ACTIVE/ SINGLE");
*/
			if (uw_cmd_extend())
				sprintf_s(num, sizeof(num), "/ACTIVE/ MULTI\n");
			else
				sprintf_s(num, sizeof(num), "/ACTIVE/ SINGLE\n");

			if (UDM_run_layout.command_att[0]==0)
				sprintf_s(num, sizeof(num), "/ATTACH/ TOP,");
			else
				sprintf_s(num, sizeof(num), "/ATTACH/ BOTTOM,");
			if (UDM_run_layout.command_att[1]==0)
				strcat_s(num, "LEFT\n");
			else
				strcat_s(num, "RIGHT\n");
			ux_fputs0(num, fd);
						
			(NCL_MainFrame->m_commandBar)->GetWindowRect(&rect);			
			NCL_Main_View->GetWindowRect(&rectW);			
			if (UDM_run_layout.command_att[0]==0)
				y = rect.top - rectW.top - GetSystemMetrics(SM_CYCAPTION)+1;
			else
				y = rect.bottom -  rectW.bottom+1;
			if (UDM_run_layout.command_att[1]==0)
				x = rect.left - rectW.left - 2;
			else
				x = rect.right - rectW.right - 2;
			sprintf_s(num, sizeof(num), "/POSITION/ %d,%d\n", x, y);
			ux_fputs0(num, fd);

			((CNCLCmdBar*)(NCL_MainFrame->m_commandBar))->GetWindowMultiSize(cx, cy, rows,1);			
			temp1 = (float)(cx)/(float)uw_gl.dev_xmax;
			temp2 = (float)(cy)/(float)uw_gl.dev_ymax;
			sprintf_s(num, sizeof(num), "/SIZE/ %f,%f,%d\n", temp1, temp2, rows);
			ux_fputs0(num, fd);
						
			if ((UDM_run_layout.command_doc[0]==0)
							&& (UDM_run_layout.command_doc[1]==0)
							&& (UDM_run_layout.command_doc[2]==0)
							&& (UDM_run_layout.command_doc[3]==0))
				sprintf_s(num, sizeof(num), "/DOCKABLE/ NO\n");
			else if ((UDM_run_layout.command_doc[0]==1)
							&& (UDM_run_layout.command_doc[1]==1)
							&& (UDM_run_layout.command_doc[2]==1)
							&& (UDM_run_layout.command_doc[3]==1))
				sprintf_s(num, sizeof(num), "/DOCKABLE/ YES\n");
			else
			{
				sprintf_s(num, sizeof(num), "/DOCKABLE/ ");
				if (UDM_run_layout.command_doc[0]==1)
					strcat_s(num, " TOP,");
				if (UDM_run_layout.command_doc[1]==1)
					strcat_s(num, " BOTTOM,");
				if (UDM_run_layout.command_doc[2]==1)
					strcat_s(num, " LEFT,");
				if (UDM_run_layout.command_doc[3]==1)
					strcat_s(num, " RIGHT");
				len = strlen(num);
				if ((len>1)&&(num[len-1]==','))
					num[len-1] = '\0';
				strcat_s(num, "\n");
			}
			ux_fputs0(num, fd);
		}
		if ((NCL_MainFrame->m_statusBar!=NULL) && ((NCL_MainFrame->m_statusBar)->IsFloating()))
		{
			statwin_saved = 1;
			ux_fputs0("\n#WINDOW#\n",fd);
			ux_fputs0("/ACTIVE/ YES\n",fd);
			if (UDM_run_layout.statwin_att[0]==0)
				sprintf_s(num, sizeof(num), "/ATTACH/ TOP,");
			else
				sprintf_s(num,sizeof(num),  "/ATTACH/ BOTTOM,");
			if (UDM_run_layout.statwin_att[1]==0)
				strcat_s(num, "LEFT\n");
			else
				strcat_s(num, "RIGHT\n");
			ux_fputs0(num, fd);

			(NCL_MainFrame->m_statusBar)->GetWindowRect(&rect);			
			NCL_Main_View->GetWindowRect(&rectW);			
			if (UDM_run_layout.statwin_att[0]==0)
				y = rect.top - rectW.top - GetSystemMetrics(SM_CYCAPTION)+1;
			else
				y = rect.bottom -  rectW.bottom+1;
			
			if (UDM_run_layout.statwin_att[1]==0)
				x = rect.left - rectW.left - 2;
			else
				x = rect.right - rectW.right - 2;

			sprintf_s(num, sizeof(num), "/POSITION/ %d,%d\n", x, y);
			ux_fputs0(num, fd);
			((CNCLDialogBar*)(NCL_MainFrame->m_statusBar))->GetWindowSize(cols, rows);			
			sprintf_s(num, sizeof(num), "/SIZE/ %d,%d\n", cols, rows);
			ux_fputs0(num, fd);
						
			if ((UDM_run_layout.statwin_doc[0]==0)
							&& (UDM_run_layout.statwin_doc[1]==0)
							&& (UDM_run_layout.statwin_doc[2]==0)
							&& (UDM_run_layout.statwin_doc[3]==0))
				sprintf_s(num, sizeof(num), "/DOCKABLE/ NO\n");
			else if ((UDM_run_layout.statwin_doc[0]==1)
							&& (UDM_run_layout.statwin_doc[1]==1)
							&& (UDM_run_layout.statwin_doc[2]==1)
							&& (UDM_run_layout.statwin_doc[3]==1))
				sprintf_s(num, sizeof(num), "/DOCKABLE/ YES\n");
			else
			{
				sprintf_s(num, sizeof(num), "/DOCKABLE/ ");
				if (UDM_run_layout.statwin_doc[0]==1)
					strcat_s(num, " TOP,");
				if (UDM_run_layout.statwin_doc[1]==1)
					strcat_s(num, " BOTTOM,");
				if (UDM_run_layout.statwin_doc[2]==1)
					strcat_s(num, " LEFT,");
				if (UDM_run_layout.statwin_doc[3]==1)
					strcat_s(num, " RIGHT");
				len = strlen(num);
				if ((len>1)&&(num[len-1]==','))
					num[len-1] = '\0';
				strcat_s(num, "\n");
			}
			ux_fputs0(num, fd);
			if (UDM_run_layout.statwin_type==1)
				sprintf_s(num, sizeof(num), "/STATIC/ NO\n");
			if (UDM_run_layout.statwin_type==0)
				sprintf_s(num, sizeof(num), "/STATIC/ YES\n");
			ux_fputs0(num, fd);
		}
/*
...save graphic layout
*/
		ux_fputs0("\n#GRAPHICS#\n",fd);
		NCL_MainFrame->GetWindowRect(&rect);			
		menu_size[0] = rect.Width();
		menu_size[1] = rect.Height();
		menu_pos[0] = rect.left;
		menu_pos[1] = rect.top;
		temp1 = (float)(menu_pos[0]);
		temp2 = (float)(menu_pos[1]);
		temp1 = temp1/(float)uw_gl.dev_xmax;
		temp2 = temp2/(float)uw_gl.dev_ymax;
		temp3 = (float)(menu_size[0]);
		temp4 = (float)(menu_size[1]);			
		temp3 = temp3/(float)uw_gl.dev_xmax;
		temp4 = temp4/(float)uw_gl.dev_ymax;
		sprintf_s(num, sizeof(num), "/POSITION/ %f,%f\n", temp1, temp2);
		ux_fputs0(num, fd);
		sprintf_s(num, sizeof(num),"/SIZE/ %f,%f\n", temp3, temp4);
		ux_fputs0(num, fd);
		if (statwin_saved != 1)
		{
			ux_fputs0("\n#WINDOW#\n",fd);
			ux_fputs0("/ACTIVE/ NO\n",fd);
			if (UDM_run_layout.statwin_att[0]==0)
				sprintf_s(num, sizeof(num), "/ATTACH/ TOP,");
			else
				sprintf_s(num, sizeof(num), "/ATTACH/ BOTTOM,");
			if (UDM_run_layout.statwin_att[1]==0)
				strcat_s(num, "LEFT\n");
			else
				strcat_s(num, "RIGHT\n");
			ux_fputs0(num, fd);
			if (UDM_layout.statwin_area[0]!='\0')
				sprintf_s(num, sizeof(num), "/POSITION/ %s\n", UDM_run_layout.statwin_area);
			else
				sprintf_s(num, sizeof(num), "/POSITION/ %d,%d\n", UDM_run_layout.statwin_pos[3], UDM_run_layout.statwin_pos[4]);
			ux_fputs0(num, fd);
			sprintf_s(num, sizeof(num), "/SIZE/ %d,%d\n", UDM_run_layout.statwin_size[0], UDM_run_layout.statwin_size[1]);
			ux_fputs0(num, fd);
			if ((UDM_run_layout.statwin_doc[0]==0)
							&& (UDM_run_layout.statwin_doc[1]==0)
							&& (UDM_run_layout.statwin_doc[2]==0)
							&& (UDM_run_layout.statwin_doc[3]==0))
				sprintf_s(num, sizeof(num), "/DOCKABLE/ NO\n");
			else if ((UDM_run_layout.statwin_doc[0]==1)
							&& (UDM_run_layout.statwin_doc[1]==1)
							&& (UDM_run_layout.statwin_doc[2]==1)
							&& (UDM_run_layout.statwin_doc[3]==1))
				sprintf_s(num, sizeof(num), "/DOCKABLE/ YES\n");
			else
			{
				sprintf_s(num, sizeof(num), "/DOCKABLE/ ");
				if (UDM_run_layout.statwin_doc[0]==1)
					strcat_s(num, " TOP,");
				if (UDM_run_layout.statwin_doc[1]==1)
					strcat_s(num, " BOTTOM,");
				if (UDM_run_layout.statwin_doc[2]==1)
					strcat_s(num, " LEFT,");
				if (UDM_run_layout.statwin_doc[3]==1)
					strcat_s(num, " RIGHT");
				strcat_s(num, "\n");
			}
			ux_fputs0(num, fd);
			if (UDM_run_layout.statwin_type==1)
				sprintf_s(num, sizeof(num), "/STATIC/ NO\n");
			if (UDM_run_layout.statwin_type==0)
				sprintf_s(num, sizeof(num), "/STATIC/ YES\n");
			ux_fputs0(num, fd);
		}
		ux_close(layout, UX_PRTERRS);
	}
	return (UU_TRUE);
}
/**************************************************************************
**
**  E_FUNCTION         :  uw_ntload_layout(infile)
**     load the layout from a layout file 
**		 	
**  PARAMETERS   
**      INPUT  : 
**          infile = Name of file to load, or blank if the file should be
**                   prompted for.
**      OUTPUT :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/
extern "C" int uw_ntload_layout(char *infile)
{
	UX_pathname filename, dir, fname;
	int i, j, status;
	int len;
	CMainFrame *MainWnd = (CMainFrame*)(AfxGetMainWnd());
/*
.....The filename is provided
*/
	if (infile[0] != '\0')
	{
		strcpy_s(fname,sizeof(fname), infile);
	}
/*
.....Get the filename
*/
	else
	{
		fname[0] = '\0';
		ud_get_filename(NULL,"Load Layout File","*.lay", fname, &len,
			"NCL Layout File (*.lay)", 1);
/*
...check if we get the file name
*/
		if((fname[len-1]=='\\')||(fname[0]=='\0'))
			return -1;
		strcpy_s(infile, len+1, fname);
	}
/* 
...Check for file existence 
*/
	status = ul_open_mod_file("UU_USER_SETTINGS", "layout", "M_UD_LAYOUT_DIR", (char*)UU_NULL,
		fname, 0, (FILE**)UU_NULL);
	if ((status == UU_SUCCESS) && (fname[0]!='\0'))
	{
/*
......destroy the displayed layout, 
......but not graphic window, only adjust size of it
*/
/*
.....take down all menus
*/
		CMenu *bmenu = NCL_MainFrame->GetMenu();
/*
.....at most 20 menu for menubar
*/
		for (i=0; i<20; i++)
		{
			status = bmenu->DeleteMenu(0, MF_BYPOSITION);
			if (status==0)
				break;
		}

		for (i=0;i<UDM_menu_count;i++)
		{
			if (NCL_menu[i]!=NULL)
			{
				delete NCL_menu[i];
				NCL_menu[i] = NULL;
			}

			if (NCL_MainFrame->NCL_menubar[i]!=NULL)
			{
				delete NCL_MainFrame->NCL_menubar[i];
				NCL_MainFrame->NCL_menubar[i] = NULL;
			}
		}
		if (NCL_MainFrame->m_commandBar!=NULL)
		{
			delete NCL_MainFrame->m_commandBar;
			NCL_MainFrame->m_commandBar = NULL;
		}
		if (NCL_MainFrame->m_promptBar!=NULL)
		{
			delete NCL_MainFrame->m_promptBar;
			NCL_MainFrame->m_promptBar = NULL;
		}
		if (NCL_MainFrame->m_errorBar!=NULL)
		{
			delete NCL_MainFrame->m_errorBar;
			NCL_MainFrame->m_errorBar = NULL;
		}
		if (NCL_MainFrame->m_statusBar!=NULL)
		{
			delete NCL_MainFrame->m_statusBar;
			NCL_MainFrame->m_statusBar = NULL;
			ncl_set_statact(0);
		}
/*
...if Menu Design is active, destroy it
*/
//		uw_mfremove_desgn_menu();
		frm_done = 0;
		UZ_item_count = 0;
		UZ_menu_count = 0;
		UW_reset_menu = 1;
/*
...read layout file
*/
//////		ux_strip_quotes(fullname);
		udm_read_layout(&UD_duimsdeflt, fname); 	
/*
...we need display all the Interface layout again
*/
		MainWnd->MoveWindow(UDM_layout.window_pos[0],UDM_layout.window_pos[1], 
			UDM_layout.window_size[0],UDM_layout.window_size[1]);
/*
......redisplay status bar
*/
		NCL_MainFrame->ReDisplayStatusBar();

		if (UDM_run_layout.error_type==1)
			uw_nterror_area(1);
		else
			uw_nterror_area(0);

		if (UDM_run_layout.prompt_type==1)
			uw_ntprompt_area(1);
		else
			uw_ntprompt_area(0);

		if (UDM_run_layout.command_type==1)
			uw_ntcommand_areabar(1);
		else
			uw_ntcommand_areabar(0);
		CRect rect;
		UW_reset_menu = 1;
/*
.....Create menus
*/
		for (i=0;i<UDM_layout.nmenu;i++)
		{
			if (UDM_layout.menu_name[i][0]=='\0')
			continue;
			for (j=0;j<UDM_menu_count;j++)
			{
				strcpy_s(filename, sizeof(filename), UDM_menu[j].file);
				ul_break_fname(filename,dir,fname);

				if (strcmp(fname, UDM_layout.menu_name[i])==0)
				{
					if (UDM_menu[j].type == UDM_MTYPE_ICON)
						uw_ntmenu(j, 0);
					else
						uw_ntmenu(j, 1);
				}
			}
		}
/*
......show ICON menu and bars
*/
		uw_ntshow_dir(1);
		uw_ntshow_dir(2);
		uw_ntshow_dir(3);
		uw_ntshow_dir(4);
/*
.....Trap Resize events
*/
		NCL_cmdwin = NCL_MainFrame->m_commandBar->getcmdwin();
		NCL_MainFrame->Enable_cmdbar(0);

		int wid, hgt;
		uw_ntget_gwsize(&wid, &hgt);	
				
		int clip[4];
		clip[0] = 0;
		clip[1] = 0;
		clip[2] = wid;
		clip[3] = hgt;
		uw_glset_scissor(clip);
		uw_glresize_graphics(wid, hgt, 1);

		znu_copyright();
		uz_status();
		frm_done = 1;
		UW_reset_menu = 0;
/*
......copy UDM_layout into UDM_run_layout
*/
		uu_move_byte((char*)&UDM_layout, (char*)&UDM_run_layout,
								sizeof(UDM_LAYOUT));
	}
	else
/*
...Can not open file
*/
	{
		uw_nterror("Can't open file");
		return -1;
	}
	return 0;
}
/*************************************************************************
**
**  E_FUNCTION         :  uw_ntsave_layout(infile)
**     select and save the layout into a layout file 
**
**  PARAMETERS   
**      INPUT  : 
**          infile = Name of file to save, or blank if the file should be
**                   prompted for.
**      OUTPUT :  
**          none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

extern "C" void uw_ntsave_layout(char *infile)
{
	UX_pathname fname;
	char msg[UX_MAX_PATH_LEN+40],*tempfile;
	int status;
	int len, ans;
	char *pathlistptr = 0;
/*
.....The filename is provided
*/
	if (infile[0] != '\0')
	{
		strcpy_s(fname,sizeof(fname), infile);
	}
/*
.....Get the filename
*/
	else
	{
/*
.....default the saving directory to UU_USER_SETTING directory, if this
.....directory is not there, create one
*/
		fname[0] = '\0';
		status = ul_open_mod_file("UU_USER_SETTINGS", "layout", (char*)UU_NULL, (char*)UU_NULL,
			fname, 1, (FILE**)UU_NULL);
		if ((status == UU_SUCCESS) && (fname[0]!='\0'))
			strcat_s (fname, "\\*.lay");
		ud_get_filename(NULL, "Save Layout File","*.lay", fname, &len,
			"NCL Layout File (*.lay)", 0);
/* 
...Check for file existence 
*/
		if((fname[len-1]=='\\')||(fname[0]=='\0'))
			return; 
	}
/*
.....open for read to check if file exist
*/
	status = ul_open_mod_file("UU_USER_SETTINGS", "layout", (char*)UU_NULL, (char*)UU_NULL,
		fname, 0, (FILE**)UU_NULL);
/*
...file exists, overwrite (y/n)?
*/
	if ((status == UU_SUCCESS) && (fname[0]!='\0') && !ul_session_active())
	{
		sprintf_s(msg, sizeof(msg), "File %s exists, overwrite?", fname);
		ans = uw_ntyes_or_no(NULL, msg, "Overwrite file?");
		if (ans!=1)
			return;
	}
/*
.....Create a temporary layout file
*/
	UX_pathname fulldir;
	tempfile = (char*)malloc(UX_MAX_PATH_LEN*sizeof(char));
	ul_get_full_dir ("HOMEDIR", fulldir);
	tempfile = _tempnam(fulldir, "lay");
/*
.....write success, copy temp file to layout file
*/
	if (uw_ntwrite_layout(tempfile))
	{
		if (fname[0]!='\0')
			remove(fname);
		status = rename(tempfile, fname);
		free(tempfile);
		return;
	}
	else
	{
		remove(tempfile);
		free(tempfile);
		return;
	}
}
	
#endif

