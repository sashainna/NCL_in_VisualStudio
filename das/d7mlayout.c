#include "usysdef.h"
#if UU_COMP != UU_VAXVMS || UU_OPSYS == UU_ALPHAVMS

/*********************************************************************
**      FILENAME: d7mlayout.c
**      CONTAINS:    udm_init_layout
**                   udm_playout_menu
**                   udm_playout_graphic
**                   udm_playout_prompt
**                   udm_playout_status
**                   udm_playout_directory
**                   udm_resize_graphics
**                   udm_signoff
**                   udm_signon_load
**                   ud_get_areadockable
**                   ud_getpos_from_bararea
**                   udm_read_layout
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       d7mlayout.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:14
*********************************************************************/
#if UU_COMP!=UU_WIN2K
#include <Xm/Xm.h>
#endif

#include "ustdio.h"
#include "xfsys1.h"
#include "driver.h"
#include "dmotif.h"
#include "gtbl.h"
#include "gdidd.h"
#include "uims.h"
#include "wsgl.h"
#if UU_COMP!=UU_WIN2K
#include "wsxw.h"
#endif
#include "xenv1.h"

#ifdef UU_OPENGL
extern UG_wdt glwdt;
#endif

/*********************************************************************
**       I_FUNCTION : udm_init_layout()
**                      This function initializes the layout structure.
**       PARAMETERS     
**               INPUT  :  none
**               OUTPUT :  none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
udm_init_layout()
{
	int i;

	for (i=0;i<MAX_EDGE_BAR;i++) 
	{
		UDM_bararea[i].flag = 0;
		UDM_bararea[i].indx = -1;
	}
	UDM_barnum = 0;
	UDM_layout.nmenu = 0;
	for (i=0;i<UDM_MAX_MENU;i++) 
	{
		UDM_layout.menu_pos[i][0] = -1;
		UDM_layout.menu_size[i][0] = -1;
		UDM_layout.menu_name[i][0] = '\0';
#if UU_COMP==UU_WIN2K
		UDM_layout.menu_area[i][0] = '\0';
		UDM_layout.menu_pos[i][1] = -1;
		UDM_layout.menu_pos[i][2] = -1;
		UDM_layout.menu_pos[i][3] = -1;
		UDM_layout.menu_pos[i][4] = -1;
		UDM_layout.menu_size[i][1] = -1;
		UDM_layout.area_dockable[i] = UDM_AREA_FRONTDOCK;
#endif
	}
#if UU_COMP!=UU_WIN2K
	UDM_layout.graphic_pos[0] = -1;
	UDM_layout.status_pos[0] = -1;
	UDM_layout.prompt_pos[0] = -1;
#else
	UDM_layout.window_pos[0] = -1;
	UDM_layout.command_type = 0;
	UDM_layout.command_pos[0] = -1;
	UDM_layout.command_pos[1] = -1;
	UDM_layout.command_pos[2] = -1;
	UDM_layout.command_pos[3] = -1;
	UDM_layout.command_pos[4] = -1;
	UDM_layout.command_size[0] = 0.4;
	UDM_layout.command_size[1] = 0.05;
	UDM_layout.command_size[2] = 5;
/*
....default current color is yellow, active color is pink
*/
	UDM_layout.command_clr[0] = 13;
	UDM_layout.command_clr[1] = 6;

	UDM_layout.command_load[0] = 500;
	UDM_layout.command_load[1] = 100;

	UDM_layout.command_active = 0;
	UDM_layout.command_area[0] = '\0';

	UDM_layout.command_att[0] = 0;
	UDM_layout.command_att[1] = 0;
/*
......default to TOP
*/
	UDM_layout.command_doc[0] = -1;
	UDM_layout.command_doc[1] = 0;
	UDM_layout.command_doc[2] = 0;
	UDM_layout.command_doc[3] = 0;


	UDM_layout.error_type = 0;
	UDM_layout.error_pos[0] = -1;
	UDM_layout.error_pos[1] = -1;
	UDM_layout.error_pos[2] = -1;
	UDM_layout.error_pos[3] = -1;
	UDM_layout.error_pos[4] = -1;
	UDM_layout.error_size[0] = -1;
	UDM_layout.error_size[1] = -1;
	UDM_layout.prompt_type = 0;
	UDM_layout.prompt_pos[0] = -1;
	UDM_layout.prompt_pos[1] = -1;
	UDM_layout.prompt_pos[2] = -1;
	UDM_layout.prompt_pos[3] = -1;
	UDM_layout.prompt_pos[4] = -1;
	UDM_layout.prompt_size[0] = -1;
	UDM_layout.prompt_size[1] = -1;
	UDM_layout.statwin_pos[0] = 0;
	UDM_layout.statwin_pos[1] = 0;
	UDM_layout.statwin_pos[2] = 0;
	UDM_layout.statwin_pos[3] = 0;
	UDM_layout.statwin_pos[4] = 0;
	UDM_layout.statwin_size[0] = 80;
	UDM_layout.statwin_size[1] = 20;
	UDM_layout.statwin_type = 1;
	UDM_layout.statwin_active = 0;
	UDM_layout.statwin_area[0] = '\0';
	UDM_layout.statwin_att[0] = UDM_layout.statwin_att[1] = -1;
	UDM_layout.statwin_doc[0] = 0;
	UDM_layout.statwin_doc[1] = -1;
	UDM_layout.statwin_doc[2] = 0;
	UDM_layout.statwin_doc[3] = 0;
	UDM_layout.menubar[0] = '\0';
	UDM_layout.statbar_file[0][0] = '\0';
	UDM_layout.statbar_file[1][0] = '\0';
	UDM_layout.statbar_no = 0;
#endif
	UDM_layout.maximize = 0;
	return 0;
}

/*********************************************************************
**       I_FUNCTION : udm_playout_menu(ctyp,cmsg)
**                      This function defines the default menus.
**       PARAMETERS     
**               INPUT  :  ctyp = Menu name begin defined.
**               cmsg = Menu filename.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int udm_playout_menu (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,status,inum;
	int maxsub=1;
	char buf[80],*p, msg[256];
	static char csub[1][20] = {"MENU"};
	UU_REAL rval[4];
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid MENU modal.  /%s/ %s\n",ctyp,cmsg);
		ud_printmsg(msg);
		goto failed;
	}
	switch(i)
	{
/*
.....MENU
*/
	case 0:
		p = (char *)index(cmsg,',');
		if (p != 0)
		{
			*p = '\0';
			strcpy(buf,++p);
			if ((ul_to_reals(rval,&inum,4,buf) != UU_SUCCESS) ||
				inum <2) goto bad_parm;
			{
/*
...add option menu size in layout file
...Yurong
*/
#if UU_COMP!=UU_WIN2K
				if(inum==2)
				{
					UDM_layout.menu_pos[UDM_layout.nmenu][0] = 
														(int)(rval[0] * uw_xw.dev_xmax);
					UDM_layout.menu_pos[UDM_layout.nmenu][1] = 
														(int)(rval[1] * uw_xw.dev_ymax);
				}
				else
				{
					UDM_layout.menu_pos[UDM_layout.nmenu][0] = 
														(int)(rval[0] * uw_xw.dev_xmax);
					UDM_layout.menu_pos[UDM_layout.nmenu][1] = 
														(int)(rval[1] * uw_xw.dev_ymax);
					UDM_layout.menu_size[UDM_layout.nmenu][0] = 
														(int)(rval[2] * uw_xw.dev_xmax);
					UDM_layout.menu_size[UDM_layout.nmenu][1] = 
														(int)(rval[3] * uw_xw.dev_ymax);
				}
#else
				if(inum==2)
				{
					UDM_layout.menu_pos[UDM_layout.nmenu][0] = 
														(int)(rval[0] * uw_gl.dev_xmax);
					UDM_layout.menu_pos[UDM_layout.nmenu][1] = 
														(int)(rval[1] * uw_gl.dev_ymax);
				}
				else
				{
					UDM_layout.menu_pos[UDM_layout.nmenu][0] = 
														(int)(rval[0] * uw_gl.dev_xmax);
					UDM_layout.menu_pos[UDM_layout.nmenu][1] = 
														(int)(rval[1] * uw_gl.dev_ymax);
					UDM_layout.menu_size[UDM_layout.nmenu][0] = 
														(int)(rval[2] * uw_gl.dev_xmax);
					UDM_layout.menu_size[UDM_layout.nmenu][1] = 
														(int)(rval[3] * uw_gl.dev_ymax);
				}
#endif
			}
		}
		strcpy(UDM_layout.menu_name[UDM_layout.nmenu],cmsg);
		break;
	}

#if UU_COMP!=UU_WIN2K
	UDM_layout.nmenu++; 
#endif
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (msg, "Invalid value for MENU modal. /%s/ %s\n",ctyp,cmsg);
	ud_printmsg(msg);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	 return(status);
}


#if UU_COMP==UU_WIN2K
static int udm_playout_menuarea (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,status,inum;
	char msg[256];
	int maxsub=5;
	static char csub[5][20] = {"POSITION","SIZE", "NAME", "MENU", "DOCKABLE"};
	UU_REAL rval[2];
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid MENUS modal.  /%s/ %s\n", ctyp,cmsg);
		ud_printmsg(msg);
		goto failed;
	}
	switch(i)
	{
/*
.....Position
*/
	case 0:
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "TOP")==0)
			UDM_layout.menu_pos[UDM_layout.nmenu][2] = 1;
		else if (strcmp(cmsg, "BOTTOM")==0)
			UDM_layout.menu_pos[UDM_layout.nmenu][2] = 2;
		else if (strcmp(cmsg, "LEFT")==0)
			UDM_layout.menu_pos[UDM_layout.nmenu][2] = 3;
		else if (strcmp(cmsg, "RIGHT")==0)
			UDM_layout.menu_pos[UDM_layout.nmenu][2] = 4;
		else
			UDM_layout.menu_pos[UDM_layout.nmenu][2] = 1;
		break;
/*
.....SIZE
*/
	case 1:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		UDM_layout.menu_size[UDM_layout.nmenu][0] = (int)(rval[0] * uw_gl.dev_xmax);
		UDM_layout.menu_size[UDM_layout.nmenu][1] = (int)(rval[1] * uw_gl.dev_ymax);
		break;
/*
.....NAME
*/
	case 2:
		strcpy(UDM_layout.menu_area[UDM_layout.nmenu], cmsg);
		break;
/*
......MENU
*/
	case 3:
		strcpy(UDM_layout.menu_name[UDM_layout.nmenu], cmsg);
		if ((UDM_barnum>=1)
			&&(UDM_bararea[UDM_barnum-1].flag==1))
		{
			UDM_bararea[UDM_barnum-1].dir = UDM_layout.menu_pos[UDM_layout.nmenu][2];
			UDM_bararea[UDM_barnum-1].indx = UDM_layout.nmenu;
		}
/*		UDM_layout.nmenu++;   */
		break;
/*
......MENUAREA
*/
	case 4:
		if (strcmp(cmsg,"NO")==0)
			UDM_layout.area_dockable[UDM_layout.nmenu] = UDM_AREA_NODOCK;
		else if (strcmp(cmsg,"FRONT")==0)
			UDM_layout.area_dockable[UDM_layout.nmenu] = UDM_AREA_FRONTDOCK;
		else if (strcmp(cmsg,"END")==0)
			UDM_layout.area_dockable[UDM_layout.nmenu] = UDM_AREA_ENDDOCK;
		else
			UDM_layout.area_dockable[UDM_layout.nmenu] = UDM_AREA_FRONTDOCK;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (msg, "Invalid value for MENUS modal. /%s/ %s\n",ctyp,cmsg);
	ud_printmsg(msg);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	if (status == UU_FAILURE)
	{
		UDM_bararea[UDM_barnum-1].flag = -1;
	}
	return(status);
}

#endif

/*********************************************************************
**       I_FUNCTION : udm_playout_graphic(ctyp,cmsg)
**                      This function defines the Graphics window.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int udm_playout_graphic (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,status,inum;
	char msg[256];
	int maxsub=3;
	static char csub[3][20] = {"POSITION","SIZE", "MAXIMIZE"};
	UU_REAL rval[2];
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid GRAPHIC modal.  /%s/ %s\n",ctyp,cmsg);
		ud_printmsg(msg);
		goto failed;
	}
	switch(i)
	{
/*
.....Position
*/
	case 0:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
#if UU_COMP!=UU_WIN2K
		UDM_layout.graphic_pos[0] = (int)(rval[0] * uw_xw.dev_xmax);
		UDM_layout.graphic_pos[1] = (int)(rval[1] * uw_xw.dev_ymax);
#else
		UDM_layout.window_pos[0] = (int)(rval[0] * uw_gl.dev_xmax);
		UDM_layout.window_pos[1] = (int)(rval[1] * uw_gl.dev_ymax);
#endif
		break;
/*
.....Size
*/
	case 1:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
#if UU_COMP!=UU_WIN2K
		UDM_layout.graphic_size[0] = (int)(rval[0] * uw_xw.dev_xmax);
		UDM_layout.graphic_size[1] = (int)(rval[1] * uw_xw.dev_ymax);
#else
		UDM_layout.window_size[0] = (int)(rval[0] * uw_gl.dev_xmax);
		UDM_layout.window_size[1] = (int)(rval[1] * uw_gl.dev_ymax);
#endif
		break;
	case 2:
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "YES")==0)
			UDM_layout.maximize = 1;
		else
			UDM_layout.maximize = 0;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (msg, "Invalid value for GRAPHIC modal. /%s/ %s\n",ctyp,cmsg);
	ud_printmsg(msg);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}
/*********************************************************************
**       I_FUNCTION : udm_playout_prompt(ctyp,cmsg)
**                      This function defines the Prompt window.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
#if UU_COMP!=UU_WIN2K
static int udm_playout_prompt (ctyp,cmsg)
char *ctyp,*cmsg;

{
	int i,status,inum;
	char msg[256];
	int maxsub=2;
	static char csub[2][20] = {"POSITION","SIZE"};
	UU_REAL rval[2];
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf ("Not a valid PROMPT modal.  /%s/ %s\n", ctyp,cmsg);
		ud_printmsg(msg);
		goto failed;
	}
	switch(i)
	{
/*
.....Position
*/
	case 0:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		UDM_layout.prompt_pos[0] = rval[0] * uw_xw.dev_xmax;
		UDM_layout.prompt_pos[1] = rval[1] * uw_xw.dev_ymax;
		break;
/*
.....Size
*/
	case 1:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		UDM_layout.prompt_size[0] = rval[0] * uw_xw.dev_xmax;
		UDM_layout.prompt_size[1] = rval[1] * uw_xw.dev_ymax;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (msg, "Invalid value for PROMPT modal. /%s/ %s\n",ctyp,cmsg);
	ud_printmsg(msg);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}
#endif

#if UU_COMP==UU_WIN2K
static int udm_playout_prompt (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,status,inum;
	char msg[256];
	int maxsub=3;
	static char csub[3][20] = {"POSITION","SIZE", "TYPE"};
	UU_REAL rval[2];
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid PROMPT modal.  /%s/ %s\n", ctyp,cmsg);
		uw_nterror (msg);
		goto failed;
	}
	switch(i)
	{
/*
.....Position
*/
	case 0:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) 
		{
			ul_to_upper(cmsg);
			if (strcmp(cmsg, "TOP")==0)
				UDM_layout.prompt_pos[2] = 1;
			else if (strcmp(cmsg, "BOTTOM")==0)
				UDM_layout.prompt_pos[2] = 2;
			else if (strcmp(cmsg, "LEFT")==0)
				UDM_layout.prompt_pos[2] = 3;
			else if (strcmp(cmsg, "RIGHT")==0)
				UDM_layout.prompt_pos[2] = 4;
			else
				UDM_layout.prompt_pos[2] = 1;
		}
		else
		{
			UDM_layout.prompt_pos[3] = (int)(rval[0] * uw_gl.dev_xmax);
			UDM_layout.prompt_pos[4] = (int)(rval[1] * uw_gl.dev_ymax);
		}
		break;
/*
.....Size
*/
	case 1:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		UDM_layout.prompt_size[0] = (int)(rval[0] * uw_gl.dev_xmax);
		UDM_layout.prompt_size[1] = (int)(rval[1] * uw_gl.dev_ymax);
		break;
	case 2:
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "ICON")==0)
			UDM_layout.prompt_type = 0;
		else if (strcmp(cmsg, "FLOATING")==0)
			UDM_layout.prompt_type = 1;
		else
			UDM_layout.prompt_type = 0;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (msg, "Invalid value for PROMPT modal. /%s/ %s\n",ctyp,cmsg);
	uw_nterror (msg);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	if (status == UU_FAILURE)
	{
		UDM_bararea[UDM_barnum-1].flag = -1;
	}
	else if ((UDM_barnum>=1)
		&&(UDM_bararea[UDM_barnum-1].flag==3))
	{
		UDM_bararea[UDM_barnum-1].dir = UDM_layout.prompt_pos[2];
		UDM_bararea[UDM_barnum-1].indx = 0;
	}
	return(status);
}
#endif


/*********************************************************************
**       I_FUNCTION : udm_playout_status(ctyp,cmsg)
**                      This function defines the Status window.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
#if UU_COMP!=UU_WIN2K
static int udm_playout_status (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,status,inum;
	char msg[256];
	int maxsub=2;
	static char csub[2][20] = {"POSITION","SIZE"};
	UU_REAL rval[2];
#if UU_COMP!=UU_WIN2K
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid STATUS modal.  /%s/ %s\n", ctyp,cmsg);
		ud_printmsg(msg);
		goto failed;
	}
	switch(i)
	{
/*
.....Position
*/
	case 0:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		UDM_layout.status_pos[0] = rval[0] * uw_xw.dev_xmax;
		UDM_layout.status_pos[1] = rval[1] * uw_xw.dev_ymax;
		break;
/*
.....Size
*/
	case 1:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		UDM_layout.status_size[0] = rval[0] * uw_xw.dev_xmax;
		UDM_layout.status_size[1] = rval[1] * uw_xw.dev_ymax;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (msg, "Invalid value for STATUS modal. /%s/ %s\n",ctyp,cmsg);
	ud_printmsg(msg);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
#endif
	return(status);
}
#endif


/*********************************************************************
**       I_FUNCTION : udm_playout_directory(ctyp,cmsg)
**                      This function defines the default menu directory.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int udm_playout_directory (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,status;
	char msg[256];
	int maxsub=2;
	static char csub[2][20] = {"MENU","BITMAP"};
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid DIRECTORY modal.  /%s/ %s\n", ctyp,cmsg);
		ud_printmsg(msg);
		goto failed;
	}
	switch(i)
	{
/*
.....MENU Directory
*/
	case 0:
		if (ux_modenv("replace","NCL_MENU",cmsg,UX_NPRTERRS) == UX_FAILURE)
			ux_modenv("add","NCL_MENU",cmsg,UX_NPRTERRS);
		break;
/*
.....BITMAP Directory
*/
	case 1:
		if (ux_modenv("replace","NCL_BITMAP",cmsg,UX_NPRTERRS) == UX_FAILURE)
			ux_modenv("add","NCL_BITMAP",cmsg,UX_NPRTERRS);
		break;
	}
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : udm_playout_title(ctyp,cmsg)
**             This function defines the default window title format.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int udm_playout_title (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,status;
	char msg[256];
/*
.....Get modal to define
*/
	ul_to_upper(ctyp);
	if (strcmp(ctyp, "NAME") != 0)
	{
		sprintf (msg, "Not a valid TITLE modal.  /%s/ %s\n", ctyp,cmsg);
		ud_printmsg(msg);
		goto failed;
	}
	strcpy(UD_window_title, cmsg);
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

/*********************************************************************
**       I_FUNCTION : udm_resize_graphics(duims,wid,hgt,initial)
**                      This function defines the Status window.
**       PARAMETERS     
**               INPUT  :  uims     = DAS layout structure.
**               wid      = Width of new graphics window.
**               hgt      = Height of new graphics window.
**               initial  = True = initial definition of Graphics Window.
**               OUTPUT :  none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
udm_resize_graphics(duims,wid,hgt,initial)
UD_UIMS *duims;
int wid,hgt;
{
	int narea,i;
	UU_REAL rx,ry;
	UD_AREA *areapt;
	Gdrect *windpt;
/*
.....Set UIMS layout variables
........Number of screens
*/
	if (initial == 1)
	{
		(*duims).nscreens = 1;
	}
/*
........Screen size
*/
	windpt = &(*duims).screen[0].wswind;
	(*windpt).ll.x = 0;
	(*windpt).ll.y = 0;
	rx = wid; ry = hgt;
	if (wid > hgt)
	{
		(*windpt).ur.x = 1;
		(*windpt).ur.y = ry / rx;
	}
	else
	{
		(*windpt).ur.y = 1;
		(*windpt).ur.x = rx / ry;
	}
/*
........Graphic area
*/
	narea = 1;
	(*duims).screen[0].noareas[0] = narea;
	if (initial == 1)
	{
		areapt = (UD_AREA *)uu_toolmalloc(sizeof(UD_AREA)*narea);
		(*duims).screen[0].areas[0] = areapt;
	}
	else
	{
		areapt = (*duims).screen[0].areas[0];
	}
	for (i=0;i<narea;i++)
	{
		areapt[i].color = 0;
		areapt[i].bordercolor = 1;
		areapt[i].contcolor = 1;
		areapt[i].posn.ll.x = 0;
		areapt[i].posn.ll.y = 0;
		areapt[i].posn.ur.x = (*windpt).ur.x;
		areapt[i].posn.ur.y = (*windpt).ur.y;
	}
/*
........Rest of areas
*/
	if (initial == 1)
	{
		areapt = (UD_AREA *)uu_toolmalloc(sizeof(UD_AREA));
		areapt[0].posn.ll.x = 0;
		areapt[0].posn.ll.y = 0;
		areapt[0].posn.ur.x = (*windpt).ur.x;
		areapt[0].posn.ur.y = (*windpt).ur.y;
		for (i=1;i<UD_NTYPES;i++)
		{
			(*duims).screen[0].noareas[i] = 0;
			(*duims).screen[0].areas[i] = areapt;
		}
	}
/*
.....Set the workstation Xform matrix
*/
#ifdef UU_OPENGL
	uw_gl.xpixels = wid;
	uw_gl.ypixels = hgt;
	glwdt.dspsize.raster.x = wid;
	glwdt.dspsize.raster.y = hgt;
	if(wid>hgt)
		uw_gl.wsxform.sf=(GLdouble)wid;
	else
		uw_gl.wsxform.sf=(GLdouble)hgt;
	glwdt.dspsize.device.x = 1;
	glwdt.dspsize.device.y = (float)hgt/(float)wid;
	if (glwdt.dspsize.device.y>1)
	{
		glwdt.dspsize.device.x = 1/glwdt.dspsize.device.y;
		glwdt.dspsize.device.y = 1.0;
	}
	uw_glclip.urb.x = glwdt.dspsize.device.x;
	uw_glclip.urb.y = glwdt.dspsize.device.y;
#endif
	return 0;
}

/*********************************************************************
**       I_FUNCTION : udm_playout_error (ctyp,cmsg)
**                      This function defines the error window.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
#if UU_COMP==UU_WIN2K
static int udm_playout_error (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,status,inum;
	char msg[256];
	int maxsub=3;
	static char csub[3][20] = {"POSITION","SIZE", "TYPE"};
	UU_REAL rval[2];
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid ERROR modal.  /%s/ %s\n", ctyp,cmsg);
		uw_nterror (msg);
		goto failed;
	}
	switch(i)
	{
/*
.....Position
*/
	case 0:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) 
		{
			ul_to_upper(cmsg);
			if (strcmp(cmsg, "TOP")==0)
				UDM_layout.error_pos[2] = 1;
			else if (strcmp(cmsg, "BOTTOM")==0)
				UDM_layout.error_pos[2] = 2;
			else if (strcmp(cmsg, "LEFT")==0)
				UDM_layout.error_pos[2] = 3;
			else if (strcmp(cmsg, "RIGHT")==0)
				UDM_layout.error_pos[2] = 4;
			else
				UDM_layout.error_pos[2] = 1;
		}
		else
		{
			UDM_layout.error_pos[3] = (int)(rval[0] * uw_gl.dev_xmax);
			UDM_layout.error_pos[4] = (int)(rval[1] * uw_gl.dev_ymax);
		}
		break;
/*
.....Size
*/
	case 1:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		UDM_layout.error_size[0] = (int)(rval[0] * uw_gl.dev_xmax);
		UDM_layout.error_size[1] = (int)(rval[1] * uw_gl.dev_ymax);
		break;
	case 2:
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "ICON")==0)
			UDM_layout.error_type = 0;
		else if (strcmp(cmsg, "FLOATING")==0)
			UDM_layout.error_type = 1;
		else
			UDM_layout.error_type = 0;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (msg, "Invalid value for ERROR modal. /%s/ %s\n",ctyp,cmsg);
	uw_nterror (msg);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	if (status == UU_FAILURE)
	{
		UDM_bararea[UDM_barnum-1].flag = -1;
	}
	else if ((UDM_barnum>=1)
		&&(UDM_bararea[UDM_barnum-1].flag==5))
	{
		UDM_bararea[UDM_barnum-1].dir = UDM_layout.error_pos[2];
		UDM_bararea[UDM_barnum-1].indx = 0;
	}
	return(status);
}
#endif
/*********************************************************************
**       I_FUNCTION : udm_playout_statwin (ctyp,cmsg)
**                      This function defines the status text window.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
#if UU_COMP==UU_WIN2K
static int udm_playout_statwin (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,status,inum;
	char msg[256];
	char *tok;
	int dir = 1;
	int maxsub=6;
	static char csub[6][20] = {"POSITION","SIZE", "STATIC", "ACTIVE", "DOCKABLE",
						"ATTACH"};
	UU_REAL rval[2];
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid WINDOW modal.  /%s/ %s\n", ctyp,cmsg);
		uw_nterror (msg);
		goto failed;
	}
	switch(i)
	{
/*
.....Position
*/
	case 0:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) 
		{
			strcpy(UDM_layout.statwin_area, cmsg);
			dir = ud_find_menuarea_pos(UDM_layout.statwin_area);
		}
		else
		{
			UDM_layout.statwin_pos[3] = (int)rval[0];
			UDM_layout.statwin_pos[4] = (int)rval[1];
		}
		break;
/*
.....Size
*/
	case 1:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		UDM_layout.statwin_size[0] = (int)rval[0];
		UDM_layout.statwin_size[1] = (int)rval[1];
		break;
	case 2:
/*
......STATIC
*/
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "YES")==0)
			UDM_layout.statwin_type = 0;
		else if (strcmp(cmsg, "NO")==0)
			UDM_layout.statwin_type = 1;
		else
			goto bad_parm;
		break;
	case 3:
/*
......ACTIVE
*/
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "YES")==0)
			UDM_layout.statwin_active = 1;
		else if (strcmp(cmsg, "NO")==0)
			UDM_layout.statwin_active = 0;
		else
			goto bad_parm;
		break;
	case 4:
/*
......DOCKABLE
*/
		ul_to_upper(cmsg);
		tok = strtok(cmsg, " ,\t\r\n");
check:;
		if (tok==NULL)
			break;
		if (strcmp(tok, "YES")==0)
		{
			UDM_layout.statwin_doc[0] = 1;
			UDM_layout.statwin_doc[1] = 1;
			UDM_layout.statwin_doc[2] = 1;
			UDM_layout.statwin_doc[3] = 1;
			break;
		}
		else if (strcmp(tok, "NO")==0)
		{
			UDM_layout.statwin_doc[0] = 0;
			UDM_layout.statwin_doc[1] = 0;
			UDM_layout.statwin_doc[2] = 0;
			UDM_layout.statwin_doc[3] = 0;
			break;
		}
		else if (strcmp(tok, "TOP")==0)
		{
			UDM_layout.statwin_doc[0] = 1;
			if (UDM_layout.statwin_doc[1]==-1)
				UDM_layout.statwin_doc[1] = 0;
		}
		else if (strcmp(tok, "BOTTOM")==0)
		{
			UDM_layout.statwin_doc[1] = 1;
			if (UDM_layout.statwin_doc[1]==-1)
				UDM_layout.statwin_doc[1] = 0;
		}
		else if (strcmp(tok, "LEFT")==0)
		{
			UDM_layout.statwin_doc[2] = 1;
			if (UDM_layout.statwin_doc[1]==-1)
				UDM_layout.statwin_doc[1] = 0;
		}
		else if (strcmp(tok, "RIGHT")==0)
		{
			UDM_layout.statwin_doc[3] = 1;
			if (UDM_layout.statwin_doc[1]==-1)
				UDM_layout.statwin_doc[1] = 0;
		}
		else
			goto bad_parm;
		tok = strtok(NULL, " ,\t\r\n");
		goto check;
		break;
	case 5:
/*
......ATTACH
*/
		ul_to_upper(cmsg);
		tok = strtok(cmsg, ", \t\r\n");
		if (tok==NULL) goto bad_parm;
		if (strcmp(tok, "TOP")==0)
			UDM_layout.statwin_att[0] = 0;
		else if (strcmp(tok, "BOTTOM")==0)
			UDM_layout.statwin_att[0] = 1;
		else if (strcmp(tok, "LEFT")==0)
			UDM_layout.statwin_att[1] = 0;
		else if (strcmp(tok, "RIGHT")==0)
			UDM_layout.statwin_att[1] = 1;

		tok = strtok(NULL, ", \t\r\n");
		if (tok==NULL) goto bad_parm;
		if (strcmp(tok, "TOP")==0)
		{
			if (UDM_layout.statwin_att[0]==-1)
				UDM_layout.statwin_att[0] = 0;
			else
				goto bad_parm;
		}
		else if (strcmp(tok, "BOTTOM")==0)
		{
			if (UDM_layout.statwin_att[0]==-1)
				UDM_layout.statwin_att[0] = 1;
			else
				goto bad_parm;
		}
		else if (strcmp(tok, "LEFT")==0)
		{
			if (UDM_layout.statwin_att[1]==-1)
				UDM_layout.statwin_att[1] = 0;
			else
				goto bad_parm;
		}
		else if (strcmp(tok, "RIGHT")==0)
		{
			if (UDM_layout.statwin_att[1]==-1)
				UDM_layout.statwin_att[1] = 1;
			else
				goto bad_parm;
		}
		else
			goto bad_parm;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (msg, "Invalid value for WINDOW modal. /%s/ %s\n",ctyp,cmsg);
	uw_nterror (msg);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	if (status == UU_FAILURE)
	{
		UDM_bararea[UDM_barnum-1].flag = -1;
	}
	else if ((UDM_barnum>=1)
		&&(UDM_bararea[UDM_barnum-1].flag==6))
	{
		if (dir!=-1)
			UDM_bararea[UDM_barnum-1].dir = dir;
		UDM_bararea[UDM_barnum-1].indx = 0;
	}
	return(status);
}
#endif
/*********************************************************************
**       I_FUNCTION : udm_playout_commandbar (ctyp,cmsg)
**                      This function defines the command bar (multi_line) window.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
#if UU_COMP==UU_WIN2K
static int udm_playout_commandbar (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,status,inum;
	char msg[256];
	char *tok;
	int dir;
	int maxsub=6;
	static char csub[6][20] = {"POSITION","SIZE", "ACTIVE", "DOCKABLE",
						"ATTACH", "LOAD"};
	UU_REAL rval[6];
/*
.....Get modal to define
*/
	for (i=0;i<maxsub;i++)
	{
		ul_to_upper(ctyp);
		if (strcmp(ctyp,csub[i]) == 0) break;
	}
	if (i >= maxsub)
	{
		sprintf (msg, "Not a valid COMMAND modal.  /%s/ %s\n", ctyp,cmsg);
		uw_nterror (msg);
		goto failed;
	}
	dir = -1;
	switch(i)
	{
/*
.....Position
*/
	case 0:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) 
		{
			strcpy(UDM_layout.command_area, cmsg);
			dir = ud_find_menuarea_pos(UDM_layout.command_area);
			if (dir!=-1)
				UDM_layout.command_pos[2] = dir;
		}
		else
		{
			UDM_layout.command_pos[3] = (int)rval[0];
			UDM_layout.command_pos[4] = (int)rval[1];
			UDM_run_layout.command_type = 1;
		}
		break;
/*
.....Size
*/
	case 1:
		if (ul_to_reals(rval,&inum,3,cmsg) != UU_SUCCESS)
			goto bad_parm;
		if (inum>=1)
			UDM_layout.command_size[0] = (int)(rval[0] * uw_gl.dev_xmax+0.5);
		if (inum>=2)
			UDM_layout.command_size[1] = (int)(rval[1] * uw_gl.dev_ymax+0.5);
		if (inum==3)
			UDM_layout.command_size[2] = (int)rval[2];
		if (inum>3) goto bad_parm;
		break;
	case 2:
/*
......ACTIVE
*/
		ul_to_upper(cmsg);
		if (strcmp(cmsg, "MULTI")==0)
			UDM_layout.command_active = 1;
		else if (strcmp(cmsg, "SINGLE")==0)
			UDM_layout.command_active = 0;
		else
			goto bad_parm;
		break;
	case 3:
/*
......DOCKABLE
*/
		ul_to_upper(cmsg);
		tok = strtok(cmsg, " ,\t\r\n");
check:;
		if (tok==NULL)
			break;
		if (strcmp(tok, "YES")==0)
		{
			UDM_layout.command_doc[0] = 1;
			UDM_layout.command_doc[1] = 1;
			UDM_layout.command_doc[2] = 1;
			UDM_layout.command_doc[3] = 1;
			break;
		}
		else if (strcmp(tok, "NO")==0)
		{
			UDM_layout.command_doc[0] = 0;
			UDM_layout.command_doc[1] = 0;
			UDM_layout.command_doc[2] = 0;
			UDM_layout.command_doc[3] = 0;
			break;
		}
		else if (strcmp(tok, "TOP")==0)
		{
			UDM_layout.command_doc[0] = 1;
			if (UDM_layout.command_doc[1]==-1)
				UDM_layout.command_doc[1] = 0;
		}
		else if (strcmp(tok, "BOTTOM")==0)
		{
			UDM_layout.command_doc[1] = 1;
			if (UDM_layout.command_doc[1]==-1)
				UDM_layout.command_doc[1] = 0;
		}
		else if (strcmp(tok, "LEFT")==0)
		{
			UDM_layout.command_doc[2] = 1;
			if (UDM_layout.command_doc[1]==-1)
				UDM_layout.command_doc[1] = 0;
		}
		else if (strcmp(tok, "RIGHT")==0)
		{
			UDM_layout.command_doc[3] = 1;
			if (UDM_layout.command_doc[1]==-1)
				UDM_layout.command_doc[1] = 0;
		}
		else
			goto bad_parm;
		tok = strtok(NULL, " ,\t\r\n");
		goto check;
		break;
	case 4:
/*
......ATTACH
*/
		ul_to_upper(cmsg);
		tok = strtok(cmsg, ", \t\r\n");
		if (tok==NULL) goto bad_parm;
		if (strcmp(tok, "TOP")==0)
			UDM_layout.command_att[0] = 0;
		else if (strcmp(tok, "BOTTOM")==0)
			UDM_layout.command_att[0] = 1;
		else if (strcmp(tok, "LEFT")==0)
			UDM_layout.command_att[1] = 0;
		else if (strcmp(tok, "RIGHT")==0)
			UDM_layout.command_att[1] = 1;
		else
			goto bad_parm;

		tok = strtok(NULL, ", \t\r\n");
		if (tok==NULL) goto bad_parm;
		if (strcmp(tok, "TOP")==0)
		{
			UDM_layout.command_att[0] = 0;
		}
		else if (strcmp(tok, "BOTTOM")==0)
		{
			UDM_layout.command_att[0] = 1;
		}
		else if (strcmp(tok, "LEFT")==0)
		{
			UDM_layout.command_att[1] = 0;
		}
		else if (strcmp(tok, "RIGHT")==0)
		{
			UDM_layout.command_att[1] = 1;
		}
		else
			goto bad_parm;
		break;
	case 5:
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS)
			|| (inum !=2))
			goto bad_parm;
		if (rval[0]<25) 
			UDM_layout.command_load[0] = 25;
		else
			UDM_layout.command_load[0] = rval[0];
		if (rval[0]<10) 
			UDM_layout.command_load[1] = 10;
		else
			UDM_layout.command_load[1] = rval[1];
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (msg, "Invalid value for COMMAND modal. /%s/ %s\n",ctyp,cmsg);
	uw_nterror (msg);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	if (status == UU_FAILURE)
	{
		UDM_bararea[UDM_barnum-1].flag = -1;
	}
	else if ((UDM_barnum>=1)
		&&(UDM_bararea[UDM_barnum-1].flag==4))
	{
		if (dir!=-1)
			UDM_bararea[UDM_barnum-1].dir = dir;
		UDM_bararea[UDM_barnum-1].indx = 0;
	}
	return(status);
}
#endif

/*********************************************************************
**       I_FUNCTION : udm_signoff(restart)
**                      This function Signs off of NCL.
**       PARAMETERS     
**               INPUT  :  restart = True = Exit to Signon Form.  False = Exit
**                         all the way out of NCL.
**               OUTPUT :  none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
udm_signoff(restart)
UU_LOGICAL restart;
{
	(*(ug_gksstli.wsopen[0].connid)[UW_SIGNOFF])(restart);
	return 0;
}

/*********************************************************************
**       I_FUNCTION : udm_signon_load(dir,fname,cam,cad)
**           This function Signs on to NCL.  Normally used when resetting
**           or loading an NCL session.
**       PARAMETERS
**               INPUT  :
**                  dir     = Starting directory for NCL.
**                  fname   = Filename to load at startup.
**                  cam     = 1 = Enable NCLCAM.
**                  cadd    = 1 = Enable NCLCADD.
**               OUTPUT :  none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
udm_signon_load(dir,fname,cam,cad)
char *dir,*fname;
int cam,cad;
{
	(*(ug_gksstli.wsopen[0].connid)[UW_SIGNON_LOAD])(dir,fname,cam,cad);
	return 0;
}

/*******************************************************************n
**       E_FUNCTION : udm_read_layout(duims,filename)
**                      This function loads the Motif Layout from a disk file.
**       PARAMETERS     
**               INPUT  :  uims     = DAS layout structure.
**               filename = Name of layout file to read.
**               OUTPUT :  none.
**       RETURNS: UU_FAILURE on failure, UU_SUCCESS otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
udm_read_layout(duims,filename)
UD_UIMS *duims;
char *filename;
{
	char buf[80],ctyp[80],cmsg[80], msg[UX_MAX_PATH_LEN+40];
	int status,stat,numint,ityp,i, isub,istat;
	FILE *fptr;
	UX_pathname fullname;
	int size[2], last_play;
	int maxsub=12;
	static char csub[12][10]={"MENUS","GRAPHICS","PROMPTS","STATUS",
		"DIRECTORY", "MENUBAR", "COMMAND", "ERROR", "MENUAREA","WINDOW", 
		"TITLE", "STATUSBAR"};
/*
.....Assume success
*/
	status = UU_SUCCESS;
	last_play  = 0;
/*
.....Initialize layout structure
*/
	udm_init_layout();
	UD_window_title[0] = '\0';
/*
.....when we open layout file, first check if the file have a path
.....it it already have it, open it, otherwise, first
.....we will open the layout file at user define directory,
.....then check the system directory
*/
	strcpy(fullname, filename);
	stat = ul_open_mod_file("UU_USER_SETTINGS","layout","M_UD_LAYOUT_DIR",
		UU_NULL,fullname, 2, &fptr);
/*
.....Check for modals file
*/
	if (stat != UU_SUCCESS || fptr == 0) 
	{
		sprintf (msg, "Cannot open Motif Layout file %s.\n",fullname);
		ud_printmsg(msg);
		uu_abort();
	}
/*
.....Read a record
*/
	do
	{
		stat = ul_fread (fptr,buf,sizeof(buf),&numint);
		if (stat == UX_EOF)
		{ 
/*
......we need read the last line
*/
			if (numint<=0)
			{
				if (last_play==1)
					UDM_layout.nmenu++;
				goto done;
			}
			buf[numint+1] = '\0';
		}
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			if (feof(fptr)) goto done;
			sprintf (msg, "Error reading from Layout file %s.\n",fullname);
			ud_printmsg(msg);
			uu_abort();
		}
/*
.....Check for record type
*/
		istat = ul_modal_check (buf,&ityp,ctyp,cmsg);
/*
.....Invalid syntax
*/
		if (istat != UU_SUCCESS)
		{
			sprintf (msg, "Layout file syntax error. %s\n",buf);
			ud_printmsg(msg);
		}
/*
.....Subsystem type
*/
		switch (ityp)
		{
		case 1:
#if UU_COMP==UU_WIN2K
			if (last_play==1)
				UDM_layout.nmenu++;
#endif
			last_play = 0;
			for (i=0;i<maxsub;i++)
			{
				ul_to_upper(ctyp);
				if (strcmp(ctyp,csub[i]) == 0) break;
			}
			if (i >= maxsub)
			{
				sprintf (msg, "Not a valid Layout parameter. %s\n",buf);
				ud_printmsg(msg);
				break;
			}
#if UU_COMP==UU_WIN2K
			if (i==3)
			{
				sprintf (msg, "Not a valid Layout parameter. %s for Windows NT\n",buf);
				ud_printmsg(msg);
				break;
			}
#endif
			isub = i + 1;
			if (isub==3)
				UDM_bararea[UDM_barnum++].flag = 3;
			else if (isub==4)
				UDM_bararea[UDM_barnum++].flag = 2;
			else if (isub==7)
				UDM_bararea[UDM_barnum++].flag = 4;
			else if (isub==8)
				UDM_bararea[UDM_barnum++].flag = 5;
			else if (isub==9)
				UDM_bararea[UDM_barnum++].flag = 1;
			else if (isub==10)
				UDM_bararea[UDM_barnum++].flag = 6;
			break;
		case 2:
			switch (isub)
			{
			case 0:
				ud_printmsg ("A main Layout form is not in effect.\n");
				break;
			case 1:
				udm_playout_menu (ctyp,cmsg);
				last_play = 1;
				break;
			case 2:
				udm_playout_graphic (ctyp,cmsg);
				break;
			case 3:
				udm_playout_prompt (ctyp,cmsg);
				break;
			case 4:
#if UU_COMP!=UU_WIN2K
				udm_playout_status (ctyp,cmsg);
#endif
				break;
			case 5:
				udm_playout_directory (ctyp,cmsg);
				break;
#if UU_COMP==UU_WIN2K
			case 6:
				strcpy(UDM_layout.menubar, cmsg);
				strcpy(UDM_layout.menu_name[UDM_layout.nmenu],cmsg);
				UDM_layout.nmenu++;
				break;
			case 7:
				udm_playout_commandbar (ctyp,cmsg);				
				break;
			case 8:
				udm_playout_error (ctyp,cmsg);
				break;
			case 9:
				udm_playout_menuarea (ctyp,cmsg);
				last_play = 1;
				break;
			case 10:
				udm_playout_statwin (ctyp,cmsg);
				break;
#endif
			case 11:
				udm_playout_title (ctyp,cmsg);
				break;
			case 12:
				if (UDM_layout.statbar_no<4)
				{
					strcpy(UDM_layout.statbar_file[UDM_layout.statbar_no], cmsg);
					UDM_layout.statbar_no++;
				}
				break;
			}
		}
	}
	while (stat == UU_SUCCESS);
/*
.....Open layout windows
*/
done:;
	ux_fclose0 (fptr);
#if UU_COMP==UU_WIN2K
/*
.....default to bottom for statusbar
*/
	if (UDM_layout.statwin_doc[1]==-1)
		UDM_layout.statwin_doc[1] = 1;
	if (UDM_layout.statwin_att[0]==-1)
		UDM_layout.statwin_att[0] = 0;
	if (UDM_layout.statwin_att[1]==-1)
		UDM_layout.statwin_att[1] = 0;
/*
.....default to top for command bar
*/
	if (UDM_layout.command_doc[0]==-1)
		UDM_layout.command_doc[0] = 1;
#endif
/*
.....copy UDM_layout to UDM_run_layout
.....we use UDM_layout for reset layout
.....but UDM_run_layout to trace layout at
.....run time (user can change layout by 
.....adding new menu area, moving the menu
.....to different menu area,... for WinNT only
*/
#if UU_COMP==UU_WIN2K
	uu_move_byte((char*)&UDM_layout, (char*)&UDM_run_layout,
								sizeof(UDM_LAYOUT));
#endif
/*
.....Set UIMS layout variables
*/
/*
.....we need adjust size because this size include
.....the window border, it will give wrong value of duims
.....Yurong 3/18/99
*/
#if UU_COMP!=UU_WIN2K
	size[0] = UDM_layout.graphic_size[0]; 
	size[1] = UDM_layout.graphic_size[1];
	uw_mfadjust_border(size);
#else
	size[0] = UDM_layout.window_size[0]; 
	size[1] = UDM_layout.window_size[1];
#endif
/*
	udm_resize_graphics(duims,UDM_layout.graphic_size[0],
		UDM_layout.graphic_size[1],1);
*/
	udm_resize_graphics(duims,size[0], size[1], 1);
#if UU_COMP!=UU_WIN2K
	(*(ug_gksstli.wsopen[0].connid)[UW_WINDOW])();
#endif
/*
.....Display menus
*/
	UDM_menu_count = 0;

	for (i=0;i<UDM_layout.nmenu;i++) 
	{
		if (UDM_layout.menu_name[i][0]!='\0')
		{
#if UU_COMP==UU_WIN2K
			udm_read_menu(UDM_layout.menu_name[i], UDM_layout.menu_pos[i],
						UDM_layout.menu_size[i], 0, 2, -1);
#else
			udm_read_menu(UDM_layout.menu_name[i],UDM_layout.menu_pos[i],
						UDM_layout.menu_size[i], 0, 1, -1);
#endif
		}
	}
/*
......in native WinNT, UDM_layout.nmenu include all menu defined in layout file
......and all menu area (include empty one), and UDM_menu_count include statusbar too
......so UDM_layout.nmenu != UDM_menu_count
......Yurong 1/2/01
*/
#if UU_COMP!=UU_WIN2K
	UDM_layout.nmenu = UDM_menu_count;
#endif
/*
.....Set UIMS layout variables
*/
/*
.....move this function to before call uw_mfwindow();
.....because uw_mfwindow will change the UDM_layout value(to exclude border,
.....size read from layout file is include border, but setval(size) in Motif
.....is exclude border, the viewport is exclude border too), and that is 
.....what we need it. call udm_resize_graphics (This function alway consider
.....the window size that can be draw, so, it is examctly same as getval(size)
.....in Motif), call this function here here will cause the size change back
.....cause UDM_layout.graphic_size in here is include border
....Yurong 12/18/97
*/
/*
	udm_resize_graphics(duims,UDM_layout.graphic_size[0],
		UDM_layout.graphic_size[1],1);
*/
/*
.....we can't display any window now because we have read the initial file yet
.....so call after evry thing is done, put this in function initial_ncl in MAinFrame function
************
#if UU_COMP==UU_WIN2K
	(*(ug_gksstli.wsopen[0].connid)[UW_WINDOW])();
#endif
*************/
	return 0;
}


/*******************************************************************n
**       E_FUNCTION : ud_getpos_from_bararea(int flag, char*fname, char *bar_area, int bar_pos[3], int bar_size[2], int flag)
**                      This function find the bar_area's position and size.
**                      if not find the input bar_area in the layout, return (1,1,1)
**       PARAMETERS     
**               INPUT  : Bar_area: bar area name
**                                      flag: 0: menu file and menu area name
**                                            1: status file or status area name 
**												 (not used now, statusbafr merge with menubar)
**                                      fname: filename of menu or status file
**
**               OUTPUT :  bar_pos:
**                                      bar_size:
**       RETURNS: .
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
#if UU_COMP==UU_WIN2K
ud_getpos_from_bararea(int flag, char*filename, char *bar_area, int bar_pos[5], int bar_size[2])
{
	int i;
	UX_pathname dir,fname;

		for (i=0; i<UDM_run_layout.nmenu; i++)
		{
			ul_break_fname(filename,dir,fname);
			if (fname[0]=='\0')
				continue;

			if (strcmp(fname, UDM_run_layout.menu_name[i])!=0)
				continue;
			bar_pos[0] = UDM_run_layout.menu_pos[i][0];
			bar_pos[1] = UDM_run_layout.menu_pos[i][1];
			bar_pos[2] = UDM_run_layout.menu_pos[i][2];
			bar_pos[3] = UDM_run_layout.menu_pos[i][3];
			bar_pos[4] = UDM_run_layout.menu_pos[i][4];
			bar_size[0] = UDM_run_layout.menu_size[i][0];
			bar_size[1] = UDM_run_layout.menu_size[i][1];
			return 0;
		}       

		for (i=0; i<UDM_run_layout.nmenu; i++)
		{
			if (bar_area[0]=='\0')
				continue;
			if (stricmp(bar_area, UDM_run_layout.menu_area[i])!=0)
				continue;
			bar_pos[0] = UDM_run_layout.menu_pos[i][0];
			bar_pos[1] = UDM_run_layout.menu_pos[i][1];
			bar_pos[2] = UDM_run_layout.menu_pos[i][2];
			bar_pos[3] = UDM_run_layout.menu_pos[i][3];
			bar_pos[4] = UDM_run_layout.menu_pos[i][4];
			bar_size[0] = UDM_run_layout.menu_size[i][0];
			bar_size[1] = UDM_run_layout.menu_size[i][1];
			return 0;
		}
		goto not_found;
not_found:;
	bar_pos[0] = 1;
	bar_pos[1] = 1;
	bar_pos[2] = 1;
	bar_pos[3] = -1;
	bar_pos[4] = -1;
	bar_size[0] = -1;
	bar_size[1] = -1;
	return -1;
}
/*******************************************************************n
**       E_FUNCTION : ud_get_areadockable(bar_area, dockable)
**                      This function find the bar_area's "dockable" valie
**                      if not find, dock at the "END"
**       PARAMETERS     
**               INPUT  : Bar_area: bar area name
**
**               OUTPUT :  dockable: dockable valie
**                                      
**       RETURNS: .
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/

ud_get_areadockable(bar_area, dockable)
char *bar_area;
int *dockable;
{
	int i;
	
	*dockable = UDM_AREA_FRONTDOCK;
	for (i=0; i<UDM_run_layout.nmenu; i++)
	{
		if (bar_area[0]=='\0')
				continue;
		if (strcmp(bar_area, UDM_run_layout.menu_area[i])!=0)
			continue;
		*dockable = UDM_run_layout.area_dockable[i];
		return 0;
	}
	return -1;
}
ud_find_menuarea_pos(areaname)
char *areaname;
{
	int i;
	for (i=0; i<UDM_layout.nmenu; i++)
	{
		if (stricmp(areaname, UDM_layout.menu_area[i])!=0)
			continue;
		return UDM_layout.menu_pos[i][2];
	}
	return -1;
}

ud_find_menuarea(pos, areaname)
int pos;
char *areaname;
{
	int i;
	areaname[0] = '\0';
	for (i=0; i<UDM_layout.nmenu; i++)
	{
		if (UDM_layout.menu_pos[i][2]==pos)
		{
			strcpy(areaname, UDM_layout.menu_area[i]);
			return 0;
		}
	}
	return -1;
}
/*******************************************************************n
**       E_FUNCTION : ud_set_areadockable(bar_area, dockable)
**              This function set the bar_area's "dockable" valie
**                      
**       PARAMETERS     
**               INPUT  : Bar_area: bar area name
**							dockable: dockable valie;
**               OUTPUT :  none
**                                      
**       RETURNS: .
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
ud_set_areadockable(area_name, dockable)
char *area_name;
int dockable;
{
	int i;
	for (i=0; i<UDM_run_layout.nmenu; i++)
	{
		if (area_name[0]=='\0')
			continue;
		if (strcmp(area_name, UDM_run_layout.menu_area[i])!=0)
			continue;
		UDM_run_layout.area_dockable[i] = dockable;
		return 0;
	}
	return -1;
}
#endif
#endif
