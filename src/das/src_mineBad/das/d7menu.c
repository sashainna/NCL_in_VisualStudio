#include "usysdef.h"
#if UU_COMP != UU_VAXVMS || UU_OPSYS == UU_ALPHAVMS

/*********************************************************************
**      FILENAME: d7menu.c
**      CONTAINS:       udm_read_menu
**                      udm_init_menu_desc
**                      udm_init_menu
**                      udm_check_menu
**                      udm_pmenu_desc
**                      udm_pmenu_menu
**                      udm_enter_drawing
**                      udm_exit_drawing
**                      udm_icon_up(devnum)
**                      udm_icon_down(devnum)
**						ud_reload_menu(j)
**						ud_down_menunum
**						ud_save_dragmenu
**						ud_upt_UDMmenu
**						ud_del_UDMmenu
**						ud_frm_getmenu_name
**						ud_create_UDMmenu
**						ud_create_UDMmenu2
**						ud_get_menuinfo
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       d7menu.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:14
*********************************************************************/
#if UU_COMP!=UU_WIN2K
#include <Xm/Xm.h>
#endif
#include "ustdio.h"
#include "uhep.h"
#include "xfsys1.h"
#include "xenv1.h"
#include "dmark.h"
#include "driver.h"
#include "dmotif.h"
#include "gi1.h"
#if UU_COMP!=UU_WIN2K
#include "wsxw.h"
#else
#include "wsgl.h"
#endif
#include "mdunits.h"
#include "mdcpln.h"
#include "nclicons.h"
#include "usysg.h"
#include "gtbl.h"
#include "gdidd.h"
#include "mfort.h"
#include "udforms.h"
#include "zkeysym.h"

int DRAW_DISPLAY=0;
int minc;
static int UD_readmenu = 0;
static int Stype = 0;
static int Smenu_itype = 2;
static char Smenu_func[UX_MAX_FILE_LEN];
static int name_click = 0;
static int key_click = 0;
static int descript_click = 0;
static UD_TLIST Sfunc_list;

void ud_printmsg();
void udm_pmenu_separator();
void udm_pmenu_bitmap();

test()
{
}
/*********************************************************************
**       I_FUNCTION : udm_pmenu_desc(ctyp,cmsg)
**                      This function parses a menu file record.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int udm_pmenu_desc (ctyp, cmsg)
char *ctyp,*cmsg;
{
	int i,inc,status,inum;
	char buf[UX_MAX_PATH_LEN*2];
/*
.....added bitmap file name
.....Yurong 7/13/00
*/
/*
	int maxsub=6;
	static char csub[6][20] = {"NAME","POSITION","ROWS","COLS","TYPE",
		"SIZE"};
*/
	int maxsub=12;
	static char csub[12][20] = {"NAME","POSITION","ROWS","COLS","TYPE",
		"SIZE", "BITMAP", "AREA", "DOCKABLE", "KEYDEFS", "FORMAT", "JUSTIFY"};
	
/*
.....added 2 more type for native WinNT
.....Yurong 7/12/00
*/
/*
	static char ltype[3][10] = {"ICON","MENU","POPUP"};
*/
	static char ltype[5][64] = {"ICON","MENU","POPUP", "MENUBAR", "PULLDOWN"};
	UU_REAL rval[2];
/*
.....Initialize routine
*/
	inc = UDM_menu_count;
	status = UU_SUCCESS;
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
		sprintf (buf, "Not a valid DESCRIPTOR modal.  /%s/ %s\n",ctyp,cmsg);
		ud_printmsg (buf);
		goto failed;
	}
	switch(i)
	{
/*
.....NAME
*/
	case 0:
/*
.....remove '\t','\r' after cmsg
.....it will display '|' in native WinNT window
*/
		i = strlen(cmsg);
		while ((cmsg[i-1]=='\t')||(cmsg[i-1]=='\r')||(cmsg[i-1]==' '))
			i--;
		cmsg[i] = '\0';
		strcpy(UDM_menu[inc].name,cmsg);
		break;
/*
.....POSITION
*/
	case 1:
#if UU_COMP!=UU_WIN2K
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		if (UDM_menu[inc].pos[0] < 0)
		{
			UDM_menu[inc].pos[0] = (int)(rval[0] * uw_xw.dev_xmax);
			UDM_menu[inc].pos[1] = (int)(rval[1] * uw_xw.dev_ymax);
		}
#else
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		if (UDM_menu[inc].pos[0] < 0)
		{
			UDM_menu[inc].pos[0] = (int)(rval[0] * uw_gl.dev_xmax);
			UDM_menu[inc].pos[1] = (int)(rval[1] * uw_gl.dev_ymax);
		}
		break;
#endif
		break;
/*
.....ROWS
*/
	case 2:
		if ((ul_to_number(cmsg,&inum) != UU_SUCCESS) ||
			inum < 1) goto bad_parm;
		UDM_menu[inc].rows = inum;
		break;
/*
.....COLUMNS
*/
	case 3:
		if ((ul_to_number(cmsg,&inum) != UU_SUCCESS) ||
			inum < 1) goto bad_parm;
		UDM_menu[inc].cols = inum;
		break;
/*
.....TYPE
*/
	case 4:
/*
.....added 2 more type for native WinNT
.....Yurong 7/12/00
*/
/*
		if (ul_modal_toggle(cmsg,ltype,3,&UDM_menu[inc].type) != UU_SUCCESS)
*/
		if (ul_modal_toggle(cmsg,ltype,5,&UDM_menu[inc].type) != UU_SUCCESS)
			goto bad_parm;
		break;
/*
.....SIZE
*/
	case 5:
#if UU_COMP!=UU_WIN2K
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		if (UDM_menu[inc].size[0] < 0)
		{
			UDM_menu[inc].size[0] = (int)(rval[0] * uw_xw.dev_xmax);
			UDM_menu[inc].size[1] = (int)(rval[1] * uw_xw.dev_ymax);
		}
#else
		if ((ul_to_reals(rval,&inum,2,cmsg) != UU_SUCCESS) ||
			inum != 2) goto bad_parm;
		if (UDM_menu[inc].size[0] < 0)
		{
			UDM_menu[inc].size[0] = (int)(rval[0] * uw_gl.dev_xmax);
			UDM_menu[inc].size[1] = (int)(rval[1] * uw_gl.dev_ymax);
		}
#endif
		break;
#if UU_COMP==UU_WIN2K
	case 6:
		strcpy(UDM_menu[inc].bmpfile,cmsg);
		break;
	case 7:
		strcpy(UDM_menu[inc].menu_area,cmsg);
		break;
	case 8:
		if (strcmp(cmsg, "NO")==0)
			UDM_menu[inc].dockable = 0;
		else
			UDM_menu[inc].dockable = 1;
		break;
	case 11:
		if (stricmp(cmsg, "RIGHT")==0)
			UDM_menu[inc].justify = 1;
		else
			UDM_menu[inc].justify = 0;
		break;
#endif
	case 9:
/*
......save keydef file, load only when displayed
*/
		strcpy(UDM_menu[inc].keyfile, cmsg);
		break;
	case 10:
		if (stricmp(cmsg, "ICON")==0)
			UDM_menu[inc].menu_format = 0;
		else if (stricmp(cmsg, "TEXT")==0)
			UDM_menu[inc].menu_format = 1;
		else
			UDM_menu[inc].menu_format = 2;
		break;
	}
	goto done;
/*
.....Bad modal value
*/
bad_parm:;
	sprintf (buf, "Invalid value for MENU modal. /%s/ %s\n",ctyp,cmsg);
	ud_printmsg (buf);
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}

static void  udm_pmenu_choice(ctyp, cmsg)
char *ctyp,*cmsg;
{
	int inc, num;
	char buf[UX_MAX_PATH_LEN*2], *p, msg[UX_MAX_PATH_LEN*2];

	inc = UDM_menu_count;

	buf[0] = '\0';
	p = (char *)index(cmsg,',');
	if (p != 0 && p != cmsg)
	{
		*p = '\0';
		++p;
		while ((*p==' ') || (*p=='\t')) p++;
		strcpy(buf, p);
	}
	strcpy(UDM_menu[UDM_menu_count].menus[UDM_menu[UDM_menu_count].num].chcfile, cmsg);
	if (buf[0]!='\0')
	{
		if (ul_to_number(buf,&num)==UU_SUCCESS)
			UDM_menu[UDM_menu_count].menus[UDM_menu[UDM_menu_count].num].chcdef = num;
		else
		{
			sprintf(msg, "#CHOICE# format is wrong in menu file \n%s\n", 
									UDM_menu[inc].file);
			ud_printmsg (msg);
			UDM_menu[UDM_menu_count].menus[UDM_menu[UDM_menu_count].num].chcdef = 0;
		}
	}
	else
		UDM_menu[UDM_menu_count].menus[UDM_menu[UDM_menu_count].num].chcdef = 0;
	UDM_menu[UDM_menu_count].num++;
}

/*********************************************************************
**       I_FUNCTION : udm_pmenu_toggle(ctyp,cmsg)
**                      This function defines the Menu toggle entries.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int udm_pmenu_toggle (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int status;
	int inc, nc;
	char *p;
	char buf[UX_MAX_PATH_LEN*2], buf1[UX_MAX_PATH_LEN*2], buf2[UX_MAX_PATH_LEN*2];
	int tog_num;
/*
.....Initialize routine
*/
	inc = UDM_menu_count;
	status = UU_SUCCESS;
	tog_num = UDM_menu[inc].menus[UDM_menu[inc].num].toggle_num;
	UDM_menu[inc].menus[UDM_menu[inc].num].toggle[tog_num].params = UU_NULL;
/*
.....Save toggle choice
*/
	strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].toggle[tog_num].label,ctyp);
/*
.....added parameter string
*/      
	p = (char *)index(cmsg,',');
	if (p != 0 && p != cmsg)
	{
		*p = '\0';
		++p;
		while ((*p==' ') || (*p=='\t')) p++;
		strcpy(buf, p);
/*
.....added tooltip string for function
*/
		if (buf[0]=='<')
		{
			p++;
			strcpy(buf, p);
			p = (char *)index(buf,'>');
			if (p!=NULL)
				*p = '\0';
			strncpy(UDM_menu[inc].menus[UDM_menu[inc].num].descrip,buf,39);
			UDM_menu[inc].menus[UDM_menu[inc].num].descrip[39] = '\0';
			if (p!=NULL)
			{
				++p;
				while ((*p==' ') || (*p=='\t') || (*p==',')) p++;
				strcpy(buf1, p);
			}
			else
				buf1[0] = '\0';
		}
		else
			strcpy(buf1, buf);
/*
.....added parameter string for function
*/
		p = buf1;
		if (buf1[0]=='\"')
		{
			p++;
			strcpy(buf1, p);
			p = (char *)rindex(buf1,'\"');
			*p = '\0';
/*			strncpy(UDM_menu[inc].menus[UDM_menu[inc].num].toggle[tog_num].params,buf1,39);
			UDM_menu[inc].menus[UDM_menu[inc].num].toggle[tog_num].params[39] = '\0';
*/
			nc = strlen (buf1);
			UDM_menu[inc].menus[UDM_menu[inc].num].toggle[tog_num].params = 
											(char*)uu_malloc((nc+1)*sizeof(char));
			strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].toggle[tog_num].params,buf1);
			++p;
			while ((*p==' ') || (*p=='\t') || (*p==',')) p++;
			strcpy(buf2, p);
		}
		else
			strcpy(buf2, buf1);
#if UU_COMP==UU_WIN2K
		if (buf2[0]!='\0')
		{
			strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].toggle[tog_num].bmpfile, buf2);
		}
		else
			UDM_menu[inc].menus[UDM_menu[inc].num].toggle[tog_num].bmpfile[0] = '\0';
#endif
	}       
	strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].toggle[tog_num].func,cmsg);
	UDM_menu[inc].menus[UDM_menu[inc].num].toggle_num++;
	return(status);
}

static void  udm_pmenu_color(ctyp, cmsg)
char *ctyp,*cmsg;
{
	int inc;
	
	inc = UDM_menu_count;
	strcpy(UDM_menu[inc].menus[UDM_menu[inc].num-1].bgcolor, cmsg);
}       

#if UU_COMP==UU_WIN2K
static void udm_pmenu_bitmap(cmsg)
char *cmsg;
{
	int inc, num;
	char buf[UX_MAX_PATH_LEN*2], *p, msg[UX_MAX_PATH_LEN*2];
	inc = UDM_menu_count;

	buf[0] = '\0';
	p = (char *)index(cmsg,',');
	if (p != 0 && p != cmsg)
	{
		*p = '\0';
		++p;
		while ((*p==' ') || (*p=='\t')) p++;
		strcpy(buf, p);
	}
	if (ul_to_number(cmsg,&num)==UU_SUCCESS)
	{
		UDM_menu[inc].menus[UDM_menu[inc].num].bmpnum = num;
/*
.....default bitmap file to 
.....UDM_menu[inc].bmpfile
*/
		if (UDM_menu[inc].bmpfile[0]!='\0')
			strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].bmpfile, UDM_menu[inc].bmpfile); 
		else
		{
			sprintf(msg, "Declared bitmap number without bitmap file delared in menu file \n%s\n", 
								UDM_menu[inc].file);
			ud_printmsg (msg);
		}
	}
	else
	{
		strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].bmpfile, cmsg);
		if (UDM_menu[inc].bmpfile[0]=='\0')
		{
			strcpy(UDM_menu[inc].bmpfile, UDM_menu[inc].menus[UDM_menu[inc].num].bmpfile); 
		}
		if (buf[0]!='\0')
		{
			if (ul_to_number(buf,&num)==UU_SUCCESS)
				UDM_menu[inc].menus[UDM_menu[inc].num].bmpnum = num;
			else
			{
				sprintf(msg, "#BITMAP# format is wrong in menu file \n%s\n", 
									UDM_menu[inc].file);
				ud_printmsg (msg);
			}
		}
		else
			UDM_menu[inc].menus[UDM_menu[inc].num].bmpnum = 0;
	}
}       

static void  udm_pmenu_separator(ctyp, cmsg)
char *ctyp,*cmsg;
{
	int inc;
	
	inc = UDM_menu_count;
	UDM_menu[inc].menus[UDM_menu[inc].num-1].separator = 1;
}       

/*********************************************************************
**       I_FUNCTION : udm_pmenu_buttons (ctyp,cmsg)
**                      This function defines the Status entries.
**       PARAMETERS     
**               INPUT  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int udm_pmenu_buttons (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,status;
	int inc;
	char *p, sfile[30];
	char buf[UX_MAX_PATH_LEN*2], buf1[UX_MAX_PATH_LEN*2];
/*
.....Initialize routine
*/
	inc = UDM_menu_count;
	status = UU_SUCCESS;

	if (ctyp[0]!='\0')
	{
/*
......'name' only allow 30 chars for name, this is for diplay label in menu
......the real file path saved in file[UX_MAX_FILE_LEN] (defined in dmotif.h)
*/
/*		strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].name, ctyp); */
		ul_short_filename(ctyp,sfile,29);
		strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].name, sfile);
	}
	else
		UDM_menu[inc].menus[UDM_menu[inc].num].name[0] = '\0';

	UDM_menu[inc].menus[UDM_menu[inc].num].kinc = inc;

	buf[0] = '\0';
	buf1[0] = '\0';
	p = (char *)index(cmsg,',');
	if (p != 0 && p != cmsg)
	{
		*p = '\0';
		++p;
		while ((*p==' ') || (*p=='\t')) p++;
		strcpy(buf, p);
/*
.....added tooltip string for function
*/
		if (buf[0]=='<')
		{
			p++;
			strcpy(buf, p);
			p = (char *)index(buf,'>');
			if (p!=NULL)
				*p = '\0';
			strncpy(UDM_menu[inc].menus[UDM_menu[inc].num].descrip,buf,39);
			UDM_menu[inc].menus[UDM_menu[inc].num].descrip[39] = '\0';
			if (p!=NULL)
			{
				++p;
				while ((*p==' ') || (*p=='\t') || (*p==',')) p++;
				strcpy(buf1, p);
			}
			else
				buf1[0] = '\0';
		}
		else
			strcpy(buf1, buf);
	}
	if (cmsg[0] != '\0')
	{
/*
.....remove trailing space and '\t'
*/
		for (i=strlen(cmsg); i>=0; i--)
		{
			if ((cmsg[i-1]==' ')||(cmsg[i-1]=='\t'))
				cmsg[i-1] = '\0';
			else
				break;
		}
/*
......'statname' only allow 30 chars for name (defined in dmotif.h)
*/
		ul_short_filename(cmsg,sfile,29);
		strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].statname, sfile);
/*		strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].statname, cmsg); */
	}
	else
	{
		sprintf (buf, "Must have Status name for BUTTON modal. /%s/ %s\n",ctyp,cmsg);
		ud_printmsg (buf);
		goto failed;
	}
	if (buf1[0]!='\0')
		strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].file,buf1);
	else
		UDM_menu[inc].menus[UDM_menu[inc].num].file[0] = '\0';
/*
.....assigned size
*/
	UDM_menu[inc].menus[UDM_menu[inc].num].size[0] = UDM_menu[inc].size[0];
	UDM_menu[inc].menus[UDM_menu[inc].num].size[1] = UDM_menu[inc].size[1];
	UDM_menu[inc].num++;
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}
#endif
/*********************************************************************
**       I_FUNCTION : udm_pmenu_menu(ctyp,cmsg)
**                      This function defines the Menu entries.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
static int udm_pmenu_menu (ctyp,cmsg)
char *ctyp,*cmsg;
{
	int i,status, nc;
	int inc;
	char *p, sfile[30];
#if UU_COMP!=UU_WIN2K
	UU_REAL rval[2];
	int inum;
#endif
	char buf[UX_MAX_PATH_LEN*2], buf1[UX_MAX_PATH_LEN*2], buf2[UX_MAX_PATH_LEN*2];
	char tempstr[UX_MAX_PATH_LEN];
/*
.....Initialize routine
*/
	inc = UDM_menu_count;
	status = UU_SUCCESS;
#if UU_COMP!=UU_WIN2K
	if (UDM_menu[inc].num >= (UDM_menu[inc].rows*UDM_menu[inc].cols))
		goto bad_parm;
#endif
/*
.....Save Menu Title
*/
/*
......allow menu item label is empty
......it may only have icon on native WinNT
*/
#if UU_COMP!=UU_WIN2K
	strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].name,ctyp);
#else
	i = 0;
	if (ctyp[i]!='\0')
	{
/*
......'name' only allow 30 chars for name, this is for diplay label in menu
......the real file path saved in file[UX_MAX_FILE_LEN] (defined in dmotif.h)
*/
		ul_short_filename(&(ctyp[i]),sfile,29);
		strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].name, sfile);
	}
	else
		UDM_menu[inc].menus[UDM_menu[inc].num].name[0] = '\0';
#endif

	UDM_menu[inc].menus[UDM_menu[inc].num].kinc = inc;
/*
.....Save Menu filename &
.....Optional position and size
*/
	UDM_menu[inc].menus[UDM_menu[inc].num].pos[0] = -1;
	UDM_menu[inc].menus[UDM_menu[inc].num].pos[1] = -1;
	UDM_menu[inc].menus[UDM_menu[inc].num].size[0] = -1;
	UDM_menu[inc].menus[UDM_menu[inc].num].size[1] = -1;
	UDM_menu[inc].menus[UDM_menu[inc].num].params = UU_NULL;
	UDM_menu[inc].menus[UDM_menu[inc].num].descrip[0] = '\0';
	p = (char *)index(cmsg,',');
	if (p != 0 && p != cmsg)
	{
		*p = '\0';
/*
.....changed to remove '\t' or space before "buf"
.....Yurong 7/13/00
*/
		++p;
		while ((*p==' ') || (*p=='\t')) p++;
		strcpy(buf, p);
/*
.....added tooltip string for function
*/
		if (buf[0]=='<')
		{
			p++;
			strcpy(buf, p);
			p = (char *)index(buf,'>');
			if (p!=NULL)
				*p = '\0';
			strncpy(UDM_menu[inc].menus[UDM_menu[inc].num].descrip,buf,39);
			UDM_menu[inc].menus[UDM_menu[inc].num].descrip[39] = '\0';
			if (p!=NULL)
			{
				++p;
				while ((*p==' ') || (*p=='\t') || (*p==',')) p++;
				strcpy(buf1, p);
			}
			else
				buf1[0] = '\0';
		}
		else
			strcpy(buf1, buf);
/*
.....added parameter string for function
*/
		p = buf1;
		if (buf1[0]=='\"')
		{
			p++;
			strcpy(buf1, p);
			p = (char *)rindex(buf1,'\"');
			*p = '\0';
/*			strncpy(UDM_menu[inc].menus[UDM_menu[inc].num].params,buf1,39);
			UDM_menu[inc].menus[UDM_menu[inc].num].params[39] = '\0';
*/
			nc = strlen (buf1);
			UDM_menu[inc].menus[UDM_menu[inc].num].params = 
											(char*)uu_malloc((nc+1)*sizeof(char));
			strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].params,buf1);
			++p;
			while ((*p==' ') || (*p=='\t') || (*p==',')) p++;
			strcpy(buf2, p);
		}
		else
			strcpy(buf2, buf1);
		if (buf2[0]!='\0')
		{
#if UU_COMP!=UU_WIN2K
			if ((ul_to_reals(rval,&inum,2,buf1) != UU_SUCCESS) ||
				inum != 2) goto bad_parm;
			UDM_menu[inc].menus[UDM_menu[inc].num].pos[0] = rval[0];
			UDM_menu[inc].menus[UDM_menu[inc].num].pos[1] = rval[1];
#else
			udm_pmenu_bitmap(buf2);
#endif
		}
	}
/*
.....remove trailing space
*/
/*
.....remove trailing space and '\t'
*/
	if (cmsg[0]!='\0')
	{
		for (i=strlen(cmsg); i>=0; i--)
		{
			if ((cmsg[i-1]==' ')||(cmsg[i-1]=='\t'))
				cmsg[i-1] = '\0';
			else
				break;
		}
		strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].file,cmsg);
	}
	else
		UDM_menu[inc].menus[UDM_menu[inc].num].file[0] = '\0';
/*
.....default bitmap file to function_name_size.bmp  .file
*/
	if ((UDM_menu[inc].menus[UDM_menu[inc].num].bmpfile[0]=='\0')
		&& (UDM_menu[inc].menus[UDM_menu[inc].num].file[0]!='\0'))
	{
		strcpy(tempstr, UDM_menu[inc].menus[UDM_menu[inc].num].file);
		p = (char *)strrchr(tempstr,'.');
		if (p == 0)
			strcat(tempstr, ".bmp");
		else
		{
			*p = 0;
			strcat(tempstr, ".bmp");
		}
		strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].bmpfile, tempstr);
		UDM_menu[inc].menus[UDM_menu[inc].num].bmpnum = 0;
	}
	UDM_menu[inc].num++;
	goto done;
/*
.....Bad modal value
*/
#if UU_COMP!=UU_WIN2K
bad_parm:;
   sprintf (buf, "Invalid value for MENU modal. /%s/ %s\n",ctyp,cmsg);
   ud_printmsg (buf);
/*
.....Failure
*/
   status = UU_FAILURE;
#endif
done:;
	return(status);
}

/*********************************************************************
**       E_FUNCTION : udm_read_menu(filename,pos,size, kdis)
**                      This function loads a Motif Menu from a disk file.
**       PARAMETERS     
**               INPUT  :  filename = Name of menu file to read.
**               pos      = Optional screen location from menu.  If
**                          'pos' is a positive location, it will
**                          override the location in the menu file.
**               size      = Optional screen location from menu.  If
**                          'size' is a positive location, it will
**                          override the size in the menu file.
**               kdis     = 1 = Display loaded menu.  0 = Don't.
**                              kflag: flag to decide if we need call WS menu display routine
**                              menutype: added a menu type to load the menu only if the menu type matched
**                                                      otherwise, ignore this menu
**                                                      -1: for all menu type
**                                                      other: 
**                                                              UDM_MTYPE_ICON 0
**                                                              UDM_MTYPE_MENU 1
**                                                              UDM_MTYPE_POPUP 2
**                                                              UDM_MTYPE_MENUBAR       3
**                                                              UDM_MTYPE_PULLDOWN      4
**                                                              UDM_MTYPE_INTERNAL 5
**                               
**               OUTPUT :  none.
**       RETURNS: UU_FAILURE on failure, UU_SUCCESS otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
/*
...add menu size by yurong
*/
int udm_read_menu(filename,pos,size,kdis,kflag, menutype)
char *filename;
int pos[3], size[2], kdis, kflag, menutype;
{
	UX_pathname fullname, tempfile, dir, menu_name, 
		tempfile1, dir1, menu_name1, tempfile2, dir2, menu_name2;
	char buf[UX_MAX_PATH_LEN*2],ctyp[UX_MAX_PATH_LEN*2],cmsg[UX_MAX_PATH_LEN*2];
	int stat,numint,ityp,i,isub,istat,kinc;
	FILE *fptr;
	char num[UX_MAX_PATH_LEN+40];
	int toggle_start, menu_first;
	static int sepnum = 0;
	int maxsub=6;
	static char csub[7][12]={"DESCRIPTOR","MENUS", "CHOICE", "COLOR", "BAR", "BUTTONS"};
/*
.....Assume fail
*/
	stat = UU_SUCCESS;
	toggle_start = 0;
	menu_first = 1;
	strcpy(fullname, filename);
/*
.....Initialize menu structure
*/
	stat = ul_open_mod_file("UU_USER_SETTINGS","menu","NCL_MENU", UU_NULL,
		fullname, 2,&fptr);

	if(UDM_menu_design == 1) goto open;
/*
.....Initialize menu structure
*/
/*
.....need pass in all path name in 
.....12/16/97 Yurong
*/
	stat = udm_init_menu_desc(fullname, pos, size, &kinc);
	if (stat != UU_SUCCESS) goto done;
	if (kinc < UDM_menu_count)
	{
		(*(ug_gksstli.wsopen[0].connid)[UW_MENU])(kinc,kdis);
		return(stat);
	}
/*
.....Check for modals file
*/
open:
	isub = 0;
	if (fptr == UU_NULL) 
	{
#if UU_COMP!=UU_WIN2K
		if(UDM_menu_design==1)
		{
			sprintf (num, "Cannot open Motif Menu file %s.\n",filename);
			uw_mfmessage((Widget)NULL,num);
		}
		else
		{ 
			sprintf (num, "Cannot open Motif Menu file %s.\n",filename);
			ud_printmsg(num);
		}
#else
		sprintf (num, "Cannot open Motif Menu file %s.\n",filename);
		ud_printmsg (num);
#endif
		stat = UU_FAILURE;
		return stat;
	}
	sepnum = 0;
/*
.....Read a record
*/
	do
	{
		stat = ul_fread (fptr,buf,sizeof(buf),&numint);
		if (numint>UX_MAX_PATH_LEN*2)
		{
			ud_printmsg ("Menu line is too long (more than 2048 chars).\n");
			numint = 131;
		}
/*
......when "end of file" and no input
......numint = -1, will cause a memory error
*/
		if (numint>=0)
			buf[numint] = '\0';
/*
.....we still need pass the last line
.....for temp we only changed for WIN2K
.....Yurong  7/10/00
*/
#if UU_COMP!=UU_WIN2K
		if (stat == UX_EOF)
		{
			stat = UU_SUCCESS;
			goto done;
		}
#else
		if ((numint<=0)&&(stat == UX_EOF))
		{
			stat = UU_SUCCESS;
			goto done;
		}
		else if ((stat == UX_EOF)&&(numint>0))
		{
			buf[numint+1] = '\0';
			stat = UU_SUCCESS;
			goto check;
		}
#endif
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
#if UU_COMP!=UU_WIN2K
			if(UDM_menu_design==1)
			{
				sprintf (num, "Error reading from menu file %s.\n", filename);
				uw_mfmessage((Widget)NULL,num);
			}
			else
			{
				ud_printmsg ("Error reading from menu file.\n");
			}
#else
			sprintf (num, "Error reading from menu file.\n", filename);
			ud_printmsg (num);
#endif
			goto done;
		}
/*
.....Check for record type
*/
check:;
		istat = ul_modal_check (buf,&ityp,ctyp,cmsg);
/*
.....Invalid syntax
*/
		if (istat != UU_SUCCESS)
		{
#if UU_COMP!=UU_WIN2K
			if(UDM_menu_design==1)
			{
				sprintf (num, "Menu file syntax error. %s\n",buf);
				uw_mfmessage((Widget)NULL,num);
			}
			else
			{
				sprintf (num, "Menu file syntax error. %s\n",buf);
				ud_printmsg(num);
			}
#else
			sprintf (num, "Menu file syntax error. %s\n",buf);
			ud_printmsg (num);
#endif
		}
/*
.....Subsystem type
*/
		switch (ityp)
		{
		case 1:
			for (i=0;i<maxsub;i++)
			{
				ul_to_upper(ctyp);
				if (strcmp(ctyp,csub[i]) == 0) break;
			}
			if (i >= maxsub)
			{
#if UU_COMP!=UU_WIN2K
				if(UDM_menu_design==1)
				{
					sprintf (num, "Not a valid Menu parameter. %s\n",buf);
					uw_mfmessage((Widget)NULL,num);
				}
				else
				{
					sprintf (num, "Not a valid Menu parameter. %s\n",buf);
					ud_printmsg(num);
				}
#else
				sprintf (num, "Not a valid Menu parameter. %s\n",buf);
				ud_printmsg (num);
#endif
				break;
			}
/*
.....End of Menus definition
.....Make sure previous was defined correctly
*/
			if(UDM_menu_design==1)
			{
				if (i==3)
				{
					uw_mfpmenu_color(ctyp, cmsg);
					break;
				}
				isub = i+1;
				if (isub == 3)
				{
					if (toggle_start==0)
					{
						toggle_start = 1;
					}
				}
				if ((isub == 2)&&(menu_first == 1))
				{
					menu_first = 0;
				}
				else if (isub == 2)
					uw_mfdsn_minc();
			}
			else
			{
/*
				if (isub == 2) stat = udm_check_menu();
*/
				if (i==3)
				{
					udm_pmenu_color(ctyp, cmsg);
					break;
				}
#if UU_COMP==UU_WIN2K   
				if(i==2)
				{
					udm_pmenu_choice(ctyp, cmsg);
					break;
				}
#endif
				isub = i + 1;
				if (((isub == 2)||(isub == 5))
						&&(menu_first==1)) 
				{
/*
.....it is not suppose to have #MENUS# in *.stat file
.....stat == UU_SUCCESS
*/
					if (UDM_menu[UDM_menu_count].statflag==1)
					{
/*
......check if this file is under
......we allow *.menu under sttausbar to have buttons
*/
						strcpy (tempfile, UDM_menu[UDM_menu_count].file);
						ul_break_fname(tempfile, dir, menu_name);
						
						strcpy (tempfile1, UDM_layout.statbar_file[0]);
						ul_break_fname(tempfile1, dir1, menu_name1);
						
						strcpy (tempfile2, UDM_layout.statbar_file[1]);
						ul_break_fname(tempfile2, dir2, menu_name2);

						if ((stricmp(menu_name, menu_name1)==0)
							|| (stricmp(menu_name, menu_name2)==0) )
							UDM_menu[UDM_menu_count].statflag = 2;
						else
						{
							stat = -1;
							goto done;
						}
					}
					if (UDM_menu[UDM_menu_count].num==0)
						stat = udm_init_menu();
					menu_first = 0;
				}
				else if ((isub == 2)||(isub == 5))
				{
					if ((sepnum>20)&&(isub==5))
						ud_printmsg ("Too many separators, at most 20.\nIgnore any more separators");
					else if (isub==5)
						UDM_menu[UDM_menu_count].num++;
					else if (UDM_menu[UDM_menu_count].statflag!=2)
						UDM_menu[UDM_menu_count].num++;
				}
				if (isub == 6)
				{
/*
.....it is not suppose to have #BUTTONS# in *.menu file
.....stat == UU_SUCCESS
*/
					if (UDM_menu[UDM_menu_count].statflag==0)
					{
/*
......check if this file is under
......we allow *.menumunder sttausbar to have buttons
*/
						strcpy (tempfile, UDM_menu[UDM_menu_count].file);
						ul_break_fname(tempfile, dir, menu_name);
						
						strcpy (tempfile1, UDM_layout.statbar_file[0]);
						ul_break_fname(tempfile1, dir1, menu_name1);
						
						strcpy (tempfile2, UDM_layout.statbar_file[1]);
						ul_break_fname(tempfile2, dir2, menu_name2);

						if ((stricmp(menu_name, menu_name1)==0)
							|| (stricmp(menu_name, menu_name2)==0) )
							UDM_menu[UDM_menu_count].statflag = 2;
						else
						{
							stat = -1;
							goto done;
						}
					}
					if (UDM_menu[UDM_menu_count].num==0)
						stat = udm_init_menu();
				}
				if (isub == 3) 
				{
/*
......not get here unless it is menu design for motif, wnidow use choice menu
*/
					if (toggle_start==0)
					{
						toggle_start = 1;
					}
				}
#if UU_COMP==UU_WIN2K
				if (isub == 5) 
				{
					if (sepnum<=20)
						udm_pmenu_separator(ctyp, cmsg);
					isub = 2;
					sepnum++;
				}
#endif
			}
			break;
		case 2:
			switch (isub)
			{
			case 0:
				ud_printmsg ("A main Menu form is not in effect.\n");
				break;
			case 1:
				if(UDM_menu_design==1)
					uw_mfpmenu_desc(ctyp, cmsg);
				else
					udm_pmenu_desc (ctyp,cmsg);
				break;
			case 2:
			case 5:
				if(UDM_menu_design==1)
					uw_mfpmenu_menu(ctyp, cmsg);
				else
					udm_pmenu_menu (ctyp,cmsg);
				break;
			case 3:
				if (toggle_start == 1)
				{
					if(UDM_menu_design==1)
						stat = uw_mfinit_toggle(ctyp,cmsg);
/*
......old choice format for WNT
*/
#if UU_COMP!=UU_WIN2K
					else
						stat = udm_init_toggle(ctyp,cmsg);
#endif
					toggle_start = 0;
				}
				else
				{
					if(UDM_menu_design==1)
						uw_mfpmenu_toggle (ctyp,cmsg);
					else
						udm_pmenu_toggle (ctyp,cmsg);
				}
				break;
			case 6:
#if UU_COMP==UU_WIN2K
				udm_pmenu_buttons (ctyp,cmsg);
#endif
				break;
			}
		}
	}
	while (stat == UU_SUCCESS);
/*
.....Display newly loaded menu
*/
done:;
	if(UDM_menu_design==1)
		return(stat);
	if (stat == UU_SUCCESS)
	{
		if ((isub == 2) || (isub == 5) || (isub == 6))
			stat = udm_check_menu(menutype);
		if (stat == UU_SUCCESS) 
		{
			if (kflag==1)
			{
				(*(ug_gksstli.wsopen[0].connid)[UW_MENU])(kinc,kdis);
			}
			else if (kflag!=2)
			{
				if (UDM_menu[kinc].type == UDM_MTYPE_PULLDOWN)
					(*(ug_gksstli.wsopen[0].connid)[UW_MENU])(kinc,kdis);
			}
		}
	}
	if (stat != UU_SUCCESS)
	{
		sprintf (buf, "Could not load Motif file %s.\n",filename);
		ud_printmsg (buf);
	}
	if (fptr != 0) 
		ux_fclose0(fptr);
	return(stat);
}

/*********************************************************************
**       I_FUNCTION : udm_init_menu_desc(filename,dpos,kinc)
**                      This function defines the default menu descriptor.
**       PARAMETERS     
**               INPUT  :  filename = Filename of menu defining.
**               dpos     = Default location of menu.
**               dsize     = Default size of menu.
**               OUTPUT :  kinc     = Position within Menu array for this menu.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
/*
...add size by Yurong
*/
int udm_init_menu_desc(filename,dpos,dsize,kinc)
char *filename;
int dpos[2],dsize[2], *kinc;
{
	int iret,i;
	UX_pathname fname;
	char *p;
/*
.....Initialize routine
*/
	iret = UU_SUCCESS;
	*kinc = UDM_menu_count;
/*
......remove trailing space
*/
/*
......do this just because filename maybe a const char*,
......such as pass as "file.menu" instead of pssing as
......as a string, so when I try to assign value to them, 
......it will hit a error
......Yurong
*/
	strcpy(fname, filename);
	for (i=strlen(fname); i>0; i--)
	{
		if (fname[i-1]==' ')
			fname[i-1] = '\0';
		else
			break;
	}
/*
.....See if this menu is already loaded
*/
	if (UD_readmenu==0)
	{
		for (i=0;i<UDM_menu_count;i++)
		{
			if (strcmp(UDM_menu[i].file,fname) == 0)
			{
				*kinc = i;
				goto done;
			}
		}
	}
/*
.....Make sure we don't have too many menus
*/
	if (UDM_menu_count >= UDM_MAX_MENU)
	{
		iret = UU_FAILURE;
		goto done;
	}

	p = (char *)rindex(fname,'.');
	if (p != 0) 
	{
		p++;
		if (strcmp(p, "stat")==0)
			UDM_menu[*kinc].statflag = 1;
		else
			UDM_menu[*kinc].statflag = 0;
	}
	else
	{
		UDM_menu[*kinc].statflag = 0;
	}
/*
.....Initialize this menu
*/
	sprintf(UDM_menu[*kinc].name,"Menu_%d",*kinc);
	strcpy(UDM_menu[*kinc].file,fname);
	UDM_menu[*kinc].pos[0] = dpos[0];
	UDM_menu[*kinc].pos[1] = dpos[1];
	UDM_menu[*kinc].rows = -1;
	UDM_menu[*kinc].cols = -1;
	UDM_menu[*kinc].size[0] = dsize[0];
	UDM_menu[*kinc].size[1] = dsize[1];
	UDM_menu[*kinc].type = UDM_MTYPE_MENU;
	UDM_menu[*kinc].num = 0;
	UDM_menu[*kinc].menus = NULL;
#if UU_COMP==UU_WIN2K
	UDM_menu[*kinc].bmpfile[0] = '\0';
	UDM_menu[*kinc].menu_area[0] = '\0';
	UDM_menu[*kinc].dockable = 1;
	UDM_menu[*kinc].justify = 0;
#endif
	UDM_menu[*kinc].key_loaded = 0;
	UDM_menu[*kinc].keyfile[0] = '\0';
	UDM_menu[*kinc].menu_format = 2;
done:;
	return(iret);
}

/*********************************************************************
**       I_FUNCTION : udm_init_menu()
**                      This function allocates memory for the menu description
**                      based on the number of menu entries.
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int udm_init_menu()
{
		int i, inc,status;
/*
.....Initialize routine
*/
	inc = UDM_menu_count;
	status = UU_SUCCESS;
	UDM_menu[inc].drawitem = 0;
/*
.....Verify menu description is valid
*/
	if (UDM_menu[inc].rows < 1 || UDM_menu[inc].cols < 1) goto failed;
/*
.....Allocate memory for menu entries
*/
	UDM_menu[inc].menus = (UDM_menu_struc *)malloc(sizeof(UDM_menu_struc)*
		(UDM_menu[inc].rows * UDM_menu[inc].cols + 20));
	if (UDM_menu[inc].menus == NULL) goto failed;
	for (i=0; i<UDM_menu[inc].rows* UDM_menu[inc].cols + 20; i++)
	{

		UDM_menu[inc].menus[i].toggle_num = 0;
		UDM_menu[inc].menus[i].toggle = 0;
		UDM_menu[inc].menus[i].chcfile[0] = '\0';
		UDM_menu[inc].menus[i].chcdef = 0;
		UDM_menu[inc].menus[i].name[0] = '\0';
		UDM_menu[inc].menus[i].file[0] = '\0';
		UDM_menu[inc].menus[i].statname[0] = '\0';
		UDM_menu[inc].menus[i].params = UU_NULL;
		UDM_menu[inc].menus[i].descrip[0] = '\0';
		UDM_menu[inc].menus[i].bgcolor[0] = '\0';
		UDM_menu[inc].menus[i].color[0] = -1;
		UDM_menu[inc].menus[i].color[1] = -1;
		UDM_menu[inc].menus[i].color[2] = -1;
/*
.....Initializing size and position to avoid UMR in Purify.
*/
		UDM_menu[inc].menus[i].pos[0] = -1;
		UDM_menu[inc].menus[i].pos[1] = -1;
		UDM_menu[inc].menus[i].size[0] = -1;
		UDM_menu[inc].menus[i].size[0] = -1;
/*
......added for bmpfile for native WinNT only
*/
#if UU_COMP == UU_WIN2K
		UDM_menu[inc].menus[i].bmpfile[0] = '\0';
		UDM_menu[inc].menus[i].bmpnum = -1;
		UDM_menu[inc].menus[i].separator = 0;
#endif
	}
	goto done;
/*
.....Invalid menu description
*/
failed:;
	status = UU_FAILURE;
done:
	return(status);
}

/*********************************************************************
**       I_FUNCTION : udm_init_toggle(ctyp,cmsg)
**                      This function allocates memory for the menu toogle
**                      based on the number of toggle choice entries.
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int udm_init_toggle(ctyp,cmsg)
char *ctyp,*cmsg;
{
		int inc,status;
/*
.....Initialize routine
*/
		inc = UDM_menu_count;
		status = UU_SUCCESS;

	strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].toggle_def, ctyp);
	strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].name, ctyp);
	strcpy(UDM_menu[inc].menus[UDM_menu[inc].num].file, cmsg);

	UDM_menu[inc].menus[UDM_menu[inc].num].toggle = 
										(UDM_menu_toggle_struc *)malloc(
										sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
	if (UDM_menu[inc].menus[UDM_menu[inc].num].toggle == NULL) goto failed;
	UDM_menu[inc].menus[UDM_menu[inc].num].toggle_num = 0;
	goto done;
/*
.....Invalid menu description
*/
failed:;
	status = UU_FAILURE;
done:
	return(status);
}
/*********************************************************************
**       I_FUNCTION : udm_check_menu()
**                      This function verifies that a menu structure is defined
**                      correctly.
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int udm_check_menu(type)
int type;
{
		int inc,status;
#if UU_COMP != UU_WIN2K
		int i;
#endif
/*
.....Initialize routine
*/
		inc = UDM_menu_count;
		status = UU_SUCCESS;
/*
.....Verify menu description is valid
*/
	if (UDM_menu[inc].rows < 1 || UDM_menu[inc].cols < 1) goto failed;
	if (UDM_menu[inc].num > (UDM_menu[inc].rows*UDM_menu[inc].cols)+20)
		goto failed;
	if (UDM_menu[inc].menus == NULL) goto failed;
/*
......when we handle menu type, we treat PULLDOWN same as POPUP
*/
/*	if ((type!=-1)&&(UDM_menu[inc].type!=type)) goto failed; */
	if ((type!=-1)&&(UDM_menu[inc].type!=type)) 
	{
		if (((UDM_menu[inc].type==UDM_MTYPE_POPUP)||(UDM_menu[inc].type==UDM_MTYPE_PULLDOWN))
			&& ((type==UDM_MTYPE_POPUP)||(type==UDM_MTYPE_PULLDOWN)))
			goto menu;
		goto failed;
	}	
menu:;
#if UU_COMP != UU_WIN2K
	if (UDM_menu[inc].num < UDM_menu[inc].rows*UDM_menu[inc].cols)
	{
/*
......fill the empty button with blank label and KEY_NOOP function in
......case it create jump label
*/
		for (i=UDM_menu[inc].num; i<UDM_menu[inc].rows*UDM_menu[inc].cols; i++)
		{
			strcpy(UDM_menu[inc].menus[i].file, "KEY_NOOP");
			strcpy(UDM_menu[inc].menus[i].name, " ");
			UDM_menu[inc].menus[i].kinc = inc;
		}
	}
#endif
/*
.....Increment menu pointer
*/
	UDM_menu_count++;
	goto done;
/*
.....Invalid menu description
*/
failed:;
/*
.....added for free toggle
.....Yurong 3/24/99
*/
	if (UDM_menu[inc].menus != NULL) 
	{
#if UU_COMP != UU_WIN2K
		for (i=0; i<UDM_menu[inc].num; i++)
		{
			if (UDM_menu[inc].menus[i].toggle != NULL )
			{
				uu_free(UDM_menu[inc].menus[i].toggle);
				UDM_menu[inc].menus[i].toggle = 0;
			}
		}
#endif
		uu_free(UDM_menu[inc].menus);
	}
	status = UU_FAILURE;
done:
	return(status);
}


/*********************************************************************
**       E_FUNCTION : udm_enter_drawing()
**                      This function enters drawing mode.
**       PARAMETERS     
**               INPUT  :  ctyp = Modal begin defined.
**                      cmsg = Modal ctyp's parameter.
**               OUTPUT :  none.
**       RETURNS: UU_SUCCESS if no problems, UU_FAILURE otherwise.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
udm_enter_drawing()
{
/*
.....Make sure we are not already
.....in drawing mode
*/
	if (UM_2d3d_mode == UM_2D)
	{
		uu_uerror0(UU_SIGNON,7);
	}
/*
.....Enter drawing mode
*/
	else
	{
		if (UM_cpln.grid.disp) umu_inactgrid();
		UM_2d3d_mode = UM_2D;
		DRAW_DISPLAY = 1;
		(*(ug_gksstli.wsopen[0].connid)[UW_RESET_PROMPT])();
		uz_status();
#if UU_COMP!=UU_WIN2K
		ud_jump(-1, UU_FALSE);
#endif
	}
return 0;
}

/*********************************************************************
**       E_FUNCTION : udm_exit_drawing()
**                      This function exits drawing mode.
**       PARAMETERS     
**               INPUT  :  none.
**               OUTPUT :  none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
udm_exit_drawing()
{
/*
.....Make sure we are
.....in drawing mode
*/
	if (UM_2d3d_mode == UM_3D)
	{
		uu_uerror0(UU_SIGNON,7);
	}
/*
.....Exit drawing mode
*/
	else
	{
		DRAW_DISPLAY = 0;
		if (UM_cpln.grid.disp) umu_inactgrid();
		UM_2d3d_mode = UM_3D;
		umu_view_model();
		(*(ug_gksstli.wsopen[0].connid)[UW_RESET_PROMPT])();
		uz_status();
#if UU_COMP!=UU_WIN2K
		ud_jump(-1, UU_FALSE);
#endif
	}
return 0;
}

/*********************************************************************
**       E_FUNCTION : udm_icon_up(devnum)
**                      This function displays an Icon menu.
**       PARAMETERS     
**               INPUT  :  devnum = device number of Icons to enable.
**               OUTPUT :  none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
udm_icon_up(devnum)
{
	int pos[2], size[2];
/*
.....Motif driver
.....Display SELECT menu only
*/
	if (devnum == SELECT_ICONS)
	{
		pos[0] = -1; pos[1] = -1;
		size[0] = -1; size[1] = -1;
		udm_read_menu("SELECT.menu",pos,size, 1, 1,-1);
	}
	return 0;
}

/*********************************************************************
**       E_FUNCTION : udm_icon_down(devnum)
**                      This function takes down an Icon menu.
**       PARAMETERS     
**               INPUT  :  devnum = device number of Icons to disable.
**               OUTPUT :  none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
udm_icon_down(devnum)
{
	int pos[2];
/*
.....Motif driver
.....Display SELECT menu only
*/
	if (devnum == SELECT_ICONS)
	{
		pos[0] = -1; pos[1] = -1;
		(*(ug_gksstli.wsopen[0].connid)[UW_DOWN_MENU])("SELECT.menu");
	}
	else if (devnum == ALTACT_ICONS)
	{
		pos[0] = -1; pos[1] = -1;
		(*(ug_gksstli.wsopen[0].connid)[UW_DOWN_MENU])("DRAFTING.menu");
	}
	return 0;
}

/*********************************************************************
**       E_FUNCTION : ud_printmsg(msg)
**           Output message into the screen
**                      For WinNT, display message box
**       PARAMETERS     
**               INPUT  :  msg: message to be output
**                                              
**               OUTPUT :  none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
void ud_printmsg(msg)
char *msg;
{
#if UU_COMP!=UU_WIN2K
	printf(msg);
#else
/*
.....Ignore this routine in batch
*/
	UM_int2 ifl, ifl35=35;
	getifl(&ifl35,&ifl);
	if (ifl == 1) return;
	uw_ntdispmsg (msg);
#endif
}
/*********************************************************************
**       E_FUNCTION : ud_ntload_toggle(minc, bindx)
**           This function load the choice button menu file.
**       PARAMETERS     
**               INPUT  :  minc: menu index
**                                                      bindx: menu bitton index
**               OUTPUT :  none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
/*
.....load toggle choice as Popup menu
*/
#if UU_COMP==UU_WIN2K
ud_ntload_toggle(minc, bindx)
int minc, bindx;
{
	int inc, status,defnum, len;
	inc = UDM_menu_count;
	status = UU_SUCCESS;
/*
.....read this menu in any case even this menu is read before
.....because this choice menu maybe read before as other menu type
*/
	UD_readmenu = 1;
	status = udm_read_menu(UDM_menu[minc].menus[bindx].chcfile,
					UDM_menu[minc].menus[bindx].pos,
					UDM_menu[minc].menus[bindx].size,0,2, -1);
	UD_readmenu = 0;
	if (status==UU_FAILURE)
		return status;
/*
.....menu type = UDM_MTYPE_POPUP
*/
	UDM_menu[inc].type = UDM_MTYPE_POPUP;
	defnum = UDM_menu[minc].menus[bindx].chcdef;
	strcpy(UDM_menu[minc].menus[bindx].name, UDM_menu[inc].menus[defnum].name);
	strcpy(UDM_menu[minc].menus[bindx].file, UDM_menu[inc].menus[defnum].file);
	strcpy(UDM_menu[minc].menus[bindx].bmpfile, UDM_menu[inc].menus[defnum].bmpfile);
	if (UDM_menu[inc].menus[defnum].params!=NULL)
	{
		len = strlen(UDM_menu[inc].menus[defnum].params);
		if (len>0)
		{
			UDM_menu[minc].menus[bindx].params = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(UDM_menu[minc].menus[bindx].params, UDM_menu[inc].menus[defnum].params);
		}
	}
	UDM_menu[minc].menus[bindx].bmpnum = UDM_menu[inc].menus[defnum].bmpnum;
	return status;
}
#endif
/*********************************************************************
**       E_FUNCTION : ud_reload_menu(menu_num)
**           This function re-load the menu. (updated the UDM_menu[menu_num]
**				and recreate the menu
**       PARAMETERS     
**               INPUT  :  menu_num: menu index to be reloaded
**                        
**               OUTPUT :  none.
**       RETURNS: none.
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
ud_reload_menu(menu_num)
int menu_num;
{
	int pos[2], size[2], drag_num;
	UX_pathname fname;
	int i, j, down_menu, save_menu_count = UDM_menu_count;
/*
......reload menu value into UDM_menu[menu_num]
*/
	UDM_menu_count = menu_num;
	strcpy(fname, UDM_menu[menu_num].file);

	if (UDM_menu[menu_num].menus == NULL) return;
	pos[0] = UDM_layout.menu_pos[menu_num][0]; 
	pos[1] = UDM_layout.menu_pos[menu_num][1];
	size[0] = UDM_layout.menu_size[menu_num][0]; 
	size[1] = UDM_layout.menu_size[menu_num][1];
/*
.....take down the menu first
*/
	down_menu = ud_down_menunum(menu_num, &drag_num);
/*
......delete old structure spaces first
*/
	for (i=0; i<UDM_menu[menu_num].num; i++)
	{
		if (UDM_menu[menu_num].menus[i].params!=UU_NULL)
		{
			uu_free(UDM_menu[menu_num].menus[i].params);
			UDM_menu[menu_num].menus[i].params = UU_NULL;
		}
		for (j=0; j<UDM_menu[menu_num].menus[i].toggle_num; j++)
		{
			if (UDM_menu[menu_num].menus[i].toggle[j].params != UU_NULL)
			{
				uu_free(UDM_menu[menu_num].menus[i].toggle[j].params);
				UDM_menu[menu_num].menus[i].toggle[j].params = UU_NULL;
			}
		}
		if (UDM_menu[menu_num].menus[i].toggle!=UU_NULL)
		{
			free(UDM_menu[menu_num].menus[i].toggle);
			UDM_menu[menu_num].menus[i].toggle = UU_NULL;
		}
	}
	uu_free(UDM_menu[menu_num].menus);
/*
.....read this menu in any case
*/
	UD_readmenu = 1;
#if UU_COMP==UU_WIN2K
	udm_read_menu(fname, pos, size,0, down_menu, -1);
#else
	udm_read_menu(fname, pos, size,1, down_menu, -1);
#endif
	UD_readmenu = 0;
	UDM_menu_count = save_menu_count;
/*
......we need reload the tear off menu too if have any
*/
#if UU_COMP==UU_WIN2K
	if (drag_num>0)
	{
		for (i=0; i<UDM_menu[drag_num].num; i++)
		{
			if (UDM_menu[drag_num].menus[i].params!=UU_NULL)
			{
				uu_free(UDM_menu[drag_num].menus[i].params);
				UDM_menu[drag_num].menus[i].params = UU_NULL;
			}
			for (j=0; j<UDM_menu[drag_num].menus[i].toggle_num; j++)
			{
				if (UDM_menu[drag_num].menus[i].toggle[j].params != UU_NULL)
				{
					uu_free(UDM_menu[drag_num].menus[i].toggle[j].params);
					UDM_menu[drag_num].menus[i].toggle[j].params = UU_NULL;
				}
			}
			if (UDM_menu[drag_num].menus[i].toggle!=UU_NULL)
			{
				free(UDM_menu[drag_num].menus[i].toggle);
				UDM_menu[drag_num].menus[i].toggle = UU_NULL;
			}
		}
		uu_free(UDM_menu[drag_num].menus);
		UDM_menu_count = drag_num;
		uw_display_dragmenu(menu_num);
		UDM_menu_count = save_menu_count;
	}
#endif
}
/*********************************************************************
**       E_FUNCTION : ud_down_menunum(menu_num, drag_num)
**           This function take down a menu
**				
**       PARAMETERS     
**               INPUT  :  menu_num: menu index to be taken down
**                        
**               OUTPUT : drag_num: take down menu's drag menu num 
**       RETURNS: 1: the menu is displayed and be taken down
**						0:	the menu is not display at all
**							
**       SIDE EFFECTS: none.
**       WARNINGS: none.
*********************************************************************/
int ud_down_menunum(menu_num, drag_num)
int menu_num, *drag_num;
{
	return (*(ug_gksstli.wsopen[0].connid)[UG_DDNMENUNUM])(menu_num, drag_num);
}

/*********************************************************************
**    S_FUNCTION     :  ud_save_dragmenu (menu)
**      save currect menu data strcture into a menu
**    PARAMETERS
**       INPUT  :
**			menu: menu item to be saved
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
int ud_save_dragmenu (menu)
int menu;
{
	UX_pathname menu_name, dir, filename, fname, tmpstr, tmpfile;
	FILE* fptr;
	char buf[UX_MAX_PATH_LEN*2], nbuf[UX_MAX_PATH_LEN*2], int_str[20], *indx;
	int i,numint, inum, status, j, num, type,nc;
	double posx, posy, cx, cy;
	int stat_flag = -1;
	static char ltype[5][64] = {"ICON","MENU","POPUP", "MENUBAR", "PULLDOWN"};
/*
......do not use the directory path, always save into user directory
*/
	if (strncmp(UDM_menu[menu].file,"Active_Temp_Menu", 16)==0)
	{
/*
.....if it is a temp file, do not save it
*/
		return 0;
	}
	strcpy(fname, UDM_menu[menu].file);
	ul_break_fname(fname, dir, menu_name);
/*
......open the menu file for writing
*/
	status = ul_open_mod_file("UU_USER_SETTINGS", "menu", (char*)UU_NULL, (char*)UU_NULL,
			menu_name, 3, &fptr);
	if (fptr==NULL) return -1;
/*
......write the recent_files.menu header into the new file
*/
	strcpy(buf, "#Descriptor#\n");
	ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
	sprintf(buf, "/NAME/ %s\n", UDM_menu[menu].name);
	ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
#if UU_COMP!=UU_WIN2K
	posx = ((double)UDM_menu[menu].pos[0])/uw_xw.dev_xmax;
	posy = ((double)UDM_menu[menu].pos[1])/uw_xw.dev_ymax;
#else
	posx = ((double)UDM_menu[menu].pos[0])/uw_gl.dev_xmax;
	posy = ((double)UDM_menu[menu].pos[1])/uw_gl.dev_ymax;
#endif
	sprintf(buf, "/POSITION/ %f,%f\n", posx, posy);
	ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
#if UU_COMP!=UU_WIN2K
	cx = ((double)UDM_menu[menu].size[0])/uw_xw.dev_xmax;
	cy = ((double)UDM_menu[menu].size[1])/uw_xw.dev_ymax;
#else
	cx = ((double)UDM_menu[menu].size[0])/uw_gl.dev_xmax;
	cy = ((double)UDM_menu[menu].size[1])/uw_gl.dev_ymax;
#endif
	sprintf(buf, "/SIZE/ %f,%f\n", cx, cy);
	ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
	sprintf(buf, "/ROWS/ %d\n", UDM_menu[menu].rows);
	ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
	sprintf(buf, "/COLS/ %d\n", UDM_menu[menu].cols);
	ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
	type = UDM_menu[menu].type;
	sprintf(buf, "/TYPE/ %s\n", ltype[type]);
	ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
#if UU_COMP==UU_WIN2K
	if (UDM_menu[menu].bmpfile[0]!='\0')
	{
		sprintf(buf, "/BITMAP/ %s\n", UDM_menu[menu].bmpfile);
		ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
	}
	if (UDM_menu[menu].menu_area[0]!='\0')
	{
		sprintf(buf, "/AREA/ %s\n", UDM_menu[menu].menu_area);
		ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
	}
	if (UDM_menu[menu].dockable)
		strcpy(buf, "/DOCKABLE/ YES\n");
	else
		strcpy(buf, "/DOCKABLE/ No\n");
	ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);

	if (UDM_menu[menu].justify)
		strcpy(buf, "/JUSTIFY/ RIGHT\n");
	else
		strcpy(buf, "/JUSTIFY/ LEFT\n");
	ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);

#endif
	if (UDM_menu[menu].keyfile[0]!='\0')
	{
		sprintf(buf, "/KEYDEFS/ %s\n", UDM_menu[menu].keyfile);
		ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
	}
	if (UDM_menu[menu].menu_format==0)
	{
		strcpy(buf, "/FORMAT/ ICON\n");
	}
	else if (UDM_menu[menu].menu_format==1)
	{
		strcpy(buf, "/FORMAT/ TEXT\n");
	}
	else
	{
		strcpy(buf, "/FORMAT/ BOTH\n");
	}
	ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
	strcpy(buf, "\n");
	ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
/*
......write menu item
*/
/*	if (UDM_menu[menu].statflag==1)
		strcpy(buf, "#BUTTONS#\n");
	else
		strcpy(buf, "#MENUS#\n");
	ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
*/
	for (i=0; i<UDM_menu[menu].num;i++)
	{
		if ((UDM_menu[menu].menus[i].statname[0]!='\0')&&(stat_flag!=1))
		{
			stat_flag = 1;
			strcpy(buf, "#BUTTONS#\n");
			ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
		}
		else if ((stat_flag!=0)&&(UDM_menu[menu].menus[i].statname[0]=='\0'))
		{
			stat_flag = 0;
			strcpy(buf, "#MENUS#\n");
			ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
		}
/* #choice# */
		if (UDM_menu[menu].menus[i].chcfile[0]!='\0')
		{
			sprintf(buf, "#choice# %s\n", UDM_menu[menu].menus[i].chcfile);
			if (UDM_menu[menu].menus[i].chcdef!=0)
			{
				sprintf(int_str, ", %d", UDM_menu[menu].menus[i].chcdef);
				strcat(buf, int_str);
			}
			ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
			continue;
		}
		if (UDM_menu[menu].menus[i].separator)
		{
			strcpy(buf, "#Bar#\n");
			ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
			continue;
		}
/*
......put item define first
*/
/*
		sprintf(buf, "/%s/ %s", UDM_menu[menu].menus[i].name, 
				UDM_menu[menu].menus[i].file);
*/
		if (UDM_menu[menu].menus[i].statname[0]!=0)
			sprintf(buf, "/%s/ %s", UDM_menu[menu].menus[i].name, 
					UDM_menu[menu].menus[i].statname);
		else
			sprintf(buf, "/%s/ %s", UDM_menu[menu].menus[i].name,
					UDM_menu[menu].menus[i].file);

		if (UDM_menu[menu].menus[i].descrip[0]!='\0')
		{
			strcat(buf, ",<");
			strcat(buf, UDM_menu[menu].menus[i].descrip);
			strcat(buf, ">");
		}
		if (UDM_menu[menu].menus[i].statname[0]!=0)
		{
			strcat(buf, ", ");
			strcat(buf, UDM_menu[menu].menus[i].file);
		}
		if (UDM_menu[menu].menus[i].params!=NULL)
		{
			strcat(buf, ",\"");
			strcat(buf, UDM_menu[menu].menus[i].params);
			strcat(buf, "\"");
		}
		if (UDM_menu[menu].menus[i].bmpfile[0]!='\0')
		{
/*
......compare the bitmap file name to see if they are the default name
......if yes, do not save it
*/
			strcpy(tmpfile, UDM_menu[menu].menus[i].bmpfile);
			strcpy(tmpstr, UDM_menu[menu].menus[i].file);
			indx = (char*)strchr(tmpfile, '.');
			if (indx!=0)
				*indx = 0;
			indx = (char*)strchr(tmpstr, '.');
			if (indx!=0)
				*indx = 0;
			nc  = strlen(tmpstr);
			if (strncmp(tmpstr, UDM_menu[menu].menus[i].bmpfile, nc)!=0)
			{
				strcat(buf, ",");
				strcat(buf, UDM_menu[menu].menus[i].bmpfile);
				if (UDM_menu[menu].menus[i].bmpnum>0)
				{
					sprintf(int_str, ", %d", UDM_menu[menu].menus[i].bmpnum);
					strcat(buf, int_str);
				}
			}
		}
		strcat(buf, "\n");
		ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
/*
......toggle filed is for old format WNT and motif menu design, so ignore here
*/
		if (UDM_menu[menu].menus[i].bgcolor[0]!='\0')
		{
			sprintf(buf, "#Color# %s\n", UDM_menu[menu].menus[i].bgcolor);
			ux_fwrite0(buf, strlen(buf), 1, fptr, &inum);
		}
	}
	if (fptr != 0) 
		ux_fclose0(fptr);
}

/*********************************************************************
**    S_FUNCTION     :  ud_upt_UDMmenu(upt_menu, upt_item, add_menu, add_item, choice)
**       update two menus by drag a item into another menu and save(data only)
**    PARAMETERS
**       INPUT  :
**			upt_menu: updated menu which insert a item at upt_item
**			add_menu: menu which add_item is the item added into upt_menu
**			choice: which choice is 2: copy, so the add_menu will be un-changed
**					1: moved: so the add_menu will be changed (add_item is removed from add_menu)
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void ud_upt_UDMmenu(upt_menu, upt_item, add_menu, add_item, choice)
int upt_menu, upt_item, add_menu, add_item, choice;
{
	int i,j,k,len, items;
	UDM_MENU tempdata;

	uu_move_byte((char*)&(UDM_menu[upt_menu]), (char*)&tempdata, sizeof(UDM_MENU));
	tempdata.menus = (UDM_menu_struc *)malloc(sizeof(UDM_menu_struc)*
						(UDM_menu[upt_menu].num + 1));
	if (upt_item==UDM_menu[upt_menu].num)
		items = UDM_menu[upt_menu].num + 1;
	else
		items = UDM_menu[upt_menu].num;

	for (i=0,k=0; i<items; i++,k++)
	{
		if ((i==upt_item)&&(k==upt_item))
		{
/*
.......added new item here
*/
			uu_move_byte((char*)&(UDM_menu[add_menu].menus[add_item]), (char*)&(tempdata.menus[k]),
					sizeof(UDM_menu_struc));
			if (UDM_menu[add_menu].menus[add_item].params!=NULL)
			{
				len = strlen(UDM_menu[add_menu].menus[add_item].params);
				if (len>0)
				{
					tempdata.menus[k].params = (char*)uu_malloc((len+1)*sizeof(char));
					strcpy(tempdata.menus[k].params, UDM_menu[add_menu].menus[add_item].params);
				}
				else
					tempdata.menus[k].params = (char*)UU_NULL;
			}
			else
				tempdata.menus[k].params = (char*)UU_NULL;
			if ((UDM_menu[add_menu].menus[add_item].toggle_num>0)&&(UDM_menu[add_menu].menus[add_item].toggle!=0))
			{
				tempdata.menus[k].toggle = (UDM_menu_toggle_struc *)malloc(
											sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
				for (j=0; j<UDM_menu[add_menu].menus[add_item].toggle_num;j++)
				{
					uu_move_byte((char*)&(UDM_menu[add_menu].menus[add_item].toggle[j]), (char*)&(tempdata.menus[k].toggle[j]),
							sizeof(UDM_menu_toggle_struc));
					if (UDM_menu[add_menu].menus[add_item].toggle[j].params!=NULL)
					{
						len = strlen(UDM_menu[add_menu].menus[add_item].toggle[j].params);
						if (len>0)
						{
							tempdata.menus[k].toggle[j].params = (char*)uu_malloc((len+1)*sizeof(char));
							strcpy(tempdata.menus[k].toggle[j].params, UDM_menu[add_menu].menus[add_item].toggle[j].params);
						}
						else
							tempdata.menus[k].toggle[j].params = (char*)UU_NULL;
					}
					else
						tempdata.menus[k].toggle[j].params = (char*)UU_NULL;
				}
			}
			k++;
			if (i==UDM_menu[upt_menu].num)
				break;
		}
		uu_move_byte((char*)&(UDM_menu[upt_menu].menus[i]), (char*)&(tempdata.menus[k]),
				sizeof(UDM_menu_struc));
		if (UDM_menu[upt_menu].menus[i].params!=NULL)
		{
			len = strlen(UDM_menu[upt_menu].menus[i].params);
			if (len>0)
			{
				tempdata.menus[k].params = (char*)uu_malloc((len+1)*sizeof(char));
				strcpy(tempdata.menus[k].params, UDM_menu[upt_menu].menus[i].params);
				uu_free (UDM_menu[upt_menu].menus[i].params);
			}
			else
				tempdata.menus[k].params = (char*)UU_NULL;
		}
		else
			tempdata.menus[k].params = (char*)UU_NULL;
		if ((UDM_menu[upt_menu].menus[i].toggle_num>0)&&(UDM_menu[upt_menu].menus[i].toggle!=0))
		{
			tempdata.menus[k].toggle = (UDM_menu_toggle_struc *)malloc(
										sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
			for (j=0; j<UDM_menu[upt_menu].menus[i].toggle_num;j++)
			{
				uu_move_byte((char*)&(UDM_menu[upt_menu].menus[i].toggle[j]), (char*)&(tempdata.menus[k].toggle[j]),
						sizeof(UDM_menu_toggle_struc));
				if (UDM_menu[upt_menu].menus[i].toggle[j].params!=NULL)
				{
					len = strlen(UDM_menu[upt_menu].menus[i].toggle[j].params);
					if (len>0)
					{
						tempdata.menus[k].toggle[j].params = (char*)uu_malloc((len+1)*sizeof(char));
						strcpy(tempdata.menus[k].toggle[j].params, UDM_menu[upt_menu].menus[i].toggle[j].params);
						uu_free (UDM_menu[upt_menu].menus[i].toggle[j].params);
					}
					else
						tempdata.menus[k].toggle[j].params = (char*)UU_NULL;
				}
				else
					tempdata.menus[k].toggle[j].params = (char*)UU_NULL;
			}
			free (UDM_menu[upt_menu].menus[i].toggle);
		}
	}
	free (UDM_menu[upt_menu].menus);
/*
.....the tempdata back into the menu
*/
	uu_move_byte((char*)&tempdata, (char*)&(UDM_menu[upt_menu]), sizeof(UDM_MENU));
	UDM_menu[upt_menu].num++;

/*
.......save the changed menu into a file in user directory
*/
/*
......do not automatically save a menu anymore
*/
/*	ud_save_dragmenu(upt_menu); */
	if (choice!=1)
		return;
/*
......remove add_item from add_menu
*/
	if (upt_menu==add_menu)
	{
/*
......same menu move
*/
		if (upt_item<add_item)
			add_item++;
	}
	if (UDM_menu[add_menu].num-1<=0)
	{
		return;
	}
	uu_move_byte((char*)&(UDM_menu[add_menu]), (char*)&tempdata, sizeof(UDM_MENU));
	tempdata.menus = (UDM_menu_struc *)malloc(sizeof(UDM_menu_struc)*
						(UDM_menu[add_menu].num + 1));
	for (i=0,k=0; i<UDM_menu[add_menu].num;i++,k++)
	{
		if ((i==add_item)&&(k==add_item))
		{
/*
.....free current space
*/
			if (UDM_menu[add_menu].menus[k].params!=NULL)
				uu_free (UDM_menu[add_menu].menus[k].params);
			if ((UDM_menu[add_menu].menus[k].toggle_num>0)&&(UDM_menu[add_menu].menus[k].toggle!=0))
			{
				for (j=0; j<UDM_menu[add_menu].menus[k].toggle_num;j++)
				{
					if (UDM_menu[add_menu].menus[k].toggle[j].params!=NULL)
					{
						uu_free (UDM_menu[add_menu].menus[k].toggle[j].params);
					}
				}
				free (UDM_menu[add_menu].menus[k].toggle);
			}
			i++;
/*
......if the item is the last one, done
*/
		}
		if (i==UDM_menu[add_menu].num)
			goto done;
		uu_move_byte((char*)&(UDM_menu[add_menu].menus[i]), (char*)&(tempdata.menus[k]),
				sizeof(UDM_menu_struc));
		if (UDM_menu[add_menu].menus[i].params!=NULL)
		{
			len = strlen(UDM_menu[add_menu].menus[i].params);
			if (len>0)
			{
				tempdata.menus[k].params = (char*)uu_malloc((len+1)*sizeof(char));
				strcpy(tempdata.menus[k].params, UDM_menu[add_menu].menus[i].params);
				uu_free (UDM_menu[add_menu].menus[i].params);
			}
			else
				tempdata.menus[k].params = (char*)UU_NULL;
		}
		else
			tempdata.menus[k].params = (char*)UU_NULL;
		if ((UDM_menu[add_menu].menus[i].toggle_num>0)&&(UDM_menu[add_menu].menus[i].toggle!=0))
		{
			tempdata.menus[k].toggle = (UDM_menu_toggle_struc *)malloc(
										sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
			for (j=0; j<UDM_menu[add_menu].menus[i].toggle_num;j++)
			{
				uu_move_byte((char*)&(UDM_menu[add_menu].menus[i].toggle[j]), (char*)&(tempdata.menus[k].toggle[j]),
						sizeof(UDM_menu_toggle_struc));
				if (UDM_menu[add_menu].menus[i].toggle[j].params!=NULL)
				{
					len = strlen(UDM_menu[add_menu].menus[i].toggle[j].params);
					if (len>0)
					{
						tempdata.menus[k].toggle[j].params = (char*)uu_malloc((len+1)*sizeof(char));
						strcpy(tempdata.menus[k].toggle[j].params, UDM_menu[add_menu].menus[i].toggle[j].params);
						uu_free (UDM_menu[add_menu].menus[i].toggle[j].params);
					}
					else
						tempdata.menus[k].toggle[j].params = (char*)UU_NULL;
				}
				else
					tempdata.menus[k].toggle[j].params = (char*)UU_NULL;
			}
			free (UDM_menu[add_menu].menus[i].toggle);
		}
	}
done:;
	free (UDM_menu[add_menu].menus);
/*
.....the tempdata back into the menu
*/
	uu_move_byte((char*)&tempdata, (char*)&(UDM_menu[add_menu]), sizeof(UDM_MENU));
	UDM_menu[add_menu].num--;
/*
.......save the changed menu into a file in user directory
*/
/*
......do not automatically save a menu anymore
*/
/*	if (add_menu!=upt_menu)
		ud_save_dragmenu(add_menu);
*/
}

/*********************************************************************
**    S_FUNCTION     :  ud_del_UDMmenu(menu, item)
**       delete a menu item from a menu and save the change (data only)
**    PARAMETERS
**       INPUT  :
**          menu:  menu number
**          item:  menu item number
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void ud_del_UDMmenu(menu, item)
int menu, item;
{
	int i,j,k,len;
	UDM_MENU tempdata;

	uu_move_byte((char*)&(UDM_menu[menu]), (char*)&tempdata, sizeof(UDM_MENU));
	tempdata.menus = (UDM_menu_struc *)malloc(sizeof(UDM_menu_struc)*
						(UDM_menu[menu].num + 1));
	for (i=0,k=0; i<UDM_menu[menu].num;i++,k++)
	{
		if ((i==item)&&(k==item))
		{
/*
.....free current space
*/
			if (UDM_menu[menu].menus[k].params!=NULL)
				uu_free (UDM_menu[menu].menus[k].params);
			if ((UDM_menu[menu].menus[k].toggle_num>0)&&(UDM_menu[menu].menus[k].toggle!=0))
			{
				for (j=0; j<UDM_menu[menu].menus[k].toggle_num;j++)
				{
					if (UDM_menu[menu].menus[k].toggle[j].params!=NULL)
					{
						uu_free (UDM_menu[menu].menus[k].toggle[j].params);
					}
				}
				free (UDM_menu[menu].menus[k].toggle);
			}
			i++;
		}
		uu_move_byte((char*)&(UDM_menu[menu].menus[i]), (char*)&(tempdata.menus[k]),
				sizeof(UDM_menu_struc));
		if (UDM_menu[menu].menus[i].params!=NULL)
		{
			len = strlen(UDM_menu[menu].menus[i].params);
			if (len>0)
			{
				tempdata.menus[k].params = (char*)uu_malloc((len+1)*sizeof(char));
				strcpy(tempdata.menus[k].params, UDM_menu[menu].menus[i].params);
				uu_free (UDM_menu[menu].menus[i].params);
			}
			else
				tempdata.menus[k].params = (char*)UU_NULL;
		}
		else
			tempdata.menus[k].params = (char*)UU_NULL;
		if ((UDM_menu[menu].menus[i].toggle_num>0)&&(UDM_menu[menu].menus[i].toggle!=0))
		{
			tempdata.menus[k].toggle = (UDM_menu_toggle_struc *)malloc(
										sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
			for (j=0; j<UDM_menu[menu].menus[i].toggle_num;j++)
			{
				uu_move_byte((char*)&(UDM_menu[menu].menus[i].toggle[j]), (char*)&(tempdata.menus[k].toggle[j]),
						sizeof(UDM_menu_toggle_struc));
				if (UDM_menu[menu].menus[i].toggle[j].params!=NULL)
				{
					len = strlen(UDM_menu[menu].menus[i].toggle[j].params);
					if (len>0)
					{
						tempdata.menus[k].toggle[j].params = (char*)uu_malloc((len+1)*sizeof(char));
						strcpy(tempdata.menus[k].toggle[j].params, UDM_menu[menu].menus[i].toggle[j].params);
						uu_free (UDM_menu[menu].menus[i].toggle[j].params);
					}
					else
						tempdata.menus[k].toggle[j].params = (char*)UU_NULL;
				}
				else
					tempdata.menus[k].toggle[j].params = (char*)UU_NULL;
			}
			free (UDM_menu[menu].menus[i].toggle);
		}
	}
	free (UDM_menu[menu].menus);
/*
.....the tempdata back into the menu
*/
	uu_move_byte((char*)&tempdata, (char*)&(UDM_menu[menu]), sizeof(UDM_MENU));
	UDM_menu[menu].num--;
/*
.......save the changed menu into a file in user directory
*/
/*
......do not automatically save a menu anymore
*/
/*	ud_save_dragmenu(menu); */
}

char **ud_get_menuarea_list(number)
int *number;
{
	int i, j, len, add;
	char **menu_area_lst, menu_area[256];

	menu_area_lst = (char **) uu_malloc(UDM_layout.nmenu *sizeof(char *));
	*number = 0;
	for (i=0; i<UDM_layout.nmenu; i++)
	{
		add = 1;
		for (j=0; j<*number; j++)
		{
			if (strcmp(menu_area_lst[j], UDM_layout.menu_area[i])==0)
			{
				add = 0;
				break;
			}
		}
		if (add==1)
		{
			len = strlen (UDM_layout.menu_area[i]);
			if (len>0)
			{
				menu_area_lst[*number] = (char *) uu_malloc(30 * sizeof(char));	
				strcpy(menu_area_lst[*number], UDM_layout.menu_area[i]);
				(*number)++;
			}
		}
	}
	if ((*number==0)&&(menu_area_lst!=NULL))
	{
		uu_free(menu_area_lst);
		return NULL;
	}
	return (menu_area_lst);
}

void ud_insert_UDMmenu(upt_menu, upt_item, menu_item)
int upt_menu, upt_item;
UDM_menu_struc* menu_item;
{
	int i,j,k,len;
	UDM_MENU tempdata;

	uu_move_byte((char*)&(UDM_menu[upt_menu]), (char*)&tempdata, sizeof(UDM_MENU));
	tempdata.menus = (UDM_menu_struc *)malloc(sizeof(UDM_menu_struc)*
						(UDM_menu[upt_menu].num + 1 + 1));
	for (i=0,k=0; ((i<UDM_menu[upt_menu].num)||((i==upt_item)&&(k==upt_item)));i++,k++)
	{
		if ((i==upt_item)&&(k==upt_item))
		{
/*
.......added new item here
*/
			uu_move_byte((char*)menu_item, (char*)&(tempdata.menus[k]),
					sizeof(UDM_menu_struc));
			if (menu_item->params!=NULL)
			{
				len = strlen(menu_item->params);
				if (len>0)
				{
					tempdata.menus[k].params = (char*)uu_malloc((len+1)*sizeof(char));
					strcpy(tempdata.menus[k].params, menu_item->params);
				}
				else
					tempdata.menus[k].params = (char*)UU_NULL;
			}
			else
				tempdata.menus[k].params = (char*)UU_NULL;
			if ((menu_item->toggle_num>0)&&(menu_item->toggle!=0))
			{
				tempdata.menus[k].toggle = (UDM_menu_toggle_struc *)malloc(
											sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
				for (j=0; j<menu_item->toggle_num;j++)
				{
					uu_move_byte((char*)&(menu_item->toggle[j]), (char*)&(tempdata.menus[k].toggle[j]),
							sizeof(UDM_menu_toggle_struc));
					if (menu_item->toggle[j].params!=NULL)
					{
						len = strlen(menu_item->toggle[j].params);
						if (len>0)
						{
							tempdata.menus[k].toggle[j].params = (char*)uu_malloc((len+1)*sizeof(char));
							strcpy(tempdata.menus[k].toggle[j].params, menu_item->toggle[j].params);
						}
						else
							tempdata.menus[k].toggle[j].params = (char*)UU_NULL;
					}
					else
						tempdata.menus[k].toggle[j].params = (char*)UU_NULL;
				}
			}
			if (upt_item==UDM_menu[upt_menu].num)
				goto done;
			k++;
		}
		uu_move_byte((char*)&(UDM_menu[upt_menu].menus[i]), (char*)&(tempdata.menus[k]),
				sizeof(UDM_menu_struc));
		if (UDM_menu[upt_menu].menus[i].params!=NULL)
		{
			len = strlen(UDM_menu[upt_menu].menus[i].params);
			if (len>0)
			{
				tempdata.menus[k].params = (char*)uu_malloc((len+1)*sizeof(char));
				strcpy(tempdata.menus[k].params, UDM_menu[upt_menu].menus[i].params);
				uu_free (UDM_menu[upt_menu].menus[i].params);
			}
			else
				tempdata.menus[k].params = (char*)UU_NULL;
		}
		else
			tempdata.menus[k].params = (char*)UU_NULL;
		if ((UDM_menu[upt_menu].menus[i].toggle_num>0)&&(UDM_menu[upt_menu].menus[i].toggle!=0))
		{
			tempdata.menus[k].toggle = (UDM_menu_toggle_struc *)malloc(
										sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
			for (j=0; j<UDM_menu[upt_menu].menus[i].toggle_num;j++)
			{
				uu_move_byte((char*)&(UDM_menu[upt_menu].menus[i].toggle[j]), (char*)&(tempdata.menus[k].toggle[j]),
						sizeof(UDM_menu_toggle_struc));
				if (UDM_menu[upt_menu].menus[i].toggle[j].params!=NULL)
				{
					len = strlen(UDM_menu[upt_menu].menus[i].toggle[j].params);
					if (len>0)
					{
						tempdata.menus[k].toggle[j].params = (char*)uu_malloc((len+1)*sizeof(char));
						strcpy(tempdata.menus[k].toggle[j].params, UDM_menu[upt_menu].menus[i].toggle[j].params);
						uu_free (UDM_menu[upt_menu].menus[i].toggle[j].params);
					}
					else
						tempdata.menus[k].toggle[j].params = (char*)UU_NULL;
				}
				else
					tempdata.menus[k].toggle[j].params = (char*)UU_NULL;
			}
			free (UDM_menu[upt_menu].menus[i].toggle);
		}
	}
done:;
/*
......before free memory, tell the DrawItem routine to not draw
*/
	UDM_menu[upt_menu].drawitem = 0;
	free (UDM_menu[upt_menu].menus);
/*
.....the tempdata back into the menu
*/
	uu_move_byte((char*)&tempdata, (char*)&(UDM_menu[upt_menu]), sizeof(UDM_MENU));
/*
......all tempdata's drawitem initial is 1 from the copy
......we need keep it as 0 until displayed
*/
	UDM_menu[upt_menu].drawitem = 0;
	UDM_menu[upt_menu].num++;

/*
.......save the changed menu into a file in user directory
*/
/*
......do not automatically save a menu anymore
*/
/*	ud_save_dragmenu(upt_menu); */
}

/*********************************************************************
**    S_FUNCTION     :  static OnMenuType(fieldno, val, stat)
**       Method called at Menu Type Selection field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnMenuType(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
/*
.....Get the material name
*/
	switch (val->frmint[0])
	{
		case 0:
			ud_set_traverse_mask(1, UU_TRUE);
			break;
		case 1:
		case 2:
		case 3:
			ud_set_traverse_mask(1, UU_FALSE);
			break;
	}
	return UD_FLDOK;
}

/*********************************************************************
**    S_FUNCTION     :  static Onbrowser(fieldno, val, stat)
**       Method called at 'browser' button field.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT Onbrowser(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	UX_pathname filename, tempstr, dir, fdir;
	int len;
	UD_DDATA data;
/*
.....Get the material name
*/
	data.frmstr = tempstr;
	ud_get_field(2,data,UU_FALSE);
	strcpy(filename, tempstr);

	if (tempstr[0]=='\0')
	{
		ul_get_full_dir("UU_USER_SETTINGS", dir);
		ul_build_full_dir(dir, "menu", fdir);
		strcpy(filename, fdir);
		if (Stype==0)
			strcat(filename, "\\*.menu");
		else
			strcat(filename, "\\*.stat");
	}
	if (Stype==0)
		ud_get_filename("browse", "Menu file", "*.menu",
						filename, &len, "Menu Files(*.menu)", UU_FALSE) ;
	else
		ud_get_filename("browse", "Status file", "*.stat",
						filename, &len, "Status Files(*.stat)", UU_FALSE) ;
	if (filename[0]!='\0')
	{
		ud_update_answer(*fieldno-1, (int*)filename);
	}
	else
		*fieldno = -1;
	return UD_FLDOK;
}

/*********************************************************************
**    S_FUNCTION     :  ud_frm_getmenu_name(fname, menu_type, menu_area, format, dockable, title)
**       display a form to get a menu information from the user
**       
**    PARAMETERS
**       INPUT  :
**          fname: menu file name
**			menu_type: menu type
**          menu_area:     menu area
**          format:     menu format
**			dockable: menu dockable
**			title: menu title
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
/*
.....title only 30 char as define in char name[30] of UDM_MENU now
*/
ud_frm_getmenu_name(fname, menu_type, menu_area, format, dockable, title, type)
char *fname, *menu_area, *title;
int *menu_type, *format, *dockable, type;
{
	int i, j, status, len;
	int mainmarkval=0;
	int *ans[7], but;
	char *p, menu_label[30], menu_name[UX_MAX_FILE_LEN], bmp_name[UX_MAX_FILE_LEN], 
		params[UX_MAX_FILE_LEN*2], descrip[40];
	UD_FSTAT uj_noop();
	UX_pathname menufile, dir, fdir;
	char menu_title[30];
	static int Smenutype, Sdockable, Sformat;
	static UD_LIST area_list;
	static char Smenu_area[256];
	static UD_METHOD methods[7] = {OnMenuType, uj_noop, uj_noop, Onbrowser, uj_noop, uj_noop, uj_noop};
	static char called[] = { 6,6,6,6,6,6,6};
	static char traverse[] = {1,1,1,1,1,1,1};
	static char display[] = {1,1,1,1,1,1,1};  

	if (*menu_type==4)
		*menu_type = 3;
	Stype = type;
	Smenutype = *menu_type;
	strcpy(Smenu_area, menu_area);
	Sdockable = *dockable;
	Sformat = *format;
	if ((title!=NULL)&&(title[0]!='\0'))
		strcpy(menu_title, title);
	else
		menu_title[0] = '\0';
	if ((fname!=NULL)&&(fname[0]!='\0'))
		strcpy(menufile, fname);
	else
	{
		menufile[0] = '\0';
	}
	area_list.item = ud_get_menuarea_list(&(area_list.num_item));
	area_list.answer = (char *) uu_malloc(256 * sizeof(char));
	strcpy(area_list.answer, Smenu_area);
	if (type!=0)
	{
		traverse[0] = 0;
		traverse[1] = 1;
		traverse[4] = 0;
		traverse[5] = 0;
		Sformat = 1;
		Sdockable = 1;
	}
	else
	{
		if (Smenutype!=0)
			traverse[1] = 0;
		else
			traverse[1] = 1;
	}
	ans[0] = (int*)&Smenutype;
	ans[1] = (int*)&area_list;
	ans[2] = (int*)menufile;
	ans[3] = (int*)&but;
	ans[4] = (int*)&Sdockable;
	ans[5] = (int*)&Sformat;
	ans[6] = (int*)menu_title;

	UD_MARK (mainmarkval,UU_FALSE);
	if (mainmarkval != 0)
	{
		goto done;
	}
	status = ud_form1("menufile.frm", ans, ans, methods, called, display,
				traverse);
	if (status==-1)
	{
		goto done;
	}
	if (Smenutype==3)
		*menu_type = 4;
	else
		*menu_type = Smenutype;
	strcpy(Smenu_area, area_list.answer);
	strcpy(menu_area, Smenu_area);
/*
......remove trailing space
*/
	for (i=strlen(menufile); i>0; i--)
	{
		if (menufile[i-1]==' ')
			menufile[i-1] = '\0';
		else
			break;
	}
	p = (char *)strrchr(menufile,'.');
	if (p == 0)
	{
		if (strlen(menufile)>0)
		{
			if (type==0)
				strcat(menufile, ".menu");
			else
				strcat(menufile, ".stat");
		}
	}
/*	else
	{
		*p = 0;
		if (strlen(menufile)>0)
			strcat(menufile, ".menu");
	}
*/
	if (strlen(menufile)>0)
		strcpy(fname, menufile);
	else
		fname[0] = '\0';
	*dockable = Sdockable;
	*format = Sformat;
	strcpy(title, menu_title);
done:;
	ud_free_flist(&area_list);
	UD_UNMARK (mainmarkval);
	return (status);
}

/*********************************************************************
**    S_FUNCTION     :  ud_create_UDMmenu(menunum1, itemnum1, filename, choice, pos)
**       Create a new menu using a menu item from a menu. optional delete the old menu item
**       
**    PARAMETERS
**       INPUT  :
**          menunum1: menu file number which strcture to get from
**			itemnum1: menu item which strcture to get from
**          filename:     initial menu filename to save
**          pos:     inital position to save
**			choice: flag to see if we need delete the old item from the menu
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
int ud_create_UDMmenu(menunum1, itemnum1, filename, choice, pos)
int menunum1, itemnum1, choice, pos[5];
char *filename;
{
	int menu = ud_create_UDMmenu2(UDM_menu[menunum1].menus[itemnum1], filename, pos);
	if (menu==-1)
		return -1;
	if (choice!=1)
		return menu;
	ud_del_UDMmenu(menunum1, itemnum1);
	return menu;
}

/*********************************************************************
**    S_FUNCTION     :  ud_create_UDMmenu2(menu_item, filename, pos)
**       Create a new menu using the menu item structure.
**       
**    PARAMETERS
**       INPUT  :
**          menu_item: menu item strcture to create a menu
**          filename:     initial menu filename to save
**          pos:     inital position to save
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
int ud_create_UDMmenu2(menu_item, filename, pos)
UDM_menu_struc menu_item;
int pos[5];
char *filename;
{
	UX_pathname fname, msg;
	char title[30];
	int i, j, k, menu, len, ans, status, statflag;
	char menu_area[256];
	UDM_menu_struc *temp;
	int format, dockable = 1;
	int menu_type = UDM_MTYPE_MENU;
/*
.....create a new menu
*/
	menu = UDM_menu_count;
	temp = &menu_item;
	if (temp->statname[0]!='\0')
		statflag = 1;
	else
		statflag = 0; 
	if (pos[2]==1)
		strcpy(menu_area, "TOP");
	else if (pos[2]==2)
		strcpy(menu_area, "BOTTOM");
	else if (pos[2]==3)
		strcpy(menu_area, "LEFT");
	else if (pos[2]==4)
		strcpy(menu_area, "RIGHT");
	else
		menu_area[0] = '\0';
/*
.....if there is no filename, need get the filename, type, area field and dockable choice
*/
	if ((filename==NULL)||(filename[0]=='\0'))
	{
		fname[0] = '\0';
	}
	else
	{
		strcpy(fname, filename);
	}
	title[0] = '\0';
	format = 2;
	status = ud_frm_getmenu_name(fname, &menu_type, menu_area, &format, &dockable, title, statflag);
	if (status==-1)
		return -1;
	if (fname[0]=='\0')
		goto step1;
/*
.....See if this menu is already loaded
*/
	for (i=0;i<UDM_menu_count;i++)
	{
		if (strcmp(UDM_menu[i].file, fname) == 0)
		{
/*
.....this menu file have already loaded, do you want to remove the loaded menu
.....and created the new one, if no, just return -1;
*/
			strcpy(msg, "The menu have already loaded, do you want to remove the loaded menu\r\n");
			strcat(msg, "and created the new one, Select No will cancel the action");
			ans = ud_yesno2(0, msg, "Menu loaded!");
			if (ans==0)
				return -1;
/*
.....remove the existing menu
*/
			uw_ntremove_menu(i);
			if (UDM_menu[i].menus!=NULL)
			{
				for (j=0; j<UDM_menu[menu].num;j++)
				{
					if (UDM_menu[menu].menus[j].params!=UU_NULL)
					{
						uu_free(UDM_menu[menu].menus[j].params);
						UDM_menu[menu].menus[j].params = UU_NULL;
					}
					for (k=0; k<UDM_menu[menu].menus[j].toggle_num; j++)
					{
						if (UDM_menu[menu].menus[j].toggle[k].params != UU_NULL)
						{
							uu_free(UDM_menu[menu].menus[j].toggle[k].params);
							UDM_menu[menu].menus[j].toggle[k].params = UU_NULL;
						}
					}
					if (UDM_menu[menu].menus[j].toggle!=UU_NULL)
					{
						free(UDM_menu[menu].menus[j].toggle);
						UDM_menu[menu].menus[j].toggle = UU_NULL;
					}
				}
				free (UDM_menu[i].menus);
				UDM_menu[i].menus = NULL;
			}
			menu = i;
			break;
		}
	}
step1:;
/*
.....Make sure we don't have too many menus
*/
	if (menu >= UDM_MAX_MENU)
	{
		return -1;
	}
	UDM_menu[menu].statflag = 0;
/*
.....Initialize this menu
*/
	if (title[0]=='\0')
		sprintf(UDM_menu[menu].name,"Menu_%d",menu);
	else
		strcpy(UDM_menu[menu].name,title);
	if (fname[0]=='\0')
		sprintf(UDM_menu[menu].file,"Active_Temp_Menu_%d",menu);
	else
		strcpy(UDM_menu[menu].file,fname);
	UDM_menu[menu].rows = 1;
	UDM_menu[menu].cols = 1;
	UDM_menu[menu].size[0] = -1;
	UDM_menu[menu].size[1] = -1;
	if ((pos[0]==-1)&&(pos[1]==-1)&&(pos[2]==-1))
	{
		UDM_menu[menu].pos[0] = pos[3];
		UDM_menu[menu].pos[1] = pos[4];
	}
	else
	{
/*
.....pos[0] = bar_index of that area
*/
		UDM_menu[menu].pos[0] = pos[0];
	}
	UDM_menu[menu].type = menu_type;
	UDM_menu[menu].num = 1;
	UDM_menu[menu].menus = (UDM_menu_struc *)malloc(sizeof(UDM_menu_struc)*2);
/*
.......added new item here
*/
	uu_move_byte((char*)&(menu_item), (char*)&(UDM_menu[menu].menus[0]),
				sizeof(UDM_menu_struc));
	if (UDM_menu[menu].menus[0].statname[0]!='\0')
	{
		UDM_menu[menu].statflag = 1;
		UDM_menu[menu].size[0] = UDM_menu[menu].menus[0].size[0];
		UDM_menu[menu].size[1] = UDM_menu[menu].menus[0].size[1];
	}
	if (menu_item.params!=NULL)
	{
		len = strlen(menu_item.params);
		if (len>0)
		{
			UDM_menu[menu].menus[0].params = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(UDM_menu[menu].menus[0].params, menu_item.params);
		}
		else
			UDM_menu[menu].menus[0].params = (char*)UU_NULL;
	}
	else
		UDM_menu[menu].menus[0].params = (char*)UU_NULL;
	if ((menu_item.toggle_num>0)&&(menu_item.toggle!=0))
	{
		UDM_menu[menu].menus[0].toggle = (UDM_menu_toggle_struc *)malloc(
										sizeof(UDM_menu_toggle_struc)*UDM_MAX_TOGGLE);
		for (j=0; j<menu_item.toggle_num;j++)
		{
			uu_move_byte((char*)&(menu_item.toggle[j]), (char*)&(UDM_menu[menu].menus[0].toggle[j]),
							sizeof(UDM_menu_toggle_struc));
			if (menu_item.toggle[j].params!=NULL)
			{
				len = strlen(menu_item.toggle[j].params);
				if (len>0)
				{
					UDM_menu[menu].menus[0].toggle[j].params = (char*)uu_malloc((len+1)*sizeof(char));
					strcpy(UDM_menu[menu].menus[0].toggle[j].params, menu_item.toggle[j].params);
				}
				else
					UDM_menu[menu].menus[0].toggle[j].params = (char*)UU_NULL;
			}
			else
				UDM_menu[menu].menus[0].toggle[j].params = (char*)UU_NULL;
		}
	}
#if UU_COMP==UU_WIN2K
	UDM_menu[menu].bmpfile[0] = '\0';
	strcpy(UDM_menu[menu].menu_area, menu_area);
	UDM_menu[menu].menu_format = format;
	UDM_menu[menu].dockable = dockable;
#endif
	UDM_menu[menu].key_loaded = 0;
	UDM_menu[menu].keyfile[0] = '\0';
/*
......do not automatically save a menu anymore
*/
/*	if (fname[0]!='\0')
		ud_save_dragmenu(menu);
*/
	UDM_menu_count++;
	return menu;
}
/*********************************************************************
**    S_FUNCTION     :  static Sonbrowser(fieldno, val, stat)
**       Method called at 'browser' button field.
**       used for "ud_desgn_menu_item", so it is not used any more
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT Sonbrowser(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	UX_pathname filename,descrip,ext;
	int len;

	filename[0] = '\0';
	if (*fieldno==3)
	{
		strcpy(descrip, "Menu Files(*.menu)");
		strcpy(ext,"*.menu");
		ud_get_filename("browse", "Menu file", ext,
						filename, &len,descrip, UU_FALSE) ;
	}
	else if (*fieldno==7)
	{
		strcpy(descrip, "Bitmap Files (*.bmp)");
		strcpy(ext,"*.bmp");
		ud_get_filename("browse", "Bitmap file", ext,
						filename, &len,descrip, UU_FALSE) ;
	}
	if (filename[0]!='\0')
	{
		ud_update_answer(*fieldno-1, (int*)filename);
	}
	else
		*fieldno = -1;
	return UD_FLDOK;
}
/*********************************************************************
**    S_FUNCTION     :  static Ssel_menutype(fieldno, val, stat)
**       Method called when "Item Type" selected.
**       used for "ud_desgn_menu_item", so it is not used any more
**		
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT Ssel_menutype(fieldno, val, stat)
int *fieldno;
UD_FSTAT stat;
UD_DDATA *val;
{
	if ((Smenu_itype==0)||(Smenu_itype==1))
	{
		ud_set_traverse_mask(0, UU_TRUE);
		ud_set_traverse_mask(5, UU_TRUE);
		ud_set_traverse_mask(6, UU_TRUE);
		ud_set_traverse_mask(7, UU_TRUE);
		ud_set_traverse_mask(8, UU_TRUE);
		ud_set_traverse_mask(9, UU_TRUE);

		ud_set_traverse_mask(2, UU_TRUE);
		ud_set_traverse_mask(3, UU_TRUE);
		ud_set_traverse_mask(4, UU_FALSE);
	}
	else if (Smenu_itype==2)
	{
		ud_set_traverse_mask(0, UU_TRUE);
		ud_set_traverse_mask(5, UU_TRUE);
		ud_set_traverse_mask(6, UU_TRUE);
		ud_set_traverse_mask(7, UU_TRUE);
		ud_set_traverse_mask(8, UU_TRUE);
		ud_set_traverse_mask(9, UU_TRUE);

		ud_set_traverse_mask(2, UU_FALSE);
		ud_set_traverse_mask(3, UU_FALSE);
		ud_set_traverse_mask(4, UU_TRUE);
	}
	else if (Smenu_itype==3)
	{
		ud_set_traverse_mask(0, UU_FALSE);
		ud_set_traverse_mask(5, UU_FALSE);
		ud_set_traverse_mask(6, UU_FALSE);
		ud_set_traverse_mask(7, UU_FALSE);
		ud_set_traverse_mask(8, UU_FALSE);
		ud_set_traverse_mask(2, UU_FALSE);
		ud_set_traverse_mask(3, UU_FALSE);
		ud_set_traverse_mask(4, UU_FALSE);
		ud_set_traverse_mask(9, UU_FALSE);
	}
}
/*********************************************************************
**   I_FUNCTION: SortFunc()
**      Sort the list on Select function list form in partical order
**       used for "ud_desgn_menu_item", so it is not used any more
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static int SortFunc(char* cData1, char* cData2, int lParamSort)
{
	UD_ITEMDATA* pData1, *pData2;
	int nRetVal;

	pData1 = (UD_ITEMDATA*)cData1;
	pData2 = (UD_ITEMDATA*)cData2;
	nRetVal = 0;
	switch(lParamSort)
	{
	case 0:
		nRetVal = strcmp(pData1->data_items[0],
                                 pData2->data_items[0]);
		break;
	case 1:	
		nRetVal = strcmp(pData1->data_items[1],
                                 pData2->data_items[1]);
		break;
	case 2:
		nRetVal = strcmp(pData1->data_items[2],
                                 pData2->data_items[2]);
		break;
	default:
		break;
	}
	return nRetVal;
}

/*********************************************************************
**   I_FUNCTION: SortFunc2()
**      Sort the list on Select Scalar form in partical order
**       used for "ud_desgn_menu_item", so it is not used any more
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static int SortFunc2(char* cData1, char* cData2, int lParamSort)
{
	UD_ITEMDATA* pData1, *pData2;
	int nRetVal;
	UU_REAL val1, val2;
	pData1 = (UD_ITEMDATA*)cData1;
	pData2 = (UD_ITEMDATA*)cData2;
	nRetVal = 0;

/*
......"toolno" "type" "Description"
*/
	switch(lParamSort)
	{
	case 0:
		nRetVal = -strcmp(pData1->data_items[0],
                                 pData2->data_items[0]);
		break;
	case 1:	
		nRetVal = -strcmp(pData1->data_items[1],
                                 pData2->data_items[1]);
		break;
	case 2:
		nRetVal = -strcmp(pData1->data_items[2],
                                 pData2->data_items[2]);
		break;
	default:
		break;
	}
	return nRetVal;
}

/*********************************************************************
**    S_FUNCTION     :  static OnListCalbacks(fieldno, val, stat)
**       Method called for function list table, used for "ud_desgn_menu_item"
**		so it is not used any more
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
static UD_FSTAT OnListCalbacks(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int sel;
	UD_ITEMDATA *data;
	UD_TABLEINFO *info = (UD_TABLEINFO *)(val->frmint);
	if (info->flag==1)
	{
/*
......list selected, doing selection callback
......if it is un-select, info->row = -1;
*/
		Sfunc_list.answer = sel = info->row;
		data = (UD_ITEMDATA *)(info->data_ptr);
		if (sel>=0)
			strcpy(Smenu_func, data->data_items[0]);
		else
			Smenu_func[0] = '\0';
	}
	else
	{
/*
......column button is pushed, doing sorting
*/
		if ((info->col==0)&&(name_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			name_click++;
		}
		else if ((info->col==0)&&(name_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			name_click++;
		}
		if ((info->col==1)&&(key_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			key_click++;
		}
		else if ((info->col==1)&&(key_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			key_click++;
		}
		if ((info->col==2)&&(descript_click%2==0))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc);
			descript_click++;
		}
		else if ((info->col==2)&&(descript_click%2))
		{
			ud_form_sorttable(info, (UD_SMETHOD)SortFunc2);
			descript_click++;
		}
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION         :  ud_get_func_flist(flist)
**       get all function list as a particulat format
**       used for "ud_desgn_menu_item", so it is not used any more
**
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT : a list of toolib list
**    RETURNS      : number: number of tools 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ud_get_func_flist(flist)
UD_TLIST *flist;
{
	int i, j, status ,len;
	char buf[81];
	struct TL_tooldata_rec *p1;

	status = 0;
/*
......Function 	Key	Description
*/
	flist->num_col = 3;
	if (flist->num_col>0)
		flist->col_label = (char**) uu_malloc(flist->num_col*sizeof (char*));
	for (i=0; i<flist->num_col;i++)
	{
		flist->col_label[i] = (char*) uu_malloc(20*sizeof(char));
	}
	strcpy(flist->col_label[0], "Functions");
	strcpy(flist->col_label[1], "Key");
	strcpy(flist->col_label[2], "Description");

	i = 0;
	j = 0;
	while (UU_TRUE)
	{
		if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
		if ((strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) || (UZ_keyfuncs[i].type==MSLKEY) )
		{
			i++;
			continue;
		}
		i++; j++;
		if (i>100000) break;
	}
	flist->num_item = j;
	flist->answer = 0;

	if (flist->num_item>0)
		flist->data = (UD_ITEMDATA *) uu_malloc(flist->num_item*sizeof(UD_ITEMDATA));
	for (i=0; i<flist->num_item;i++)
	{
		(flist->data[i]).itemnum = flist->num_col;
		(flist->data[i]).data_items = 
					(char **) uu_malloc(flist->num_col*sizeof(char*));
	}
	i = 0;
	j = 0;
	while (UU_TRUE)
	{
		if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
/*
.....no MSLKEY for NCL
*/
		if ((strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) || (UZ_keyfuncs[i].type==MSLKEY) )
		{
			i++;
			continue;
		}

		len = strlen(UZ_keyfuncs[i].name);
		flist->data[j].data_items[0] = (char*)uu_malloc((len+1)*sizeof(char));
		strcpy(flist->data[j].data_items[0], UZ_keyfuncs[i].name);

		if (UZ_keyfuncs[i].type==DASKEY) 
		{
			flist->data[j].data_items[1] = (char*)uu_malloc(10*sizeof(char));
			strcpy(flist->data[j].data_items[1], "DASKEY");
		}
		else if (UZ_keyfuncs[i].type==NISKEY) 
		{
			flist->data[j].data_items[1] = (char*)uu_malloc(10*sizeof(char));
			strcpy(flist->data[j].data_items[1], "NISKEY");
		}
		else if (UZ_keyfuncs[i].type==NCLKEY) 
		{
			flist->data[j].data_items[1] = (char*)uu_malloc(10*sizeof(char));
			strcpy(flist->data[j].data_items[1], "NCLKEY");
		}
		else if (UZ_keyfuncs[i].type==CAMKEY) 
		{
			flist->data[j].data_items[1] = (char*)uu_malloc(10*sizeof(char));
			strcpy(flist->data[j].data_items[1], "CAMKEY");
		}
		else if (UZ_keyfuncs[i].type==CADKEY) 
		{
			flist->data[j].data_items[1] = (char*)uu_malloc(10*sizeof(char));
			strcpy(flist->data[j].data_items[1], "CADKEY");
		}
		else if (UZ_keyfuncs[i].type==ALPHAKEY) 
		{
			flist->data[j].data_items[1] = (char*)uu_malloc(10*sizeof(char));
			strcpy(flist->data[j].data_items[1], "ALPHAKEY");
		}
		else if (UZ_keyfuncs[i].type==MENUKEY) 
		{
			flist->data[j].data_items[1] = (char*)uu_malloc(10*sizeof(char));
			strcpy(flist->data[j].data_items[1], "MENUKEY");
		}
		else if (UZ_keyfuncs[i].type==IPVKEY) 
		{
			flist->data[j].data_items[1] = (char*)uu_malloc(10*sizeof(char));
			strcpy(flist->data[j].data_items[1], "IPVKEY");
		}
/*
......data_items[2] for Description
*/
		len = strlen(UZ_keyfuncs[i].descrip);
		flist->data[j].data_items[2] = (char*)uu_malloc((len+1)*sizeof(char));
		strcpy(flist->data[j].data_items[2], UZ_keyfuncs[i].descrip);

		i++; j++;
		if (j==flist->num_item) break;
		if (i>100000) break;
	}
	return flist->num_item;
}

/*********************************************************************
**	 I_FUNCTION :ud_desgn_menu_item (UDM_menu_struc *menu_item)
**		This function bring up a menu design form, not used any more
**		keep it for now
**	 PARAMETERS	
**		 INPUT  :
**		 OUTPUT :
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
int ud_desgn_menu_item (UDM_menu_struc *menu_item)
{
	int i, j, status, len;
	UX_pathname chcfile, bmpfile, file;
	int *ans[10], but;
	char menu_label[30], menu_name[UX_MAX_FILE_LEN], bmp_name[UX_MAX_FILE_LEN], 
		params[UX_MAX_FILE_LEN*2], descrip[40];
	UD_FSTAT uj_noop();
	char bmp_num[20];
	static UD_METHOD methods[10] = {uj_noop, Ssel_menutype, uj_noop, Sonbrowser,
		OnListCalbacks, uj_noop, uj_noop, Sonbrowser, uj_noop, uj_noop };
	static char called[] = { 6,6,6,6,6,6,6,6,6,6};
	static char traverse[10] = {1, 1,1,1,1,1,1,1,1,1};
	static char display[10] = {1,1,1,1,1,1,1,1,1,1};  

	Sfunc_list.num_item = ud_get_func_flist(&Sfunc_list);
	Sfunc_list.answer = 0;
	Sfunc_list.sort = 0;

	ans[0] = (int*)menu_label;
	ans[1] = (int*)&Smenu_itype;
	ans[2] = (int*)menu_name;
	ans[3] = (int*)&but;
	ans[4] = (int*)&Sfunc_list;
	ans[5] = (int*)params;
	ans[6] = (int*)bmp_name;
	ans[7] = (int*)&but;
	ans[8] = (int*)bmp_num;
	ans[9] = (int*)descrip;

	strcpy(menu_label, menu_item->name);
	strcpy(descrip, menu_item->descrip);
	strcpy(bmp_name, menu_item->bmpfile);
	if (menu_item->params==NULL)
		params[0] = '\0';
	else
		strcpy(params, menu_item->params);
	if (menu_item->bmpnum!=-1)
		sprintf (bmp_num, "%d", menu_item->bmpnum);
	else
		bmp_num[0] = '\0';
	if (menu_item->chcfile[0]!='\0')
	{
		strcpy(menu_name, menu_item->chcfile);
		Smenu_itype = 0;
	}
	else if (menu_item->file[0]=='\0')
	{
		if (menu_item->separator)
			Smenu_itype = 3;
		else
			Smenu_itype = 2;
		Sfunc_list.answer = 0;
	}
	else
	{
		i = 0;
		j = 0;
		Sfunc_list.answer = -1;
		Smenu_itype = 2;
		while (UU_TRUE)
		{
			if (strcmp(UZ_keyfuncs[i].name,"~END~") == 0) break;
			if ((strcmp(UZ_keyfuncs[i].name,"DUMMY") == 0) || (UZ_keyfuncs[i].type==MSLKEY) )
			{
				i++;
				continue;
			}
			if (strcmp(UZ_keyfuncs[i].name, menu_item->file)==0)
			{
				Sfunc_list.answer = j;
				break;
			}
			i++; j++;
			if (i>100000) break;
		}
		if (Sfunc_list.answer==-1)
		{
			Sfunc_list.answer = 0;
			Smenu_itype = 1;
		}
	}
/*
......temp
*/
	Sfunc_list.answer = 2;
	if ((Smenu_itype==0)||(Smenu_itype==1))
	{
		traverse[0] = 1;
		traverse[1] = 1;
		traverse[2] = 1;
		traverse[3] = 1;
		traverse[4] = 0;
		traverse[5] = 1;
		traverse[6] = 1;
		traverse[7] = 1;
		traverse[8] = 1;
		traverse[9] = 1;
	}
	else if (Smenu_itype==2)
	{
		traverse[0] = 1;
		traverse[1] = 1;
		traverse[2] = 0;
		traverse[3] = 0;
		traverse[4] = 1;
		traverse[5] = 1;
		traverse[6] = 1;
		traverse[7] = 1;
		traverse[8] = 1;
		traverse[9] = 1;
	}
	else if (Smenu_itype==3)
	{
		traverse[0] = 0;
		traverse[1] = 1;
		traverse[2] = 0;
		traverse[3] = 0;
		traverse[4] = 0;
		traverse[5] = 0;
		traverse[6] = 0;
		traverse[7] = 0;
		traverse[8] = 0;
		traverse[9] = 0;
	}
	status = ud_form1("menudesgn.frm", ans, ans, methods, called, display,
				traverse);
	if (status==-1)
		goto done; ;
	if (Smenu_itype==3)
	{
		menu_item->separator = 1;
		menu_item->toggle_num = 0;
		menu_item->toggle = 0;
		menu_item->chcfile[0] = '\0';
		menu_item->chcdef = 0;
		menu_item->name[0] = '\0';
		menu_item->file[0] = '\0';
		menu_item->statname[0] = '\0';
		menu_item->params = UU_NULL;
		menu_item->descrip[0] = '\0';
		menu_item->bgcolor[0] = '\0';
		menu_item->color[0] = -1;
		menu_item->color[1] = -1;
		menu_item->color[2] = -1;
		menu_item->pos[0] = -1;
		menu_item->pos[1] = -1;
		menu_item->size[0] = -1;
		menu_item->size[0] = -1;
		menu_item->bmpfile[0] = '\0';
		menu_item->bmpnum = -1;
		goto done;
	}
	strcpy(menu_item->name, menu_label);
	strcpy(menu_item->descrip, descrip);
	strcpy(menu_item->bmpfile, bmp_name);
	if (params[0]!='\0')
	{
		if (menu_item->params!=NULL)
		{
			uu_free(menu_item->params);
			len = strlen(params);
			menu_item->params = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(menu_item->params, params);
		}
	}
	else
	{
		if (menu_item->params!=NULL)
		{
			uu_free(menu_item->params);
		}
		menu_item->params = UU_NULL;
	}
	if (bmp_num[0]!='\0')
		menu_item->bmpnum = atoi(bmp_num);
	else
		menu_item->bmpnum = -1;
	if (Smenu_itype==0)
	{
		if (menu_name[0]!='\0')
			strcpy(menu_item->chcfile, menu_name);
		else
			menu_item->chcfile[0] = '\0';
		menu_item->file[0] = '\0';
	}
	else if (Smenu_itype==1)
	{
		if (menu_name[0]!='\0')
			strcpy(menu_item->file, menu_name);
		else
			menu_item->file[0] = '\0';
		menu_item->chcfile[0] = '\0';
	}
	else if (Smenu_itype==2)
	{
		if (Sfunc_list.answer>=0)
		{
/*
.....Smenu_func
*/
			strcpy(menu_item->file, Smenu_func);
		}
		else
			menu_item->file[0] = '\0';
		menu_item->chcfile[0] = '\0';
	}
done:;
	ud_free_tlist(&Sfunc_list);
	return (status);
}
/*********************************************************************
**	 I_FUNCTION :ud_get_menuinfo(fname, type, descript)
**		This function get the menu type and description information from a menu file
**	 PARAMETERS	
**		 INPUT  :
**					fname: menu filename
**		 OUTPUT :
**					type: menu type
**					descript: menu description
**	 RETURNS: none
**	 SIDE EFFECTS: none.
**	 WARNINGS: none.
*********************************************************************/
int ud_get_menuinfo(fname, type, descript)
char *fname, *descript;
int *type;
{
	UX_pathname fullname;
	int start;
	char buf[UX_MAX_PATH_LEN*2],ctyp[UX_MAX_PATH_LEN*2],cmsg[UX_MAX_PATH_LEN*2];
	int stat, numint, ityp, i, isub,istat, kinc;
	FILE *fptr;
	int maxsub=7;
	int maxsub2=12;
	static char csub[7][12]={"DESCRIPTOR","MENUS", "CHOICE", "COLOR", "BAR", "BUTTONS"};
	static char csub2[12][20] = {"NAME","POSITION","ROWS","COLS","TYPE",
		"SIZE", "BITMAP", "AREA", "DOCKABLE", "KEYDEFS", "FORMAT", "JUSTIFY"};
	static char ltype[5][64] = {"ICON","MENU","POPUP", "MENUBAR", "PULLDOWN"};

	stat = UU_SUCCESS;
	start = 0;
	strcpy(fullname, fname);
/*
.....Initialize menu structure
*/
	stat = ul_open_mod_file("UU_USER_SETTINGS","menu","NCL_MENU", UU_NULL,
		fullname, 2,&fptr);
	if (fptr == UU_NULL) 
	{
		return UU_FAILURE;
	}
/*
.....Read a record
*/
	do
	{
		stat = ul_fread (fptr,buf,sizeof(buf),&numint);
		if (numint>UX_MAX_PATH_LEN*2)
		{
			stat = UU_FAILURE;
			goto done;
		}
/*
......when "end of file" and no input
......numint = -1, will cause a memory error
*/
		if (numint>=0)
			buf[numint] = '\0';
		if ((numint<=0)&&(stat == UX_EOF))
		{
			stat = UU_FAILURE;
			goto done;
		}
		else if ((stat == UX_EOF)&&(numint>0))
		{
			stat = UU_FAILURE;
			goto done;
		}
		else if (stat != UU_SUCCESS && stat != UX_NO_SPACE)
		{
			stat = UU_FAILURE;
			goto done;
		}
check:;
		istat = ul_modal_check (buf,&ityp,ctyp,cmsg);
/*
.....Invalid syntax
*/
		if (istat != UU_SUCCESS)
		{
			stat = UU_FAILURE;
			goto done;
		}
/*
.....Subsystem type
*/
		switch (ityp)
		{
		case 1:
			for (i=0;i<maxsub;i++)
			{
				ul_to_upper(ctyp);
				if (strcmp(ctyp,csub[i]) == 0) break;
			}
			if (i >= maxsub)
			{
				stat = UU_FAILURE;
				goto done;
			}
			if (i==0) 
				start = 1;
			if ((start==1)&&(i!=0))
			{
				stat = UU_SUCCESS;
				goto done;
			}
			isub = i + 1;
			break;
		case 2:
			switch (isub)
			{
			case 1:
				for (i=0;i<maxsub2;i++)
				{
					ul_to_upper(ctyp);
					if (strcmp(ctyp,csub2[i]) == 0) break;
				}
				if (i >= maxsub2)
				{
					stat = UU_FAILURE;
					goto done;
				}
				switch(i)
				{
					case 0:
						strcpy(descript, cmsg);
						break;
					case 1:
					case 2:
					case 3:
						break;
					case 4:
						if (ul_modal_toggle(cmsg,ltype,5,type) != UU_SUCCESS)
						{
							stat = UU_FAILURE;
							goto done;
						}
						break;
					case 5:
					case 6:
					case 7:
					case 8:
					case 9:
					case 10:
						break;
				}
				break;
			case 2:
			case 5:
			case 3:
			case 6:
				break;
			}
		}
	}
	while (stat == UU_SUCCESS);
done:;
	if (fptr != 0) 
		ux_fclose0(fptr);
	return(stat);
}
#endif


