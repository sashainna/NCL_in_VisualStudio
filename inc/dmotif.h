/********************************************************************
**    NAME         :  dmotif.h
**
**       CONTAINS:
**          Motif DAS definitions
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       dmotif.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:14
**
*********************************************************************/

#ifndef DMOTIFH
#define DMOTIFH

#include "xenv1.h"

#ifdef DPGM
#define EXT
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif

#define UDM_MAX_MENU 200
#define MAX_EDGE_BAR 100

#define UDM_MTYPE_ICON 0
#define UDM_MTYPE_MENU 1
#define UDM_MTYPE_POPUP 2
#define UDM_MTYPE_MENUBAR       3
#define UDM_MTYPE_PULLDOWN      4
#define UDM_MTYPE_INTERNAL 5
#define UDM_MAX_TOGGLE 50

#define UDM_AREA_NODOCK 1
#define UDM_AREA_FRONTDOCK      2
#define UDM_AREA_ENDDOCK        3

typedef struct
{
/*
.....-1: no bar (read layout error)
.....0: no bar (initial value)
.....1: menubar
.....2: status bar: not used now, statusbar merge with menu
.....3: prompt bar
.....4: command bar
.....5: error bar
*/
	short flag;
/*
.....1: TOP
.....2: BOTTOM
.....3: LEFT
.....4: RIGHT
*/
	short dir;
	short indx;
} UDM_BARPOS;

typedef struct
{
	int nmenu;
	int maximize;
/*
.....changes for native WinNT
.....Yurong 7/27/00
*/
#if UU_COMP!=UU_WIN2K
	char menu_name[UDM_MAX_MENU][256];
	int menu_pos[UDM_MAX_MENU][2];
/*
...add size by yurong
*/
	int menu_size[UDM_MAX_MENU][2];
	int graphic_pos[2], graphic_size[2];
	int prompt_pos[2], prompt_size[2];
	int status_pos[2], status_size[2];
#else
	char menu_area[UDM_MAX_MENU][256];
	int area_dockable[UDM_MAX_MENU];
	char menu_name[UDM_MAX_MENU][256];
/*
.....pos[0], pos[1] will be row, col of menu area
.....pos[2] will be direction of menu area
.....pos[2] = 1 : TOP
.....         2:  BOTTOM
.....         3:  LEFT
.....           = 4:  RIGHT
.....pos[3], pos[4] will be screen position of menu area
*/
	int menu_pos[UDM_MAX_MENU][5];
	int menu_size[UDM_MAX_MENU][2];

	int window_pos[2], window_size[2];
	int prompt_pos[5], prompt_size[2], prompt_type;
	int command_pos[5], command_size[3], command_type, command_clr[2],
		command_doc[4], command_active, command_att[2], command_load[2];
	int error_pos[5], error_size[2], error_type;
	int statwin_pos[5], statwin_size[2], statwin_type,
		statwin_doc[4], statwin_active, statwin_att[2];
	char menubar[256], statwin_area[256], command_area[256];
	UX_pathname statbar_file[4];
	int statbar_no;
#endif
} UDM_LAYOUT;

/*
......added for toggle button
......Yurong 3/24/99
*/
typedef struct
{
	char label[30];
	char func[20];
/*
.....added function parameter string
*/
	char *params;
/*
.....pos[0]: menu number
.....pos[1]: menu item number
.....pos[2]: toggle choice number
*/
	int pos[3];
/*
.....add image for this toggle item
*/
	char bmpfile[UX_MAX_FILE_LEN];
	char descrip[40];
} UDM_menu_toggle_struc;

typedef struct
{
	char name[30];
/*
.....for function need execute or *.menu/.stat for open
*/
	char file[UX_MAX_FILE_LEN];  
/*
.....added parameter string for function name
*/
	char *params;
/*
......added status name for status button
*/
	char statname[30];
	int pos[2];
	int size[2];
	int kinc;
	char toggle_def[30];
	int toggle_num;
	UDM_menu_toggle_struc *toggle;
	char chcfile[UX_MAX_FILE_LEN];
	int chcdef;
#if UU_COMP==UU_WIN2K
/*
.....added bitmap file for menubar, pulldown menu items
.....used for native WinNT only
*/
	char bmpfile[UX_MAX_FILE_LEN];
/*
.....added bitmap index for "MENU", "ICON" menu items
.....used for native WinNT only
*/
	int bmpnum;
	int separator;
#endif
/*
.....added color, user can change from menu file
*/
	char bgcolor[40];
/*
.....pixel color for easy reset
.....color[0]: background color
.....color[1]: foreground color
.....color[2]: highlight color
*/
	unsigned long color[3];
	char descrip[40];
} UDM_menu_struc;


typedef struct
{
	char name[30];
	char file[UX_MAX_FILE_LEN];
	int type;
	int num;
/*
.....if statflag = 1: status dialog
.....                   0: menu
*/
	int statflag;
	short drawitem;
	UDM_menu_struc *menus;
	int rows,cols;
	int pos[2];
	int size[2];
/*
.....added bitmap file for "ICON", "MENU" menu items
.....used for native WinNT only
*/
#if UU_COMP==UU_WIN2K
	char bmpfile[UX_MAX_FILE_LEN];
	char menu_area[256];
	int dockable; /* 0: can't dock
					1: dockable  */
	int menu_format;
	short justify; /* 0: left, default, 1: right justify */
#endif
/*
.....add keydef file for this item
*/
	char keyfile[UX_MAX_FILE_LEN];
	int key_loaded;
} UDM_MENU;


EXT int UDM_menu_count;
EXT UDM_LAYOUT UDM_layout;
/*
.....added for run time layout
.....Yurong 1/4/01
*/
EXT UDM_LAYOUT UDM_run_layout;
EXT UDM_MENU UDM_menu[UDM_MAX_MENU];
EXT int UDM_menu_design;
EXT UDM_BARPOS UDM_bararea[MAX_EDGE_BAR];
EXT int UDM_barnum;
EXT char UD_window_title[500];
#undef EXT
#endif
