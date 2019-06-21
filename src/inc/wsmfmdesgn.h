/*********************************************************************
**    NAME         :  wsmfmdesgn.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       wsmfmdesgn.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:12
*********************************************************************/

/*
.....added by Yurong
.....8/25/97
*/
#ifdef  WS_MENUDEGN
#define EXT
#else
#define EXT extern
#endif

#ifndef WSMFMDESGNH
#define WSMFMDESGNH
#define UW_TOGGLE_MENU 1
#define UW_BUTTON_MENU 2

EXT Widget menu_layout, menu_form, func_select;
EXT Widget menu_win, desgn_win, funlist_win;
/*
...Text Widget for input Menu size and Position
*/
EXT Widget row_area, col_area, size0, size1, pos0, pos1, 
			title_area, file_area,titled_opt, icon_opt, popup_opt, option; 
/*
...save menu structure
*/
EXT UDM_MENU UMM_menu;
EXT char load_filename[200];
EXT char save_filename[200];
EXT int menu_changed;
EXT UDM_menu_struc UW_menu_item[UDM_MAX_MENU];
/*
...save menu labels and corrosponding functions
*/
/**********temptemp
EXT char menu_label[UDM_MAX_MENU][50];
EXT char menu_funcs[UDM_MAX_MENU][50];
***********************************************/
EXT int menu_count ;
EXT int UW_menu_type[UDM_MAX_MENU];
/*
...menu number of user picked 
...for input function and label
*/
EXT int  pick_menu;
/*
...store menu number that is active but takedown for edit 
*/
EXT int takedown ;
EXT char takemenu[40];
/*
...check for if push Accept button 
*/
EXT int menu_accept; 
EXT int mfirst;
EXT int menu_first;
EXT int menu_border[2] ;
EXT int uw_oldmenu_type;
extern UG_Gxwdat uw_xw;
extern UDM_LAYOUT UDM_layout;
#endif
#undef EXT
