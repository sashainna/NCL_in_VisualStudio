
/*************************************************************************
**		NAME	:	udforms.h
**			Contains:
**				form definition structure defs for the forms runtime package.
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       udforms.h , 26.3
**    DATE AND TIME OF LAST  MODIFICATION
**       05/22/18 , 10:18:36
**************************************************************************/

#ifndef UDFORMSH

/*
**		include files
*/

#include "usysdef.h"
#include "dasnog.h"				/* for def of UD_DASIN, das data type */
#include "xenv1.h"
#include "udfconst.h"
#include "udfdata.h"
#include "ulist.h"

#define FORM_STRING		1
#define FORM_PICK		2
#define FORM_LOCATE		3
#define FORM_RECORD		4
#define FORM_LABEL		5
#define FORM_SUBSCR		6

#define FORM_MAX_INT_HEADER	200
/*
**		forms data structure definitions
*/

#ifdef __cplusplus 
	typedef UD_FSTAT (*UD_METHOD)(int *, UD_DDATA*, UD_FSTAT);
#else
	typedef UD_FSTAT (*UD_METHOD)();
#endif

#ifdef __cplusplus 
	typedef int (*UD_SMETHOD)(char*, char*, int);
#else
	typedef int (*UD_SMETHOD)();
#endif

typedef struct					/* definition of 2d NDC point */
{
	UU_REAL x;							/* x-coord */
	UU_REAL y;							/* y-coord */
} UD_LOC;

typedef struct					/* definition of window extent */
{
	UD_LOC ll;					/* lower left corner */
	UD_LOC ur;					/* upper right corner */
} UG_DRECT;

typedef struct					/* form window-relative location */
{
/*
.....original is for rows and cols, 
.....but now used for label position
.....on Motif version and WinNT
.....don't if it affect other platform, 
.....so still keep those names
*/
	int ud_r;						
	int ud_c;	
/*
.....added for input field position
*/
	int x;
	int y;
} UD_RCLOC;

typedef struct					/* form definition struct def */
{
	char ud_fid[UD_MAXNAM];		/* form name string */
	UG_DRECT	ud_fwin;				/* window size and loc in NDC */
	UD_RCLOC ud_frc;				/* window size in rows,cols */
	int ud_pgcolr;					/* window bkrnd color */
	UU_REAL ud_txsiz;				/* text size */
	int ud_txcolr;					/* text color */
	int ud_txfont;					/* text font */
} UD_WINDEF;

typedef struct					/* form display message index */
{
	int x,y,cx,cy;
	char title[80];				/* frame title */
	char fcolor[40];
	char bcolor[40];
	UU_REAL font_scale;
	int section;
} UD_FRAME;

typedef struct					
{
	int color; 
	UU_LIST select_list;
} UD_SELECT_STR;

typedef struct					
{
	int color; 
	UU_KEY_ID key;
} UD_sel_geo;

typedef struct					
{
	char name[40];				/* section name */
	char color[40];
	int type; /* separator type = 1, default = 0 */
} UD_SECTION;

typedef struct					/* form display message index */
{
	UD_RCLOC pos;				/* row/col location */
	char *message;				/* Ptr to message */
	char fcolor[40];
	char bcolor[40];
	UU_REAL font_scale;
	int section;
} UD_DISPLAY;						

typedef struct	
{
	int x,y,cx,cy;
	char name[40];
	char fname[UX_MAX_FILE_LEN];
	int upt_flag;
	int section;
/*
.....add flag for show/hide
*/
	int show_flag;
} UD_PICTURE;						

typedef struct	
{
	char name[40];
	char *tooltext;
	float xmin, ymin, xmax, ymax;
	char *params;
} UD_PICAREA;

typedef struct					/* field definition struct def */
{
/*
.....this value used for beginning prompt pixel position now
*/
	UD_RCLOC ud_prmloc;			/* row/col prompt location */
	char *prompt;					/* Pointer to prompt string */
/*
.....this value used for whole field size in pixel now
*/
	UD_RCLOC ud_fldloc;			/* row/col field location */
	int n_defaults;				/* Number of default values for this field	*/
	UD_DASIN *defaults;			/* Array of default values	*/
	short range_flag;				/* 0=no range checking, 1=range checking	*/
	short active_flag;			/* -1=Field is inactive by default,
											0=Use default active/display flags,
											1=Field is active by default.*/
	UD_DASIN range[2];			/* If range_flag, upper and lower bounds */
	int toggle;						/* 0=display only (not used)
											1=not a toggle, 
											2=is a toggle	*/
	short ud_datatyp;				/* data type: das types */
	short ud_mndflg;				/* mandatory input flag: 0=not, 1= mandatory */
	short ud_echo;					/* Not used: for old form capatability	*/
									/* but some where used for update field flag */
								/* used for double click now*/
	short ud_input;            /* Input type, FORM_STRING, FORM_PICK, etc. */
	int ud_limit[UD_NMENTWD];	/* Limit selection mask */
	int ud_fprec;					/* field precision */ 
								/* used for SORT flag when LISTTABLE, default = 0, sorted, 1, not sort */
	int ud_flen;					/* field length */ 
								/* used for FILTER flag when LISTTABLE, default = 0, filter, 1, no filter*/
	UD_METHOD method;					/* ptr to field method	*/
	char method_returns;			/* mask telling when to call the method	*/
										/* See explaination on udfconst.h			*/
/*
......added for EDIT field When pick/locate 
......Yurong 8/15/05
*/
	short ud_modal;            
	short justified; 
	UU_REAL font_scale;
	char shortcut[20];
	char fcolor[40];
	char bcolor[40];
	char pfcolor[40];
	char pbcolor[40];
	int n_picarea;
	UD_PICAREA *picarea;
	int section;
	char *vfile;
} UD_INPUT;


typedef struct					/* form structure definition */
{
	UD_WINDEF ud_frmdf;			/* Form window specification */

/* Picture field defs	*/
	int n_picture_fields;		/* Number of picture fields	*/
	UD_PICTURE *picture_flds;	/* Array of picture fields 	*/

/* Display field defs	*/
	int n_display_fields;		/* Number of display only fields	*/
	UD_DISPLAY *display_flds;	/* Array of display only fields 	*/

/* Input field defs	*/
	int n_input_fields;				/* Number of input fields		*/
	UD_INPUT *input_flds;		/* Array of input fields	*/
	char *traverse_mask;			/* Mask to determine what fields are traversed */

/* Both display and input field defs	*/
	char *ud_display_mask;		/* Mask for field display(Both inp and disp)*/
/*
.....added help text
*/
	int helpflag;
	char *frmhelp;
	char frmname[UX_MAX_FILE_LEN];
/*
.....added Frame
*/
	int n_frame_fields;				/* Number of frame	*/
	UD_FRAME *frame_flds;
/*
.....added Section
*/
	int n_section_fields;				/* Number of section */
	UD_SECTION *section_flds;
	int dock_dir, dockable[4];
	char win_area[80];
	int ref;
	int att_win;
/*
.....added macro flag, new macro form with have this flag on
.....all other form and old macro form have macroflag=0
*/
	int macroflag;
} UD_FSTRUCT;

/*
.....structure for List
.....added by Yurong 8/15/97
*/
typedef struct
{
   int num_item;
   char* answer;
   char **item;
} UD_LIST;

/*
.....structure for Table List
.....added by Yurong 10/15/08
*/
typedef struct
{
   int itemnum;
   char** data_items;
/*
.....this is for user callback function to know
.....such as list selection callback or sorting (column clicked callback)
.....which form and field we are using
.....the form callback will pass into 
.....this data struction into the user callback function
*/
   int frmid, fldno;
/*
......when flag is 1, the table list selection is changed.
......when flag is 2, the column header is clicked
......default flag = 1, the user define form callback function
......should use flag to decide what to do. The flag will also
......can used for return from function, for example, in sorting function
......for return the result.
*/
   int flag;
} UD_ITEMDATA;
typedef struct
{
   int num_item;
   int num_col;
   int sort;
/*
.....selected index
*/
   int answer;
   char **col_label;
   UD_ITEMDATA *data;
} UD_TLIST;

typedef struct
{
	int flag;
	int frmid;
	unsigned int fldid;
	int col;
	int row;
	int *data_ptr;
} UD_TABLEINFO;

typedef struct
{
   int num_item;
   int num_col;
   int sort;
/*
.....selected data
*/
   int answer[2];
   char **col_label;
   UD_ITEMDATA *data;
} UD_DLIST;

#define UDFORMSH
#endif

