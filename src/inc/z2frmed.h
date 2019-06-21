/*********************************************************************
**
**    NAME         :  z2frmed.h
**
**       CONTAINS:
**					Form Editor Initialization
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       z2frmed.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:24
**
*********************************************************************/

#ifndef Z2FRMEDH

#include "usysdef.h"
#include "gobas.h"
#include "unserve.h"
#include "gtblvar4.h"

#ifdef FORMEDINIT
#define EXT
#else
#define EXT extern
#endif

#define MAXSTR 80
#define ORIGX (UU_REAL) 0.2
#define ORIGY (UU_REAL) 0.2
#define UD_MAXDFL	32				/* maximum of 32 defaults in a toggle field */
#define UD_MAXINPUT		64				/* maximum of 64 fields per form */
#define UD_MAXDISPLAY	64				/* maximum of 64 fields per form */
#define DEVNO 5

#define XYTORC(xy,row,col) \
	{	\
	char wsstr[24];	\
	\
	strcpy(wsstr,ug_gksstli.wsopen[0].wdtptr->type); \
	if((strcmp(wsstr,"411x") == 0) || ((strcmp(wsstr,"410x") == 0))) \
		{ \
		xy[1] += (0.5*ud_formed_data.chary); \
		col = (Gint)((xy[0]-ud_formed_data.loc.ll.x)/ud_formed_data.charx);\
		row = ud_formed_data.rows- \
			(Gint)((ud_formed_data.loc.ur.y-xy[1])/ud_formed_data.chary)+2; \
		}\
	else \
		{ \
		col = (Gint)((xy[0]-ud_formed_data.loc.ll.x)/ud_formed_data.charx);\
		row = ud_formed_data.rows- \
			(Gint)((ud_formed_data.loc.ur.y-xy[1])/ud_formed_data.chary)+1;\
		} \
	}

#define MSG 0
#define PRMPT 1

#define NCHC 9			/* Number of choices in form attribute menu	*/
#define NCHC_1 10

#define TOGGLE 16		/* One more than number of das types in dtypes.h */

#define DEFAULT UU_APPL_NM1 
#define RANGE UU_APPL_NM2 
#define INDEX UU_APPL_NM3 

typedef struct
	{
	char str[MAXSTR];
	int row;
	int col;
	} str_loc;

typedef struct{
	char *str;
	int len;
	int prec;
	Gfloat delx;
	Gfloat dely;
	Gfloat charx, chary;
	char def_fname[40];
	int mod_flag;				/* 0 if initial form struct not modified,else 1 */
	int rows;
	int cols;
	int maxrows;
	int maxcols;
	int minrows,mincols;		/*Minimum size for can be and still fit all fields*/
	char toggle_marker;
	int type;
	int dastype;
	int mndflg;
	int echoflg;
	int rangeflg;
	int number_of_defaults;
	int rangeindex;
	int defaultindex;
	int answer_pos;
	Gnrect loc;
	int sort_flag;	/* 0 = not sorted
							1 = Row-maj sort
							2 = Col-maj sort
							3 = user sorted - save in order specified in save_order
							*/
	int *save_order;
	int fldflag;
	} ud_formed_struct;

typedef struct{
	int type;
	int fieldno;
	int fldtype;
	char string[80];
	int dastype;
	int ndefaults;
	int *defaults[25];
	int ans_pos[2];
	int rangeflag;
	int *range[2];
	int ans_len;
	int ans_prec;
	int mnd_flg;
	int echo;
	int row,col;
	} ud_del_struct;

EXT ud_formed_struct ud_formed_data;
EXT int last[2];	/* last[0] = type of last created,
							last[1] = field number of last created				*/
EXT ud_del_struct delfld;

#undef EXT

#define Z2FRMEDH
#endif
