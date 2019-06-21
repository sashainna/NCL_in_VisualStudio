/*********************************************************************
**    NAME         :  tiges.h
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       tiges.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:54
*********************************************************************/

#include "usysdef.h"
#include "xenv1.h"

#ifndef TIGESH
#define TIGESH


#ifdef MAINIGES
#define TIGMAIN
#endif
#include "tigglobal.h"
#undef TIGMAIN

#ifdef MAINIGES
#define EXT 
#else
#ifdef __cplusplus
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif
 
  /***************************************************************************/
  /*                                                                         */
  /*                                                                         */
  /*   include file for IGES translator project                              */
  /*                                                                         */
 /*                                                                         */
  /***************************************************************************/
  /*                                                                         */
  /*   the following structure is the directory record structure             */
  /*                                                                         */
struct dir_rec{
   int drec_num;			   /*jkd15a: used for unique labels */		
   int rel_type;               /* IGES entity type */
   int par_ptr;                /* pointer to entities parameter data */
   int line_font;              /* line font */
   int level;                  /* entities level number */
   int view_ptr;               /* pointer to entities view */
   int matrix_ptr;             /* pointer to entities construction matrix */
   int blank;                  /* blank/unblank files */
   int sub_swt;                /* subordinates switch */
   int use_flg;                /* netity use flag */
   int hier;                   /* entity hierarchy flag */
   int line_wt;                /* line weight */
   int pen_no;                 /* pen number */
   int par_cnt;                /* number of parameter */
   int form_no;                /* entity form number */
   char label[10];              /* entity label */
   int seq_no;                 /* sequence number */
   int subsno;                 /* subscript number 20.7.92vp - reversed order 
                                  with seq_no (unlike in IGES spec.) but put
											 in production version in 18.12.92. */
 };
/*                                                                     */
/*   the following structure is the global record structure             */
/*                                                                     */
 struct global_rec {
   char delim;                /* field delimitor */
   char term;                 /* record delimitor */
   int sing_pr_int;           /* bits per integer */
   int sing_pr_exp;           /* bits per float exp */
   int sing_pr_mant;          /* bits per float mantissa */
   int doub_pr_exp;           /* bites per double exp */
   int doub_pr_mant;          /* bits per double mantissa */
   UU_REAL scale;             /* model space scale */
   int units;                 /* units flag 1=inches 2=MM */
   char unitstr[7];           /* units string info */
   char datestr[17];          /* creation date */
   UU_REAL granularity;       /* minimum resolution */
   UU_REAL coor_max;          /* max of x || y|| z */
   char prod_id1[80];		  /* product id (cpp)  */
   UX_pathname file_id;		  /* internal file id (cpp)   */
   char sys_id[80];			  /* system id (cpp)   */
   char process_id[80];		  /* (cpp) */
   char prod_id2[80];		  /* (cpp) */
   char author[80];		      /* (cpp) */
   char author_org[80];		  /* (cpp) */
   };

EXT  UU_REAL unit_scale;        /* units conversion factor */
EXT  UU_REAL line_scale;        /*jkd33: line width scale */
/*                                                                     */
/* Section pointers                                                    */
/*                                                                     */
EXT  int sect_ptr[4];
/*                                                                     */
/*  Field Delimitors                                                   */
/*                                                                     */
EXT  char delim;                   /* parameter field delimitor */
EXT  char term;                    /* parameter record delimetor */
/*                                                                     */
/* IGES file designator                                                */ 
/*                                                                     */
EXT  int iges_fd;                  /* IGES disc file designator */
/*                                                                     */
/* UNIDDL file designator                                              */ 
/*                                                                     */
EXT  int ddl_fd;                  /* UNIDDL disc file designator */

/*                                                                     */
/* Directory & Parameter buffers                                       */
/*                                                                     */
EXT  char dir_buff[2048];
EXT  char par_buff[2048];
EXT  int dbyte,pbyte;              /* byte offset in buffers */
 
/*jkd5: flag each entity in directory: translated or not */
#define MAX_ENTITIES 30000	/*jkd54: change from 20000 to 30000 */
EXT 	char *xlated_flag;
EXT 	UU_KEY_ID *xlated_key;

/*
...vp 1/12/98 list of surfaces converted to IGES
*/
typedef struct {
	UU_KEY_ID bskey;
	int drec;
} surf_list;

/*
.....This list contains the color and the pointer to the 314 color
.....entities present in the iges file.
*/
struct UIG_color_struct{
	int color;
	int irec;};

/*                                                                     */
/* Start index for the variable length list storage                    */
/*                                                                     */
#define IDX1 2000
#define IDX2 22000
#define IDX3 42000
#define IDX4 62000
#define IDX5 82000
#define IDX6 102000			/* cpp original numbers were in error */
#define IDX7 122000

/*                                                                      */
/* TRANSFORMATION data                                                  */
/*                                                                      */
/*jkd4: change MAXTRAN from 100 to 200 */
/*jkd49: change MAXTRAN from 200 to 1000 */
/*NCL: changed to 5000 in MPE 2.6 */
#define MAXTRAN 5000
EXT  int  t_num;
EXT  int  t_ptr[MAXTRAN];
EXT  UU_REAL tran[MAXTRAN][12];

/*                                                                      */
/* IGES entity types & names.                                           */
/* Changing 36 to 38 due to the addition of GBOUNDRY and GBDSRF */
/* Changing 38 to 39 due to the addition of GMSBO */
/*                                                                      */
#define IG_NUM 46				/* Number of IGES entity types handled */
#define DDL_NUM 46				/* Number of IGES entity types handled */
#define IG_LAYER 9999
EXT  int iges_type[IG_NUM+1];		/*jkd29: fix iges type mapping */	
EXT  char *iges_name[IG_NUM+1];
EXT  char *ddl_nam[DDL_NUM+1];

#define GCOMPOSITE   102
#define GARC         100
#define GCONIC       104
#define GPOLY        106
#define GPOLY3D      606
#define GPOLY6D      906
#define GPLANE       108
#define GLINE        110
#define GSPLINE      112
#define GSPLSURF     114
#define GPOINT       116
#define GRULEDSRF    118
#define GREVSRF      120
#define GTBCYSRF     122
#define GTRAN        124
#define GRSPLINE     126
#define GRSPLSRF     128
#define GOFFSTSRF    140
#define GBOUNDARY    141        /*Boundary entity. JLS 10-30-98 */
#define GCRVONSRF    142
#define GBDSRF       143        /*Bounded surface entity. JLS 10-30-98 */ 
#define GTRIMSRF     144
#define GMSBO			186		  /*Manifold solid B-Rep Object Entity.06-16-05 */
#define GPLSF			190		  /*Planar Surface Entity */
#define GNOTE        212
#define GLEADER      214
#define GRADIM       222
#define GDIADIM      206
#define GLINDIM      216
#define GANGDIM      202
#define GLABEL       210
#define GGENSYM      228
#define GGROUP       402
/*
.....Changed user defined entity number from 502 to 605 
.....Since 502 is defines as a vertex list by iges.
*/
#define VIEWVS       605		 										
#define PLNASSOC     602
#define SUB_FIG      308
#define DRAW         404
#define PROPERTY     406
#define INST         408
#define VIEW         410
/*
..... Added these entities for solid and color
..... Himani
 */
#define GVERLST      502
#define GEDGLST      504
#define GLOOP        508
#define GFACE        510
#define GSHELL       514
#define GCOLOR			314

#define STANDARD     0
#define SCROLL       1
#define RESET        2
#define INIT         3

#if UU_COMP == UU_WIN2K
#define UIG_MAXCOLOR 64
#else
#define UIG_MAXCOLOR 16
#endif
#define UIG_STDCOLOR 16

#ifdef  MAINIGES
int uw_color_table[64][3] =
{
		0, 0, 0,				/* Black */
		255,  255, 255, 	/* White */
		0, 0, 255,			/* Dodger Blue */
		255,  0,   0, 		/* Red */
		0,    255, 0,		/* Green */
		255,  0,   255, 	/* Magenta */
		255,  255, 0, 		/* Yellow */
		0,    255, 255,	/* Cyan */
		184,  134, 11, 	/* Dark Goldenrod */
		210,  180, 140,	/* Tan */
		173,  216, 230,	/* Light Blue */
		84,   255, 159,	/* SeaGreen1 */
		255,  165, 0,		/* Orange */
		255,  195, 203,	/* Pink */
		221,  160, 221, 	/* Plum */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */

		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */

		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192,	/* Gray */
		192,  192, 192	/* Gray */
};
char uw_color_name[64][96] =
{
		"BLACK",
		"WHITE",
		"BLUE",
		"RED",
		"GREEN",
		"MAGNTA",
		"YELLOW",
		"CYAN",
		"BROWN",
		"LTTAN",
		"LTBLUE",
		"SEAGRN",
		"ORANGE",
		"PINK",
		"PURPLE",
		"GREY",
		"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
		"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
		"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", ""};
int uw_color_table_sav[64][3];
char uw_color_name_sav[64][96];
struct UIG_color_struct *UIG_color_array = UU_NULL;
#else
EXT int uw_color_table[64][3];
EXT char uw_color_name[64][96];
EXT int uw_color_table_sav[64][3];
EXT char uw_color_name_sav[64][96];
EXT struct UIG_color_struct *UIG_color_array;
#endif
#undef EXT
#endif
