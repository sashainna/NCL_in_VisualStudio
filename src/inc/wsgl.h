/*********************************************************************
**    NAME         :  wsgl.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       wsgl.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:10
*********************************************************************/
#ifdef UU_OPENGL
/*
.....added by Yurong
.....8/25/97
*/
#ifdef  WSGL_FIRST
#define EXT
#else
	#ifdef __cplusplus 
		#define EXT extern "C"
	#else
		#define EXT extern
	#endif
#endif


#ifndef WSGLH
#if UU_COMP == UU_WIN2K
#include <Windows.h>
#include <wingdi.h>
#include <GL/glu.h>
#else
#include <GL/glx.h>
#endif

#include <GL/gl.h>
#include "gobas.h"
#include "gtbldef.h"
/*
.....Definitions for size of arrays
*/
#define NCHOICEPETS 5   /* number of choice device prompt-and-echo types */
#define NLOCPETS 12	/* number of locator device prompt-and-echo types */
#define NPICKPETS 1	/* number of pick device prompt-and-echo types */
#define NSTROKEPETS 1	/* number of stroke device prompt-and-echo types */
#define MAXCHOICES 30   /* max number of choices for any choice device */
#define NUMPETS 6
#define NMARKTYPES 6	/* number of marker types available */
#define NLINETYPES 5	/* number of line types available */
#define NCHDEV 500	/* number of choice devices */
#define MAXPOINTS 5000	/* max number of points on a polyline,polymarker...*/
#define MAXPRMPT 50	/* max number of prompts displayed */
#define MENUPRMPTSIZE 20/* max number of chars in menu prompt */
/*
.....Macros to convert to/from screen coordinates
*/
#define YTRANSPOSE(y) (uw_gl.ypixels-(y))

#ifdef  WSGL_FIRST
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
#else
EXT int uw_color_table[64][3];
EXT char uw_color_name[64][96];
EXT int uw_color_table_sav[64][3];
EXT char uw_color_name_sav[64][96];
#endif

typedef  GLdouble Matrix[16];

#define NOTRANS 15				/* number of xforms used 								*/
/************************* iris specific typedefs **************************/

typedef struct {

		Gint wid;				/* workstation id of this workstation */
		int xpixels;			/* X range in device coordinates (View) */
		int ypixels;			/* Y range in device coordinates (View) */
		GLdouble xndc;			/* X range in NDC coordinates */
		GLdouble yndc;			/* Y range in NDC coordinates */
		int dev_xmax;			/* machine display width */
		int dev_ymax;			/* machine display height */
		struct {
			Gfloat sf;
			Gfloat dx,dy;
		} wsxform;					/* sf and dx,dy to map ws window to raster*/

		int linetype;     /* current linetype */         
		int erase;						/* 1 = erasing a segment, 0=not 			*/
		int rasmode;					/* True, identity matrix is on stack	*/
		int maxgcolors;				/* maximum # of GRAPHIC colors			*/
		Matrix modxf;					/* current modeling xform 					*/
} UG_gldat;

typedef struct
{
	int x;
	int y;
	int width;
	int height;
} UW_glscreenvp;

EXT GLuint uw_gltext_base;

/*********************  Codes for device dependent icons **********************/

#define UW_GL_COLORREP 1
#define UW_GL_SETPOPCLR 2
#define UW_GL_RASLINE 3 
#define UW_GL_FILLRAS 4
#define UW_GL_RASTEXT 5
#define UW_GL_PICKID 6
#define UW_GL_CELLRAS 7


/********************** magic numbers *****************************************/

#define GRAPHIC 1					/* graphic surface flag									*/
#define POPUP 2					/* popup surface flag									*/


/***************** handy macros ***************************/
#define MAX(a,b) ((a>b)?a:b)		/* handy macros for clipping */
#define MIN(a,b) ((a<b)?a:b)


/******************** useful constants ****************************************/
#ifndef PI
#define PI		(3.1415926454)
#endif
#define DTOR	(PI/180.0)
#define RTOD	(180.0/PI)

/* Workstation variables */

/* Visible area of screen (apply uw_wsxform to clip) */
EXT Gipoint uw_glscrnclip[2];

#define uw_gl_ident(a)          { int i;                                \
                                        for ( i=0 ; i<16 ; i++ )        \
                                                a[i] = 0.0 ;            \
                                        for (i=0; i<4; i++)             \
                                                a[i*5]=1.0;         \
                                 }
 
EXT UW_glscreenvp uw_glscrn_vport[NOTRANS];
EXT UG_gldat uw_gl;

/* Intersection of current vport and ws_window */
EXT Gnrect3 uw_glclip;			

#ifdef NCOLORS
#undef NCOLORS
#endif
#define NCOLORS 256

EXT GLuint gl_line_style;

/*
.....Internal GL matrices are maintained
.....by NCL, since matrix manipulation by
.....OpenGL is extremely slow.
.....Bobby  -  03/12/98
*/
typedef struct
{
	GLdouble ortho[6];
	GLdouble w2v[3];
	GLdouble s1[3];
	GLdouble t2[3];
	GLdouble vup;
	GLdouble vpn[2];
	GLdouble trn[3];
	GLdouble matrix[16];
	GLint vpt[4];
	int changed;
} UW_glmatrix;	
EXT UW_glmatrix uw_gl_matrix[NOTRANS];

typedef struct
{
	GLdouble chrhgt;		/* remember dimensions of one char */
	GLdouble chrwid;
	GLdouble offset;		/* Line spacing */
	GLdouble chrphgt;		/* ... in pixels */
	GLdouble chrpwid;
	GLdouble chrpasc;		/* Char ascent */
	GLdouble chrpdsc;		/* Char descent */
	GLdouble poffset;
} UW_glfont;

EXT UW_glfont uw_glfont;

#if UU_COMP == UU_WIN2K
EXT HWND UW_NTgraphic;
EXT HDC  UW_NTgraphicDC;
EXT int current_glctx;
#endif

#define WSGLH
#undef EXT
#endif
#endif
