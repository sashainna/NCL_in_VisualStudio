/********************************************************************* 
**  NAME:  wsmc.h
**
**      GKS Masscomp workstation: data definitions section.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL 
**       wsmc.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:11
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) wsmc.h 3.2 12/10/87 13:57:07 single"};
#else
static char uu_sccsident[]={"@(#) wsmc.h 3.2 12/10/87 13:57:07 double"};
#endif
#endif

#include "gtbl.h"

typedef struct {
		int wid;						/* workstation id of this workstation */
		int ttfd;					/* I/O line file descriptor */
		int curr_line_index;    /* current color flags */
		int curr_marker_index;
		int curr_text_index;
		int curr_fa_index;
		int nocolor;
		int cms_size;          /* number of definable colors */
		int nplanes;           /* number of planes */
		int chrhgt;            /* character height */
		int chrwid;            /* character width */
		int max_poly_sides;    /* IGP=64, Aurora=256 */
		int cursmask;				/* plane(s) to write cursor to */
		} UG_Gmcdat;


/* screen will hold ROWMAX rows of COLMAX  characters each, when using
	the standard size alpha characters */
#define NCHOICEPETS 5		/* number of choice device prompt-and-echo types */
#define NSTRINGPETS 6		/* number of string device prompt-and-echo types */
#define NLOCPETS 5			/* number of locator device prompt-and-echo types */
#define NPICKPETS 1			/* number of pick device prompt-and-echo types */
#define NSTROKEPETS 1		/* number of stroke device prompt-and-echo types */
#define NUMPETS 6          /* number of available prompt-and-echo types */

#define NMARKTYPES 6			/* number of marker types available */
#define NLINETYPES 5			/* number of line types available */
#define NCHDEV 350			/* number of choice devices */
#define MAXCHOICES 50		/* max number of choices for any choice device */

#ifdef IGP
#define UG_DESC BB_DESC
#define UG_COPY_BB BB_MODE_S
#define UG_XOR_BB BB_MODE_S_XOR_D
#define UG_NOT_BB BB_MODE_NS
#define BACKGROUND 1
#define WHITE 2
#define BLACK 3

#else /* Aurora */
#define UG_DESC MGBB_DESC
#define UG_COPY_BB MGBB_M_S
#define UG_XOR_BB MGBB_M_S_XOR_D
#define UG_NOT_BB MGBB_M_NS
#define BACKGROUND 0
#define WHITE 1
#define BLACK 2

#endif

int CLEAR_COLOR;           /* color to use for clearing */
