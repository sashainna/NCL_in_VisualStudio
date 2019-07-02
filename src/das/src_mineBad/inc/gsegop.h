/********************************************************************* 
**  NAME:  gsegop.h
**	 CONTAINS:
**		1. All structure typedefs for DIGS segment element data.
**		2. Character string definitions for opcode numbers.
**
**  COPYRIGHT  1984  UNICAD, Inc.
**
**    MODULE NAME AND RELEASE LEVEL 
**       gsegop.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:22
**********************************************************************/

#ifndef GSEGOPH

#include "gobas.h"
#include "gomisc.h"
#include "goatt.h"
#include "goseg.h"
#include "gobndl.h"
#include "gipick.h"

/* Part I -- higher level access to Digs structures */
#define ELTTYPE unsigned char elttype

/* GRECT2OP */
typedef struct {
	ELTTYPE;
	Gfloat  xs;
	Gfloat  ys;
	Gfloat  xe;
	Gfloat  ye;
} UG_segrect2op;

/* GCIRCLE2OP */
typedef struct {
	ELTTYPE;
	Gfloat  xc;
	Gfloat  yc;
	Gfloat  radius;
} UG_segcircle2op;

/* GARROW2OP */
typedef struct {
ELTTYPE;
Gfloat  x1;
Gfloat  y1;
Gfloat  x2;
Gfloat  y2;
} UG_segarrow2op;

/* UG_segNOOP */
typedef struct {
	ELTTYPE;
} UG_segnoop;

/* UG_segPAGEOP */
typedef struct {
	ELTTYPE;
} UG_segpageop;

/* UG_segPLYLNA3OP */
typedef struct {
	ELTTYPE;
	short      len;
	Gwpoint3 pts[16999];
} UG_segplylna3op;

/* UG_segPLYLNA2OP */ 
typedef struct {
	ELTTYPE;
	short     len;
	Gwpoint pts[10000];
} UG_segplylna2op;

/* UG_segPLYLNRASOP */
typedef struct {
	ELTTYPE;
	short     len;
	Gipoint pts[10000];
} UG_segplylnrasop;

/* UG_segPLYMKA3OP */
typedef struct {
	ELTTYPE;
	short      len;
	Gwpoint3 pts[10000];
} UG_segplymka3op;

/* UG_segPLYMKA2OP */
typedef struct {
	ELTTYPE;
	short     len;
	Gwpoint pts[10000];
} UG_segplymka2op;

/* UG_segPLYMKRASOP */
typedef struct {
	ELTTYPE;
	short     len;
	Gipoint pts[10000];
} UG_segplymkrasop;

/* UG_segTEXTOP */
typedef struct {
	ELTTYPE;
	short      len;
	Gwpoint3 position;
	char     string[10000];
} UG_segtextop;

/* UG_segTEXTRASOP */
typedef struct {
	ELTTYPE;
	short      len;
	Gipoint  position;
	char     string[10000];
} UG_segtextrasop;

/* UG_segFLAREA3OP */
typedef struct {
	ELTTYPE;
	short       len;
	Gwpoint3  pts[10000];
} UG_segflarea3op;

/* UG_segFLAREAOP */
typedef struct {
	ELTTYPE;
	short     len;
	Gwpoint pts[10000];
} UG_segflareaop;	

/* UG_segFLAREANM3OP */
typedef struct {
	ELTTYPE;
	short       len;
	Gwpoint3  pts[10000];
	Gwpoint3  norms[10000];
} UG_segflareanorm3op;

/* UG_segFLAREARASOP */
typedef struct {
	ELTTYPE;
	short     len;
	Gipoint pts[10000];
} UG_segflarearasop;	

/* UG_segCELLOP */	
typedef struct {
	ELTTYPE;
	Gwrect  rect;
	Gipoint dims;
	Gcolor  colorarray[200000];
} UG_segcellop;

/* UG_segCELLRUNOP */
typedef struct {
	ELTTYPE;
	Gwrect  rect;
	Gipoint dims;
	int     totalruns;
	int     arrays[100000];
} UG_segcellrunop;		

/* UG_segCELLRASOP */
typedef struct {
	ELTTYPE;
	Gipoint rect[2];
	Gipoint dims;
	Gcolor  colors[100000];
} UG_segcellrasop;	

/* UG_segCELLRUNRASOP */
typedef struct {			
	ELTTYPE;
	Gipoint np;
	Gipoint nq;
	Gipoint nr;
	Gipoint dims;
	int totalruns;
	int arrays[100000];
} UG_segcellrunrasop;

/* UG_segPROCOP */ 
typedef struct {
	ELTTYPE;
	int (*func)();
	int len;
	int prms[10000];
} UG_segprocop;

/* UG_segCALLOP */ 
typedef struct {
	ELTTYPE;
	int     segno;
	Gfloat  xform[4][4];
} UG_segcallop;

/* UG_segSNTRANOP */
typedef struct {
	ELTTYPE;
	short xform;		
} UG_segsntranop;

/* UG_segMTRANOP */ 
typedef struct {
	ELTTYPE;
	Gtran    xf;
	Gmodtran type;
} UG_segmtranop;

/* UG_segDFATSOP */
typedef struct {
	ELTTYPE;
	} UG_segdfatsop;

/* UG_segINTENSOP */

/* UG_segLSTYLOP */
typedef struct {
	ELTTYPE;
	Glntype ls;
} UG_seglstylop;

/* UG_segLWIDOP */
typedef struct {
	ELTTYPE;
	Gscale width;
} UG_seglwidop;

/* UG_segPENOP */ 

/* UG_segFONTOP */ 
typedef struct {
	ELTTYPE;
	Gtxfp p;
} UG_segfontop;

/* UG_segCHHGTOP */ 
typedef struct {
	ELTTYPE;
	Gchrht height;
} UG_segchhgtop;

/* UG_segCHEXPOP */
typedef struct {
	ELTTYPE;
	Gchrexp	expn;
} UG_segchexpop;

/* UG_segCHPLANEOP */
typedef struct {
	ELTTYPE;
	Gwpoint3 txpvc;
} UG_segchplaneop;

/* UG_segCHUP3OP */
typedef struct {
	ELTTYPE;
	Gwpoint3 upvec;
} UG_segchup3op;

/* UG_segCHUP2OP */
typedef struct {
	ELTTYPE;
	Gwpoint upvec;
} UG_segchup2op;

/* UG_segCHPATHOP */
typedef struct {
	ELTTYPE;
	Gtxpath path;
} UG_segchpathop;

/* UG_segCHSPACEOP */
typedef struct {
	ELTTYPE;
	Gchrsp spacing;
} UG_segchspaceop;

  /* UG_segCHJUSTOP */
typedef struct {
	ELTTYPE;
	Gtxalign	align;
} UG_segchjustop;

/* UG_segSYMBOLOP */
typedef struct {
	ELTTYPE;
	Gmktype	type;
} UG_segsymbolop;

/* UG_segPICKIDOP */
typedef struct {
	ELTTYPE;
	Gpickid	pid;
} UG_segpickidop;

/* UG_segLNCOLROP */
typedef struct {
	ELTTYPE;
	Gcolor color;
} UG_seglncolrop;

/* UG_segMKCOLROP */
typedef struct {
	ELTTYPE;
	Gcolor color;
} UG_segmkcolorop;

/* UG_segTXCOLROP */
typedef struct {
	ELTTYPE;
	Gcolor color;
} UG_segtxcolorop;

/* UG_segFACOLROP */
typedef struct {
	ELTTYPE;
	Gcolor color;
} UG_segfacolorop;

/* UG_segFACOLRREPOP */
typedef struct {
	ELTTYPE;
	Gcobundl color;
} UG_segfacolorrepop;

/* UG_segEDGEFLAGOP */
typedef struct {
	ELTTYPE;
	Gtoggle f;
} UG_segedgeflagop;

/* UG_segLMTRANOP */
typedef struct {
	ELTTYPE;
	Gtran    xf;
	Gmodtran type;
} UG_seglmtranop;

/****************************************************************/
/* Part II - Internal Digs storage. May be double or single      */
/****************************************************************/
/*
#if ( (UU_SEGDATA==SINGLE) && (UU_PREC==DOUBLE) )
*/
#if ( (UU_SEGDATA==SINGL) && (UU_PREC==DOUBL) )
	typedef	float Segfloat;
	typedef 	struct {
		float x;
		float y;
		float z;
	} Segpoint3;
	typedef 	struct {
		float x;
		float y;
	} Segpoint;
	typedef 	struct {
		Segpoint ll;
		Segpoint ur;
	} Segrect;
	typedef 	float Segtran[4][4];
	typedef 	struct {
		Gint typeno;
		int npatn;
		float patnlen;
		Gchar typepatn[100];
	} Seglntype;
	typedef 	float Segscale; 
	typedef 	float	Segchrht;
	typedef 	float	Segchrexp;
	typedef	float	Segchrsp;
#else
	typedef Gfloat Segfloat;
	typedef 	Gwpoint3 Segpoint3;
	typedef 	Gwpoint Segpoint;
	typedef 	Gwrect Segrect;
	typedef 	Gtran  Segtran;
	typedef 	Glntype Seglntype;
	typedef 	Gscale Segscale;
	typedef 	Gchrht	Segchrht;
	typedef 	Gchrexp	Segchrexp;
	typedef Gchrsp Segchrsp;
#endif

/* GRECT2OP */
typedef struct {
	ELTTYPE;
	Segfloat  xs;
	Segfloat  ys;
	Segfloat  xe;
	Segfloat  ye;
} UG_rect2op;

/* GCIRCLE2OP */
typedef struct {
	ELTTYPE;
	Segfloat  xc;
	Segfloat  yc;
	Segfloat  radius;
} UG_circle2op;

/* GARROW2OP */
typedef struct {
ELTTYPE;
Segfloat  x1;
Segfloat  y1;
Segfloat  x2;
Segfloat  y2;
} UG_arrow2op;

/* UG_NOOP */
typedef struct {
	ELTTYPE;
} UG_noop;

/* UG_PAGEOP */
typedef struct {
	ELTTYPE;
} UG_pageop;

/* UG_PLYLNA3OP */
typedef struct {
	ELTTYPE;
	short      len;
	Segpoint3 pts[16999];
} UG_plylna3op;

/* UG_PLYLNA2OP */ 
typedef struct {
	ELTTYPE;
	short     len;
	Segpoint pts[10000];
} UG_plylna2op;

/* UG_PLYLNRASOP */
typedef struct {
	ELTTYPE;
	short     len;
	Gipoint pts[10000];
} UG_plylnrasop;

/* UG_PLYMKA3OP */
typedef struct {
	ELTTYPE;
	short      len;
	Segpoint3 pts[10000];
} UG_plymka3op;

/* UG_PLYMKA2OP */
typedef struct {
	ELTTYPE;
	short     len;
	Segpoint pts[10000];
} UG_plymka2op;

/* UG_PLYMKRASOP */
typedef struct {
	ELTTYPE;
	short     len;
	Gipoint pts[10000];
} UG_plymkrasop;

/* UG_TEXTOP */
typedef struct {
	ELTTYPE;
	short      len;
	Segpoint3 position;
	char     string[10000];
} UG_textop;

/* UG_TEXTRASOP */
typedef struct {
	ELTTYPE;
	short      len;
	Gipoint  position;
	char     string[10000];
} UG_textrasop;

/* UG_FLAREA3OP */
typedef struct {
	ELTTYPE;
	short       len;
	Segpoint3  pts[10000];
} UG_flarea3op;

/* UG_FLAREAOP */
typedef struct {
	ELTTYPE;
	short     len;
	Segpoint pts[10000];
} UG_flareaop;	

/* UG_FLAREANM3OP */
typedef struct {
	ELTTYPE;
	short       len;
	Segpoint3  pts[10000];
	Segpoint3  norms[10000];
} UG_flareanorm3op;

/* UG_FLAREARASOP */
typedef struct {
	ELTTYPE;
	short     len;
	Gipoint pts[10000];
} UG_flarearasop;	

/* UG_CELLOP */	
typedef struct {
	ELTTYPE;
	Segrect  rect;
	Gipoint dims;
	Gcolor  colorarray[200000];
} UG_cellop;

/* UG_CELLRUNOP */
typedef struct {
	ELTTYPE;
	Segrect  rect;
	Gipoint dims;
	int     totalruns;
	int     arrays[100000];
} UG_cellrunop;		

/* UG_CELLRASOP */
typedef struct {
	ELTTYPE;
	Gipoint rect[2];
	Gipoint dims;
	Gcolor  colors[100000];
} UG_cellrasop;	

/* UG_CELLRUNRASOP */
typedef struct {			
	ELTTYPE;
	Gipoint np;
	Gipoint nq;
	Gipoint nr;
	Gipoint dims;
	int totalruns;
	int arrays[100000];
} UG_cellrunrasop;

/* UG_SHADEAREAOP */
typedef struct {
   ELTTYPE;
   short       len;
   short       type;
   Segpoint3  pts[10000];
} UG_shadearea;

/* UG_PROCOP */ 
typedef struct {
	ELTTYPE;
	int (*func)();
	int len;
	int prms[10000];
} UG_procop;

/* UG_CALLOP */ 
typedef struct {
	ELTTYPE;
	int     segno;
	Segfloat  xform[4][4];
} UG_callop;

/* UG_SNTRANOP */
typedef struct {
	ELTTYPE;
	short xform;		
} UG_sntranop;

/* UG_MTRANOP */ 
typedef struct {
	ELTTYPE;
	Segtran    xf;
	Gmodtran type;
} UG_mtranop;

/* UG_DFATSOP */
typedef struct {
	ELTTYPE;
	} UG_dfatsop;

/* UG_INTENSOP */

/* UG_LSTYLOP */
typedef struct {
	ELTTYPE;
	Seglntype ls;
} UG_lstylop;

/* UG_LWIDOP */
typedef struct {
	ELTTYPE;
	Segscale width;
} UG_lwidop;

/* UG_PENOP */ 

/* UG_FONTOP */ 
typedef struct {
	ELTTYPE;
	Gtxfp p;
} UG_fontop;

/* UG_CHHGTOP */ 
typedef struct {
	ELTTYPE;
	Segchrht height;
} UG_chhgtop;

/* UG_CHEXPOP */
typedef struct {
	ELTTYPE;
	Segchrexp	expn;
} UG_chexpop;

/* UG_CHPLANEOP */
typedef struct {
	ELTTYPE;
	Segpoint3 txpvc;
} UG_chplaneop;

/* UG_CHUP3OP */
typedef struct {
	ELTTYPE;
	Segpoint3 upvec;
} UG_chup3op;

/* UG_CHUP2OP */
typedef struct {
	ELTTYPE;
	Segpoint upvec;
} UG_chup2op;

/* UG_CHPATHOP */
typedef struct {
	ELTTYPE;
	Gtxpath path;
} UG_chpathop;

/* UG_CHSPACEOP */
typedef struct {
	ELTTYPE;
	Segchrsp spacing;
} UG_chspaceop;

  /* UG_CHJUSTOP */
typedef struct {
	ELTTYPE;
	Gtxalign	align;
} UG_chjustop;

/* UG_SYMBOLOP */
typedef struct {
	ELTTYPE;
	Gmktype	type;
} UG_symbolop;

/* UG_PICKIDOP */
typedef struct {
	ELTTYPE;
	Gpickid	pid;
} UG_pickidop;

/* UG_LNCOLROP */
typedef struct {
	ELTTYPE;
	Gcolor color;
} UG_lncolrop;

/* UG_LUCENCYOP */
typedef struct {
	ELTTYPE;
	int lucency;
} UG_lucencyop;

/* UG_STIPPLEOP */
/*temp yurong typedef struct {
	ELTTYPE;
	int dummy;
	unsigned char stipple[128];
} UG_stippleop;
*/
/* UG_LUCENCYOP */
typedef struct {
	ELTTYPE;
	int material;
} UG_materialop;

/* UG_MKCOLROP */
typedef struct {
	ELTTYPE;
	Gcolor color;
} UG_mkcolorop;

/* UG_TXCOLROP */
typedef struct {
	ELTTYPE;
	Gcolor color;
} UG_txcolorop;

/* UG_FACOLROP */
typedef struct {
	ELTTYPE;
	Gcolor color;
} UG_facolorop;

/* UG_FACOLRREPOP */
typedef struct {
	ELTTYPE;
	Gcobundl color;
} UG_facolorrepop;

/* UG_EDGEFLAGOP */
typedef struct {
	ELTTYPE;
	Gtoggle f;
} UG_edgeflagop;

/* UG_LMTRANOP */
typedef struct {
	ELTTYPE;
	Segtran    xf;
	Gmodtran type;
} UG_lmtranop;

/****************************************
** Opcode character arrays             **
*****************************************/

extern char ug_ops[74][16];

#define GSEGOPH
#endif
