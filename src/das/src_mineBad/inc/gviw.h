
/********************************************************************* 
**  NAME:  gviw.h
**
**      Define op-codes for WISS to display segments. 
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       gviw.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:25
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
#ifndef GVIWH
#include "gtblopst.h"
/* define output primitive op codes 0-31 */
#define UG_NOOP 0
#define UG_PAGEOP 1
 
#define UG_PLYLNA3OP 14
#define UG_PLYLNA2OP 15
#define UG_PLYLNRASOP 16

#define UG_PLYMKA3OP 18
#define UG_PLYMKA2OP 19
#define UG_PLYMKRASOP 20
#define UG_TEXTRASOP 21
#define UG_TEXTOP 22
#define UG_PROCOP 23
#define UG_CALLOP 24
#define UG_SNTRANOP 25
#define UG_FLAREAOP 26
#define UG_FLAREA3OP 27
#define UG_FLAREARASOP 28
#define UG_CELLOP3 29
#define UG_CELLOP 30
#define UG_CELLRUNOP3 31
#define UG_CELLRUNOP 32
#define UG_CELLRASOP 33
#define UG_CELLRUNRASOP 34
#define UG_SHADEAREAOP 35
/*
.....added for translucency
.....Yurong 2/1/99
*/
#define UG_LUCENCYOP 36
/*temp yurong #define UG_STIPPLEOP 37 */

/*  opcodes 42-73 attributes */
#define UG_DFATSOP 42
#define UG_COLOROP 43
#define UG_INTENSOP 44
#define UG_LSTYLOP 45
#define UG_LWIDOP 46
#define UG_PENOP 47
#define UG_FONTOP 48
#define UG_CHHGTOP 49
#define UG_CHPLANEOP 50
#define UG_CHUP2OP 51
#define UG_CHUP3OP 52
#define UG_CHPATHOP 53
#define UG_CHSPACEOP 54
#define UG_CHJUSTOP 55
#define UG_CHPRECOP 56
#define UG_SYMBOLOP 57
#define UG_PICKIDOP 58
#define UG_LNCOLROP 59
#define UG_MKCOLROP 60
#define UG_TXCOLROP 61
#define UG_FACOLROP 62
#define UG_CHEXPOP 63
#define UG_MTRANOP 64
#define UG_EDGEFLAGOP 65
#define UG_LMTRANOP 66
#define UG_FLAREANORM3OP 67
#define UG_FACOLRREPOP 68
#define UG_SHDCOLROP 69
#define UG_MATERIALOP 70

#define UG_OPPTR 128

typedef struct {
	UG_prat3 prats; 		/* primitive atts (only text used). */
	int curxform;			/*current viewing xform.*/
	int pkid; 				/*current pick id.*/
	Gtran gmodxf;			/* global modelling xform.*/
} UG_findprms;


#define GVIWH
#endif
