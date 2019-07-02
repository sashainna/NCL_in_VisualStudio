/*********************************************************************
**    NAME         :  ncluvmp.h
**       CONTAINS: Pocket Modal definitions
**    COPYRIGHT 2011 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       ncluvmp.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:37
*********************************************************************/
#include "udforms.h"
#include "nclfc.h"
#include "dselmask.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "uhep.h"

#define VSTRL NCL_MAX_LABEL_AND_SUBSCRIPT+1

typedef struct
{
	int deptype;
	int dir;
	int smooth;
	int etype;
	int level;
	int btype;
	int ttype;
	int ctype;
	int side;
	int adjfed;
	int use_contour;
	int use_min;

	char flutes[VSTRL];
	char maxdep[VSTRL];
	char trandep[VSTRL];
	char maxstep[VSTRL];
	char transtep[VSTRL];
	char minrad[VSTRL];
	char finrad[VSTRL];
	char smang[VSTRL];
	char eangle[VSTRL];
	char hrad[VSTRL];
	char feed[7][VSTRL];
	char speed[4][VSTRL];
	char ret1[VSTRL];
	char ret2[VSTRL];
	char cpln[VSTRL];
	char rapto[VSTRL];
	char botpln[VSTRL];
	char toppln[VSTRL];
	char flutelen[VSTRL];
	char toollen[VSTRL];
	char minfeed[VSTRL];
	char xyrapid[VSTRL];
	char zrapid[VSTRL];
	char drilldia[VSTRL];
	char drillang[VSTRL];
} NCLU_vmpmod;
