/*********************************************************************
**    I_FUNCTION :  plotfrm.h
**       description
**     MODULE NAME AND RELEASE LEVEL 
**       plotfrm.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:41
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

#ifndef PLOTFRMH

#include "usysdef.h"


/***	pen color	***/
#define PWHITE 1
#define PBLACK 2
#define PGREEN 3
#define PBLUE 4
#define PCYAN 5
#define PRED 6
#define PYELLOW 7
#define PMAGENTA 8

/*
.....That is too small for plot string
.....changed by Yurong
*/
/*#define   PMAXSTRING     21 */
#define   PMAXSTRING  80

typedef enum
	{
	 A,
	 B,
	 C,
	 D,
	 E,
	 A0,
	 A1,
	 A2,
	 A3,
	 A4
	}	DCSIZE;

typedef enum
	{
	 chart_paper,
	 vellum,
	 tracing_bond,
	 polyester_film,
	 paper_spool
	}	MDTYPE;

typedef struct
	{
	 char		sfnm[PMAXSTRING];
	 DCSIZE	dsize;	/* paper size */
	 char		mdtype[PMAXSTRING];		/* media type */
	 int	 	formno;		/* form number */
	 char		pentbnm[PMAXSTRING];		/* pen table name */
	 char		pen[256][PMAXSTRING];	/* pen holder setup */
	}	PLOTFRM;

typedef struct
	{
	 char		ptbnm[PMAXSTRING];
	 int		penno[256][3];
	}	PENTBFRM;

static struct
	{	UU_REAL	x, y;	}	ppsize[] =
	{{0.2794,0.2159},{0.4317,0.2794}, 
	 {0.5407,0.4264},{0.8582,0.5280},
	 {1.0731,0.8445}, 
	 {1.1590,0.8110},{0.8210,0.5740},
	 {0.5740,0.4000},{0.4100,0.2870},
	 {0.2870,0.2000}};

/* changed by kathy */
/*	{{0.2801,0.2232},{0.4325,0.2679}, */
/*	{{0.2667,0.2032},{0.4317,0.2794}, */
/*	 {1.1176,0.8636}, */

/*
.....The following unused integers caused link warnings on IRIX 6.2
.....removed 14 Oct 1997 IJD
*/
/* int drwsize, psize, qkplot; */

#define PLOTFRMH
#endif
