/*********************************************************************
**    NAME         :  twool.h
**       CONTAINS:
**       definitions for Unibase wizards tool.
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       taool.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:53
*********************************************************************/

#ifndef TWOOLH
#include "usysdef.h"

#define UT_STR_LEN 80
#define UT_MAX_ARGS 5
#define UT_TUP_STR_SIZE 10000
#define UT_TERM_WIDTH 80
#define UT_TERM_LEN 24

typedef struct tw_cmdDesc		/* command descriptor structure */
{
	char		cmd[UT_STR_LEN];	/* command string */
	int		min_accept;			/* min. len required to accept as this command */
	int		req_args;			/* number of required arguments */
	int		req_index;			/* index of starting optional argument */
	int		opt_args;			/* number of optional arguments */
	int		opt_index;			/* index of starting optional argument */
	UU_LOGICAL	(*cmdProc)();	/* pointer to command processor */
}	tw_cmdDesc;

typedef struct tw_argDesc		/* argument descriptor structure */
{
	int			argType;		/* data type required for this argument */
	UU_LOGICAL	repeatArg;
}	tw_argDesc;

/* values for argType(make enum?): */
#define INTARG 0
#define STRARG 1
#define FLTARG 2
#define DBLARG 3
#define REALARG 4
#define KEYARG 5
#define LOGARG 6

typedef union
{
	int			intval;
	char			*charval;
	float			floatval;
	double		doubleval;
	UU_REAL		realval;
	UU_KEY_ID	keyval;
	UU_LOGICAL	logval;
} anyType;

typedef struct argBuf
{
	char		argStr[UT_STR_LEN];	/* argument string as input */
	anyType	value;					/* argument value if not string type */
} argBuf;

typedef struct cmdBuf
{
	char	cmdStr[UT_STR_LEN];	/* argument string as input */
	int	index;					/* argument value if not string type */
} cmdBuf;

#define TWOOLH
#endif

