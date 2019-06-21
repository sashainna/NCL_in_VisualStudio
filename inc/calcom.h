
/*********************************************************************
**    NAME         :  calcom.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       calcom.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:11
*********************************************************************/

/**  calcom.h - calculator's common definition  **/
#ifndef	UQ_CALCOM
# define	UQ_SYMSIZE	9			/* effective symbol size for symbol table */
# define UQ_ARGNUM  5			/* one more legal parameter number  */
# define UQ_FUNCSIZE	 20		/* function table size  */
# define UQ_VALSIZE	 50		/* number and coordinate table size */

/**  symbol type definition  **/

#define UQ_CNONE			7
#define UQ_SCALAR			8
#define UQ_FUNCTION		9
#define UQ_FUNCARG		10
#define UQ_MATRIX       11
#define UQ_QUERYFUNC		12

#define	UQ_COORD		0
#define  UQ_VECT		1
#define	UQ_QSTB_BUFSZ	4

#include	"qcddl.h"
typedef	struct UQ_func_rec	UQ_func;
typedef	struct UQ_qstb_rec	UQ_qstb;

typedef struct rslt
	{
	 int  ctype;
	 union
		{
		 UU_REAL  sval;
		 UU_REAL  cval[3];
		} val;
	} CRSLT;

typedef	int	(*UQ_queryfunc)();
typedef struct query
	{
	 int	type;
	 char	*querynm;
	 UQ_queryfunc	funcnm;
	}	UQ_query;

#define	UQ_CALCOM
#endif
