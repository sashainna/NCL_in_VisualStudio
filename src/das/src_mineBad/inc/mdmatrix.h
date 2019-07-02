
/*********************************************************************
**    NAME         :  mdmatrix
**       CONTAINS: matrix definitions
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       mdmatrix.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:29
*********************************************************************/
#ifndef   UM_MDMATRIX

#define  UM_SOLVEQNS        1
#define  UM_SOLVANDINVERT   0
#define  UM_INVERT          -1
#define  UM_ILLCONDITIONED  0
#define  UM_WELLCONDITIONED 1

struct UM_matrix
	{
	int nrow;				/* number of rows */
	int ncol;				/* number of columns */
	double *ary;			/* array [nrow, ncol] */
	};

#define   UM_MDMATRIX
#endif
