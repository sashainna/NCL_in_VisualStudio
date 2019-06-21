/*********************************************************************
**    NAME         : adrfdefs.h
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       adrfdefs.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:10
*********************************************************************/

struct UA_PPICKREC
	{
	int		depth;	/* depth */
	int		pickpath[5];	/* pickpath */
	} ;
struct UA_NDCLOCREC
	{
	UU_REAL	loc[3];	/* loc */
	int		transform;	/* transform */
	int		choice;	/* choice */
	UU_REAL		wndc3_mat[4][4];
	UU_REAL		ndcw3_mat[4][4];
    char        label[80]; /*** Added by Paul for "text" input. 10/06/93 ***/
	} ;
struct UA_PLOCREC
	{
	struct UA_PPICKREC	ppick;	/* ppick */
	struct UA_NDCLOCREC	ndcloc;	/* ndcloc */
	} ;
struct UA_PICKENT
	{
	int		num;	/* num */
	int		key[5];	/* key */
	} ;
struct UD_SELREC
	{
	int		depth;	/* depth */
	int		pickpath[5];	/* pickpath */
	} ;
/* declarations of calculation functions */
UU_REAL ua_dim_value();
UU_REAL ua_dir_angle();
UU_REAL ua_arclen_arcang();
UU_REAL ua_angular_arcang();
UU_REAL ua_arrowang();
