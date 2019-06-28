/*********************************************************************
**    NAME         :  rict
**       CONTAINS:
**       ur_create_tuple
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rict.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:42
*********************************************************************/

#include	"usysdef.h"
#include	"ribase.h"
#include	"udebug.h"

/*********************************************************************
** I_FUNCTION : status = ur_create_tuple(rel_num,&tuple_indx,data_ptr)
**      create a tuple in a specified relation
**    PARAMETERS   
**       INPUT  : 
**         rel_num, relation for which an entry needs to be created
**       OUTPUT :  
**			 tuple_indx, created tuple within the relation
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_create_tuple(rel_num,tuple_indx,data_ptr)
/* argument declarations         */
long 	rel_num;				/* rel for which a tuple id is requested	*/
long	*tuple_indx;		/* tuple allocated for use						*/
char	*data_ptr;			/* pointer to data to create with			*/
{
	/* local  parameter declarations */
	int			status;	/* status, -1 if error, 0 otherwise		*/
	UU_REL_ID	dkey;		/* a relation,tuple key						*/
	int			i;			/* for index									*/

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/

	uu_denter(UU_RITRC,(us,"ur_create_tuple, rel_num= %d",rel_num));
	status = ur_alloc_rel_tuple(rel_num,tuple_indx) ;	/* get an tuple id*/
	uu_dprint(UU_RITRC,(us,"ur_create_tuple, tuple_indx= %d",*tuple_indx));
/*
	now update the data at rel_num,tuple_indx
*/
	ur_update_tuple(rel_num,*tuple_indx,data_ptr) ;
	uu_dexit ;
	return(status)	;
}
