#include "usysdef.h"
#include	"udebug.h"
#include	"ribase.h"

/*********************************************************************
**    NAME         :  reru
**       CONTAINS:
**       ur_reset_unibase
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reru.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:35
*********************************************************************/

/*********************************************************************
**    E_FUNCTION     :  status = ur_reset_unibase()
**      reset unibase
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_reset_unibase()
    /* argument declarations         */

{

    /* local  parameter declarations */
		int	status				;	/* status, -1 if error, 0 otherwise		*/
		int	rel					;	/* relation number							*/
		int	tuple_indx			;	/* index of tuple to delete				*/

/*
		externals
*/
		extern	char UR_dpn[]	;

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
		uu_denter(UU_RTRC,(us," reset unibase")) ;

		status = 0 ;
		UR_dpn[0] = '\000'	 ;	/* reset default pathname	*/
/*
		clear last modified keys
*/
		UR_last_mod_mkey = 0 ;
		UR_last_mod_rkey = 0 ;
/*
		flush the delete stack
*/
		ur_flush_del_stack() ;
/*
		now delete each and every tuple
*/

		for(rel = 0; rel <= UR_MAX_REL; rel++)
		{
			if(UR_rcb[rel].status >= 0 && UR_rcb[rel].active_tuple_cnt > 0)
			{
				tuple_indx = 1			 ;
				while(!ur_get_next_tuple_index(rel,&tuple_indx))
				{
					ur_force_delete_tuple(rel,tuple_indx) ;
					tuple_indx++ ;
				}
			}
		}
		uu_dexit ;
		return(status) ;
	}
