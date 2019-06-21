/*********************************************************************
**    NAME         :  rics.c
**       CONTAINS:
**       uri_calc_space()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rics.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:41
*********************************************************************/

#include	"usysdef.h"
#include	"udebug.h"
#include	"ribase.h"

/*********************************************************************
**    I_FUNCTION :  status = uri_calc_space(bytes, rel, bytes_each)
**       calculate the distribution of the number of bytes specified
**			according to the weights of the varlists of rel
**    PARAMETERS   
**       INPUT  : 
**          bytes		int	number of bytes to distribute
**				rel		int	relation to distribute the bytes for
**       OUTPUT :  
**          bytes_each	int[UR_rcb[rel].n_varl]	array of # bytes for each array
**    RETURNS      : 0 - OK; non-zero - no good
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uri_calc_space(bytes, rel, bytes_each)
int					bytes;	/* # bytes to be distributed among the varlists */
int					rel;		/* relation to distribute the above for	*/
int					bytes_each[]; /* # of bytes for each varlist */
{
		int	status;			/* return status	*/
		int	i;					/* an index	*/
		int	req_size;		/* required size of lists as initialized */
		UU_REAL	weight;		/* percentage required for each list	*/

		uu_denter(UU_RTRC,(us,"uri_calc_space for rel= %d, size= %d", rel,bytes));
		status = 0;
		if(bytes < 0)
		{
			status = -1;
		}
		else
		{
			/* calc the weight of each list and # of bytes to be assigned */
			req_size = 0;
			for(i = 0; i < UR_rcb[rel].n_varl; i++)
			{
				req_size += UR_rcb[rel].lparms[i].atom_size *
								UR_rcb[rel].lparms[i].list_size;
			}
			for(i = 0; i < UR_rcb[rel].n_varl; i++)
			{
				if(bytes > 0)
				{
					weight = (float) (UR_rcb[rel].lparms[i].atom_size *
										UR_rcb[rel].lparms[i].list_size) / req_size;
					uu_dprint(UU_RITRC,(us,"weight[%d]= %f",i,weight)) ;
/*
... aak 11-nov-1997: enforce alignment at sizeof(UU_REAL) for any 
... value of "bytes"; was
					bytes_each[i+1] = sizeof(long) * 
									(int)((bytes * weight) / sizeof(long));
*/
					bytes_each[i+1] = sizeof(UU_REAL) * 
									(int)((bytes * weight) / sizeof(UU_REAL));
				}
				else
				{
					bytes_each[i+1] = 0;
				}
			}
		}
		uu_dexit	;
		return(status) ;
}
