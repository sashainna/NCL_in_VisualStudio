/*********************************************************************
**    NAME         :  ucompb.c
**       CONTAINS:
**       uu_comp_byte
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ucompb.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:52
*********************************************************************/
/*********************************************************************
**    E_FUNCTION :  uu_comp_byte(b1,b2,n)
**       Compare memory byte array b1 to b2 for length of n.
**    PARAMETERS   
**       INPUT  : 
**          char b1[]             byte arrays to compare
**          char b2[]
**          int  n                 number bytes to compare
**       OUTPUT :  
**          output
**    RETURNS      : =0   byte arrays are equal for length n.
**                   <0   byte array b1 less than array b2.
**                   >0   byte array b1 greater than array b2.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
uu_comp_byte(b1,b2,n)
char  b1[];
char  b2[];
int    n;
{
	int   i;
	int   status;
/*------------- begin function code --------------------------------*/
	status = 0;                        /* assume arrays are equal */
	for ( i=0; i<n; i++)
	{
		if (b1[i]==b2[i])               /* we are still equal */
			continue;                    /* so continue the scan */
		if (b1[i]<b2[i]) {              /* b1 less than b2 */
			status = -1;                 /* set less than status */
			break;                       /* no more scanning */
		}
		if (b1[i]>b2[i]) {              /* b1 greater than b2 */
			status =  1;                 /* set greater than status */
			break;                       /* no more scanning */
		}
	}	
	return( status);
}
