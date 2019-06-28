/*********************************************************************
**    NAME         :  adist.c
**       CONTAINS:
**			us_dist
**			us_clospnt
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       adist.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:33
*********************************************************************/

#include "umath.h"
/*********************************************************************
**    I_FUNCTION :  float us_dist(p1,p2)
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_REAL	us_dist(p1,p2)
	UU_REAL	p1[3];
	UU_REAL	p2[3];
	{
	UU_REAL dis;
	UU_REAL term;
	int	i;

	dis = 0;
	for(i=0;i<3;i++)
		{
		term = p1[i] - p2[i];
		dis = dis + term * term;
		}
	return(sqrt(dis));
	}
/*********************************************************************
**    I_FUNCTION :  int us_clospnt(n,pt,tst_pt)
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int us_clospnt(n,pt,tst_pt)
	int 	n;
	UU_REAL pt[][3];
	UU_REAL tst_pt[3];
	{
	int j;
	int um_clospnt();

	j = um_clospnt(n,pt,tst_pt);
	return(j+1);
	}
