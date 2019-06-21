/*********************************************************************
**    NAME         :  amclospt.c
**       CONTAINS:
**			us_clospnt
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       amclospt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:36
*********************************************************************/
#include "umath.h"
#include "usysdef.h"
/*********************************************************************
**    E_FUNCTION     :  um_clospnt(npt,pt,tst_pt)
**       Find point in array pt closest to test point.
**    PARAMETERS   
**       INPUT  : 
**          npt							number of points in point array
**          pt								point array
**          tst_pt						test point
**       OUTPUT :  
**          none
**    RETURNS      : index to closest point
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

 int um_clospnt(npt,pt,tst_pt)
	int		npt;
	UU_REAL	pt[][3];
	UU_REAL tst_pt[3];
	{
	UU_REAL dis,temp;
	int i,indx;
	UU_REAL um_dcccc();

	/*------------------------------------------------------------------------
	**  Start of Executable Code
	**-----------------------------------------------------------------------*/

	dis = 1.0e20;
	indx = -1;
	for(i=0;i<npt;i++)
		{
		temp = um_dcccc(tst_pt,&pt[i][0]);
		if( dis > temp )
			{
			dis = temp;
			indx = i;
			}
		}
	return(indx);
	}
