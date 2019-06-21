/*********************************************************************
**
**    NAME         :  gksftn.c
**
**       CONTAINS:
**          gsetdsplst(iset)
**				glina3()
**				gmova3()
**				gdraw()
**				gpta3() ** commented out **
**
**
**    MODULE NAME AND RELEASE LEVEL 
**       gksftn.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:20
**
*********************************************************************/
#include "ustdio.h"
#include "nclfc.h"
#include "g.h"

#define max 70
static int num = 0;
static Gwpoint3 npt[max];
static int displst = 0;

/*********************************************************************
**    E_FUNCTION     : gsetdsplst(iset)
**       Set the display list to start or stop saving points in entity display list.
**    PARAMETERS   
**       INPUT  : 
**          iset       - value for displst
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gsetdsplst(iset)
int iset;
{
	displst = iset;
}

/*********************************************************************
**    E_FUNCTION     : glina3(x,y,z)
**       Draw from the current position to X,Y,Z.
**    PARAMETERS   
**       INPUT  : 
**          x							coordinates to draw to
**          y
**          z
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
glina3(x,y,z)
	double *x,*y,*z;

	{
	UU_KEY_ID sav_key = 0;

	if (num == max)
	{
		sav_key = ncl_displst_getckey();
		gmova3(&npt[num-1].x, &npt[num-1].y, &npt[num-1].z);
/*
.....restore the pick id of the curve being displayed currently
*/
		ncl_displst_setckey(sav_key);
	}
	npt[num].x = *x;
	npt[num].y = *y;
	npt[num].z = *z;
	num++;
	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : gmova3(x,y,z)
**       Move to X,Y,Z.
**    PARAMETERS   
**       INPUT  : 
**          x							coordinates to move to
**          y
**          z
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
gmova3(x,y,z)
	double *x,*y,*z;

	{

	if (num > 1) gdraw();
	npt[num].x = *x;
	npt[num].y = *y;
	npt[num].z = *z;
	num++;

	return 0;
	}

/*********************************************************************
**    E_FUNCTION     : gpta3(x,y,z)
**       Draw a marker at the point X,Y,Z.
**    PARAMETERS   
**       INPUT  : 
**          x							coordinates to move to
**          y
**          z
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#ifdef OLDNCL
gpta3(x,y,z)
	double *x,*y,*z;

	{
	Gwpoint3 pos;
	char str[256];

	pos.x = *x;
	pos.y = *y;
	pos.z = *z;
	sprintf(str, "put up marker at (%f,%f,%f)", pos.x, pos.y, pos.z);
	um_pscroll(str);
	gpolymarker3(1,&pos);
	}
#endif /* OLDNCL */

/*********************************************************************
**    E_FUNCTION     : gdraw()
**       Call DIGS to draw the current polyline buffer.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
**    NOTE: This routine had been called gplout for quite a while
**          and was changed to gdraw to remove a multiply defined
**          when linking with an unmodified version of unilib.a
**
*********************************************************************/
gdraw()

	{
	if (num > 1)
	{
		gpolyline3(num, npt);
		if (displst) ncl_displst (num, npt);
	}
	num = 0;
	return 0;
	}

