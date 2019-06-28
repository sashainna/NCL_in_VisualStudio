/*********************************************************************
**    NAME         : aiboxarc.c
**       CONTAINS:
**				ua_iboxcir
**				ua_iboxarc
**	
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aiboxarc.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:34
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aiboxarc.c 3.1 2/2/88 14:40:46 single"};
#else
static char uu_sccsident[]={"@(#) aiboxarc.c 3.1 2/2/88 14:40:46 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h" 
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"

/*********************************************************************
**    E_FUNCTION     : ua_iboxarc(corners, cpt, nvec, radius, sang,
**									eang, iangs, angs)
**				ua_iboxarc(corners,cpt,nvec,radius,sang,eang,iangs,angs)
**			Intersect a box and an arc.  Return 1 or 2 arcs(2 or 4 angles)
**			after splitting the arc in two at the box intersect points.
**			Only a single trimmed arc will be returned IF the arc end
**			is inside the box.
**    PARAMETERS   
**       INPUT  : 
**				corners[4]				ll,lr,ur,ul corners points
**				cpt						arc center point
**				nvec						arc normal vector
**				radius					arc radius
**				sang						arc start angle
**				eang						arc end angle
**       OUTPUT :  
**				iangs						number angles in angs
**				angs[4]					returned angles- each odd,even
**											pair is an arc
**    PARAMETERS   
**       INPUT  : 
**          none	
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_iboxarc(corners, cpt, nvec, radius, sang, eang, iangs, angs)
UU_REAL	corners[4][3];
UU_REAL	cpt[3];
UU_REAL	nvec[3];
UU_REAL	radius;
UU_REAL	sang;
UU_REAL	eang;
int		(*iangs);
UU_REAL	angs[4];
	{
	int		iangle;
	int		icirpts;
	UU_REAL	tempang;
	UU_REAL	ang1;
	UU_REAL	ang2;
	UU_REAL	cirpts[2][3];
	UU_REAL	uvc1[3];
	UU_REAL	uvc2[3];

	uu_denter(UU_STRC,(us,"ua_iboxarc(corners=%s, cpt=<%g,%g,%g>,\
		nvec=<%g,%g,%g>, radius=%g, sang=%g, eang=%g, iangs=%d, angs=%s)",
		"...", cpt[0],cpt[1],cpt[2], nvec[0],nvec[1],nvec[2],
		radius, sang, eang, *iangs, "..."));

	ua_iboxcir(corners,cpt,nvec,radius,&(icirpts),cirpts);
	uu_dprint(UU_STRC,(us,"   intersections = %d\n",icirpts));
	if( ( icirpts<2 ) )
		{
		(*iangs) = 0;
		goto procexit;
		}
		{
		UU_REAL	us_t35[3];
		um_vcmnvc(cirpts[0],cpt,us_t35);
		um_unitvc(us_t35,uvc1);
		}
		{
		UU_REAL	us_t36[3];
		um_vcmnvc(cirpts[1],cpt,us_t36);
		um_unitvc(us_t36,uvc2);
		}
		{
		UU_REAL	us_t37[3];
		UU_REAL	us_t38[3];
		us_t37[0] = 1.000000e+000;
		us_t37[1] = 0.000000e+000;
		us_t37[2] = 0.000000e+000;
		us_t38[0] = 0.000000e+000;
		us_t38[1] = 0.000000e+000;
		us_t38[2] = 1.000000e+000;
		ang1 = um_angle2p(us_t37,uvc1,us_t38);
		ang2 = um_angle2p(us_t37,uvc2,us_t38);
		}
	if( ( ang2>ang1 ) )
		{
		if( ( ( ang2-ang1 )>3.141593e+000 ) )
			{
			tempang = ang1;
			ang1 = ang2;
			ang2 = tempang;
			}
		}
	else
		{
		if( ( ( ang1-ang2 )<3.141593e+000 ) )
			{
			tempang = ang1;
			ang1 = ang2;
			ang2 = tempang;
			}
		}
	(*iangs) = 0;
	if( ( eang<sang ) )
		{
		if( ( ( ang1>sang )||( ang1<eang ) ) )
			{
			(*iangs) = ( (*iangs)+1 );
			angs[(*iangs)-1] = sang;
			(*iangs) = ( (*iangs)+1 );
			angs[(*iangs)-1] = ang1;
			}
		if( ( ( ang2>sang )||( ang2<eang ) ) )
			{
			(*iangs) = ( (*iangs)+1 );
			angs[(*iangs)-1] = ang2;
			(*iangs) = ( (*iangs)+1 );
			angs[(*iangs)-1] = eang;
			}
		}
	else
		{
		if( ( ( ang1>sang )&&( ang1<=eang ) ) )
			{
			(*iangs) = ( (*iangs)+1 );
			angs[(*iangs)-1] = sang;
			(*iangs) = ( (*iangs)+1 );
			angs[(*iangs)-1] = ang1;
			}
		if( ( ( ang2<eang )&&( ang2>=sang ) ) )
			{
			(*iangs) = ( (*iangs)+1 );
			angs[(*iangs)-1] = ang2;
			(*iangs) = ( (*iangs)+1 );
			angs[(*iangs)-1] = eang;
			}
		}
procexit:
		{
		int		us_t43;
		us_t43 = (*iangs);
		iangle = 1;
		for(;;)
			{
			if( iangle > us_t43 ) 	break;
us_l41:;
			iangle++ ;
			}
us_l42:; 
		}
	uu_dexit;
	}
