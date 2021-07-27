
/*********************************************************************
**    NAME         :  m7mathco1.c
**       CONTAINS:
**          UU_LOGICAL um_layonpln(key,plan)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m7mathco1.c , 25.1           
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:09         
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdcoord.h"
#include "modef.h"

#include "mcrv.h"
#include "mdrel.h"


#define UM_CYL_RAD 0
#define UM_CYL_AZ	1
#define UM_CYL_Z 2
#define UM_SPH_RAD 0
#define UM_SPH_AZ 1
#define UM_SPH_EL 2

extern double toler;  /* it is defined in the das/d5sel.c */

UU_REAL um_dsupt();

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL um_layonpln(key,plan)
**      Determine if an entity lies on a plane.
**    PARAMETERS   
**       INPUT  : 
**				key         Key of entity to test.
**			  	plan        Plane to check.
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_TRUE if true ; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_layonpln(key,plan)
UU_KEY_ID *key;
double *plan;

{
struct UM_rbsplcrv_rec rbsplcrv;
struct UM_conic_rec   conics;
struct UM_point_rec   points;
struct UM_line_rec    lines;
struct UM_circle_rec  circles;
struct UM_compcrv_rec compcrv;
int relnum,i;

ur_retrieve_data_relnum(*key,&relnum);

if(relnum == UM_POINT_REL)
{
    points.key = *key;
    ur_retrieve_data_fixed(&points);
    if(um_dsupt(plan,&points.pt[0]) < toler )
         return(UU_TRUE);
    else
         return(UU_FALSE);
}

else if(relnum == UM_LINE_REL)
{
    lines.key = *key;
    ur_retrieve_data_fixed(&lines);
    if(um_dsupt(plan,&lines.spt[0]) < toler && 
       um_dsupt(plan,&lines.ept[0]) < toler)
         return(UU_TRUE);
    else
         return(UU_FALSE);
}

else if(relnum == UM_CIRCLE_REL)
{
    circles.key = *key;
    ur_retrieve_data_fixed(&circles);
    if(um_dsupt(plan,&circles.center[0]) < toler &&
       um_vcparall_ch(plan,&circles.nvec[0]) == UU_TRUE)
        return(UU_TRUE);
    else
        return(UU_FALSE);
}

else if(relnum == UM_CONIC_REL)
{
    conics.key = *key;
    ur_retrieve_data_fixed(&conics);
    um_inverttf(conics.tfmat,conics.tfmat);
    if(um_dsupt(plan,&conics.tfmat[3][0]) < toler &&
       um_vcparall_ch(plan,&conics.tfmat[2][0]) == UU_TRUE)
       return(UU_TRUE);
    else
       return(UU_FALSE);
}

else if(relnum == UM_RBSPLCRV_REL)
{
    rbsplcrv.key = *key;
    ncl_retrieve_data_fixed(&rbsplcrv);
    for(i=0; i < rbsplcrv.no_pt; i++)
    {
       if(um_dsupt(plan,rbsplcrv.pt+i*3) > toler) return(UU_FALSE);
    }
    return (UU_TRUE);
}

else if(relnum == UM_COMPCRV_REL)
{
    compcrv.key = *key;
    ncl_retrieve_data_fixed(&compcrv);
    for (i=0; i < compcrv.no_cid; i++)
       if(!um_layonpln(&compcrv.cid[i].crvid,plan)) return(UU_FALSE);
    return(UU_TRUE);
}

}

