/*********************************************************************
**    NAME         :  m7mathco.c
**       CONTAINS:
**			UU_LOGICAL um_cceqcc(cc1,cc2)
**			UU_LOGICAL um_cceqcc_tol(cc1,cc2, tolerance)
**			UU_LOGICAL um_cceqcc_ch(cc1,cc2)
**			um_cotocc(stype,sc,cc)
**			um_cotocc(stype,sc,cc)
**			um_cctoco(cc,stype,sc)
**			um_cotoco(typei,sci,typeo,sco)
**			um_vctoco(vc,type,co)
**			UU_REAL um_dcccc(cc1,cc2)
**			um_cctmtf(cci, tf, cco)
**			int um_ccnearcc(pt,npts,ptary)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m7mathco.c , 25.1
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
**    E_FUNCTION     : UU_LOGICAL um_cceqcc(cc1,cc2)
**      Determine if two cartesian  coordinates are equal.
**    PARAMETERS   
**       INPUT  : 
**				cc1							first cartesian  coordinate
**			  	cc2							second cartesian  coordinate
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_TRUE if they are equal; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
um_cceqcc(cc1,cc2)
	UM_coord cc1;
	UM_coord cc2;

	{
	UM_length dist;				/* distance between the two points */

/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
**
**  block comment
*/
	dist = um_dcccc(cc1,cc2);
    /* 
	    sprintf(UM_sbuf,"um_cceqcc: cc1(%g,%g,%g)",cc1[0],cc1[1],cc1[2]);
	    um_pscroll(UM_sbuf);
	    sprintf(UM_sbuf,"        cc2(%g,%g,%g)",cc2[0],cc2[1],cc2[2]);
	    um_pscroll(UM_sbuf);
	    sprintf(UM_sbuf,"        dist=%g",dist);
	    um_pscroll(UM_sbuf);
    /* */
	if (dist < UM_FUZZ) return (UU_TRUE); else return (UU_FALSE);
	}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL um_cceqcc_tol(cc1,cc2,tolerance)
**      Determine if two cartesian  coordinates are equal within tolerance.
**    PARAMETERS
**       INPUT  :
**              cc1                         first cartesian  coordinate
**              cc2                         second cartesian  coordinate
**              tolerance 
**       OUTPUT :
**          none
**    RETURNS      :
**          UU_TRUE if they are equal; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
um_cceqcc_tol(cc1,cc2,tolerance)
    UM_coord cc1;
    UM_coord cc2;
	 UU_REAL tolerance; 
    {
    UU_REAL dist;             /* distance between the two points */

    dist = um_dcccc(cc1,cc2);
    if (dist < tolerance) 
		return (UU_TRUE); 
  	 else 
		return (UU_FALSE);
    }


/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL um_cceqcc_ch(cc1,cc2)
**      Determine if two cartesian  coordinates are equal.
**    PARAMETERS
**       INPUT  :
**              cc1                         first cartesian  coordinate
**              cc2                         second cartesian  coordinate
**       OUTPUT :
**          none
**    RETURNS      :
**          UU_TRUE if they are equal; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
um_cceqcc_ch(cc1,cc2)
    UM_coord cc1;
    UM_coord cc2;

    {
    UM_length dist;             /* distance between the two points */

/*------------------------------------------------------------------------
**  Start of Executable Code
**------------------------------------------------------------------------
**
**  block comment
*/
    dist = um_dcccc(cc1,cc2);
    /*
        sprintf(UM_sbuf,"um_cceqcc: cc1(%g,%g,%g)",cc1[0],cc1[1],cc1[2]);
        um_pscroll(UM_sbuf);
        sprintf(UM_sbuf,"        cc2(%g,%g,%g)",cc2[0],cc2[1],cc2[2]);
        um_pscroll(UM_sbuf);
        sprintf(UM_sbuf,"        dist=%g",dist);
        um_pscroll(UM_sbuf);
    /* */
    if (dist < toler) return (UU_TRUE); else return (UU_FALSE);
    }


/*********************************************************************
**    I_FUNCTION     : um_cotocc(stype,sc,cc)
**      Convert a coordinate represented in either  CARTESIAN,
**	  	  CYLINDRICAL, or SPHERICAL  coordinates into an internal 
**      CARTESIAN  coordinate system.
**    PARAMETERS   
**       INPUT  : 
**				stype						 type of input coordinate
**				sc                    input coordinate
**       OUTPUT :  
**				cc                    internal  CARTESIAN  coordinate
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_cotocc(stype,sc,cc)
	int stype;
	UM_coord sc;
	UM_coord cc;

	{
	int i;

	switch (stype)
		{
		case  UM_CARTESIAN:
			for (i = 0; i < 3; i++) cc[i]  =  sc[i];
			break;
		case  UM_CYLINDRICAL:
			cc[0] = sc[UM_CYL_RAD]  * cos(sc[UM_CYL_AZ]);
			cc[1] = sc[UM_CYL_RAD]  * sin(sc[UM_CYL_AZ]);
			cc[2] = sc[UM_CYL_Z];
			break;
		case  UM_SPHERICAL:
			cc[0] = sc[UM_SPH_RAD]  * cos(sc[UM_SPH_AZ])  * sin(sc[UM_SPH_EL]);
			cc[1] = sc[UM_SPH_RAD]  * sin(sc[UM_SPH_AZ])  * sin(sc[UM_SPH_EL]);
			cc[2] = sc[UM_SPH_RAD]  * cos(sc[UM_SPH_EL]);
			break;
		default:;
			break;
		}
	}
/*********************************************************************
**    I_FUNCTION     : um_cctoco(cc,stype,sc)
**      Convert an internally represented CARTESIAN 
**			coordinate to the specified representation.
**    PARAMETERS   
**       INPUT  : 
**				cc        internal CARTESIAN  coordinate
**          stype     type to convert to
**       OUTPUT :  
**				sc        output  coordinate representation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_cctoco(cc,stype,sc)
	UM_coord cc;
	int stype;
	UM_coord sc;

	{
	int i;

	switch (stype)
		{
		case  UM_CARTESIAN:
			for (i = 0; i < 3; i++) sc[i]  =  cc[i];
			break;
		case  UM_CYLINDRICAL:
			sc[UM_CYL_RAD] = sqrt(cc[0] *cc[0] + cc[1] *cc[1]);
			sc[UM_CYL_AZ] = atan2(cc[1], cc[0]);
			sc[UM_CYL_Z] = cc[2];
			break;
		case  UM_SPHERICAL:
			sc[UM_SPH_RAD] = sqrt(cc[0] *cc[0] + cc[1] *cc[1] + cc[2] *cc[2]);
			if (sc[UM_SPH_RAD] < UM_FUZZ)
				{
				sc[UM_SPH_RAD] = 0.0;
				sc[UM_SPH_EL] = 0.0;
				sc[UM_SPH_AZ] = 0.0;
				}
			else
				{
				sc[UM_SPH_EL] = acos(cc[2] / sc[UM_SPH_RAD]);
				sc[UM_SPH_AZ] = atan2(cc[1], cc[0]);
				}
			break;
		default:;
			break;
		}
	}
/*********************************************************************
**    I_FUNCTION     : um_cotoco(typei,sci,typeo,sco)
**      Convert a coordinate to an equivalent representation 
**			in another  coordinate system.
**    PARAMETERS   
**       INPUT  : 
**				typei      representation of input
**          sci        coordinate in
**				typeo		  representation of output
**       OUTPUT :  
**				sco        coordinate out
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_cotoco(typei,sci,typeo,sco)
	int typei;
	UM_coord sci;
	int typeo;
	UM_coord sco;

	{
	UM_coord cc;		/* internal CARTESIAN  coordinate */
	int i;				/* index */

	if (typei == typeo)
		{
		for (i = 0; i < 3; i++) sco[i]  =  sci[i];
		}
	else
		{
		um_cotocc(typei, sci, cc);
		um_cctoco(cc, typeo, sco);
		}
	}
/*********************************************************************
**    I_FUNCTION     : um_vctoco(vc,type,co)
**      Convert an internally represented  CARTESIAN  coordinate to 
**      the specified  coordinate type representation.
**    PARAMETERS   
**       INPUT  : 
**				vc        internal CARTESIAN  coordinate
**          type      type to convert to
**       OUTPUT :  
**				co         coordinate representation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_vctoco(vc,type,co)
	UM_vector vc;
	int type;
	UM_coord co;

	{
   int i;

	if ((vc[0]==0.0)&&(vc[1]==0.0)&&(vc[2]==0.0))
	  {
		co[0] = 0;  co[1] = 0;  co[2] = 0;
	  }
	else
   switch (type)
      {
      case  UM_CARTESIAN:
      case  UM_VCARTESIAN:
         for (i = 0; i < 3; i++) co[i]  =  vc[i];
         break;
      case  UM_CYLINDRICAL:
      case  UM_VCYLINDRICAL:
         co[0] = sqrt(vc[0] * vc[0] + vc[1] * vc[1]);
         co[1] = atan2(vc[1], vc[0]);
         co[2] = vc[2];
         break;
      case  UM_SPHERICAL:
      case  UM_VSPHERICAL:
         co[0] = sqrt(vc[0] * vc[0] + vc[1] * vc[1] + vc[2] * vc[2]);
         co[1] = atan2(vc[1], vc[0]);
         co[2] = acos(vc[2] / co[0]);
         break;
      default:;
         break;
      }
   }
/*********************************************************************
**    E_FUNCTION     : UU_REAL um_dcccc(cc1,cc2)
**      Calculate the distance between two points represented in
**			the internal CARTESIAN coordinate format.
**    PARAMETERS   
**       INPUT  : 
**				cc1        first internal  coordinate
**          cc2        second internal  coordinate
**       OUTPUT :  
**    RETURNS      : 
**				dist		  distance between points
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL
um_dcccc(cc1,cc2)
	UM_coord cc1;
	UM_coord cc2;

	{
	UU_REAL temp;			/* temporary variable */
	UU_REAL dist;
	int i;					/* index */

	dist = 0.0;
	temp = 0.0;
	for (i = 0; i < 3; i++) 
		{
		temp = cc1[i] - cc2[i];
		dist  = dist + (temp * temp);
		}
	dist = sqrt( dist);
	return(dist);
	}

/*********************************************************************
**    E_FUNCTION     : UU_REAL um_dsupt(cc1,cc2)
**      Calculate the distance between surf. and point represented in
**          the internal CARTESIAN coordinate format.
**    PARAMETERS
**       INPUT  :
**              cc1        vect. & dist.    (x,y,z,dist)
**              cc2        point coordinate (a,b,c)
**       OUTPUT :
**    RETURNS      :
**              dist          distance 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_dsupt(cc1,cc2)
UU_REAL  *cc1;
UU_REAL  *cc2;
{
   UU_REAL cos_alpha, cos_betta, cos_gamma, lenth;

   lenth = sqrt(cc1[0]*cc1[0] + cc1[1]*cc1[1] + cc1[2]*cc1[2]);

   cos_alpha = cc1[0] / lenth;
   cos_betta = cc1[1] / lenth;
   cos_gamma = cc1[2] / lenth;

   return (fabs(cc2[0]*cos_alpha+cc2[1]*cos_betta+cc2[2]*cos_gamma - cc1[3]));
}


/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL um_vcparall_ch(cc1,cc2)
**      Checks if vectors cc1 & cc2 are parallel.
**      The original code of this routinre is in "model/m7mathvc.c"
**                                                  um_vcparall(...)
**    PARAMETERS
**       INPUT  :
**              cc1        vect. 1  (x,y,z)
**              cc1        vect. 2  (x,y,z)
**       OUTPUT :
**    RETURNS      :
**              dist          distance
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_vcparall_ch(cc1,cc2)
UU_REAL  *cc1;
UU_REAL  *cc2;
{

UU_REAL proj;

proj = um_dot(cc1,cc2);
if((1.0 - fabs(proj)) < toler) return (UU_TRUE); else return (UU_FALSE);

}

/*********************************************************************
**    E_FUNCTION     :	um_cctmtf( cci, tf, cco )
**       Multiply a cartesian coordinate by a transformation matrix
**			and return the result cco = cci * tf. The output coordinate
**			may be identical to the input.
**    PARAMETERS   
**       INPUT  : 
**				cci		coordinate
**				tf			transformation	-- if UM_DEFAULT_TF: identity
**       OUTPUT :  
**          cco		coordinate * transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
um_cctmtf( cci, tf, cco )
	UM_coord		cci;
	UM_transf	tf;
	UM_coord		cco;
	{
	int i,j;
	UM_coord temp;

	if (tf == UM_DEFAULT_TF)
		{
		for (i=0; i<3; i++) cco[i] = cci[i];
		}
	else
		{
		for (i=0; i<3; i++)
			{
			temp[i] = tf[3][i];
			for (j=0; j<3; j++) temp[i] = temp[i] + (cci[j] * tf[j][i]);
			}

		for (i=0; i<3; i++) cco[i] = temp[i];
		}
	}

/*********************************************************************
**    I_FUNCTION     : int um_ccnearcc(pt,npts,ptary)
**			Given an array of points  and a pick location (cartesian
**			model  coordinates), return the index of the closest
**			point to the pick location.
**			(model coordinate version of um_nearto_to_ploc)
**    PARAMETERS   
**       INPUT  : 
**				pt              given point (model coordinates)
**          npts            number of points in array
**          ptary           array of points (model  coordinate)
**       OUTPUT :  
**          none
**    RETURNS      : 
**			um_ccnearcc          index of closest point
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_ccnearcc(pt,npts,ptary)
	UM_coord pt;
	int npts;
	UM_coord ptary[];

	{
	int closest;						/* index of closest point */
	UM_length dist, mindist;			/* distance (minimum distance) to point */
	int i;								/* index */

	if (npts  ==  0) closest = -1;
	else
		{
		for (i = 0; i < npts; i++)
			{
			dist = um_dcccc(pt, ptary[i]);
			if ( (i == 0) || (dist < mindist) )
				{
				closest = i;
				mindist = dist;
				}
			}
		}
	return (closest);
	}

