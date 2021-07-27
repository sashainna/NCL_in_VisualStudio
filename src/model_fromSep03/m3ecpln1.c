/*********************************************************************
**    NAME         :  m3ecpln1.c
**       CONTAINS: basic routines to initialize construction plane
**							and convert between input units and model units
**			um_setunits(units)
**			um_setangunits(units)
**			um_init_cpln()
**			um_ccstomcs(option, ccs, wcs)
**			um_mcstoccs(option, mcs, ccs)
**   um_tf2_tranfpln(eptr,tranfmat,store)
**			UU_REAL um_length_conversion(unit)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ecpln1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:53
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdcoord.h"
#include "mdunits.h"
#include "modef.h"
#include "mdcpln.h"
#include "mdebug.h"
#include "nccs.h"

/*********************************************************************
**    E_FUNCTION     : um_setunits(units)
**       Set the current input units.
**    PARAMETERS   
**       INPUT  : 
**          units				new current input units
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_setunits(units)
	int units;
	{
	uu_denter( UU_MTRC,(us,"um_setunits(%d)",units));
	UM_cpln.length_unit = units;
	UM_cpln.length_to_cm =  UM_cpln.conv_factors[units];
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_setangunits(units)
**       Set the current angular input units.
**    PARAMETERS   
**       INPUT  : 
**          units				new current angular  input units
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_setangunits(units)
	int units;
	{
	uu_denter( UU_MTRC,(us,"um_setangunits(%d)",units));
	UM_cpln.angle_unit = units;
	if (units == UM_DEGR) UM_cpln.ang_to_radians = UM_TWOPI / 360.0;
	else UM_cpln.ang_to_radians = 1.0;
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : um_init_cpln()
**      Initialize the parameters defining the construction plane
**			to be identical with the model (world) coordinate system.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_init_cpln()

	{

	uu_denter( UU_MTRC,(us,"um_init_cpln()"));

	/* initialize the current working construction coordinate system */
	um_vctovc(UM_zerovec, UM_cpln.origin);
	um_vctovc(UM_xaxis, UM_cpln.xaxis);
	um_vctovc(UM_yaxis, UM_cpln.yaxis);
	um_vctovc(UM_zaxis, UM_cpln.zaxis);
/*
	UM_cpln.conv_factors[ UM_INCH] = 2.54;
	UM_cpln.conv_factors[ UM_FEET] = 30.48;
	UM_cpln.conv_factors[ UM_MILE] = 160934.4;
	UM_cpln.conv_factors[ UM_MM] = 0.1;
	UM_cpln.conv_factors[ UM_CM] = 1.0;
	UM_cpln.conv_factors[ UM_M] = 100.0;
	UM_cpln.conv_factors[ UM_KM] = 100000.0;
	UM_cpln.conv_factors[ UM_MIL] = 0.001;
	UM_cpln.zdepth = 0.0;
	UM_cpln.length_unit =  UM_CM;
	UM_cpln.length_to_cm = 1.0;
*/
	UM_cpln.conv_factors[ UM_INCH] = 1.0;
	UM_cpln.conv_factors[ UM_FEET] = 12.0;
	UM_cpln.conv_factors[ UM_MILE] = 63360.0;
	UM_cpln.conv_factors[ UM_MM] = 1.0 / 25.4;
	UM_cpln.conv_factors[ UM_CM] = 1.0 / 2.54;
	UM_cpln.conv_factors[ UM_M] = 100.0 / 2.54;
	UM_cpln.conv_factors[ UM_KM] = 100000.0 / 2.54;
	UM_cpln.conv_factors[ UM_MIL] = 1.0 / 2540.0;
	UM_cpln.zdepth = 0.0;
	UM_cpln.length_unit =  UM_INCH;
	UM_cpln.length_to_cm = 1.0;
	UM_cpln.angle_unit =  UM_DEGR;
	UM_cpln.ang_to_radians = UM_TWOPI / 360.0;
	UM_cpln.grid.dsegid = -1;
	UM_cpln.grid.vpid = -1;
	UM_cpln.grid.viewid = -1;
	UM_cpln.grid.disp = UU_FALSE;
	UM_cpln.grid.snap = UU_FALSE;

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_ccstomcs(option, ccs, wcs)
**			Convert a vector or cartesian  coordinate specified in the
**			current construction  coordinate system to a vector or
**			cartestian  coordinate specified relative to the modeling
**			coordinate system.
**    PARAMETERS   
**       INPUT  : 
**				option		0 => cartesian  coordinate;
**								1 => vector
**				ccs			input (construction  coordinate system)
**       OUTPUT :  
**				wcs			output (modeling coordinate system)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ccstomcs(option, ccs, mcs)
	int option;
	UM_coord  ccs;
	UM_coord  mcs;

	{
	UM_vector	temp;

	um_vctovc(ccs,temp);
	mcs[ UM_X] = (temp[ UM_X]*UM_cpln.xaxis[ UM_X] + temp[ UM_Y]*UM_cpln.yaxis[ UM_X] + temp[ UM_Z]*UM_cpln.zaxis[ UM_X] );
	mcs[ UM_Y] = (temp[ UM_X]*UM_cpln.xaxis[ UM_Y] + temp[ UM_Y]*UM_cpln.yaxis[ UM_Y] + temp[ UM_Z]*UM_cpln.zaxis[ UM_Y] );
	mcs[ UM_Z] = (temp[ UM_X]*UM_cpln.xaxis[ UM_Z] + temp[ UM_Y]*UM_cpln.yaxis[ UM_Z] + temp[ UM_Z]*UM_cpln.zaxis[ UM_Z] );
	if (option == 0) um_vcplvc(mcs,  UM_cpln.origin, mcs);
	}

/*********************************************************************
**    E_FUNCTION     : um_mcstoccs(option, mcs, ccs)
**			Convert a vector or cartesian  coordinate specified in the
**			modeling  coordinate system to a vector or cartesian  coordinate
**			specified relative to the current construction  coordinate
**			system.
**    PARAMETERS   
**       INPUT  : 
**				option			0 => cartesian  coordinate;
**									1 => vector;
**				mcs				input (modeling coordinate system)
**       OUTPUT :  
**				ccs				output (construction  coordinate system)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_mcstoccs(option, mcs, ccs)
	int option;
	UM_coord mcs;
	UM_coord ccs;

	{
	UM_vector temp;
   UU_REAL xmag, ymag, zmag, um_mag();

   xmag = um_mag (UM_cpln.xaxis);
   ymag = um_mag (UM_cpln.yaxis);
   zmag = um_mag (UM_cpln.zaxis);
   xmag *= xmag;
   ymag *= ymag;
   zmag *= zmag;

	if (option == 1) um_vctovc(mcs, temp); else um_vcmnvc(mcs,  UM_cpln.origin, temp);
	ccs[UM_X] = um_dot(temp, UM_cpln.xaxis) / xmag;
	ccs[UM_Y] = um_dot(temp, UM_cpln.yaxis) / ymag;
	ccs[UM_Z] = um_dot(temp, UM_cpln.zaxis) / zmag;
	}

/*********************************************************************
**    E_FUNCTION     : UU_REAL um_length_conversion(unit)
**       Return the conversion factor which will convert from the
**			current internal units to the specified UNITS.
**			(e.g. cm = internal_length * um_length_conversion(UM_CM))
**    PARAMETERS   
**       INPUT  : 
**          unit						one of UM_INCH, ...
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL
um_length_conversion(unit)
	int unit;

	{
	UU_REAL conv_factor;
	uu_denter(UU_MTRC,(us,"um_length_conversion(units=%d)", unit));
	if ((0 <= unit) && (unit <= 7))
		conv_factor = 1.0 / UM_cpln.conv_factors[unit];
	else
		conv_factor = 1.0;
	uu_dexit;
	return (conv_factor);
	}

/*********************************************************************
**    E_FUNCTION     : int um_tf2_tranfpln(eptr,tranfmat,store)
**   Transform NCL plane by applying the specified 4X3 transformation
**   and update UNIBASE iff store == UU_TRUE.
**    PARAMETERS
**       INPUT  :
**    eptr          pointer to the entity to be transformed
**          tranfmat      the 4x3 transformation matrix
**    store     TRUE iff UNIBASE is to be updated here.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tf2_tranfpln(eptr,tranfmat,store)
 struct NCL_nclpl_rec *eptr;
 UM_transf    tranfmat;
 UU_LOGICAL store;

 {

 uu_denter(UU_MTRC,(us,"um_tf2_tranfpln(key:%d,tfmat:%x,store:%d)",
     eptr->key, tranfmat,store));

   um_cctmtf(eptr->pt, tranfmat, eptr->pt);
   um_vctmtf(eptr->nvec, tranfmat, eptr->nvec);
   um_unitvc (eptr->nvec, eptr->nvec);

 if (store)
  um_update_geom(eptr, UM_DEFAULT_TF);
 uu_dexit;
 return (UU_SUCCESS);
 }

