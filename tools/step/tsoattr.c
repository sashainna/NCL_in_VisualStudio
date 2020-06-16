/*********************************************************************
**    NAME         :  tsoattr.c
**       CONTAINS:
**			  utp_out_identity_attr
**			  utp_get_identity_rec
**    COPYRIGHT 2014 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			tsoattr.c . 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 . 15:13:21
*********************************************************************/

#include "ustdio.h"
#include "usysdef.h"
#include "udebug.h"
#include "tiges.h"
#include "tioconv.h"
#include "mdcoord.h"
#include "nclver.h"
#include "tstep.h"
#include "xenv1.h"

static char Sbuf[81920];
static int Smxrec,Sunrec,Sidrec;

extern UM_int2 NCL_ubas_unit;

/*********************************************************************
**    E_FUNCTION :  utp_out_identity_attr()
**			Outputs the identity matrix and units records to the step file.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_out_identity_attr()
{
	int irec[4],jrec[2],trec;
	UU_REAL units,tol;
	char tbuf[80],ustr1[20],ustr2[20],astr1[20],astr2[20];
	UM_coord pt;
	UM_vector svec,nvec;
/*
.....Initialize routine
*/
	Smxrec = 0;
	Sunrec = 0;
/*
.....Output identiy matrix
*/
	pt[0] = pt[1] = pt[2] = 0.;
	nvec[0] = nvec[1] = 0.; nvec[1] = 1.;
	svec[0] = 1.; svec[1] = svec[2] = 0.;
	Smxrec = utp_out_ptaxis(pt,nvec,svec,UU_FALSE);
	Sidrec = Smxrec;
/*
.....Get units strings
*/
	utp_get_units_str(output_units,&units,ustr1,ustr2,astr1,astr2);
	gettol(&tol);
/*
.....Output UNITS records
*/
/*
.....Metric
*/
	if (output_units == 1)
	{
		strcpy(Sbuf,"(LENGTH_UNIT() NAMED_UNIT(*) SI_UNIT(.MILLI.,.METRE.));");
		irec[1] = utp_out_record(Sbuf);
	}
/*
.....Inches
*/
	else
	{
		strcpy(Sbuf,"(LENGTH_UNIT() NAMED_UNIT(*) SI_UNIT($,.METRE.));");
		trec = utp_out_record(Sbuf);
		ul_format_numbers(&units,1,STEP_ACY,UU_TRUE,tbuf);
		sprintf(Sbuf,"LENGTH_MEASURE_WITH_UNIT (LENGTH_MEASURE(%s), #%d);",
			tbuf,trec);
		jrec[0] = utp_out_record(Sbuf);

		strcpy(Sbuf,"DIMENSIONAL_EXPONENTS(1.0,0.0,0.0,0.0,0.0,0.0,0.0);");
		jrec[1] = utp_out_record(Sbuf);

		sprintf(Sbuf,"(CONVERSION_BASED_UNIT('%s',#%d) LENGTH_UNIT() NAMED_UNIT(#%d));",
			ustr1,jrec[0],jrec[1]);
		irec[1] = utp_out_record(Sbuf);
	}

	UIO_LEN_INTTOEXT(tol,tol);
		ul_format_numbers(&tol,1,STEP_ACY,UU_TRUE,tbuf);
	sprintf(Sbuf,"UNCERTAINTY_MEASURE_WITH_UNIT(LENGTH_MEASURE(%s), #%d, 'distance_accuracy_value', 'NONE');",tbuf,irec[1]);
	irec[0] = utp_out_record(Sbuf);
/*
.....Output NAMED_UNIT records
*/
	sprintf(Sbuf,"(NAMED_UNIT(*) PLANE_ANGLE_UNIT()  SI_UNIT($,%s));",
		astr1);
	irec[2] = utp_out_record(Sbuf);
	
	sprintf(Sbuf,"(NAMED_UNIT(*) SI_UNIT($,%s) SOLID_ANGLE_UNIT());",astr2);
	irec[3] = utp_out_record(Sbuf);
/*
.....Output GEOMETRIC_REPRESENTATION_CONTEXT record
*/
	strcpy(Sbuf,"(GEOMETRIC_REPRESENTATION_CONTEXT(3) ");
	sprintf(tbuf,"GLOBAL_UNCERTAINTY_ASSIGNED_CONTEXT((#%d)) ",irec[0]);
	strcat(Sbuf,tbuf);
	sprintf(tbuf,"GLOBAL_UNIT_ASSIGNED_CONTEXT((#%d,#%d,#%d)) ",irec[1],irec[2],
		irec[3]);
	strcat(Sbuf,tbuf);
	strcat(Sbuf,"REPRESENTATION_CONTEXT('NONE','WORKASPACE'));");
	Sunrec = utp_out_record(Sbuf);
}

/*********************************************************************
**    E_FUNCTION :  utp_set_identity_mx(tfmat)
**			Changes the active transformation matrix.
**    PARAMETERS   
**       INPUT  : 
**          tfmat   = Transformation matrix to output to STEP file and
**                    make the default.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_set_identity_mx(tfmat)
UM_transf tfmat;
{
	UM_coord pt;
	UM_vector svec,nvec;
/*
.....Output transformation matrix
*/
	pt[0] = pt[1] = pt[2] = 0.;
	nvec[0] = nvec[1] = 0.; nvec[1] = 1.;
	svec[0] = 1.; svec[1] = svec[2] = 0.;
	um_cctmtf(pt,tfmat,pt);
	um_vctmtf(nvec,tfmat,nvec);
	um_vctmtf(svec,tfmat,svec);
	Smxrec = utp_out_ptaxis(pt,nvec,svec,UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION :  utp_get_identity_rec(mxrec,unitrec)
**			Returns the record numbers of the identity matrix and units
**       records.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :
**          mxrec     = Record number of identity matrix output.
**          unitrec   = Record number of unit record output.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_get_identity_rec(mxrec,unitrec)
int *mxrec,*unitrec;
{
	*mxrec = Smxrec;
	*unitrec = Sunrec;
}

/*********************************************************************
**    E_FUNCTION :  utp_reset_identity_mx()
**			Resets the active transformation matrix to the identity matrix.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void utp_reset_identity_mx()
{
	Smxrec = Sidrec;
}
