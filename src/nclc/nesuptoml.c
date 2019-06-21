/*********************************************************************
**    NAME         :  nesuptoml.c
**       CONTAINS:
**
**			evstup
**			gtclsd
**			gtrld
**			isitwf
**			sftype
**			uystup
**
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nesuptoml.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:53
*********************************************************************/

#include "usysdef.h"
#include "mfort.h"
#include "ncl.h"
#include "ycom.h"
#include "nclfc.h"
#include "nclxmdl.h"

extern int NCLX_internal_geom;
NCLX_mdl_plane autocompcs;

/*********************************************************************
**    FUNCTION  : int evstup (nclkey,isrf)
**       Buffer routine for OpenNCL library.
**    PARAMETERS
**       INPUT  :
**          see actual NCL routine.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int evstup(nclkey,isrf)
UM_int4 *nclkey;
UM_int2 *isrf;
{
	int status;
	if (!NCLX_internal_geom) status = evstup1(nclkey,isrf);
	else status = NclxMdlEvalSetup (*nclkey,*isrf);
	return(status);
}

/*********************************************************************
**    FUNCTION  : gtclsd (nclkey,iflg,iclosd)
**       Buffer routine for OpenNCL library.
**    PARAMETERS
**       INPUT  :
**          see actual NCL routine.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtclsd(nclkey,iflg,iclosd)
UM_int4 *nclkey;
UM_int2 *iflg,*iclosd;
{
	if (NCLX_internal_geom)
	{
		NclxMdlGetClosed(*nclkey,*iflg,iclosd);
	}
	else
		gtclsd1(nclkey,iflg,iclosd);
	return;
}

/*********************************************************************
**    E_FUNCTION     : gtrld(nclkey,irld)
**       Buffer routine for OpenNCL library.
**    PARAMETERS
**       INPUT  :
**          see actual NCL routine.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void gtrld(nclkey,irld)
UM_int4 *nclkey;
UM_int2 *irld;
{
	if (NCLX_internal_geom)
	{
		NclxMdlGetRuled(*nclkey,irld);
	}
	else
		gtrld1(nclkey,irld);
	return;
}

/*********************************************************************
**    E_FUNCTION     : int isitwf(nclkey,iret)
**       Buffer routine for OpenNCL library.
**    PARAMETERS
**       INPUT  :
**          see actual NCL routine.
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int isitwf(nclkey,iret)
UM_int4 *nclkey;
UM_int2 *iret;
{
	int status;
	if (NCLX_internal_geom)
	{
		*iret = 1;
		status = UU_SUCCESS;
	}
	else
		status = isitwf1 (nclkey,iret);

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int sftype(nclkey,ietype)
**       Buffer routine for OpenNCL library.
**    PARAMETERS
**       INPUT  :
**          see actual NCL routine.
**       OUTPUT :
**          none
**    RETURNS      :
**          see actual NCL routine.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int sftype (nclkey,ietype)
UM_int4 *nclkey;
UM_int2 *ietype;
{
	int status;
	if (NCLX_internal_geom)
	{
		NCLX_mdl_struct *geo;
		geo = 0;
		*ietype = 0;
		status = NclxMdlFindGeo(*nclkey,&geo);
		if (!geo || status != UU_SUCCESS) return (UU_FAILURE);

		if (geo->relnum == NCLX_MDL_NETSF) *ietype = NCLI_NETSF;
		else *ietype = NCLI_EVALSF;
	}
	else
		status = sftype1 (nclkey,ietype);

	return (status);
}
/*********************************************************************
**    FUNCTION  : int uystup (isrf,nclkey,ics,ncs)
**       Buffer routine for OpenNCL library.
**    PARAMETERS
**       INPUT  :
**	    isrf  : 	2 for ds, 3 for cs
**	    nclkey: 	key for ds/cs.
**	    ics	  : 	cs index.
**	    ncs	  :	number of cs.
**       OUTPUT :
**          UY_ds, UY_cs.
**    RETURNS      :
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uystup(isrf,nclkey,ics,ncs)
UM_int4 *nclkey;
UM_int2 *isrf,*ics,*ncs;
{
	int status;

	status = UU_SUCCESS;
	if (NCLX_internal_geom)
	{
		NCLX_mdl_struct *geo;

		status = NclxMdlFindGeo(*nclkey,&geo);
		if (status == UU_SUCCESS)
		{
			if (*isrf ==2)
				UY_ds = (NCLX_mdl_struct *)geo;
			else if (*isrf == 3)
			{
				if(*ics == *ncs)
				{
				 	UY_ncs = *ncs;
					UY_ics = 0;
				}
				UY_cs[*ics-1] = (NCLX_mdl_struct *)geo;
			}
		}
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int uycspl(nclkey,buf)
**       Create a (local) NCLX plane - not in Unibase
**    PARAMETERS
**       INPUT  :
**          buf - the plane data
**       OUTPUT :
**          none
**    RETURNS      :
**          nclkey - the key of a created NCLX plane
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uycspl (nclkey,buf)
UM_real8 buf[];
UM_int4 *nclkey;
{
	if (NCLX_internal_geom)
	{
		int i,stat;
		UM_int4 lkey;


		autocompcs.header.key = 0;
		autocompcs.header.relnum = NCLX_MDL_PLANE;
		strncpy (autocompcs.header.label,"@UN    ",7);
		for (i = 0; i < 3; i++)
		{
			autocompcs.vec[i] = buf[i];
			autocompcs.pt[i] = buf[6+i];
		}
		autocompcs.dist = buf[3];
		lkey = 0;
		stat = NclxMdlGenKey(&lkey);
		if (stat == 0 && lkey > 0)
		{
			autocompcs.header.key = lkey;
			*nclkey = autocompcs.header.key;
		}
		return (stat);	
	}
	else
		return (-1);
}

/*********************************************************************
**    E_FUNCTION     : int uystcs (ics,ncs)
**       Buffer routine for OpenNCL library.
**       Set up local autocompcs plane as a Check Surface
**    PARAMETERS
**       INPUT  :
**          see actual NCL routine.
**       OUTPUT :
**          UY_cs
**    RETURNS      :
**          success / failure
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uystcs (ics,ncs)
UM_int2 *ics,*ncs;
{
	if (NCLX_internal_geom)
	{
		if (*ics == *ncs)
		{
		 	UY_ncs = *ncs;
			UY_ics = 0;
		}
		UY_cs[*ics-1] = (NCLX_mdl_struct *)&autocompcs;
		return (0);	
	}
	else
		return (-1);
}
