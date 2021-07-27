/*********************************************************************
**    NAME         :  nclxcatia.c
**       CONTAINS:
**          NclxMdlStoreCatia()
**          print_cl()
**          NclxMotLibInit()
**          NclxMotLibClose()
**
**    COPYRIGHT 2009 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nclxcatia.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:22
*********************************************************************/

#include "usysdef.h"
#include "class.h"
#include "mattrddl.h"
#include "mdattr.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mfort.h"
#include "modef.h"
#include "mcrv.h"
#include "msrf.h"
#include "nccs.h"
#include "nclcmd.h"
#include "ncl.h"
#include "nclinp.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include "nclfc.h"

extern int NCLX_internal_geom;
static int Ssub=1;

/*********************************************************************
**    E_FUNCTION     : NclxMdlStoreCatia(rec)
**       Stores a Catia surface in the Unibase.
**    PARAMETERS
**       INPUT  :
**          rec               Surface record created by Catia.
**       OUTPUT :
**    RETURNS      :
**       NCLX_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMdlStoreCatia(rec)
NCLX_mdl_data *rec;
{
	int ts[10],status,isav,i;
	struct UC_entitydatabag e1;
/*
.....Setup trimmed surface entity
*/
	if (rec->data.header.relnum == NCLX_MDL_TRIMSF)
	{
		NCLX_mdl_trimsf *sf = (NCLX_mdl_trimsf *)rec;
		NclxMdlSetupData(sf,NCLX_MDL_TRIMSF,"CATSF",Ssub++);
		sf->surf->sfhead.upaths = 5;
		sf->surf->sfhead.vpaths = 5;
		sf->surf->sfhead.upts = 0;
		sf->surf->sfhead.vpts = 0;
		sf->surf->sfhead.material = 0;
		sf->surf->sfhead.shaded = 1;
		sf->surf->sfhead.lucency = 100;
		rec->data.header.attrib.color = NCLX_LT_BLUE;
		rec->data.header.attrib.displayable = 0;
	}
/*
.....Setup net surface entity
*/
	else if (rec->data.header.relnum == NCLX_MDL_NETSF)
	{
		NCLX_mdl_netsf *netsf = (NCLX_mdl_netsf *)rec;
		NCLX_mdl_trimsf *tsf = (NCLX_mdl_trimsf *)rec;
		NclxMdlSetupData(netsf,NCLX_MDL_NETSF,"CATSF",Ssub++);
		rec->data.header.attrib.color = NCLX_ORANGE;
		rec->data.header.attrib.displayable = 0;
		for (i=0;i<netsf->nsf;i++)
		{
			tsf = (NCLX_mdl_trimsf *)netsf->sfptr[i];
			NclxMdlSetupData(tsf,NCLX_MDL_TRIMSF,"@UN",0);
			tsf->surf->sfhead.upaths = 5;
			tsf->surf->sfhead.vpaths = 5;
			tsf->surf->sfhead.upts = 0;
			tsf->surf->sfhead.vpts = 0;
			tsf->surf->sfhead.material = 0;
			tsf->surf->sfhead.shaded = 1;
			tsf->surf->sfhead.lucency = 100;
			tsf->header.attrib.color = NCLX_ORANGE;
			tsf->header.attrib.displayable = 1;
		}
	}
/*
.....Setup composite curve entity
*/
	else if (rec->data.header.relnum == NCLX_MDL_COMPOSITE)
	{
		NCLX_mdl_composite *cv = (NCLX_mdl_composite *)rec;
		NclxMdlSetupData(cv,NCLX_MDL_COMPOSITE,"CATCV",Ssub++);
		cv->cvhead.planar = UU_FALSE;
		cv->cvhead.fcolor = UM_BACKGROUND;
		rec->data.header.attrib.color = NCLX_BROWN;
		rec->data.header.attrib.displayable = 0;
	}
/*
.....Setup all other geometry types
*/
	else
	{
		NclxMdlSetupData(rec,rec->data.header.relnum,"CATGEO",Ssub++);
		rec->data.header.attrib.color = NCLX_PURPLE;
		rec->data.header.attrib.displayable = 0;
	}
/*
.....Store the geometry
*/
	status = NclxMdlStoreGeom(rec,NCLX_TRUE,ts);
/*
.....Display the entity
*/
	if (status == UU_SUCCESS)
	{
		e1.key = rec->data.header.key;
		status = ncl_retrieve_data(&e1,sizeof(e1));
		isav = NCLX_internal_geom; NCLX_internal_geom = 0;
		if (status == UU_SUCCESS) uc_display(&e1);
		NCLX_internal_geom = isav;
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : print_cl(clrec)
**       Dummy clprint routine used when running Catia test program
**       within NCL.
**    PARAMETERS
**       INPUT  :
**          clrec             Clfile record to print.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int print_cl(clrec)
NCLX_mot_clrec *clrec;
{return(NCLX_SUCCESS);}

/*********************************************************************
**    E_FUNCTION     : NclxMotLibInit(cpwd,units,version)
**       Dummy library initialization routine used when running Catia
**       test program within NCL.
**    PARAMETERS
**       INPUT  :
**       OUTPUT : none
**    RETURNS      : NCLX_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotLibInit(cpwd,units,version)
char *cpwd;
int units;
double version;
{
	UM_int2 idx,ival;
	NCL_cmdbuf cmdbuf;
/*
.....MULTAX/ON
*/
	ncl_init_cmdbuf(&cmdbuf);
	ncl_add_token(&cmdbuf,"MULTAX/ON",NCL_nocomma);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....UNITS/in-mm
.....In order to use programmed units
.....all geometry must first be converted to inches
*/
/*
	if (units == 0) inches();
	else millim();
*/
	return(NCLX_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : NclxMotLibClose()
**       Dummy library close routine used when running Catia
**       test program within NCL.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : NCLX_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotLibClose()
{return(NCLX_SUCCESS);}
