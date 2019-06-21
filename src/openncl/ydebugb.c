/*********************************************************************
**    NAME         :  ydebugb.c
**       CONTAINS:
**
**				NclxDbgPdata
**				NclxDbgPpoint
**				NclxDbgPline
**				NclxDbgPpolyline
**				NclxDbgPplane
**				NclxDbgPpntvec
**				NclxDbgPcircle
**				NclxDbgPcurve
**				NclxDbgPcompos
**				NclxDbgPshape
**				NclxDbgPshpEnt
**				NclxDbgPsurf
**				NclxDbgPtrimsf
**				NclxDbgPcmphead
**				NclxDbgPsfhead
**				NclxDbgPcmpEnt
**				NclxDbgPcvhead
**				NclxDbgPInteger
**				NclxDbgPDouble
**				NclxDbgPVector
**				NclxDbgPWords
**				NclxDbgPrmName
**				NclxDbgPrmNameAd
**				NclxDbgPrmNameAd1
**				NclxDbgPpointer
**				NclxDbgPint
**				NclxDbgPstring
**				NclxDbgPreal
**				NclxDbgPfloat
**				NclxDbgP2real
**				NclxDbgPvec
**				NclxDbgPdblvec
**				NclxDbgPword
**				NclxDbgPenum
**				NclxDbgPstr
**				nclxostr
**
**    COPYRIGHT 1997 (c) Numerical Control Computer Sciences Inc.
**              All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       ydebugb.c, 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15, 15:10:58
*********************************************************************/
#include "usysdef.h"
#include "mdeval.h"
#include "mdrel.h"
#include "modef.h"
#include "mfort.h"
#include "nccs.h"
#include "nclfile.h"
#include "nclfc.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include "ycom.h"
#include "ydebug.h"

#if (UU_COMP == UU_SUN) || (UU_COMP == UU_IRIS4D)
#ifndef UU_RS6000
#define nclxdbgevaltime nclxdbgevaltime_
#endif
#endif

#define dlevel(stat) ( ((stat) < (2))? (2): (stat+1) )

extern int (*UY_dbout)();  /* write debug routine */
extern int UY_nclxdebug;
static int out_parm = 0;

void NclxDbgPpoint();
void NclxDbgPline();
void NclxDbgPpolyline();
void NclxDbgPplane();
void NclxDbgPpntvec();
void NclxDbgPcircle();
void NclxDbgPcurve();
void NclxDbgPcompos();
void NclxDbgPshpEnt();
void NclxDbgPnetsf();
void NclxDbgPsurf();
void NclxDbgPtrimsf();
void NclxDbgPcmphead();
void NclxDbgPsfhead();
void NclxDbgPcmpEnt();
void NclxDbgPcvhead();
void NclxDbgPrmName();
void NclxDbgPpointer();
void NclxDbgPint();
void NclxDbgPstring();
void NclxDbgPreal();
void NclxDbgPfloat();
void NclxDbgP2real();
void NclxDbgPvec();
void NclxDbgPword();
void NclxDbgPenum();
void NclxDbgPstr();

/*********************************************************************
**		E_FUNCTION     : NclxDbgPdata (iostat, name_prm, ptr)
**			This debug function outputs NCLX_mdl_data structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Geometry structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPdata (iostat, name_prm, ptr)
char *name_prm;
int iostat;
NCLX_mdl_data *ptr;
{
	int stat = dlevel(iostat);
	char *nilptr = "    -->Null pointer";
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		if (!ptr)
		{
			NclxDbgPstr (nilptr);
			return;
		}	
		switch (ptr->data.header.relnum)
		{
		case NCLX_MDL_POINT:
			NclxDbgPpoint (stat,"NCLX_mdl_point",&ptr->data);
			break;
		case NCLX_MDL_LINE:
			NclxDbgPline (stat,"NCLX_mdl_line",&ptr->data);
			break;
		case NCLX_MDL_PLANE:
			NclxDbgPplane (stat,"NCLX_mdl_plane",&ptr->data);
			break;
		case NCLX_MDL_PNTVEC:
			NclxDbgPpntvec (stat,"NCLX_mdl_pntvec",&ptr->data);
			break;
		case NCLX_MDL_CIRCLE:
			NclxDbgPcircle (stat,"NCLX_mdl_circle",&ptr->data);
			break;
/*
.....Add cases for Bsplines,Polylines and Nsurfs  JLS 11/4/99
*/
		case NCLX_MDL_BSPLINE:
			NclxDbgPcurve (stat,"NCLX_mdl_bspline",&ptr->data);
			break;
		case NCLX_MDL_CURVE:
			NclxDbgPcurve (stat,"NCLX_mdl_curve",&ptr->data);
			break;
		case NCLX_MDL_POLYLINE:
			NclxDbgPpolyline(stat,"NCLX_mdl_polyline",&ptr->data);
			break;
		case NCLX_MDL_COMPOSITE:
			NclxDbgPcompos (stat,"NCLX_mdl_composite",&ptr->data);
			break;
		case NCLX_MDL_SHAPE:
			NclxDbgPsurf (stat,"NCLX_mdl_shape",&ptr->data);
			break;
		case NCLX_MDL_NSURF:
			NclxDbgPsurf (stat,"NCLX_mdl_nsurf",&ptr->data);
			break;
		case NCLX_MDL_SURF:
			NclxDbgPsurf (stat,"NCLX_mdl_surf",&ptr->data);
			break;
		case NCLX_MDL_TRIMSF:
			NclxDbgPtrimsf (stat,"NCLX_mdl_trimsf",&ptr->data);
			break;
		case NCLX_MDL_NETSF:
			NclxDbgPnetsf (stat,"NCLX_mdl_netsf",&ptr->data);
			break;
		default:
			NclxDbgPrmName (stat,"UNDEFINED");
		}
	}
	return;
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPpoint (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_point struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Point structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPpoint (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_point *ptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPmdl (stat,"header",ptr);
		NclxDbgPvec (stat,"pt",ptr->pt);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPline (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_line struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Line structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPline (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_line *ptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPmdl (stat,"header",ptr);
		NclxDbgPvec (stat,"spt",ptr->spt);
		NclxDbgPvec (stat,"ept",ptr->ept);
	}
}
/*********************************************************************
**		E_FUNCTION     : NclxDbgPpolyline (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_polyline struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  polyline structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPpolyline (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_polyline *ptr;
{
	int i;
	int num;
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPmdl (stat,"header",ptr);
		NclxDbgPint (stat,"npts",ptr->npts);
		num = (ptr->npts) * 3;
		for (i=0;i<num;i=i+3)
		{
			NclxDbgPvec (stat,"pts",&ptr->pts[i]);
		}
	}
}


/*********************************************************************
**		E_FUNCTION     : NclxDbgPplane (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_plane struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Plane structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPplane (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_plane *ptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPmdl (stat,"header",ptr);
		NclxDbgPvec (stat,"pt",ptr->pt);
		NclxDbgPvec (stat,"vec",ptr->vec);
		NclxDbgPreal (stat,"dist",ptr->dist);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPpntvec (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_pntvec struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Point vector structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPpntvec (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_pntvec *ptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPmdl (stat,"header",ptr);
		NclxDbgPvec (stat,"pt",ptr->pt);
		NclxDbgPvec (stat,"vec",ptr->vec);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPcircle (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_circle struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Circle structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPcircle (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_circle *ptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPmdl (stat,"header",ptr);
		NclxDbgPvec (stat,"nvec",ptr->nvec);
		NclxDbgPvec (stat,"center",ptr->center);
		NclxDbgPreal (stat,"radius",ptr->radius);
		NclxDbgPvec (stat,"svec",ptr->svec);
		NclxDbgPreal (stat,"dang",ptr->dang);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPcurve (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_curve struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Curve structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPcurve (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_curve *ptr;
{
	int i;
	int NclxMdlEvalCurve();
	int *rtnnam=(int *)NclxMdlEvalCurve;
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPmdl (stat,"header",ptr);
		NclxDbgPcvhead (stat,"cvhead",&ptr->cvhead);
		if(ptr->evaluator == rtnnam)
		{
			NclxDbgPstring (stat,"evaluator","NclxMdlEvalCurve");
			NclxDbgPint (stat,"ntparm",ptr->ntparm);
			for (i=0;i<ptr->ntparm;i++)
				NclxDbgPreal (stat,"tparms",ptr->tparms[i]);
	
			NclxDbgPint (stat,"npt",ptr->npt);
			for (i=0;i<ptr->npt*3;i=i+3)
				NclxDbgPvec (stat,"pt",&ptr->pt[i]);
			
			NclxDbgPint (stat,"nwgt",ptr->nwgt);
			for (i=0;i<ptr->nwgt;i++)
				NclxDbgPreal(stat,"wgt",ptr->wgt[i]);
		}
		else
			NclxDbgPpointer (stat,"evaluator",ptr->evaluator);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPcompos (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_composite struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Composite Curve structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPcompos (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_composite *ptr;
{
	int i;
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPmdl (stat,"header",ptr);
		NclxDbgPcmphead (stat,"cvhead",&ptr->cvhead);
		NclxDbgPpointer (stat,"evaluator",ptr->evaluator);
		NclxDbgPint (stat,"ncurve",ptr->ncurve);
		for (i=0; i<ptr->ncurve; i++)
			NclxDbgPcmpEnt (stat,"cvid",&ptr->cvid[i]);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPshape (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_shape struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Shape structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPshape (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_shape *ptr;
{
	int i;
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPmdl (stat,"header",ptr);
		NclxDbgPenum (stat,"side",3,tlcond,ptr->side);
		NclxDbgPenum (stat,"dir",2,tldir,ptr->dir);
		NclxDbgPint (stat,"nents",ptr->nents);
		for (i=0; i<ptr->nents; i++)
			NclxDbgPshpEnt (stat,"shid",&ptr->shid[i]);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPshpEnt (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_sph_entity struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Shape structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPshpEnt (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_shp_entity *ptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPenum (stat,"type",5,shptyp,ptr->type);
		switch (ptr->type)
		{
		case NCLX_S_ENDPT:
			NclxDbgP2real (stat,"pt",ptr->pt);
			break;
		case NCLX_S_ARC:
			NclxDbgP2real (stat,"arc.cen",ptr->arc.cen);
			NclxDbgPreal (stat,"arc.rad",ptr->arc.rad);
			NclxDbgPreal (stat,"arc.sang",ptr->arc.sang);
			NclxDbgPreal (stat,"arc.eang",ptr->arc.eang);
			break;
		case NCLX_S_DIRECTION:
			NclxDbgPenum (stat,"dir",2,arcdir,ptr->dir);
			break;
		case NCLX_S_VOCAB:
			NclxDbgPint (stat,"vocab",ptr->vocab);
			break;
		case NCLX_S_VALUE:
			NclxDbgPreal (stat,"value",ptr->value);
			break;
		}
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPnetsf (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_netsf struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Net surface structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPnetsf (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_netsf *ptr;
{
	int i;
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPmdl (stat,"header",ptr);
		NclxDbgPint (stat,"nsf",ptr->nsf);
		for (i=0; i<ptr->nsf; i++)
			NclxDbgPdata (stat,"sfid",(NCLX_mdl_data *)ptr->sfptr[i]);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPmsfdat (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_surf struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Surface structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPmsfdat (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_data *ptr;
{
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		out_parm = 1;
		switch (ptr->data.header.relnum)
		{
/*JLS*/
		case NCLX_MDL_NSURF:
		case NCLX_MDL_SURF:
			NclxDbgPsurf(1,name_prm,&ptr->data);
			break;
		case NCLX_MDL_TRIMSF:
			NclxDbgPtrimsf(1,name_prm,&ptr->data);
			break;
		case NCLX_MDL_NETSF:
			NclxDbgPnetsf(1,name_prm,(NCLX_mdl_netsf *)&ptr->data);
			break;
		}
		out_parm = 0;
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPsurf (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_surf struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Surface structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPsurf (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_surf *ptr;
{
	int i;
	int stat = dlevel(iostat);
	int NclxMdlEvalSurf();
	int *rtnnam=(int *)NclxMdlEvalSurf;
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPmdl (stat,"header",ptr);
		if (out_parm == 0)
		{
			NclxDbgPsfhead (stat,"sfhead",&ptr->sfhead);
			switch (ptr->primitive)
			{
				case 0:
					NclxDbgPstring (stat,"primitive","Unknown");
					break;
				case 1:
					NclxDbgPstring (stat,"primitive","Freeform");
					break;
				case 2:
					if (ptr->prim_param[0] == 1)
						NclxDbgPstring (stat,"primitive","Ruled in u");
					else
						NclxDbgPstring (stat,"primitive","Ruled in v");
					break;
				case 3:
					NclxDbgPstring (stat,"primitive","Plane");
					NclxDbgPvec (stat,"pt",&ptr->prim_param[4]);
					NclxDbgPvec (stat,"normal",ptr->prim_param);
					NclxDbgPreal (stat,"dist",ptr->prim_param[3]);
					break;
				case 4:
					NclxDbgPstring (stat,"primitive","Sphere");
					NclxDbgPvec (stat,"center",ptr->prim_param);
					NclxDbgPreal (stat,"radius",ptr->prim_param[3]);
					break;
				case 5:
					NclxDbgPstring (stat,"primitive","Cylinder");
					NclxDbgPvec (stat,"base center",ptr->prim_param);
					NclxDbgPreal (stat,"base radius",ptr->prim_param[6]);
					NclxDbgPvec (stat,"axis",&ptr->prim_param[3]);
					NclxDbgPreal (stat,"height",ptr->prim_param[7]);
					break;
				case 6:
					NclxDbgPstring (stat,"primitive","Cone");
					NclxDbgPvec (stat,"apex",ptr->prim_param);
					NclxDbgPreal (stat,"angle",ptr->prim_param[6]*UM_RADIAN);
					NclxDbgPvec (stat,"axis",&ptr->prim_param[3]);
					NclxDbgPreal (stat,"height",ptr->prim_param[7]);
					break;
				default:
					break;
			}

			if (ptr->evaluator == rtnnam)
			{
				NclxDbgPstring (stat,"evaluator","NclxMdlEvalSurf");
				NclxDbgPint (stat,"ntu",ptr->ntu);
				for (i=0;i<ptr->ntu;i++)
					NclxDbgPreal (stat,"tu",ptr->tu[i]);
		
				NclxDbgPint (stat,"ntv",ptr->ntv);
				for (i=0;i<ptr->ntv;i++)
					NclxDbgPreal(stat,"tv",ptr->tv[i]);
	
				NclxDbgPint (stat,"npt",ptr->npt);
				for (i=0;i<ptr->npt*3;i=i+3)
					NclxDbgPvec (stat,"pt",&ptr->pt[i]);
	
				NclxDbgPint (stat,"nwgt",ptr->nwgt);
				for (i=0;i<ptr->nwgt;i++)
					NclxDbgPreal(stat,"wgt",ptr->wgt[i]);
			}
			else
				NclxDbgPpointer (stat,"evaluator",ptr->evaluator);
		}
		NclxDbgPint (stat,"no_boxlst",ptr->no_boxlst);
		NclxDbgPpointer (stat,"boxlst",ptr->boxlst);
		NclxDbgPint (stat,"no_bndrylst",ptr->no_bndrylst);
		NclxDbgPpointer (stat,"bndrylst",ptr->bndrylst);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPtrimsf (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_trimsf struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Trimmed Surface structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPtrimsf (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_trimsf *ptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPmdl (stat,"header",ptr);
		if (out_parm == 0)
		{
			NclxDbgPdata (stat,"surf",(NCLX_mdl_data *)ptr->surf);
			NclxDbgPdata (stat,"uv_cv",(NCLX_mdl_data *)ptr->uv_cv);
/*
.......Inner boudary not used at this time

			NclxDbgPint (stat,"ncurve",ptr->ncurve);
			for (i=0; i<ptr->ncurve; i+=2)
				NclxDbgPdata (stat,"inner",ptr->inner[i]);
*/
			NclxDbgPreal (stat,"u_min",ptr->u_min);
			NclxDbgPreal (stat,"u_max",ptr->u_max);
			NclxDbgPreal (stat,"v_min",ptr->v_min);
			NclxDbgPreal (stat,"v_max",ptr->v_max);
			NclxDbgPenum (stat,"trim_type",2,trimtype,ptr->trim_type);
		}
		NclxDbgPint (stat,"no_boxlst",ptr->no_boxlst);
		NclxDbgPpointer (stat,"boxlst",ptr->boxlst);
		NclxDbgPint (stat,"no_bndrylst",ptr->no_bndrylst);
		NclxDbgPpointer (stat,"bndrylst",ptr->bndrylst);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPcmphead (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_cmphead struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Composite curve header structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPcmphead (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_cmphead *ptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPword (stat,"closed",ptr->closed);
		NclxDbgPreal (stat,"length",ptr->length);
		NclxDbgPreal (stat,"eval",ptr->eval);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPsfhead (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_sfhead struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Surface header structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPsfhead (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_sfhead *ptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPword (stat,"offset",ptr->offset);
		NclxDbgPword (stat,"uclosed",ptr->uclosed);
		NclxDbgPword (stat,"vclosed",ptr->vclosed);
		NclxDbgPfloat (stat,"offdist",ptr->offdist);
		NclxDbgPreal (stat,"eval[0]",ptr->eval[0]);
		NclxDbgPreal (stat,"eval[1]",ptr->eval[1]);
		NclxDbgPint(stat,"udegree",ptr->udegree);
		NclxDbgPint(stat,"vdegree",ptr->vdegree);
		NclxDbgPint(stat,"udegseg",ptr->udegseg);
		NclxDbgPint(stat,"vdegseg",ptr->vdegseg);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPcmpEnt (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_cmp_entity struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Curve structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPcmpEnt (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_cmp_entity *ptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPword (stat,"reverse",ptr->reverse);
		NclxDbgPreal (stat,"endparam",ptr->endparam);
		NclxDbgPdata (stat,"curve",(NCLX_mdl_data *)ptr->curve);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPcvhead (iostat,name_prm,ptr)
**			This function outputs NCLX_mdl_cvhead struct data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Curve header structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPcvhead (iostat,name_prm,ptr)
char *name_prm;
int iostat;
NCLX_mdl_cvhead *ptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPword (stat,"closed",ptr->closed);
		NclxDbgPreal (stat,"eval",ptr->eval);
		NclxDbgPint(stat,"degree",ptr->degree);
		NclxDbgPint(stat,"degseg",ptr->degseg);
		NclxDbgPreal(stat,"t0",ptr->t0);
		NclxDbgPreal(stat,"t1",ptr->t1);
	}
		
}

/************************************************************************
.....Following functions are wraps used to handle properly level of
.....debuging.  Generic functions should not be called in NclxMot...
.....functions to prevent unexpected debug output.
.....
*********************************************************************/

/*********************************************************************
**		E_FUNCTION     : NclxDbgPInteger (iostat,name_prm,ptr)
**			This function outputs integer data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Integer data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPInteger (iostat,name_prm,ptr)
char *name_prm;
int iostat;
int ptr;
{
	if (UY_nclxdebug & NCLX_DBG_PRM)
		NclxDbgPint (iostat,name_prm,ptr);
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPDouble (iostat,name_prm,ptr)
**			This function outputs double data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Double data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPDouble (iostat,name_prm,ptr)
char *name_prm;
int iostat;
double *ptr;
{
	if (UY_nclxdebug & NCLX_DBG_PRM)
		NclxDbgPreal (iostat,name_prm,*ptr);
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPVector (iostat,name_prm,ptr)
**			This function outputs vector data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Vector data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPVector (iostat,name_prm,ptr)
char *name_prm;
int iostat;
double *ptr;
{
	if (UY_nclxdebug & NCLX_DBG_PRM)
		NclxDbgPvec (iostat,name_prm,ptr);
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPWords (iostat,name_prm,ptr)
**			This function outputs vocabulary word data
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Vocabulary word data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPWords (iostat,name_prm,ptr)
char *name_prm;
int iostat;
int ptr;
{
	if (UY_nclxdebug & NCLX_DBG_PRM)
		NclxDbgPword (iostat,name_prm,ptr);
}

/***********************************************************************
                     Generic debug functions
.....Following functions support NclxDbg, format output buffer etc.  
.....
************************************************************************/

/*********************************************************************
**		E_FUNCTION     : NclxDbgPrmName (stat,name)
**			This function outputs debug line with in/out parameter name.
**		PARAMETERS
**		INPUT  :
**			stat                 0 = Input parameter, 1 = Output parameter.
**			name                 Parameter name of calling routine.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPrmName (stat,name)
char *name;
int stat;
{
	char buff[300];
	buff[0] = '\0';

	if (UY_nclxdebug > 1)
	{
		if (stat == 0)
			sprintf (buff,"Nclxi-->%s\n",name);
		else if (stat == 1)
			sprintf (buff,"Nclxo-->%s\n",name);
		else
		{
			strncat (buff,"    -----------------------",4+stat);
			sprintf (&buff[strlen(buff)],">%s\n",name);
		}

		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : int NclxDbgPrmNameAd (stat,name,subs,buff)
**			This function puts in/out parameter name in debug buffer
**			followed by = character.
**			The actual data should be furnished in calling function.
**		PARAMETERS
**		INPUT  :
**			stat                 0 = Input parameter, 1 = Output parameter.
**			name                 Parameter name of calling routine.
**			subs                 Parameter subscript.
**		OUTPUT :
**			buff                 Formatted buffer.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxDbgPrmNameAd (stat,name,subs,buff)
char *name, *buff;
int stat, subs;
{
	if (stat == 0)
		sprintf (buff,"Nclxi-->%s = ",name);
	else if (stat == 1)
		sprintf (buff,"Nclxo-->%s = ",name);
	else 
	{	
		strncat (buff,"    -----------------------",4+stat);
		if (subs < 0)
			sprintf (&buff[strlen(buff)],">%s = ",name);
		else
			sprintf (&buff[strlen(buff)],">%s(%d) = ",name,subs);
	}
	return (strlen(buff));
}

/*********************************************************************
**		E_FUNCTION     : int NclxDbgPrmNameAd1 (stat,name,buff)
**			This function puts in/out parameter name in debug buffer.
**			The actual data should be furnished in calling function,
**			no "=" sign is output for use with command alike output
**		PARAMETERS
**		INPUT  :
**			stat                 0 = Input parameter, 1 = Output parameter.
**			name                 Parameter name of calling routine.
**		OUTPUT :
**			buff                 Formatted buffer.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxDbgPrmNameAd1 (stat,name,buff)
char *name, *buff;
int stat;
{
	if (stat == 0)
		sprintf (buff,"Nclxi-->%s",name);
	else if (stat == 1)
		sprintf (buff,"Nclxo-->%s",name);
	else 
	{	
		strncat (buff,"    -----------------------",4+stat);
		sprintf (&buff[strlen(buff)],">%s",name);
	}
	return (strlen(buff));
}
/*********************************************************************
**		E_FUNCTION     : NclxDbgPpointer (iostat, name_prm, prm)
**			This function outputs pointer parameter as it is
**			preceded by its name.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Pointer data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPpointer (iostat, name_prm, prm)
char *name_prm;
int iostat;
char *prm;
{
	char buff[132];
	buff[0] = '\0';

	if (UY_nclxdebug > 1)
	{
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name_prm,-1,buff)],"%x\n",prm);  
		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPint (iostat, name_prm, prm)
**			This function outputs single integer parameter as it is
**			preceded by its name.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Integer data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPint (iostat, name_prm, prm)
char *name_prm;
int iostat;
int prm;
{
	char buff[132];
	buff[0] = '\0';

	if (UY_nclxdebug > 1)
	{
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name_prm,-1,buff)],"%d\n",prm);  
		UY_dbout (buff);
	}
}
/*********************************************************************
**		E_FUNCTION     : NclxDbgPstring (iostat, name_prm, prm)
**			This function outputs string  parameter as it is
**			preceded by its name.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  string data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPstring (iostat, name_prm, prm)
char *name_prm;
char *prm;
int iostat;
{
	char buff[132];
	buff[0] = '\0';

	if (UY_nclxdebug > 1)
	{
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name_prm,-1,buff)],"%s\n",prm);  
		UY_dbout (buff);
	}
}


/*********************************************************************
**		E_FUNCTION     : NclxDbgPreal (iostat, name_prm, prm)
**			This function outputs single double number as it is
**			preceded by its name.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Real data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPreal (iostat, name_prm, prm)
char *name_prm;
int iostat;
double prm;
{
	char buff[132];
	buff[0] = '\0';

	if (UY_nclxdebug > 1)
	{
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name_prm,-1,buff)],"%g\n",prm);  
		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPfloat (iostat, name_prm, prm)
**			This function outputs single real number as it is
**			preceded by its name.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Real data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPfloat (iostat, name_prm, prm)
char *name_prm;
int iostat;
float prm;
{
	char buff[132];

	buff[0] = '\0';
	if (UY_nclxdebug > 1)
	{
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name_prm,-1,buff)],"%g\n",prm);  
		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgP2real (iostat, name_prm, prm)
**			This function outputs 2D vector as it is
**			preceded by its name.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Vector data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgP2real (iostat, name_prm, prm)
char *name_prm;
int iostat;
double *prm;
{
	char buff[132];

	buff[0] = '\0';
	if (UY_nclxdebug > 1)
	{
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name_prm,-1,buff)],
					"%g, %g\n",prm[0],prm[1]);  
		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPvec (iostat, name_prm, prm)
**			This function outputs vector data
**			preceded by its name.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Vector data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPvec (iostat, name_prm, prm)
char *name_prm;
int iostat;
double *prm;
{
	char buff[132];

	buff[0] = '\0';

	if (UY_nclxdebug > 1)
	{
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name_prm,-1,buff)],
					"%g, %g, %g\n",prm[0],prm[1],prm[2]);  
		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPdblvec (iostat, name_prm, subs, prm)
**			This function outputs pntvec data as a sixplet
**			preceided by its name. Input data (prm) is array of 6 double.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			subs                 Parameter subscript.
**			prm                  Point Vector data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPdblvec (iostat, name_prm, subs, prm)
char *name_prm;
int iostat, subs;
double *prm;
{
	char buff[132];

	buff[0] = '\0';

	if (UY_nclxdebug > 1)
	{
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name_prm,subs,buff)],
					"%g, %g, %g, %g, %g, %g\n",
					prm[0],prm[1],prm[2],prm[3],prm[4],prm[5]);
		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPword (iostat,name,prm)
**			This function outputs misceleneous NCLX words
**			used as modifiers in motion commands
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name                 Parameter text string of calling routine.
**			prm                  NCLX word data.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPword (iostat,name,prm)
int iostat,prm;
char *name;
{
	int i;
	char buff[132];
	char cword[24];

	buff[0] = cword[0] = '\0';

	if (UY_nclxdebug > 1)
	{
		switch (prm)
		{
		case NCLX_ON:    i = 0; 
			break;
		case 72:         i = 1; 
			break;
		case 88:         i = 2; 
			break;
		case NCLX_TRUE:  i = 3; 
			break;
		case NCLX_FALSE: i = 4; 
			break;
		case NCLX_SCRUB: i = 5; 
			break;
		case NCLX_LACE: i = 6; 
			break;
		case NCLX_FIXED: i = 7; 
			break;
		case NCLX_SCALLOP: i = 8; 
			break;
		default:         i = -1; 
		}
		if (i < 0) 
			sprintf (cword,"%d",prm);
		else
			strcpy (cword,vocab[i]);
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name,-1,buff)],"%s\n",cword);
	
		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPenum (iostat,name,max,tenum,prm)
**			This function outputs misceleneous words/modifiers defined
**			using enumerators, Use corresponding array of names to these enums.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name                 Parameter text string of calling routine.
**			max                  Maximum size of enumeration.
**			enum                 Enumeration.
**			prm                  Enumeration value.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPenum (iostat,name,max,tenum,prm)
int iostat,prm,max;
char *name;
char *tenum[];
{
	char buff[132];
	char cword[24];

	buff[0] = cword[0] = '\0';

	if (UY_nclxdebug > 1)
	{
		if (prm < 0 || prm > max)  
			sprintf (cword,"%d",prm);
		else
			strcpy (cword,tenum[prm]);
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name,-1,buff)],"%s\n",cword);

		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPstr (sbuf)
**			This function outputs a string as is to the debug output file.
**		PARAMETERS
**		INPUT  :
**			sbuf               Character string to output.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPstr (sbuf)
char *sbuf;
{
	if (UY_nclxdebug > 0)
	{
		char tbuf[256];
		strcpy(tbuf,sbuf);
		strcat(tbuf,"\n");
		UY_dbout (tbuf);
	}
}

/*********************************************************************
**		E_FUNCTION     : nclxostr (sbuf)
**			Fortran callable routine to output a string as is to the debug
**			output file.
**		PARAMETERS
**		INPUT  :
**			sbuf               Character string to output.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : 'sbuf' must be in a NULL terminated byte string
**		               format.
**		WARNINGS     : none
*********************************************************************/
void nclxostr (sbuf)
char *sbuf;
{
	NclxDbgPstr(sbuf);
}
