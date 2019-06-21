/*********************************************************************
**    NAME         :  ymotsup.c
**       CONTAINS:
**
**          NclxMotGetAutost
**          NclxMotGetAutost1
**          NclxMotGetContact
**          NclxMotGetCutter
**          NclxMotGetGougck
**          NclxMotGetGougck1
**          NclxMotGetTool
**          NclxMotGetFeedrate
**          NclxMotGetFillet
**          NclxMotGetFwd
**          NclxMotGetMaxang
**          NclxMotGetMaxdp
**          NclxMotGetNumpts
**          NclxMotGetThick
**          NclxMotGetTlaxis
**          NclxMotGetToler
**          NclxMotGetPsis
**          NclxMotGetPSCond
**          NclxMotGetFanInterp
**          NclxMotSetAutost
**          NclxMotSetAutost1
**          NclxMotSetContact
**          NclxMotSetCutter
**          NclxMotSetFeedrate
**          NclxMotSetGougck
**          NclxMotSetGougck1
**          NclxMotIndirp
**          NclxMotIndirv
**          NclxMotSetMaxang
**          NclxMotSetMaxdp
**          NclxMotSetNumpts
**          NclxMotSetSrfvct
**          NclxMotSetThick
**          NclxMotSetTlaxis
**          NclxMotSetToler
**          NclxMotSetFillet
**          NclxMotSetFanInterp
**
**    COPYRIGHT 1997 (c) Numerical Control Computer Sciences Inc.
**              All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       ymotsup.c , 26.2
**    DATE AND TIME OF LAST MODIFICATION
**       04/12/18 , 10:29:09
*********************************************************************/
#include "usysdef.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mfort.h"
#include "nccs.h"
#include "nclfile.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include "ycom.h"
#include "nclfc.h"

static int GET[]={0},SET[]={1};

/*********************************************************************
**    E_FUNCTION     : int NclxMotGetAutost (autost)
**       This function returns the current AUTOST mode.
**    PARAMETERS
**    INPUT  :
**       none
**    OUTPUT :
**       autost       AUTOST mode.
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotGetAutost (autost)
int *autost;
{
	int stat = 0;
/*
.....Debug
*/
	NclxDbgEnter ("NclxMotGetAutost (autost)");

/*
...Get auto start parameter from NCL.
*/
	yautst (autost, GET, &stat);

	NclxDbgPWords (1,"autost",*autost);
	NclxDbgExit ("NclxMotGetAutost",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : int NclxMotGetAutost1 (autost)
**			This function returns the current AUTOST parameters.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			autost       AUTOST mode.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMotGetAutost1 (autost,omit)
int *autost,*omit;
{
	int stat = 0;
/*
.....Debug
*/
	NclxDbgEnter ("NclxMotGetAutost (autost,omit)");
/*
...Get auto start parameter from NCL.
*/
	yautst1 (autost,omit, GET, &stat);

	NclxDbgPWords (1,"autost",*autost);
	NclxDbgPWords (1,"autost_omit",*omit);
	NclxDbgExit ("NclxMotGetAutost1",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetContact(icntct)
**			This function returns the current CONTCT mode.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			icntct       CONTCT mode.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotGetContact(icntct)
int *icntct;
{
	int stat;
/*
...  Get the CONTACT parameter from NCL
*/
	NclxDbgEnter ("NclxMotGetContact(icntct)");
	ycntct (icntct,GET,&stat);
/*
.....End of routine
*/
	NclxDbgPWords (1,"icntct",*icntct);
	NclxDbgExit ("NclxMotGetContact",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetCutter(cparm)
**			This function returns the current CUTTER definition.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			cparm        Cutter parameters.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotGetCutter(cparm)
NCLX_mot_cutter *cparm;
{
	double ccut[6];
	int ic = 0;
/*
.....Pass the CUTTER parameters onto NCL
*/
	NclxDbgEnter ("NclxMotGetCutter(cparm)");

	obcutr(ccut,&ic);
/*
.....Store cutter parameters
*/
	cparm->diameter = ccut[0];
	cparm->radius = ccut[1];
	cparm->height = ccut[2];
	cparm->side_angle = ccut[3];
	cparm->zheight = ccut[4];
	cparm->flat_angle = ccut[5];
/*
.....End of routine
*/
	NclxDbgPcutter (1,"cparm",cparm);
	NclxDbgExit ("NclxMotGetCutter",0);
	return 0;
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetGougck(gouge)
**			This function returns the current GOUGCK value.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			gouge        GOUGCK value.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotGetGougck(gouge)
int *gouge;
{
	int stat = 0;
/*
.....Pass the parameters from NCL
*/
	NclxDbgEnter ("NclxMotGetGougck(gouge)");
	ygchk(gouge,GET,&stat);
/*
.....End of routine
*/
	NclxDbgPInteger (1,"gouge",*gouge);
	NclxDbgExit ("NclxMotGetGougck",stat);
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : NclxMotGetGougck1(psgoug,dsgoug,csgoug)
**       This function returns the current GOUGCK value.
**    PARAMETERS
**    INPUT  :
**       none
**    OUTPUT :
**       psgoug       PS GOUGCK value.
**       dsgoug       DS GOUGCK value.
**       csgoug       CS GOUGCK value.
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMotGetGougck1(psgoug,dsgoug,csgoug)
int *psgoug,*dsgoug,*csgoug;
{
	int stat = 0;
/*
.....Pass the parameters from NCL
*/
	NclxDbgEnter ("NclxMotGetGougck1(psgoug,dsgoug,csgoug)");
	ygchk1(psgoug,dsgoug,csgoug,GET,&stat);
/*
.....End of routine
*/
	NclxDbgPInteger (1,"PS gouge",*psgoug);
	NclxDbgPInteger (1,"DS gouge",*dsgoug);
	NclxDbgPInteger (1,"CS gouge",*csgoug);
	NclxDbgExit ("NclxMotGetGougck",stat);
	return 0;
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetTool(pv)
**			This function returns the current tool position.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			pv           Current tool position and tool axis.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotGetTool(pv)
NCLX_mdl_pntvec *pv;
{
	double pt[6];
/*
.....Pass the parameters from NCL
*/
	NclxDbgEnter ("NclxMotGetTool(pv)");
	ymtool(pt);
	pv->pt[0] = pt[0];
	pv->pt[1] = pt[1];
	pv->pt[2] = pt[2];
	pv->vec[0] = pt[3];
	pv->vec[1] = pt[4];
	pv->vec[2] = pt[5];

	pv->header.key = 0;
	pv->header.relnum = NCLX_MDL_PNTVEC;
/*
.....End of routine
*/
	NclxDbgPdata (1,"position",pv);
	NclxDbgEnter ("NclxMotGetTool(pv)",0);
	return 0;
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetFeedrate(feedrate)
**			This function gets the current FEEDRATE mode.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			feedrate     FEEDRATE structure.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotGetFeedrate(feedrate)
NCLX_mot_feedrate *feedrate;
{
	int stat = 0, ivals[5];
	double dvals[6];
/*
...  Get the FEED RATE parameters from NCL
*/
	NclxDbgEnter ("NclxMotGetFeedrate(feedrate)");
	yfdrat (ivals,dvals,GET,&stat);
	feedrate->accel_flag        = ivals[0];
	feedrate->slowdown_flag     = ivals[1];
	feedrate->mode              = ivals[2];
	feedrate->accel_type        = ivals[3];
	feedrate->slowdown_type     = ivals[4];
	feedrate->base_feedrate     = dvals[0];
	feedrate->accel_dist        = dvals[1];
	feedrate->accel_feedrate    = dvals[2];
	feedrate->slowdown_dist     = dvals[3];
	feedrate->slowdown_feedrate = dvals[4];
	feedrate->height            = dvals[5];
/*
.....End of routine
*/
	NclxDbgPfedrat (1,"feedrate",feedrate);
	NclxDbgExit ("NclxMotGetFeedrate",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetFillet(fillet)
**			This function gets the current ARCSLP/FILLET settings.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			feedrate     FEEDRATE structure.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxMotGetFillet(fillet)
NCLX_mot_fillet *fillet;
{
	UM_real8 rval[10];
	UM_int2 ival[10];
/*
.....Get FILLET parameters
*/
	yfilet(ival,rval,GET);
/*
.....Define the fillet parameters
*/
	fillet->fedctl = ival[0];
	fillet->same = ival[1];
	fillet->combine = ival[2];
	fillet->rad = rval[0];
	fillet->tol = rval[1];
	fillet->fedrt = rval[2];
	fillet->fmax = rval[3];
	fillet->cdia = rval[4];
	fillet->maxang = rval[5];
	if (ival[3] == 1)
		fillet->direction = NCLX_TLRGT;
	else
		fillet->direction = NCLX_TLLFT;
	fillet->warn = ival[4];
	return;
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetFwd(vec)
**			This function returns the current forward direction.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			vec          Forward direction.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotGetFwd(vec)
double vec[3];
{
/*
.....Pass the parameters from NCL
*/
	NclxDbgEnter ("NclxMotGetFwd(fwdvec)");
	ymfwd(vec);
/*
.....End of routine
*/
	NclxDbgPVector (1,"fwdvec",vec);
	NclxDbgExit ("NclxMotGetFwd",0);
	return 0;
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetMaxang(ang)
**			This function returns the current MAXANG value.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			ang          MAXANG value.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotGetMaxang(ang)
double *ang;
{
	int stat = 0;
/*
.....Pass the parameters from NCL
*/
	NclxDbgEnter ("NclxMotGetMaxang(maxang)");
	ymaxan(ang,GET,&stat);
/*
.....End of routine
*/
	NclxDbgPDouble (1,"maxang",ang);
	NclxDbgExit ("NclxMotGetMaxang",stat);
	return 0;
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetMaxdp(maxdp)
**			This function returns the current MAXDP value.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			maxdp        MAXDP structure.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotGetMaxdp(maxdp)
NCLX_mot_maxdp *maxdp;
{
	double dd[2];
	int di[4],stat = 0;
/*
.....Pass the MAXDP parameters onto NCL
*/
	NclxDbgEnter ("NclxMotGetMaxdp(maxdp)");
	ymaxdp(di,dd,GET,&stat);
/*
.....Store MAXDP parameters
*/
	maxdp->min = dd[0];
	maxdp->max = dd[1];
	maxdp->mauto = di[0];
	maxdp->step = di[1];
	maxdp->attempts = di[2];
	maxdp->warn = di[3];
/*
.....End of routine
*/
	NclxDbgPmaxdp (1,"maxdp",maxdp);
	NclxDbgExit ("NclxMotGetMaxdp",stat);
	return 0;
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetNumpts(numpts)
**			This function returns the current NUMPTS value.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			numpts       NUMPTS value.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotGetNumpts(numpts)
int *numpts;
{
	int stat = 0;
/*
.....Pass the parameters from NCL
*/
	NclxDbgEnter ("NclxMotGetNumpts(numpts)");
	ynumpt(numpts,GET,&stat);
/*
.....End of routine
*/
	NclxDbgPInteger (1,"numpts",*numpts);
	NclxDbgExit ("NclxMotGetNumpts",stat);
	return 0;
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetThick(thick)
**			This function returns the current THICK value.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			thick        THICK structure.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotGetThick(thick)
NCLX_mot_thick *thick;
{
	int stat = 0;
/*
.....Changed size of dd from 3 to 7 to accommodate multiple
.....Thick statements.
*/
	double dd[7];
/*
.....Pass the THICK parameters from NCL
*/
	NclxDbgEnter ("NclxMotGetThick(thick)");
	ythick(dd,GET,&stat);
/*
.....Store THICK parameters
*/
	thick->ps = dd[0];
	thick->ds = dd[1];
	thick->cs = dd[2];
	thick->cs2= dd[3];
	thick->cs3= dd[4];
	thick->cs4= dd[5];
	thick->cs5= dd[6];
/*
.....End of routine
*/
	NclxDbgPthick (1,"thick",thick);
	NclxDbgExit ("NclxMotGetThick",stat);
	return 0;
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetTlaxis(tlaxis)
**			This function returns the current TLAXIS mode.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			tlaxis       TLAXIS structure.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotGetTlaxis(tlaxis)
NCLX_mot_tlaxis *tlaxis;
{
	static int gcond[3]={NCLX_TLLFT,NCLX_TLON,NCLX_TLRGT};
	int stat = 0,ktl[30];
	double gtl[30];
	int i;
/*
.....Pass the TLAXIS parameters from NCL
*/
	NclxDbgEnter ("NclxMotGetTlaxis(tlaxis)");

	for (i = 0; i < 30; i++)
	{
		ktl[i] = 0; gtl[i] = 0.;
	}

	tlaxis->mode = NCLX_MOT_TLAXIS_SAME; 
	tlaxis->normal = 0; 
	tlaxis->vector[0] = 0.; 
	tlaxis->vector[1] = 0.; 
	tlaxis->vector[2] = 0.; 
	tlaxis->perpto_flag = 0; 
	tlaxis->perpto[0] = 0.; 
	tlaxis->perpto[1] = 0.; 
	tlaxis->perpto[2] = 0.; 
	tlaxis->angle = 0.; 
	tlaxis->contact = 0; 
	tlaxis->heel = 0.; 
	tlaxis->height = 0.;
	tlaxis->cmb_depart = 0.; 
	tlaxis->cmb_approach = 0.; 
	tlaxis->parelm = 0; 
	tlaxis->point[0] = 0.; 
	tlaxis->point[1] = 0.; 
	tlaxis->point[2] = 0.; 
	tlaxis->curve_dist = 0.;
	tlaxis->adjust_flag = 0;
	tlaxis->center = NCLX_TOOL_CENTER_OFF;
	tlaxis->modify.angle_flag = 0;
	tlaxis->modify.right_angle = 0.; 
	tlaxis->modify.fwd_angle = 0.; 
	tlaxis->modify.guide_flag = 0;
	tlaxis->modify.guide_contact = 0;
	tlaxis->modify.guide_cond = 0; 
	tlaxis->modify.guide_offset = 0.; 
	tlaxis->modify.secps_flag = 0; 
	tlaxis->modify.gouge = 0; 
	tlaxis->modify.lock_mode = NCLX_LOCK_OFF; 
	tlaxis->modify.lock_transition = 0; 
	tlaxis->modify.lock_radius = 0; 
	tlaxis->modify.lock_dist = 0.; 
	tlaxis->modify.lock_interp_dist = 0.; 
	tlaxis->adjust.right_offset	= 0.; 
	tlaxis->adjust.fwd_offset = 0.; 
	tlaxis->adjust.up_offset = 0.; 
	tlaxis->adjust.right_tilt = 0.; 
	tlaxis->adjust.fwd_tilt = 0.; 

	ytlaxs(ktl,gtl,GET,&stat);
/*
.....Store TLAXIS parameters
*/
	tlaxis->vector[0] = gtl[0];
	tlaxis->vector[1] = gtl[1];
	tlaxis->vector[2] = gtl[2];
/*
........FIXED
*/
	if (ktl[0] == 0)
	{
		tlaxis->mode = NCLX_MOT_TLAXIS_FIXED;
		tlaxis->normal = ktl[1];
	}
/*
.....NORMAL,PS
*/
	else if (ktl[0] == 1 || ktl[0] == 7)
	{
		tlaxis->mode = NCLX_MOT_TLAXIS_NORMAL;
		tlaxis->perpto_flag = 0;
	}
/*
.....ATANGL
*/
	else if (ktl[0] == 2 || ktl[0] == 10 || ktl[0] == 11 || ktl[0] == 12)
	{
		tlaxis->mode = NCLX_MOT_TLAXIS_ATANGL;
		tlaxis->angle = gtl[6];
		tlaxis->contact = ktl[4];
		tlaxis->heel = gtl[7];
		tlaxis->perpto_flag = 0;
	}
/*
.....TANTO,DS
*/
	else if (ktl[0] == 3 || ktl[0] == 5 || ktl[0] == 6)
	{
		tlaxis->mode = NCLX_MOT_TLAXIS_TANTO;
		tlaxis->height = gtl[3];
		tlaxis->parelm = ktl[2];
		tlaxis->perpto_flag = 0;
	}
/*
.....FAN
*/
	else if (ktl[0] == 4)
	{
		tlaxis->mode = NCLX_MOT_TLAXIS_FAN;
		tlaxis->height = gtl[3];
	}
/*
.....COMBIN
*/
	else if (ktl[0] == 8 || ktl[0] == 9)
	{
		tlaxis->mode = NCLX_MOT_TLAXIS_COMBIN;
		tlaxis->height = gtl[3];
		tlaxis->parelm = ktl[2];
		tlaxis->cmb_depart = gtl[4];
		tlaxis->cmb_approach = gtl[5];
	}
/*
.....THRU,POINT
*/
	else if (ktl[0] == 13)
	{
		tlaxis->mode = NCLX_MOT_TLAXIS_POINT;
		tlaxis->point[0] = gtl[11];
		tlaxis->point[1] = gtl[12];
		tlaxis->point[2] = gtl[13];
	}
/*
.....THRU,CURVE
*/
	else if (ktl[0] == 14)
	{
		if (!tlaxis->curve) return (NCLX_FAILURE);
		tlaxis->mode = NCLX_MOT_TLAXIS_CURVE;
		tlaxis->curve_dist = gtl[14];
		tlaxis->curve->header.key = ktl[5];
		tlaxis->curve->header.relnum = (NCLX_mdl_type)ktl[6];
	}
/*
.....INTERP
*/
	else if (ktl[0] == 15)
	{
		tlaxis->mode = NCLX_MOT_TLAXIS_INTERP;
	}
/*
.....PERPTO,VECTOR
*/
	tlaxis->perpto_flag = 0;
	if (ktl[0] == 5 || ktl[0] == 7 || ktl[0] == 10 || ktl[0] == 12)
	{
		tlaxis->perpto_flag = 1;
		if (ktl[22] == 1) tlaxis->perpto_flag = 2;
		tlaxis->perpto[0] = gtl[8];
		tlaxis->perpto[1] = gtl[9];
		tlaxis->perpto[2] = gtl[10];
	}
/*
..... CENTER
*/
	tlaxis->center = NCLX_TOOL_CENTER_OFF;
	if (tlaxis->mode == NCLX_MOT_TLAXIS_FAN ||
			tlaxis->mode == NCLX_MOT_TLAXIS_COMBIN)
	{
		if (ktl[21] == 1) tlaxis->center = NCLX_TOOL_CENTER_ON;
		else if (ktl[21] == 2) tlaxis->center = NCLX_TOOL_CENTER_AUTO;
	}
/*
.....RIGHT-FWD
*/
	tlaxis->modify.angle_flag = ktl[14];
	if (tlaxis->modify.angle_flag == NCLX_TRUE)
	{
		(*tlaxis).modify.right_angle = gtl[15];
		(*tlaxis).modify.fwd_angle = gtl[16];
	}
/*
.....GUIDE,CURVE
*/
	tlaxis->modify.guide_flag = ktl[7];
	if (tlaxis->modify.guide_flag == NCLX_TRUE)
	{
		if (!tlaxis->modify.guide) return (NCLX_FAILURE);
		tlaxis->modify.guide->data.header.key = ktl[8];
		tlaxis->modify.guide->data.header.relnum = (NCLX_mdl_type)ktl[9];
		tlaxis->modify.guide_contact = ktl[10];
		tlaxis->modify.guide_cond = gcond[ktl[11] - 1];
		tlaxis->modify.guide_offset = gtl[17];
	}
/*
.....Secondary PS
*/
	tlaxis->modify.secps_flag = ktl[15];
	if (tlaxis->modify.secps_flag == NCLX_TRUE)
	{
		if (!tlaxis->modify.secps) return (NCLX_FAILURE);
		tlaxis->modify.secps->key = ktl[16];
		tlaxis->modify.secps->relnum = (NCLX_mdl_type)ktl[17];
	}
/*
.....GOUGCK
*/
	tlaxis->modify.gouge = ktl[12];
/*
.....LOCK
*/
	if (ktl[18] == 1)
		tlaxis->modify.lock_mode = NCLX_LOCK_OMIT;
	else if (ktl[18] == 2)
		tlaxis->modify.lock_mode = NCLX_LOCK_ON;
	else if (ktl[18] == 3)
		tlaxis->modify.lock_mode = NCLX_LOCK_END;
	else
		tlaxis->modify.lock_mode = NCLX_LOCK_OFF;
	tlaxis->modify.lock_transition  = ktl[19];
	tlaxis->modify.lock_radius      = ktl[20];
	tlaxis->modify.lock_dist        = gtl[24];
	tlaxis->modify.lock_interp_dist = gtl[25];
/*
.....MODIFY
*/
	tlaxis->adjust_flag = ktl[13];
	tlaxis->adjust.right_offset = gtl[18];
	tlaxis->adjust.fwd_offset = gtl[19];
	tlaxis->adjust.up_offset = gtl[20];
	tlaxis->adjust.right_tilt = gtl[21];
	tlaxis->adjust.fwd_tilt = gtl[22];
/*
.....End of routine
*/
	NclxDbgPtlaxis (1,"tlaxis",tlaxis);
	NclxDbgExit ("NclxMotGetTlaxis",stat);	

	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotGetToler(toler)
**			This function returns the current TOLER value.
**		PARAMETERS
**		INPUT  :
**			none
**		OUTPUT :
**			toler        TOLER structure.
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotGetToler(toler)
NCLX_mot_toler *toler;
{
	int stat = 0;
	double dd[5];
/*
.....Pass the TOLER parameters from NCL
*/
	NclxDbgEnter ("NclxMotGetToler(toler)");	
	ytoler(dd,GET,&stat);
/*
.....Store tolerance parameters
*/
	toler->chordal = dd[0];
	toler->dsps = dd[1];
	toler->cs = dd[2];
/* FIXXES */
	toler->start_pt = dd[3];
	toler->start_cos = dd[4];
/*
.....End of routine
*/
	NclxDbgPtoler (1,"toler",toler);
	NclxDbgExit ("NclxMotGetToler",stat);	
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMotGetPsis(nclkey,gpl)
**       This function returns the current part surface.
**    PARAMETERS
**    INPUT  :
**       none
**    OUTPUT :
**       nclkey       - Part surface key.  If set to 0, then the part
**                      surface is a plane.
**
**       gpl          - Planar part surface parameters when key=0.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotGetPsis(nclkey,gpl)
UU_KEY_ID *nclkey;
UU_REAL gpl[];
{
	int i,stat = 0;
	NclxDbgEnter ("NclxMotGetPsis(nclkey,gpl)");
/*
.....Pass the parameters from NCL
*/
	ypsis(nclkey,gpl);

	NclxDbgPInteger(1,"PSIS",nclkey);
	NclxDbgPVector(1,"normal",gpl);
	NclxDbgPDouble(1,"distance",&gpl[3]);
	NclxDbgExit("NclxMotGetPSCond",NCLX_SUCCESS);
	return(NCLX_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : NclxMotGetPSCond (cond)
**       This function returns the current part surface condition.
**    PARAMETERS
**    INPUT  :
**       none
**    OUTPUT :
**       cond         - Part surface contition ,NCLX_TLONPS, NCLX_TLOFPS
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
NclxMotGetPSCond (cond)
NCLX_mot_pscond *cond;
{
	int i,stat = 0;
	NclxDbgEnter ("NclxMotGetPSCond(cond)");
/*
.....Pass the parameters from NCL
*/
	ypscnd(&i,GET,&stat);
	if (i==0) 
	  *cond = NCLX_TLOFPS;
	else
	  *cond = NCLX_TLONPS;

	NclxDbgPInteger (1,"PSCond",i);
	NclxDbgExit ("NclxMotGetPSCond",stat);
	return (stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotSetAutost (autost)
**       This function sets the current AUTOST mode.
**    PARAMETERS
**    INPUT  :
**       autost       AUTOST mode.
**    OUTPUT :
**       none
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotSetAutost (autost)
int *autost;
{
	int i, stat;
/*
...Set auto start parameter in NCL.
*/
	NclxDbgEnter ("NclxMotSetAutost (autost)");
	NclxDbgPWords (0,"autost",*autost);
	i = (*autost) ? 1 : 0;
	yautst (&i, SET, &stat);

	NclxDbgExit ("NclxMotSetAutost",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : int NclxMotSetAutost1 (autost)
**			This function sets the current AUTOST parameters.
**		PARAMETERS
**		INPUT  :
**			autoptr       AUTOST mode.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
int NclxMotSetAutost1 (autost,omit)
int *autost,*omit;
{
	int stat;
	int i,j;
/*
...Set auto start parameter in NCL.
*/
	NclxDbgEnter ("NclxMotSetAutost (autost,omit)");
	NclxDbgPWords (0,"autostart",*autost);
	NclxDbgPWords (0,"autostart_omit",*omit);
	i = (*autost) ? 1 : 0;
	j = (*omit) ? 1 : 0;
	yautst1 (&i,&j, SET, &stat);

	NclxDbgExit ("NclxMotSetAutost",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotSetContact(icntct)
**			This function sets the current CONTCT mode.
**		PARAMETERS
**		INPUT  :
**			icntct       CONTCT mode.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotSetContact(icntct)
int *icntct;
{
	int stat;
/*
.....Pass the CONTACT parameter to NCL
*/
	NclxDbgEnter ("NclxMotSetContact(icntct)");
	NclxDbgPWords (0,"icntct",*icntct);
	ycntct (icntct,SET,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotSetContact",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotSetCutter(cparm)
**			This function sets the current CUTTER parameters.
**		PARAMETERS
**		INPUT  :
**			cparm        CUTTER structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotSetCutter(cparm)
NCLX_mot_cutter *cparm;
{
	double dcut[6];
	int stat;
/*
.....Store cutter parameters
*/
	NclxDbgEnter ("NclxMotSetCutter(cparm)");	
	NclxDbgPcutter (0,"cparm",cparm);

	dcut[0] = cparm->diameter;
	dcut[1] = cparm->radius;
	dcut[2] = cparm->height;
	dcut[3] = cparm->side_angle;
	dcut[4] = cparm->zheight;
	dcut[5] = cparm->flat_angle;
/*
.....Pass the CUTTER parameters onto NCL
*/
	if (UY_clstart == 0) UY_clstart = (char *)UN_clpt[0];
	cutset(dcut,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotSetCutter",stat);	
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotSetFeedrate(feedrate)
**			This function sets the current FEEDRATE mode.
**		PARAMETERS
**		INPUT  :
**			feedrate     FEEDRATE structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotSetFeedrate(feedrate)
NCLX_mot_feedrate *feedrate;
{
	int stat, ivals[5];
	double dvals[6];
/*
.....Set the FEED RATE parameters for NCL
*/
	NclxDbgEnter ("NclxMotSetFeedrate(feedrate)");
	NclxDbgPfedrat (0,"feedrate",feedrate);
	ivals[0] = feedrate->accel_flag;
	ivals[1] = feedrate->slowdown_flag;
	dvals[0] = feedrate->base_feedrate;
	dvals[1] = feedrate->accel_dist;
	dvals[2] = feedrate->accel_feedrate;
	dvals[3] = feedrate->slowdown_dist;
	dvals[4] = feedrate->slowdown_feedrate;
	dvals[5] = feedrate->height;
	if (UY_clstart == 0) UY_clstart = (char *)UN_clpt[0];
	yfdrat (ivals,dvals,SET,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotSetFeedrate",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotSetGougck(gouge)
**			This function sets the current GOUGCK level.
**		PARAMETERS
**		INPUT  :
**			gouge        GOUGCK level.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotSetGougck(gouge)
int *gouge;
{
	int stat;
/*
.....Pass the parameters onto NCL
*/
	NclxDbgEnter ("NclxMotSetGougck(gouge)");	
	NclxDbgPInteger (0,"gouge",*gouge);
	
	ygchk(gouge,SET,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotSetGougck",stat);
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMotSetGougck1(psgoug,dsgoug,csgoug)
**       This function sets the current GOUGCK levels.
**    PARAMETERS
**    INPUT  :
**       psgoug       PS GOUGCK level.
**       dsgoug       DS GOUGCK level.
**       csgoug       CS GOUGCK level.
**    OUTPUT :
**       none
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
NclxMotSetGougck1(psgoug,dsgoug,csgoug)
int *psgoug,*dsgoug,*csgoug;
{
	int stat;
/*
.....Pass the parameters onto NCL
*/
	NclxDbgEnter ("NclxMotSetGougck1(psgoug,dsgoug,csgoug)");
	NclxDbgPInteger (0,"PS gouge",*psgoug);
	NclxDbgPInteger (0,"DS gouge",*dsgoug);
	NclxDbgPInteger (0,"CS gouge",*csgoug);
  
	ygchk1(psgoug,dsgoug,csgoug,SET,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotSetGougck",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotIndirp(pt)
**			This function sets the forward direction using a point.
**		PARAMETERS
**		INPUT  :
**			pt           Forward direction point.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotIndirp(pt)
double pt[3];
{
	int stat = 0;
/*
.....Pass the parameters onto NCL
*/
	NclxDbgEnter ("NclxMotIndirp(pt)");	
	NclxDbgPVector (0,"pt",pt);
	yindpt(pt,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotIndirp",stat);	
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotIndirv(vec)
**			This function sets the forward direction using a vector.
**		PARAMETERS
**		INPUT  :
**			vec          Forward direction vector.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotIndirv(vec)
double vec[3];
{
	int stat;
/*
.....Pass the parameters onto NCL
*/
	NclxDbgEnter ("NclxMotIndirv(vec)");	
	NclxDbgPVector (0,"vec",vec);
	yindvc(vec,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotIndirv",stat);	
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotSetMaxang(ang)
**			This function sets the MAXANG value.
**		PARAMETERS
**		INPUT  :
**			ang          MAXANG value.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotSetMaxang(ang)
double *ang;
{
	int stat;
/*
.....Pass the parameters onto NCL
*/
	NclxDbgEnter ("NclxMotSetMaxang(maxang)");
	NclxDbgPDouble (0,"maxang",ang);
	ymaxan(ang,SET,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotSetMaxang",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotSetMaxdp(maxdp)
**			This function sets the MAXDP values.
**		PARAMETERS
**		INPUT  :
**			maxdp        MAXDP structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotSetMaxdp(maxdp)
NCLX_mot_maxdp *maxdp;
{
	double dd[2];
	int di[3],stat;
/*
.....Store MAXDP parameters
*/
	NclxDbgEnter ("NclxMotSetMaxdp(maxdp)");
	NclxDbgPmaxdp (0,"maxdp",maxdp);
	dd[0] = maxdp->min;
	dd[1] = maxdp->max;
	di[0] = maxdp->mauto;
	di[1] = maxdp->step;
	di[2] = maxdp->attempts;
/*
.....Pass the MAXDP parameters onto NCL
*/
	ymaxdp(di,dd,SET,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotSetMaxdp",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotSetNumpts(numpts)
**			This function sets the NUMPTS value.
**		PARAMETERS
**		INPUT  :
**			numpts       NUMPTS value.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotSetNumpts(numpts)
int *numpts;
{
	int stat;
/*
.....Pass the parameters onto NCL
*/
	NclxDbgEnter ("NclxMotSetNumpts(numpts)");
	NclxDbgPInteger (0,"numpts",*numpts);
	ynumpt(numpts,SET,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotSetNumpts",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotSetSrfvct (srfvct)
**			This function sets the SRFVCT values.
**		PARAMETERS
**		INPUT  :
**			thick        SRFVCT structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotSetSrfvct (srfvct)
NCLX_mot_srfvct *srfvct;
{
	int stat;
	NclxDbgEnter ("NclxMotSetSrfvct (srfvct)");
	NclxDbgPsrfvct (0,"srfvct",srfvct);
	ysfvct (&srfvct->dsflag, srfvct->dsvec,
        &srfvct->csflag, srfvct->csvec, &stat);
	NclxDbgExit ("NclxMotSetSrfvct",stat);
	return(stat);
}
/*********************************************************************
**		E_FUNCTION     : NclxMotSetThick(thick)
**			This function sets the THICK values.
**		PARAMETERS
**		INPUT  :
**			thick        THICK structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotSetThick(thick)
NCLX_mot_thick *thick;
{
/*
.....Changed size of dd to 7 to accommodate multiple thick.
*/
	double dd[7];
	int stat;
/*
.....Store THICK parameters
*/
	NclxDbgEnter ("NclxMotSetThick(thick)");
	NclxDbgPthick (0,"thick",thick);
	dd[0] = thick->ps;
	dd[1] = thick->ds;
	dd[2] = thick->cs;
	dd[3] = thick->cs2;
	dd[4] = thick->cs3;
	dd[5] = thick->cs4;
	dd[6] = thick->cs5;
/*
.....Pass the THICK parameters onto NCL
*/
	ythick(dd,SET,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotSetThick",stat);
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotSetTlaxis(tlaxis)
**			This function sets the TLAXIS mode.
**		PARAMETERS
**		INPUT  :
**			tlaxis       TLAXIS structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotSetTlaxis(tlaxis)
NCLX_mot_tlaxis *tlaxis;
{
	int i,stat,ktl[30];
	double gtl[30];
/*
.....Initialize arrays
*/
	for (i=0;i<30;i++)
	{
		ktl[i] = 0;
		gtl[i] = 0.;
	}
/*
.....Store TLAXIS parameters
*/
	NclxDbgEnter ("NclxMotSetTlaxis(tlaxis)");	
	NclxDbgPtlaxis (0,"tlaxis",tlaxis);
	switch (tlaxis->mode)
	{
	case NCLX_MOT_TLAXIS_SAME:
		ktl[0] = -1;
		break;
	case NCLX_MOT_TLAXIS_FIXED:
		ktl[0] = 0;
		break;
	case NCLX_MOT_TLAXIS_NORMAL:
		ktl[0] = 1;
		if (tlaxis->perpto_flag != 0) ktl[0] = 7;
		if (tlaxis->perpto_flag == 2) ktl[22] = 1;
		break;
	case NCLX_MOT_TLAXIS_ATANGL:
		ktl[0] = 2;
		if (tlaxis->perpto_flag != 0) ktl[0] = 10;
		if (tlaxis->perpto_flag == 2) ktl[22] = 1;
		if (tlaxis->heel != 0.) ktl[0] = 11;
		if (tlaxis->heel != 0. && tlaxis->perpto_flag != 0) ktl[0] = 12;
		break;
	case NCLX_MOT_TLAXIS_TANTO:
		ktl[0] = 3;
		if (tlaxis->perpto_flag != 0) ktl[0] = 5;
		if (tlaxis->perpto_flag == 2) ktl[22] = 1;
		if (tlaxis->parelm == 1) ktl[0] = 6;
		break;
	case NCLX_MOT_TLAXIS_FAN:
		ktl[0] = 4;
		break;
	case NCLX_MOT_TLAXIS_COMBIN:
		ktl[0] = 8;
		if (tlaxis->parelm == 1) ktl[0] = 9;
		break;
	case NCLX_MOT_TLAXIS_POINT:
		ktl[0] = 13;
		break;
	case NCLX_MOT_TLAXIS_CURVE:
		ktl[0] = 14;
		break;
	case NCLX_MOT_TLAXIS_INTERP:
		ktl[0] = 15;
		break;
	}
/*
........Integer array
*/
	ktl[1] = tlaxis->normal;
	ktl[2] = tlaxis->parelm;
	ktl[3] = tlaxis->perpto_flag;
	ktl[4] = tlaxis->contact;
	if (ktl[0] == 14)
	{
		UY_sf4 = (NCLX_mdl_struct *)tlaxis->curve;
		ktl[5] = tlaxis->curve->header.key;
		ktl[6] = NclxMdlToNclType(tlaxis->curve->header.relnum);
	}
	ktl[7] = tlaxis->modify.guide_flag;
	UY_hldgeo = 0;
	gtl[23] = 0.0;
	if (ktl[7] == 1)
	{
		UY_hldgeo = (NCLX_mdl_struct *)tlaxis->modify.guide;
		ktl[8] = tlaxis->modify.guide->data.header.key;
		ktl[9] = NclxMdlToNclType(tlaxis->modify.guide->data.header.relnum);
		ktl[10] = tlaxis->modify.guide_contact;
		ktl[11] = 1;
		if (tlaxis->modify.guide_cond == NCLX_TLON) ktl[11] = 2;
		else if (tlaxis->modify.guide_cond == NCLX_TLRGT) ktl[11] = 3;
		NclxSetuv (UY_hldgeo, &gtl[23]);
		NclxDbgPdata (0,"guide_curve:",tlaxis->modify.guide);
	}
	ktl[12] = tlaxis->modify.gouge;
	ktl[13] = tlaxis->adjust_flag;
	ktl[14] = tlaxis->modify.angle_flag;

	ktl[15] = tlaxis->modify.secps_flag;
	if (ktl[15] == 1)
	{
		UY_secps = tlaxis->modify.secps;
		ktl[16] = tlaxis->modify.secps->key;
		ktl[17] = NclxMdlToNclType(tlaxis->modify.secps->relnum);
		NclxDbgPdata (0,"secondary PS:",tlaxis->modify.secps);
	}
/*
.....LOCK
*/
	ktl[18] = 0;
	if (tlaxis->modify.lock_mode == NCLX_LOCK_OMIT)
		ktl[18] = 1;
	else if (tlaxis->modify.lock_mode == NCLX_LOCK_ON)
		ktl[18] = 2;
	else if (tlaxis->modify.lock_mode == NCLX_LOCK_END)
		ktl[18] = 3;
	ktl[19] = tlaxis->modify.lock_transition;
	ktl[20] = tlaxis->modify.lock_radius;
/*
..... CENTER
*/
	ktl[21] = 0;
	if (tlaxis->mode == NCLX_MOT_TLAXIS_FAN ||
			tlaxis->mode == NCLX_MOT_TLAXIS_COMBIN)
	{
		if (tlaxis->center == NCLX_TOOL_CENTER_ON) ktl[21] = 1;
		if (tlaxis->center == NCLX_TOOL_CENTER_AUTO) ktl[21] = 2;
	}
/*
........Real array
*/
	gtl[0] = tlaxis->vector[0];
	gtl[1] = tlaxis->vector[1];
	gtl[2] = tlaxis->vector[2];
	gtl[3] = tlaxis->height;
	gtl[4] = tlaxis->cmb_depart;
	gtl[5] = tlaxis->cmb_approach;
	gtl[6] = tlaxis->angle;
	gtl[7] = tlaxis->heel;
	gtl[8] = tlaxis->perpto[0];
	gtl[9] = tlaxis->perpto[1];
	gtl[10] = tlaxis->perpto[2];
	gtl[11] = tlaxis->point[0];
	gtl[12] = tlaxis->point[1];
	gtl[13] = tlaxis->point[2];
	gtl[14] = tlaxis->curve_dist;
	gtl[15] = (*tlaxis).modify.right_angle;
	gtl[16] = (*tlaxis).modify.fwd_angle;
	gtl[17] = tlaxis->modify.guide_offset;
	gtl[18] = tlaxis->adjust.right_offset;
	gtl[19] = tlaxis->adjust.fwd_offset;
	gtl[20] = tlaxis->adjust.up_offset;
	gtl[21] = tlaxis->adjust.right_tilt;
	gtl[22] = tlaxis->adjust.fwd_tilt;
	gtl[24] = tlaxis->modify.lock_dist;
	gtl[25] = tlaxis->modify.lock_interp_dist;
/*
.....Pass the TLAXIS parameters to NCL
*/
	ytlaxs(ktl,gtl,SET,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotSetTlaxis",stat);	
	return(stat);
}

/*********************************************************************
**		E_FUNCTION     : NclxMotSetToler(toler)
**			This function sets the TOLER values.
**		PARAMETERS
**		INPUT  :
**			toler        TOLER structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
NclxMotSetToler(toler)
NCLX_mot_toler *toler;
{
	double dd[5];
	int stat;
/*
.....Store tolerance parameters
*/
	NclxDbgEnter ("NclxMotSetToler(toler)");	
	NclxDbgPtoler (0,"toler",toler);
	dd[0] = toler->chordal;
	dd[1] = toler->dsps;
	dd[2] = toler->cs;
/* FIXXES */
	dd[3] = toler->start_pt;
	dd[4] = toler->start_cos;
/*	dd[3] = toler->chordal * 2.;
	dd[4] = .9998;*/
/*
.....Pass the TOLER parameters onto NCL
*/
	ytoler(dd,SET,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotSetToler",stat);	
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : NclxMotSetPSCond (cond)
**       This function sets the current part surface condition.
**    PARAMETERS
**    INPUT  :
**       cond         - Part surface condition, NCLX_TLONPS or NCLX_TLOFPS
**    OUTPUT :
**       none
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotSetPSCond (cond)
NCLX_mot_pscond *cond;
{
	int i,stat = 0;
	NclxDbgEnter ("NclxMotSetPSCond(cond)");
/*
.....Pass the parameters from NCL
*/
	i = 0;
	if (*cond == NCLX_TLONPS) i = 1;
	ypscnd(&i,SET,&stat);

	NclxDbgPInteger (1,"PSCond",*cond);
	NclxDbgExit ("NclxMotSetPSCond",stat);
	return (stat);
}
/***************************************************
**
**     FUNCTION:  NclxMotSetFillet
**
**     PURPOSE:   Sets fillet variables.
**                Added 4/14/99 JLS
**
****************************************************/
void NclxMotSetFillet(fillet)
NCLX_mot_fillet *fillet;
{
	UM_real8 rval[10];
	UM_int2 ival[10];
/*
.....Define the fillet parameters
*/
	ival[0] = fillet->fedctl;
	ival[1] = fillet->same;
	ival[2] = fillet->combine;
	ival[4] = fillet->warn;
	rval[0] = fillet->rad;
	rval[1] = fillet->tol;
	rval[2] = fillet->fedrt;
	rval[3] = fillet->fmax;
	rval[4] = fillet->cdia;
	rval[5] = fillet->maxang;
/*
.....Right
*/
	if (fillet->direction == 1)
		ival[3] = 24;
/*
.....Left
*/
	else
		ival[3] = 8;
/*
.....Let Fortran know the settings
*/
	yfilet(ival,rval,SET);

	return;
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotGetFanInterp (deg,rate)
**       This function sets the current Fan Interpolation mode.
**    PARAMETERS
**    INPUT  :
**       none
**    OUTPUT :
**       deg        interpolation degree (should be 1,2,3,4,5)
**       rate       interpolation rate (should be positive and no more than 2)
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotGetFanInterp (deg,rate)
int *deg;
double *rate;
{
	UM_real4 rate4;
	UM_int2 deg2;
	int stat;
/*
...Set auto start parameter in NCL.
*/
	NclxDbgEnter ("NclxMotGetFanInterp (deg,rate)");
	
	yfnint (&deg2,&rate4, GET, &stat);
	*rate = rate4;
	*deg = deg2;

	NclxDbgPWords (1,"deg",*deg);
	NclxDbgPWords (1,"rate",*rate);

	NclxDbgExit ("NclxMotGetFanInterp",stat);
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotSetFanInterp (deg,rate)
**       This function sets the current Fan Interpolation mode.
**    PARAMETERS
**    INPUT  :
**       deg        interpolation degree (should be 1,2,3,4,5)
**       rate       interpolation rate (should be positive and no more than 2)
**    OUTPUT :
**       none
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotSetFanInterp (deg,rate)
int *deg;
double *rate;
{
	UM_real4 rate4;
	UM_int2 deg2;
	int stat;
/*
...Set auto start parameter in NCL.
*/
	NclxDbgEnter ("NclxMotSetFanInterp (deg,rate)");
	NclxDbgPWords (0,"deg",*deg);
	NclxDbgPWords (0,"rate",*rate);
	
	rate4 = *rate;
	deg2 = *deg;

	if (deg2 < 1)
	{
		deg2 = 0; rate4 = 0;
	}
	else if (rate4 < 0.0001 || rate4 > 1.)
		rate4 = 1;

	yfnint (&deg2,&rate4, SET, &stat);

	NclxDbgExit ("NclxMotSetFanInterp",stat);
	return(stat);
}
