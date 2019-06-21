/*********************************************************************
**    NAME         :  ymotrtn.c
**       CONTAINS:
**				int NclxMotScrub (ps,srec,clrec)
**				int NclxMotRmill (ps,dsatt,ds1,ds2,csatt,cs1,cs2,rrec,clrec)
**				int NclxMotAdvPocket (pln,perimeter,endpt,loops,pocket,clrec)
**				int NclxMotLatheRough (stock,part,rough,clrec)
**				int NclxMotLatheFinish (part,finish,clrec)
**				void nclx_mot_postcmd (pcmd,irdat,pdat)
**    COPYRIGHT 1999 (c) Numerical Control Computer Sciences Inc.
**              All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       ymotrtn.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:00
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
#include <setjmp.h>

extern int NCLX_internal_geom;
extern int UY_debugDrive;
extern jmp_buf UU_jmpb;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
extern int ncall;
extern int etim;
UU_LIST lathelist;
void nclx_mot_postcmd();

/*********************************************************************
**    E_FUNCTION     : int NclxMotScrub (ps,srec,clrec)
**       This function processes a SCRUB command.
**    PARAMETERS
**    INPUT  :
**       ps           PS structure.
**       srec         Scrub parameters.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotScrub (ps,srec,clrec)
NCLX_mdl_struct *ps;
NCLX_mot_scrub *srec;
NCLX_mot_clrec *clrec;
{
	int i,stat,sdat[4],ix,pts[4],psgeom[4];
	int stims,stimm,etims,etimm;
/*
.....debug stuff
*/
	NclxDbgEnter ("NclxMotScrub (ps,srec,clrec)");
	NclxDbgPdata (0,"ps",ps);
	NclxDbgPscrub (0,"scrub",srec);
/*
.....All user to interrupt motion
*/
	ix = setjmp(UU_jmpb);
	if (ix != 0)
	{
		return(471);
	}
/*
.....Setup scrub data
*/
	sdat[0] = srec->numpass;
	sdat[1] = srec->numpts;
	sdat[2] = srec->bounded;
	if (sdat[2] == 1)
	{
		for (i=0;i<4;i++)
		{
			pts[i] = srec->boundary[i].header.key;
			UY_cs[i] = (NCLX_mdl_struct *)&(srec->boundary[i]);
		}
		UY_ncs = 4;
	}
/*
.....Setup ps data
*/
	UY_ps = ps;
	psgeom[0] = ps->key;
	psgeom[1] = NclxMdlToNclType(ps->relnum);
/*
.....Mark the start of the clfile record
*/
	if (UY_clstart != 0) clrec->start = (char *)UY_clstart;
	else clrec->start = (char *)UN_clpt[0];
	clrec->current = clrec->start;
/*
.....Initialize evaluator time
*/
	ncall = 0;
	etim = 0;
	gtimx(&stims,&stimm);
/*
.....Drive motion
*/
	NCLX_internal_geom = NCLX_TRUE;
	yfscrub (psgeom,sdat,pts,&stat);
	NCLX_internal_geom = NCLX_FALSE;
/*
.....Calculate evaluator time
*/
	gtimx(&etims,&etimm);
	stims = (etims-stims)*1000 + (etimm - stimm);
	NclxDbgEvalTime(&stims,&ncall,&etim);
/*
.....Mark the end of the clfile record
*/
	UY_clstart = 0;
	if (stat <= 0)
	{
		clrec->end = (char *)UN_clpt[0];
	}
	else
	{
		clrec->start = NULL;
		clrec->current = NULL;
		clrec->end = NULL;
	}
/*
.....End of routine
*/
	NclxDbgPclrec (1,"clrec",clrec);  
	NclxDbgExit ("NclxMotScrub",stat);
   return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotRmill (ps,dsatt,ds,csatt,cs,rrec,clrec)
**       This function processes a RMILL command.
**    PARAMETERS
**    INPUT  :
**       ps           PS structure.
**			dsatt        Drive surfaces condition (TO,ON,PAST)
**       ds           Drive surfaces.
**			csatt        Check surfaces condition (TO,ON,PAST)
**       cs           Check surfaces.
**       rrec         Rmill parameters.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotRmill (ps,dsatt,ds1,ds2,csatt,cs1,cs2,rrec,clrec)
NCLX_mdl_struct *ps,*ds1,*ds2,*cs1,*cs2;
NCLX_mot_rmill *rrec;
NCLX_mot_clrec *clrec;
NCLX_int dsatt[2],csatt[2];
{
	int i,stat,irdat[8],ix,psgeom[2],dsgeom[6],csgeom[6];
	int stims,stimm,etims,etimm;
	double rdat[16];
/*
.....debug stuff
*/
	NclxDbgEnter ("NclxMotRmill (ps,dsatt,ds1,ds2,,csatt,cs1,cs2,rrec,clrec)");
	NclxDbgPdata (0,"ps",ps);
	NclxDbgPcsat (0,"dsatt[0]",dsatt[0]);
	NclxDbgPdata (0,"ds1",ds1);
	NclxDbgPcsat (0,"dsatt[1]",dsatt[1]);
	NclxDbgPdata (0,"ds2",ds2);
	NclxDbgPcsat (0,"csatt[0]",csatt[0]);
	NclxDbgPdata (0,"cs1",cs1);
	NclxDbgPcsat (0,"csatt[1]",csatt[1]);
	NclxDbgPdata (0,"cs2",cs2);
	NclxDbgPrmill (0,"rmill",rrec);
/*
.....Allow user to interrupt motion
*/
	ix = setjmp(UU_jmpb);
	if (ix != 0)
	{
		return(471);
	}
/*
.....Setup rmill data
*/
	irdat[0] = 1;
	if (rrec->motion_type == NCLX_SCRUB) irdat[0] = 2;
	if (rrec->profile) irdat[0] = irdat[0] * -1;
	irdat[1] = 1;
	if (rrec->step_type == NCLX_SCALLOP) irdat[1] = 2;
	irdat[2] = rrec->clpl.header.key;
	irdat[3] = NclxMdlToNclType(rrec->clpl.header.relnum);
	rdat[0] = rrec->cldis;
	rdat[1] = rrec->pldis;
	rdat[2] = rrec->step_dis;
	rdat[3] = rrec->fed;
	rdat[4] = rrec->pfed;
	rdat[5] = rrec->plfed;
	rdat[6] = rrec->retdis;
	irdat[4] = rrec->retpl.header.key;
	irdat[5] = NclxMdlToNclType(rrec->retpl.header.relnum);
	irdat[6] = 0;
	UY_ncs = 3;
	if (rrec->clpl_type == NCLX_CLR_PLANE)
	{
		irdat[6] = 1;
		UY_cs[3] = (NCLX_mdl_struct *)&(rrec->clpl);
		UY_ncs = 4;
	}
	else
		UY_cs[3] = ds2;
	irdat[7] = 0;
	if (rrec->ret_type == NCLX_CLR_PLANE)
	{
		irdat[7] = 1;
		UY_cs[4] = (NCLX_mdl_struct *)&(rrec->retpl);
		UY_ncs = 5;
	}
	for (i=0;i<4;i++)
	{
		rdat[i+7] = rrec->rough_thick[i];
		rdat[i+11] = rrec->finish_thick[i];
	}
/*
.....Setup ps data
*/
	UY_ps = ps;
	psgeom[0] = ps->key;
	psgeom[1] = NclxMdlToNclType(ps->relnum);
	UY_nps = 1;
/*
.....Setup DS/CS data
*/
	UY_ds = ds1;
	dsgeom[0] = ds1->key;
	dsgeom[1] = NclxMdlToNclType(ds1->relnum);
	dsgeom[2] = dsatt[0];
	UY_cs[0] = ds2;
	dsgeom[3] = ds2->key;
	dsgeom[4] = NclxMdlToNclType(ds2->relnum);
	dsgeom[5] = dsatt[1];
	UY_cs[1] = cs1;
	csgeom[0] = cs1->key;
	csgeom[1] = NclxMdlToNclType(cs1->relnum);
	csgeom[2] = csatt[0];
	UY_cs[2] = cs2;
	csgeom[3] = cs2->key;
	csgeom[4] = NclxMdlToNclType(cs2->relnum);
	csgeom[5] = csatt[1];
/*
.....Mark the start of the clfile record
*/
	if (UY_clstart != 0) clrec->start = (char *)UY_clstart;
	else clrec->start = (char *)UN_clpt[0];
	clrec->current = clrec->start;
/*
.....Initialize evaluator time
*/
	ncall = 0;
	etim = 0;
	gtimx(&stims,&stimm);
/*
.....Drive motion
*/
	NCLX_internal_geom = NCLX_TRUE;
	yfrmill (psgeom,dsgeom,csgeom,irdat,rdat,&stat);
	NCLX_internal_geom = NCLX_FALSE;
/*
.....Calculate evaluator time
*/
	gtimx(&etims,&etimm);
	stims = (etims-stims)*1000 + (etimm - stimm);
	NclxDbgEvalTime(&stims,&ncall,&etim);
/*
.....Mark the end of the clfile record
*/
	UY_clstart = 0;
	if (stat <= 0)
	{
		clrec->end = (char *)UN_clpt[0];
	}
	else
	{
		clrec->start = NULL;
		clrec->current = NULL;
		clrec->end = NULL;
	}
/*
.....End of routine
*/
	NclxDbgPclrec (1,"clrec",clrec);  
	NclxDbgExit ("NclxMotRmill",stat);
   return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotAdvPocket (pln,boundary,endpt,loops,
**							        pocket,clrec)
**       This function processes a POCKET command.
**    PARAMETERS
**    INPUT  :
**       pln          Planar/surface geometry (top,bottom,clearance)
**			perimeter    Perimiter/Island geometry and attributes (IN,OUT,ON).
**       endpt        Optional ending point.
**       loops        Maximum number of loops around the pocket perimeter.
**       pocket       Pocketing structure.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotAdvPocket (pln,perimeter,endpt,loops,pocket,clrec)
NCLX_mot_advpocket_pl *pln;
NCLX_mot_advpocket_perim *perimeter;
NCLX_mdl_point *endpt;
NCLX_mot_advpocket *pocket;
int loops;
NCLX_mot_clrec *clrec;
{
	int i,stat,irdat[20],ix,psgeom[2],dsgeom[6];
	int stims,stimm,etims,etimm;
	int island[200],islatt[100];
	double rdat[16];
/*
.....debug stuff
*/
	NclxDbgEnter ("NclxMotAdvPocket (pln,perimeter,endpt,loops,pocket,clrec)");
	NclxDbgPadvpl (0,"pln",pln);
	NclxDbgPadvperim (0,"perimeter",perimeter);
	NclxDbgPdata (0,"endpt",endpt);
	NclxDbgPint (0,"loops",loops);
	NclxDbgPadvpocket (0,"pocket",pocket);
/*
.....Allow user to interrupt motion
*/
	ix = setjmp(UU_jmpb);
	if (ix != 0)
	{
		return(471);
	}
/*
.....Setup pocket data
*/
	irdat[0] = pocket->nramp;
	rdat[0] = pocket->ramp_dis;
	switch (pocket->entry)
	{
	case NCLX_RAMP: irdat[1] = 1; break;
	case NCLX_PLUNGE: irdat[1] = 2; break;
	case NCLX_HELIX:
		irdat[1] = 3;
		irdat[0] = pocket->nrev;
		rdat[0] = pocket->helix_rad;
		break;
	case NCLX_OMIT: irdat[1] = 4; break;
	}
	irdat[2] = pocket->retract;
	rdat[1] = pocket->level_dis;
	rdat[2] = pocket->retdis;
	switch (pocket->pocket_dir)
	{
	case NCLX_P_CLW: irdat[3] = 1; break;
	case NCLX_P_CCLW: irdat[3] = 2; break;
	}
	switch (pocket->spiral_dir)
	{
	case NCLX_P_OUT: irdat[4] = 1; break;
	case NCLX_P_IN: irdat[4] = 2; break;
	}
	irdat[5] = pocket->section_ret;
	switch (pocket->corner)
	{
	case NCLX_ARC: irdat[6] = 1; break;
	case NCLX_SHARP: irdat[6] = 2; break;
	}
	switch (pocket->warn)
	{
	case NCLX_NOWARN: irdat[7] = 1; break;
	case NCLX_AVOID: irdat[7] = -1; break;
	default: irdat[7] = 0; break;
	}
	rdat[12] = pocket->step_max;
	rdat[13] = pocket->step_min;
/*
.....Setup ps data
*/
	UY_ps = pln->ps;
	psgeom[0] = pln->ps->key;
	psgeom[1] = NclxMdlToNclType(pln->ps->relnum);
/*
.....Top plane
*/
	UY_ncs = 0;
	irdat[10] = 0;
	rdat[3] = pln->topdis;
	if (pln->top_type == NCLX_CLR_PLANE)
	{
		irdat[10] = 1;
		irdat[11] = pln->top.header.key;
		irdat[12] = NclxMdlToNclType(pln->top.header.relnum);
		UY_cs[UY_ncs] = (NCLX_mdl_struct *)&(pln->top); UY_ncs++;
	}
/*
.....Clearance Plane
*/
	irdat[13] = 0;
	rdat[4] = pln->cldis;
	if (pln->clpl_type == NCLX_CLR_PLANE)
	{
		irdat[13] = 1;
		irdat[14] = pln->clpl.header.key;
		irdat[15] = NclxMdlToNclType(pln->clpl.header.relnum);
		UY_cs[UY_ncs] = (NCLX_mdl_struct *)&(pln->clpl); UY_ncs++;
	}
	else if (pln->clpl_type == NCLX_CLR_INCR) irdat[13] = 2;
/*
.....Number of loops
*/
	irdat[14] = loops;
/*
.....Feedrates
*/
	irdat[16] = pocket->fedrat.secondary;
	rdat[5] = pocket->fedrat.angle;
	rdat[6] = pocket->fedrat.general;
	rdat[7] = pocket->fedrat.retract;
	rdat[8] = pocket->fedrat.position;
	rdat[9] = pocket->fedrat.entry;
	rdat[10] = pocket->fedrat.transition;
	rdat[11] = pocket->fedrat.finish;
/*
.....Pocket Perimeter
*/
	UY_ds = perimeter->perimeter;
	dsgeom[0] = perimeter->perimeter->key;
	dsgeom[1] = NclxMdlToNclType(perimeter->perimeter->relnum);
	dsgeom[2] = perimeter->peratt;
/*
.....Pocket Islands
*/
	dsgeom[3] = perimeter->num_islands;
	UY_nislands = perimeter->num_islands;
	for (i=0;i<perimeter->num_islands;i++)
	{
		islatt[i] = perimeter->islatt[i];
		island[i*2] = perimeter->island[i]->key;
		island[i*2+1] = NclxMdlToNclType(perimeter->island[i]->relnum);
		UY_island[i] = perimeter->island[i];
	}
/*
.....Optional endpoint
*/
	if (endpt != UU_NULL)
	{
		irdat[17] = 1;
		irdat[18] = endpt->header.key;
		irdat[19] = NclxMdlToNclType(endpt->header.relnum);
		UY_cs[UY_ncs] = (NCLX_mdl_struct *)endpt; UY_ncs++;
	}
	else irdat[17] = 0;
/*
.....Mark the start of the clfile record
*/
	if (UY_clstart != 0) clrec->start = (char *)UY_clstart;
	else clrec->start = (char *)UN_clpt[0];
	clrec->current = clrec->start;
/*
.....Initialize evaluator time
*/
	ncall = 0;
	etim = 0;
	gtimx(&stims,&stimm);
/*
.....Drive motion
*/
	NCLX_internal_geom = NCLX_TRUE;
	yfapok (psgeom,dsgeom,islatt,island,irdat,rdat,&stat);
	NCLX_internal_geom = NCLX_FALSE;
/*
.....Calculate evaluator time
*/
	gtimx(&etims,&etimm);
	stims = (etims-stims)*1000 + (etimm - stimm);
	NclxDbgEvalTime(&stims,&ncall,&etim);
/*
.....Mark the end of the clfile record
*/
	UY_clstart = 0;
	if (stat <= 0)
	{
		clrec->end = (char *)UN_clpt[0];
	}
	else
	{
		clrec->start = NULL;
		clrec->current = NULL;
		clrec->end = NULL;
	}
/*
.....End of routine
*/
	UY_nislands = 0;
	NclxDbgPclrec (1,"clrec",clrec);  
	NclxDbgExit ("NclxMotAdvPocket",stat);
   return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotLatheRough (stock,part,rough,clrec)
**       This function processes a LATHE/ROUGH command.
**    PARAMETERS
**    INPUT  :
**       stock        Shape which defines the rough stock.
**			part         Shape which defines the finished part.
**       rough        Lathe rough structure.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotLatheRough (stock,part,rough,clrec)
NCLX_mdl_shape *stock,*part;
NCLX_mot_lathe_rough *rough;
NCLX_mot_clrec *clrec;
{
	int stat,irdat[20],ix,psgeom[2],dsgeom[6];
	int stims,stimm,etims,etimm;
	double rdat[16],pcmd[300];
/*
.....debug stuff
*/
	NclxDbgEnter ("NclxMotLatheRough (stock,part,rough,clrec)");
	NclxDbgPdata (0,"stock",stock);
	NclxDbgPdata (0,"part",part);
	NclxDbgPlrough (0,"rough",rough);
/*
.....Allow user to interrupt motion
*/
	ix = setjmp(UU_jmpb);
	if (ix != 0)
	{
		return(471);
	}
/*
.....Initialize routine
*/
	UY_ncs = 0;
	uu_list_init (&lathelist,sizeof(UM_int2),1960,1960);
/*
.....Setup lathe data
*/
	rdat[0] = rough->cldist;
	rdat[1] = rough->stock_x;
	rdat[2] = rough->stock_y;
	nclx_mot_postcmd(rough->pcmd_depth,&irdat[0],&pcmd[0]);
	rdat[3] = rough->depth;
	nclx_mot_postcmd(rough->pcmd_cutang,&irdat[5],&pcmd[50]);
	rdat[4] = rough->cutang;
	nclx_mot_postcmd(rough->pcmd_retrct,&irdat[7],&pcmd[100]);
	rdat[5] = rough->retang;
	rdat[6] = rough->retdis;
/*
........Return logic
*/
	nclx_mot_postcmd(rough->pcmd_return,&irdat[9],&pcmd[150]);
	switch (rough->rettyp.type)
	{
	case NCLX_P_XAXIS: irdat[2] = 1; break;
	case NCLX_P_YAXIS: irdat[2] = 2; break;
	case NCLX_P_START: irdat[2] = 3; break;
	case NCLX_P_OFF: irdat[2] = 4; break;
	case NCLX_P_POINT:
			irdat[2] = 5;
			irdat[3] = rough->rettyp.pt->header.key;
			irdat[4] = NclxMdlToNclType(rough->rettyp.pt->header.relnum);
			UY_cs[UY_ncs] = (NCLX_mdl_struct *)rough->rettyp.pt; UY_ncs++;
			break;
	}
/*
........Final post command
*/
	nclx_mot_postcmd(rough->pcmd_final,&irdat[11],&pcmd[200]);
/*
.....Setup rough stock
*/
	UY_ps = (NCLX_mdl_struct *)stock;
	psgeom[0] = stock->header.key;
	psgeom[1] = NclxMdlToNclType(stock->header.relnum);
/*
.....Setup part shape
*/
	UY_ds = (NCLX_mdl_struct *)part;
	dsgeom[0] = part->header.key;
	dsgeom[1] = NclxMdlToNclType(part->header.relnum);
/*
.....Mark the start of the clfile record
*/
	if (UY_clstart != 0) clrec->start = (char *)UY_clstart;
	else clrec->start = (char *)UN_clpt[0];
	clrec->current = clrec->start;
/*
.....Initialize evaluator time
*/
	ncall = 0;
	etim = 0;
	gtimx(&stims,&stimm);
/*
.....Drive motion
*/
	NCLX_internal_geom = NCLX_TRUE;
	yflrgh (psgeom,dsgeom,irdat,rdat,pcmd,&stat);
	NCLX_internal_geom = NCLX_FALSE;
/*
.....Calculate evaluator time
*/
	gtimx(&etims,&etimm);
	stims = (etims-stims)*1000 + (etimm - stimm);
	NclxDbgEvalTime(&stims,&ncall,&etim);
/*
.....Mark the end of the clfile record
*/
	UY_clstart = 0;
	if (stat <= 0)
	{
		clrec->end = (char *)UN_clpt[0];
	}
	else
	{
		clrec->start = NULL;
		clrec->current = NULL;
		clrec->end = NULL;
	}
/*
.....End of routine
*/
	uu_list_free (&lathelist);
	NclxDbgPclrec (1,"clrec",clrec);  
	NclxDbgExit ("NclxMotLatheRough",stat);
   return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotLatheFinish (part,rough,clrec)
**       This function processes a LATHE/ROUGH command.
**    PARAMETERS
**    INPUT  :
**			part         Shape which defines the finished part.
**       rough        Lathe finish structure.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotLatheFinish (part,finish,clrec)
NCLX_mdl_shape *part;
NCLX_mot_lathe_finish *finish;
NCLX_mot_clrec *clrec;
{
	int stat,irdat[20],ix,dsgeom[6];
	int stims,stimm,etims,etimm;
	double rdat[16],pcmd[200];
/*
.....debug stuff
*/
	NclxDbgEnter ("NclxMotLatheFinish (stock,part,rough,clrec)");
	NclxDbgPdata (0,"part",part);
	NclxDbgPlfinish (0,"finish",finish);
/*
.....Allow user to interrupt motion
*/
	ix = setjmp(UU_jmpb);
	if (ix != 0)
	{
		return(471);
	}
/*
.....Initialize routine
*/
	UY_ncs = 0;
	uu_list_init (&lathelist,sizeof(UM_int2),1960,1960);
/*
.....Setup lathe data
*/
	rdat[0] = finish->stock_x;
	rdat[1] = finish->stock_y;
	irdat[0] = finish->inverse;
	nclx_mot_postcmd(finish->pcmd_engage,&irdat[1],&pcmd[0]);
	rdat[2] = finish->engang;
	rdat[3] = finish->engdis;
	nclx_mot_postcmd(finish->pcmd_retrct,&irdat[3],&pcmd[50]);
	rdat[4] = finish->retang;
	rdat[5] = finish->retdis;
/*
........Return logic
*/
	nclx_mot_postcmd(finish->pcmd_return,&irdat[5],&pcmd[100]);
	switch (finish->rettyp.type)
	{
	case NCLX_P_XAXIS: irdat[7] = 1; break;
	case NCLX_P_START: irdat[7] = 3; break;
	case NCLX_P_OFF: irdat[7] = 4; break;
	case NCLX_P_POINT:
			irdat[7] = 5;
			irdat[8] = finish->rettyp.pt->header.key;
			irdat[9] = NclxMdlToNclType(finish->rettyp.pt->header.relnum);
			UY_cs[UY_ncs] = (NCLX_mdl_struct *)finish->rettyp.pt; UY_ncs++;
			break;
	}
/*
........Final post command
*/
	nclx_mot_postcmd(finish->pcmd_final,&irdat[10],&pcmd[150]);
/*
.....Setup part shape
*/
	UY_ds = (NCLX_mdl_struct *)part;
	dsgeom[0] = part->header.key;
	dsgeom[1] = NclxMdlToNclType(part->header.relnum);
/*
.....Mark the start of the clfile record
*/
	if (UY_clstart != 0) clrec->start = (char *)UY_clstart;
	else clrec->start = (char *)UN_clpt[0];
	clrec->current = clrec->start;
/*
.....Initialize evaluator time
*/
	ncall = 0;
	etim = 0;
	gtimx(&stims,&stimm);
/*
.....Drive motion
*/
	NCLX_internal_geom = NCLX_TRUE;
	yflfin (dsgeom,irdat,rdat,pcmd,&stat);
	NCLX_internal_geom = NCLX_FALSE;
/*
.....Calculate evaluator time
*/
	gtimx(&etims,&etimm);
	stims = (etims-stims)*1000 + (etimm - stimm);
	NclxDbgEvalTime(&stims,&ncall,&etim);
/*
.....Mark the end of the clfile record
*/
	UY_clstart = 0;
	if (stat <= 0)
	{
		clrec->end = (char *)UN_clpt[0];
	}
	else
	{
		clrec->start = NULL;
		clrec->current = NULL;
		clrec->end = NULL;
	}
/*
.....End of routine
*/
	uu_list_free (&lathelist);
	NclxDbgPclrec (1,"clrec",clrec);  
	NclxDbgExit ("NclxMotLathefinish",stat);
   return(stat);
}

/*********************************************************************
**    E_FUNCTION     : void nclx_mot_postcmd (pcmd,irdat,pdat)
**       This function transfers a Lathe post-processor command into
**			arrays accepted by the Fortran controlling routine.
**    PARAMETERS
**    INPUT  :
**       pcmd         Post-processor command structure.
**    OUTPUT :
**			irdat        [0] = Major word, [1] = Number of words.
**       pdat         Minor word/values.
**    RETURNS      :
**       none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclx_mot_postcmd(pcmd,irdat,pdat)
NCLX_mot_post_cmd *pcmd;
int irdat[];
double pdat[];
{
	int i;
	UM_int2 ityp,i2;
	UM_real8 r8;
/*
.....Null command
*/
	if (pcmd == UU_NULL)
	{
		irdat[0] = irdat[1] = 0;
	}
/*
.....Store major word & nwds
*/
	else
	{
		irdat[0] = pcmd->major;
		irdat[1] = pcmd->nwds;
/*
.....Store minor words/values
*/
		for (i=0;i<pcmd->nwds;i++)
		{
			ityp = pcmd->type[i];
			i2 = pcmd->ppwrd[i];
			r8 = pcmd->ppval[i];
			clspwd(&pdat[i],&ityp,&i2,&r8);
		}
	}
	return;
}
