/*********************************************************************
**    NAME         :  ydebuga.c
**       CONTAINS:
**
**				NclxDbgPscrub
**				NclxDbgPrmill
**				NclxDbgPadvpocket
**				NclxDbgPlrough
**				NclxDbgPlfinish
**				NclxDbgPpcmd
**				NclxDbgPdscntl
**				NclxDbgPfillet
**
**    COPYRIGHT 2000 (c) Numerical Control Computer Sciences Inc.
**              All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       ydebugc.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:10:59
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
#include "ydebug.h"

#define dlevel(stat) ( ((stat) < (2))? (2): (stat+1) )

extern int (*UY_dbout)();  /* write debug routine */
extern int UY_nclxdebug;

void	NclxDbgPscrub();
void	NclxDbgPrmill();
void	NclxDbgPadvpocket();
void	NclxDbgPlrough();
void	NclxDbgPlfinish();
void	NclxDbgPpcmd();
void	NclxDbgPdscntl();

/*********************************************************************
**		E_FUNCTION     : NclxDbgPscrub (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_scrub structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  SCRUB structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPscrub (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_scrub *prm;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPint (stat,"numpass",prm->numpass);
		NclxDbgPint (stat,"numpts",prm->numpts);
		NclxDbgPint (stat,"bounded",prm->bounded);
/*
.....Boundary points
*/
		if (prm->bounded)
		{
			NclxDbgPdata (stat,"Boundary[1]",&prm->boundary[0]);
			NclxDbgPdata (stat,"Boundary[2]",&prm->boundary[1]);
			NclxDbgPdata (stat,"Boundary[3]",&prm->boundary[2]);
			NclxDbgPdata (stat,"Boundary[4]",&prm->boundary[3]);
		}
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPrmill (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_rmill structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  RMILL structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPrmill (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_rmill *prm;
{
	int i;
	char sbuf[20];
	int stat = dlevel(iostat);
/*
.....Rmill parameters
*/
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPword (stat,"motion_type",prm->motion_type);
		NclxDbgPword (stat,"profile",prm->profile);
		NclxDbgPenum (stat,"clpl_type",3,mrtnpl,prm->clpl_type);
		if (prm->clpl_type == NCLX_CLR_PLANE)
			NclxDbgPdata (stat,"clpl",&prm->clpl);
		else
			NclxDbgPreal (stat,"cldis",prm->cldis);
		NclxDbgPreal (stat,"pldis",prm->pldis);
		NclxDbgPword (stat,"step_type",prm->step_type);
		NclxDbgPreal (stat,"step_dis",prm->step_dis);
		NclxDbgPreal (stat,"fed",prm->fed);
		NclxDbgPreal (stat,"pfed",prm->pfed);
		NclxDbgPreal (stat,"plfed",prm->plfed);
		NclxDbgPenum (stat,"ret_type",3,mrtnpl,prm->ret_type);
		if (prm->ret_type == NCLX_CLR_PLANE)
			NclxDbgPdata (stat,"retpl",&prm->retpl);
		else
			NclxDbgPreal (stat,"retdis",prm->retdis);
/*
.....Thicks
*/
		for (i=0;i<4;i++)
		{
			sprintf(sbuf,"rough_thick[%d]",i);
			NclxDbgPreal (stat,sbuf,prm->rough_thick[i]);
			sprintf(sbuf,"finish_thick[%d]",i);
			NclxDbgPreal (stat,sbuf,prm->finish_thick[i]);
		}
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPadvpl (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_advpocket_pl structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Advance Pocket Planes structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPadvpl (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_advpocket_pl *prm;
{
	int stat = dlevel(iostat);
/*
.....Pocket plane parameters
*/
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPdata (stat,"ps",prm->ps);
		NclxDbgPenum (stat,"top_type",3,mrtnpl,prm->top_type);
		if (prm->top_type == NCLX_CLR_PLANE)
			NclxDbgPdata (stat,"top",&prm->top);
		else
			NclxDbgPreal (stat,"topdis",prm->topdis);
		NclxDbgPenum (stat,"clpl_type",3,mrtnpl,prm->clpl_type);
		if (prm->clpl_type == NCLX_CLR_PLANE)
			NclxDbgPdata (stat,"clpl",&prm->clpl);
		else
			NclxDbgPreal (stat,"cldis",prm->cldis);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPadvperim (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_advpocket_perim structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Advance Pocket Perimeter structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPadvperim (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_advpocket_perim *prm;
{
	int i;
	char sbuf[20];
	int stat = dlevel(iostat);
/*
.....Pocket plane parameters
*/
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPenum (stat,"peratt",3,aptlcond,prm->peratt);
		NclxDbgPdata (stat,"perimeter",prm->perimeter);
		NclxDbgPint (stat,"num_islands",prm->num_islands);
		for (i=0;i<prm->num_islands;i++)
		{
			sprintf(sbuf,"islatt[%d]",i);
			NclxDbgPenum (stat,sbuf,3,aptlcond,prm->islatt[i]);
			sprintf(sbuf,"island[%d]",i);
			NclxDbgPdata(stat,sbuf,prm->island[i]);
		}
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPadvpocket (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_advpocket structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Advanced Pocket structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPadvpocket (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_advpocket *prm;
{
	int stat = dlevel(iostat);
/*
.....Advanced Pocket parameters
*/
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPenum (stat,"entry",4,apentry,prm->entry);
		if (prm->entry == NCLX_RAMP)
		{
			NclxDbgPint (stat,"nramp",prm->nramp);
			NclxDbgPreal (stat,"ramp_dis",prm->ramp_dis);
		}
		else if (prm->entry == NCLX_HELIX)
		{
			NclxDbgPint (stat,"nrev",prm->nrev);
			NclxDbgPreal (stat,"helix_rad",prm->helix_rad);
		}
		NclxDbgPword (stat,"retract",prm->retract);
		NclxDbgPreal (stat,"level_dis",prm->level_dis);
		NclxDbgPreal (stat,"retdis",prm->retdis);
		NclxDbgPenum (stat,"pocket_dir",2,apdir,prm->pocket_dir);
		NclxDbgPenum (stat,"spiral_dir",2,aptlcond,prm->spiral_dir);
		NclxDbgPword (stat,"section_ret",prm->section_ret);
		NclxDbgPenum (stat,"corner",2,apcorner,prm->corner);
		NclxDbgPreal (stat,"step_max",prm->step_max);
		NclxDbgPreal (stat,"step_min",prm->step_min);
/*
.....Feed Rates
*/
		NclxDbgPword (stat,"fedrat->secondary",prm->fedrat.secondary);
		NclxDbgPreal (stat,"fedrat->angle",prm->fedrat.general);
		NclxDbgPreal (stat,"fedrat->general",prm->fedrat.general);
		NclxDbgPreal (stat,"fedrat->retract",prm->fedrat.retract);
		NclxDbgPreal (stat,"fedrat->position",prm->fedrat.position);
		NclxDbgPreal (stat,"fedrat->entry",prm->fedrat.entry);
		NclxDbgPreal (stat,"fedrat->transition",prm->fedrat.transition);
		NclxDbgPreal (stat,"fedrat->finish",prm->fedrat.finish);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPlrough (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_lathe_rough structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Lathe Rough structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPlrough (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_lathe_rough *prm;
{
	int stat = dlevel(iostat);
/*
.....Lathe Rough parameters
*/
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPreal (stat,"cldist",prm->cldist);
		NclxDbgPreal (stat,"stock_x",prm->stock_x);
		NclxDbgPreal (stat,"stock_y",prm->stock_y);
		NclxDbgPpcmd (stat,"pcmd_depth",prm->pcmd_depth);
		NclxDbgPreal (stat,"depth",prm->depth);
		NclxDbgPpcmd (stat,"pcmd_cutang",prm->pcmd_cutang);
		NclxDbgPreal (stat,"cutang",prm->cutang);
		NclxDbgPpcmd (stat,"pcmd_retrct",prm->pcmd_retrct);
		NclxDbgPreal (stat,"retang",prm->retang);
		NclxDbgPreal (stat,"retdis",prm->retdis);
/*
........Return type
*/
		NclxDbgPpcmd (stat,"pcmd_return",prm->pcmd_return);
		NclxDbgPenum (stat,"rettyp->type",5,rettyp,prm->rettyp.type);
		if (prm->rettyp.type == NCLX_P_POINT)
			NclxDbgPdata (stat,"rettyp->pt",prm->rettyp.pt);
		NclxDbgPpcmd (stat,"pcmd_final",prm->pcmd_final);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPlfinish (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_lathe_finish structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Lathe Finish structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPlfinish (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_lathe_finish *prm;
{
	int stat = dlevel(iostat);
/*
.....Lathe finish parameters
*/
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPreal (stat,"stock_x",prm->stock_x);
		NclxDbgPreal (stat,"stock_y",prm->stock_y);
		NclxDbgPword (stat,"inverse",prm->inverse);
		NclxDbgPpcmd (stat,"pcmd_engage",prm->pcmd_engage);
		NclxDbgPreal (stat,"engang",prm->engang);
		NclxDbgPreal (stat,"engdis",prm->engdis);
		NclxDbgPpcmd (stat,"pcmd_retrct",prm->pcmd_retrct);
		NclxDbgPreal (stat,"retang",prm->retang);
		NclxDbgPreal (stat,"retdis",prm->retdis);
/*
........Return type
*/
		NclxDbgPpcmd (stat,"pcmd_return",prm->pcmd_return);
		NclxDbgPenum (stat,"rettyp->type",5,rettyp,prm->rettyp.type);
		if (prm->rettyp.type == NCLX_P_POINT)
			NclxDbgPdata (stat,"rettyp->pt",prm->rettyp.pt);
		NclxDbgPpcmd (stat,"pcmd_final",prm->pcmd_final);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPpcmd (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_post_cmd structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Post command structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPpcmd (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_post_cmd *prm;
{
	int i;
	char sbuf[20];
	int stat = dlevel(iostat);
/*
........Post-processor command
*/
	if (prm != 0)
	{
		sprintf(sbuf,"%s->major",name_prm);
		NclxDbgPint (stat,"sbuf",prm->major);
		sprintf(sbuf,"%s->nwds",name_prm);
		NclxDbgPint (stat,sbuf,prm->nwds);
		for (i=0;i<prm->nwds;i++)
		{
			sprintf(sbuf,"%s->type[%d]",name_prm,i);
			NclxDbgPenum (stat,sbuf,2,pptype,prm->type[i]);
			if (prm->type[i] == NCLX_P_VOCAB)
			{
				sprintf(sbuf,"%s->ppwrd[%d]",name_prm,i);
				NclxDbgPint (stat,sbuf,prm->ppwrd[i]);
			}
			else
			{
				sprintf(sbuf,"%s->ppval[%d]",name_prm,i);
				NclxDbgPreal (stat,sbuf,prm->ppval[i]);
			}
		}
	}
}
/*********************************************************************
**              E_FUNCTION     : NclxDbgPdscntl (iostat, name_prm, ptr)
**                      This debug function outputs NCLX_mot_ds structure data
**              PARAMETERS
**              INPUT  :
**                      iostat       0 = Input parameter,
**                                   1 = Output parameter.
**                      name_prm     Parameter text string of calling routine.
**                      ptr          Drive surface control structure.
**              OUTPUT :
**                      none
**              RETURNS      :
**                      none
**              SIDE EFFECTS : none
**              WARNINGS     : none
*********************************************************************/
void NclxDbgPdscntl (iostat, name_prm, ptr)
char *name_prm;
int iostat;
NCLX_mot_ds *ptr;
{
	int i;
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		if (iostat == 0)
		{
			NclxDbgPint (stat,"numds",ptr->numds);
			NclxDbgPint (stat,"look",ptr->look);
			for (i=0; i<ptr->numds; i++)
				NclxDbgPcsrec(stat,"NCLX_mot_ds_rec",&ptr->dsrec[i]);
		}
	}
}
/*********************************************************************
**		E_FUNCTION     : NclxDbgPfillet (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_fillet structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Fillet structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPfillet (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_fillet *prm;
{
	int stat = dlevel(iostat);
/*
.....Output fillet parameters
*/
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPhdclrec(stat,"clrange",&(prm->clrange));
		NclxDbgPreal (stat,"rad",prm->rad);
		NclxDbgPreal (stat,"tol",prm->tol);
		NclxDbgPword (stat,"same",prm->same);
		if (prm->same) NclxDbgPreal (stat,"maxang",prm->maxang);
		NclxDbgPword (stat,"combine",prm->combine);
		NclxDbgPword (stat,"fedctl",prm->fedctl);
		if (prm->fedctl)
		{
			NclxDbgPreal (stat,"fedrt",prm->fedrt);
			NclxDbgPreal (stat,"fmax",prm->fmax);
			NclxDbgPdsat (stat,"direction",prm->direction);
			NclxDbgPreal (stat,"cdia",prm->cdia);
		}
	}
}
