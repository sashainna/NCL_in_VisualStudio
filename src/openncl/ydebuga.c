/*********************************************************************
**    NAME         :  ydebuga.c
**       CONTAINS:
**
**				NclxMotTrace
**				NclxDbgEnter
**				NclxDbgExit
**				NclxDbgPmotprm
**				NclxDbgPhdclrec
**				NclxDbgPclrec
**				NclxDbgPmdl
**				NclxDbgPmdlEvalsf
**				NclxDbgPmdlEvalcv
**				NclxDbgPdsat
**				NclxDbgPdsdir
**				NclxDbgPcsat
**				NclxDbgPmaxdp
**				NclxDbgPtlaxis
**				NclxDbgPcutter
**				NclxDbgPsrfvct
**				NclxDbgPthick
**				NclxDbgPtoler
**				NclxDbgPfedrat
**				NclxDbgPcscntl
**				NclxDbgPcsrec
**				NclxDbgEvalRtn
**				NclxDbgEvalTime
**				NclxDbgMemerr
**				NclxDbgMemalloc
**
**    COPYRIGHT 1997 (c) Numerical Control Computer Sciences Inc.
**              All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       ydebuga.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:10:58
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

#if (UU_COMP == UU_SUN) || (UU_COMP == UU_IRIS4D)
#ifndef UU_RS6000
#define nclxdbgevaltime nclxdbgevaltime_
#endif
#endif

#define dlevel(stat) ( ((stat) < (2))? (2): (stat+1) )

int (*UY_dbout)();  /* write debug routine */
int UY_nclxdebug;
int UY_debugDrive = 0;

void NclxMotTrace();
void NclxDbgEnter();
void NclxDbgExit();
void NclxDbgPmotprm();
void NclxDbgPhdclrec();
void NclxDbgPclrec();
void NclxDbgPmdl();
void NclxDbgPmdlEvalsf();
void NclxDbgPmdlEvalcv();
void NclxDbgPdsat();
void NclxDbgPdsdir();
void NclxDbgPcsat();
void NclxDbgPmaxdp();
void NclxDbgPtlaxis();
void NclxDbgPcutter();
void NclxDbgPsrfvct();
void NclxDbgPthick();
void NclxDbgPtoler();
void NclxDbgPfedrat();
void NclxDbgPcscntl();
void NclxDbgPcsrec();
void NclxDbgEvalRtn();
void NclxDbgEvalTime();
void NclxDbgMemerr();
void NclxDbgMemalloc();

/*********************************************************************
**		E_FUNCTION     : int NclxMotTrace (dboutput,level)
**			Initializes the OpenNCL calling routine trace function.
**		PARAMETERS
**		INPUT  :
**			dboutput             Output function to call.  
**			level                Level of trace required.
**		OUTPUT :
**			none
**		RETURNS      :
**			UU_SUCCESS iff no error; else UU_FAILURE
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxMotTrace (dboutput,level)
int (*dboutput)();
int level;
{
/*
.....Set global variables
*/
	UY_nclxdebug = level;
	UY_dbout = dboutput;
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgEnter (call_prm)
**			This function outputs debug line with subroutine header.
**		PARAMETERS
**		INPUT  :
**			call_prm             Calling parameters text string.
**		OUTPUT :
**			none
**		RETURNS      :
**			UU_SUCCESS iff no error; else UU_FAILURE
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgEnter (call_prm)
char *call_prm;
{
	char buff[132];
	buff[0] = '\0';
	if (UY_nclxdebug & NCLX_DBG_NAM)
	{
		sprintf (buff,"Nclx->%s\n",call_prm);
		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgExit (call_prm,status)
**			This function outputs debug subroutine exit status.
**		PARAMETERS
**		INPUT  :
**			call_prm             Calling parameters text string.
**			status               Return status of calling routine.
**		OUTPUT :
**			none
**		RETURNS      :
**			UU_SUCCESS iff no error; else UU_FAILURE
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgExit (call_prm,status)
char *call_prm;
int status;
{
	char buff[132];
	buff[0] = '\0';

	if (UY_nclxdebug & NCLX_DBG_NAM)
	{
		sprintf (buff,"NclxR>%s = %d\n",call_prm,status);
		UY_dbout (buff);
	}
}
/*********************************************************************
**		E_FUNCTION     : NclxDbgPmotprm (iostat, name_prm)
**			This debug function outputs motion parameters only
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**		OUTPUT :
**			none
**		RETURNS      :
**			UU_SUCCESS iff no error; else UU_FAILURE
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPmotprm (iostat, name_prm)
int iostat;
char *name_prm;
{
	int dbgsv;
	if (UY_nclxdebug & NCLX_DBG_MOT)
	{
		int inum;
		double rnum, vec[3];
		NCLX_mot_data motdata;
		NCLX_mdl_data cvg;
		NCLX_mot_tlaxis *tlaxis;

		tlaxis = (NCLX_mot_tlaxis *) &motdata;

		NclxDbgPrmName (iostat,name_prm);
/*
.....fake debug level to output parameters from MotGet... functions
*/
		dbgsv = UY_nclxdebug;
		UY_nclxdebug = NCLX_DBG_MOT | NCLX_DBG_PRM;

		NclxMotGetAutost (&inum);
		NclxMotGetContact (&inum);
		NclxMotGetCutter (&motdata);
		NclxMotGetFeedrate (&motdata);
		NclxMotGetFwd (vec);
		NclxMotGetGougck (&inum);
		NclxMotGetMaxang (&rnum);
		NclxMotGetMaxdp (&motdata);
		NclxMotGetNumpts (&inum);
		NclxMotGetThick (&motdata);
		tlaxis->curve = (NCLX_mdl_curve *) &cvg;
		tlaxis->modify.guide = &cvg;
		NclxMotGetTlaxis (tlaxis);
		NclxMotGetToler (&motdata);
		NclxMotGetTool (&motdata);
	
		UY_nclxdebug = dbgsv;
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPhdclrec (iostat, name_prm, clrec)
**			This debug function outputs NCLX_mot_clrec header structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			clrec                Cl record to print.
**		OUTPUT :
**			none
**		RETURNS      :
**			UU_SUCCESS iff no error; else UU_FAILURE
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPhdclrec (iostat, name_prm, clrec)
int iostat;
char *name_prm;
NCLX_mot_clrec *clrec;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPpointer (stat,"start",clrec->start);
		NclxDbgPpointer (stat,"end",clrec->end);
		NclxDbgPpointer (stat,"current",clrec->current);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPclrec (iostat, name_prm, clrec)
**			This debug function outputs NCLX_mot_clrec structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			clrec                Cl record to print.
**		OUTPUT :
**			none
**		RETURNS      :
**			UU_SUCCESS iff no error; else UU_FAILURE
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPclrec (iostat, name_prm, clrec)
int iostat;
char *name_prm;
NCLX_mot_clrec *clrec;
{
	int stat = dlevel(iostat);
	int inc, i;
/*	if (UY_nclxdebug & NCLX_DBG_PRM || UY_nclxdebug & NCLX_DBG_CLF)*/
	if (UY_nclxdebug & NCLX_DBG_CLF)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxMotClRewind((*clrec));
/*
.....output current clfile record number etc...
*/
		do
		{
			NclxMotClRead(clrec);
			{
				NclxDbgPint (stat,"type",clrec->type);
				switch (clrec->type)
				{
/*
.....cutter record
*/
				case 6000:
					{
						NclxDbgPrmName (stat,"cutter");
						for (i=0; i<clrec->mxcl; i++)
							NclxDbgPreal (stat+1,cuttprm[i],clrec->cldata[i]);
					}
					break;
/*
.....motion records
*/
				case 5000:
					{
						inc  = 6;
						NclxDbgPenum (stat,"subtype",9,gtsubtp,clrec->subtype);
						NclxDbgPint (stat,"npts",clrec->mxcl/inc);
						for (i=0; i<clrec->mxcl; i+=inc)
							NclxDbgPdblvec (stat+1,"endpt",(i/inc+1),&clrec->cldata[i]);
					}
					break;
				case 5200:
				case 5210:
				case 5220:
					{
						inc  = 21;
						NclxDbgPenum (stat,"subtype",9,gtsubtp,clrec->subtype);
						NclxDbgPint (stat,"npts",clrec->mxcl/inc);
						for (i=0; i<clrec->mxcl; i+=inc)
						{
							NclxDbgPdblvec (stat+1,"endpt",(i/inc+1),&clrec->cldata[i]);
							if (clrec->type == 5220)
								NclxDbgPdblvec (stat+1,sftyp[2],-1,&clrec->cldata[i+6]);
							else
							{
								NclxDbgPvec (stat+1,"fwd",&clrec->cldata[i+6]);
								NclxDbgPdblvec (stat+1,sftyp[0],-1,&clrec->cldata[i+9]);
								NclxDbgPdblvec (stat+1,sftyp[1],-1,&clrec->cldata[i+15]);
							}
						}
					}
					break;
/*
.....circle record
*/
				case 3000:
					{
						NclxDbgPrmName (stat,"circle");
						NclxDbgPint (stat,"subtype",clrec->subtype);
/*						NclxDbgPint (stat,"mxcl",clrec->mxcl);    */
						NclxDbgPvec (stat+1,"center",&clrec->cldata[5]);
						NclxDbgPvec (stat+1,"normal",&clrec->cldata[8]);
						NclxDbgPreal (stat+1,"radius",clrec->cldata[11]);
					}
					break;
/*
.....feedrate/rapid commands
*/
				case 2000:
					{
						NclxDbgPint (stat,"subtype",clrec->subtype);
						if (clrec->subtype == 1009)
						{
							NclxDbgPreal (stat+1,"feedrate",clrec->cldata[0]);
						}
					}
					break;
				default:
					break;
				}
			}
		} while (clrec->current != UU_NULL);
		NclxMotClRewind((*clrec));
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPmdl (iostat, name_prm, mdlptr)
**			This debug function outputs NCLX_mdl_struct parameter data 
**			preceded by parameter name.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			mdlptr               Model structure to print.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPmdl (iostat, name_prm, mdlptr)
char *name_prm;
int iostat;
NCLX_mdl_struct *mdlptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug > 1)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPint (stat,"key",mdlptr->key);
		NclxDbgPenum (stat,"relnum",18,mdltype,mdlptr->relnum);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPmdlEvalsf (uprm, vprm, srf, ptr)
**			This debug function outputs NCLX_mdl_surf_eval parameter data 
**			preceded by parameter name.
**		PARAMETERS
**		INPUT  :
**			uprm                 Evaluated u-parameter.
**			vprm                 Evaluated u-parameter.
**			srf                  Surface structure.
**			ptr                  Pointer to evaluator structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPmdlEvalsf (uprm, vprm, srf, ptr)
double uprm, vprm;
NCLX_mdl_surf *srf;
NCLX_mdl_surf_eval *ptr;
{
	int iostat = 1, stat = 2;
	double uvprm[2];
	if (UY_nclxdebug & NCLX_DBG_EVL)
	{
		uvprm[0] = uprm;
		uvprm[1] = vprm;
		NclxDbgP2real (iostat,"Evaluating surface at (u,v)",uvprm);
		NclxDbgPint (stat,"key",srf->header.key);
		NclxDbgPenum (stat,"relnum",18,mdltype,srf->header.relnum);  
/*		NclxDbgPint (stat,"key",ptr->key);  same as srf->header.key */
		NclxDbgPvec (stat,"pt",ptr->pt);
		NclxDbgPvec (stat,"normal",ptr->normal);
		NclxDbgPvec (stat,"udrv1",ptr->udrv1);
		NclxDbgPvec (stat,"vdrv1",ptr->vdrv1);
		NclxDbgPvec (stat,"udrv2",ptr->udrv2);
		NclxDbgPvec (stat,"vdrv2",ptr->vdrv2);
		NclxDbgPreal (stat,"ucrv",ptr->ucrv);
		NclxDbgPreal (stat,"vcrv",ptr->ucrv);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPmdlEvalcv (tprm, crv, ptr)
**			This debug function outputs NCLX_mdl_curve_eval parameter data 
**			preceded by parameter name.
**		PARAMETERS
**		INPUT  :
**			tprm                 Evaluated u-parameter.
**			crv                  Curve structure.
**			ptr                  Pointer to evaluator structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPmdlEvalcv (tprm, crv, ptr)
double tprm;
NCLX_mdl_curve *crv;
NCLX_mdl_curve_eval *ptr;
{
	int iostat = 1, stat = 2;
	if (UY_nclxdebug & NCLX_DBG_EVL)
	{
		NclxDbgPreal (iostat,"Evaluating curve at u",tprm);
		NclxDbgPint (stat,"key",crv->header.key);
		NclxDbgPenum (stat,"relnum",18,mdltype,crv->header.relnum);  
		NclxDbgPvec (stat,"pt",ptr->pt);
		NclxDbgPvec (stat,"udrv1",ptr->udrv1);
		NclxDbgPvec (stat,"udrv2",ptr->udrv2);
		NclxDbgPreal (stat,"ucrv",ptr->ucrv);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPdsat (iostat, name_prm, prm)
**			This debug function outputs drive surface parameter 
**			preceded by its name.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Tool condition (NCLX_TLON, etc.).
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPdsat (iostat, name_prm, prm)
char *name_prm;
int iostat;
int prm;
{
	char buff[132];
	char cword[24];
	int i;

	buff[0] = cword[0] = '\0';

	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		switch (prm)
		{
		case NCLX_TLRGT: i = 0; 
			break;
		case NCLX_TLLFT: i = 1; 
			break;
		case NCLX_TLON:  i = 2; 
			break;
		case NCLX_TO:    i = 3; 
			break;
		case NCLX_PAST:  i = 4; 
			break;
		case NCLX_ON:    i = 5; 
			break;
		default:         i = -1; 
		}
		if (i < 0)
			sprintf (cword,"%d",prm);
		else if (i > 2)
			strcpy (cword,csatt[i-3]);
		else
			strcpy (cword,dsatt[i]);
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name_prm,-1,buff)],"%s\n",cword);  
		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPdsdir (iostat, name_prm, prm)
**			This debug function outputs drive direction parameter 
**			preceded by its name.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Tool direction (NCLX_GOFWD, etc.).
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPdsdir (iostat, name_prm, prm)
char *name_prm;
int iostat;
int prm;
{
	char buff[132] , cword[24];
	int i;
	buff[0] = cword[0] = '\0';

	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		switch (prm)
		{
		case NCLX_GOFWD: i = 0; 
			break;
		case NCLX_GOLFT: i = 1; 
			break;
		case NCLX_GORGT: i = 2; 
			break;
		case NCLX_GOBCK: i = 3; 
			break;
		case NCLX_GOUP:  i = 4; 
			break;
		case NCLX_GODWN: i = 5; 
			break;
		default:
			i = -1; 
		}
		if (i < 0)
			sprintf (cword,"%d",prm);
		else
			strcpy (cword,godir[i]);
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name_prm,-1,buff)],"%s\n",cword);  
		UY_dbout (buff);
	}
}
/*********************************************************************
**		E_FUNCTION     : NclxDbgPcsat (iostat, name_prm, prm)
**			This debug function outputs check surface parameter 
**			preceded by its name.
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Check surface condition (NCLX_TO, etc.).
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPcsat (iostat, name_prm, prm)
char *name_prm;
int iostat;
int prm;
{
	char buff[132] , cword[24];
	int i;

	buff[0] = cword[0] = '\0';
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		switch (prm)
		{
		case NCLX_TO:    i = 0; 
			break;
		case NCLX_PAST:  i = 1; 
			break;
		case NCLX_ON:    i = 2; 
			break;
		case NCLX_TANTO: i = 3; 
			break;
		case NCLX_PSTAN: i = 4; 
			break;
		case NCLX_AUTO:  i = 5; 
			break;
		default:         i = -1; 
		}
		if (i < 0)
			sprintf (cword,"%d",prm);
		else
			strcpy (cword,csatt[i]);
		sprintf (&buff[NclxDbgPrmNameAd(iostat,name_prm,-1,buff)],"%s\n",cword);  
		UY_dbout (buff);
	}
}
/*********************************************************************
**		E_FUNCTION     : NclxDbgPmaxdp (iostat, name_prm, prm)
**			This debug function outputs maxdp structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  MAXDP structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPmaxdp (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_maxdp *prm;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPreal (stat,"min",prm->min);
		NclxDbgPreal (stat,"max",prm->max);
		NclxDbgPword (stat,"mauto",prm->mauto);
		NclxDbgPword (stat,"step",prm->step);
		NclxDbgPint (stat,"attempts",prm->attempts);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPtlaxis (iostat, name_prm, tlaxis)
**			This debug function outputs NCLX_mot_tlaxis structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			tlaxis               TLAXIS structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPtlaxis (iostat, name_prm, tlaxis)
char *name_prm;
NCLX_mot_tlaxis *tlaxis;
int iostat;
{
	char tls[300], buf[300];
	int stat = dlevel(iostat);
	int deg = 0;
	double rate;

	tls[0] = buf[0] = '\0';

	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPrmNameAd1(stat,"TLAXIS/",tls);
		
		switch (tlaxis->mode)
		{
		case NCLX_MOT_TLAXIS_FIXED:
			sprintf(buf,"%g,%g,%g",tlaxis->vector[0],tlaxis->vector[1],
				tlaxis->vector[2]);
			strcat(tls,buf);
			if (tlaxis->normal) strcat(tls,",NORMAL");
			break;
	
		case NCLX_MOT_TLAXIS_NORMAL:
			strcat(tls,"NORMAL,PS");
			if (tlaxis->perpto_flag)
			{
				strcat(tls,",PERPTO");
				sprintf(buf,",%g,%g,%g",tlaxis->vector[0],tlaxis->vector[1],
					tlaxis->vector[2]);
				strcat(tls,buf);
			}
			break;
	
		case NCLX_MOT_TLAXIS_TANTO:
			sprintf(buf,"TANTO,DS,%g",tlaxis->height);
			strcat(tls,buf);
			if (tlaxis->perpto_flag)
			{
				strcat(tls,",PERPTO");
				sprintf(buf,",%g,%g,%g",tlaxis->perpto[0],tlaxis->perpto[1],
					tlaxis->perpto[2]);
				strcat(tls,buf);
			}
			if (tlaxis->parelm) strcat(tls,",PARELM");
			break;
	
		case NCLX_MOT_TLAXIS_FAN:
			sprintf(buf,"TANTO,DS,%g,FAN",tlaxis->height);
			strcat(tls,buf);
			if (tlaxis->center == NCLX_TOOL_CENTER_ON) 
				strcat(tls,",CENTER,ON");
			if (tlaxis->center == NCLX_TOOL_CENTER_AUTO) 
				strcat(tls,",CENTER,AUTO");
			else
				strcat(tls,",CENTER,OFF");
			NclxMotGetFanInterp (&deg,&rate);
			if (deg > 1)
			{
				sprintf(buf,",SMOOTH,%d,%g",deg,rate);
				strcat(tls,buf);
			}
			break;
	
		case NCLX_MOT_TLAXIS_ATANGL:
			sprintf(buf,"ATANGL,%g,PS",tlaxis->angle);
			strcat(tls,buf);
			if (tlaxis->heel < -.0001 || tlaxis->heel > .0001)
			{
				sprintf(buf,",CLDIST,%g",tlaxis->heel);
				strcat(tls,buf);
			}
			if (tlaxis->contact) strcat(tls,",CONTCT");
			if (tlaxis->perpto_flag)
			{
				sprintf(buf,",PERPTO,%g,%g,%g",tlaxis->perpto[0],tlaxis->perpto[1],
					tlaxis->perpto[2]);
				strcat(tls,buf);
			}
			break;
	
		case NCLX_MOT_TLAXIS_COMBIN:
			sprintf(buf,"COMBIN,%g",tlaxis->height);
			strcat(tls,buf);
			if (tlaxis->parelm) strcat(tls,",PARELM");
			if (tlaxis->center == NCLX_TOOL_CENTER_ON) 
				strcat(tls,",CENTER,ON");
			if (tlaxis->center == NCLX_TOOL_CENTER_AUTO) 
				strcat(tls,",CENTER,AUTO");
			else
				strcat(tls,",CENTER,OFF");
			sprintf(buf,",%g,%g",tlaxis->cmb_depart,tlaxis->cmb_approach);
			strcat(tls,buf);
			NclxMotGetFanInterp (&deg,&rate);
			if (deg > 1)
			{
				sprintf(buf,",SMOOTH,%d,%g",deg,rate);
				strcat(tls,buf);
			}
			break;

		case NCLX_MOT_TLAXIS_POINT:
			sprintf(buf,"THRU,(POINT/%g,%g,%g)",tlaxis->point[0],tlaxis->point[1],
				tlaxis->point[2]);
			strcat(tls,buf);
			break;
		case NCLX_MOT_TLAXIS_CURVE:
			sprintf(buf,"THRU,CV%d,%g",tlaxis->curve->header.key,
						tlaxis->curve_dist);
			strcat(tls,buf);
			break;
		default:
			sprintf(buf,"Unknown Mode = %d",tlaxis->mode);
			strcat(tls,buf);
			break;
		}
/*
.....TLAXIS/RIGHT-FWD
*/
		if (tlaxis->modify.angle_flag)
		{
			sprintf(buf,",RIGHT,%g,FWD,%g",tlaxis->modify.right_angle,
						tlaxis->modify.fwd_angle);
			strcat(tls,buf);
		}
/*
.....TLAXIS/GUIDE
*/
		if (tlaxis->modify.guide_flag)
		{
			sprintf(buf,",GUIDE,CV%d",tlaxis->modify.guide->data.header.key);
			if (tlaxis->modify.guide_contact) strcat(buf,",CONTCT");
			else strcat(buf,",OFFSET");
			if (tlaxis->modify.guide_cond == NCLX_TLLFT) strcat(buf,",TLLFT");
			else if (tlaxis->modify.guide_cond == NCLX_TLON) strcat(buf,",TLON");
			else if (tlaxis->modify.guide_cond == NCLX_TLRGT) strcat(buf,",TLRGT");
			strcat(tls,buf);
			if (tlaxis->modify.guide_offset < -.0001 ||
				 tlaxis->modify.guide_offset >  .0001)
			{
				sprintf(buf,",%g",tlaxis->modify.guide_offset);
				strcat(tls,buf);
			}
		}
/*
.....TLAXIS/SECPS
*/
		if (tlaxis->modify.secps_flag)
		{
			sprintf(buf,",SECPS, key%d",tlaxis->modify.secps->key);
			strcat(tls,buf);
		}
/*
.....TLAXIS/GOUGCK
*/
		if (tlaxis->modify.gouge) strcat(tls,",GOUGCK,ON");
/*
.....TLAXIS/MODIFY
*/
		if (tlaxis->adjust_flag)
		{
			sprintf(buf,",MODIFY,%g,%g,%g,%g,%g",tlaxis->adjust.right_offset,
				tlaxis->adjust.fwd_offset,tlaxis->adjust.up_offset,
				tlaxis->adjust.right_tilt,tlaxis->adjust.fwd_tilt);
			strcat(tls,buf);
		}
		strcat(tls,"\n");
		UY_dbout (tls);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPcutter (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_cutter structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  CUTTER structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPcutter (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_cutter *prm;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPreal (stat,cuttprm[0],prm->diameter);
		NclxDbgPreal (stat,cuttprm[1],prm->radius);
		NclxDbgPreal (stat,cuttprm[2],prm->height);
		NclxDbgPreal (stat,cuttprm[3],prm->side_angle);
		NclxDbgPreal (stat,cuttprm[4],prm->zheight);
		NclxDbgPreal (stat,cuttprm[5],prm->flat_angle);
	}
}
/*********************************************************************
**		E_FUNCTION     : NclxDbgPsrfvct (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_srfvct structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  Surface vector structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPsrfvct (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_srfvct *prm;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPword (stat,"dsflag",prm->dsflag);
		if (prm->dsflag == NCLX_TRUE) NclxDbgPvec (stat,"dsvec",prm->dsvec);
		NclxDbgPword (stat,"csflag",prm->csflag);
		if (prm->csflag == NCLX_TRUE) NclxDbgPvec (stat,"csvec",prm->csvec);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPthick (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_thick structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  THICK structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPthick (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_thick *prm;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPreal (stat,"ps",prm->ps);
		NclxDbgPreal (stat,"ds",prm->ds);
		NclxDbgPreal (stat,"cs",prm->cs);
/*
.....Four more check surfaces for multiple thick.
*/
		NclxDbgPreal (stat,"cs2",prm->cs2);
		NclxDbgPreal (stat,"cs3",prm->cs3);
		NclxDbgPreal (stat,"cs4",prm->cs4);
		NclxDbgPreal (stat,"cs5",prm->cs5);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPtoler (iostat, name_prm, prm)
**			This debug function outputs NCLX_mot_toler structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			prm                  TOLER structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPtoler (iostat, name_prm, prm)
char *name_prm;
int iostat;
NCLX_mot_toler *prm;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPreal (stat,"chordal",prm->chordal);
		NclxDbgPreal (stat,"dsps",prm->dsps);
		NclxDbgPreal (stat,"cs",prm->cs);
/* FIXXES */
		NclxDbgPreal (stat,"start_pt",prm->start_pt);
		NclxDbgPreal (stat,"start_cos",prm->start_cos);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPfedrat (iostat, name_prm, ptr)
**			This debug function outputs NCLX_mot_feedrate structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  FEDRAT structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPfedrat (iostat, name_prm, ptr)
char *name_prm;
int iostat;
NCLX_mot_feedrate *ptr;
{
	int stat = dlevel(iostat);
	char buff[300];
	buff[0] = '\0';
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		sprintf (&buff[NclxDbgPrmNameAd1(stat,"FEDRAT/",buff)],"%g\n",
					ptr->base_feedrate);
		if (ptr->slowdown_flag == NCLX_TRUE)
		{
			sprintf (&buff[NclxDbgPrmNameAd1(stat,"FEDRAT/AT,",buff)],"%g,%g,LENGTH,%g\n",
						ptr->slowdown_dist,ptr->slowdown_feedrate,ptr->height);
		}
		if (ptr->accel_flag == NCLX_TRUE)
		{
			sprintf (&buff[NclxDbgPrmNameAd1(stat,"FEDRAT/OUT,",buff)],"%g,%g,LENGTH,%g\n",
						ptr->accel_dist,ptr->accel_feedrate,ptr->height);
		}
		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPcscntl (iostat, name_prm, ptr)
**			This debug function outputs NCLX_mot_cs structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Check surface control structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPcscntl (iostat, name_prm, ptr)
char *name_prm;
int iostat;
NCLX_mot_cs *ptr;
{
	int i;
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		if (iostat == 0) NclxDbgPint (stat,"numchk",ptr->numchk);
		else NclxDbgPint (stat,"wchk",ptr->wchk);
		if (iostat == 0)
		{
			for (i=0; i<ptr->numchk; i++)
				NclxDbgPcsrec (stat,"NCLX_mot_cs_rec",&ptr->csrec[i]);
		}
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgPcsrec (iostat, name_prm, ptr)
**			This debug function outputs NCLX_mot_cs_rec structure data 
**		PARAMETERS
**		INPUT  :
**			iostat               0 = Input parameter, 1 = Output parameter.
**			name_prm             Parameter text string of calling routine.
**			ptr                  Check surface structure.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgPcsrec (iostat, name_prm, ptr)
char *name_prm;
int iostat;
NCLX_mot_cs_rec *ptr;
{
	int stat = dlevel(iostat);
	if (UY_nclxdebug & NCLX_DBG_PRM)
	{
		NclxDbgPrmName (iostat,name_prm);
		NclxDbgPword (stat,"nrpt_flag",ptr->nrpt_flag);
		if (ptr->nrpt_flag)
			NclxDbgPpoint (stat,"NCLX_mdl_point",ptr->nrpt);
		NclxDbgPint (stat,"nintof",ptr->nintof);
		NclxDbgPcsat (stat,"csatt", ptr->csatt);
		NclxDbgPdata (stat,"cs", ptr->cs);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgEvalRtn (eval,status)
**			This routine outputs the status code from a user defined evaluator
**		PARAMETERS
**		INPUT  :
**			eval                 Evaluator name.
**			status               Return status.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgEvalRtn (eval,status)
char *eval;
int status;
{
	char buff[132];
	if (UY_nclxdebug & NCLX_DBG_EVL)
	{
		sprintf(buff,"NclxR>%s evaluator returns: %d\n",eval,status);
		UY_dbout(buff);
	}
}
/*********************************************************************
**		E_FUNCTION     : NclxDbgEvalTime (mtim,ncalls,etim)
**			This function outputs debug line with evaluator calls & times
**		PARAMETERS
**		INPUT  :
**			mtim                 Motion time.
**			ncalls               Number of calls to evaluator.
**			etim                 Evaluator time.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void nclxdbgevaltime (mtim,ncalls,etim)
int *mtim,*ncalls,*etim;
{
	NclxDbgEvalTime(mtim,ncalls,etim);
}

void NclxDbgEvalTime (mtim,ncalls,etim)
int *mtim,*ncalls,*etim;
{
	char buff[132];
	buff[0] = '\0';
	if (UY_nclxdebug & NCLX_DBG_TIM)
	{
		sprintf (buff,"Motion Time = %d   Evaluator Calls = %d   Evaluator Time = %d\n",*mtim,*ncalls,*etim);
		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgMemerr (isize)
**			This function outputs debug line with when memory allocation
**			fails.
**		PARAMETERS
**		INPUT  :
**			isize                Atempted memory allocation size.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgMemerr (isize)
int isize;
{
	char buff[132];
	buff[0] = '\0';
	if (UY_nclxdebug & NCLX_DBG_MEM)
	{
		sprintf (buff,"Failed attempt to allocate memory block of %d bytes\n",isize);
		UY_dbout (buff);
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgMemalloc (isize)
**			This function outputs the current memory allocation table.
**		PARAMETERS
**		INPUT  :
**			al                Memory allocation table entry.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgMemalloc (inc,al)
int al[],inc;
{
	char buff[132];
	buff[0] = '\0';
/*
.....Print out memory allocation table
*/
	if (UY_nclxdebug & NCLX_DBG_MEM)
	{
		if (al[0] != 0) 
		{
			sprintf(buff,"Allocated memory block #%d= %d bytes at %d\n",inc,al[1],al[0])
;
			UY_dbout(buff);
		}
	}
}

/*********************************************************************
**		E_FUNCTION     : NclxDbgMemtotal (iall,ifree,iallnow)
**			This function outputs the total memory allocated and freed.
**		PARAMETERS
**		INPUT  :
**			iall             Total memory allocated.
**			ifree            Total memory freed.
**			iallnow          Memory still allocated.
**		OUTPUT :
**			none
**		RETURNS      :
**			none
**		SIDE EFFECTS : none
**		WARNINGS     : none
*********************************************************************/
void NclxDbgMemtotal (iall,ifree,iallnow)
int iall,ifree,iallnow;
{
	char buff[132];
	buff[0] = '\0';
/*
.....Print out memory list
*/
	if (UY_nclxdebug & NCLX_DBG_MEM)
	{
		sprintf(buff,"Total allocated memory   = %d\n",iall);
		UY_dbout(buff);
		sprintf(buff,"Total freed memory       = %d\n",ifree);
		UY_dbout(buff);
		sprintf(buff,"Current allocated memory = %d\n",iallnow);
		UY_dbout(buff);
	}
}
