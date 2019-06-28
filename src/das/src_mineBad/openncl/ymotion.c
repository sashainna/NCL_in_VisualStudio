/*********************************************************************
**    NAME         :  ymotion.c
**       CONTAINS:
**				int NclxSetuv (eptr, eval)
**				int NclxGetuv (eval, eptr)
**				int NclxMotGoto(geo,clrec)
**				int NclxMotFrom (geo)
**				int NclxMotDrive (ps,dsatt,dsdir,ds,csatt,cs,clrec)
**				int NclxMotDrive1 (ps,dsatt,dsdir,ds,cscntl,clrec)
**				int NclxMotGo1(dsatt,ds,clrec)
**				int NclxMotGo2(ps,dsatt,ds,clrec)
**				int NclxMotGo3(ps,dsatt,ds,csatt,cs,clrec)
**				int NclxMotDriveCurve (dsatt,ds,clrec)
**				int NclxMotDriveAuto (ps,dsatt,dscntl,cscntl,clrec)
**    COPYRIGHT 1996 (c) Numerical Control Computer Sciences Inc.
**              All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       ymotion.c , 25.1
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
#include <setjmp.h>

extern int NCLX_internal_geom;
extern int UY_debugDrive;
jmp_buf UU_jmpb;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
int ncall;
int etim;
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : int NclxSetuv (eptr, eval)
**       This function returns the initial UV settings for a curve/
**			surface.
**    PARAMETERS
**    INPUT  :
**       eptr         Curve/Surface to obtain the UV settings from.
**    OUTPUT :
**       eval         Current UV settings.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxSetuv (eptr, eval)
NCLX_mdl_struct *eptr;
double eval[2];
{
	NCLX_mdl_surf *sf;
	NCLX_mdl_trimsf *tsf;
	NCLX_mdl_curve  *cv;

	eval[0] = .5; eval[1] = .5;
	if (eptr->relnum == NCLX_MDL_TRIMSF
			|| eptr->relnum == NCLX_MDL_SURF
			|| eptr->relnum == NCLX_MDL_NSURF)
	{
		if (eptr->relnum == NCLX_MDL_TRIMSF)
		{
			tsf = (NCLX_mdl_trimsf *)eptr;
			sf = tsf->surf;
		}
		else
			sf = (NCLX_mdl_surf *)eptr;
		eval[0] = sf->sfhead.eval[0];
		eval[1] = sf->sfhead.eval[1];
	}
	else if (eptr->relnum == NCLX_MDL_CURVE
			|| eptr->relnum == NCLX_MDL_BSPLINE
			|| eptr->relnum == NCLX_MDL_COMPOSITE)
	{
		cv = (NCLX_mdl_curve *)eptr;
		eval[0] = cv->cvhead.eval;
	}
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : int NclxGetuv (eptr, eval)
**       This function sets the initial UV settings for a curve/
**			surface.
**    PARAMETERS
**    INPUT  :
**       eptr         Curve/Surface to receive the UV settings.
**    OUTPUT :
**       eval         Current UV settings.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int NclxGetuv (eval, eptr)
NCLX_mdl_struct *eptr;
double eval[2];
{
	NCLX_mdl_surf *sf;
	NCLX_mdl_trimsf *tsf;
	NCLX_mdl_curve  *cv;

	if (eptr->relnum == NCLX_MDL_TRIMSF
			|| eptr->relnum == NCLX_MDL_SURF
			|| eptr->relnum == NCLX_MDL_NSURF)
	{
		if (eptr->relnum == NCLX_MDL_TRIMSF)
		{
			tsf = (NCLX_mdl_trimsf *)eptr;
			sf = tsf->surf;
		}
		else
			sf = (NCLX_mdl_surf *)eptr;
		sf->sfhead.eval[0] = eval[0];
		sf->sfhead.eval[1] = eval[1];
	}
	else if (eptr->relnum == NCLX_MDL_CURVE
			|| eptr->relnum == NCLX_MDL_BSPLINE
			|| eptr->relnum == NCLX_MDL_COMPOSITE)
	{
		cv = (NCLX_mdl_curve *)eptr;
		cv->cvhead.eval = eval[0];
	}
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotGoto(geo,clrec)
**       This function processes a GOTO command.
**    PARAMETERS
**    INPUT  :
**       geo          Point vector to GOTO.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotGoto(geo,clrec)
NCLX_mdl_struct *geo;
NCLX_mot_clrec *clrec;
{
	NCLX_mdl_pntvec *pv;
	double dbuf[6];
	int stat,ix;
/*
.....Set up te GOTO location
........Point Vector
*/
	NclxDbgEnter ("NclxMotGoto(geo,clrec)");
	NclxDbgPdata (0,"geo",geo);
	ix = setjmp(UU_jmpb);
	if (ix != 0)
	{
		return(471);
	}
	if (geo->relnum == NCLX_MDL_PNTVEC)
	{
		pv = (NCLX_mdl_pntvec *)geo;
		dbuf[0] = pv->pt[0]; dbuf[1] = pv->pt[1]; dbuf[2] = pv->pt[2];
		dbuf[3] = pv->vec[0]; dbuf[4] = pv->vec[1]; dbuf[5] = pv->vec[2];
	}
/*
.....Unrecognized geometry type
*/
	else
	{
		return(NCLX_FAILURE);
	}
/*
.....Mark the start of the clfile record
*/
	if (UY_clstart != 0) clrec->start = (char *)UY_clstart;
	else clrec->start = (char *)UN_clpt[0];
	clrec->current = clrec->start;
/*
.....Go to the point
*/
	yfgoto(dbuf,&stat);
/*
.....Mark the end of the clfile record
*/
	UY_clstart = 0;
	if (stat <= 0) clrec->end = (char *)UN_clpt[0];
	else
	{
		clrec->start = UU_NULL;
		clrec->current = UU_NULL;
		clrec->end = UU_NULL;
	}
/*
.....End of routine
*/
	NclxDbgPclrec (1,"clrec",clrec);
	uu_dump_memalloc();
	NclxDbgExit ("NclxMotGoto",stat);
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotFrom(geo,clrec)
**       This function processes a FROM command.
**    PARAMETERS
**    INPUT  :
**       geo          Point vector of FROM.
**    OUTPUT :
**       none
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotFrom (geo)
NCLX_mdl_struct *geo;
{
	NCLX_mdl_pntvec *pv;
	double dbuf[6];
	int stat,ix;
/*
.....Set up te GOTO location
........Point Vector
*/
	NclxDbgEnter ("NclxMotFrom(geo)");
	NclxDbgPdata (0,"geo",geo);
	ix = setjmp(UU_jmpb);
	if (ix != 0)
	{
		return(471);
	}
	if (geo->relnum == NCLX_MDL_PNTVEC)
	{
		pv = (NCLX_mdl_pntvec *)geo;
		dbuf[0] = pv->pt[0]; dbuf[1] = pv->pt[1]; dbuf[2] = pv->pt[2];
		dbuf[3] = pv->vec[0]; dbuf[4] = pv->vec[1]; dbuf[5] = pv->vec[2];
	}
/*
.....Unrecognized geometry type
*/
	else
	{
		return(NCLX_FAILURE);
	}
/*
.....Perform FROM
*/
	yffrom(dbuf,&stat);
/*
.....End of routine
*/
	NclxDbgExit ("NclxMotFrom",stat);
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotDrive (ps,dsatt,dsdir,ds,csatt,cs,clrec)
**       This function processes a simple GOFWD,GOLFT,etc. command.
**    PARAMETERS
**    INPUT  :
**       ps           PS structure.
**       dsatt        DS attributes.
**       dsdir        DS direction.
**       ds           DS structure.
**       csatt        CS attributes.
**       cs           CS structure.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotDrive (ps,dsatt,dsdir,ds,csatt,cs,clrec)
NCLX_mdl_struct *ps,*ds,*cs;
int dsatt,dsdir,csatt;
NCLX_mot_clrec *clrec;
{
   NCLX_mot_cs_rec csrec;
   int status,ix;
   NCLX_mot_cs cs_cntrl;
/*
.....debug stuff
*/
	NclxDbgEnter ("NclxMotDrive (ps,dsatt,dsdir,ds,csatt,cs,clrec)");
	UY_debugDrive = 1;
	NclxDbgPdata (0,"ps",ps);
	NclxDbgPdsat (0,"dsatt",dsatt);
	NclxDbgPdsdir (0,"dsdir",dsdir);
	NclxDbgPdata (0,"ds",ds);
	NclxDbgPcsat (0,"csatt",csatt);
	NclxDbgPdata (0,"cs",cs);
	NclxDbgPmotprm (0,"MOTION PARAMETERS");  
/*
.....end of debug stuff
*/
	ix = setjmp(UU_jmpb);
	if (ix != 0)
	{
		return(471);
	}
   csrec.nrpt_flag = 0;
   csrec.nrpt      = (NCLX_mdl_point *)UU_NULL;
   csrec.nintof    = 1;
   csrec.csatt     = csatt;
   csrec.cs        = (NCLX_mdl_data *)cs;
   csrec.avoid     = 0;
   cs_cntrl.numchk = 1;
   cs_cntrl.csrec  = &csrec;

   status = NclxMotDrive1 (ps,dsatt,dsdir,ds,&cs_cntrl,clrec);

	UY_debugDrive = 0;
	NclxDbgPclrec (1,"clrec",clrec);  
	NclxDbgExit ("NclxMotDrive",status);

   return(status);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotDrive1 (ps,dsatt,dsdir,ds,cscntl,clrec)
**       This function processes a GOFWD,GOLFT,etc. command.
**    PARAMETERS
**    INPUT  :
**       ps           PS structure.
**       dsatt        DS attributes.
**       dsdir        DS direction.
**       ds           DS structure.
**       cscntl       CS structure.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotDrive1 (ps,dsatt,dsdir,ds,cscntl,clrec)
NCLX_mdl_struct *ps;
NCLX_mdl_struct *ds;
NCLX_mot_cs *cscntl;
int dsatt,dsdir;
NCLX_mot_clrec *clrec;
{
	int i, ix, ncs, stat, ixx;
	int psgeom[4],dsgeom[4],csgeom[32],nearpt[8];
	double eval[22];
	NCLX_mdl_data *cs;
	NCLX_mot_cs_rec *csrec;
	NCLX_mdl_netsf *netsfptr;

/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   int stims,stimm,etims,etimm;

/*
.....debug stuff
*/
	NclxDbgEnter ("NclxMotDrive1 (ps,dsatt,dsdir,ds,cscntl,clrec)");
	if (UY_debugDrive == 0)
	{
		NclxDbgPdata (0,"ps",ps);
		NclxDbgPdsat (0,"dsatt",dsatt);
		NclxDbgPdsdir (0,"dsdir",dsdir);
		NclxDbgPdata (0,"ds",ds);
		NclxDbgPcscntl (0,"cscntl",cscntl);
	}
	if (UY_debugDrive == 0)
		NclxDbgPmotprm (0,"MOTION PARAMETERS");
/*
.....end of debug stuff
*/
	ixx = setjmp(UU_jmpb);
	if (ixx != 0)
	{
		return(471);
	}
/*
.....Mark the start of the clfile record
*/
	if (UY_clstart != 0) clrec->start = (char *)UY_clstart;
	else clrec->start = (char *)UN_clpt[0];
	clrec->current = clrec->start;
/*
.....Set up the PS, DS, CS geometry
*/
	if (ps->relnum == NCLX_MDL_NETSF)
	{
		netsfptr = (NCLX_mdl_netsf *)ps;
		UY_nps = netsfptr->nsf;
		if (UY_nps > 1)
		{
			UY_ps = ps;
			UY_netps = netsfptr->sfptr;
		}
		else
			UY_ps = (NCLX_mdl_struct *)*netsfptr->sfptr;
	}
	else
	{
/*
... aak 05-dec-1997: added composite CVonSF as multi-PS
*/
		if(ps->relnum == NCLX_MDL_COMPOSITE)
		{
			NCLX_mdl_composite *cvsfcomp;
			cvsfcomp = (NCLX_mdl_composite *)ps;
			UY_nps = cvsfcomp->ncurve;
		}
		else UY_nps = 1;

		UY_ps = ps;
	}
/*
.....DS is a NET surface
*/
	if (ds->relnum == NCLX_MDL_NETSF)
	{
		netsfptr = (NCLX_mdl_netsf *)ds;
		UY_nds = netsfptr->nsf;
		if (UY_nds > 1)
		{
			UY_ds = ds;
			UY_netds = netsfptr->sfptr;
		}
		else
			UY_ds = (NCLX_mdl_struct *)*netsfptr->sfptr;
	}
	else
	{
		UY_nds = 1;
		UY_ds = ds;
	}

	psgeom[0] = ps->key;
	psgeom[1] = NclxMdlToNclType(ps->relnum);
	dsgeom[0] = ds->key;
	dsgeom[1] = NclxMdlToNclType(ds->relnum);
	dsgeom[2] = dsatt;
/*	dsgeom[2] = -1;		 ERROR - CHANGE THIS BACK */
	dsgeom[3] = dsdir;
	csrec = cscntl->csrec;
	ncs = cscntl->numchk;
	if (ncs > 5) ncs = 5;
	csgeom[0] = ncs;
	ix = 1;
	for (i=0;i<ncs;i++,csrec++,ix+=5)
	{
		cs = csrec->cs;
		csgeom[ix] = cs->data.header.key;
		csgeom[ix+1] = NclxMdlToNclType(cs->data.header.relnum);
		csgeom[ix+2] = csrec->csatt;
		csgeom[ix+3] = csrec->nintof;
		if (csrec->avoid)
		  csgeom[ix+4] = 1;
		else
		  csgeom[ix+4] = 0;
		UY_cs[i] = (NCLX_mdl_struct *)cs;
		if (csrec->nrpt_flag)
		{
			UY_nearpt[i] = csrec->nrpt;
			nearpt[i]  = csrec->nrpt->header.key;
		}
		else
		{
			UY_nearpt[i] = (NCLX_mdl_point *)UU_NULL;
			nearpt[i]  = 0;
		}
/*
.....Store the previously evaluated UV parameters for check surfaces.
*/
		NclxSetuv ((NCLX_mdl_struct *)cs, &eval[2*i+4]);
	}
	UY_ics = 0;
	UY_ncs = ncs;
	if (UY_hldgeo != 0)
	{
		UY_cs[ncs] = UY_hldgeo;
		UY_ncs++;
	}
	UY_hldps = UY_ps;
	UY_hldds = UY_ds;
	UY_hldcs = UY_cs[0];
/*
.....Store the previously evaluated UV parameters for part and drive surfaces.
*/
	NclxSetuv (ps, &eval[0]);
	NclxSetuv (ds, &eval[2]);
/*
.....Drive geometry
*/
	ix = 0;
	NCLX_internal_geom = NCLX_TRUE;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   ncall = 0;
	etim = 0;
   gtimx(&stims,&stimm);

	yfgoxx(psgeom,dsgeom,csgeom,nearpt,eval,&ix,&stat);

/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   gtimx(&etims,&etimm);
   stims = (etims-stims)*1000 + (etimm - stimm);
	NclxDbgEvalTime (&stims,&ncall,&etim);

	NCLX_internal_geom = NCLX_FALSE;
	cscntl->wchk = ix;
/*
.....Mark the end of the clfile record
*/
	UY_clstart = 0;
	if (stat <= 0)
	{
		clrec->end = (char *)UN_clpt[0];
/*
.....Store the last evaluated UV parameters
*/
		NclxGetuv (eval, ps);
		NclxGetuv (&eval[2], ds);
		csrec = &cscntl->csrec[ix];
		cs = csrec->cs;
		NclxGetuv (&eval[2*ix+4], (NCLX_mdl_struct *)cs);
	}
	else
	{
		clrec->start = UU_NULL;
		clrec->current = UU_NULL;
		clrec->end = UU_NULL;
	}
/*
.....Fillet test
*/
/*	filet++;
	if (filet == 1) cs1 = clrec->start;
	else if (filet == 5)
	{
		corn.clrange = *clrec;
		corn.clrange.start = cs1;
		print_cl(&corn.clrange);
		corn.rad = 5.0;
		corn.tol = 0.01;
		corn.fmax = 0.;
		corn.fedrt = 0.;
		corn.direction = NCLX_TLLFT;
		corn.cdia = 10.;
		NclxMotFillet(&corn);
		print_cl(&corn.clrange);
	}*/
/*
.....End of routine
*/
	if (UY_debugDrive == 0)
	{
		NclxDbgPclrec (1,"clrec",clrec);
	}
	if (stat <= 0)
	{
		NclxDbgPmsfdat (1,"ps",ps);
		NclxDbgPmsfdat (1,"ds",ds);
		csrec = &cscntl->csrec[ix];
		cs = csrec->cs;
		NclxDbgPmsfdat (1,"cs",cs);
		NclxDbgPcscntl (1,"cscntl",cscntl);
	}
	uu_dump_memalloc();
	NclxDbgExit ("NclxMotDrive1",stat);
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotGo1(dsatt,ds,clrec)
**       This function processes a single surface GO.
**    PARAMETERS
**    INPUT  :
**       dsatt        DS attributes.
**       ds           DS structure.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotGo1(dsatt,ds,clrec)
NCLX_mdl_struct *ds;
int dsatt;
NCLX_mot_clrec *clrec;
{
	int stat,ix;
	int nsrfs,psgeom[4],dsgeom[4],csgeom[4];
	double eval[6];
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   int stims,stimm,etims,etimm;


	NclxDbgEnter ("NclxMotGo1 (dsatt,ds,clrec)");
	NclxDbgPdsat (0,"dsatt",dsatt);
	NclxDbgPdata (0,"ds",ds);
	NclxDbgPmotprm (0,"MOTION PARAMETERS");

	nsrfs = 1;
	ix = setjmp(UU_jmpb);
	if (ix != 0)
	{
		return(471);
	}
/*
.....Mark the start of the clfile record
*/
	if (UY_clstart != 0) clrec->start = (char *)UY_clstart;
	else clrec->start = (char *)UN_clpt[0];
	clrec->current = clrec->start;
/*
.....Set up the PS, DS, CS geometry
*/
	UY_ds = ds;
	UY_ics = 0;
	UY_ncs = 1;
	UY_cs[0] = ds;
	if (UY_hldgeo)
	{
		UY_cs[UY_ncs] = UY_hldgeo;
		UY_ncs++;
	}
	dsgeom[0] = ds->key;
	dsgeom[1] = NclxMdlToNclType(ds->relnum);
	dsgeom[2] = dsatt;
/*
.....Store the previously evaluated UV parameters
*/
	NclxSetuv (ds, &eval[2]);
/*
.....Drive geometry
*/
	NCLX_internal_geom = NCLX_TRUE;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   ncall = 0;
   etim = 0;
   gtimx(&stims,&stimm);

	yfgosf(&nsrfs,psgeom,dsgeom,csgeom,eval,&stat);

/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   gtimx(&etims,&etimm);
   stims = (etims-stims)*1000 + (etimm - stimm);
   NclxDbgEvalTime (&stims,&ncall,&etim);

	NCLX_internal_geom = NCLX_FALSE;
/*
.....Mark the end of the clfile record
*/
	UY_clstart = 0;
	if (stat <= 0) clrec->end = (char *)UN_clpt[0];
	else
	{
		clrec->start = UU_NULL;
		clrec->current = UU_NULL;
		clrec->end = UU_NULL;
	}
/*
.....Store the last evaluated UV parameters
*/
	NclxGetuv (&eval[2], ds);
/*
.....End of routine
*/
	if (stat <= 0) NclxDbgPmsfdat (1,"ds",ds);
	NclxDbgPclrec (1,"clrec",clrec);
	uu_dump_memalloc();
	NclxDbgExit ("NclxMotGo1",stat);
	return(stat);
}
/*********************************************************************
**    E_FUNCTION     : int NclxMotGo2(ps,dsatt,ds,clrec)
**       This function processes a two surface GO.
**    PARAMETERS
**    INPUT  :
**       ps           PS structure.
**       dsatt        DS attributes.
**       ds           DS structure.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotGo2(ps,dsatt,ds,clrec)
NCLX_mdl_struct *ps,*ds;
int dsatt;
NCLX_mot_clrec *clrec;
{
	int stat,ix;
	int nsrfs,psgeom[4],dsgeom[4],csgeom[4];
	double eval[6];
	NCLX_mdl_netsf *netsfptr;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   int stims,stimm,etims,etimm;


	NclxDbgEnter ("NclxMotGo2 (ps,dsatt,ds,clrec)");
	NclxDbgPdata (0,"ps",ps);
	NclxDbgPdsat (0,"dsatt",dsatt);
	NclxDbgPdata (0,"ds",ds);
	NclxDbgPmotprm (0,"MOTION PARAMETERS");

	nsrfs = 2;
	ix = setjmp(UU_jmpb);
	if (ix != 0)
	{
		return(471);
	}
/*
.....Mark the start of the clfile record
*/
	if (UY_clstart != 0) clrec->start = (char *)UY_clstart;
	else clrec->start = (char *)UN_clpt[0];
	clrec->current = clrec->start;
/*
.....Set up the PS, DS, CS geometry
*/
	if (ps->relnum == NCLX_MDL_NETSF)
	{
		netsfptr = (NCLX_mdl_netsf *)ps;
		UY_nps = netsfptr->nsf;
		if (UY_nps > 1)
		{
			UY_ps = ps;
			UY_netps = netsfptr->sfptr;
		}
		else
			UY_ps = (NCLX_mdl_struct *)*netsfptr->sfptr;
	}
	else
	{
/*
... aak 06-march-1998: added composite CVonSF as multi-PS
*/
      if(ps->relnum == NCLX_MDL_COMPOSITE)
      {
         NCLX_mdl_composite *cvsfcomp;
         cvsfcomp = (NCLX_mdl_composite *)ps;
         UY_nps = cvsfcomp->ncurve;
      }
      else UY_nps = 1;

		UY_ps = ps;
	}
/*
.....DS is a NET surface
*/
	if (ds->relnum == NCLX_MDL_NETSF)
	{
		netsfptr = (NCLX_mdl_netsf *)ds;
		UY_nds = netsfptr->nsf;
		if (UY_nds > 1)
		{
			UY_ds = ds;
			UY_netds = netsfptr->sfptr;
		}
		else
			UY_ds = (NCLX_mdl_struct *)*netsfptr->sfptr;
	}
	else
	{
		UY_nds = 1;
		UY_ds = ds;
	}

	UY_ics = 0;
	UY_ncs = 1;
	UY_cs[0] = ds;
	if (UY_hldgeo)
	{
		UY_cs[UY_ncs] = UY_hldgeo;
		UY_ncs++;
	}
	psgeom[0] = ps->key;
	psgeom[1] = NclxMdlToNclType(ps->relnum);
	dsgeom[0] = ds->key;
	dsgeom[1] = NclxMdlToNclType(ds->relnum);
	dsgeom[2] = dsatt;
	csgeom[0] = dsgeom[0];
	csgeom[1] = dsgeom[1];
	csgeom[2] = dsgeom[2];
	UY_hldps = UY_ps;
	UY_hldds = UY_ds;
	UY_hldcs = UY_cs[0];
/*
.....Store the previously evaluated UV parameters
*/
	NclxSetuv (ps, &eval[0]);
	NclxSetuv (ds, &eval[2]);
	NclxSetuv (UY_cs[0], &eval[4]);
/*
.....Drive geometry
*/
	NCLX_internal_geom = NCLX_TRUE;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
	ncall = 0;
	etim = 0;
	gtimx(&stims,&stimm);

	yfgosf(&nsrfs,psgeom,dsgeom,csgeom,eval,&stat);

/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   gtimx(&etims,&etimm);
   stims = (etims-stims)*1000 + (etimm - stimm);
   NclxDbgEvalTime (&stims,&ncall,&etim);

	NCLX_internal_geom = NCLX_FALSE;
/*
.....Mark the end of the clfile record
*/
	UY_clstart = 0;
	if (stat <= 0) clrec->end = (char *)UN_clpt[0];
	else
	{
		clrec->start = UU_NULL;
		clrec->current = UU_NULL;
		clrec->end = UU_NULL;
	}
/*
.....Store the last evaluated UV parameters
*/
	NclxGetuv (&eval[0], ps);
	NclxGetuv (&eval[2], ds);
/*
.....End of routine
*/
	if (stat <= 0)
	{
		NclxDbgPmsfdat (1,"ps",ps);
		NclxDbgPmsfdat (1,"ds",ds);
	}
	NclxDbgPclrec (1,"clrec",clrec);
	uu_dump_memalloc();
	NclxDbgExit ("NclxMotGo2",stat);
	return(stat);
}
/*********************************************************************
**    E_FUNCTION     : int NclxMotGo3(ps,dsatt,ds,csatt,cs,clrec)
**       This function processes a three surface GO.
**    PARAMETERS
**    INPUT  :
**       ps           PS structure.
**       dsatt        DS attributes.
**       ds           DS structure.
**       csatt        CS attributes.
**       cs           CS structure.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotGo3(ps,dsatt,ds,csatt,cs,clrec)
NCLX_mdl_struct *ps,*ds,*cs;
int dsatt,csatt;
NCLX_mot_clrec *clrec;
{
	int stat,ix;
	int nsrfs,psgeom[4],dsgeom[4],csgeom[4];
	double eval[6];
	NCLX_mdl_netsf *netsfptr;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   int stims,stimm,etims,etimm;


	NclxDbgEnter ("NclxMotGo3 (ps,dsatt,ds,csatt,cs,clrec)");
	NclxDbgPdata (0,"ps",ps);
	NclxDbgPdsat (0,"dsatt",dsatt);
	NclxDbgPdata (0,"ds",ds);
	NclxDbgPcsat (0,"csatt",csatt);
   NclxDbgPdata (0,"cs",cs);
	NclxDbgPmotprm (0,"MOTION PARAMETERS");

	nsrfs = 3;
	ix = setjmp(UU_jmpb);
	if (ix != 0)
	{
		return(471);
	}
/*
.....Mark the start of the clfile record
*/
	if (UY_clstart != 0) clrec->start = (char *)UY_clstart;
	else clrec->start = (char *)UN_clpt[0];
	clrec->current = clrec->start;
/*
.....Set up the PS, DS, CS geometry
*/
	if (ps->relnum == NCLX_MDL_NETSF)
	{
		netsfptr = (NCLX_mdl_netsf *)ps;
		UY_nps = netsfptr->nsf;
		if (UY_nps > 1)
		{
			UY_ps = ps;
			UY_netps = netsfptr->sfptr;
		}
		else
			UY_ps = (NCLX_mdl_struct *)*netsfptr->sfptr;
	}
	else
	{
/*
... aak 06-march-1998: added composite CVonSF as multi-PS
*/
      if(ps->relnum == NCLX_MDL_COMPOSITE)
      {
         NCLX_mdl_composite *cvsfcomp;
         cvsfcomp = (NCLX_mdl_composite *)ps;
         UY_nps = cvsfcomp->ncurve;
      }
      else UY_nps = 1;

		UY_ps = ps;
	}
/*
.....DS is a NET surface
*/
	if (ds->relnum == NCLX_MDL_NETSF)
	{
		netsfptr = (NCLX_mdl_netsf *)ds;
		UY_nds = netsfptr->nsf;
		if (UY_nds > 1)
		{
			UY_ds = ds;
			UY_netds = netsfptr->sfptr;
		}
		else
			UY_ds = (NCLX_mdl_struct *)*netsfptr->sfptr;
	}
	else
	{
		UY_nds = 1;
		UY_ds = ds;
	}

	UY_cs[0] = cs;
	UY_ics = 0;
	UY_ncs = 1;
	if (UY_hldgeo)
	{
		UY_cs[UY_ncs] = UY_hldgeo;
		UY_ncs++;
	}
	psgeom[0] = ps->key;
	psgeom[1] = NclxMdlToNclType(ps->relnum);
	dsgeom[0] = ds->key;
	dsgeom[1] = NclxMdlToNclType(ds->relnum);
	dsgeom[2] = dsatt;
	csgeom[0] = cs->key;
	csgeom[1] = NclxMdlToNclType(cs->relnum);
	csgeom[2] = csatt;
	UY_hldps = UY_ps;
	UY_hldds = UY_ds;
	UY_hldcs = UY_cs[0];
/*
.....Store the previously evaluated UV parameters
*/
	NclxSetuv (ps, eval);
	NclxSetuv (ds, &eval[2]);
	NclxSetuv (cs, &eval[4]);
/*
.....Drive geometry
*/
	NCLX_internal_geom = NCLX_TRUE;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   ncall = 0;
   etim = 0;
   gtimx(&stims,&stimm);

	yfgosf(&nsrfs,psgeom,dsgeom,csgeom,eval,&stat);

/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   gtimx(&etims,&etimm);
   stims = (etims-stims)*1000 + (etimm - stimm);
   NclxDbgEvalTime (&stims,&ncall,&etim);

	NCLX_internal_geom = NCLX_FALSE;
/*
.....Mark the end of the clfile record
*/
	UY_clstart = 0;
	if (stat <= 0) clrec->end = (char *)UN_clpt[0];
	else
	{
		clrec->start = UU_NULL;
		clrec->current = UU_NULL;
		clrec->end = UU_NULL;
	}
/*
.....Store the last evaluated UV parameters
*/
	NclxGetuv (&eval[0], ps);
	NclxGetuv (&eval[2], ds);
	NclxGetuv (&eval[4], cs);
/*
.....End of routine
*/
	if (stat <= 0)
	{
		NclxDbgPmsfdat (1,"ps",ps);
		NclxDbgPmsfdat (1,"ds",ds);
		NclxDbgPmsfdat (1,"cs",cs);
	}
	NclxDbgPclrec (1,"clrec",clrec);
	uu_dump_memalloc();
	NclxDbgExit ("NclxMotGo3",stat);

	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotDriveCurve (dsatt,ds,clrec)
**       This function processes a drive/ps curve command.
**    PARAMETERS
**    INPUT  :
**       dsatt        DS attributes.
**       ds           DS/PS structure.
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotDriveCurve (dsatt,ds,clrec)
int dsatt;
NCLX_mdl_struct *ds;
NCLX_mot_clrec *clrec;
{
	int istat, dsgeom[4],ix;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   int stims,stimm,etims,etimm;
/*
.....debug stuff
*/
	NclxDbgEnter ("NclxMotDriveCurve (dsatt,ds,clrec)");
	NclxDbgPdsat (0,"dsatt",dsatt);
	NclxDbgPdata (0,"ds",ds);
	NclxDbgPmotprm (0,"MOTION PARAMETERS");
	ix = setjmp(UU_jmpb);
	if (ix != 0)
	{
		return(471);
	}
/*
.....Mark the start of the clfile record
*/
	if (UY_clstart != 0) clrec->start = (char *)UY_clstart;
	else clrec->start = (char *)UN_clpt[0];
	clrec->current = clrec->start;
/*
.....Set up the PS, DS, CS geometry
*/
	UY_ps = (NCLX_mdl_struct *)UU_NULL;
	UY_ds = ds;
	UY_ncs = 0;
	dsgeom[0] = ds->key;
	dsgeom[1] = NclxMdlToNclType(ds->relnum);
	dsgeom[2] = dsatt;
	UY_hldps = UY_ps;
	UY_hldds = UY_ds;
	UY_hldcs = UY_cs[0];
/*
.....Drive geometry
*/
	NCLX_internal_geom = NCLX_TRUE;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   ncall = 0;
   etim = 0;
   gtimx(&stims,&stimm);

	yfdrcv(dsgeom,&istat);
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   gtimx(&etims,&etimm);
   stims = (etims-stims)*1000 + (etimm - stimm);
   NclxDbgEvalTime (&stims,&ncall,&etim);

	NCLX_internal_geom = NCLX_FALSE;
/*
.....Mark the end of the clfile record
*/
	UY_clstart = 0;
	if (istat <= 0)
	{
		clrec->end = (char *)UN_clpt[0];
	}
	else
	{
		clrec->start = UU_NULL;
		clrec->current = UU_NULL;
		clrec->end = UU_NULL;
	}

	NclxDbgPclrec (1,"clrec",clrec);
	uu_dump_memalloc();
	NclxDbgExit ("NclxMotDriveCurve",istat);
	return (istat);
}

/*********************************************************************
**    E_FUNCTION     : int NclxMotDriveAuto (ps,dsatt,dscntl,csrec,clrec)
**       This function processes a AutoGofwd command.
**    PARAMETERS
**    INPUT  :
**       ps           PS structure.
**       dsatt        DS attributes.
**       dscntl       DS structure.
**       csrec        Final CS structure. 
**    OUTPUT :
**       clrec        Clfile record.
**    RETURNS      :
**       Non-zero on error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int NclxMotDriveAuto (ps,dsatt,dscntl,cscntl,clrec)
NCLX_mdl_struct *ps;
int dsatt;
NCLX_mot_ds *dscntl;
NCLX_mot_cs *cscntl;
NCLX_mot_clrec *clrec;
{
	int i, j, k, ix, nds, ncs, stat, ixx;
/*
... max allowed nds = 500, ncs = 5.
*/  
	int psgeom[4],dsgeom[8],csgeom[2520],nearpt[504];
	double eval[1012];
	NCLX_mdl_data *cs;
	NCLX_mot_cs_rec *csrec;
	NCLX_mdl_netsf *netsfptr;
 	int stims,stimm,etims,etimm;
/*
.....debug stuff
*/
        NclxDbgEnter ("NclxMotDriveAuto (ps,dsatt,dscntl,cscntl,clrec)");
        if (UY_debugDrive == 0)
        {
                NclxDbgPdata (0,"ps",ps);
                NclxDbgPdsat (0,"dsatt",dsatt);
                NclxDbgPdscntl (0,"dscntl",dscntl);
                NclxDbgPcscntl (0,"cscntl",cscntl);
        }
        if (UY_debugDrive == 0)
                NclxDbgPmotprm (0,"MOTION PARAMETERS");
/*
.....end of debug stuff
*/
        ixx = setjmp(UU_jmpb);
        if (ixx != 0)
        {
                return(471);
        }
/*
.....Mark the start of the clfile record
*/
        if (UY_clstart != 0) clrec->start = (char *)UY_clstart;
        else clrec->start = (char *)UN_clpt[0];
        clrec->current = clrec->start;
/*
.....Set up the PS, DS, CS geometry
*/
        if (ps->relnum == NCLX_MDL_NETSF)
        {
                netsfptr = (NCLX_mdl_netsf *)ps;
                UY_nps = netsfptr->nsf;
                if (UY_nps > 1)
                {
                        UY_ps = ps;
                        UY_netps = netsfptr->sfptr;
                }
                else
                        UY_ps = (NCLX_mdl_struct *)*netsfptr->sfptr;
        }
        else
        {
                if(ps->relnum == NCLX_MDL_COMPOSITE)
                {
                        NCLX_mdl_composite *cvsfcomp;
                        cvsfcomp = (NCLX_mdl_composite *)ps;
                        UY_nps = cvsfcomp->ncurve;
                }
                else UY_nps = 1;

                UY_ps = ps;
        }
	psgeom[0] = ps->key;
	psgeom[1] = NclxMdlToNclType(ps->relnum);
	dsgeom[0] = dsatt;
	nds = dscntl->numds;
	if (nds > 500) nds = 500;
	dsgeom[1] = nds;
	dsgeom[2] = dscntl->look;
	if (dscntl->look <1) dsgeom[2] = 1;
	if (dscntl->look >5) dsgeom[2] = 5;
/*
.. First drive surface
*/
	csrec = dscntl->dsrec;
	cs = csrec->cs;
	if (nds == 1 && cs->data.header.relnum == NCLX_MDL_COMPOSITE)
	{
		dsgeom[2] = 0;
		UY_cs[0] = (NCLX_mdl_struct *)cs;
	}
	dsgeom[3] = cs->data.header.key;
	dsgeom[4] = NclxMdlToNclType(cs->data.header.relnum);
	UY_island[0] = (NCLX_mdl_struct *)cs;
	NclxSetuv ((NCLX_mdl_struct *)cs, &eval[2]);
/*
.. The rest of the drive surfaces
*/
	csrec++;
	ix = 0;
	for (i=1;i<nds;i++,csrec++,ix+=5)
	{
		cs = csrec->cs;
		csgeom[ix] = cs->data.header.key;
		csgeom[ix+1] = NclxMdlToNclType(cs->data.header.relnum);
		csgeom[ix+2] = csrec->csatt;
		csgeom[ix+3] = csrec->nintof;
		if (csrec->avoid)
			csgeom[ix+4] = 1;
		else
			csgeom[ix+4] = 0;
		UY_island[i] = (NCLX_mdl_struct *)cs;
		k = i-1;
		if (csrec->nrpt_flag)
		{
			UY_nearpt[k] = csrec->nrpt;
			nearpt[k]  = csrec->nrpt->header.key;
		}
		else
		{
			UY_nearpt[k] = (NCLX_mdl_point *)UU_NULL;
			nearpt[k]  = 0;
		}
		NclxSetuv ((NCLX_mdl_struct *)cs, &eval[2*i+2]);
	}
/*
... The final check surfaces.
*/
        csrec = cscntl->csrec;
        ncs = cscntl->numchk;
        if (ncs > 5) ncs = 5;
        for (j=0;j<ncs;j++,csrec++,i++,ix+=5)
        {
                cs = csrec->cs;
                csgeom[ix] = cs->data.header.key;
                csgeom[ix+1] = NclxMdlToNclType(cs->data.header.relnum);
                csgeom[ix+2] = csrec->csatt;
                csgeom[ix+3] = csrec->nintof;
                if (csrec->avoid)
                  csgeom[ix+4] = 1;
                else
                  csgeom[ix+4] = 0;
                UY_island[i] = (NCLX_mdl_struct *)cs;
                k = i -1;
                if (csrec->nrpt_flag)
                {
                        UY_nearpt[k] = csrec->nrpt;
                        nearpt[k]  = csrec->nrpt->header.key;
                }
                else
                {
                        UY_nearpt[k] = (NCLX_mdl_point *)UU_NULL;
                        nearpt[k]  = 0;
                }
                NclxSetuv ((NCLX_mdl_struct *)cs, &eval[2*i+2]);
        }

        UY_nislands = nds + ncs;
        if (UY_hldgeo != 0)
        {
                UY_island[i] = UY_hldgeo;
                UY_nislands ++;
        }
        UY_ncs = 0;
        UY_hldps = UY_ps;
/*
.....Store the previously evaluated UV parameters for part surfaces.
*/
        NclxSetuv (ps, &eval[0]);
/*
.....Drive geometry
*/
        ix = 0;
        NCLX_internal_geom = NCLX_TRUE;
        ncall = 0;
        etim = 0;
        gtimx(&stims,&stimm);

        yfgoauto(psgeom,dsgeom,csgeom,nearpt,eval,&ncs,&ix,&stat);

        gtimx(&etims,&etimm);
        stims = (etims-stims)*1000 + (etimm - stimm);
        NclxDbgEvalTime (&stims,&ncall,&etim);

        NCLX_internal_geom = NCLX_FALSE;
        cscntl->wchk = ix;
/*
.....Mark the end of the clfile record
*/
        UY_clstart = 0;
        if (stat <= 0)
        {
                clrec->end = (char *)UN_clpt[0];
/*
.....Store the last evaluated UV parameters
*/
                NclxGetuv (eval, ps);
                csrec = &cscntl->csrec[ix];
                cs = csrec->cs;
                NclxGetuv (&eval[2*(nds+ix)+2], (NCLX_mdl_struct *)cs);
        }
        else
        {
                clrec->start = UU_NULL;
                clrec->current = UU_NULL;
                clrec->end = UU_NULL;
        }
/*
.....End of routine
*/
        UY_nislands = 0;
        if (UY_debugDrive == 0)
        {
                NclxDbgPclrec (1,"clrec",clrec);
        }
        if (stat <= 0)
        {
                NclxDbgPmsfdat (1,"ps",ps);
                NclxDbgPdscntl (1,"dscntl",dscntl);
                csrec = &cscntl->csrec[ix];
                cs = csrec->cs;
                NclxDbgPmsfdat (1,"cs",cs);
                NclxDbgPcscntl (1,"cscntl",cscntl);
        }
        uu_dump_memalloc();
        NclxDbgExit ("NclxMotDriveAuto",stat);
        return(stat);
}
