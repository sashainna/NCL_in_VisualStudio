/*******************************************************************
**    NAME         :  yuser.c
**       CONTAINS:
**
**				my_gofwd_auto(narg,args);
**				my_drive_motion
**				my_scrub
**				my_rmill
**				my_apocket
**				my_lathe_rough
**				my_lathe_finish
**				my_get_drive_parms
**				my_get_surface
**				my_get_modifier
**				my_pick_id
**				my_patern_rec
**				my_polyline_rec
**				my_shape_rec
**          my_surf_eval
**
**    COPYRIGHT 1997 (c) Numerical Control Computer Sciences Inc.
**              All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       yuser.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:01
*********************************************************************/

#include "usysdef.h"
#include "uhep.h"
#include "class.h"
#include "dasg.h"
#include "dasnog.h"
#include "ginqatt.h"
#include "gtbl.h"
#include "gobas.h"
#include "go3.h"
#include "mdattr.h"
#include "m2dattr.h"
#include "mdcoord.h"
#include "mdpick.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include "nkeywd.h"
#include "nclinp.h"
#include "mfort.h"
#include "nclfc.h"
#include "ycom.h"
#include <math.h>

extern UD_POPUPREC nmodfy[];
extern int NCLX_internal_geom;

/*********************************************************************
**    FUNCTION     : int my_gofwd_auto (narg,args)
**       This function is used as the user interface to the OpenNCL
**                      drive routine 'NclxMotDriveAuto'.
**    PARAMETERS
**    INPUT  :
**       narg         Number of arguments provided.
**       args         Actual arguments.
**    OUTPUT :
**                      none.
**    RETURNS      :
**       NCLX_FAILURE on failure, NCLX_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int my_gofwd_auto (narg,args)
int narg;
char args[35][64];
{
	int status = NCLX_FAILURE;
	NCLX_mdl_type relnum;
	UU_KEY_ID key;
	NCLX_mdl_data ps;
	NCLX_mdl_data ds,cs[505];
	NCLX_mot_clrec clrec;
	NCLX_mdl_data dsgeo, csgeo;
	NCLX_mdl_surf *sf;
	NCLX_mot_cs cscntl;
	NCLX_mot_ds dscntl;
	NCLX_mot_cs_rec csrec[505];
	double val;
	int cond, cs_cond,cs_avoid;
	int nds,nck,i,inc,n,lchk,k;
/*
.....No parameters given
.....Prompt the user for all paramters
*/
	n = narg;
	status = UU_SUCCESS;
	if (n == 0) status = my_get_drive_parms(args,&n);
	if (status == UU_FAILURE) return(status);
/*
.....Get part surface
*/
	NclxMdlInquire(args[0],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ps);
/*
.....Get tool condition
*/
	inc = 1;
	cond = NCLX_TLON;
	if (strcmp(args[inc],"TLON") == 0)
	{
	        cond = NCLX_TLON;
	        inc++;
	}
	else if (strcmp(args[inc],"TLRGT") == 0)
	{
	        cond = NCLX_TLRGT;
	        inc++;
	}
	else if (strcmp(args[inc],"TLLFT") == 0)
	{
	        cond = NCLX_TLLFT;
	        inc++;
	}
/*
.....Get check surface condition
*/
	nds = 0;
	lchk = 0;
	nck = 1;
	for (i=inc;inc<n && !lchk;i++)
	{
/*
...ds look ahead
*/
	        if (strcmp(args[inc],"LOOK") == 0)
	        {
	                inc++;
	                nck = atoi(args[inc]);
	                inc++;
	        }
/*
..cs avoid
*/
	        cs_avoid = 0;
	        if (strcmp(args[inc],"AVOID") == 0)
	        {
	                cs_avoid = 1;
	                inc++;
	        }
/*
... cs modifier
*/
	        cs_cond = NCLX_AUTO;
	        if (strcmp(args[inc],"TO") == 0)
	        {
	                cs_cond = NCLX_TO;
	                inc++;
	        }
	        else if (strcmp(args[inc],"PAST") == 0)
	        {
	                cs_cond = NCLX_PAST;
	                inc++;
	        }
	        else if (strcmp(args[inc],"ON") == 0)
	        {
	                cs_cond = NCLX_ON;
	                inc++;
	        }
	        else if (strcmp(args[inc],"TANTO") == 0)
	        {
	                cs_cond = NCLX_TANTO;
	                inc++;
	        }
	        csrec[nds].nrpt_flag = 0;
	        csrec[nds].nintof = 1;
	        csrec[nds].csatt = cs_cond;
	        csrec[nds].avoid = cs_avoid;
	        csrec[nds].cs = &cs[nds];
/*
.....Get drive surface
*/
	        NclxMdlInquire(args[inc],&relnum,&key,&val);
	        status = NclxMdlGetGeom (key,&cs[nds]);
	        nds++;
	        inc ++;
	        if (strcmp(args[inc],"CHECK") == 0)
	        {
	                lchk = 1;
	                inc ++;
	        }
	}
	dscntl.numds = nds;
	dscntl.look = nck;
	dscntl.dsrec = &csrec[0];
/*
.....Get check surface condition
*/
	if (lchk)
	{
	        nck = 0;
	        for (i=inc;inc<n;i++)
	        {
/*
..cs avoid
*/
	                cs_avoid = 0;
	                if (strcmp(args[inc],"AVOID") == 0)
	                {
	                        cs_avoid = 1;
	                        inc++;
	                }
/*
... cs modifier
*/
	                cs_cond = 0;
	                if (strcmp(args[inc],"TO") == 0)
	                {
	                        cs_cond = NCLX_TO;
	                        inc++;
	                }
	                else if (strcmp(args[inc],"PAST") == 0)
	                {
	                        cs_cond = NCLX_PAST;
	                        inc++;
	                }
	                else if (strcmp(args[inc],"ON") == 0)
	                {
	                        cs_cond = NCLX_ON;
	                        inc++;
	                }
	                else if (strcmp(args[inc],"TANTO") == 0)
	                {
	                        cs_cond = NCLX_TANTO;
	                        inc++;
	                }
	                k = nds + nck;
	                csrec[k].nrpt_flag = 0;
	                csrec[k].nintof = 1;
	                csrec[k].csatt = cs_cond;
	                csrec[k].cs = &cs[k];
/*
.....Get check surface
*/
	                NclxMdlInquire(args[inc],&relnum,&key,&val);
	                status = NclxMdlGetGeom (key,&cs[k]);
	                nck ++;
	                inc ++;
	        }
	        cscntl.numchk = nck;
	        cscntl.csrec = &csrec[nds];
	}
/*
... No "CHECK" specified, the last DS will be final CS.
*/
	else
	{
		if (nds > 1)
		{
	        cscntl.numchk = 1;
	        nds = nds -1;
	        cscntl.csrec = &dscntl.dsrec[nds];
	        dscntl.numds = nds;
		}
		else
			cscntl.numchk = 0;
	}
	
/*
.....Drive the geometry
*/
	if (status == NCLX_SUCCESS)
	{
	        status = NclxMotDriveAuto(&ps,cond,&dscntl,&cscntl,&clrec);
	}
done:;
	return(status);
}
/*********************************************************************
**    FUNCTION     : int my_drive_motion (narg,args)
**       This function is used as the user interface to the OpenNCL
**			drive routine 'NclxMotDrive1'.
**    PARAMETERS
**    INPUT  :
**       narg         Number of arguments provided.
**       args         Actual arguments.
**    OUTPUT :
**			none.
**    RETURNS      :
**       NCLX_FAILURE on failure, NCLX_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int my_drive_motion (narg,args)
int narg;
char args[35][64];
{
	int status = NCLX_FAILURE;
	NCLX_mdl_type relnum;
	UU_KEY_ID key;
	NCLX_mdl_data ps;
	NCLX_mdl_data ds,cs[5];
	NCLX_mot_clrec clrec;
	NCLX_mdl_data dsgeo, csgeo;
   NCLX_mdl_surf *sf;
	NCLX_mot_cs cscntl;
	NCLX_mot_cs_rec csrec[5];
	double val;
	int cond, cs_cond;
	int nck,i,inc,n;
/*
.....No parameters given
.....Prompt the user for all paramters
*/
	n = narg;
	status = UU_SUCCESS;
	if (n == 0) status = my_get_drive_parms(args,&n);
	if (status == UU_FAILURE) return(status);
/*
.....Get part surface
*/
	NclxMdlInquire(args[0],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ps);
/*
.....Get tool condition
*/
	inc = 1;
   cond = NCLX_TLON;
	if (strcmp(args[inc],"TLON") == 0)
	{
		cond = NCLX_TLON;
		inc++;
	}
	else if (strcmp(args[inc],"TLRGT") == 0)
	{
		cond = NCLX_TLRGT;
		inc++;
	}
	else if (strcmp(args[inc],"TLLFT") == 0)
	{
		cond = NCLX_TLLFT;
		inc++;
	}
/*
.....Get drive surface
*/
	NclxMdlInquire(args[inc],&relnum,&key,&val);
	status = NclxMdlGetGeom (key,&ds);
	inc++;
/*
.....Get check surface condition
*/
	nck = 0;
	for (i=inc;i<n;i+=2)
	{
	   cs_cond = NCLX_TO;
		if (strcmp(args[inc],"TO") == 0)
		{
			cs_cond = NCLX_TO;
			inc++;
		}
		else if (strcmp(args[inc],"PAST") == 0)
		{
			cs_cond = NCLX_PAST;
			inc++;
		}
		else if (strcmp(args[inc],"ON") == 0)
		{
			cs_cond = NCLX_ON;
			inc++;
		}
		else if (strcmp(args[inc],"TANTO") == 0)
		{
			cs_cond = NCLX_TANTO;
			inc++;
		}
		csrec[nck].nrpt_flag = 0;
		csrec[nck].nintof = 1;
		csrec[nck].csatt = cs_cond;
		csrec[nck].cs = &cs[nck];
/*
.....Get check surface
*/
/* aak: 
		NclxMdlInquire(args[i+1],&relnum,&key,&val);
*/
		NclxMdlInquire(args[inc],&relnum,&key,&val);
		status = NclxMdlGetGeom (key,&cs[nck]);
		nck++;
	}
	cscntl.numchk = nck;
	cscntl.csrec = csrec;
/*
.....Drive the geometry
*/
	if (status == NCLX_SUCCESS)
	{
		status = NclxMotDrive1(&ps,cond,NCLX_GOFWD,&ds,&cscntl,&clrec);
	}
done:;
	return(status);
}

/*********************************************************************
**    FUNCTION     : int my_scrub (narg,args)
**       This function interactively gets the surface to scrub from
**			the user and then scrubs it at.
**    PARAMETERS
**    INPUT  :
**			none.
**    OUTPUT :
**       narg         Number of arguments provided.
**       args         Actual arguments.
**    RETURNS      :
**       NCLX_FAILURE on failure, NCLX_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int my_scrub (narg,args)
int *narg;
char args[35][64];
{
	char buf[80],sbuf[80],lbuf[80];
	UU_KEY_ID key;
	int status,stat,n,inum,numint,relnum;
	UU_REAL ary[2];
	NCLX_mdl_data ps;
	NCLX_mot_scrub scrub;
	NCLX_mot_clrec clrec;
	double val;
/*
.....Initialize routine
*/
	n = *narg;
	status = UU_FAILURE;
	stat = UU_SUCCESS;
/*
.....Get the part surface
*/
	if (n == 0)
		stat = my_get_surface("Select the Surface to Scrub",args[0]);
	if (stat == UU_FAILURE) goto done;
/*
.....Get scrub parameters
*/
	if (n <= 1)
	{
		numint = 0;
		sprintf(lbuf,"Enter passes,pts");
		ud_ddas(UD_DASSTRING,lbuf,sbuf,sizeof(sbuf),&numint,UD_NODEFAULT);
		if (numint == 0) goto done;
	}
	else
	{
		strcpy(sbuf,args[1]); strcat(sbuf,","); strcat(sbuf,args[2]);
	}
	stat = ul_to_reals(ary,&inum,2,sbuf);
	if (stat != UU_SUCCESS || inum == 0)
	{
		ud_wrerr("Invalid input.");
		goto done;
	}
/*
.....Scrub surface
*/
	NclxMdlInquire(args[0],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ps);
	scrub.numpass = ary[0];
	scrub.numpts = ary[1];
	scrub.bounded = 0;
	stat = NclxMotScrub(&ps,&scrub,&clrec);
	if (stat == UU_FAILURE) goto done;
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    FUNCTION     : int my_rmill (narg,args)
**       This function interactively gets the rmill surfaces from
**			the user and then rmills them.
**    PARAMETERS
**    INPUT  :
**			none.
**    OUTPUT :
**       narg         Number of arguments provided.
**       args         Actual arguments.
**    RETURNS      :
**       NCLX_FAILURE on failure, NCLX_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int my_rmill (narg,args)
int *narg;
char args[35][64];
{
	char buf[80],sbuf[80],lbuf[80];
	UU_KEY_ID key;
	int status,stat,n,inum,numint,relnum,dsatt[2],csatt[2];
	int i;
	UU_REAL ary[2];
	NCLX_mdl_data ps,ds1,ds2,cs1,cs2;
	NCLX_mot_rmill rmill;
	NCLX_mot_clrec clrec;
	double val;
/*
.....Initialize routine
*/
	n = *narg;
	status = UU_FAILURE;
	stat = UU_SUCCESS;
/*
.....Get the part surface
*/
	if (n == 0)
		stat = my_get_surface("Select the Surface to Rmill",args[0]);
	if (stat == UU_FAILURE) goto done;
/*
.....Get the drive surfaces
*/
	if (n < 2)
		stat = my_get_surface("Select the 1st Drive Surface",args[1]);
	if (stat == UU_FAILURE) goto done;
	if (n < 3)
		stat = my_get_surface("Select the 2nd Drive Surface",args[2]);
	if (stat == UU_FAILURE) goto done;
/*
.....Get the check surfaces
*/
	if (n < 4)
		stat = my_get_surface("Select the 1st Check Surface",args[3]);
	if (stat == UU_FAILURE) goto done;
	if (n < 5)
		stat = my_get_surface("Select the 2nd Check Surface",args[4]);
	if (stat == UU_FAILURE) goto done;
/*
.....Define surfaces
*/
	NclxMdlInquire(args[0],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ps);
	NclxMdlInquire(args[1],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ds1);
	dsatt[0] = NCLX_TO;
	NclxMdlInquire(args[2],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ds2);
	dsatt[1] = NCLX_ON;
	NclxMdlInquire(args[3],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&cs1);
	csatt[0] = NCLX_ON;
	NclxMdlInquire(args[4],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&cs2);
	csatt[1] = NCLX_PAST;
/*
.....Set rmill parameters
*/
	rmill.motion_type = NCLX_SCRUB;
	rmill.profile = NCLX_TRUE;
	rmill.clpl_type = NCLX_CLR_PLANE;
	rmill.clpl.header.key = 1001;
	rmill.clpl.header.relnum = NCLX_MDL_PLANE;
	rmill.clpl.pt[0] = 0;
	rmill.clpl.pt[1] = 0;
	rmill.clpl.pt[2] = 2;
	rmill.clpl.vec[0] = 0;
	rmill.clpl.vec[1] = 0;
	rmill.clpl.vec[2] = 1;
	rmill.clpl.dist = 2;
	rmill.pldis = .1;
	rmill.step_type = NCLX_SCALLOP;
	rmill.step_dis = .001;
	rmill.fed = 10;
	rmill.pfed = 0;
	rmill.plfed = 5;
	rmill.ret_type = NCLX_CLR_PLANE;
	rmill.retpl.header.key = 1002;
	rmill.retpl.header.relnum = NCLX_MDL_PLANE;
	rmill.retpl.pt[0] = 0;
	rmill.retpl.pt[1] = 0;
	rmill.retpl.pt[2] = 1;
	rmill.retpl.vec[0] = 0;
	rmill.retpl.vec[1] = 0;
	rmill.retpl.vec[2] = 1;
	rmill.retpl.dist = 1;
	for (i=0;i<4;i++)
	{
		rmill.rough_thick[i] = 0.;
		rmill.finish_thick[i] = 0.;
	}

/*
.....Rmill surface
*/
	NclxMdlInquire(args[0],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ps);
	stat = NclxMotRmill(&ps,dsatt,&ds1,&ds2,csatt,&cs1,&cs2,&rmill,&clrec);
	if (stat == UU_FAILURE) 
	{
		sprintf(sbuf,"Rmill failure = %d\n",stat);
		ud_wrerr(sbuf);
		goto done;
	}
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    FUNCTION     : int my_apocket (narg,args)
**       This function interactively gets the pocket geometry from
**			the user and then performs an advanced pocket.
**    PARAMETERS
**    INPUT  :
**			none.
**    OUTPUT :
**       narg         Number of arguments provided.
**       args         Actual arguments.
**    RETURNS      :
**       NCLX_FAILURE on failure, NCLX_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int my_apocket (narg,args)
int *narg;
char args[35][64];
{
	char buf[80],sbuf[80],lbuf[80];
	UU_KEY_ID key;
	int status,stat,n,inum,numint,relnum,dsatt[2],csatt[2];
	int i,ns;
	UU_REAL ary[2];
	NCLX_mdl_data ps,ds,isle[100];
	NCLX_mot_advpocket_pl pln;
	NCLX_mot_advpocket_perim perim;
	NCLX_mot_advpocket pocket;
	NCLX_mot_clrec clrec;
	double val;
/*
.....Initialize routine
*/
	n = *narg;
	status = UU_FAILURE;
	stat = UU_SUCCESS;
/*
.....Get the Pocket Floor
*/
	if (n == 0)
		stat = my_get_surface("Select the Pocket Floor",args[0]);
	if (stat == UU_FAILURE) goto done;
/*
.....Get the Pocket Boundary
*/
	if (n < 2)
		stat = my_get_surface("Select the Pocket Boundary",args[1]);
	if (stat == UU_FAILURE) goto done;
/*
.....Get the islands
*/
	if (n < 3)
	{
		for (ns=0;ns<100;ns++)
		{
			sprintf(sbuf,"Select island #%d",ns+1);
			stat = my_get_surface(sbuf,args[ns+2]);
			if (stat == UU_FAILURE) break;
		}
	}
	else
	{
		ns = n - 2;
	}
/*
.....Define the planar geometry
*/
	NclxMdlInquire(args[0],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ps);
	pln.ps = (NCLX_mdl_struct *)&ps;

	pln.top_type = NCLX_CLR_PLANE;
	pln.top.header.key = 1000;
	pln.top.header.relnum = NCLX_MDL_PLANE;
	pln.top.pt[0] = 0.; pln.top.pt[1] = 0.; pln.top.pt[2] = 3.;
	pln.top.vec[0] = 0.; pln.top.vec[1] = 0.; pln.top.vec[2] = 1.;

	pln.clpl_type = NCLX_CLR_DISTANCE;
	pln.cldis = 1.0;
/*
.....Pocket perimeter and islands
*/
	perim.peratt = NCLX_P_IN;
	NclxMdlInquire(args[1],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ds);
	perim.perimeter = (NCLX_mdl_struct *)&ds;

	perim.num_islands = ns;
	for (i=0;i<ns;i++)
	{
		perim.islatt[i] = NCLX_P_OUT;
		NclxMdlInquire(args[i+2],&relnum,&key,&val);
		status = NclxMdlGetGeom(key,&isle[i]);
		perim.island[i] = (NCLX_mdl_struct *)&isle[i];
	}
/*
.....Pocket parameters
*/
	pocket.entry = NCLX_RAMP;
	pocket.nramp = 3;
	pocket.ramp_dis = 1.;
	pocket.retract = NCLX_TRUE;
	pocket.level_dis = -1.;
	pocket.retdis = .1;
	pocket.pocket_dir = NCLX_P_CCLW;
	pocket.spiral_dir = NCLX_P_OUT;
	pocket.section_ret = NCLX_TRUE;
	pocket.corner = NCLX_SHARP;
	pocket.step_max = .5;
	pocket.step_min = .1;
	pocket.fedrat.secondary = NCLX_FALSE;
	pocket.fedrat.general = 20.;
	pocket.fedrat.position = 0.;
	pocket.fedrat.entry = -.5;
	pocket.fedrat.transition = -.8;
	pocket.fedrat.finish = -2.;
/*
.....Perform pocket operation
*/
/*	NclxMdlInquire(args[0],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ps);*/
	stat = NclxMotAdvPocket(&pln,&perim,0,0,&pocket,&clrec);
	if (stat == UU_FAILURE) 
	{
		sprintf(sbuf,"Pocket failure = %d\n",stat);
		ud_wrerr(sbuf);
		goto done;
	}
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    FUNCTION     : int my_lthrgh (narg,args)
**       This function interactively gets the shape geometry from
**			the user and then performs an lathe roughing cycle.
**    PARAMETERS
**    INPUT  :
**			none.
**    OUTPUT :
**       narg         Number of arguments provided.
**       args         Actual arguments.
**    RETURNS      :
**       NCLX_FAILURE on failure, NCLX_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int my_lathe_rough (narg,args)
int *narg;
char args[35][64];
{
	char buf[80],sbuf[80];
	UU_KEY_ID key;
	int status,stat,n,numint,relnum;
	int i;
	UU_REAL ary[2];
	NCLX_mdl_data ps,ds;
	NCLX_mot_lathe_rough rough;
	NCLX_mot_clrec clrec;
	NCLX_mot_post_cmd pcmd;
	double val;
/*
.....Initialize routine
*/
	n = *narg;
	status = UU_FAILURE;
	stat = UU_SUCCESS;
/*
.....Get the stock shape
*/
	if (n == 0)
		stat = my_get_surface("Select the Stock Shape",args[0]);
	if (stat == UU_FAILURE) goto done;
	NclxMdlInquire(args[0],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ps);
/*
.....Get the part shape
*/
	if (n < 2)
		stat = my_get_surface("Select the Part Shape",args[1]);
	if (stat == UU_FAILURE) goto done;
	NclxMdlInquire(args[1],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ds);
/*
.....Roughing parameters
*/
	rough.cldist = 0;
	rough.stock_x = .2;
	rough.stock_y = .1;
	rough.depth = .1;
	rough.cutang = 180.;
	rough.retang = 60.;
	rough.retdis = .1;
	pcmd.major = 1031;
	pcmd.nwds = 2;
	pcmd.type[0] = NCLX_P_VOCAB;
	pcmd.ppwrd[0] = 59;
	pcmd.type[1] = NCLX_P_VALUE;
	pcmd.ppval[1] = 300.;
	rough.pcmd_depth = 0;
	rough.pcmd_cutang = 0;
	rough.pcmd_retrct = 0;
	rough.pcmd_return = &pcmd;
	rough.pcmd_final = 0;
	rough.rettyp.type = NCLX_P_XAXIS;
/*
.....Perform roughing operation
*/
	stat = NclxMotLatheRough(&ps,&ds,&rough,&clrec);
	if (stat != UU_SUCCESS) 
	{
		sprintf(sbuf,"Lathe Rough failure = %d\n",stat);
		ud_wrerr(sbuf);
		goto done;
	}
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    FUNCTION     : int my_lthfin (narg,args)
**       This function interactively gets the shape geometry from
**			the user and then performs an lathe finishing cycle.
**    PARAMETERS
**    INPUT  :
**			none.
**    OUTPUT :
**       narg         Number of arguments provided.
**       args         Actual arguments.
**    RETURNS      :
**       NCLX_FAILURE on failure, NCLX_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int my_lathe_finish (narg,args)
int *narg;
char args[35][64];
{
	char buf[80],sbuf[80];
	UU_KEY_ID key;
	int status,stat,n,numint,relnum;
	int i;
	UU_REAL ary[2];
	NCLX_mdl_data ds;
	NCLX_mot_lathe_finish finish;
	NCLX_mot_clrec clrec;
	NCLX_mot_post_cmd pcmd;
	double val;
/*
.....Initialize routine
*/
	n = *narg;
	status = UU_FAILURE;
	stat = UU_SUCCESS;
/*
.....Get the part shape
*/
	if (n == 0)
		stat = my_get_surface("Select the Part Shape",args[0]);
	if (stat == UU_FAILURE) goto done;
	NclxMdlInquire(args[0],&relnum,&key,&val);
	status = NclxMdlGetGeom(key,&ds);
/*
.....Finshing parameters
*/
	finish.stock_x = .2;
	finish.stock_y = .1;
	finish.engang = 30.;
	finish.engdis = 1.;
	finish.retang = 90.;
	finish.retdis = .5;
	pcmd.major = 1031;
	pcmd.nwds = 2;
	pcmd.type[0] = NCLX_P_VOCAB;
	pcmd.ppwrd[0] = 59;
	pcmd.type[1] = NCLX_P_VALUE;
	pcmd.ppval[1] = 300.;
	finish.pcmd_engage = 0;
	finish.pcmd_retrct = 0;
	finish.pcmd_return = &pcmd;
	finish.pcmd_final = 0;
	finish.rettyp.type = NCLX_P_XAXIS;
/*
.....Perform roughing operation
*/
	stat = NclxMotLatheFinish(&ds,&finish,&clrec);
	if (stat != UU_SUCCESS) 
	{
		sprintf(sbuf,"Lathe Rough failure = %d\n",stat);
		ud_wrerr(sbuf);
		goto done;
	}
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    FUNCTION     : int my_get_drive_parms (narg,args)
**       This function interactively gets the drive parameters from
**			the user.
**    PARAMETERS
**    INPUT  :
**			none.
**    OUTPUT :
**       narg         Number of arguments provided.
**       args         Actual arguments.
**    RETURNS      :
**       NCLX_FAILURE on failure, NCLX_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int my_get_drive_parms (args,narg)
int *narg;
char args[35][64];
{
	char buf[80];
	int status,stat,n;
/*
.....Initialize routine
*/
	status = UU_FAILURE;
/*
.....Get the part surface
*/
	stat = my_get_surface("Select the Part Surface",args[0]);
	if (stat == UU_FAILURE) goto done;
/*
.....Get drive surface condition
*/
	stat = my_get_modifier(NCL_TOOL_RELATION,args[1]);
	if (stat == NCL_NOINPUT) goto done;
/*
.....Get drive surface
*/
	stat = my_get_surface("Select the Drive Surface",args[2]);
	if (stat == UU_FAILURE) goto done;
/*
.....Get the check surface condition
*/
	stat = my_get_modifier(NCL_CS_CONDITION,args[3]);
	if (stat == NCL_NOINPUT) goto done;
/*
.....Get the check surface(s)
*/
	*narg = 4;
	n = 1;
	do
	{
		sprintf(buf,"Select Check Surface #%d",n);
		stat = my_get_surface(buf,args[*narg]);
		if (stat == UU_FAILURE) goto done;
		status = UU_SUCCESS;
		*narg = *narg + 1;
		n++;
	} while (stat != UU_FAILURE);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    FUNCTION     : int my_get_surface (prm,str)
**       This function allows the user to pick the PS,DS, or CS.
**    PARAMETERS
**    INPUT  :
**       prm          Prompt to display.
**    OUTPUT :
**       str          Label of entity picked.
**    RETURNS      :
**       NCLX_FAILURE on failure, NCLX_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int my_get_surface (prm,str)
char *prm,*str;
{
	int status;
	int numint;
	UD_PPICKREC pick;
	UM_PICKENT data;
	struct UC_entitydatabag e;
/*
.....Initialize routine
*/
	status = UU_FAILURE;
/*
.....Prompt the user to pick the surface
*/
	ud_ddas(UD_DASPICK,prm,&pick,1,&numint,UD_NODEFAULT);
	if (numint == 0) goto done;
	um_d_pickresolve(&pick,2,&data);
	e.key = um_get_pickkey(&data,1);
	ur_retrieve_data_fixed(&e);
	ncl_get_label(&e,str);
	status = UU_SUCCESS;
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int my_get_modifier (menu,str)
**       This function displays a Popup menu and returns the item
**			picked.  The NCL_TOOL_RELATION and NCL_CS_CONDITION Popup
**			menus are currently supported.
**    PARAMETERS
**    INPUT  :
**       menu         Which menu to display.
**    OUTPUT :
**       str          Text string of menu item selected.
**    RETURNS      :
**       NCLX_FAILURE on failure, NCLX_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int my_get_modifier(menu,str)
int menu;
char *str;
{
	int status,choice,outchoice;
	do
	{
		status = ud_ddas(UD_POPUP, &nmodfy[menu], &choice,
			1, &outchoice, UD_NODEFAULT);
	}
	while  (status != 1);
	status = NCL_OKINPUT;
	switch (menu)
	{
	case NCL_TOOL_RELATION:
		switch(choice)
		{
		case 1:
			strcpy(str, NCL_tllft);
			break;
		case 2:
			strcpy(str, NCL_tlon);
			break;
		case 3:
			strcpy(str, NCL_tlrgt);
			break;
		default:
			status = NCL_NOINPUT;
			break;
		}
		break;
	case NCL_CS_CONDITION:
		switch (choice)
		{
		case 1:
		case 2:
			strcpy(str, NCL_to);
			break;
		case 3:
		case 4:
			strcpy(str, NCL_on);
			break;
		case 5:
		case 6:
			strcpy(str, NCL_past);
			break;
		case 7:
		case 8:
			strcpy(str, NCL_tanto);
			break;
		default:
			status = NCL_NOINPUT;
			break;
		}
		break;
	default:
		status = NCL_NOINPUT;
		break;
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int my_pick_id ()
**       This function displays the primary and secondary Keys for the
**			geometric item picked.  The user is prompted to pick an item.
**    PARAMETERS
**    INPUT  :
**       none.
**    OUTPUT :
**			none.
**    RETURNS      :
**       none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
my_pick_id()
{
	int segid,numint;
	Gwpoint3 gpt[2];
	int args[2];
	UU_KEY_ID key;
	UD_PPICKREC pick;
	char buf[80];
	int xform=0;
	UM_PICKENT data;
	Glntype ls;
/*
.....Open scrolling window
*/
	ul_open_window(10,80,args);
/*
.....Let the user pick the lines
*/
	while (1)
	{
		ud_ddas(UD_DASPICK,"Pick a line",&pick,1,&numint,UD_NODEFAULT);
		if (numint == 0) break;
		um_d_pickresolve(&pick,2,&data);
		key = um_get_pickkey(&data,1);
		sprintf(buf,"Level 1 = %d",key);
		ul_win_out(buf,0);
		key = um_get_pickkey(&data,2);
		sprintf(buf,"Level 2 = %d",key);
		ul_win_out(buf,0);
	}
/*	ul_close_window();*/
/*
.....End of routine
*/
done:;
}

/*********************************************************************
**    E_FUNCTION     : int my_patern_rec ()
**       This function displays a PATERN record.
**    PARAMETERS
**    INPUT  :
**       none.
**    OUTPUT :
**			none.
**    RETURNS      :
**       none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
my_patern_rec()
{
	int numint,i,n,stat;
	int args[2];
	UU_KEY_ID key;
	UD_PPICKREC pick;
	char buf[80];
	UM_PICKENT data;
	NCLX_mdl_patern pat;
	int pntyp;
	UM_int2 npts,ipt;
	UM_real8 pts[6];
/*
.....Initialize routine
*/
	NCLX_internal_geom = NCLX_TRUE;
/*
.....Let the user pick the PATERN
*/
	ud_ddas(UD_DASPICK,"Pick a Pattern",&pick,1,&numint,UD_NODEFAULT);
	if (numint == 0) goto done;
	um_d_pickresolve(&pick,2,&data);
	key = um_get_pickkey(&data,1);
	stat = NclxMdlGetPatern(key,&pat);
	if (stat == NCLX_FAILURE) goto done;
	UY_ds = (NCLX_mdl_struct *)&pat;
	stat = gtpnnp(&key,&npts,&pntyp);
	if (stat == NCLX_FAILURE) goto done;
/*
.....Open scrolling window
*/
	ul_open_window(10,80,args);
/*
.....Print out the pattern
*/	
	sprintf(buf,"Pattern %s",pat.header.label);
	ul_win_out(buf,0);
/*	sprintf(buf,"Numpts = %d",pat.npts);*/
	sprintf(buf,"Numpts = %d",npts);
	ul_win_out(buf,0);
/*
.....Print out the point data
*/
	i = 0;
/*	if (pat.npts < 0 || pat.npts > 100) goto done;*/
	if (npts < 0 || npts > 100) goto done;
/*	for (n=0;n<pat.npts;n++)*/
	for (n=0;n<npts;n++)
	{
/*		if (pat.pntype == NCLX_MDL_POINT)*/
		ipt = n + 1;
		stat = gtpnpt(pts,&pntyp,&key,&ipt);
		if (stat == NCLX_FAILURE) goto done;
		if (pntyp == 1)
		{
/*			sprintf(buf,"Point #%d = %g,%g,%g",n,pat.pts[i],pat.pts[i+1],
				pat.pts[i+2]);
			i = i + 3;*/
			sprintf(buf,"Point #%d = %g,%g,%g",n,pts[0],pts[1],
				pts[2]);
		}
		else
		{
/*			sprintf(buf,"Pntvec #%d = %g,%g,%g, %g,%g,%g",n,pat.pts[i],
				pat.pts[i+1],pat.pts[i+2],pat.pts[i+3],pat.pts[i+4],
				pat.pts[i+5]);
			i = i + 6;*/
			sprintf(buf,"Pntvec #%d = %g,%g,%g, %g,%g,%g",n,pts[0],
				pts[1],pts[2],pts[3],pts[4],pts[5]);
		}
		ul_win_out(buf,0);
	}
/*	ul_close_window();*/
/*
.....End of routine
*/
done:;
	NCLX_internal_geom = NCLX_FALSE;
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int my_polyline_rec ()
**       This function defines and displays a Polyline record.
**    PARAMETERS
**    INPUT  :
**       none.
**    OUTPUT :
**			none.
**    RETURNS      :
**       none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
my_polyline_rec()
{
	int numint,i,n,stat;
	int args[2];
	UU_KEY_ID key;
	UD_PPICKREC pick;
	char buf[80];
	UM_PICKENT data;
	NCLX_mdl_polyline pat;
	int pntyp;
	UM_int2 npts,ipt;
	UM_real8 pts[6];
/*
.....Define the polyline
*/
	umu_c42_polyline();
/*
.....Let the user pick the Polyline
*/
	ud_ddas(UD_DASPICK,"Pick a Polyline",&pick,1,&numint,UD_NODEFAULT);
	if (numint == 0) goto done;
	um_d_pickresolve(&pick,2,&data);
	key = um_get_pickkey(&data,1);
	stat = NclxMdlGetPolyline(key,&pat);
	if (stat == NCLX_FAILURE) goto done;
/*	UY_ds = (NCLX_mdl_struct *)&pat;
	stat = gtpnnp(&key,&npts,&pntyp);
	if (stat == NCLX_FAILURE) goto done;*/
/*
.....Open scrolling window
*/
	ul_open_window(10,80,args);
/*
.....Print out the polyline
*/	
	sprintf(buf,"Polyline %s",pat.header.label);
	ul_win_out(buf,0);
	sprintf(buf,"Numpts = %d",pat.npts);
/*	sprintf(buf,"Numpts = %d",npts);*/
	ul_win_out(buf,0);
/*
.....Print out the point data
*/
	i = 0;
	if (pat.npts < 0 || pat.npts > 100) goto done;
/*	if (npts < 0 || npts > 100) goto done;*/
	for (n=0;n<pat.npts;n++)
/*	for (n=0;n<npts;n++)*/
	{
		sprintf(buf,"Point #%d = %g,%g,%g",n,pat.pts[i],pat.pts[i+1],
			pat.pts[i+2]);
		i = i + 3;
		ul_win_out(buf,0);
	}
/*	ul_close_window();*/
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int my_shape_rec ()
**       This function displays a SHAPE record.
**    PARAMETERS
**    INPUT  :
**       none.
**    OUTPUT :
**			none.
**    RETURNS      :
**       none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
my_shape_rec()
{
	int numint,i,n,stat;
	int args[2];
	UU_KEY_ID key;
	UD_PPICKREC pick;
	char buf[80];
	UM_PICKENT data;
	NCLX_mdl_shape rec;
	NCLX_mdl_struct *dat;
/*
.....Let the user pick the SHAPE
*/
	ud_ddas(UD_DASPICK,"Pick a Shape",&pick,1,&numint,UD_NODEFAULT);
	if (numint == 0) goto done;
	um_d_pickresolve(&pick,2,&data);
	key = um_get_pickkey(&data,1);
	stat = NclxMdlGetShape(key,&rec);
	if (stat == NCLX_FAILURE) goto done;
/*
.....Open scrolling window
*/
	ul_open_window(10,80,args);
/*
.....Print out the pattern
*/	
	sprintf(buf,"Shape %s",rec.header.label);
	ul_win_out(buf,0);
	switch (rec.side)
	{
	case NCLX_P_IN: ul_win_out("side = NCLX_P_IN",0); break;
	case NCLX_P_OUT: ul_win_out("side = NCLX_P_OUT",0); break;
	case NCLX_P_ON: ul_win_out("side = NCLX_P_ON",0); break;
	}
	switch (rec.dir)
	{
	case NCLX_P_LEFT: ul_win_out("dir = NCLX_P_LEFT",0); break;
	case NCLX_P_RIGHT: ul_win_out("dir = NCLX_P_RIGHT",0); break;
	}
	sprintf(buf,"Nents = %d",rec.nents);
	ul_win_out(buf,0);
/*
.....Print out the shape data
*/
	for (i=0;i<rec.nents;i++)
	{
		sprintf(buf,"Entity #%d",i+1);
		ul_win_out(buf,0);
		switch (rec.shid[i].type)
		{
		case NCLX_S_ENDPT:
			ul_win_out("   NCLX_S_ENDPT",0);
			sprintf(buf,"     Endpt = %g, %g",rec.shid[i].pt[0],rec.shid[i].pt[1]);
			ul_win_out(buf,0);
			break;
		case NCLX_S_ARC:
			ul_win_out("   NCLX_S_ARC",0);
			sprintf(buf,"     Endpt = %g, %g",rec.shid[i].pt[0],rec.shid[i].pt[1]);
			ul_win_out(buf,0);
			sprintf(buf,"     Center = %g, %g",rec.shid[i].arc.cen[0],
				rec.shid[i].arc.cen[1]);
			ul_win_out(buf,0);
			sprintf(buf,"     Radius = %g",rec.shid[i].arc.rad);
			ul_win_out(buf,0);
			sprintf(buf,"     Angles = %g, %g",rec.shid[i].arc.sang,
				rec.shid[i].arc.eang);
			ul_win_out(buf,0);
			break;
		case NCLX_S_DIRECTION:
			switch (rec.shid[i].dir)
			{
			case NCLX_P_CLW: ul_win_out("   dir = NCLX_P_CLW",0); break;
			case NCLX_P_CCLW: ul_win_out("   dir = NCLX_P_CCLW",0); break;
			}
			break;
		case NCLX_S_VOCAB:
			sprintf(buf,"   VOCAB = %d",rec.shid[i].vocab);
			ul_win_out(buf,0);
			break;
		case NCLX_S_VALUE:
			sprintf(buf,"   VALUE = %g",rec.shid[i].value);
			ul_win_out(buf,0);
			break;
		}
	}
/*	ul_close_window();*/
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : int my_surf_eval ()
**       This function displays point vectors for the normal and
**			1st derivative vectors (the tangencies are normalized to 
**			the unit length, just like the normal) on an NCL or NURB surface.
**    PARAMETERS
**    INPUT  :
**       none 				
**    OUTPUT :
**			none.
**    RETURNS      :
**       NCLX_FAILURE on failure, NCLX_SUCCESS on success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
my_surf_eval()

{
	int status, stat;
	char label[64];
	NCLX_KEY_ID key = (NCLX_KEY_ID) 0;
	NCLX_mdl_type rel;
	double scalar;
	NCLX_mdl_data sf;
	NCLX_mdl_pntvec normal;
	NCLX_mdl_pntvec uderiv;
	NCLX_mdl_pntvec vderiv;
	int i, j, k;
	double u,v;
	NCLX_mdl_surf *surf;
	NCLX_mdl_surf_eval eval;
	static char pntveclab[] = "PV";
	int sub, vis = 0;
	static double pos[3] = { 0.0, 0.0, 0.0 }; 

/*
.....Initialize routine
*/
	status = UU_FAILURE;

/*
.....Pick a surface using my_get_surface
*/
	stat = my_get_surface("Select the Surface",label);
	if (stat != UU_SUCCESS) goto done;

/*
.....Get Unibase key of the surface using 
.....NclxMdlInquire from ymodel.c
*/
	stat = NclxMdlInquire(label, &rel, &key, &scalar);
	if (stat != UU_SUCCESS) goto done;

/*
.....Get geometry of the surface using 
.....NclxMdlGetGeom from ymdlget.c
*/
	stat = NclxMdlGetGeom(key, &sf);
	if (stat != UU_SUCCESS) goto done;
	surf = (NCLX_mdl_surf *)&sf;

/*
.....Evaluate pointvectors normal, uderiv, vderiv
.....using NclxMdlEvalSurf from ymodel.c
.....Store the pointvectors using NclxMdlSetLabel 
.....from ymdlattrib.c, NclxMdlStorePntvec from ymdlput.c
*/
	for (i=0; i<5; i++)
		for (j=0; j<5; j++) 
		{
			u = (double)i * 0.25; 
			v = (double)j * 0.25; 

			stat = NclxMdlEvalSurf(surf, u, v, &eval);
			if (stat != UU_SUCCESS) goto done;
	
			um_vctovc(eval.pt, normal.pt);
			um_vctovc(eval.normal, normal.vec);
			um_vctovc(eval.pt, uderiv.pt);
			um_vctovc(eval.udrv1, uderiv.vec);
			um_unitvc(uderiv.vec, uderiv.vec);
			um_vctovc(eval.pt, vderiv.pt);
			um_vctovc(eval.vdrv1, vderiv.vec);
			um_unitvc(vderiv.vec, vderiv.vec);

			sub = 5*i+j+1;

			normal.header.key = 0;
			normal.header.relnum = NCLX_MDL_PNTVEC; 
			NclxMdlSetLabel(&(normal.header), pntveclab, sub, vis, pos);  
			NclxMdlSetColor(normal, 1);
			NclxMdlSetLayer(normal, 1);
			NclxMdlSetPen(normal, 1);
			NclxMdlSetStyle(normal, 1);
			NclxMdlSetWeight(normal, 1.0);
			NclxMdlSetInvis(normal, UU_FALSE);
			stat = NclxMdlStorePntvec(&normal, NCLX_TRUE);
			if (stat != UU_SUCCESS) goto done;

  			uderiv.header.key = 0;
			uderiv.header.relnum = NCLX_MDL_PNTVEC;
			NclxMdlSetLabel(&(uderiv.header), pntveclab, 25 + sub, vis, pos); 
			NclxMdlSetColor(uderiv, 1);
			NclxMdlSetLayer(uderiv, 1);
			NclxMdlSetPen(uderiv, 1);
			NclxMdlSetStyle(uderiv, 1);
			NclxMdlSetWeight(uderiv, 1.0);
			NclxMdlSetInvis(uderiv, UU_FALSE);
			stat = NclxMdlStorePntvec(&uderiv, NCLX_TRUE);
			if (stat != UU_SUCCESS) goto done;

  			vderiv.header.key = 0;
			vderiv.header.relnum = NCLX_MDL_PNTVEC;
			NclxMdlSetLabel(&(vderiv.header), pntveclab, 50 + sub, vis, pos); 
			NclxMdlSetColor(vderiv, 1);
			NclxMdlSetLayer(vderiv, 1);
			NclxMdlSetPen(vderiv, 1);
			NclxMdlSetStyle(vderiv, 1);
			NclxMdlSetWeight(vderiv, 1.0);
			NclxMdlSetInvis(vderiv, UU_FALSE);
			stat = NclxMdlStorePntvec(&vderiv, NCLX_TRUE);
			if (stat != UU_SUCCESS) goto done; 

		}		

	status = UU_SUCCESS;
done:;
	return(status); 
}
