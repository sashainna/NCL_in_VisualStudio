/*********************************************************************
**	FILENAME: lverify1.c
**	CONTAINS:
**				ul_verify_box()
**				ul_verify_cone()
**				ul_verify_cyl()
**				ul_verify_sphere()
**				ul_verify_torus()
**				ul_verify_solid()
**				ul_verify_sweep()
**				ul_verify_contour()
**				ul_verify_revsf()
**				ul_verify_draw_box()
**				ul_verify_draw_cyl()
**				ul_verify_draw_sweep()
**				ul_verify_draw_revsf()
**     MODULE NAME AND RELEASE LEVEL 
**       lverify1.c , 25.2
**     DATE AND TIME OF LAST  MODIFICATION
**       07/28/15 , 11:18:02
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "lcom.h"
#include "mfort.h"
#include "nclfc.h"
#include <ctype.h>
#include <math.h>
#include "gtbl.h"
#include "gobas.h"
#include "view.h"
#include "mdattr.h"
#include "mpocket.h"
#include "ginqatt.h"
#include "zsysdep.h"
#include "mcrv.h"
#include "uhep.h"
#include "dselmask.h"
#include "mdpick.h"
#include "modef.h"
#include "ulist.h"
#include "m2dattr.h"
#include "mdcpln.h"
#include "mcrv.h"
#include "msol.h"
#include "nccs.h"
#include "mdrel.h"
#include "nclmplay.h"
#include "udfconst.h"
#include "udforms.h"
#include "lipv.h"

static char ltype[2][10] = {"Stock","Fixture"};
static int Sfrm,Scolor;
static UU_LIST Ssurf,*Skeylist;
static UU_LOGICAL Sinit = UU_FALSE;

//FILE* fp;
//
//int writeFile1 (char* value) 
//{
//
//	//FILE* fp;
//	if (fp = fopen( "..\\IvpErrors.txt", "a" )) // Open file for writing
//	{
//
//		fprintf(fp, "%s\n" , value);
//		fclose(fp);
//		return 0;
//	}
//	return -1;
//}

/*********************************************************************
**   E_FUNCTION:int ul_verify_box(which,ifl)
**      This function defines the NCLIPV stock in the form of a box.
**   PARAMETERS
**       INPUT  : which = 0 - Stock definition.
**						  1 - Fixture definition.
**				  ifl = 1 - Box PT PT
**				        2 - Box PT L W H
**				        3 - Bounding box
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_box(which,ifl)
    int which,ifl;
{
	int stat,numint,inum,i,modals[20],*ans[8],nt;
	int icurpt;
	UN_clstruc *iclpt[4];
	UU_REAL ll[3],ur[3],pc[3],ary[4],rnum,stock[6],*rpt;
	UU_REAL fabs();
	UD_NDCLOCREC ln;
	static char lbox[2][6] = {"CBOX","FCBOX"};
	char lbuf[80],sbuf[80];
	FILE *fd;

/*
.....Make sure a Stock filename has been defined
*/
	if (LW_version == LW_NCVERIFY && UL_stk_file[0] == '\0')
	{
		ud_wrerr("An NCLIPV Stock file name has not yet been defined.");
		goto done;
	}
/*
.....Create stock/fixture box using
.....opposite corners of the box
*/
	if (ifl == 1)
	{
/*
........Get the opposite corners of the box
*/
		sprintf(lbuf,"Enter 1st corner of %s",ltype[which]);
		stat = ud_ddas(UD_DASCART,lbuf,&ln,1,&numint,UD_NODEFAULT);
		ll[0] = ln.cord[0]; ll[1] = ln.cord[1]; ll[2] = ln.cord[2];
		if (stat != 1) goto done;
		sprintf(lbuf,"Enter 2nd corner of %s",ltype[which]);
		stat = ud_ddas(UD_DASCART,lbuf,&ln,1,&numint,UD_NODEFAULT);
		ur[0] = ln.cord[0]; ur[1] = ln.cord[1]; ur[2] = ln.cord[2];
		if (stat != 1) goto done;
/*
........Verify the coordinates of the box are valid
........If the Z-values of both points are the same
........then prompt for Z-levels of points
*/
		ncl_wcstomcs(0,ll,ll);
		ncl_wcstomcs(0,ur,ur);
		if (fabs(ll[0]-ur[0]) <= .001 || fabs(ll[1]-ur[1]) <= .001)
			goto err1;
		rnum = fabs(ll[2]-ur[2]);
		if (rnum <= .001)
		{
			sprintf(lbuf,"Enter upper and lower Z-limits of %s",ltype[which]);
			numint = 0;
			ud_ddas(UD_DASSTRING,lbuf,sbuf,sizeof(sbuf),&numint,UD_NODEFAULT);
			ary[0] = ll[2];
			ary[1] = ur[2];
			inum = 2;
			stat = ul_to_reals(ary,&inum,2,sbuf);
			if (stat != UU_SUCCESS || inum == 0 ||
				fabs(ary[0]-ary[1]) <= .001) goto err1;
			UM_len_exttoint(ary[0],ll[2]);
			UM_len_exttoint(ary[1],ur[2]);
		}
/*
........Set the actual lower left and
........upper right corners of box
*/
		if (ur[0] < ll[0])
		{
			ary[0] = ll[0];
			ll[0] = ur[0];
			ur[0] = ary[0];
		}
		if (ur[1] < ll[1])
		{
			ary[1] = ll[1];
			ll[1] = ur[1];
			ur[1] = ary[1];
		}
		if (ur[2] < ll[2])
		{
			ary[2] = ll[2];
			ll[2] = ur[2];
			ur[2] = ary[2];
		}
	}
/*
.....Create stock/fixture box using
.....center, len, wid, and hgt
*/
	else if (ifl == 2)
	{
/*
........Get the center of the box
*/
		sprintf(lbuf,"Enter center point of %s",ltype[which]);
		stat = ud_ddas(UD_DASCART,lbuf,&ln,1,&numint,UD_NODEFAULT);
		pc[0] = ln.cord[0]; pc[1] = ln.cord[1]; pc[2] = ln.cord[2];
		if (stat != 1) goto done;
		ncl_wcstomcs(0,pc,pc);
/*
........Get the length, width and height of box
*/
		sprintf(lbuf,"Enter length, width, height, [Z-level] of %s",
			ltype[which]);
		numint = 0;
		ud_ddas(UD_DASSTRING,lbuf,sbuf,sizeof(sbuf),&numint,UD_NODEFAULT);
		ary[0] = 0;
		ary[1] = 0;
		ary[2] = 0;
		ary[3] = 0;
		inum = 4;
		stat = ul_to_reals(ary,&inum,4,sbuf);
		UM_len_exttoint(ary[0],ary[0]);
		UM_len_exttoint(ary[1],ary[1]);
		UM_len_exttoint(ary[2],ary[2]);
		UM_len_exttoint(ary[3],ary[3]);
/*
........Verify the coordinates of the box are valid
*/
		if (stat != UU_SUCCESS || (inum != 3 && inum != 4)) goto err1;
		if (fabs(ary[0]) <= .001 || fabs(ary[1]) <= .001 ||
			fabs(ary[2]) <=.001) goto err1;
		if (inum == 4) pc[2] = ary[3];
/*
........Set the actual lower left and
........upper right corners of box
*/
		ll[0] = pc[0] - ary[0]/2.;
		ll[1] = pc[1] - ary[1]/2.;
		ll[2] = pc[2] - ary[2]/2.;
		ur[0] = ll[0] + ary[0];
		ur[1] = ll[1] + ary[1];
		ur[2] = ll[2] + ary[2];
/*		ncl_mcstowcs(0,ll,ll);
		ncl_mcstowcs(0,ur,ur);*/
	}
/*
.....Create stock/fixture box using
.....bounding box of motion
*/
	else if (ifl == 3)
	{
/*
........Get the motion bounding box
*/
		ncl_play_initscan(modals,iclpt,&icurpt,UU_FALSE);
		nt = 0;
		ncl_motion_playback(modals,2,stock,UU_NULL,&nt);
		rnum = LW_box_expansion;
		ncl_play_resetscan(iclpt,icurpt);
/*
........Set up the form
*/
		ans[0] = (int *)&stock[0];
		ans[1] = (int *)&stock[1];
		ans[2] = (int *)&stock[2];
		ans[3] = (int *)&stock[3];
		ans[4] = (int *)&stock[4];
		ans[5] = (int *)&stock[5];
		ans[6] = (int *)&rnum;
/*
........Get the form input
*/
		UM_cc_inttoext(stock,stock);
		rpt = &stock[3];
		UM_cc_inttoext(rpt,rpt);
		UM_len_inttoext(rnum,rnum);
		stat = ud_form("ipvbox.frm",ans,ans);
		if (stat == -1) goto done;
		UM_cc_exttoint(stock,stock);
		UM_cc_exttoint(rpt,rpt);
		UM_len_exttoint(rnum,rnum);
		LW_box_expansion = rnum;
		for (i=0;i<3;i++)
		{
			ll[i] = stock[i] - rnum;
			ur[i] = stock[i+3] + rnum;
		}
/*
........Verify the coordinates of the box are valid
........If the Z-values of both points are the same
........then prompt for Z-levels of points
*/
		if (fabs(ll[0]-ur[0]) <= .001 || fabs(ll[1]-ur[1]) <= .001 ||
			fabs(ll[2]-ur[2]) <= .001) goto err1;
		rnum = fabs(ll[2]-ur[2]);
/*
........Set the actual lower left and
........upper right corners of box
*/
		if (ur[0] < ll[0])
		{
			ary[0] = ll[0];
			ll[0] = ur[0];
			ur[0] = ary[0];
		}
		if (ur[1] < ll[1])
		{
			ary[1] = ll[1];
			ll[1] = ur[1];
			ur[1] = ary[1];
		}
		if (ur[2] < ll[2])
		{
			ary[2] = ll[2];
			ll[2] = ur[2];
			ur[2] = ary[2];
		}
	}
/*
.....Open the stock file
*/
	if (LW_version == LW_NCVERIFY)
	{
		stat = ul_verify_open(&fd);
		if (stat != UU_SUCCESS) goto done;
/*
.....Draw the box
*/
		ul_verify_draw_box(which,ll,ur,LW_stock_default[which].color,
			LW_stock_default[which].mxflag,LW_stock_default[which].matrix);
		UM_cc_inttoext(ll,ll);
		UM_cc_inttoext(ur,ur);
/*
........Run box through MODSYS
........Bobby  -  5/19/94
*/
		ncl_wcstomcs(0,stock,stock);
		ncl_wcstomcs(0,&stock[3],&stock[3]);
	}
/*
........NCVERIFY
*/
	if (LW_version == LW_NCVERIFY)
	{
		sprintf(lbuf,"%s %f %f %f %f %f %f\n",lbox[which],ll[0],ll[1],
			ll[2],ur[0],ur[1],ur[2]);
		stat = ux_fwrite0(lbuf,strlen(lbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;
		ux_fclose0(fd);
	}
/*
........MachineWorks
*/
	else
	{
		if (ll[0] < ur[0])
		{
			stock[0] = ll[0];
			stock[3] = ur[0];
		}
		else
		{
			stock[0] = ur[0];
			stock[3] = ll[0];
		}
		if (ll[1] < ur[1])
		{
			stock[1] = ll[1];
			stock[4] = ur[1];
		}
		else
		{
			stock[1] = ur[1];
			stock[4] = ll[1];
		}
		if (ll[2] < ur[2])
		{
			stock[2] = ll[2];
			stock[5] = ur[2];
		}
		else
		{
			stock[2] = ur[2];
			stock[5] = ll[2];
		}
		i = 0;
		ul_ipv_add_stock(which,LW_STOCK_BOX,stock,&i,UU_NULL,0,1);
	}
	goto done;
/*
.....Invalid coordinates for stock
*/
err1:;
	ud_wrerr ("Input coordinates do not form a 3-D box.");
	stat = UU_FAILURE;
	goto done;
/*
.....Error trying to write to stock file
*/
wrterr:;
	ud_wrerr ("Error trying to write to stock file.");
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_cone(which,ifl)
**      This function defines the stock for NCLIPV in the form of a
**		cone.
**   PARAMETERS
**       INPUT  :
**          which = 0 - Stock definition.
**						  1 - Fixture definition.
**				  ifl = 2 - Cone PT PT
**				        3 - Cone CE HGT
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_cone(which,ifl)
    int which,ifl;
{
	int stat,numint,inum,i;
	UU_REAL tv[3],ary[8],rnum;
	UU_REAL fabs();
	UD_NDCLOCREC ln;
	char lbuf[80];
/*
.....Define the stock as a cone
.....using 2 points
*/
	if (ifl == 2)
	{
/*
........Get the 2 points which define
........the axis of the cone
*/
		sprintf(lbuf,"Enter 1st point of %s cone",ltype[which]);
		stat = ud_ddas(UD_DASCART,lbuf,&ln,1,&numint,UD_NODEFAULT);
		ary[0] = ln.cord[0]; ary[1] = ln.cord[1]; ary[2] = ln.cord[2];
		if (stat != 1) goto done;
		sprintf(lbuf,"Enter 2nd point of %s cone",ltype[which]);
		stat = ud_ddas(UD_DASCART,lbuf,&ln,1,&numint,UD_NODEFAULT);
		ary[3] = ln.cord[0]; ary[4] = ln.cord[1]; ary[5] = ln.cord[2];
		if (stat != 1) goto done;
/*
........Verify that cone data is correct
........Cylinder axis must have a length
*/
		tv[0] = ary[3] - ary[0];
		tv[1] = ary[4] - ary[1];
		tv[2] = ary[5] - ary[2];
		rnum = um_mag(tv);
		if (rnum <= .001) goto err2;
		um_vctovc(tv,&ary[3]);
/*
........Get the radii of the cone
*/
		ud_ddas(UD_DASVAL,"Enter bottom radius of cone",&ary[6],1,&numint,
			UD_NODEFAULT);
		if (ary[6] <= .001) goto err2;
		ud_ddas(UD_DASVAL,"Enter top radius of cone",&ary[7],1,&numint,
			UD_NODEFAULT);
		if (ary[7] <= .001) goto err2;
	}
/*
.....Define the stock as a cone
.....using base point, axis, height, and radius
*/
	else if (ifl == 3)
	{
/*
........Get the base point
........of the cylinder
*/
		sprintf(lbuf,"Enter base point of %s cone",ltype[which]);
		stat = ud_ddas(UD_DASCART,lbuf,&ln,1,&numint,UD_NODEFAULT);
		ary[0] = ln.cord[0]; ary[1] = ln.cord[1]; ary[2] = ln.cord[2];
		if (stat != 1) goto done;
		sprintf(lbuf,"Enter axis of %s cone",ltype[which]);
		stat = ud_ddas(UD_DASVEC,lbuf,&ln,1,&numint,UD_NODEFAULT);
		tv[0] = ln.cord[0]; tv[1] = ln.cord[1]; tv[2] = ln.cord[2];
		if (stat != 1) goto done;
/*
........Get the height of the cylinder
*/
		ud_ddas(UD_DASVAL,"Enter height of cone",&rnum,1,&numint,
			UD_NODEFAULT);
		if (rnum < UM_FUZZ) goto err2;
		um_vctmsc(tv,rnum,&ary[3]);
/*
........Get the radii of the cone
*/
		ud_ddas(UD_DASVAL,"Enter bottom radius of cone",&ary[6],1,&numint,
			UD_NODEFAULT);
		if (ary[6] <= .001) goto err2;
		ud_ddas(UD_DASVAL,"Enter top radius of cone",&ary[7],1,&numint,
			UD_NODEFAULT);
		if (ary[7] <= .001) goto err2;
	}
/*
........Run cylinder through MODSYS
*/
	ncl_wcstomcs(0,&ary[0],&ary[0]);
	ncl_wcstomcs(1,&ary[3],&ary[3]);
/*
.....Create the stock
*/
	i = 0;
	ul_ipv_add_stock(which,LW_STOCK_CONE,ary,&i,UU_NULL,0,1);
	goto done;
/*
.....Invalid coordinates for stock
*/
err2:;
	ud_wrerr ("Cylinder has zero length.");
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_cyl(which,ifl)
**      This function defines the stock for NCLIPV in the form of a
**		cylinder.
**   PARAMETERS
**       INPUT  :
**          which = 0 - Stock definition.
**						  1 - Fixture definition.
**				  ifl = 1 - Cylinder CI L
**				        2 - Cylinder PT PT
**				        3 - Cylinder CE HGT
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_cyl(which,ifl)
    int which,ifl;
{
	int stat,numint,inum,i;
	UU_REAL tv[3],ary[7],rnum;
	UU_REAL fabs();
	struct UM_circle_rec ce;
	UM_PLOCREC pick;
	UD_NDCLOCREC ln;
	char lbuf[80];
	static char lcyl[2][6] = {"CYL","FCYL"};
	FILE *fd;
	Gwpoint3 pts[2];
	UM_vector vc;
/*
.....Make sure a Stock filename has been defined
*/
	if (LW_version == LW_NCVERIFY && UL_stk_file[0] == '\0')
	{
		ud_wrerr("An NCLIPV Stock file name has not yet been defined.");
		goto done;
	}
/*
.....Define the stock as a cylinder
.....using a circle and length
*/
	if (ifl == 1)
	{
/*
........Get the Circle which defines
........the base of the cylinder
*/
		ud_lgeo(UU_TRUE,UD_ncl_ci);
		um_dl_pldas(UD_DASPCKLOC,UA_NCL,220,&pick,1,&numint,2);
		if (numint == 0) goto done;
		ce.key = um_get_pickkey(&pick.pent,2);
		stat = um_get_all_geom(&ce,sizeof(ce));
/*
........Get the length of the cylinder
*/
		ud_ddas(UD_DASVAL,"Enter length of cylinder",&rnum,1,&numint,UD_NODEFAULT);
		if (fabs(rnum) <= .001) goto err2;
/*
........Calculate the actual cylinder
*/
		ary[0] = ce.center[0];
		ary[1] = ce.center[1];
		ary[2] = ce.center[2];
		ary[3] = ce.nvec[0] * rnum;
		ary[4] = ce.nvec[1] * rnum;
		ary[5] = ce.nvec[2] * rnum;
		ary[6] = ce.radius;
    }
/*
.....Define the stock as a cylinder
.....using 2 points
*/
	else if (ifl == 2)
	{
/*
........Get the 2 points which define
........the axis of the cylinder
*/
		sprintf(lbuf,"Enter 1st point of %s cylinder",ltype[which]);
		stat = ud_ddas(UD_DASCART,lbuf,&ln,1,&numint,UD_NODEFAULT);
		ary[0] = ln.cord[0]; ary[1] = ln.cord[1]; ary[2] = ln.cord[2];
		if (stat != 1) goto done;
		sprintf(lbuf,"Enter 2nd point of %s cylinder",ltype[which]);
		stat = ud_ddas(UD_DASCART,lbuf,&ln,1,&numint,UD_NODEFAULT);
		ary[3] = ln.cord[0]; ary[4] = ln.cord[1]; ary[5] = ln.cord[2];
		if (stat != 1) goto done;
/*
........Verify that cylinder data is correct
........Cylinder axis must have a length
*/
		ary[3] = ary[3] - ary[0];
		ary[4] = ary[4] - ary[1];
		ary[5] = ary[5] - ary[2];
		rnum = um_mag(&ary[3]);
		if (rnum <= .001) goto err2;
/*
........Get the radius of the cylinder
*/
		ud_ddas(UD_DASVAL,"Enter radius of cylinder",&ary[6],1,&numint,UD_NODEFAULT);
		if (fabs(ary[6]) <= .001) goto err2;
	}
/*
.....Define the stock as a cylinder
.....using base point, axis, height, and radius
*/
	else if (ifl == 3)
	{
/*
........Get the base point
........of the cylinder
*/
		sprintf(lbuf,"Enter base point of %s cylinder",ltype[which]);
		stat = ud_ddas(UD_DASCART,lbuf,&ln,1,&numint,UD_NODEFAULT);
		ary[0] = ln.cord[0]; ary[1] = ln.cord[1]; ary[2] = ln.cord[2];
		if (stat != 1) goto done;
		sprintf(lbuf,"Enter axis of %s cylinder",ltype[which]);
		stat = ud_ddas(UD_DASVEC,lbuf,&ln,1,&numint,UD_NODEFAULT);
		tv[0] = ln.cord[0]; tv[1] = ln.cord[1]; tv[2] = ln.cord[2];
		if (stat != 1) goto done;
/*
........Get the height of the cylinder
*/
		ud_ddas(UD_DASVAL,"Enter height of cylinder",&rnum,1,&numint,
			UD_NODEFAULT);
		if (rnum < UM_FUZZ) goto err2;
		ary[3] = tv[0] * rnum;
		ary[4] = tv[1] * rnum;
		ary[5] = tv[2] * rnum;
/*
........Get the radius of the cylinder
*/
		ud_ddas(UD_DASVAL,"Enter radius of cylinder",&ary[6],1,&numint,UD_NODEFAULT);
		if (ary[6] <= .001) goto err2;
	}
/*
.....Open the stock file
*/
	if (LW_version == LW_NCVERIFY)
	{
		stat = ul_verify_open(&fd);
		if (stat != UU_SUCCESS) goto done;
/*
.....Draw the cylinder
*/
		ul_verify_draw_cyl(which,ary,LW_stock_default[which].color,
			LW_stock_default[which].mxflag,LW_stock_default[which].matrix);
		for (i=0;i<7;i++) UM_len_inttoext(ary[i],ary[i]);
	}
/*
........Run cylinder through MODSYS
........Bobby  -  5/19/94
*/
	ncl_wcstomcs(0,&ary[0],&ary[0]);
	ncl_wcstomcs(1,&ary[3],&ary[3]);
/*
.....NCVERIFY
*/
	if (LW_version == LW_NCVERIFY)
	{
		sprintf (lbuf,"%s %f %f %f %f %f %f %f\n",lcyl[which],ary[0],ary[1],
			ary[2],ary[3],ary[4],ary[5],ary[6]);
		stat = ux_fwrite0 (lbuf,strlen(lbuf),1,fd,&inum);
		ux_fclose0 (fd);
		if (stat != UU_SUCCESS) goto wrterr;
	}
/*
.....MachineWorks
*/
	else
	{
		i = 0;
		ul_ipv_add_stock(which,LW_STOCK_CYLINDER,ary,&i,UU_NULL,0,1);
	}
/*
.....Close the stock file
*/
	goto done;
/*
.....Invalid coordinates for stock
*/
err2:;
	ud_wrerr ("Cylinder has zero length.");
	stat = UU_FAILURE;
	goto done;
/*
.....Error trying to write to stock file
*/
wrterr:;
	ud_wrerr ("Error trying to write to stock file.");
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_sphere(which,ifl)
**      This function defines the stock for NCLIPV in the form of a
**      sphere.
**   PARAMETERS
**       INPUT  :
**          which = 0 - Stock definition.
**						  1 - Fixture definition.
**				  ifl = 1 - Sphere CI
**				        2 - Sphere PT RA
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_sphere(which,ifl)
int which,ifl;
{
	int stat,numint,inum,i;
	UU_REAL tv[3],ary[4],rnum;
	struct UM_circle_rec ce;
	UM_PLOCREC pick;
	UD_NDCLOCREC ln;
	char lbuf[80];
/*
.....Define the stock as a sphere
*/
	if (ifl == 1)
	{
/*
........Get the Circle which defines
........the sphere
*/
		ud_lgeo(UU_TRUE,UD_ncl_ci);
		um_dl_pldas(UD_DASPCKLOC,UA_NCL,220,&pick,1,&numint,2);
		if (numint == 0) goto done;
		ce.key = um_get_pickkey(&pick.pent,2);
		stat = um_get_all_geom(&ce,sizeof(ce));
/*
........Calculate the actual sphere
*/
		ary[0] = ce.center[0];
		ary[1] = ce.center[1];
		ary[2] = ce.center[2];
		ary[3] = ce.radius;
    }
/*
.....Define the stock as a sphere
.....using a center point and radius
*/
	else if (ifl == 2)
	{
/*
........Get the point which defines
........the center of the sphere
*/
		sprintf(lbuf,"Enter center point of %s sphere",ltype[which]);
		stat = ud_ddas(UD_DASCART,lbuf,&ln,1,&numint,UD_NODEFAULT);
		ary[0] = ln.cord[0]; ary[1] = ln.cord[1]; ary[2] = ln.cord[2];
		if (stat != 1) goto done;
/*
........Get the radius of the sphere
*/
		ud_ddas(UD_DASVAL,"Enter radius of sphere",&ary[3],1,&numint,
			UD_NODEFAULT);
		if (ary[3] <= .001) goto err2;
	}
/*
.....Run sphere through MODSYS
*/
	ncl_wcstomcs(0,&ary[0],&ary[0]);
/*
.....Add the stock
*/
	i = 0;
	ul_ipv_add_stock(which,LW_STOCK_SPHERE,ary,&i,UU_NULL,0,1);
	goto done;
/*
.....Invalid coordinates for stock
*/
err2:;
	ud_wrerr ("Sphere has illegal radius.");
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_torus(which,ifl)
**      This function defines the stock for NCLIPV in the form of a
**      torus.
**   PARAMETERS
**       INPUT  :
**          which = 0 - Stock definition.
**						  1 - Fixture definition.
**				  ifl = 1 - Torus CI CI
**				        2 - Torus CI RA
**				        3 - Torus PT VE RA
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_torus(which,ifl)
int which,ifl;
{
	int stat,numint,inum,i;
	UU_REAL tv[3],ary[8],rnum;
	UU_REAL fabs();
	struct UM_circle_rec ce;
	UM_PLOCREC pick;
	UD_NDCLOCREC ln;
	char lbuf[80];
/*
.....Define the stock as a cylinder
.....using a circle and length
*/
	if (ifl == 1)
	{
/*
........Get the Circle which defines
........the inner circle of the torus
*/
		ud_lgeo(UU_TRUE,UD_ncl_ci);
		um_dl_pldas(UD_DASPCKLOC,UA_NCL,220,&pick,1,&numint,2);
		if (numint == 0) goto done;
		ce.key = um_get_pickkey(&pick.pent,2);
		stat = um_get_all_geom(&ce,sizeof(ce));
		um_vctovc(ce.center,&ary[0]);
		um_vctovc(ce.nvec,&ary[3]);
		ary[6] = ce.radius;
/*
........Get the Circle which defines
........the outer torus radius
*/
		ud_lgeo(UU_TRUE,UD_ncl_ci);
		um_dl_pldas(UD_DASPCKLOC,UA_NCL,220,&pick,1,&numint,2);
		if (numint == 0) goto done;
		ce.key = um_get_pickkey(&pick.pent,2);
		stat = um_get_all_geom(&ce,sizeof(ce));
		ary[7] = fabs(ce.radius-ary[6]) / 2.;
		if (ary[6] < ce.radius)
			ary[6] = ary[6] + ary[7];
		else
			ary[6] = ce.radius + ary[7];
    }
/*
.....Define the stock as a torus
.....using a circle and a radius
*/
	else if (ifl == 2)
	{
/*
........Get the Circle which defines
........the axial circle of the torus
*/
		ud_lgeo(UU_TRUE,UD_ncl_ci);
		um_dl_pldas(UD_DASPCKLOC,UA_NCL,220,&pick,1,&numint,2);
		if (numint == 0) goto done;
		ce.key = um_get_pickkey(&pick.pent,2);
		stat = um_get_all_geom(&ce,sizeof(ce));
		um_vctovc(ce.center,&ary[0]);
		um_vctovc(ce.nvec,&ary[3]);
		ary[6] = ce.radius;
/*
........Get the circular radius of the torus
*/
		ud_ddas(UD_DASVAL,"Enter circular radius of torus",&ary[7],1,&numint,
			UD_NODEFAULT);
		if (ary[7] <= .001) goto err2;
	}
/*
.....Define the stock as a torus
.....using center point, axis, 2 radii
*/
	else if (ifl == 3)
	{
/*
........Get the center point
........of the cylinder
*/
		sprintf(lbuf,"Enter center point of %s torus",ltype[which]);
		stat = ud_ddas(UD_DASCART,lbuf,&ln,1,&numint,UD_NODEFAULT);
		ary[0] = ln.cord[0]; ary[1] = ln.cord[1]; ary[2] = ln.cord[2];
		if (stat != 1) goto done;
		sprintf(lbuf,"Enter axis of %s torus",ltype[which]);
		stat = ud_ddas(UD_DASVEC,lbuf,&ln,1,&numint,UD_NODEFAULT);
		ary[3] = ln.cord[0]; ary[4] = ln.cord[1]; ary[5] = ln.cord[2];
		if (stat != 1) goto done;
/*
........Get the axis radius of the torus
*/
		ud_ddas(UD_DASVAL,"Enter axial radius of torus",&ary[6],1,&numint,
			UD_NODEFAULT);
		if (ary[6] <= .001) goto err2;
/*
........Get the circular radius of the torus
*/
		ud_ddas(UD_DASVAL,"Enter circular radius of torus",&ary[7],1,&numint,
			UD_NODEFAULT);
		if (ary[7] <= .001) goto err2;
	}
/*
........Run torus through MODSYS
*/
	ncl_wcstomcs(0,&ary[0],&ary[0]);
	ncl_wcstomcs(1,&ary[3],&ary[3]);
/*
.....Create the stock
*/
	i = 0;
	if (ary[7] > ary[6]) goto err2;
	ul_ipv_add_stock(which,LW_STOCK_TORUS,ary,&i,UU_NULL,0,1);
	goto done;
/*
.....Invalid coordinates for stock
*/
err2:;
	ud_wrerr ("Torus has illegal radius.");
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_solid(which)
**      This function defines the stock for NCLIPV from an Visual Solid.
**   PARAMETERS
**       INPUT  :
**          which = 0 - Stock definition.
**						  1 - Fixture definition.
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_solid(which)
int which;
{
	int status,stat,numint,id,ifxt,ierr;
	UU_LOGICAL init;
	UU_KEY_ID key;
/*
.....Get the Solid which
.....defines the stock
*/
	stat = UU_SUCCESS;
	ud_lgeo(UU_TRUE,UD_solid);
	status = ud_ldas(UD_DASSELECT,UM_MODEL,91,UU_NULL,1,&numint,UD_NODEFAULT);
	if (numint == 0) goto done;
/*
.....Loop through solids
*/
	init = UU_TRUE;
	id = 0;
	ifxt = which;
	while (ud_gnxt(init,UU_NULL,&key,1))
	{
		init = UU_FALSE;
		ulf_verify_solid(&ifxt,&key,&id,&ierr);
		if (ierr != 0) goto failed;
	}
	goto done;
/*
.....Could not create stock
*/
failed:;
	ud_wrerr("Could not create stock.");
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**    S_FUNCTION     :  OnSrfSelect()
**       Routine to select a list of surfaces.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnSrfSelect()
{
	int numint,init,color;
	struct NCL_fixed_databag e;
	UU_LOGICAL cmdreject;
	UM_sgeo geo;

//	char p_buff[80];
/*
.....Take down form
*/
	ud_form_invis();

	//if (fp = fopen( "..\\IvpErrors.txt", "a" )) // Open file for writing
	//{

	//	fclose(fp);
	//	remove( "..\\IvpErrors.txt");
	//}
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_TRUE);

	/*sprintf(p_buff, " (cmdreject = %d) cmdreject value\n", cmdreject);
	writeFile1 (p_buff) ;*/

	if (cmdreject != 0) goto done;
/*
.....Set the appropriate selection mask
*/
	ud_lgeo(UU_TRUE,UD_ncl_allsfsh);
/*
.....Get the next geometry selection
*/
	ud_ldas(UD_DASSELECT,UA_NCL,478,UU_NULL,1,&numint,UD_NODEFAULT);
	/*sprintf(p_buff, " (numint = %d) numint value\n", numint);
	writeFile1 (p_buff) ;*/
	if (numint == 0) goto done;
/*
//.....Loop through selections
*/
	init = UU_TRUE;
	color = Scolor;
//	ud_gnxt(init,UU_NULL,&e.key,1);
	while(ud_gnxt(init,UU_NULL,&e.key,1))
	{
		/*sprintf(p_buff, " (e.key = %d) e.key\n", e.key);
		writeFile1 (p_buff) ;*/

		init = UU_FALSE;
/*
.....Store this item in the list
*/
		if (ncl_retrieve_data_fixed(&e) != 0) continue;
		geo.key = e.key;
		geo.relnum = e.rel_num;
		/*sprintf(p_buff, " (geo.relnum = %d) geo.relnum\n", geo.relnum);
		writeFile1 (p_buff) ;*/
		ncl_get_label(&e,geo.label);
		ncl_get_geo_color(e.key,&geo.color);
		nclu_add_list(&Ssurf,&geo,&color);
/*
.....Update the entities color
*/
		if (color != -1)
		{
			ncl_update_geo_color(e.key,color,UU_TRUE);
			uc_display(&e);
		}
		color = Scolor;
	}
///*
//.....End of routine
//.....Redisplay form
//*/
done:;
	ud_unlimit();
	ud_form_vis();
	UD_UNMARK(cmdreject);

	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnColor(fieldno, val, stat)
**			Color change callback.  Changes the color of all entities.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Current field value.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnColor(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int nc,color;
	UM_sgeo *geo = UU_NULL;
/*
.....Call the default method
.....This causes the answer field to be updated
*/
	ud_default_method(fieldno, val, stat);
/*
.....Reference correct geometry list
.....Depending on which button was pushed
*/
	nc = Ssurf.cur_cnt;
	if (nc > 0)
	{
		geo = (UM_sgeo *) UU_LIST_ARRAY (&Ssurf);
		color = Scolor;
	}
	else
		return (UD_FLDOK);
/*
.....Change the color of all entities
.....in this list
*/
	nclu_repaint (geo,nc,color);

	return (UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnDesel(fieldno, val, stat)
**			Deselects all surfaces from the list.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Not used, form initiated through pushbutton.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnDesel()
{
	UM_sgeo *geo;
	int nc;
/*
.....Remove all entities in this list
*/
	geo = (UM_sgeo *) UU_LIST_ARRAY(&Ssurf);
	nc = Ssurf.cur_cnt;
	nclu_repaint (geo,nc,-1);
	Ssurf.cur_cnt = 0;
	return(UD_FLDOK);
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_contour(which)
**     This function defines the stock for NCLIPV as a profile sweep
**		 using a collection of surfaces to form the profile.
**   PARAMETERS
**       INPUT  : 
**          which   = 0 - Stock definition.
**                    1 - Fixture definition.
**          keylist = List of surface keys when 'kflag' == UU_TRUE;
**          kflag   = UU_FALSE = Surface list is provided.
**                    UU_TRUE  = Prompt for surfaces.
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_contour(which,keylist,kflag)
int which;
UU_LIST *keylist;
UU_LOGICAL kflag;
{
	int stat;
	UU_REAL stock[6];
	UM_coord *prf=UU_NULL;
	UU_REAL fabs();

	int npts,i;
	
	UM_sgeo *geo;

	int nc;
	UU_LOGICAL cmdreject;
	UU_LIST *stklst = UU_NULL,sfky;
	UU_REAL d;
	UM_vector nvec;

	static UU_REAL Sstock_off,Sbot_off,Stop_off;
	static UM_vector Snvec;

	static char traverse[] = {1,1,1,1,1,1,1};
	static char called[] =   {6,6,6,6,6,6,6};
	static char display[] =  {1,1,1,1,1,1,1};

	static UD_METHOD methods[] = 
		{OnSrfSelect,OnColor,OnDesel,UU_NULL,UU_NULL,UU_NULL,UU_NULL};
	static int *ans[] =
		{UU_NULL,&Scolor,UU_NULL,(int *)&Sstock_off,(int *)Snvec,
		(int *)&Sbot_off,(int *)&Stop_off};
/*
.....Initialize routine
*/
	if (LW_version == LW_NCVERIFY) return (0);
	display[0] = display[1] = display[2] = kflag;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)  goto fini;

	if (kflag)
		uu_list_init(&Ssurf,sizeof(UM_sgeo),50,50);
	if (!Sinit)
	{
		Sinit = UU_TRUE;
		Sstock_off = Sbot_off = Stop_off = 0.;
		Snvec[0] = Snvec[1] = 0.; Snvec[2] = 1.;
		Scolor = 8;
	}
	sfky.cur_cnt = 0; sfky.data = UU_NULL;

repeat:
	UM_cc_exttoint(Snvec,Snvec);
	ncl_mcstowcs(1,Snvec,Snvec);
/*
........Get the form input
*/
	Sfrm = ud_form1("ipvcontour.frm",ans,ans,methods,called,display,traverse);
	ncl_wcstomcs(1,Snvec,Snvec);
	UM_cc_inttoext(Snvec,Snvec);
	if (Sfrm == -1) goto done;
	if (kflag && Ssurf.cur_cnt == 0) goto repeat;
	d = UM_MAG (Snvec);
	if (d < UM_DFUZZ)
	{
		ud_wrerr ("Undefined vertical direction.");
		goto repeat;
	}

	for (i = 0; i < 3; i++) nvec[i] = Snvec[i]/d;
	ncl_mcstowcs(1,nvec,nvec);
/*
.....Creat list of surface keys
*/
	if (kflag)
	{
		nc = Ssurf.cur_cnt;
		uu_list_init(&sfky,sizeof(UU_KEY_ID),nc+1,nc);

		geo = (UM_sgeo *) UU_LIST_ARRAY(&Ssurf);
		for (i = 0; i < nc; i++)
			nclu_push_sfkey (&sfky,&geo[i]);
	}
	else
	{
		uu_list_init(&sfky,sizeof(UU_KEY_ID),UU_LIST_LENGTH(keylist)+1,10);
		uu_list_push_list(&sfky,keylist);
	}
/*
.....Create contour stock
*/
	stat = ncl_ipv_contour_stock(&sfky,nvec,Sstock_off,Sbot_off,Stop_off,&stklst,
		UU_TRUE);
	if (stat != UU_SUCCESS || !stklst) goto err3;
	npts = stklst->cur_cnt;
	prf = (UM_coord *) UU_LIST_ARRAY(stklst);
	if (npts < 4 || !prf) goto err3;
/*
.....Run points through MODSYS

	for (i = 0; i < npts; i++)
		ncl_wcstomcs(0,prf[i],prf[i]);
*/
/*
.....Store stock definition

		ncl_wcstomcs(1,nvec,stock);*/
	for (i = 0; i < 3; i++)
	{
		stock[i] = nvec[i];
		stock[i+3] = 0.;
	}
	i = 0;
	stat = ul_ipv_add_stock(which,LW_STOCK_SWEEP,stock,&i,prf,npts,1);
	goto done;
/*
.....Invalid stock coordinates
*/
err3:
	ud_wrerr ("Could not create a stock.");
	stat = UU_FAILURE;
/*
.....End of routine
*/
done:
	geo = (UM_sgeo *) UU_LIST_ARRAY (&Ssurf); nc = Ssurf.cur_cnt;
	nclu_repaint (geo,nc,-1);
	uu_list_free(&Ssurf); uu_list_free (&sfky); 
	if (stklst)
	{
		uu_list_free (stklst); uu_free (stklst);
	}

fini:
	UD_UNMARK(cmdreject);
	return(stat);
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_sweep(which)
**      This function defines the stock for NCLIPV as a profile sweep
**		using a composite curve to form the profile.
**   PARAMETERS
**       INPUT  : which = 0 - Stock definition.
**                        1 - Fixture definition.
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_sweep(which)
int which;
{
	int stat,numint,inum,flag;
	UU_REAL ary[4],ptary[3],rsav[2],mxary[3],stock[6];
	UM_coord *prf=UU_NULL;
	UU_REAL fabs();
	UM_PLOCREC pick;
	char lbuf[80],sbuf[80];
	FILE *fd;
	UU_LIST list;
	int cnt=800;
	Gwpoint3 *pts;
	UU_KEY_ID key;
	int npts,i,dsav,psav,status;
	struct UM_compcrv_rec e;
	UM_transf tfmat;
	UU_REAL tol;
	UM_vector nvec,xaxis,yaxis;
	UU_REAL plane[4];
	UU_LOGICAL um_planar_curve();
/*
.....Make sure a Stock filename has been defined
*/
	if (LW_version == LW_NCVERIFY && UL_stk_file[0] == '\0')
	{
		ud_wrerr("An NCLIPV Stock file name has not yet been defined.");
		goto done;
	}
/*
.....Initialize the list to receive the
.....points on the composite curve
*/
repeat:;
	uu_list_init(&list,sizeof(UM_coord),cnt,cnt);
/*
.....Get the Composite Curve which defines
.....the base of the swept surface
*/
	ud_lgeo(UU_TRUE,UD_ncl_cicv);
	um_dl_pldas(UD_DASPCKLOC,UM_MODEL,251,&pick,1,&numint,1);
	if (numint == 0) goto done;
	key = um_get_pickkey(&pick.pent,1);
/*
.....Set the model attribute models
.....for evaluating curves
*/
	dsav = UM_2dattr.disp_flag;
	psav = UM_2dattr.pts_per_span;
	UM_2dattr.disp_flag = UU_FALSE;
	UM_2dattr.pts_per_span = UL_ipv_npts;
/*
.....Generate the points around the composite curve
*/
	e.key = key;
	status = ncl_retrieve_data(&e,sizeof(struct UM_compcrv_rec));
	if (status == UU_SUCCESS) status = uc_retrieve_transf(e.key,tfmat);
	if (status != UU_SUCCESS) goto err3;
	gettol(&tol);
	npts = ncl_evolve_all_curves(&e,tfmat,tol,&list,UU_NULL,UU_FALSE);
	if (npts <= 1) goto err3;

	pts = (Gwpoint3 *) UU_LIST_ARRAY(&list);
/*
.....NCVERIFY does not allow closed curves
*/
	if (LW_version == LW_NCVERIFY)
	{
		if (pts[npts-1].x == pts[0].x && pts[npts-1].y == pts[0].y &&
			pts[npts-1].z == pts[0].z) npts--;
	}
/*
.....Restore the model attribute models
.....for evaluating curves
*/
	UM_2dattr.disp_flag = dsav;
	UM_2dattr.pts_per_span = psav;
/*
.....Get the upper and lower Z-limits of stock
*/
	if (LW_version == LW_NCVERIFY)
	{
		sprintf(lbuf,"Enter upper and lower Z-limits of stock");
		numint = 0;
		ud_ddas(UD_DASSTRING,lbuf,sbuf,sizeof(sbuf),&numint,UD_NODEFAULT);
		ary[0] = 0.;
		ary[1] = 0.;
		inum = 2;
		stat = ul_to_reals(ary,&inum,2,sbuf);
		UM_len_exttoint(ary[0],ary[0]);
		UM_len_exttoint(ary[1],ary[1]);
/*
.....Run Z-limits thru MODSYS
*/
		rsav[0] = ary[0]; rsav[1] = ary[1];
		ptary[0] = pts[0].x;
		ptary[1] = pts[0].y;
		ptary[2] = ary[0];
		ncl_mcstowcs(0,ptary,mxary);
		ary[0] = mxary[2];
		ptary[2] = ary[1];
		ncl_mcstowcs(0,ptary,mxary);
		ary[1] = mxary[2];
		ary[2] = fabs(ary[0]-ary[1]);
		if (stat != UU_SUCCESS || inum == 0 || ary[2] <= .001) goto err3;
		ary[3] = ary[0] + (ary[1]-ary[0]) * .5;
		flag = 0;
	}
/*
.....MachineWorks
*/
	else
	{
/*
.....Get vector for curve translation
*/
		ud_ldas(UD_DASVEC,UM_MODEL,10,nvec,1,&numint,UD_NODEFAULT);
		if (numint == 0)
		{
			uu_list_free(&list);
			goto repeat;
		}
		flag = 1;
	}
/*
.....Draw the stock
*/
	if (LW_version == LW_NCVERIFY)
	{
		ul_verify_draw_sweep(which,pts,npts,flag,ary,nvec,
			LW_stock_default[which].color,
			LW_stock_default[which].mxflag,LW_stock_default[which].matrix);
		ary[0] = rsav[0];
		ary[1] = rsav[1];
		ary[2] = fabs(ary[0]-ary[1]);
		ary[3] = ary[0] + (ary[1]-ary[0]) * .5;
/*
.....Open the stock file
*/
		stat = ul_verify_open(&fd); stat = UU_SUCCESS;
		if (stat != UU_SUCCESS) goto done;
		UM_len_inttoext(ary[2],ary[2]);
		UM_len_inttoext(ary[3],ary[3]);
	}
/*
.....Run points through MODSYS
*/
	for (i=0;i<npts;i++)
	{
		if (LW_version == LW_NCVERIFY)
		{
			UM_len_inttoext(pts[i].x,pts[i].x);
			UM_len_inttoext(pts[i].y,pts[i].y);
		}
		ptary[0] = pts[i].x;
		ptary[1] = pts[i].y;
		ptary[2] = pts[i].z;
		ncl_wcstomcs(0,ptary,ptary);
		pts[i].x = ptary[0];
		pts[i].y = ptary[1];
		pts[i].z = ptary[2];
	}
/*
.....NCVERIFY
*/
	if (LW_version == LW_NCVERIFY)
	{
		sprintf (lbuf,"SPROF 0. 0. %f %f\n",ary[3],ary[2]);
		stat = ux_fwrite0 (lbuf,strlen(lbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;
/*
........Output point array
*/
		for (i=0;i<npts;i++)
		{
			sprintf (lbuf,"SPPROF %f %f\n",pts[i].x,pts[i].y);
       	stat = ux_fwrite0 (lbuf,strlen(lbuf),1,fd,&inum);
       	if (stat != UU_SUCCESS) goto wrterr;
		}
		ux_fclose0 (fd);
	}
/*
.....MachineWorks
*/
	else
	{
		prf = (UM_coord *)uu_malloc((npts+1)*sizeof(UM_coord));
		if (prf == 0) goto done;
		for (i=0;i<npts;i++)
		{
			prf[i][0] = pts[i].x;
			prf[i][1] = pts[i].y;
			prf[i][2] = pts[i].z;
		}
/*
.....Add closing point if necessary
*/
		if (um_dcccc(prf[0],prf[npts-1]) > UM_FUZZ)
		{
			prf[npts][0] = prf[0][0]; prf[npts][1] = prf[0][1];
			prf[npts][2] = prf[0][2];
			npts++;
		}
/*
.....Make sure points are coplanar
*/
		if (!(um_planar_curve(prf,npts,plane,xaxis,yaxis))) goto notplane;
/*
.....Store stock definition
*/
		ncl_wcstomcs(1,nvec,stock);
		i = 0;
		stat = ul_ipv_add_stock(which,LW_STOCK_SWEEP,stock,&i,prf,npts,1);
	}
	goto done;
/*
.....Invalid stock coordinates
*/
err3:;
	ud_wrerr ("Invalid dimensions for composite curve stock.");
	stat = UU_FAILURE;
	goto done;
/*
.....Curve is not coplanar
*/
err4:;
	ud_wrerr ("Input curve is not coplanar.");
	stat = UU_FAILURE;
	goto done;
/*
.....Error trying to write to stock file
*/
wrterr:;
	ud_wrerr ("Error trying to write to stock file.");
	stat = UU_FAILURE;
	goto done;
/*
.....Curve is not planar
*/
notplane:;
	ud_wrerr ("Input curve is not coplanar.");
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	if (prf != UU_NULL) uu_free(prf);
	uu_list_free(&list);
	return(stat);
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_revsf()
**      This function defines the stock for NCLIPV as a surface of
**      revolution.
**   PARAMETERS
**       INPUT  : none.
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_revsf(which)
int which;
{
	int stat,numint,rel_num;
	UU_REAL stock[8];
	UU_REAL fabs();
	UM_PLOCREC pick;
	UU_LIST list;
	int cnt=800;
	Gwpoint3 *pts;
	UU_KEY_ID key;
	int npts,i,dsav,psav,status;
	struct NCL_fixed_databag e;
	struct NCL_revsurf_rec *revp;
	struct UM_point_rec *ptp;
	struct NCL_vector_rec *vep;
	struct NCL_nclpv_rec *pvp;
	UM_coord  origpt,ptb;
	UM_vector axisvc;
	UM_transf tfmat;
	UM_coord *prf=UU_NULL;
	UU_REAL tol,sa,ea;
/*
.....Surface of revolution is only valid for MachineWorks
*/
	status = UU_SUCCESS;
	if (LW_version == LW_NCVERIFY) goto done;
/*
.....Initialize the list to receive the
.....points on the composite curve
*/
	uu_list_init(&list,sizeof(UM_coord),cnt,cnt);
/*
.....Get a surface of revolution or a Curve which defines
.....the generatrix of the surf of revolution.
*/
start:;
	ud_lgeo(UU_TRUE,UD_ncl_show);
	um_dl_pldas(UD_DASPCKLOC,UA_NCL,590,&pick,1,&numint,1);
	if (numint == 0) goto done;
	key = um_get_pickkey(&pick.pent,1);
	status = ur_retrieve_data_relnum(key,&rel_num);
	switch (rel_num)
	{
	case NCL_REVSURF_REL:
		e.key = key;
		status = ncl_retrieve_data_fixed(&e);
		if (status != UU_SUCCESS) goto err3;
		revp = (struct NCL_revsurf_rec *)&e;
		um_vctovc(revp->pta,origpt);
		um_vctovc(revp->vca,axisvc);
		sa = revp->sa;
		ea = revp->ta;
		key = revp->cvkey;
		break;
	case UM_LINE_REL:
	case UM_CIRCLE_REL:
	case UM_CONIC_REL:
	case UM_COMPCRV_REL:
	case UM_RBSPLCRV_REL:
	case NCL_CURVE_REL:
	case NCL_LINE_REL:
	case NCL_CIRCLE_REL:
/*
.....Get the Origin point
*/
		ud_lgeo(UU_TRUE,UD_ncl_ptpv);
		um_dl_pldas(UD_DASPCKLOC,UA_NCL,606,&pick,1,&numint,1);
		if (numint == 0) goto start;
		e.key = um_get_pickkey(&pick.pent,1);
		status = ncl_retrieve_data_fixed(&e);
		if (status != UU_SUCCESS) goto err3;
		status = ur_retrieve_data_relnum(e.key,&rel_num);
		if (rel_num == NCL_POINTVEC_REL)
		{
			pvp = (struct NCL_nclpv_rec *)&e;
			um_vctovc(pvp->pt,origpt);
			um_vctovc(pvp->ve,axisvc);
			
		}
		else
		{
			ptp = (struct UM_point_rec *)&e;
			um_vctovc(ptp->pt,origpt);
/*
.....Get the axis vector
*/
			ud_lgeo(UU_TRUE,UD_ncl_vepv);
			um_dl_pldas(UD_DASPCKLOC,UA_NCL,607,&pick,1,&numint,1);
			if (numint == 0) goto start;
			e.key = um_get_pickkey(&pick.pent,1);
			status = ncl_retrieve_data_fixed(&e);
			if (status != UU_SUCCESS) goto err3;
			vep = (struct NCL_vector_rec *)&e;
			um_vctovc(vep->vec,axisvc);
		}
/*
.....Get the starting and ending angles
*/
		ud_ldas(UD_DASUNITLESS,UA_NCL,608,&sa,1,&numint,UD_NODEFAULT);
		if (sa < 0.) sa = sa + 360.;
		if (numint == 0)
		{
			sa = 0.;
			ea = 360.;
		}
		else
		{
			ud_ldas(UD_DASUNITLESS,UA_NCL,609,&ea,1,&numint,UD_NODEFAULT);
			if (numint == 0) ea = 360.;
			if (ea < 0.) ea = ea + 360.;
			if (ea < sa) ea = ea + 360.;
		}
		break;
	case NCL_SHAPE_REL:
		origpt[0] = 0.; origpt[1] = 0.; origpt[2] = 0.;
		axisvc[0] = 1.; axisvc[1] = 0.; axisvc[2] = 0.;
		sa = 0.;
		ea = 360.;
		break;
	default:
		goto err3;
	}
/*
.....Set the model attribute models
.....for evaluating curves
*/
	dsav = UM_2dattr.disp_flag;
	psav = UM_2dattr.pts_per_span;
	UM_2dattr.disp_flag = UU_FALSE;
	UM_2dattr.pts_per_span = UL_ipv_npts;
/*
.....Generate the points around the composite curve
*/
	e.key = key;
	status = ncl_retrieve_data_fixed(&e);
	if (status == UU_SUCCESS) status = uc_retrieve_transf(e.key,tfmat);
	if (status != UU_SUCCESS) goto err3;
	gettol(&tol);
	if (e.rel_num == NCL_SHAPE_REL)
		npts = ncl_evolve_shape(&e,tfmat,tol,&list);
	else
		npts = ncl_evolve_all_curves(&e,tfmat,tol,&list,UU_NULL,UU_FALSE);
	if (npts <= 1) goto err3;

	pts = (Gwpoint3 *) UU_LIST_ARRAY(&list);
/*	if (pts[npts-1].x == pts[0].x && pts[npts-1].y == pts[0].y &&
		pts[npts-1].z == pts[0].z) npts--;*/
/*
.....Restore the model attribute models
.....for evaluating curves
*/
	UM_2dattr.disp_flag = dsav;
	UM_2dattr.pts_per_span = psav;
/*
.....Generate the Surface of Revolution
*/
	ncl_wcstomcs(0,origpt,&stock[0]);
	ncl_wcstomcs(1,axisvc,&stock[3]);
	stock[6] = sa; stock[7] = ea;
	for (i=0;i<npts;i++)
	{
		ptb[0] = pts[i].x ; ptb[1] = pts[i].y ; ptb[2] = pts[i].z;
		ncl_wcstomcs(0,ptb,ptb);
		pts[i].x = ptb[0] ; pts[i].y = ptb[1] ; pts[i].z = ptb[2];
	}
	i = 0;
	ul_ipv_add_stock(which,LW_STOCK_REVOLVE,stock,&i,pts,npts,1);
	stat = UU_SUCCESS;
	goto done;
/*
.....Invalid stock coordinates
*/
err3:;
	ud_wrerr ("Invalid dimensions for composite curve stock.");
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	uu_list_free(&list);
	if (prf) uu_free(prf);
	return(stat);
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_draw_box(which,ll,ur,color,tfflag,tf)
**      This function draws the stock and fixture defined for NCLIPV
**		in the form of a box.
**   PARAMETERS
**       INPUT  : which = 0 - Stock definition.
**                        1 - Fixture definition.
**                ll = Lower left coordinates of box.
**                ur = Upper right coordinates of box.
**                color = Color of stock/fixture.
**                tfflag = UU_TRUE = stock has a transformation matrix.
**                tf     = Stock transformation matrix.
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_draw_box(which,ll,ur,color,tfflag,tf)
int which,color;
UU_REAL ll[],ur[];
UU_LOGICAL tfflag;
UM_transf tf;
{
	int k,isav;
	Gcolor lincol;
	Glntype lintyp,line_style;
	Gwpoint3 gpt[6];
	UM_coord vpx, vpy, vpz, llt, urt;
	UU_REAL dtmp;
	UU_REAL um_dot();
/*
.....Save the current viewport, linestyle and linecolor
.....Also set the linestyle to PHANTOM and the
.....linecolor to WHITE
*/
	isav = ug_gksstli.curvwindex;
	zbytecp(lintyp,*gqlinetype());
	lincol = gqlinecolor();
	line_style.typeno = UM_PHANTOM_LINE;
	line_style.npatn = 0;
	gslinetype(&line_style);
	gslinecolor(color);
	vpx[1] = vpx[2] = vpy[0] = vpy[2] = vpz[0] = vpz[1] = 0.0;
	vpx[0] = vpy[1] = vpz[2] = 1.0;
	ncl_mcstowcs (1, vpx, vpx);
	ncl_mcstowcs (1, vpy, vpy);
	ncl_mcstowcs (1, vpz, vpz);
	dtmp = um_dot(ur,vpx) - um_dot(ll,vpx);
	um_vctmsc (vpx, dtmp, vpx);
	dtmp = um_dot(ur,vpy) - um_dot(ll,vpy);
	um_vctmsc (vpy, dtmp, vpy);
	dtmp = um_dot(ur,vpz) - um_dot(ll,vpz);
	um_vctmsc (vpz, dtmp, vpz);
/*
.....Run box through stock matrix
*/
	if (tfflag)
	{
		um_cctmtf(ll,tf,llt);
		um_cctmtf(ur,tf,urt);
		um_vctmtf(vpx,tf,vpx);
		um_vctmtf(vpy,tf,vpy);
		um_vctmtf(vpz,tf,vpz);
	}
	else
	{
		um_vctovc(ll,llt);
		um_vctovc(ur,urt);
	}
/*
.....Draw bottom of box
*/
	for (k=1;k<=UV_act_screen[0].nvports;k++)
	{
		gsnormtran(k);
		ug_gksstli.curvwindex = k;
		gpt[0].x = llt[0];
		gpt[0].y = llt[1];
		gpt[0].z = llt[2];
		gpt[1].x = llt[0] + vpx[0];
		gpt[1].y = llt[1] + vpx[1];
		gpt[1].z = llt[2] + vpx[2];
		gpt[2].x = urt[0] - vpz[0];
		gpt[2].y = urt[1] - vpz[1];
		gpt[2].z = urt[2] - vpz[2];
		gpt[3].x = llt[0] + vpy[0];
		gpt[3].y = llt[1] + vpy[1];
		gpt[3].z = llt[2] + vpy[2];
		gpt[4].x = llt[0];
		gpt[4].y = llt[1];
		gpt[4].z = llt[2];
		gpolyline3(5,gpt);
/*
.....Draw sides of box
*/
		gpt[4].x = gpt[3].x + vpz[0];
		gpt[4].y = gpt[3].y + vpz[1];
		gpt[4].z = gpt[3].z + vpz[2];
		gpolyline3(2,&gpt[3]);

		gpt[3].x = gpt[2].x + vpz[0];
		gpt[3].y = gpt[2].y + vpz[1];
		gpt[3].z = gpt[2].z + vpz[2];
		gpolyline3(2,&gpt[2]);

		gpt[2].x = gpt[1].x + vpz[0];
		gpt[2].y = gpt[1].y + vpz[1];
		gpt[2].z = gpt[1].z + vpz[2];
		gpolyline3(2,&gpt[1]);

		gpt[1].x = gpt[0].x + vpz[0];
		gpt[1].y = gpt[0].y + vpz[1];
		gpt[1].z = gpt[0].z + vpz[2];
		gpolyline3(2,&gpt[0]);

/*
.....Draw top of box
*/
		gpt[5].x = gpt[1].x;
		gpt[5].y = gpt[1].y;
		gpt[5].z = gpt[1].z;
		gpolyline3(5,&gpt[1]);
	}
/*
.....Reset active viewport, linestyle and linecolor
*/
	ug_gksstli.curvwindex = isav;
	gslinetype(&lintyp);
	gslinecolor(lincol);
	return(UU_SUCCESS);
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_draw_cyl(which,cyl,color)
**      This function draws the stock and fixture defined for NCLIPV
**		as a cylinder.
**   PARAMETERS
**       INPUT  : which = 0 - Stock definition.
**						  1 - Fixture definition.
**				  cyli = X,Y,Z, X,Y,Z, R  of Cylinder.
**                color = Color of stock/fixture.
**                tfflag = UU_TRUE = stock has a transformation matrix.
**                tf     = Stock transformation matrix.
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_draw_cyl(which,cyli,color,tfflag,tf)
int which,color;
UU_REAL *cyli;
UU_LOGICAL tfflag;
UM_transf tf;
{
	int i,k,isav,is1,is2,is3;
	Gcolor lincol;
	Glntype lintyp,line_style;
	Gwpoint3 gpt[102],hpt[102],ipt[102][2];
	UU_REAL dv[3],tv[3],vx[3],cv[3],vl,angl,ainc;
	UU_REAL fabs();
	UU_REAL cyl[7];
/*
.....Save the current viewport, linestyle and linecolor
.....Also set the linestyle to PHANTOM and the
.....linecolor to WHITE
*/
	isav = ug_gksstli.curvwindex;
	zbytecp(lintyp,*gqlinetype());
	lincol = gqlinecolor();
	line_style.typeno = UM_PHANTOM_LINE;
	line_style.npatn = 0;
	gslinetype(&line_style);
	gslinecolor(color);
/*
.....Run cylinder through stock matrix
*/
	if (tfflag)
	{
		um_cctmtf(&cyli[0],tf,&cyl[0]);
		um_cctmtf(&cyli[3],tf,&cyl[3]);
		cyl[6] = cyli[6];
		dv[0] = tf[0][0]; dv[1] = tf[1][0]; dv[2] = tf[2][0];
		vl = um_mag(dv);
		if (vl != 0. && vl != 1.) cyl[6] = cyl[6] * vl;
	}
	else
	{
		for (i=0;i<7;i++) cyl[i] = cyli[i];
	}
/*
.....Draw a cylinder
.....Used the Draw Cutter routine from TKPLT1
.....Calculate the axis vector of cylinder
.....from delimiter points on axis
*/
	dv[0] = cyl[3] - cyl[0];
	dv[1] = cyl[4] - cyl[1];
	dv[2] = cyl[5] - cyl[2];
	vl = um_mag(dv);
	um_unitvc(dv,tv);
/*
.....Calculate major tool axis plane
.....Determine XY, YZ, or ZX plane
.....based on largest component of tool axis
*/
	is1 = 0 ; is2 = 1 ; is3 = 2;
	if (fabs(tv[0]) > fabs(tv[is3]))
	{
		is1 = 1 ; is2 = 2 ; is3 = 0;
	}
	if (fabs(tv[1]) > fabs(tv[is3]))
	{
		is1 = 2 ; is2 = 0 ; is3 = 1;
	}
/*
.....Calculate points around cylinder
.....One point at each 18 deg around circle
*/
	ainc = UM_PI / 10.;
	angl = 0.;
	cv[is3] = 0.;
	for (i=0;i<=UL_ipv_npts;i++)
	{
		cv[is1] = cos(angl);
		cv[is2] = sin(angl);
		angl = angl + ainc;
		dv[0] = cv[1]*tv[2] - cv[2]*tv[1];
		dv[1] = cv[2]*tv[0] - cv[0]*tv[2];
		dv[2] = cv[0]*tv[1] - cv[1]*tv[0];
		um_unitvc(dv,vx);
/*
........Bottom of cylinder
*/
		gpt[i].x = cyl[0] + vx[0]*cyl[6];
		gpt[i].y = cyl[1] + vx[1]*cyl[6];
		gpt[i].z = cyl[2] + vx[2]*cyl[6];
/*
........Top of cylinder
*/
		hpt[i].x = gpt[i].x + tv[0]*vl;
		hpt[i].y = gpt[i].y + tv[1]*vl;
		hpt[i].z = gpt[i].z + tv[2]*vl;
/*
........Sides of cylinder
*/
		ipt[i][0].x = gpt[i].x;
		ipt[i][0].y = gpt[i].y;
		ipt[i][0].z = gpt[i].z;
		ipt[i][1].x = hpt[i].x;
		ipt[i][1].y = hpt[i].y;
		ipt[i][1].z = hpt[i].z;
	}
/*
.....Display cylinder
*/
	for (k=1;k<=UV_act_screen[0].nvports;k++)
	{
		gsnormtran(k);
		ug_gksstli.curvwindex = k;
		gpolyline3(UL_ipv_npts+1,gpt);
		gpolyline3(UL_ipv_npts+1,hpt);
		for (i=0;i<UL_ipv_npts;i++) gpolyline3(2,&ipt[i][0]);
	}
/*
.....Reset active viewport, linestyle and linecolor
*/
	ug_gksstli.curvwindex = isav;
	gslinetype(&lintyp);
	gslinecolor(lincol);
	return(UU_SUCCESS);
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_draw_sweep(which,pts,npts,flag,zlim,nvec,color)
**      This function draws the stock defined for NCLIPV as a profile
**		sweep.
**   PARAMETERS
**       INPUT  :
**                which = 0 - Stock.  1 - Fixture.
**                pts   = Array of X,Y,Z values of points in the
**                       profile sweep.
**                npts  = Number of points in 'pts'.
**                flag  = 0 = Z-plane stock.  1 = XYZ stock;
**                zlim  = Lower and upper Z-limits of stock when flag = 0.
**                nveci = Extrusion vector and distance when flag = 1.
**                color = Color of stock/fixture.
**                tfflag = UU_TRUE = stock has a transformation matrix.
**                tf     = Stock transformation matrix.
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
ul_verify_draw_sweep(which,pts,npts,ifl,zlim,nveci,color,tfflag,tf)
int which,color;
UU_REAL zlim[2];
Gwpoint3 *pts;
int npts,ifl;
UM_vector nveci;
UU_LOGICAL tfflag;
UM_transf tf;
{
	int i,k,isav,np,flag;
	Gcolor lincol;
	Glntype lintyp,line_style;
	Gwpoint3 gpt[2],*tpts;
	UU_REAL vl;
	UU_REAL fabs();
	UM_coord pt1,pt2;
	UM_vector nvec,vc;
/*
.....Save the current viewport, linestyle and linecolor
.....Also set the linestyle to PHANTOM and the
.....linecolor to WHITE
*/
	isav = ug_gksstli.curvwindex;
	zbytecp(lintyp,*gqlinetype());
	lincol = gqlinecolor();
	line_style.typeno = UM_PHANTOM_LINE;
	line_style.npatn = 0;
	gslinetype(&line_style);
	gslinecolor(color);
/*
.....Close the composite curve
*/
	np = npts + 1;
	tpts = (Gwpoint3 *)uu_malloc(sizeof(Gwpoint3)*np);
	if (tpts == UU_NULL) goto done;
	for (i=0;i<npts;i++)
	{
		tpts[i].x = pts[i].x;
		tpts[i].y = pts[i].y;
		tpts[i].z = pts[i].z;
	}
	tpts[npts].x = pts[0].x;
	tpts[npts].y = pts[0].y;
	tpts[npts].z = pts[0].z;
/*
.....Adjust the lower z-axis coords
*/
	flag = ifl;
	if (flag == 0)
	{
		vl = zlim[1] - zlim[0];
		for (k=0;k<np;k++)
		{
			tpts[k].z = zlim[0];
		}
	}
/*
.....Apply stock transformation matrix
*/
	if (tfflag)
	{
		for (i=0;i<np;i++)
		{
			pt1[0] = tpts[i].x; pt1[1] = tpts[i].y; pt1[2] = tpts[i].z;
			um_cctmtf(pt1,tf,pt2);
			tpts[i].x = pt2[0]; tpts[i].y = pt2[1]; tpts[i].z = pt2[2];
		}
		if (flag == 0)
		{
			vc[0] = 0.; vc[1] = 0.; vc[2] = vl;
			um_vctmtf(vc,tf,nvec);
			flag = 1;
		}
		else
			um_vctmtf(nveci,tf,nvec);
	}
	else
		um_vctovc(nveci,nvec);
/*
.....Display the base curve
*/
	for (k=1;k<=UV_act_screen[0].nvports;k++)
	{
		gsnormtran(k);
		ug_gksstli.curvwindex = k;
		gpolyline3(np,tpts);
	}
/*
.....Calculate top curve
*/
	if (flag == 0)
	{
		vl = zlim[1] - zlim[0];
		for (k=0;k<np;k++) tpts[k].z = zlim[1];
	}
	else
	{
		for (k=0;k<np;k++)
		{
			tpts[k].x = tpts[k].x + nvec[0];
			tpts[k].y = tpts[k].y + nvec[1];
			tpts[k].z = tpts[k].z + nvec[2];
		}
	}
/*
.....Draw the extruded composite curve
*/
	for (k=1;k<=UV_act_screen[0].nvports;k++)
	{
		gsnormtran(k);
		ug_gksstli.curvwindex = k;
		gpolyline3(np,tpts);
	}
/*
.....Draw the connecting lines
*/
	for (i=0;i<np;i++)
	{
		if (i != npts)
		{
			gpt[0].x = pts[i].x;
			gpt[0].y = pts[i].y;
			gpt[0].z = pts[i].z;
		}
		else
		{
			gpt[0].x = pts[0].x;
			gpt[0].y = pts[0].y;
			gpt[0].z = pts[0].z;
		}
		if (tfflag)
		{
			pt1[0] = gpt[0].x; pt1[1] = gpt[0].y; pt1[2] = gpt[0].z;
			um_cctmtf(pt1,tf,pt2);
			gpt[0].x = pt2[0]; gpt[0].y = pt2[1]; gpt[0].z = pt2[2];
		}
		gpt[1].x = tpts[i].x;
		gpt[1].y = tpts[i].y;
		gpt[1].z = tpts[i].z;
		for (k=1;k<=UV_act_screen[0].nvports;k++)
		{
			gsnormtran(k);
			ug_gksstli.curvwindex = k;
			gpolyline3(2,gpt);
		}
	}
/*
.....Reset active viewport, linestyle and linecolor
*/
	ug_gksstli.curvwindex = isav;
	gslinetype(&lintyp);
	gslinecolor(lincol);
	uu_free(tpts);
done:;
	return(UU_SUCCESS);
}

/*********************************************************************
**   E_FUNCTION:int ul_verify_draw_revsf(origpt,axisvc,pts,npts,sang,eang,
**                     which,color);
**      This function draws the stock defined for NCLIPV as a profile
**		revsf.
**   PARAMETERS
**       INPUT  : origpt = Origin of revsurf.
**                axisvc = Axis vector of revsurf.
**                pts    = Array of points in curve to be revolved.
**                npts   = Number of points in 'pts'.
**                sang   = Starting angle.
**                eang   = Ending angle.
**                which  = 0 = Stock.  1 = Fixture.
**                color = Color of stock/fixture.
**                tfflag = UU_TRUE = stock has a transformation matrix.
**                tf     = Stock transformation matrix.
**       OUTPUT : none.
**   RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_draw_revsf(origpt,axisvc,pts,npts,sang,eang,which,color,tfflag,tf)
UM_coord origpt;
UM_vector axisvc;
UM_angle sang,eang;
Gwpoint3 *pts;
int npts;
int which,color;
UU_LOGICAL tfflag;
UM_transf tf;
{
#define NSEG 21
	int i,j,k,inc,isav;
	Gcolor lincol;
	Glntype lintyp,line_style;
	Gwpoint3 *opt,spt[NSEG],ept[NSEG];
	UM_coord temp;
	UU_REAL ang,ainc;
	UM_transf mtf;
/*
.....Initialize routine
*/
	ang  = sang / (180./UM_PI);
	ainc = ((eang-sang)/(180./UM_PI)) / (UU_REAL)(NSEG-1);
	inc = 0;
/*
.....Save the current viewport, linestyle and linecolor
.....Also set the linestyle to PHANTOM and the
.....linecolor to WHITE
*/
	isav = ug_gksstli.curvwindex;
	zbytecp(lintyp,*gqlinetype());
	lincol = gqlinecolor();
	line_style.typeno = UM_PHANTOM_LINE;
	line_style.npatn = 0;
	gslinetype(&line_style);
	gslinecolor(color);
/*
.....Allocate internal storage
*/
	opt = (Gwpoint3 *)uu_malloc(sizeof(Gwpoint3)*npts);
	if (opt == UU_NULL) goto done;
/*
.....Draw the base composite curve
*/
	for (i=0;i<NSEG;i++)
	{
		um_rotlntf(origpt,axisvc,ang,mtf);
		ang = ang + ainc;
		for (j=0;j<npts;j++)
		{
			temp[0] = pts[j].x; temp[1] = pts[j].y; temp[2] = pts[j].z;
			um_cctmtf(temp,mtf,temp);
			if (tfflag) um_cctmtf(temp,tf,temp);
			opt[j].x = temp[0]; opt[j].y = temp[1]; opt[j].z = temp[2];
			if (j == 0)
			{
				spt[inc].x = opt[j].x;
				spt[inc].y = opt[j].y;
				spt[inc].z = opt[j].z;
			}
			else if (j == (npts-1))
			{
				ept[inc].x = opt[j].x;
				ept[inc].y = opt[j].y;
				ept[inc].z = opt[j].z;
			}
		}
		inc++;
		for (k=1;k<=UV_act_screen[0].nvports;k++)
		{
			gsnormtran(k);
			ug_gksstli.curvwindex = k;
			gpolyline3(npts,opt);
		}
	}
/*
.....Draw the connecting lines
*/
	gpolyline3(NSEG,spt);
	gpolyline3(NSEG,ept);
/*
.....Reset active viewport, linestyle and linecolor
*/
	ug_gksstli.curvwindex = isav;
	gslinetype(&lintyp);
	gslinecolor(lincol);
	uu_free(opt);
done:;
	return(UU_SUCCESS);
}
