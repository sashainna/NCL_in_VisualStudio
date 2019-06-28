/*********************************************************************
**	FILENAME: lverify2.c
**	CONTAINS:
**				ulf_verify_box()
**				ulf_verify_chips()
**				ulf_verify_compos()
**				ulf_verify_cone()
**				ulf_verify_copy()
**				ul_verify_copy()
**				ulf_verify_cyl()
**				ulf_verify_decomp()
**				ulf_verify_load()
**				ulf_verify_modify()
**				ulf_verify_move()
**				ulf_verify_remove()
**				ulf_verify_solid()
**				ulf_verify_sphere()
**				ulf_verify_stl()
**				ulf_verify_torus()
**          ulf_create_idlist()
**
**     MODULE NAME AND RELEASE LEVEL 
**       lverify2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:22
*********************************************************************/

#include <stdio.h>
#include <math.h>
#include "usysdef.h"
#include "lcom.h"
#include "mfort.h"
#include "nclfc.h"
#include "mdattr.h"
#include "mdrel.h"
#include "msol.h"
#include "nccs.h"
#include "nclmplay.h"
#include "lipv.h"
#include "xenv1.h"
#include "xfsys1.h"

extern UX_pathname NCL_tpdir;

/*********************************************************************
**   E_FUNCTION:void ulf_verify_box(which,ifl,param,numid,kerr)
**      This function defines the NCLIPV stock in the form of a box.
**   PARAMETERS
**       INPUT  :
**            which = 0 - Stock definition.
**                    1 - Fixture definition.
**
**            ifl   = 1 - Box PT PT
**                    2 - Box PT L W H
**                    3 - Bounding box
**
**            param = ifl = 1,2 - x1,y1,z1,x2,y2,z2
**                          3   - Box expansion
**
**            numid = ID number for stock.
**
**       OUTPUT :
**            numid = Incremented stock ID number.
**
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_box(which,ifl,param,numid,kerr)
UM_int4 *which,*ifl,*numid,*kerr;
UM_real8 param[];
{
	int i,nt,modals[20];
	int icurpt;
	UN_clstruc *iclpt[4];
	UU_REAL ll[3],ur[3],stock[6];
	UU_REAL fabs();
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Create stock/fixture box using
.....using corner points of box
*/
	if (*ifl == 1 || *ifl == 2)
	{
		for (i=0;i<3;i++)
		{
			ll[i] = param[i];
			ur[i] = param[i+3];
		}
	}
/*
.....Create stock/fixture box using
.....bounding box of motion
*/
	else if (*ifl == 3)
	{
/*
........Get the motion bounding box
*/
		ncl_play_initscan(modals,iclpt,&icurpt,UU_FALSE);
		nt = 0;
		ncl_motion_playback(modals,2,stock,UU_NULL,&nt);
		ncl_play_resetscan(iclpt,icurpt);
		for (i=0;i<3;i++)
		{
			ll[i] = stock[i] - param[0];
			ur[i] = stock[i+3] + param[0];
		}
	}
/*
........Verify the coordinates of the box are valid
........If the Z-values of both points are the same
........then prompt for Z-levels of points
*/
	if (fabs(ll[0]-ur[0]) <= .001 || fabs(ll[1]-ur[1]) <= .001 ||
		fabs(ll[2]-ur[2]) <= .001) goto err1;
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
/*
.....Create the stock
*/
	ul_ipv_set_defered();
	ul_ipv_add_stock(*which,LW_STOCK_BOX,stock,numid,UU_NULL,0,1);
	ul_ipv_set_immediate(UU_FALSE);
	goto done;
/*
.....Invalid coordinates for stock
*/
err1:;
	*kerr = 1;
	goto done;
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_chips(pvs,npvs)
**      This function removes stock chips from an NCLIPV session.
**   PARAMETERS
**       INPUT  :
**            pvs   = Array of point-vectors to use to select stocks
**                    to keep.
**
**            npvs  = Number of point-vectors in array.
**
**       OUTPUT : none
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_chips(pvs,npvs)
UM_real8 pvs[];
UM_int4 *npvs;
{
/*
.....Do nothing if IPV is not active
*/
	if (UL_ipv == 0) return;
/*
.....Remove requested chips
*/
	ul_ipv_remove_chips(UU_TRUE,pvs,*npvs);
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_compos(which,idn,numids,nids,kerr)
**      This function create a composite NCLIPV stock.
**   PARAMETERS
**       INPUT  :
**            which  = 0 - Stock definition.
**                     1 - Fixture definition.
**
**            idn    = ID number for stock.
**
**            numids = ID numbers to combine into a single stock.
**
**            nids   = Number of IDs in 'numids' array.
**
**       OUTPUT :
**            idn    = Incremented stock ID number.
**
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_compos(which,idn,numids,nids,kerr)
UM_int4 *which,*idn,numids[],*nids,*kerr;
{
	int i,n,*iptr,status;
	UU_LIST idlist;
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Create list of ids.
*/
	status = ulf_create_idlist(*which,numids,*nids,&idlist,kerr);
	if (status == UU_FAILURE) goto done;
	iptr = (int *)UU_LIST_ARRAY(&idlist);
	n = UU_LIST_LENGTH(&idlist);
/*
.....Create the composite stock
*/
	status = ul_ipv_add_stock(*which,LW_STOCK_COMPOS,UU_NULL,idn,iptr,n);
	if (status != UU_SUCCESS) *kerr = 1;
/*
.....End of routine
*/
done:;
	uu_list_free(&idlist);
	return;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_cone(which,param,numid,kerr)
**      This function defines the NCLIPV stock in the form of a cone.
**   PARAMETERS
**       INPUT  :
**            which = 0 - Stock definition.
**                    1 - Fixture definition.
**
**            param = x,y,z,i,j,k,r,length
**
**            numid = ID number for stock.
**
**       OUTPUT :
**            numid = Incremented stock ID number.
**
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_cone(which,param,numid,kerr)
UM_int4 *which,*numid,*kerr;
UM_real8 param[];
{
	UU_REAL stock[8];
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Define the stock as a cone
*/
	um_vctovc(&param[0],&stock[0]);
	um_vctmsc(&param[3],param[8],&stock[3]);
	stock[6] = param[6];
	stock[7] = param[7];
	ul_ipv_set_defered();
	ul_ipv_add_stock(*which,LW_STOCK_CONE,stock,numid,UU_NULL,0,1);
	ul_ipv_set_immediate(UU_FALSE);
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_copy(which,istk,numid,xfl,tf,kerr)
**      This function copies an IPV stock into a new stock definition.
**   PARAMETERS
**       INPUT  :
**            which = 0 - Stock definition.
**                    1 - Fixture definition.
**
**            istk  = Stock to copy from.
**
**            numid = ID number for stock.
**
**            ncopy = Number of copies to make.
**
**            xfl   = 1 - Transform copied stock.  The stock is transformed
**                        from its current position.
**                    2 - Translate copied stock.
**                    3 - XYROT of copied stock.
**                    4 - YZROT of copied stock.
**                    5 - ZXROT of copied stock.
**                    0 - Don't.
**
**            tf    = Transformation matrix when xfl = 1, XYZ translation
**                    distances when xfl = 2, or rotation angle when
**                    xfl = 3,4,5.
**
**       OUTPUT :
**            numid = Incremented stock ID number.
**
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_copy(which,istk,numid,ncopy,xfl,params,kerr)
UM_int4 *which,*numid,*kerr,*istk,*ncopy,*xfl;
UU_REAL *params;
{
	int npts,i;
	UU_LOGICAL flag;
	UU_REAL stock[6];
	UM_vector tvec;
	UM_transf tf1,tf2;
	LW_stock_struc *sd;
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Find the requested stock
*/
	ul_ipv_find_stock(*which,*istk,&sd);
	if (sd == UU_NULL) goto failed;
	ul_ipv_set_defered();
/*
.....Setup base transformation matrix
*/
		flag = UU_FALSE;
		if (*xfl != 0)
		{
/*
........Standard matrix
*/
			if (*xfl == 1)
				ncl_34mx_to_43mx(params,tf1);
/*
........Translation matrix
*/
			else if (*xfl == 2)
			{
				um_identtf(tf1);
				tf1[3][0] += params[0];
				tf1[3][1] += params[1];
				tf1[3][2] += params[2];
			}
/*
........Rotation matrix
*/
			else
			{
				um_identtf(tf1);
				um_nullvc(tvec);
				if (*xfl == 3) tvec[2] = 1.;
				else if (*xfl == 4) tvec[0] = 1.;
				else tvec[2] = 1.;
				um_rottf(tvec,params[0]/UM_RADIAN,tf1);
			}
			um_tftotf(tf1,tf2);
			flag = UU_TRUE;
		}
/*
.....Copy stock data
*/
	for (i=0;i<*ncopy;i++)
	{
		ul_verify_copy(*which,sd,numid,flag,tf1);
/*
.....Update matrix to next increment
*/
		if (flag) um_tftmtf(tf1,tf2,tf1);
	}
/*
.....End of routine
*/
done:;
	ul_ipv_set_immediate(UU_FALSE);
	return;
/*
.....Could not create stock
*/
failed:;
	*kerr = 1;
	goto done;
}

/*********************************************************************
**   E_FUNCTION: ul_verify_copy(which,sd,numid,xfl,tf)
**      This function clones an existing IPV stock into a new IPV stock.
**   PARAMETERS
**       INPUT  :
**            which = 0 - Stock definition.
**                    1 - Fixture definition.
**
**            sd    = Stock to copy from.
**
**            numid = ID number for stock.
**
**            xfl   = 1 - Transform copied stock.  The stock is transformed
**                        from its current position.
**                    2 - Translate copied stock.
**                    0 - Don't.
**
**            tf    = Transformation matrix when xfl = 1, or XYZ translation
**                    distances when xfl = 2.
**
**       OUTPUT :
**            numid = Incremented stock ID number.
**            tf    = Returns the updated transformation matrix.
**
**   RETURNS: UU_SUCCESS or UU_FAILURE.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_copy(which,sd,numid,xfl,tf)
int which;
LW_stock_struc *sd;
int *numid;
UU_LOGICAL xfl;
UU_REAL *tf;
{
	int status,idn,ifl,inc;
	LW_stock_struc sdtmp,*stock,*sdcomp;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Copy stock data
*/
	ifl = -1;
	do
	{
		inc = ifl;
		ul_ipv_get_next_stock(sd,&stock,&ifl,UU_TRUE);
		if (ifl == -2) break;
		ul_ipv_clone_stock(which,stock,&sdtmp,numid);
/*
.....Update stock by specified matrix
*/
		if (xfl)
		{
			if (!sdtmp.mxflag) um_identtf(sdtmp.matrix);
			um_tftmtf(sdtmp.matrix,tf,sdtmp.matrix);
			um_inverttf(sdtmp.matrix,sdtmp.invmx);
			sdtmp.mxflag = UU_TRUE;
			sdtmp.mxchg = UU_TRUE;
			sdtmp.invflag = UU_TRUE;
		}
/*
.....Get Stock ID
*/
		if (*numid == 0 || (sd->type == LW_STOCK_COMPOS && ifl != 0))
			idn = sdtmp.id;
/*
.....Stock ID is provided
*/
		else
			idn = *numid;
/*
.....Stock already exists
.....Delete old stock
*/
		if (sd->type != LW_STOCK_COMPOS || ifl == 0)
		{
			ul_ipv_find_stock(which,idn,&stock);
			if (stock != UU_NULL) ul_ipv_remove_stock(which,stock);
		}
/*
.....Transform stock
*/
		if (sd->type != LW_STOCK_COMPOS || ifl != 0)
			if (xfl) ul_ipv_modify_stock(sdtmp,UU_FALSE);
/*
.....Add the stock to the main stock list
*/
		if (sd->type == LW_STOCK_COMPOS && ifl != 0)
		{
			stock = (LW_stock_struc *)sdcomp->data;
			stock[inc] = sdtmp;
		}
		else
		{
			stock = LW_stock_data[which];
			*stock = sdtmp;
			stock->id = idn; idn = idn + 1; if (*numid != 0) *numid = idn;
			sdcomp = stock;
/*
.....Allocate next stock
*/
			LW_stock_data[which] =
				(LW_stock_struc *)uu_lsinsrt(LW_stock_data[which],
				sizeof(LW_stock_struc));
			if (LW_stock_data[which] == UU_NULL) goto failed;
			LW_nstock[which]++;
		}
	} while (ifl != -1);
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	return(status);
/*
.....Failed to create new stock
*/
failed:;
	status = UU_FAILURE;
	goto done;
}

/*********************************************************************
**   E_FUNCTION: ul_verify_copy1(which,sd,numid)
**      This function copies an IPV stock into a new stock definition.
**   PARAMETERS
**       INPUT  :
**            which = 0 - Stock definition.
**                    1 - Fixture definition.
**
**            sd    = Stock to copy from.
**
**            numid = ID number for stock.
**
**       OUTPUT :
**            numid = Incremented stock ID number.
**
**   RETURNS: UU_SUCCESS or UU_FAILURE.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_verify_copy1(which,sd,numid)
int which;
LW_stock_struc *sd;
int *numid;
{
	int npts,i,status;
	UU_REAL stock[6];
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....Copy stock data
*/
	switch(sd->type)
	{
/*
........Stock primitives
*/
	case LW_STOCK_BOX:
	case LW_STOCK_CONE:
	case LW_STOCK_CYLINDER:
	case LW_STOCK_SPHERE:
	case LW_STOCK_TORUS:
		ul_ipv_add_stock(which,sd->type,sd->data,numid,UU_NULL,0,1);
		break;
/*
........Revolved stock
*/
	case LW_STOCK_REVOLVE:
		npts = (int)sd->data[8];
		ul_ipv_add_stock(which,LW_STOCK_REVOLVE,sd->data,numid,&(sd->data[9]),
			npts,1);
		break;
/*
........Swept stock
*/
	case LW_STOCK_SWEEP:
		npts = (int)sd->data[3];
		ul_ipv_add_stock(which,LW_STOCK_SWEEP,sd->data,numid,&(sd->data[4]),
			npts,1);
		break;
/*
........STL file stock
*/
	case LW_STOCK_FILE:
		npts = strlen((char *)sd->data);
		stock[0] = (UU_REAL)sd->bin; stock[1] = (UU_REAL)sd->units;
		ul_ipv_add_stock(which,LW_STOCK_FILE,stock,numid,sd->data,
			npts,1);
		break;
	}
/*
.....End of routine
*/
done:;
	return(status);
/*
.....Could not create stock
*/
failed:;
	status = UU_FAILURE;
	goto done;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_cyl(which,param,numid,kerr)
**      This function defines the NCLIPV stock in the form of a cylinder.
**   PARAMETERS
**       INPUT  :
**            which = 0 - Stock definition.
**                    1 - Fixture definition.
**
**            param = x,y,z,i,j,k,r,length
**
**            numid = ID number for stock.
**
**       OUTPUT :
**            numid = Incremented stock ID number.
**
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_cyl(which,param,numid,kerr)
UM_int4 *which,*numid,*kerr;
UM_real8 param[];
{
	UU_REAL stock[8];
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Define the stock as a cylinder
*/
	um_vctovc(&param[0],&stock[0]);
	um_vctmsc(&param[3],param[7],&stock[3]);
	stock[6] = param[6];
	ul_ipv_set_defered();
	ul_ipv_add_stock(*which,LW_STOCK_CYLINDER,stock,numid,UU_NULL,0,1);
	ul_ipv_set_immediate(UU_FALSE);
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**   E_FUNCTION: ulf_verify_decomp(which,idn,numids,nids,kerr)
**      This function decomposes composite NCLIPV stocks.
**   PARAMETERS
**       INPUT  :
**            which  = 0 - Stock definition.
**                     1 - Fixture definition.
**
**            idn    = ID number for stock.
**
**            numids = ID numbers to decompose.
**
**            nids   = Number of IDs in 'numids' array.
**
**       OUTPUT :
**            idn    = Incremented stock ID number.
**
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_decomp(which,idn,numids,nids,kerr)
UM_int4 *which,*idn,numids[],*nids,*kerr;
{
	int i,n,*iptr,status;
	UU_LIST idlist;
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Create list of ids.
*/
	status = ulf_create_idlist(*which,numids,*nids,&idlist,kerr);
	if (status == UU_FAILURE) goto done;
	iptr = (int *)UU_LIST_ARRAY(&idlist);
	n = UU_LIST_LENGTH(&idlist);
/*
.....Decompose the composite stocks
*/
	for (i=0;i<n;i++)
	{
		status = ul_ipv_decomp_stock(*which,idn,iptr[i]);
		if (status != UU_SUCCESS) *kerr = 1;
	}
/*
.....End of routine
*/
done:;
	uu_list_free(&idlist);
	return;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_load(fnam,numid,kerr)
**      This function loads an external NCLIPV STOCK/FIXTURE file.  It
**      will first attempt to load the exact filename and if that fails
**      it will attempt to load the file from the 'NCL_INCDIR' directory.
**   PARAMETERS
**       INPUT  :
**            fnam  = Name of file to load.
**
**            nci   = Number of chars in 'fnam'.
**
**            numid = ID number for stock.
**
**       OUTPUT :
**            numid = Incremented stock ID number.
**
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_load(fnam,nci,numid,kerr)
UM_int4 *numid,*kerr,*nci;
UM_f77_str_ptr fnam;
{
	int stat,fstat,mode,idn[2],nid;
	char *cnam;
	UX_pathname fullname,tnam,lbuf;
	FILE *fd;
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Initialize routine
*/
	cnam = UM_cstr_of_f77_str(fnam);
	strncpy(tnam,cnam,*nci);
	tnam[*nci] = '\0';

	mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
/*
.....Make sure the file exists
*/
	stat = ux_file_inquire(UU_NULL,UU_NULL,tnam,UU_NULL,UU_NULL,
		&mode,&fstat,lbuf,UX_NPRTERRS);
/*
.....Open the file for reading
*/
	if (stat == UU_SUCCESS) stat = ux_fopen0(tnam,"r",&fd);
/*
.....Open failed
.....try the NCL_INCDIR directory
*/
	if (stat != UU_SUCCESS)
	{
		ul_build_full_fname("NCL_INCDIR",tnam,".stk",fullname);
		mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
		stat = ux_file_inquire(UU_NULL,UU_NULL,fullname,UU_NULL,UU_NULL,
			&mode,&fstat,lbuf,UX_NPRTERRS);
		stat = ux_fopen0(fullname,"r",&fd);
	}
/*
......Open failed
......check if this file has a path, if it is not, look at the clfile directory
*/
	if ((stat != UU_SUCCESS)&&(NCL_tpdir[0]!='\0')&&(UN_clfile!=0))
/*
......check if this file has a path, if it is not, default to the clfile directory
*/
	{
		ul_build_full_fname(NCL_tpdir, tnam, ".stk", fullname);
		mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
		stat = ux_file_inquire(UU_NULL,UU_NULL,fullname,UU_NULL,UU_NULL,
			&mode,&fstat,lbuf,UX_NPRTERRS);
		stat = ux_fopen0(fullname,"r",&fd);
	}
	if (stat != UU_SUCCESS) goto failed;
/*
.....Process file
*/
	nid = *numid;
	idn[0] = idn[1] = nid;
	ul_ipv_set_defered();
	stat = ul_ipv_load_stock_cmds(fd,UU_TRUE,idn,UU_NULL,UU_NULL,UU_TRUE);
	*numid = (idn[0]-nid) + (idn[1]-nid);
	ul_ipv_set_immediate(UU_FALSE);
	ux_fclose0(fd);
	if (stat != UU_SUCCESS) goto failed;
/*
.....End of routine
*/
done:;
	return;
/*
.....Could not read stock file
*/
failed:
	*kerr = 1;
	goto done;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_modify(which,numid,icol,ivis,tol,itrans,iact,
**                                     kerr)
**      This function modifies an NCLIPV stock attributes.
**   PARAMETERS
**       INPUT  :
**            which  = 0 - Stock definition.
**                     1 - Fixture definition.
**
**            numids = ID numbers for stock to modify.
**
**            nids   = Number of IDs in 'numids' array.
**
**            icol   = New color for stock.  -1 = don't change.
**
**            ivis   = New visibility for stock.  -1 = don't change.
**
**            tol    = New tolerance for stock.  -1 = don't change.
**
**            itrans = New translucency for stock.  -1 = don't change.
**
**            iact   = New active flag for stock.  -1 = don't change.
**
**       OUTPUT :
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_modify(which,numids,nids,icol,ivis,tol,itrans,iact,kerr)
UM_int4 *which,numids[],*nids,*icol,*ivis,*itrans,*iact,*kerr;
UM_real8 *tol;
{
	int i,n,*iptr,status,ifl;
	LW_stock_struc *sd,*stock;
	UU_LIST idlist;
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Create list of ids.
*/
	status = ulf_create_idlist(*which,numids,*nids,&idlist,kerr);
	if (status == UU_FAILURE) goto done;
	iptr = (int *)UU_LIST_ARRAY(&idlist);
	n = UU_LIST_LENGTH(&idlist);
/*
.....Set the new stock attributes
*/
	ul_ipv_set_defered();
	for (i=0;i<n;i++)
	{
		ul_ipv_find_stock(*which,iptr[i],&sd);
		if (sd == UU_NULL) goto done;
		ifl = -1;
		do
		{
			ul_ipv_get_next_stock(sd,&stock,&ifl,UU_TRUE);
			if (ifl == -2) break;
			if (*icol != -1) stock->color = *icol;
			if (*ivis != -1) stock->visible = *ivis;
			if (*tol != -1.) stock->toler = *tol;
			if (*itrans != -1) stock->translucency = *itrans;
			if (*iact != -1) stock->active = *iact;
/*
.....Modify the stock attributes
*/
			if (ifl != 0) ul_ipv_modify_stock(stock,UU_FALSE);
		} while (ifl != -1);
	}
	ul_ipv_set_immediate(UU_FALSE);
/*
.....End of routine
*/
done:;
	uu_list_free(&idlist);
	return;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_move(which,numids,nids,params,mxname,incr,kerr)
**      This function applies a matrix to an NCLIPV stock.
**   PARAMETERS
**       INPUT  :
**            which  = 0 - Stock definition.
**                     1 - Fixture definition.
**
**            numids = ID numbers for stock.
**
**            nids   = Number of IDs in 'numids' array.
**
**            params = Matrix canonical data.
**
**            mxname = Matrix name.
**            nci   = length of Matrix name.
**
**            incr  = 0 = Absolute positioning, 1 = Incremental.
**
**       OUTPUT :
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_move(which,numids,nids,params,mxname, nci, incr, kerr)
UM_int4 *which,numids[], *nids, *nci, *incr, *kerr;
UM_real8 params[];
UM_f77_str_ptr mxname;
{
	int i,n,*iptr,status,ifl;
	char *cnam, tnam[NCL_MAX_LABEL+1];
	UU_REAL mxparam[4][3],det;
	LW_stock_struc *sd,*stock;
	UU_LIST idlist;
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Initialize routine
*/
	cnam = UM_cstr_of_f77_str(mxname);
	strncpy(tnam,cnam,*nci);
	tnam[*nci] = '\0';
/*
.....Create list of ids.
*/
	status = ulf_create_idlist(*which,numids,*nids,&idlist,kerr);
	if (status == UU_FAILURE) goto done;
	iptr = (int *)UU_LIST_ARRAY(&idlist);
	n = UU_LIST_LENGTH(&idlist);
/*
.....Get the matrix parameters
*/
	ncl_34mx_to_43mx(params,mxparam);
	um_determinant(mxparam,&det);
	if (det <= 0.) goto nomx;
/*
.....Modify the stock matrices
*/
	ul_ipv_set_defered();
	for (i=0;i<n;i++)
	{
		ul_ipv_find_stock(*which,iptr[i],&sd);
		if (sd == UU_NULL) goto done;
		ifl = -1;
		do
		{
			ul_ipv_get_next_stock(sd,&stock,&ifl,UU_TRUE);
			if (ifl == -2) break;
			strcpy(stock->mxname, tnam);
			if (*incr == 1) stock->mxchg = UU_TRUE;
			if (!um_tfeqtf(mxparam,stock->matrix)) stock->mxchg = UU_TRUE;
			if (!stock->mxflag) stock->mxchg = UU_TRUE;
			stock->mxflag = UU_TRUE;
/*
.....Store incremental matrix
*/
			if (*incr == 1)
				um_tftmtf(stock->matrix,mxparam,stock->matrix);
			else
				um_tftotf(mxparam,stock->matrix);
			if (stock->type != LW_STOCK_COMPOS)
				ul_ipv_modify_stock(stock,UU_FALSE);
		} while (ifl != -1);
	}
	ul_ipv_set_immediate(UU_FALSE);
/*
.....End of routine
*/
done:;
	uu_list_free(&idlist);
	return;
/*
.....Could not load matrix
*/
nomx:
	*kerr = -1;
	goto done;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_remove(which,numids,nids,kerr)
**      This function removes an NCLIPV stock.
**   PARAMETERS
**       INPUT  :
**            which  = 0 - Stock definition.
**                     1 - Fixture definition.
**
**            numids = ID numbers for stocks to remove.
**
**            nids   = Number of IDs in 'numids' array.
**
**
**       OUTPUT :
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_remove(which,numids,nids,kerr)
UM_int4 *which,numids[],*nids,*kerr;
{
	int i, n, *iptr, status = UU_SUCCESS;
	LW_stock_struc *sd;
	UU_LIST idlist;
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Create list of ids.
*/
	status = ulf_create_idlist(*which,numids,*nids,&idlist,kerr);
	if (status == UU_FAILURE) goto done;
	iptr = (int *)UU_LIST_ARRAY(&idlist);
	n = UU_LIST_LENGTH(&idlist);
/*
.....Remove stocks
*/
	for (i=0;i<n;i++)
	{
		ul_ipv_find_stock(*which,iptr[i],&sd);
		if (sd != UU_NULL) ul_ipv_remove_stock(*which,sd);
	}
/*
.....Flush the graphics
*/
/*
	ul_ipv_flush();
	um_reset_pocket_graphics(UM_IPV_WINDOW);
*/
/*
.....End of routine
*/
done:;
	uu_list_free(&idlist);
	return;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_reset_cutcolor(which,numids,nids,kerr)
**      This function resets all cut faces of a solid back to the
**      default color of the solid.
**   PARAMETERS
**       INPUT  :
**            which  = 0 - Stock definition.
**                     1 - Fixture definition.
**
**            numids = ID numbers for stocks to remove.
**
**            nids   = Number of IDs in 'numids' array.
**
**
**       OUTPUT :
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_reset_cutcolor(which,numids,nids,kerr)
UM_int4 *which,numids[],*nids,*kerr;
{
	int i, n, *iptr, status = UU_SUCCESS,ifl;
	LW_stock_struc *sd,*sdtmp;
	UU_LIST idlist;
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Create list of ids.
*/
	if (*nids == 0)
	{
		sd = LW_stock_first[*which];
		n = LW_nstock[*which];
	}
	else
	{
		status = ulf_create_idlist(*which,numids,*nids,&idlist,kerr);
		if (status == UU_FAILURE) goto done;
		iptr = (int *)UU_LIST_ARRAY(&idlist);
		n = UU_LIST_LENGTH(&idlist);
	}
/*
.....Remove stocks
*/
	for (i=0;i<n;i++)
	{
		if (*nids != 0)
			ul_ipv_find_stock(*which,iptr[i],&sd);
		if (sd != UU_NULL)
		{
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sd,&sdtmp,&ifl);
				if (ifl == -2) break;
				ul_ipv_color_faces(sd);
			} while (ifl != -1);
		}
		if (*nids == 0)
			sd = (LW_stock_struc *)uu_lsnext(sd);
	}
/*
.....Flush the graphics
*/
/*
	ul_ipv_flush();
	um_reset_pocket_graphics(UM_IPV_WINDOW);
*/
/*
.....End of routine
*/
done:;
	if (*nids != 0) uu_list_free(&idlist);
	return;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_solid(which,nclkey,numid,kerr)
**      This function defines the NCLIPV stock using a Visual Solid.
**   PARAMETERS
**       INPUT  :
**            which  = 0 - Stock definition.
**                    1 - Fixture definition.
**
**            nclkey = Unibase Key of Visual Solid.
**
**            numid  = ID number for stock.
**
**       OUTPUT :
**            numid  = Incremented stock ID number.
**
**            kerr   = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_solid(which,nclkey,numid,kerr)
UM_int4 *which,*numid,*kerr;
UU_KEY_ID *nclkey;
{
	int status,np,inc,rel,nso,nsf,ipt;
	UM_int4 stkid,keys[50],nkeys;
	UU_LOGICAL compfl;
	UU_REAL *p;
	LW_stock_struc stock,*sd;
	struct UM_solid_rec solid,csol;
	union {UU_REAL rval; int ival[2];} tprm;
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Get the Solid which
.....defines the stock
*/
	solid.key = *nclkey;
	status = uc_retrieve_data(&solid,sizeof(solid));
	if (status != UU_SUCCESS) goto failed;
/*
.....Composite solid
*/
	compfl = UU_FALSE;
	nkeys = 1;
	stkid = *numid;
	if (solid.type == UM_COMPOS_SOLID)
	{
		ncl_solid_count_components(&solid,&nso,&nsf);
		if (nsf != 0) goto nosf;
		compfl = UU_TRUE;
		csol = solid;
		nkeys = csol.no_netkey;
		stkid += nso;
	}
/*
.....Loop through solids
*/
	ipt = 0;
	for (inc=0;inc<nkeys;inc++)
	{
		if (compfl)
		{
			solid.key = csol.netkey[inc];
			ur_retrieve_data_relnum(solid.key,&rel);
			if (rel != UM_SOLID_REL) continue;
			status = uc_retrieve_data(&solid,sizeof(solid));
			if (status != UU_SUCCESS) goto failed;
		}
/*
........Apply MODSYS
*/
		np = solid.no_sdata;
		p = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*np);
		if (p == UU_NULL) goto failed;
		status = ur_retrieve_data_varlist(solid.key,1,p,1,np);
		ncl_solid_wcstomcs(solid.type,p,UU_TRUE);
		if (p == UU_NULL) goto failed;
		solid.sdata = p;
/*
........Convert solid to stock
*/
		ncl_solid_to_stock(&solid,&stock);
/*
........Store stock
*/
		keys[ipt++] = stkid;
		status = ul_verify_copy1(*which,&stock,&stkid);
		if (status != UU_SUCCESS) goto failed;
/*
.....Use xform from STL solid
*/
		if (solid.type == UM_STL_SOLID)
		{
			tprm.rval = solid.sdata[1];
			if (tprm.ival[0] != 0)
			{
				ul_ipv_find_stock(*which,LW_stock_idn[*which]-1,&sd);
				sd->mxflag = 1;
				sd->mxchg = 1;
				um_tftotf(&solid.sdata[2],sd->matrix);
				sd->invflag = UU_TRUE;
				um_inverttf(sd->matrix,sd->invmx);
				ul_ipv_modify_stock(sd,UU_FALSE);
			}
		}
/*
.....Free the allocated memory
*/
		uu_free(p);
	}
/*
.....Store composite stock
*/
	if (compfl)
		ulf_verify_compos(which,numid,keys,&nso,kerr);
	else
		*numid = stkid;
/*
.....End of routine
*/
done:;
	return;
/*
.....Could not create stock
*/
failed:;
	*kerr = 1;
	goto done;
/*
.....Composite solid must contain solids
*/
nosf:;
	*kerr = 554;
	goto done;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_sphere(which,param,numid,kerr)
**      This function defines the NCLIPV stock in the form of a sphere.
**   PARAMETERS
**       INPUT  :
**            which = 0 - Stock definition.
**                    1 - Fixture definition.
**
**            param = x,y,z,i,j,k,r,length
**
**            numid = ID number for stock.
**
**       OUTPUT :
**            numid = Incremented stock ID number.
**
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_sphere(which,param,numid,kerr)
UM_int4 *which,*numid,*kerr;
UM_real8 param[];
{
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Define the stock as a sphere
*/
	ul_ipv_set_defered();
	ul_ipv_add_stock(*which,LW_STOCK_SPHERE,param,numid,UU_NULL,0,1);
	ul_ipv_set_immediate(UU_FALSE);
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_stl(which,fnam,units,numid,kerr)
**      This function loads an external NCLIPV STOCK/FIXTURE file.  It
**      will first attempt to load the exact filename and if that fails
**      it will attempt to load the file from the 'NCL_INCDIR' directory.
**   PARAMETERS
**       INPUT  :
**            which = 0 - Stock definition.
**                    1 - Fixture definition.
**
**            fnam  = Name of file to load.
**
**            nci   = Number of chars in 'fnam'.
**
**            units = 0 = Inches.
**                    1 = Millimeters
**
**            numid = ID number for stock.
**
**       OUTPUT :
**            numid = Incremented stock ID number.
**
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_stl(which,fnam,nci,units,numid,kerr)
UM_int4 *which,*kerr,*units,*numid,*nci;
UM_f77_str_ptr fnam;
{
	int stat,fstat,mode;
	char *cnam;
	UX_pathname fullname,tnam,lbuf;
	LtPrim prim;
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return;
/*
.....Initialize routine
*/
	cnam = UM_cstr_of_f77_str(fnam);
	strncpy(tnam,cnam,*nci);
	tnam[*nci] = '\0';
/*
.....Make sure the file exists
*/
	mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
	stat = ux_file_inquire(UU_NULL,UU_NULL,tnam,UU_NULL,UU_NULL,
		&mode,&fstat,lbuf,UX_NPRTERRS);
/*
.....File does not exist
.....try the NCL_INCDIR directory
*/
	if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS))
	{
		ul_build_full_fname("NCL_INCDIR",tnam,".stl",fullname);
		mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
		stat = ux_file_inquire(UU_NULL,UU_NULL,fullname,UU_NULL,UU_NULL,
			&mode,&fstat,lbuf,UX_NPRTERRS);
/*		if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS)) goto failed;
		strcpy(tnam,fullname); */
		if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS)) 
			stat = -1;
		else
			strcpy(tnam, fullname);
	}
/*
......Open failed
......check if this file has a path, if it is not, look at the clfile directory
*/
	if ((stat != UU_SUCCESS)&&(NCL_tpdir[0]!='\0')&&(UN_clfile!=0))
/*
......check if this file has a path, if it is not, default to the clfile directory
*/
	{
		ul_build_full_fname(NCL_tpdir, tnam, ".stk", fullname);
		mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
		stat = ux_file_inquire(UU_NULL,UU_NULL,fullname,UU_NULL,UU_NULL,
			&mode,&fstat,lbuf,UX_NPRTERRS);
		if (stat != UU_SUCCESS || mode == (mode|UX_NEXISTS)) 
			stat = -1;
		else
			strcpy(tnam, fullname);
	}
	if (stat != UU_SUCCESS) goto failed;
/*
.....Load the STL file
*/
	LW_stl_units = *units;
	ul_ipv_set_defered();
	ul_ipv_enable_stl_form(UU_FALSE);
	ul_ipv_import_stl(*which,tnam,numid,&prim,*units,LW_stl_toler);
	ul_ipv_enable_stl_form(UU_TRUE);
	ul_ipv_set_immediate(UU_FALSE);
/*
.....End of routine
*/
done:;
	return;
/*
.....Could not read stock file
*/
failed:
	*kerr = 1;
	goto done;
}

/*********************************************************************
**   E_FUNCTION:void ulf_verify_torus(which,param,numid,kerr)
**      This function defines the NCLIPV stock in the form of a torus.
**   PARAMETERS
**       INPUT  :
**            which = 0 - Stock definition.
**                    1 - Fixture definition.
**
**            param = x,y,z,i,j,k,r,length
**
**            numid = ID number for stock.
**
**       OUTPUT :
**            numid = Incremented stock ID number.
**
**            kerr  = 0 on success. Non-zero otherwise.
**
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ulf_verify_torus(which,param,numid,kerr)
UM_int4 *which,*numid,*kerr;
UM_real8 param[];
{
/*
.....Verify torus parameters are correct
*/
	*kerr = 0;
	if (param[7] > param[6])
	{
		*kerr = 1;
		return;
	}
/*
.....Do nothing if IPV is not active
*/
	if (UL_ipv == 0) return;
/*
.....Define the stock as a torus
*/
	ul_ipv_set_defered();
	ul_ipv_add_stock(*which,LW_STOCK_TORUS,param,numid,UU_NULL,0,1);
	ul_ipv_set_immediate(UU_FALSE);
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**   E_FUNCTION:int ulf_create_idlist(which,numids,nids,idlist)
**      This function creates a list of valid ID nukbers
**   PARAMETERS
**       INPUT  :
**            which  = 0 - Stock definition.
**                     1 - Fixture definition.
**            numids = ID numbers for stock.
**            nids   = Number of IDs in 'numids' array.
**       OUTPUT :
**            idlist - List of valid ids.
**            kerr   - id number of error, else 0
**   RETURNS: UU_FAILURE if error, else UU_SUCCESS
**   SIDE EFFECTS: none
**   WARNINGS: List must be freed by caller.
*********************************************************************/
int ulf_create_idlist(which,numids,nids,idlist,kerr)
UM_int4 which,*numids, nids, *kerr;
UU_LIST *idlist;
{
	int i,j,k1,k2,status = UU_SUCCESS;
	LW_stock_struc *sd;
/*
.....Do nothing if IPV is not active
*/
	*kerr = 0;
	if (UL_ipv == 0) return(status);

	uu_list_init(idlist,sizeof(int),100,100);
	for (i=0;i<nids;i++)
	{
		k2 = numids[i];
		if (k2 < 0)
		{
			k2 = -k2;
			for (j=k1+1;j<k2;j++)
			{
				ul_ipv_find_stock(which,j,&sd);
				if (sd == UU_NULL) 
				{
					*kerr = j;
					goto failed;
				}
				uu_list_push(idlist,&j);
			}
		}
		ul_ipv_find_stock(which,k2,&sd);
		if (sd == UU_NULL)
		{
			*kerr = k2;
			goto failed;
		}
		uu_list_push(idlist,&k2);
		k1 = k2;
	}
	return(status);
failed:;
	status = UU_FAILURE;
	return(status);
}
