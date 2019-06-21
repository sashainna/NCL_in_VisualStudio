/*********************************************************************
**   FILENAME: lipvstk2.c
**   CONTAINS: 
**             ul_ipv_view_stock()
**             ul_ipv_create_stocks()
**             ul_ipv_highlight_stock()
**             ul_ipv_get_next_stock()
**             ul_ipv_decomp_stock()
**             ul_ipv_clone_stock()
**             ul_ipv_count_stocks()
**     MODULE NAME AND RELEASE LEVEL 
**       lipvstk2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:17
*********************************************************************/
#include <stdio.h>
#include <string.h>
#include "usysdef.h"
#include "lcom.h"
#include "lipv.h"
#include "lipvmach.h"
#include "msol.h"

void ul_ipv_get_next_stock();

/*********************************************************************
**   E_FUNCTION: ul_ipv_view_stock(which,stock)
**      This function displays a stock or fixture in the NCL window.
**   PARAMETERS
**       INPUT  : which = 0 - Stock definition.
**                        1 - Fixture definition.
**                stock = Solid geometry modifiers/parameters.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_view_stock(which,stock)
int which;
LW_stock_struc *stock;
{
	int npts,i;
	UM_coord pt1,pt2,*sdpt,*ptr;
	LW_stock_struc *sx,sdtemp;
	struct UM_solid_rec solid;
/*
.....Box Solid
*/
	switch (stock->type)
	case LW_STOCK_BOX:
	{
		ncl_mcstowcs(0,&(stock->data[0]),pt1);
		ncl_mcstowcs(0,&(stock->data[3]),pt2);
		ul_verify_draw_box(which,pt1,pt2,stock->color,stock->mxflag,
			stock->matrix);
		break;
/*
.....Revolved Solid
*/
	case LW_STOCK_REVOLVE:
	case LW_STOCK_CONE:
	case LW_STOCK_CYLINDER:
	case LW_STOCK_SPHERE:
	case LW_STOCK_TORUS:
		sx = stock;
		if (stock->type != LW_STOCK_REVOLVE) sx = &sdtemp;
/*
........Convert solid primitive to revolved solid
*/
		if (stock->type == LW_STOCK_CONE)
		{
			solid.key = 0;
			solid.type = UM_CONE_SOLID;
			ncl_calc_cyl_list(&solid,stock->data,2,UU_NULL,UU_NULL,&sx->data);
		}
		else if (stock->type == LW_STOCK_CYLINDER)
		{
			solid.key = 0;
			solid.type = UM_CYLINDER_SOLID;
			ncl_calc_cyl_list(&solid,stock->data,2,UU_NULL,UU_NULL,&sx->data);
		}
		else if (stock->type == LW_STOCK_SPHERE)
		{
			solid.key = 0;
			solid.type = UM_SPHERE_SOLID;
			ncl_calc_sphere_list(&solid,stock->data,2,UU_NULL,UU_NULL,&sx->data);
		}
		else if (stock->type == LW_STOCK_TORUS)
		{
			solid.key = 0;
			solid.type = UM_TORUS_SOLID;
			ncl_calc_torus_list(&solid,stock->data,2,UU_NULL,UU_NULL,&sx->data);
		}
		sdpt = (UM_coord *)&(sx->data[9]);
		npts = (int)sx->data[8];
		ncl_mcstowcs(0,&(sx->data[0]),pt1);
		ncl_mcstowcs(1,&(sx->data[3]),pt2);
		ptr = (UM_coord *)uu_malloc(sizeof(UM_coord)*npts);
		if (ptr != UU_NULL)
		{
			for (i=0;i<npts;i++) ncl_mcstowcs(0,sdpt[i],ptr[i]);
			ul_verify_draw_revsf(pt1,pt2,ptr,npts,sx->data[6],sx->data[7],
				which,stock->color,stock->mxflag,stock->matrix);
			uu_free(ptr);
		}
		if (stock->type  != LW_STOCK_REVOLVE) uu_free(sx->data);
		break;
/*
.....Swept Solid
*/
	case LW_STOCK_SWEEP:
		sdpt = (UM_coord *)&(stock->data[4]);
		npts = (int)stock->data[3];
		ncl_mcstowcs(1,&(stock->data[0]),pt1);
		ptr = (UM_coord *)uu_malloc(sizeof(UM_coord)*npts);
		if (ptr != UU_NULL)
		{
			for (i=0;i<npts;i++) ncl_mcstowcs(0,sdpt[i],ptr[i]);
			ul_verify_draw_sweep(which,ptr,npts,1,pt1,pt1,stock->color,
				stock->mxflag,stock->matrix);
			uu_free(ptr);
		}
		break;
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_create_stocks(sd,type,sess)
**      This function is used to create a stock or fixture solid.
**      It controls the creation of composite stocks.
**   PARAMETERS
**       INPUT  :
**          sd         Stock to create.
**                type  = 0 - Stock definition.
**                        1 - Fixture definition.
**                        2 - Machine assembly.
**                        3 - General primitive (Tools, etc.).
**                sess  = 1 - Start the IPV Machining Session.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_create_stocks(sd,type,sess)
LW_stock_struc *sd;
int type,sess;
{
	int inc;
	UU_LOGICAL ifl;
	LW_stock_struc *stock;
/*
.....Create all solids associated with stocks
*/
	ifl = 0;
	do
	{
		inc = ifl;
		ul_ipv_get_next_stock(sd,&stock,&ifl);
		if (ifl == -2) break;
		ul_ipv_create_stock(stock,type,sess);
	} while (ifl != -1);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_highlight_stock(attr,sd,color,edge,ecolor)
**      This function is used to highlight a stock or fixture solid.
**      It controls the highlighting of composite stocks.
**   PARAMETERS
**       INPUT  :
**          sd         Stock to highlight.
**          color      Highlight color.  -1 = Unhighlight entity.
**          edge       UU_TRUE if solid edges should be displayed when
**                     unhighlighting.
**          ecolor     Color of displayed edge when unhighlighting.
**       OUTPUT :
**          attr       Returns -1 when highlighting solid.
**          edge       Returns UU_TRUE if solid edges are currently highlighted.
**          ecolor     Returns color of edges.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_highlight_stock(attr,sd,color,edge,ecolor)
LtMaterial *attr;
LtMaterial color;
LW_stock_struc *sd;
LtBoolean edge[];
LtColour ecolor[];
{
	int inc;
	UU_LOGICAL ifl;
	LW_stock_struc *stock;
/*
.....Highlight all solids associtiated with stocks
*/
	ifl = 0;
	do
	{
		inc = ifl;
		ul_ipv_get_next_stock(sd,&stock,&ifl);
		if (ifl == -2) break;
		ul_ipv_highlight_entity(attr,stock->stock,LI_ENTITY_TYPE_SOLID,
			color,&edge[inc],ecolor[inc]);
	} while (ifl != -1);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_get_next_stock(stock,sd,ptr,ifl)
**      This function returns the next stock in a composite stock
**      definition.  If the input stock is not a composite stock then
**      the input stock definition is returned.
**   PARAMETERS
**       INPUT  :
**          stock      Input (composite) stock.
**          ptr        Set to 0 on first call and then maintained by
**                     this routine.  When 'compfl' = UU_TRUE, then 'ptr'
**                     should be set to -1 on first call.
**          compfl     UU_TRUE = Return COMPOS style stock.  UU_FALSE = don't.
**       OUTPUT :
**          sd         Sub-stock from composite stock or original stock
**                     if input stock is not a composite.
**          ptr        Points to the next stock in the composite or
**                     -1 if the last stock is being returned with this
**                     call.  Returns -2 when the requested stock does
**                     not exist.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_get_next_stock(stock,sd,ptr,compfl)
LW_stock_struc *stock,**sd;
int *ptr;
UU_LOGICAL compfl;
{
	LW_stock_struc *sptr;
/*
.....Already at the end of stocks
*/
	if (*ptr == -1 && !compfl) goto failed;
/*
.....Loop through composite stocks
*/
	if (stock->type == LW_STOCK_COMPOS)
	{
		if (*ptr == -1)
			*sd = stock;
		else
		{
			if (*ptr >= stock->bin) goto failed;
			sptr = (LW_stock_struc *)stock->data;
			*sd = &sptr[*ptr];
		}
		*ptr = *ptr + 1;
		if (*ptr == stock->bin) *ptr = -1;
	}
/*
.....Input stock is not a composite stock
.....return input stock
*/
	else
	{
		*sd = stock;
		*ptr = -1;
	}
	goto done;
/*
.....Requested stock does not exist
*/
failed:;
	*ptr = -2;
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_decomp_stock(which,idn,sid)
**      This function decomposes a composite stock.
**   PARAMETERS
**       INPUT  :
**          which      0 = Stock definition.
**                     1 = Fixture definition.
**          idn        ID number for extracted stocks.  If set to 0 then
**                     the ID numbers stored with the composite compenents
**                     will be used.
**          sid        ID number of the composite stock to decompose.
**       OUTPUT :
**          idn        Updated ID number if it is not set to 0 on input.
**   RETURNS: UU_SUCCESS if no errors, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_decomp_stock(which,idn,sid)
int which,*idn,sid;
{
	int ifl,status,numid;
	LW_stock_struc *sdcomp,*sdtmp,*sd;
/*
.....Initialize routine
*/
	status = UU_FAILURE;
/*
.....Find the requested stock and make sure it is a composite stock
*/
	ul_ipv_find_stock(which,sid,&sdcomp);
	if (sdcomp == UU_NULL) goto done;
	if (sdcomp->type != LW_STOCK_COMPOS) goto done;
/*
.....Loop through composite stocks
*/
	ifl = 0;
	do
	{
		ul_ipv_get_next_stock(sdcomp,&sdtmp,&ifl,UU_FALSE);
		if (ifl == -2) break;
/*
........Get Stock ID
*/
		if (*idn == 0)
			numid = sdtmp->id;
/*
........Stock ID is provided
*/
		else
			numid = *idn;
/*
........Stock already exists
........Delete old stock
*/
		ul_ipv_find_stock(which,numid,&sd);
		if (sd != UU_NULL)
			ul_ipv_remove_stock(which,sd);
/*
.....Extract stock
*/
		sd = LW_stock_data[which];
		*sd = *sdtmp;
		sd->id = numid; numid = numid + 1; if (*idn != 0) *idn = numid;
		if (numid > LW_stock_idn[which]) LW_stock_idn[which] = numid;
/*
.....Allocate next stock
*/
		LW_stock_data[which] = (LW_stock_struc *)uu_lsinsrt(LW_stock_data[which],
			sizeof(LW_stock_struc));
		if (LW_stock_data[which] == UU_NULL) goto failed;
		LW_nstock[which]++;
	} while (ifl != -1);
/*
.....Remove the composite stock definition
*/
	ul_ipv_delist_stock(which,sdcomp);
	status = UU_SUCCESS;
	goto done;
/*
.....Failed to allocate memory
*/
failed:;
	ud_wrerr("Could not allocate memory for stock.");
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_clone_stock(which,sd,sdo)
**      Clones a predefined stock by simply copying the stock structure
**      and primitives.
**   PARAMETERS
**       INPUT  :
**          which      0 = Stock definition.
**                     1 = Fixture definition.
**          sd         Stock to clone.
**       OUTPUT :
**          sdo        Cloned stock.
**   RETURNS: UU_SUCCESS if no errors, UU_FAILURE otherwise.
**   SIDE EFFECTS:
**          Adds the stock to the active session.  Does not assign a
**          new ID number to the cloned stock, nor does it add it to
**          the list of defined stocks.
**   WARNINGS: none
*********************************************************************/
int ul_ipv_clone_stock(which,sd,sdo)
int which;
LW_stock_struc *sd,*sdo;
{
	int status,npts;
	UU_LOGICAL iflag;
	char *p;
/*
.....Initialize routine
*/
	status = UU_FAILURE;
/*
.....Clone the stock
*/
	*sdo = *sd;
/*
.....Copy the primitive data
*/
	switch (sd->type)
	{
	case LW_STOCK_SPHERE:
		npts = 4 * sizeof(UU_REAL);
		break;
	case LW_STOCK_BOX:
		npts = 6 * sizeof(UU_REAL);
		break;
	case LW_STOCK_CYLINDER:
		npts = 7 * sizeof(UU_REAL);
		break;
	case LW_STOCK_CONE:
	case LW_STOCK_TORUS:
		npts = 8 * sizeof(UU_REAL);
		break;
	case LW_STOCK_SWEEP:
		npts = (sd->data[3]+4) * sizeof(UM_coord);
		break;
	case LW_STOCK_REVOLVE:
		npts = (sd->data[8]+9) * sizeof(UM_coord);
		break;
	case LW_STOCK_SESSION:
	case LW_STOCK_FILE:
		p = (char *)sd->data;
		npts = (strlen(p)+1) * sizeof(char);
		break;
	case LW_STOCK_COMPOS:
		if (sdo->data == UU_NULL) goto done;
		npts = sd->bin*sizeof(LW_stock_struc);
		break;
	default:;
		npts = 0;
		break;
	}
	if (npts == 0) sdo->data = UU_NULL;
	else
	{
		sdo->data = (UU_REAL *)uu_malloc(npts);
		if (sdo->data == UU_NULL) goto done;
		if (sd->type != LW_STOCK_COMPOS)
			uu_move_byte(sd->data,sdo->data,npts);
	}
/*
.....Make copies of the primitives
*/
	if ((LW_active || LW_session[LW_mach_mode] != 0) &&
		sd->type != LW_STOCK_COMPOS)
	{
		sdo->prim = LiPrimitiveCopy(sd->prim,UU_NULL);
/*
......Lathe
*/
		if (LW_mach_type == LW_LATHE && !LW_mach_simul && LW_lathe != 0)
		{
			iflag = ul_ipv_lathe_stop();
			sdo->stock = LiSessionAddPrim(LW_session[LW_mach_mode],sdo->prim);
			LiViSolidSetEnabled(sdo->stock,UU_TRUE);
			LiViLatheAddSolid(LW_lathe,sdo->stock);
			if (iflag) ul_ipv_lathe_start();
		}
		else
		{
			sdo->stock = LiSessionAddPrim(LW_session[LW_mach_mode],sdo->prim);
			LiViSolidSetEnabled(sdo->stock,UU_TRUE);
		}
	}
/*
.....NCLIPV is not active
*/
	else
	{
		sdo->stock = 0;
		sdo->prim = 0;
	}
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_count_stocks(nstock)
**      This function counts all defined stocks and fixtures within
**      the global arrays.
**   PARAMETERS
**       INPUT  :
**          compfl     UU_TRUE = include composite stock in count,
**                     UU_FALSE = don't include composite stock in count,
**                                only its components.
**       OUTPUT :
**          nstock     [0] = Number of stocks, [1] = Number of fixtures.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_count_stocks(nstock,compfl)
int nstock[];
UU_LOGICAL compfl;
{
	int i,j,ifl;
	LW_stock_struc *sd,*sdtmp;
/*
.....Loop through all stocks and fixtures
*/
	for (i=0;i<2;i++)
	{
		sd = LW_stock_first[i];
		nstock[i] = 0;
		for (j=0;j<LW_nstock[i];j++)
		{
			ifl = 0;
			if (compfl) ifl = -1;
			do
			{
				ul_ipv_get_next_stock(sd,&sdtmp,&ifl,compfl);
				if (ifl == -2) break;
				nstock[i]++;
			} while (ifl != -1);
			sd = (LW_stock_struc *)uu_lsnext(sd);
		}
	}
}
