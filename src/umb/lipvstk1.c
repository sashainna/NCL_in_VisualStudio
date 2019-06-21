/*********************************************************************
**   FILENAME: lipvstk1.c
**   CONTAINS: ul_ipv_delete_stocks()
**             ul_ipv_find_stock()
**             ul_ipv_remove_stock_ind()
**             ul_ipv_remove_stock()
**             ul_ipv_remove_prim()
**             ul_ipv_reset_prim()
**             ul_ipv_invis_stock()
**             ul_ipv_vis_stock()
**             ul_ipv_delist_stock()
**             ul_ipv_set_stk_attr()
**             ul_ipv_stl_binary()
**             ul_ipv_stock_cmd()
**             ul_ipv_stock_dir()
**             ul_ipv_stock_ptcmd()
**             ul_ipv_stock_same()
**             ul_ipv_stock_test()
**             ul_ipv_stl_test()
**             ul_ipv_stock_trim()
**             ul_ipv_is_closed()
**             ul_ipv_enable_stl_form()
**             ul_ipv_open_stl_form()
**             ul_ipv_close_stl_form()
**     MODULE NAME AND RELEASE LEVEL 
**       lipvstk1.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       07/28/15 , 11:13:58
*********************************************************************/
#include <stdio.h>
#include <string.h>
#include "usysdef.h"
#include "lcom.h"
#include "lipv.h"
#include "lipvmach.h"
#include "mfort.h"
#include "mpocket.h"
#include "nclfc.h"
#include <ctype.h>
#include <math.h>
#include "gtbl.h"
#include "gobas.h"
#include "view.h"
#include "mdattr.h"
#include "mpocket.h"
#include "mcrv.h"
#include "modef.h"
#include "ulist.h"
#include "m2dattr.h"
#include "mdcpln.h"
#include "mcrv.h"
#include "nccs.h"
#include "mdrel.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "xfsys1.h"

#define FMSG 0
#define FLAB 1
#define FPRG 0
#define FINT 1

static int Sfrm = -1;
static int Sprogress;
static UU_LOGICAL Senable_stl_form=UU_TRUE;
static char Slabel[40];
static UX_pathname Sfile;

void ul_ipv_stock_trim();
void ul_ipv_stock_cmd();
void ul_ipv_stock_ptcmd();
void ul_ipv_find_stock();
void ul_ipv_remove_stock();
LW_stock_struc *ul_ipv_delist_stock();
void ul_ipv_remove_prim();

static UD_FSTAT OnInterrupt(),OnClose();
static LtStatus S_progress(),S_interrupt();
static void S_form_label();
static void S_flush_form();

/*********************************************************************
**   E_FUNCTION: ul_ipv_delete_stocks()
**      This function deletes all stocks and fixtures.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_delete_stocks()
{
	int i,j,ifl;
	LW_stock_struc *sptr,*sdtmp;
	LtData data;
/*
.....Delete all stocks and fixtures
*/
	for (j=0;j<2;j++)
	{
		sptr = LW_stock_first[j];
		if (LW_active)
		{
			LiDataSetBoolean(&data,FALSE);
			for (i=0;i<LW_nstock[j];i++)
			{
				ifl = 0;
				do
				{
					ul_ipv_get_next_stock(sptr,&sdtmp,&ifl,UU_FALSE);
					if (ifl == -2) break;
					ul_ipv_remove_prim(sdtmp);
				} while (ifl != -1);
				sptr = (LW_stock_struc *)uu_lsnext(sptr);
			}
		}
		LW_stock_data[j] = (LW_stock_struc *)uu_lsempty(LW_stock_first[j]);
		LW_stock_data[j] = (LW_stock_struc *)uu_lsinsrt((char *)LW_stock_data[j],
			sizeof(LW_stock_struc));
		LW_stock_first[j] = LW_stock_data[j];
		LW_nstock[j] = 0;
	}
}

/*********************************************************************
**   E_FUNCTION:int ul_ipv_find_stock(which,idn,sd)
**      This function finds the stock defined with the specified Stock
**      ID number.
**   PARAMETERS
**       INPUT  : which = 0 - Stock definition.
**                        1 - Fixture definition.
**                idn   = Stock ID number to search for.
**       OUTPUT : sd    = Pointer to stock definition or UU_NULL if not
**                        found.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_find_stock(which,idn,sd)
int which,idn;
LW_stock_struc **sd;
{
	int i;
	LW_stock_struc *sptr;
/*
.....Initialize routine
*/
	*sd = UU_NULL;
/*
.....Search for requested stock/fixture
*/
	sptr = LW_stock_first[which];
	for (i=0;i<LW_nstock[which];i++)
	{
		if (sptr->id == idn)
		{
			*sd = sptr;
			break;
		}
		sptr = (LW_stock_struc *)uu_lsnext(sptr);
	}
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_remove_stock_ind(which,istk)
**      Removes a stock/fixture based on its index into the LW_stock arrays.
**   PARAMETERS
**       INPUT  : which  = 0 - Stock definition.
**                         1 - Fixture definition.
**                iprim  = Index into stock array of stock to delete.
**                         -1 = Delete last stock.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_remove_stock_ind(which,istk)
int which;
int istk;
{
	int ifl,i;
	LW_stock_struc *sd;
/*
.....Initialize routine
*/
	if (LW_active)
	{
/*
.....Find stock to delete
*/
		ifl = 0;
		sd = LW_stock_first[which];
		for (i=0;i<LW_nstock[which]-1;i++)
		{
			if (i == istk) break;
			sd = (LW_stock_struc *)uu_lsnext(sd);
		}
/*
.....Remove the primitive
*/
		ul_ipv_remove_stock(which,sd);
	}
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_remove_stock(which,sd)
**      This function deletes a single stock/fixture.
**   PARAMETERS
**       INPUT  : which = 0 - Stock definition.
**                        1 - Fixture definition.
**                sd    = Stock to delete.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_remove_stock(which,sptr)
int which;
LW_stock_struc *sptr;
{
	int ifl;
	UU_LOGICAL iflag;
	LW_stock_struc *sd,*stock;
	LtData data;
/*
.....Delete active stock
*/
	if (LW_active)
	{
		iflag = UU_FALSE;
		if (LW_mach_type == LW_LATHE) iflag = ul_ipv_lathe_stop();
		ifl = 0;
		do
		{
			ul_ipv_get_next_stock(sptr,&stock,&ifl,UU_FALSE);
			if (ifl == -2) break;
			ul_ipv_remove_prim(stock);
		} while (ifl != -1);
	}
	if (iflag) iflag = ul_ipv_lathe_start();
/*
.....Delete the stock parameters
*/
	ul_ipv_delist_stock(which,sptr);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_reset_prim(which,iprim)
**      Resets a primitive to its uncut condition.
**   PARAMETERS
**       INPUT  : which  = 0 - Stock definition.
**                         1 - Fixture definition.
**                iprim  = Index into stock array of stock to reset.
**                         -1 = Reset last stock.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_reset_prim(which,iprim)
int which;
int iprim;
{
	int ifl,i,ns;
	UU_LOGICAL iflag;
	LW_stock_struc *sd;
/*
.....Initialize routine
*/
	if (LW_active)
	{
		iflag = UU_FALSE;
		if (LW_mach_type == LW_LATHE) iflag = ul_ipv_lathe_stop();
/*
.....Find stock to delete
*/
		ifl = 0;
		ns = iprim;
		if (ns == -1) ns = LW_nstock[which];
		sd = LW_stock_first[which];
		for (i=0;i<ns-1;i++)
		{
			sd = (LW_stock_struc *)uu_lsnext(sd);
			if (ifl == -2) goto done;
		}
/*
.....Remove the primitive
*/
		ul_ipv_remove_prim(sd);
/*
.....Add back the primitive
*/
		ul_ipv_create_stock(sd,which,0);
	}
/*
.....End of routine
*/
done:;
	if (iflag) iflag = ul_ipv_lathe_start();
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_remove_prim(sptr)
**      This function deletes a single stock/fixture from the session
**      without removing it from the list of stocks/fixtures.
**   PARAMETERS
**       INPUT  :
**          sptr    = Stock to delete.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_remove_prim(sptr)
LW_stock_struc *sptr;
{
	int ifl,inc;
	UU_LOGICAL iflag;
	LW_stock_struc *sd,*stock;
	LtData data;
/*
.....Delete active stock
*/
	if (LW_active)
	{
		iflag = UU_FALSE;
		if (LW_mach_type == LW_LATHE) iflag = ul_ipv_lathe_stop();
		LiDataSetBoolean(&data,FALSE);
		LiMWViewportSetSessPrimProperty(LW_viewport,sptr->stock,
			LI_VPSP_PROP_MW_VISIBLE,&data);
		if (LW_mach_mode == LW_VISICUT)
			LiViSolidSetEnabled(sptr->stock,FALSE);
		if (LW_is_lathe)
			LiViAssemblyRemoveSolid(sptr->stock);
		LiSessionRemovePrim(sptr->stock);
		LiPrimitiveDestroy(sptr->prim);
		if (sptr->axis_seg != -1) ul_delaxis_ipv(sptr);
		if (iflag) ul_ipv_lathe_start();
	}
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_invis_stock(sptr)
**      This function invisibles and deactivates a stock regardless
**      of its attribute settings.
**   PARAMETERS
**       INPUT  :
**          sptr    = Stock to invisible.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_invis_stock(sptr)
LW_stock_struc *sptr;
{
	int ifl,inc;
	UU_LOGICAL iflag;
	LW_stock_struc *sd,*stock;
	LtData data;
/*
.....Invisible active stock
*/
	if (LW_active)
	{
		iflag = UU_FALSE;
		if (LW_mach_type == LW_LATHE) iflag = ul_ipv_lathe_stop();
		LiDataSetBoolean(&data,FALSE);
		LiMWViewportSetSessPrimProperty(LW_viewport,sptr->stock,
			LI_VPSP_PROP_MW_VISIBLE,&data);
		if (LW_mach_mode == LW_VISICUT)
			LiViSolidSetEnabled(sptr->stock,FALSE);
		if (iflag) ul_ipv_lathe_start();
	}
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_vis_stock(sptr)
**      This function visibles and activates a stock depending on its
**      attribute settings.
**   PARAMETERS
**       INPUT  :
**          sptr    = Stock to invisible.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_vis_stock(sptr)
LW_stock_struc *sptr;
{
	int ifl,inc;
	UU_LOGICAL iflag;
	LW_stock_struc *sd,*stock;
	LtData data;
/*
.....Visible active stock
*/
	if (LW_active)
	{
		iflag = UU_FALSE;
		if (LW_mach_type == LW_LATHE) iflag = ul_ipv_lathe_stop();
		LiDataSetBoolean(&data,sptr->visible);
		LiMWViewportSetSessPrimProperty(LW_viewport,sptr->stock,
			LI_VPSP_PROP_MW_VISIBLE,&data);
		if (LW_mach_mode == LW_VISICUT)
		{
			LiDataSetBoolean(&data,sptr->active);
			LiViSolidSetEnabled(sptr->stock,FALSE);
		}
		if (iflag) ul_ipv_lathe_start();
	}
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_delist_stock(which,sd)
**      This function removes a single stock/fixture from the internal
**      list.
**   PARAMETERS
**       INPUT  :
**          which  = 0 - Stock definition.
**                   1 - Fixture definition.
**
**          sd     = Stock to remove from stock list;
**       OUTPUT : none
**   RETURNS: Pointer to the next item in the list or UU_NULL.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
LW_stock_struc *ul_ipv_delist_stock(which,sptr)
int which;
LW_stock_struc *sptr;
{
	LW_stock_struc *sd;
/*
.....Remove stock from list of stocks
*/
	if (sptr == LW_stock_first[which])
	{
		LW_stock_first[which] = (LW_stock_struc *)uu_lsdele(sptr);
		sd = LW_stock_first[which];
	}
	else if (sptr == (LW_stock_struc *)uu_lsprev(LW_stock_data[which]))
	{
		LW_stock_data[which] = (LW_stock_struc *)uu_lsdele(sptr);
		sd = LW_stock_data[which];
	}
	else
		sd = (LW_stock_struc *)uu_lsdele(sptr);
	LW_nstock[which]--;
/*
.....Reset stock lists if no more stocks
*/
	if (LW_nstock[0] == 0 && LW_nstock[1] == 0)
	{
		LW_stock_first[0] = LW_stock_data[0];
		LW_stock_first[1] = LW_stock_data[1];
	}
/*
.....End of routine
*/
	return(sd);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_set_stock_attr(stock)
**      This function sets the stock/fixture attributes within the
**      MachineWorks libraries.
**   PARAMETERS
**       INPUT  : stock = Solid geometry modifiers/parameters.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_set_stk_attr(solid)
LW_stock_struc *solid;
{
	LtData transp,data;
/*
.....Set the stock attributes
*/
	if (solid->stock != 0)
	{
		ul_ipv_stock_trim(solid);
/*		ul_ipv_stock_test(solid);*/
		if (LW_mach_mode == LW_VISICUT) LiViSolidSetEnabled(solid->stock,UU_TRUE);
		LiDataSetBoolean(&data,!solid->active);
		LiSessionPrimSetProperty(solid->stock,LI_SPRIM_PROP_VI_PASSIVE,&data);
		if (LW_viewport != UU_NULL)
		{
			LiDataSetBoolean(&data,solid->visible);
			LiMWViewportSetSessPrimProperty(LW_viewport,solid->stock,
				LI_VPSP_PROP_MW_VISIBLE,&data);
		}
		LiDataSetNat32(&transp,100-solid->translucency);
		LiSessionPrimSetVisualProperty(solid->stock,
			LI_MW_VIS_PROP_TRANSPARENCY,&transp);
		ul_ipv_render_edges(solid->stock,solid->edge,solid->edge_color,
			solid->color,UU_FALSE);
	}
	return;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_stl_binary(fd,file,bin)
**      This function opens an STL file and determines if it is binary
**      or Ascii.
**   PARAMETERS
**       INPUT  :
**         file  = File name to open.
**       OUTPUT : 
**         fd    = File descriptor of opened STL file.
**         bin   = UU_TRUE = Binary STL file, UU_FALSE = Ascii.
**   RETURNS:
**         UU_SUCCESS if the file was successfully read, UX_NFOUND if
**         the file cannot be opened, or UX_BAD_FILE if it could not be
**         read.
**   SIDE EFFECTS:
**       The file is left open for reading.
**   WARNINGS: none
*********************************************************************/
int ul_ipv_stl_binary(fd,file,bin)
FILE **fd;
char *file;
UU_LOGICAL *bin;
{
	int stat;
/*
.....Determine if Binary or Ascii file
*/
	*bin = 0;
	stat = ul_is_binary(file);
	if (stat == -1) goto filerr;
	if (stat == 1) *bin = UU_TRUE;
	*fd = LiFileOpen(file,LI_FILE_READ);
	stat = UU_SUCCESS;
	goto done;
/*
.....Error reading STL file
*/
filerr:;
	stat = UX_NFOUND;
	goto done;
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_stock_cmd(which,stock,scmd,maxc,cnvflag)
**      This function creates the stock and fixture commands which can
**      be stored in an external text file.
**   PARAMETERS
**       INPUT  : which   = 0 - Stock definition.
**                          1 - Fixture definition.
**                stock   = Solid geometry modifiers/parameters.
**                maxc    = Size of 'scmd' string.
**                cnvflag = UU_TRUE - perform metric conversion prior
**                          to outputing commands.
**       OUTPUT : scmd    = Text stock command.
**   RETURNS: none
**   SIDE EFFECTS: Call 'ul_ipv_stock_ptcmd' to create the point definition
**                 commands (profile, surface of revolution, etc.).
**   WARNINGS: none
*********************************************************************/
void ul_ipv_stock_cmd(which,stock,scmd,maxc,cnvflag)
int which,maxc;
LW_stock_struc *stock;
char *scmd;
UU_LOGICAL cnvflag;
{
	UM_coord *sdpt;
	int bin;
	char *file,lbuf[UX_MAX_PATH_LEN+40];
	UU_REAL *rpt1,*rpt2,pt1[3],pt2[3];
	static char lbox[2][8] = {"CBOX","FCBOX"};
	static char lcone[2][8] = {"CONE","FCONE"};
	static char lcyl[2][8] = {"CYL","FCYL"};
	static char ltorus[2][8] = {"TORUS","FTORUS"};
	static char lspher[2][8] = {"SPHER","FSPHER"};
	static char lrev[2][8] = {"CREV","FCREV"};
	static char lprof[2][8] = {"SPROF","FSPROF"};
	static char lfile[2][8] = {"STL","FSTL"};
	static char lcomp[2][8] = {"COMP","FCOMP"};
/*
........Box Solid
*/
	switch (stock->type)
	{
	case LW_STOCK_BOX:
		rpt1 = &(stock->data[0]);
		rpt2 = &(stock->data[3]);
		if (cnvflag)
		{
			UM_cc_inttoext(rpt1,pt1);
			UM_cc_inttoext(rpt2,pt2);
		}
		else
		{
			um_vctovc(rpt1,pt1);
			um_vctovc(rpt2,pt2);
		}
		sprintf(lbuf,"%s %f %f %f %f %f %f",lbox[which],pt1[0],pt1[1],pt1[2],
			pt2[0],pt2[1],pt2[2]);
		break;
/*
........Cone Solid
*/
	case LW_STOCK_CONE:
		rpt1 = &(stock->data[0]);
		if (cnvflag)
		{
			UM_cc_inttoext(rpt1,pt1);
		}
		else
		{
			um_vctovc(rpt1,pt1);
		}
		sprintf(lbuf,"%s %f %f %f %f %f %f %f %f",lcone[which],
			pt1[0],pt1[1],pt1[2],stock->data[3],stock->data[4],stock->data[5],
			stock->data[6],stock->data[7]);
		break;
/*
........Cylinder Solid
*/
	case LW_STOCK_CYLINDER:
		rpt1 = &(stock->data[0]);
		if (cnvflag)
		{
			UM_cc_inttoext(rpt1,pt1);
		}
		else
		{
			um_vctovc(rpt1,pt1);
		}
		sprintf(lbuf,"%s %f %f %f %f %f %f %f",lcyl[which],
			pt1[0],pt1[1],pt1[2],stock->data[3],stock->data[4],stock->data[5],
			stock->data[6]);
		break;
/*
........Sphere Solid
*/
	case LW_STOCK_SPHERE:
		rpt1 = &(stock->data[0]);
		if (cnvflag)
		{
			UM_cc_inttoext(rpt1,pt1);
		}
		else
		{
			um_vctovc(rpt1,pt1);
		}
		sprintf(lbuf,"%s %f %f %f %f",lspher[which],pt1[0],pt1[1],pt1[2],
			stock->data[3]);
		break;
/*
........Torus Solid
*/
	case LW_STOCK_TORUS:
		rpt1 = &(stock->data[0]);
		if (cnvflag)
		{
			UM_cc_inttoext(rpt1,pt1);
		}
		else
		{
			um_vctovc(rpt1,pt1);
		}
		sprintf(lbuf,"%s %f %f %f %f %f %f %f %f",ltorus[which],
			pt1[0],pt1[1],pt1[2],stock->data[3],stock->data[4],stock->data[5],
			stock->data[6],stock->data[7]);
		break;
/*
.....Revolved Solid
*/
	case LW_STOCK_REVOLVE:
		rpt1 = &(stock->data[0]);
		if (cnvflag)
		{
			UM_cc_inttoext(rpt1,pt1);
		}
		else um_vctovc(rpt1,pt1);
		sprintf(lbuf,"%s %f %f %f %f %f %f %f %f",lrev[which],pt1[0],pt1[1],
			pt1[2],stock->data[3],stock->data[4],stock->data[5],
			stock->data[6],stock->data[7]);
		break;
/*
.....Swept Solid
*/
	case LW_STOCK_SWEEP:
		sdpt = (UM_coord *)&(stock->data[4]);
		rpt1 = sdpt[0];
		if (cnvflag)
		{
			UM_cc_inttoext(rpt1,pt1);
		}
		else um_vctovc(rpt1,pt1);
		sdpt = (UM_coord *)&(stock->data[0]);
		rpt1 = sdpt[0];
		if (cnvflag)
		{
			UM_cc_inttoext(rpt1,pt2);
		}
		else um_vctovc(rpt1,pt2);
		sprintf(lbuf,"%s %f %f %f %f %f %f",lprof[which],pt1[0],pt1[1],pt1[2],
			pt2[0],pt2[1],pt2[2]);
		break;
/*
.....STL File
*/
	case LW_STOCK_FILE:
		file = (char *)stock->data;
		bin = stock->bin;
		sprintf(lbuf,"%s %d %s",lfile[which],bin,file);
		break;
/*
.....Composite stock
*/
	case LW_STOCK_COMPOS:
		bin = stock->bin;
		sprintf(lbuf,"%s %d entities.",lcomp[which],stock->bin);
		break;
/*
.....Unknown stock
*/
	default:
		strcpy(lbuf,"Unknown stock type.");
		break;
	}
/*
.....End of routine
*/
done:;
	if (strlen(lbuf) < maxc-1)
		strcpy(scmd,lbuf);
	else
	{
		strncpy(scmd,lbuf,maxc-1);
		lbuf[maxc-1] = '\0';
	}
	return;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_stock_dir(pts,npts)
**      This function determines the direction of a polyline.  The
**      resultant point array will be output in a CCLW direction.
**      
**
**   PARAMETERS
**       INPUT  : pts   = Array of points in polyline.
**                npts  = Number of points in 'npts'
**       OUTPUT : pts   = Input point array in a CCLW direction.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_stock_dir(pts,npts)
UM_coord *pts;
int npts;
{
	int i;
	UU_REAL dir,x1,y1,x2,y2;
	UM_coord lpts;
/*
.....Get direction of profile
*/
	dir = 0.;
	x1 = 0.; y1 = 0.;
	for (i=1;i<npts;i++)
	{
		x2 = pts[i][0] - pts[0][0];
		y2 = pts[i][1] - pts[0][1];
		dir = dir + (x1*y2 - x2*y1);
		x1 = x2; y1 = y2;
	}
/*
.....If current direction is clockwise
.....then reverse points
*/
	if (dir < 0.)
	{
		for (i=0;i<npts/2;i++)
		{
			um_vctovc(pts[i],lpts);
			um_vctovc(pts[npts-i-1],pts[i]);
			um_vctovc(lpts,pts[npts-i-1]);
		}
	}
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_stock_ptcmd(pt,scmd,cnvflag)
**      This function creates the stock and fixture point definitions
**      which can be stored in an external text file.
**   PARAMETERS
**       INPUT  : pt      = Point to format for output.
**                cnvflag = UU_TRUE - perform metric conversion prior
**                          to outputing commands.
**       OUTPUT : scmd    = Text point command.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_stock_ptcmd(pt,scmd,cnvflag)
UM_coord pt;
char *scmd;
UU_LOGICAL cnvflag;
{
/*
.....Format point command
*/
		if (cnvflag)
		{
			UM_cc_inttoext(pt,pt);
		}
	sprintf(scmd,"SPPROF %f %f %f",pt[0],pt[1],pt[2]);
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_stock_same(flag,sd,data,pts,npts)
**      This function determines if the defined stock and supplied data
**      are the same.
**   PARAMETERS
**       INPUT  : flag  = Type of solid to add (LW_stock_type).
**                sd    = Existing stock to compare with 'data'.
**                data  = Solid geometry modifiers/parameters.
**                pts   = Point data for curve generated solids.
**                npts  = Number of points in 'pts'.
**       OUTPUT : none
**   RETURNS: UU_TRUE if stocks are the same, UU_FALSE if not.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
UU_LOGICAL ul_ipv_stock_same(flag,sd,data,pts,npts)
LW_stock_type flag;
LW_stock_struc *sd;
UU_REAL *data;
UM_coord *pts;
int npts;
{
	int i,n;
	UU_LOGICAL same;
/*
.....Initialize routine
*/
	same = UU_TRUE;
/*
.....Stock Types are not the same
*/
	if (flag != sd->type || flag == LW_STOCK_COMPOS)
		same = UU_FALSE;
/*
.....Solid primitives
*/
	else
	{
		switch (sd->type)
		{
		case LW_STOCK_BOX:
		case LW_STOCK_CONE:
		case LW_STOCK_CYLINDER:
		case LW_STOCK_SPHERE:
		case LW_STOCK_TORUS:
		case LW_STOCK_REVOLVE:
		case LW_STOCK_SWEEP:
			n = 6;
			if (sd->type == LW_STOCK_SWEEP) n = 3;
			if (sd->type == LW_STOCK_SPHERE) n = 4;
			else if (sd->type == LW_STOCK_CYLINDER) n = 7;
			else if (sd->type == LW_STOCK_CONE || sd->type == LW_STOCK_TORUS ||
				sd->type == LW_STOCK_REVOLVE) n = 8;
			for (i=0;i<n;i++) if (fabs(sd->data[i]-data[i]) > UM_FUZZ)
			{
				same = UU_FALSE;
				break;
			}
			break;
/*
.....Session File
*/
		case LW_STOCK_SESSION:
			if (strcmp((char *)sd->data,(char *)pts) != 0) same = UU_FALSE;
			break;
/*
.....STL File
*/
		case LW_STOCK_FILE:
			if (strcmp((char *)sd->data,(char *)pts) != 0) same = UU_FALSE;
			break;
		}
	}
/*
.....End of routine
*/
	return(same);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_stock_test(sd)
**      This function determines if an STL type stock is valid.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**   RETURNS:   UU_SUCCESS if the stock is valid, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_stock_test(sd)
LW_stock_struc *sd;
{
	int status;
	UX_pathname msg;
/*
.....Only test STL solids
*/
	status = UU_SUCCESS;
	if (sd->type != LW_STOCK_FILE) goto done;
/*
.....Test the STL file
*/
	status = ul_ipv_stl_test(sd->prim,(char *)sd->data,msg);
/*
.....Bad STL file
.....Disable stock
*/
	if (status == UU_FAILURE)
	{
		if (LW_stl_flag[1])
		{
			sd->visible = UU_FALSE;
			sd->active = UU_FALSE;
		}
		ul_ipv_write_log(" ");
		ul_ipv_write_log(msg);
		if (LW_stl_flag[0]) ud_wrerr(msg);
	}
done:;
	return(status);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_stl_test(stock,file)
**      This function determines if an STL type stock is valid.
**   PARAMETERS
**       INPUT  :
**          stock   = Solid primitive to test.
**          file    = Name of STL file.
**       OUTPUT : none
**   RETURNS:   UU_SUCCESS if the stock is valid, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_stl_test(stock,file,msg)
LtPrim stock;
char *file,*msg;
{
	LtData data;
	LtStatus stat;
	LtSelfIntersectionList isect_list;
	LtSelfIntersection isect_ent;
	LtSession sess;
	LtSessionPrim sprim;
	LtBody body;
	int status;
	UU_LOGICAL is_closed;
	char sbuf[80];
	UX_pathname dir,fnam;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	if (!LW_stl_flag[2])
	{
		is_closed = UU_TRUE;
		ul_break_fname(file,dir,fnam);
/*
.....Determine if solid is closed
*/
		is_closed = LiPrimitiveGetSolidClosure(stock);
/*
.....Open a Solids session
*/
		sess = LiSessionCreate(LI_MW_SESSION_SOLID_OPS);
		sprim = LiSessionAddPrim(sess,stock);
/*
.....Try and fix any problems with the solid
*/
		S_form_label(3);
		LiSOEntityGetProperty(LiSOSolidGetBody(sprim),LI_SO_BODY_PROP_INSIDE_OUT,
			&data);
		if (LiDataGetBoolean(&data))
		{
			S_form_label(1);
			LiSOSolidMakeConsistent(sprim);
		}
/*
.....Determine if solid is closed
*/
		body = LiSOSolidGetBody(sprim);
		LiSOEntityGetProperty(body,LI_SO_BODY_PROP_CLOSED,&data);
		is_closed = LiDataGetBoolean(&data);
		if (!is_closed)
		{
			S_form_label(4);
			stat = LiSOSolidClose(sprim,LI_CE_CLOSE_DEFAULT);
			if (stat != LI_STATUS_OK)
			{
				is_closed = UU_FALSE;
				ul_short_filename(fnam,sbuf,40);
				sprintf(msg,"Solid from '%s' is not closed.",sbuf);
				status = UU_FAILURE;
			}
			is_closed = UU_TRUE;
		}
/*
.....Determine if there are self intersections
*/
		if (is_closed)
		{
			isect_list = LiSOSolidGetSelfIntersections(sprim,UU_TRUE);
			if (isect_list != 0)
			{
				isect_ent = LiSelfIntersectionListGetFirst(isect_list);
				if (isect_ent != 0)
				{
/*
........Remove self intersections
*/
					LiSelfIntersectionListDestroy(isect_list);
					S_form_label(2);
					LiSOSolidFixSelfIntersections(sprim);
					isect_list = LiSOSolidGetSelfIntersections(sprim,UU_TRUE);
					if (isect_list != 0)
					{
						isect_ent = LiSelfIntersectionListGetFirst(isect_list);
						if (isect_ent != 0)
						{
							ul_short_filename(fnam,sbuf,40);
							sprintf(msg,"Solid from '%s' is self intersecting.",sbuf);
							status = UU_FAILURE;
						}
					}
				}
				LiSelfIntersectionListDestroy(isect_list);
			}
		}
		if (status == UU_SUCCESS)
			LiSOSolidStore(sprim);
		LiSessionDestroy(sess);
		S_form_label(0);
	}
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_stock_trim(sd)
**      This function trims any surface of revolution stocks.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_stock_trim(sd)
LW_stock_struc *sd;
{
	UU_REAL pl1[6],pl2[6],r;
	UM_coord *sdpt;
	LtPrim st1,st2;
	LtSessionPrim st1a;
/*
.....Sectioning only allowed during an active sessions
*/
	if (LW_session[0] == 0) return;
/*
.....Section revolved solid
*/
	if (sd->type == LW_STOCK_REVOLVE)
	{
		if (sd->data[6] != 0. || sd->data[7] != 360.)
		{
			sdpt = (UM_coord *)&(sd->data[9]);
			ncl_revsf_planes(&(sd->data[0]),&(sd->data[3]),sdpt[0],
				sd->data[6],sd->data[7],pl1,pl2);
/*
.....If angle is greater than 180 degrees
.....then 2 solids have to be split then merged
*/
			r = sd->data[7] - sd->data[6];
			if (r < 0.) r = r + 360.;
/*
.....If angle is greater than 180 degrees
.....then 2 solids have to be split then merged
*/
			if (r > 180.)
			{
				st1 = LiPrimitiveCopy(sd->prim,LiPrimitiveGetContents(sd->prim));
				LiPrimitiveSetSolidType(st1,LI_MW_SOLID_TYPE_GENERAL);
				st1a = LiSessionAddPrim(LW_session[LW_mach_mode],st1);
				um_vctmsc(&pl1[3],-1.,&pl1[3]);
				um_vctmsc(&pl2[3],-1.,&pl2[3]);
				LiViSolidSection(st1a,pl1,&pl1[3],0,NULL,NULL);
				LiViSolidSection(st1a,pl2,&pl2[3],0,NULL,NULL);
				st2 = LiPrimitiveSolidExtract(st1a);
				LiViSolidSubtract(sd->stock,st1a);
				if (LW_mach_mode == LW_VISICUT) LiViSolidSetEnabled(st1a,FALSE);
				LiSessionRemovePrim(st1a);
				LiPrimitiveDestroy(st2);
				LiPrimitiveDestroy(st1);
			}
/*
.....Less than or equal to 180 degrees
.....Apply second section
*/
			else
			{
				LiViSolidSection(sd->stock,pl1,&pl1[3],0,NULL,NULL);
/*
.....Only split the solid once if 180 degrees
*/
				if (180.-r > UM_FUZZ)
					LiViSolidSection(sd->stock,pl2,&pl2[3],0,NULL,NULL);
			}
		}
	}
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_is_closed(prim)
**      This function determines if a stock primitive is closed and if
**      not prints out a list of its open edges.  The solid must be in
**      an active LiSOSessionBegin prior to calling this routine.
**   PARAMETERS
**       INPUT  : prim  = Solid primitive.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: UU_DEBUGL must be set to output this data.
**   WARNINGS: none
*********************************************************************/
UU_LOGICAL ul_ipv_is_closed(prim)
LtSessionPrim prim;
{
	UU_LOGICAL is_closed;
	char sbuf[80];
	LtData data;
	LtBody body;
	LtPatch patch;
	LtFace face;
	LtLoop loop;
	LtLoopEdge loop_edge, first_loop_edge;
	LtSEdge edge;
	LtDoublePoint point1, point2;
	LtVertex vertex;

	body = LiSOSolidGetBody (prim);
	LiSOEntityGetProperty (body, LI_SO_BODY_PROP_CLOSED, &data);
	is_closed = LiDataGetBoolean(&data);
	if (!is_closed)
	{
		for (patch = LiSOBodyGetFirstPatch (body); patch;
			patch = LiSOPatchGetNextPatch (patch))
		{
			LiSOEntityGetProperty (patch, LI_SO_PATCH_PROP_CLOSED, &data);
			if (!LiDataGetBoolean (&data))
			{
				for (face = LiSOPatchGetFirstFace (patch); face;
					face = LiSOFaceGetNextFace (face))
				{
					for (loop = LiSOFaceGetFirstLoop (face); loop;
						loop = LiSOLoopGetNextLoop (loop))
					{
						loop_edge = first_loop_edge =
						LiSOLoopGetFirstLoopEdge (loop);
						do
						{
							edge = LiSOLoopEdgeGetSEdge (loop_edge);
							LiSOEntityGetProperty (edge, LI_SO_EDGE_PROP_CLOSED,
								&data);
							if (!LiDataGetBoolean (&data))
							{
								vertex = LiSOLoopEdgeGetStartVertex (loop_edge);
								LiSOVertexGetPosition (vertex, point1);
								vertex = LiSOLoopEdgeGetEndVertex (loop_edge);
								LiSOVertexGetPosition (vertex, point2);
								sprintf(sbuf,"LINE/%lf,%lf,%lf, %lf,%lf,%lf)\n",
									point1[0], point1[1], point1[2],
									point2[0], point2[1], point2[2]);
								NclxDbgPstr(sbuf);
							}
							loop_edge = LiSOLoopEdgeGetNextOnFace (loop_edge);
						} while (loop_edge != first_loop_edge);
					}
				}
			}
		}
	}
	return(is_closed);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_enable_stl_form(flag)
**      Determines if the STL File status form should be displayed
**      when loading an STL model.
**
**   PARAMETERS
**       INPUT  :
**          flag    = UU_TRUE = Display form, UU_FALSE = Don't.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_enable_stl_form(flag)
UU_LOGICAL flag;
{
	Senable_stl_form = flag;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_open_stl_form(file)
**      Displays the STL File status form.
**
**   PARAMETERS
**       INPUT  : file  = Name of STL file that is loading.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_open_stl_form(file)
char *file;
{
	UX_pathname fname;
/*
.....Setup form fields
*/
	static char traverse[] = {1,1};
	static UD_METHOD methods[] = {UU_NULL,OnInterrupt,OnClose};
	static char called[] = {6,6};
	static char display[] = {1,0,1,1};
	static int *ans[] = {&Sprogress,UU_NULL};
/*
.....Initialize form
*/
	if (Senable_stl_form)
	{
		ul_short_filename(file,fname,40);
		sprintf(Sfile,"Loading: %s",fname);
		Sprogress = 0;
		LW_interrupt = 0;
		ans[0] = &Sprogress;
		ans[1] = UU_NULL;
/*
.....Display the form
*/
		Sfrm = ud_form_display1("ipvstlstat.frm",ans,ans,methods,called,display,
			traverse);
/*
.....Display the file loading sign
*/
		ud_dispfrm_update_label(Sfrm,FMSG,Sfile);
		LiCallBackSet(LI_CALL_MW_PROGRESS,(LtFunc)S_progress);
		LiCallBackSet(LI_CALL_DRIVER_CHECK_INTERRUPT,(LtFunc)S_interrupt);
		S_flush_form();
	}
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_close_stl_form()
**      Closes the STL File status form.
**
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_close_stl_form()
{
/*
.....Close STL status form if it is open
*/
	if (Sfrm != -1) ud_close_dispfrm(Sfrm);
	Sfrm = -1;
}

/*********************************************************************
**   I_FUNCTION: OnInterrupt(fieldno,val,stat)
**      Displays the STL File status form.
**
**   PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnInterrupt(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	LW_interrupt = 1;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnClose()
**      Marks the STL File status form as closed.
**
**   PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnClose()
{
	Sfrm = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: S_progress(progress, data);
**      Receives the STL progress report from the MachineWorks libraries
**      and updates the STL status form.
**
**   PARAMETERS
**       INPUT  :
**          progress  = Progress structure.
**       OUTPUT : none
**   RETURNS: LI_STATUS_INTERRUPT if the user interrupted the STL load.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static LtStatus S_progress(progress)
LtMWProgress progress;
{
	LtProgressProperty property;
	LtData data;
/*
.....Form is not open
*/
	if (Sfrm == -1)
		return(LI_STATUS_OK);
/*
.....User interrupted STL load
*/
	else if (LW_interrupt == 1)
	{
		LW_interrupt = 2;
		return(LI_STATUS_INTERRUPT);
	}
/*
.....Update progress field
*/
	else
	{
		property = LI_PROG_PROP_MW_PERCENT;
		LiProgressGetProperty(progress,property,&data);
		Sprogress = LiDataGetDouble(&data);
		ud_dispfrm_update_answer(Sfrm,FPRG,&Sprogress);
		S_flush_form();
		return(LI_STATUS_OK);
	}
}

/*********************************************************************
**   I_FUNCTION: S_interrupt();
**      Called from the MachineWorks libraries to see if the user
**      interrupts the current action.
**
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**   RETURNS: LI_STATUS_INTERRUPT if the user interrupted the STL load.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static LtStatus S_interrupt()
{
/*
.....Form is not open
*/
	if (Sfrm == -1)
		return(LI_STATUS_OK);
/*
.....Check for user interrupt
*/
	S_flush_form();
/*
.....User interrupted current action
*/
	if (LW_interrupt == 1)
	{
		LW_interrupt = 2;
		return(LI_STATUS_INTERRUPT);
	}
	else
		return(LI_STATUS_OK);
}

/*********************************************************************
**   I_FUNCTION: S_form_label(type)
**      Update the label displayed in the STL status form.
**
**   PARAMETERS
**       INPUT  :
**          type      0 = Erase label.
**                    1 = Display 'Making Consistent' label,
**                    2 = Display 'Fixing Self Intersections' label,
**                    3 = Display 'Checking Solid Orientation label,
**                    4 = Display 'Closing Solid' label,
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static void S_form_label(type)
int type;
{
	int itrav;
	if (Sfrm != -1)
	{
		itrav = 0;
		ud_setfrm_display_mask(Sfrm,UD_DISPLAYF,FLAB,UU_TRUE);
		if (type == 1) strcpy(Slabel,"Making Consistent...");
		else if (type == 2)
		{
			strcpy(Slabel,"Fixing Self-Intersections...");
			itrav = 1;
		}
		else if (type == 3) strcpy(Slabel,"Checking Solid Orientation...");
		else if (type == 4)
		{
			strcpy(Slabel,"Closing Solid...");
			itrav = 1;
		}
		else Slabel[0] = '\0';
		ud_dispfrm_update_label(Sfrm,FLAB,Slabel);
		ud_setfrm_traverse_mask(Sfrm,FINT,itrav);
		S_flush_form();
	}
}

/*********************************************************************
**   I_FUNCTION: S_flush_form()
**      Causes the operating system to update any active forms.
**
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static void S_flush_form()
{
	UM_int2 ifl35,ifl86;
/*
.....Flush the output buffer
.....by checking the input buffer
*/
	if (Sfrm != -1) ud_update_form(Sfrm);
	ifl35 = 0;
	ckintr(&ifl86,&ifl35);
}
