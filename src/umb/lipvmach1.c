/*********************************************************************
**   FILENAME: lipvmach1.c
**   CONTAINS: ul_ipv_free_mach()
**             ul_ipv_match_mach()
**             ul_ipv_reverse_axes()
**             ul_ipv_place_stock()
**             ul_ipv_place_axes()
**             ul_ipv_tool_pin()
**             ul_ipv_positn_axis()
**             ul_ipv_load_offsets()
**             ul_ipv_define_offsets()
**             ul_ipv_define_clash()
**             ul_ipv_tool_clash()
**             ul_ipv_clash_axes()
**             ul_ipv_circle_pts()
**
**     COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       lipvmach1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:13
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "lcom.h"
#include "mcrv.h"
#include "mdcpln.h"
#include "mdrel.h"
#include "mfort.h"
#include "m2dattr.h"
#include "nclfc.h"
#include "nccs.h"
#include "xfsys1.h"
#include "lipv.h"
#include "lipvmach.h"

void ul_ipv_clash_axes();

/*********************************************************************
**	 E_FUNCTION : ul_ipv_free_mach(mlist,nmodel,slist,nsolid,plist,npin)
**			This function frees the memory of the Machine Simulation
**       definitions.
**	 PARAMETERS	
**		 INPUT  :
**         mlist   = List of Machine model axis structures.
**         nmodel  = Number of axis defined in 'mlist'.
**         slist   = List of Solid primitives that define the machine model.
**         nsolid  = Number of solids defined in 'slist'.
**         plist   = List of defined toolpins.
**         npin    = Number of toolpins defined in 'plist'.
**		 OUTPUT : none.
**	 RETURNS: None
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
void ul_ipv_free_mach(mlist,nmodel,slist,nsolid,plist,npin)
UU_LIST *mlist,*slist,*plist;
int *nmodel,*nsolid,*npin;
{
	int i;
	LW_mach_solid_struc *spt;
	LW_mach_model_struc *mpt;
	LtData data;
/*
.....Delete all defined stocks
*/
	spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(slist);
	LiDataSetBoolean(&data,FALSE);
	for (i=0;i<*nsolid;i++)
	{
		if (spt[i].stock.id != 0 && spt[i].stock.stock != 0 &&
			LW_session[0] != 0)
		{
			LiMWViewportSetSessPrimProperty(LW_viewport,spt[i].stock.stock,
				LI_VPSP_PROP_MW_VISIBLE,&data);
			LiViSolidSetEnabled(spt[i].stock.stock,FALSE);
			LiSessionRemovePrim(spt[i].stock.stock);
			LiPrimitiveDestroy(spt[i].stock.prim);
		}
		if (spt[i].stock.data != UU_NULL) uu_free(spt[i].stock.data);
	}
/*
.....Delete model memory
*/
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(mlist);
	for (i=0;i<*nmodel;i++)
	{
		if (mpt[i].turret != UU_NULL) uu_free(mpt[i].turret);
	}
/*
.....Free lists
*/
	uu_list_free(mlist);
	uu_list_free(slist);
	uu_list_free(plist);
	*nmodel = 0;
	*nsolid = 0;
	*npin = 0;
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_match_mach(simul,model,nmodel)
**			This function matches the simulation file axes with the machine
**       model axes.  If they do not match or a simulation file is not
**       loaded, then Machine Simulation will be disabled.
**	 PARAMETERS	
**		 INPUT  : 
**           simul   = UU_TRUE is machine simulation is in effect.
**           model   = Machine model structure array.
**           nmodel  = Number of solids in 'model'.
**		 OUTPUT :
**           model.type = Type of axis (Linear, Rotary, or Unknown).
**	 RETURNS: None
**	 SIDE EFFECTS: Could disable Machine Simulation.
**	 WARNINGS:
*********************************************************************/
void ul_ipv_match_mach(simul,model,nmodel)
UU_LOGICAL simul;
UU_LIST *model;
int nmodel;
{
#define NAXS 18
	int i,j,k,ipt,inc,minc[LW_MAX_AXES],nspare;
	UU_LOGICAL idid;
	LW_mach_model_struc *mpt;
	static UM_vector xaxis = {1.,0.,0.};
	static UM_vector yaxis = {0.,1.,0.};
	static UM_vector zaxis = {0.,0.,1.};
	static char saxs[NAXS][3]={"X","Y","Z","C","A","X","Y","Z2","A2","B2","C2",
		"U","V","W","I","J","K","Q"};
/*
.....Machine Simulation is not active
*/
	LW_mach_naxes = 0;
	if (!simul) return;
	for (i=0;i<LW_MAX_AXES;i++) LW_mach_axes[i] = -1;
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(model);
/*
.....Match stringer type machine
*/
	if (LW_mach_type == LW_STRINGER)
	{
		ipt = 0;
		LW_mach_axes[5] = LW_mach_axes[6] = -1;
		LW_mach_naxes += 2;
		nspare = NAXS;
		for (i=0;i<nmodel;i++)
		{
			if (mpt[i].type == LW_MACH_LINEAR || mpt[i].type == LW_MACH_ROTARY)
			{
				idid = UU_FALSE;
				for (j=0;j<NAXS;j++)
				{
					if (strcmp(mpt[i].axisname,saxs[j]) == 0)
					{
						LW_mach_axes[j] = i;
/*						if (j == 0 || j == 1) LW_mach_axes[j+5] = i;*/
						idid = UU_TRUE;
						break;
					}
				}
/*
.....Axis not found
.....Store at end of array
*/
				if (!idid)
				LW_mach_axes[nspare++] = i;
			}
		}
	}
/*
.....A simulation file is not loaded
........Use machine components
*/
	else if (LW_mach_desc.type == -1)
	{
		ipt = 0;
		for (i=0;i<nmodel;i++)
		{
			if (mpt[i].type == LW_MACH_LINEAR || mpt[i].type == LW_MACH_ROTARY)
			{
/*
........Determine the priority of the axis
*/
				inc = 0;
				if (mpt[i].type == LW_MACH_ROTARY) inc = 4;
				if (um_cceqcc(&(mpt[i].axis[3]),xaxis)) inc = inc + 0;
				else if (um_cceqcc(&(mpt[i].axis[3]),yaxis)) inc = inc + 1;
				else if (um_cceqcc(&(mpt[i].axis[3]),zaxis)) inc = inc + 2;
				else inc = inc + 3;
/*
........Sort the axis
*/
				for (j=0;j<ipt;j++)
				{
					if (inc < minc[j])
					{
						for (k=ipt;k>j;k--)
						{
							LW_mach_axes[k] = LW_mach_axes[k-1];
							minc[k] = minc[k-1];
						}
						LW_mach_axes[j] = i;
						break;
					}
				}
				if (j >= ipt)
				{
					LW_mach_axes[ipt] = i;
					minc[ipt] = inc;
				}
				ipt++;
				if (ipt == LW_MAX_AXES) break;
			}
		}
	}
/*
.....Match simulation file axes
.....with machine model axes
*/
	else
	{
		nspare = 10;
		for (i=0;i<nmodel;i++)
		{
/*			mpt[i].type = LW_MACH_NOTYPE;*/
			idid = UU_FALSE;
/*
........Linear axes
*/
			ipt = 0;
			for (j=0;j<3;j++)
			{
				for (k=0;k<LW_mach_desc.numlin[j];k++)
				{
					if (strcmp(LW_mach_desc.linaxs[ipt+k],mpt[i].axisname) == 0)
					{
						LW_mach_axes[ipt+k] = i;
						mpt[i].type = LW_MACH_LINEAR;
						idid = UU_TRUE;
						break;
					}
				}
				if (idid) break;
				ipt = ipt + 2;
			}
/*
........Rotary axes
*/
			if (!idid)
			{
				for (j=0;j<LW_mach_desc.numrot;j++)
				{
					if (strcmp(LW_mach_desc.rotaxs[j],mpt[i].axisname) == 0)
					{
						LW_mach_axes[j+6] = i;
						mpt[i].type = LW_MACH_ROTARY;
						idid = UU_TRUE;
						break;
					}
				}
			}
/*
........Axis not found
........Place at end of axes
*/
			if (!idid && mpt[i].type != LW_MACH_NOTYPE)
				LW_mach_axes[nspare++] = i;
		}
	}
/*
........Count number of axes defined
*/
	for (i=0;i<LW_MAX_AXES;i++)
	{
		if (LW_mach_axes[i] >= 0)
		{
			LW_mach_naxes++;
			LW_mach_max_axes = i + 1;
		}
	}
	if (LW_mach_type == LW_STRINGER) LW_mach_naxes = nspare;
	if (LW_mach_naxes < 2) LW_mach_naxes = 0; /*LW_mach_simul = UU_FALSE;*/
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_reverse_axes(model,nmodel,spindle)
**			This function determines which axes should have their directions
**       reversed, based on whether or not they are attached to the
**       spindle.  It also determines which solid to use as the spindle and
**       the styles of axes (Head,Axis).
**	 PARAMETERS	
**		 INPUT  : 
**           model   = Machine model structure array.
**           nmodel  = Number of solids in 'model'.
**           spindle = Array of defined spindles.
**		 OUTPUT :
**           model.style   = This axis is connected to the Head or Axes.
**           model.reverse = UU_TRUE if axis is separate from the Spindle.
**           spindle       = The calculated spindle axis.
**	 RETURNS: None
**	 SIDE EFFECTS: Could disable Machine Simulation.
**	 WARNINGS:
*********************************************************************/
void ul_ipv_reverse_axes(model,nmodel,spindle)
UU_LIST *model;
int nmodel;
int *spindle;
{
	int i,j,ipt,inc,nspin;
	char sbuf[20];
	LW_mach_model_struc *mpt;
/*
.....Initialize routine
*/
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(model);
	for (i=0;i<nmodel;i++) mpt[i].style = LW_MACH_AXIS;
/*
.....Define the spindles
*/
	nspin = 0;
	for (i=0;i<LW_MAX_SPINDLE;i++)
	{
		LW_spindle[i] = spindle[i];
		if (LW_spindle[i] != -1) nspin++;
	}
	if (nspin == 0) nspin = 1;
/*
.....Loop through the spindles
*/
	for (inc=0;inc<nspin;inc++)
	{
		if (LW_spindle[inc] == -1)
		{
			if (nspin == 1)
				LW_spindle[inc] = nmodel - 1;
			else
			{
				LW_spindle[inc] = LW_spindle[nspin-1];
				continue;
			}
		}
/*
.....Find spindle tree axes
.....These will be changed to Head style axes
*/
		ipt = LW_spindle[inc];
		do
		{
			if (mpt[ipt].reverse == -1 || !mpt[ipt].reverse ||
				ipt == LW_spindle[inc])
			{
				mpt[ipt].style = LW_MACH_HEAD;
				mpt[ipt].reverse = UU_FALSE;
			}
			ipt = mpt[ipt].parent;
		} while (ipt > 0);
	}
/*
.....Now follow the branches
.....of the spindle tree
*/
	for (i=0;i<nmodel-1;i++)
	{
		if (mpt[i].style == LW_MACH_HEAD)
		{
			for (j=i+1;j<nmodel;j++)
			{
				if (mpt[j].parent == i && (mpt[j].reverse == -1 || !mpt[j].reverse))
				{
					mpt[j].style = LW_MACH_HEAD;
					mpt[j].reverse = UU_FALSE;
				}
			}
		}
	}
/*
.....Change rest of axes to machine slides
*/
	for (i=0;i<nmodel;i++)
	{
		if (mpt[i].reverse == -1) mpt[i].reverse = UU_TRUE;
	}
}

/*********************************************************************
**	 I_FUNCTION : ul_ipv_place_stock(sdin,resfl,modfl)
**			This function places the stocks and fixtures onto the machine.
**	 PARAMETERS	
**		 INPUT  :
**         sdin    = Pointer to single stock/fixture to place or UU_NULL
**                   if all stocks and fixtures should be placed.
**         resfl   = UU_TRUE = Reset stock to original position.
**                   UU_FALSE = Place stock on machine.
**		 OUTPUT :
**         modfl   = UU_TRUE = stock attributes were modified with call
**                   to 'ul_ipv_modify_stock'.
**	 RETURNS: None
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
void ul_ipv_place_stock(sdin,resfl,modfl)
LW_stock_struc *sdin;
UU_LOGICAL resfl,*modfl;
{
	int i,j,inc,ifl,ix;
	UU_LOGICAL iflag;
	UU_REAL rnum;
	UM_vector vc1,vc2;
	UM_transf tf;
	LW_stock_struc *sd,*sdtmp;
	LW_mach_toolpin_struc *tptr;
/*
.....If NCLIPV is not active
.....then do nothing
*/
	*modfl = UU_FALSE;
	if (LW_session[0] == 0 || !LW_mach_simul ||
		(!LW_mach_defined && !resfl)) return;
/*
.....Place stocks on machine
*/
	tptr = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
	for (i=0;i<2;i++)
	{
		if (sdin != UU_NULL)
		{
			inc = -1;
			sd = sdin;
		}
		else
		{
			sd = LW_stock_first[i];
			inc = 0;
		}
		for (j=inc;j<LW_nstock[i];j++)
		{
/*
.....Unitize all vectors
*/
			ix = sd->tpin;
			if (!resfl)
			{
				um_unitvc(tptr[ix].mpin[1],tptr[ix].mpin[1]);
				um_unitvc(tptr[ix].mpin[2],tptr[ix].mpin[2]);
				um_unitvc(tptr[ix].ppin[1],tptr[ix].ppin[1]);
				um_unitvc(tptr[ix].ppin[2],tptr[ix].ppin[2]);
/*
.....Calculate placement transformation matrix
*/
				um_cross(tptr[ix].mpin[1],tptr[ix].mpin[2],vc1);
				um_cross(tptr[ix].ppin[1],tptr[ix].ppin[2],vc2);
/*
				um_chgcstf(tptr[ix].ppin[0],tptr[ix].ppin[2],vc2,tptr[ix].ppin[1],
					tptr[ix].mpin[0],tptr[ix].mpin[2],vc1,tptr[ix].mpin[1],
					tf);
*/
				um_chgcstf(tptr[ix].mpin[0],tptr[ix].mpin[2],vc1,tptr[ix].mpin[1],
					tptr[ix].ppin[0],tptr[ix].ppin[2],vc2,tptr[ix].ppin[1],
					tf);
				um_inverttf(tf,tf);
			}
/*
.....Take stock off of machine
*/
			else
				um_tftotf(tptr[ix].invmx,tf);
/*
.....Make sure it is not a mirror matrix
*/
			um_determinant(tf,&rnum);
			if (rnum <= 0.)
			{
				ud_wrerr("A mirror matrix is not allowed.");
				return;
			}
/*
.....Remove reference to last stock position
*/
			um_identtf(tptr[ix].matrix);
			um_identtf(tptr[ix].invmx);
/*
........First remove it from machine
*/
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
				if (ifl == -2) break;
/*				ul_ipv_detach_assembly(sd);*/
				if (resfl)
					sdtmp->mxflag = UU_FALSE;
				else
					sdtmp->mxflag = UU_TRUE;
				sdtmp->mxchg = UU_TRUE;
				if (sdin == UU_NULL)
					sdtmp->invflag = sdtmp->placed;
				else
					sdtmp->invflag = UU_FALSE;
				um_tftotf(tf,sdtmp->matrix);
				um_tftotf(tptr[ix].invmx,sdtmp->invmx);
				ul_ipv_modify_stock(sdtmp,UU_FALSE);
				*modfl = UU_TRUE;
				sdtmp->mxflag = UU_FALSE;
				sdtmp->invflag = UU_FALSE;
				um_identtf(sdtmp->matrix);
				um_identtf(sdtmp->invmx);
/*
........Join stock/fixture to machine
*/
				if (!resfl && LW_mach_type == LW_LATHE && LW_lathe != 0)
				{
					iflag = ul_ipv_lathe_stop();
					LiViLatheAddSolid(LW_lathe,sdtmp->stock);
					if (iflag) ul_ipv_lathe_start();
				}
/*				if (!resfl) ul_ipv_place_assembly(sd);*/
			} while (ifl != -1);
/*
.....Store values in global arrays
*/
			if (resfl)
			{
				um_identtf(tptr[ix].matrix);
				um_identtf(tptr[ix].invmx);
			}
			else
			{
				um_tftotf(tf,tptr[ix].matrix);
				um_inverttf(tptr[ix].matrix,tptr[ix].invmx);
			}
			if (sdin != UU_NULL) break;
			sd = (LW_stock_struc *)uu_lsnext(sd);
		}
		if (sdin != UU_NULL) break;
	}
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**	 I_FUNCTION : ul_ipv_place_axes()
**			This function places the machine axes at their predefined
**       position.
**	 PARAMETERS	
**		 INPUT  :
**         none
**		 OUTPUT :
**         none
**	 RETURNS: None
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
void ul_ipv_place_axes()
{
	int i,ipt;
	UU_REAL posit[LW_MAX_AXES];
	LW_mach_model_struc *mpt;
	if (LW_mach_naxes > 0)
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		ipt = 0;
		for (i=0;i<LW_mach_max_axes;i++)
		{
			if (LW_mach_axes[i] != -1)
				posit[i] = mpt[LW_mach_axes[i]].position;
		}
		ul_ipv_move_assemblies(posit,UU_TRUE,UU_TRUE);
	}
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_tool_pin(label,parms,ifl)
**			This function processes the TOOLPN command.
**	 PARAMETERS	
**		 INPUT  :
**         label   = Tooling pin label to assign parameters to.  If
**                   it is blank, then the last used tooling pin will
**                   be assigned.
**         parms   = Part Tooling pin parameters.  0:2 = Location,
**                   3:5 = Axis vector, 6:8 = Flat vector.
**         ifl     = UU_TRUE - store tool pin parms.  UU_FALSE = Set active
**                   tool pin only.
**		 OUTPUT :
**         none
**	 RETURNS: UU_FAILURE if the tooling pin 'label' cannot be found.
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
int ul_ipv_tool_pin(label,parms,ifl)
char *label;
UU_REAL parms[];
UU_LOGICAL ifl;
{
	int i,status;
	UU_LOGICAL modfl;
	LW_mach_toolpin_struc *tptr;
/*
.....Find the requested tooling pin
*/
	status = UU_SUCCESS;
	if (LW_mach_simul)
	{
		tptr = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
		if (label[0] != '\0')
		{
			status = UU_FAILURE;
			for (i=0;i<LW_mach_num_tpin;i++)
			{
				if (!ul_compare_upper(label,tptr[i].label))
				{
					LW_mach_tpin_ix = i;
					status = UU_SUCCESS;
					break;
				}
			}
		}
/*
.....Store the part's tooling pin parameters
*/
		if (status == UU_SUCCESS && ifl)
		{
			um_vctovc(&parms[0],tptr[LW_mach_tpin_ix].ppin[0]);
			um_unitvc(&parms[3],tptr[LW_mach_tpin_ix].ppin[1]);
			um_unitvc(&parms[6],tptr[LW_mach_tpin_ix].ppin[2]);
/*
.....Attach the stock to the machine
*/
			ul_ipv_place_stock(0,UU_FALSE,&modfl);
		}
	}
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_positn_axis(label,pos,naxes,ofs)
**			This function processes the POSITN and OFFSET commands.
**	 PARAMETERS	
**		 INPUT  :
**         label   = Labels of axes to position.
**         pos     = New positions of axes.
**         naxes   = Number of axes to position.
**         ofs     = UU_FALSE = process for POSITN command.
**                   UU_TRUE  = process for OFFSET command.
**		 OUTPUT :
**         none
**	 RETURNS: UU_FAILURE if the machine axis is not defined.
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
int ul_ipv_positn_axis(label,pos,naxes,ofs)
char label[LW_MAX_AXES][20];
UU_REAL *pos;
int naxes;
UU_LOGICAL ofs;
{
	int i,j,status;
	UU_REAL posit[LW_MAX_AXES];
	LW_mach_model_struc *mpt;
/*
.....Find the requested axis
*/
	status = UU_FAILURE;
	if (LW_mach_simul)
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		for (i=0;i<LW_mach_nmodel;i++)
		{
			if (mpt[i].type == LW_MACH_LINEAR || mpt[i].type == LW_MACH_ROTARY)
			{
				for (j=0;j<naxes;j++)
				{
					if (ul_compare_upper(label[j],mpt[i].axisname) == 0)
					{
						status = UU_SUCCESS;
						if (ofs)
							mpt[i].offset = pos[j];
						else
							mpt[i].position = pos[j];
						break;
					}
				}
			}
		}
/*
.....Store the axes positions
*/
		if (status == UU_SUCCESS)
		{
			for (i=0;i<LW_mach_max_axes;i++)
			{
				if (LW_mach_axes[i] != -1)
					posit[i] = mpt[LW_mach_axes[i]].position;
			}
/*
.....Position the axes
*/
			ul_ipv_move_assemblies(posit,UU_TRUE,UU_FALSE);
		}
	}
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_load_offsets(fname)
**      Loads an axis offset file.
**
**   PARAMETERS
**       INPUT  :
**          fname   = Name of offsets file to load.
**       OUTPUT : none
**   RETURNS: UU_FAILURE if there is a problem loading the file.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_load_offsets(fname)
UX_pathname fname;
{
	int stat,status,mode,fstat,inum,inc,ix,nc;
	char sbuf[80],lbuf[256];
	FILE *fd;
	LW_offsets_struc treg;
/*
.....Add default extension
*/
	ux_add_ftype("ofs",fname,UX_NPRTERRS);
/*
.....Make sure file already exists
*/
	mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
	stat = ux_file_inquire(UU_NULL,UU_NULL,fname,UU_NULL,UU_NULL,
		&mode,&fstat,lbuf,UX_NPRTERRS);
	if (stat != UU_SUCCESS) goto filerr;
	if (mode == (mode|UX_NEXISTS)) goto filerr;
/*
.....Open file for reading
*/
	stat = ux_fopen0(fname,"r",&fd);
	if (stat != UU_SUCCESS) goto filerr;
/*
.....Initialize register lists
*/
	uu_list_free(&LW_register_offsets[0]);
	uu_list_free(&LW_register_offsets[1]);
	uu_list_init(&LW_register_offsets[0],sizeof(LW_offsets_struc),10,10);
	uu_list_init(&LW_register_offsets[1],sizeof(LW_offsets_struc),10,10);
/*
.....Read a record
*/
	do
	{
		stat = ul_fread(fd,lbuf,sizeof(lbuf),&inum);
		if (stat == UX_EOF) break;
		if (stat != UU_SUCCESS) goto failed;
/*
.....Parse the offsets line
*/
		inc =0;
		ul_parse_string(lbuf,inum,&inc,sbuf);
/*
........FIXTURE
........TOOL
*/
		if (!ul_compare_upper(sbuf,"FIXTURE"))
			ix = 0;
		else if (!ul_compare_upper(sbuf,"TOOL"))
			ix = 1;
		else
			goto failed;
/*
........Register number for offsets
*/
		ul_parse_string(lbuf,inum,&inc,sbuf);
		stat = ul_to_number(sbuf,&treg.reg);
		if (stat != UU_SUCCESS || treg.reg < 0) goto failed;
/*
........Get the offsets
*/
		treg.nofs = 0;
		do
		{
			ul_parse_string(lbuf,inum,&inc,treg.label[treg.nofs]);
			if (treg.label[treg.nofs][0] == '\0') break;
			ul_parse_string(lbuf,inum,&inc,sbuf);
			if (sbuf[0] == '\0') goto failed;
			stat = ul_to_reals(&treg.offset[treg.nofs++],&nc,1,sbuf);
			if (stat != UU_SUCCESS) goto failed;
		} while (sbuf[0] != '\0');
/*
.....Store the register offsets in the list
*/
		uu_list_push(&LW_register_offsets[ix],&treg);
	} while (stat != UX_EOF);
	status = UU_SUCCESS;
	ux_fclose0(fd);
	goto done;
/*
.....Could not open file
*/
filerr:;
	status = UU_FAILURE;
/*
	ul_short_filename(file,sbuf,60);
	sprintf(lbuf,"Could not open %s.",sbuf);
	ud_wrerr(lbuf);
*/
	goto done;
/*
.....Error reading from file
*/
readerr:;
	status = UU_FAILURE;
/*
	ul_short_filename(file,sbuf,60);
	sprintf(lbuf,"Error reading from %s.",sbuf);
	ud_wrerr(lbuf);
*/
	ux_fclose0(fd);
	goto done;
/*
.....Error parsing line
*/
failed:;
	status = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_define_offsets(cmode,creg,tmode,treg)
**      Defines the machine offsets based the active offset register and
**      loaded offsets file.
**
**   PARAMETERS
**       INPUT  :
**          cmode   = Cutcom offset mode.  0 = Cancel offsets, 1 = Plus
**                    offsets, 2 = Minus offsets.
**
**          creg    = Cutcom offset register to use to load offsets.
**
**          tmode   = Toolno offset mode.  0 = Cancel offsets, 1 = Plus
**                    offsets, 2 = Minus offsets.
**
**          treg    = Toolno offset register to use to load offsets.
**
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_define_offsets(cmode,creg,tmode,treg)
int cmode,creg,tmode,treg;
{
	int mode[2],reg[2],i,j,k,l,nc;
	UU_REAL sgn;
	LW_mach_model_struc *mpt;
	LW_offsets_struc *tofs;
/*
.....Don't do anything if an offset file
.....was not defined
*/
	if (!LW_mach_simul) goto done;
	if (UU_LIST_LENGTH(&LW_register_offsets[0]) == 0 &&
		UU_LIST_LENGTH(&LW_register_offsets[1]) == 0) goto done;
/*
.....Store base offsets defined in
.....machine configuration file
*/
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	for (i=0;i<LW_mach_nmodel;i++)
	{
		if (mpt[i].type == LW_MACH_LINEAR || mpt[i].type == LW_MACH_ROTARY)
			mpt[i].offset = mpt[i].baseofs;
	}
/*
.....Find Cutcom offset values
*/
	mode[0] = cmode; mode[1] = tmode;
	reg[0] = creg; reg[1] = treg;
	for (i=0;i<2;i++)
	{
		if (mode[i] == -1)
		{
			mode[i] = LW_offset_mode[i];
			reg[i] = LW_offset_reg[i];
		}
		if (mode[i] > 0)
		{
			sgn = (mode[i] == 1) ? 1. : -1.;
			tofs = (LW_offsets_struc *)UU_LIST_ARRAY(&LW_register_offsets[i]);
			nc = UU_LIST_LENGTH(&LW_register_offsets[i]);
			for (j=0;j<nc;j++)
			{
				if (reg[i] == tofs[j].reg)
				{
					for (k=0;k<LW_mach_nmodel;k++)
					{
						if (mpt[k].type == LW_MACH_LINEAR ||
							mpt[k].type == LW_MACH_ROTARY)
						{
							for (l=0;l<tofs[j].nofs;l++)
							{
								if (!ul_compare_upper(tofs[j].label[l],mpt[k].axisname))
									mpt[k].offset =
										mpt[k].offset + tofs[j].offset[l]*sgn;
							}
						}
					}
				}
			}
		}
		LW_offset_mode[i] = mode[i];
		LW_offset_reg[i] = reg[i];
	}
done:;
	return;
}
					

/*********************************************************************
**	 I_FUNCTION : ul_ipv_define_clash()
**			This function defines the clash detection style between all
**       defined machine components and stocks and fixtures.
**	 PARAMETERS	
**		 INPUT  :
**         sdin   = Single stock/fixture to define clash detection for.
**                  If UU_NULL, then clash detection is set for all
**                  machine components and stocks and fixtures.
**		 OUTPUT :
**         none
**	 RETURNS: None
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
void ul_ipv_define_clash(sdin)
LW_stock_struc *sdin;
{
	int i,j,k,m,ifl;
	UU_LOGICAL stopfl,logfl;
	LW_mach_axis_style style,fstyle;
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	LW_stock_struc *sd,*sdtmp;
	LtInteractType itype;
	LtData stuff;
	LtSolidType type;
/*
.....Initialize routine
*/
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
	if (sdin != UU_NULL)
	{
		LiSessionPrimGetProperty(sdin->stock,LI_SPRIM_PROP_SOLID_TYPE,&stuff);
		type = LiDataGetEnum(&stuff);
		if (type == LI_MW_SOLID_TYPE_STOCK)
			fstyle = LW_MACH_STOCK;
		else
			fstyle = LW_MACH_FIXTURE;
	}
/*
.....Loop through machine components
*/
	for (i=0;i<LW_mach_nmodel;i++)
	{
		for (j=mpt[i].beg_solid;j<=mpt[i].end_solid;j++)
		{
/*
........Ignore clashes against solids
........of same axis
*/
			if (sdin == UU_NULL)
			{
				if (j < mpt[i].end_solid)
					LiViSolidPairSetInteractType(spt[j].stock.stock,
						spt[j+1].stock.stock,LI_MW_INTERACT_IGNORE);
/*
........Loop through rest of components
*/
				if (i+1 < LW_mach_nmodel)
				{
					for (k=i+1;k<LW_mach_nmodel;k++)
					{
/*
...........Determine type of clash to set
...........based on axis types
*/
						if (mpt[i].parent == k || mpt[k].parent == i)
							itype = LI_MW_INTERACT_IGNORE;
						else
						{
							ul_ipv_clash_axes(mpt[i].style,mpt[k].style,&stopfl,&logfl);
							if (stopfl || logfl)
								itype = LI_MW_INTERACT_CLASH_CONTINUE;
							else
								itype = LI_MW_INTERACT_IGNORE;
						}
						for (m=mpt[k].beg_solid;m<=mpt[k].end_solid;m++)
						{
							LiViSolidPairSetInteractType(spt[j].stock.stock,
								spt[m].stock.stock,itype);
						}
					}
				}
			}
/*
........Now set the clash type with
........machine component and stocks/fixtures
*/
			style = LW_MACH_STOCK;
			for (k=0;k<2;k++)
			{
				if (sdin == UU_NULL)
				{
					sd = LW_stock_first[k];
				}
				else
				{
					sd = sdin;
					style = fstyle;
				}
				ul_ipv_clash_axes(mpt[i].style,style,&stopfl,&logfl);
				if (stopfl || logfl)
					itype = LI_MW_INTERACT_CLASH_CONTINUE;
				else
					itype = LI_MW_INTERACT_IGNORE;
				if (sdin != UU_NULL)
				{
					LiViSolidPairSetInteractType(spt[j].stock.stock,sd->stock,itype);
					break;
				}
				else
				{
					for (m=0;m<LW_nstock[k];m++)
					{
						ifl = 0;
						do
						{
							ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
							if (ifl == -2) break;
							LiViSolidPairSetInteractType(spt[j].stock.stock,
								sdtmp->stock,itype);
						} while (ifl != -1);
						sd = (LW_stock_struc *)uu_lsnext(sd);
					}
				}
				style = LW_MACH_FIXTURE;
			}
		}
	}
}

/*********************************************************************
**	 I_FUNCTION : ul_ipv_tool_clash()
**			This function defines the clash detection style between the
**       tool components and machine components, stocks, and fixtures.
**	 PARAMETERS	
**		 INPUT  :
**         none
**		 OUTPUT :
**         none
**	 RETURNS: None
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
void ul_ipv_tool_clash()
{
	int i,j,ix,is,inc,ccut;
	UU_LOGICAL stopfl,logfl;
	LW_mach_axis_style style;
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	LtInteractType itype,ftype;
	UN_cutter_list *cpt;
	LtSessionPrim prim;
/*
.....Initialize routine
*/
	cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
/*
.....Loop through active spindles
*/
	for (inc=0;inc<LW_spindle_num;inc++)
	{
/*
........Loop through tool components
*/
		for (ix=0;ix<2+LW_num_holder[inc];ix++)
		{
			ccut = LI_MW_INTERACT_CLASH_CUT;
			if (ix == 0)
			{
				prim = LW_tool[inc];
				style = LW_MACH_TOOL;
				is = 0;
			}
			else if (ix == 1)
			{
				prim = LW_shank[inc];
				if (cpt[LW_act_tool[inc]].shank_clash == 1 ||
					cpt[LW_act_tool[inc]].ctype[1] == 5)
				{
					style = LW_MACH_HOLDER;
					is = 4;
					if (cpt[LW_act_tool[inc]].ctype[1] == 5)
						ccut = LI_MW_INTERACT_CLASH_CONTINUE;
				}
				else
				{
					style = LW_MACH_TOOL;
					is = 0;
				}
			}
			else
			{
				prim = LW_holder[inc][ix-2];
				style = LW_MACH_HOLDER;
				is = 4;
				if (cpt[LW_act_tool[inc]].ctype[1] == 5)
					ccut = LI_MW_INTERACT_CLASH_CONTINUE;
			}
			if (prim == 0) continue;
/*
...........Stock clash
*/
			if (is != 0 || ix == 1)
			{
				if ((LW_clash_stop[is] || LW_clash_log[is] ||
					ccut == LI_MW_INTERACT_CLASH_CONTINUE) && is != 0)
					LiSessionPrimSetInteractType(prim,LI_MW_SOLID_TYPE_STOCK,ccut);
				else
					LiSessionPrimSetInteractType(prim,LI_MW_SOLID_TYPE_STOCK,
						LI_MW_INTERACT_CUT);
			}
/*
...........Fixture clash
*/
			if (LW_clash_stop[is+1] || LW_clash_log[is+1] ||
				ccut == LI_MW_INTERACT_CLASH_CONTINUE)
				ftype = ccut;
			else
				ftype = LI_MW_INTERACT_CUT;
			LiSessionPrimSetInteractType(prim,LI_MW_SOLID_TYPE_FIXTURE,ftype);
/*
...........Tool clash
*/
			LiSessionPrimSetInteractType(prim,LI_MW_SOLID_TYPE_TOOL,
				LI_MW_INTERACT_CLASH_CONTINUE);
			LiSessionPrimSetInteractType(prim,LI_MW_SOLID_TYPE_HOLDER,
				LI_MW_INTERACT_CLASH_CONTINUE);
/*
...........Machine components
*/
			if (LW_mach_simul && LW_mach_nmodel > 0)
			{
				for (i=0;i<LW_mach_nmodel;i++)
				{
					if (i == LW_spindle[LW_spindle_ix[inc]])
						itype = LI_MW_INTERACT_IGNORE;
					else
					{
						ul_ipv_clash_axes(style,mpt[i].style,&stopfl,&logfl);
						if (stopfl || logfl || ccut == LI_MW_INTERACT_CLASH_CONTINUE)
							itype = ccut;
						else
							itype = LI_MW_INTERACT_CUT;
					}
					if (itype != ftype)
					{
						for (j=mpt[i].beg_solid;j<=mpt[i].end_solid;j++)
						{
							LiViSolidPairSetInteractType(prim,spt[j].stock.stock,
								itype);
						}
					}
				}
			}
		}
	}
}

/*********************************************************************
**	 I_FUNCTION : ul_ipv_clash_axes(style1,style2,stopfl,logfl)
**			This function determines the clash detection logic to used
**       based on the style of the solid components and the clash
**       settings made by the user.
**
**	 PARAMETERS	
**		 INPUT  :
**         istyle1 = Style of first solid (Tool, Holder, Head, Axis, Stock,
**                                         Fixture).
**         istyle2 = Style of second solid.
**		 OUTPUT :
**         stopfl  = UU_TRUE if playback should stop when these two axes
**                   clash.
**         logfl   = UU_TRUE if a message should be logged when these two
**                   axes clash.
**	 RETURNS: None
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
void ul_ipv_clash_axes(istyle1,istyle2,stopfl,logfl)
LW_mach_axis_style istyle1,istyle2;
UU_LOGICAL *stopfl,*logfl;
{
	int inc;
	LW_mach_axis_style style1,style2;
/*
.....Sort the styles
*/
	inc = -1;
	if ((int)istyle1 <= (int)istyle2)
	{
		style1 = istyle1;
		style2 = istyle2;
	}
	else
	{
		style1 = istyle2;
		style2 = istyle1;
	}
/*
.....Tool
*/
	if (style1 == LW_MACH_TOOL)
	{
		if (style2 == LW_MACH_STOCK) inc = 0;
		else if (style2 == LW_MACH_FIXTURE) inc = 1;
		else if (style2 == LW_MACH_HEAD) inc = 2;
		else if (style2 == LW_MACH_AXIS) inc = 3;
		else if (LW_spindle_num > 1)
		{
			if (style2 == LW_MACH_TOOL) inc = 2;
			else if (style2 == LW_MACH_HOLDER) inc = 2;
		}
	}
/*
.....Holder
*/
	else if (style1 == LW_MACH_HOLDER)
	{
		if (style2 == LW_MACH_STOCK) inc = 4;
		else if (style2 == LW_MACH_FIXTURE) inc = 5;
		else if (style2 == LW_MACH_HEAD) inc = 6;
		else if (style2 == LW_MACH_AXIS) inc = 7;
	}
/*
.....Head
*/
	else if (style1 == LW_MACH_HEAD)
	{
		if (style2 == LW_MACH_STOCK) inc = 8;
		else if (style2 == LW_MACH_FIXTURE) inc = 9;
		else if (style2 == LW_MACH_HEAD) inc = 10;
		else if (style2 == LW_MACH_AXIS) inc = 11;
	}
/*
.....Axis
*/
	else if (style1 == LW_MACH_AXIS)
	{
		if (style2 == LW_MACH_STOCK) inc = 12;
		else if (style2 == LW_MACH_FIXTURE) inc = 13;
		else if (style2 == LW_MACH_AXIS) inc = 14;
	}
/*
.....Return Stop & Log flags
*/
	if (inc != -1)
	{
		*stopfl = LW_clash_stop[inc];
		*logfl = LW_clash_log[inc];
	}
	else
	{
		*stopfl = UU_FALSE;
		*logfl = UU_FALSE;
	}
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_circle_pts(cpt,spt,ept,dir,tlist,npt)
**			This function generates points around a circle defined with a
**       Center, Start, and End point and Direction.
**	 PARAMETERS	
**		 INPUT  :
**         cpt     = Center point of circle.
**         spt     = Starting point of circle.
**         ept     = Ending point of circle.
**         dir     = 0 - generate points in a CLW direction, 1 = CCLW.
**		 OUTPUT :
**         tlist   = List of generated points.
**         npt     = Number of points in 'tlist'.
**	 RETURNS:
**         UU_SUCESS if the circle and points were generated correctly,
**         UU_FAILURE otherwise.
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
int ul_ipv_circle_pts(cpt,spt,ept,dir,tlist,npt)
UM_coord cpt,spt,ept;
int dir;
UU_LIST *tlist;
int *npt;
{
	int dsav,psav,n,status;
	struct UM_circle_rec c1;
	UM_transf tm;
	static UM_vector zneg={0.,0.,-1.};
/*
.....Create the circle record
*/
	status = UU_SUCCESS;
	if (dir == 0)
	{
		status = ncl_cutr_arcc2p(cpt,ept,spt,&c1);
		um_reverse_circle(&c1);
	}
	else
		status = ncl_cutr_arcc2p(cpt,spt,ept,&c1);
	if (status == UU_SUCCESS)
	{
		c1.rel_num = UM_CIRCLE_REL;
/*
.....If vector is pointing in negative direction
.....then user defined a circle of more than 180 degrees
.....so change to the opposite circle
*/
		if (um_cceqcc(c1.nvec,zneg))
		{
			um_vctmsc(c1.nvec,-1.,c1.nvec);
			c1.dang = UM_TWOPI - c1.dang;
		}
/*
.....Generate points around circle
*/
		dsav = UM_2dattr.disp_flag;
		psav = UM_2dattr.pts_per_span;
		UM_2dattr.disp_flag = UU_FALSE;
		UM_2dattr.pts_per_span = UL_ipv_npts;
		um_identtf(tm);
		uu_list_init(tlist,sizeof(UM_coord),50,20);
		n = ncl_evolve_all_curves(&c1,tm,LW_stock_default[1].toler,tlist,
			UU_NULL,UU_FALSE);
		UM_2dattr.disp_flag = dsav;
		UM_2dattr.pts_per_span = psav;
		if (n < 1) status = UU_FAILURE;
		*npt = n;
	}
/*
.....End of routine
*/
	return(status);
}
