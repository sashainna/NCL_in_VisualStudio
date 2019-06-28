/*********************************************************************
**   FILENAME: lipvmach3.c
**   CONTAINS:
**             ul_ipv_load_mach()
**             ul_ipv_mach_parse_rec()
**
**     COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       lipvmach3.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:14
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "lcom.h"
#include "mdcpln.h"
#include "mfort.h"
#include "nclfc.h"
#include "nccs.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "lipvmach.h"

static int Sunits;
static UX_pathname Sfulldir;
static char Sinternal[25][80];

static int S_load_dat();
static void S_init_model();
static int S_load_model();
static int S_init_solid();
static int S_load_profile();
static void S_open_err();
static void S_read_err();
static void S_parse_err();
static void S_unit_conv();

extern char uw_color_name[64][96];
/*********************************************************************
**	 E_FUNCTION : ul_ipv_load_mach(dir,mname,fulldir,desc,model,solid,nmodel,
**                                nsolid,spindle,plist,npin)
**			This function loads Machine Simulation data files.
**	 PARAMETERS	
**		 INPUT  :
**        dir       = Main level directory for machine data files.
**        mname     = Name (sub-directory) of machine data files.
**		 OUTPUT :
**        fullname  = Full directory path of machine data files.
**        desc      = Machine description structure.
**        model     = Machine model array.
**        solid     = Machine solids array.
**        nmodel    = Number of axes in 'model'.
**        nsolid    = Number of solids in 'solid'.
**        spindle   = Array of defined spindle axes.
**        plist     = List of defined tooling pins.  This list is initialized
**                    in this routine, but must be freed in a calling routine.
**        npin      = Number of tooling pins defined.
**	 RETURNS: UU_SUCCESS when files are read correctly, UU_FAILURE otherwise.
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
int ul_ipv_load_mach(dir,mname,fulldir,desc,model,solid,nmodel,nsolid,spindle,
	plist,npin)
UX_pathname dir,mname,fulldir;
LW_mach_data_struc *desc;
UU_LIST *model,*solid,*plist;
int *nmodel,*nsolid,*npin;
int spindle[];
{
	int status;
	UX_pathname fullname;
	if ((((dir==NULL)||dir[0]=='\0'))&&(((mname==NULL)||mname[0]=='\0')))
		return -1;
/*
.....Build the full directory path
*/
	ul_build_full_fname(dir,mname,"",fulldir);
	strcpy(Sfulldir,fulldir);
/*
.....Load the Machine Data file
*/
	ul_build_full_fname(fulldir,"machine.dat","",fullname);
	status = S_load_dat(fullname,desc);
/*
.....Load the Machine Model file
*/
	if (status == UU_SUCCESS)
	{
		ul_build_full_fname(fulldir,"postworks.mdl","",fullname);
		status = S_load_model(fullname,model,nmodel,solid,nsolid,spindle,plist,
			npin);
	}
/*
.....Set the correct machine type
.....Using the 'Family' machine type setting
*/
	if (status == UU_SUCCESS)
	{
		if (!ul_compare_upper(desc->desc[1],"LATHE"))
		{
			LW_mach_type_main = LW_mach_type = LW_LATHE;
			LW_is_lathe = UU_TRUE;
		}
		else if (!ul_compare_upper(desc->desc[1],"MILL/TURN"))
		{
			LW_mach_type_main = LW_mach_type = LW_MILLTURN;
			LW_is_lathe = UU_TRUE;
		}
		else if (!ul_compare_upper(desc->desc[1],"STRINGER"))
		{
			LW_mach_type_main = LW_mach_type = LW_STRINGER;
			LW_is_lathe = UU_FALSE;
		}
		else
		{
			LW_mach_type_main = LW_mach_type = LW_MILL;
			LW_is_lathe = UU_FALSE;
		}
/*
.....Match the machine axes with
.....the simulation axes
*/
		ul_ipv_match_mach(UU_TRUE,model,*nmodel);
/*
.....Reverse the proper axes
*/
		ul_ipv_reverse_axes(model,*nmodel,spindle);
	}
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_mach_parse_rec(lbuf,nc,parms,nparm,istext)
**			This function parses a Machine Simulation record from any of
**       the data files.
**	 PARAMETERS	
**		 INPUT  :
**         lbuf    = Machine Simulation file record to parse.
**         nc      = Number of chars in 'lbuf'.
**         ptext   = 0 - No parameters contain a text string.
**                   Non-zero - Parameter that is purely a text string.
**                   This will be the last parameter parsed for this record.
**		 OUTPUT :
**         parms   = Array of text parameters in record.
**         nparm   = Number of text parameters returned in 'parms'.
**	 RETURNS:
**         UU_SUCCESS when parsing is successful, UU_FAILURE otherwise.
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
int ul_ipv_mach_parse_rec(lbuf,nc,parms,nparm,ptext)
char *lbuf;
int nc;
char parms[50][80];
int *nparm;
int ptext;
{
	int i,ix,ipt,status,inc;
	UU_LOGICAL coment;
	char *p,*strchr();
	static char *delim={"/ 	,()="};
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	i = ix = ipt = 0;
	coment = UU_FALSE;
/*
.....Find parameters
*/
	do
	{
/*
........Get rid of delimiters
*/
		for (i=i;i<nc;i++)
		{
			p = strchr(delim,lbuf[i]);
			if (p == UU_NULL) break;
/*
...........Comment
*/
			if (lbuf[i] == '/' && lbuf[i+1] == '/')
			{
				coment = UU_TRUE;
				break;
			}
		}
		if (coment || i >= nc) break;
		if (ix >= 50)
		{
			status = UU_FAILURE;
			break;
		}
/*
........Text record
*/
		if (ptext != 0 && ix == ptext)
		{
			strncpy(parms[ix],&lbuf[i], 79);
			parms[ix][79] = '\0';
			inc = strlen(parms[ix]) - 1;
			if (inc >= 0 && parms[ix][inc] == ')') parms[ix][inc] = '\0';
			ix++;
			break;
		}
/*
........Break out parameter
*/
		for (i=i;i<nc;i++)
		{
			p = strchr(delim,lbuf[i]);
			if (p != UU_NULL) break;
			parms[ix][ipt] = lbuf[i];
			ipt++;
		}
		parms[ix][ipt] = '\0';
		ix++;
		ipt = 0;
	} while (i < nc);
	*nparm = ix;
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**	 S_FUNCTION : S_load_dat(fname,mach)
**			This function loads the Machine Data file (machine.dat).
**	 PARAMETERS	
**		 INPUT  :
**         fname   = Filename to load.
**		 OUTPUT :
**         mach    = Machine descriptor structure.
**	 RETURNS:
**         UU_SUCCESS when files are read correctly, UU_FAILURE otherwise.
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static int S_load_dat(fname,mach)
UX_pathname fname;
LW_mach_data_struc *mach;
{
	int status,stat,nparm,nc,inc,ist,i;
	char parms[50][80],lbuf[256];
	FILE *fd;
/*
.....Open the data file
*/
	status = UU_SUCCESS;
	fd = LiFileOpen(fname,LI_FILE_READ);
	if (fd == UU_NULL)
	{
		S_open_err(fname);
		status = UU_FAILURE;
	}
/*
.....Initialize structure
*/
	for (i=0;i<=MTBF;i++) mach->desc[i][0] = '\0';
	for (i=0;i<MACHINEAGE-MTBF-1;i++) mach->rary[i] = 0.;
	for (i=0;i<3;i++) mach->vec[i][0] = mach->vec[i][1] = mach->vec[i][2] = 0.;
	Sunits = 0;
/*
.....Loop to read the file
*/
	if (status == UU_SUCCESS)
	{
		do
		{
			stat = ul_fread(fd,lbuf,sizeof(lbuf),&nc);
			if (stat == UX_EOF) break;
			status = stat;
			if (status != UU_SUCCESS)
			{
				S_read_err(fname);
				break;
			}
/*
.....Parse the input block
*/
			status = ul_ipv_mach_parse_rec(lbuf,nc,parms,&nparm,0);
			if (status != UU_SUCCESS)
			{
				S_parse_err(lbuf);
				break;
			}
			if (nparm == 0) continue;
/*
.....Determine the Parameter type
*/
			for (inc=0;inc<NMACHDAT;inc++)
				if (!ul_compare_upper(LW_mach_desc_opt[inc],parms[0])) break;
			if (inc >= NMACHDAT)
			{
				S_parse_err(lbuf);
				status = UU_FAILURE;
				break;
			}
/*
.....Machine Description
........MachineName thru MTBF
*/
			if (inc <= MTBF)
			{
				if (!ul_compare_upper(parms[1],"null") || nparm == 1)
					mach->desc[inc][0] = '\0';
				else 
				{
					ul_ipv_mach_parse_rec(lbuf,nc,parms,&nparm,1);
					strcpy(mach->desc[inc],parms[1]);
				}
				if (inc == UNIT)
				{
					if (!ul_compare_upper(mach->desc[inc],"MM")) Sunits = 1;
					else if (!ul_compare_upper(mach->desc[inc],"INCH")) Sunits = 0;
					else
					{
						S_parse_err(lbuf);
						status = UU_FAILURE;
						break;
					}
				}
			}
/*
.....Machine Attributes
........TotalLength thru MachineAge
*/
			else if (inc <= MACHINEAGE)
				status = ul_to_reals(&(mach->rary[inc-MTBF-1]),&nc,1,parms[1]);
/*
.....Machine vectors
........ToolChangeAxis thru ViewUpVector
*/
			else
			{
				if (nparm != 4) status = UU_FAILURE;
				ist = inc - MACHINEAGE - 1;
				if (status == UU_SUCCESS)
					status = ul_to_reals(&(mach->vec[ist][0]),&nc,1,parms[1]);
				if (status == UU_SUCCESS)
					status = ul_to_reals(&(mach->vec[ist][1]),&nc,1,parms[2]);
				if (status == UU_SUCCESS)
					status = ul_to_reals(&(mach->vec[ist][2]),&nc,1,parms[3]);
			}
			if (status != UU_SUCCESS) S_parse_err(lbuf);
		} while (status == UU_SUCCESS);
	}
/*
.....End of routine
*/
	if (fd != UU_NULL) LiFileClose(fd);
	return(status);
}

/*********************************************************************
**	 S_FUNCTION : S_load_model(fname,mlist,nmodel,slist,nsolid,spindle,plist,
**                            npin)
**			This function loads the Machine Model file (*.mdl).
**	 PARAMETERS	
**		 INPUT  :
**         fname   = Filename to load.
**		 OUTPUT :
**         mlist   = List of Machine model axis structures.
**         nmodel  = Number of axis defined in 'mlist'.
**         slist   = List of Solid primitives that define the machine model.
**         nsolid  = Number of solids defined in 'slist'.
**         spindle = Array of defined spindle axes.
**         plist   = List of defined tooling pins.  This list is initialized
**                   in this routine, but must be freed in a calling routine.
**         npin    = Number of tooling pins defined.
**	 RETURNS:
**         UU_SUCCESS when files are read correctly, UU_FAILURE otherwise.
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static int S_load_model(fname,mlist,nmodel,slist,nsolid,spindle,plist,npin)
UX_pathname fname;
UU_LIST *mlist,*slist;
int *nmodel,*nsolid;
UU_LIST *plist;
int spindle[],*npin;
{
	int i,j,ipt,status,stat,nparm,nc,inc,inc1,nmod,nsol,n;
	UU_LOGICAL match;
	char parms[50][80],lbuf[256];
	UU_REAL rnum,rval[9];
	UX_pathname dir,aname;
	LW_mach_model_struc *mpt,smodel;
	LW_mach_solid_struc *spt,*sols;
	LW_mach_toolpin_struc tpin;
	FILE *fd,*fd1;
#define NCOL 64
	char colr[65][96];

	strcpy(colr[0], "DEFAULT");
	for (i=0; i<64;i++)
	{
		sprintf(colr[i+1], "%s", uw_color_name[i]);
	}
/*
.....Free previously loaded model
*/
	ul_ipv_free_mach(mlist,nmodel,slist,nsolid,plist,npin);
/*
.....Initialize routine
*/
	nmod = 0;
	nsol = 0;
	sols = UU_NULL;
	uu_list_init(mlist,sizeof(LW_mach_model_struc),10,5);
	uu_list_init(slist,sizeof(LW_mach_solid_struc),50,20);
	uu_list_init(plist,sizeof(LW_mach_toolpin_struc),10,5);
/*
.....Initialize the model structure
*/
	S_init_model(&smodel);
/*
.....Open the data file
*/
	status = UU_SUCCESS;
	fd1 = UU_NULL;
	fd = LiFileOpen(fname,LI_FILE_READ);
	if (fd == UU_NULL)
	{
		S_open_err(fname);
		status = UU_FAILURE;
	}
/*
.....Loop to read the file
*/
	if (status == UU_SUCCESS)
	{
		do
		{
			stat = ul_fread(fd,lbuf,sizeof(lbuf),&nc);
			if (stat == UX_EOF)
			{
				if (fd1 != UU_NULL)
				{
					LiFileClose(fd);
					fd = fd1;
					fd1 = UU_NULL;
					status = UU_SUCCESS;
					continue;
				}
				else
					break;
			}
			status = stat;
			if (status != UU_SUCCESS)
			{
				S_read_err(fname);
				break;
			}
/*
.....Parse the input block
*/
			status = ul_ipv_mach_parse_rec(lbuf,nc,parms,&nparm,0);
			if (status != UU_SUCCESS)
			{
				S_parse_err(lbuf);
				break;
			}
			if (nparm == 0) continue;
/*
.....Determine the Parameter type
*/
			for (inc=0;inc<NMACHMDL;inc++)
				if (!ul_compare_upper(LW_mach_model_opt[inc],parms[0])) break;
			switch (inc)
			{
/*
.....MachineName
*/
			case 0:
				if (!ul_compare_upper(parms[1],"null") || nparm == 1)
					smodel.name[0] = '\0';
				else 
				{
					ul_ipv_mach_parse_rec(lbuf,nc,parms,&nparm,1);
					strcpy(smodel.name,parms[1]);
				}
				break;
/*
.....Unit
*/
			case 1:
				if (!ul_compare_upper(parms[1],"MM")) smodel.units = 1;
				else if (!ul_compare_upper(parms[1],"INCH")) smodel.units = 0;
				else goto parse_err;
				break;
/*
.....AxisName
*/
			case 2:
				if (fd1 != UU_NULL) break;
				if (nmod != 0)
				{
					if (smodel.beg_solid == -1) goto parse_err;
					uu_list_push(mlist,&smodel);
					S_init_model(&smodel);
				}
				ul_ipv_mach_parse_rec(lbuf,nc,parms,&nparm,1);
				strncpy(smodel.axisname,parms[1],19);
				smodel.axisname[19] = '\0';
				nmod++;
/*
........Search for Axis.Dat
*/
				fd1 = fd;
				ul_break_fname(fname,dir,aname);
				ul_build_full_fname(dir,smodel.axisname,"dat",aname);
				fd = LiFileOpen(aname,LI_FILE_READ);
				if (fd == UU_NULL)
				{
					fd = fd1;
					fd1 = UU_NULL;
					break;
				}
				break;
/*
.....Color
*/
			case 3:
				if (nparm != 3) goto parse_err;
				if (!ul_compare_upper(parms[2],"Olive"))
					strcpy(parms[2],"SeaGrn");
				if (!ul_compare_upper(parms[2],"Gray")) strcpy(parms[2],"Grey");
				if (!ul_compare_upper(parms[2],"Magenta"))
					strcpy(parms[2],"Magnta");
				for (inc=-1;inc<NCOL;inc++)
					if (!ul_compare_upper(parms[2],colr[inc+1])) break;
				if (inc >= NCOL) goto parse_err;
				if (!ul_compare_upper(parms[1],smodel.axisname))
					smodel.color = inc;
				else
				{
					spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(slist);
					nc = strlen(parms[1]);
					match = UU_FALSE;
					for (i=0;i<nsol;i++)
					{
						if (S_match_assem(parms[1],nc,spt[i].name))
						{
							spt[i].stock.color = inc;
							match = UU_TRUE;
						}
					}
					if (!match) goto parse_err;
				}
				break;
/*
.....Rotate
*/
			case 4:
/*
........Rotate(solid,XYZ,angle)
*/
				if (nparm == 4)
				{
					status = ul_to_reals(&rnum,&nc,1,parms[3]);
					if (status != UU_SUCCESS) goto parse_err;
					if (!ul_compare_upper(parms[2],"X"))
					{
						rval[0] = 1.;
						rval[3] = 0.;
						rval[6] = 0.;
						rval[1] = 0.;
						rval[4] = cos(rnum/UM_RADIAN);
						rval[7] = -sin(rnum/UM_RADIAN);
						rval[2] = 0.;
						rval[5] = sin(rnum/UM_RADIAN);
						rval[8] = cos(rnum/UM_RADIAN);
					}
					else if (!ul_compare_upper(parms[2],"Y"))
					{
						rval[0] = cos(rnum/UM_RADIAN);
						rval[3] = 0.;
						rval[6] = sin(rnum/UM_RADIAN);
						rval[1] = 0.;
						rval[4] = 1.;
						rval[7] = 0.;
						rval[2] = -sin(rnum/UM_RADIAN);
						rval[5] = 0.;
						rval[8] = cos(rnum/UM_RADIAN);
					}
					else if (!ul_compare_upper(parms[2],"Z"))
					{
						rval[0] = cos(rnum/UM_RADIAN);
						rval[3] = -sin(rnum/UM_RADIAN);
						rval[6] = 0.;
						rval[1] = sin(rnum/UM_RADIAN);
						rval[4] = cos(rnum/UM_RADIAN);
						rval[7] = 0.;
						rval[2] = 0.;
						rval[5] = 0.;
						rval[8] = 1.;
					}
					else
						goto parse_err;
				}
/*
........Rotate(solid,matrix)
*/
				else if (nparm == 11)
				{
					ipt = 2;
					inc = 0;
					for (i=0;i<3;i++)
					{
						for (j=0;j<3;j++)
						{
							status = ul_to_reals(&rval[inc++],&nc,1,parms[ipt++]);
							if (status != UU_SUCCESS) goto parse_err;
						}
					}
				}
				else
					goto parse_err;
				spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(slist);
				nc = strlen(parms[1]);
				match = UU_FALSE;
				for (inc=0;inc<nsol;inc++)
				{
					if (S_match_assem(parms[1],nc,spt[inc].name))
					{
						ipt = 0;
						for (i=0;i<3;i++)
						{
							for (j=0;j<3;j++)
								spt[inc].stock.matrix[i][j] = rval[ipt++];
						}
						spt[inc].stock.mxflag = UU_TRUE;
						match = UU_TRUE;
					}
				}
				if (!match) goto parse_err;
				break;
/*
.....Translate
*/
			case 5:
				if (nparm != 5) goto parse_err;
				spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(slist);
				ipt = 2;
				for (i=0;i<3;i++)
				{
					status = ul_to_reals(&rval[i],&nc,1,parms[ipt++]);
					if (status != UU_SUCCESS) goto parse_err;
					S_unit_conv(&rval[i],1,smodel.units);
				}
				nc = strlen(parms[1]);
				match = UU_FALSE;
				for (inc=0;inc<nsol;inc++)
				{
					if (S_match_assem(parms[1],nc,spt[inc].name))
					{
						for (i=0;i<3;i++) spt[inc].stock.matrix[3][i] = rval[i];
						spt[inc].stock.mxflag = UU_TRUE;
						match = UU_TRUE;
					}
				}
				if (!match) goto parse_err;
				break;
/*
.....Join
*/
			case 6:
				if (nparm != 4) goto parse_err;
				if (ul_compare_upper(parms[1],smodel.axisname)) goto parse_err;
				mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(mlist);
				for (inc=0;inc<nmod-1;inc++)
					if (!ul_compare_upper(parms[3],mpt[inc].axisname)) break;
				if (inc >= nmod-1) goto parse_err;
				smodel.parent = inc;
				break;
/*
.....AxisLine
*/
			case 7:
				if (nparm != 7) goto parse_err;
				ipt = 1;
				for (i=0;i<6;i++)
				{
					status = ul_to_reals(&(smodel.axis[i]),&nc,1,parms[ipt++]);
					if (status != UU_SUCCESS) goto parse_err;
				}
				S_unit_conv(smodel.axis,3,smodel.units);
/*
........Store multiple axis lines with turret
*/
				if (smodel.turret != UU_NULL)
				{
					if (smodel.turret->naxis < LW_MAX_TURRET)
					{
						for (i=0;i<6;i++)
							smodel.turret->axis[smodel.turret->naxis][i] =
								smodel.axis[i];
						smodel.turret->naxis++;
					}
				}
					
				break;
/*
.....AxisPosition
*/
			case 8:
				if (nparm != 4) goto parse_err;
				if (ul_compare_upper(parms[1],smodel.axisname)) goto parse_err;
				status = ul_to_reals(&(smodel.position),&nc,1,parms[3]);
				if (status != UU_SUCCESS) goto parse_err;
				if (smodel.type == LW_MACH_LINEAR)
					S_unit_conv(&smodel.position,1,smodel.units);
/*				smodel.home = smodel.position;*/
				break;
/*
.....ToolingPin
*/
			case 9:
				if (nparm != 10 && nparm != 11) goto parse_err;
				ipt = 1;
/*
........Store label
*/
				if (nparm == 11)
					strcpy(tpin.label,parms[ipt++]);
				else
					strcpy(tpin.label,smodel.axisname);
/*
........Store Coordinates
*/
				for (i=0;i<3;i++)
				{
					status = ul_to_reals(&(tpin.mpin[0][i]),&nc,1,parms[ipt]);
					if (status != UU_SUCCESS) goto parse_err;
					status = ul_to_reals(&(tpin.mpin[1][i]),&nc,1,parms[ipt+3]);
					if (status != UU_SUCCESS) goto parse_err;
					status = ul_to_reals(&(tpin.mpin[2][i]),&nc,1,parms[ipt+6]);
					if (status != UU_SUCCESS) goto parse_err;
					ipt++;
				}
				S_unit_conv(tpin.mpin[0],3,smodel.units);
				um_unitvc(tpin.mpin[1],tpin.mpin[1]);
				um_unitvc(tpin.mpin[2],tpin.mpin[2]);
				tpin.axis = nmod - 1;
/*
........Use default part pin
*/
				um_nullvc(tpin.ppin[0]);
				tpin.ppin[1][0] = tpin.ppin[1][1] = 0.; tpin.ppin[1][2] = 1.;
				tpin.ppin[2][0] = 1.; tpin.ppin[2][1] = tpin.ppin[2][2] = 0.;
/*
........Initialize placement matrix
*/
				um_identtf(tpin.matrix);
				um_identtf(tpin.invmx);
/*
........Store in Toolpin list
*/
				uu_list_push(plist,&tpin);
				*npin = *npin + 1;
				break;
/*
.....ToolSpindle
*/
			case 10:
				if (nparm != 2) goto parse_err;
				status = ul_to_number(parms[1],&i);
				if (status != UU_SUCCESS || i < 0 || i > 9) goto parse_err;
				spindle[i] = nmod - 1;
				break;
/*
.....AxisOffset
*/
			case 11:
				if (nparm != 4) goto parse_err;
				if (ul_compare_upper(parms[1],smodel.axisname)) goto parse_err;
				status = ul_to_reals(&(smodel.offset),&nc,1,parms[3]);
				if (status != UU_SUCCESS) goto parse_err;
				if (smodel.type == LW_MACH_LINEAR)
					S_unit_conv(&smodel.offset,1,smodel.units);
				smodel.baseofs = smodel.offset;
				break;
/*
.....Visible
*/
			case 12:
				if (nparm != 3) goto parse_err;
				if (!ul_compare_upper(parms[2],"Yes")) inc = 1;
				else if (!ul_compare_upper(parms[2],"No")) inc = 0;
				else goto parse_err;
				if (!ul_compare_upper(parms[1],smodel.axisname))
					smodel.visible = inc;
				else
				{
					spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(slist);
					nc = strlen(parms[1]);
					match = UU_FALSE;
					for (i=0;i<nsol;i++)
					{
						if (S_match_assem(parms[1],nc,spt[i].name))
						{
							spt[i].stock.visible = inc;
							match = UU_TRUE;
						}
					}
					if (!match) goto parse_err;
				}
				break;
/*
.....Translucency
*/
			case 13:
				if (nparm != 3) goto parse_err;
				status = ul_to_number(parms[2],&inc);
				if (status != UU_SUCCESS || inc < 0 || inc > 100) goto parse_err;
				if (!ul_compare_upper(parms[1],smodel.axisname))
					smodel.translucency = inc;
				else
				{
					spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(slist);
					nc = strlen(parms[1]);
					match = UU_FALSE;
					for (i=0;i<nsol;i++)
					{
						if (S_match_assem(parms[1],nc,spt[i].name))
						{
							spt[i].stock.translucency = inc;
							match = UU_TRUE;
						}
					}
					if (!match) goto parse_err;
				}
				break;
/*
.....Reverse
*/
			case 14:
				if (nparm != 3) goto parse_err;
				if (ul_compare_upper(parms[1],smodel.axisname)) goto parse_err;
				if (!ul_compare_upper(parms[2],"Yes")) inc = 1;
				else if (!ul_compare_upper(parms[2],"No")) inc = 0;
				else goto parse_err;
				smodel.reverse = inc;
				break;
/*
.....AxisType
*/
			case 15:
				if (!ul_compare_upper(parms[1],"Linear"))
					smodel.type = LW_MACH_LINEAR;
				else if (!ul_compare_upper(parms[1],"Rotary"))
					smodel.type = LW_MACH_ROTARY;
				else if (!ul_compare_upper(parms[1],"None"))
					smodel.type = LW_MACH_NOTYPE;
				else goto parse_err;
				break;
/*
.....MaxTravel
.....MinTravel
.....MaxFeed
.....MinFeed
.....Accuracy
.....Repeatability
.....MaxAcceleration
.....MaxDeceleration
*/
			case 16:
			case 17:
			case 18:
			case 19:
			case 20:
			case 21:
			case 22:
			case 23:
				if (nparm != 2) goto parse_err;
				status = ul_to_reals(&(smodel.parms[inc-16]),&nc,1,parms[1]);
				if (status != UU_SUCCESS) goto parse_err;
				S_unit_conv(&smodel.parms[inc-16],1,smodel.units);
				break;
/*
.....Home
*/
			case 24:
				if (nparm != 2) goto parse_err;
				status = ul_to_reals(&(smodel.home),&nc,1,parms[1]);
				if (status != UU_SUCCESS) goto parse_err;
				if (smodel.type == LW_MACH_LINEAR)
					S_unit_conv(&smodel.home,1,smodel.units);
				smodel.position = smodel.home;
				break;
/*
.....Description
.....FeedbackSystem
.....ControlSystem
*/
			case 25:
			case 26:
			case 27:
				ul_ipv_mach_parse_rec(lbuf,nc,parms,&nparm,1);
				strcpy(smodel.desc[inc-23],parms[1]);
				break;
/*
.....EdgeDisplay
*/
			case 28:
				if (nparm != 3 && nparm != 4) goto parse_err;
				if (!ul_compare_upper(parms[2],"On")) inc1 = 1;
				else if (!ul_compare_upper(parms[2],"Off")) inc1= 0;
				else goto parse_err;
				inc = smodel.edge_color;
				if (nparm == 4)
				{
					if (!ul_compare_upper(parms[3],"Olive"))
						strcpy(parms[3],"SeaGrn");
					if (!ul_compare_upper(parms[3],"Gray")) strcpy(parms[3],"Grey");
					if (!ul_compare_upper(parms[3],"Magenta"))
						strcpy(parms[3],"Magnta");
/*					if (!ul_compare_upper(parms[3],"Black")) inc = 1;
					else*/
					{
						for (inc=-1;inc<NCOL;inc++)
							if (!ul_compare_upper(parms[3],colr[inc+1])) break;
						if (inc >= NCOL) goto parse_err;
/*						if (inc != 0) inc = inc + 1;*/
					}
				}
				if (!ul_compare_upper(parms[1],smodel.axisname))
				{
					smodel.edge = inc1;
					smodel.edge_color = inc;
				}
				else
				{
					spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(slist);
					nc = strlen(parms[1]);
					match = UU_FALSE;
					for (i=0;i<nsol;i++)
					{
						if (S_match_assem(parms[1],nc,spt[i].name))
						{
							spt[i].stock.edge = inc1;
							spt[i].stock.edge_color = inc;
							match = UU_TRUE;
						}
					}
					if (!match) goto parse_err;
				}
				break;
/*
.....ToolTurret
*/
			case 29:
				if (nparm != 11) goto parse_err;
				status = S_init_turret(&smodel.turret);
				if (status != UU_SUCCESS) goto parse_err;
				for (i=0;i<8;i++)
				{
					status = ul_to_reals(&(smodel.turret->parms[i]),&nc,1,
						parms[i+1]);
					if (status != UU_SUCCESS) goto parse_err;
				}
				S_unit_conv(&smodel.turret[0],3,smodel.units);
				um_unitvc(&smodel.turret->parms[3],&smodel.turret->parms[3]);

				status = ul_to_number(parms[9],&inc);
				if (status != UU_SUCCESS || inc > LW_MAX_TURRET) goto parse_err;
				smodel.turret->iparm[0] = inc;

				if (!ul_compare_upper(parms[10],"OFF"))
					smodel.turret->iparm[1] = 0;
				else if (!ul_compare_upper(parms[10],"ON"))
					smodel.turret->iparm[1] = 1;
				else
					goto parse_err;

				if (smodel.turret->parms[6] != 0. && smodel.turret->parms[7] != 0.)
				{
					status = S_create_turret(&sols,&n,smodel.turret->parms,
						smodel.turret->iparm[0],smodel.axisname,smodel.units);
					for (i=0;i<n;i++) uu_list_push(slist,&sols[i]);
					if (smodel.beg_solid < 0) smodel.beg_solid = nsol;
					nsol += n;
					smodel.end_solid = nsol - 1;
					smodel.type = LW_MACH_ROTARY;
				}
				break;
/*
.....AxisScale
*/
			case 30:
				if (nparm != 4) goto parse_err;
				if (ul_compare_upper(parms[1],smodel.axisname)) goto parse_err;
				status = ul_to_reals(&(smodel.scale),&nc,1,parms[3]);
				if (status != UU_SUCCESS) goto parse_err;
				break;
/*
.....Solid primitive
*/
			default:
				status = S_init_solid(&sols,&n,lbuf,nc,parms,&nparm,smodel.units);
				if (status != UU_SUCCESS) goto parse_err;
				for (i=0;i<n;i++) uu_list_push(slist,&sols[i]);
				if (smodel.beg_solid < 0) smodel.beg_solid = nsol;
				nsol += n;
				smodel.end_solid = nsol - 1;
				break;
			}
		} while (status == UU_SUCCESS);
	}
/*
.....Add last model
*/
	uu_list_push(mlist,&smodel);
	goto done;
/*
.....Parsing error
*/
parse_err:
	S_parse_err(lbuf);
	nmod--;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	*nmodel = nmod;
	*nsolid = nsol;
	if (fd != UU_NULL) LiFileClose(fd);
	if (sols != UU_NULL) uu_free(sols);
	return(status);
}

/*********************************************************************
**	 S_FUNCTION : S_init_model(model)
**			This function initializes a Machine Model structure.
**	 PARAMETERS	
**		 INPUT  :
**         none.
**		 OUTPUT :
**         model   = Initialized Machine Model structure.
**	 RETURNS:
**         none.
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static void S_init_model(model)
LW_mach_model_struc *model;
{
	int i;
/*
.....Initialize the data structure
*/
	model->name[0] = '\0';
	model->units = Sunits;
	for (i=0;i<3;i++) model->desc[i][0] = '\0';
	model->color = 14;
	model->visible = 1;
	model->translucency = 100;
	model->edge = UU_FALSE;
	model->edge_color = -1;
	model->axisname[0] = '\0';
	model->axis[0] = model->axis[1] = model->axis[2] = 0.;
	model->axis[3] = 1.; model->axis[4] = model->axis[5] = 0.;
	for (i=0;i<6;i++) model->modaxis[i] = model->axis[i];
	model->position = 0.;
	model->last_pos = 0.;
	model->home = 0.;
	model->offset = model->baseofs = 0.;
	model->scale = 1.;
	for (i=0;i<8;i++)
	{
		model->parms[i] = 0.;
	}
	model->turret = UU_NULL;
	model->beg_solid = -1;
	model->end_solid = -1;
	model->parent = -1;
	model->type = LW_MACH_NOTYPE;
	model->style = LW_MACH_HEAD;
	model->reverse = -1;
	model->assembly = 0;
}

/*********************************************************************
**	 S_FUNCTION : S_init_turret(turret)
**			This function initializes a Machine Turret structure.
**	 PARAMETERS	
**		 INPUT  :
**         none.
**		 OUTPUT :
**         turret   = Initialized Machine Turret structure.
**	 RETURNS:
**         none.
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static int S_init_turret(turret)
LW_mach_turret_struc **turret;
{
	int i,j;
/*
.....Only initialize turret if it
.....has not been initialized before
*/
	if (*turret == UU_NULL)
	{
/*
.....Allocate memory for turret
*/
		*turret =
			(LW_mach_turret_struc *)uu_malloc(sizeof(LW_mach_turret_struc));
		if (*turret == UU_NULL) return(UU_FAILURE);
/*
.....Initialize turret parameters
*/
		for (i=0;i<8;i++) (*turret)->parms[i] = 0.;
		(*turret)->parms[4] = 1.;
		(*turret)->iparm[0] = 0;
		(*turret)->iparm[1] = 0;
		(*turret)->naxis = 0;
		for (j=0;j<LW_MAX_TURRET;j++)
		{
			for (i=0;i<6;i++) (*turret)->axis[j][i] = 0.;
			(*turret)->axis[j][3] = 1.;
			(*turret)->nholder[j] = 0;
			(*turret)->tool_prim[j] = 0;
			(*turret)->shank_prim[j] = 0;
		}
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**	 S_FUNCTION : S_create_turret(sols,nsol,turret,nsidex,axisname,units)
**			This function builds the turret Machine solid.
**	 PARAMETERS	
**		 INPUT  :
**         turret   = Turret definition.
**         nsidex   = Number of sides to turret.
**         axisname = Name of axis currently being defined.
**         units    = 0 = Inch, 1 = MM.
**		 OUTPUT :
**         sols     = Machine Solid structure list.
**         nsol     = Number of solids in 'sols'.
**	 RETURNS:
**         UU_SUCCESS on success, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS:
*********************************************************************/
static int S_create_turret(sols,nsol,turret,nsidex,axisname,units)
LW_mach_solid_struc **sols;
int *nsol;
UU_REAL turret[];
int nsidex;
char *axisname;
int units;
{
	int status,nc,nparm,i,nsides;
	UU_REAL ang,ainc,rad;
	char parms[50][80],lbuf[256];
	UM_coord pt[25];
	UM_vector xaxis,yaxis,zaxis;
	LW_mach_solid_struc *sptr;
/*
.....Build turret as extrusion
*/
	sprintf(lbuf,"%s_1 = SweepSolid(%f,internal.internal)",axisname,
		turret[6]);
	nsides = nsidex;
	if (nsides < 3) nsides = 4;
	ainc = UM_TWOPI / nsides;
	ang = ainc / 2.;
	rad = turret[7] / cos(ang);
	for (i=0;i<nsides;i++)
	{
		pt[i][0] = rad * cos(ang);
		pt[i][1] = rad * sin(ang);
		pt[i][2] = 0;
/*
		pt[i][0] = turret[0] + rad * cos(ang);
		pt[i][1] = turret[2] + rad * sin(ang);
		pt[i][2] = turret[1];
*/
		ang += ainc;
	}
	um_vctovc(pt[0],pt[nsides]);
	for (i=0;i<nsides;i++)
	{
		sprintf(Sinternal[i],"Line = (%f,%f,%f, %f,%f,%f)",pt[i][0],pt[i][1],
			pt[i][2],pt[i+1][0],pt[i+1][1],pt[i+1][2]);
	}
	strcpy(Sinternal[nsides],"EOF");
	nc = strlen(lbuf);
	ul_ipv_mach_parse_rec(lbuf,nc,parms,&nparm,0);
/*
.....Create turret solid
*/
	status = S_init_solid(sols,nsol,lbuf,nc,parms,&nparm,units);
/*
.....Create transformation matrix for turret
*/
	xaxis[0] = 1; xaxis[1] = xaxis[2] = 0;
	yaxis[1] = 1; yaxis[0] = yaxis[2] = 0;
	zaxis[2] = 1; zaxis[0] = zaxis[1] = 0;
	sptr = *sols;
/*
........Use standard 0,1,0 rotation
*/
	if (um_vcparall(&turret[3],zaxis))
	{
		um_vctovc(xaxis,sptr[0].stock.matrix[0]);
		um_vctovc(yaxis,sptr[0].stock.matrix[1]);
		um_vctovc(zaxis,sptr[0].stock.matrix[2]);
	}
/*
........Create rotation matrix
*/
	else
	{
			um_cross(zaxis,&turret[3],sptr[0].stock.matrix[0]);
			um_cross(sptr[0].stock.matrix[0],&turret[3],sptr[0].stock.matrix[1]);
			um_vctovc(&turret[3],sptr[0].stock.matrix[2]);
	}
/*
........Create translation portion of the matrix
*/
	sptr[0].stock.matrix[3][0] = turret[0];
	sptr[0].stock.matrix[3][1] = turret[1];
	sptr[0].stock.matrix[3][2] = turret[2];
	sptr[0].stock.mxflag = UU_TRUE;
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**	 S_FUNCTION : S_init_solid(sols,nsol,lbuf,nc,parms,nparm)
**			This function parses a Solid primitive record and initializes
**       the Machine Solid structure.
**	 PARAMETERS	
**		 INPUT  :
**         lbuf    = Machine Simulation file record to parse.
**         nc      = Number of chars in 'lbuf'.
**         parms   = Array of text parameters in record.
**         nparm   = Number of text parameters returned in 'parms'.
**         units   = 0 = Inch, 1 = MM.
**		 OUTPUT :
**         sols    = Machine Solid structure list.
**         nsol    = Number of solids in 'sols'.
**	 RETURNS:
**         UU_SUCCESS on success, UU_FAILURE otherwise.
**	 SIDE EFFECTS:
**         Memory is allocated for the Machine solids and must be
**         deallocated by the calling routine after processing the
**         entire model file.
**	 WARNINGS:
*********************************************************************/
static int S_init_solid(sols,nsol,lbuf,nc,parms,nparm,units)
LW_mach_solid_struc **sols;
int *nsol;
char *lbuf;
int nc;
char parms[50][80];
int *nparm,units;
{
	FILE *fd;
	int i,j,k,status,inc,maxp,nc1,npts,n,nstk[2],mode,fstat;
	UU_REAL rdata[12],rtmp[6];
	char *p,*strrchr();
	UX_pathname fullname,tnam;
	UU_LIST plist;
	UM_coord *pts,*opt;
	LW_mach_solid_struc solid;
	LW_stock_struc *sp[2];

	static int Snsol=0;
	static LW_mach_solid_struc *Ssptr;

	int nprim=9;
	static char *prim[]={
		"Box","Cylinder","Cone","Sphere","RevolveSolid","SweepSolid","STLSolid",
		"StockFile","Torus"};
	static LW_stock_type Stype[]={LW_STOCK_BOX,LW_STOCK_CYLINDER,LW_STOCK_CONE,
		LW_STOCK_SPHERE,LW_STOCK_REVOLVE,LW_STOCK_SWEEP,LW_STOCK_FILE,
		LW_STOCK_SESSION,LW_STOCK_TORUS,LW_STOCK_NONE};
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	*nsol = 0;
/*
.....Initialize the data structure
*/
	strcpy(solid.name,parms[0]);
	solid.stock.type = LW_STOCK_NONE;
	solid.stock.id = 0;
	solid.stock.color = -1;
	solid.stock.visible = -1;
	solid.stock.active = UU_TRUE;
	solid.stock.translucency = -1;
	solid.stock.edge = -1;
	solid.stock.edge_color = -1;
	solid.stock.toler = LW_stock_default[1].toler;
	solid.stock.mxflag = UU_FALSE;
	solid.stock.invflag = UU_FALSE;
	solid.stock.mxchg = UU_FALSE;
	solid.stock.mxname[0] = '\0';
	solid.stock.axes = UU_FALSE;
	solid.stock.axes_color = -1;
	solid.stock.axis_seg = 0;
	um_identtf(solid.stock.matrix);
	solid.stock.data = UU_NULL;
/*
.....Determine the solid type
*/
	for (inc=0;inc<nprim;inc++) if (!ul_compare_upper(parms[1],prim[inc])) break;
	if (inc > nprim) goto failed;
	solid.stock.type = Stype[inc];
/*
.....Allocate memory for standard solid
*/
	if (solid.stock.type != LW_STOCK_SESSION)
	{
		if (Snsol == 0 || *sols == UU_NULL)
		{
			if (*sols != UU_NULL) uu_free(*sols);
			*sols = (LW_mach_solid_struc *)uu_malloc(sizeof(LW_mach_solid_struc));
			if (*sols == UU_NULL) goto failed;
			Snsol = 1;
		}
		Ssptr = *sols;
		*nsol = 1;
	}
/*
.....Create the solid
*/
	switch (solid.stock.type)
	{
/*
........Box
*/
	case LW_STOCK_BOX:
		maxp = 6;
		n = *nparm - 2;
		status = S_parse_reals(&parms[2],n,maxp,rtmp);
		if (status != UU_SUCCESS) goto failed;
		rtmp[3] += rtmp[0];
		rtmp[4] += rtmp[1];
		rtmp[5] += rtmp[2];
		ncl_init_box(&rtmp[0],rdata);
		ncl_update_box(&rtmp[3],rdata);
		S_unit_conv(rdata,maxp,units);
		npts = 0;
		break;
/*
........Cylinder
*/
	case LW_STOCK_CYLINDER:
		maxp = 8;
		n = *nparm - 2;
		status = S_parse_reals(&parms[2],n,maxp,rdata);
		if (status != UU_SUCCESS) goto failed;
		S_unit_conv(rdata,3,units);
		S_unit_conv(&rdata[6],1,units);
		um_vctmsc(&rdata[3],rdata[6],&rdata[3]);
		rdata[6] = rdata[7];
		S_unit_conv(&rdata[6],1,units);
		maxp = 7;
		npts = 0;
		break;
/*
........Cone
*/
	case LW_STOCK_CONE:
		maxp = 9;
		n = *nparm - 2;
		status = S_parse_reals(&parms[2],n,maxp,rdata);
		if (status != UU_SUCCESS) goto failed;
		S_unit_conv(rdata,3,units);
		S_unit_conv(&rdata[6],1,units);
		um_vctmsc(&rdata[3],rdata[6],&rdata[3]);
		rdata[6] = rdata[7];
		rdata[7] = rdata[8];
		S_unit_conv(&rdata[6],2,units);
		maxp = 8;
		npts = 0;
		break;
/*
........Sphere
*/
	case LW_STOCK_SPHERE:
		maxp = 4;
		n = *nparm - 2;
		status = S_parse_reals(&parms[2],n,maxp,rdata);
		if (status != UU_SUCCESS) goto failed;
		S_unit_conv(rdata,maxp,units);
		npts = 0;
		break;
/*
........Torus
*/
	case LW_STOCK_TORUS:
		maxp = 8;
		n = *nparm - 2;
		status = S_parse_reals(&parms[2],n,maxp,rdata);
		if (status != UU_SUCCESS) goto failed;
		S_unit_conv(rdata,3,units);
		S_unit_conv(&rdata[6],2,units);
		npts = 0;
		break;
/*
........Revolve
*/
	case LW_STOCK_REVOLVE:
		if (*nparm < 3) goto failed;
		ul_ipv_mach_parse_rec(lbuf,nc,parms,nparm,2);
		status = S_load_profile(parms[2],&plist,&npts,units);
		if (status != UU_SUCCESS) goto failed;
		rdata[0] = rdata[1] = rdata[2] = 0.;
		rdata[3] = 0.; rdata[4] = 1.; rdata[5] = 0.;
		rdata[6] = 0.; rdata[7] = 360.;
		maxp = 8;
		break;
/*
........Extruded
*/
	case LW_STOCK_SWEEP:
		if (*nparm < 4) goto failed;
		ul_ipv_mach_parse_rec(lbuf,nc,parms,nparm,3);
		status = S_load_profile(parms[3],&plist,&npts,units);
		if (status != UU_SUCCESS) goto failed;
		rdata[0] = 0.; rdata[1] = 0.;
		status = ul_to_reals(&rdata[2],&nc,1,parms[2]);
		if (status != UU_SUCCESS) goto failed;
		S_unit_conv(&rdata[2],1,units);
		maxp = 3;
		break;
/*
........STL
*/
	case LW_STOCK_FILE:
		if (*nparm < 3) goto failed;
		ul_ipv_mach_parse_rec(lbuf,nc,parms,nparm,2);
/*
...........Check for INCH or MM
*/
		solid.stock.units = Sunits;
		p = strrchr(parms[2],',');
		if (p != UU_NULL)
		{
			p++;
			if (!ul_compare_upper(p,"INCH"))
			{
				solid.stock.units = 0;
				p--; *p = '\0';
			}
			else if (!ul_compare_upper(p,"MM"))
			{
				solid.stock.units = 1;
				p--; *p = '\0';
			}
		}
		ul_build_full_fname(Sfulldir,parms[2],"",fullname);
/*
...........Determine if Binary or Ascii file
*/
		status = ul_ipv_stl_binary(&fd,fullname,&solid.stock.bin);
		if (status == UU_SUCCESS)
		{
			nc1 = strlen(fullname) + 1;
			solid.stock.data = (UU_REAL *)uu_malloc(sizeof(char)*nc1);
			if (solid.stock.data == UU_NULL) status = UU_FAILURE;
			else strcpy((char *)solid.stock.data,fullname);
		}
		if (status != UX_BAD_FILE && status != UX_NFOUND) LiFileClose(fd);
		break;
/*
........Stock File
*/
	case LW_STOCK_SESSION:
		if (*nparm < 3) goto failed;
		ul_ipv_mach_parse_rec(lbuf,nc,parms,nparm,2);
/*
...........Open the stock file
*/
		ul_build_full_fname(Sfulldir,parms[2],"",fullname);
		mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
		status = ux_file_inquire(UU_NULL,UU_NULL,fullname,UU_NULL,UU_NULL,
			&mode,&fstat,tnam,UX_NPRTERRS);
		if (status != UU_SUCCESS) goto failed;
		status = ux_fopen0(fullname,"r",&fd);
		if (status != UU_SUCCESS) goto failed;
/*
...........Load stock commands
*/
		ul_ipv_count_stock_cmds(fd,nstk);
		for (i=0;i<2;i++)
		{
			if (nstk[i] != 0)
			{
				sp[i] = (LW_stock_struc *)uu_malloc(sizeof(LW_stock_struc)*nstk[i]);
				if (sp[i] == UU_NULL) goto failed;
				for (j=0;j<nstk[i];j++) sp[i][j] = solid.stock;
			}
		}
		status = ul_ipv_load_stock_cmds(fd,UU_FALSE,0,sp,nstk,UU_FALSE);
		ux_fclose0(fd);
		if (status != UU_SUCCESS) goto failed;
/*
...........Allocate memory for solids
*/
		*nsol = nstk[0] + nstk[1];
		if (Snsol < *nsol || *sols == UU_NULL)
		{
			nc1 = *nsol;
			if (*sols != UU_NULL) uu_free(*sols);
			*sols =
				(LW_mach_solid_struc *)uu_malloc(sizeof(LW_mach_solid_struc)*nc1);
			if (*sols == UU_NULL) goto failed;
			Ssptr = *sols;
			Snsol = *nsol;
		}
/*
...........Define the solids
*/
		n = 0;
		for (k=0;k<2;k++)
		{
			for (j=0;j<nstk[k];j++)
			{
				Ssptr[n].stock = sp[k][j];
				sprintf(Ssptr[n].name,"%s_%d",parms[0],n+1);
				n++;
			}
		}
		if (nstk[0] != 0) uu_free(sp[0]);
		if (nstk[1] != 0) uu_free(sp[1]);
		maxp = 0;
		npts = 0;
		break;
	}
/*
.....Store the solid data in the machine solid
*/
	if (solid.stock.type != LW_STOCK_SESSION &&
		solid.stock.type != LW_STOCK_FILE)
	{
		if (npts == 0)
			solid.stock.data = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*maxp);
		else
			solid.stock.data =
				(UU_REAL *)uu_malloc(sizeof(UU_REAL)*(maxp+1) +
					sizeof(UM_coord)*npts);
		if (solid.stock.data == UU_NULL) goto failed;
		for (i=0;i<maxp;i++) solid.stock.data[i] = rdata[i];
		if (npts != 0)
		{
			pts = (UM_coord *)UU_LIST_ARRAY(&plist);
			opt = (UM_coord *)&(solid.stock.data[maxp+1]);
			solid.stock.data[maxp] = npts;
			for (i=0;i<npts;i++) um_vctovc(pts[i],opt[i]);
			uu_list_free(&plist);
		}
	}
	if (solid.stock.type != LW_STOCK_SESSION) Ssptr[0] = solid;
	goto done;
/*
.....Failed to create solid
*/
failed:;
	status = UU_FAILURE;
	if (solid.stock.data != UU_NULL) uu_free(solid.stock.data);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**	 S_FUNCTION : S_load_profile(fname,pts,npt,units)
**			This function loads a profile text file (*.txt).
**	 PARAMETERS	
**		 INPUT  :
**         fname   = Name of file to load.
**		 OUTPUT :
**         plist   = List of profile points from text file.
**         npt     = Number of points in 'plist'.
**         units   = 0 = Inch, 1 = MM.
**	 RETURNS:
**         UU_SUCCESS when all went well.
**	 SIDE EFFECTS: 
**         Initializes the point list, which must be freed by the
**         calling routine.
**	 WARNINGS:
*********************************************************************/
static int S_load_profile(fname,plist,npt,units)
UX_pathname fname;
UU_LIST *plist;
int *npt;
{
	int status,stat,nc,nparm,ipt,n,inc,i,irec;
	UU_LOGICAL lfl;
	char lbuf[256],parms[50][80];
	UU_REAL spt[9],lpt[3],um_dcccc();
	UM_coord *ptx;
	UX_pathname fullname;
	UU_LIST tlist;
	FILE *fd=UU_NULL;
	static char *prim[]={"Line","Arc"};
/*
.....Initialize routine
*/
	*npt = 0;
	lpt[0] = lpt[1] = lpt[2] = 0.;
	uu_list_init(plist,sizeof(UM_coord),50,20);
	lfl = UU_FALSE;
	if (strcmp(fname,"internal.internal") == 0) lfl = UU_TRUE;
	irec = 0;
/*
.....Open the profile
*/
	status = UU_SUCCESS;
	if (!lfl)
	{
		ul_build_full_fname(Sfulldir,fname,"",fullname);
		fd = LiFileOpen(fullname,LI_FILE_READ);
		if (fd == UU_NULL) status = UU_FAILURE;
	}
/*
.....Loop to read the file
*/
	if (status == UU_SUCCESS)
	{
		do
		{
			if (!lfl)
			{
				stat = ul_fread(fd,lbuf,sizeof(lbuf),&nc);
				if (stat == UX_EOF) break;
				status = stat;
				if (status != UU_SUCCESS) break;
			}
			else
			{
				if (strcmp(Sinternal[irec],"EOF") == 0) break;
				strcpy(lbuf,Sinternal[irec]);
				nc = strlen(lbuf);
				irec++;
			}
/*
.....Parse the input block
*/
			status = ul_ipv_mach_parse_rec(lbuf,nc,parms,&nparm,0);
			if (status != UU_SUCCESS) break;
			if (nparm == 0) continue;
/*
.....Determine the Parameter type
*/
			for (inc=0;inc<2;inc++)
				if (!ul_compare_upper(prim[inc],parms[0])) break;
			switch (inc)
			{
/*
.....Line
*/
			case 0:
				if (nparm != 7)
				{
					status = UU_FAILURE;
					break;
				}
				ipt = 1;
				for (i=0;i<6;i++)
				{
					status = ul_to_reals(&(spt[i]),&nc,1,parms[ipt++]);
					if (status != UU_SUCCESS) break;
					S_unit_conv(&spt[i],1,units);
				}
				if (status != UU_SUCCESS) break;
				if (*npt == 0 || um_dcccc(spt,lpt) > UM_FUZZ)
				{
					uu_list_push(plist,&spt[0]); *npt = *npt + 1;
				}
				uu_list_push(plist,&spt[3]); *npt = *npt + 1;
				um_vctovc(&spt[3],lpt);
				break;
/*
.....Arc
*/
			case 1:
				if (nparm != 11)
				{
					status = UU_FAILURE;
					break;
				}
				ipt = 1;
				for (i=0;i<9;i++)
				{
					status = ul_to_reals(&(spt[i]),&nc,1,parms[ipt++]);
					if (status != UU_SUCCESS) break;
					S_unit_conv(&spt[i],1,units);
				}
/*
........Generate points around circle
*/
				if (status != UU_SUCCESS) break;
				if (!ul_compare_upper(parms[10],"CLW")) i = 0;
				else if (!ul_compare_upper(parms[10],"CCLW")) i = 1;
				else status = UU_FAILURE;
				if (status != UU_SUCCESS) break;
				status = ul_ipv_circle_pts(&spt[6],&spt[0],&spt[3],i,&tlist,&n);
				if (status != UU_SUCCESS) break;
/*
........Store points in list
*/
				ptx = (UM_coord *)UU_LIST_ARRAY(&tlist);
				if (*npt == 0 || um_dcccc(&spt[0],lpt) > UM_FUZZ) i = 0;
				else i = 1;
				for (i=i;i<n;i++)
				{
					if (i == 0) uu_list_push(plist,&spt[0]);
					else if (i == n-1) uu_list_push(plist,&spt[3]);
					else uu_list_push(plist,ptx[i]);
					*npt = *npt + 1;
				}
				um_vctovc(&spt[3],lpt);
				uu_list_free(&tlist);
				break;
/*
.....Unrecognized parameter
*/
			default:
				status = UU_FAILURE;
				break;
			}
		} while (status == UU_SUCCESS);
	}
/*
.....End of routine
*/
	if (fd != UU_NULL) LiFileClose(fd);
	return(status);
}

/*********************************************************************
**	 I_FUNCTION : S_open_err(fname)
**			This function outputs an Open Error style message.
**	 PARAMETERS	
**		 INPUT  :
**         fname   = Filename in error.
**		 OUTPUT :
**         none
**	 RETURNS:
**         none
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static void S_open_err(fname)
UX_pathname fname;
{
	UX_pathname errmsg,dir,lfile;
/*
.....Output error message with filename only
*/
		ul_break_fname(fname,dir,lfile);
		sprintf(errmsg,"Could not open %s.",lfile);
		ud_wrerr(errmsg);
}

/*********************************************************************
**	 I_FUNCTION : S_read_err(fname)
**			This function outputs a Read Error style message.
**	 PARAMETERS	
**		 INPUT  :
**         fname   = Filename in error.
**		 OUTPUT :
**         none
**	 RETURNS:
**         none
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static void S_read_err(fname)
UX_pathname fname;
{
	UX_pathname errmsg,dir,lfile;
/*
.....Output error message with filename only
*/
	ul_break_fname(fname,dir,lfile);
	sprintf(errmsg,"Error reading file %s.",lfile);
	ud_wrerr(errmsg);
}

/*********************************************************************
**	 I_FUNCTION : S_parse_err(lbuf)
**			This function outputs a Parsing Error style message.
**	 PARAMETERS	
**		 INPUT  :
**         lbuf    = Text that could not be parsed.
**		 OUTPUT :
**         none
**	 RETURNS:
**         none
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static void S_parse_err(lbuf)
char *lbuf;
{
	char errmsg[160];
/*
.....Output error message
*/
	sprintf(errmsg,"Error trying to parse machine data record.\n%s",
			lbuf);
	ud_wrerr(errmsg);
}

/*********************************************************************
**	 I_FUNCTION : S_unit_conv(rnum,npt,unit)
**			This function converts metric machine parameters to inches.
**	 PARAMETERS	
**		 INPUT  :
**         rnum   = Input real value array.
**         npt    = Number of points in 'rnum'.
**         unit   = 1 = Convert from MM to Inches.
**		 OUTPUT :
**         rnum   = Converted real values.
**	 RETURNS: None
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static void S_unit_conv(rnum,npt,unit)
UU_REAL rnum[];
int npt,unit;
{
	int i;
/*
.....Convert to inches
*/
	if (unit == 1)
	{
		for (i=0;i<npt;i++) rnum[i] = rnum[i] / 25.4;
	}
}

/*********************************************************************
**	 I_FUNCTION : S_parse_reals(parms,nparm,maxp,rdata)
**			Parses real values from a machine description command.
**	 PARAMETERS	
**		 INPUT  :
**         parms  = Input parameter strings.
**         nparm  = Number of parameters in 'parms'.
**         maxp   = Expected number of reals to parse.
**		 OUTPUT :
**         rdata  = Output real values.
**	 RETURNS: None
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static int S_parse_reals(parms,nparm,maxp,rdata)
char parms[][80];
int nparm,maxp;
UU_REAL *rdata;
{
	int i,status,nc;
	status = UU_SUCCESS;
	if (nparm == maxp)
	{
		for (i=0;i<maxp;i++)
		{
			status = ul_to_reals(&rdata[i],&nc,1,parms[i]);
			if (status != UU_SUCCESS) break;
		}
	}
	else
	{
		status = UU_FAILURE;
	}
/*
.....End of routine
*/
	return(status);
}

/*********************************************************************
**	 I_FUNCTION : S_match_assem(name1,nc,name2)
**			Compares an assembly name to a name provided on a commmand.
**       It performs an exact match and a base match up to the '_'
**       character.
**	 PARAMETERS	
**		 INPUT  :
**         name1  = Name provided on command (Translate, Rotate, etc.).
**         nc     = Number of characters in 'name1'.
**         name2  = Name of solid within assembly.
**		 OUTPUT : none
**	 RETURNS: UU_TRUE if names match.
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static UU_LOGICAL S_match_assem(name1,nc,name2)
char *name1;
int nc;
char *name2;
{
	char ctemp[80];
/*
.....Determine if exact match or
.....match up to '_' character
*/
	strcpy(ctemp,name2);
	if (ctemp[nc] == '_') ctemp[nc] = '\0';
	if (!ul_compare_upper(name1,ctemp))
		return(UU_TRUE);
	else
		return(UU_FALSE);
}
