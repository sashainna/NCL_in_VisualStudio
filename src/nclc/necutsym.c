/*********************************************************************
**	 NAME:  necutsym.c
**		 CONTAINS:
**			ldcsym
**			ncl_get_shade_symbol
**			ncl_move_cutsym
**       nclf_open_cutprof
**			ncl_open_cutprof
**			nclf_load_cutprof
**			ncl_load_cutprof
**			ncl_get_cutprof
**			ncl_next_cutprof
**			ncl_cutsym_tf
**       ncl_getcut_profnam
**
**	 COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       necutsym.c , 25.4
**    DATE AND TIME OF LAST MODIFICATION
**       12/01/15 , 08:26:56
*********************************************************************/

#include "usysdef.h"
#include "zsysdep.h"
#include "class.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdcpln.h"
#include "mdcoord.h"
#include "msol.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "bsym.h"
#include "gsegac.h"
#include "gomisc.h"
#include "gtbl.h"
#include "g.h"
#include "ginq.h"
#include "gdidd.h"
#include "gviw.h"
#include "gsegop.h"
#include "gmat4.h"
#include "gconvert.h"
#include "mdcoord.h"
#include "mfort.h"
#include "modef.h"
#include "mplot.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nclvx.h"
#include "nccs.h"

#define DTOR (UM_PI/180.0)

struct S_tools_struc
{
	char label[21];
	char clas[21];
	UU_LIST pts;
	double length;
};

static int NCL_shade_symbol = 0;

void ncl_cutsym_tf();
void ncl_store_cutgeo();

static UU_LOGICAL S_lib_loaded=UU_FALSE;
extern UU_LIST NCL_plib_tools;

static char Sfilename[81];

/*********************************************************************
**	 E_FUNCTION : int ldcsym(cutsyb,ishadfl,ictype,ierr) 
**			Fortran interface to the 'ncl_get_cutsym' routine.
**	 PARAMETERS	
**		 INPUT  :
**			sym     = Name of symbol to load.
**			ishadfl = >0 means cutter is shaded.
**			ictype  = 1 = Mill cutter, 2 = Lathe cutter.
**		 OUTPUT :
**			ierr    = 0 = Symbol loaded OK.  1 = Failed trying to load.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ldcsym(cutsyb,ishadfl,ictype,ierr)
UM_int2 *ierr;
UM_int4 *ishadfl,*ictype;
UM_f77_str_ptr cutsyb;
{
	char sym[MAXSYMLEN],*p;
	int i;
	UN_motseg_symgeo cutsym;
/*
.....Convert Fortran Character string
.....to C string
*/
	p = UM_cstr_of_f77_str(cutsyb);
	i = MAXSYMLEN;
	strncpy(sym,p,i);
	ul_strip_blanks(sym,&i);
/*
.....Try and load the cutter symbol
*/
	*ierr = 0;
	strcpy(cutsym.symbol,sym);
	cutsym.shaded = *ishadfl;
	if (ncl_get_cutsym(&cutsym,0,*ictype) != UU_SUCCESS) *ierr = 1;
	return;
}

/*********************************************************************
**	 E_FUNCTION : ncl_get_shade_symbol (sh_symbol)
*********************************************************************/
void ncl_get_shade_symbol (sh_symbol)
int *sh_symbol;
{
	*sh_symbol = NCL_shade_symbol;
}

/*********************************************************************
**    E_FUNCTION     : nclf_open_cutprof(filename,errfl)
*********************************************************************/
void nclf_open_cutprof(filename,errfl)
UM_f77_str_ptr filename;
int *errfl;
{
	int i,inval,len,nc = 80;
	char *p,*dot,*rindex(),file_name[81];

	*errfl = 0;
	p = UM_cstr_of_f77_str(filename);
	strncpy(file_name,p,nc);
	i = 79;
	while (file_name[i] == 32) i--;
	file_name[i+1] = '\0';
	dot = rindex(file_name,'.');
	if (dot == UU_NULL)
		strcat(file_name,".lib");

	*errfl = ncl_open_cutprof(&file_name,&inval);
	if (*errfl == 0) 
		strcpy(Sfilename,file_name);
}

/*********************************************************************
**	 E_FUNCTION : int ncl_open_cutprof(infile, nprofs)
**       Loads the external tool description file if it has not already
**       been loaded.
**	 PARAMETERS	
**		 INPUT  :
**       infile  =  profile name to be loaded.  If filename=NULL, using
**                  the current loaded profile or load the default
**                  NCL_TOOL_DESC file.
**		 OUTPUT :
**			nprofs  = Number of profiles defined in file.
**
**	 RETURNS: UU_SUCCESS of no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ncl_open_cutprof(infile, nprofs)
int *nprofs;
char *infile;
{
	int mode,status,stat;
	char *p,lib[UX_MAX_PATH_LEN+1],direc[UX_MAX_PATH_LEN+1];
	char filename[UX_MAX_PATH_LEN+1],libname[UX_MAX_PATH_LEN+1];
	char *ux_getenv();
	UU_LOGICAL idid,ifree;
	UU_REAL cnv;
	FILE *fptr = NULL;
/*
.....Initialize routine
*/
	*nprofs = 0;
	cnv = 1.;
	status = UU_FAILURE;
	ifree = UU_FALSE;
	idid = UU_FALSE;
/*
.....Load the tool descriptions if necessary
*/
	if (!S_lib_loaded || infile != UU_NULL)
	{
/*
........Get the library name
*/
		if (infile == UU_NULL || infile[0] == '\0')
		{
			p = ux_getenv("NCL_TOOL_DESC");
			if (p == UU_NULL) goto done;
			strcpy(filename, p);
		}
		else
			strcpy(filename,infile);
/*
......if the filename already have a path, use it.
......Otherwise, use the current directory first (not "HOMEDIR")
......then the system directory
*/
		ul_break_fname(filename, direc, lib);
		if (lib[0]=='\0') goto done;
/*
........Try the current directory first
*/
/*		ul_get_full_dir("HOMEDIR",direc);
		mode = 0;
		stat = ux_mk_chk_syspath(UU_NULL , direc, p,
			UU_NULL, UU_NULL, &mode, lib, UX_PRTERRS); */
		mode = 0;
		stat = ux_mk_chk_syspath(UU_NULL , direc, lib,
			UU_NULL, UU_NULL, &mode, libname, UX_PRTERRS);
		if (mode & UX_NEXISTS)
		{
/*
........Then the current directory
*/
			mode = 0;
			stat = ux_mk_chk_syspath(UU_NULL , UU_NULL, lib,
				UU_NULL, UU_NULL, &mode, libname, UX_PRTERRS);
/*
........Then the system directory
*/
			if (mode & UX_NEXISTS)
			{
/*
				ul_get_full_dir("NCL_TOOL", direc);
				if (direc[0]=='\0') goto done;
				mode = 0;
				stat = ux_mk_chk_syspath(UU_NULL , direc, lib,
					UU_NULL, UU_NULL, &mode, libname, UX_PRTERRS);
				if (mode & UX_NEXISTS) goto done;
*/
				status = ul_open_mod_file2("NCL_TOOL", NULL, lib, 0,  &fptr, UU_NULL, UU_NULL);
				if (status==-1)
					libname[0] = '\0';
				else
					strcpy(libname, lib);
			}
		}
/*
.....Open the file
*/
		ul_remove_quotes(libname);
		status = ncl_opend_cutprof(libname, 2); 
		if (UU_LIST_LENGTH(&NCL_plib_tools) == 0) goto done;
		status = UU_SUCCESS;
		S_lib_loaded = UU_TRUE;
	}
	else status = UU_SUCCESS;
/*
.....Return number of defined profiles
*/
	*nprofs = UU_LIST_LENGTH(&NCL_plib_tools);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**	 E_FUNCTION : int nclf_load_cutprof(cutsyb,ierr) 
**			Fortran interface to the 'ncl_load_cutprof' routine.
**	 PARAMETERS	
**		 INPUT  :
**			cutsyb  = Name of cutter profile to load.
**		 OUTPUT :
**			ierr    = 0 = Profile loaded OK.  1 = Failed trying to load.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void nclf_load_cutprof(cutsyb,ierr)
UM_int2 *ierr;
UM_f77_str_ptr cutsyb;
{
	int i,npt;
	char sym[81],*p;
	UM_coord *pts;
	double length;
/*
.....Convert Fortran Character string
.....to C string
*/
	p = UM_cstr_of_f77_str(cutsyb);
	i = 80;
	strncpy(sym,p,i);
	ul_strip_blanks(sym,&i);
/*
.....Try and load the cutter symbol
*/
	*ierr = 0;
	if (ncl_load_cutprof(sym,&pts,&npt, &length) != UU_SUCCESS) *ierr = 1;
	return;
}

/*********************************************************************
**	 E_FUNCTION : int ncl_load_cutprof(sym,opts,npts)
**       Finds the requested cutter profile from those that were loaded
**       from an external cutter description file.
**	 PARAMETERS	
**		 INPUT  :
**			sym     = Name of symbol to load.
**		 OUTPUT :
**			opts    = Pointer to cutter profile point array.
**			npts    = Number of points in 'opts'.
**
**	 RETURNS: UU_SUCCESS of no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS:
**       Loads the external tool description file if it has not already
**       been loaded.
**	 WARNINGS: none
*********************************************************************/
int ncl_load_cutprof(sym,opts,npts,length)
char *sym;
UM_coord **opts;
int *npts;
double *length;
{
	int status,npt,i;
	char csym[41],ctool[41];
	struct S_tools_struc *ptool;
/*
.....Initialize routine
*/
	status = UU_FAILURE;
/*
.....Load the cutter profile file
*/
	ncl_open_cutprof(UU_NULL, &npt);
/*
.....Find the requested tool
*/
	ptool = (struct S_tools_struc *)UU_LIST_ARRAY(&NCL_plib_tools);
	strcpy(csym,sym); ul_to_upper(csym);
	*length = 0;
	for (i=0;i<npt;i++)
	{
		strcpy(ctool,ptool[i].label); ul_to_upper(ctool);
		if (strcmp(csym,ctool) == 0)
		{
			*opts = (UM_coord *)UU_LIST_ARRAY(&ptool[i].pts);
			*npts = UU_LIST_LENGTH(&ptool[i].pts);
			*length = ptool[i].length;
			status = UU_SUCCESS;
			break;
		}
	}
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**	 E_FUNCTION : int ncl_get_cutprof(cutsym,type)
**			This function retrieves the requested cutter profile from the
**       tool description file, calculates the normals, and stores the
**       points and normals in the curve list.
**	 PARAMETERS	
**		 INPUT  :
**			cutsym->symbol  = Label of profile to load.
**			type            = 1 = Mill cutter, 2 = Lathe cutter.
**		 OUTPUT :
**			cutsym  = Symbol structure to receive point list.
**	 RETURNS: UU_SUCCESS of no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ncl_get_cutprof(cutsym,type)
UN_motseg_symgeo *cutsym;
int type;
{
	int i,status,npts,stat;
	UM_coord *pts;
	UM_vector *vcs,vctmp,vcn,vc1;
	UU_LIST *cvlist,*cnlist;
	double length;
/*
.....Initialize routine
*/
	status = UU_FAILURE;
	cvlist = &cutsym->curve;
	cnlist = &cutsym->cnorm;
	vcn[0] = 0.;
	vcn[1] = 0.;
	vcn[2] = 1.;
/*
.....Load the profile
*/
	stat = ncl_load_cutprof(cutsym->symbol,&pts,&npts, &length);
	if (stat != UU_SUCCESS) goto done;
/*
.....Calculate normal vectors
*/
	vcs = (UM_vector *)uu_malloc(sizeof(UM_vector)*npts);
	if (vcs == UU_NULL) goto done;
	for (i=0;i<npts;i++)
	{
		if (i == npts-1)
		{
			um_vctovc(vctmp,vcs[i]);
		}
		else
		{
			um_vcmnvc(pts[i+1],pts[i],vc1);
			um_unitvc(vc1,vc1);
			if (um_mag(vc1) < UM_FUZZ || i == npts-2) um_vctovc(vctmp,vcs[i]);
			else
			{
				um_cross(vc1,vcn,vctmp);
				um_unitvc(vctmp,vctmp);
				um_vctovc(vctmp,vcs[i]);
			}
		}
	}
/*
.....Store points
*/
	ncl_store_cutgeo(cutsym,pts,vcs,npts,type);
	uu_free(vcs);
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:
	return(status);
}

/*********************************************************************
**	 E_FUNCTION : int ncl_cutsym_tf(cpt,taxis,tf,yrot,zrot,itsk)
**			This function calculates the transformation matrix required
**			to convert the current tool axis system to the 1,0,0 system
**			required for moving symbol/full cutters.
**	 PARAMETERS	
**		 INPUT  :
**			cpt     = Tool end point.
**			taxis   = Tool axis.
**			itsk    = 0 returns 0,0,1-to-Tool axis matrix.
**			          1 returns Tool axis-to-0,0,1 matrix.
**		 OUTPUT :
**			tf      = Transformation matrix.
**			yrot    = Euler Y-axis rotation angle in degrees.
**			zrot    = Euler Z-axis rotation angle in degrees.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ncl_cutsym_tf(cpt,taxis,tf,yrot,zrot,itsk)
UM_coord cpt;
UM_vector taxis;
UM_transf tf;
UU_REAL *yrot,*zrot;
int itsk;
{
	UU_REAL rnum;
	UM_vector tvec,vc1,xvec,yvec,zvec;
	UM_transf tfr1,tfr2;
/*
.....Store standard vectors
*/
	xvec[0] = 1.; xvec[1] = 0.; xvec[2] = 0.;
	yvec[0] = 0.; yvec[1] = 1.; yvec[2] = 0.;
	zvec[0] = 0.; zvec[1] = 0.; zvec[2] = 1.;
/*
.....Adjust for pure Y-axis vector
*/
	if (fabs(taxis[1]) > 1. - UM_FUZZ)
	{
		*yrot = 0;
		*zrot = UM_HALFPI;
		if (taxis[1] < 0) *zrot = -UM_HALFPI;
	}
/*
.....Calculate Y-axis rotation
*/
	else
	{
		tvec[0] = taxis[0]; tvec[1] = 0.; tvec[2] = taxis[2];
		rnum = um_mag(tvec);
		rnum = 1./rnum;
		um_vctmsc(tvec,rnum,tvec);
		*yrot = um_angle2p(xvec,tvec,yvec);
/*
.....Calculate Z-axis rotation
*/
		vc1[0] = -taxis[2]; vc1[1] = 0.; vc1[2] = taxis[0];
		um_vctmsc(vc1,rnum,vc1);
		*zrot = um_angle2p(tvec,taxis,vc1);
	}
/*
.....Merge matrices
*/
	if (itsk == 0)
	{
		um_rotlntf(cpt,yvec,*yrot,tfr1);
		um_rotlntf(cpt,zvec,*zrot,tfr2);
		um_tftmtf(tfr2,tfr1,tf);
	}
	else
	{
		*yrot = *yrot * -1.;
		*zrot = *zrot * -1.;
		um_rotlntf(cpt,yvec,*yrot,tfr1);
		um_rotlntf(cpt,zvec,*zrot,tfr2);
		um_tftmtf(tfr1,tfr2,tf);
	}
/*
.....Convert angles to degrees
*/
	*yrot = *yrot * (180./UM_PI);
	*zrot = *zrot * (180./UM_PI);
}

/*********************************************************************
**	 I_FUNCTION : ncl_store_cutgeo(cutsym,pts,vcs,npts,type)
**			This function stores the provided points and vectors in a
**       cutter symbol structure.
**	 PARAMETERS	
**		 INPUT  :
**		 	pts     = Points to store in list.
**		 	vcs     = Vectors to store in list.
**			npts    = Number of points to store.
**			type    = 1 = Mill cutter, 2 = Lathe cutter.
**		 OUTPUT :
**			cutsym  = Symbol structure to receive point list.
**	 RETURNS: UU_SUCCESS of no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ncl_store_cutgeo(cutsym,pts,vcs,npts,type)
UN_motseg_symgeo *cutsym;
UM_coord *pts;
UM_vector *vcs;
int npts,type;
{
	int i,inc;
	UM_coord pttmp,origpt,*ptx;
	UM_vector vctmp,vcn,*vcx;
	UU_LIST *cvlist,*cnlist;
/*
.....Initialize routine
*/
	cvlist = &cutsym->curve;
	cnlist = &cutsym->cnorm;
	origpt[0] = origpt[1] = origpt[2] = 0.;
	vcn[0] = vcn[1] = 0.; vcn[2] = 1.;
/*
.....Store points
*/
	inc = 0;
	for (i=0;i<npts;i++)
	{
		if (type == 1)
		{
			pttmp[0] = pts[i][0];
			pttmp[1] = pts[i][2];
			pttmp[2] = pts[i][1];
			vctmp[0] = vcs[i][0];
			vctmp[1] = vcs[i][2];
			vctmp[2] = vcs[i][1];
		}
		else
		{
			if (i != 0 && um_cceqcc(pts[i],pts[i-1])) continue;
			pttmp[0] = pts[i][0];
			pttmp[1] = pts[i][1];
			pttmp[2] = pts[i][2];
			if (pttmp[0] > origpt[0]) origpt[0] = pttmp[0];
			if (pttmp[1] > origpt[1]) origpt[1] = pttmp[1];
			um_vctovc(vcs[i],vctmp);
		}
		uu_list_push(cvlist,pttmp);
		uu_list_push(cnlist,vctmp);
		inc++;
	}
/*
.....Store attach point for composite curve (Lathe)
*/
	if (type == 2)
	{
		ptx = (UM_coord *)UU_LIST_ARRAY(cvlist);
		vcx = (UM_coord *)UU_LIST_ARRAY(cnlist);
		if (um_dcccc(ptx[0],ptx[inc-1]) > UM_FUZZ)
		{
			uu_list_push(cvlist,ptx[0]);
			ptx = (UM_coord *)UU_LIST_ARRAY(cvlist);
			uu_list_push(cnlist,vcx[inc-1]);
			vcx = (UM_coord *)UU_LIST_ARRAY(cnlist);
			inc++;
		}
		ul_ipv_stock_dir(ptx,inc);
		if (origpt[0] > origpt[1])
		{
			cutsym->axis[0] = 1.; cutsym->axis[1] = 0.; cutsym->axis[2] = 0.;
			origpt[1] = 0.;
		}
		else
		{
			cutsym->axis[0] = 0.; cutsym->axis[1] = 1.; cutsym->axis[2] = 0.;
			origpt[0] = 0.;
		}
		uu_list_insert(cvlist,0,origpt);
		origpt[0] = 0.; origpt[1] = 0.; origpt[2] = 1.;
		uu_list_insert(cnlist,0,origpt);
	}
}

/*********************************************************************
**    E_FUNCTION     : nclf_getcut_profnam(outnam)
**       Get the default profile library name.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          outnam - filename for default profile library
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_getcut_profnam(outnam)
char *outnam;
{
	strcpy(outnam,Sfilename);
}
