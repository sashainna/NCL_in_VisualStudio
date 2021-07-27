/*********************************************************************
**	FILENAME: nusolid.c
**	CONTAINS:
**				nclu_solid_box()
**				nclu_solid_cone()
**				nclu_solid_cyl()
**				nclu_solid_sphere()
**				nclu_solid_torus()
**				nclu_solid_sweep()
**				nclu_solid_revolve()
**				nclu_solid_load()
**				nclu_solid_load_stl()
**				nclu_solid_sf_out()
**     MODULE NAME AND RELEASE LEVEL 
**       nusolid.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:16
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "mfort.h"
#include "nclfc.h"
#include <ctype.h>
#include <math.h>
#include "gtbl.h"
#include "gobas.h"
#include "zsysdep.h"
#include "uhep.h"
#include "dselmask.h"
#include "mdgenent.h"
#include "mdpick.h"
#include "modef.h"
#include "ulist.h"
#include "m2dattr.h"
#include "mdcpln.h"
#include "mcrv.h"
#include "mdunits.h"
#include "msol.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nclmodals.h"
#include "nconst.h"
#include "nkeywd.h"
#include "mdrel.h"
#include "udfconst.h"
#include "udforms.h"
#include "vsegbf.h"

#define MXLAB NCL_MAX_LABEL_AND_SUBSCRIPT+1
/*
.....Load solid fields
*/
#define FLFI 0
#define FLFN 1
#define FLSC 2
#define FLXF 3
#define FLMX 4
#define FLRO 5
#define FLDG 6
#define FLAB 7
#define FLPO 8

static int Sxform,Srot,Sunits=-1,Sftyp,Sopt=1;
static UU_REAL Sdeg;
static char Sload[UX_MAX_PATH_LEN],Sscalar[MXLAB],Smatrix[MXLAB],Slabel[MXLAB];

static UD_FSTAT OnApply(),OnBrowse(),OnToggle();
static int S_build_loadcmd();

#define SINGLE 1
#define ALL 2
#define THRU 3

/*********************************************************************
**   E_FUNCTION: nclu_solid_box(ifl)
**      This function defines a Visula Solid in the form of a box.
**   PARAMETERS
**       INPUT  :
**				ifl = 1 - Box PT PT
**				      2 - Box PT L W H
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void nclu_solid_box(ifl)
int ifl;
{
	int status,numint;
	UU_LOGICAL zfl;
	char str1[80];
	NCL_cmdbuf cmdbuf;
/*
.....Loop until done
*/
	while (UU_TRUE)
	{
/*
.....Create box using
.....opposite corners of the box
*/
		if (ifl == 1)
		{
/*
........Get minimum and maximum values
........if coordinates of selected points are different
*/
			zfl = UU_FALSE;
			status = ud_ldas(UD_DASSTRING,UA_NCL,706,str1,80,&numint,UD_NODEFAULT);
			if (numint != 0) zfl = UU_TRUE;
/*
........Get the opposite corners of the box
*/
			while (UU_TRUE)
			{
				S_init_cmd(&cmdbuf,NCL_box);
				status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,674,UD_ncl_ptpv);
				if (status != NCL_OKINPUT) break;

				status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,675,UD_ncl_ptpv);
				if (status != NCL_OKINPUT) break;
/*
........Process the command
*/
				if (status == NCL_OKINPUT)
				{
					if (zfl) ncl_add_token(&cmdbuf,str1,NCL_comma);
					ncl_set_cmdmode(UU_TRUE);
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
				}
			}
		}
/*
.....Create box using
.....center, len, wid, and hgt
*/
		else if (ifl == 2)
		{
/*
........Get LWH of box
*/
			status = ud_ldas(UD_DASSTRING,UA_NCL,678,str1,80,&numint,UD_NODEFAULT);
			if (numint == 0) break;
/*
........Get the center of the box
*/
			while (UU_TRUE)
			{
				S_init_cmd(&cmdbuf,NCL_box);
				status = ncl_add_token(&cmdbuf,NCL_center,NCL_comma);
				status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,677,UD_ncl_ptpv);
				if (status != NCL_OKINPUT) break;
/*
........Process the command
*/
				status = ncl_add_token(&cmdbuf,str1,NCL_nocomma);
				if (status == NCL_OKINPUT)
				{
					ncl_set_cmdmode(UU_TRUE);
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
				}
			}
		}
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION: nclu_solid_cone(ifl)
**      This function defines a Visula Solid in the form of a cone.
**   PARAMETERS
**       INPUT  :
**				ifl = 1 - Cone PT PT RA RA
**				      2 - Cone PT HGT RA RA
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void nclu_solid_cone(ifl)
int ifl;
{
	int status,relnum,numint;
	char str1[80],str2[80];
	NCL_cmdbuf cmdbuf;
/*
.....Loop until done
*/
	while (UU_TRUE)
	{
/*
.....Create cone using
.....two points and radii
*/
		if (ifl == 1)
		{
/*
........Get radii of cone
*/
			status = ud_ldas(UD_DASSTRING,UA_NCL,681,str1,80,&numint,UD_NODEFAULT);
			if (numint == 0) break;
/*
........Get the two points of the cone
*/
			while (UU_TRUE)
			{
				S_init_cmd(&cmdbuf,NCL_cone);
				status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,679,UD_ncl_ptpv);
				if (status != NCL_OKINPUT) break;

				status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,680,UD_ncl_ptpv);
				if (status != NCL_OKINPUT) break;
/*
........Add radii of cone
*/
				status = ncl_add_token(&cmdbuf,NCL_radius,NCL_comma);
				status = ncl_add_token(&cmdbuf,str1,NCL_comma);
/*
........Process the command
*/
				if (status == NCL_OKINPUT)
				{
					ncl_set_cmdmode(UU_TRUE);
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
				}
			}
		}
/*
.....Create cone using
.....center, vector, height, and radii
*/
		else if (ifl == 2)
		{
/*
........Get radii of cone
*/
			status = ud_ldas(UD_DASSTRING,UA_NCL,681,str1,80,&numint,UD_NODEFAULT);
			if (numint == 0) break;
/*
........Get height of cone
*/
			status = ud_ldas(UD_DASSTRING,UA_NCL,682,str2,80,&numint,UD_NODEFAULT);
			if (numint == 0) break;
/*
........Get the center and axis of the cone
*/
			while (UU_TRUE)
			{
				S_init_cmd(&cmdbuf,NCL_cone);
				ncl_add_token(&cmdbuf,NCL_center,NCL_comma);
				status = ncl_add_label_rel(UD_DASPCKLOC,&cmdbuf,683,UD_ncl_ptpv,
					&relnum);
				if (status != NCL_OKINPUT) break;

				if (relnum != NCL_POINTVEC_REL)
				{
					status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,684,UD_ncl_ve);
					if (status != NCL_OKINPUT) break;
				}
/*
........Add the height of the cone
*/
				ncl_add_token(&cmdbuf,str2,NCL_comma);
/*
........Add the radii of the cone
*/
				ncl_add_token(&cmdbuf,NCL_radius,NCL_comma);
				ncl_add_token(&cmdbuf,str1,NCL_comma);
/*
........Process the command
*/
				if (status == NCL_OKINPUT)
				{
					ncl_set_cmdmode(UU_TRUE);
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
				}
			}
		}
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION: nclu_solid_cyl(ifl)
**      This function defines a Visula Solid in the form of a cylinder.
**   PARAMETERS
**       INPUT  :
**				ifl = 1 - Cylinder CI RA
**                2 - Cylinder PT PT RA
**				      3 - Cylinder PT HGT RA
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void nclu_solid_cyl(ifl)
int ifl;
{
	int status,relnum,numint;
	char str1[80],str2[80];
	NCL_cmdbuf cmdbuf;
/*
.....Loop until done
*/
	while (UU_TRUE)
	{
/*
.....Create cylinder using
.....circle and height
*/
		if (ifl == 1)
		{
/*
........Get height of cylinder
*/
			status = ud_ldas(UD_DASSTRING,UA_NCL,686,str1,80,&numint,UD_NODEFAULT);
			if (numint == 0) break;
/*
........Get the circle defining the cylinder
*/
			while (UU_TRUE)
			{
				S_init_cmd(&cmdbuf,NCL_cylinder);
				status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,685,UD_ncl_ci);
				if (status != NCL_OKINPUT) break;
/*
........Store height of cylinder
*/
				ncl_add_token(&cmdbuf,str1,NCL_comma);
/*
........Process the command
*/
				if (status == NCL_OKINPUT)
				{
					ncl_set_cmdmode(UU_TRUE);
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
				}
			}
		}
/*
.....Create cylinder using
.....two points and radius
*/
		else if (ifl == 2)
		{
/*
........Get radius of cylinder
*/
			status = ud_ldas(UD_DASSTRING,UA_NCL,689,str1,80,&numint,UD_NODEFAULT);
			if (numint == 0) break;
/*
........Get the two points of the cylinder
*/
			while (UU_TRUE)
			{
				S_init_cmd(&cmdbuf,NCL_cylinder);
				status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,687,UD_ncl_ptpv);
				if (status != NCL_OKINPUT) break;

				status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,688,UD_ncl_ptpv);
				if (status != NCL_OKINPUT) break;
/*
........Add radius of cylinder
*/
				status = ncl_add_token(&cmdbuf,NCL_radius,NCL_comma);
				status = ncl_add_token(&cmdbuf,str1,NCL_comma);
/*
........Process the command
*/
				if (status == NCL_OKINPUT)
				{
					ncl_set_cmdmode(UU_TRUE);
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
				}
			}
		}
/*
.....Create cylinder using
.....center, vector, height, and radius
*/
		else if (ifl == 3)
		{
/*
........Get radius of cylinder
*/
			status = ud_ldas(UD_DASSTRING,UA_NCL,689,str1,80,&numint,UD_NODEFAULT);
			if (numint == 0) break;
/*
........Get height of cylinder
*/
			status = ud_ldas(UD_DASSTRING,UA_NCL,686,str2,80,&numint,UD_NODEFAULT);
			if (numint == 0) break;
/*
........Get the center and axis of the cylinder
*/
			while (UU_TRUE)
			{
				S_init_cmd(&cmdbuf,NCL_cylinder);
				ncl_add_token(&cmdbuf,NCL_center,NCL_comma);
				status = ncl_add_label_rel(UD_DASPCKLOC,&cmdbuf,690,UD_ncl_ptpv,
					&relnum);
				if (status != NCL_OKINPUT) break;

				if (relnum != NCL_POINTVEC_REL)
				{
					status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,691,UD_ncl_ve);
					if (status != NCL_OKINPUT) break;
				}
/*
........Add the height of the cylinder
*/
				ncl_add_token(&cmdbuf,str2,NCL_comma);
/*
........Add the radius of the cylinder
*/
				ncl_add_token(&cmdbuf,NCL_radius,NCL_comma);
				ncl_add_token(&cmdbuf,str1,NCL_comma);
/*
........Process the command
*/
				if (status == NCL_OKINPUT)
				{
					ncl_set_cmdmode(UU_TRUE);
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
				}
			}
		}
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION: nclu_solid_sphere(ifl)
**      This function defines a Visula Solid in the form of a sphere.
**   PARAMETERS
**       INPUT  :
**				ifl = 1 - Sphere CI
**                2 - Sphere PT RA
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void nclu_solid_sphere(ifl)
int ifl;
{
	int status,numint;
	char str1[80];
	NCL_cmdbuf cmdbuf;
/*
.....Loop until done
*/
	while (UU_TRUE)
	{
/*
.....Create sphere using circle
*/
		if (ifl == 1)
		{
/*
.....Get the circle defining the sphere
*/
			S_init_cmd(&cmdbuf,NCL_sphere);
			status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,692,UD_ncl_ci);
			if (status != NCL_OKINPUT) break;
/*
........Process the command
*/
			if (status == NCL_OKINPUT)
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
			}
		}
/*
.....Create sphere using
.....center and radius
*/
		else if (ifl == 2)
		{
/*
........Get radius of sphere
*/
			status = ud_ldas(UD_DASSTRING,UA_NCL,693,str1,80,&numint,UD_NODEFAULT);
			if (numint == 0) break;
/*
........Get the sphere center
*/
			while (UU_TRUE)
			{
				S_init_cmd(&cmdbuf,NCL_sphere);
				ncl_add_token(&cmdbuf,NCL_center,NCL_comma);
				status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,694,UD_ncl_ptpv);
				if (status != NCL_OKINPUT) break;
/*
........Add radius of sphere
*/
				status = ncl_add_token(&cmdbuf,NCL_radius,NCL_comma);
				status = ncl_add_token(&cmdbuf,str1,NCL_comma);
/*
........Process the command
*/
				if (status == NCL_OKINPUT)
				{
					ncl_set_cmdmode(UU_TRUE);
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
				}
			}
		}
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION: nclu_solid_torus(ifl)
**      This function defines a Visula Solid in the form of a torus.
**   PARAMETERS
**       INPUT  :
**				ifl = 1 - Torus CI RA
**                2 - Torus PT PT RA
**				      3 - Torus PT HGT RA
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void nclu_solid_torus(ifl)
int ifl;
{
	int status,relnum,numint;
	char str1[80],str2[80];
	NCL_cmdbuf cmdbuf;
/*
.....Loop until done
*/
	while (UU_TRUE)
	{
/*
.....Create torus using two circles
*/
		if (ifl == 1)
		{
/*
........Get the circle defining the inner radius
*/
			S_init_cmd(&cmdbuf,NCL_torus);
			status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,695,UD_ncl_ci);
			if (status != NCL_OKINPUT) break;
/*
........Get the circle defining the outer radius
*/
			status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,696,UD_ncl_ci);
			if (status != NCL_OKINPUT) break;
/*
........Process the command
*/
			if (status == NCL_OKINPUT)
			{
				ncl_set_cmdmode(UU_TRUE);
				ncl_add_cmdbuf(&cmdbuf);
				ncl_call(&cmdbuf);
			}
		}
/*
.....Create torus using
.....axial circle and radius
*/
		else if (ifl == 2)
		{
/*
........Get radius of torus
*/
			status = ud_ldas(UD_DASSTRING,UA_NCL,697,str1,80,&numint,UD_NODEFAULT);
			if (numint == 0) break;
/*
........Get the axial circle of the torus
*/
			while (UU_TRUE)
			{
				S_init_cmd(&cmdbuf,NCL_torus);
				status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,701,UD_ncl_ci);
				if (status != NCL_OKINPUT) break;
/*
........Add radius of torus
*/
				ncl_add_token(&cmdbuf,NCL_radius,NCL_comma);
				ncl_add_token(&cmdbuf,str1,NCL_comma);
/*
........Process the command
*/
				if (status == NCL_OKINPUT)
				{
					ncl_set_cmdmode(UU_TRUE);
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
				}
			}
		}
/*
.....Create torus using
.....center, vector, axial radius, and circular radius
*/
		else if (ifl == 3)
		{
/*
........Get axial radius of torus
*/
			status = ud_ldas(UD_DASSTRING,UA_NCL,698,str1,80,&numint,UD_NODEFAULT);
			if (numint == 0) break;
/*
........Get circular radius of torus
*/
			status = ud_ldas(UD_DASSTRING,UA_NCL,697,str2,80,&numint,UD_NODEFAULT);
			if (numint == 0) break;
/*
........Get the center and axis of the torus
*/
			while (UU_TRUE)
			{
				S_init_cmd(&cmdbuf,NCL_torus);
				ncl_add_token(&cmdbuf,NCL_center,NCL_comma);
				status = ncl_add_label_rel(UD_DASPCKLOC,&cmdbuf,699,UD_ncl_ptpv,
					&relnum);
				if (status != NCL_OKINPUT) break;

				if (relnum != NCL_POINTVEC_REL)
				{
					status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,700,UD_ncl_ve);
					if (status != NCL_OKINPUT) break;
				}
/*
........Add the radii of the torus
*/
				ncl_add_token(&cmdbuf,NCL_radius,NCL_comma);
				ncl_add_token(&cmdbuf,str1,NCL_comma);
				ncl_add_token(&cmdbuf,str2,NCL_comma);
/*
........Process the command
*/
				if (status == NCL_OKINPUT)
				{
					ncl_set_cmdmode(UU_TRUE);
					ncl_add_cmdbuf(&cmdbuf);
					ncl_call(&cmdbuf);
				}
			}
		}
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION: nclu_solid_sweep()
**      This function defines a Visula Solid in the form of an extrusion.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void nclu_solid_sweep()
{
	int status;
	NCL_cmdbuf cmdbuf;
/*
.....Loop until done
*/
	while (UU_TRUE)
	{
		S_init_cmd(&cmdbuf,NCL_extrud);
/*
........Get the extruded curve
*/
		status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,702,UD_ncl_cicv);
		if (status != NCL_OKINPUT) break;
/*
........Get the extrusion vector
*/
		status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,703,UD_ncl_vepv);
		if (status != NCL_OKINPUT) break;
/*
........Get the extrusion distance
*/
		status = ncl_add_length(&cmdbuf,704);
		if (status != NCL_OKINPUT) break;
/*
........Process the command
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION: nclu_solid_revolve()
**      This function defines a Visula Solid in the form of a revolved
**      curve.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void nclu_solid_revolve()
{
	int status,relnum;
	NCL_cmdbuf cmdbuf;
/*
.....Loop until done
*/
	while (UU_TRUE)
	{
		S_init_cmd(&cmdbuf,NCL_revolv);
/*
........Get the revolved curve
*/
		status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,605,UD_ncl_nsfcv);
		if (status != NCL_OKINPUT) break;
/*
........Get the center and axis of revolution
*/
		status = ncl_add_label_rel(UD_DASPCKLOC,&cmdbuf,606,UD_ncl_ptpvln,
			&relnum);
		if (status != NCL_OKINPUT) break;

		if (relnum == UM_POINT_REL)
		{
			status = ncl_add_label(UD_DASPCKLOC,&cmdbuf,607,UD_ncl_ve);
			if (status != NCL_OKINPUT) break;
		}
/*
........Get the starting angle
*/
		status = ncl_add_length(&cmdbuf,608);
/*
........Get the ending angle
*/
		if (status == NCL_OKINPUT)
			status = ncl_add_length(&cmdbuf,609);
/*
........Process the command
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclu_solid_load()
**       Interface for loading a stock file as a SOLID.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_solid_load()
{
	int nc,status;
	UU_LOGICAL cmdreject;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1, 1, 1,0, 1};
	static char called[]   = {6,6, 6, 6,6, 6};
	static char display[]  = {1,1, 1, 1,1, 1};

	static UD_METHOD methods[] = {
		 OnBrowse,UU_NULL, UU_NULL, OnToggle,UU_NULL, UU_NULL};

	static int *ans[] = {UU_NULL, (int *)Sload, (int *)&Sscalar, &Sxform,
		(int *)&Smatrix, (int *)&Slabel};
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)	goto done;
/*
..... Initialize answers
*/
	Sload[0] = '\0';
	Sscalar[0] = '\0';
	Sxform = 0; Smatrix[0] = '\0';
	Slabel[0] = '\0';
	Sftyp = 0;
/*
.....Get the Form input
*/
repeat:
	status = ud_form1("loadsolid.frm",ans,ans,methods,called,display,traverse);
	if (status == -1) goto done;
/*
.....Make sure a filename was specified
*/
	nc = ul_cut_string(Sload,UX_MAX_PATH_LEN);
	if (nc == 0)
	{
		ud_wrerr("A filename must be specified.");
		goto repeat;
	}
/*
.....Output the SOLID command
*/
	S_build_loadcmd();
/*
.....End of routine
*/
done:
	UD_UNMARK(cmdreject); 	
	return;
}

/*********************************************************************
**   E_FUNCTION: nclu_solid_load_stl()
**      Build and output the SOLID/STL command.
**
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**   RETURNS: UU_SUCCESS
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int nclu_solid_load_stl()
{
	int status,nc;
	UU_LOGICAL cmdreject;
	char buf[MXLAB];
	NCL_cmdbuf cmdbuf;
/*
.....Setup form fields
*/
	static char traverse[] = {1,1,1,1,1,1};
	static UD_METHOD methods[] = {OnBrowse,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		OnApply};
	static char called[] = {6,6,6,6,6,6};
	static char display[] = {1,1,1,1,1,1};
	static int *ans[] = {UU_NULL, (int *)Sload, (int *)&Slabel, &Sunits,
		&Sopt, UU_NULL};
/*
.....Initialize form
*/
	Sload[0] = '\0';
	Slabel[0] = '\0';
	Sftyp = 1;
	if (Sunits == -1)
	{
		Sunits = 0;
		if (UM_cpln.length_unit == UM_MM) Sunits = 1;
	}
/*
.....Get the form input
*/
repeat:
	status = ud_form1("loadstl.frm",ans,ans,methods,called,display,traverse);
	if (status == -1) goto done;
/*
.....Make sure a filename was specified
*/
	nc = ul_cut_string(Sload,UX_MAX_PATH_LEN);
	if (nc == 0)
	{
		ud_wrerr("A filename must be specified.");
		goto repeat;
	}
/*
.....Load the STL file
*/
	S_build_stlcmd();
/*
.....End of routine
*/
done:
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  OnApply(filedno, val, stat)
**       Creates a SOLID/STL command and outputs it.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnApply(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Output SOLID/STL command
*/
	S_build_stlcmd();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnBrowse(filedno, val, stat)
**       Routine to get the Stock file name.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnBrowse(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char sbuf[80],ext[UX_SUFFIX_LEN],descrip[80];
	int inum;
/*
.....Get filename to load
*/
	if (Sftyp == 0)
	{
		strcpy(sbuf,"Load Stock File");
		strcpy(ext,"*.stk");
		strcpy(descrip,"Stock Files (*.stk)");
	}
	else
	{
		strcpy(sbuf,"Load STL File");
		strcpy(ext,"*.stl");
		strcpy(descrip,"STL Files (*.stl)");
	}
	inum = 0;
	Sload[0] = '\0';
	ud_get_filename(sbuf,sbuf,ext,Sload,&inum,descrip, 1);
	if (inum != 0)
	{
		if (Sftyp == 0) ux_add_ftype("stk",Sload,UX_NPRTERRS);
		else ux_add_ftype("stl",Sload,UX_NPRTERRS);
		ud_update_answer(FLFN,(int *)Sload);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnToggle()
**       Method called when a toggle makes other fields active/inactive.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnToggle(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int ltf;

	switch (*fieldno)
	{
/*
.....Apply transformation matrix
*/
	case FLXF:
		ud_set_traverse_mask(FLMX,Sxform);
		break;
	default:
		break;
	}

	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_init_cmd(cmdbuf,token)
**		   Initializes a standard SOLID command adding the optional label
**       and solid type.
**    PARAMETERS   
**       INPUT  :
**          cmdbuf   = Command buffer to initialize.
**          token    = Solid type (SPHERE, BOX, etc.).
**       OUTPUT : none
**    RETURNS      : UU_SUCCESS if successful.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_init_cmd(cmdbuf,token)
NCL_cmdbuf *cmdbuf;
char *token;
{
	int status;
/*
.....Initialize command
*/
	ncl_init_cmdbuf(cmdbuf);
	status = NCL_OKINPUT;
/*
.....Check to see if auto_label is on, if not, prompt user for label.
*/
	if (!NCL_auto_label) status = ncl_add_name(cmdbuf,1);
/*
.....Put SOLID/ in the command
*/
	status = ncl_add_token(cmdbuf,NCL_so,NCL_nocomma);
	status = ncl_add_token(cmdbuf,token,NCL_comma);
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_build_loadcmd()
**		   Build and output SOLID/LOAD command.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_build_loadcmd()
{
	NCL_cmdbuf cmdbuf;
	UU_LOGICAL cmdreject;
	int nc;
	char buf[MXLAB];
/*
.....Trap command reject
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....Add label
*/
	nc = strlen(Slabel);
	ul_strip_blanks(Slabel,&nc);
	if (nc != 0)
	{
		sprintf(buf,"%s=",Slabel);
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
	}

	ncl_add_token(&cmdbuf, NCL_so, NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_load, NCL_comma);

	ul_add_quotes(Sload,Sload);
	ncl_add_token(&cmdbuf, Sload, NCL_comma);

	nc = strlen(Sscalar);
	ul_strip_blanks(Sscalar,&nc);
	if (nc != 0)
		ncl_add_token(&cmdbuf,Sscalar,NCL_comma);

	if (Sxform)
	{
		nc = strlen(Smatrix);
		ul_strip_blanks(Smatrix,&nc);
		if (nc != 0)
		{
			ncl_add_token(&cmdbuf,NCL_trform,NCL_comma);
			ncl_add_token(&cmdbuf,Smatrix,NCL_comma);
		}
	}

	ncl_set_cmdmode(UU_TRUE);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....End of routine
*/
done:
	UD_UNMARK(cmdreject);
	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_build_stlcmd()
**		   Build and output SOLID/STL command.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_build_stlcmd()
{
	NCL_cmdbuf cmdbuf;
	UU_LOGICAL cmdreject;
	int nc;
	char buf[MXLAB];
/*
.....Trap command reject
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....Add label
*/
	nc = strlen(Slabel);
	ul_strip_blanks(Slabel,&nc);
	if (nc != 0)
	{
		sprintf(buf,"%s=",Slabel);
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
	}

	ncl_add_token(&cmdbuf, NCL_so, NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_stl, NCL_comma);

	if (Sunits == 0) ncl_add_token(&cmdbuf, NCL_inches, NCL_comma);
	else ncl_add_token(&cmdbuf, NCL_mm, NCL_comma);

	if (Sopt == 0)
	{
		ncl_add_token(&cmdbuf, NCL_edge, NCL_comma);
		ncl_add_token(&cmdbuf, NCL_off, NCL_comma);
	}

	ul_add_quotes(Sload,Sload);
	ncl_add_token(&cmdbuf, Sload, NCL_comma);

	ncl_set_cmdmode(UU_TRUE);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....End of routine
*/
done:
	UD_UNMARK(cmdreject);
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_solid_sf_out()
**       Interface for extracting surfaces/solids from a composite solid.
**    PARAMETERS   
**       INPUT  :  none 
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_solid_sf_out()
{
	NCL_cmdbuf cmdbuf;
	int status,lstatus,cur_len,len,sub,nc,tval,limit_key,type,rel;
	int numint,i,i0,i1,choice,nkeys,ind=0,pick_mask[UD_NMENTWD],nokeys;
	int labtyp;
	UU_KEY_ID key,key1,*keys,*netkeys;
	char str[80];
	struct UM_srfdatabag ent;
	struct UM_solid_rec solid,*solptr;
	struct NCL_netsf_rec *netptr;
	UU_LOGICAL found,done,first,lstring,lpar;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT],lab[NCL_MAX_LABEL];
	char tstr[NCL_MAX_LABEL],slab[NCL_MAX_LABEL],*scalab;
	UM_f77_str f77lab;
	UM_int2 subsc;
	UU_LIST key_list;

	UM_PLOCREC pick;
	UU_LOGICAL cmdreject;	
	uv_segbuff(udata);

	uu_denter(UU_MTRC,(us,"nclu_cv_proj_sf"));

	choice = SINGLE;
	done = UU_FALSE;
	status = ncl_popup(NCL_COMPCV_OUT, &choice);

	limit_key = -1;
	UD_MARK(cmdreject, UU_FALSE);
	if (cmdreject != 0)
	{
		if (limit_key!=-1)
			ud_limit_entity(UU_FALSE, limit_key);
		ud_unlimit ();
		return (0);
	}
/*
.....Set picking mask to solids and net surfaces
*/
	for (i=0; i<UD_NMENTWD; i++)
		pick_mask[i] = UD_solid[i] | UD_ncl_netsf[i];
	while (status == NCL_OKINPUT)
	{
		first = UU_TRUE;
		lstring = UU_FALSE;
/*
.....Prepick the entities if ALL
*/
		if (choice == ALL)
		{
			do
			{
				ud_lgeo(UU_TRUE, pick_mask);
				ud_ldas(UD_DASSELECT,UM_MODEL,350,UU_NULL,NCL_MAXPICK,&numint,
					UD_NODEFAULT);
				if (numint <= 0) break;
				else
				{
					uu_list_init(&key_list,sizeof(UU_KEY_ID),numint,1);
					lstatus = UU_TRUE;
					while (ud_gnxt(lstatus,UU_NULL,&key,1))
					{
						ur_retrieve_data_relnum(key,&rel);
						if (rel == UM_SOLID_REL)
						{
							solid.key = key;
							ncl_retrieve_data_fixed(&solid,sizeof(solid));
							type = solid.type;
						}
						else
							type = UM_COMPOS_SOLID;
						if (type != UM_COMPOS_SOLID)
							ud_wrerr("A composite solid must be picked.");
						else
						{
							uu_list_push(&key_list,&key);
						}
						lstatus = UU_FALSE;
					}
					keys = (UU_KEY_ID *)UU_LIST_ARRAY(&key_list);
					nkeys = key_list.cur_cnt;
				}
			} while (type != UM_COMPOS_SOLID);
			if (numint <= 0) break;
		}
		do
		{
/*
.....Initialize command buffer and status.
*/
			ncl_init_cmdbuf(&cmdbuf);
/*
.....If auto label is off, prompt user for label.
*/
			if (!NCL_auto_label)
				status = ncl_add_name(&cmdbuf, 1);
/*
.....Have user pick a solid/net-surface if not ALL
*/
			if (choice != ALL)
			{
				do
				{
					ud_lgeo(UU_TRUE, pick_mask);
					if (choice == THRU)
						ua_dl_pldas(UD_DASPCKLOC,UA_NCL,577,&pick,1,&numint,1);
					else
						ua_dl_pldas(UD_DASPCKLOC,UM_MODEL,350,&pick,1,&numint,1);
					key = um_get_pickkey(&pick.pent, 1);
					if (numint <= 0) break;
					ur_retrieve_data_relnum(key,&rel);
					if (rel == UM_SOLID_REL)
					{
						solid.key = key;
						ncl_retrieve_data_fixed(&solid,sizeof(solid));
						type = solid.type;
						if (type == UM_COMPOS_SOLID)
							um_retrieve_data_relnum(solid.netkey[0],&rel);
					}
					else
						type = UM_COMPOS_SOLID;
					if (type != UM_COMPOS_SOLID)
						ud_wrerr("A composite solid must be picked.");
					else
					{
						limit_key = key;
						ud_limit_entity(UU_TRUE, limit_key);
					}
				} while (type != UM_COMPOS_SOLID);
				if (numint <= 0) break;
			}
			else
			{
				if (ind == nkeys) 
				{
					uu_list_free(&key_list);
					break;
				}
				key = keys[ind++];
			}
			i0 = i1 = 1;

			ent.key = key;
			status = ncl_retrieve_data_fixed(&ent);
			if (status != UU_SUCCESS) break;
			labtyp = 1;
			if (ent.rel_num == UM_SOLID_REL)
			{
				solptr = (struct UM_solid_rec *)&ent;
				netkeys = solptr->netkey;
				nokeys = solptr->no_netkey;
				ur_retrieve_data_relnum(netkeys[0],&rel);
				if (rel == UM_SOLID_REL) labtyp = 2;
			}
			else
			{
				netptr = (struct NCL_netsf_rec *)&ent;
				netkeys = netptr->netkey;
				nokeys = netptr->no_netkey;
			}

			if (choice != ALL)
			{
				key1 = um_get_pickkey(&pick.pent, 2);
				found = UU_FALSE;
				for (i = 0; i < nokeys && !found; i++)
					found = (key1 == netkeys[i]);

				if (found) i0 = i1 = i;

			}
/*
.....Add SURF /OUT
.....    SOLID
*/
			if (labtyp == 1)
				ncl_add_token(&cmdbuf, NCL_sf, NCL_nocomma);
			else
				ncl_add_token(&cmdbuf, NCL_so, NCL_nocomma);
			ncl_add_token(&cmdbuf, NCL_out, NCL_comma);
/*
.....Format label used in command to include subscripts
*/
			ncl_get_label(&ent,label);
			ncl_add_token(&cmdbuf,label,NCL_comma);

			if (choice == SINGLE)
			{
				sprintf(str, "%d",i0); 
     			status = ncl_add_token(&cmdbuf, str, NCL_nocomma);
			}
			else if (choice == ALL)
     			status = ncl_add_token(&cmdbuf, "ALL", NCL_nocomma);
			else /* choice == THRU */
			{
				if (found)
				{
					i1 = i0;
					ua_dl_pldas(UD_DASPCKLOC,UA_NCL,578,&pick,1,&numint,1);
					if (numint > 0)
					{
						key = um_get_pickkey(&pick.pent, 1);
						if (ent.key == key)
						{
							key1 = um_get_pickkey(&pick.pent, 2);
							found = UU_FALSE;
							for (i = 0; i < nokeys && !found; i++)
								found = (key1 == netkeys[i]);
							if (found) i1 = i;
						}
					}
				}
				if (i0 < i1)
					sprintf(str,"%d,THRU,%d",i0,i1);
				else if (i0 > i1)
					sprintf(str,"%d,THRU,%d",i1,i0);
				else
					sprintf(str, "%d",i); 
     			status = ncl_add_token(&cmdbuf, str, NCL_nocomma);
			}
			if (status != UU_SUCCESS) break;
/*
.....Get number of components variable
*/
			if (choice != SINGLE && first)
			{
				ncl_get_str(&slab,673);
				nc = strlen(slab);
				ul_strip_blanks(slab,&nc);
				lpar = (slab[nc-1] == ')');
				if (choice == ALL) first = UU_FALSE;
				if (nc > 0)
				{
					UM_init_f77_str(f77lab,slab,NCL_MAX_LABEL);
					subsc = parslb(UM_addr_of_f77_str(f77lab),&tval);
					scalab = UM_cstr_of_f77_str(f77lab);
					nc = strlen(scalab);
					if (lpar) scalab[nc-1] = 0;
					sub = subsc;
					if (sub == 0 && nkeys > 1 && choice == ALL && tval == 1)
						sub++;
					lstring = UU_TRUE;
					if (sub > 0 || tval == 0)
					{
						if (lpar || (tval == 1 && choice == ALL && nkeys > 1))
							sprintf(tstr,"%s(%d)",scalab,sub);
						else
							sprintf(tstr,"%s%d",scalab,sub);
					}
					else
						sprintf(tstr,"%s",scalab);
					ncl_add_token(&cmdbuf,",NUM", NCL_comma);
					ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
				}
			}
			else if (choice == ALL && !first && lstring)
			{
				sub++;
				if (lpar || tval == 1)
					sprintf(tstr,"%s(%d)",scalab,sub);
				else
					sprintf(tstr,"%s%d",scalab,sub);
				ncl_add_token(&cmdbuf,",NUM", NCL_comma);
				ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			}
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);
			if (limit_key!=-1)
			{
				ud_limit_entity(UU_FALSE, limit_key);
				limit_key = -1;
			}
		} while (!done);
		if (choice == ALL && key_list.data != UU_NULL)
			uu_list_free(&key_list);
		break;
	}
	ud_unlimit();
	if (limit_key!=-1)
		ud_limit_entity(UU_FALSE, limit_key);
	return 0;
}
