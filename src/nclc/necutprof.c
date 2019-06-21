/*********************************************************************
**	 NAME:  necutprof.c
**		 CONTAINS:
**			ncl_opend_cutprof
**
**	 COPYRIGHT 2007 (c) NCCS Inc.  All Rights Reserved.
**	 MODULE NAME AND RELEASE LEVEL 
**       necutprof.c , 25.3
**    DATE AND TIME OF LAST MODIFICATION
**       12/01/15 , 08:25:56
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

UU_LIST NCL_plib_tools;

/*********************************************************************
**	 E_FUNCTION : int ncl_opend_cutprof(filename)
**       Loads the external tool description file
**
**	 PARAMETERS	
**		 INPUT  : filename: profile lib name
**					flag: 1: only contains class and label, no points info
**					flag: 2: all info
**		 OUTPUT :
**			none
**
**	 RETURNS: UU_SUCCESS of no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ncl_opend_cutprof(filename, flag)
char *filename;
{
	int i,mode,status,nc,stat,which,inum,idir,line;
	char msg[400];
	char *p,lib[UX_MAX_PATH_LEN+1],direc[UX_MAX_PATH_LEN+1],lbuf[132];
	char parms[50][80],*ux_getenv();
	UU_LOGICAL idid,ifree,ldesc,opened;
	UU_REAL cnv;
	UM_coord pt,cpt,spt;
	UU_LIST tlist;
	struct S_tools_struc stool;
	FILE *fd;
/*
.....Initialize routine
*/
	cnv = 1.;
	status = UU_FAILURE;
	ifree = UU_FALSE;
	idid = UU_FALSE;
	opened = UU_FALSE;
	line = 0;

	fd = fopen(filename, "r");
	if(fd == 0) goto open_err;
	uu_list_init(&NCL_plib_tools,sizeof(struct S_tools_struc),20,5);
	uu_list_init(&stool.pts,sizeof(UM_coord),20,5);
	stool.length = 0;
	ifree = UU_TRUE;
	opened = UU_TRUE;
/*
.....Search file for tool definitions
*/
	cpt[2] = 0.;
	pt[2] = 0.;
	ldesc = UU_FALSE;
	do
	{
		stat = ul_fread(fd,lbuf,sizeof(lbuf),&inum);
		line++;
		if (stat == UX_EOF) break;
		if (stat != UU_SUCCESS) goto read_err;
/*
........Parse record
*/
		stat = ul_ipv_mach_parse_rec(lbuf,inum,parms,&nc,0);
		if (stat != UU_SUCCESS) goto parse_err;
		if (nc == 0) continue;
/*
........Tool description
*/
		if (nc == 1)
		{		
			if (ldesc && !idid)
			{
				if (strlen(parms[0]) > 20) goto class_err;
				strcpy(stool.clas,parms[0]);
				ldesc = UU_FALSE;
			}
			else
			{
				if (idid) uu_list_push(&NCL_plib_tools,&stool);
				if (strlen(parms[0]) > 20) goto name_err;
				ul_to_upper(parms[0]);
				strncpy(stool.label,parms[0],20);
				stool.label[20] = '\0';
				strcpy(stool.clas,"None");
				stool.length = 0;
				uu_list_init(&stool.pts,sizeof(UM_coord),20,5);
				idid = UU_FALSE;
				ldesc = UU_TRUE;
				which = 1;
			}
		}
/*
........Units
*/
		else
		{
			ldesc = UU_FALSE;
			ul_to_upper(parms[0]);
			if (strcmp(parms[0],"UNITS") == 0 && nc == 2)
			{
				ul_to_upper(parms[1]);
				if (strcmp(parms[1],"MM") == 0) cnv = 25.4;
				else if (strcmp(parms[1],"INCH") == 0) cnv = 1.;
				else goto units_err;
			}
			else if (strcmp(parms[0],"LENGTH") == 0 && nc == 2)
			{
				stool.length = atof(parms[1]);
			}
/*
........Point data
*/
			else if (strcmp(parms[0],"POINT") == 0 && nc == 3)
			{
				if (flag==1)
				{
					which = 1;
					idid = UU_TRUE;
					continue;
				}
				for (i=0;i<2;i++)
				{
					stat = ul_to_reals(&pt[i],&nc,1,parms[i+1]);
					if (stat != UU_SUCCESS) goto point_err;
					pt[i] = pt[i] / cnv;
				}
				if (stat != UU_SUCCESS) goto point_err;
/*
...........Last entity was an arc
...........Create the arc now
*/
				if (which == 2)
				{
					stat = ul_ipv_circle_pts(cpt,spt,pt,idir,&tlist,&nc);
					if (stat != UU_SUCCESS) goto arc_err;
					uu_list_push_list(&stool.pts,&tlist);
					uu_list_free(&tlist);
				}
/*
...........Store point
*/
				which = 1;
				uu_list_push(&stool.pts,pt);
				um_vctovc(pt,spt);
				idid = UU_TRUE;
			}
/*
........Arc data
*/
			else if (strcmp(parms[0],"ARC") == 0 && nc == 4)
			{
				if (which == 2) goto arc_err;
				which = 2;
				if (flag==1)
				{
					continue;
				}
				for (i=0;i<2;i++)
				{
					stat = ul_to_reals(&cpt[i],&nc,1,parms[i+1]);
					if (stat != UU_SUCCESS) goto arc_err;
					cpt[i] = cpt[i] / cnv;
				}
				if (stat != UU_SUCCESS) goto arc_err;
				ul_to_upper(parms[3]);
				if (!strcmp(parms[3],"CLW")) idir = 0;
				else if (!strcmp(parms[3],"CCLW")) idir = 1;
				else goto arc_err;
			}
/*
........Unrecognized entry
*/
			else goto syntax_err;
		}
	} while (stat != UX_EOF);
/*
.....Store last tool profile
*/
	if (idid) uu_list_push(&NCL_plib_tools,&stool);
/*
.....Close the file
*/
	status = UU_SUCCESS;
	goto done;
/*
.....Cannot open file
*/
open_err:
	ul_short_filename(filename,lbuf,40);
	sprintf(msg, "Can't open Profile library %s", lbuf);
	ud_wrerr(msg);
	return -1;
/*
.....Error reading file
*/
read_err:
	ul_short_filename(filename,lbuf,40);
	sprintf(msg,"Error reading tool library at line #%d: %s",line,lbuf);
	ud_wrerr(msg);
	goto done;
/*
.....Class name too long
*/
class_err:
	sprintf(msg,
		"Cutter profile class name is longer than 20 characters.\n%d: %s",
		line,parms[0]);
	ud_wrerr(msg);
	goto done;
/*
.....Profile name too long
*/
name_err:
	sprintf(msg,"Cutter profile name is longer than 20 characters.\n%d: %s",
		line,parms[0]);
	ud_wrerr(msg);
	goto done;
/*
.....Parsing error
*/
parse_err:
	sprintf(msg,"Error parsing tool entry:\n%d: %s",line,lbuf);
	ud_wrerr(msg);
	goto done;
/*
.....Invalid command
*/
syntax_err:
	sprintf(msg,"Error unrecognized command in tool profile.\n%d: %s",line,lbuf);
	ud_wrerr(msg);
	goto done;
/*
.....Invalid Units Command
*/
units_err:
	sprintf(msg,
		"UNITS command in the tool profile library has incorrect syntax.\n%d: %s",
		line,lbuf);
	ud_wrerr(msg);
	goto done;
/*
.....Invalid POINT command
*/
point_err:
	sprintf(msg,"Error invalid POINT definition in tool profile.\n%d: %s",
		line,lbuf);
	ud_wrerr(msg);
	goto done;
/*
.....Invalid ARC command
*/
arc_err:
	sprintf(msg,"Error invalid ARC definition in tool profile.\n%d: %s",
		line,lbuf);
	ud_wrerr(msg);
	goto done;
/*
.....End of routine
*/
done:;
	if (opened) fclose(fd);
	if (ifree && !idid) uu_list_free(&stool.pts);
	return(status);
}

/*********************************************************************
**	 E_FUNCTION : int ncl_next_cutprof(inxt,sprof,sclass)
**			This function returns the next cutter profile name from the
**       tool description file.
**	 PARAMETERS	
**		 INPUT  :
**			inxt    = Which profile to return.
**		 OUTPUT :
**			inxt    = Updated to point to next profile.
**			sprof   = Profile name.
**			sclass  = Profile class.
**	 RETURNS: UU_SUCCESS of no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ncl_next_cutprof(inxt,sprof,sclass)
int *inxt;
char *sprof,*sclass;
{
	int status,npt;
	struct S_tools_struc *ptool;
/*
.....Initialize routine
*/
	status = UU_FAILURE;
/*
.....Find the requested tool
*/
	ptool = (struct S_tools_struc *)UU_LIST_ARRAY(&NCL_plib_tools);
	npt = UU_LIST_LENGTH(&NCL_plib_tools);
	if (*inxt < npt)
	{
		strcpy(sprof,ptool[*inxt].label);
		strcpy(sclass,ptool[*inxt].clas);
		*inxt = *inxt + 1;
		status = UU_SUCCESS;
	}
/*
.....End of routine
*/
done:
	return(status);
}
