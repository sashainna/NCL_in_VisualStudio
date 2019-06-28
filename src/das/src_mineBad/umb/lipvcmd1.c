/*********************************************************************
**    NAME         :  lipvcmd1.c
**       CONTAINS:
**			ul_ipv_parse_cutter()
**			ul_ipv_parse_holder()
**       ul_ipv_parse_mixed_parms()
**    COPYRIGHT 2011 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvcmd1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:12
*********************************************************************/

#include "usysdef.h"
#include "lipv.h"
#include "modef.h"
#include "mfort.h"
#include "nclfc.h"
#include "nclfile.h"
#include "nclmplay.h"
#include "umath.h"

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_cutter(cstr,iclw,rclw,nclw,errstr)
**       Parses and executes a PPRINT IPV CUTTER statement (command).
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of PPRINT statement.
**       OUTPUT :  
**          iclw      = Clfile parameters.
**          rclw      = Cutter parameters.
**          nclw      = Number of values in 'rclw'.
**          errstr    = Part of command that caused error.
**    RETURNS      : UU_SUCCESS if a valid command, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_cutter(cstr,iclw,rclw,nclw,errstr)
char *cstr;
int *iclw;
UU_REAL *rclw;
int *nclw;
char *errstr;
{
#define BLADE 191
#define LATHE 700
	int status,nc,i;
	char buf[100],*p,*strstr();
	char tstr[1024],cstk[100],ctyp[100];
	union
	{
		UM_int2 iptr[52];
		UU_REAL rptr[13];
		char cptr[104];
	} ary;
/*
.....Convert string to upper case
*/
	strcpy(tstr,cstr);
	ul_to_upper(tstr);
	nc = ul_cut_string(tstr,1024);
	tstr[nc] = '\0';
/*
.....Parse beginning of string
*/
   buf[0] = '\0';
   nc = sscanf(tstr,"%s%s%s",buf,cstk,ctyp);
   strcpy(errstr,cstk);
   if (nc != 3) goto failed;
/*
.....CUTTER/DISPLY
*/
	if (strcmp(ctyp,"DISPLY") == 0)
	{
		p = strstr(tstr,ctyp);
		p = p + strlen(ctyp);
		while (*p == ' ') p++;
		p = cstr + (int)(p-tstr);
		strcpy(tstr,p);
		ul_remove_quotes(tstr);
		nc = strlen(tstr);
	
		*nclw = 3 + (nc+7) / 8 + 1;
		iclw[2] = 7100;
		iclw[3] = 7;
		iclw[4] = *nclw;

		ary.iptr[0] = nc;
		ary.iptr[1] = 4;
		ary.rptr[1] = 0.;
		ary.rptr[2] = 0.;
		strcpy(&ary.cptr[24],tstr);
		for (i=0;i<*nclw;i++) rclw[i] = ary.rptr[i];
	}
/*
.....CUTTER/BLADE,parms
.....Position at CUTTER parameters
*/
	else if (strcmp(ctyp,"BLADE") == 0)
	{
		buf[0] = '\0';
		p = strstr(tstr,"BLADE");
		if (p == UU_NULL) goto failed;
		p += 5;
		while (*p == ' ') p++;
/*
.....Break out CUTTER parameters
*/
		status = ul_to_reals(&rclw[1],nclw,4,p);
		if (*nclw != 4)
		{
			if (*nclw > 0) sprintf(errstr,"%lf",rclw[*nclw-1]);
			goto failed;
		}
/*
.....Store clfile record type
*/
		rclw[0] = BLADE - 10000;
		rclw[4] = sin(rclw[4]/UM_RADIAN);
		iclw[2] = 6000;
		iclw[3] = 6;
		iclw[4] = *nclw + 1;
	}
/*
.....CUTTER/LATHE,parms
.....Position at CUTTER parameters
*/
	else if (strcmp(ctyp,"LATHE") == 0)
	{
		buf[0] = '\0';
		p = strstr(tstr,"LATHE");
		if (p == UU_NULL) goto failed;
		p += 5;
		while (*p == ' ') p++;
/*
.....Break out CUTTER parameters
*/
		status = ul_to_reals(&rclw[1],nclw,5,p);
		if (*nclw < 3)
		{
			if (*nclw > 0) sprintf(errstr,"%lf",rclw[*nclw-1]);
			goto failed;
		}
/*
.....Store clfile record type
*/
		rclw[0] = LATHE - 10000;
		iclw[2] = 6000;
		iclw[3] = 6;
		iclw[4] = *nclw + 1;
	}
/*
.....CUTTER/parms
.....Position at CUTTER parameters
*/
	else
	{
		buf[0] = '\0';
		p = strstr(tstr,"CUTTER");
		if (p == UU_NULL) goto failed;
		p += 6;
		while (*p == ' ') p++;
/*
.....Break out CUTTER parameters
*/
		status = ul_to_reals(rclw,nclw,7,p);
		if (*nclw == 0)
		{
			if (*nclw > 0) sprintf(errstr,"%lf",rclw[*nclw-1]);
			goto failed;
		}
/*
.....Store clfile record type
*/
		iclw[2] = 6000;
		iclw[3] = 1;
		iclw[4] = *nclw;
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Invalid CUTTER command
*/
failed:;
	*nclw = 0;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_holder(cstr,iclw,rclw,nclw,errstr)
**       Parses and executes a PPRINT IPV SHANK/HOLDER statement (command).
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of PPRINT statement.
**       OUTPUT :  
**          iclw      = Clfile parameters.
**          rclw      = Cutter parameters.
**          nclw      = Number of values in 'rclw'.
**          errstr    = Part of command that caused error.
**    RETURNS      : UU_SUCCESS if a valid command, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_holder(cstr,iclw,rclw,nclw,errstr)
char *cstr;
int *iclw;
UU_REAL *rclw;
int *nclw;
char *errstr;
{
	int status,nc,i,np,nc1,ncp,inc;
	char buf[100],*p,*strstr();
	char tstr[1024],cstk[100],ctyp[100],parms[20][64];
	union
	{
		UM_int2 iptr[52];
		UU_REAL rptr[13];
		char cptr[104];
	} ary;
/*
.....Convert string to upper case
*/
	strcpy(tstr,cstr);
	ul_to_upper(tstr);
	nc = ul_cut_string(tstr,1024);
	tstr[nc] = '\0';
/*
.....Parse beginning of string
*/
	buf[0] = '\0';
	nc = sscanf(tstr,"%s%s%s",buf,cstk,ctyp);
	strcpy(errstr,cstk);
	if (nc != 3) goto failed;
	for (i=1;i<5;i++) rclw[i] = 0.;
/*
.....Determine if SHANK or HOLDER
*/
	if (strcmp(cstk,"SHANK") == 0) ary.iptr[2] = LW_default_tool.shank_clash;
	else ary.iptr[2] = 2;
/*
.....HOLDER/DISPLY
*/
	if (strcmp(ctyp,"DISPLY") == 0)
	{
		ary.iptr[1] = 4;
/*
........Parse command
*/
		p = strstr(tstr,ctyp);
		p = p + strlen(ctyp);
		while (*p == ' ') p++;
/*		p = cstr + (int)(p-tstr);*/
		np = ul_ipv_parse_mixed_parms(p,parms);
		if (np == 0) goto failed;
/*
........Pt-list name
*/
		ul_remove_quotes(parms[0]);
		nc1 = strlen(parms[0]);
		if (nc1 == 0) goto failed;
		strcpy(&ary.cptr[40],parms[0]);
		ary.iptr[0] = nc1;
		ncp = (nc1+7) / 8;
		for (i=0;i<ncp;i++) rclw[i+5] = ary.rptr[i+5];
/*
........ofs
........x,y
*/
		inc = 1;
		if (np > inc && ul_to_reals(&rclw[1],&nc,1,parms[inc]) == UU_SUCCESS)
		{
			inc++;
			if (np > inc &&
				ul_to_reals(&rclw[2],&nc,1,parms[inc]) == UU_SUCCESS) inc++;
		}
/*
........OFFSET,z1,z2
*/
		if (np > inc && strcmp(parms[inc],"OFFSET") == 0)
		{
			inc++;
			if (inc+1 > np) goto failed;
			if (ul_to_reals(&rclw[3],&nc,1,parms[inc]) != UU_SUCCESS) goto failed;
			inc++;
			if (ul_to_reals(&rclw[4],&nc,1,parms[inc]) != UU_SUCCESS) goto failed;
			inc++;
		}
/*
........CUTTER,HOLDER
*/
		if (inc < np && strcmp(cstk,"SHANK") == 0)
		{
			if (strcmp(parms[inc],"CUTTER") == 0) ary.iptr[2] = 0;
			else if (strcmp(parms[inc],"HOLDER") == 0) ary.iptr[2] = 1;
			else goto failed;
			inc++;
		}
		if (inc < np) goto failed;
	}
/*
.....HOLDER/parms
*/
	else
	{
		ncp = 0;
		ary.iptr[0] = 0;
		ary.iptr[1] = 1;
/*
........Parse command
*/
		p = strstr(tstr,cstk);
		if (p == UU_NULL) goto failed;
		p += strlen(cstk);
		while (*p == ' ') p++;
		np = ul_ipv_parse_mixed_parms(p,parms);
		if (np < 2 || np > 5) goto failed;
/*
........Break out CUTTER parameters
*/
		for (i=0;i<np;i++)
		{
			status = ul_to_reals(&rclw[i+1],&nc,1,parms[i]);
			if (status == UU_SUCCESS && i > 3) goto failed;
			else if (status != UU_SUCCESS)
			{
				if (i+1 != np) goto failed;
				if (strcmp(parms[i],"CUTTER") == 0) ary.iptr[2] = 0;
				else if (strcmp(parms[i],"HOLDER") == 0) ary.iptr[2] = 1;
				else goto failed;
			}
		}
	}
/*
.....Setup clfile buffer
*/
	*nclw = 5 + ncp;
	rclw[0] = ary.rptr[0];
	iclw[2] = 7100;
	iclw[3] = 8;
	iclw[4] = *nclw;
	status = UU_SUCCESS;
	goto done;
/*
.....Invalid CUTTER command
*/
failed:;
	*nclw = 0;
	status = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_parse_mixed_parms(p,parms)
**       Parses and breaks out individual parameters of a PPRINT IPV
**       command.
**    PARAMETERS   
**       INPUT  : 
**          cstr      = Text of commands to parse.
**       OUTPUT :  
**          parms     = Array to receive parameters.
**    RETURNS      : Number of paramters parsed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_parse_mixed_parms(cstr,parms)
char *cstr,parms[20][64];
{
	int inc,i;
	char *p;
/*
.....Parse command
*/
	p = cstr;
	inc = 0;
	while (*p != '\0')
	{
		while (*p == ' ' || *p == ',') p++;
		if (*p == '\0') break;
		i = 0;
		do
		{
			parms[inc][i++] = *p++;
		} while (*p != ' ' && *p != ',' && *p != '\0');
		parms[inc++][i] = '\0';
	}
	return(inc);
}
