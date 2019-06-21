/*********************************************************************
**    NAME         :  nemcdcnv.c
**       CONTAINS:
**				ncl_mcd_create_toolfile()
**				ncl_mcd_load()
**				ncl_mcd_read()
**				ncl_mcd_free()
**    COPYRIGHT 2006 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nemcdcnv.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:36
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include "bsym.h"
#include "lcom.h"
#include "lipv.h"
#include "mfort.h"
#include "mdcpln.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nclfile.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "driver.h"

#define BLADE 191
#define LATHE 700

#define FWRITE(buf) \
{ \
	if (strlen(buf) != 0) \
	{ \
	stat = ux_fwrite0(buf,strlen(buf),1,fd,&nc); \
	if (stat != UU_SUCCESS) goto done; \
	} \
	ux_fwrite0("\n",1,1,fd,&nc); \
	if (stat != UU_SUCCESS) goto done; \
}

void ncl_mcd_free();

/*********************************************************************
**    E_FUNCTION     : ncl_mcd_create_toolfile(fnam,irep)
**       Loads the tools used in the active clfile and stores them
**			into the form list.
**    PARAMETERS
**       INPUT  :
**          fnam    = Name of cutter file to create.
**          irep    = UU_TRUE = Report that file was created.
**       OUTPUT : none.
**    RETURNS      : Number of tools stored or -1 if an error occured.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_mcd_create_toolfile(fnam,irep)
char *fnam;
UU_LOGICAL irep;
{
	int i,j,k,ntl,stat,nc,mode,fstat;
	UU_LOGICAL opened;
	UM_int2 i2b,jnc[2],ifl307,val307,val1,*ipt,ifl,ival;
	UM_int4 i4a,i4b;
	UU_REAL rsav[3],rbuf[7],cutr[12],cnv;
	char tbuf[100],sbuf[100],fstr[10][80],ysym[MAXSYMLEN];
	UX_pathname tfil,lbuf;
	UM_f77_str fbuf[10],ybuf;
	UU_LIST clist;
	UN_cutter_list *cpt;
	FILE *fd;
/*
.....Initialize routine
*/
	opened = UU_FALSE;
	ntl = 0;
	ifl307 = 307;
	val1 = 1;
	getifl(&ifl307,&val307);
	setifl(&ifl307,&val1);
	for (i=0;i<10;i++) UM_init_f77_str(fbuf[i],fstr[i],80);
/*
.....Set Units conversion
*/
	ifl = 264;
	getifl(&ifl,&ival);
	if (ival == 0) cnv = 1.;
	else cnv = 25.4;
/*
.....Disable IPV tool limits
*/
	rsav[0] = LW_tool_limit[0];
	rsav[1] = LW_tool_limit[1];
	rsav[2] = LW_tool_limit[2];
	LW_tool_limit[0] = UM_DFUZZ;
	LW_tool_limit[1] = UM_MAXREAL;
	LW_tool_limit[2] = UM_DFUZZ;
/*
.....Load the tool list
*/
	ul_ipv_load_tools(&clist,&ntl);
	cpt = (UN_cutter_list *) UU_LIST_ARRAY(&clist);
/*
.....Merge tools with active list
*/
	if (ntl != 0)
	{
		ul_ipv_merge_tools(&clist,ntl);
	}
/*
.....Restore IPV tool limits
*/
	LW_tool_limit[0] = rsav[0];
	LW_tool_limit[1] = rsav[1];
	LW_tool_limit[2] = rsav[2];
/*
.....Prepare the filename
*/
	strcpy(tfil,fnam);
	ul_cut_string(tfil,UX_MAX_PATH_LEN);
	if (strlen(tfil) == 0)
	{
		strcpy(tfil,"ncltemp.dat");
	}
/*
.....Ask to overwrite if file exists
*/
	else
	{
		mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
		stat = ux_file_inquire(UU_NULL,UU_NULL,tfil,UU_NULL,UU_NULL,&mode,&fstat,
			lbuf,UX_NPRTERRS);
		if (stat != UU_SUCCESS) goto failed;
		if (mode != (mode|UX_NEXISTS))
		{
			ul_short_filename(tfil,tbuf,40);
			sprintf(lbuf,"%s already exists.\nDo you want to overwrite it?",tbuf);
			stat = ud_yesno(0,lbuf,"File exists");
			if (!stat) goto done;
		}
	}
/*
.....Open the tool file
*/
	stat = ux_fopen0(tfil,"w",&fd);
	if (stat != UU_SUCCESS) goto failed;
	opened = UU_TRUE;
/*
.....Create the tool file
*/
	for (i=0;i<ntl;i++)
	{
		if (cpt[i].tlno != 0 && cpt[i].used)
		{
/*
........LOADTL/tlno
*/
			FWRITE("");
			if (cpt[i].tlen == 0.)
				sprintf(sbuf,"LOADTL/%d",cpt[i].tlno);
			else
				sprintf(sbuf,"LOADTL/%d,LENGTH,%lf",cpt[i].tlno,cpt[i].tlen*cnv);
			FWRITE(sbuf);
/*
........CUTTER/dia
*/
			if (cpt[i].cutter[0] != 0.)
			{
/*
...........Lathe/Blade Cutter
*/
				if (cpt[i].type == NCL_CUTTER_LATHE ||
					cpt[i].type == NCL_CUTTER_BLADE)
				{
					cutr[0] = LATHE - 10000;
					if (cpt[i].type == NCL_CUTTER_BLADE) cutr[0] = BLADE - 10000;
					for (k=0;k<cpt[i].ncparm;k++)
					{
						cutr[k+1] = cpt[i].cutter[k];
						if (k < 3) cutr[k+1] = cutr[k+1] * cnv;
					}
					i4a = cpt[i].ncparm + 1;
				}
/*
...........Standard Cutter
*/
				else
				{
					for (k=0;k<cpt[i].ncparm;k++)
					{
						cutr[k] = cpt[i].cutter[k];
						if ((k < 3) || (cpt[i].ncparm > 4 && k < 5))
							cutr[k] = cutr[k] * cnv;
					}
					i4a = cpt[i].ncparm;
				}
				i2b = 0;
				ascutr(cutr,&i4a,&i2b,UM_addr_of_f77_str(fbuf[0]),jnc);
				fstr[0][jnc[0]] = '\0';
				if (jnc[1] != 0)
				{
					nc = jnc[0];
					ul_strip_blanks(fstr[0],&nc);
					fstr[0][nc-1] = '\0';
					nc = jnc[1];
					fstr[1][nc] = '\0';
					ul_strip_blanks(fstr[1],&nc);
					strcpy(tbuf,fstr[0]); strcat(tbuf,fstr[1]);
					FWRITE(tbuf);
				}
				else if (jnc[0] == 0)
				{
					FWRITE("CUTTER/0");
				}
				else
				{
					FWRITE(fstr[0]);
				}
			}
/*
........CUTTER/DISPLY(,HOLDER/SHANK)symbol
*/
			for (j=0;j<3;j++)
			{
				if (cpt[i].ctype[j] > 1|| (j != 0 && cpt[i].ctype[j] > 0))
				{
					strcpy(ysym,cpt[i].symbol[j]);
					for (k=strlen(ysym);k<MAXSYMLEN;k++) ysym[k] = ' ';
					UM_init_f77_str(ybuf,ysym,20);
					i4a = 5; if (j != 0) i4a = 6;
					i4b = 4;
					ipt = (UM_int2 *)&rbuf[2];
					ipt[2] = cpt[i].ctype[j];
					ipt[3] = j;
					if (j == 0)
					{
						rbuf[3] = cpt[i].parms[j][2] * cnv;
						rbuf[4] = cpt[i].parms[j][3] * cnv;
					}
					else
					{
						for (k=0;k<4;k++)
						{
							rbuf[k+3] = cpt[i].parms[j][k];
							if (cpt[i].ctype[j] != 1 || k != 2)
								rbuf[k+3] = rbuf[k+3] * cnv;
						}
					}
					ascudi(&i4a,rbuf,&i4b,UM_addr_of_f77_str(ybuf),
						UM_addr_of_f77_str(fbuf[0]),jnc);
					fstr[0][jnc[0]] = '\0';
					if (jnc[1] != 0)
					{
						nc = jnc[0];
						ul_strip_blanks(fstr[0],&nc);
						fstr[0][nc-1] = '\0';
						nc = jnc[1];
						fstr[1][nc] = '\0';
						ul_strip_blanks(fstr[1],&nc);
						strcpy(tbuf,fstr[0]); strcat(tbuf,fstr[1]);
						FWRITE(tbuf);
					}
					else
						FWRITE(fstr[0]);
				}
			}
		}
	}
/*
.....Report that file was created
*/
	if (irep)
	{
		ul_short_filename(tfil,tbuf,40);
		sprintf(sbuf,"Tool file '%s' created.",tbuf);
		ud_printmsg(sbuf);
	}
	goto done;
/*
.....Could not open tool file
*/
failed:;
	ul_short_filename(tfil,tbuf,80);
	sprintf(sbuf,"Could not create %s.",tbuf);
	ud_wrerr(sbuf);
	ntl = -1;
/*
.....Close the file
*/
done:;
	if (opened) ux_fclose0(fd);
	uu_list_free(&clist);
	setifl(&ifl307,&val307);
	return(ntl);
}

/*********************************************************************
**    E_FUNCTION     : ncl_mcd_load(fnam)
**       Loads an MCD file for viewing.
**    PARAMETERS
**       INPUT  :
**          fnam    = Name of MCD file to load.
**       OUTPUT : none.
**    RETURNS      : UU_FAILURE if could not load file, UU_SUCCESS otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_mcd_load(fnam)
char *fnam;
{
	int i,n,nc,mode,fstat,stat,status;
	UU_LOGICAL opened;
	char buf[256];
	UX_pathname lbuf;
	FILE *fd;
/*
.....Initialize routine
*/
	opened = UU_FALSE;
	status = UU_SUCCESS;
	ncl_mcd_free();
/*
.....Make sure the file exists
*/
	mode = UX_EXISTS|UX_READ;
	stat = ux_file_inquire(UU_NULL,UU_NULL,fnam,UU_NULL,UU_NULL,&mode,&fstat,
		lbuf,UX_NPRTERRS);
	if (stat != UU_SUCCESS) goto failed;
	if (mode == (mode|UX_NEXISTS)) goto failed;
/*
.....Open the MCD file
*/
	stat = ux_fopen0(fnam,"r",&fd);
	if (stat != UU_SUCCESS) goto failed;
	opened = UU_TRUE;
/*
.....Count the lines in the file
*/
	n = 0;
	while (fgets(buf,sizeof(buf)-1,fd) != UU_NULL) n++;
	if (n == 0) goto failed;
/*
.....Allocate memory for the file storage
*/
	UN_mcd_fptr = (char **)uu_malloc(sizeof(char *)*n);
	if (UN_mcd_fptr == UU_NULL) goto failed;
/*
.....Read in the file
*/
	rewind(fd);
	for (i=0;i<n;i++)
	{
		stat = ul_fread(fd,buf,sizeof(buf),&nc);
		if (stat != UU_SUCCESS) break;
		UN_mcd_fptr[i] = (char *)uu_malloc(sizeof(char)*nc+1);
		if (UN_mcd_fptr[i] == UU_NULL) goto failed;
		strcpy(UN_mcd_fptr[i],buf);
	}
	UN_mcd_nlines = i;
	goto done;
/*
.....Could not load file
*/
failed:;
	UN_mcd_nlines = 0;
	status = UU_FAILURE;
/*
.....Close the file
*/
done:;
	if (opened) ux_fclose0(fd);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_mcd_read(irec,buf,nc)
**       Reads a record from the loaded MCD file.
**    PARAMETERS
**       INPUT  :
**          irec    = Record number to read.
**       OUTPUT : none.
**          buf     = Text of record or blank if record does not exist.
**          nc      = Number of characters in 'buf'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mcd_read(irec,buf,nc)
int irec;
char *buf;
int *nc;
{
/*
.....Transfer the requested record
*/
	if (irec >= 0 && irec < UN_mcd_nlines)
	{
		strcpy(buf,UN_mcd_fptr[irec]);
		*nc = strlen(buf);
	}
/*
.....Record does not exist
*/
	else
	{
		buf[0] = '\0';
		*nc = 0;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_mcd_free()
**       Frees the memory associated with the loaded MCD file.
**    PARAMETERS
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_mcd_free()
{
	int i;
/*
.....Free allocated data memory
*/
	if (UN_mcd_nlines > 0)
	{
		for (i=0;i<UN_mcd_nlines;i++) uu_free(UN_mcd_fptr[i]);
/*
.....Free pointer memory
*/
		uu_free(UN_mcd_fptr);
/*
.....Mark memory as freed
*/
		UN_mcd_nlines = 0;
	}
}
