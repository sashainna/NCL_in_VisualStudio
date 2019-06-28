/*********************************************************************
**   FILENAME: lipvstk.c
**   CONTAINS: ul_ipv_save_stock()
**             ul_ipv_load_stock()
**             ul_ipv_save_stock_cmds()
**             ul_ipv_count_stock_cmds()
**             ul_ipv_load_stock_cmds()
**             ul_ipv_save_stl()
**             ul_ipv_load_stl()
**             ul_ipv_import_stl()
**             ul_ipv_gen_revsf()
**             ul_ipv_add_stock()
**             ul_ipv_create_stock()
**             ul_ipv_modify_stock()
**             ul_ipv_place_solid()
**     MODULE NAME AND RELEASE LEVEL 
**       lipvstk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:16
*********************************************************************/
#include <stdio.h>
#include <string.h>
#include "usysdef.h"
#include "lcom.h"
#include "mfort.h"
#include "nclfc.h"
#include <ctype.h>
#include <math.h>
#include "gtbl.h"
#include "gobas.h"
#include "view.h"
#include "mdattr.h"
#include "mdunits.h"
#include "mpocket.h"
#include "ginqatt.h"
#include "zsysdep.h"
#include "mcrv.h"
#include "uhep.h"
#include "dselmask.h"
#include "mdpick.h"
#include "modef.h"
#include "ulist.h"
#include "m2dattr.h"
#include "mdcpln.h"
#include "mcrv.h"
#include "msol.h"
#include "nccs.h"
#include "mdrel.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "xfsys1.h"

#include "lipv.h"
#include "lipvmach.h"

#define FBRO 0
#define FSTL 1

#define NITMX(MX) {int QI,QJ; \
	for (QI=0;QI<3;QI++) { \
	for (QJ=0;QJ<3;QJ++) { \
	if (fabs(MX[QI][QJ]) < UM_DFUZZ) MX[QI][QJ] = 0.; \
	}}}

static int stype[4]={LI_MW_SOLID_TYPE_STOCK,LI_MW_SOLID_TYPE_FIXTURE,
	LI_MW_SOLID_TYPE_TARGET,LI_MW_SOLID_TYPE_GENERAL};
static char *gname[3]={"Stock","Fixture","Target"};
static UM_coord Sorigin = {0,0,0};
static UM_vector Sxaxis = {1,0,0};
static UM_vector Syaxis = {0,1,0};
static UM_vector Szaxis = {0,0,1};
static UX_pathname Sfile;
static int Swhich;

void ul_ipv_modify_stock();
void ul_ipv_place_solid();
static void S_init_mx();

extern char uw_color_name[64][96];

/*********************************************************************
**   E_FUNCTION: ul_ipv_save_stock()
**      Saves the defined stock and fixture primitives in an external
**      text file.
**
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_save_stock()
{
	int stat,mode,fstat,inum;
	char sbuf[80],ext[UX_SUFFIX_LEN],descrip[80];
	UX_pathname file,lbuf;
	FILE *fd;
/*
.....Make sure at least 1 stock is defined
*/
	if (LW_nstock[0] == 0 && LW_nstock[1] == 0) goto nogeom;
/*
.....Get filename to save
*/
	sprintf(sbuf,"Save Stock File");
	strcpy(ext,"*.stk");
	strcpy(descrip,"Stock Files (*.stk)");
	inum = 0;
	file[0] = '\0';
	ud_get_filename(sbuf,sbuf,ext,file,&inum,descrip, 0,UU_FALSE);
	if (inum == 0) goto done;
	ux_add_ftype("stk",file,UX_NPRTERRS);
/*
.....See if file already exists
*/
	mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
	stat = ux_file_inquire(UU_NULL,UU_NULL,file,UU_NULL,UU_NULL,
		&mode,&fstat,lbuf,UX_NPRTERRS);
	if (stat != UU_SUCCESS) goto filerr;
/*
.....File already exists
.....Ask if the user wants to overwrite it
*/
	if (mode != (mode|UX_NEXISTS))
	{
		sprintf(lbuf,"%s already exists.\nDo you want to overwrite it?",file);
		stat = ud_yesno(0, lbuf, "File exists");
		if (!stat) goto done;
	}
/*
.....Open file for writing
*/
	stat = ux_fopen0(file,"w",&fd);
	if (stat != UU_SUCCESS) goto filerr;
/*
.....Loop thru stocks & fixtures and
.....write to file
*/
	stat = ul_ipv_save_stock_cmds(fd,UU_TRUE,UU_TRUE,UU_NULL,UU_NULL);
	if (stat != UU_SUCCESS) goto wrterr;
	ux_fclose0(fd);
	goto done;
/*
.....No stocks defined
*/
nogeom:;
	ud_wrerr("No stocks nor fixtures have been defined.");
	goto done;
/*
.....Could not open file
*/
filerr:;
	ul_short_filename(file,sbuf,60);
	sprintf(lbuf,"Could not create %s.",sbuf);
	ud_wrerr(lbuf);
	goto done;
/*
.....Error writing to file
*/
wrterr:;
	ul_short_filename(file,sbuf,60);
	sprintf(lbuf,"Error writing to %s.",sbuf);
	ud_wrerr(lbuf);
	ux_fclose0(fd);
	goto done;
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_load_stock()
**      Loads a previously saved stock and fixture primitive file.
**
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_load_stock()
{
	int stat,status,mode,fstat,inum,idn[2];
	char sbuf[80],ext[UX_SUFFIX_LEN],descrip[80];
	UX_pathname file,lbuf;
	FILE *fd;
/*
.....Get filename to load
*/
	sprintf(sbuf,"Load Stock File");
	strcpy(ext,"*.stk");
	strcpy(descrip,"Stock Files (*.stk)");
	inum = 0;
	file[0] = '\0';
	ud_get_filename(sbuf,sbuf,ext,file,&inum,descrip, 1,UU_FALSE);
	if (inum == 0) goto done;
	ux_add_ftype("stk",file,UX_NPRTERRS);
/*
.....Make sure file already exists
*/
	mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
	stat = ux_file_inquire(UU_NULL,UU_NULL,file,UU_NULL,UU_NULL,
		&mode,&fstat,lbuf,UX_NPRTERRS);
	if (stat != UU_SUCCESS) goto filerr;
	if (mode == (mode|UX_NEXISTS)) goto filerr;
/*
.....Open file for reading
*/
	stat = ux_fopen0(file,"r",&fd);
	if (stat != UU_SUCCESS) goto filerr;
/*
.....Process file
*/
	idn[0] = idn[1] = 0;
	status = ul_ipv_load_stock_cmds(fd,UU_TRUE,&idn,UU_NULL,UU_NULL,UU_TRUE);
	if (status != UU_SUCCESS) goto readerr;
	ux_fclose0(fd);
	goto done;
/*
.....Could not open file
*/
filerr:;
	ul_short_filename(file,sbuf,60);
	sprintf(lbuf,"Could not open %s.",sbuf);
	ud_wrerr(lbuf);
	goto done;
/*
.....Error reading from file
*/
readerr:;
	ul_short_filename(file,sbuf,60);
	sprintf(lbuf,"Error reading from %s.",sbuf);
	ud_wrerr(lbuf);
	ux_fclose0(fd);
	goto done;
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_save_stock_cmds(fd,cnvflag,global,slist,nstk)
**      Saves the defined stock and fixture primitives in an external
**      text file.
**
**   PARAMETERS
**       INPUT  : fd      = File descriptor of stock file.
**                cnvflag = UU_TRUE - perform metric conversion prior
**                          to outputing commands.
**                global  = UU_TRUE - Save stocks from global stock list.
**                          UU_FALSE - Save stocks from 'slist'.
**                slist   = Local stock structures when 'global' is False.
**                nstk    = Number of stocks & fixtures to save when
**                          'global' is False.
**       OUTPUT : none
**   RETURNS: UU_FAILURE if could not write to stock file.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_save_stock_cmds(fd,cnvflag,global,slist,nstk)
FILE *fd;
UU_LOGICAL cnvflag,global;
LW_stock_struc *slist[2];
int nstk[2];
{
	int i,j,k,inum,npts,stat,status,nent,ifl;
	LW_stock_struc *sd,*stock;
	char sbuf[UX_MAX_PATH_LEN+40];
	UM_coord *sdpt;
	char colors[65][96];
/*
.....Initialize routine
*/
	strcpy(colors[0], "DEFALT");
	for (i=0; i<64;i++)
	{
		sprintf(colors[i+1], "%s", uw_color_name[i]);
	}
	status = UU_SUCCESS;
/*
.....Save Units setting
*/
	if (cnvflag)
	{
		if (UM_cpln.length_unit == UM_MM)
		{
			strcpy(sbuf,"UNITS/MM\n");
			stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		}
		else
		{
			strcpy(sbuf,"UNITS/INCH\n");
			stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		}
	}
/*
.....Loop thru stocks & fixtures and
.....write to file
*/
	for (j=0;j<2;j++)
	{
		if (global)
		{
			sd = LW_stock_first[j];
			nent = LW_nstock[j];
		}
		else
		{
			nent = nstk[j];
		}
		for (i=0;i<nent;i++)
		{
			if (!global) sd = &slist[j][i];
/*
........Write out Composite record
*/
			if (sd->type == LW_STOCK_COMPOS)
			{
				sprintf(sbuf,"COLOR/%s\n",colors[sd->color+1]);
				stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
				if (stat != UU_SUCCESS) goto failed;
				ul_ipv_stock_cmd(j,sd,sbuf,UX_MAX_PATH_LEN+40,cnvflag);
				strcat(sbuf,"\n");
				stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
				if (stat != UU_SUCCESS) goto failed;
				if (!global) continue;
			}

			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sd,&stock,&ifl,UU_FALSE);
				if (ifl == -2) break;
				sprintf(sbuf,"COLOR/%s\n",colors[stock->color+1]);
				stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
				if (stat != UU_SUCCESS) goto failed;
				ul_ipv_stock_cmd(j,stock,sbuf,UX_MAX_PATH_LEN+40,cnvflag);
				strcat(sbuf,"\n");
				stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
				if (stat != UU_SUCCESS) goto failed;
/*
........Output profile points
*/
				if (stock->type == LW_STOCK_REVOLVE ||
					stock->type == LW_STOCK_SWEEP)
				{
					if (stock->type == LW_STOCK_REVOLVE)
					{
						sdpt = (UM_coord *)&(stock->data[9]);
						npts = (int)stock->data[8];
					}
					else
					{
						sdpt = (UM_coord *)&(stock->data[4]);
						npts = (int)stock->data[3];
					}
					for (k=0;k<npts;k++)
					{
						if (k == 0 || um_dcccc(sdpt[k],sdpt[k-1]) > UM_FUZZ)
						{
							ul_ipv_stock_ptcmd(sdpt[k],sbuf,cnvflag);
							strcat(sbuf,"\n");
							stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
							if (stat != UU_SUCCESS) goto failed;
						}
					}
					stat = ux_fwrite0("EOS\n",4,1,fd,&inum);
					if (stat != UU_SUCCESS) goto failed;
				}
			} while (ifl != -1);
			if (global) sd = (LW_stock_struc *)uu_lsnext(sd);
		}
	}
	goto done;
/*
.....Could not write to file
*/
failed:;
	status = UU_FAILURE;
/*
....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_count_stock_cmds(fd,nstk)
**      Counts the number of saved stock and fixture primitives in a text
**      file.
**
**   PARAMETERS
**       INPUT  : fd      = File descriptor of stock file.
**       OUTPUT : nstk    = Number of stocks & fixtures defined in
**                          stock file.
**   RETURNS: UU_FAILURE if could not read from stock file.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_count_stock_cmds(fd,nstk)
FILE *fd;
int nstk[2];
{
	int status,stat,inum;
	char lbuf[UX_MAX_PATH_LEN+40],styp[80];
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	nstk[0] = nstk[1] = 0;
/*
.....Process file
*/
	do
	{
/*
.....Read line
*/
		stat = ul_fread(fd,lbuf,sizeof(lbuf),&inum);
		if (stat == UX_EOF) goto done;
		if (stat != UU_SUCCESS) goto failed;
/*
.....Get type of stock to define
*/
repeat:;
		stat = sscanf(lbuf,"%s",styp);
/*
.....Stock definition
*/
		if (stat >= 1)
		{
			if (strcmp(styp,"CBOX") == 0 || strcmp(styp,"CONE") == 0 ||
				strcmp(styp,"CYL") == 0 || strcmp(styp,"SPHER") == 0 ||
				strcmp(styp,"TORUS") == 0 || strcmp(styp,"CREV") == 0 ||
				strcmp(styp,"SPROF") == 0 || strcmp(styp,"STL") == 0 ||
				strcmp(styp,"COMP") == 0) nstk[0] += 1;
			else if (strcmp(styp,"FCBOX") == 0 || strcmp(styp,"FCONE") == 0 ||
				strcmp(styp,"FCYL") == 0 || strcmp(styp,"FSPHER") == 0 ||
				strcmp(styp,"FTORUS") == 0 || strcmp(styp,"FCREV") == 0 ||
				strcmp(styp,"FSPROF") == 0 || strcmp(styp,"FSTL") == 0 ||
				strcmp(styp,"FCOMP") == 0) nstk[1] += 1;
		}
	} while(status == UU_SUCCESS);
	goto done;
/*
.....Could not read from file
*/
failed:;
	status = UU_FAILURE;
/*
....End of routine
*/
done:;
	rewind(fd);
	return(status);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_load_stock_cmds(fd,iadd,idn,slist,nstk,compfl)
**      Loads previously saved stock and fixture primitives from a text
**      file.  Only the parameter data of the stock structure and its type
**      is loaded, not the attributes.
**
**   PARAMETERS
**       INPUT  : fd      = File descriptor of stock file.
**                iadd    = UU_TRUE = Add stocks to global stock list.
**                          UU_FALSE = Return stocks in 'slist'.
**                idn     = ID number for stock.
**                nstk    = Maximum number of stocks & fixtures that
**                          can be loaded into 'slist'.
**                compfl  = UU_TRUE = Import composite stock definitions.
**                          UU_FALSE = Import composite stock components
**                                     as separate stocks.
**       OUTPUT : slist   = Local stock structure list when iadd is False.
**                nstk    = Actual number of stocks & fixtures loaded
**                          into 'slist'.
**                idn     = Incremented stock ID number.
**   RETURNS: UU_FAILURE if could not write to stock file.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_load_stock_cmds(fd,iadd,idn,slist,nstk,compfl)
FILE *fd;
UU_LOGICAL iadd,compfl;
LW_stock_struc *slist[2];
int nstk[2],idn[2];
{
	int i,inum,npts,stat,status,which,istk,totstk,istkpt[2],icol,tstk[2],nc;
	int colsav[2],np,ncomp,cids[50],ncids,cmpid;
	UU_LOGICAL readfl;
	LW_stock_struc *sd;
	LW_stock_type itype;
	char lbuf[UX_MAX_PATH_LEN+40],styp[80],*p;
	UU_REAL ary[10],*rpt1,*rpt2,cnv;
	UM_coord *sdpt,*sdpt1,pts;
	UU_LIST list;
	UX_pathname filename;
	char colors[65][96];
/*
.....Initialize routine
*/
	strcpy(colors[0], "DEFALT");
	for (i=0; i<64;i++)
	{
		sprintf(colors[i+1], "%s", uw_color_name[i]);
	}
	status = UU_SUCCESS;

	if (iadd) totstk = 0;
	else
	{
		tstk[0] = nstk[0]; tstk[1] = nstk[1];
		totstk = nstk[0] + nstk[1];
		nstk[0] = nstk[1] = 0;
	}
	istk = 0;
	icol = -1;
	colsav[0] = LW_stock_default[0].color;
	colsav[1] = LW_stock_default[1].color;
	istkpt[0] = 0; istkpt[1] = 0;
	cnv = UM_cpln.length_to_cm;
	ncomp = 0;
	readfl = UU_TRUE;
/*
.....Process file
*/
	do
	{
		if (!iadd && istk == totstk) break;
/*
.....Read line
*/
		if (readfl)
		{
			stat = ul_fread(fd,lbuf,sizeof(lbuf),&inum);
/*
...........Create composite stock
...........If currently being created
*/
			if (stat == UX_EOF)
			{
				if (ncomp != 0)
				{
					status = ul_ipv_add_stock(which,LW_STOCK_COMPOS,UU_NULL,
						&idn[which],cids,ncids);
				}
				goto done;
			}
			if (stat != UU_SUCCESS) goto failed;
		}
		readfl = UU_TRUE;
/*
.....Get type of stock to define
*/
		stat = sscanf(lbuf,"%s%lf%lf%lf%lf%lf%lf%lf%lf%lf",styp,&ary[0],&ary[1],
			&ary[2],&ary[3],&ary[4],&ary[5],&ary[6],&ary[7],&ary[8]);
		if (stat < 1) continue;
/*
.....UNITS
*/
		if (strcmp(styp,"UNITS/INCH") == 0)
			UM_cpln.length_to_cm = 1.;
		else if (strcmp(styp,"UNITS/MM") == 0)
			UM_cpln.length_to_cm = 1. / 25.4;
/*
.....COLOR
*/
		else if (strncmp(styp,"COLOR/",6) == 0)
		{
			p = &styp[6];
			nc = strlen(p);
			ul_strip_blanks(p,&nc);
			for (i=0;i<65;i++)
			{
				if (strcmp(p,colors[i]) == 0)
				{
					icol = i-1;
					break;
				}
			}
		}
/*
.....Stock definition
*/
		else
		{
			istk++;
			which = 0;
			if (styp[0] == 'F') which = 1;
			if (iadd)
			{
				if (icol != -1) LW_stock_default[which].color = icol;
			}
			else
				sd = &(slist[which][istkpt[which]]);
			np = 0;
/*
.....Composite stock
*/
			if (strcmp(styp,"COMP") == 0 || strcmp(styp,"FCOMP") == 0)
			{
				if (compfl)
				{
/*
........When adding composites directly to the NCLIPV session
........(STOCK/LOAD,file), then
........the COMPOS entry will be added after its sub-entities
*/
					if (iadd)
					{
						if (stat == 1 || ary[0] == 0) ncomp = 10000;
						else ncomp = ary[0];
						ncids = 0;
						if (idn[which] == 0)
							cmpid = LW_stock_idn[which];
						else
							cmpid = idn[which];
						idn[which] = cmpid + 1;
					}
/*
........When loading a session and storing in the provided array
........then the COMPOS entry will be prior to its sub-entities
*/
					else
					{
						istkpt[which]++;
						sd->type = LW_STOCK_COMPOS;
						sd->color = icol;
						sd->bin = ary[0];
						nstk[which]++;
					}
				}
				else
					istk--;
				continue;
			}
/*
.....Box Stock
*/
			else if (strcmp(styp,"CBOX") == 0 || strcmp(styp,"FCBOX") == 0)
			{
				if (stat != 7) goto failed;
				rpt1 = &ary[0];
				rpt2 = &ary[3];
				UM_cc_exttoint(rpt1,rpt1);
				UM_cc_exttoint(rpt2,rpt2);
				itype = LW_STOCK_BOX;
				np = 6;
			}
/*
.....Cone Stock
*/
			else if (strcmp(styp,"CONE") == 0 || strcmp(styp,"FCONE") == 0)
			{
				if (stat != 9) goto failed;
				rpt1 = &ary[0];
				UM_cc_exttoint(rpt1,rpt1);
				itype = LW_STOCK_CONE;
				np = 8;
			}
/*
.....Cylinder Stock
*/
			else if (strcmp(styp,"CYL") == 0 || strcmp(styp,"FCYL") == 0)
			{
				if (stat != 8) goto failed;
				rpt1 = &ary[0];
				UM_cc_exttoint(rpt1,rpt1);
				itype = LW_STOCK_CYLINDER;
				np = 7;
			}
/*
.....Sphere Stock
*/
			else if (strcmp(styp,"SPHER") == 0 || strcmp(styp,"FSPHER") == 0)
			{
				if (stat != 5) goto failed;
				rpt1 = &ary[0];
				UM_cc_exttoint(rpt1,rpt1);
				itype = LW_STOCK_SPHERE;
				np = 4;
			}
/*
.....Torus Stock
*/
			else if (strcmp(styp,"TORUS") == 0 || strcmp(styp,"FTORUS") == 0)
			{
				if (stat != 9) goto failed;
				rpt1 = &ary[0];
				UM_cc_exttoint(rpt1,rpt1);
				itype = LW_STOCK_TORUS;
				np = 8;
			}
/*
.....Revolved Stock
*/
			else if (strcmp(styp,"CREV") == 0 || strcmp(styp,"FCREV") == 0)
			{
				if (stat != 9) goto failed;
				rpt1 = &ary[0];
				UM_cc_exttoint(rpt1,rpt1);
				uu_list_init(&list,sizeof(UM_coord),200,200);
				if (list.data == UU_NULL) goto failed;
/*
........Get the profile points
*/
				npts = 0;
				do
				{
					status = ul_fread(fd,lbuf,sizeof(lbuf),&inum);
					if (status == UX_EOF) break;
					if (status != UU_SUCCESS) goto failed;
					stat = sscanf(lbuf,"%s%lf%lf%lf",styp,&pts[0],&pts[1],&pts[2]);
					if (stat < 1) continue;
					if (stat != 4 || strcmp(styp,"SPPROF") != 0) break;
					UM_cc_exttoint(pts,pts);
					uu_list_push(&list,pts);
					npts++;
				} while (status == UU_SUCCESS);
				if (npts < 2) goto failed;
/*
........Create the stock
*/
				if (iadd)
					ul_ipv_add_stock(which,LW_STOCK_REVOLVE,ary,&idn[which],
						list.data,npts,1);
				else
				{
					istkpt[which]++;
					sd->type = LW_STOCK_REVOLVE;
					sd->data =
						(UU_REAL *)uu_malloc(sizeof(UU_REAL)*9+sizeof(UM_coord)*npts);
					if (sd->data == UU_NULL) goto failed;
					for (i=0;i<8;i++) sd->data[i] = ary[i];
					sd->data[8] = (UU_REAL)npts;
					sdpt = (UM_coord *)UU_LIST_ARRAY(&list);
					sdpt1 = (UM_coord *)&(sd->data[9]);
					for (i=0;i<npts;i++) um_vctovc(sdpt[i],sdpt1[i]);
					sd->color = icol;
					nstk[which]++;
				}
				uu_list_free(&list);
				np = 0;
				if (strcmp(styp,"EOS") != 0) readfl = UU_FALSE;
			}
/*
.....Sweep Stock
*/
			else if (strcmp(styp,"SPROF") == 0 || strcmp(styp,"FSPROF") == 0)
			{
				if (stat != 7) goto failed;
				uu_list_init(&list,sizeof(UM_coord),200,200);
				if (list.data == UU_NULL) goto failed;
				rpt1 = &ary[3];
				UM_cc_exttoint(rpt1,rpt1);
/*
........Get the profile points
*/
				npts = 0;
				do
				{
					status = ul_fread(fd,lbuf,sizeof(lbuf),&inum);
					if (status == UX_EOF) break;
					if (status != UU_SUCCESS) goto failed;
					stat = sscanf(lbuf,"%s%lf%lf%lf",styp,&pts[0],&pts[1],&pts[2]);
					if (stat < 1) continue;
					if (stat != 4 || strcmp(styp,"SPPROF") != 0) break;
					UM_cc_exttoint(pts,pts);
					uu_list_push(&list,pts);
					npts++;
				} while (status == UU_SUCCESS);
				if (npts < 3) goto failed;
/*
........Create the stock
*/
				if (iadd)
					ul_ipv_add_stock(which,LW_STOCK_SWEEP,&ary[3],&idn[which],
						list.data,npts,1);
				else
				{
					istkpt[which]++;
					sd->type = LW_STOCK_SWEEP;
					sd->data =
						(UU_REAL *)uu_malloc(sizeof(UU_REAL)*4+sizeof(UM_coord)*npts);
					if (sd->data == UU_NULL) goto failed;
					for (i=0;i<3;i++) sd->data[i] = ary[i+3];
					sd->data[3] = (UU_REAL)npts;
					sdpt = (UM_coord *)UU_LIST_ARRAY(&list);
					sdpt1 = (UM_coord *)&(sd->data[4]);
					for (i=0;i<npts;i++) um_vctovc(sdpt[i],sdpt1[i]);
					sd->color = icol;
					nstk[which]++;
				}
				uu_list_free(&list);
				np = 0;
				if (strcmp(styp,"EOS") != 0) readfl = UU_FALSE;
			}
/*
.....STL File
*/
			else if (strcmp(styp,"STL") == 0 || strcmp(styp,"FSTL") == 0)
			{
				stat = sscanf(lbuf,"%s%d%s",styp,&i,filename);
				if (stat != 3) goto failed;
				ary[0] = i;
				ary[1] = 0;
				ary[2] = UU_FALSE;
				if (iadd)
					ul_ipv_add_stock(which,LW_STOCK_FILE,ary,&idn[which],filename,
						strlen(filename),1);
				else
				{
					istkpt[which]++;
					sd->type = LW_STOCK_FILE;
					sd->data =
						(UU_REAL *)uu_malloc(sizeof(char)*(strlen(filename)+1));
					if (sd->data == UU_NULL) goto failed;
					strcpy((char *)sd->data,filename);
					sd->bin = i;
					sd->color = icol;
					nstk[which]++;
				}
			}
/*
........Store standard stock primitives
*/
			if (np != 0)
			{
				if (iadd)
					ul_ipv_add_stock(which,itype,ary,&idn[which],UU_NULL,0,1);
				else
				{
					istkpt[which]++;
					sd->type = itype;
					sd->data = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*np);
					if (sd->data == UU_NULL) goto failed;
					for (i=0;i<np;i++) sd->data[i] = ary[i];
					sd->color = icol;
					nstk[which]++;
				}
			}
/*
........Increment Composite counts
*/
			if (compfl && ncomp != 0 && iadd)
			{
				ncomp--;
				if (idn[which] == 0)
					cids[ncids++] = LW_stock_idn[which] - 1;
				else
					cids[ncids++] = idn[which] - 1;
/*
...........Create composite stock
*/
				if (ncomp == 0)
				{
					idn[which] = cmpid;
					status = ul_ipv_add_stock(which,LW_STOCK_COMPOS,UU_NULL,
						&idn[which],cids,ncids);
				}
			}
		}
	} while(status == UU_SUCCESS);
	goto done;
/*
.....Could not read from file
*/
failed:;
	status = UU_FAILURE;
/*
....End of routine
*/
done:;
	if (iadd)
	{
		LW_stock_default[0].color = colsav[0];
		LW_stock_default[1].color = colsav[1];
	}
	UM_cpln.length_to_cm = cnv;
	return(status);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_save_stl(which,cfil,knc,idn)
**      Saves the defined stocks or fixtures in an external STL file.
**
**   PARAMETERS
**       INPUT  :
**          which   = Type of solid to load.  0 = Stock, 1 = Fixture.
**          idn     = ID number for stock. 0 = Save all stocks/fixtures.
**          cfil    = Filename to save.  If blank then the user will
**                    be prompted for the filename.
**          knc     = Number of characters in 'cfil'.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_save_stl(which,cfil,knc,idn)
int which;
char *cfil;
int knc,idn;
{
	int i,stat,mode,fstat,inum,ifl,inc;
	char sbuf[80],*p,ext[UX_SUFFIX_LEN],descrip[80];
	UX_pathname file,filename,lbuf;
	UM_int2 idx,iunits;
	LW_stock_struc *sd,*sdtmp;
	LtData value;
	LtPrim prim;
	LtSession sess;
	LtSessionPrim sprim;
	FILE *fd;
	static LtSinglePrecisionFormat prec[2] = {
		LI_SO_SINGLE_PRECISION_ASCII, LI_SO_SINGLE_PRECISION_BINARY};
/*
.....NCLIPV must be active to store STL file
*/
	if (!LW_active) goto notact;
/*
.....Make sure at least 1 stock is defined
*/
	if (LW_nstock[which] == 0) goto nogeom;
/*
.....Get filename to save
*/
	if (knc == 0)
	{
		sprintf(sbuf,"Save %s STL File",gname[which]);
		strcpy(ext,"*.stl");
		strcpy(descrip,"STL Files (*.stl)");
		inum = 0;
		file[0] = '\0';
		ud_get_filename(sbuf,sbuf,ext,file,&inum,descrip, 0,UU_FALSE);
		if (inum == 0) goto done;
		ux_add_ftype("stl",file,UX_NPRTERRS);
/*
.....See if file already exists
*/
		mode = UX_EXISTS|UX_READ|UX_WRITE|UX_CREATE;
		stat = ux_file_inquire(UU_NULL,UU_NULL,file,UU_NULL,UU_NULL,
			&mode,&fstat,lbuf,UX_NPRTERRS);
		if (stat != UU_SUCCESS) goto filerr;
/*
.....File already exists
.....Ask if the user wants to overwrite it
*/
		if (mode != (mode|UX_NEXISTS))
		{
			sprintf(lbuf,"%s already exists.\nDo you want to overwrite it",file);
			stat = ud_yesno(0, lbuf, "File exists");
			if (!stat) goto done;
		}
	}
/*
.....Use input file name
*/
	else
	{
		strcpy(file,cfil);
		ux_add_ftype("stl",file,UX_NPRTERRS);
	}
/*
.....Merge coplanar faces
*/
/*	LiDataSetBoolean(&value,TRUE);
	LiControlSet(LI_CONTROL_NU_STL_MERGE_FACES,&value);*/
/*
.....Determine Units setting
*/
	idx = 264;
	getifl(&idx,&iunits);
/*
.....Loop thru stocks & fixtures and
.....write to file
*/
	strcpy(filename,file);
	p = strrchr(filename,'.');
	if (p != UU_NULL) *p = '\0';
	sd = LW_stock_first[which];
	inc = 0;
	for (i=0;i<LW_nstock[which];i++)
	{
		if (idn == 0 || sd->id == idn)
		{
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
				if (ifl == -2) break;
/*
........Open file for writing
*/
				ux_delete(file,UX_NPRTERRS);
				fd = LiFileOpen(file,LI_FILE_WRITE);
				if (fd == UU_NULL) goto filerr;
/*
........Extract primitive
*/
				prim = LiPrimitiveSolidExtract(sdtmp->stock);
				if (iunits == 1) LiPrimitiveSetSolidTransform(prim,LW_xform_inchmm);
/*
........Convert to single precision
*/
				sess = LiSessionCreate(LI_MW_SESSION_SOLID_OPS);
				sprim = LiSessionAddPrim(sess,prim);
				if (sprim != UU_NULL)
				{
					LiSOSolidPrepareSinglePrecision(sprim,prec[LW_stl_format]);
					LiSOSolidStore(sprim);
				}
/*
........Save STL file
*/
				LiSessionDestroy(sess);
				stat = LiMWUSolidSaveToSTLFile(fd,LW_stl_format,prim,UL_program);
				LiPrimitiveDestroy(prim);
				if (stat != LI_STATUS_OK) goto wrterr;
/*
........Point to next stock
*/
				sprintf(sbuf,"_%d.stl",++inc);
				strcpy(file,filename);
				strcat(file,sbuf);
				LiFileClose(fd);
			} while (ifl != -1);
			if (idn != 0) goto done;
		}
		sd = (LW_stock_struc *)uu_lsnext(sd);
	}
	goto done;
/*
.....NCLIPV not active
*/
notact:;
	if (knc == 0)
		ud_wrerr("NCLIPV must be active to store an STL file.");
	goto done;
/*
.....No stocks defined
*/
nogeom:;
	if (knc == 0)
	{
		sprintf(sbuf,"No %ss have been defined.",gname[which]);
		ud_wrerr(sbuf);
	}
	goto done;
/*
.....Could not open file
*/
filerr:;
	if (knc == 0)
	{
		ul_short_filename(file,sbuf,60);
		sprintf(lbuf,"Could not create %s.",sbuf);
		ud_wrerr(lbuf);
	}
	goto done;
/*
.....Error writing to file
*/
wrterr:;
	if (knc == 0)
	{
		ul_short_filename(file,sbuf,60);
		sprintf(lbuf,"Error writing to %s.",sbuf);
		ud_wrerr(lbuf);
		LiFileClose(fd);
	}
	goto done;
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    S_FUNCTION     :  OnBrowse(filedno, val, stat)
**       Routine to get the STL file name.
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
	sprintf(sbuf,"Load %s STL File",gname[Swhich]);
	strcpy(ext,"*.stl");
	strcpy(descrip,"STL Files (*.stl)");
	inum = 0;
	Sfile[0] = '\0';
	ud_get_filename(sbuf,sbuf,ext,Sfile,&inum,descrip, 1,UU_FALSE);
	if (inum != 0)
	{
		ux_add_ftype("stl",Sfile,UX_NPRTERRS);
		if (*fieldno == FBRO)
			ud_update_answer(FSTL,(int *)Sfile);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_load_stl(which)
**      Loads a previously saved stock or fixture STL file.
**
**   PARAMETERS
**       INPUT  : which = Type of solid to load.  0 = Stock, 1 = Fixture.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_load_stl(which)
int which;
{
	LtPrim prim;
	int status;
	int units,nc;
	UU_REAL toler;
/*
.....Setup form fields
*/
	static char traverse[] = {1,1,1,1};
	static UD_METHOD methods[] = {OnBrowse,UU_NULL,UU_NULL,UU_NULL};
	static char called[] = {6,6,6,6};
	static char display[] = {1,1,1,1};
	static int *ans[8];
/*
.....Initialize form
*/
	Sfile[0] = '\0';
	units = LW_stl_units;
	UM_len_inttoext(LW_stl_toler,toler);
	Swhich = which;
	ans[0] = UU_NULL;
	ans[1] = (int *)Sfile;
	ans[2] = &units;
	ans[3] = (int *)&toler;
/*
.....Get the form input
*/
	status = ud_form1("ipvloadstl.frm",ans,ans,methods,called,display,traverse);
/*
.....Load the STL file
*/
	if (status != -1)
	{
		LW_stl_units = units;
		UM_len_exttoint(toler,LW_stl_toler);
		nc = 0;
		ul_ipv_import_stl(which,&Sfile,&nc,&prim,units,toler);
	}
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_import_stl(which,file,idn,prim,units,toler)
**      Loads a previously saved stock or fixture STL file.
**
**   PARAMETERS
**       INPUT  : which = Type of solid to load.  0 = Stock, 1 = Fixture,
**                        2 = Target (for comparison).
**                file  = Non-blank string if filename is furnished.
**                idn   = ID number for stock.
**                units = 0 = Inches, 1 = Millimeters.
**                toler = Tolerance for closing STL model.
**       OUTPUT : file  = Name of STL file.
**                prim  = Solid primitive created.  Destroyed (not used)
**                        if requested stock is not set to Target.
**                idn   = Incremented stock ID number.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_import_stl(which,file,idn,prim,units,toler)
int which,units,*idn;
char *file;
LtPrim *prim;
UU_REAL toler;
{
#define TARGET 2
	int i,stat;
	UU_REAL r[2],tol,rsav;
	char sbuf[80],*p;
	UX_pathname filename,lbuf;
	UU_LOGICAL bin;
	FILE *fd;
	UD_DDATA val;
	LtPrim lprim;
	LtSession sess;
	LtSessionPrim sprim;
/*
.....Get filename to load
*/
	fd = UU_NULL;
	if (strlen(file) == 0)
	{
		i = -1;
		Swhich = which;
		stat = 0;
		OnBrowse(&i,&val,stat);
		if (strlen(Sfile) == 0) goto done;
		strcpy(file,Sfile);
	}
/*
.....Loop thru stocks & fixtures and
.....read from file
*/
	strcpy(filename,file);
	p = strrchr(filename,'.');
	if (p != UU_NULL) *p = '\0';
	if (which == TARGET) tol = units == 0 ? .001 : .0254;
	else tol = toler;
	i = 0;
	do
	{
/*
........Determine if Binary or Ascii file
*/
		stat = ul_ipv_stl_binary(&fd,file,&bin);
		if (stat == UX_NFOUND)
		{
			if (i == 0) goto filerr;
			else break;
		}
		else if (stat == UX_BAD_FILE) goto readerr;
		stat = UU_SUCCESS;
/*
........Read STL file
*/
		if (which == TARGET)
		{
			ul_ipv_open_stl_form(file);
			lprim = LiMWUSolidCreateFromSTLFile(fd,bin,stype[which],FALSE,
				tol/10.,tol*10.);
			if (LW_interrupt == 2) goto interrupt;
			if (lprim == 0) goto readerr;
			if (units == 1)
			{
				LiPrimitiveSetSolidTransform(lprim,LW_xform_mminch);
				sess = LiSessionCreate(LI_MW_SESSION_SOLID_OPS);
				sprim = LiSessionAddPrim(sess,lprim);
				LiSOSolidStore(sprim);
				*prim = LiPrimitiveSolidExtract(sprim);
				LiSessionDestroy(sess);
				LiPrimitiveDestroy(lprim);
			}
			else
				*prim = lprim;
/*
........Test for valid stock file
*/
			if (ul_ipv_stl_test(*prim,file,lbuf) == UU_FAILURE) goto stlerr;
			ul_ipv_close_stl_form();
		}
/*
........Store stock
*/
		if (which == TARGET) break;
		r[0] = bin; r[1] = units;
		rsav = LW_stock_default[which].toler;
		LW_stock_default[which].toler = toler;
		ul_ipv_add_stock(which,LW_STOCK_FILE,r,idn,file,strlen(file),1);
		toler = LW_stock_default[which].toler;
/*
........Point to next stock file
*/
		sprintf(sbuf,"_%d.stl",i);
		strcpy(file,filename);
		strcat(file,sbuf);
		i++;
	} while (stat == UU_SUCCESS);
	goto done;
/*
.....Could not open file
*/
filerr:;
	ul_short_filename(file,sbuf,60);
	sprintf(lbuf,"Could not open %s.",sbuf);
	ud_wrerr(lbuf);
	stat = UU_FAILURE;
	goto done;
/*
.....Error reading from file
*/
readerr:;
	ul_short_filename(file,sbuf,60);
	sprintf(lbuf,"Error reading from %s.",sbuf);
	ud_wrerr(lbuf);
	stat = UU_FAILURE;
	goto done;
/*
.....User interrupted read
*/
interrupt:;
	ud_wrerr("STL load interrupted.");
	stat = UU_FAILURE;
	*prim = 0;
	goto done;
/*
.....Stl file is invalid
*/
stlerr:;
	ud_wrerr(lbuf);
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	ul_ipv_close_stl_form();
	if (fd != UU_NULL) LiFileClose(fd);
	return(stat);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_gen_revsf(origpt,axisvc,pts,vcs,npts,opts,ovcs,
**                                vx,cutter,concave,errfl)
**      This function generates a surface of revolution to use as a 
**      MacineWorks stock or fixture.
**
**   PARAMETERS
**       INPUT  : origpt  = Origin of revsurf.
**                axisvc  = Axis vector of revsurf.
**                pts     = Array of points in curve to be revolved.
**                vcs     = Array of slope vectors in curve to be revolved.
**                npts    = Number of points in 'pts'
**                cutter  = UU_TRUE = Generating a cutter.
**                concave = UU_TRUE = Allow concave cutter.
**                errfl   = UU_TRUE = Output error message if cannot create
**                          revsurf.
**       OUTPUT : origpt  = Updated origin if curve is reversed.
**                pts     = Updated point array if curve is reversed.
**                opts    = Output point array suitable for creating stock.
**                          This array should be large enough to hold npts+2
**                          points.
**                ovcs    = Output vector array suitable for creating cutter.
**                          This array should be large enough to hold npts+2
**                          points.
**                vx      = Starting vector of revsurf.
**                npts    = Number of points in 'opts'
**   RETURNS: UU_FAILURE if could not generate stock.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_gen_revsf(origpt,axisvc,pts,vcs,npts,opts,ovcs,vx,cutter,concave,
	errfl)
UM_coord origpt;
UM_vector axisvc;
Gwpoint3 *pts;
UM_vector *vcs;
UM_coord *opts;
UM_vector *ovcs;
UM_vector vx;
int *npts;
UU_LOGICAL cutter,concave,errfl;
{
	int i,j,stat,np;
	UU_LOGICAL irev,cutfl;
	UM_vector v1,v2,vn;
	UM_coord tpt;
	UM_transf tfmat;
	UU_REAL um_dcccc();
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
	np = *npts;
	um_unitvc(axisvc,axisvc);
	cutfl = cutter;
	if (um_dcccc(&pts[0],&pts[np-1]) < UM_FUZZ) cutfl = UU_FALSE;
/*
.....Reverse points if necessary
*/
	irev = UU_FALSE;
	um_vcmnvc(&pts[0],origpt,v1); um_unitvc(v1,v1);
	um_vcmnvc(&pts[np-1],origpt,v2); um_unitvc(v2,v2);
	if (um_mag(v2) < UM_FUZZ && um_mag(v1) > UM_FUZZ) irev = UU_TRUE;
	if (!irev && um_dot(axisvc,v2) < um_dot(axisvc,v1)) irev = UU_TRUE;
	if (irev)
	{
		for (i=0;i<np/2;i++)
		{
			um_vctovc(&pts[np-i-1],tpt);
			um_vctovc(&pts[i],&pts[np-i-1]);
			um_vctovc(tpt,&pts[i]);
			if (vcs != UU_NULL)
			{
				um_vctovc(&vcs[np-i-1],tpt);
				um_vctovc(&vcs[i],&vcs[np-i-1]);
				um_vctovc(tpt,&vcs[i]);
			}
		}
		if (vcs != UU_NULL)
		{
			for (i=0;i<np;i++) um_vctmsc(vcs[i],-1.,vcs[i]);
		}
/*
.....Move origin point to new start point plane
*/
		um_nptpln(origpt,&pts[0],axisvc,origpt);
	}
/*
.....Get vector to first point
.....from origin
*/
	um_vcmnvc(pts,origpt,v1);
	if (um_mag(v1) < UM_FUZZ) um_vcmnvc(&pts[1],origpt,v1);
/*
.....Calculate a normal vector, then an "X" vector.
*/
	um_cross(v1,axisvc,vn);
	if (um_mag(vn) < UM_FUZZ)
	{
		um_vcmnvc(&pts[1],origpt,vx);
		um_cross(vx,axisvc,vn);
	}
	if (um_mag(vn) < UM_FUZZ) goto err3;
	um_unitvc(vn,vn);
	um_cross(axisvc,vn,vx);
/*
.....Calculate a matrix which will tranform and orient the points at the origin.
*/
	um_chgcstf(origpt,vx,axisvc,vn,Sorigin,Sxaxis,Syaxis,Szaxis,tfmat);
/*
.....Transform point and project onto Z Plane
*/
	for (i=0,j=0;i<np;i++,j++)
	{
		um_cctmtf(&pts[i],&tfmat,&opts[j]);
		um_nptpln(&opts[j],Sorigin,Szaxis,&opts[j]);
		if (vcs != UU_NULL)
		{
			um_vctmtf(&vcs[i],&tfmat,&ovcs[j]);
			um_nptpln(&ovcs[j],Sorigin,Szaxis,&ovcs[j]);
			um_unitvc(&ovcs[j],&ovcs[j]);
		}
/*
.....If defining a cutter,
.....then curve must start on Y-axis
*/
		if (cutfl)
		{
			if (i == 0 && opts[j][0] != 0.0)
			{
				um_vctovc(&opts[0],&opts[1]);
				opts[0][0] = 0.0;
				if (vcs != UU_NULL)
				{
					um_vctovc(&ovcs[0],&ovcs[1]);
					um_vcmnvc(&opts[1],&opts[0],&ovcs[0]);
					um_unitvc(&ovcs[0],&ovcs[0]);
				}
				j++; *npts = *npts + 1;
			}
/*
.....Only allow convex shapes for cutters
*/
			else if (!concave && i > 0 && opts[j][1] < opts[j-1][1])
			{
				opts[j][1] = opts[j-1][1];
			}
		}
	}
	if (opts[j-1][0] < 0.0) opts[j-1][0] = 0.0;
/*
.....If First and Last points are within tolerance
.....then make them the same
*/
	if (um_dcccc(opts[0],opts[j-1]) < UM_FUZZ) um_vctovc(opts[0],opts[j-1]);
/*
.....Add closing point if necessary.
.....Only for Cutters
*/
	if (cutfl)
	{
		if (opts[j-1][0] > UM_FUZZ)
		{
			opts[j][0] = 0.0;
			opts[j][1] = opts[j-1][1];
			opts[j][2] = opts[j-1][2];
			if (vcs != UU_NULL)
			{
				um_vcmnvc(&opts[j],&opts[j-1],&ovcs[j]);
				um_unitvc(&ovcs[j],&ovcs[j]);
			}
			*npts = *npts + 1;
		}
	}
	goto done;
/*
.....Invalid stock coordinates
*/
err3:;
	if (errfl)
		ud_wrerr ("Invalid dimensions for surface of revolution stock.");
	stat = UU_FAILURE;
	goto done;
done:;
	return(stat);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_add_stock(which,flag,stock,idn,pts,npts,sess)
**      This function adds all stock and fixture defintions to the
**      Machining Session.
**   PARAMETERS
**       INPUT  : which = 0 - Stock definition.
**                        1 - Fixture definition.
**                flag  = Type of solid to add (LW_stock_type).
**                stock = Solid geometry modifiers/parameters.
**                idn   = ID number for stock.  If set to 0 then a new stock
**                        number will be generated.  If set to positive,then
**                        the stock will first be deleted if it already exists.
**                pts   = Point data for curve generated solids.
**                npts  = Number of points in 'pts'.
**                sess  = 1 - Start the IPV Machining Session.
**       OUTPUT : idn   = Incremented stock ID number.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_add_stock(which,flag,stock,idn,pts,npts,sess)
UU_REAL *stock;
UM_coord *pts;
LW_stock_type flag;
int npts,*idn;
int which,sess;
{
	int i,stat,numid,*iptr,ifl,inc;
	UU_LOGICAL lmod;
	void ipv_start_session(),ul_ipv_view_stock();
	LW_stock_struc *sd,*sdtmp,*sptr,*sdx;
	UM_coord *sdpt;
	LtSession tsess;
	LtPrim prim,tprim;
	LtSessionPrim sprim;
	LtMatrix mx1;
	LtData data;
char sbuf[80];
/*
.....If IPV is not authorized
.....then just return
*/
	if (UL_ipv == 0) return(UU_SUCCESS);
/*
.....Get next available Stock ID
*/
	if (*idn == 0)
	{
		numid = LW_stock_idn[which];
		do
		{
			ul_ipv_find_stock(which,numid,&sdtmp);
			numid = numid + 1;
		} while (sdtmp != UU_NULL);
		numid = numid - 1;
	}
/*
.....Stock ID is provided
*/
	else
	{
		numid = *idn;
		ul_ipv_find_stock(which,numid,&sdtmp);
/*
.....Stock already exists
*/
		if (sdtmp != UU_NULL)
		{
/*
........New stock is the same as
........previously defined stock
........So just set new attributes
*/
			if (ul_ipv_stock_same(flag,sdtmp,stock,pts,npts))
			{
				if (LW_reset_attr[which])
				{
					sdtmp->color = LW_stock_default[which].color;
					sdtmp->translucency = LW_stock_default[which].translucency;
					sdtmp->visible = LW_stock_default[which].visible;
					sdtmp->active = LW_stock_default[which].active;
					sdtmp->toler = LW_stock_default[which].toler;
					sdtmp->edge = LW_stock_default[which].edge;
					sdtmp->edge_color = LW_stock_default[which].edge_color;
				}
				sdtmp->tpin = LW_mach_tpin_ix;
				sdtmp->mxflag = LW_stock_default[which].mxflag;
				sdtmp->mxchg = UU_TRUE;
				strcpy(sdtmp->mxname,LW_stock_default[which].mxname);
				um_tftotf(LW_stock_default[which].matrix,sdtmp->matrix);
				sdtmp->axes = LW_stock_default[which].axes;
				sdtmp->axes_color = LW_stock_default[which].axes_color;
				ul_ipv_place_stock(sdtmp,UU_FALSE,&lmod);
				if (!lmod) ul_ipv_modify_stock(sdtmp,UU_FALSE);
				*idn = *idn + 1;
				stat = UU_SUCCESS;
				goto done;
			}
/*
........Stocks are different
........Delete old stock
*/
			else
			{
				ul_ipv_remove_stock(which,sdtmp);
			}
		}
	}
/*
.....Save stock parameters
*/
	sd = LW_stock_data[which];
	sd->type = flag;
	sd->id = numid; numid = numid + 1; if (*idn != 0) *idn = numid;
	if (numid > LW_stock_idn[which]) LW_stock_idn[which] = numid;
	sd->color = LW_stock_default[which].color;
	sd->translucency = LW_stock_default[which].translucency;
	sd->visible = LW_stock_default[which].visible;
	sd->active = LW_stock_default[which].active;
	sd->important = LW_stock_default[which].important;
	sd->toler = LW_stock_default[which].toler;
	sd->edge = LW_stock_default[which].edge;
	sd->edge_color = LW_stock_default[which].edge_color;
	sd->tpin = LW_mach_tpin_ix;
	sd->mxflag = LW_stock_default[which].mxflag;
	sd->mxchg = UU_FALSE;
	sd->axes = LW_stock_default[which].axes;
	sd->axes_color = LW_stock_default[which].axes_color;
	sd->stock = 0;
	sd->axis_seg = -1;

	strcpy(sd->mxname,LW_stock_default[which].mxname);
	um_tftotf(LW_stock_default[which].matrix,sd->matrix);
/*
........Box Solid
*/
	if (sd->type == LW_STOCK_BOX)
	{
		sd->data = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*6);
		if (sd->data == UU_NULL) goto failed;
		for (i=0;i<6;i++) sd->data[i] = stock[i];
	}
/*
........Cone, Torus Solid
*/
	else if (sd->type == LW_STOCK_CONE || sd->type == LW_STOCK_TORUS)
	{
		sd->data = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*8);
		if (sd->data == UU_NULL) goto failed;
		for (i=0;i<8;i++) sd->data[i] = stock[i];
	}
/*
........Cylinder Solid
*/
	else if (sd->type == LW_STOCK_CYLINDER)
	{
		sd->data = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*7);
		if (sd->data == UU_NULL) goto failed;
		for (i=0;i<7;i++) sd->data[i] = stock[i];
	}
/*
........Sphere Solid
*/
	else if (sd->type == LW_STOCK_SPHERE)
	{
		sd->data = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*4);
		if (sd->data == UU_NULL) goto failed;
		for (i=0;i<4;i++) sd->data[i] = stock[i];
	}
/*
.....Revolved Solid
*/
	else if (sd->type == LW_STOCK_REVOLVE)
	{
		sd->data = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*9+sizeof(UM_coord)*npts);
		if (sd->data == UU_NULL) goto failed;
		for (i=0;i<8;i++) sd->data[i] = stock[i];
		sd->data[8] = (UU_REAL)npts;
		sdpt = (UM_coord *)&(sd->data[9]);
		for (i=0;i<npts;i++) um_vctovc(pts[i],sdpt[i]);
	}
/*
.....Swept Solid
*/
	else if (sd->type == LW_STOCK_SWEEP)
	{
		sd->data = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*4+sizeof(UM_coord)*npts);
		if (sd->data == UU_NULL) goto failed;
		for (i=0;i<3;i++) sd->data[i] = stock[i];
		sd->data[3] = (UU_REAL)npts;
		sdpt = (UM_coord *)&(sd->data[4]);
		for (i=0;i<npts;i++) um_vctovc(pts[i],sdpt[i]);
	}
/*
.....Session File
*/
	else if (sd->type == LW_STOCK_SESSION)
	{
		sd->data = (UU_REAL *)uu_malloc(sizeof(char)*(npts+1));
		if (sd->data == UU_NULL) goto failed;
		strcpy((char *)sd->data,(char *)pts);
	}
/*
.....STL File
*/
	else if (sd->type == LW_STOCK_FILE)
	{
		sd->data = (UU_REAL *)uu_malloc(sizeof(char)*(npts+1));
		if (sd->data == UU_NULL) goto failed;
		strcpy((char *)sd->data,(char *)pts);
		sd->bin = (UU_LOGICAL)stock[0];
		sd->units = (int)stock[1];
	}
/*
.....Composite stock
*/
	else if (sd->type == LW_STOCK_COMPOS)
	{
		iptr = (int *)pts;
/*
........Get actual count of stocks
........including any composite stocks
*/
		sd->bin = 0;
		for (i=0;i<npts;i++)
		{
			ul_ipv_find_stock(which,iptr[i],&sdtmp);
			if (sdtmp == UU_NULL) break;
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sdtmp,&sdx,&ifl,UU_FALSE);
				if (ifl == -2) break;
				sd->bin++;
			} while (ifl != -1);
		}
		sd->data = (UU_REAL *)uu_malloc(sizeof(LW_stock_struc)*sd->bin);
		if (sd->data == UU_NULL) goto failed;
		sptr = (LW_stock_struc *)sd->data;
		if (sd->mxflag)
		{
			sd->invflag = UU_TRUE;
			um_inverttf(sd->matrix,sd->invmx);
		}
		else
		{
			sd->invflag = UU_FALSE;
			um_identtf(sd->invmx);
		}
		inc = 0;
		for (i=0;i<npts;i++)
		{
			ul_ipv_find_stock(which,iptr[i],&sdtmp);
			if (sdtmp == UU_NULL) goto failed;
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sdtmp,&sdx,&ifl,UU_FALSE);
				if (ifl == -2) break;
				sptr[inc] = *sdx;
/*
........Delete the display axes for each stock
*/
				sptr[inc].axes = UU_FALSE;
				ul_delaxis_ipv(&sptr[inc]);
/*
........The transformation matrix must be reset
........when placing a stock in a composite stock
*/
				if (LW_active && sptr[inc].mxflag)
				{
					prim = LiPrimitiveSolidExtract(sptr[inc].stock);
					ncl_43mx_to_44mx(sptr[inc].matrix,mx1);
					sptr[inc].xform = LiTransformDefine(mx1);
					LiPrimitiveSetSolidTransform(prim,sptr[inc].xform);
					LiTransformDestroy(sptr[inc].xform);
/*
					tsess = LiSessionCreate(LI_MW_SESSION_SOLID_OPS);
					sprim = LiSessionAddPrim(tsess,prim);
					LiSOSolidStore(sprim);
					tprim = LiPrimitiveSolidExtract(sprim);
					LiSessionDestroy(tsess);
					LiPrimitiveDestroy(prim);
*/
					ul_ipv_remove_prim(&sptr[inc]);
					S_init_mx(&sptr[inc]);
					sptr[inc].prim = prim;
					sptr[inc].stock = LiSessionAddPrim(LW_session[0],sptr[inc].prim);
					ul_ipv_modify_stock(&sptr[inc],UU_FALSE);
				}
/*
........Store COMPOS matrix in each sub-stock
*/
				um_tftotf(sd->matrix,sptr[inc].matrix);
				um_tftotf(sd->invmx,sptr[inc].invmx);
				sptr[inc].mxflag = sd->mxflag;
				sptr[inc].mxchg = sd->mxchg;
				sptr[inc].invflag = sd->invflag;
				strcpy(sptr[inc].mxname,sd->mxname);
				sptr[inc].xform = 0;
				inc++;
			} while (ifl != -1);
			ul_ipv_delist_stock(which,sdtmp);
		}
	}
/*
.....Allocate next stock
*/
	LW_stock_data[which] = (LW_stock_struc *)uu_lsinsrt(LW_stock_data[which],
		sizeof(LW_stock_struc));
	if (LW_stock_data[which] == UU_NULL) goto failed;
	LW_nstock[which]++;
/*
.....Create stock
*/
	stat = UU_SUCCESS;
	if (sd->type != LW_STOCK_COMPOS)
	{
		if (LW_active || LW_session[LW_mach_mode] != 0)
			stat = ul_ipv_create_stock(sd,which,sess);
/*
.....View the stock
*/
		ul_ipv_view_stock(which,sd);
	}
/*
......Display axes for Composite stock
*/
	else
	{
		if ((sd->axes)&&(sd->axes_color!=0))
			sd->axis_seg = ul_ipv_create_axis(sd);
	}
	goto done;
/*
.....Failed to allocate memory
*/
failed:;
	ud_wrerr("Could not allocate memory for stock.");
	stat = UU_FAILURE;
/*
.....End of routine
*/
done:;
	return(stat);
}

/*********************************************************************
**   E_FUNCTION:int ul_ipv_create_stock(sd,type,sess)
**      This function adds all stock and fixture defintions to the
**      Machinining Session.
**   PARAMETERS
**       INPUT  : sd    = Pointer to stock definition.
**                type  = 0 - Stock definition.
**                        1 - Fixture definition.
**                        2 - Machine assembly.
**                        3 - General primitive (Tools, etc.).
**                sess  = 1 - Start the IPV Machining Session.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_create_stock(sd,type,sess)
LW_stock_struc *sd;
int type,sess;
{
	LtPrim revprof,lprim;
	LtDoubleBounds stock;
	int npts,i,stat,which,isav[LW_MAX_SPINDLE];
	UU_LOGICAL iflag,lmod;
	UM_int2 ifli,ifl86;
	UX_pathname lbuf,file;
	char sbuf[80];
	FILE *fd;
	UU_REAL plane[4];
	UU_LOGICAL bin;
	UM_coord *sdpt,*pts,origpt;
	UM_vector xaxis,yaxis,zaxis,vx;
	UM_transf tfmat;
	LW_stock_struc *sx,sdtmp;
	LtMatrix mx1;
	LtSession lsess;
	LtSessionPrim sprim;
	struct UM_solid_rec solid;
/*
.....Initialize routine
*/
	which = type;
	if (type == 2) which = 1;
/*
.....Box Solid
*/
	switch (sd->type)
	{
	case LW_STOCK_BOX:
		stock[LI_MINX] = sd->data[0];
		stock[LI_MINY] = sd->data[1];
		stock[LI_MINZ] = sd->data[2];
		stock[LI_MAXX] = sd->data[3];
		stock[LI_MAXY] = sd->data[4];
		stock[LI_MAXZ] = sd->data[5];
		sd->prim = LiPrimitiveCreateBoxSolid(stock,stype[which]);
		break;
/*
.....Revolved Solid
*/
	case LW_STOCK_REVOLVE:
	case LW_STOCK_CONE:
	case LW_STOCK_CYLINDER:
	case LW_STOCK_SPHERE:
	case LW_STOCK_TORUS:
		sx = sd;
		if (sd->type != LW_STOCK_REVOLVE) sx = &sdtmp;
/*
........Convert solid primitive to revolved solid
*/
		if (sd->type == LW_STOCK_CONE)
		{
			solid.key = 0;
			solid.type = UM_CONE_SOLID;
			ncl_calc_cyl_list(&solid,sd->data,2,UU_NULL,UU_NULL,&sx->data);
		}
		else if (sd->type == LW_STOCK_CYLINDER)
		{
			solid.key = 0;
			solid.type = UM_CYLINDER_SOLID;
			ncl_calc_cyl_list(&solid,sd->data,2,UU_NULL,UU_NULL,&sx->data);
		}
		else if (sd->type == LW_STOCK_SPHERE)
		{
			solid.key = 0;
			solid.type = UM_SPHERE_SOLID;
			ncl_calc_sphere_list(&solid,sd->data,2,UU_NULL,UU_NULL,&sx->data);
		}
		else if (sd->type == LW_STOCK_TORUS)
		{
			solid.key = 0;
			solid.type = UM_TORUS_SOLID;
			ncl_calc_torus_list(&solid,sd->data,2,UU_NULL,UU_NULL,&sx->data);
		}
		sdpt = (UM_coord *)&(sx->data[9]);
		npts = (int)sx->data[8];
		pts = (UM_coord *)uu_malloc(sizeof(UM_coord)*(npts+2));
		if (pts == UU_NULL) goto nomem;
		stat = ul_ipv_gen_revsf(&(sx->data[0]),&(sx->data[3]),(Gwpoint3 *)sdpt,
			0,&npts,pts,0,vx,UU_FALSE,UU_FALSE,UU_TRUE);
		if (stat != UU_SUCCESS) goto done;
		revprof = LiProfileCreate(pts[0]);
		for (i=1;i<npts;i++)
			LiProfileAddLine(revprof,pts[i],FALSE);
		sd->prim = LiPrimitiveCreateRotatedSolid(revprof,
			&(sx->data[0]),&(sx->data[3]),sd->toler,stype[which]);
		uu_free(pts);
		if (sd->type != LW_STOCK_REVOLVE) uu_free(sx->data);
		LiProfileDestroy(revprof);
		break;
/*
.....Swept Solid
*/
	case LW_STOCK_SWEEP:
		sdpt = (UM_coord *)&(sd->data[4]);
		npts = (int)sd->data[3];
		pts = (UM_coord *)uu_malloc(sizeof(UM_coord)*npts);
		if (pts == UU_NULL) goto nomem;
/*
........Determine plane of curve
*/
		if (!(um_planar_curve(sdpt,npts,plane,xaxis,yaxis))) goto notplane;
/*
........Calculate matrix which will transform points to XY-plane
*/
		um_chgcstf(sdpt[0],xaxis,yaxis,plane,Sorigin,Sxaxis,Syaxis,
			Szaxis,tfmat);
/*
........Transform points
*/
		um_vctovc(sdpt[0],origpt);
		um_vctmtf(&(sd->data[0]),tfmat,zaxis);
		for (i=0;i<npts;i++) um_cctmtf(sdpt[i],tfmat,pts[i]);
		pts[npts-1][0] = pts[0][0];
		pts[npts-1][1] = pts[0][1];
		pts[npts-1][2] = pts[0][2];
/*
........Determine if points need to be reversed
*/
		ul_ipv_stock_dir(pts,npts);
/*
........Create swept surface
*/
		revprof = LiProfileCreate(pts[0]);
		for (i=1;i<npts;i++)
			LiProfileAddLine(revprof,pts[i],FALSE);
		sd->prim = LiPrimitiveCreateExtrudedSolid(revprof,
			zaxis,xaxis,yaxis,origpt,sd->toler,stype[which]);
		uu_free(pts);
		LiProfileDestroy(revprof);
		break;
/*
.....Saved Session
*/
	case LW_STOCK_SESSION:
		strcpy(file,(char *)sd->data);
		fd = LiFileOpen(file,LI_FILE_READ);
		if (fd == UU_NULL) goto filerr;
		sd->prim = LiMWUViSolidRestoreFromFile(fd,TRUE,stype[which]);
		LiFileClose(fd);
		break;
/*
.....STL File
*/
	case LW_STOCK_FILE:
/*
........Open file for reading
*/
		strcpy(file,(char *)sd->data);
		bin = (UU_LOGICAL)sd->bin;
		if (bin)
			stat = ux_fopen0(file,"rb",&fd);
		else
			stat = ux_fopen0(file,"r",&fd);
		if (stat != UU_SUCCESS || fd == UU_NULL) goto filerr;
/*
........Display STL file form
*/
		ul_ipv_open_stl_form(file);
/*
........Read STL file
*/
		lprim = LiMWUSolidCreateFromSTLFile(fd,bin,stype[which],FALSE,
			sd->toler/10.,sd->toler*10.);
		ux_fclose0(fd);
		if (LW_interrupt == 2) goto interrupt;
		if (lprim == 0) goto readerr;
		if (sd->units == 1 && type != 3)
		{
			LiPrimitiveSetSolidTransform(lprim,LW_xform_mminch);
			lsess = LiSessionCreate(LI_MW_SESSION_SOLID_OPS);
			sprim = LiSessionAddPrim(lsess,lprim);
			LiSOSolidStore(sprim);
			sd->prim = LiPrimitiveSolidExtract(sprim);
			LiSessionDestroy(lsess);
			LiPrimitiveDestroy(lprim);
		}
		else
			sd->prim = lprim;
/*
........Test for valid stock file
*/
		ul_ipv_stock_test(sd);
		ul_ipv_close_stl_form();
		break;
	}
/*
.....Failed to create stock
*/
	if (sd->prim == 0)
	{
		stat = UU_FAILURE;
		goto done;
	}
/*
.....Set the primitive's color
*/
	LiPrimitiveSetMaterial(sd->prim,LW_material[sd->color]);
/*
.....Make sure only 1 stock and no fixtures
.....are defined for RapidCut
*/
	if (LW_mach_mode == LW_RAPIDCUT && which == 1) goto rcerr;
/*
.....Save the active tool
*/
	for (i=0;i<LW_spindle_num;i++) isav[i] = LW_act_tool[i];
/*
.....Apply the stock transformation
*/
	sd->invflag = UU_FALSE;
	sd->mxchg = UU_FALSE;
	if (sd->mxflag)
	{
		if (type != 2)
			sd->mxchg = UU_TRUE;
		else
		{
/*			LiViSolidSetEnabled(sd->stock,TRUE);*/
			NITMX(sd->matrix);
/*			ul_ipv_place_solid(sd->stock,sd->matrix);*/
			ncl_43mx_to_44mx(sd->matrix,mx1);
			sd->xform = LiTransformDefine(mx1);
			if (sd->xform == 0) goto nomx;
			LiPrimitiveSetSolidTransform(sd->prim,sd->xform);
			LiTransformDestroy(sd->xform);
			sd->invflag = UU_TRUE;
			um_inverttf(sd->matrix,sd->invmx);
		}
	}
/*
.....Add the primitive to the session
*/
	if (which != 3 && LW_session[LW_mach_mode] != 0)
	{
/*
......Lathe
*/
		if (LW_mach_type == LW_LATHE && !LW_mach_simul && LW_lathe != 0)
		{
			iflag = ul_ipv_lathe_stop();
			sd->stock = LiSessionAddPrim(LW_session[LW_mach_mode],sd->prim);
			LiViLatheAddSolid(LW_lathe,sd->stock);
			if (iflag) ul_ipv_lathe_start();
		}
		else
		{
			sd->stock = LiSessionAddPrim(LW_session[LW_mach_mode],sd->prim);
			if (sess == 1)
			{
				if (LW_session[LW_mach_mode] == 0)
					ul_ipv_start_session(UU_FALSE,UU_TRUE);
				else
					if (LW_nstock[0]+LW_nstock[1] == 0)
						ul_ipv_reset_session(UU_FALSE);
			}
		}
	}
/*
.....Restore the active tool
*/
	for (i=0;i<LW_spindle_num;i++) LW_act_tool[i] = isav[i];
/*
.....Set the stock attributes
*/
	ul_ipv_set_stk_attr(sd);
/*
.....Section revolved solid
*/
	ul_ipv_stock_trim(sd);
/*
.....Place the stock on the machine
*/
	sd->placed = UU_FALSE;
	ul_ipv_place_stock(sd,UU_FALSE,&lmod);
/*
.....Update important solids
*/
	ul_ipv_display_obstruct(UU_FALSE);
/*
......create stock axis segment
*/
	if ((sd->axes)&&(sd->axes_color!=0))
		sd->axis_seg = ul_ipv_create_axis(sd);
/*
.....Display the solid
*/
	if (LW_active)
	{
		ul_ipv_flush();
		um_reset_pocket_graphics(UM_IPV_WINDOW);
	}
	stat = UU_SUCCESS;
	goto done;
/*
.....Could not allocate memory
*/
nomem:;
	ud_wrerr("Could not allocate memory for stock.");
	stat = UU_FAILURE;
	goto done;
/*
.....Curve is not planar
*/
notplane:;
	ud_wrerr("Input curve is not coplanar.");
	stat = UU_FAILURE;
	goto done;
/*
.....Could not create transformation matrix
*/
nomx:;
	ud_wrerr("Could not create transformation matrix.");
	stat = UU_FAILURE;
	goto done;
/*
.....Could not open file
*/
filerr:;
	ul_short_filename(file,sbuf,60);
	sprintf(lbuf,"Could not open %s.",sbuf);
	ud_wrerr(lbuf);
	stat = UU_FAILURE;
	goto done;
/*
.....Error reading from file
*/
readerr:;
	ul_short_filename(file,sbuf,60);
	sprintf(lbuf,"Error reading from %s.",sbuf);
	ud_wrerr(lbuf);
	stat = UU_FAILURE;
	goto done;
/*
.....User interrupted read
*/
interrupt:;
	ud_wrerr("STL load interrupted.");
	stat = UU_FAILURE;
	goto done;
/*
.....Only 1 stock allowed in RapidCut
*/
rcerr:;
	ul_ipv_write_log("RAPIDCUT- RapidCut allows for only 1 stock and no fixtures.");
	LW_errors++;
	stat = UU_FAILURE;
   goto done;
/*
.....End of routine
*/
done:;
	ul_ipv_close_stl_form();
	return(stat);
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_modify_stock(stock,change_faces)
**      This function modifies the attributes of an existing stock.
**   PARAMETERS
**       INPUT  :
**          stock        = Solid geometry modifiers/parameters.
**          change_faces = UU_TRUE = change color of cut faces to match
**                         stock color.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_modify_stock(stock,change_faces)
LW_stock_struc *stock;
UU_LOGICAL change_faces;
{
	int *m,dinc;
	UN_mot_data *dd;
	UM_transf tfmat1,tf;
	LW_mach_toolpin_struc *tptr;
	LtData stuff;
/*
.....Only modify if NCLIPV is active
*/
	if (LW_session[LW_mach_mode] != 0)
	{
/*
.....Apply the stock transformation
*/
		m = UU_NULL;
		if (stock->axis_seg!=-1)
			ul_delaxis_ipv(stock);
		if (stock->mxchg && stock->active)
		{
/*
.....Simply reset the session in RapidCut
.....Only way to do it if session is already started
.....otherwise matrix will not be applied
*/
			if (LW_mach_mode == LW_RAPIDCUT)
			{
				ul_ipv_reset_session();
				goto done;
			}
/*
.....Visicut can modify the matrix
.....during an active session and
.....maintain the cut paramters
*/
			stock->mxchg = UU_FALSE;
/*
.....Detach stock from machine
*/
			ul_ipv_detach_assembly(stock);
/*
.....Set stock using new matrix
*/
			tptr = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
			if (stock->mxflag)
			{
				NITMX(stock->matrix);
				if (LW_mach_defined)
				{
					um_tftmtf(stock->matrix,tptr[stock->tpin].matrix,tfmat1);
				}
				else
				{
					um_tftotf(stock->matrix,tfmat1);
				}
				ul_ipv_place_solid(stock->stock,tfmat1);
				um_inverttf(stock->matrix,stock->invmx);
				stock->invflag = UU_TRUE;
			}
/*
.....Reset stock to original position
*/
			else if (stock->invflag)
			{
				{
					if (LW_mach_defined)
					{
						um_identtf(tf);
						um_tftmtf(tptr[stock->tpin].matrix,tf,tfmat1);
					}
					else
					{
						um_identtf(tfmat1);
					}
					ul_ipv_place_solid(stock->stock,tfmat1);
				}
				stock->invflag = UU_FALSE;
			}
/*
.....Join stock/fixture to machine
*/
			ul_ipv_place_assembly(stock);
		}
/*
.....Set the material color
*/
		LiDataSetGenericPtr(&stuff,LW_material[stock->color]);
		LiSessionPrimSetVisualProperty(stock->stock,LI_MW_VIS_PROP_MATERIAL,
			&stuff);
/*
........Change color of cut faces also
*/
		if (change_faces) ul_ipv_color_faces(stock);
/*
.....Set the attributes
*/
		if (LW_mach_mode == LW_VISICUT)
		{
			ul_ipv_set_stk_attr(stock);
		}			
		if ((stock->axes)&&(stock->axes_color!=0))
		{
/*
......create stock axis segment
*/
			stock->axis_seg = ul_ipv_create_axis(stock);
		}
/*
.....Restore the face colors
*/
		ul_ipv_flush();
		if (m != UU_NULL)
		{
			dd = LW_mot_data_first;
			dd = (UN_mot_data *)uu_lsnext(dd);
			dinc = 0;
			ul_ipv_restore_faces(stock->stock,m,&dd,&dinc);
			uu_free(m);
		}
/*
.....Display the new stock
*/
		ul_ipv_flush();
		um_reset_pocket_graphics(UM_IPV_WINDOW);
	}
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_place_solid(prim,tf)
**      This function transforms a solid through a matrix.
**   PARAMETERS
**       INPUT  : prim  = Solid to transform.
**                tf    = Matrix to transform solid through.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_place_solid(solid,tf)
LtSessionPrim solid;
UM_transf tf;
{
	LtDoublePoint pt1;
	LtDoubleVector vc1,vc2;
/*
.....Calculate the transformation geometry
.....based on input matrix
*/
	um_cctmtf(Sorigin,tf,pt1);
	um_vctmtf(Sxaxis,tf,vc1);
	um_vctmtf(Szaxis,tf,vc2);
/*
.....Make sure vectors are orthogonal
*/
	um_vcortho(vc2,vc1);
/*
.....Transform solid
*/
	LiViSolidSetAxes(solid,vc1,vc2);
	LiViSolidSetPosition(solid,pt1);
}

/*********************************************************************
**   I_FUNCTION: S_init_mx(sd)
**      This function resets a stock matrix and its flags to the
**      default mode (identity matrix).
**   PARAMETERS
**       INPUT  :
**          sd    = Stock to initialize the matrix for.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void S_init_mx(sd)
LW_stock_struc *sd;
{
/*
.....Initialize matrix
*/
	um_identtf(sd->matrix); um_identtf(sd->invmx);
	sd->mxflag = UU_FALSE;
	sd->mxchg = UU_FALSE;
	sd->invflag = UU_FALSE;
	sd->mxname[0] = '\0';
	sd->xform = 0;
}

