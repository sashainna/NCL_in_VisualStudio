/*********************************************************************
**    NAME         :  netoolib.c
**       CONTAINS:
**			ncl_load_tool
**			ncl_toollist_init
**			ncl_store_toolhead(tool_head)
**			ncl_store_tool(tool_data)
**			ncl_getcurrent_tooldata (tool_data)
**			ncl_gettool_head (tool_head)
**			ncl_get_tooldata (tool, data)
**			int ncl_idx_tooldata(indx, tool, type, disp)
**			ncl_inittool_data (tool_data)
**			ncl_tool_initunit(orig_unit)
**			ncl_tool_inithead()
**			ncl_init_tldata()
**			ncl_reinit_tldata()
**			ncl_tool_reinitlib()
**			ncl_del_toollist()
**			ncl_get_toolist_pt(tool_list)
**			ncl_cutool
**			ncl_findtl
**			ncl_gettparms
**			ncl_load_toolbat
**			ncl_parse_loadtl
**			ncl_parse_cutter
**			ncl_parse_values(str, ary, inum)
**			ncl_trf_tooldata
**			ncl_setcut_outfl
**			ncl_resetcut_outfl
**			ncl_getcut_outfl
**       ncl_setcut_proffl()
**       ncl_resetcut_proffl()
**       ncl_getcut_proffl()
**       
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       netoolib.c , 25.6
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 10:45:58
*********************************************************************/

#include "stdio.h"
#include "usysdef.h"
#include "lcom.h"
#include "mfort.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "toolibdata.h"
#include "nclfc.h"
#include "nclver.h"
#include "umoveb.h"
#include "udforms.h"

void ncl_del_toollist();
void ncl_tool_reinitlib();
/*
.....save the current tool head/data
*/
static struct TL_tooldata_rec NCL_tool_data;
static struct TL_toolhead_rec NCL_tool_head;
static struct TL_tooldata_rec *TOOL_data_list = UU_NULL;
static int NCL_tool_unit = 1;
static UU_LOGICAL Soutflg=UU_FALSE;
void ncl_tool_inithead();
void ncl_parse_loadtl(char*);
void ncl_inittool_data (struct TL_tooldata_rec *);
void ncl_inittool_data95 (struct TL_tooldata_rec95 *);
static UU_LOGICAL Sprof_flg = UU_FALSE;
			
/*********************************************************************
**    I_FUNCTION     : 		ncl_trf_tooldata(tooldata, tool_data, size)
**       Convert a Toolib Data from TOOL_data structure to current 64 bit data structure 
**    PARAMETERS
**       INPUT  :
**				tooldata: general data structure
**				size: size of the data
**       OUTPUT :
**				tool_data: current data structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_trf_tooldata(tooldata, tool_data, size)
struct TOOL_data *tooldata;
struct TL_tooldata_rec *tool_data;
int size;
{
	int i;
	struct TL_tooldata_rec locrec;
	int *ptro, *ptri;
/*
.....this 'size' of "struct TL_tooldata_rec" is always 32 bit size
*/
	uu_move_byte(tooldata, &locrec, size);
	uu_move_byte(tooldata, tool_data, size);
	ptri = &(locrec.no_loadtl);
	ptro = &(tool_data->no_loadtl);
	for (i=0; i<NJOINS;i++)
	{
		ptro[i*4] = ptri[i*2];
	}
}
/*********************************************************************
**    I_FUNCTION     : 		ncl_updtool_data(tooldata, 	size, tool_data, version);
**       Convert a Toolib Data from TOOL_data structure to current used data structure 
**    PARAMETERS
**       INPUT  :
**				tooldata: general data structure
**				size: size of the data
**				version: input data version
**       OUTPUT :
**				tool_data: current data structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_updtool_data(tooldata, size, tool_data, version)
struct TOOL_data *tooldata;
struct TL_tooldata_rec *tool_data;
UU_REAL version;
int size;
{
	int i;
	struct TL_tooldata_rec95 tool_data95;

	if (version<9.6)
	{
		uu_move_byte(tooldata, &tool_data95, size);
		strcpy(tool_data->description, tool_data95.description);
		strcpy(tool_data->symbol, tool_data95.symbol);
		strcpy(tool_data->symshk, tool_data95.symshk);
		strcpy(tool_data->symhld, tool_data95.symhld);
		strcpy(tool_data->drawing, tool_data95.drawing);
		strcpy(tool_data->major, tool_data95.major);
		tool_data->toolno = tool_data95.toolno;
		tool_data->ncvals = tool_data95.ncvals;
		tool_data->ndvals = tool_data95.ndvals;
		tool_data->ctype = tool_data95.ctype;
		tool_data->shade = tool_data95.shade;
		tool_data->sshade = tool_data95.sshade;
		tool_data->hshade = tool_data95.hshade;
		tool_data->segments = tool_data95.segments;
		tool_data->move = tool_data95.move;
		for (i=0; i<6;i++)
		{
			tool_data->cparms[i] = tool_data95.cparms[i];
			tool_data->dparms[i] = tool_data95.dparms[i];
			tool_data->cutter[i] = tool_data95.cutter[i];
			tool_data->pseudo[i] = tool_data95.pseudo[i];
		}
		for (i=0; i<4;i++)
		{
			tool_data->yparms[i] = tool_data95.yparms[i];
			tool_data->sparms[i] = tool_data95.sparms[i];
			tool_data->hparms[i] = tool_data95.hparms[i];
			tool_data->catt[i] = tool_data95.catt[i];
			tool_data->satt[i] = tool_data95.satt[i];
			tool_data->hatt[i] = tool_data95.hatt[i];
		}
		tool_data->fpseudo = tool_data95.fpseudo;
		tool_data->fsymbol = tool_data95.fsymbol;
		tool_data->fshank = tool_data95.fshank;
		tool_data->fholder = tool_data95.fholder;
		tool_data->floadtl = tool_data95.floadtl;
		tool_data->no_loadtl = tool_data95.no_loadtl;
		tool_data->loadtl = tool_data95.loadtl;
		tool_data->no_plabel = tool_data95.no_plabel;
		tool_data->plabel = tool_data95.plabel;
		tool_data->no_command = tool_data95.no_command;
		tool_data->command = tool_data95.command;
	}
	else if(size==sizeof(struct TL_tooldata_rec))
/*
......32bit 
*/
		uu_move_byte(tooldata, tool_data, size);
	else
/*
......64bit
*/
		ncl_trf_tooldata(tooldata, tool_data, size);
}
/*********************************************************************
**    I_FUNCTION     : 		ncl_updtool_head(tooldata, 	size, tool_head, version);
**       Convert a Toolib Data from TOOL_data (char strings) structure to current used head structure 
**    PARAMETERS
**       INPUT  :
**				tooldata: general data structure
**				size: size of the data
**				version: input data version
**       OUTPUT :
**				tool_head: current head structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_updtool_head(tooldata, size, tool_head, version)
struct TOOL_data *tooldata;
struct TL_toolhead_rec *tool_head;
UU_REAL version;
int size;
{
	struct TL_toolhead_rec95 tool_head95;
	struct TL_toolhead_rec102 tool_head102;
	
	if (version<9.6)
	{
		uu_move_byte(tooldata, &tool_head95, size);
		strcpy(tool_head->create_date, tool_head95.create_date);
		strcpy(tool_head->create_time, tool_head95.create_time);
		strcpy(tool_head->mod_date, tool_head95.mod_date);
		strcpy(tool_head->mod_time, tool_head95.mod_time);
		strcpy(tool_head->name, tool_head95.name);
		strcpy(tool_head->description, tool_head95.description);
		strcpy(tool_head->symlib, tool_head95.symlib);
		strcpy(tool_head->proflib, tool_head95.proflib);
		tool_head->units = tool_head95.units;
		tool_head->no_tools = tool_head95.no_tools;
		tool_head->low_tool = tool_head95.low_tool;
		tool_head->high_tool = tool_head95.high_tool;
		tool_head->version = tool_head95.version;
	}
	else
	{
		if (size==HEADREC_SIZE32_102)
		{
			uu_move_byte(tooldata, &tool_head102, size);
			strcpy(tool_head->create_date, tool_head102.create_date);
			strcpy(tool_head->create_time, tool_head102.create_time);
			strcpy(tool_head->mod_date, tool_head102.mod_date);
			strcpy(tool_head->mod_time, tool_head102.mod_time);
			strcpy(tool_head->name, tool_head102.name);
			strcpy(tool_head->description, tool_head102.description);
			strcpy(tool_head->symlib, tool_head102.symlib);
			strcpy(tool_head->proflib, tool_head102.proflib);
			tool_head->units = tool_head102.units;
			tool_head->no_tools = tool_head102.no_tools;
			tool_head->low_tool = tool_head102.low_tool;
			tool_head->high_tool = tool_head102.high_tool;
			tool_head->version = tool_head102.version;
			tool_head->utype_no = 0;
			tool_head->utype = NULL;
		}
		else
		{
			uu_move_byte(tooldata, tool_head, size);
			tool_head->utype = NULL;
		}
	}
/*
.....new version changed data structure Yurong 0716
.....need use new header
*/
	strcpy (tool_head->head,"10155UNICAD FILE HEADER DATA\n");
/*	strcpy (tool_head->head,"67890UNICAD FILE HEADER DATA\n"); */
}
/*********************************************************************
**    I_FUNCTION     : ncl_load_tool(libfile)
**       Loads the tools from the toolib file.
**    PARAMETERS
**       INPUT  :
**				libfile: toolib file to be load
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_load_tool(libfile)
char *libfile;
{
	int lrecl, nb, nc;
	int      iostat; 
	FILE *fstream;
	char msg[400], headstr[40], cmsg[80];
	int nc1, nc2, nc3, save_tool;
	char tlib[256], disp[42], slib[256], savestr[1000], *tok;
	double version;
	char create_date[12], create_time[12], mod_date[12], mod_time[12];
	int units;
/*
......used by fortain function, still int for toolno
*/
	int toolno, low_tool, high_tool;
	int i, k, len, isize, eof, ii;
	int *tool_num_list;
	int knc[20],knc2[20], knc3[200], knc4[20], parms[20], mnc;

	char lib[UX_MAX_PATH_LEN],ltool[42],major[8];
	UM_f77_str f77_str, f77_str2;
	UM_int4 ktool,kerr,ctype,ntl;
	UM_f77_str f77_lib,f77_tool, f77_major;
	UM_f77_str fstr[20], fprms[20], fcmds[200], ftlcs[20];
	char fcstr[20][20],cparms[20][80], ccoms[200][80], ctlcs[20][22];
	char drawng[30];
	UM_int4 iary[20],nary,ierr, comnum, tempcprm[6], tempdprm[6];
	UM_real8 tempcbuf[6],tempdbuf[20];
	int toolnum;
	char symtemp[80], tempstr[8000];
	UM_int4 tempicfl[10];
	struct TL_tooldata_rec tool_data;
	struct TL_tooldata_rec95 tool_data95;
	struct TOOL_data tooldata;

	ncl_tool_reinitlib();
/*
.....Open NCL Tool Library &
.....Initiate search for old version
*/
	UM_init_f77_str(f77_lib,lib,UX_MAX_PATH_LEN);
	strcpy(lib, libfile);

	fstream = fopen(libfile, "rb");
	if(fstream == 0) 
	{
		sprintf(msg, "Can't open file %s", libfile);
		ud_wrerr(msg);
		return -1;
	}
	strcpy (NCL_tool_head.name,libfile);
	strcpy (NCL_tool_head.head,"67890UNICAD FILE HEADER DATA\n");

	ux_fgets1(headstr, 30, fstream, 1);
	if ((strcmp(headstr,"12345UNICAD FILE HEADER DATA\n") != 0) /* version 9.5 */
		&& (strcmp(headstr,"67890UNICAD FILE HEADER DATA\n") != 0)  /*before version 10.15 */
		&& (strcmp(headstr,"10155UNICAD FILE HEADER DATA\n") != 0)) /*after version 10.15 */
	{
/*
.....old version, using fortran routine to load
*/
		fclose(fstream);
		nc = strlen (lib);
		intool(UM_addr_of_f77_str(f77_lib),&nc,&ntl,&kerr);
		nclf_readhead(create_date, create_time, mod_date, mod_time,
				&units, &version, msg, &kerr);
		strncpy(NCL_tool_head.create_date, create_date, 11);
		NCL_tool_head.create_date[11] = '\0';
		strncpy(NCL_tool_head.create_time, create_time, 11);
		NCL_tool_head.create_time[9] = '\0';
		strncpy(NCL_tool_head.mod_date, mod_date, 11);
		NCL_tool_head.mod_date[11] = '\0';
		strncpy(NCL_tool_head.mod_time, mod_time, 11);
		NCL_tool_head.mod_time[9] = '\0';
		NCL_tool_head.units = units;
		NCL_tool_head.version = version; 
		nclf_readtl_info (&toolno, &low_tool, &high_tool);
		NCL_tool_head.low_tool = low_tool;
		NCL_tool_head.high_tool = high_tool;
/*
......don't assign NCL_tool_head.no_tools yet
......because when we store the tool into the link-list,
......it will add up the NCL_tool_head.no_tools number
*/
/*		NCL_tool_head.no_tools = toolno; */
		nclf_rdlib_disp(tlib, &nc1, disp, &nc2, slib, &nc3, cmsg, &kerr);

		strncpy (NCL_tool_head.name, tlib, nc1);
		NCL_tool_head.name[nc1] = '\0';
		strncpy (NCL_tool_head.description, disp, nc2);
		NCL_tool_head.description[nc2] = '\0';
		strncpy (NCL_tool_head.symlib, slib, nc3);
		NCL_tool_head.symlib[nc3] = '\0';
		strcpy (NCL_tool_head.name,libfile);
		NCL_tool_head.utype_no = 0;
		NCL_tool_head.utype = NULL;
		lrecl =  sizeof (struct TL_toolhead_rec);
		nb = 1;

		tool_num_list = (int *)uu_malloc(toolno*sizeof(int));
		UM_init_f77_str(f77_tool,ltool,41);
		intool2 (&ntl,&kerr);
		if (kerr != 0) goto error;
		for (i=0; i<toolno;i++)
		{
/*
........Get the next tool
*/
			gtnxtl(&ktool,UM_addr_of_f77_str(f77_tool),&ctype,&kerr);
/*
........Error reading file
*/
			if (kerr != 0) goto error;
			if (ktool == 0) break;
			tool_num_list[i] = ktool;
		}		
		toolno = i;
		ncl_inittool_data(&tool_data);
		for (ii=0; ii<toolno;ii++)
		{
			toolnum = tool_num_list[ii];
			UM_init_f77_str(f77_str,symtemp,80);
			UM_init_f77_str(f77_str2,drawng,30);
			gettdata(&toolnum, tempcbuf,tempdbuf, tempcprm, tempdprm, tempicfl, &tool_data.ctype,
				UM_addr_of_f77_str(f77_str2), UM_addr_of_f77_str(f77_str),
				UM_addr_of_f77_str(f77_tool));
/*
.....Determine if any parameters are defined
*/
			for (i=0;i<20;i++)
			{
				UM_init_f77_str(fstr[i],fcstr[i],20);
			}
			findtl(&toolnum, UM_addr_of_f77_str(f77_str2), iary,
							UM_addr_of_f77_str(fstr[0]),knc,&nary,&ierr);
			nc = sizeof(drawng);
/*
.....Setup form fields
*/
			ul_strip_blanks(drawng,&nc);
			nc = 80;
			ul_strip_blanks(symtemp,&nc);
			tool_data.ncvals = tempicfl[8];
			tool_data.ndvals = tempicfl[9];
			tool_data.fshank = tempicfl[4];
			tool_data.fholder = tempicfl[5];
			if (tool_data.fholder==1)
			{
				tool_data.symbol[0] = '\0';
				strcpy(tool_data.symhld, symtemp);
			}
			else
			{
				strcpy(tool_data.symbol,symtemp);
				tool_data.symhld[0] = '\0';
			}
			if (tempicfl[0] <= 2)
			{
				if (tempicfl[0] == 2) tool_data.fsymbol = 1;
				else tool_data.fsymbol = 0;
			}
			else
			{
				tool_data.fsymbol = 0;
			}
			if (tool_data.ctype==-1)
			{
				tool_data.ctype = 0;
				if (ltool[0]!='\0')
				{
					nc = strlen(ltool);
					if (nc>40) nc = 40;
					strncpy(tool_data.description, ltool, nc);
					tool_data.description[nc] = '\0';
/*
.....remove ending spaces
*/
					nc--;
					while (nc>=0)
					{
						if (tool_data.description[nc]==' ')
							nc--;
						else
							break;
					}
					tool_data.description[nc+1] = '\0';
				}
				else
					tool_data.description[0] = '\0';
			}
			else
			{
				if (ltool[0]!='\0')
				{
					nc = strlen(ltool);
					if (nc>40) nc = 40;
					strncpy(tool_data.description, ltool, nc);
					tool_data.description[nc] = '\0';
/*
.....remove ending spaces
*/
					nc--;
					while (nc>=0)
					{
						if (tool_data.description[nc]==' ')
							nc--;
						else
							break;
					}
					tool_data.description[nc+1] = '\0';
				}
				else
					tool_data.description[0] = '\0';
			}
/*
.....save the data
*/
			tool_data.toolno = toolnum;
/*
.....now we remove smooth (which tempicfl[3]=3), so when tempicfl[3]=3, 
.....we consider it "Shaded" (=1), "Off" (tempicfl[3]=4 should be 2
*/
			if (tempicfl[3]<=2)
				tool_data.shade = tempicfl[3] - 1;
			else
				tool_data.shade = tempicfl[3] - 2;

			tool_data.segments = tempicfl[1] - 1;
			tool_data.move = tempicfl[2] - 1;
			for (k=0; k<6; k++)
			{
				tool_data.cutter[k] = tempcbuf[k];
				tool_data.pseudo[k] = tempdbuf[k];
				tool_data.cparms[k] = tempcprm[k];
				tool_data.dparms[k] = tempdprm[k];
			}
			if ((tempicfl[0]==1) || (tempicfl[0]==4) )
				tool_data.fpseudo = 1;
			else
				tool_data.fpseudo = 0;
			if (strlen(drawng)>0)
			{
				strcpy(tool_data.drawing, drawng);
			}
			else
				tool_data.drawing[0] = '\0';
			for (k=0; k<4;k++)
			{
				tool_data.catt[k] = 0.;
				tool_data.satt[k] = 0.;
				tool_data.hatt[k] = 0.;
			}
			if (tool_data.ctype < 10) 
				tool_data.hatt[0] = tempdbuf[9];
			else
			{
				tool_data.hatt[0] = tempdbuf[9];
				tool_data.hatt[1] = tempdbuf[10];
				if (tempdbuf[11] != 0. || tempdbuf[12] != 0.)
				{
					if (tool_data.fholder)
					{
						tool_data.hatt[2] = tempdbuf[11];
						tool_data.hatt[3] = tempdbuf[12];
					}
					else
					{
						tool_data.catt[0] = tempdbuf[11];
						tool_data.catt[1] = tempdbuf[12];
					}
				}
			}
			if (tool_data.fshank)
			{
 				tool_data.satt[0] = tempdbuf[13];
				tool_data.satt[1] = tempdbuf[14];
				tool_data.satt[2] = tempdbuf[15];			
				if (tool_data.ctype >= 10) tool_data.satt[3] = tempdbuf[16];
			}
			for (i=0;i<20;i++)
			{
				UM_init_f77_str(fprms[i],cparms[i],80);
			}
			save_tool = toolnum;
			gettparms (&toolnum, UM_addr_of_f77_str(fprms[0]), knc2, &kerr);
			if ((toolnum!=0) && (kerr==0))
			{
/*
......save the parameters
*/
				len = 0;
				for (k=0; k<20;k++)
				{
					len += knc2[k]+1;
					cparms[k][knc2[k]] = '\0';
				}
				tool_data.no_plabel = len;
				tool_data.plabel = (char *)uu_malloc ( (len+1)*sizeof(char) );
				tool_data.plabel[0] = '\0';
				savestr[0] = '\0';
				for (k=0; k<20;k++)
				{
					strcat(tool_data.plabel, cparms[k]);
					if (cparms[k][0]!='\0')
						strcpy(savestr, tool_data.plabel);
					strcat(tool_data.plabel, "\n");
				}
				strcpy(tool_data.plabel, savestr);
				tool_data.no_plabel = strlen (tool_data.plabel);
			}
			for (i=0;i<200;i++)
			{
				UM_init_f77_str(fcmds[i],ccoms[i],80);
			}
			toolnum = save_tool;
			gettcomds (&toolnum, UM_addr_of_f77_str(fcmds[0]), knc3, &comnum, &kerr);
			if ((toolnum!=0) && (kerr==0) && (comnum>0))
			{
/*
......save the command
*/
				len = 0;
				for (k=0; k<comnum;k++)
				{
					len += knc3[k]+1;
					ccoms[k][knc3[k]] = '\0';
				}
				tool_data.no_command = len;
				tool_data.command = (char *)uu_malloc ( (len+1)*sizeof(char) );
				tool_data.command[0] = '\0';
				for (k=0; k<comnum;k++)
				{
					strcat(tool_data.command, ccoms[k]);
					strcat(tool_data.command, "\n");
				}
			}
			for (i=0;i<20;i++)
			{
				UM_init_f77_str(ftlcs[i],ctlcs[i],22);
			}
			UM_init_f77_str(f77_major,major,22);
			toolnum = save_tool;
			getloadtl (&toolnum, UM_addr_of_f77_str(f77_major), &mnc, 
				UM_addr_of_f77_str(ftlcs[0]), knc4, parms, &kerr);
			major[mnc] = '\0';
			if ((toolnum!=0) && (kerr==0))
			{
/*
......save load command
*/
				if (mnc!=0)
					tool_data.floadtl = 1;
				for (k=0; k<20;k++)
				{
					if ((knc4[k]!=0) || (parms[k]>0))
					{
						tool_data.floadtl = 1;
						tool_data.no_loadtl = k + 1;
					}
				}
				strcpy(tool_data.major, major);
				tool_data.loadtl = (struct TL_loadtl_rec*)
								uu_malloc (tool_data.no_loadtl*sizeof (struct TL_loadtl_rec));
				for (k=0; k<tool_data.no_loadtl; k++)
				{
					tool_data.loadtl[k].parm = parms[k];
					ctlcs[k][knc4[k]] = '\0';
					strcpy(tool_data.loadtl[k].value, ctlcs[k]);
				}
			}
/*
......save in the unibase
*/
			if (tool_data.sshade==-1)
				tool_data.sshade = tool_data.shade;
			if (tool_data.hshade==-1)
				tool_data.hshade = tool_data.shade;
			ncl_store_tool (&tool_data);
			if (tool_data.command!=NULL)
				uu_free (tool_data.command);
			if (tool_data.plabel!=NULL)
				uu_free (tool_data.plabel);
			if (tool_data.loadtl!=NULL)
				uu_free (tool_data.loadtl);
			ncl_inittool_data(&tool_data);
		}
		fntool();
		goto done;
	}
	rewind(fstream);
	nb = 1;		
	if (strcmp(headstr,"12345UNICAD FILE HEADER DATA\n") == 0) 
	{
		version = 9.5;
		lrecl =  sizeof (struct TL_toolhead_rec95);
	}
	else if (strcmp(headstr,"67890UNICAD FILE HEADER DATA\n") == 0) 
	{
		version = NCL_version;
		lrecl =  sizeof (struct TL_toolhead_rec102);
		if (lrecl>HEADREC_SIZE32_102)
			lrecl = HEADREC_SIZE32_102;
	}
	else
	{
		version = NCL_version;
		lrecl =  sizeof (struct TL_toolhead_rec);
		if (lrecl>HEADREC_SIZE32)
			lrecl = HEADREC_SIZE32;
	}
	iostat =  fread(&tooldata, lrecl, nb, fstream);
	ncl_updtool_head(&tooldata, lrecl, &NCL_tool_head, version);
	if (NCL_tool_head.version>=10.15)
	{
		if (NCL_tool_head.version<10.154)
			iostat =  fread(&NCL_tool_head.utype_no, sizeof(int), 1, fstream);
		iostat =  fread(&lrecl, sizeof(int), 1, fstream);
		if (lrecl>0)
		{
			iostat =  fread(&tempstr, lrecl, 1, fstream);
			NCL_tool_head.utype = (char **)uu_malloc ((NCL_tool_head.utype_no)*sizeof (char*));
			for (i=0;i<NCL_tool_head.utype_no;i++)
				NCL_tool_head.utype[i] = (char*)uu_malloc(81*sizeof(char));
			i=0;
			tok = (char*)strtok(tempstr, ";");
			while (tok!=NULL)
			{
				strcpy(NCL_tool_head.utype[i++], tok);
				tok = (char*)strtok(NULL, ";");
				if (i>=NCL_tool_head.utype_no)
					break;
			}
		}
	}
/*
.....CUTTER/PROFIL was issued so the proflib should be set accordingly
*/
	if (Sprof_flg) 
		ncl_getcut_profnam(&NCL_tool_head.proflib);
	strcpy (NCL_tool_head.name, libfile);
	
	if (NCL_tool_head.version < 9.6)
		lrecl =  sizeof (struct TL_tooldata_rec95);
	else
	{
		lrecl = sizeof(int);
		iostat =  fread(&lrecl, sizeof(int), nb, fstream);
	}
	nb = 1;
	eof = 0;
/*
......reset NCL_tool_head.no_tools
......because when we store the tool into the link-list,
......it will add up the NCL_tool_head.no_tools number
*/
	NCL_tool_head.no_tools = 0;
	while (eof==0)
	{
		isize =  fread(&tooldata, lrecl, nb, fstream);
		ncl_updtool_data(&tooldata, lrecl, &tool_data, NCL_tool_head.version);

		if (tool_data.toolno==0) break;
		if (tool_data.no_loadtl>0)
		{
			tool_data.loadtl = (struct TL_loadtl_rec*)
					uu_malloc (tool_data.no_loadtl*sizeof (struct TL_loadtl_rec));
			for (i=0; i<tool_data.no_loadtl; i++)
			{
				fread (&(tool_data.loadtl[i]), sizeof(struct TL_loadtl_rec), nb, fstream);
			}
		}
/*
.....the old version may saved tool_data.loadtl as non-NULL but if the no_loadtl is 0
.....there is no loadtl pointer
*/
		else
			tool_data.loadtl = NULL;
		if (tool_data.no_plabel)
		{
			tool_data.plabel = (char *)uu_malloc ( (tool_data.no_plabel+1)*sizeof(char) );
			fread (tool_data.plabel, tool_data.no_plabel+1, nb, fstream);
		}
/*
.....conversion of 64bit/32bit will let tool_data.loadtl be non-NULL (tool_data.loadtl[0]=0)
.....but if the no_plabel is 0
.....there is no plabel pointer
*/
		else
			tool_data.plabel = NULL;
		if (tool_data.no_command)
		{
			tool_data.command = (char *)uu_malloc ( (tool_data.no_command+1)*sizeof(char) );
			fread (tool_data.command, (tool_data.no_command+1), nb, fstream);
		}
/*
.....conversion of 64bit/32bit will let tool_data.loadtl be non-NULL (tool_data.loadtl[0]=0)
.....but if the command is 0
.....there is no command pointer
*/
		else
			tool_data.command = NULL;
		if (isize>0)
		{
/*
......save in the unibase
*/
			if (tool_data.sshade==-1)
				tool_data.sshade = tool_data.shade;
			if (tool_data.hshade==-1)
				tool_data.hshade = tool_data.shade;
			ncl_store_tool (&tool_data);
			if (tool_data.command!=NULL)
				uu_free (tool_data.command);
			if (tool_data.plabel!=NULL)
				uu_free (tool_data.plabel);
			if (tool_data.loadtl!=NULL)
				uu_free (tool_data.loadtl);
			ncl_inittool_data(&tool_data);
			if (NCL_tool_head.version < 9.6)
				ncl_inittool_data95(&tool_data95);
		}
		else if (feof(fstream))
		{
			eof = 1;
			break;
		}
	}
	fclose(fstream);
	goto done;
error:;
	return -1;
done:;
	len = strlen (libfile);
	stllib (libfile,&len);
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : int ncl_toollist_init ()
**       Initialize the tool list.
**    PARAMETERS
**       INPUT  : 
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_toollist_init ()
{
	int status = UU_SUCCESS;

	if (TOOL_data_list == UU_NULL)
	{
		TOOL_data_list = (struct TL_tooldata_rec *) uu_lsnew();
		if (TOOL_data_list == UU_NULL) status = UU_FAILURE;
	}
	return (status);
}
/***********************************************************************
c
c   SUBROUTINE:  ncl_store_toolhead(tool_head)
c
c   FUNCTION:  store a tool's head info into the global tool head struction
c
c   INPUT:  tool_head: tool head to be stored
c
c   OUTPUT: none
c	RETUEN: -1:  failed
c
c***********************************************************************/
void ncl_store_toolhead(tool_head)
struct TL_toolhead_rec *tool_head;
{
	int i, len;
	if ((NCL_tool_head.utype_no>0)&&(NCL_tool_head.utype!=NULL))
	{
		for (i=0; i<NCL_tool_head.utype_no;i++)
		{
			if (NCL_tool_head.utype[i]!=NULL)
				uu_free(NCL_tool_head.utype[i]);
			NCL_tool_head.utype[i] = NULL;
		}
		uu_free(NCL_tool_head.utype);
	}
	uu_move_byte((char*)tool_head, (char*)&NCL_tool_head, sizeof(struct TL_toolhead_rec));
	if (tool_head->utype_no>0)
	{
		NCL_tool_head.utype = (char**)uu_malloc(tool_head->utype_no*sizeof(char*));
		for (i=0; i<tool_head->utype_no;i++)
		{
			NCL_tool_head.utype[i] = (char *)uu_malloc((81)*sizeof(char));
			strcpy(NCL_tool_head.utype[i], tool_head->utype[i]);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_sel_tool(nctool)
**       set the toolno as the current active tool
**    PARAMETERS
**       INPUT  : 
**          nctool: tool number to be set as courrect active tool
**       OUTPUT :
**          none
**    RETURNS -1: failed     :
**         
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_sel_tool(toolno)
double toolno;
{
	if (NCL_tool_data.command!=NULL)
		uu_free (NCL_tool_data.command);
	if (NCL_tool_data.plabel!=NULL)
		uu_free (NCL_tool_data.plabel);
	if (NCL_tool_data.loadtl!=NULL)
		uu_free (NCL_tool_data.loadtl);
	return ncl_get_tooldata (toolno, &NCL_tool_data);
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_store_tool(tool_data)
c
c   FUNCTION:  store a tool's data info into the toolib link list
c
c   INPUT:  tool_data: tool data to be stored
c
c   OUTPUT: none
c	RETUEN: -1:  failed
c
c***********************************************************************/
ncl_store_tool(tool_data)
struct TL_tooldata_rec *tool_data;
{
	int status = UU_SUCCESS;
	int k, n1;
	struct TL_tooldata_rec *p2;
/*
.....Allocate next block
*/
	n1 = sizeof(struct TL_tooldata_rec);
	if (TOOL_data_list == UU_NULL) status = ncl_toollist_init();
	p2 = TOOL_data_list;
	if (p2 != UU_NULL)
	{
		p2 = (struct TL_tooldata_rec *) uu_lsend(p2);
		if (p2 == UU_NULL) p2 = TOOL_data_list;
		if (p2 != UU_NULL) p2 = (struct TL_tooldata_rec *) uu_lsinsrt((char*)p2,n1);
		if (p2 != UU_NULL) NCL_tool_head.no_tools++;
	}
	if (p2 == UU_NULL)
		status = UU_FAILURE;
/*
.....Store structure
*/
	else
	{
		if (tool_data->toolno>NCL_tool_head.high_tool)
			NCL_tool_head.high_tool = tool_data->toolno;
		if (tool_data->toolno<NCL_tool_head.low_tool)
			NCL_tool_head.low_tool = tool_data->toolno;
		uu_move_byte((char*)tool_data, (char*)p2, sizeof(struct TL_tooldata_rec));
		if (tool_data->plabel!=NULL)
		{
			p2->plabel = (char *)uu_malloc ( (p2->no_plabel+1)*sizeof(char) );
			strcpy(p2->plabel, tool_data->plabel);
		}
		if (tool_data->command!=NULL)
		{
			p2->command = (char *)uu_malloc ( (p2->no_command+1)*sizeof(char) );
			strcpy(p2->command, tool_data->command);
		}
		if (tool_data->loadtl!=NULL)
		{
			p2->loadtl = (struct TL_loadtl_rec*)
					uu_malloc (p2->no_loadtl*sizeof (struct TL_loadtl_rec));
			for (k=0; k<p2->no_loadtl; k++)
			{
				p2->loadtl[k].parm = tool_data->loadtl[k].parm;
				strcpy(p2->loadtl[k].value, tool_data->loadtl[k].value);
			}
		}
	}
/*
.....End of routine
*/
	return(status);
}
		
/***********************************************************************
c
c   SUBROUTINE:  ncl_getcurrent_tooldata (tool_data)
c
c   FUNCTION:  get current tool's data info
c
c   INPUT:  none
c
c   OUTPUT: tool_data: data struction contain tool data
c	RETUEN: -1:  failed
c
c***********************************************************************/
void ncl_getcurrent_tooldata (tool_data)
struct TL_tooldata_rec *tool_data;
{
	int k;
	uu_move_byte((char*)&NCL_tool_data, (char*)tool_data, sizeof(struct TL_tooldata_rec));
	if (NCL_tool_data.plabel!=NULL)
	{
		tool_data->plabel = (char *)uu_malloc ( (tool_data->no_plabel+1)*sizeof(char) );
		strcpy(tool_data->plabel, NCL_tool_data.plabel);
	}
	if (NCL_tool_data.command!=NULL)
	{
		tool_data->command = (char *)uu_malloc ( (tool_data->no_command+1)*sizeof(char) );
		strcpy(tool_data->command, NCL_tool_data.command);
	}
	if (NCL_tool_data.loadtl!=NULL)
	{
		tool_data->loadtl = (struct TL_loadtl_rec*)
				uu_malloc (tool_data->no_loadtl*sizeof (struct TL_loadtl_rec));
		for (k=0; k<tool_data->no_loadtl; k++)
		{
			tool_data->loadtl[k].parm = NCL_tool_data.loadtl[k].parm;
			strcpy(tool_data->loadtl[k].value, NCL_tool_data.loadtl[k].value);
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_gettool_head (tool_head)
c
c   FUNCTION:  get current tool's head info
c
c   INPUT:  none
c
c   OUTPUT: tool_head: data struction contain tool head
c	RETUEN: -1:  failed
c
c***********************************************************************/
void ncl_gettool_head (tool_head)
struct TL_toolhead_rec *tool_head;
{
	int i, len;
	if (TOOL_data_list==NULL)
		ncl_tool_inithead();
	uu_move_byte((char*)&NCL_tool_head, (char*)tool_head, sizeof(struct TL_toolhead_rec));
	if (NCL_tool_head.utype_no>0)
	{
		tool_head->utype = (char**)uu_malloc(NCL_tool_head.utype_no*sizeof(char*));
		for (i=0; i<tool_head->utype_no;i++)
		{
			tool_head->utype[i] = (char *)uu_malloc((81)*sizeof(char));
			strcpy(tool_head->utype[i], NCL_tool_head.utype[i]);
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_get_tooldata (tool, data)
c
c   FUNCTION:  get a tool's data
c
c   INPUT:  tool: tool number
c
c   OUTPUT: data: data struction contain tool data
c	RETUEN: -1:  failed
c
c***********************************************************************/
ncl_get_tooldata (tool, data)
double tool;
struct TL_tooldata_rec *data;
{
	struct TL_tooldata_rec *p1;
	int k;
	
	p1 = (struct TL_tooldata_rec *) TOOL_data_list;
	if (p1 != UU_NULL) p1 = (struct TL_tooldata_rec *) uu_lsnext(p1);
	while (p1)
	{
		if (tool==p1->toolno)
		{
			uu_move_byte((char*)p1, (char*)data, sizeof(struct TL_tooldata_rec));
			if (p1->plabel!=NULL)
			{
				data->plabel = (char *)uu_malloc ( (data->no_plabel+1)*sizeof(char) );
				strcpy(data->plabel, p1->plabel);
			}
			if (p1->command!=NULL)
			{
				data->command = (char *)uu_malloc ( (data->no_command+1)*sizeof(char) );
				strcpy(data->command, p1->command);
			}
			if (p1->loadtl!=NULL)
			{
				data->loadtl = (struct TL_loadtl_rec*)
					uu_malloc (data->no_loadtl*sizeof (struct TL_loadtl_rec));
				for (k=0; k<data->no_loadtl; k++)
				{
					data->loadtl[k].parm = p1->loadtl[k].parm;
					strcpy(data->loadtl[k].value, p1->loadtl[k].value);
				}
			}
			return 0;
		}
		p1 = (struct TL_tooldata_rec *) uu_lsnext(p1);
	}
	return -1;
}
/***********************************************************************
c
c   SUBROUTINE:  ncl_idx_tooldata(indx, tool, type, disp)
c
c   FUNCTION:  get a tool's number, type and desprition
c
c   INPUT:  indx: indx in toolib linklist
c
c   OUTPUT: none
c	RETUEN: none
c
c***********************************************************************/
int ncl_idx_tooldata(indx, tool, type, disp)
int indx, *type;
double *tool;
char *disp;
{
	struct TL_tooldata_rec *p1;
	int num = 0;

	p1 = (struct TL_tooldata_rec *) TOOL_data_list;
	if (p1 != UU_NULL) p1 = (struct TL_tooldata_rec *) uu_lsnext(p1);
	while (p1)
	{
		if (num==indx)
		{
			*tool = p1->toolno;
			*type = p1->ctype;
			strncpy(disp, p1->description, 40);
			disp[40] = '\0';
			return 0;
		}
		p1 = (struct TL_tooldata_rec *) uu_lsnext(p1);
		num++;
	}
	return -1;
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_inittool_data (tool_data)
c
c   FUNCTION:  initial toolib data value: tool_data
c
c   INPUT:  tool_data: data structure to be initialize
c
c   OUTPUT: none
c	RETUEN: none
c
c***********************************************************************/
void ncl_inittool_data (tool_data)
struct TL_tooldata_rec* tool_data;
{
	int i;
	tool_data->description[0] = '\0';
	tool_data->symbol[0] = '\0';
	tool_data->symshk[0] = '\0';
	tool_data->symhld[0] = '\0';
	tool_data->drawing[0] = '\0';
	tool_data->major[0] = '\0';

	tool_data->toolno = 0;
	tool_data->ncvals = 0;
	tool_data->ndvals = 0;
	tool_data->ctype = 0;
	tool_data->shade = 0;
/*
.....if not set it = tool_data->shade;
*/
	tool_data->sshade = -1;
	tool_data->hshade = -1;
	tool_data->segments = 0;
	tool_data->move = 0;
	for (i=0; i<6; i++)
	{
		tool_data->cparms[i] = 0;
		tool_data->dparms[i] = 0;
		tool_data->cutter[i] = 0.0;
		tool_data->pseudo[i] = 0.0;
	}
	for (i=0; i<4; i++)
	{
		tool_data->yparms[i] = 0;
		tool_data->sparms[i] = 0;
		tool_data->hparms[i] = 0;
		tool_data->catt[i] = 0.0;
		tool_data->satt[i] = 0.0;
		tool_data->hatt[i] = 0.0;
	}
	tool_data->fpseudo = 0;
	tool_data->fshank = 0;
	tool_data->fholder = 0;
	tool_data->fsymbol = 0;
	tool_data->floadtl = 0;
	tool_data->no_loadtl = 0;
	tool_data->no_plabel = 0;
	tool_data->no_command = 0;
	tool_data->loadtl = NULL;
	tool_data->plabel = NULL;
	tool_data->command = NULL;
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_tool_initunit(orig_unit)
c
c   FUNCTION:  initial unit
c
c   INPUT:  none
c
c   OUTPUT: none
c	RETUEN: none
c
c***********************************************************************/
void ncl_tool_initunit(orig_unit)
int orig_unit;
{
	NCL_tool_unit = orig_unit;
}
/***********************************************************************
c
c   SUBROUTINE:  ncl_tool_inithead()
c
c   FUNCTION:  re-init tool head data
c
c   INPUT:  none
c
c   OUTPUT: none
c	RETUEN: none
c
c***********************************************************************/
void ncl_tool_inithead()
{
/*
.....new version changed data structure Yurong 0716
*/
/*	strcpy (NCL_tool_head.head,"67890UNICAD FILE HEADER DATA\n"); */
	strcpy (NCL_tool_head.head,"10155UNICAD FILE HEADER DATA\n");
	NCL_tool_head.create_date[0] = '\0';
	NCL_tool_head.create_time[0] = '\0';
	NCL_tool_head.mod_date[0] = '\0';
	NCL_tool_head.mod_time[0] = '\0';
	NCL_tool_head.name[0] = '\0';
	NCL_tool_head.description[0] = '\0';
	NCL_tool_head.symlib[0] = '\0';
	NCL_tool_head.proflib[0] = '\0';
	NCL_tool_head.units = NCL_tool_unit;
	NCL_tool_head.no_tools = 0;
	NCL_tool_head.low_tool = 0;
	NCL_tool_head.high_tool = 0;
	NCL_tool_head.version = NCL_version;
	NCL_tool_head.utype_no = 0;
	NCL_tool_head.utype = NULL;
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_init_tldata()
c
c   FUNCTION:  re-init tool data
c
c   INPUT:  none
c
c   OUTPUT: none
c	RETUEN: none
c
c***********************************************************************/
void ncl_init_tldata()
{
	ncl_inittool_data(&NCL_tool_data);
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_reinit_tldata()
c
c   FUNCTION:  re-init tool data
c
c   INPUT:  none
c
c   OUTPUT: none
c	RETUEN: none
c
c***********************************************************************/
void ncl_reinit_tldata()
{
	if (NCL_tool_data.command!=NULL)
		uu_free (NCL_tool_data.command);
	if (NCL_tool_data.plabel!=NULL)
		uu_free (NCL_tool_data.plabel);
	if (NCL_tool_data.loadtl!=NULL)
		uu_free (NCL_tool_data.loadtl);
	NCL_tool_data.plabel = NULL;
	NCL_tool_data.command = NULL;
	NCL_tool_data.loadtl = NULL;
	ncl_inittool_data(&NCL_tool_data);
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_tool_reinitlib()
c
c   FUNCTION:  init tool list and global tool data
c
c   INPUT:  none
c
c   OUTPUT: none
c	RETUEN: none
c
c***********************************************************************/
void ncl_tool_reinitlib()
{
	ncl_del_toollist();
	TOOL_data_list = (struct TL_tooldata_rec *) uu_lsnew();
	ncl_tool_inithead();
	ncl_reinit_tldata();
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_del_toollist()
c
c   FUNCTION:  delete the toolib list
c
c   INPUT:  none
c
c   OUTPUT: none
c	RETUEN: none
c
c***********************************************************************/
void ncl_del_toollist()
{
	struct TL_tooldata_rec *tool_data;
	if (TOOL_data_list!=NULL)
	{
		tool_data = (struct TL_tooldata_rec *) TOOL_data_list;
		tool_data = (struct TL_tooldata_rec *) uu_lsnext(tool_data);
		while (tool_data)
		{
			if (tool_data->command!=NULL)
				uu_free (tool_data->command);
			if (tool_data->plabel!=NULL)
				uu_free (tool_data->plabel);
			if (tool_data->loadtl!=NULL)
				uu_free (tool_data->loadtl);
			tool_data = (struct TL_tooldata_rec *) uu_lsnext(tool_data);
		}
		uu_lsdel(TOOL_data_list);
		TOOL_data_list = NULL;
	}
	if (NCL_tool_data.command!=NULL)
		uu_free (NCL_tool_data.command);
	if (NCL_tool_data.plabel!=NULL)
		uu_free (NCL_tool_data.plabel);
	if (NCL_tool_data.loadtl!=NULL)
		uu_free (NCL_tool_data.loadtl);
	NCL_tool_data.plabel = NULL;
	NCL_tool_data.command = NULL;
	NCL_tool_data.loadtl = NULL;
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_get_toolist_pt(tool_list)
c
c   FUNCTION:  get the toolib list pointer
c
c   INPUT:  tool_list
c
c   OUTPUT: tool_list:  toolib list pointer
c           
c	RETUEN: none
c
c***********************************************************************/
void ncl_get_toolist_pt(tool_list)
struct TL_tooldata_rec **tool_list;
{
	*tool_list = TOOL_data_list;
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_cutool (kent,cbuf,knc)
c
c   FUNCTION:  This function execute the CUTTER/TOOL command, the toolib already loaded
c				it will issue different CUTTER/ command according to kent number
c
c   INPUT:  kent    I*4  D1  -  command flag number
c
c   OUTPUT: kent    I*4  D1  -  Set to the entry type for the next call
c                               to this routine and can have the
c                               following values.
c
c                                  1 = Issue CUTTER/parms.
c                                  2 = Issue CUTTER/DISPLY,parms
c                                  3 = Issue CUTTER/DISPLY,(PART,ALL)
c                                  4 = Issue CUTTER/DISPLY,MOVE
c                                  5 = Issue CUTTER/DISPLY,SHADE
c                                  6 = Issue CUTTER/DISPLY,symbol
c                                  7 = Issue CUTTER/DISPLY,SHANK command.
c                                  8 = Issue CUTTER/DISPLY,HOLDER command.
c                                  9 = Issue CUTTER/DISPLY,SHADE, On/Off, SHANK
c                                  10 = Issue CUTTER/DISPLY,SHADE, On/Off, HOLDER
c                                  11 = Issue Load Tool command.
c                                 12 = Load Tool continuation line.
c                                 13+= Issue Operator Commands
c                                 -1 = Ran out of commands.  No command
c                                      was issued.
c
c           cbuf    C*n  D1  -  Text of command when 'kent'.
c
c           knc     I*4  D1  -  Number of chars in 'cbuf'.
c
c***********************************************************************/
void ncl_cutool (kent,cbuf,knc)
int *kent, *knc;
char *cbuf;
{
	int it,i,j,k,nc,nc1,status;
	UM_int2 ifl;
	int ncom;
	int inum, inum0, inum1, inum2, inum3;
	char ldlm[2],scutr[10];
	char lnum[80], lnum0[80], lnum1[80], lnum2[80], lnum3[80];
	int pnc[20], nval, nctool;
	UM_f77_str fprms[20];
	char lctool[20][20], ccoms[200][80];
	static int ictinc = 0;
	static UM_int2 Sins_sav[2];
	UM_int2 insfl[2];
	static UU_LOGICAL Scalled = UU_FALSE;

	*knc = 0;
	cbuf[0] = '\0';
	for (i=0;i<20;i++)
	{
		UM_init_f77_str(fprms[i], lctool[i], 20);
	}
	toolparms (&nctool, UM_addr_of_f77_str(fprms[0]), pnc);
	for (i=0; i<20;i++)
	{
		lctool[i][pnc[i]] = '\0';
	}
/*
......1st Entry
......Issue CUTTER/parms command
*/
	if (Soutflg) strcpy(scutr,"CUTTER");
	else strcpy(scutr,"*CUTTER");
	if (*kent > 1) Scalled = UU_FALSE;
	if (*kent==1)
	{
		if (!Scalled)
		{
			getins(&Sins_sav[0],&Sins_sav[1]);
/*
.....Issue a CUTTER/PROFIL command if a CUTTER/READ was issued so the
.....correct profil library is used when defining the cutter.
.......kent is not changed so the CUTTER/parms command is not skipped
.......when ncl_tool is called again.
*/
			if (Soutflg)
			{
				status = ncl_getcut_profil(cbuf);
				*knc = strlen(cbuf);
				Scalled = UU_TRUE;
				if (status == UU_SUCCESS) goto done;
			}
		}
		if (NCL_tool_data.fpseudo==0)
			nval   = NCL_tool_data.ncvals;
		else
			nval   = NCL_tool_data.ndvals;
		if (nval==0)
			*kent = 2;
		else
		{
			if (NCL_tool_data.ctype>=10)
			{
				sprintf(cbuf,"%s/LATHE",scutr);
				strcpy(ldlm, ",");
			}
			else if (NCL_tool_data.ctype==9)
			{
				sprintf(cbuf,"%s/BLADE",scutr);
				strcpy(ldlm, ",");
			}
			else
			{
				strcpy(cbuf,scutr);
				strcpy(ldlm, "/");
			}
			if (NCL_tool_data.fpseudo==0)
				nval = NCL_tool_data.ncvals;
			else
				nval = NCL_tool_data.ndvals;
			for (i=0; i<nval; i++)
			{
				if (NCL_tool_data.fpseudo==0)
					inum   = NCL_tool_data.cparms[i];
				else
					inum   = NCL_tool_data.dparms[i];
				strcat (cbuf, ldlm);
				strcpy(ldlm, ",");
				if ((inum<=0) || ( strcmp (lctool[inum-1], "SAME")==0)
					|| (inum > nctool))
				{
					if (NCL_tool_data.fpseudo==0)
						sprintf(lnum, "%f", NCL_tool_data.cutter[i]);
					else
						sprintf(lnum, "%f", NCL_tool_data.pseudo[i]);
				}
				else if (inum <= nctool)
					strcpy (lnum, lctool[inum-1]);
				else
					strcpy (lnum, "0.");
				strcat (cbuf, lnum);
			}
			*kent = 2;
			*knc = strlen (cbuf);
			goto done;
		}
	}
/*
......2nd Entry
......Issue CUTTER/DISPLY,parms command
*/
	if (*kent == 2)
	{
		if (NCL_tool_data.fpseudo==0)
			nval   = NCL_tool_data.ndvals;
		else
			nval   = NCL_tool_data.ncvals;
		if (nval==0)
			*kent = 3;
		else
		{
			sprintf(cbuf,"%s/DISPLY",scutr);
			strcpy(ldlm, ",");
			for (i=0; i<nval; i++)
			{
				if (NCL_tool_data.fpseudo==0)
					inum   = NCL_tool_data.dparms[i];
				else
					inum   = NCL_tool_data.cparms[i];
				strcat (cbuf, ldlm);
				if ((inum<=0) || ( strcmp (lctool[inum-1], "SAME")==0)
					|| (inum > nctool))
				{
					if (NCL_tool_data.fpseudo==0)
						sprintf(lnum, "%f", NCL_tool_data.pseudo[i]);
					else
						sprintf(lnum, "%f", NCL_tool_data.cutter[i]);
				}
				else if (inum <= nctool)
					strcpy (lnum, lctool[inum-1]);
				else
					strcpy (lnum, "0.");
				strcat (cbuf, lnum);
			}
			*kent = 3;
			*knc = strlen (cbuf);
			goto done;
		}
	}
/*
......3rd Entry
......Issue CUTTER/DISPLY,(PART,ALL) command
*/
	if (*kent==3)
	{
		if (NCL_tool_data.segments==1)
		{
			sprintf(cbuf,"%s/DISPLY,PART",scutr);
			*knc = strlen (cbuf);
			*kent = 4;
			goto done;
		}
		else if (NCL_tool_data.segments==2)
		{
			sprintf(cbuf,"%s/DISPLY,ALL",scutr);
			*knc = strlen (cbuf);
			*kent = 4;
			goto done;
		}
		else
			*kent = 4;
	}
/*
......4th Entry
......Issue CUTTER/DISPLY,MOVE command
*/
	if (*kent==4)
	{
		if (NCL_tool_data.move==1)
		{
			sprintf(cbuf,"%s/DISPLY,MOVE,ON",scutr);
			*knc = strlen (cbuf);
			*kent = 5;
			goto done;
		}
		else if (NCL_tool_data.move==2)
		{
			sprintf(cbuf,"%s/DISPLY,MOVE,OFF",scutr);
			*knc = strlen (cbuf);
			*kent = 5;
			goto done;
		}
		else
			*kent = 5;
	}
/*
......5th Entry
......Issue CUTTER/DISPLY,SHADE command
*/
	if (*kent==5)
	{
		if (NCL_tool_data.shade==1)
		{
			sprintf(cbuf,"%s/DISPLY,SHADE,ON",scutr);
			*knc = strlen (cbuf);
			*kent = 6;
			goto done;
		}
		else if (NCL_tool_data.shade==2)
		{
			sprintf(cbuf,"%s/DISPLY,SHADE,OFF",scutr);
			*knc = strlen (cbuf);
			*kent = 6;
			goto done;
		}
		else
			*kent = 6;
	}
/*
......6th Entry
......Issue CUTTER/DISPLY,symbol command
*/
	if (*kent==6)
	{
		nc = strlen (NCL_tool_data.symbol);
		if ((nc!=0)&&(NCL_tool_data.fsymbol))
		{
			sprintf(cbuf,"%s/DISPLY",scutr);
			strcpy(ldlm, ",");
			if (strlen(NCL_tool_head.symlib)!=0)
			{
				strcat (cbuf, ldlm);
				if (strlen(NCL_tool_head.symlib)>0)
				{
					strcat (cbuf, NCL_tool_head.symlib);
					strcat (cbuf, ldlm);
				}
				strcat (cbuf, NCL_tool_data.symbol);
			}
			inum0 = NCL_tool_data.yparms[0];
			inum1 = NCL_tool_data.yparms[1];
			if ((inum0<=0) || ( strcmp (lctool[inum0-1], "SAME")==0)
					|| (inum0 > nctool))
			{
				sprintf(lnum0, "%f", NCL_tool_data.catt[0]);
			}
			else if (inum0<=nctool)
				strcpy (lnum0, lctool[inum0-1]);
			else
				strcpy (lnum0, "0.");
			if ((inum1<=0) || ( strcmp (lctool[inum1-1], "SAME")==0)
					|| (inum1 > nctool))
			{
				sprintf(lnum1, "%f", NCL_tool_data.catt[1]);
			}
			else if (inum1<=nctool)
				strcpy (lnum1, lctool[inum1-1]);
			else
				strcpy (lnum1, "0.");

			if ((NCL_tool_data.ctype>=10) && ((atof(lnum0)!=0.) 
				|| (atof(lnum1)!=0.)))
			{
				strcat (cbuf, ",OFFSET,");
				sprintf(lnum, "%s,%s", lnum0, lnum1);
				strcat (cbuf, lnum);
			}
			*kent = 7;
			*knc = strlen (cbuf);
			goto done;
		}
		else
			*kent = 7;
	}
/*
......7th Entry
......Issue CUTTER/DISPLY,SHANK command
*/
	if (*kent==7)
	{
		if (NCL_tool_data.fshank)
		{
			sprintf(cbuf,"%s/DISPLY,SHANK",scutr);
			inum0 = NCL_tool_data.sparms[0];
			inum1 = NCL_tool_data.sparms[1];
			if ((inum0<=0) || ( strcmp (lctool[inum0-1], "SAME")==0)
					|| (inum0 > nctool))
			{
				sprintf(lnum0, "%f", NCL_tool_data.satt[0]);
			}
			else if (inum0<=nctool)
				strcpy (lnum0, lctool[inum0-1]);
			else
				strcpy (lnum0, "0.");
			if ((inum1<=0) || ( strcmp (lctool[inum1-1], "SAME")==0)
					|| (inum1 > nctool))
			{
				sprintf(lnum1, "%f", NCL_tool_data.satt[1]);
			}
			else if (inum1<=nctool)
				strcpy (lnum1, lctool[inum1-1]);
			else
				strcpy (lnum1, "0.");
			inum2 = NCL_tool_data.sparms[2];
			inum3 = NCL_tool_data.sparms[3];
			if ((inum2<=0) || ( strcmp (lctool[inum2-1], "SAME")==0)
				|| (inum2 > nctool))
			{
				sprintf(lnum2, "%f", NCL_tool_data.satt[2]);
			}
			else if (inum2<=nctool)
				strcpy (lnum2, lctool[inum2-1]);
			else
				strcpy (lnum2, "0.");
			if ((inum3<=0) || ( strcmp (lctool[inum3-1], "SAME")==0)
					|| (inum3 > nctool))
			{
				sprintf(lnum3, "%f", NCL_tool_data.satt[3]);
			}
			else if (inum3<=nctool)
				strcpy (lnum3, lctool[inum3-1]);
			else
				strcpy (lnum3, "0.");

			strcpy(ldlm, ",");
			if (strlen(NCL_tool_data.symshk)!=0)
			{
				strcat (cbuf, ldlm);
				strcat (cbuf, NCL_tool_data.symshk);
				if (NCL_tool_data.ctype<=9)
				{
					strcat (cbuf, ldlm);
					strcat(cbuf,lnum0);
				}
				else
				{
					strcat (cbuf, ldlm);
					strcat(cbuf,lnum0);
					strcat (cbuf, ldlm);
					strcat(cbuf,lnum1);
					strcat (cbuf, ldlm);
					strcat(cbuf,lnum2);
					strcat (cbuf, ldlm);
					strcat(cbuf,lnum3);
				}
			}
			else
			{
				strcat (cbuf, ldlm);
				strcat(cbuf,lnum0);
				strcat (cbuf, ldlm);
				strcat(cbuf,lnum1);
				strcat (cbuf, ldlm);
				strcat(cbuf,lnum2);
				strcat (cbuf, ldlm);
				strcat(cbuf,lnum3);
			}			
 
			strcat(cbuf, ",");
			if (NCL_tool_data.fshank==1)
				strcat (cbuf, "CUTTER");
			else
				strcat (cbuf, "HOLDER");
 
			*kent = 8;
			*knc = strlen (cbuf);
			goto done;
		}
		else
			*kent = 8;
	}
/*
......8th Entry
......Issue CUTTER/DISPLY,HOLDER command
*/
	if (*kent==8)
	{
/*		nc = strlen (NCL_tool_data.symhld);*/
/*		if ((nc!=0)&&(NCL_tool_data.fholder==1))*/
		if (NCL_tool_data.fholder==1)
		{
			sprintf(cbuf,"%s/DISPLY,HOLDER",scutr);
			inum0 = NCL_tool_data.hparms[0];
			inum1 = NCL_tool_data.hparms[1];
			if ((inum0<=0) || ( strcmp (lctool[inum0-1], "SAME")==0)
					|| (inum0 > nctool))
			{
				sprintf(lnum0, "%f", NCL_tool_data.hatt[0]);
			}
			else if (inum0<=nctool)
				strcpy (lnum0, lctool[inum0-1]);
			else
				strcpy (lnum0, "0.");
			if ((inum1<=0) || ( strcmp (lctool[inum1-1], "SAME")==0)
					|| (inum1 > nctool))
			{
				sprintf(lnum1, "%f", NCL_tool_data.hatt[1]);
			}
			else if (inum1<=nctool)
				strcpy (lnum1, lctool[inum1-1]);
			else
				strcpy (lnum1, "0.");
			inum2 = NCL_tool_data.hparms[2];
			inum3 = NCL_tool_data.hparms[3];
			if ((inum2<=0) || ( strcmp (lctool[inum2-1], "SAME")==0)
				|| (inum2 > nctool))
			{
				sprintf(lnum2, "%f", NCL_tool_data.hatt[2]);
			}
			else if (inum2<=nctool)
				strcpy (lnum2, lctool[inum2-1]);
			else
				strcpy (lnum2, "0.");
			if ((inum3<=0) || ( strcmp (lctool[inum3-1], "SAME")==0)
					|| (inum3 > nctool))
			{
				sprintf(lnum3, "%f", NCL_tool_data.hatt[3]);
			}
			else if (inum3<=nctool)
				strcpy (lnum3, lctool[inum3-1]);
			else
				strcpy (lnum3, "0.");

			strcpy(ldlm, ",");
			if (strlen(NCL_tool_data.symhld)!=0)
			{
				strcat (cbuf, ldlm);
				strcat (cbuf, NCL_tool_data.symhld);
				if (NCL_tool_data.ctype<=9)
				{
					strcat (cbuf, ldlm);
					strcat(cbuf,lnum0);
				}
				else
				{
					strcat (cbuf, ldlm);
					strcat(cbuf,lnum0);
					strcat (cbuf, ldlm);
					strcat(cbuf,lnum1);
					strcat (cbuf, ldlm);
					strcat(cbuf,lnum2);
					strcat (cbuf, ldlm);
					strcat(cbuf,lnum3);
				}
			}
			else
			{
				strcat (cbuf, ldlm);
				strcat(cbuf,lnum0);
				strcat (cbuf, ldlm);
				strcat(cbuf,lnum1);
				strcat (cbuf, ldlm);
				strcat(cbuf,lnum2);
				strcat (cbuf, ldlm);
				strcat(cbuf,lnum3);
			}			
			*kent = 9;
			*knc = strlen (cbuf);
			goto done;
		}
		else
			*kent = 9;
	}
/*
......9th Entry
......Issue CUTTER/DISPLY,SHADE On/Off, SHANK
*/
	if (*kent==9)
	{
		if (NCL_tool_data.fshank)
		{
			if (NCL_tool_data.sshade==1)
			{
				sprintf(cbuf,"%s/DISPLY,SHADE,ON,SHANK",scutr);
				*knc = strlen (cbuf);
				*kent = 10;
				goto done;
			}
			else if (NCL_tool_data.shade==2)
			{
				sprintf(cbuf,"%s/DISPLY,SHADE,OFF,SHANK",scutr);
				*knc = strlen (cbuf);
				*kent = 10;
				goto done;
			}
			else
				*kent = 10;
		}
		else
			*kent = 10;
	}
/*
......10th Entry
......Issue CUTTER/DISPLY,SHADE On/Off, HOLDER
*/
	if (*kent==10)
	{
		if (NCL_tool_data.fholder)
		{
			if (NCL_tool_data.hshade==1)
			{
				sprintf(cbuf,"%s/DISPLY,SHADE,ON,HOLDER",scutr);
				*knc = strlen (cbuf);
				*kent = 11;
				goto done;
			}
			else if (NCL_tool_data.hshade==2)
			{
				sprintf(cbuf,"%s/DISPLY,SHADE,OFF,HOLDER",scutr);
				*knc = strlen (cbuf);
				*kent = 11;
				goto done;
			}
			else
				*kent = 11;
		}
		else
			*kent = 11;
	}
/*
......11th & 12th Entry
......Issue Load Tool Command
*/
	if ((*kent==11)||(*kent==12))
	{
		nc = strlen(NCL_tool_data.major);
		if (nc!=0)
		{
			if (*kent == 11)
			{
				if (!Soutflg) strcpy (cbuf, "*");
				strcat(cbuf, NCL_tool_data.major);
				strcat (cbuf, "/");
				strcpy (ldlm, ",");
				ictinc = 0;
			}
			else
			{
				strcpy (cbuf, " ");
				strcpy (ldlm, " ");
			}
			if (NCL_tool_data.no_loadtl!=0)
			{
				for (i=ictinc; i<NCL_tool_data.no_loadtl; i++)
				{
					if (i!=0)
						strcat (cbuf, ldlm);
					strcpy (ldlm, ",");
					inum   = NCL_tool_data.loadtl[i].parm;
					nc1    = strlen (NCL_tool_data.loadtl[i].value);
					if ((inum>0) && (strcmp (lctool[inum-1], "SAME")!=0)
						&& (inum<=nctool))
					{
						strcpy(lnum, lctool[inum-1]);
					}
					else if (nc1>0)
					{
						strcpy(lnum, NCL_tool_data.loadtl[i].value);
					}
					else
						lnum[0] = '\0';
/*
.......Command is longer than UL_line_len columns
.......We have to issue it in multiple lines
*/
					if (strlen(cbuf)+strlen(lnum)+2>=UL_line_len)
					{
						strcat (cbuf, "$");
						ictinc = i;
						*kent = 12;
						*knc = strlen (cbuf);
						goto done;
					}
/*
......Append minor word
*/
					strcat (cbuf, lnum);
					if (inum==-1) 
						strcpy(ldlm, "=");
				}
			}
			*kent = 13;
			*knc = strlen (cbuf);
			goto done;
		}
		else
			*kent = 13;
	}
/*
......13th+ Entry
......Issue Operator Commands
*/
	if (*kent>=13)
	{
		ifl = 0;
		setncsfl5 (&ifl);
		if ((NCL_tool_data.no_command<=0) || (NCL_tool_data.command==NULL))
		{
			*kent = -1;
			ictinc = 0;
			goto done;
		}
		k = 0;
		for (i=0,j=0; i<NCL_tool_data.no_command;i++)
		{
			if ((NCL_tool_data.command[i]!='\n') && (NCL_tool_data.command[i]!='\r'))
				ccoms[k][j++] = NCL_tool_data.command[i];
			else
			{
				ccoms[k][j] = '\0';
				if ((NCL_tool_data.command[i]=='\r') && (NCL_tool_data.command[i+1]=='\n'))
					i++;
				j = 0;
				k++;
			}
		}
		if (j>0)
		{
			ccoms[k][j] = '\0';
			ncom = k + 1;
		}
		else
			ncom = k;
		it = *kent - 12;
		if (it>ncom)
		{
			*kent = -1;
			ictinc = 0;
			goto done;
		}
		strcpy(cbuf, ccoms[it-1]);
		*knc = strlen (cbuf);
		*kent   = *kent   + 1;
		if (!Soutflg)
		{
			ifl = 1;
			setncsfl5 (&ifl);
		}
	}
/*
.....End of routine
*/
done:;
	if (*kent<0)
	{
		ictinc = 0;
		setins(&Sins_sav[0],&Sins_sav[1]);
	}
	else if (Soutflg)
	{
		insfl[0] = 2; insfl[1] = 2;
		setins(&insfl[0],&insfl[1]);
	}
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_findtl (ktool,cdrawn,kary,cary,knc,knary)
c
c   FUNCTION:  get the partical tool's parameter and drawing info
c				we will do similar function to findtl (which read old version toolib)
c
c   INPUT:  ktool:    -  Tool number to load.
c
c   OUTPUT: cdrawn:   -  Name of Drawing associated with this
c                               tool.
c
c           kary:     -  Array element contains 1 when this user
c                               defined parameter is defined.  Contains
c                               0 if undefined.
c
c           cary:     -  Default text string for each user defined
c                               parameter.
c           knary     -  Highest user defined parameter defined.
c	RETUEN: -1 if error
c
c***********************************************************************/
ncl_findtl (ktool, cdrawn, kary, cary, knary)
double ktool;
int kary[20], *knary;
char cary[20][22], *cdrawn;
{
	int i, inum, status;
	struct TL_tooldata_rec tool_data;

	*knary  = 0;
	for (i=0; i<20; i++)
	{
		kary[i] = 0;
	}
	status = ncl_get_tooldata (ktool, &tool_data);
	if (status==-1) return -1;
/*
......Cutter parameters
*/
	for (i=0; i<6;i++)
	{
		inum = tool_data.cparms[i];
/*
.....only allow 20 parms
*/
		if (inum>20) continue;
		if (inum>0)
		{
			ncl_sprintf(cary[inum-1], &tool_data.cutter[i], 1);
			kary[inum-1] = 1;
			if (inum > *knary) *knary = inum;
		}
		inum = tool_data.dparms[i];
/*
.....only allow 20 parms
*/
		if (inum>20) continue;
		if (inum>0)
		{
			ncl_sprintf(cary[inum-1], &tool_data.pseudo[i], 1);
			kary[inum-1] = 1;
			if (inum > *knary) *knary = inum;
		}
	}
	for (i=0; i<4;i++)
	{
/*
......Shank parameters
*/
		inum = tool_data.sparms[i];
/*
.....only allow 20 parms
*/
		if (inum>20) continue;
		if (inum>0)
		{
			ncl_sprintf(cary[inum-1], &tool_data.satt[i], 1);
			kary[inum-1] = 1;
			if (inum > *knary) *knary = inum;
		}
/*
......Holder parameters
*/
		inum = tool_data.hparms[i];
/*
.....only allow 20 parms
*/
		if (inum>20) continue;
		if (inum>0)
		{
			ncl_sprintf(cary[inum-1], &tool_data.hatt[i], 1);
			kary[inum-1] = 1;
			if (inum > *knary) *knary = inum;
		}
/*
......Symbol parameters
*/
		inum = tool_data.yparms[i];
/*
.....only allow 20 parms
*/
		if (inum>20) continue;
		if (inum>0)
		{
			ncl_sprintf(cary[inum-1], &tool_data.catt[i], 1);
			kary[inum-1] = 1;
			if (inum > *knary) *knary = inum;
		}
	}
	for (i=0; i<tool_data.no_loadtl;i++)
	{
/*
.....only allow 20 parms
*/
		if (inum>20) continue;
		inum = tool_data.loadtl[i].parm;
		if (inum>0)
		{
			strcpy(cary[inum-1], &tool_data.loadtl[i].value);
			kary[inum-1] = 1;
			if (inum > *knary) *knary = inum;
		}
	}
	strcpy(cdrawn, tool_data.drawing);
	if (tool_data.command!=NULL)
		uu_free (tool_data.command);
	if (tool_data.plabel!=NULL)
		uu_free (tool_data.plabel);
	if (tool_data.loadtl!=NULL)
		uu_free (tool_data.loadtl);
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  ncl_gettparms(ktool, cparms)
c
c   FUNCTION:  get the partical tool's parameter "parameter label"
c
c   INPUT:  ktool:    -  Tool number to read.
c
c   OUTPUT: 
c           cparms:   -  parameter label
c	RETUEN: -1 if error
c
c***********************************************************************/
ncl_gettparms (ktool, cparms)
double ktool;
char cparms[20][80];
{
	int i, j, k, status;
	struct TL_tooldata_rec tool_data;

	for (i=0; i<20; i++)
	{
		cparms[i][0] = '\0';
	}
	status = ncl_get_tooldata (ktool, &tool_data);
	if (status==-1) return -1;
	k = 0;
	for (i=0,j=0; i<tool_data.no_plabel;i++)
	{
		if ((tool_data.plabel[i]!='\n') && (tool_data.plabel[i]!='\r'))
			cparms[k][j++] = tool_data.plabel[i];
		else
		{
			cparms[k][j] = '\0';
			if ((tool_data.plabel[i]=='\r') && (tool_data.plabel[i+1]=='\n'))
				i++;
			j = 0;
			k++;
			if (k>=20) break;
		}
	}
	if ((j>0)&&(k<20))
	{
		cparms[k][j] = '\0';
	}
	if (tool_data.command!=NULL)
		uu_free (tool_data.command);
	if (tool_data.plabel!=NULL)
		uu_free (tool_data.plabel);
	if (tool_data.loadtl!=NULL)
		uu_free (tool_data.loadtl);
	return 0;
}


/**********************************************************************
**    I_FUNCTION : ncl_load_toolbat(filename)
**       load a batch (text) tool library
**    PARAMETERS
**       INPUT  :
**				filename: batch file to be loaded
**       OUTPUT : None
**			
**    RETURNS      : -1: failed. 0: OK
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_load_toolbat(filename)
char *filename;
{
	char loadtl_str[2000], cutter_str[2000], comdtmp[2000], labeltmp[2000];
	int itype, utype_no;
	int  i, numint, status; 
	FILE *fstream;
	char msg[400], buf[256], *indx, token[80], leftstr[80];
	char tempstr[80], *tok;
	int len;
	double low_tool, high_tl;

	utype_no = 0;
	fstream = fopen(filename, "r");
	if(fstream == 0) 
	{
		sprintf(msg, "Can't open batch file %s", filename);
		ud_wrerr(msg);
		return -1;
	}
/*
......delete the old one and init new one
*/
	ncl_tool_reinitlib();
	strcpy (NCL_tool_head.name,filename);
	low_tool = 1000000000000000;
	high_tl = 0;
	labeltmp[0] = '\0';
	comdtmp[0] = '\0';
/*
.....Read a record
*/
	rewind(fstream);
	itype = -1;
	do
	{
		status = ul_fread (fstream,buf,sizeof(buf),&numint);
#if UU_COMP!=UU_WIN2K
		if (status == UX_EOF)
		{
			status = UU_SUCCESS;
			goto read_done;
		}
#else
		if ((numint<=0)&&(status == UX_EOF))
		{
			status = UU_SUCCESS;
			goto read_done;
		}
		else if ((status == UX_EOF)&&(numint>0))
		{
			buf[numint+1] = '\0';
			status = UU_SUCCESS;
		}
#endif
		else
			buf[numint] = '\0';
		while (numint>0)
		{
			if ((buf[numint-1]==' ') || (buf[numint-1]=='\t')
				||(buf[numint-1]=='\r') || (buf[numint-1]=='\n'))
				numint--;
			else
				break;
		}
		buf[numint] = '\0';
		if (itype==1)
		{
			if (strcmp (buf, "##END##")==0)
			{
/*
.....remove last "\n"
*/
				if (NCL_tool_data.no_command>=1)
				{
					NCL_tool_data.no_command -= 1;
					NCL_tool_data.command[NCL_tool_data.no_command] = '\0';
				}
				comdtmp[0] = '\0';
				itype = -1;
				continue;
			}
			if (NCL_tool_data.command!=NULL)
			{
				strcpy(comdtmp, NCL_tool_data.command);
				uu_free (NCL_tool_data.command);
			}
			len = strlen (comdtmp) + numint + 1;
			NCL_tool_data.command = (char *)uu_malloc ( (len+1)*sizeof(char) );
			strcpy(NCL_tool_data.command, comdtmp);
			strcat(NCL_tool_data.command, buf);
			strcat(NCL_tool_data.command, "\n");
			NCL_tool_data.no_command = len;
			continue;
		}
		else if (itype==2)
		{
			if (strcmp (buf, "##END##")==0)
			{
/*
.....remove last "\n"
*/
				if (NCL_tool_data.no_plabel>=1)
				{
					NCL_tool_data.no_plabel -= 1;
					NCL_tool_data.plabel[NCL_tool_data.no_plabel] = '\0';
				}
				itype = -1;
				labeltmp[0] = '\0';
				continue;
			}
			if (NCL_tool_data.plabel!=NULL)
			{
				strcpy(labeltmp, NCL_tool_data.plabel);
				uu_free (NCL_tool_data.plabel);
			}
			len = strlen (labeltmp) + numint + 1;
			NCL_tool_data.plabel = (char *)uu_malloc ( (len+1)*sizeof(char) );
			strcpy(NCL_tool_data.plabel, labeltmp);
			strcat(NCL_tool_data.plabel, buf);
			strcat(NCL_tool_data.plabel, "\n");
			NCL_tool_data.no_plabel = len;
			continue;
		}
		else if (itype==3)
		{
			if (numint==0) continue;
			len = strlen(buf);
			if (buf[len-1]=='$')
			{
				buf[len-1] = '\0';
				itype= 3;
			}
			else
				itype = -1;
			strcat (loadtl_str, buf);
			if (itype==-1)
			{
				ncl_parse_loadtl(loadtl_str);
			}
			continue;
		}
		else if (itype==4)
		{
			if (numint==0) continue;
			len = strlen(buf);
			if (buf[len-1]=='$')
			{
				buf[len-1] = '\0';
				itype= 4;
			}
			else
				itype = -1;
			strcat (cutter_str, buf);
			if (itype==-1)
			{
				ncl_parse_cutter(cutter_str);
			}
			continue;
		}
		if (numint==0) continue;
/*
.....cut the string to 80 chars
.....because it may have syntax error to have longer string
.....but we only allow less than 80
*/
		strncpy(tempstr, buf,79);
		tempstr[79] = '\0';
		if (tempstr[0]=='#')
		{
			if (strncmp (&(tempstr[1]), "LIBRARY", 7)==0)
			{
/*				tok = (char*)strtok (&(tempstr[9]), " #\r\n");
				if (tok!=NULL)
					strcpy (NCL_tool_head.description, tok);
*/
				if (tempstr[8]=='#')
					strcpy(NCL_tool_head.description, &(tempstr[10]));
				continue;
			}
			else if (strncmp (&(tempstr[1]), "TOOL", 4)==0)
			{
				tok = (char*)strtok (&(tempstr[5]), "#");
				if (tok==NULL)
				{
/*
.....wrong, it must be a tool number
*/
					ud_wrerr("Error parsing record");
					goto error;
				}
/*
.....save the previos tool data
*/
				if (NCL_tool_data.toolno>0)
				{
					if (NCL_tool_data.sshade==-1)
						NCL_tool_data.sshade = NCL_tool_data.shade;
					if (NCL_tool_data.hshade==-1)
						NCL_tool_data.hshade = NCL_tool_data.shade;
					ncl_store_tool (&NCL_tool_data);
					if (low_tool>NCL_tool_data.toolno) 
						low_tool = NCL_tool_data.toolno;
					if (high_tl<NCL_tool_data.toolno)
						high_tl = NCL_tool_data.toolno;
					if (NCL_tool_data.command!=NULL)
						uu_free (NCL_tool_data.command);
					if (NCL_tool_data.plabel!=NULL)
						uu_free (NCL_tool_data.plabel);
					if (NCL_tool_data.loadtl!=NULL)
						uu_free (NCL_tool_data.loadtl);
					ncl_inittool_data(&NCL_tool_data);
				}
				NCL_tool_data.toolno = atof(tok);
				tok = (char*)strtok (NULL, "#\r\n");
				if (tok!=NULL)
					strcpy (NCL_tool_data.description, &(tok[1]));
				itype = 0;
			}
			else if (strncmp (&(tempstr[1]), "COMMANDS", 8)==0)
			{
				tok = (char*)strtok (&(tempstr[9]), "#");
				if (tok==NULL)
				{
/*
.....wrong, it must be a tool number
*/
					ud_wrerr("Error parsing record");
					goto error;
				}
				NCL_tool_data.toolno = atof(tok);
				itype = 1;
			}
			else if (strncmp (&(tempstr[1]), "PARMLBS", 7)==0)
			{
				tok = (char*)strtok (&(tempstr[8]), "#");
				if (tok==NULL)
				{
/*
.....wrong, it must be a tool number
*/
					ud_wrerr("Error parsing record");
					goto error;
				}
				NCL_tool_data.toolno = atof(tok);
				itype = 2;
			}
			else if (strcmp (tempstr, "##END##")==0)
			{
				itype = -1;
			}
		}
		else
		{
			indx = (char*)strchr(tempstr, '/');
			if (indx==NULL)
			{
				ud_wrerr("Error parsing record");
				goto error;
			}
			*indx = '\0';
			strcpy(token, tempstr);
			strcpy(leftstr, indx + 1);
			if (strcmp(token,"SYMLIB")==0)
			{
				strcpy(NCL_tool_head.symlib, leftstr);
			}
			else if (strcmp(token,"PROFLIB")==0)
			{
				strcpy(NCL_tool_head.proflib, leftstr);
			}
			else if (strcmp(token,"CREATED")==0)
			{
				tok = (char*)strtok (leftstr, " \t\r\n");
				if (tok==NULL)
				{
					ud_wrerr("Error parsing record");
					goto error;
				}
				strcpy(NCL_tool_head.create_date, tok);
				tok = (char*)strtok (NULL, " \t\r\n");
				if (tok==NULL)
				{
					ud_wrerr("Error parsing record");
					goto error;
				}
				strcpy(NCL_tool_head.create_time, tok);
			}
			else if (strcmp(token,"MODIFIED")==0)
			{
				tok = (char*)strtok (leftstr, " \t\r\n");
				if (tok==NULL)
				{
					ud_wrerr("Error parsing record");
					goto error;
				}
				strcpy(NCL_tool_head.mod_date, tok);
				tok = (char*)strtok (NULL, " \t\r\n");
				if (tok==NULL)
				{
					ud_wrerr("Error parsing record");
					goto error;
				}
				strcpy(NCL_tool_head.mod_time, tok);
			}
			else if (strcmp(token,"UNITS")==0)
			{
				if (strcmp (leftstr, "INCH")==0)
					NCL_tool_head.units = 1;
				else
					NCL_tool_head.units = 2;
			}
			else if (strcmp(token,"CUTTER")==0)
			{
				len = strlen(leftstr);
				if (leftstr[len-1]=='$')
				{
					leftstr[len-1] = '\0';
					itype= 4;
				}
				else
					itype = -1;
				strcpy (cutter_str, leftstr);
				ncl_parse_cutter(cutter_str);
			}
			else if (strcmp(token,"DRAWING")==0)
			{
				strcpy (NCL_tool_data.drawing, leftstr);
			}
			else if (strcmp(token,"TYPENO")==0)
			{
				NCL_tool_head.utype_no = atoi(leftstr);
				utype_no = 0;
				if (NCL_tool_head.utype_no>0)
				{
					NCL_tool_head.utype = (char**)uu_malloc(NCL_tool_head.utype_no*sizeof(char*));
					for (i=0; i<NCL_tool_head.utype_no;i++)
					{
						NCL_tool_head.utype[i] = (char *)uu_malloc((81)*sizeof(char));
					}
				}
			}
			else if (strcmp(token,"UTYPE")==0)
			{
				strcpy(NCL_tool_head.utype[utype_no++], leftstr);
			}
			else
/*
.....load tool parameters
*/
			{
				strcpy(NCL_tool_data.major, token);
				len = strlen(leftstr);
				if (leftstr[len-1]=='$')
				{
					leftstr[len-1] = '\0';
					itype= 3;
				}
				else
					itype= -1;
				strcpy (loadtl_str, leftstr);
				if (itype==-1)
					ncl_parse_loadtl(loadtl_str);
			}
		}
	} while (status == UU_SUCCESS);
read_done:;	
	if (NCL_tool_data.toolno>0)
	{
		if (NCL_tool_data.sshade==-1)
			NCL_tool_data.sshade = NCL_tool_data.shade;
		if (NCL_tool_data.hshade==-1)
			NCL_tool_data.hshade = NCL_tool_data.shade;
		ncl_store_tool (&NCL_tool_data);
		if (low_tool>NCL_tool_data.toolno) 
			low_tool = NCL_tool_data.toolno;
		if (high_tl<NCL_tool_data.toolno)
			high_tl = NCL_tool_data.toolno;
		NCL_tool_head.low_tool = low_tool;
		NCL_tool_head.high_tool = high_tl;
	}
	goto done;
error:;
	status = -1;
done:;
	fclose(fstream);
	len = strlen (filename);
	stllib (filename,&len);
	return status;
}

/*********************************************************************
**  E_FUNCTION: ncl_parse_loadtl(loadtl_str)
**          This function parse the load tool parameters
**
**
**    PARAMETERS
**       INPUT  :   loadtl_str: load tool command string to be parsed
**					
**       OUTPUT :   none
**    RETURNS      : UU_SUCCESS if no problems, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
void ncl_parse_loadtl(loadtl_str)
char *loadtl_str;
{
	char *tok, lodstr[2000];
	int i,j,len;
	char lodtok[20][40], tempstr[40],tempstr2[40],*tmp2;;

/*
.....remove spaces, tab and newline first
*/
	len = strlen(loadtl_str);
	for (i=0,j=0; i<len;i++)
	{
		if ((loadtl_str[i]!=' ')&&(loadtl_str[i]!='\t')&&(loadtl_str[i]!='\n')
			&&(loadtl_str[i]!='\r'))
			lodstr[j++] = loadtl_str[i];
	}
	lodstr[j++] = '\0';
	NCL_tool_data.no_loadtl = 0;
	NCL_tool_data.loadtl = NULL;
	tok = (char*)strtok (lodstr, ",");
	while (tok!=NULL)
	{
		i = NCL_tool_data.no_loadtl;
		if (i>20) break;
		strcpy(lodtok[i], tok);
		NCL_tool_data.no_loadtl++;
		tok = (char*)strtok (NULL, ",");
	}
	if (NCL_tool_data.no_loadtl==0)
		return;
	NCL_tool_data.loadtl = (struct TL_loadtl_rec*)
			uu_malloc (NCL_tool_data.no_loadtl*2*sizeof (struct TL_loadtl_rec));

	for (i=0,j=0; i<NCL_tool_data.no_loadtl; i++,j++)
	{
		strcpy(tempstr, lodtok[i]);
		strcpy(tempstr2, lodtok[i]);
start:;
		tok = (char*)strtok (tempstr, "=_");
		if (tok!=NULL)
		{
			strcpy(NCL_tool_data.loadtl[j].value, tok);
			tmp2 = tempstr2+strlen(tok);
			if (tmp2[0]=='=')
			{
				NCL_tool_data.loadtl[j].parm = -1;
				j++;
				strcpy(tempstr, tempstr2+strlen(tok)+1);
				strcpy(tempstr2, tempstr);
				goto start;
			}
			else if (tmp2[0]=='\0')
			{
				NCL_tool_data.loadtl[j].parm = 0;
				continue;
			}
			else
			{
				tok = (char*)strtok (NULL, "_");
				if (tok!=NULL)
				{
					NCL_tool_data.loadtl[j].parm = atoi (tok);
				}
				else
					NCL_tool_data.loadtl[j].parm = 0;
				continue;
			}
		}
		else
		{
			NCL_tool_data.loadtl[j].parm = 0;
			strcpy(NCL_tool_data.loadtl[j].value, lodtok[i]);
			continue;
		}
	}
	NCL_tool_data.no_loadtl = j;
	if (NCL_tool_data.no_loadtl>0)
		NCL_tool_data.floadtl = 1;
}
/*********************************************************************
**  E_FUNCTION: ncl_parse_cutter(cutterstr)
**          This function parse the cutter command string
**
**
**    PARAMETERS
**       INPUT  :   cutterstr: cutter command string to be parsed
**					
**       OUTPUT :   none
**    RETURNS      : UU_SUCCESS if no problems, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
ncl_parse_cutter(cutterstr)
char *cutterstr;
{
	char tempstr[80], *tok;
	UU_REAL ary[6], ary2[6];
	int i, j, inum, status, parm[6],fholder;

	status = 0;
	strcpy(tempstr, cutterstr);
	tok = (char*)strtok(tempstr, ", \r\n");
	if (tok==NULL)
		return -1;

	if (strcmp (tok, "TYPE")==0)
	{
		tok = (char*)strtok(NULL, ", \r\n");
		if (tok==NULL)
			return -1;
		NCL_tool_data.ctype = atoi (tok);
	}
	else if (strcmp (tok, "PSEUDO")==0)
	{
		tok = (char*)strtok(NULL, ", \r\n");
		if (tok==NULL)
			return -1;
		status = ncl_parse_values(tok, ary, &inum);
/*
......number
*/
		if ((status==0) && (inum!=0))
		{
			i = 0;
			while (tok!=NULL)
			{
				if (i>5) return -1;
				status = ncl_parse_values(tok, ary, &inum);
				if (status!=0) goto error;
				if (inum==0) goto done;
				NCL_tool_data.pseudo[i] = ary[0];
				if (inum==2)
					NCL_tool_data.dparms[i] = (int)ary[1];
				tok = (char*)strtok(NULL, ", \r\n");
				i++;
			}	
			if (i>0) NCL_tool_data.fpseudo = 1;
			NCL_tool_data.ndvals = i;
		}
		else
			return -1;
	}
	else if (strcmp (tok, "DISPLY")==0)
	{
		tok = (char*)strtok(NULL, ", \r\n");
		if (tok==NULL)
			return -1;
		status = ncl_parse_values(tok, ary, &inum);
/*
......number
*/
		if ((status==0) && (inum!=0))
		{
			i = 0;
			while (tok!=NULL)
			{
				if (i>5) return -1;
				status = ncl_parse_values(tok, ary, &inum);
				if (status!=0) goto error;
				if (inum==0) goto done;
				NCL_tool_data.pseudo[i] = ary[0];
				if (inum==2)
					NCL_tool_data.dparms[i] = (int)ary[1];
				tok = (char*)strtok(NULL, ", \r\n");
				i++;
			}	
			if (i>0) NCL_tool_data.fpseudo = 1;
			NCL_tool_data.ndvals = i;
		}
/*
.....CUTTER/DISPLY,(ALL,PART)
*/
		else if (strcmp (tok, "ALL")==0)
		{
			NCL_tool_data.segments = 2;
		}
		else if (strcmp (tok, "PART")==0)
		{
			NCL_tool_data.segments = 1;
		}
/*
.....CUTTER/DISPLY,MOVE,(ON,OFF)
*/
		else if (strcmp (tok, "MOVE")==0)
		{
			tok = (char*)strtok(NULL, ", \r\n");
			if (tok==NULL)
				goto error;
			if (strcmp (tok, "ON")==0)
			{
				NCL_tool_data.move = 1;
			}
			else if (strcmp (tok, "OFF")==0)
			{
				NCL_tool_data.move = 2;
			}
		}
/*
.....CUTTER/DISPLY,SHADE,(ON,SMOOTH,OFF) old version still have "SMOOTH"
*/
		else if (strcmp (tok, "SHADE")==0)
		{
			tok = (char*)strtok(NULL, ", \r\n");
			if (tok==NULL)
				goto error;
			if (strcmp (tok, "ON")==0)
			{
				tok = (char*)strtok(NULL, ", \r\n");
				if (tok==NULL)
					NCL_tool_data.shade = 1;
				else if (strcmp (tok, "SHANK")==0)
					NCL_tool_data.sshade = 1;
				else if (strcmp (tok, "HOLDER")==0)
					NCL_tool_data.hshade = 1;
			}
			else if (strcmp (tok, "SMOOTH")==0)
			{
				NCL_tool_data.shade = 1;
			}
			else if (strcmp (tok, "OFF")==0)
			{
				tok = (char*)strtok(NULL, ", \r\n");
				if (tok==NULL)
					NCL_tool_data.shade = 2;
				else if (strcmp (tok, "SHANK")==0)
					NCL_tool_data.sshade = 2;
				else if (strcmp (tok, "HOLDER")==0)
					NCL_tool_data.hshade = 2;
			}
		}
/*
.....CUTTER/DISPLY,SHANK
*/
		else if (strcmp (tok, "SHANK")==0)
		{
			i = 0;
			fholder = 0;
			NCL_tool_data.fshank = 1;
			ary2[0] = ary2[1] = ary2[2] = ary2[3] = 0.0;
			parm[0] = parm[1] = parm[2] = parm[3] = 0;
			tok = (char*)strtok(NULL, ", \r\n");
			if (tok==NULL)
				goto error;
			while (tok!=NULL)
			{				
/*
......the old version still have "CUTTER, HOLDER"
......the new version don't have it
*/
				if (strcmp (tok, "NONE")==0)
				{
					NCL_tool_data.fshank = 0;
					NCL_tool_data.fholder = 0;
				}
				else if (strcmp (tok, "CUTTER")==0)
				{
					NCL_tool_data.fshank = 1;
				}
				else if (strcmp (tok, "HOLDER")==0)
				{
/*					NCL_tool_data.fshank = 0; */
/*					NCL_tool_data.fholder = 1; */
					NCL_tool_data.fshank = 2;
					fholder = 1;
				}
				else
				{
/*
.....parameters
*/
					status = ncl_parse_values(tok, ary, &inum);
					if ((status!=0)|| (inum==0))
					{
						if (i!=0) goto error;
/*
......it must be symbol name
*/
						strcpy(NCL_tool_data.symshk, tok);
						NCL_tool_data.fshank = 1;
						tok = (char*)strtok(NULL, ", \r\n");
						continue;
					}
/*
......number
*/
					if ((status==0) && (inum!=0))
					{
						ary2[i] = ary[0];
						if (inum==2)
							parm[i] = (int)ary[1];
						else
							parm[i] = 0;
						i++;
					}
					if (i>4) goto error;
				}
				tok = (char*)strtok(NULL, ", \r\n");
			}
			for (j=0; j<4;j++)
			{
				if (NCL_tool_data.fshank)
				{
					NCL_tool_data.satt[j] = ary2[j];
					NCL_tool_data.sparms[j] = parm[j];
				}
				if (fholder)
				{
					NCL_tool_data.hatt[j] = ary2[j];
					NCL_tool_data.hparms[j] = parm[j];
				}
			}
		}
		else if (strcmp (tok, "HOLDER")==0)
		{
			NCL_tool_data.fholder = 1;
			tok = (char*)strtok(NULL, ", \r\n");
			if (tok==NULL)
				goto error;
			i = 0;
			while (tok!=NULL)
			{				
				status = ncl_parse_values(tok, ary, &inum);
				if ((status!=0)|| (inum==0))
				{
					if (i!=0) goto error;
/*
......it must be symbol name
*/
					strcpy(NCL_tool_data.symhld, tok);
					NCL_tool_data.fholder = 1;
					tok = (char*)strtok(NULL, ", \r\n");
					continue;
				}
/*
......number
*/
				if (i>3) return -1;
				NCL_tool_data.hatt[i] = ary[0];
				if (inum==2)
					NCL_tool_data.hparms[i] = (int)ary[1];
				i++;
				tok = (char*)strtok(NULL, ", \r\n");
			}	
		}
/*
.....the token is not a word and not a number, so it must be a symbol name
.....CUTTER/DISPLY,symbol
*/
		else
		{
			strcpy(NCL_tool_data.symbol, tok);
			NCL_tool_data.fsymbol = 1;
			tok = (char*)strtok(NULL, ", \r\n");
			if (tok==NULL) goto done;
			if (NCL_tool_data.ctype>=10)
			{
				if (strcmp (tok, "OFFSET")==0)
				{
					tok = (char*)strtok(NULL, ", \r\n");
					if (tok==NULL) goto error;
					status = ncl_parse_values(tok, ary, &inum);
					if ((status!=0)|| (inum==0))
						goto error;									
					NCL_tool_data.catt[0] = ary[0];
					if (inum==2)
						NCL_tool_data.yparms[0] = (int)ary[1];
					tok = (char*)strtok(NULL, ", \r\n");
					if (tok==NULL) goto error;
					status = ncl_parse_values(tok, ary, &inum);
					if ((status!=0)|| (inum==0))
						goto error;									
					NCL_tool_data.catt[1] = ary[0];
					if (inum==2)
						NCL_tool_data.yparms[1] = (int)ary[1];
				}
			}
		}
	}
	else
	{
		i = 0;
		while (tok!=NULL)
		{
			if (i>5) return -1;
			status = ncl_parse_values(tok, ary, &inum);
			if (status!=0) goto error;
			if (inum==0) goto done;
			NCL_tool_data.cutter[i] = ary[0];
			if (inum==2)
				NCL_tool_data.cparms[i] = (int)ary[1];
			tok = (char*)strtok(NULL, ", \r\n");
			i++;
		}		
		NCL_tool_data.ncvals = i;
	}
	goto done;
error: status = -1;
done:;
	return(status);
}
/*********************************************************************
**  E_FUNCTION: ncl_parse_values(str, ary, inum)
**          This function converts an Ascii string into an array of
**          2 real numbers.  Each number in the string should be
**			separated by '_'. Checking is done to verify that
**          the string contains only numeric data.
**
**			Example: -1.5_3.456
**
**    PARAMETERS
**       INPUT  :   
**					str = input text string
**       OUTPUT :   ary = Real array to receive converted numbers.
**					inum = Number of real numbers converted.
**    RETURNS      : UU_SUCCESS if no problems, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
ncl_parse_values(str, ary, inum)
UU_REAL ary[];
int *inum;
char *str;
{
	int status,i,idot,nc;
	char *p,*pe,*es;
	char lbuf[80];
/*
.....Assume success
*/
	status = UU_SUCCESS;
	*inum = 0;
/*
.....Zero length string
*/
	if (strlen(str) == 0) goto done;
/*
.....Get next number from string
*/
	p = &str[0];
	es = p + strlen(str);
	do
	{
/*
.....remove heading space and '\t'
*/
		while ((*p==' ') || (*p=='\t')) 
		{
			p++;
		}
		pe = (char*)strchr(p,'_');
		if (pe == 0) pe = es;
		if (pe > p)
		{
			nc = (int)pe - (int)p;
			strncpy (lbuf,p,nc);
			lbuf[nc] = '\0';
/*
.....remove trailing space and '\t'
*/
			for (i=strlen(lbuf); i>=0; i--)
			{
				if ((lbuf[i-1]==' ')||(lbuf[i-1]=='\t'))
					lbuf[i-1] = '\0';
				else
					break;
			}
/*
.....Check for valid number
*/
			idot = 0;
			for (i=0;i<nc;i++)
			{	
				if (lbuf[i] == '.')
				{
					if (idot == 1) goto failed;
					idot = 1;
				}
				else if (lbuf[i] == '-' || lbuf[i] == '+')
				{
					if (i != 0) goto failed;
				}
				else
				{
					if (isdigit(lbuf[i]) == 0) goto failed;
				}
			}
/*
.....String contains only numeric data
.....Convert it to a number
*/
			ary[*inum] = atof(lbuf);
		}
/*
.....Point to next number in string
*/
		*inum = *inum + 1;
		if (*inum > 2) goto failed;
		p = pe + 1;
	} while (p < es);
	goto done;
/*
.....Failure
*/
failed:;
	status = UU_FAILURE;
done:;
	return(status);
}
/***********************************************************************
c
c   SUBROUTINE:  ncl_inittool_data95 (tool_data)
c
c   FUNCTION:  initial toolib data value: tool_data (old version)
c
c   INPUT:  tool_data: value to initialize
c
c   OUTPUT: none
c	 RETURNS: none
c
c***********************************************************************/
void ncl_inittool_data95(tool_data)
struct TL_tooldata_rec95 *tool_data;
{
	int i;
	tool_data->description[0] = '\0';
	tool_data->symbol[0] = '\0';
	tool_data->symshk[0] = '\0';
	tool_data->symhld[0] = '\0';
	tool_data->drawing[0] = '\0';
	tool_data->major[0] = '\0';

	tool_data->toolno = 0;
	tool_data->ncvals = 0;
	tool_data->ndvals = 0;
	tool_data->ctype = 0;
	tool_data->shade = 0;
/*
.....if not set it = tool_data->shade;
*/
	tool_data->sshade = -1;
	tool_data->hshade = -1;
	tool_data->segments = 0;
	tool_data->move = 0;
	for (i=0; i<6; i++)
	{
		tool_data->cparms[i] = 0;
		tool_data->dparms[i] = 0;
		tool_data->cutter[i] = 0.0;
		tool_data->pseudo[i] = 0.0;
	}
	for (i=0; i<4; i++)
	{
		tool_data->yparms[i] = 0;
		tool_data->sparms[i] = 0;
		tool_data->hparms[i] = 0;
		tool_data->catt[i] = 0.0;
		tool_data->satt[i] = 0.0;
		tool_data->hatt[i] = 0.0;
	}
	tool_data->fpseudo = 0;
	tool_data->fshank = 0;
	tool_data->fholder = 0;
	tool_data->fsymbol = 0;
	tool_data->floadtl = 0;
	tool_data->no_loadtl = 0;
	tool_data->no_plabel = 0;
	tool_data->no_command = 0;
	tool_data->loadtl = NULL;
	tool_data->plabel = NULL;
	tool_data->command = NULL;
}

/*********************************************************************
**  E_FUNCTION: ncl_setcut_outfl()
**       Enables the internally generated commands from a CUTTER/TOOL
**       statement to be output to the part program file.
**
**    PARAMETERS
**       INPUT  : none
**					str = input text string
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
ncl_setcut_outfl()
{
	Soutflg = UU_TRUE;
}

/*********************************************************************
**  E_FUNCTION: ncl_resetcut_outfl()
**       Enables the internally generated commands from a CUTTER/TOOL
**       statement to be output to the part program file.
**
**    PARAMETERS
**       INPUT  : none
**					str = input text string
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
ncl_resetcut_outfl()
{
	Soutflg = UU_FALSE;
}

/*********************************************************************
**  E_FUNCTION: ncl_getcut_outfl()
**       Returns the state of outputting commands from a CUTTER/TOOL
**       statement.
**
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          outfl  = UU_TRUE = Output CUTTER/TOOL commands to the
**                   part program file.  UU_FALSE = Use the CUTTER/TOOL
**                   commands as reference.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
ncl_getcut_outfl(outfl)
int *outfl;
{
	*outfl = Soutflg;
}

/********************************************************************
**  E_FUNCTION: ncl_setcut_outfl()
**      Sets the flag that denotes that the default profile library
**      generated from a CUTTER/PROFIL command is to be used when a
**      cutter is defined.
**
**    PARAMETERS
**       INPUT  : none
**					str = input text string
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none.
********************************************************************/
void ncl_setcut_proffl()
{
	Sprof_flg = UU_TRUE;
}

/********************************************************************
**  E_FUNCTION: ncl_resetcut_outfl()
**      Resets the flag that denotes that the default profile library
**      generated from a CUTTER/PROFIL command is no longer used.
**
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none.
********************************************************************/
void ncl_resetcut_proffl()
{
	Sprof_flg = UU_FALSE;
}

/********************************************************************
**  E_FUNCTION: ncl_getcut_outfl()
**       Retrieve the flag that siginifies whether or not a default
**       profile library is in use.
**
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : outfl - default profile library flag's value
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none.
********************************************************************/
void ncl_getcut_proffl(outfl)
UU_LOGICAL *outfl;
{
	*outfl = Sprof_flg;
}
/*********************************************************************
**    E_FUNCTION         :  ncl_get_tool_type(number, char *add_str)
**       get toolib type list
**
**    PARAMETERS   
**       INPUT  : add_str: added type string
**       OUTPUT : number: number of toolib type
**    RETURNS      : a list of toolib type name
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char **ncl_get_tool_type(number, addstr)
int *number;
char *addstr;
{
	int i, status;
	char **type_list;
	struct TL_toolhead_rec tool_head;

	status = 0;
	*number = 0;
	type_list = NULL;

	ncl_gettool_head (&tool_head);
	if ((addstr!=NULL)&&(addstr[0]!='\0'))
		type_list = (char **) uu_malloc((15+tool_head.utype_no+1)*sizeof(char *));
	else
		type_list = (char **) uu_malloc((15+tool_head.utype_no)*sizeof(char *));
	for (i=0; i<15;i++)
		type_list[i] = (char *) uu_malloc(81 * sizeof(char));	
	strcpy(type_list[*number], "Face Mill");
	(*number)++;
	strcpy(type_list[*number], "End Mill");
	(*number)++;
	strcpy(type_list[*number], "Barrel");
	(*number)++;
	strcpy(type_list[*number], "Cone");
	(*number)++;
	strcpy(type_list[*number], "Bell");
	(*number)++;
	strcpy(type_list[*number], "Drill");
	(*number)++;
	strcpy(type_list[*number], "Boring Tool");
	(*number)++;
	strcpy(type_list[*number], "Reamer");
	(*number)++;
	strcpy(type_list[*number], "Chamfer Tool");
	(*number)++;
	strcpy(type_list[*number], "Blade");
	(*number)++;
	strcpy(type_list[*number], "Square Insert");
	(*number)++;
	strcpy(type_list[*number], "Diamond Insert");
	(*number)++;
	strcpy(type_list[*number], "Triangle Insert");
	(*number)++;
	strcpy(type_list[*number], "Circular Insert");
	(*number)++;
	strcpy(type_list[*number], "Grooving Tool");
	(*number)++;

	for (i=0;i<tool_head.utype_no;i++)
	{
		type_list[*number] = (char *) uu_malloc(81 * sizeof(char));
		strcpy(type_list[*number], tool_head.utype[i]);
		(*number)++;
	}
	if ((addstr!=NULL)&&(addstr[0]!='\0'))
	{
		type_list[*number] = (char *) uu_malloc(81 * sizeof(char));
		strcpy(type_list[*number], addstr);
		(*number)++;
	}
	return (type_list);
}

/*********************************************************************
**    E_FUNCTION         :  ncl_get_tool_tlist(tlist, filter_type)
**       get all toolib list as a particulat format
**
**    PARAMETERS   
**       INPUT  : 
**				filter_type: tool list only for this type
**       OUTPUT : a list of toolib list
**    RETURNS      : number: number of tools 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_tool_tlist(tlist, filter_type)
UD_TLIST *tlist;
int filter_type;
{
	int i, status, ctype,len;
	char buf[81], **typept;
	struct TL_tooldata_rec *p1;
	int number, typ_num;
	struct TL_toolhead_rec tool_head;
	UD_LIST type_list;

	status = 0;
	number = 0;	
	typ_num = 0;

	ncl_gettool_head (&tool_head);
	ncl_get_toolist_pt(&p1);
	if (p1!=NULL)
		p1 = (struct TL_tooldata_rec *) uu_lsnext(p1);
/*
......toolno	type	Description
*/
	tlist->num_col = 7; 
	tlist->num_item = tool_head.no_tools;
	tlist->answer = 0;

	if (tlist->num_col>0)
		tlist->col_label = (char**) uu_malloc(tlist->num_col*sizeof (char*));
	for (i=0; i<tlist->num_col;i++)
	{
		tlist->col_label[i] = (char*) uu_malloc(20*sizeof(char));
	}
	strcpy(tlist->col_label[0], "Toolno");
	strcpy(tlist->col_label[1], "Type");
	strcpy(tlist->col_label[2], "Description");
	strcpy(tlist->col_label[3], "Diameter");
	strcpy(tlist->col_label[4], "Radius");
	strcpy(tlist->col_label[5], "Height");
	strcpy(tlist->col_label[6], "Angle");
	if (tlist->num_item>0)
		tlist->data = (UD_ITEMDATA *) uu_malloc(tlist->num_item*sizeof(UD_ITEMDATA));
	else
		tlist->data = NULL;
	for (i=0; i<tlist->num_item;i++)
	{
		(tlist->data[i]).itemnum = tlist->num_col;
		(tlist->data[i]).data_items = 
					(char **) uu_malloc(tlist->num_col*sizeof(char*));
	}
/*
.....even list len is 0, we still need column label to create the empty
.....table list in order to add list item later
*/
	if (tool_head.no_tools==0)
		return 0;
	while (p1)
	{
		ctype = p1->ctype;
		if ((ctype == filter_type || filter_type == -1)) 
		{
/*
......data_items[0] for Toolno
*/
			sprintf (buf, "%-15.0f", p1->toolno);
			len = strlen(buf);
			tlist->data[number].data_items[0] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(tlist->data[number].data_items[0], buf);
/*
......data_items[2] for Description
*/
			len = strlen(p1->description);
			tlist->data[number].data_items[2] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(tlist->data[number].data_items[2], p1->description);
/*
......data_items[1] for type
*/
			type_list.item = ncl_get_tool_type(&(type_list.num_item), UU_NULL);
			if (type_list.num_item>0)
			{
				strcpy (buf, type_list.item[ctype]);
			}
			else
				strcpy (buf, " ");
			len = strlen(buf);
			tlist->data[number].data_items[1] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(tlist->data[number].data_items[1], buf);
/*
......data_items[3] for Diameter
*/	
			if (p1->cutter[0]==0.0)
				buf[0] = '\0';
			else
				sprintf(buf, "%f", p1->cutter[0]);
			len = strlen(buf);
			tlist->data[number].data_items[3] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(tlist->data[number].data_items[3], buf);
/*
......data_items[4] for Radius
*/	
			if (p1->cutter[1]==0.0)
				buf[0] = '\0';
			else
				sprintf(buf, "%f", p1->cutter[1]);
			len = strlen(buf);
			tlist->data[number].data_items[4] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(tlist->data[number].data_items[4], buf);
/*
......data_items[5] for Height
*/	
			if (p1->cutter[2]==0.0)
				buf[0] = '\0';
			else
				sprintf(buf, "%f", p1->cutter[2]);
			len = strlen(buf);
			tlist->data[number].data_items[5] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(tlist->data[number].data_items[5], buf);
/*
......data_items[6] for Angle
*/	
			if (p1->cutter[3]==0.0)
				buf[0] = '\0';
			else
				sprintf(buf, "%f", p1->cutter[3]);
			len = strlen(buf);
			tlist->data[number].data_items[6] = (char*)uu_malloc((len+1)*sizeof(char));
			strcpy(tlist->data[number].data_items[6], buf);
			number++;
		}
		p1 = (struct TL_tooldata_rec *) uu_lsnext(p1);
	}
/*
.....even list len is 0, we still need column label to create the empty
.....table list in order to add list item later, so don't free list's col_label
.....it never changed when form display, we only free it after form done.
*/
	if (number==0)
	{
		for (i=0; i<tlist->num_item; i++)
		{
			if (tlist->data[i].itemnum>0)
			{
				if (tlist->data[i].data_items!=NULL)
					uu_free(tlist->data[i].data_items);
				tlist->data[i].data_items = NULL;
			}
		}
		if ((tlist->num_item)&&(tlist->data!=NULL))
			uu_free(tlist->data);
		tlist->data = NULL;
	}
	return number;
}

