#include "usysdef.h"
/***********************************************************************
**
**   FILE NAME:  toolioc.c
**			contains load/save functions
**	
**   CONTAINS:
**			tool_load(filename, &kerr);
**			tool_get_toolno(tool)
**			toolc_savlib(filename);
**			tool_addrec(tool_data)
**			tool_delrec(toolnum)
**			tool_findtool(find_str, pos)
**			tool_init_tool(cont, err)
**			tool_loadbat(filename, &kerr);
**			tool_createbat(filename);
**			toolc_list(filename, flag)
**			tool_list_header(fstream, page_num)
**			tool_list_simple(fstream, tool_data)
**			tool_list_full(fstream, tool_data)
**			tool_getprmt_string(cuttype, prompt, indx)
**			tool_getprmt_string2(flag, tool_data, prompt, indx)
**			tool_parse_opt(opt_str, tool_option, filename)
**			tool_open_cutprof(nprofs)
**   
**    COPYRIGHT 2006(c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       toolioc.c , 25.5
**    DATE AND TIME OF LAST MODIFICATION
**       01/20/17 , 11:45:19
***********************************************************************/
#include <stdio.h>
#include "string.h"
#include "xfsys1.h"
#include "xenv1.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "ulist.h"
#include "nclfc.h"
#include "toolibdata.h"
#include "nclver.h"

#if (UU_COMP == UU_WIN2K)
#include <string.h>
#define index strchr
#define rindex strrchr
#endif

void ul_to_upper(char*);

struct S_tools_struc
{
	char label[21];
	char clas[21];
};

struct TL_toolopt_rec
{
	char load_file[256];
	char save_file[256];
	int	option;
};
int Tool_orig_unit=1, Tool_list_pagelen=66;
/*
.....save the current tool head/data
*/
struct TL_toolhead_rec Tool_head = {"","","","","","","","","",0,0,0,0,0,0,NULL};
struct TL_tooldata_rec Tool_current_data;
int Tool_data_num = 0;
int Tool_modify = 0;
double Current_sel_tool = -1;
void tool_getprmt_string(int, char*, int);
void tool_getprmt_string2(int, struct TL_tooldata_rec *, char*, int);
void tool_parse_opt(char*, struct TL_toolopt_rec *, char*);
void tool_list_full(FILE *, struct TL_tooldata_rec *);
void tool_list_simple(FILE *, struct TL_tooldata_rec *);
void tool_list_header(FILE *, int);
void tool_loadbat(char*, int*);

#define uu_move_byte(from,to,n)     (bcopy(from,to,n),to)
extern UU_LIST NCL_plib_tools;

/*********************************************************************
**    E_FUNCTION     : issame (data1, data2)
**       Check if the two data struture is the same.
**    PARAMETERS
**       INPUT  : 
**          data1, data2: Two data structure
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int issame (data1, data2)
struct TL_tooldata_rec *data1, *data2;
{
	int i;
	if (strcmp(data1->description, data2->description)!=0)
		return 0;
	if (strcmp(data1->symbol, data2->symbol)!=0)
		return 0;
	if (strcmp(data1->symshk, data2->symshk)!=0)
		return 0;
	if (strcmp(data1->symhld, data2->symhld)!=0)
		return 0;
	if (strcmp(data1->drawing, data2->drawing)!=0)
		return 0;
	if (strcmp(data1->major, data2->major)!=0)
		return 0;
	if (data1->toolno!=data2->toolno)
		return 0;
	if (data1->ncvals!=data2->ncvals)
		return 0;
	if (data1->ndvals!=data2->ndvals)
		return 0;
	if (data1->ctype!=data2->ctype)
		return 0;
	if (data1->shade!=data2->shade)
		return 0;
	if (data1->sshade!=data2->sshade)
		return 0;
	if (data1->hshade!=data2->hshade)
		return 0;
	if (data1->segments!=data2->segments)
		return 0;
	if (data1->move!=data2->move)
		return 0;
	for (i=0; i<6; i++)
	{
		if (data1->cparms[i]!=data2->cparms[i])
			return 0;
		if (data1->dparms[i]!=data2->dparms[i])
			return 0;
		if (data1->cutter[i]!=data2->cutter[i])
			return 0;
		if (data1->pseudo[i]!=data2->pseudo[i])
			return 0;
	}
	for (i=0; i<4; i++)
	{
		if (data1->yparms[i]!=data2->yparms[i])
			return 0;
		if (data1->sparms[i]!=data2->sparms[i])
			return 0;
		if (data1->hparms[i]!=data2->hparms[i])
			return 0;
		if (data1->catt[i]!=data2->catt[i])
			return 0;
		if (data1->satt[i]!=data2->satt[i])
			return 0;
		if (data1->hatt[i]!=data2->hatt[i])
			return 0;
	}

	if (data1->fpseudo!=data2->fpseudo)
		return 0;
	if (data1->fshank!=data2->fshank)
		return 0;
	if (data1->fholder!=data2->fholder)
		return 0;
	if (data1->floadtl!=data2->floadtl)
		return 0;
	if (data1->no_loadtl!=data2->no_loadtl)
		return 0;
	if (data1->no_plabel!=data2->no_plabel)
		return 0;
	if (((data1->plabel!=NULL)&&(data2->plabel!=NULL)) 
			&&(strcmp(data1->plabel, data2->plabel)!=0))
		return 0;
	for (i=0; i<data1->no_loadtl;i++)
	{
		if (data1->loadtl[i].parm!=data2->loadtl[i].parm)
			return 0;
		if (strcmp(data1->loadtl[i].value, data2->loadtl[i].value)!=0)
			return 0;
	}
	if (data1->no_command!=data2->no_command)
		return 0;
	if (((data1->plabel!=NULL)&&(data2->plabel!=NULL)) 
			&& (strcmp(data1->command, data2->command)!=0))
		return 0;
	return 1;
}
/**********************************************************************
**    I_FUNCTION : tool_load(filename, &kerr);
**       load a toolib file into interal structure
**    PARAMETERS
**       INPUT  :
**				filename: toolib file to be loaded
**       OUTPUT :
**				kerr : 1: failed
**						0= OK
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void tool_load(filename, kerr)
char *filename;
int *kerr;
{
	FILE *fd;
	char msg[400];
	int status;
	char *p, *tool_getenv(), direc[UX_MAX_PATH_LEN], fullname[UX_MAX_PATH_LEN], libfile[UX_MAX_PATH_LEN];
	char fname[UX_MAX_PATH_LEN],fext[UX_MAX_PATH_LEN];
	strcpy (libfile, filename);
	*kerr = 0;
	if (strlen(libfile)==0)
	{
		p = tool_getenv ("NCL_TOOLIB");
		if (p != UU_NULL)
		{
			strcpy(libfile, p);
		}
	}
	tool_break_fname(libfile,direc,fname,fext);
	tool_get_dir(direc,direc);
	tool_build_fname(fullname,direc,fname,fext);

	fd = fopen (fullname,"r");
	if (fd != 0)
	{
		fclose (fd);
		goto load;
	}
	else
	{
/*
.....we will open file without extension at first try, if not there
.....then we use filename.tlb to open
*/
/*
.....if there is no extension, we use ".tlb" as default
*/
		if (fext[0]=='\0')
		{
			tool_build_fname(fullname,direc,fname,"tlb");
			fd = fopen (fullname,"r");
			if (fd != 0)
			{
				fclose (fd);
				goto load;
			}
		}
		if (fext[0]=='\0')
			strcat(fname, ".tlb");
		status = ul_open_mod_file(NULL, NULL, "NCL_TOOL",
						NULL, fname, 2,  &fd);
		if (fd!=NULL)
		{
			fclose(fd);
			strcpy(fullname, fname);
		}
		else
		{
			sprintf(msg, "Can't open file %s to load", libfile);
			tool_mfmsg_box(0, "File open error", msg, 1);
			*kerr = -1;
			return;
		}
	}
load:;
	if (fullname[0]=='\0') return;
	status = ncl_load_tool(fullname);
	if (status==-1)
	{
		sprintf(msg, "Error loading toolib file %s", fullname);
		tool_mfmsg_box(0, "File open error", msg, 1);
		*kerr = -1;
		return;
	}
	ncl_gettool_head (&Tool_head);
	if (Tool_current_data.command!=NULL)
		uu_free (Tool_current_data.command);
	if (Tool_current_data.plabel!=NULL)
		uu_free (Tool_current_data.plabel);
	if (Tool_current_data.loadtl!=NULL)
		uu_free (Tool_current_data.loadtl);
	ncl_getcurrent_tooldata(&Tool_current_data);
	return;
}
/***********************************************************************
c
c   SUBROUTINE:  tool_get_toolno(tool)
c
c   FUNCTION:  This routine get the total tool numbers in the current working 
c				Tool Library 
c
c   INPUT:  
c           tool: total tools
c
c   OUTPUT: none
c
c***********************************************************************/
void tool_get_toolno(tool)
int *tool;
{
	*tool = Tool_head.no_tools;
}
/***********************************************************************
c
c   SUBROUTINE:  toolc_savlib(filename);
c
c   FUNCTION:  This routine saves the current working Tool Library to a
c              disk file.
c
c   INPUT:  
c           cfil      -  Filename to save.  If it is blank, then
c                               the current filename will be used.
c
c   OUTPUT: none
c
c***********************************************************************/
toolc_savlib(filename)
char *filename;
{
	UX_pathname dir,file,ext;
	int i,lrecl, nb, ret, ityp;
	FILE *fstream;
	char msg[400], tempstr[8000];
	struct TL_tooldata_rec *tool_data, locrec;
	int tool_os;
	int *ptro, *ptri;
/*
......check the name type
*/
	tool_break_fname(filename,dir,file,ext);
	if (_stricmp(ext,"TLA") == 0)
		ityp = 0;
	else ityp = 1;
	if (ityp == 0)
	{
		return tool_createbat(filename);
	}

	fstream = fopen(filename, "r");
	if(fstream != 0) 
	{
		fclose (fstream);
		ret = tool_mfyesno("File Exists", "This file already exists, Do you wish to overwrite it?", 0);
		if (ret == 1)
			return 0;
	}
	fstream = fopen(filename, "wb");
	if(fstream == 0) 
	{
		sprintf(msg, "Can't open file %s to write", filename);
		tool_mfmsg_box(0, "File open error", msg, 1);
		return -1;
	}
/*
.....update verson before save
*/
	Tool_head.version = NCL_version;
	lrecl =  sizeof (struct TL_toolhead_rec);
	if (lrecl>HEADREC_SIZE32)
		lrecl = HEADREC_SIZE32;
	nb = 1;
/*
.....write the header structure
*/
	strcpy(Tool_head.name, filename);
/*
.....the heading will be always the current version header
*/
	strcpy (Tool_head.head,"10155UNICAD FILE HEADER DATA\n");

	fwrite(&Tool_head, lrecl, nb, fstream);

	tempstr[0] = '\0'; 
	for (i=0; i<Tool_head.utype_no;i++)
	{
		strcat (tempstr, Tool_head.utype[i]);
		strcat (tempstr, ";");
	}
	lrecl = strlen(tempstr);
	fwrite(&lrecl, sizeof(int), 1, fstream);
	fwrite(tempstr, lrecl, nb, fstream);
	fflush(fstream);
	lrecl =  sizeof (struct TL_tooldata_rec);
/*
.....check this size with 32bit size which is TOOLREC_SIZE32
.....we need always save as 32bit size even though we might run 64bit window
*/
	if (lrecl>TOOLREC_SIZE32)
	{
		lrecl = TOOLREC_SIZE32;
		tool_os = 64;
	}
	else
		tool_os = 32;
	nb = 1;
/*
.....write the data structure size
*/
	fwrite(&lrecl, sizeof(int), 1, fstream);
/*
.....write the tool data
*/
	ncl_get_toolist_pt(&tool_data);
	if (tool_data!=NULL)
		tool_data = (struct TL_tooldata_rec *) uu_lsnext(tool_data);
		
	while (tool_data)
	{
		if (tool_os!=32)
		{
			uu_move_byte(tool_data, &locrec, sizeof(struct TL_tooldata_rec));
			ptro = &(locrec.no_loadtl);
			ptri = &(tool_data->no_loadtl);
			for (i=0; i<NJOINS;i++)
			{
				ptro[i*2] = ptri[i*4];
			}
			fwrite (&locrec, lrecl, nb, fstream);
		}
		else
			fwrite (tool_data, lrecl, nb, fstream);
		for (i=0; i<tool_data->no_loadtl; i++)
		{
			fwrite (&(tool_data->loadtl[i]), sizeof(struct TL_loadtl_rec), nb, fstream);
		}
		if (tool_data->no_plabel)
			fwrite (tool_data->plabel, tool_data->no_plabel+1, nb, fstream);
		if (tool_data->no_command)
			fwrite (tool_data->command, tool_data->no_command+1, nb, fstream);
		tool_data = (struct TL_tooldata_rec *) uu_lsnext(tool_data);
	}
	fclose(fstream);
done:;
	return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  tool_addrec(tool_data)
c
c   FUNCTION:  This routine add a tool into the current working Tool Library
c
c   INPUT:  
c           tool_data      -  tool structure to be added
c
c   OUTPUT: none
c   RETURN: -1: failed. 
c			0: added the tool into the data
c			1: canceled: no error but not update the data
c			1: not update the data because it is the same
c			2: tool exist and updated.
c
c***********************************************************************/
tool_addrec(tool_data)
struct TL_tooldata_rec *tool_data;
{
	struct TL_tooldata_rec *p1;
	char ctime[9], cdate[12];
	int k, n1;

	n1 = sizeof(struct TL_tooldata_rec);
	if (tool_data->toolno<=0)
	{
		tool_mfmsg_box(NULL, "Error!", "Not a valid tool number to be added",
			 0);
		return -1;
	}
	ncdate (cdate);
	cdate[11] = '\0';
	ftim (ctime);
	ctime[8] = '\0';
	ncl_get_toolist_pt(&p1);
	if (p1 != UU_NULL) p1 = (struct TL_tooldata_rec *) uu_lsnext(p1);
	while (p1)
	{
/*
.....check the added data is the same as saved
*/
		if (issame (p1, tool_data))
			return 1;
/*
......check the added data have the same tool number, 
......if yes, replace with the new data
*/
		if (p1->toolno==tool_data->toolno)
		{
			if (Current_sel_tool!=tool_data->toolno)
			{
/*
.....ask if user want to ovrwrite
*/
				int ret = tool_mfyesno("Overwrite?", 
					"This tool already exists in the library, Do you wish to overwrite it?", 0);
				if (ret != 0)
				{
/*
......No
*/
					return 1;
				}
			}
			if (p1->plabel!=NULL)
				uu_free (p1->plabel);
			if (p1->command!=NULL)
				uu_free (p1->command);
			if (p1->loadtl!=NULL)
				uu_free (p1->loadtl);
			uu_move_byte((char*)tool_data, (char*)p1, sizeof(struct TL_tooldata_rec));
			if (tool_data->plabel!=NULL)
			{
				p1->plabel = (char *)uu_malloc ( (p1->no_plabel+1)*sizeof(char) );
				strcpy(p1->plabel, tool_data->plabel);
			}
			if (tool_data->command!=NULL)
			{
				p1->command = (char *)uu_malloc ( (p1->no_command+1)*sizeof(char) );
				strcpy(p1->command, tool_data->command);
			}
			if (tool_data->loadtl!=NULL)
			{
				p1->loadtl = (struct TL_loadtl_rec*)
					uu_malloc (p1->no_loadtl*sizeof (struct TL_loadtl_rec));
				for (k=0; k<p1->no_loadtl; k++)
				{
					p1->loadtl[k].parm = tool_data->loadtl[k].parm;
					strcpy(p1->loadtl[k].value, tool_data->loadtl[k].value);
				}
			}
			strcpy (Tool_head.mod_date, cdate);
			strcpy (Tool_head.mod_time, ctime);
			Tool_head.version = NCL_version;
			ncl_store_toolhead(&Tool_head);
			Tool_modify = 1;
			return 2;
		}
		p1 = (struct TL_tooldata_rec *) uu_lsnext(p1);
	}
	ncl_get_toolist_pt(&p1);
	if (p1 != UU_NULL)
	{
		p1 = (struct TL_tooldata_rec *) uu_lsend(p1);
		if (p1 == UU_NULL) ncl_get_toolist_pt(&p1);
		if (p1 != UU_NULL) 
			p1 = (struct TL_tooldata_rec *) uu_lsinsrt((char*)p1,n1);
		if (p1 != UU_NULL) 
		{
			Tool_head.no_tools++;
//			tool_mfmsg_box(0, "Tool Added", "A new tool added", 0);
		}
	}
	if (p1 == UU_NULL)
		return -1;
/*
.....Store structure
*/
	else
	{
		uu_move_byte((char*)tool_data, (char*)p1, sizeof(struct TL_tooldata_rec));
		if (tool_data->plabel!=NULL)
		{
			p1->plabel = (char *)uu_malloc ( (p1->no_plabel+1)*sizeof(char) );
			strcpy(p1->plabel, tool_data->plabel);
		}
		if (tool_data->command!=NULL)
		{
			p1->command = (char *)uu_malloc ( (p1->no_command+1)*sizeof(char) );
			strcpy(p1->command, tool_data->command);
		}
		if (tool_data->loadtl!=NULL)
		{
			p1->loadtl = (struct TL_loadtl_rec*)
						uu_malloc (p1->no_loadtl*sizeof (struct TL_loadtl_rec));
			for (k=0; k<p1->no_loadtl; k++)
			{
				p1->loadtl[k].parm = tool_data->loadtl[k].parm;
				strcpy(p1->loadtl[k].value, tool_data->loadtl[k].value);
			}
		}
		if (tool_data->toolno>Tool_head.high_tool)
			Tool_head.high_tool = tool_data->toolno;
		if (tool_data->toolno<Tool_head.low_tool)
			Tool_head.low_tool = tool_data->toolno;
		strcpy (Tool_head.mod_date, cdate);
		strcpy (Tool_head.mod_time, ctime);
		Tool_head.version = NCL_version;

		if ((Tool_head.no_tools==1) && (Tool_head.create_date[0]=='\0'))
/*
......first time create the toolib
*/
		{
			strcpy (Tool_head.create_date, cdate);
			strcpy (Tool_head.create_time, ctime);
		}
		ncl_store_toolhead(&Tool_head);
		Tool_modify = 1;
	}
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  tool_delrec(toolnum)
c
c   FUNCTION:  This routine delete a tool from the current working Tool Library
c
c   INPUT:  
c           toolnum      -  tool number to be deleted
c
c   OUTPUT: none
c   RETURN: -1: failed. 
c			0: deleted
c
c***********************************************************************/
tool_delrec(toolnum)
double toolnum;
{
	struct TL_tooldata_rec *p1;
	char ctime[9], cdate[12];

	ncdate (cdate);
	cdate[11] = '\0';
	ftim (ctime);
	ctime[8] = '\0';

	if (toolnum<=0)
	{
		tool_mfmsg_box(NULL, "Error!", "Not a valid tool number to be deleted",
			 0);
		return -1;
	}

	ncl_get_toolist_pt(&p1);
	if (p1 == UU_NULL) return -1;
	if (p1 != UU_NULL) p1 = (struct TL_tooldata_rec *) uu_lsnext(p1);
	while (p1)
	{
/*
.....search the tool number
*/
		if (p1->toolno==toolnum)
		{
			if (p1->toolno=Tool_head.high_tool)
				Tool_head.high_tool = Tool_head.high_tool - 1;
			if (p1->toolno=Tool_head.low_tool)
				Tool_head.low_tool = Tool_head.low_tool - 1;	
			uu_lsdele(p1);
			Tool_head.no_tools--;
			strcpy (Tool_head.mod_date, cdate);
			strcpy (Tool_head.mod_time, ctime);
			Tool_head.version = NCL_version;
			ncl_store_toolhead(&Tool_head);
			Tool_modify = 1;
			return 0;
		}
		p1 = (struct TL_tooldata_rec *) uu_lsnext(p1);
	}
	return -1;
}

/***********************************************************************
c
c   SUBROUTINE:  tool_findtool(find_str, pos)
c
c   FUNCTION:  This routine initial toolib 
c
c   INPUT:  cont: if the application need continue
c			err: 0 OK
c
c   OUTPUT: none
c	RETURNS: none
c
c***********************************************************************/
tool_findtool(find_str, pos)
char *find_str;
int *pos;
{
	int toolnum;
	struct TL_tooldata_rec *p1;
	int cmp_desp = 0;

	toolnum = atoi (find_str);
	if (toolnum<=0)
		cmp_desp = 1;

	*pos = 0;

	ncl_get_toolist_pt(&p1);
	if (p1 == UU_NULL) return -1;
	if (p1 != UU_NULL) p1 = (struct TL_tooldata_rec *) uu_lsnext(p1);
	while (p1)
	{
/*
.....search the tool number/description
*/
		*pos = *pos  + 1;
		if ((cmp_desp==0)&&(p1->toolno==toolnum))
		{
			break;
		}
		else if ((cmp_desp==1)&&(strcmp(p1->description,find_str)==0))
			break;
		p1 = (struct TL_tooldata_rec *) uu_lsnext(p1);
	}
	if (*pos==0)
		return -1;
	else
		return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  tool_init_tool(cont, err)
c
c   FUNCTION:  This routine initial toolib 
c
c   INPUT:  cont: if the application need continue
c			err: 0 OK
c
c   OUTPUT: none
c	RETURNS: none
c
c***********************************************************************/
void tool_init_tool(cont, err)
int *cont, *err;
{
	UX_pathname dir,file,ext,filename,save_file,load_file,fname;
	int i, status, numint,ityp;
	char *indx, buf[256];
	FILE *fstream;
	struct TL_toolopt_rec tool_option;
	int loaded = 0;

	*err = 0;
	*cont = 1;
/*
.....read envoirment init file first
*/
	tool_loadenv(err);
	if (*err!=0)
		return;
	fname[0] = '\0';
/*
.....read in toolib.ini
*/
	fstream = fopen("toolib.ini", "r");
	if(fstream != 0) 
	{
		do
		{
			status = ul_fread (fstream,buf,sizeof(buf),&numint);
			buf[numint] = '\0';
			filename[0] = '\0';
			tool_parse_opt(buf, &tool_option, filename);
			if (filename[0]!='\0')
				strcpy(fname, filename);
		}while (status==0);
	}
/*
.....pass into command line and execute
*/
	getmcr (buf, &i);
	buf[i] = '\0';
	filename[0] = '\0';
	tool_parse_opt(buf, &tool_option, filename);
	if (filename[0]!='\0')
		strcpy(fname, filename);
/*
.....initial for NCL/TOOLIB value/struction
*/
	ncl_tool_initunit(Tool_orig_unit);
	ncl_init_tldata();
	ncl_tool_inithead();
	ncl_toollist_init ();
	ncl_gettool_head (&Tool_head);

	if (tool_option.option==1)
	{
/*
.....load batch file
*/
		strcpy(load_file, tool_option.load_file);

		if (load_file[0]=='\0')
		{
/*
.....using toolib file we input
*/ 
			if (fname[0]!='\0')
			{
				strcpy(load_file, fname);
/*
//				indx = strrchr (load_file, '.');
//				if (indx!=NULL)
//					*indx = 0;
//				strcat (save_file, ".tla");
*/
			}		
		}
		if (load_file[0]!='\0')
		{
			strcpy(fname, load_file);
			tool_break_fname(fname,dir,file,ext);
			if (_stricmp(ext,"TLA") == 0)
				ityp = 0;
			else ityp = 1;
			if (ityp == 1)
				tool_load(load_file, err);
			else
				tool_loadbat(load_file, err);
			loaded = ityp+1;
		}
	}
	else if (fname[0]!='\0')
	{
/*
.....load input file, now we allow TLA file with add batch switch
.....just by name extension
*/
		tool_break_fname(fname,dir,file,ext);
		if (_stricmp(ext,"TLA") == 0)
			ityp = 0;
		else ityp = 1;
		if (ityp == 1)
			tool_load(filename, err);
		else
			tool_loadbat(filename, err);
		loaded = ityp+1;
	}
	if (*err)
		return;
	if ((tool_option.option==2) || (tool_option.option==3) || (tool_option.option==4))
	{
/*
......Save batch/list file
*/
		strcpy(save_file, tool_option.save_file);
		if (save_file[0]=='\0')
		{
/*
.....using toolib file we input
*/
			if (fname[0]!='\0')
			{
				strcpy(save_file, fname);
				indx = strrchr (save_file, '.');
				if (indx!=NULL)
					*indx = 0;
				if (tool_option.option==2) 
				{
					if (loaded==2)
						strcat (save_file, ".tla");
					else
						strcat (save_file, ".tlb");
				}
				else
					strcat (save_file, ".lis");
			}
		}
		if (save_file[0]!='\0')
		{
			if (tool_option.option==2) 
			{
				if (loaded==1)
					toolc_savlib(save_file);
				else
					tool_createbat(save_file);
			}
			else if (tool_option.option==3) 
				toolc_list(save_file, 1);
			else
				toolc_list(save_file, 2);
		}
		*cont = 0;
	}
	Tool_modify = 0;
}

/**********************************************************************
**    I_FUNCTION : tool_loadbat(filename, &kerr);
**       load a batch (text) tool library
**    PARAMETERS
**       INPUT  :
**				filename: batch file to be loaded
**       OUTPUT :
**				kerr : 1: failed
**						0= OK
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void tool_loadbat(filename, kerr)
char *filename;
int *kerr;
{
	int  status; 
	FILE *fstream;
	char msg[400];

	fstream = fopen(filename, "r");
	if(fstream == 0) 
	{
		sprintf(msg, "Can't open batch file %s", filename);
		tool_mfmsg_box(0, "File open error", msg, 1);
		*kerr = -1;
		return;
	}
	status = ncl_load_toolbat(filename);
	if (status==-1)
	{
		sprintf(msg, "Error loading batch file %s", filename);
		tool_mfmsg_box(0, "File open error", msg, 1);
		*kerr = -1;
		return;
	}
	*kerr = 0;
	ncl_gettool_head (&Tool_head);
	if (Tool_current_data.command!=NULL)
		uu_free (Tool_current_data.command);
	if (Tool_current_data.plabel!=NULL)
		uu_free (Tool_current_data.plabel);
	if (Tool_current_data.loadtl!=NULL)
		uu_free (Tool_current_data.loadtl);
	ncl_getcurrent_tooldata(&Tool_current_data);

	return;
}
/**********************************************************************
**    I_FUNCTION : tool_createbat(filename);
**       create a batch (text) file
**    PARAMETERS
**       INPUT  :
**				filename: batch file to be created
**       OUTPUT : none
**    RETURNS      : -1: failed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int tool_createbat(filename)
char *filename;
{
	char loadtl_str[80];
	FILE *fstream;
	char msg[400];
	char tempstr[80];
	int ret;
	int i,len,comma;
	struct TL_tooldata_rec *tool_data;

	fstream = fopen(filename, "r");
	if(fstream != 0) 
	{
		fclose (fstream);
		ret = tool_mfyesno("File Exists", "This file already exists, Do you wish to overwrite it?", 0);
		if (ret == 1)
			return 0;
	}
	fstream = fopen(filename, "w");
	if(fstream == 0) 
	{
		sprintf(msg, "Can't open file %s to write", filename);
		tool_mfmsg_box(0, "File open error", msg, 1);
		return -1;
	}
	if (Tool_head.description[0]!='\0')
		fprintf (fstream, "#LIBRARY# %s\n", Tool_head.description);
	fprintf (fstream, "SYMLIB/%s\n", Tool_head.symlib);
	fprintf (fstream, "PROFLIB/%s\n", Tool_head.proflib);
	fprintf (fstream, "CREATED/%s %s\n", Tool_head.create_date,
							Tool_head.create_time);
	fprintf (fstream, "MODIFIED/%s %s\n", Tool_head.mod_date,
							Tool_head.mod_time);
	if (Tool_head.units==1)
		fprintf (fstream, "UNITS/%s\n", "INCH");
	else
		fprintf (fstream, "UNITS/%s\n", "MM");
/*
......write user defined cutter type
*/
	if (Tool_head.utype_no>0)
	{
		fprintf (fstream, "TYPENO/%d\n", Tool_head.utype_no);
		for (i=0; i<Tool_head.utype_no;i++)
		{
			fprintf (fstream, "UTYPE/%s\n", Tool_head.utype[i]);
		}
	}
/*
.....put tool data info
*/
	ncl_get_toolist_pt(&tool_data);
	if (tool_data != UU_NULL) tool_data = (struct TL_tooldata_rec *) uu_lsnext(tool_data);
	while (tool_data)
	{
		fprintf (fstream, "\n#TOOL %-15.0f# %s\n", tool_data->toolno,
								tool_data->description);
		fprintf (fstream, "CUTTER/TYPE %d\n", tool_data->ctype);
		for (i=0; i<tool_data->ncvals; i++)
		{
			if (i==0)
				fprintf (fstream, "CUTTER/");
			else
				fprintf (fstream, ",");
			fprintf (fstream, "%f", tool_data->cutter[i]);
			if (tool_data->cparms[i]>0)
				fprintf (fstream, "_%d", tool_data->cparms[i]);
		}
		fprintf (fstream, "\n");
		if (tool_data->fpseudo)
		{
			for (i=0; i<tool_data->ndvals; i++)
			{
				if (i==0)
/*					fprintf (fstream, "CUTTER/DISPLY,"); */
					fprintf (fstream, "CUTTER/PSEUDO,");
				else
					fprintf (fstream, ",");
				fprintf (fstream, "%f", tool_data->pseudo[i]);
				if (tool_data->dparms[i]>0)
					fprintf (fstream, "_%d", tool_data->dparms[i]);
			}
			fprintf (fstream, "\n");
		}
		if (tool_data->segments==2)
			fprintf (fstream, "CUTTER/DISPLY, ALL\n");
		else if (tool_data->segments==1)
			fprintf (fstream, "CUTTER/DISPLY, PART\n");

		if (tool_data->move==1)
			fprintf (fstream, "CUTTER/DISPLY, MOVE, ON\n");
		else if (tool_data->move==2)
			fprintf (fstream, "CUTTER/DISPLY, MOVE, OFF\n");

		if (tool_data->shade==1)
			fprintf (fstream, "CUTTER/DISPLY, SHADE, ON\n");
		else if (tool_data->shade==2)
			fprintf (fstream, "CUTTER/DISPLY, SHADE, OFF\n");
	
		if (tool_data->sshade==1)
			fprintf (fstream, "CUTTER/DISPLY, SHADE, ON, SHANK\n");
		else if (tool_data->sshade==2)
			fprintf (fstream, "CUTTER/DISPLY, SHADE, OFF, SHANK\n");

		if (tool_data->hshade==1)
			fprintf (fstream, "CUTTER/DISPLY, SHADE, ON, HOLDER\n");
		else if (tool_data->hshade==2)
			fprintf (fstream, "CUTTER/DISPLY, SHADE, OFF, HOLDER\n");

		if (tool_data->fsymbol)
		{
			if (tool_data->symbol[0]!='\0')
			{
				fprintf (fstream, "CUTTER/DISPLY, %s", tool_data->symbol);
				if (tool_data->ctype>=10)
				{
					if (!((tool_data->catt[0]==tool_data->catt[1]) && (tool_data->catt[1]==0.0)))
					{
						fprintf (fstream, ",OFFSET");
						for (i=0; i<2; i++)
						{
							fprintf (fstream, ",%f", tool_data->catt[i]);
							if (tool_data->yparms[i]>0)
								fprintf (fstream, "_%d", tool_data->yparms[i]);
						}
					}
				}
				fprintf (fstream, "\n");
			}
		}
		if (tool_data->fshank)
		{
			if (tool_data->symshk[0]!='\0')
			{
				fprintf (fstream, "CUTTER/DISPLY, SHANK, %s", tool_data->symshk);
				if (tool_data->ctype<10)
				{
					if (tool_data->satt[0]!=0.0)
					{
						fprintf (fstream, ",%f", tool_data->satt[0]);
						if (tool_data->sparms[0]>0)
							fprintf (fstream, "_%d", tool_data->sparms[0]);
					}
				}
				else
				{
					for (i=0; i<2; i++)
					{
						fprintf (fstream, ",%f", tool_data->satt[i]);
						if (tool_data->sparms[i]>0)
							fprintf (fstream, "_%d", tool_data->sparms[i]);
					}
					if (!((tool_data->satt[3]==tool_data->satt[2]) && (tool_data->satt[2]==0.0)))
					{
						for (i=2; i<4; i++)
						{
							fprintf (fstream, ",%f", tool_data->satt[i]);
							if (tool_data->sparms[i]>0)
								fprintf (fstream, "_%d", tool_data->sparms[i]);
						}
					}
				}
			}
			else
			{
				fprintf (fstream, "CUTTER/DISPLY, SHANK");
				for (i=0; i<2; i++)
				{
					fprintf (fstream, ",%f", tool_data->satt[i]);
					if (tool_data->sparms[i]>0)
						fprintf (fstream, "_%d", tool_data->sparms[i]);
				}
				if (!((tool_data->satt[3]==tool_data->satt[2]) && (tool_data->satt[2]==0.0)))
				{
					for (i=2; i<4; i++)
					{
						fprintf (fstream, ",%f", tool_data->satt[i]);
						if (tool_data->sparms[i]>0)
							fprintf (fstream, "_%d", tool_data->sparms[i]);
					}
				}
			}
/*
.....CUTTER/HOLDER
*/
			if (tool_data->fshank==1)
				fprintf (fstream, ",CUTTER");
			else
				fprintf (fstream, ",HOLDER");
			fprintf (fstream, "\n");
		}
		if (tool_data->fholder)
		{
			if (tool_data->symhld[0]!='\0')
			{
				fprintf (fstream, "CUTTER/DISPLY, HOLDER, %s", tool_data->symhld);
				if (tool_data->ctype<10)
				{
					if ((tool_data->hatt[0]!=0.0) || (tool_data->hparms[0]>0))
					{
						fprintf (fstream, ",%f", tool_data->hatt[0]);
						if (tool_data->hparms[0]>0)
							fprintf (fstream, "_%d", tool_data->hparms[0]);
					}
				}
				else
				{
					for (i=0; i<2; i++)
					{
						fprintf (fstream, ",%f", tool_data->hatt[i]);
						if (tool_data->hparms[i]>0)
							fprintf (fstream, "_%d", tool_data->hparms[i]);
					}
					if (!((tool_data->hatt[3]==tool_data->hatt[2]) && (tool_data->hatt[2]==0.0)))
					{
						for (i=2; i<4; i++)
						{
							fprintf (fstream, ",%f", tool_data->hatt[i]);
							if (tool_data->hparms[i]>0)
								fprintf (fstream, "_%d", tool_data->hparms[i]);
						}
					}
				}
				fprintf (fstream, "\n");
			}
			else
			{
				fprintf (fstream, "CUTTER/DISPLY, HOLDER");
				for (i=0; i<2; i++)
				{
					fprintf (fstream, ",%f", tool_data->hatt[i]);
					if (tool_data->hparms[i]>0)
						fprintf (fstream, "_%d", tool_data->hparms[i]);
				}
				if (!((tool_data->hatt[3]==tool_data->hatt[2]) && (tool_data->hatt[2]==0.0)))
				{
					for (i=2; i<4; i++)
					{
						fprintf (fstream, ",%f", tool_data->hatt[i]);
						if (tool_data->hparms[i]>0)
							fprintf (fstream, "_%d", tool_data->hparms[i]);
					}
				}
				fprintf (fstream, "\n");
			}
		}
		if (tool_data->drawing[0]!='\0')
			fprintf (fstream, "DRAWING/%s\n", tool_data->drawing);
		
		loadtl_str[0] = '\0';
		if (tool_data->major[0]!='\0')
		{
			fprintf (fstream, "%s/ ", tool_data->major);
		}
		comma = 1;
		for (i=0; i<tool_data->no_loadtl; i++)
		{
			len = strlen (loadtl_str);
			if (tool_data->loadtl[i].parm>0)
			{
				sprintf (tempstr, "_%d", tool_data->loadtl[i].parm);
				len = len + strlen (tempstr) + 1;
			}
			else if (tool_data->loadtl[i].parm<0)
			{
				strcpy(tempstr, "=");
				len = len + 2;
			}
			else
				tempstr[0]= '\0';
			len = len + strlen (tool_data->loadtl[i].value) + 1;			
			if (len>=70)	
			{
				if (comma==1)
				{
					strcat (loadtl_str, ", $");
					comma = 0;
				}
				else
					strcat (loadtl_str, "$");
				fprintf(fstream, "%s\n", loadtl_str);
				strcpy (loadtl_str, "      ");
			}
			if (((tool_data->loadtl[i].value[0]!='\0') 
					|| (tempstr[0]!='\0')) && (i!=0) && (comma==1))
				strcat (loadtl_str, ", ");
			if (tool_data->loadtl[i].value[0]!='\0')
				strcat (loadtl_str, tool_data->loadtl[i].value);
			if (tempstr[0]!='\0')
				strcat (loadtl_str, tempstr);
			if (strcmp(tempstr, "=")==0)
				comma = 0;
			else
				comma = 1;
		}
		fprintf(fstream, "%s\n", loadtl_str);

		if ((tool_data->command!=NULL) && (tool_data->command[0]!='\0'))
		{
			fprintf(fstream, "#COMMANDS %-15.0f#\n", tool_data->toolno);
			fprintf(fstream, "%s\n", tool_data->command);
			fprintf(fstream, "##END##\n");
		}
		if ((tool_data->plabel!=NULL) && (tool_data->plabel[0]!='\0'))
		{
			fprintf(fstream, "#PARMLBS %-15.0f#\n", tool_data->toolno);
			fprintf(fstream, "%s\n", tool_data->plabel);
			fprintf(fstream, "##END##\n");
		}
		tool_data = (struct TL_tooldata_rec *) uu_lsnext(tool_data);
	}
	fclose(fstream);
done:;
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  toolc_list(filename, flag)
c
c   FUNCTION:  This routine create a tool listing file.
c
c   INPUT:  filename: file to be output.
c			flag: 1: simple list
c					2: full list
c
c   OUTPUT: none
c	RETURNS: UU_SUCCESS of no problems encountered, UU_FAILURE otherwise.
c
c***********************************************************************/
toolc_list(filename, flag)
char *filename;
int flag;
{
	FILE *fstream;
	char msg[400];
	int ret;
	int page_num, page_len, line_num;
	struct TL_tooldata_rec *tool_data;

	fstream = fopen(filename, "r");
	if(fstream != 0) 
	{
		fclose (fstream);
		ret = tool_mfyesno("File Exists", "This file already exists, Do you wish to overwrite it?", 0);
		if (ret == 1)
			return 0;
	}
	fstream = fopen(filename, "w");
	if(fstream == 0) 
	{
		sprintf(msg, "Can't open file %s to write", filename);
		tool_mfmsg_box(0, "File open error", msg, 1);
		return -1;
	}
	fprintf (fstream, "Tool Library: %s\n", Tool_head.name);
	fprintf (fstream, "Description: %s\n", Tool_head.description);
	fprintf (fstream, "Symbol Library: %s\n", Tool_head.symlib);
	fprintf (fstream, "Created: %s %s\tUpdated: %s %s\n", 
			Tool_head.create_date, Tool_head.create_time, 
			Tool_head.mod_date, Tool_head.mod_time);
	fprintf (fstream, "Tool Entries: %d\tLow Tool Number: %d High Tool Number: %d\n", 
			Tool_head.no_tools, (int)Tool_head.low_tool, (int)Tool_head.high_tool);
	if (Tool_head.units==1)
		fprintf (fstream, "Units: %s\n", "INCH");
	else
		fprintf (fstream, "Units: %s\n", "MM");
	if (Tool_modify)
		fprintf (fstream, "Modified: YES\n");
	else
		fprintf (fstream, "Modified: NO\n");
	page_num = 1;
	page_len = 66;
	line_num = 1;
start:;
/*
......Display header
*/
	tool_list_header(fstream, page_num);
	page_num++;
/*
.....put tool data info
*/
	ncl_get_toolist_pt(&tool_data);
	if (tool_data != UU_NULL) tool_data = (struct TL_tooldata_rec *) uu_lsnext(tool_data);
	while (tool_data)
	{
		if (line_num>page_len)
		{
			tool_list_header(fstream, page_num);
			page_num++;
			line_num = 1;
		}
		if (flag==1)
		{
			tool_list_simple(fstream, tool_data);
			line_num++;
		}
		else
		{
			tool_list_full(fstream, tool_data);
			line_num = page_len + 1;
		}
		tool_data = (struct TL_tooldata_rec *) uu_lsnext(tool_data);
	}
	if(fstream != 0) 
		fclose (fstream);
	return 0;
}


/***********************************************************************
c
c   SUBROUTINE:  tool_list_header(fstream, page_num)
c
c   FUNCTION:  This routine outputs a Tool listing file head.
c
c   INPUT:  fstream: file to be output.
c			page_num: page number
c
c   OUTPUT: fstream: file to be output.
c
c***********************************************************************/
void tool_list_header(fstream, page_num)
FILE *fstream;
int page_num;
{
	char msg[100], buf[40];
	int i, nc;
	int parms[11];

	getlist_opt(parms);
	if (parms[0]==0)
		return;

	fprintf (fstream, "%c", 12);
	
	strcpy (msg, Tool_head.description);
	nc = strlen (msg);
	for (i=nc; i<69; i++)
		msg[i] = ' ';
	msg[69] = '\0';
	fprintf (fstream, "%sPage: %5d\n", msg, page_num);
/*
......Display Library name & Symbol library
*/
	tool_short_filename(Tool_head.name, buf, 20);
	fprintf (fstream, "NCL Tool Library: %s * Symbol Library: %s\n\n", 
				buf, Tool_head.symlib);
}

/***********************************************************************
c
c   SUBROUTINE:  tool_list_simple(fstream, tool_data)
c
c   FUNCTION:  This routine outputs a simple Tool listing file record.
c
c   INPUT:  fstream: file to be output.
c			tool_data: tool data to write in
c
c   OUTPUT: fstream: file to be output.
c
c***********************************************************************/
void tool_list_simple(fstream, tool_data)
FILE *fstream;
struct TL_tooldata_rec *tool_data;
{
	char typestr[40];
	int cuttype = tool_data->ctype;
		
	if (cuttype==0)
		strcpy(typestr, "Face Mill");
	else if (cuttype==1)
		strcpy(typestr, "End Mill");
	else if (cuttype==2)
		strcpy(typestr, "Barrel");
	else if (cuttype==3)
		strcpy(typestr, "Cone");
	else if (cuttype==4)
		strcpy(typestr, "Bell");
	else if (cuttype==5)
		strcpy(typestr, "Drill");
	else if (cuttype==6)
		strcpy(typestr, "Boring Tool");
	else if (cuttype==7)
		strcpy(typestr, "Reamer");
	else if (cuttype==8)
		strcpy(typestr, "Chamfer Tool");
	else if (cuttype==9)
		strcpy(typestr, "Blade");
	else if (cuttype==10)
		strcpy(typestr, "Square Insert");
	else if (cuttype==11)
		strcpy(typestr, "Diamond Insert");
	else if (cuttype==12)
		strcpy(typestr, "Triangle Insert");
	else if (cuttype==13)
		strcpy(typestr, "Circular Insert");
	else if (cuttype==14)
		strcpy(typestr, "Grooving Tool");
	else
		strcpy(typestr, "               ");
	fprintf (fstream, "%-15.0f   %s   %s\n", tool_data->toolno, typestr, tool_data->description);
}

/***********************************************************************
c
c   SUBROUTINE:  tool_list_full(fstream, tool_data)
c
c   FUNCTION:  This routine outputs a full Tool listing file record.
c
c   INPUT:  fstream: file to be output.
c			tool_data: tool data to write in
c
c   OUTPUT: fstream: file to be output.
c
c***********************************************************************/
void tool_list_full(fstream, tool_data)
FILE *fstream;
struct TL_tooldata_rec *tool_data;
{
	int i, nc, parms[11];
	char typestr[40],tempstr[80], tempstr2[80], tempstr3[80], tempstr4[80], 
		tempstr5[80], tempspc[40], tempspc2[12];
	int cuttype = tool_data->ctype;

	for (i=0; i<25; i++)
		tempspc[i] = ' ';
	tempspc[25] = '\0';
	for (i=0; i<12; i++)
		tempspc2[i] = ' ';
	tempspc2[11] = '\0';
	getlist_opt(parms);
	if (parms[1])
	{
/*
.....Write out Tool number, and description
*/
		fprintf (fstream, "Tool: %-15.0f  Description:%s\n", tool_data->toolno, tool_data->description);
		if (cuttype==0)
			strcpy(typestr, "Face Mill");
		else if (cuttype==1)
			strcpy(typestr, "End Mill");
		else if (cuttype==2)
			strcpy(typestr, "Barrel");
		else if (cuttype==3)
			strcpy(typestr, "Cone");
		else if (cuttype==4)
			strcpy(typestr, "Bell");
		else if (cuttype==5)
			strcpy(typestr, "Drill");
		else if (cuttype==6)
			strcpy(typestr, "Boring Tool");
		else if (cuttype==7)
			strcpy(typestr, "Reamer");
		else if (cuttype==8)
			strcpy(typestr, "Chamfer Tool");
		else if (cuttype==9)
			strcpy(typestr, "Blade");
		else if (cuttype==10)
			strcpy(typestr, "Square Insert");
		else if (cuttype==11)
			strcpy(typestr, "Diamond Insert");
		else if (cuttype==12)
			strcpy(typestr, "Triangle Insert");
		else if (cuttype==13)
			strcpy(typestr, "Circular Insert");
		else if (cuttype==14)
			strcpy(typestr, "Grooving Tool");
		else
			strcpy(typestr, "               ");
		sprintf (tempstr, "Cutter Type: %s", typestr);
		nc = strlen (tempstr);
		for (i=nc; i<40; i++)
			tempstr[i] = ' ';
		tempstr[40] = '\0';
		if (tool_data->fpseudo)
			strcpy(tempstr2, "Pseudo Cutter: Yes");
		else
			strcpy(tempstr2, "Pseudo Cutter: No");
		fprintf (fstream, "%s     %s\n", tempstr, tempstr2);
	}
	if (parms[2])
	{
		fprintf (fstream, "Cutter Parameters:\n%sDefault        Parm\n", tempspc);
		for (i=0; i<tool_data->ncvals; i++)
		{
			tool_getprmt_string	(cuttype, tempstr, i);
			if (tempstr[0]=='\0') continue;
			strcat (tempstr, tempspc);
			tempstr[25] = '\0';
			sprintf (tempstr2, "%f", tool_data->cutter[i]);
			strcat (tempstr2, tempspc);
			tempstr2[15] = '\0';
			fprintf (fstream, "%s%s%d\n", tempstr, tempstr2, tool_data->cparms[i]);
		}
	}
	if (parms[3])
	{
		if (tool_data->fpseudo)
		{
			fprintf (fstream, "Pseudo Cutter Parameters:\n%sDefault        Parm\n", tempspc);
			for (i=0; i<tool_data->ndvals; i++)
			{
				tool_getprmt_string	(cuttype, tempstr, i);
				if (tempstr[0]=='\0') continue;
				strcat (tempstr, tempspc);
				tempstr[25] = '\0';
				sprintf (tempstr2, "%f", tool_data->pseudo[i]);
				strcat (tempstr2, tempspc);
				tempstr2[15] = '\0';
				fprintf (fstream, "%s%s%d\n", tempstr, tempstr2, tool_data->dparms[i]);
			}
		}
	}
	if (parms[4])
	{
/*
......Output Display Parameters need to do
*/
		fprintf (fstream, "Display Parameters:\n");
/*
......cutter segment
*/
		if (tool_data->segments==0)
			fprintf (fstream, "Cutter Segment: Default\n");
		else if (tool_data->segments==1)
			fprintf (fstream, "Cutter Segment: Part\n");
		else if (tool_data->segments==2)
			fprintf (fstream, "Cutter Segment: All\n");
/*
......Moving Cutter:
*/
		if (tool_data->move==0)
			fprintf (fstream, "Moving Cutter: Default\n");
		else if (tool_data->move==1)
			fprintf (fstream, "Moving Cutter: On\n");
		else if (tool_data->move==2)
			fprintf (fstream, "Moving Cutter: Off\n");
/*
......Shaded:
*/
		if (tool_data->shade==0)
			fprintf (fstream, "Shaded: Default\n");
		else if (tool_data->shade==1)
			fprintf (fstream, "Shaded: On\n");
		else if (tool_data->shade==2)
			fprintf (fstream, "Shaded: Off\n");
/*
		if (tool_data->sshade==0)
			fprintf (fstream, "Shank Shaded: Default\n");
		else if (tool_data->sshade==1)
			fprintf (fstream, "Shank Shaded: On\n");
		else if (tool_data->sshade==2)
			fprintf (fstream, "Shank Shaded: Off\n");
		if (tool_data->hshade==0)
			fprintf (fstream, "Holder Shaded: Default\n");
		else if (tool_data->hshade==1)
			fprintf (fstream, "Holder Shaded: On\n");
		else if (tool_data->hshade==2)
			fprintf (fstream, "Holder Shaded: Off\n");
*/
/*
......Tool Drawing:
*/
		if (tool_data->drawing[0]!=0)
			fprintf (fstream, "Tool Drawing: %s\n", tool_data->drawing);
	}
	if (parms[5])
	{
/*
......output Load Command
*/
		if (tool_data->floadtl)
		{
			fprintf (fstream, "Load Tool Command:\n");
			fprintf (fstream, "%sWord/Value %sParm No    Word/Value %sParm No\n", 
								tempspc2, tempspc2,tempspc2);
			if (tool_data->major[0]!='\0')
			{
				sprintf (tempstr, "%s/ ", tool_data->major);
				strcat (tempstr, tempspc2);
				tempstr[11] = '\0';
			}
			for (i=0; i<tool_data->no_loadtl; i++)
			{
				strcpy(tempstr2, tool_data->loadtl[i].value);
				strcat (tempstr2, tempspc);
				tempstr2[22] = '\0';
				sprintf (tempstr3, "%d", tool_data->loadtl[i].parm);
				strcat (tempstr3, tempspc);
				tempstr3[11] = '\0';
				i++;
				if (i<tool_data->no_loadtl)
				{
					strcpy(tempstr4, tool_data->loadtl[i].value);
					strcat (tempstr4, tempspc);
					tempstr4[22] = '\0';
					sprintf (tempstr5, "%d", tool_data->loadtl[i].parm);
					strcat (tempstr5, tempspc);
					tempstr5[10] = '\0';
					fprintf (fstream, "%s%s%s%s%s\n", tempstr, tempstr2, tempstr3, tempstr4, tempstr5);
					strcpy(tempstr, tempspc2);
				}
				else
				{
					fprintf (fstream, "%s%s%s\n", tempstr, tempstr2, tempstr3);
					break;
				}
			}
		}
	}
	if (parms[6])
	{
/*
......Output Operator commands
*/
		if (tool_data->no_command!=0)
		{
			fprintf (fstream, "Optional Commands:\n");
			fprintf (fstream, "%s\n", tool_data->command);
		}
	}
	if (parms[7])
	{
/*
......Output Parameter Labels
*/
		if (tool_data->no_plabel!=0)
		{
			fprintf (fstream, "Parameter Labels:\n");
			fprintf (fstream, "%s\n", tool_data->plabel);
		}
	}
	if (parms[8])
	{
/*
......output symbol parameters
*/
		if (tool_data->fsymbol)
		{
			fprintf (fstream, "Symbol Parameters:\n%sDefault        Parm\n", tempspc);
			for (i=0; i<4; i++)
			{
				tool_getprmt_string2(1, tool_data, tempstr, i);
				if (tempstr[0]=='\0') continue;
				strcat (tempstr, tempspc);
				tempstr[25] = '\0';
				sprintf (tempstr2, "%f", tool_data->catt[i]);
				strcat (tempstr2, tempspc);
				tempstr2[15] = '\0';
				fprintf (fstream, "%s%s%d\n", tempstr, tempstr2, tool_data->yparms[i]);
			}
		}
	}
	if (parms[9])
	{
/*
......output shank parameters
*/
		if (tool_data->fshank)
		{
/*
......Shaded:
*/
			if (tool_data->sshade==0)
				fprintf (fstream, "Shank Shaded: Default\n");
			else if (tool_data->sshade==1)
				fprintf (fstream, "Shank Shaded: On\n");
			else if (tool_data->sshade==2)
				fprintf (fstream, "Shank Shaded: Off\n");
			fprintf (fstream, "Shank Parameters:\n%sDefault        Parm\n", tempspc);
			for (i=0; i<4; i++)
			{
				tool_getprmt_string2(2, tool_data, tempstr, i);
				if (tempstr[0]=='\0') continue;
				strcat (tempstr, tempspc);
				tempstr[25] = '\0';
				sprintf (tempstr2, "%f", tool_data->satt[i]);
				strcat (tempstr2, tempspc);
				tempstr2[15] = '\0';
				fprintf (fstream, "%s%s%d\n", tempstr, tempstr2, tool_data->sparms[i]);
			}
		}
	}
	if (parms[10])
	{
/*
......output symbol parameters
*/
		if (tool_data->fholder)
		{
/*
......Shaded:
*/
			if (tool_data->hshade==0)
				fprintf (fstream, "Holder Shaded: Default\n");
			else if (tool_data->hshade==1)
				fprintf (fstream, "Holder Shaded: On\n");
			else if (tool_data->hshade==2)
				fprintf (fstream, "Holder Shaded: Off\n");
			fprintf (fstream, "Holder Parameters:\n%sDefault        Parm\n", tempspc);
			for (i=0; i<4; i++)
			{
				tool_getprmt_string2(3, tool_data, tempstr, i);
				if (tempstr[0]=='\0') continue;
				strcat (tempstr, tempspc);
				tempstr[25] = '\0';
				sprintf (tempstr2, "%f", tool_data->hatt[i]);
				strcat (tempstr2, tempspc);
				tempstr2[15] = '\0';
				fprintf (fstream, "%s%s%d\n", tempstr, tempstr2, tool_data->hparms[i]);
			}
		}
	}
}
/*********************************************************************
**  E_FUNCTION: tool_getprmt_string(cuttype, prompt, indx)
**          This function get the parameter prompt according to tool type
**
**    PARAMETERS
**       INPUT  :   cuttype: tool type
**					indx: field number
**					
**       OUTPUT :   prompt: parameter prompt
**					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
void tool_getprmt_string(cuttype, prompt, indx)
int cuttype, indx;
char *prompt;
{
/*
.....Field 1 - Diameter,Radius
*/
	if (indx==0)
	{
		if (cuttype==9)
			strcpy(prompt, "Width:");
		else if (cuttype >= 10)
			strcpy(prompt, "Radius:");
		else
			strcpy(prompt, "Diameter:");
		return;
	}
	else if (indx==1)
	{
/*
.....Field 2 - Radius,Diameter,Chizel,Width
*/
		if (cuttype == 5 || cuttype == 8 || cuttype == 13)
		{
			prompt[0] = '\0';
		}
		else
		{
			if (cuttype==9)
				strcpy(prompt, "Chizel:");
			else if (cuttype==14)
				strcpy(prompt, "Width:");
			else if (cuttype >= 10)
				strcpy(prompt, "Diameter:");
			else
				strcpy(prompt, "Corner Radius:");
		}
		return;
	}
	else if (indx==2)
	{
/*
.....Field 3 - Height
*/
		strcpy(prompt, "Height:");
		return;
	}
	else if (indx==3)
	{
/*
.....Field 4 - Side Angle, Side Radius, Angle
*/
		if (cuttype == 2 || cuttype == 3 || cuttype == 4 || cuttype == 5 ||
			cuttype == 8 || cuttype == 9 || cuttype == 11)
		{
			if (cuttype==2)
				strcpy(prompt, "Side Radius:");
			else if (cuttype >= 9)
				strcpy(prompt, "Angle:");
			else
				strcpy(prompt, "Side Angle:");
		}
		else
		{
			prompt[0] = '\0';
		}
		return;
	}
	else if (indx==4)
	{
/*
.....Field 5 - Z-Height, Mount Angle, Length
*/
		if (cuttype == 2 || (cuttype >= 10 && cuttype != 13))
		{
			if (cuttype==14)
				strcpy(prompt, "Length:");
			else if (cuttype >= 10)
				strcpy(prompt, "Mount Angle:");
			else
				strcpy(prompt, "Z-Height:");
		}
		else
		{
			prompt[0] = '\0';
		}
		return;
	}
	else if (indx==5)
	{
/*
.....Field 6 - Flat Angle
*/
		if (cuttype == 2)
		{
			strcpy(prompt, "Flat Angle:");
		}
		else
		{
			prompt[0] = '\0';
		}
		return;
	}
}
/*********************************************************************
**  E_FUNCTION: tool_getprmt_string2(flag, tool_data, prompt, indx)
**          This function get the parameter prompt according to tool type
**
**    PARAMETERS
**       INPUT  :   tool_data: tool data
**					indx: field number
**					flag: 1: symbol
**							2: shank
**							3: holder
**       OUTPUT :   prompt: parameter prompt
**					
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
void tool_getprmt_string2(flag, tool_data, prompt, indx)
int flag, indx;
char *prompt;
struct TL_tooldata_rec *tool_data;
{
	int icfl[10];
	char symbol[22];
	int j, itrv[4], nc;
	char spr[4][20];
	int nprofs;
	struct S_tools_struc *ptool = NULL;
	
	ptool = (struct S_tools_struc *)UU_LIST_ARRAY(&NCL_plib_tools);
	nprofs = UU_LIST_LENGTH(&NCL_plib_tools);

	if (flag==1)
	{
		strcpy(symbol, tool_data->symbol);
		nc = strlen (symbol);
		if (nc != 0)
		{
			icfl[0] = 3;
			ul_to_upper(symbol);
			for (j=0;j<nprofs;j++)
			{
				if (strcmp(symbol, ptool[j].label) == 0)
				{
					icfl[0] = 4;
					break;
				}
			}
		}
		else
		{
			icfl[0] = tool_data->fpseudo;
		}
	}
	if (flag==2)
	{
		strcpy(symbol, tool_data->symshk);
		nc = strlen (symbol);
		if (nc != 0)
		{
			icfl[4] = 3;
			ul_to_upper(symbol);
			for (j=0;j<nprofs;j++)
			{
				if (strcmp(symbol, ptool[j].label) == 0)
				{
					icfl[4] = 4;
					break;
				}
			}
		}
		else
		{
			icfl[4] = 1;
		}
	}
	if (flag==3)
	{
		strcpy(symbol, tool_data->symhld);
		nc = strlen (symbol);
		if (nc != 0)
		{
			icfl[5] = 3;
			ul_to_upper(symbol);
			for (j=0;j<nprofs;j++)
			{
				if (strcmp(symbol, ptool[j].label) == 0)
				{
					icfl[5] = 4;
					break;
				}
			}
		}
		else
		{
			icfl[5] = 1;
		}
	}
	if (flag==1)
	{
/*
........Mill cutter / Lathe Parameters
*/
		if (tool_data->ctype < 10 || icfl[0] < 2)
		{
			itrv[0] = itrv[1] = itrv[2] = itrv[3] = UU_FALSE;
			spr[0][0] = spr[1][0] = spr[2][0] = spr[3][0] = '\0';
		}
/*
........Lathe symbol cutter
*/
		else
		{
			itrv[0] = itrv[1] = UU_TRUE;
			itrv[2] = itrv[3] = UU_FALSE;
			strcpy(spr[0],"Z-Attach:");
			strcpy(spr[1],"Z-Depth:");
			spr[2][0] = spr[3][0] = '\0';
		}
	}
/*
.....Shank / Holder
*/
	else
	{
/*
........Mill
*/
		if (tool_data->ctype < 10)
		{
/*
...........Parameters
*/
			if (((icfl[4] < 2) && (flag==2))
				|| ((icfl[5] < 2) && (flag==3)))
			{
				itrv[0] = itrv[1] = itrv[2] = itrv[3] = UU_TRUE;
				strcpy(spr[0],"Diameter:");
				strcpy(spr[1],"Height:");
				strcpy(spr[2],"Angle:");
				strcpy(spr[3],"Z-Attach:");
			}
/*
...........Symbol
*/
			else
			{
				itrv[0] = UU_TRUE;
				itrv[1] = itrv[2] = itrv[3] = UU_FALSE;
				strcpy(spr[0],"Z-Attach:");
				spr[1][0] = spr[2][0] = spr[3][0] = '\0';
			}
		}
/*
........Lathe
*/
		else
		{
/*
...........Parameters
*/
			if (((icfl[4] < 2) && (flag==2))
				|| ((icfl[5] < 2) && (flag==3)))
			{
				itrv[0] = itrv[1] = itrv[2] = itrv[3] = UU_TRUE;
				strcpy(spr[0],"Width:");
				strcpy(spr[1],"Length:");
				strcpy(spr[2],"Z-Depth:");
				strcpy(spr[3],"Y-Offset:");
			}
/*
...........Symbol
*/
			else
			{
				itrv[0] = itrv[1] = itrv[2] = itrv[3] = UU_TRUE;
				strcpy(spr[0],"X-Offset:");
				strcpy(spr[1],"Y-Offset:");
				strcpy(spr[2],"Z-Attach:");
				strcpy(spr[3],"Z-Depth:");
			}
		}
	}
	if (itrv[indx]==UU_TRUE)
	{
		strcpy (prompt, spr[indx]);
	}
	else
	{
		prompt[0] = '\0';
	}
}
/*********************************************************************
**  E_FUNCTION: tool_parse_opt(opt_str, tool_option, filename)
**          This function parse the toolib option string
**
**
**    PARAMETERS
**       INPUT  :   opt_str: optionstring to be parsed
**					
**       OUTPUT :   tool_option: toolib option
**					filename: option include a filename
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none.
*********************************************************************/
void tool_parse_opt(opt_str, tool_option, filename)
struct TL_toolopt_rec *tool_option;
char *opt_str;
char *filename;
{
	char *tok, *indx, tempstr[256], token[40], optstr[20], valstr[256];
	int i;
	char opt_token[6][10] = {"LOAD", "BATCH", "BRIEF", "FULL", "UNITS", "PAGE_LEN"};
	tool_option->option = 0;
	tool_option->save_file[0] = '\0';
	tool_option->load_file[0] = '\0';
	if (opt_str[0]=='\0')
		return;
	strcpy(tempstr, opt_str);
	tok = strtok (tempstr, " \t\n");
	while (tok!=NULL)
	{
		if (tok[0]!='-')
		{
			strcpy (filename, tok);
		}
		else
		{
			strcpy (token, tok);
			indx = strchr (token, ':');
			if (indx!=NULL)
			{
				*indx = 0;
/*
.....token[0] is '-'
*/
				strcpy(optstr, &(token[1]));
				strcpy(valstr, indx+1);
			}
			else
			{
/*
.....tok[0] is '-'
*/
				strcpy(optstr, &(tok[1]));
				valstr[0] = '\0';
			}
			if (strlen (optstr)>8)
			{
				return;
			}
			for (i=0;i<6;i++)
			{
				ul_to_upper(optstr);
				if (strcmp(optstr, opt_token[i]) == 0) break;
			}
			if (i<4)
			{
				tool_option->option = i + 1;
				if (i==0)
					strcpy (tool_option->load_file, valstr);
				else
					strcpy (tool_option->save_file, valstr);
			}
			if (i==4)
			{
				if (_stricmp (valstr, "MM")==0)
					Tool_orig_unit = 2;
				else
					Tool_orig_unit = 1;
			}
			if (i==5)
			{
				Tool_list_pagelen = atoi(valstr);
			}
		}
		tok = strtok (NULL, " \t\n");
	}
}

/*********************************************************************
**	 E_FUNCTION : int tool_open_cutprof(nprofs)
**       Loads the external tool description file, don't save the point 
**			because we don't need it
**       
**	 PARAMETERS	
**		 INPUT  : filename:the external tool description file to be load
**		 OUTPUT :
**			none
**
**	 RETURNS: UU_SUCCESS of no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int tool_open_cutprof(filename)
char *filename;
{
	int status;
	char msg[400];
	int ifree;
	FILE *fstream;
/*
.....Initialize routine
*/
	status = UU_FAILURE;
	ifree = UU_FALSE;
	fstream = fopen(filename, "r");
	if(fstream == 0)
	{
		sprintf(msg, "Can't open file %s to read", filename);
		tool_mfmsg_box(0, "File open error", msg, 1);
		return -1;
	}
	fclose(fstream);
	ncl_opend_cutprof(filename, 2);
/*
.....End of routine
*/
done:;
	return(status);
}

/**********************************************************************
**    I_FUNCTION :  ud_wrerr(error)
**       Creates and displays an error dialog.
**		we use this name instead of 'tool_xxxx' is because nutoolib.c use it
**		and nutoolib.c is used for NCL too
**    PARAMETERS   
**       INPUT  : 
**          error     = error to display.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    WARNINGS     : none
*********************************************************************/
void ud_wrerr(error)
char *error;
{
	tool_mfmsg_box(0,"TOOLIB Error", error, 0);
}

char *ux_getenv(char *env, int options)
{
	char *tool_getenv();
	return (char*) tool_getenv(env);
}

