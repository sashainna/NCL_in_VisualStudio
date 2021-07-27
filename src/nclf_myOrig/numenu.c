/*********************************************************************
**    NAME         :  numenu.c
**       CONTAINS:
**           nclc_save_recent_file
**           nclf_save_recent_file
**           ncl_open_file
**
**    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
**       numenu.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       07/05/16 , 12:30:26
*********************************************************************/
#include "usysdef.h"
#include "ustdio.h"
#include "xenv1.h"
#include "dmotif.h"
#include "lcom.h"
#include "lipvmach.h"
#include "mdunits.h"
#include "mfort.h"
#include "mxxx.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"

UX_pathname NCL_load_part = "";

#define MAXEXT 19
#define F_PART 0
#define F_UNIBASE 1
#define F_RECORD 2
#define F_IGES 3
#define F_STEP 4
#define F_SESSION 5
#define F_STOCK 6
#define F_STL 7
#define F_LAY 8
#define F_MENU 9
#define F_KEYDEF 10
#define F_TOOLIB 11
#define F_CLFILE 12
#define F_MCD 13
#define F_CATIA 14
#define F_MASTERCAM 15
#define F_SIMUL 16
#define F_IPVSESS 17
#define F_DRAWING 18

static int Sfilt[MAXEXT]={0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18};
static char Sext[MAXEXT][UX_SUFFIX_LEN]={"geo,mot,ncl,pp,run", "u,ud", "rp",
	"igs,iges", "step,stp", "ncs", "stk", "stl", "lay", "menu", "wnt", "TLB",
	"cl,cln", "pu*,mcd", "clfile", "nci", "sim", "ipv", "dw"};
static char Sdes[MAXEXT][30]={
	"Part Program Files", "Unibase Files", "Record/Playback Files",
	"IGES Files", "STEP Files", "NCL Session Files", "Stock Files", "STL Models",
	"Layout Files", "Menu Files", "Key Definition Files", "Tool Libraries",
	"Clfiles", "MCD Files", "Catia Clfiles", "Mastercam Clfiles",
	"Simulation Files", "NCLIPV Session Files", "Drawing Files"};

static void S_open_file();
static UU_LOGICAL S_compare_ext();

/*********************************************************************
**    E_FUNCTION     : IsSameRecentFile(filename, str)
**				check if the filename already ine recent file string
**    PARAMETERS
**       INPUT  : filename: file to be checked
**					str: recent file string
**       OUTPUT :
**          none
**    RETURNS      : 1: yes; 0: No
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int IsSameRecentFile(filename, str)
char *filename;
char *str;
{
	char fname[UX_MAX_FILE_LEN], tmpstr[UX_MAX_PATH_LEN*2];
	char *tok,*strtok();
	if ((str==NULL)||(str[0]=='\0'))
		return 0;
	strcpy(tmpstr, str);
	tok = strtok (tmpstr, ",\t\n");
	if (tok==NULL) return 0;
	tok = strtok(NULL, ",\t\n");
	if (tok==NULL) return 0;
	tok = strtok(NULL, ",\t\n");
	if (tok==NULL) return 0;
	strcpy(fname, tok);
	ul_remove_quotes(fname);
		if (fname[0]=='"')
			strcpy(fname, &(fname[1]));
	ul_remove_quotes(filename);
	if (stricmp(fname, filename)==0)
		return 1;
	return 0;
}
/*********************************************************************
**    E_FUNCTION     : nclc_save_recent_file(fullname, flag)
**				save the fullname into recently file menu
**    PARAMETERS
**       INPUT  : fullname: file to be saved
**					flag: file type 
**                    -1: Unknown file (determined by file type)
**                     0: Part-program file
**                     1: Unibase file
**                     2: Playback file
**                     3: IGES file
**                     4: STEP file
**                     5: Session file
**                     6: Stock file
**                     7: STL model
**                     8: Layout file
**                     9: Menu file
**                    10: Keydef file
**                    11: Tool library
**                    12: NCL clfile
**                    13: MCD clfile
**                    14: Catia clfile
**                    15: Mastercam clfile
**                    16: Simulation file
**                    17: NCLIPV Session file
**                    18: Drawing file
**
**
**
**
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclc_save_recent_file(fullname, flag)
char *fullname;
int flag;
{
	char *tempfile, tmpstr[UX_MAX_PATH_LEN*2];
	UX_pathname menu_name, fulldir, filename, sfile;
	FILE* fptr, *new_fptr;
	char buf[UX_MAX_PATH_LEN*2], nbuf[UX_MAX_PATH_LEN*2];
	int numint, inum, status, j, num;
	UX_pathname fname, dir;
	if ((fullname==NULL)||(fullname[0]=='\0'))
		return;
	fptr = new_fptr = NULL;
/*
......create the temp file for writing
*/
	tempfile = (char*)malloc(UX_MAX_PATH_LEN*sizeof(char));
	ul_get_full_dir ("HOMEDIR", fulldir);
	tempfile = (char *)tempnam(fulldir, "menu");
/*
......open the recent_files.menu
*/
	strcpy(fname,"recent_files.menu");
/*
......open the reading
*/
	status = ul_open_mod_file("UU_USER_SETTINGS", "menu", "NCL_MENU", (char*)UU_NULL,
		fname, 2, &fptr);
/*
......open the tempfile
*/
	new_fptr = fopen(tempfile, "w");
	if (new_fptr==NULL) return;
/*
......write the recent_files.menu header into the new file
*/
	strcpy(buf, "#Descriptor#\n");
	ux_fwrite0(buf, strlen(buf), 1, new_fptr, &inum);
	strcpy(buf, "/NAME/ Recent Files\n");
	ux_fwrite0(buf, strlen(buf), 1, new_fptr, &inum);
	strcpy(buf, "/POSITION/ 0,0\n");
	ux_fwrite0(buf, strlen(buf), 1, new_fptr, &inum);
	strcpy(buf, "/SIZE/ 0.089844,0.274844\n");
	ux_fwrite0(buf, strlen(buf), 1, new_fptr, &inum);
	strcpy(buf, "/ROWS/ 12\n");
	ux_fwrite0(buf, strlen(buf), 1, new_fptr, &inum);
	strcpy(buf, "/COLS/ 1\n");
	ux_fwrite0(buf, strlen(buf), 1, new_fptr, &inum);
#if UU_COMP == UU_WIN2K
	strcpy(buf, "/TYPE/ Pulldown\n\n");
#else
	strcpy(buf, "/TYPE/ POPUP\n\n");
#endif
	ux_fwrite0(buf, strlen(buf), 1, new_fptr, &inum);
	strcpy(buf, "#MENUS#\n");
	ux_fwrite0(buf, strlen(buf), 1, new_fptr, &inum);
	if (fptr!=NULL)
	{
		do
		{
			buf[0] = '\0';
			status = ul_fread (fptr, buf, sizeof(buf), &numint);
			if (numint>=0)
				buf[numint] = '\0';
			if (strncmp(buf, "#MENUS#", 7)==0)
				break;
			if (status == UX_EOF)
				break;
		}
		while (status == UU_SUCCESS);
	}
	else
		buf[0] = '\0';
/*
......write the new filename line into the first line of the menu
*/
	ul_remove_quotes(fullname);
	ul_break_fname(fullname, dir, fname);
/*
.....we need save the path
*/
	if (strlen(dir)==0)
	{
		ul_get_full_dir (".", fulldir);
		ul_build_full_fname(fulldir, fname, "", fullname);
	}
	ul_short_filename(fullname, sfile, 40);
	sprintf(nbuf, "/%s/ NCL_OPEN_FILE, <%s>, \"%s, %d\"\n",
				fname, sfile, fullname, flag);
	if (strlen(nbuf)>=UX_MAX_PATH_LEN*2)
	{
		ud_printmsg ("Menu line is too long (more than 2048 chars).\r\nCannot save into Recent Files menu.");
		num = 0;
	}
	else
	{
		ux_fwrite0(nbuf, strlen(nbuf), 1, new_fptr, &inum);
		num = 1;
	}
	if (fptr==NULL)
	{
/*
......no old menu
......so just write the new line and return
*/
		goto done;
	}
/*
......copy the menu filename lines from old file into the new file
......until 12 menu files
*/
	do
	{
		status = ul_fread (fptr, buf, sizeof(buf), &numint);
		if (numint>=0)
			buf[numint] = '\0';
#if UU_COMP!=UU_WIN2K
		if (status == UX_EOF)
		{
			goto done;
		}
#else
		if ((numint<=0)&&(status == UX_EOF))
		{
			goto done;
		}
		else if ((status == UX_EOF)&&(numint>0))
		{
			buf[numint+1] = '\0';
			if ((strncmp(nbuf, buf, numint)!=0)&&(num<12))
				ux_fwrite0(buf, strlen(buf), 1, new_fptr, &inum);
			goto done;
		}
#endif
		strcat(buf, "\n");
/*		if ((stricmp(nbuf, buf)!=0)&&(num<12)) */
		if ((IsSameRecentFile(fullname, buf)==0)&&(num<12))
		{
			strcpy(tmpstr, buf);
			if (strstr(tmpstr, "KEY_NOOP")==0)
			{
				ux_fwrite0(buf, strlen(buf), 1, new_fptr, &inum);
				num++;
			}
		}
	}
	while (status == UU_SUCCESS);
done:;
/*
.....close both files, delete the old recent_files.menu, 
.....rename the temp file to recent_files.menu
*/
	if (fptr != 0) 
		ux_fclose0(fptr);
	if (new_fptr!= 0) 
		fclose(new_fptr);
/*
......don't save into the opened recent_files.menu (it could 
......locate in the production directory) but only
......saved into the UU_USER_SETTINGS/menu directory
*/
/*
......we will created the path if not exist
*/
	strcpy(menu_name,"recent_files.menu");
	status = ul_open_mod_file("UU_USER_SETTINGS", "menu", (char*)UU_NULL, (char*)UU_NULL,
			menu_name, 3, &fptr);
	if (fptr==NULL)
/*
......error, file can't be write or create
*/
	{
		ud_winerror("Can not write into recent_files.menu.\nIt may be write protected");
		return;
	}
	ux_fclose0(fptr);
	remove(menu_name);
	status = rename(tempfile, menu_name);
	free(tempfile);
/*
.....then update the display menu
*/
/*
......search to find the menu then update
*/
	for (j=0;j<UDM_menu_count;j++)
	{
		strcpy(filename, UDM_menu[j].file);
		ul_break_fname(filename, dir, fname);
		if (fname[0]=='\0')
			continue;
		if (strcmp(fname, "recent_files.menu")==0)
		{
			strcpy(UDM_menu[j].file, menu_name);
			ud_reload_menu(j);
			break;
		}
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclf_save_recent_file(fullname, nc, flag)
**				save the fullname into recently file menu
**				call by fortran function
**    PARAMETERS
**       INPUT  : fullname: file to be saved
**						nc: length of fullname
**					flag: file type 
**                    -1: Unknown file (determined by file type)
**                     0: Part-program file
**                     1: Unibase file
**                     2: Playback file
**                     3: IGES file
**                     4: IGES file
**                     5: Session file
**                     6: Stock file
**                     7: STL model
**                     8: Layout file
**                     9: Menu file
**                    10: Keydef file
**                    11: Tool library
**                    12: NCL clfile
**                    13: MCD clfile
**                    14: Catia clfile
**                    15: Mastercam clfile
**                    16: Simulation file
**                    17: NCLIPV Session file
**                    18: Drawing file
**
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_save_recent_file(fullname, nc, flag)
UM_f77_str_ptr fullname;
int *nc, *flag;
{
	UX_pathname fname;
	char *p;
	p = UM_cstr_of_f77_str(fullname);
	if (*nc>0)
	{
		strncpy(fname, p, *nc);
		fname[*nc] = '\0';
	}
	else
	{
		if (*flag==F_PART)
		{
			strcpy(fname, UL_program);
			strcat(fname, ".");
			strcat(fname, UL_program_suffix);
		}
	}
	if (strlen(fname) != 0) nclc_save_recent_file(fname, *flag);
}

/*********************************************************************
**    E_FUNCTION     : ncl_open_file(namestr)
**           Load the file according to file name string.
**           It has format "filename,type"
**    PARAMETERS
**       INPUT  :
**          namestr  = Filename and type of file to open.  It is in
**                     the format of "filename,type', where 'type'
**                     can be one of the following.
**
**                       -1: Unknown file (determined by file type)
**                        0: Part-program file
**                        1: Unibase file
**                        2: Playback file
**                        3: IGES file
**                        4: IGES file
**                        5: Session file
**                        6: Stock file
**                        7: STL model
**                        8: Layout file
**                        9: Menu file
**                       10: Keydef file
**                       11: Tool library
**                       12: NCL clfile
**                       13: MCD clfile
**                       14: Catia clfile
**                       15: Mastercam clfile
**                       16: Simulation file
**                       17: NCLIPV Session file
**                       18: Drawing file
**
**                     If 'namestr' is blank, then the user will be
**                     prompted for a filename.
**
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_open_file(namestr)
char *namestr;
{
	int i,type,nc;
	UU_LOGICAL bflag;
	char *tok,*strtok(),*p,*ux_getenv();
	char sbuf[UX_SUFFIX_LEN],tbuf[UX_SUFFIX_LEN];
	UX_pathname fname,descrip,filter;
/*
.....Get acceptable file extensions
*/
	p = ux_getenv("UL_PROGRAM_SUFFIX",UX_NPRTERRS); 
	if (p != UU_NULL)
	{
		strcpy(Sext[F_PART],p);
		ul_remove_quotes(Sext[0]);
	}
	p = ux_getenv("UR_UNB_FILE",UX_NPRTERRS);
	if (p != UU_NULL) strcpy(Sext[1],p);
	p = ux_getenv("UR_ASCII_PART",UX_NPRTERRS);
	if (p != UU_NULL)
	{
		strcat(Sext[F_UNIBASE],",");
		strcat(Sext[F_UNIBASE],p);
	}
	ul_remove_quotes(Sext[F_UNIBASE]);
	p = ux_getenv("UD_RPB_SUFFIX",UX_NPRTERRS); 
	if (p != UU_NULL)
	{
		strcpy(Sext[F_RECORD],p);
		ul_remove_quotes(Sext[F_RECORD]);
	}
	p = ux_getenv("UD_LAY_SUFFIX",UX_NPRTERRS); 
	if (p != UU_NULL)
	{
		strcpy(Sext[F_LAY],p);
		ul_remove_quotes(Sext[F_LAY]);
	}
	p = ux_getenv("UD_MENU_SUFFIX",UX_NPRTERRS); 
	if (p != UU_NULL)
	{
		strcpy(Sext[F_MENU],p);
		ul_remove_quotes(Sext[F_MENU]);
	}
	p = ux_getenv("UD_KEYFILE_SUFFIX",UX_NPRTERRS);
	if (p != UU_NULL)
	{
		strcpy(Sext[F_KEYDEF],p);
		ul_remove_quotes(Sext[F_KEYDEF]);
	}
	p = ux_getenv("UL_CLFILE1_SUFFIX",UX_NPRTERRS); 
	if (p != UU_NULL) strcpy(Sext[F_CLFILE],p);
	p = ux_getenv("UL_CLFILE2_SUFFIX",UX_NPRTERRS); 
	if (p != UU_NULL)
	{
		strcat(Sext[F_CLFILE],",");
		strcat(Sext[F_CLFILE],p);
	}
	ul_remove_quotes(Sext[F_CLFILE]);
	p = ux_getenv("UL_MCD_SUFFIX",UX_NPRTERRS);
	if (p != UU_NULL)
	{
		strcpy(Sext[F_MCD],p);
		ul_remove_quotes(Sext[F_MCD]);
	}
	p = ux_getenv("UM_DRAWING_SUFFIX",UX_NPRTERRS);
	if (p != UU_NULL)
	{
		strcpy(Sext[F_DRAWING],p);
		ul_remove_quotes(Sext[F_DRAWING]);
	}
/*
.....Filename not defined
*/
	if (namestr == UU_NULL || namestr[0] == '\0')
	{
		bflag = UU_TRUE;
/*
........Build file filter
*/
		filter[0] = '\0';
		descrip[0] = '\0';
		for (i=0;i<MAXEXT;i++)
		{
			ud_format_file_filter(sbuf,Sext[Sfilt[i]]);
			if (i != 0)
			{
				strcat(filter,"|");
				strcat(descrip,"|");
			}
			strcat(filter,sbuf);
			sprintf(tbuf,"%s (%s)",Sdes[Sfilt[i]],sbuf);
			strcat(descrip,tbuf);
		}
/*
........Display the browser and
........Get the filename
*/
		fname[0] = '\0';
		nc = 0;
		ud_get_filename("Open File","Open File",filter,fname,&nc,descrip,1,
			UU_TRUE);
		if (nc == 0) return;
		type = -1;
	}
/*
.....User supplied filename
........Search for file type
*/
	else
	{
		bflag = UU_FALSE;
		tok = strtok(namestr,",\t\n");
		if (tok == UU_NULL) return;
		strcpy(fname,tok);
		tok = strtok(UU_NULL,",\t\n");
		if (tok == UU_NULL)
			type = -1;
		else
			type = atoi(tok);
	}
/*
.....Open the requested file
*/
	S_open_file(fname,&type);
/*
.....Store in the Recent Files menu
*/
	if (bflag) nclc_save_recent_file(fname,type);
}

/*********************************************************************
**    E_FUNCTION     : S_open_file(fullname,stype)
**			Load the file according to the file type.
**    PARAMETERS
**       INPUT  :
**          fullname = Name of file to be loaded.
**          stype    = Type of file to open (see ncl_open_file description).
**
**       OUTPUT :
**          stype    = Updated file type if it was set to -1 on entry.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_open_file(inname,stype)
char *inname;
int *stype;
{
	int status,pos[2],size[2],type,subt,i,imach,filt[MAXEXT],inc;
	char *p,*q,*ux_getenv(),*rindex();
	char fext[UX_SUFFIX_LEN],sfile[42],cmd[UX_MAX_PATH_LEN*2];
	UX_pathname fname,sbuf,cutfil,fullname,ebuf;
	NCL_cmdbuf cmdbuf;
	struct UM_drawing_rec drawing;
/*
.....Create full filename in case
.....resetting NCL restores the initial directory
*/
	ul_build_full_fname("",inname,"",fullname);
/*
.....Determine type of file
*/
	type = *stype;
	if (type < 0 || type >= MAXEXT)
	{
		p = (char *)rindex(fullname,'.');
		if (p == UU_NULL)
			type = 0;
		else
		{
			type = -1;
			strcpy(fext,p+1);
			for (i=0;i<MAXEXT;i++)
			{
				if (S_compare_ext(fext,Sext[i]))
				{
					type = i;
					break;
				}
			}
		}
	}
/*
.....Adjust the file filter
.....for the selected file type
*/
	if (type >= 0 && type < MAXEXT && type != Sfilt[0])
	{
		filt[0] = type;
		inc = 1;
		for (i=0;i<MAXEXT;i++)
			if (Sfilt[i] != type) filt[inc++] = Sfilt[i];
		for (i=0;i<MAXEXT;i++) Sfilt[i] = filt[i];
	}
/*
.....Open correct file type
*/
	*stype = type;
	switch (type)
	{
/*
........Unibase file
*/
	case F_UNIBASE:
		status = uz_load(fullname); 
		break;
/*
........Record/Playback file
*/
	case F_RECORD:
		status = uz_zplayback(fullname); 
		break;
/*
........IGES model
........STEP model
*/
	case F_IGES:
	case F_STEP:
		status = UU_FAILURE;
		if (type == F_IGES) strcpy(ebuf,"IGES_EXE");
		else strcpy(ebuf,"STEP_EXE");
		p = ux_getenv(ebuf,UX_NPRTERRS); 
		if (p != UU_NULL)
		{
			ul_break_fname(p,sbuf,fname);
			sprintf(cmd,"\"%s\"",fullname);
			strcat(cmd," -out:ncltemp.u -progress");
			status = ul_run_process(sbuf,fname,cmd);
			if (status == 1)
			{
				status = uz_load("ncltemp.u"); 
				ux_delete("ncltemp.u",UX_NPRTERRS);
				ux_delete("ncltemp.lst",UX_NPRTERRS);
			}
		}
		else
		{
			sprintf(sbuf,"%s is undefined.",ebuf);
			ud_wrerr(sbuf);
			status = UU_FAILURE;
		}
		break;
/*
........NCL session file
*/
	case F_SESSION:
		ul_break_fname(fullname,sbuf,fname);
		ul_getvalid_fulldir(sbuf,fname);
		status = ul_session_load(fname,UU_FALSE);
		break;
/*
........Stock file
........STL model
*/
	case F_STOCK:
	case F_STL:
		if (type == F_STOCK)
			sprintf(sbuf,"*SOLID/LOAD,\"%s\"",fullname);
		else
			sprintf(sbuf,"*SOLID/STL,\"%s\"",fullname);
		ncl_init_cmdbuf(&cmdbuf);
		ncl_add_token(&cmdbuf,sbuf,NCL_nocomma);
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		status = UU_SUCCESS;
		break;
/*
........Layout file
*/
	case F_LAY:
		status = ud_load_layout(fullname);
		break;
/*
........Menu file
*/
	case F_MENU:
		pos[0] = pos[1] = -1;
		size[0] = size[1] = -1;
		status = udm_read_menu(fullname,pos,size,1,1,-1);
		break;
/*
........Key definition file
*/
	case F_KEYDEF: 
		status = uz_load_keydef_file(fullname); 
		break;
/*
........Tool Library
*/
	case F_TOOLIB:
		status = ncl_load_toolncl(fullname);
		if (status == UU_SUCCESS) nclu_cutter_def();
		break;
/*
........Clfile
........Catia clfile
*/
	case F_CLFILE:
	case F_CATIA:
		status = nclu_load_clfile(fullname,1);
		if (status == UU_SUCCESS)
		{
			nclu_set_external_clfile(1,fullname);
			nclu_playback();
		}
		break;
/*
........MCD file
*/
	case F_MCD:
		status = nclu_mcd_options(sbuf,cutfil,UU_FALSE);
		if (status == UU_SUCCESS)
		{
			status = nclu_load_mcd(fullname,sbuf,cutfil);
			if (status == UU_SUCCESS)
			{
				nclu_set_external_clfile(2,fullname);
				nclu_playback();
			}
		}
		break;
/*
........MasterCam clfile
*/
	case F_MASTERCAM:
		status = nclu_load_clfile(fullname,9);
		if (status == UU_SUCCESS)
		{
			nclu_set_external_clfile(9,fullname);
			nclu_playback();
		}
		break;
/*
........Simulation file
*/
	case F_SIMUL:
		status = ncl_simulate_load(fullname,&LW_mach_desc);
		if (status == UU_SUCCESS)
		{
			nclu_set_external_clfile(1,fullname);
			nclu_playback();
		}
		break;
/*
........NCLIPV Session file
*/
	case F_IPVSESS:
		status = ul_ipv_load_session(fullname,UU_TRUE);
		break;
/*
........Drawing file
*/
	case F_DRAWING:
		sbuf[0] = '\0';
		status = um_retrieve_drawing(sbuf,fullname,"",UU_FALSE);
		if (status == UU_SUCCESS)
		{
			if (um_key_from_drawing_name(sbuf,&drawing.key) == UU_SUCCESS)
			{
				UM_2d3d_mode = UM_2D;
				status = um_get_all_geom(&drawing,sizeof(struct UM_drawing_rec));
				if (status == UU_SUCCESS)
					um_view_drawing(&drawing);
			}
		}
		break;
/*
........Part program file
*/
	default:
		strcpy(NCL_load_part,fullname);
		udm_signoff(UU_TRUE); 
		NCL_load_part[0] = '\0';
		status = UU_SUCCESS;
		break;
	}
/*
.....Could not load file
*/
	if (status != UU_SUCCESS)
	{
		ul_short_filename(fullname,sfile,40);
		sprintf(fname,"Could not load %s",sfile);
		ud_wrerr(fname);
	}
}

/*********************************************************************
**    E_FUNCTION     : S_compare_ext(fext,sfilter);
**			Compares a file extension with extensions stored in a file
**       filter string.
**    PARAMETERS
**       INPUT  :
**          fext     = File extension.
**          sfilter  = File filter in the format of "fext1,fext2,fext3".
**
**       OUTPUT : none
**    RETURNS      :
**          UU_TRUE if the file extension matches one of those in the
**          file filter.  UU_FALSE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_compare_ext(fext,sfilter)
char *sfilter,*fext;
{
	char sbuf[UX_SUFFIX_LEN],fbuf[UX_SUFFIX_LEN],tbuf[UX_SUFFIX_LEN];
	char *p,*q,*r;
/*
.....Initialize routine
*/
	strcpy(sbuf,sfilter);
	strcat(sbuf,",");
	p = sbuf;
/*
.....See if extension matches
.....one stored in the filter
*/
	do
	{
		strcpy(fbuf,fext);
		q = strchr(p,',');
		strncpy(tbuf,p,q-p);
		tbuf[q-p] = '\0';
		r = strchr(tbuf,'*');
		if (r != UU_NULL)
		{
			fbuf[r-tbuf] = '*'; fbuf[r-tbuf+1] = '\0';
		}
		if (ul_compare_upper(fbuf,tbuf) == 0) return(UU_TRUE);
		p = q+1;
	} while (*p != '\0');
	return(UU_FALSE);
}
