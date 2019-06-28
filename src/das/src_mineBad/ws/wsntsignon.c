#include "usysdef.h"
#if UU_COMP == UU_WIN2K

/********************************************************************* 
**  NAME:  wsntsignon.c
**
**		CONTAINS:
**			uw_ntsignon()
**			uw_ntsignload()
**			uw_ntsignoff()
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsntsignon.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 16:56:34
**    
*********************************************************************/
#include "dasnog.h"
#include "driver.h"
#include "dinput.h"
#include "mfort.h"
#include "nclfc.h"
#include "lcom.h"
#include "dmotif.h"
#include "dmark.h"
#include "gobas.h"
#include "gdidd.h"
#include "gtblvar4.h"
#include "gtblvar6.h"
#include "nclfile.h"
#include "udfdata.h"
#include "udforms.h"
#include "usysg.h"
#include "xenv1.h"
#include "xfsys1.h"
#include<string.h>
#include "spmouse.h"
#include "dselect.h"
#include "nccs.h"
/*
.....Global variable definitions
*/
int  UW_nosignonform = 0;
int UW_signon_failed = 0;
int UW_signon = 0;

extern UU_LOGICAL UR_changed;
extern char UR_dpn[];
extern UX_pathname partdir;

int isign[2];
int tpp,tub,tcl,tas, tsess;
UX_pathname tpps,tubs,tcls,tass, tssion;

static UD_FSTAT filtog();
static int signon_cam = 1;
static int signon_cad = 1;
extern int NCL_vx_flag;
static char traverse[]		= {1,0,0, 1,0,0, 1,0,0, 1,0,0, 1,0,0};

extern int NAUTLTH;
extern int N3AXIS;
extern int NPRPST;
extern int NAUTIPV;
int WS_update_tip = 0;
extern UU_LIST NCL_ldr_list;
extern UX_pathname NCL_load_part;
extern unsigned int NCL_subprocess;
extern char NCL_keyinfo[9];
/**********************************************************************
**    I_FUNCTION :  uw_ntsignon ()
**       Processes the WinNT Signon.
**    PARAMETERS   
**       INPUT  : 
**				None
**       OUTPUT :  
**				None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_ntsignon()
{
	int nc,i;
	char fname1[UX_MAX_FILE_LEN];
	char *ux_getenv();
	int cam,cad,iop[2];
	char *filen,fn[UX_MAX_PATH_LEN],fullname[UX_MAX_PATH_LEN];
	char dir[UX_MAX_PATH_LEN], dir1[UX_MAX_PATH_LEN];
	char buff[80],*p,sufx[80];
	char filter[500], filter1[500], descrip[500];
	static int first = 1;
	static int loopvar=UU_TRUE;

	dir1[0] = '\0';
	dir[0] = '\0';
	cad = 1;
	cam = 1;

	filen = fn;
	filen[0] = '\0';

	fname1[0] = '\0';

	UW_signon = 1;
	if((UU_comargc>1)&&(strcmp(UU_comargv[0],"-x")!=0)&&(first==1))
	{
		for(i=0; i<UU_comargc-1; i++)
		{
			if((strcmp(UU_comargv[i],"-nocam")==0)||
							 (strcmp(UU_comargv[i],"-NOCAM")==0))
				cam = 0;
			else if((strcmp(UU_comargv[i],"-cam")==0)||
							 (strcmp(UU_comargv[i],"-CAM")==0))
				cam = 1;
			else if((strcmp(UU_comargv[i],"-nocad")==0)|| 
							 (strcmp(UU_comargv[i],"-NOCAD")==0))
				cad = 0;
			else if((strcmp(UU_comargv[i],"-cad")==0)||
							 (strcmp(UU_comargv[i],"-CAD")==0))
				cad = 1;
			else if((strncmp(UU_comargv[i],"-d",2)==0)||
							(strncmp(UU_comargv[i],"-D",2)==0))
				strcpy(dir1, &(UU_comargv[i][2]));
			else if((strncmp(UU_comargv[i],"-l",2)==0)||
							(strncmp(UU_comargv[i],"-L",2)==0))
/*
.....do nothing because we have already do something about layout file 
*/
				continue;				
/*
......sub-process
*/
			else if((strncmp(UU_comargv[i],"-n=", 3)==0)||
							 (strncmp(UU_comargv[i],"-N=", 3)==0))
				continue;					
/*
.....filename
*/
			else if (filen[0]=='\0')
				strcpy(filen, UU_comargv[i]);
		}
/*
...save signon cad , cam for later signon form
*/
		signon_cad = cad;
		signon_cam = cam;
		strcpy(fullname,filen);
		ul_break_fname(fullname,dir,fname1);
		strcat(dir, dir1);
		iop[0] = 1; iop[1] = 0;
		ul_get_base_fname(fname1,fname1,iop,UX_NPRTERRS);
	}
/*
......if there are filename
...... don't using signon browser
*/
	if((fname1[0]!='\0')&&(first==1))
	{
		first = 0;
		goto doform;
	}
/*
.....if we already have a part program to be load
.....don't using signon browser
*/
	if (NCL_load_part[0]!='\0')
	{
		strcpy(fullname, NCL_load_part);
		ul_break_fname(fullname,dir,fname1);
		NCL_load_part[0] = '\0';
		goto doform;
	}
/*
.....Get Part Program suffices
*/
	strcpy(descrip, "Part Program Files (");
	p = ux_getenv("UL_PROGRAM_SUFFIX",UX_NPRTERRS);
	if (p != 0) strcpy(sufx,p);
	else strcpy(sufx,"*.pp");

	fullname[0] = '\0';
	filter1[0] = '\0';
	do
	{
		strcat(filter1,"*.");
		strcpy(buff,sufx);
		p = (char *) index(buff,',');
		if (p == 0) sufx[0] = '\0';
		else
		{
			strcpy(sufx,p+1);
			*p = '\0';
		}
		strcat(filter1,buff);
		strcat(filter1,",");
	} while (strlen(sufx) != 0);
	i = strlen(filter1) ; if (i != 0) filter1[i-1] = '\0';
	strcat(descrip, filter1);
	strcat(descrip, ")|");

	strcpy(filter, filter1);
	strcat(filter,"|*.");
	strcat(descrip, "Unibase Files (*.");
	p = ux_getenv("UR_PART_SUFFIX");
	if (p != UU_NULL)
	{
		strcpy(filter1,p);
		ul_remove_quotes(filter1);
		strcat(filter,filter1);
		strcat(descrip, filter1);
	}     
	else 
	{
		strcat(filter,"u");
		strcat(descrip, "u");
	}
	
	strcat(descrip, ")|");
	strcat(descrip, "Textual Unibase Files (*.");
					
	p = ux_getenv("UR_ASCII_PART");
	strcat(filter,"|*.");
	if (p != UU_NULL)
	{
		strcpy(filter1,p);
		ul_remove_quotes(filter1);
		strcat(filter,filter1);
		strcat(descrip, filter1);
	}       
	else
	{
		strcat(filter,"ud");
		strcat(descrip, "ud");
	}
	strcat(descrip, ")|");
	strcat(descrip, "Record/Playback Files (*.");
					
	p = ux_getenv("UD_RPB_SUFFIX");
	strcat(filter,"|*.");
	if (p != UU_NULL)
	{
		strcpy(filter1,p);
		ul_remove_quotes(filter1);
		strcat(filter,filter1);
		strcat(descrip, filter1);
	}       
	else
	{
		strcat(filter,"rp");
		strcat(descrip, "rp");
	}
	strcat(descrip, ")");
/*
.....open signon browser
*/
	if (first)
	{
/*
.....don't user browser when first in
*/
		first = 0;
		goto doform;
	}
	ud_get_filename(NULL, "NCL Signon", filter, fullname, &nc, descrip, 1);
/*	accept = uw_ntget_signon(NULL, "NCL Signon", filter, fullname, &nc, descrip);
	if (accept!=1)
	{
		if (NCL_vx_flag) 
		{
			ncl_vx_setunits();
			vxload();
		}
		return;
	}
*/
	if (NCL_vx_flag) 
	{
		ncl_vx_setunits();
		vxload();
	}
	ul_break_fname(fullname,dir,fname1);
doform:;
/*
...   Load VX geometry if running under VX.
*/
	if (NCL_vx_flag) 
	{
		ncl_vx_setunits();
		vxload();
	}
	uw_ntsignload(dir, fname1, cam, cad);
 	UD_pickmode = 0;
	UV_Cur_Dyn = 0;	
	ud_setpick_type(UD_PICK_NORMAL);
	uu_list_init (&NCL_ldr_list,sizeof(UU_KEY_ID),0,100);
}

/**********************************************************************
**    I_FUNCTION : uw_ntsignload(dir,fname,cam, cad) 
**       authorizes the product, loads the
**			input file, and enters interactive NCL.
**    PARAMETERS   
**       INPUT  : 
**         		dir: directory name 
**					fname: filename (not include dir name)
**					cad : 0 or 1
**				   cam : 0 or 1  
**       OUTPUT :  
**         		none 
**    RETURNS      : none
**    SIDE EFFECTS : Brings up the initial interactive layout.
**    WARNINGS     : none
*********************************************************************/
uw_ntsignload(dir, fname, cam, cad)
char *dir, *fname;
int cam, cad;
{
	int i, ocam,ocad,lathe,tl3ax, prepst,stat,vmill;
	char mbuf[80],cbuf[80],*p,*ux_getenv();
	char ppname[UX_MAX_FILE_LEN],buf[UX_MAX_FILE_LEN+40],*pos;
	char *q, *m ,pc[UX_SUFFIX_LEN],qc[UX_SUFFIX_LEN], mc[UX_SUFFIX_LEN];
	static char ustr[]="u", udstr[]="ud", rpstr[] = "rp";
	UM_int2 idx,ival,ierr=0,batch=0,wflg=0,kfl;
	UM_int4 jval;
	UM_f77_str ppn;
	char fullname[UX_MAX_PATH_LEN];
/*
.....Initialize modals
*/
	ul_load_nis_mod();
/*
.....Allocate CAM & CAD user
*/
	cauth(&cam,&cad,&ocam,&ocad,&lathe,&tl3ax,&prepst,&vmill,mbuf,cbuf);
/*
.....Could not authorize
*/
	if (ocam == -1) uw_nterror(mbuf);
	else if (ocad == -1) uw_nterror(cbuf);
	else if (ocam == 0 && ocad == 0)
	{
		uw_nterror("You must enable either CAM or CAD");
		UW_signon_failed = 1;
		goto done;
	}
	if (ocam == -1) ocam = 0;
	if (ocad == -1) ocad = 0;
	if (ocam == 0 && ocad == 0)
	{
		UW_signon_failed = 1;
		goto done;
	}
/*
.....Set CAD/CAM flags
*/
	UL_ipv = NAUTIPV;
	UL_cam = ocam; UL_cad = ocad; UL_lathe = lathe; UL_3axis = tl3ax;
	idx = 142; ival = lathe; setifl(&idx,&ival);
	idx = 304; ival = tl3ax; setifl(&idx,&ival);
	idx = 351; ival = prepst; setifl(&idx,&ival);
	idx = 389; ival = vmill; setifl(&idx,&ival);
	idx = 1; jval = 0; setjfl(&idx,&jval);	/* Program is empty */
/*
.....Set working directory
*/
	strcpy(UL_part,dir);
	if (ul_set_dir(dir,UL_part) != UU_SUCCESS) goto done;
	strcpy(partdir,UL_part);
/*
.....Initialize NCL
*/
	UU_application = UU_NCLCAM;
/*
	ppini(&ierr);
	if (ierr != 0)
	{
		uw_nterror("Could not open Part Program Work File.");
		UW_signon_failed = 1;
		goto done;
	}
*/
/*
.....Initialize IPV
*/
	ul_verify_reset();
	ul_init_axisseg();
/*
.....Initialize internal clfile
*/
	clinit();
/*
.....Initialize motion stack
*/
	ncl_mot_stack_init();
/*
.....Initialize cutter display
*/
	ncl_cutter_reset();
/*
...for recording
*/
	ud_wrsignon("DONE", NULL);
/*
.....Set wait cursor when loading
.....Yurong  7/28/97
*/
	uw_ntsetcursor(21); 
/*
.....Check for NCL Session file
.....
.....Can't load an NCL session here
.....because the interface is not ready
.....to be manipulated yet
*/
	stat = UU_FAILURE;
/*
	ul_build_full_fname(dir,fname,"",sfil);
	mode = UX_EXISTS|UX_READ|UX_FAREA;
	stat = ux_file_inquire(UU_NULL,UU_NULL,sfil,UU_NULL,UU_NULL,&mode,
		&fstat,buf,UX_NPRTERRS);
	if (mode != (mode|UX_FAREA)) stat = UU_FAILURE;
	if (stat == UU_SUCCESS)
	{
		stat = ul_session_load(sfil,UU_FALSE);
	}
*/
/*
.....Determine input file type
*/
	if (stat != UU_SUCCESS)
	{
		pos = (char *) rindex(fname,'.');

		p = ux_getenv("UR_UNB_FILE",UX_NPRTERRS);
		q = ux_getenv("UR_ASCII_PART",UX_NPRTERRS); 
		m = ux_getenv("UD_RPB_SUFFIX",UX_NPRTERRS); 
		if (p == 0) p = (char *)&ustr;
		if (q == 0) q = (char *)&udstr;
		if (m == 0) m = (char *)&rpstr;
		strcpy(pc,p) ; strcpy(qc,q); strcpy(mc, m);
		ul_remove_quotes(pc) ; ul_remove_quotes(qc) ; ul_remove_quotes(mc);
		ul_remove_quotes(p) ; ul_remove_quotes(q) ; ul_remove_quotes(m);

		ul_to_upper(pc) ; ul_to_upper(qc); ul_to_upper(mc);
/*
.........Load Unibase
*/
		if (pos != UU_NULL && (strcmp(pos+1,pc) == 0 || strcmp(pos+1,qc) == 0 ||
		  			strcmp(pos+1,p) == 0 || strcmp(pos+1,q) == 0))
		{
			UM_init_f77_str(ppn,ppname,UX_MAX_FILE_LEN);
			strcpy(ppname,fname);
			strcpy(buf,"Loading "); strcat(buf,ppname);
			ud_prmerr(buf);
			i = strlen(ppname);
/*			for (i=strlen(ppname);i<UX_MAX_FILE_LEN;i++) ppname[i] = ' ';*/
			loadu(UM_addr_of_f77_str(ppn),&i,&batch,&ierr);
			if (ierr==0)
			{
/*
......save the whole path
*/
				ul_build_full_fname(UL_part, fname, NULL, fullname);
				nclc_save_recent_file(fullname, 1);
			}
		}
/*
......... initialize playback file
*/
		else if (pos != UU_NULL && (strcmp(pos+1,m) == 0 || strcmp(pos+1,mc)==0)) 
		{	
			ud_signinit(fname);
/*
......save the whole path
*/
			ul_build_full_fname(dir, fname, NULL, fullname);
			nclc_save_recent_file(fullname, 2);
		}
/*
.....Load Part Program
*/
		else
		{
			UM_init_f77_str(ppn,ppname,UX_MAX_FILE_LEN);
			strcpy(ppname,fname);
			if (pos != UU_NULL)
			{
				strcpy(UL_program_suffix,(pos+1));
				*pos = '\0';
			}
			strcpy(UL_program,fname);
			strcpy(buf,"Loading "); strcat(buf,ppname);
			ud_prmerr(buf);
			i = strlen(ppname);
/*			for (i=strlen(ppname);i<UX_MAX_FILE_LEN;i++) ppname[i] = ' ';*/
			loadpp(UM_addr_of_f77_str(ppn),&i,&ierr,&wflg);
			uw_ntret_actln();
			ptppnm(UM_addr_of_f77_str(ppn),&i);
			ptdfnm(UM_addr_of_f77_str(ppn),&i);
			kfl = UL_create_cl;
			setclf(UM_addr_of_f77_str(ppn),&i,&kfl);
			kfl = UL_create_as;
			setas(UM_addr_of_f77_str(ppn),&i,&kfl);
			if (ierr==0)
			{
/*
......save the whole path
*/
				ul_build_full_fname(UL_part, UL_program, UL_program_suffix, fullname);
				if (UL_program[0]!='\0')
					nclc_save_recent_file(fullname, 0);
			}
		}
	}
done:;
	znu_copyright();
	uz_status();
	return(UU_SUCCESS);
}

/***********************************************************************
c
c   FUNCTION: OnBrowser(fieldno, val, stat)
c
c          callback function when button on the signoff form is pushed
c
c   INPUT  : fieldno  = Field number being changed.
c                val      = Current field value.
c                stat     = Field status.
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
static UD_FSTAT OnBrowser(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char sbuf[80],ext[UX_SUFFIX_LEN],descrip[80],*p;
	char sufx[UX_SUFFIX_LEN];
	int i,inum;
	UX_pathname filename;
	char buff[80];
	switch(*fieldno)
	{
		case 1:
			ext[0] = '\0';
			strcpy(descrip, "Part Program Files (");
			p = ux_getenv("UL_PROGRAM_SUFFIX");
			if (p != 0) strcpy(sufx,p);
			else strcpy(sufx,"*");
			ul_remove_quotes(sufx);
			do
			{
				strcat(ext,"*.");
				strcpy(buff,sufx);
				p = (char *) index(buff,',');
				if (p == 0) sufx[0] = '\0';
				else
				{
					strcpy(sufx,p+1);
					*p = '\0';
				}
				strcat(ext,buff);
				strcat(ext,",");
			} while (strlen(sufx) != 0);
			i = strlen(ext) ; if (i != 0) ext[i-1] = '\0';
			strcat(descrip, ext);
			strcat(descrip, ")");
			strcpy(sbuf,"Save Program File");
			strcpy(filename, tpps);
			ud_get_filename(sbuf,sbuf,ext,filename,&inum,descrip, 0,UU_FALSE);
			if (inum != 0)
				ud_update_answer(2, (int *)filename);
			break;
		case 4:
			strcpy(ext,"*.");
			strcpy(descrip, "Unibase Files (");
			p = ux_getenv("UR_PART_SUFFIX");
			if (p != UU_NULL)
			{
				strcpy(sufx, p);
				ul_remove_quotes(sufx);
				strcat(ext,sufx);
			}       
			else 
				strcat(ext,"u");

			strcat(descrip, ext);
			strcat(descrip, ")|Textual Unibase Files (*.");
			p = ux_getenv("UR_ASCII_PART");
			strcat(ext,"|*.");
			if (p != UU_NULL)
			{
				strcpy(sufx, p);
				ul_remove_quotes(sufx);
				strcat(ext, sufx);
				strcat(descrip, sufx);
			}       
			else
			{
				strcat(ext,"ud");
				strcat(descrip, "ud");
			}
			strcat(descrip, ")");
			strcpy(filename, tubs);
			ud_get_filename("Save Unibase Name", "Save Unibase Name", ext,
									filename, &inum, descrip, 0,UU_FALSE);
			if (inum != 0)
				ud_update_answer(5, (int *)filename);
			break;
		case 7:
			strcpy(descrip, "Clfiles File (");
			strcpy(ext,"*.");
			p = ux_getenv("UL_CLFILE1_SUFFIX");
			if (p != UU_NULL)
			{
				strcpy(sufx,p);
				ul_remove_quotes(sufx);
				strcat(ext,sufx);
			}       
			else strcat(ext,"cl");
			strcat(descrip, ext);
			strcat(descrip, ")|Secondary Clfiles (*.");
			
			p = ux_getenv("UL_CLFILE2_SUFFIX");
			strcat(ext,"|*.");
			if (p != UU_NULL)
			{
				strcpy(sufx,p);
				ul_remove_quotes(sufx);
				strcat(ext,sufx);
				strcat(descrip, sufx);
			}
			else
			{
				strcat(ext,"cln");
				strcat(descrip, "cln");
			}
			strcat(descrip, ")");
			strcpy(filename, tcls);
			ud_get_filename("Save Clfile Name", "Save Clfile Name", ext,
									filename, &inum, descrip, 0,UU_FALSE);
			if (inum != 0)
				ud_update_answer(8, (int *)filename);
			break;
		case 10:
			strcpy(filename, tass);
			ud_get_filename("APT Files", "APT Files", "*.as",
						filename, &inum, "APT Source Files (*.as)", 1,UU_FALSE) ;
			if (inum != 0)
				ud_update_answer(11, (int *)filename);
			break;
		case 13:
			strcpy(filename, tssion);
			ud_get_dirname("Save NCL session in ...", "Save NCL Session", filename, &inum);
			if (inum != 0)
				ud_update_answer(14, (int *)filename);
			break;
	}
	return(UD_FLDOK);
}
/*********************************************************************
**
**	 I_FUNCTION : filtog(fieldno, val, stat)
**
*********************************************************************/
static UD_FSTAT filtog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Enable correct fields
.....based on toggle field value
*/
	switch(*fieldno)
	{
		case 0:
			if (tpp == 1) 
			{
				traverse[1] = traverse[2] = UU_TRUE;
				ud_set_traverse_mask(1,UU_TRUE);
				ud_set_traverse_mask(2,UU_TRUE);
			}
			else 
			{
				traverse[1] = traverse[2] = UU_FALSE;
				ud_set_traverse_mask(1,UU_FALSE);
				ud_set_traverse_mask(2,UU_FALSE);
			}
			break;
		case 3:
			if (tub == 1)
			{
				traverse[4] = traverse[5] = UU_TRUE;
				ud_set_traverse_mask(4,UU_TRUE);
				ud_set_traverse_mask(5,UU_TRUE);
			}
			else 
			{
				traverse[4] = traverse[5] = UU_FALSE;
				ud_set_traverse_mask(4,UU_FALSE);
				ud_set_traverse_mask(5,UU_FALSE);
			}
			break;
		case 6:
			if (tcl == 1)
			{
				ud_set_traverse_mask(7,UU_TRUE);
				ud_set_traverse_mask(8,UU_TRUE);
				traverse[7] = traverse[8] = UU_TRUE;
			}
			else 
			{
				ud_set_traverse_mask(7,UU_FALSE);
				ud_set_traverse_mask(8,UU_FALSE);
				traverse[7] = traverse[8] = UU_FALSE;
			}
			break;
		case 9:
			if (tas == 1)
			{
				ud_set_traverse_mask(10,UU_TRUE);
				ud_set_traverse_mask(11,UU_TRUE);
				traverse[10] = traverse[11] = UU_TRUE;
			}
			else 
			{
				ud_set_traverse_mask(10,UU_FALSE);
				ud_set_traverse_mask(11,UU_FALSE);
				traverse[10] = traverse[11] = UU_FALSE;
			}
			break;
		case 12:
			if (tsess == 1)
			{
				ud_set_traverse_mask(13,UU_TRUE);
				ud_set_traverse_mask(14,UU_TRUE);
				traverse[13] = traverse[14] = UU_TRUE;
			}
			else 
			{
				ud_set_traverse_mask(13,UU_FALSE);
				ud_set_traverse_mask(14,UU_FALSE);
				traverse[13] = traverse[14] = UU_FALSE;
			}
			break;
		default:
			break;
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**	 E_FUNCTION : uw_ntsignoff(restart)
**			This function terminates the current session.
**	 PARAMETERS	
**		 INPUT  :	restart  =  0 = Go to Signon Form.  1 = Reset current
**						            job, but stay in NCL, 2 = Same as 1, but
**                            does not display the signon form.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Initializes NCL502 variables.
**	 WARNINGS:
*********************************************************************/
int uw_ntsignoff(restart)
UU_LOGICAL restart;
{
	int ncpp,ncub,nccl,ncas,nc, status,istat,markval;
	UX_pathname fullname;
	char msgbuf[100],*p,sbuf[80];
	UM_int2 idx,ival,wflg=0,ifl,iflx;
	UM_int4 jval;
	UM_f77_str ppn;
	char *ux_getenv();
	UM_f77_str ftcls,ftass;
/*
.....Set up form fields
*/
	static UD_METHOD methods[]	= {filtog, OnBrowser, UU_NULL,
									filtog, OnBrowser, UU_NULL,
									filtog, OnBrowser, UU_NULL,
									filtog, OnBrowser, UU_NULL,
									filtog, OnBrowser, UU_NULL};
	static char called[]			= {6,6,6, 6,6,6, 6,6,6, 6,6,6, 6,6,6};
	static int *ans[]				= {&tpp, UU_NULL, (int *)&tpps[0],
										   &tub, UU_NULL, (int *)&tubs[0],
										   &tcl, UU_NULL, (int *)&tcls[0],
										   &tas, UU_NULL, (int *)&tass[0],
											&tsess, UU_NULL, (int *)&tssion[0]};
	if (NCL_subprocess==0)
	{
		if (uw_ntget_subncl()>0)
		{
			uw_nterror("There are still active NCL sessions.\r\nPlease close all sessions before exiting.");
			return 0;
		}
	}
/*
.....reject any event before sign off form display
*/
	if ((restart==0) && (UD_pickmode==1))
	{
		uw_ntpost_closemsg();
		return 0;
	}
/*
.....Set Form Parameters
........Save Part Program
*/
	idx = 1; getjfl(&idx,&jval);
	tpp = UL_save_pp;
	if (jval == 0) tpp = 0;
	traverse[0] = 1;
	if (jval == 0) traverse[0] = 0;
	traverse[1] = tpp;
	traverse[2] = tpp;
	strcpy(tpps, UL_program); strcat(tpps,"."); strcat(tpps,UL_program_suffix);
/*
.......Save Unibase
*/
	tub = 0;
	traverse[3] = 1;
	traverse[4] = 0;
	traverse[5] = 0;
	if (!ur_unibase_used()) traverse[3] = 0;
	strcpy(tubs,UR_dpn);
	ux_add_ftype("UR_UNB_FILE",tubs,UX_NPRTERRS);
/*
........Save clfile & APT Source file
*/
	if (UN_clnum[0] == 0)
	{
		traverse[6] = 0;
		traverse[9] = 0;
		tcl = 0;
		tas = 0;
	}
	else
	{
		traverse[6] = 1;
		traverse[9] = 1;
		tcl = 0;
		tas = 0;
		ifl = 69; getifl(&ifl,&iflx);
		if (iflx == 1 || UL_create_cl == 1) tcl = 1;
		ifl = 88; getifl(&ifl,&iflx);
		if (iflx == 1 || UL_create_as == 1) tas = 1;
	}
	traverse[8] = 0;
	traverse[7] = 0;
	traverse[10] = 0;
	traverse[11] = 0;
	UM_init_f77_str(ftcls,tcls,UX_MAX_FILE_LEN);
	UM_init_f77_str(ftass,tass,UX_MAX_FILE_LEN);
	getcln(UM_addr_of_f77_str(ftcls),&nc); tcls[nc] = '\0';
	getapn(UM_addr_of_f77_str(ftass),&nc); tass[nc] = '\0';
	traverse[12] = 1;
	traverse[13] = 0;
	traverse[14] = 0;
/*
.....Get the Form input
*/
again:
	if (traverse[0] == 1 || traverse[3] == 1 || traverse[6] == 1 ||
		traverse[9] == 1)
	{
		status = ud_form1("lsignoff.frm",ans, ans, methods, called, UU_NULL, traverse);
		if (status==-1)
		{
			if (NCL_subprocess==0)
				return -1;
			else
			{
				if (!(strcmp(NCL_keyinfo, "00000000")==0))
					return -1;
/*
......else forced out (even though user cancel the form
*/
			}
		}
	}
/*
.....Empty the list used for labels and leader lines
*/
		uu_list_free (&NCL_ldr_list);
		if (restart) 
			uu_list_init (&NCL_ldr_list,sizeof(UU_KEY_ID),0,100);
	ncpp=strlen(tpps);
	ncub=strlen(tubs);
	nccl=strlen(tcls);
	ncas=strlen(tass);
/*
.....Save the part program file
*/
	if (tpp == 1)
	{
		UM_init_f77_str(ppn,tpps,UX_MAX_FILE_LEN);
		ul_short_filename(tpps,sbuf,60);
		sprintf(msgbuf,"Saving %s.",sbuf);
		ud_prmerr(msgbuf);
/*		for (i=ncpp;i<UX_MAX_FILE_LEN;i++) tpps[i] = ' ';*/
		UD_MARK(markval,UU_TRUE);
		if (markval != 0) goto again;
		istat = savepp(UM_addr_of_f77_str(ppn),&ncpp,&wflg);
		ud_killmsg(UD_ERRORMSG);
		UD_UNMARK(markval);
		if (istat != UU_SUCCESS)
		{
			fullname[0] = '\0';
			istat = nclu_read_part_prog(fullname,1);
			if (istat != 0) goto again;
		}
		ptppnm(UM_addr_of_f77_str(ppn),&ncpp);
	}
/*
.....Set the clfile name
*/
	UM_init_f77_str(ppn,tcls,UX_MAX_FILE_LEN);
/*	for (i=nccl;i<UX_MAX_FILE_LEN;i++) tcls[i] = ' ';*/
	ival = tcl;
	setclf(UM_addr_of_f77_str(ppn),&nccl,&ival);
/*
.....Set the APT source name
*/
	UM_init_f77_str(ppn,tass,UX_MAX_FILE_LEN);
/*	for (i=ncas;i<UX_MAX_FILE_LEN;i++) tass[i] = ' ';*/
	ival = tas;
	setas(UM_addr_of_f77_str(ppn),&ncas,&ival);
/*
.....Save the Unibase
*/
	uv_set_defered_mode();
	if (tub == 1)
	{
		strcpy(UR_dpn,tubs);
		ul_short_filename(tubs,sbuf,60);
		sprintf(msgbuf,"Saving %s.",sbuf);
		ud_prmerr(msgbuf);
		UD_MARK(markval,UU_TRUE);
		if (markval != 0) goto again;
		istat = ur_save_part(UU_TRUE,UU_FALSE);
		UD_UNMARK(markval);
		if (istat != UU_SUCCESS)
		{
			istat = ur_save_part(UU_FALSE,UU_FALSE);
			if (istat != UU_SUCCESS) goto again;
		}
	}
	if ((tsess==1)&&(tssion[0]!='\0'))
	{
		ul_session_save(tssion);
	}
/*
.....Deallocate this user
*/
	unauth(&UL_cam,&UL_cad);
/*
.....Straight Exit 
*/
	if (restart == 0 && !NCL_vx_flag)
	{
		ul_sysexit();
		goto done;
	}
/*
.....Reset the Unibase & NCL
*/
	if (restart == 0 || (restart != 0 && !NCL_vx_flag))
	{
		ud_prmerr("Resetting NCL");
		nclfin();
		umu_inactgrid();
		nclu_erase_motion();
		gdelallsegs();
		uz_status();
		ud_actscrn(0);
		ul_reset_unibase();
		ncl_init_attrmdl_color();
		nclini();
/*
.....Need to set these ifl values after calling nclini, otherwise
.....they will be reset to zero.  These are the values for the
.....SOURCE modals.
*/
		idx = 360;
		ival = UL_format_line;
		setifl(&idx,&ival);
		idx = 230;
		ival = UL_indent_all;
		setifl(&idx,&ival);
		idx = 231;
		ival = UL_indent_sep;
		setifl(&idx,&ival);
		idx = 355;
		ival = UL_major_case;
		setifl(&idx,&ival);
		idx = 356;
		ival = UL_vocab_case;
		setifl(&idx,&ival);
		idx = 357;
		ival = UL_label_case;
		setifl(&idx,&ival);
		idx = 358;
		ival = UL_alignment;
		setifl(&idx,&ival);

		ncl_init_labtbl(NULL);
		ncl_reset_labels();
		uv_set_immediate_mode();
		znu_init_units();
		ul_verify_end();
		UL_program[0] = '\0';
		UL_part[0] = '\0';
		uz_dyn_unzoom();
		UR_changed = UU_FALSE;
		ud_killmsg(UD_ERRORMSG);
/*
.....Change directory to top level
*/
		if ((p = ux_getenv("HOMEDIR")) != UU_NULL)
		{
			chdir(p);
			ul_set_dir(p,fullname);
		}
	}
/*
...   If running under VX, jump to back to VX
*/
	if (NCL_vx_flag)
	{
		if (restart != 0) ival = 1; else ival = 0;
		vxjump (&ival);
	}
	if (restart != 0) 
	{
		ul_ipv_terminate();
		if (restart == 1) uw_ntsignon();
	}
/*
.....End of routine
*/
done:;
	return(UU_SUCCESS);
}

void uw_nt_drop_file(filename)
char *filename;
{
	strcpy(NCL_load_part, filename);
	udm_signoff(UU_TRUE); 
	NCL_load_part[0] = '\0';
}
#endif
