#include "usysdef.h"
#if UU_COMP != UU_WIN2K

/********************************************************************* 
**  NAME:  wsmfsignon.c
**
**		CONTAINS:
**			exitCB (widget, client_data, call_data)
**			okCB (widget, client_data, call_data)
... added for record
... Yurong
**			recordCB (widget, client_data, call_data)
**
**...removed from this file			
**...		uw_mfsignon_play()
**...changed name to ud_signon_play and
**...put into file d3rp1.c
**			uw_mfsignload(dir, fname, cam, cad)
**			do_search(widget, sdata)
**			uw_mfsignon(arg)
**			uw_mfmainloop ()
**			uw_mfsignoff(restart)
...Added this function to
...avoid using uw_xwterm in wsxwwin.c
...Yurong
**			uw_mfterm(prms,reply)
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       wsmfsignon.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:13
**
*********************************************************************/
#ifdef VMS
#include <decw$include:Xm.h>
#include <decw$include:Label.h>
#include <decw$include:PushB.h>
#include <decw$include:RowColumn.h>
#include <decw$include:MessageB.h>
#include <decw$include:SelectioB.h>
#include <decw$include:Frame.h>
#include <decw$include:ToggleB.h>
#else
#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/Frame.h>
#include <Xm/ToggleB.h>
#endif
#include "dasnog.h"
#include "driver.h"
#include "dinput.h"
#include "mfort.h"
#include "nclfc.h"
#include "lcom.h"
#include "ws.h"
#include "wsxw.h"
#include "wsmf.h"
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
#include "wsmfmdesgn.h"
/*
.....Global variable definitions
*/
extern int NAUTIPV;
extern UWS_MF uw_mf;
extern UU_LOGICAL UR_changed;
extern char UR_dpn[];
extern UX_pathname partdir;
extern unsigned int NCL_subprocess;
extern char NCL_keyinfo[9];

static Widget mine,toggle1,toggle2;
/*
... For answer
*/
static int tpp,tub,tcl,tas, tsess;
static UX_pathname tpps,tubs,tcls,tass, tssion;

static UD_FSTAT filtog();
/*
...check if need signon form
*/
int  UW_nosignonform = 0;
static int signon_need = 0;
/*
...for signon
*/
static int signon_cam = 1;
static int signon_cad = 1;
extern int NCL_vx_flag;
/*
.....added by Yurong
.....8/27/97
*/
extern UWS_MFLAYOUT uw_mflayout;
extern int UW_close_mdsgn;
extern UX_pathname NCL_load_part;

void uw_mfmainloop ();
void uw_mfsignload();

int UW_signon = 0;
/**********************************************************************
**    I_FUNCTION :  exitCB (widget, client_data, call_data)
**       Exits NCL.
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = Ignored.
**				call_data   = Motif callback structure.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

static void exitCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	UM_int2 ival = 2;

/*
...   If running under VX, jump to back to VX
*/
	if (NCL_vx_flag)
	{
		XtUnmanageChild(mine);
		vxjump (&ival);
	}

	uz_unlight_buttons();
	ul_sysexit();
}

/**********************************************************************
**    I_FUNCTION :  recordCB
**       Accepts the Signon form, pupup a Prompt Dialog for 
**			input record file name
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = Ignored.
**				call_data   = Ignored
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void recordCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{	
/*
.....change this function to use file browse button
.....Yurong 9/24/98
*/
	UX_pathname fname, descrip, ext;
	char ext1[UX_SUFFIX_LEN], *p, *ux_getenv();
	int len;
	fname[0] = '\0';
	strcpy(ext,"*.");
	p = ux_getenv("UD_RPB_SUFFIX");
	if (p != UU_NULL)
	{
		strcpy(ext1,p);
		ul_remove_quotes(ext1);
		strcat(ext,ext1);
	}       
	else 
		strcat(ext,"rp");
	strcpy(descrip, "Record/Playback File (");
	strcat(descrip,ext);
	strcat(descrip, ")");
	ud_get_filename("PLAYBACK","Please Enter Record File Name:",
							"*.rp", fname, &len, descrip, 1, UU_FALSE);
	if (len<=0) return;
	ud_recstart(fname, 1); 
}

/**********************************************************************
**    I_FUNCTION :  okCB
**       Accepts the Signon form, authorizes the product, loads the
**			input file, and enters interactive NCL.
**    PARAMETERS   
**       INPUT  : 
**          widget      = Ignored.
**				client_data = Ignored.
**				call_data   = Motif callback structure.  Contains the input
**				              filename selected by the user.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : Brings up the initial interactive layout.
**    WARNINGS     : none
*********************************************************************/
static void okCB(widget, client_data, call_data)
Widget widget;
XtPointer client_data,call_data;
{
	int cam,cad,iop[2];
	UX_pathname fullname,dir,fname;
	char *filen;
	char num[80];
	void uw_mfgraphicCB();
/*
.....Get the directory path and filename entered
*/
	XmFileSelectionBoxCallbackStruct *cbs =
		(XmFileSelectionBoxCallbackStruct *)call_data;
	if (!XmStringGetLtoR(cbs->value,XmSTRING_DEFAULT_CHARSET,&filen))
		return;
/*
.....Set cursor
*/
	uw_mfsetcursor(21); 
	strcpy(fullname,filen);
/*
...if recording, write signon info to playback file
*/
	ud_wrsignon("START", NULL);
	ud_wrsignon("SELECTION", filen);

	XtFree(filen);
	ul_break_fname(fullname,dir,fname);
	iop[0] = 1; iop[1] = 0;
	ul_get_base_fname(fname,fname,iop,UX_NPRTERRS);

	if (dir[0] == '\0')
	{
		if (XmStringGetLtoR(cbs->dir,XmSTRING_DEFAULT_CHARSET,&filen))
		{
			strcpy(dir,filen);
			XtFree(filen);
		}
	}
/*
.....Authorize user
*/	
	cam = 0; cad = 0;
	if (XmToggleButtonGetState(toggle1)) cam = 1;
	sprintf(num,"%d", cam);
/*
... for recording
*/
	ud_wrsignon("CAM", num);

	if (XmToggleButtonGetState(toggle2)) cad = 1;
	sprintf(num,"%d", cad);
/*
... for recording
*/
	ud_wrsignon("CAD", num);
	uw_mfsignload(dir, fname, cam, cad);
}

/**********************************************************************
**    I_FUNCTION : uw_mfsignload(dir,fname,cam, cad) 
**       Accepts the Signon information, authorizes the product, loads the
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
void uw_mfsignload(dir, fname, cam, cad)
char *dir, *fname;
int cam, cad;
{
	int i, ocam,ocad,lathe,tl3ax, prepst,isav,nc,vmill;
	UX_pathname ppname;
	char mbuf[80],cbuf[80],*p, *ux_getenv();
	char buf[80],*pos;
	char *q, *m ,pc[80],qc[80], mc[80];
	static char ustr[]="u", udstr[]="ud", rpstr[] = "rp";
	UM_int2 idx,ival,ierr=0,batch=0,wflg=0,kfl;
	UM_int4 jval;
	UM_f77_str ppn;
	int y1;
	void uw_mfgraphicCB();
	cauth(&cam,&cad,&ocam,&ocad,&lathe,&tl3ax,&prepst,&vmill,mbuf,cbuf);
/*
.....Could not authorize
*/
	if (ocam == -1) uw_mferror(mbuf);
	else if (ocad == -1) uw_mferror(cbuf);
	else if (ocam == 0 && ocad == 0)
		uw_mferror("You must enable either CAM or CAD");
/*
.....Set CAD/CAM flags
*/
	else
	{
		UL_cam = ocam; UL_cad = ocad; UL_lathe = lathe; UL_3axis = tl3ax;
		UL_ipv = NAUTIPV;
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
			uw_mferror("Could not open Part Program Work File.");
			goto done;
		}
*/
/*
.....check if need quit because process initialized error
*/
		if (isquit ())
			goto done;
/*
.....Reset NCLIPV
*/
		ul_verify_reset();
/*
.....Initialize internal clfile
*/
		clinit();
/*
.....Initialize modals
*/
		ul_load_nis_mod();
/*
.....Initialize motion stack
*/
		ncl_mot_stack_init();
/*
.....Initialize cutter display storage
*/
		ncl_cutter_reset();
/*
.....update font
*/
	uw_glupd_font();
/*
.....Get rid of signon form
*/
	if(UW_nosignonform != 1)
	{
		XtUnmanageChild(mine);
		XtDestroyWidget(mine);
		UW_nosignonform = 1;
	}
	else
	{
		UW_nosignonform = 0;
	}
/*
...for recording
*/
		ud_wrsignon("DONE", NULL);
/*
.....Display Interface
*/
		XtMapWidget(uw_mf.prompt_app);
		XtMapWidget(uw_mf.status_app);
		y1 = UDM_layout.graphic_pos[1]+UDM_layout.graphic_size[1];
		for (i=0;i<UDM_layout.nmenu;i++)
			if (UDM_layout.menu_pos[i][1] > y1)
				XtMapWidget(uw_mflayout.menu_app[i]);
		XtMapWidget(uw_mf.graphic_app);
		uw_mfresize_graphics();
/*
..... tmp remove flush_expose 
.....Yurong 7/28/97
*/
/*#if UU_COMP != UU_HPUX
		uw_mf_xflush_expose();
#endif */
		for (i=0;i<UDM_layout.nmenu;i++)
			if (UDM_layout.menu_pos[i][1] <= y1)
				XtMapWidget(uw_mflayout.menu_app[i]);
/*
......reset command line and disable command line at first
*/
		uw_mfreset_prompt();
/*
.....Set wait cursor when loading
.....Yurong  7/28/97
*/
	uw_mfsetcursor(21); 
/*
.....Determine if part program or Unibase
*/
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
			UM_init_f77_str(ppn,ppname,UX_MAX_PATH_LEN);
			strcpy(ppname,fname);
			strcpy(buf,"Loading "); strcat(buf,ppname);
			ud_prmerr(buf);
			nc = strlen(ppname);
			for (i=strlen(ppname);i<UX_MAX_PATH_LEN;i++) ppname[i] = ' ';
/*
.....Force display of interface
.....prior to loading unibase
.....Bobby  -  3/25/98
*/
			{
				int event,xy[2];
				for (i=0;i<400;i++) uw_mfevent(&event,xy);
			}
			isav = UU_application;
			UU_application = UU_NCLCADD;
			loadu(UM_addr_of_f77_str(ppn),&nc,&batch,&ierr);
			UU_application = isav;
		}
/*
......... initialize playback file
*/
		else if (pos != UU_NULL && (strcmp(pos+1,m) == 0 || strcmp(pos+1,mc)==0)) 
		{	
			ud_signinit(fname);
		}
/*
.....Load Part Program
*/
		else
		{
			UM_init_f77_str(ppn,ppname,UX_MAX_PATH_LEN);
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
/*			for (i=strlen(ppname);i<UX_MAX_PATH_LEN;i++) ppname[i] = ' ';*/
			loadpp(UM_addr_of_f77_str(ppn),&i,&ierr,&wflg);
			ptppnm(UM_addr_of_f77_str(ppn),&i);
			ptdfnm(UM_addr_of_f77_str(ppn),&i);
			kfl = UL_create_cl;
			setclf(UM_addr_of_f77_str(ppn),&i,&kfl);
			kfl = UL_create_as;
			setas(UM_addr_of_f77_str(ppn),&i,&kfl);
		}
		znu_copyright();
		uz_status();
	}
done:;
}

/**********************************************************************
**    I_FUNCTION :  do_search (widget,sdata)
**       Creates the FileName list for the File Selection Widget using
**			the specialized file mask (file1,file2,...,filen).
**    PARAMETERS   
**       INPUT  : 
**          widget = File selection widget.
**				sdata  = Motif callback structure.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void do_search(widget, sdata)
Widget widget;
XtPointer sdata;
{
	int nfile,i;
	char *dir,*pattern;
	UX_pathname direc,file,fname;
	char *list,*listhead;
/*
.....changed to 1024 in order to avoid final errors when
.....there are too many files in one directory
.....Yurong 11/7/97
*/
/*	XmString names[256]; */
	XmString names[1024];
	XmFileSelectionBoxCallbackStruct *cbs = 
		(XmFileSelectionBoxCallbackStruct *)sdata;
/*
.....Make sure something is in the directory & filename
*/
	if (!XmStringGetLtoR(cbs->dir,XmSTRING_DEFAULT_CHARSET,&dir))
		return;
	strcpy(direc,dir);

	XtFree(dir);
	if (!XmStringGetLtoR(cbs->pattern,XmSTRING_DEFAULT_CHARSET,&pattern))
		return;
	strcpy(file,pattern);
	XtFree(pattern);
/*
.....Get list of filenames
*/
	ul_get_flist(direc,file,&list,0,(UX_NPRTERRS|UX_NCHK));
	listhead = list;
/*
.....No files in list
*/
	nfile = 0;
	if (list == NULL)
	{
		XtVaSetValues(widget,
			XmNfileListItems, NULL,
			XmNfileListItemCount, nfile,
			XmNlistUpdated, True,
			NULL);
	}
/*
.....Store files in File Selection List
*/
	else
	{
		while (ux_nxt_file(&list,fname,UX_NPRTERRS) == UU_SUCCESS)
		{
#if UU_COMP == UU_VAXVMS
			strcpy(file,fname);
			nc = strlen(file); ul_strip_blanks(file,&nc);
#else
			strcpy(file,direc); strcat(file,fname);
#endif
			names[nfile++] = XmStringCreateSimple(file);
		}
		XtVaSetValues(widget,
			XmNfileListItems, names,
			XmNfileListItemCount, nfile,
			XmNdirSpec, cbs->dir,
		XmNlistUpdated, True,
			NULL);
		if (listhead != NULL) uu_lsdel(listhead);
		for (i=0;i<nfile;i++) XmStringFree(names[i]);
	}
}

/**********************************************************************
**    I_FUNCTION :  uw_mfsignon (arg)
**       Processes the Signon form.
**    PARAMETERS   
**       INPUT  : 
**          arg    - = 1 if unauth should be called.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfsignon(arg)
int arg;
{
	Widget board,frame;
#if UU_COMP == UU_SUN
	Widget list;
#endif
	XmString labstr,labstr1;
	Arg args[20];
	Cardinal n;
	int i,idid;
	char fname[UX_MAX_PATH_LEN],direc[UX_MAX_PATH_LEN], fname1[UX_MAX_PATH_LEN];
	char *ux_getenv();
	int cam,cad,iop[2];
	char *filen,fn[UX_MAX_PATH_LEN],fullname[UX_MAX_PATH_LEN];
	char dir[UX_MAX_PATH_LEN], dir1[UX_MAX_PATH_LEN];
	int state;
	char buff[UX_MAX_PATH_LEN],*p,sufx[UX_SUFFIX_LEN];
	static int first = 1;
/*
.....LOOP
*/
	dir1[0] = '\0';
	dir[0] = '\0';
	cad = 1;
	cam = 1;
	UW_signon = 1;

/*
...accept command argment
...and put in signon variable
...Yurong
*/
	filen = fn;
	filen[0] = '\0';

	fname1[0] = '\0';	/* <---- Look Here for Fix */

	if((UU_comargc>1)&&(strcmp(UU_comargv[0],"-x")!=0)&&(first==1))
	{
		for(i=0; i<UU_comargc-1; i++)
		{
/*
.....The file name or path name may include '-', so old code
.....will cause proble when we have file name like xx-xx.u
.....changed by Yurong 8/17/98
*/
/*
			if(strchr(UU_comargv[i],'-')==NULL)
				strcpy(filen, UU_comargv[i]);
			else if((strcmp(UU_comargv[i],"-nocam")==0)||
							 (strcmp(UU_comargv[i],"-NOCAM")==0))
*/
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
			else if((strstr(UU_comargv[i],"-d")!=NULL)|| 
							(strstr(UU_comargv[i],"-D")!=NULL))
				strcpy(dir1, &(UU_comargv[i][3]));
			else if((strstr(UU_comargv[i],"-l")!=NULL)|| 
							(strstr(UU_comargv[i],"-L")!=NULL))
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
			else
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
.....Deallocate CAM and CAD terminal
*/
/*	if (arg == 1) unauth(&UL_cam,&UL_cad);*/
/*
.....if we already have a part program to be load
.....don't using signon browser
*/
	if (NCL_load_part[0]!='\0')
	{
		strcpy(fullname, NCL_load_part);
		ul_break_fname(fullname,dir,fname1);
		UW_nosignonform = 1;
		goto form_done;
	}
	if((fname1[0]!='\0')&&(first==1))
	{
		UW_nosignonform = 1;
		goto form_done;
	}
/*
.....Get Part Program suffices
*/
	p = ux_getenv("UL_PROGRAM_SUFFIX",UX_NPRTERRS);
	if (p != 0) strcpy(sufx,p);
	else strcpy(sufx,"*");

	fname[0] = '\0';
	do
	{
		strcat(fname,"*.");
		strcpy(buff,sufx);
		p = (char *) index(buff,',');
		if (p == 0) sufx[0] = '\0';
		else
		{
			strcpy(sufx,p+1);
			*p = '\0';
		}
		strcat(fname,buff);
		strcat(fname,",");
	} while (strlen(sufx) != 0);
	i = strlen(fname) ; if (i != 0) fname[i-1] = '\0';
/*
.....Store default file extensions
*/
	n = 0;
	labstr = XmStringCreateSimple(fname);
	XtSetArg(args[n],XmNdirMask,labstr); n++;
/*
.....Store default directory
*/
	if(dir[0]!='\0')
	{
		idid = 1;
		if (dir[0] == '/')
			strcpy(direc,dir);
		else
		{
				if (ul_get_full_dir("HOMEDIR",direc) == UU_SUCCESS)
				{
					strcat(direc,"/"); strcat(direc,dir);
				}
				else
					idid = 0;
		}
		if (idid == 1)
		{
			labstr1 = XmStringCreateSimple(direc);
			XtSetArg(args[n],XmNdirectory,labstr1); n++;
		}
	}
	XtSetArg(args[n],XmNfileSearchProc,do_search); n++;
	mine = (Widget)XmCreateFileSelectionDialog((Widget)uw_mf.parent,
					"signon",args,n);
	XtVaSetValues(XtParent(mine),XmNtitle,"NCL Signon",NULL);
	XmStringFree(labstr);
/*
.....let scroll bar always displayed
.....for fixed prolems on SunOS pluto 5.5.1
.....(there is no scroll bar show on the 
.....file select list
.....Yurong 11/14/97
*/
#if UU_COMP == UU_SUN
	list = (Widget)XmFileSelectionBoxGetChild(mine, XmDIALOG_LIST);
	XtVaSetValues(list, XmNscrollBarDisplayPolicy , XmSTATIC,
						NULL);
#endif


	if (idid == 1) XmStringFree(labstr1);
/*
...   If running VX, set directory, part program name & units obtained from VX.
*/
	if (NCL_vx_flag)
	{
		ncl_vx_getdir (fname);
		if (fname[0] != '/')
		{
			ul_get_full_dir ("HOMEDIR", buff);
			ul_build_full_dir (buff, fname, buff);
			strcpy (fname, buff);
		}
		i = strlen(fname);
		if (fname[i-1] != '/') fname[i++] = '/';
		ncl_vx_getppname (&fname[i]);
		labstr = XmStringCreateSimple(fname);
		XtVaSetValues(mine,XmNdirSpec,labstr,NULL);
		ncl_vx_setunits();
		XmStringFree(labstr);
	}
/*
.....Get rid of the HELP button
.....Change CANCEL to EXIT
*/
/*
.....Changed Cancel to Record
.....Changed Help to Exit
.....Yurong 2/13/97
*/
/*	XtUnmanageChild(XmFileSelectionBoxGetChild(mine,XmDIALOG_HELP_BUTTON)); */
	
	labstr = XmStringCreateSimple("Record");
	XtVaSetValues((Widget)XmFileSelectionBoxGetChild(mine,XmDIALOG_CANCEL_BUTTON),
		XmNlabelString,labstr,NULL);
	XmStringFree(labstr);
	labstr = XmStringCreateSimple("Exit");
	XtVaSetValues((Widget)XmFileSelectionBoxGetChild(mine,XmDIALOG_HELP_BUTTON),
		XmNlabelString,labstr,NULL);
	XmStringFree(labstr);
/*
.....Add Callbacks
*/
/* 
...add callback for record
...Yurong
*/	
	XtAddCallback(mine,XmNcancelCallback,recordCB,NULL);
	XtAddCallback(mine,XmNhelpCallback,exitCB,NULL);
	XtAddCallback(mine,XmNokCallback,okCB,NULL);
/*
.....Create the CAM & CADD toggles
*/
	frame = XtVaCreateWidget("rowcol",xmFrameWidgetClass,mine,
		XmNshadowType,XmSHADOW_ETCHED_IN,NULL);
	board = XtVaCreateWidget("rowcol",xmRowColumnWidgetClass,frame,
		XmNorientation,XmVERTICAL,NULL);
	toggle1 = XtVaCreateManagedWidget("CAM Terminal", xmToggleButtonWidgetClass,
		board,NULL,0);
	toggle2 = XtVaCreateManagedWidget("CAD Terminal", xmToggleButtonWidgetClass,
		board,NULL,0);
	state = False ; if ((UL_cam == 1)&&(signon_cam==1)) state = True;
	XmToggleButtonSetState(toggle1,state,False);
	state = False ; if ((UL_cad == 1)&&(signon_cad==1)) state = True;
	XmToggleButtonSetState(toggle2,state,False);
	XtManageChild(board);
	XtManageChild(frame);
	XtManageChild(mine);
form_done:;
/*
...   Load VX geometry if running under VX.
*/
	if (NCL_vx_flag) vxload();
/*
...if user put filenamme in command line
...load file and getinto it now
...Yurong
*/
	if((fname1[0]!='\0')&&(first==1))
	{
		uw_mfsignload(dir, fname1, cam, cad);
		signon_need = 1;
		first = 0;
		goto start;
	}
	if (NCL_load_part[0]!='\0')
	{
		NCL_load_part[0] = '\0';
		uw_mfsignload(dir, fname1, cam, cad);
		signon_need = 1;
		return;
	}
start:
/*
... add UD_pick to set pick mode = 0
... Yurong
*/
	UD_pickmode = 0;
/*
.....Application main loop
*/ 
	uw_open_keycom();
	uw_mfmainloop ();
	uw_close_keycom();
}

/**********************************************************************
**    I_FUNCTION :  uw_mfmainloop()
**       Main Motif loop.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfmainloop ()
{
	int stat, jmpflag;
	XEvent event;
	Gnrect ndcrect2;
	UD_GKSEVENT gks_event;
/*
.....Check for initial playback file
.....Yurong
*/
	if((UD_firsttime==UU_TRUE)&&(signon_need==0))
	{
		ud_playinit();
		UD_firsttime = UU_FALSE;
	}	
	signon_need = 0;
/*
.....Application main loop
*/
	UD_MARK(jmpflag,UU_FALSE);
#ifdef UU_OPENGL
	XtRealizeWidget(uw_mf.parent);
#endif
	for (;;)
	{
/*
.....Flush the picking matrix stack
*/
		ud_updatews(UG_SUPPRESS);
/*
.....if we close & Destroy menu design window in 
.....menu design function "ACCEPT", "CANCEL" callback
.....on same system (Maybe most because time matter)
.....the active menu we just changed and put back 
.....in the menu design function will
.....no longer active. So, we don't destroy menu design window
.....in menu design window callback function, only set 
.....a UW_close_mdsgn flag in menu design callback function
.....and destroy them here
.....This is for fix problem on IRIX64 octane 6.4
.....Yurong 4/22/99
*/
		if (UW_close_mdsgn != 0)
		{
		   XtUnrealizeWidget(menu_win);
		   XtUnrealizeWidget(desgn_win);
			XtUnrealizeWidget(funlist_win);

		   XtDestroyWidget(menu_win);
		   XtDestroyWidget(desgn_win);
		   XtDestroyWidget(funlist_win);
		   menu_win = NULL;
		   desgn_win = NULL;
		   funlist_win = NULL;
			UW_close_mdsgn = 0;
		}
/*
.....Flush output buffer
*/
		ud_updatews(UG_SUPPRESS);
/*
...if playback, we don't need wait for event
...Yurong
*/
		if (UD_Rpstate[UD_Rpstate_ptr].flag==PLAYBACK)
		{
			ud_rprd(&gks_event,UU_FALSE);
		}
		else
		{
			uw_mfsetcursor(1);  
			XtAppNextEvent(uw_mf.application,&event);
			if (event.type!=MotionNotify)
				uw_mfsetcursor(21);  
/*
......if it is a special key, we need handle it here as accelerator key
*/
			stat = uw_mfaccel_key(&event);
			if (stat==0)
				XtDispatchEvent(&event);
		}
/*
.....The following was stolen from 'ug_dawaitdev'
........Inform workstation if an xform has changed
*/
		if (ug_getredrwflag()==1)
		{
/*
...........Make a 2D copy of ug_redrw.ndcrect in ndcrect2
*/
			ug_getredrwrect2(&ndcrect2);
			uw_glupdrec(&ndcrect2);
/*
...........Reset ndcrect to null rectangle
*/
			ug_resetredrwrect();
			ug_setredrwflag(0);
		}
	}
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
	char *ux_getenv();
	switch(*fieldno)
	{
		case 1:
			ext[0] = '\0';
			strcpy(descrip, "Part Program Files (");
			p = (char*)ux_getenv("UL_PROGRAM_SUFFIX");
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
			ud_get_filename(sbuf,sbuf,ext,filename,&inum,descrip, 0, UU_FALSE);
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
									filename, &inum, descrip, 0, UU_FALSE);
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
									filename, &inum, descrip, 0, UU_FALSE);
			if (inum != 0)
				ud_update_answer(8, (int *)filename);
			break;
		case 10:
			strcpy(filename, tass);
			ud_get_filename("APT Files", "APT Files", "*.as",
						filename, &inum, "APT Source Files (*.as)", 1, UU_FALSE) ;
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
				ud_set_traverse_mask(1,UU_TRUE);
				ud_set_traverse_mask(2,UU_TRUE);
			}
			else 
			{
				ud_set_traverse_mask(1,UU_FALSE);
				ud_set_traverse_mask(2,UU_FALSE);
			}
			break;
		case 3:
			if (tub == 1)
			{
				ud_set_traverse_mask(4,UU_TRUE);
				ud_set_traverse_mask(5,UU_TRUE);
			}
			else 
			{
				ud_set_traverse_mask(4,UU_FALSE);
				ud_set_traverse_mask(5,UU_FALSE);
			}
			break;
		case 6:
			if (tcl == 1)
			{
				ud_set_traverse_mask(7,UU_TRUE);
				ud_set_traverse_mask(8,UU_TRUE);
			}
			else 
			{
				ud_set_traverse_mask(7,UU_FALSE);
				ud_set_traverse_mask(8,UU_FALSE);
			}
			break;
		case 9:
			if (tas == 1)
			{
				ud_set_traverse_mask(10,UU_TRUE);
				ud_set_traverse_mask(11,UU_TRUE);
			}
			else 
			{
				ud_set_traverse_mask(10,UU_FALSE);
				ud_set_traverse_mask(11,UU_FALSE);
			}
			break;
		case 12:
			if (tsess == 1)
			{
				ud_set_traverse_mask(13,UU_TRUE);
				ud_set_traverse_mask(14,UU_TRUE);
			}
			else 
			{
				ud_set_traverse_mask(13,UU_FALSE);
				ud_set_traverse_mask(14,UU_FALSE);
			}
			break;
		default:
			break;
	}
	return(UU_SUCCESS);
}
/*********************************************************************
**	 E_FUNCTION : uw_mfsignoff(restart)
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
void uw_mfsignoff(restart)
int restart;
{
	int ncpp,ncub,nccl,ncas,nc, status;
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
	static char traverse[]		= {1,0,0, 1,0,0, 1,0,0, 1,0,0, 1,0,0};
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
		if (uw_mfget_subncl()>0)
		{
			uw_mferror("There are still active NCL sessions.\r\nPlease close all sessions before exiting.");
			return;
		}
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
	strcpy(tpps,UL_program); strcat(tpps,"."); strcat(tpps,UL_program_suffix);
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
		traverse[9] = 1;
		traverse[6] = 1;
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
	UM_init_f77_str(ftcls,tcls,UX_MAX_PATH_LEN);
	UM_init_f77_str(ftass,tass,UX_MAX_PATH_LEN);
	getcln(UM_addr_of_f77_str(ftcls),&nc); tcls[nc] = '\0';
	getapn(UM_addr_of_f77_str(ftass),&nc); tass[nc] = '\0';
	traverse[12] = 1;
	traverse[13] = 0;
	traverse[14] = 0;
/*
.....Get the Form input
*/
	if (traverse[0] == 1 || traverse[3] == 1 || traverse[6] == 1 ||
		traverse[9] == 1)
	{
		status = ud_form1("lsignoff.frm",ans, ans, methods, 
							called, UU_NULL, traverse);
		if (status==-1)
		{
			if (NCL_subprocess==0)
				return;
			else
			{
				if (!(strcmp(NCL_keyinfo, "00000000")==0))
					return;
/*
......else forced out (even though user cancel the form
*/
			}
		}
	}	
	ncpp = strlen(tpps); ul_strip_blanks(tpps,&ncpp);
	ncub = strlen(tubs); ul_strip_blanks(tubs,&ncub);
	nccl = strlen(tcls); ul_strip_blanks(tcls,&nccl);
	ncas = strlen(tass); ul_strip_blanks(tass,&ncas);
/*
.....Save the part program file
*/
	if (tpp == 1)
	{
		UM_init_f77_str(ppn,tpps,UX_MAX_PATH_LEN);
		ul_short_filename(tpps,sbuf,60);
		sprintf(msgbuf,"Saving %s.",sbuf);
		ud_prmerr(msgbuf);
		uw_mfflush();
		savepp(UM_addr_of_f77_str(ppn),&ncpp,&wflg);
		ptppnm(UM_addr_of_f77_str(ppn),&ncpp);
		ud_killmsg(UD_ERRORMSG);
	}
/*
.....Set the clfile name
*/
	UM_init_f77_str(ppn,tcls,UX_MAX_PATH_LEN);
	ival = tcl;
	setclf(UM_addr_of_f77_str(ppn),&nccl,&ival);
/*
.....Set the APT source name
*/
	UM_init_f77_str(ppn,tass,UX_MAX_PATH_LEN);
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
		uw_mfflush();
		ur_save_part(UU_TRUE);
	}
	if ((tsess==1)&&(tssion[0]!='\0'))
	{
		ul_session_save(tssion);
	}
/*
.....Deallocate this user
*/
	unauth(&UL_cam,&UL_cad);
/***same as WinNT
	ul_ipv_terminate();
*/
/*
.....Straight Exit 
*/
	if (restart == 0 && !NCL_vx_flag)
	{
/***same as WinNT
		uz_unlight_buttons();
*/
		uw_mfterm_sub();
		ul_sysexit();
	}
/*
.....Reset the Unibase & NCL
*/
	if (restart == 0 || (restart != 0 && !NCL_vx_flag))
	{
		ud_prmerr("Resetting NCL");
		nclfin();
		uw_mfflush();
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
/*
.....Go to initial menu layout
*/
/*
...take done the menudesign forms if it is up
...Yurong added
*/
	uw_mfremove_desgn_menu();  
	if (restart != 0)
	{
		ul_ipv_terminate();
		uw_mfmenu_reset(UU_FALSE,UU_FALSE,UU_FALSE);
	}
/*
.....Restart ... Going to Signon Form
... aak oct-1998: if in shaded mode, switch it off and return
... to wireframe mode.
*/
	if (restart == 1) 
	{
		uw_mfsignon((int)1);
	}
/*
.....End of routine
*/
done:;
	return;
}
/*********************************************************************
**    I_FUNCTION : uw_mfterm(prms,reply) ------ UG_DCLOSEWS
**       Close the NCL application window.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void uw_mfterm(prms,reply)
int prms[];			/* no input parameters */
int reply[];			/* no output parameters */
{
	uu_denter(UU_GITRC,(us,"uw_xwterm."));
/*
.....Take down the layout and menus
.....when transferring back to VX
*/
	if (NCL_vx_flag)
	{
		uw_mfmenu_reset(UU_FALSE,UU_FALSE,UU_FALSE);
	}
/*
.....Close the NCL window
*/
	else
	{
		XDestroyWindow(uw_xw.disp, uw_xw.wd_id);
/*
.....Close the NCL display
*/
		XCloseDisplay (uw_xw.disp);
	}
	uu_dexit;
}

#endif
