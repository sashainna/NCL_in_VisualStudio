/*********************************************************************
**      FILENAME: lsession2.c
**      CONTAINS:
**                 ul_session_load()
**                 ul_session_import()
**     MODULE NAME AND RELEASE LEVEL 
**       lsession2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:20
*********************************************************************/

#include "usysdef.h"

#include "stdio.h"
#include "lcom.h"
#include "lumb.h"
#include "udebug.h"
#include "ulist.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "lipv.h"
#include "nclfc.h"
#include "mdunits.h"
#include "mfort.h"
#include "nclfile.h"
#include "nclver.h"
#include "nclmplay.h"
#include "nclvx.h"
#include "view.h"

#define PMAIN 0
#define PUNIB 1
#define PSECU 2
#define PLAYO 3
#define PPGMF 4
#define PCOMF 5
#define PMACR 6
#define PMOTN 7
#define PSRCC 8
#define PNIPV 9
#define PDONE 10

extern UN_motseg *mlist_first_ptr;
extern UN_motseg *mlist_ptr;
extern char UR_dpn[];
extern UX_pathname UR_exnam[];
extern UX_pathname UBopen;
extern int lub2,UB_ftype;
extern int UR_restore_mtrl,UR_restore_lights,UR_restore_clr;
extern UU_LOGICAL UR_EOF_stat,NCL_ubnew;

char *ux_getenv();

static char *Ssession={"NCL_session"};
static UX_pathname Sdir,Sclf2,Sdefault,Sprogram,Sunibase,Sunibase2;
static char Ssuffix[UX_SUFFIX_LEN],Smode[12],S2sstr[40],S2estr[40];
static UU_LOGICAL Sipv[2],Sbeof,Sbnew;
static int Slic[10],S2src,S2strt,S2tend,Sbtype;

#if (UU_COMP == UU_WIN2K)
#define DIRSEP "\\"
#else
#define DIRSEP "/"
#endif

#define FREAD(buf,nc) \
{ \
	stat = ul_fread(fd,buf,sizeof(buf),&nc); \
	if (stat == UX_EOF) goto failed; \
	if (stat != UU_SUCCESS) goto failed; \
}
static int Session_plen = 80;
static int S_import_data(),S_import_motion(),S_import_macro(),S_import_stocks();
static int S_import_srcctl();

/*******************************************************************
**   E_FUNCTION : ul_session_load()
**      This function prompts the user for a session to load and then
**      calls the appropriate functions to load the NCL session.
**   PARAMETERS  
**       INPUT  :
**          ldir   = Session directory to load when 'iload' = UU_FALSE.
**          iload  = UU_TRUE = prompt for Session directory.
**                   UU_FALSE = Session directory is provided in 'ldir'.
**       OUTPUT :  none.
**   RETURNS:    none.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
int ul_session_load(ldir,iload)
UX_pathname ldir;
UU_LOGICAL iload;
{
	int nc,stat,fstat,mode;
	char sbuf[256],title[80];
	UX_pathname lbuf,sfil,msg;
/*
.....Get the directory to load
.....the NCL session from
*/
	stat = UU_SUCCESS;
	if (iload)
	{
		strcpy(sbuf,"Load NCL session from ...");
		strcpy(title,"Load NCL Session");
		Sdir[0] = '\0'; nc = 0;
		ud_get_dirname(sbuf,title,Sdir,&nc);
		strcpy(ldir, Sdir);
	}
	else
	{
		strcpy(Sdir,ldir);
		nc = strlen(Sdir);
	}
	if (nc == 0) goto done;
	stat = UU_FAILURE;
/*
.....Make sure the directory already exists
.....and is an NCL Session directory
*/
	mode = UX_EXISTS|UX_READ;
	stat = ux_file_inquire(UU_NULL,UU_NULL,Sdir,UU_NULL,UU_NULL,&mode,
		&fstat,lbuf,UX_NPRTERRS);
/*
........Directory exists
........Check for NCL session file
*/
	if (stat == UU_SUCCESS && mode != (mode|UX_NEXISTS))
	{
		if (mode != (mode|UX_READ)) goto failed;
		ul_build_full_fname(Sdir,Ssession,"ncs",sfil);
		mode = UX_EXISTS|UX_READ;
		stat = ux_file_inquire(UU_NULL,UU_NULL,sfil,UU_NULL,UU_NULL,&mode,
			&fstat,lbuf,UX_NPRTERRS);
		if (stat != UU_SUCCESS || mode != (mode|UX_EXISTS))
			goto not_session;
	}
/*
.....Directory does not exist
*/
	else if (mode == (mode|UX_NEXISTS))
		goto failed;
/*
.....Load the main NCL session file
*/
	stat = S_import_data();
	if (stat != UU_SUCCESS)
	{
		sprintf(msg,"Could not load %s.ncs",Ssession);
		goto noload;
	}
/*
.....Restart NCL
*/
	if (iload)
	{
		udm_signoff(2);
		udm_signon_load(Sdefault,"",1,1);
	}
/*
.....Store program name
*/
	strcpy(UL_program,Sprogram);
	strcpy(UL_program_suffix,Ssuffix);
/*
.....Import the session file
*/
	stat = ul_session_import(msg);
	if (stat != UU_SUCCESS) goto noload;
/*
.....Set the default directory
*/
	ul_set_dir(Sdefault,lbuf);
	if (iload)
		nclc_save_recent_file(Sdir, 4);
	goto done;
/*
.....Not an NCL session directory
*/
not_session:;
	sprintf(lbuf,"'%s' is not an NCL session directory.",Sdir);
	if (iload) ud_wrerr(lbuf);
	stat = UU_FAILURE;
	goto done;
/*
.....Could not access directory
*/
failed:;
	sprintf(lbuf,"Could not access '%s'.",Sdir);
	if (iload) ud_wrerr(lbuf);
	stat = UU_FAILURE;
	goto done;
/*
.....Could not load session
*/
noload:;
	if (iload) ud_wrerr(msg);
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	return(stat);
}

/*******************************************************************
**   E_FUNCTION : ul_session_import(msg)
**      This function controls the input of all NCL session files.
**   PARAMETERS  
**       INPUT  :  none.
**       OUTPUT :
**         msg     = Error message text if an error occurred trying
**                   to load an NCL session file (status = UU_FAILURE).
**   RETURNS:
**         UU_SUCCESS when all files are loaded, UU_FAILURE otherwise.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
int ul_session_import(msg)
UX_pathname msg;
{
	UM_int2 ierr,itype;
	int stat,nc,jerr,jfl,idx,i,lic[10],status,frm,perc[PDONE+1],tperc,isav1;
	int isav2,isav3;
	char *pos;
	UX_pathname dir,sfil,sext;
	UM_f77_str fdir,ffil;
/*
.....Determine the time percentages
*/
	perc[PMAIN] = 5;
	perc[PUNIB] = 60;
	perc[PSECU] = 0;
	if (Sunibase2[0] != '\0')
	{
		perc[PUNIB] = 35;
		perc[PSECU] = 25;
	}
	perc[PLAYO] = 5;
	perc[PPGMF] = 10;
	perc[PCOMF] = 5;
	perc[PMACR] = 5;
	perc[PMOTN] = 5;
	perc[PSRCC] = 5;
	perc[PNIPV] = 0;
	if (Sipv[0])
	{
		if (perc[PSECU] != 0)
		{
			perc[PSECU] = perc[PSECU] - 10;
			perc[PUNIB] = perc[PUNIB] - 10;
		}
		else
			perc[PUNIB] = perc[PUNIB] - 20;
		perc[PNIPV] = 20;
	}
/*
.....Display the Progress form
*/
	ul_session_form_display(1,&frm);
/*
.....Verify the licenses
*/
	ncl_get_licenses(lic);
	for (i=0;i<7;i++)
	{
		if (Slic[i] == 1 && lic[i] == 0)
		{
/*
........CAM or CADD do not match
*/
			if (i == 0 || i == 1 && strcmp(Smode,"DRAWING") == 0)
			{
				strcpy(msg,"NCL licenses do not match.");
				goto failed;
			}
/*
........NCLIPV does not match
*/
			else if (i == 5 || i == 6)
			{
				Sipv[0] = 0;
			}
		}
	}
	tperc = perc[PMAIN];
	ul_session_form_update(frm,tperc,PMAIN);
/*
.....Load the primary Unibase
*/
	ux_getsys_ext(1,UR_exnam,sext);
	ul_build_full_fname(Sdir,Ssession,sext,sfil);
	isav1 = UR_restore_mtrl;
	UR_restore_mtrl = UU_FALSE;
	isav2 = UR_restore_lights;
	UR_restore_lights = UU_FALSE;
	isav3 = UR_restore_clr;
	UR_restore_clr = UU_FALSE;
	stat = ur_load_part_exec(sfil,UU_TRUE);
	UR_restore_mtrl = isav1;
	UR_restore_lights = isav2;
	UR_restore_clr = isav3;
	if (stat != UU_SUCCESS)
	{
		sprintf(msg,"Could not load '%s'.",sfil);
		goto failed;
	}
	pos = (char *)strrchr(Sunibase,'.');
	if (pos != UU_NULL) *pos = '\0';
	strcpy(UR_dpn,Sunibase);
	tperc = tperc + perc[PUNIB];
	ul_session_form_update(frm,tperc,PUNIB);
/*
.....Load the secondary Unibase
.....if one is defined
*/
	if (Sunibase2[0] != '\0')
	{
		ul_build_full_fname(Sdir,Ssession,sext,sfil);
		strcat(sfil,"2");
		nc = strlen(sfil);
		UM_init_f77_str(ffil,sfil,UX_MAX_PATH_LEN);
/*
........Open the Unibase
*/
		ur_op04(UM_addr_of_f77_str(ffil),&nc,&itype,&ierr);
		if (ierr != 0)
		{
			sprintf(msg,"Could not open '%s'.",sfil);
			goto failed;
		}
/*
........Reset the Unibase
*/
		ur_flush_del_stack();
		ur_getu_second();
		ncl_reset_unicad();
/*
........Load the Unibase
*/
		ub2lod(&itype,&ierr);
		if (ierr != 1)
		{
			sprintf(msg,"Could not load '%s'.",sfil);
			ur_cl04(&ierr);
			ur_getu_work();
			goto failed;
		}
/*
........Initialize Unibase lists
*/
		ncl_ublist_init();
/*
........Open the actual secondary Unibase
*/
		ux_close(lub2,UX_NPRTERRS);
		status = ur_op04_session(Sunibase2,&Sbtype,UU_FALSE);
		if (status != UU_SUCCESS)
		{
			sprintf(msg,"Could not open '%s'.",sfil);
			goto failed;
		}
		UR_EOF_stat = Sbeof;
		NCL_ubnew = Sbnew;
		ur_getu_work();
	}
	tperc = tperc + perc[PSECU];
	ul_session_form_update(frm,tperc,PSECU);
/*
.....Load the layout file
*/
	ul_build_full_fname(Sdir,Ssession,"lay",sfil);
	ud_load_layout(sfil);
	tperc = tperc + perc[PLAYO];
	ul_session_form_update(frm,tperc,PLAYO);
/*
.....Load the part program, ranfil, and clfiles
*/
	strcpy(dir,Sdir); strcat(dir,DIRSEP);
	UM_init_f77_str(fdir,dir,UX_MAX_PATH_LEN);
	nc = strlen(dir);
	jfl = Sclf2[0] == '\0' ? 0 : 1;
	nclf_load_session_file(UM_addr_of_f77_str(fdir),&nc,&jfl, &Session_plen, &jerr);
	if (jerr != 0)
	{
		sprintf(msg,"Could not load part program files.");
		goto failed;
	}
	tperc = tperc + perc[PPGMF];
	ul_session_form_update(frm,tperc,PPGMF);
/*
.....Load the Fortran commons
*/
	nclf_load_session_common(UM_addr_of_f77_str(fdir),&nc,&jerr);
	if (jerr != 0)
	{
		sprintf(msg,"Could not load common files.");
		goto failed;
	}
	tperc = tperc + perc[PCOMF];
	ul_session_form_update(frm,tperc,PCOMF);
/*
.....Load the Macro definitions
*/
	stat = S_import_macro();
	if (stat != UU_SUCCESS)
	{
		sprintf(msg,"Could not load Macro definitions.");
		goto done;
	}
	tperc = tperc + perc[PMACR];
	ul_session_form_update(frm,tperc,PMACR);
/*
.....Store the secondary clfile settings
*/
	ncl_set_clfile_src(S2src,Sclf2,S2strt,S2sstr,S2tend,S2estr);
/*
.....Load the motion display
*/
	stat = S_import_motion();
	if (stat != UU_SUCCESS)
	{
		sprintf(msg,"Could not load '%s.mot'.",Ssession);
		goto done;
	}
	tperc = tperc + perc[PMOTN];
	ul_session_form_update(frm,tperc,PMOTN);
/*
.....Load the source control
*/
	stat = S_import_srcctl();
	if (stat != UU_SUCCESS)
	{
		sprintf(msg,"Could not load '%s.acs'.",Ssession);
		goto done;
	}
	tperc = tperc + perc[PSRCC];
	ul_session_form_update(frm,tperc,PSRCC);
/*
.....Load NCLIPV
*/
	if (Sipv[0])
	{
		ul_build_full_fname(Sdir,Ssession,"ipv",sfil);
		stat = ul_ipv_load_session(sfil,Sipv[1]);
		if (stat != UU_SUCCESS)
		{
			sprintf(msg,"Could not load NCLIPV session.");
			goto failed;
		}
	}
	else
	{
		stat = S_import_stocks();
		if (stat != UU_SUCCESS)
		{
			sprintf(msg,"Could not load NCLIPV stocks.");
			goto failed;
		}
	}
	tperc = tperc + perc[PNIPV];
	ul_session_form_update(frm,tperc,PNIPV);
/*
.....Set Drawing mode if active
*/
	if (strcmp(Smode,"DRAWING") == 0)
	{
		UM_2d3d_mode = UM_2D;
	}
	goto done;
/*
.....Load session procedure failed
.....Reset NCL session so that it
.....is in a usable state
*/
failed:;
	stat = UU_FAILURE;
	UN_clnum[0] = 0;
	idx = 1; jfl = 0; setjfl(&idx,&jfl);
	ur_unibase_not_used();
	udm_signoff(2);
	goto done;
/*
.....End of routine
*/
done:;
	ul_session_form_close(frm);
	return(stat);
}

/*******************************************************************
**   I_FUNCTION : S_import_data()
**      This function loads the main NCL session data file.
**   PARAMETERS  
**       INPUT  :  none.
**       OUTPUT :  none.
**   RETURNS:
**         UU_SUCCESS when file is saved successfully,
**         UU_FAILURE otherwise.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
static int S_import_data()
{
	int stat,nc;
	UU_REAL ver;
	char stuff[12];
	UX_pathname sfil,sbuf;
	FILE *fd;
	UM_int2 indx, plen;
/*
.....Open the NCL session file
*/
	ul_build_full_fname(Sdir,Ssession,"ncs",sfil);
	stat = ux_fopen0(sfil,"r",&fd);
	if (stat != UU_SUCCESS) goto done;
/*
.....NCL version number
*/
	stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto done;
	sscanf(sbuf,"%s %lf",stuff,&ver);
/*
.....if there is a version difference, then all fortran common array may different,
.....so don't allow load in different version session files. allow for reading difference 
	if (NCL_version > ver+.75)
*/
	if (!((NCL_version > ver - 0.0001) && (NCL_version < ver + 0.0001)))
	{
		ud_winerror("The session files are not the currect version.");  
		stat = UU_FAILURE;
		goto done;
	}
/*
.....Licenses
*/
	stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto done;
	sscanf(sbuf,"%d %d %d %d %d %d %d",&Slic[0],&Slic[1],&Slic[2],&Slic[3],
		&Slic[4],&Slic[5],&Slic[6]);
/*
.....Default directory
*/
	stat = ul_fread(fd,Sdefault,sizeof(Sdefault),&nc);
	if (stat != UU_SUCCESS) goto done;
/*
.....Part program name
*/
	stat = ul_fread(fd,Sprogram,sizeof(Sprogram),&nc);
	if (stat != UU_SUCCESS) goto done;
	stat = ul_fread(fd,Ssuffix,sizeof(Ssuffix),&nc);
	if (stat != UU_SUCCESS) goto done;
/*
.....Unibase name
*/
	stat = ul_fread(fd,Sunibase,sizeof(Sunibase),&nc);
	if (stat != UU_SUCCESS) goto done;
/*
.....Secondary Unibase name
*/
	stat = ul_fread(fd,Sunibase2,sizeof(Sunibase2),&nc);
	if (stat != UU_SUCCESS) goto done;
	stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto done;
	sscanf(sbuf,"%d %d %d",&Sbeof,&Sbnew,&Sbtype);
/*
.....Secondary clfile
*/
	stat = ul_fread(fd,Sclf2,sizeof(Sclf2),&nc);
	if (stat != UU_SUCCESS) goto done;
	stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto done;
	sscanf(sbuf,"%d %d %d",&S2src,&S2strt,&S2tend);
	stat = ul_fread(fd,S2sstr,sizeof(S2sstr),&nc);
	if (stat != UU_SUCCESS) goto done;
	stat = ul_fread(fd,S2estr,sizeof(S2estr),&nc);
	if (stat != UU_SUCCESS) goto done;
/*
.....3d (NCLCAM) or 2d (DRAWING) mode
*/
	stat = ul_fread(fd,Smode,sizeof(Smode),&nc);
	if (stat != UU_SUCCESS) goto done;
/*
.....NCLIPV
*/
	stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto done;
	sscanf(sbuf,"%s %d %d",stuff,&Sipv[0],&Sipv[1]);
/*
......get the maxinum line length  allowed in session
......we needed when loading session PP file
*/
	if (ver>9.64)
	{
/*
....read the new line length
*/
		stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
		if (stat != UU_SUCCESS) goto done;
		sscanf(sbuf,"%d", &Session_plen);
/*
......see if the old line length is the same as current one
......using the bigger one as the line length
*/
		if (UL_line_len<Session_plen)
			UL_line_len = Session_plen;
		indx = 106;
		plen = UL_line_len;
		setifl(&indx,&plen);
	}
/*
.....Close the open file
*/
	ux_fclose0(fd);
/*
.....End of routine
*/
done:;
	return(stat);
}

/*******************************************************************
**   I_FUNCTION : S_import_motion()
**      This function loads the motion display from an NCL session file.
**   PARAMETERS  
**       INPUT  :  none.
**       OUTPUT :  none.
**   RETURNS:
**         UU_SUCCESS when file is saved successfully,
**         UU_FAILURE otherwise.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
static int S_import_motion()
{
	int stat,nc,i,j,nview,inc,np,aptr[5],cptr[3],pnc;
	UU_LOGICAL first;
	char sbuf[256],styp[10],tbuf[256];
	UM_coord pts;
	UM_vector vcs;
	UX_pathname sfil;
	UU_KEY_ID view[20];
	UV_vport vport;
	UN_motseg mpt;
	UN_motseg_attr attr;
	UN_mot_attr mattr;
	UN_motseg_isn isn;
	UN_motseg_blade blade;
	UN_motseg_cutter_struc cutseg;
	UN_motseg_view mview;
	UN_motseg_cutparm cparm;
	UN_motseg_cutattr cattr;
	UN_motseg_symbol csym;
	UN_motseg_symgeo symgeo;
	FILE *fd;
/*
.....Open the NCL session file
.....for motion display
*/
	ul_build_full_fname(Sdir,Ssession,"mot",sfil);
	stat = ux_fopen0(sfil,"r",&fd);
	if (stat != UU_SUCCESS) goto done;
/*
....Get the active views
*/
	nview = UV_act_screen[0].nvports;
	for (i=0;i<nview;i++)
	{
		uv_getvpid(UV_act_screen[0].vports[i],&vport);
		view[i] = vport.cur_view;
	}
/*
.....Load Motion Attributes
*/
	FREAD(sbuf,nc);
	sscanf(sbuf,"%s %d",styp,&np);
	if (strcmp(styp,"MA") != 0) goto failed;
	for (i=0;i<np;i++)
	{
		FREAD(sbuf,nc);
		sscanf(sbuf,"%d %d %d",&attr.color,&attr.lnstyle,&attr.pen);
		ncl_motattr_store(&attr);
	}
/*
.....Load ISN list
*/
	FREAD(sbuf,nc);
	sscanf(sbuf,"%s %d",styp,&np);
	if (strcmp(styp,"SN") != 0) goto failed;
	isn.line = UU_NULL;
	pnc = 0;
	for (i=0;i<np;i++)
	{
		FREAD(sbuf,nc);
		ncl_simulate_parse(sbuf,1,tbuf);
		ul_to_number(tbuf,&isn.nent);
		if (isn.nent > pnc)
		{
			if (isn.line != UU_NULL) uu_free(isn.line);
			isn.line = (int *)uu_malloc(sizeof(int)*isn.nent);
			pnc = isn.nent;
		}
		for (j=0;j<isn.nent;j++)
		{
			ncl_simulate_parse(sbuf,1,tbuf);
			ul_to_number(tbuf,&isn.line[j]);
		}
		ncl_motisn_set(&isn,UU_FALSE);
	}
	if (isn.line != UU_NULL) uu_free(isn.line);
/*
.....Load Motion Cutting Attributes
*/
	FREAD(sbuf,nc);
	sscanf(sbuf,"%s %d",styp,&np);
	if (strcmp(styp,"MC") != 0) goto failed;
	mattr.tlno = 0;
	for (i=0;i<np;i++)
	{
		FREAD(sbuf,nc);
		sscanf(sbuf,"%d %lf %d %lf %d %d %d",&mattr.loadtl,&mattr.tlen,
			&mattr.sp_mode,&mattr.sp_val,&mattr.coolnt,&mattr.cc_mode,
			&mattr.cc_dir);
		ncl_motmattr_store(&mattr);
	}
/*
.....Load Motion Views
*/
	FREAD(sbuf,nc);
	sscanf(sbuf,"%s %d",styp,&np);
	if (strcmp(styp,"MV") != 0) goto failed;
	for (i=0;i<np;i++)
	{
		FREAD(sbuf,nc);
		sscanf(sbuf,
			"%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
			&mview.nview,
			&mview.vinc[0],&mview.vinc[1],&mview.vinc[2],&mview.vinc[3],
			&mview.vinc[4],&mview.vinc[5],&mview.vinc[6],&mview.vinc[7],
			&mview.vinc[8],&mview.vinc[9],&mview.vinc[10],&mview.vinc[11],
			&mview.vinc[12],&mview.vinc[13],&mview.vinc[14],&mview.vinc[15],
			&mview.vinc[16],&mview.vinc[17],&mview.vinc[18],&mview.vinc[19]);
		for (j=0;j<mview.nview;j++) mview.view[j] = view[mview.vinc[j]-1];
		ncl_motview_store(&mview);
	}
/*
.....Load Blade Directions
*/
	FREAD(sbuf,nc);
	sscanf(sbuf,"%s %d",styp,&np);
	if (strcmp(styp,"BL") != 0) goto failed;
	for (i=0;i<np;i++)
	{
		FREAD(sbuf,nc);
		sscanf(sbuf,"%lf %lf %lf",&blade.tfwd[0],&blade.tfwd[1],&blade.tfwd[2]);
		ncl_motblade_store(&attr);
	}
/*
.....Load Cutter Structures
*/
	FREAD(sbuf,nc);
	sscanf(sbuf,"%s %d",styp,&np);
	if (strcmp(styp,"NC") != 0) goto failed;
	for (i=0;i<np;i++)
	{
		FREAD(sbuf,nc);
		sscanf(sbuf,"%d %d %d %d %d",&cutseg.cutter,&cutseg.cattr,
			&cutseg.cutsym,&cutseg.shank,&cutseg.holder);
		ncl_cutter_store_struc(&cutseg);
	}
/*
.....Load Cutter Parameters
*/
	FREAD(sbuf,nc);
	sscanf(sbuf,"%s %d",styp,&np);
	if (strcmp(styp,"CU") != 0) goto failed;
	for (i=0;i<np;i++)
	{
		FREAD(sbuf,nc);
		sscanf(sbuf,"%lf %lf %lf %lf %lf",&cparm.parms[0],&cparm.parms[1],
			&cparm.parms[2],&cparm.parms[3],&cparm.parms[4]);
		FREAD(sbuf,nc);
		sscanf(sbuf,"%lf %lf %lf %lf",&cparm.parms[5],&cparm.parms[6],
			&cparm.parms[7],&cparm.parms[8]);
		ncl_cutter_store_parms(&cparm);
	}
/*
.....Load Cutter Attributes
*/
	FREAD(sbuf,nc);
	sscanf(sbuf,"%s %d",styp,&np);
	if (strcmp(styp,"CA") != 0) goto failed;
	for (i=0;i<np;i++)
	{
		FREAD(sbuf,nc);
		sscanf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",&cattr.mov,
			&cattr.segfl,&cattr.shaded[0],&cattr.shaded[1],&cattr.shaded[2],
			&cattr.color[0],&cattr.color[1],&cattr.color[2],&cattr.pen[0],
			&cattr.pen[1],&cattr.pen[2],&cattr.pen[3],&cattr.trans[0],
			&cattr.trans[1],&cattr.trans[2]);
		ncl_cutter_store_attr(&cattr);
	}
/*
.....Load Cutter Symbol Geometry
*/
	FREAD(sbuf,nc);
	sscanf(sbuf,"%s %d",styp,&np);
	if (strcmp(styp,"GM") != 0) goto failed;
	for (i=0;i<np;i++)
	{
		FREAD(sbuf,nc);
		sscanf(sbuf,"%d %d %d %d %d %lf %lf %lf",&symgeo.key,&symgeo.segno,
			&symgeo.type,&symgeo.shaded,&inc,&symgeo.axis[0],&symgeo.axis[1],
			&symgeo.axis[2]);
		FREAD(symgeo.symbol,nc);
		uu_list_init(&symgeo.curve,sizeof(UM_coord),inc,inc);
		uu_list_init(&symgeo.cnorm,sizeof(UM_vector),inc,inc);
		for (j=0;j<inc;j++)
		{
			FREAD(sbuf,nc);
			sscanf(sbuf,"%lf %lf %lf %lf %lf %lf",&pts[0],&pts[1],&pts[2],
				&vcs[0],&vcs[1],&vcs[2]);
			uu_list_push(&symgeo.curve,pts);
			uu_list_push(&symgeo.cnorm,vcs);
		}
		ncl_cutter_store_symgeo(&symgeo);
	}
/*
.....Load Cutter Symbols
*/
	FREAD(sbuf,nc);
	sscanf(sbuf,"%s %d",styp,&np);
	if (strcmp(styp,"SY") != 0) goto failed;
	for (i=0;i<np;i++)
	{
		FREAD(sbuf,nc);
		sscanf(sbuf,"%lf %lf %lf %lf %lf %lf %d",&csym.parms[0],&csym.parms[1],
			&csym.parms[2],&csym.parms[3],&csym.zlim[0],&csym.zlim[1],
			&csym.geoptr);
		ncl_cutter_store_symbol(&csym);
	}
/*
.....Store active structure pointers
*/
	FREAD(sbuf,nc);
	sscanf(sbuf,"%s %d %d %d %d %d %d %d %d",styp,&aptr[0],&aptr[1],&aptr[2],
		&aptr[3],&aptr[4],&cptr[0],&cptr[1],&cptr[2]);
	ncl_motattr_set_ptrs(aptr);
	ncl_cutter_set_ptrs(cptr);
/*
.....Initialize the motion display structure
*/
	mpt.tend.x = 0; mpt.tend.y = 0; mpt.tend.z = 0;
	mpt.taxis.x = 0; mpt.taxis.y = 0; mpt.taxis.z = 0;
	mpt.fr_mode = 0; mpt.fr_val = 0;
	mpt.attr = 0; mpt.cutter = 0; mpt.view = 0; mpt.blade = -1;
	mpt.isn = -1; mpt.mattr = -1;
/*
.....Loop through the motion display file
*/
	first = UU_TRUE;
	do
	{
/*
........Read the record and
........Get the record type
*/
		stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
		if (stat == UX_EOF) break;
		if (stat != UU_SUCCESS) goto failed;
		sscanf(sbuf,"%s",styp);
/*
........Tool End Point
*/
		if (strcmp(styp,"TE") == 0)
		{
			if (!first)
			{
				stat = ncl_store_mdisplay(&mpt);
				if (stat != UU_SUCCESS) goto done;
			}
			first = UU_FALSE;
			sscanf(sbuf,"%s %lf %lf %lf",styp,&(mpt.tend.x),&(mpt.tend.y),
				&(mpt.tend.z));
		}
/*
........Tool Axis
*/
		else if (strcmp(styp,"TA") == 0)
			sscanf(sbuf,"%s %lf %lf %lf",styp,&(mpt.taxis.x),&(mpt.taxis.y),
				&(mpt.taxis.z));
/*
........Feedrate
*/
		else if (strcmp(styp,"FR") == 0)
			sscanf(sbuf,"%s %d %lf",styp,&(mpt.fr_mode),&(mpt.fr_val));
/*
........Motion Attributes
*/
		else if (strcmp(styp,"PM") == 0)
			sscanf(sbuf,"%s %d %d %d %d %d %d", styp,&(mpt.attr),&(mpt.cutter),
				&(mpt.view),&(mpt.blade),&(mpt.isn),&(mpt.mattr));
	} while (stat == UU_SUCCESS);
/*
.....Save the last motion
*/
	if (!first)
	{
		stat = ncl_store_mdisplay(&mpt);
		if (stat != UU_SUCCESS) goto done;
	}
/*
.....Display motion
*/
	ncl_display_motion(-1,UU_NULL,0,0,UU_TRUE,UU_FALSE,UU_NULL);
	stat = UU_SUCCESS;
	goto done;
/*
.....Error reading from file
*/
failed:;
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
.....Close the open file
*/
done:;
	ux_fclose0(fd);
	return(stat);
}

/*******************************************************************
**   I_FUNCTION : S_import_macro()
**      This function loads the Macro list from an NCL session file.
**   PARAMETERS  
**       INPUT  :  none.
**       OUTPUT :  none.
**   RETURNS:
**         UU_SUCCESS when file is loaded successfully,
**         UU_FAILURE otherwise.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
static int S_import_macro()
{
	UM_int2 ipg,iel,ipgo,ielo,nwds;
	UM_int4 isub;
	UU_KEY_ID key;
	int stat,i,j,nc,svpt[4][2],nmac,nent,nif,*p;
	char sbuf[256],macnam[NCL_MAX_LABEL+1];
	char *ncl_get_macptr();
	UX_pathname sfil;
	struct NCL_macro_list_data p1,*p3[4],*p4;
	struct NCL_macro_call_data p2,*mpt;
	struct NCL_macro_define_data p5;
	UU_LIST list;
	FILE *fd;
	UM_f77_str flab;
	union {UU_REAL rval; int ival[2];} tval;
/*
.....Initialize routine
*/
	nmac = 0;
/*
.....Open the NCL session file
.....for Macro lists
*/
	ul_build_full_fname(Sdir,Ssession,"mac",sfil);
	stat = ux_fopen0(sfil,"r",&fd);
	if (stat != UU_SUCCESS) goto done;
/*
.....Get the number of Macros
*/
	stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto failed;
	sscanf(sbuf,"%d",&nmac);
	if (nmac != 0)
	{
		uu_list_init(&list,sizeof(struct NCL_macro_call_data),nmac,nmac);
		mpt = (struct NCL_macro_call_data *)UU_LIST_ARRAY(&list);
		UM_init_f77_str(flab,macnam,NCL_MAX_LABEL+1);
/*
.....Get the next defined Macro
*/
		for (i=0;i<nmac;i++)
		{
			stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
			if (stat != UU_SUCCESS) goto failed;
			sscanf(sbuf,"%s %hd %hd %hd %hd %hd",macnam,&ipg,&iel,&ipgo,&ielo,
				&nwds);
/*
........Store the Macro
*/
			mclini();
			isub = 0;
			key = 0;
			ncl_store_name(macnam,isub,ipgo,ielo,nwds,NCLI_MACRO,isub,&key);
			strcpy(mpt[i].macnam,macnam);
			mpt[i].macptr = ncl_get_macptr();
/*
.....Get MAcro definition structur first
*/
			stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
			if (stat != UU_SUCCESS) goto failed;
			if (strcmp(sbuf,"~EOM") == 0) break;
			sscanf(sbuf,"%d %d %hd %d %d %d %hd %hd %hd %d",&(p5.outflag),
				&(p5.dispflag),&(p5.rvsaved),&(p5.maclin),&(p5.next),&(p5.termln),
				&(p5.mode),&(p5.pr_rec),&(p5.pr_ele),&(p5.callin));
			stat = ul_fread(fd,p5.classname,sizeof(p5.classname),&nc);
			stat = ul_fread(fd,p5.prompt,sizeof(p5.prompt),&nc);
			ncl_store_macinfo(&p5);
/*
.....Get the Macro Argument list
*/
			do
			{
				stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto failed;
				if (strcmp(sbuf,"~EOM") == 0) break;
/*
........Store the Macro Argument list
*/
				sscanf(sbuf,"%lf %lf %ld %ld %lf %lf %hd",&(p1.dvalue),&(p1.avalue),
					&tval.ival[0],&tval.ival[1],&(p1.max),&(p1.min), &(p1.rtype));
				p1.rvalue = tval.rval;
				stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto failed;
				sscanf(sbuf,"%hd %d %d %hd %hd %d %hd %hd %d",&(p1.iflg),&(p1.psub),
					&(p1.dsub),&(p1.dclas),&(p1.detype),&(p1.asub),&(p1.iclas),
					&(p1.ietype),&(p1.keyscl));
				stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto failed;
				sscanf(sbuf,"%hd %d %hd %hd %hd %d %d",&(p1.pclas),&(p1.count),
					&(p1.geomflag),&(p1.prsvalue),&(p1.lenvalue),&(p1.substr),&(p1.pflag));
				stat = ul_fread(fd,p1.plab,sizeof(p1.plab),&nc);
				stat = ul_fread(fd,p1.dlab,sizeof(p1.dlab),&nc);
				stat = ul_fread(fd,p1.alab,sizeof(p1.alab),&nc);
				stat = ul_fread(fd,p1.prompt,sizeof(p1.prompt),&nc);
				stat = ul_fread(fd,p1.wrdlist,sizeof(p1.wrdlist),&nc);
				stat = ncl_store_maclist_data(&p1);
				if (stat != UU_SUCCESS) goto failed;
/*
........Store the Macro Argument
*/
				ipg = iel = nwds = 0;
				ncl_store_name(p1.plab,p1.psub,ipg,iel,nwds,NCLI_MPARM,nwds,&key);
			} while (stat == UU_SUCCESS);
		}
	}
/*
.....Get the next Macro on the call list
*/
	do
	{
		stat = ul_fread(fd,p2.macnam,sizeof(p2.macnam),&nc);
		if (stat != UU_SUCCESS) goto failed;
		if (strcmp(p2.macnam,"~EOC") == 0) break;
/*
........Get the Macro list pointer
*/
		for (i=0;i<nmac;i++)
		{
			if (strcmp(p2.macnam,mpt[i].macnam) == 0)
			{
				p2.macptr = mpt[i].macptr;
				break;
			}
		}
/*
........Store the Macro Call list
*/
		ncl_store_macall_data(&p2);
	} while (strcmp(p2.macnam,"~EOC") != 0);
/*
.....Get the Macro pointers
*/
	stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto failed;
	sscanf(sbuf,"%d %d %d %d %d %d %d %d",&svpt[0][0],&svpt[0][1],
		&svpt[1][0],&svpt[1][1],&svpt[2][0],&svpt[2][1],&svpt[3][0],&svpt[3][1]);
/*
........Match up the Macro pointers
*/
	if (nmac != 0)
	{
		p3[0] = p3[1] = p3[2] = p3[3] = UU_NULL;
		for (j=0;j<4;j++)
		{
			if (svpt[j][0] != -1)
			{
				p4 = (struct NCL_macro_list_data *)mpt[svpt[j][0]].macptr;
				ncl_set_macptr(p4);
				for (i=0;i<svpt[i][1];i++) 
					ncl_get_next_maclist(&p4, i+1);
				p3[j] = p4;
			}
		}
/*
........Store the Macro pointers
*/
		ncl_set_maclist_ptrs(p3);
	}
	if (nmac != 0) uu_list_free(&list);
/*
.....Get the IF-THEN-ELSE structures
*/
	stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto failed;
	sscanf(sbuf,"%d",&nif);
	for (i=0;i<nif;i++)
	{
		stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
		if (stat != UU_SUCCESS) goto failed;
		sscanf(sbuf,"%d",&nent);
		if (nent > 0)
		{
			p = (int *)uu_malloc((nent+10)*sizeof(int));
			if (p == UU_NULL) goto failed;
			for (j=0;j<nent;j=j+10)
			{
				stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto failed;
				sscanf(sbuf,"%d %d %d %d %d %d %d %d %d %d",&p[j],&p[j+1],&p[j+2],
					&p[j+3],&p[j+4],&p[j+5],&p[j+6],&p[j+7],&p[j+8],&p[j+9]);
			}
		}
		ncl_ifset_arrays(1,p,nent);
		if (nent > 0) uu_free(p);
	}

	stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto failed;
	sscanf(sbuf,"%d",&nent);
	if (nent > 0)
	{
		p = (int *)uu_malloc((nent+10)*sizeof(int));
		if (p == UU_NULL) goto failed;
		for (j=0;j<nent;j=j+10)
		{
			stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
			if (stat != UU_SUCCESS) goto failed;
			sscanf(sbuf,"%d %d %d %d %d %d %d %d %d %d",&p[j],&p[j+1],&p[j+2],
				&p[j+3],&p[j+4],&p[j+5],&p[j+6],&p[j+7],&p[j+8],&p[j+9]);
		}
	}
	ncl_ifset_arrays(2,p,nent);
	if (nent > 0) uu_free(p);
/*
.....Get the number of Labels
*/
	stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto failed;
	sscanf(sbuf,"%d",&nmac);
	if (nmac != 0)
	{
		UM_init_f77_str(flab,macnam,NCL_MAX_LABEL+1);
/*
.....Get the next defined Label
*/
		for (i=0;i<nmac;i++)
		{
			stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
			if (stat != UU_SUCCESS) goto failed;
			sscanf(sbuf,"%s %d %hd %hd",macnam,&key,&ipg,&iel);
/*
........Store the Label
*/
/*			if (ul_to_number(macnam,&nc) == UU_SUCCESS) isub = ' ';
			else*/ isub = 0;
			nwds = 0;
			ncl_store_name(macnam,isub,ipg,iel,nwds,13,isub,&key);
		}
	}
/*
.....Error writing to file
*/
	goto done;
failed:;
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
.....Close the open file
*/
done:;
	ux_fclose0(fd);
	return(stat);
}
/*******************************************************************
**   I_FUNCTION : S_import_stocks()
**      This function loads the stock and fixture definitions in a
**      NCL session file when NCLIPV is not active.
**   PARAMETERS  
**       INPUT  :  none.
**       OUTPUT :  none.
**   RETURNS:
**         UU_SUCCESS when file is saved successfully,
**         UU_FAILURE otherwise.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
static int S_import_stocks()
{
	int stat,i,j,k,nc,idn[2];
	char sbuf[100];
	LW_stock_struc *sd,*sptr;
	UX_pathname sfil;
	FILE *fd;
/*
.....Open the NCL session file
.....for stock commands
*/
	ul_build_full_fname(Sdir,Ssession,"stk",sfil);
	stat = ux_fopen0(sfil,"r",&fd);
	if (stat != UU_SUCCESS) goto done;
/*
.....Load stock commands
*/
	idn[0] = idn[1] = 0;
	ul_ipv_load_stock_cmds(fd,UU_TRUE,idn,UU_NULL,UU_NULL,UU_TRUE);
/*
.....Open the NCL session file
.....for stock attributes
*/
	ux_fclose0(fd);
	if (LW_nstock[0]+LW_nstock[1] > 0)
	{
		ul_build_full_fname(Sdir,Ssession,"ska",sfil);
		stat = ux_fopen0(sfil,"r",&fd);
		if (stat != UU_SUCCESS) goto done;
/*
.....Save stock attributes
*/
		for (j=0;j<2;j++)
		{
			sd = LW_stock_first[j];
			for (i=0;i<LW_nstock[j];i++)
			{
				ul_fread(fd,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto failed;
				sscanf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d %d %d %lf",&sd->id,
					&sd->color,&sd->translucency,&sd->units,&sd->mxflag,&sd->mxchg,
					&sd->axes,&sd->axes_color,&sd->visible,&sd->active,&sd->bin,
					&sd->edge,&sd->edge_color,&sd->toler);
				if (sd->mxflag)
				{
					ul_fread(fd,sd->mxname,sizeof(sd->mxname),&nc);
					if (stat != UU_SUCCESS) goto failed;
					ul_fread(fd,sbuf,sizeof(sbuf),&nc);
					if (stat != UU_SUCCESS) goto failed;
					sscanf(sbuf,"%lf %lf %lf %lf %lf %lf",&sd->matrix[0][0],
						&sd->matrix[0][1],&sd->matrix[0][2],&sd->matrix[1][0],
						&sd->matrix[1][1],&sd->matrix[1][2]);
					ul_fread(fd,sbuf,sizeof(sbuf),&nc);
					if (stat != UU_SUCCESS) goto failed;
					sscanf(sbuf,"%lf %lf %lf %lf %lf %lf",&sd->matrix[2][0],
						&sd->matrix[2][1],&sd->matrix[2][2],&sd->matrix[3][0],
						&sd->matrix[3][1],&sd->matrix[3][2]);
				}
/*
........Store attributes in composite component stocks
*/
				if (sd->type == LW_STOCK_COMPOS)
				{
					sptr = (LW_stock_struc *)sd->data;
					for (k=0;k<sd->bin;k++)
					{
						sptr->id = 0;
						sptr->color = sd->color;
						sptr->translucency = sd->translucency;
						sptr->units = sd->units;
						sptr->mxflag = sd->mxflag;
						sptr->mxchg = sd->mxchg;
						sptr->axes = 0;
						sptr->axes_color = sd->axes_color;
						sptr->visible = sd->visible;
						sptr->active = sd->active;
						sptr->edge = sd->edge;
						sptr->edge_color = sd->edge_color;
						sptr->toler = sd->toler;
						if (sd->mxflag)
						{
							strcpy(sptr->mxname,sd->mxname);
							um_tftotf(sd->matrix,sptr->matrix);
						}
					}
				}
				sd = (LW_stock_struc *)uu_lsnext(sd);
			}
		}
	}
/*
.....Error reading from file
*/
	goto done;
failed:;
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
.....Close the open file
*/
done:;
	ux_fclose0(fd);
	return(stat);
}

/*******************************************************************
**   I_FUNCTION : S_import_srcctl()
**      This function creates the Source Control key association list.
**   PARAMETERS  
**       INPUT  :  none.
**       OUTPUT :  none.
**   RETURNS:
**         UU_SUCCESS when file is saved successfully,
**         UU_FAILURE otherwise.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
static int S_import_srcctl()
{
	int stat,*data,nc,i,memsiz,nent;
	UU_KEY_ID key;
	char *tok,*strtok();
	UX_pathname sfil,sbuf;
	FILE *fd;
/*
.....Initialize routine
*/
	fd = UU_NULL;
	data = UU_NULL;
/*
.....Open the NCL session file
.....for Macro lists
*/
	ul_build_full_fname(Sdir,Ssession,"acs",sfil);
	stat = ux_fopen0(sfil,"r",&fd);
	if (stat != UU_SUCCESS)
	{
		stat = UU_SUCCESS;
		goto done;
	}
/*
.....Allocate memory for line entries
*/
	memsiz = 10;
	data = (int *)uu_malloc(sizeof(int)*memsiz);
	if (data == UU_NULL) goto failed;
/*
.....Get and store source control entries
*/
	do
	{
		stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
		if (stat == UX_EOF) break;
		if (stat != UU_SUCCESS) goto failed;
/*
........Get key and number of line entries
*/
		sscanf(sbuf,"%d %d",&key,&nent);
/*
........Allocate memory for line entries
*/
		if (nent > memsiz)
		{
			uu_free(data);
			memsiz = nent;
			data = (int *)uu_malloc(sizeof(int)*memsiz);
			if (data == UU_NULL) goto failed;
		}
/*
........Get line entries
*/
		stat = ul_fread(fd,sbuf,sizeof(sbuf),&nc);
		if (stat != UU_SUCCESS) goto failed;
		tok = strtok(sbuf," ");
		if (tok == UU_NULL) goto failed;
		for (i=0;i<nent;i++)
		{
			sscanf(tok,"%d",&data[i]);
			tok = strtok(NULL," ");
			if (tok == NULL && i != nent-1) goto failed;
		}
/*
........Store source line entries
*/
		ncl_srcctl_store(key,nent,data);
	} while (stat == UU_SUCCESS);
/*
.....Close the open file
*/
	stat = UU_SUCCESS;
	goto done;
/*
.....Failed to read file
*/
failed:;
	stat = UU_FAILURE;
/*
.....End of routine
*/
done:;
	if (fd != UU_NULL) ux_fclose0(fd);
	if (data != UU_NULL) uu_free(data);
	return(stat);
}
