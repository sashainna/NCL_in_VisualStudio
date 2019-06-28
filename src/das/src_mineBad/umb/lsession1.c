/*********************************************************************
**      FILENAME: lsession1.c
**      CONTAINS:
**                 ul_session_active()
**                 ul_session_save()
**                 ul_session_create()
**                 ul_session_form_display();
**                 ul_session_form_update();
**                 ul_session_form_close();
**                 ul_new_session();
**                 ul_chkkey_common();
**     MODULE NAME AND RELEASE LEVEL 
**       lsession1.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       01/04/16 , 09:04:07
*********************************************************************/

#include "usysdef.h"

#include "stdio.h"
#include "lcom.h"
#include "lumb.h"
#include "udebug.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "lipv.h"
#include "nclfc.h"
#include "mdunits.h"
#include "mfort.h"
#include "nclver.h"
#include "nclmplay.h"
#include "nclvx.h"
#include "udfconst.h"
#include "ulist.h"
#include "view.h"
#include "gtbl.h"
#include "gdidd.h"

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
extern UU_LOGICAL UR_EOF_stat,NCL_ubnew;

char *ux_getenv();
void ul_session_form_display();
void ul_session_form_update();
void ul_session_form_close();

static Sactive = UU_FALSE;
static char *Ssession={"NCL_session"};
static UX_pathname Sdir;

static int S_create_data(),S_create_motion(),S_create_stocks(),S_create_macro();
static int S_create_srcctl();

#if (UU_COMP == UU_WIN2K)
#define DIRSEP "\\"
#else
#define DIRSEP "/"
#endif

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

/*******************************************************************
**   E_FUNCTION : ul_session_active()
**      This function determines if a session is currently being saved.
**   PARAMETERS  
**       INPUT  :  none.
**       OUTPUT :  none.
**   RETURNS:    UU_TRUE if a session is currently active.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
UU_LOGICAL ul_session_active()
{
	return(Sactive);
}

/*******************************************************************
**   E_FUNCTION : ul_session_save()
**      This function prompts the user for a session to save and then
**      calls the appropriate functions to save the NCL session.
**   PARAMETERS  
**       INPUT  :
**          s_dir   = Directory to save the session in.  If blank,
**                    then the user will be prompted for the
**                    directory.
**       OUTPUT :  none.
**   RETURNS:    none.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
void ul_session_save(s_dir)
char *s_dir;
{
	int nc,stat,fstat,mode;
	char sbuf[80],title[80];
	UX_pathname lbuf,sfil,msg;
/*
.....Get the directory to save
.....the NCL session in
*/
	strcpy(sbuf,"Save NCL session in ...");
	strcpy(title,"Save NCL Session");
	if ((s_dir==UU_NULL)||(s_dir[0]=='\0'))
	{
		Sdir[0] = '\0'; 
		nc = 0;
		ud_get_dirname(sbuf,title,Sdir,&nc);
		if (nc == 0) goto done;
	}
	else
	{
		strcpy(Sdir, s_dir);
	}
/*
.....If the directory already exists
.....Make sure it is an NCL Session directory
.....and then prompt the user to overwrite it
*/
	mode = UX_EXISTS|UX_READ|UX_WRITE;
	stat = ux_file_inquire(UU_NULL,UU_NULL,Sdir,UU_NULL,UU_NULL,&mode,
		&fstat,lbuf,UX_NPRTERRS);
/*
........Directory already exists
*/
	if (stat == UU_SUCCESS && mode != (mode|UX_NEXISTS))
	{
		if (mode != (mode|UX_WRITE|UX_READ)) goto failed;
/*
...........Check for Session file
*/
		ul_build_full_fname(Sdir,Ssession,"ncs",sfil);
		mode = UX_EXISTS|UX_READ|UX_WRITE;
		stat = ux_file_inquire(UU_NULL,UU_NULL,sfil,UU_NULL,UU_NULL,&mode,
			&fstat,lbuf,UX_NPRTERRS);
		if (stat != UU_SUCCESS || mode != (mode|UX_EXISTS|UX_WRITE))
			goto not_session;
/*
...........Make sure the user wants to overwrite it
*/
		sprintf(lbuf,"%s already exists.\nDo you want to overwrite it?",Sdir);
		stat = ud_yesno(0,lbuf,"NCL Session Exists");
		if (!stat) goto failed;
	}
/*
.....Directory does not exist
.....Create it
*/
	else if (mode == (mode|UX_NEXISTS|UX_CREATE))
	{
		mode = 493;
		stat = ux_mk_dir(Sdir,mode,UX_NPRTERRS,UX_NCHK);
		if (stat != UU_SUCCESS) goto failed;
	}
/*
.....Cannot access directory
*/
	else
		goto failed;
/*
.....Create the session file
*/
	Sactive = UU_TRUE;
	stat = ul_session_create(msg);
	if (stat != UU_SUCCESS) goto nosave;
	goto done;
/*
.....Not an NCL session directory
*/
not_session:;
	sprintf(lbuf,"'%s' is not an NCL session directory.",Sdir);
	ud_wrerr(lbuf);
	goto done;
/*
.....Could not access directory
*/
failed:;
	sprintf(lbuf,"Could not access '%s'.",Sdir);
	ud_wrerr(lbuf);
	goto done;
/*
.....Could not save session
*/
nosave:;
	ud_wrerr(msg);
	goto done;
/*
.....End of routine
*/
done:;
	Sactive = UU_FALSE;
	return;
}

/*******************************************************************
**   E_FUNCTION : ul_session_create(msg)
**      This function controls the output of all NCL session files.
**   PARAMETERS  
**       INPUT  :  none.
**       OUTPUT :
**         msg     = Error message text if an error occurred trying
**                   to save an NCL session file (status = UU_FAILURE).
**   RETURNS:
**         UU_SUCCESS when all files are save, UU_FAILURE otherwise.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
int ul_session_create(msg)
UX_pathname msg;
{
	int stat,nc,ierr,perc[PDONE+1],tperc,frm;
	UU_LOGICAL win;
	UX_pathname dir,sfil,sext;
	UM_f77_str fdir;
/*
.....Determine the time percentages
*/
	perc[PMAIN] = 5;
	perc[PUNIB] = 60;
	perc[PSECU] = 0;
	if (lub2 > 0)
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
	if (ul_ipv_session_active(&win))
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
	ul_session_form_display(0,&frm);
/*
.....Create the main NCL session file
*/
	stat = S_create_data();
	if (stat != UU_SUCCESS)
	{
		sprintf(msg,"Could not create '%s.ncs'.",Ssession);
		goto done;
	}
	tperc = perc[PMAIN];
	ul_session_form_update(frm,tperc,PMAIN);
/*
.....Save the primary Unibase
*/
	ux_getsys_ext(1,UR_exnam,sext);
	ul_build_full_fname(Sdir,Ssession,sext,sfil);
	stat = ur_sp02(sfil);
	if (stat != UU_SUCCESS)
	{
		sprintf(msg,"Could not create '%s'.",sfil);
		goto done;
	}
	tperc = tperc + perc[PUNIB];
	ul_session_form_update(frm,tperc,PUNIB);
/*
.....Save the secondary Unibase
.....if one is defined
*/
	if (lub2 > 0) 
	{
		ur_getu_second();
		strcat (sext,"2");
		ul_build_full_fname(Sdir,Ssession,sext,sfil);
		stat = ur_sp02(sfil);
		ur_getu_work();
		if (stat != UU_SUCCESS)
		{
			sprintf(msg,"Could not create '%s'.",sfil);
			goto done;
		}
	}
	tperc = tperc + perc[PSECU];
	ul_session_form_update(frm,tperc,PSECU);
/*
.....Save the layout file
*/
	ul_build_full_fname(Sdir,Ssession,"lay",sfil);
	ud_save_layout(sfil);
	tperc = tperc + perc[PLAYO];
	ul_session_form_update(frm,tperc,PLAYO);
/*
.....Save the part program, ranfil, and clfiles
*/
	strcpy(dir,Sdir); strcat(dir,DIRSEP);
	UM_init_f77_str(fdir,dir,UX_MAX_PATH_LEN);
	nc = strlen(dir);
	nclf_save_session_file(UM_addr_of_f77_str(fdir),&nc,&ierr);
	if (ierr != 0)
	{
		sprintf(msg,"Could not create part program files.");
		goto failed;
	}
	tperc = tperc + perc[PPGMF];
	ul_session_form_update(frm,tperc,PPGMF);
/*
.....Save the Fortran commons
*/
	nclf_save_session_common(UM_addr_of_f77_str(fdir),&nc,&ierr);
	if (ierr != 0)
	{
		sprintf(msg,"Could not create common files.");
		goto failed;
	}
	tperc = tperc + perc[PCOMF];
	ul_session_form_update(frm,tperc,PCOMF);
/*
.....Save the Macro lists
*/
	stat = S_create_macro();
	if (stat != UU_SUCCESS)
	{
		sprintf(msg,"Could not create '%s.mac'.",Ssession);
		goto done;
	}
	tperc = tperc + perc[PMACR];
	ul_session_form_update(frm,tperc,PMACR);
/*
.....Save the Motion display
*/
	stat = S_create_motion();
	if (stat != UU_SUCCESS)
	{
		sprintf(msg,"Could not create '%s.mot'.",Ssession);
		goto done;
	}
	tperc = tperc + perc[PMOTN];
	ul_session_form_update(frm,tperc,PMOTN);
/*
.....Save the Source control associated keys
*/
	stat = S_create_srcctl();
	if (stat != UU_SUCCESS)
	{
		sprintf(msg,"Could not create '%s.asc'.",Ssession);
		goto done;
	}
	tperc = tperc + perc[PSRCC];
	ul_session_form_update(frm,tperc,PSRCC);
/*
.....Save NCLIPV
*/
	if (ul_ipv_session_active(&win))
	{
		ul_build_full_fname(Sdir,Ssession,"ipv",sfil);
		stat = ul_ipv_archive_session(sfil);
		if (stat != UU_SUCCESS)
		{
			sprintf(msg,"Could not save NCLIPV session.");
			goto done;
		}
	}
/*
.....NCLIPV not active
.....Save any stocks that have been defined
*/
	else
	{
		stat = S_create_stocks();
		if (stat != UU_SUCCESS)
		{
			sprintf(msg,"Could not create '%s.stk'.",Ssession);
			goto done;
		}
	}
	tperc = tperc + perc[PNIPV];
	ul_session_form_update(frm,tperc,PNIPV);
	goto done;
/*
.....Could not create sesssion
*/
failed:;
	stat = UU_FAILURE;
	goto done;
/*
.....End of routine
*/
done:;
	ul_session_form_close(frm);
	return(stat);
}

/*******************************************************************
**   E_FUNCTION : ul_session_form_display(itype,frm)
**      Displays the NCL Session progress form when saving or loading
**      an NCL Session.
**   PARAMETERS  
**       INPUT  :
**          itype   = 0 = Saving session, 1 = Loading session.
**       OUTPUT :
**          frm     = ID of created form.  Returns -1 if the form could
**                    not be displayed.
**   RETURNS: none
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
void ul_session_form_display(itype,frm)
int itype;
int *frm;
{
	static int i;
	char traverse;
	char called;
	static char display[] = {1,0, 1,0,0,0,0,0,0,0,0,0,0, 1};
	int *ans[1];
/*
.....Enable the correct field
*/
	traverse = 1;
	called = 0;
	ans[0] = &i; i = 0;
	display[0] = display[1] = 0; display[itype] = 1;
	*frm = ud_form_display1("session.frm",&ans,&ans,UU_NULL,&called,display,
		&traverse);
/*	ud_hakt(10,1);*/
}

/*******************************************************************
**   E_FUNCTION : ul_session_form_update(frm,perc,label)
**      Updates the NCL Session progress form.
**   PARAMETERS  
**       INPUT  :
**          frm     = ID of progress form.
**          perc    = Percentage complete.
**          label   = Which label to display.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none 
**   WARNINGS:
*********************************************************************/
void ul_session_form_update(frm,perc,label)
int frm,perc,label;
{
	int tperc;
	static int Slabel=0;
/*
.....Form is not displayed
*/
	if (frm == -1) return;
/*
.....Display the correct label
*/
	ud_setfrm_display_mask(frm,UD_DISPLAYF,Slabel+2,UU_FALSE);
	ud_setfrm_display_mask(frm,UD_DISPLAYF,Slabel+3,UU_TRUE);
	Slabel = label;
/*
.....Update the percentage complete
*/
	tperc = perc;
	ud_dispfrm_update_answer(frm,0,&tperc);
	ud_update_form(frm);
}

/*******************************************************************
**   E_FUNCTION : ul_session_form_close()
**      Closes the NCL Session progress form.
**   PARAMETERS  
**       INPUT  :
**          frm     = ID of progress form.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none 
**   WARNINGS:
*********************************************************************/
void ul_session_form_close(frm)
{
	ud_close_dispfrm(frm);
}

/*******************************************************************
**   E_FUNCTION : ul_new_session()
**      start a new NCL Session progress (spawn a sub-ncl process)
**   PARAMETERS  
**       INPUT  : none
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none 
**   WARNINGS:
*********************************************************************/
void ul_new_session()
{
	(*(ug_gksstli.wsopen[0].connid)[UW_NEW_SESSION])();
}

/*******************************************************************
**   E_FUNCTION : ul_chkkey_common(keys)
**      Check the key common area to get the keys
**   PARAMETERS  
**       INPUT  : none
**       OUTPUT : keys
**   RETURNS: none
**   SIDE EFFECTS: none 
**   WARNINGS:
*********************************************************************/
int ul_chkkey_common(keys)
char *keys;
{
/*
.....this function called before we setup table, so can't use UW_CHKKEY_COM
*/
/*
	(*(ug_gksstli.wsopen[0].connid)[UW_CHKKEY_COM])(keys);
*/
	return uw_chkkey_common(keys);
}

/*******************************************************************
**   I_FUNCTION : S_create_data()
**      This function creates the main NCL session data file.
**   PARAMETERS  
**       INPUT  :  none.
**       OUTPUT :  none.
**   RETURNS:
**         UU_SUCCESS when file is saved successfully,
**         UU_FAILURE otherwise.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
static int S_create_data()
{
	int stat,nc,lic[10],src,strt,tend;
	char sstr[40],estr[40];
	UU_LOGICAL win;
	UX_pathname sfil,sbuf;
	FILE *fd;
/*
.....Open the NCL session file
*/
	ul_build_full_fname(Sdir,Ssession,"ncs",sfil);
	stat = ux_fopen0(sfil,"w",&fd);
	if (stat != UU_SUCCESS) goto done;
/*
.....NCL version number
*/
	sprintf(sbuf,"NCL %g",NCL_version);
	FWRITE(sbuf);
/*
.....Licenses
*/
	ncl_get_licenses(lic);
	sprintf(sbuf,"%d %d %d %d %d %d %d",lic[0],lic[1],lic[2],lic[3],lic[4],
		lic[5],lic[6]);
	FWRITE(sbuf);
/*
.....Default directory
*/
	ul_get_full_dir(".",sbuf);
	FWRITE(sbuf);
/*
.....Part program name
*/
	FWRITE(UL_program);
	FWRITE(UL_program_suffix);
/*
.....Unibase name
*/
	strcpy(sfil,UR_dpn);
	ux_add_ftype("UR_UNB_FILE",sfil,UX_NPRTERRS);
	FWRITE(sfil);
/*
.....Secondary Unibase name
*/
	FWRITE(UBopen);
	sprintf(sbuf,"%d %d %d",UR_EOF_stat,NCL_ubnew,UB_ftype);
	FWRITE(sbuf);
/*
.....Secondary clfile
*/
	ncl_get_clfile_src(&src,sfil,&strt,sstr,&tend,estr,UU_FALSE);
	FWRITE(sfil);
	sprintf(sbuf,"%d %d %d",src,strt,tend);
	FWRITE(sbuf);
	FWRITE(sstr);
	FWRITE(estr);
/*
.....3d (NCLCAM) or 2d (DRAWING) mode
*/
	if (UM_2d3d_mode == UM_2D) strcpy(sbuf,"DRAWING");
	else strcpy(sbuf,"NCLCAM");
	FWRITE(sbuf);
/*
.....NCLIPV
*/
	if (ul_ipv_session_active(&win)) sprintf(sbuf,"NCLIPV 1 %d",win);
	else strcpy(sbuf,"NCLIPV 0 0");
	FWRITE(sbuf);
/*
......write the maxinum line length allowed
*/
	sprintf(sbuf,"%d",UL_line_len);
	FWRITE(sbuf);
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
**   I_FUNCTION : S_create_motion()
**      This function saves the motion display in an NCL session file.
**   PARAMETERS  
**       INPUT  :  none.
**       OUTPUT :  none.
**   RETURNS:
**         UU_SUCCESS when file is saved successfully,
**         UU_FAILURE otherwise.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
static int S_create_motion()
{
	int stat,nc,np,i,j,inc,aptr[5],cptr[3],*lines,pnc;
	char tbuf[80];
	UM_coord *pts;
	UM_vector *vcs;
	UX_pathname sfil,sbuf;
	UN_motseg *mpt,xmpt;
	UN_motseg_attr attr;
	UN_mot_attr mattr;
	UN_motseg_blade blade;
	UN_motseg_cutter_struc *cutseg;
	UN_motseg_view mview;
	UN_motseg_cutparm *cparm;
	UN_motseg_cutattr *cattr;
	UN_motseg_symbol *csym;
	UN_motseg_symgeo *symgeo;
	FILE *fd;
/*
.....Open the NCL session file
.....for motion display
*/
	ul_build_full_fname(Sdir,Ssession,"mot",sfil);
	stat = ux_fopen0(sfil,"w",&fd);
	if (stat != UU_SUCCESS) goto done;
/*
.....Save Motion Attributes
*/
	np = ncl_motattr_size();
	sprintf(sbuf,"MA %d",np);
	FWRITE(sbuf);
	for (i=0;i<np;i++)
	{
		ncl_motattr_get(&attr,i);
		sprintf(sbuf,"%d %d %d",attr.color,attr.lnstyle,attr.pen);
		FWRITE(sbuf);
	}
/*
.....Save ISN list
*/
	np = ncl_motisn_size();
	sprintf(sbuf,"SN %d",np);
	FWRITE(sbuf);
	pnc = -1;
	lines = UU_NULL;
	for (i=0;i<np;i++)
	{
		ncl_motisn_get_count(i,&nc);
		sprintf(sbuf,"%d",nc);
		if (nc > 0)
		{
			if (nc > pnc)
			{
				if (lines != UU_NULL) uu_free(lines);
				lines = (int *)uu_malloc(sizeof(int)*nc);
				pnc = nc;
			}
		}
		ncl_motisn_getrec(lines,i);
		for (j=0;j<nc;j++)
		{
			sprintf(tbuf,",%d",lines[j]);
			strcat(sbuf,tbuf);
		}
		FWRITE(sbuf);
	}
	if (lines != UU_NULL) uu_free(lines);
/*
.....Save Motion Cutting Attributes
*/
	np = ncl_motmattr_size();
	sprintf(sbuf,"MC %d",np);
	FWRITE(sbuf);
	for (i=0;i<np;i++)
	{
		ncl_motmattr_get(&mattr,i);
		sprintf(sbuf,"%d %lf %d %lf %d %d %d",mattr.loadtl,mattr.tlen,
			mattr.sp_mode,mattr.sp_val,mattr.coolnt,mattr.cc_mode,mattr.cc_dir);
		FWRITE(sbuf);
	}
/*
.....Save Motion Views
*/
	np = ncl_motview_size();
	sprintf(sbuf,"MV %d",np);
	FWRITE(sbuf);
	for (i=0;i<np;i++)
	{
		ncl_motview_get(&mview,i);
		sprintf(sbuf,"%d",mview.nview);
		for (j=0;j<mview.nview;j++)
		{
			sprintf(tbuf," %d",mview.vinc[j]);
			strcat(sbuf,tbuf);
		}
		FWRITE(sbuf);
	}
/*
.....Save Blade Directions
*/
	np = ncl_motblade_size();
	sprintf(sbuf,"BL %d",np);
	FWRITE(sbuf);
	for (i=0;i<np;i++)
	{
		ncl_motblade_get(&blade,i);
		sprintf(sbuf,"%lf %lf %lf",blade.tfwd[0],blade.tfwd[1],blade.tfwd[2]);
		FWRITE(sbuf);
	}
/*
.....Save Cutter Structures
*/
	np = ncl_cutter_struc_size();
	sprintf(sbuf,"NC %d",np);
	FWRITE(sbuf);
	for (i=0;i<np;i++)
	{
		ncl_cutter_get_struc(&cutseg,i);
		sprintf(sbuf,"%d %d %d %d %d",cutseg->cutter,cutseg->cattr,
			cutseg->cutsym,cutseg->shank,cutseg->holder);
		FWRITE(sbuf);
	}
/*
.....Save Cutter Parameters
*/
	np = ncl_cutter_parms_size();
	sprintf(sbuf,"CU %d",np);
	FWRITE(sbuf);
	for (i=0;i<np;i++)
	{
		ncl_cutter_get_parms(&cparm,i);
		sprintf(sbuf,"%lf %lf %lf %lf %lf",cparm->parms[0],cparm->parms[1],
			cparm->parms[2],cparm->parms[3],cparm->parms[4]);
		FWRITE(sbuf);
		sprintf(sbuf,"%lf %lf %lf %lf",cparm->parms[5],cparm->parms[6],
			cparm->parms[7],cparm->parms[8]);
		FWRITE(sbuf);
	}
/*
.....Save Cutter Attributes
*/
	np = ncl_cutter_attr_size();
	sprintf(sbuf,"CA %d",np);
	FWRITE(sbuf);
	for (i=0;i<np;i++)
	{
		ncl_cutter_get_attr(&cattr,i);
		sprintf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",cattr->mov,
			cattr->segfl,cattr->shaded[0],cattr->shaded[1],cattr->shaded[2],
			cattr->color[0],cattr->color[1],cattr->color[2],cattr->pen[0],
			cattr->pen[1],cattr->pen[2],cattr->pen[3],cattr->trans[0],
			cattr->trans[1],cattr->trans[2]);
		FWRITE(sbuf);
	}
/*
.....Save Cutter Symbol Geometry
*/
	np = ncl_cutter_symgeo_size();
	sprintf(sbuf,"GM %d",np);
	FWRITE(sbuf);
	for (i=0;i<np;i++)
	{
		ncl_cutter_get_symgeo(&symgeo,i);
		inc = UU_LIST_LENGTH(&symgeo->curve);
		sprintf(sbuf,"%d %d %d %d %d %lf %lf %lf",symgeo->key,symgeo->segno,
			symgeo->type,symgeo->shaded,inc,symgeo->axis[0],symgeo->axis[1],
			symgeo->axis[2]);
		FWRITE(sbuf);
		FWRITE(symgeo->symbol);
		pts = (UM_coord *)UU_LIST_ARRAY(&symgeo->curve);
		vcs = (UM_vector *)UU_LIST_ARRAY(&symgeo->cnorm);
		for (j=0;j<inc;j++)
		{
			sprintf(sbuf,"%lf %lf %lf %lf %lf %lf",pts[j][0],pts[j][1],pts[j][2],
				vcs[j][0],vcs[j][1],vcs[j][2]);
			FWRITE(sbuf);
		}
	}
/*
.....Save Cutter Symbols
*/
	np = ncl_cutter_symbol_size();
	sprintf(sbuf,"SY %d",np);
	FWRITE(sbuf);
	for (i=0;i<np;i++)
	{
		ncl_cutter_get_symbol(&csym,i);
		sprintf(sbuf,"%lf %lf %lf %lf %lf %lf %d",csym->parms[0],csym->parms[1],
			csym->parms[2],csym->parms[3],csym->zlim[0],csym->zlim[1],
			csym->geoptr);
		FWRITE(sbuf);
	}
/*
.....Save active structure pointers
*/
	ncl_motattr_get_ptrs(aptr);
	ncl_cutter_get_ptrs(cptr);
	sprintf(sbuf,"CP %d %d %d %d %d %d %d %d",aptr[0],aptr[1],aptr[2],aptr[3],
		aptr[4],cptr[0],cptr[1],cptr[2]);
	FWRITE(sbuf);
/*
.....Motion is not displayed
.....Don't save the motion list
*/
	if (!ncl_motion_displayed()) goto done;
/*
.....Initialize the motion display structure
*/
	xmpt.tend.x = 0; xmpt.tend.y = 0; xmpt.tend.z = 0;
	xmpt.taxis.x = 0; xmpt.taxis.y = 0; xmpt.taxis.z = 0;
	xmpt.fr_mode = 0; xmpt.fr_val = 0;
	xmpt.attr = -1; xmpt.cutter = -1; xmpt.view = -1; xmpt.blade = -1;
	xmpt.isn = -1; xmpt.mattr = -1;
/*
.....Loop through the motion display list
*/
	mpt = mlist_first_ptr;
	do
	{
		mpt = (UN_motseg *)uu_lsnext(mpt);
		if (mpt == 0) break;
/*
........Save the motion segment
*/
		sprintf(sbuf,"TE %lf %lf %lf",mpt->tend.x,mpt->tend.y,mpt->tend.z);
		FWRITE(sbuf);

		if (mpt->taxis.x != xmpt.taxis.x || mpt->taxis.y != xmpt.taxis.y ||
			mpt->taxis.z != xmpt.taxis.z)
		{
			sprintf(sbuf,"TA %lf %lf %lf",mpt->taxis.x,mpt->taxis.y,mpt->taxis.z);
			FWRITE(sbuf);
		}

		if (mpt->fr_mode != xmpt.fr_mode || mpt->fr_val != xmpt.fr_val)
		{
			sprintf(sbuf,"FR %d %lf",mpt->fr_mode,mpt->fr_val);
			FWRITE(sbuf);
		}

		if (mpt->attr != xmpt.attr || mpt->cutter != xmpt.cutter ||
			mpt->view != xmpt.view || mpt->blade != xmpt.blade ||
			mpt->isn != xmpt.isn || mpt->mattr != xmpt.mattr)
		{
			sprintf(sbuf,"PM %d %d %d %d %d %d", mpt->attr,mpt->cutter,mpt->view,
				mpt->blade,mpt->isn,mpt->mattr);
			FWRITE(sbuf);
		}
/*
.....Save the current motion segment
*/
		xmpt = *mpt;
	} while (mpt != mlist_ptr);
/*
.....End of routine
.....Close the open file
*/
done:;
	ux_fclose0(fd);
	return(stat);
}

/*******************************************************************
**   I_FUNCTION : S_create_macro()
**      This function saves the Macro list in an NCL session file.
**   PARAMETERS  
**       INPUT  :  none.
**       OUTPUT :  none.
**   RETURNS:
**         UU_SUCCESS when file is saved successfully,
**         UU_FAILURE otherwise.
**   SIDE EFFECTS: none. 
**   WARNINGS:
*********************************************************************/
static int S_create_macro()
{
	UM_int2 nwds,ietype;
	UM_int4 ipg,iel,isub,key,nif,*ary,nent;
	int stat,i,j,nc,inc,svpt[4][2],class,nmac;
	char lab[NCL_MAX_LABEL+1],sbuf[80],snum[20],*tptr,*tptrf;
	UX_pathname sfil;
	struct NCL_macro_list_data *p1,*p3[4];
	struct NCL_macro_call_data *p2;
	struct NCL_macro_define_data *p4;
	FILE *fd;
	UM_f77_str flab;
	UU_LIST *lstp;
	union {UU_REAL rval; int ival[2];} tval;
/*
.....Open the NCL session file
.....for Macro lists
*/
	ul_build_full_fname(Sdir,Ssession,"mac",sfil);
	stat = ux_fopen0(sfil,"w",&fd);
	if (stat != UU_SUCCESS) goto done;
/*
.....Get the active Macro List pointers
*/
	ncl_get_maclist_ptrs(p3);
	svpt[0][0] = svpt[0][1] = svpt[1][0] = svpt[1][1] = -1;
	svpt[2][0] = svpt[2][1] = svpt[3][0] = svpt[3][1] = -1;
/*
.....Get the list of defined Macros
*/
	tptr = (char *)uu_lsnew();
	tptrf = tptr;
	if (tptr == UU_NULL) goto failed;
	class = 11;
	stat = ncl_entnam_list(class,tptr,&nmac);
	if (stat != UU_SUCCESS) goto done;
	UM_init_f77_str(flab,lab,NCL_MAX_LABEL+1);
/*
.....Store the number of Macros
*/
	sprintf(sbuf,"%d",nmac);
	FWRITE(sbuf);
/*
.....Loop through the defined Macros
*/
	isub = 0;
	for (i=0;i<nmac;i++)
	{
		tptr = (char *)uu_lsnext(tptr);
		if (tptr == UU_NULL) goto failed;
		strcpy(lab,tptr);
		stat = vxchk(UM_addr_of_f77_str(flab),&isub,&key,&ipg,&iel,&nwds,&ietype);
		if (stat != UU_SUCCESS) goto done;
		sprintf(sbuf,"%s %d %d %d %d %d",tptr,ipg,iel,0,0,nwds);
		FWRITE(sbuf);

/*
........Store Macro Arguments
*/
		p1 = UU_NULL;
		inc = 0;
		do
		{
			if (inc==0)
			{
				p1 = (struct NCL_macro_list_data *)ncl_get_macptr();
				ncl_get_macinfo(&p4);
				if (p4==UU_NULL) break;
			}
			else
			{
				ncl_get_next_maclist(&p1, inc);
			}
			if (p1 == UU_NULL) break;
			for (j=0;j<4;j++)
			{
				if (p1 == p3[j])
				{
					svpt[j][0] = i;
					svpt[j][1] = inc;
				}
			}
/*
........First item in list is used for
........pointer comparisons only, there
........is no valid data in it, so don't write it
*/
			if (inc != 0)
			{
				if (p1->iflg <= 1) p1->dvalue = 0.;
				tval.rval = p1->rvalue;
				sprintf(sbuf,"%lf %lf %ld %ld %lf %lf %d",p1->dvalue,p1->avalue,
					tval.ival[0],tval.ival[1],p1->max,p1->min, p1->rtype);
				FWRITE(sbuf);
				sprintf(sbuf,"%d %d %d %d %d %d %d %d %d",p1->iflg,p1->psub,
					p1->dsub,p1->dclas,p1->detype,p1->asub,p1->iclas,p1->ietype,
					p1->keyscl);
				FWRITE(sbuf);
				sprintf(sbuf,"%d %d %d %d %d %d %d",p1->pclas,p1->count,
					p1->geomflag,p1->prsvalue,p1->lenvalue,p1->substr,p1->pflag);
				FWRITE(sbuf);
				FWRITE(p1->plab);
				FWRITE(p1->dlab);
				FWRITE(p1->alab);
				FWRITE(p1->prompt);
				p1->wrdlist[p1->count*8] = '\0';
				FWRITE(p1->wrdlist);
			}
			else
			{
				sprintf(sbuf,"%d %d %d %d %d %d %d %d %d %d",p4->outflag,p4->dispflag,
					p4->rvsaved,p4->maclin,p4->next,p4->termln,p4->mode,p4->pr_rec,
					p4->pr_ele, p4->callin);
				FWRITE(sbuf);
				FWRITE(p4->classname);
				FWRITE(p4->prompt);
			}
			inc++;
		} while (p1 != UU_NULL);
/*
........End of Macro List
*/
		FWRITE("~EOM");
	}
/*
.....Loop through the called Macros
*/
	p2 = UU_NULL;
	do
	{
		ncl_get_next_macall(&p2);
		if (p2 == UU_NULL) break;
/*
........Store Macro Call
*/
		FWRITE(p2->macnam);
	} while (p2 != UU_NULL);
/*
........End of Macro Call List
*/
	FWRITE("~EOC");
/*
.....Store Macro pointers
*/
	sprintf(sbuf,"%d %d %d %d %d %d %d %d",svpt[0][0],svpt[0][1],
		svpt[1][0],svpt[1][1],svpt[2][0],svpt[2][1],svpt[3][0],svpt[3][1]);
	FWRITE(sbuf);
/*
.....Delete the list of macros
*/
	if (tptrf != UU_NULL) uu_lsdele(tptrf);
/*
.....Save IF-THEN-ELSE structures
*/
	ncl_ifget_arrays(0,&lstp,&ary,&nif);
	sprintf(sbuf,"%d",nif);
	FWRITE(sbuf);
	for (i=0;i<nif;i++)
	{
		ncl_ifget_arrays(1,&lstp,&ary,&nent);
		sprintf(sbuf,"%d",nent);
		FWRITE(sbuf);
		inc = 0;
		sbuf[0] = '\0';
		for (j=0;j<nent;j++)
		{
			sprintf(snum,"%d ",ary[j]);
			strcat(sbuf,snum);
			inc++;
			if (inc == 10)
			{
				FWRITE(sbuf);
				sbuf[0] = '\0';
				inc = 0;
			}
		}
		if (strlen(sbuf) > 0)
		{
			FWRITE(sbuf);
		}
		lstp++;
	}

	ncl_ifget_arrays(2,&lstp,&ary,&nif);
	sprintf(sbuf,"%d",nif);
	FWRITE(sbuf);
	sbuf[0] = '\0';
	inc = 0;
	for (i=0;i<nif;i++)
	{
		sprintf(snum,"%d ",ary[i]);
		strcat(sbuf,snum);
		inc++;
		if (inc == 10)
		{
			FWRITE(sbuf);
			sbuf[0] = '\0';
			inc = 0;
		}
	}
	if (strlen(sbuf) > 0)
	{
		FWRITE(sbuf);
	}
/*
.....Get the list of defined Labels
*/
	tptr = (char *)uu_lsnew();
	tptrf = tptr;
	if (tptr == UU_NULL) goto failed;
	class = 13;
	stat = ncl_entnam_list(class,tptr,&nmac);
	if (stat != UU_SUCCESS) goto done;
	UM_init_f77_str(flab,lab,NCL_MAX_LABEL+1);
/*
.....Store the number of Labels
*/
	sprintf(sbuf,"%d",nmac);
	FWRITE(sbuf);
/*
.....Loop through the defined Labels
*/
	for (i=0;i<nmac;i++)
	{
		tptr = (char *)uu_lsnext(tptr);
		if (tptr == UU_NULL) goto failed;
		strcpy(lab,tptr);
/*
......we are not using 8 bytes for label and subscript now
......but seperate 64chars label and subscript interger number
......so the following code will cause problem because the isub = 8224
......is wrong, if the label is a number, the isub = 0;
......Yurong
*/
/*
		if (ul_to_number(lab,&nc) == UU_SUCCESS) 
		{
*/
/*
.....this will have problem, in Fortran, equivalence of "  " is 8224
.....but in here '  ' as the old code will be 32 and if we put
....."  " will be 22444, so I just changed it to use 8224 ("0001111100011111")
.....directly
*/
/*			isub = '  ';  */
/*************************
			isub = 8224;
		}
		else isub = 0;
*/
		isub = 0;
		stat = vxchk(UM_addr_of_f77_str(flab),&isub,&key,&ipg,&iel,&nwds,&ietype);
		if (stat != UU_SUCCESS) goto done;
		sprintf(sbuf,"%s %d %d %d",tptr,key,ipg,iel);
		FWRITE(sbuf);
	}
/*
.....Delete the list of labels
*/
	if (tptrf != UU_NULL) uu_lsdele(tptrf);
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
**   I_FUNCTION : S_create_stocks()
**      This function saves the stock and fixture definitions in a
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
static int S_create_stocks()
{
	int stat,i,j,nc;
	char sbuf[100];
	LW_stock_struc *sd;
	UX_pathname sfil;
	FILE *fd;
/*
.....Open the NCL session file
.....for stock commands
*/
	ul_build_full_fname(Sdir,Ssession,"stk",sfil);
	stat = ux_fopen0(sfil,"w",&fd);
	if (stat != UU_SUCCESS) goto failed;
/*
.....Save stock commands
*/
	ul_ipv_save_stock_cmds(fd,UU_FALSE,UU_TRUE,UU_NULL,UU_NULL);
/*
.....Open the NCL session file
.....for stock attributes
*/
	ux_fclose0(fd);
	if (LW_nstock[0]+LW_nstock[1] > 0)
	{
		ul_build_full_fname(Sdir,Ssession,"ska",sfil);
		stat = ux_fopen0(sfil,"w",&fd);
		if (stat != UU_SUCCESS) goto failed;
/*
.....Save stock attributes
*/
		for (j=0;j<2;j++)
		{
			sd = LW_stock_first[j];
			for (i=0;i<LW_nstock[j];i++)
			{
				sprintf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d %d %d %lf",sd->id,
					sd->color,sd->translucency,sd->units,sd->mxflag,sd->mxchg,
					sd->axes,sd->axes_color,sd->visible,sd->active,sd->bin,
					sd->edge,sd->edge_color,sd->toler);
				FWRITE(sbuf);
				if (sd->mxflag)
				{
					FWRITE(sd->mxname);
					sprintf(sbuf,"%lf %lf %lf %lf %lf %lf",sd->matrix[0][0],
						sd->matrix[0][1],sd->matrix[0][2],sd->matrix[1][0],
						sd->matrix[1][1],sd->matrix[1][2]);
					FWRITE(sbuf);
					sprintf(sbuf,"%lf %lf %lf %lf %lf %lf",sd->matrix[2][0],
						sd->matrix[2][1],sd->matrix[2][2],sd->matrix[3][0],
						sd->matrix[3][1],sd->matrix[3][2]);
					FWRITE(sbuf);
				}
				sd = (LW_stock_struc *)uu_lsnext(sd);
			}
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
**   I_FUNCTION : S_create_srcctl()
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
static int S_create_srcctl()
{
	int stat,*data,nc,i;
	UU_LOGICAL rfl;
	char tbuf[20];
	UX_pathname sfil,sbuf;
	FILE *fd;
/*
.....Make sure that source control is being used
*/
	stat = UU_SUCCESS;
	if (ncl_srcctl_scan_init() == UU_FAILURE) goto done;
/*
.....Open the NCL session file
*/
	ul_build_full_fname(Sdir,Ssession,"acs",sfil);
	stat = ux_fopen0(sfil,"w",&fd);
	if (stat != UU_SUCCESS) goto done;
/*
.....Get and store source control entries
*/
	do
	{
		data = (int *)ncl_srcctl_scan();
		if (data != UU_NULL)
		{
			sprintf(sbuf,"%d %d",data[0],data[1]);
			FWRITE(sbuf);
			sbuf[0] = '\0';
			rfl = UU_FALSE;
			for (i=0;i<data[1];i++) 
			{
				if (rfl)
				{
					sprintf(tbuf,"%g ",data[i+2]);
					rfl = UU_FALSE;
				}
				else
				{
/*					if (data[i+2] < 0) rfl = UU_TRUE;*/
					sprintf(tbuf,"%d ",data[i+2]);
				}
				strcat(sbuf,tbuf);
			}
			FWRITE(sbuf);
		}
	} while (data != UU_NULL);
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
