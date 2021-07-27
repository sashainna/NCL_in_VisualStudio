/*********************************************************************
**    NAME         :  numot.c
**       CONTAINS:
**			nclu_playback()
**			nclu_playfile()
**       nclu_set_external_clfile()
**       nclu_mcd_options()
**			nclu_playfeed()
**			nclu_playinterp()
**			nclu_playclip()
**			rsmply()
**			ncl_reset_mplayback()
**			ncl_reset_playparm()
**       ncl_get_playfile()
**       ncl_get_clfile_src()
**       ncl_set_clfile_src()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       numot.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       07/28/15 , 11:08:54
*********************************************************************/

#define NCLPLAYBACK
#include "usysdef.h"
#include "xenv1.h"
#include "lipv.h"
#include "lipvmach.h"
#include "lipvmplay.h"
#include "nclmplay.h"
#include "gtblseg.h"
#include "mdrel.h"
#include "mdcpln.h"
#include "mfort.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclfile.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "view.h"
#include <ctype.h>
#include "lcom.h"
#include "lumb.h"
#include "xenv1.h"
#include "xfsys1.h"
#undef NCLPLAYBACK

#define FPST 0
#define FPSD 1
#define FPMD 2
#define FPSP 3
#define FPER 4
#define FPDY 5
#define FPSH 6
#define FPTR 7
#define FPCS 8
#define FPCT 9
#define FPKS 10
#define FPKT 11
#define FPHS 12
#define FPHT 13
#define FPCY 14
#define FPSC 15
#define FPRP 16
#define FPAN 17

#define FSRC 0
#define FOPT 1
#define FMAC 2
#define FFIL 3
#define FBRO 4
#define FSTR 5
#define FSSB 6
#define FSSQ 7
#define FSCM 8
#define FEND 9
#define FESB 10
#define FESQ 11
#define FECM 12

#define FOSC1 0
#define FOSC2 1
#define FOSC3 2
#define FOSC4 3
#define FOMF1 4
#define FOMF2 5
#define FOMF3 6
#define FOCF1 7
#define FOCF2 8
#define FOCF3 9

#define T_CURRENT 0
#define T_EXTERNAL 1
#define T_MCD 2
#define T_SIMULATE 3
#define T_POSTED 4
#define T_APTSRC 5
#define T_UGII 6
#define T_CATV4 7
#define T_CATV5 8
#define T_MASTER 9
#define T_REVERSED 10
#define T_END 11
/*
.....Defined those value because we will have
.....different choice for NCLIPV application and NCL
*/
static int Sfidx[T_END];
static UX_pathname Stclfil,Sclfil;
static char Sfsseq[22],Sfscmd[42],Sfeseq[22],Sfecmd[42];
static char Stsseq[22],Stscmd[42],Steseq[22],Stecmd[42];
static int Splay_mod[18] = {0,100,0,1,1,1, 2,0,0,0, 100,100,100,100, 1,0,0,0};
static int Sstrt,Smode,Sspd,Sstep,Seras,Sdyn,Sshow,Scyc,Sanlz,Srapid,Ssrc=0;
static int Sshadeall,Sshade[3],Strans[3],Stransall;
static int Sfstrt,Send,Sfld;
static int Sfrm=0,Sfrm1=0;
static char *Ssrctxt[] = {"Current","External","MCD","Simulate","Posted",
	"APT Source","UG II","Catia V4","Catia V5","Mastercam","Reversed"};
static char lstyle1[9][10] = {"*DEFAULT","*SOLID","*DASH","*DOTS","*CENTER",
   "*PHANTOM","*DASHLN","*DASHDT","*DASHSP"};
static char scolor1[65][96];
static UD_LIST Sgeom,Slist,Smach;

static int Sscan=0,Sloadmcd=1,Sselmach=0;
static UU_LOGICAL Sscanned = UU_FALSE;
static char Stedcmd[80]={""};
static UX_pathname Sscanfil={""}, Smdffil={""}, Scutfil={""};
static UX_pathname Stempname;

static UD_FSTAT OnPlayTog();
static UD_FSTAT OnFilTog(),OnSequnc(),OnSelect1(),OnClose(),OnOption();

static UD_FSTAT OnOpScan(),OnOpTog(),OnOpBrowse(),OnOpEdit(),OnOpClose();

static void S_set_filter();
/*
static int S_load_mcd(),S_load_clfile();
*/
int nclu_load_mcd(), nclu_load_clfile();
static UU_LOGICAL S_chdir();

static int S_save_pfeeds(),S_save_pinterp();

char *uu_malloc();
UX_pathname NCL_tpfile = "";
UX_pathname NCL_tpdir = "";

extern char uw_color_name[64][96];
/*********************************************************************
**    E_FUNCTION     : nclu_playback()
**       Processes the NCL PLAYBACK form.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_playback()
{
	int status,trans[3],ifl,i,markval,cfl[10];
	UU_LOGICAL itrans;
	static int idrw;
	UV_vport vport;
	UN_motseg_cutattr cattr;
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1,1,0,1,1, 1,1,1,1,1,1,1,1, 1,1,1,1};
	static UD_METHOD methods[] = {UU_NULL,UU_NULL,OnPlayTog,UU_NULL,
	   UU_NULL,UU_NULL, OnPlayTog,OnPlayTog, OnPlayTog,UU_NULL,OnPlayTog,UU_NULL,
		OnPlayTog,UU_NULL, UU_NULL,UU_NULL,UU_NULL,UU_NULL};
	static char called[]       = {6,6,6,6,6,6, 6,6,6,6,6,6,6,6, 6,6,6,6};
	static int *ans[] = {&Sstrt, &Sspd, &Smode, &Sstep, &Seras, &Sdyn,
		&Sshadeall, &Stransall, &Sshade[0], &Strans[0], &Sshade[1], &Strans[1],
		&Sshade[2], &Strans[2], &Scyc, &Sshow, &Srapid, &Sanlz};
/*
.....Playback is already active
*/
	if (UN_playback_active) return UU_SUCCESS;
/*
.....Make sure NCLIPV is not active
*/
	if (LW_active)
	{
		ud_wrerr("Motion playback is not allowed while NCLIPV is active.");
		return(UU_SUCCESS);
	}
/*
.....Trap reject op
.....For Analyzation Playback
*/
	ifl = 0;
	idrw = 0;
	UD_MARK(markval,UU_TRUE);
	if (markval != 0) goto done;
/*
.....Set up the field entries
*/
	Sstrt = Splay_mod[0]; Sspd  = Splay_mod[1]; Smode  = Splay_mod[2];
	Sstep = Splay_mod[3]; Seras = Splay_mod[4]; Sdyn  = Splay_mod[5];
	Sshadeall = Splay_mod[6]; Sshade[0] = Splay_mod[7];
	Sshade[1] = Splay_mod[8]; Sshade[2] = Splay_mod[9];
	Stransall = Splay_mod[10]; Strans[0] = Splay_mod[11];
	Strans[1] = Splay_mod[12]; Strans[2] = Splay_mod[13];
	Scyc = Splay_mod[14]; Sanlz  = Splay_mod[15]; Sshow = Splay_mod[16];
	Srapid = Splay_mod[17];
/*
.....Enable correct fields
.....based on DISPLAY field value
*/
form:;
	if (Smode == 0) traverse[FPSP] = 0;
	else traverse[FPSP] = 1;
	if (Sshadeall == 2)
	{
		traverse[FPTR] = 0;
		traverse[FPCS] = 0; traverse[FPCT] = 0;
		traverse[FPKS] = 0; traverse[FPKT] = 0;
		traverse[FPHS] = 0; traverse[FPHT] = 0;
	}
	else
	{
		traverse[FPTR] = 1;
		traverse[FPCS] = 1; traverse[FPCT] = Sshade[0];
		traverse[FPKS] = 1; traverse[FPKT] = Sshade[1];
		traverse[FPHS] = 1; traverse[FPHT] = Sshade[2];
	}
/*
.....Get the Form input
*/
	UN_playback_active = UU_FALSE;
	status = ud_form1("playback.frm", ans, ans, methods, called, UU_NULL, traverse);
	if (status==-1) goto done;
/*
.....Save the form entries
*/
	Splay_mod[0] = Sstrt; Splay_mod[1] = Sspd;  Splay_mod[2] = Smode;
	Splay_mod[3] = Sstep; Splay_mod[4] = Seras; Splay_mod[5] = Sdyn;
	Splay_mod[6] = Sshadeall; Splay_mod[7] = Sshade[0];
	Splay_mod[8] = Sshade[1]; Splay_mod[9] = Sshade[2];
	Splay_mod[10] = Stransall; Splay_mod[11] = Strans[0];
	Splay_mod[12] = Strans[1]; Splay_mod[13] = Strans[2];
	Splay_mod[14] = Scyc; Splay_mod[15] = Sanlz;  Splay_mod[16] = Sshow;
	Splay_mod[17] = Srapid;
/*
.....Initialize the playback parameter structure
*/
	if (Sstrt == 0)
	{
		UN_clfile_current = UN_clfile_start;
		UN_clfile_curpt = 0;
/*
........Load the current clfile parameters
........when starting from a "*Command"
*/
		if (UN_playfile_start == 2 && UN_playfile_src != 2)
			ncl_fill_sequnc(UN_clfile_start);
	}
	if (Splay_mod[0] == 1 && UN_clfile_current == UU_NULL)
		UN_clfile_current = UN_last_clpt[0];
	if (UN_clfile_current == UU_NULL && UN_clfile_curpt == 0)
	{
		ncl_reset_mplayback();
	}
/*
.....Save the tool axis
*/
	if (ifl == 0)
	{
		UN_playtax_save[0] = UN_playparm.spt[3];
		UN_playtax_save[1] = UN_playparm.spt[4];
		UN_playtax_save[2] = UN_playparm.spt[5];
		ifl = 1;
	}
/*
.....Analyzation mode
.....Draw the part in the correct color
*/
	if (Splay_mod[15] != 0 && (UN_anlz_geo[0] != -1 || UN_anlz_geo[1] != -1) &&
		idrw == 0)
	{
		idrw   = 1;
		UN_override_geo_attr[0] = UN_anlz_geo[0];
		UN_override_geo_attr[1] = UN_anlz_geo[1];
		UN_override_geo_mask = 0;
		for (i=1;i<=UV_act_screen[0].nvports;i++)
		{
			uv_getvpid(UV_act_screen[0].vports[i-1],&vport);
			if (vport.motion)
			{
				UN_override_geo_mask = UN_override_geo_mask | (1 << i) |
					UG_SEGINNTRAN;
			}
		}
		ncl_redraw_geo(-1,1,0,Splay_mod[5],UU_FALSE);
	}
/*
.....Set the cutter attribute settings
*/
	ncl_cutter_get_defattr(&cattr);
	if (Splay_mod[6] == 2)
	{
		cutget_flag(cfl);
		Splay_mod[7] = cfl[3];
		Splay_mod[8] = cfl[6];
		Splay_mod[9] = cfl[7];
		itrans = UU_FALSE;
	}
	else
	{
		trans[0] = cattr.trans[0];
		trans[1] = cattr.trans[1];
		trans[2] = cattr.trans[2];
		cattr.trans[0] = Strans[0];
		cattr.trans[1] = Strans[1];
		cattr.trans[2] = Strans[2];
		ncl_cutter_set_attr(&cattr);
		itrans = UU_TRUE;
	}
/*
.....Start the playback
*/
	ncl_motion_playback(Splay_mod,0,UU_NULL,&LW_tool_list,&LW_ntool);
	UN_playtax_save[0] = UN_playparm.spt[3];
	UN_playtax_save[1] = UN_playparm.spt[4];
	UN_playtax_save[2] = UN_playparm.spt[5];
	UN_last_clpt[0] = UN_clpt[UN_clfile];
	if (itrans)
	{
		cattr.trans[0] = trans[0];
		cattr.trans[1] = trans[1];
		cattr.trans[2] = trans[2];
		ncl_cutter_set_attr(&cattr);
	}
	goto form;
/*
.....End of playback
*/
done:;
	UN_override_geo_attr[0] = -1;
	UN_override_geo_attr[1] = -1;
	if ((UN_anlz_geo[0] != -1 || UN_anlz_geo[1] != -1) && idrw == 1)
		ncl_redraw_geo(-1,1,0,0,UU_FALSE);
	UN_playback_active = UU_FALSE;
	UD_UNMARK(markval);
	return UU_SUCCESS;
}

/*********************************************************************
**   I_FUNCTION: OnPlayTog(fieldno,val,stat)
**      Callback function Motion Playback form fields.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnPlayTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_default_method(fieldno, val, stat);
/*
.....Enable correct fields
.....based on toggle field value
*/
	switch(*fieldno)
	{
	case FPMD:
		if (Smode == 1) ud_set_traverse_mask(3,UU_TRUE);
		else ud_set_traverse_mask(3,UU_FALSE);
		break;
	case FPSH:
		if (*(val->frmint) == 0 || *(val->frmint) == 1)
		{
			Sshade[0] = Sshade[1] = Sshade[2] = *(val->frmint);
			ud_update_answer(FPCS,&Sshade[0]);
			ud_update_answer(FPKS,&Sshade[1]);
			ud_update_answer(FPHS,&Sshade[2]);
			ud_set_traverse_mask(FPCS,UU_TRUE);
			ud_set_traverse_mask(FPKS,UU_TRUE);
			ud_set_traverse_mask(FPHS,UU_TRUE);
			ud_set_traverse_mask(FPTR,Sshade[0]);
			ud_set_traverse_mask(FPCT,Sshade[0]);
			ud_set_traverse_mask(FPKT,Sshade[1]);
			ud_set_traverse_mask(FPHT,Sshade[2]);
		}
		else
		{
			ud_set_traverse_mask(FPCS,UU_FALSE);
			ud_set_traverse_mask(FPKS,UU_FALSE);
			ud_set_traverse_mask(FPHS,UU_FALSE);
			ud_set_traverse_mask(FPTR,UU_FALSE);
			ud_set_traverse_mask(FPCT,UU_FALSE);
			ud_set_traverse_mask(FPKT,UU_FALSE);
			ud_set_traverse_mask(FPHT,UU_FALSE);
		}
		break;
	case FPCS:
	case FPKS:
	case FPHS:
		if (*(val->frmint) == 1) ud_set_traverse_mask(*fieldno+1,UU_TRUE);
		else ud_set_traverse_mask(*fieldno+1,UU_FALSE);
		break;
	case FPTR:
		Strans[0] = Strans[1] = Strans[2] = *(val->frmint);
		ud_update_answer(FPCT,&Strans[0]);
		ud_update_answer(FPKT,&Strans[1]);
		ud_update_answer(FPHT,&Strans[2]);
		break;
	default:
		break;
    }
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : nclu_playfile()
**       Processes the NCL PLAYBACK FILE form.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_playfile()
{
	int stat,isav,irtn,status,n,i,markval,modals[20],icurpt,nt,imach,nc;
	UN_clstruc *clst,*clen,*tmpst,*tmpen,*iclpt[4];
	char buf[80];
	char *seqerr = "Could not find sequence: %s";
	char *rindex(),*ux_getenv(),*p,*q;
	UM_int2 jfl,istat,iclf;
	UX_pathname dir, fname;
/*
.....Set up form fields
*/
	static char traverse[]     = {1,0,0,0,0, 1,0,0,0, 1,0,0,0};
	static char display[]      = {1,0,0,1,1, 1,1,1,1, 1,1,1,1};
	UD_FSTAT uj_browse_file();
	static UD_METHOD methods[] = {OnFilTog,OnOption,OnFilTog,UU_NULL,
											uj_browse_file,
											OnFilTog,OnSequnc,UU_NULL,UU_NULL,
	                              OnFilTog,OnSequnc,UU_NULL,UU_NULL};
	static char called[]       = {6,6,6,6,6, 6,6,6,6, 6,6,6,6};
	static int option;
	static int *ans[] = {(int *)&Slist, (int *)&option,(int *)&Smach,
								(int *)&Stclfil[0],
								(int *)&option, &Sfstrt,
	                     (int *)&option, (int *)&Stsseq[0], (int *)&Stscmd[0],
								&Send, (int *)&option,
	                     (int *)&Steseq[0], (int *)&Stecmd[0]};
/*
.....Initialize clfile type indexes
*/
	for (i=0;i<T_END;i++) Sfidx[i] = 0;
/*
.....Trap reject op
*/
	UD_MARK(markval,UU_TRUE);
	if (markval != 0) goto done;
/*
.....Get name of temporary clfile
*/
	strcpy (Stempname,"ncltemp");
	p = ux_getenv("NCL_TEMP_CLFILE",UX_NPRTERRS);
	if (p != UU_NULL)
	{
		q = strchr(p,'%');
		if (q != UU_NULL)
		{
			if (*(q+1) == 's')
			{
				nc = q - p;
				if (nc == 0) Stempname[0] = '\0';
				else strncpy(Stempname,p,nc);
				strcat(Stempname,UL_program);
				q += 2;
				strcat(Stempname,q);
			}
		}
	}
/*
.....Set up the field entries
*/
	if (LW_nclipv == LW_STANDALONE && UN_playfile_src == T_CURRENT)
		UN_playfile_src = T_EXTERNAL;
	Ssrc = UN_playfile_src; strcpy(Stclfil,Sclfil);
	Sfstrt = UN_playfile_start; strcpy(Stsseq,Sfsseq); strcpy(Stscmd,Sfscmd);
	Send = UN_playfile_end; strcpy(Steseq,Sfeseq); strcpy(Stecmd,Sfecmd);
	Sscanned = UU_FALSE;
/*
.....Initialize source list
*/
	Slist.item = (char **)uu_malloc((T_END)*sizeof(char *));
	if (Slist.item == UU_NULL) goto nomem;
	Slist.answer = (char *)uu_malloc(sizeof(char)*80);
	if (Slist.answer == UU_NULL) goto nomem;
/*
.....Store source list toggles
*/
	n = 0;
	jfl = 367; getifl(&jfl,&istat);
	for (i=0;i<T_END;i++)
	{
		if (i != T_REVERSED || istat != 0)
		{
			if (LW_nclipv == LW_INPROCESS || i == T_EXTERNAL || i == T_MCD ||
				i == T_APTSRC || i == T_UGII || i == T_CATV4 || i == T_CATV5 ||
				i == T_MASTER)
			{
#if UU_COMP != UU_WIN2K
				if (i != T_MCD && i != T_POSTED)
#endif
				{
					Slist.item[n] = (char *)uu_malloc(sizeof(char)*80);
					strcpy(Slist.item[n], Ssrctxt[i]);
					if (i == Ssrc) strcpy(Slist.answer,Ssrctxt[i]);
					Sfidx[i] = n;
					n++;
				}
			}
		}
	}
	Slist.num_item = n;
/*
.....Store machine list toggles
*/
	n = 0;
	Smach.item = UU_NULL;
	Smach.num_item = 0;
	if (LW_nclipv==LW_INPROCESS)
	{
/*
........Get machine numbers
*/
		if (UL_pworks_nmach == 0)
		{
			ncl_play_initscan(modals,iclpt,&icurpt,UU_FALSE);
			ncl_motion_playback(modals,5,UU_NULL,UU_NULL,&nt);
			ncl_play_resetscan(iclpt,icurpt);
		}
/*
........Initialize machine list
*/
		if (UL_pworks_nmach > 1)
		{
			Smach.item = (char **)uu_malloc(UL_pworks_nmach*sizeof(char *));
			if (Smach.item == UU_NULL) goto nomem;
			Smach.answer = (char *)uu_malloc(sizeof(char)*80);
			if (Smach.answer == UU_NULL) goto nomem;
/*
........Assign machine numbers
*/
			for (i=0;i<UL_pworks_nmach;i++)
			{
				Smach.item[i] = (char *)uu_malloc(sizeof(char)*80);
				sprintf(Smach.item[i],"%d",UL_pworks_machs[i]);
				if (i == Sselmach) strcpy(Smach.answer,Smach.item[i]);
			}
			Smach.num_item = UL_pworks_nmach;
		}
	}
/*
.....Enable correct fields
.....based on DISPLAY field value
*/
form:;
	traverse[FFIL] = 0;
	traverse[FBRO] = 0;
	display[FMAC] = traverse[FMAC] = 0;
	if (Ssrc == T_EXTERNAL || Ssrc == T_MCD || Ssrc == T_APTSRC ||
		Ssrc == T_UGII || Ssrc == T_CATV4 || Ssrc == T_CATV5 || Ssrc == T_MASTER) 
	{
		traverse[FFIL] = 1;
		traverse[FBRO] = 1;
	}
	traverse[FOPT] = display[FOPT] = 0;
	if (Ssrc == T_MCD || Ssrc == T_POSTED)
	{
		traverse[FOPT] = display[FOPT] = 1;
	}
	if (Ssrc == T_SIMULATE)
	{
		if (UL_pworks_nmach == 0)
		{
			ncl_play_initscan(modals,iclpt,&icurpt,UU_FALSE);
			ncl_motion_playback(modals,5,UU_NULL,UU_NULL,&nt);
			ncl_play_resetscan(iclpt,icurpt);
		}
		if (UL_pworks_nmach > 1)
			display[FMAC] = traverse[FMAC] = 1;
	}

	traverse[FSSB] = traverse[FSSQ] = traverse[FSCM] = 0;
	if (Sfstrt == 1) traverse[FSSB] = traverse[FSSQ] = 1;
	if (Sfstrt == 2) traverse[FSCM] = 1;

	traverse[FESB] = traverse[FESQ] = traverse[FECM] = 0;
	if (Send == 1) traverse[FESB] = traverse[FESQ] = 1;
	if (Send == 2) traverse[FECM] = 1;
/*
.....set file filter for browse button
*/
	S_set_filter(Ssrc);
/*
.....Get the Form input
*/
	status = ud_form1("playfile.frm", ans, ans, methods, called, display, traverse);
	if (status==-1)
		goto done;
	ul_to_upper(Stsseq); ul_to_upper(Stscmd);
	ul_to_upper(Steseq); ul_to_upper(Stecmd);
/*
.....Reversed clfile
*/
	isav = UN_clfile;
	LW_mach_desc.type = -1;
	if (Ssrc == T_REVERSED)
	{
		iclf = 0;
		tmpst = UU_NULL; tmpen = UU_NULL;
		clrev (&iclf,&tmpst,&tmpen);
		UN_clfile = iclf;
	}
/*
.....Internal clfile
*/
	else if (Ssrc == T_CURRENT || Ssrc == T_SIMULATE || Ssrc == T_POSTED ||
		Ssrc == T_REVERSED)
	{
		UN_clfile = 0;
	}
/*
.....Load an external clfile
*/
	else
	{
		if (Ssrc == T_MCD)
			stat = nclu_load_mcd(Stclfil,Smdffil,Scutfil);
		else
			stat = nclu_load_clfile(Stclfil,Ssrc);
		if (stat != UU_SUCCESS)
		{
			UN_clfile = isav;
			goto form;
		}
	}
/*
.....Wipe out the previous tool list
*/
	if (Ssrc == T_EXTERNAL || Ssrc == T_MCD || Ssrc == T_POSTED ||
		Ssrc == T_APTSRC || Ssrc == T_UGII || Ssrc == T_CATV4 ||
		Ssrc == T_CATV5 || Ssrc == T_MASTER || Ssrc != UN_playfile_src)
	{
		if (LW_ntool != 0)
		{
			uu_list_free(&LW_tool_list);
			uu_list_init(&LW_tool_list,sizeof(UN_cutter_list),50,50);
			LW_ntool = 0;
		}
	}
/*
.....Set up the starting and ending
.....clfile locations for playback
*/
	stat = ncl_get_clseq(Stsseq,Stscmd,Sfstrt,Steseq,Stecmd,Send,&clst,&clen);
	if (stat != 0) UN_clfile = isav;
	if (stat == 1)
	{
		if (Sfstrt == 1) sprintf(buf,seqerr,Stsseq);
		else if (Sfstrt == 2) sprintf(buf,seqerr,Stscmd);
		else sprintf(buf,seqerr," ");
		ud_wrerr(buf);
		goto form;
	}
	else if (stat == 2)
	{
		if (Send == 1) sprintf(buf,seqerr,Steseq);
		else if (Send == 2) sprintf(buf,seqerr,Stecmd);
		else sprintf(buf,seqerr," ");
		ud_wrerr(buf);
		goto form;
	}
	if (Ssrc == T_SIMULATE || Ssrc == T_POSTED)
	{
		UN_clfile_start = 0; UN_clfile_end = 0;
	}
	else
	{
		UN_clfile_start = clst; UN_clfile_end = clen;
	}
	UN_clfile_current = UN_clfile_start;
	UN_clfile_curpt = 0;
	UN_last_clpt[0] = UN_last_clpt[1] = 0;
/*
.....Load the current clfile parameters
.....when starting from a "*Command"
........Done in nclu_playback now
*/
/*	if (Sfstrt == 2) ncl_fill_sequnc(clst);*/
/*
.....Create simulation file from current clfile
*/
	if (Ssrc == T_SIMULATE || Ssrc == T_POSTED)
	{
/*
.....Load the current clfile parameters
.....when starting from a "*Command"
*/
		if (Sfstrt == 2) ncl_fill_sequnc(clst);
/*
........Create simulation file
*/
		n = 0;
		if (Ssrc == T_POSTED) n = 1;
		if (UL_pworks_nmach > 1)
			ul_to_number(Smach.item[Sselmach],&imach);
		else
			imach = -1;
		irtn = ncl_create_simulate(&clst,&clen,n,imach);
		if (irtn != UU_SUCCESS)
		{
			UN_clfile = isav;
			goto form;
		}
		UN_clfile = 1;
	}
/*
.....Save the form entries
*/
	UN_playfile_src = Ssrc; strcpy(Sclfil,Stclfil);
	UN_playfile_start = Sfstrt; strcpy(Sfsseq,Stsseq); strcpy(Sfscmd,Stscmd);
	UN_playfile_end = Send; strcpy(Sfeseq,Steseq); strcpy(Sfecmd,Stecmd);
		
	strcpy(NCL_tpfile, Stclfil);
/*
......we need save toolpath directory too
*/
	ul_break_fname(Stclfil, NCL_tpdir, fname);
	if (NCL_tpdir[0]=='\0')
		getcwd(NCL_tpdir,UX_MAX_PATH_LEN);
	ud_update_win_title();
/*
.....Update NCLIPV monitor form
.....if Simulation file is active
.....(Displays the correct Machine Axes fields)
*/
	if (LW_active && LW_monitor &&
		(LW_monitor_field[IPVMON_LINAXS] || LW_monitor_field[IPVMON_ROTAXS]))
	{
		ul_ipv_monitor_close();
		ul_ipv_monitor_form();
	}
	goto done;
/*
.....Could not allocate memory
*/
nomem:
	ud_wrerr("Could not allocate memory for form.");
	goto done;
/*
.....End of routine
*/
done:
	ud_free_flist(&Slist);
	if (Smach.item != UU_NULL) ud_free_flist(&Smach);
	uj_browse_reset_filter(FBRO);
	if (Ssrc != T_MCD && Ssrc != T_POSTED && Ssrc != T_EXTERNAL &&
		Ssrc != T_APTSRC && Ssrc != T_UGII && Ssrc != T_MASTER) ncl_mcd_free();
/*
........Delete temporary cutter file
*/
	if (Scutfil[0] == '\0' && Sscanned) ux_delete("ncltemp.dat",UX_NPRTERRS);
	UD_UNMARK(markval);
	return(UU_SUCCESS);
}

/*********************************************************************
**   E_FUNCTION: nclu_set_external_clfile(type,clfile)
**      This routine sets the default Source and File fields for the
**      Playback File form.
**   PARAMETERS
**       INPUT  :
**          type     = Source file type, refer to the T_ definitions
**                     at the top of the file, i.e. T_EXTERNAL.
**          clfile   = Name of external motion file that is loaded.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void nclu_set_external_clfile(type,clfile)
int type;
char *clfile;
{
	UN_playfile_src = type;
	if (UN_playfile_src != 0) UN_clfile = 1;
	else UN_clfile = 0;
	strcpy(Sclfil,clfile);
}

/*********************************************************************
**   E_FUNCTION: nclu_mcd_options(cmach,cutfil,kflag)
**      This routine controls the selection/creation of a cutter data
**      file when converting from an MCD file to a simulation file.
**   PARAMETERS
**       INPUT  :
**          kflag    = UU_TRUE = Open display type form.
**                     UU_FALSE = Open standalone form.
**       OUTPUT :
**          cmach    = Machine name entered in form.
**          cutfil   = Cutter filename entered in form.
**   RETURNS:
**          UU_SUCCESS if form data is accepted, UU_FAILURE if form
**          is rejected.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int nclu_mcd_options(cmach,cutfil,kflag)
char *cmach,*cutfil;
UU_LOGICAL kflag;
{
	int status,markval;
	static int *ans[] = {
		UU_NULL,&Sscan,(int *)&Sscanfil,UU_NULL, (int *)&Smdffil,UU_NULL,UU_NULL,
		(int *)&Scutfil,UU_NULL,UU_NULL, (int *)&Stedcmd, &Sloadmcd};
	static UD_METHOD methods[] = {
		OnOpScan,OnOpTog,UU_NULL,OnOpBrowse, UU_NULL,OnOpEdit,OnOpBrowse,
		UU_NULL,OnOpEdit,OnOpBrowse, UU_NULL, UU_NULL, OnOpClose};
	static char called[]       = {6,6,6,6, 6,6,6, 6,6,6, 6, 6};
	static char traverse[]     = {1,1,1,1, 1,1,1, 1,1,1, 1, 1};
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	if (Sfrm1 != 0) goto done;
/*
.....Trap reject op
*/
	if (!kflag)
	{
		UD_MARK(markval,UU_TRUE);
		if (markval != 0) goto failed;
	}
/*
.....Set up traverse flags
*/
	traverse[FOSC3] = traverse[FOSC4] = Sscan;
/*
.....Display only form
*/
	if (kflag)
	{
		Sfrm1 = ud_form_display1("mcdoption.frm", ans, ans, methods, called,
			UU_NULL, traverse);
		if (Sfrm1 == -1) goto nofrm;
	}
/*
.....Standard form
*/
	else
	{
		status = ud_form1("mcdoption.frm", ans, ans, methods, called,
			UU_NULL, traverse);
		if (status == -1) goto failed;
		strcpy(cmach,Smdffil);
		strcpy(cutfil,Scutfil);
	}
	goto done;
/*
.....Could not load form
*/
nofrm:
	ud_wrerr("Could not load 'mcdoption.frm'.");
	status = UU_FAILURE;
	goto done;
/*
.....User rejected form
*/
failed:
	status = UU_FAILURE;
/*
.....End of routine
*/
done:
	if (!kflag)
	{
		UD_UNMARK(markval);
	}
	return(status);
}


/*********************************************************************
**   I_FUNCTION: OnFilTog(fieldno,val,stat)
**      Callback function Motion Playback File form fields.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT  OnFilTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;

	ud_default_method(fieldno, val, stat);
/*
.....Enable correct fields
.....based on toggle field value
*/
	switch(*fieldno)
	{
	case FSRC:
/*
........Set file masks
*/
		for (i=0;i<=T_REVERSED;i++)
		{
			if (strcmp(val->frmstr,Ssrctxt[i]) == 0) break;
		}
		Ssrc = i;
		if (Ssrc == T_EXTERNAL || Ssrc == T_MCD || Ssrc == T_APTSRC ||
			Ssrc == T_UGII || Ssrc == T_CATV4 || Ssrc == T_CATV5 ||
			Ssrc == T_MASTER)
		{
			uj_browse_reset_filter(FBRO);
			S_set_filter(Ssrc);
			ud_set_traverse_mask(FFIL,UU_TRUE);
			ud_set_traverse_mask(FBRO,UU_TRUE);
		}
		else 
		{
			ud_set_traverse_mask(FFIL,UU_FALSE);
			ud_set_traverse_mask(FBRO,UU_FALSE);
		}
/*
........Set Option masks
*/
		if (Ssrc == T_MCD || Ssrc == T_POSTED)
		{
			ud_set_display_mask(UD_INPUTF,FOPT,UU_TRUE);
			ud_set_traverse_mask(FOPT,UU_TRUE);
		}
		else
		{
			ud_set_display_mask(UD_INPUTF,FOPT,UU_FALSE);
			ud_set_traverse_mask(FOPT,UU_FALSE);
		}
/*
........Set Machine masks
*/
		if (Ssrc == T_SIMULATE && UL_pworks_nmach > 1)
		{
			ud_set_display_mask(UD_INPUTF,FMAC,UU_TRUE);
			ud_set_traverse_mask(FMAC,UU_TRUE);
		}
		else
		{
			ud_set_display_mask(UD_INPUTF,FMAC,UU_FALSE);
			ud_set_traverse_mask(FMAC,UU_FALSE);
		}
		break;
	case FSTR:
		if (Sfstrt == 1)
		{
			ud_set_traverse_mask(FSSB,UU_TRUE);
			ud_set_traverse_mask(FSSQ,UU_TRUE);
		}
		else
		{
			ud_set_traverse_mask(FSSB,UU_FALSE);
			ud_set_traverse_mask(FSSQ,UU_FALSE);
		}
		if (Sfstrt == 2) ud_set_traverse_mask(FSCM,UU_TRUE);
		else ud_set_traverse_mask(FSCM,UU_FALSE);
		break;
	case FEND:
		if (Send == 1)
		{
			ud_set_traverse_mask(FESB,UU_TRUE);
			ud_set_traverse_mask(FESQ,UU_TRUE);
		}
		else
		{
			ud_set_traverse_mask(FESB,UU_FALSE);
			ud_set_traverse_mask(FESQ,UU_FALSE);
		}
		if (Send == 2) ud_set_traverse_mask(FECM, UU_TRUE);
		else ud_set_traverse_mask(FECM,UU_FALSE);
		break;
/*
........Machine selection
*/
	case FMAC:
		for (i=0;i<UL_pworks_nmach;i++)
		{
			if (strcmp(val->frmstr,Smach.item[i]) == 0) break;
		}
		Sselmach = i;
		break;

	default:
		break;
    }
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnOption(fieldno,val,stat)
**      This routine controls the selection/creation of a cutter data
**      file when converting from an MCD file to a simulation file.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnOption(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Initialize routine
*/
	if (Sfrm1 != 0) goto done;
/*
.....Display the form
*/
	nclu_mcd_options(&Smdffil,Scutfil,UU_TRUE);
/*
.....End of routine
*/
done:
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnOpScan(fieldno,val,stat)
**      This routine scans the internal or an external clfile in order
**      to automatically generate the Pted cutter file.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnOpScan(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int ntl,isav,status;
	UU_LOGICAL iflg;

/*
	if ((Sfrm1==0)&&(fieldno!=UU_NULL))
	{
		*fieldno = -1;
		return (UD_FLDOK);
	}
*/
/*
.....Initialize routine
*/
	if (fieldno == UU_NULL) iflg = UU_FALSE;
	else iflg = UU_TRUE;
/*
.....Delete previous version of temporary tool file
*/
	if (Scutfil[0] == '\0' && Sscanned) ux_delete("ncltemp.dat",UX_NPRTERRS);
	Sscanned = UU_FALSE;
/*
.....Load external clfile for scanning
*/
	isav = UN_clfile;
	if (Sscan == 1)
	{
		status = nclu_load_clfile(Sscanfil,Ssrc);
		if (status != UU_SUCCESS) goto done;
	}
	else
		UN_clfile = 0;
/*
.....Create the cutter file
*/
	ntl = ncl_mcd_create_toolfile(Scutfil,iflg);
	if (ntl != -1) Sscanned = UU_TRUE;
/*
.....Update the Machine number
*/
	if (iflg)
	{
		strcpy(Smdffil,UL_pworks_mdf);
		ud_dispfrm_update_answer(Sfrm1,FOMF1,Smdffil);
	}
/*
.....End of routine
*/
done:;
	UN_clfile = isav;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnOpTog(fieldno,val,stat)
**      Callback function when Option toggle field is changed.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT  OnOpTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
	if (Sfrm1==0)
	{
		*fieldno = -1;
		return (UD_FLDOK);
	}
*/
	ud_default_method(fieldno, val, stat);
/*
.....Enable correct fields
.....based on toggle field value
*/
	switch(*fieldno)
	{
	case FOSC2:
		ud_setfrm_traverse_mask(Sfrm1,FOSC3,Sscan);
		ud_setfrm_traverse_mask(Sfrm1,FOSC4,Sscan);
		break;
	default:
		break;
    }
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnOpBrowse(fieldno, val, stat)
**       Method called when the Browse button is pushed from the
**       MCD Options form.
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
static UD_FSTAT OnOpBrowse(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int inum,ifld;
	char *p,*ux_getenv(),*fnam,title[20];
	char ext[UX_SUFFIX_LEN],ext1[UX_SUFFIX_LEN],descrip[80];

/*
	if (Sfrm1==0)
	{
		*fieldno = -1;
		return (UD_FLDOK);
	}
*/
/*
.....Set proper file filter
*/
	switch (*fieldno)
	{
/*
.....Scan clfiles
*/
	case FOSC4:
		fnam = Sscanfil;
		ifld = FOSC3;
		strcpy(title,"Clfiles");
		strcpy(ext,"*.");
		strcpy(descrip, "Clfiles (");
		p = ux_getenv("UL_CLFILE1_SUFFIX",UX_NPRTERRS);
		if (p != UU_NULL)
		{
			strcpy(ext1,p);
			ul_remove_quotes(ext1);
			strcat(ext,ext1);
		}       
		else strcat(ext,"cl");
		strcat(descrip, ext);
		strcat(descrip, ")|Secondary Clfiles (*.");
			
		p = ux_getenv("UL_CLFILE2_SUFFIX",UX_NPRTERRS);
		strcat(ext,"|*.");
		if (p != UU_NULL)
		{
			strcpy(ext1,p);
			ul_remove_quotes(ext1);
			strcat(ext,ext1);
			strcat(descrip, ext1);
		}       
		else
		{
			strcat(ext,"cln");
			strcat(descrip, "cln");
		}
		strcat(descrip, ")");
		break;
/*
.....MDF files
*/
	case FOMF3:
		fnam = Smdffil;
		ifld = FOMF1;
		strcpy(title,"MDF Files");
		strcpy(ext,"*.mdf");
		strcpy(descrip,"MDF Files (*.MDF)");
		break;
/*
.....Cutter files
*/
	case FOCF3:
		fnam = Scutfil;
		ifld = FOCF1;
		strcpy(title,"Cutter Data Files");
		strcpy(ext,"*.dat");
		strcpy(descrip,"Cutter Data Files (*.dat)");
		break;
	}
/*
.....Display the browser and
.....Get the filename
*/
	ud_get_filename(title,title,ext,fnam,&inum,descrip, 1,UU_FALSE);
	if (fnam[0] != '\0') ud_dispfrm_update_answer(Sfrm1,ifld,fnam);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnOpEdit(fieldno, val, stat)
**       Method called when the Edit button is pushed from the
**       MCD Options form.
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
static UD_FSTAT OnOpEdit(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int mode,fstat,irtn,ival;
	UU_LOGICAL found,ichdir;
	char *p,*q,*strchr(),*strstr();
	char *list=UU_NULL;
	UX_pathname fnam,msg,path;

/*
	if (Sfrm1==0)
	{
		*fieldno = -1;
		return (UD_FLDOK);
	}
*/
/*
.....Determine active vield
*/
	switch (*fieldno)
	{
/*
.....Edit MDF file
*/
	case FOMF2:
		irtn = ul_to_number(Smdffil,&ival);
/*
........PWORKS_n.MDF filename specified
........Break out number
*/
		if (irtn != UU_SUCCESS)
		{
			p = strstr(Smdffil,"PWORKS_");
			if (p != UU_NULL)
			{
				p += 7;
				strcpy(fnam,p);
				q = strchr(fnam,'.');
				if (q != 0) *q = '\0';
				irtn = ul_to_number(p,&ival);
				if (irtn == UU_SUCCESS) sprintf(Smdffil,"%d",ival);
				else strcpy(Smdffil,fnam);
			}
			irtn = UU_SUCCESS;
		}
		else
			sprintf(Smdffil,"%d",ival);
/*
........Create MDF file name
*/
		if (irtn == UU_SUCCESS)
		{
			sprintf(fnam,"PWORKS_%s.MDF",Smdffil);
/*
........Make sure local file exists
*/
			ichdir = S_chdir(Stclfil,Ssrc,0);
			mode = UX_EXISTS|UX_READ;
			irtn = ux_file_inquire(UU_NULL,UU_NULL,fnam,UU_NULL,UU_NULL,&mode,
				&fstat,msg,UX_NPRTERRS);
/*
...........Try system directory
*/
			if (mode == (mode|UX_NEXISTS))
			{
				ul_get_full_dir("PWORKS_DATA",path);
				mode = UX_EXISTS|UX_READ;
				irtn = ux_file_inquire(UU_NULL,path,fnam,UU_NULL,UU_NULL,&mode,
					&fstat,msg,UX_NPRTERRS);
				if (mode == (mode|UX_NEXISTS)) irtn = UU_FAILURE;
			}
		}
/*
........Error accessing MDF file
*/
		if (irtn != UU_SUCCESS)
		{
			sprintf(msg,
				"MDF file '%s' does not exist in local or system directory.",fnam);
			ud_wrerr(msg);
		}
/*
........Spawn MPOST to access MDF file
*/
		else
		{
			ux_get_syspath("NCL_POST",&list,path,&found,UX_NPRTERRS);
			if (found)
			{
				ux_cat_paths(path,"mpost",path,UX_NPRTERRS);
				ul_remove_quotes(path);
			}
			else strcpy(path,"mpost");
			sprintf(msg,"\"%s\" %s",path,Smdffil);
			ul_spawn(msg,0);
		}
		if (ichdir) S_chdir(Stclfil,Ssrc,1);
		break;
/*
.....Edit Cutter file
*/
	case FOCF2:
		if (Scutfil[0] == '\0') ul_edit("ncltemp.dat");
		else ul_edit(Scutfil);
		break;
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnOpClose()
**      This routine is called when the MCD Options form is closed.
**   PARAMETERS
**       INPUT  : none.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnOpClose()
{
	Sfrm1 = 0;
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnSequnc(fieldno,val,stat)
**      This routine lists the available sequences and allows the user
**      to select one.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSequnc(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,inc,ipt,nseq;

	int *ans[2];
	static UD_METHOD methods[] = {OnSelect1,OnClose};
	static char called[]       = {6,6};
	static char traverse[]     = {1};
	static char disp[]         = {1,0, 1};
/*
.....Initialize routine
*/
	if (Sfrm != 0) goto done;
	ipt = 0;
	if (Ssrc == T_EXTERNAL || Ssrc == T_MCD || Ssrc == T_APTSRC ||
		Ssrc == T_UGII || Ssrc == T_CATV4 || Ssrc == T_CATV5 ||
		Ssrc == T_MASTER) ipt = 1;
	nseq = UN_clseq[ipt] + 1;
	if (nseq <= 0) goto done;
/*
.....Allocate memory for form list
*/
	Sgeom.item = UU_NULL;
	Sgeom.item = (char **)uu_malloc(nseq * sizeof(char *));
	if (Sgeom.item == UU_NULL) goto nomem;
	Sgeom.answer = (char *)uu_malloc(sizeof(char)*80);
	if (Sgeom.answer == UU_NULL) goto nomem;
/*
.....Store layer list in form list
*/
	for (i=0;i<nseq;i++)
	{
		Sgeom.item[i] = (char *)uu_malloc(sizeof(char)*80);
		strcpy(Sgeom.item[i], UN_clseq_label[ipt][i]);
		if (i == 0) strcpy(Sgeom.answer, UN_clseq_label[ipt][i]);
	}
	Sgeom.num_item = nseq;
/*
.....Setup visible fields
*/
	Sfld = *fieldno + 1;
	if (*fieldno == FSSB) inc = 0;
	else inc = 1;
	disp[inc] = 1;
	disp[1-inc] = 0;
/*
.....Setup the default answers
*/
	ans[0] = (int *)&Sgeom;
/*
.....Display the form
*/
	Sfrm = ud_form_display1("sequncsel.frm", ans, ans, methods, called, disp,
		traverse);
	if (Sfrm == -1) goto nofrm;
	goto done;
/*
.....Could not allocate memory
*/
nomem:
	ud_wrerr("Could not allocate memory for form.");
	goto done;
/*
.....Could not load form
*/
nofrm:
	ud_wrerr("Could not load 'sequncsel.frm'.");
	goto done;
/*
.....End of routine
*/
done:
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnSelect1(fieldno,val,stat)
**      Callback function for when a SEQUNC is selected from the form list.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSelect1(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Store the Starting Sequence
*/
	if (Sfld == FSSQ)
	{
		strcpy(Stsseq,val->frmstr);
		ud_update_answer(FSSQ,Stsseq);
		ud_update_form(0);
	}
/*
.....Store the Starting Sequence
*/
	else if (Sfld == FESQ)
	{
		strcpy(Steseq,val->frmstr);
		ud_update_answer(FESQ,Steseq);
		ud_update_form(0);
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnClose()
**      Callback function for the Close button.
**   PARAMETERS
**       INPUT  : none.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnClose()
{
/*
.....Mark the form as closed
*/
	Sfrm = 0;
	ud_free_flist(&Sgeom);
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : nclu_playfeed()
**       Processes the FEEDRATE ANALYZATION form.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_playfeed()
{
/*
.....Set up form fields
*/
	UU_REAL tfeed[10],rnum;
	int status;
	int tgeo[2],tcolor[10],inc,i,inum,isw;

	int *ans[23];
/*
.....Initialize form
*/
	ans[0] = &tgeo[0];
	ans[1] = &tgeo[1];
	inc = 2;
	for (i=0;i<10;i++)
	{
		ans[inc] = (int *)&tfeed[i];
		ans[inc+1] = &tcolor[i];
		inc = inc + 2;
	}
/*
.....Set up the field entries
*/
	tgeo[0] = UN_anlz_geo[0];
	tgeo[1] = UN_anlz_geo[1];
	for (i=0;i<10;i++)
	{
		UM_len_inttoext(UN_anlz_feed[i],tfeed[i]);
		tcolor[i] = UN_anlz_fcolor[i];
	}
/*
.....Get the Form input
*/
	status = ud_form("anlzfeed.frm", ans, ans);
	if (status==-1)
		return -1;
/*
.....Sort the feedrates
*/
	do
	{
		isw = 0;
		for (i=0;i<9;i++)
		{
			if (tfeed[i+1] < tfeed[i])
			{
				isw = 1;
				rnum = tfeed[i];
				tfeed[i] = tfeed[i+1];
				tfeed[i+1] = rnum;
				inum = tcolor[i];
				tcolor[i] = tcolor[i+1];
				tcolor[i+1] = inum;
			}
		}
	} while (isw == 1);
/*
.....Save the form entries
*/
	UN_anlz_geo[0] = tgeo[0];
	UN_anlz_geo[1] = tgeo[1];
	for (i=0;i<10;i++)
	{
		UM_len_exttoint(tfeed[i],UN_anlz_feed[i]);
		UN_anlz_fcolor[i] = tcolor[i];
	}
/*
.....Save modals file
*/
	S_save_pfeeds();
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_playinterp()
**       Processes the INTERPOLATION ANALYZATION form.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_playinterp()
{
/*
.....Set up form fields
*/
	int status;
	int tgeo[2],tcolor[5],tstyle[5],inc,i;
	int *ans[12];
/*
.....Initialize form
*/
	ans[0] = &tgeo[0];
	ans[1] = &tgeo[1];
	inc = 2;
	for (i=0;i<5;i++)
	{
		ans[inc] = (int *)&tcolor[i];
		ans[inc+1] = &tstyle[i];
		inc = inc + 2;
	}
/*
.....Set up the field entries
*/
	tgeo[0] = UN_anlz_geo[0];
	tgeo[1] = UN_anlz_geo[1];
	for (i=0;i<5;i++)
	{
		tcolor[i] = UN_anlz_icolor[i];
		tstyle[i] = UN_anlz_istyle[i];
	}
/*
.....Get the Form input
*/
	status = ud_form("anlzint.frm", ans, ans);
	if (status==-1)
		return -1;

/*
.....Save the form entries
*/
	UN_anlz_geo[0] = tgeo[0];
	UN_anlz_geo[1] = tgeo[1];
	for (i=0;i<5;i++)
	{
		UN_anlz_icolor[i] = tcolor[i];
		UN_anlz_istyle[i] = tstyle[i];
	}
/*
.....Save modals file
*/
	S_save_pinterp();
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_playclip()
**       Processes the PLAYBACK BOUNDING REGION form.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_playclip()
{
	int status;
/*
.....Set up form fields
*/
	UU_REAL rplane[5][4],uu_mag();
	int tside[5],tclip,inc,i,j,inum,ipl,nc,cside[6];
	int *ans[23];
	char tplane[5][96],*tpp,lerr[80];
	UM_f77_str fsym;
	struct NCL_nclpl_rec upl;
	struct NCLI_plane_rec npl;
/*
.....Initialize routine
*/
	cside[0] = 0; cside[1] = 0;
	cside[2] = 1; cside[3] = 1;
	cside[4] = 2; cside[5] = 2;
/*
.....Initialize form
*/
	ans[0] = &tclip;
	inc = 1;
	for (i=0;i<5;i++)
	{
		ans[inc] = (int *)&tplane[i][0];
		ans[inc+1] = &tside[i];
		inc = inc + 2;
	}
/*
.....Set up the field entries
*/
	tclip = UN_clip_enable;
	for (i=0;i<5;i++)
	{
		if (i < UN_clip_npl)
		{
			sprintf(tplane[i],"%g,%g,%g,%g",UN_clip_plane[i][0],
				UN_clip_plane[i][1],UN_clip_plane[i][2],UN_clip_plane[i][3]);
		}
		else
		{
			tplane[i][0] = '\0';
		}
		tside[i] = UN_clip_side[i];
	}
/*
.....Get the Form input
*/
form:;
	status = ud_form("playclip.frm", ans, ans);
	if (status==-1)
		return -1;
/*
.....Clip planes turned off
.....Don't check or save clip planes
*/
	if (tclip == 0)
	{
		UN_clip_enable = 0;
		goto done;
	}
/*
.....Verify planes are valid
*/
    ipl = 0;
	for (i=0;i<5;i++)
	{
      tpp = tplane[i];
		nc = strlen(tpp);
		ul_strip_blanks(tpp,&nc);
		if (nc != 0)
		{
         for (j=0;j<nc;j++)
            tpp[j] = islower(tpp[j]) ? toupper(tpp[j]) : tpp[j];
			if (ul_to_reals(rplane[ipl],&inum,4,tpp) != UU_SUCCESS)
			{
				UM_init_f77_str(fsym,tpp,nc);
				getkey(UM_addr_of_f77_str(fsym),&upl.key);
				if (upl.key == 0) goto invplane;
				ncl_retrieve_data_fixed(&upl);
				if (upl.rel_num != NCL_PLN_REL) goto invplane;

				ncl_wcstomcs(1,upl.nvec,upl.nvec);
				ncl_wcstomcs(0,upl.pt,upl.pt);
				UM_cc_inttoext(upl.pt,upl.pt);
				ncl_plane_to_nclpln(&upl,&npl);
				
				rplane[ipl][0] = npl.ijk[0];
				rplane[ipl][1] = npl.ijk[1];
				rplane[ipl][2] = npl.ijk[2];
				rplane[ipl][3] = npl.dist;
				inum = 4;
			}
			if (inum != 4) goto invplane;
			um_unitvc(rplane[ipl],rplane[ipl]);
			if (um_mag(rplane[i]) == 0.) goto invplane;
			if (rplane[ipl][cside[tside[i]]] == 0.) goto invside;
			ipl++;
		}
	}
/*
.....Save the form entries
*/
	UN_clip_enable = tclip;
	UN_clip_npl = ipl;
	for (i=0;i<ipl;i++)
	{
		for (j=0;j<4;j++)
		{
			UN_clip_plane[i][j] = rplane[i][j];
		}
		UN_clip_side[i] = tside[i];
	}
	goto done;
/*
.....Invalid plane plane
*/
invplane:;
	sprintf (lerr,"Invalid clip plane: %s",tplane[i]);
	ud_wrerr(lerr);
	goto form;
/*
.....Invalid clip side
*/
invside:;
	sprintf (lerr,"Clipping side parallel with plane: %s",tplane[i]);
	ud_wrerr(lerr);
	goto form;
/*
.....End of routine
*/
done:;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : rsmply()
**       Initializes clfile and motion playback variables.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
rsmply()
{
	UM_int2 icl;
/*
.....Initialize clfile SEQUNC stack
*/
	UN_clseq[0] = -1; UN_clseq[1] = -1;
	UN_clseq_cur[0] = -1; UN_clseq_cur[1] = -1;
	UN_clfile = 0;
	icl = 0; clclos(&icl);
	icl = 1; clclos(&icl);
	UN_playfile_src =  UN_playfile_start =  UN_playfile_end = 0;
	Sclfil[0] = '\0';
	Sfsseq[0] = '\0'; Sfscmd[0] = '\0';
	Sfeseq[0] = '\0'; Sfecmd[0] = '\0';
	UN_step_ptr = (UN_motseg *)0;
	ncl_reset_mplayback();
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_reset_mplayback()
**       Resets motion playback variables.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_reset_mplayback()
{
/*
.....Initialize the playback parameter structure
*/
	UN_clfile_start = UU_NULL;
	UN_clfile_current = UU_NULL;
	UN_clfile_curpt = 0;
	ncl_reset_playparm();
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_reset_playparm()
**       Resets the motion playback parameter structure.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_reset_playparm()
{
	int i;
	UN_cutter_list *tpt;
/*
.....Initialize the playback parameter structure
*/
	UN_playparm.ftyp = 1;
	UN_playparm.fedrat[0] = 0.;
	UN_playparm.fedrat[1] = 0.;
	UN_playparm.rpm = 0.;
	UN_playparm.spdir = 0;
	UN_playparm.rapid = 0;
	UN_playparm.cfl[0] = 1;
	UN_playparm.cfl[1] = 0;
	UN_playparm.cfl[2] = 0;
	UN_playparm.npt = 3;
	UN_playparm.ccmode = 0;
	UN_playparm.ccdir = 0;
	UN_playparm.coolnt = 0;
	UN_playparm.tlno = 0;
	UN_playparm.tlen = 0.;
	UN_playparm.sym[0][0] = '\0';
	UN_playparm.sym[1][0] = '\0';
	UN_playparm.sym[2][0] = '\0';
	for (i=0;i<5;i++)
	{
		UN_playparm.spt[i] = 0.;
	}
	UN_playparm.spt[5] = 1.;
/*
.....Transform FROM point thru MODSYS
.....Bobby  -  2/28/94
*/
	if (!LW_active)
	{
		ncl_mcstowcs(0,UN_playparm.spt,UN_playparm.spt);
		ncl_mcstowcs(1,&UN_playparm.spt[3],&UN_playparm.spt[3]);
	}

	if (LW_active && LW_ntool_sess != 0)
	{
		tpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_sess);
		for (i=0;i<7;i++) UN_playparm.cutr[i] = tpt[LW_tool_sess_act].cutter[i];
		for (i=0;i<LW_MAX_SPINDLE;i++) LW_act_tool[i] = 0;
	}
	else
	{
		for (i=0;i<8;i++) UN_playparm.cutr[i] = 0.;
	}

	for (i=0;i<10;i++)
	{
		UN_playparm.icyc[i] = 0;
		UN_playparm.rcyc[i] = 0.;
	}
	UN_playparm.cycret = 2;
	for (i=0;i<12;i++)
	{
		UN_playparm.tracut[i] = 0.;
	}
	UN_playparm.tracut[0] = 1.; UN_playparm.tracut[5] = 1.;
	UN_playparm.tracut[10] = 1.;
	for (i=0;i<4;i++) UN_playparm.axis[i] = 0.;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_create_simulate(clst,clen,ifl,mach)
**          This routine converts the internal clfile to a simulation
**			file which will be loaded into the secondary clfile.
**    PARAMETERS
**       INPUT  :
**          clst    = Starting clfile record for conversion.
**          clen    = Ending clfile record for conversion.
**          ifl     = 0 = Create simulation file from Pworks.
**                    1 = From Pted.
**          mach    = Machine number to create simulation for.
**                    -1 = Don't specify machine number on command line.
**       OUTPUT :
**          none.
**    RETURNS      : Returns UU_FAILURE on error, UU_SUCCESS
**                   otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_simulate(clst,clen,ifl,mach)
UN_clstruc **clst,**clen;
int ifl;
int mach;
{
	int irtn,i,nc;
	char buf[UX_MAX_PATH_LEN+40],tbuf[20];
	UM_int2 ierr;
	UM_f77_str label;
	UX_pathname path;
/*
.....Save temporary clfile
*/
	irtn = UU_SUCCESS;
	sprintf(path,"%s.cl",Stempname);
	nc = strlen(path);
	for (i=nc;i<UX_MAX_PATH_LEN;i++) path[i] = ' ';
	UM_init_f77_str(label,path,UX_MAX_PATH_LEN);
	clsave(UM_addr_of_f77_str(label),clst,clen,&ierr);
	if (ierr != 0)
	{
		ud_wrerr("Could not create simulation file.");
		goto failed;
	}
/*
.....Use PWORKS to create simulation or punch file
*/
	if (ifl == 0)
	{
		sprintf(buf,"%s.cl -clf:1 -nolis -nopri -nopun -sim:.sim",Stempname);
		if (mach != -1)
		{
			sprintf(tbuf," -mac:%d",mach);
			strcat(buf,tbuf);
		}
		ul_run_process ("NCL_POST","pworks",buf);
	}
	else
	{
		sprintf(buf,
			"%s.cl -clf:1 -nolis -nopri -pun:.pu1 -nosim -qui -mac:%s",
				Stempname,Smdffil);
		ul_run_process ("NCL_POST","pworks",buf);
		sprintf(buf,"%s.pu1",Stempname);
		nclu_load_mcd(buf,Smdffil,Scutfil);
		ux_delete(buf,UX_NPRTERRS);
	}
/*
.....Delete temporary clfile
*/
	sprintf(buf,"%s.cl",Stempname);
	ux_delete(buf,UX_NPRTERRS);
/*
.....Load simulation file
*/
	if (ifl == 0)
	{
		sprintf(buf,"%s.sim",Stempname);
		irtn = ncl_simulate_load(buf,&LW_mach_desc);
		if (irtn != 0)
		{
			ud_wrerr("Could not load simulation file.");
			goto failed;
		}
/*
.....Delete temporary simulation file
*/
		ux_delete(buf,UX_NPRTERRS);
	}
	goto done;
/*
.....End of routine
*/
failed:;
	irtn = UU_FAILURE;
done:;
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_playfile(file)
**          This routine returns the name of the external clfile being
**          used for playback.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          file    = Name of external clfile.
**    RETURNS      : Returns UU_TRUE if an external clfile is being used.
**                   UU_FALSE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_get_playfile(file)
char *file;
{
/*
.....Return name of external clfile
*/
	if (UN_clfile == 1)
	{
		strcpy(file,Stclfil);
		return(UU_TRUE);
	}
	else
	{
		file[0] = '\0';
		return(UU_FALSE);
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_clfile_src(tf_src,tf_file,tf_strt,ts_strt,
**                                        tf_end,ts_end,flist)
**          This routine returns the name of the clfile/mode being
**          used for playback.  It returns the playback file modes.
**
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          tf_src    = Current playback file mode.
**          tf_file   = Current playback file text.
**          tf_strt   = Starting position in clfile.
**          ts_strt   = Text of starting position.
**          tf_end    = Ending position in clfile.
**          ts_end    = Text of ending position.
**          flist     = UU_TRUE = Return listing style output.
**                      UU_FALSE = Return actual settings.
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_clfile_src(tf_src,tf_file,tf_strt,ts_strt,tf_end,ts_end,flist)
int *tf_src,*tf_strt,*tf_end;
char *tf_file,*ts_strt,*ts_end;
UU_LOGICAL flist;
{
/*
.....Return name of external clfile
*/
	*tf_src = UN_playfile_src;
	if (flist)
	{
		switch(UN_playfile_src)
		{
		case T_EXTERNAL:
		case T_MCD:
		case T_APTSRC:
		case T_UGII:
		case T_CATV4:
		case T_CATV5:
		case T_MASTER:
			ul_build_full_fname("",Stclfil,"",tf_file);
			break;
		case T_SIMULATE:
			strcpy(tf_file,"Simulated clfile");
			break;
		case T_POSTED:
			strcpy(tf_file,"Post-processed clfile");
			break;
		case T_REVERSED:
			strcpy(tf_file,"Reversed clfile");
			break;
		default:
			strcpy(tf_file,"Internal clfile");
			break;
		}
	}
	else
		ncl_get_playfile(tf_file);
/*
.....Return starting point
*/
	*tf_strt = UN_playfile_start;
	switch(UN_playfile_start)
	{
	case 0:
		if (flist) strcpy(ts_strt,"Beginning");
		else ts_strt[0] = '\0';
		break;
	case 1:
		if (flist) sprintf(ts_strt,"SEQUNC/%s",Sfsseq);
		else strcpy(ts_strt,Sfsseq);
		break;
	case 2:
		strcpy(ts_strt,Sfscmd);
		break;
	}
/*
.....Return ending point
*/
	*tf_end = UN_playfile_end;
	switch(UN_playfile_end)
	{
	case 0:
		if (flist) strcpy(ts_end,"Ending");
		else ts_end[0] = '\0';
		break;
	case 1:
		if (flist) sprintf(ts_end,"SEQUNC/%s",Sfeseq);
		else strcpy(ts_end,Sfeseq);
		break;
	case 2:
		strcpy(ts_end,Sfecmd);
		break;
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_set_clfile_src(tf_src,tf_file,tf_strt,ts_strt,
**                                        tf_end,ts_end)
**          This routine sets the clfile playback file settings.
**
**    PARAMETERS
**       INPUT  :
**          tf_src    = Current playback file mode.
**          tf_file   = Current playback file.
**          tf_strt   = Starting position in clfile.
**          ts_strt   = Text of starting position.
**          tf_end    = Ending position in clfile.
**          ts_end    = Text of ending position.
**       OUTPUT :
**          none
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_set_clfile_src(tf_src,tf_file,tf_strt,ts_strt,tf_end,ts_end)
int tf_src,tf_strt,tf_end;
char *tf_file,*ts_strt,*ts_end;
{
/*
.....Set the name of external clfile
*/
	UN_playfile_src = tf_src;
	if (UN_playfile_src != 0) UN_clfile = 1;
	else UN_clfile = 0;
	strcpy(Sclfil,tf_file);
/*
.....Return starting point
*/
	UN_playfile_start = tf_strt;
	if (UN_playfile_start == 1)
		strcpy(Sfsseq,ts_strt);
	else if (UN_playfile_start == 2)
		strcpy(Sfscmd,ts_strt);
/*
.....Return ending point
*/
	UN_playfile_end = tf_end;
	if (UN_playfile_end == 1)
		strcpy(Sfeseq,ts_end);
	else if (UN_playfile_end == 2)
		strcpy(Sfscmd,ts_end);
}

/*********************************************************************
**    I_FUNCTION     : S_set_filter(which)
**       This routine sets the browser filter for the playback file form.
**
**    PARAMETERS
**       INPUT  :
**          which    = Type of files to set filter for.
**       OUTPUT :
**          none
**    RETURNS      : none.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_set_filter(which)
int which;
{
	int nc;
	char *p,*ux_getenv();
	char sbuf[UX_SUFFIX_LEN],tbuf[UX_SUFFIX_LEN];
	UX_pathname descrip,filter,ext1;
/*
.....Set file filter for browse button
........MCD Files
*/
	if (which == T_MCD)
	{
		p = ux_getenv("UL_MCD_SUFFIX",UX_NPRTERRS);
		if (p != UU_NULL) strcpy(ext1,p);
		else strcpy(ext1,".pu*,.mcd");
		ud_format_file_filter(filter,ext1);
		sprintf(descrip, "MCD Files (%s)",filter);
	}
/*
.....External clfiles
*/
	else
	{
		filter[0] = '\0';
		descrip[0] = '\0';
		if (which == T_EXTERNAL)
		{
			p = ux_getenv("UL_CLFILE1_SUFFIX",UX_NPRTERRS);
			if (p != UU_NULL)
			{
				strcpy(ext1,p);
				ul_remove_quotes(ext1);
			}
			else
				strcpy(ext1,"cl");
			strcat(ext1,",");
			p = ux_getenv("UL_CLFILE2_SUFFIX",UX_NPRTERRS);
			if (p != UU_NULL) strcat(ext1,p);
			else strcat(ext1,"cln");
			ud_format_file_filter(sbuf,ext1);
			strcat(filter,sbuf);
			sprintf(tbuf,"Clfiles (%s)",sbuf);
			strcat(descrip,tbuf);

			strcat(descrip, "|Simulation Files (*.sim)");
			strcat(filter, "|*.sim");
			strcat(filter,"|"); strcat(descrip,"|");
		}
/*
........APT Source files
*/
		if (which == T_EXTERNAL || which == T_APTSRC || which == T_UGII)
		{
			strcat(descrip,"Apt Source Files (*.as,*.cla,*.aptsrc)");
			strcat(filter,"*.as,*.cla,*.aptsrc");
			strcat(filter,"|"); strcat(descrip,"|");
		}
/*
........Catia files
*/
		if (which == T_EXTERNAL || which == T_CATV4 || which == T_CATV5)
		{
			strcat(descrip,"Catia Files (*.clfile)");
			strcat(filter,"*.clfile");
			strcat(filter,"|"); strcat(descrip,"|");
		}
/*
........Mastercam files
*/
		if (which == T_EXTERNAL || which == T_MASTER)
		{
			strcat(descrip,"Mastercam Files (*.nci)");
			strcat(filter,"*.nci");
			strcat(filter,"|"); strcat(descrip,"|");
		}
		nc = strlen(filter);
		if (filter[nc-1] == '|')
		{
			filter[nc-1] = '\0';
			descrip[strlen(descrip)-1] = '\0';
		}
	}
/*
.....Set browser filter
*/
	uj_browse_set_filter(FBRO, filter, descrip);
}

/*********************************************************************
**    I_FUNCTION     : nclu_load_clfile(sfil)
**       This routine loads an external simulation/clfile.
**
**    PARAMETERS
**       INPUT  :
**          sfil     = Name of external file to load.
**          ityp     = 0,1,2,4,10 = Determine clfile type.
**                     3 = Simulation file.
**                     5 = APT Source file.
**                     6 = Unigraphics II file.
**                     7 = Catia V4 file.
**                     8 = Catia V5 file.
**                     9 = Mastercam file.
**       OUTPUT :
**          sfil     = Updated name of file with clfile extension.
**    RETURNS      : UU_FAILURE if could not load file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_load_clfile(sfil,ityp)
char *sfil;
int ityp;
{
	int nc,irtn,isav;
	UU_LOGICAL ichdir;
	UM_int2 jfl,istat;
	char *p,*q,*ux_getenv(),*rindex();
	char sbuf[80];
	UX_pathname lfil,buf;
	UM_f77_str label;
	static int iclfl[T_END]={0,0,0,0,0,2,4,3,5,6,0};
/*
.....Set secondary clfile active
*/
	ichdir = UU_FALSE;
	isav = UN_clfile;
	UN_clfile = 1;
	p = ux_getenv("UL_CLFILE1_SUFFIX",UX_NPRTERRS);
	q = rindex(sfil,'.');
	if (p != 0 && q == 0)
	{
		strcat(sfil,".");
		strcat(sfil,p);
	}
	nc = strlen(sfil);
/*
.....Clear out MCD file storage
*/
	ncl_mcd_free();
/*
.....Try and load as a Simulation file first
*/
	irtn = UU_FAILURE;
	if (ityp == T_EXTERNAL || ityp == T_SIMULATE)
		irtn = ncl_simulate_load(sfil,&LW_mach_desc);
/*
.....Load as a clfile
*/
	if (irtn != UU_SUCCESS && ityp == T_EXTERNAL)
	{
		UM_init_f77_str(label,sfil,nc);
		jfl = 0;
		clload(&UN_clfile,UM_addr_of_f77_str(label),&nc,&jfl,&istat);
		if (istat == 0) irtn = UU_SUCCESS;
	}
/*
.....Load as a foreign clfile
*/
	if (irtn != UU_SUCCESS)
	{
/*
........Make sure we are in the same directory
*/
		Ssrc = T_EXTERNAL;
		ichdir = S_chdir(sfil,T_EXTERNAL,0);
/*
........Convert foreign clfile
*/
		sprintf(lfil,"%s.cln",Stempname);
		sprintf(buf,"%s -CLFILE:%d -OBJECT:%s",sfil,iclfl[ityp],lfil);
		ul_run_process("NCL_POST","pwconv",buf);
/*
........Load converted clfile
*/
		nc = strlen(lfil);
		UM_init_f77_str(label,lfil,nc);
		jfl = 0;
		clload(&UN_clfile,UM_addr_of_f77_str(label),&nc,&jfl,&istat);
		if (istat != 0)
		{
			ul_short_filename(sfil,sbuf,40);
			sprintf(buf,"Could not load external clfile: %s",sbuf);
			ud_wrerr(buf);
			irtn = UU_FAILURE;
		}
		else irtn = UU_SUCCESS;
/*
.....Delete temporary clfile
*/
		ux_delete(lfil,UX_NPRTERRS);
/*
.....If ASCII file then
.....load a copy of it for source line reference
*/
		if (ul_is_binary(sfil) == 0) ncl_mcd_load(sfil);
	}
/*
.....End of routine
*/
	if (irtn != UU_SUCCESS) UN_clfile = isav;
	if (ichdir) S_chdir(sfil,T_EXTERNAL,1);
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     : nclu_load_mcd(sfil,mach,cfil)
**       This routine loads an external MCD file, first converting it
**       to a simulation file using Pted.
**
**    PARAMETERS
**       INPUT  :
**          sfil     = Name of external file to load.
**          mach     = MDF file name or number for reverse posting.
**          cfil     = Name of cutter file to use for reverse posting.
**       OUTPUT : none
**    RETURNS      : UU_FAILURE if could not load file.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_load_mcd(sfil,mach,cfil)
char *sfil,*mach,*cfil;
{
	int irtn,isav;
	UM_int2 ifl,ival;
	char buf[UX_MAX_PATH_LEN+40],lunit[10];
	UX_pathname simf,cutf;
	UU_LOGICAL ichdir;
/*
.....Set secondary clfile active
*/
	isav = UN_clfile;
	UN_clfile = 1;
/*
.....Scan the clfile for tools
.....if it has not been done yet
*/
	if (cfil[0] == '\0' && !Sscanned) OnOpScan(0,0,0);
/*
.....Make sure we are in the same directory
.....as the MCD file
*/
	ichdir = S_chdir(sfil,T_MCD,0);
/*
.....Use active Units in conversion
*/
	ifl = 264; getifl(&ifl,&ival);
	if (ival == 0) strcpy(lunit,"INCH");
	else strcpy(lunit,"MM");
/*
.....Use Pted to create simulation file
*/
	sprintf(simf,"%s.sim",Stempname);
	strcpy(cutf,cfil);
	if (cutf[0] == '\0') strcpy(cutf,"ncltemp.dat");
	sprintf(buf,"\"%s\" -mac:\"%s\" -cut:\"%s\" -sim:\"%s\" -units:%s ",sfil,
		mach,cutf,simf,lunit);
	if (strlen(Stedcmd) != 0)
	{
		strcat(buf,Stedcmd);
		strcat(buf," ");
	}
	strcat(buf,"-ex");
	ul_run_process ("NCL_POST","pted", buf);
/*
.....Load simulation file
*/
	irtn = ncl_simulate_load(simf,&LW_mach_desc);
	if (irtn != 0)
	{
		ud_wrerr("Could not load MCD file.");
		goto failed;
	}
/*
.....Delete temporary simulation file
*/
	ux_delete(simf,UX_NPRTERRS);
/*
.....Load MCD file if requested
*/
	if (Sloadmcd) ncl_mcd_load(sfil);
	else ncl_mcd_free();
	goto done;
/*
.....End of routine
*/
failed:;
	irtn = UU_FAILURE;
done:;
	if (irtn != UU_SUCCESS) UN_clfile = isav;
	if (ichdir) S_chdir(sfil,T_MCD,1);
	return(irtn);
}

/*********************************************************************
**    I_FUNCTION     : S_chdir(sfil,tsrc,which)
**       This routine changes the directory to the current MCD file
**       specification when loading an external MCD file, viewing the
**       MDF file, or modifying the Cutter file.
**
**    PARAMETERS
**       INPUT  :
**          sfil     = Name of file to load, including directory path.
**          tsrc     = Type of file being loaded.
**          which    = 0 = Change directory to MCD directory.
**                     1 = Change directory to home directory.
**       OUTPUT : none
**    RETURNS      : UU_TRUE if directory was changed when 'which' = 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_chdir(sfil,tsrc,which)
char *sfil;
int tsrc,which;
{
	UU_LOGICAL ichdir;
	UX_pathname home,path,tfil;
/*
.....Only change the directory when
.....an external MCD file or
.....foreign clfile is specified
*/
	ichdir = UU_FALSE;
	if (tsrc == T_EXTERNAL || tsrc == T_MCD || tsrc == T_APTSRC ||
		tsrc == T_UGII || tsrc == T_CATV4 || tsrc == T_CATV5 || tsrc == T_MASTER)
	{
/*
........Determine if the directories are different
*/
		getcwd(home,UX_MAX_PATH_LEN);
/*
........Directories are different
........Change directories
*/
		if (which == 0)
		{
			ul_break_fname(sfil,path,tfil);
			if (strcmp(home,path) != 0 && path[0] != '\0')
			{
				ichdir = UU_TRUE;
				chdir(path);
			}
		}
/*
........Change directory back to Home
*/
		else
		{
			chdir(home);
		}
	}
/*
.....End of routine
*/
	return(ichdir);
}

/*********************************************************************
**    E_FUNCTION     : S_save_pfeeds()
**       Save the NCL Playback feed rate interpolation settings into
**       modals file.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_pfeeds()
{
	int stat,i;
	char msg[80];
	UU_REAL rval;
	UX_pathname fname;
	FILE *fptr;
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
	strcpy(scolor1[0], "*DEFAULT");
	for (i=0; i<64;i++)
		sprintf(scolor1[i+1], "*%s", uw_color_name[i]);
/*
.....Open modals file
*/
	strcpy(fname, "ncl_playfeed.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
		fname, 3, &fptr);
	if (stat!=UU_SUCCESS || fptr==UU_NULL) goto done;
/*
.....Store playback modals
*/
	ux_fputs0("#PLAY_FEEDS#\n", fptr);
	for (i=0;i<10;i++)
	{
		UM_len_inttoext(UN_anlz_feed[i],rval);
		sprintf(msg,"/FEED%d/ %lf\n",i+1,rval);
		ux_fputs0(msg, fptr);
	}
	for (i=0;i<10;i++)
	{
		sprintf(msg,"/FEED%d_COLOR/ %s\n",i+1,scolor1[UN_anlz_fcolor[i]+1]);
		ux_fputs0(msg, fptr);
	}
/*
.....Close modals file
*/
	ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
	return(stat);
}

/*********************************************************************
**    E_FUNCTION     : S_save_pinterp()
**       Save the NCL Playback motion interpolation settings into
**       modals file.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_pinterp()
{
	int stat,i;
	char msg[80];
	UX_pathname fname;
	FILE *fptr;
	static char interp[5][10] = {"LINEAR","CIRCLE","RAPID","CYCLE","TLAXIS"};
/*
.....Initialize routine
*/
	strcpy(scolor1[0], "*DEFAULT");
	for (i=0; i<64;i++)
		sprintf(scolor1[i+1], "*%s", uw_color_name[i]);
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy(fname,"ncl_playinterp.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
		fname, 3, &fptr);
	if (stat!=UU_SUCCESS || fptr==UU_NULL) goto done;
/*
.....Store playback modals
*/
	ux_fputs0("#PLAY_INTERP#\n", fptr);

	sprintf(msg,"/GEO_COLOR/ %s\n",scolor1[UN_anlz_geo[0]+1]);
	ux_fputs0(msg, fptr);
	sprintf(msg,"/GEO_LINE/ %s\n",lstyle1[UN_anlz_geo[1]]);
	ux_fputs0(msg, fptr);
	
	for (i=0;i<5;i++)
	{
		sprintf(msg,"/%s_COLOR/ %s\n",interp[i],scolor1[UN_anlz_icolor[i]+1]);
		ux_fputs0(msg, fptr);
		sprintf(msg,"/%s_LINE/ %s\n",interp[i],lstyle1[UN_anlz_istyle[i]]);
		ux_fputs0(msg, fptr);
	}
/*
.....Close modals file
*/
	ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
	return(stat);
}
