/*********************************************************************
**    NAME         :  nemotpb.c
**       CONTAINS:
**			ncl_motion_playback
**			ncl_playback_speed
**			ncl_motvwp
**			ncl_motion_extrema
**       ncl_save_iclw
**       ncl_reset_iclw
**    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nemotpb.c , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/10/18 , 15:20:56
*********************************************************************/

#include "usysdef.h"
#include "bsym.h"
#include "dmark.h"
#include "dtypes.h"
#include "gsegop.h"
#include "gtbl.h"
#include "gobas.h"
#include "lcom.h"
#include "mdcoord.h"
#include "mfort.h"
#include "mpocket.h"
#include "nclver.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nclmodals.h"
#include "nclfile.h"
#include "view.h"
#include "driver.h"

#include "lipv.h"
#include "lipvmach.h"
#include "lipvmplay.h"


int UN_playback_speed;
extern UN_motseg *mlist_ptr;
/* extern UN_motseg *mlist_first_ptr; */
extern int UN_motion_color,UN_motion_line,UN_rapid_color,UN_rapid_line;
extern int UV_dynview_active;

int moving_part=0; 
static UM_int2 kret;

static int vis;
static UU_REAL lintol,cutr[20];
static UM_int4 flags[10],idc[3];
static UM_f77_str fsym,fgsym;
static char sym[3][MAXSYMLEN],gsym[MAXSYMLEN];
static UM_int4 iclw[6]={-1,-1,-1,-1,-1,-1},Siclw[6];
static UM_real8 rclw[420],Srclw[420];

#define segfl flags[0]
#define movfl flags[1]
#define shadfl flags[2]
#define shadshk flags[5]
#define shadhld flags[6]

#define BOXV 340
#define CHIPV 331
#define CLONEV 576
#define CONEV 632
#define CYLV 620
#define LOADV 1075
#define MODV 732
#define MOVEV 577
#define OFFSEV 666
#define REMOVV 843
#define SPHERV 631
#define STLV 330
#define TORUSV 627

#if UU_COMP == UU_WIN2K
#define IS1 4
#define IS4 1
#else
#define IS1 1
#define IS4 4
#endif
#define PW_CUTTER 716
#define PW_HOLDER 157


extern int NCL_animation;

/*********************************************************************
**    E_FUNCTION     : ncl_motion_playback(pmod,scan,bounds,clist,ncutr)
**			This is the controlling routine for playing back (back
**			plotting) motion from a clfile.
**    PARAMETERS   
**       INPUT  : 
**			pmod    = Array of flags which control the playback.
**			          [0]  - 0 = Start playback at start of range
**			                     defined in PLAYBACK FILE form.
**			                 1 = Start playback at point where we last
**			                     left off.
**			          [1]  - Speed at which to run playback (1-100).
**			          [2]  - 0 = Run mode.  1 = Step mode.  2 = Run to next tool.
**                        3 = Run to next motion. -1 = Perform single step.
**			          [3]  - Number of motion records to plot per each
**			                 step when in step mode.
**			          [4]  - 0 = Don't erase motion prior to playback.
**			                 1 = Erase motion first.
**			          [5]  - 0 = Static cutter display.  1 = Moving
**			                 cutter.  2 = Moving part.
**                 [6]  - Not used.
**                 [7]  - 0 = Cutter shade off.
**                        1 = Cutter shade on
**                 [8]  - 0 = Shank shade off.
**                        1 = Shank shade on
**                 [9]  - 0 = Holder shade off.
**                        1 = Holder shade on
**                 [10] - Not used.
**                 [11] - Not used.
**                 [12] - Not used.
**                 [13] - Not used.
**			          [14] - 0 = Don't simulate cycles.
**			                 1 = Perform simple simulation (all drill
**			                     cycles).
**			                 2 = Perform detailed simulations (include
**			                     pecking motion).
**			          [15] - 0 = Analyzation mode is turned off.
**			                 1 = Set motion colors based on feed rates.
**			                 2 = Set motion colors based on interpolation.
**			          [16] - 0 = Don't show source during playback.
**			                 1 = Show source.
**			          [17] - 0 = Don't modify rapid moves.
**			                 1 = Modify rapid moves along X-axis.
**			                 2 = Modify rapid moves along Y-axis.
**			                 3 = Modify rapid moves along Z-axis.
**			                 4 = Modify rapid moves along tool axis.
**
**			scan    = 0 = Perform normal playback.
**			          1 = Performing SEQUNC scanning operation only.
**			          2 = Performing bounding box scanning operation.
**			          3 = Scan for list of cutters used.
**			          4 = Preload lathe cutters onto turret.
**			          5 = Check for machine card.
**       OUTPUT :  
**			bounds  = Motion bounding box when scan=2.
**			clist   = List of cutters when scan=3.
**			ncutr   = Number of cutters in list.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_motion_playback(pmod,scan,bounds,clist,ncutr)
int pmod[],scan;
UU_REAL bounds[6];
UU_LIST *clist;
int *ncutr;
{
	static int imark,iclwt[6];
	static UU_REAL version=0.;

	int npt,nptt,ll[2],ur[2],vp,status,ix,i,j,nstep,markval;
	int nc,wargs[3],numcyc,k,iary[2],imach;
	int iscirc,vmode,inc,pb_stat,ssrc,mcdrec,isrec;
	int mdid,ist,tfl,dfl,ctype,nout,ninp;
	UU_LOGICAL isnfl,exefl;
	char fstr[10][80],*sympt,tsym[MAXSYMLEN],mbuf[256];
	char ltxt[68],lpprint[1536];
	Gwpoint3 pt;
	Gfloat vpn[3],vup[3];
	UU_REAL vpsc,cnv,feed,mscale,vscale,cycfed,rpm,feed2,fsav,tlen,cirrec[8];
	UM_int2 *ipt,ifl,cutinc,incr,ifl86,ifl35,irap,ival,knc[11],knpt;
	UM_int2 ifli,itra,ifl307,val307,val1,kncyc,icyc[10],idid,isav,ftyp;
	UM_int2 i2v0=0,i2v1=1,ifl319,val319,irapsv,*ipt1,jerr;
	UM_int4 ccdir,ccmode,coolnt,tlno,jval;
	UM_int4 cctp,ccm,ccd;
	UM_int4 cfl[3],trafl,idcs,*jpt,spdir,ierr;
	UN_clstruc *irec,*irecsv;
	UM_real8 tracut[12],cycpt[500][4],rcyc[10];
	UM_f77_str fbuf[10];
	UV_vport vport;
	UV_view view;
	UN_cutter_list cdata,*cpt,*tpt;
	UN_mot_attr mattr;
	UN_mot_data mdata;
	UU_LOGICAL fromt,first,ifnd;
	UM_int4 iftyp,ifxt,numid,iunit,kerr,icol,ivis,itrans,iact,nidns,idns[100];
	UM_real8 param[12],tol;
	char *fpt,stkstr[80];
	UM_f77_str stkbuf;
	UN_motion_block mblock;
	UM_coord *pts;
	UM_vector *vcs;
	UN_motseg_cutattr cattr;
	UU_KEY_ID symkey[3];
	int lthbit = 0;
/*
.....Initialize routine
*/
	if (version == 0) version = NCL_version;
	NCL_animation = 1;
	imark = 0;
	for (i=0;i<10;i++) mblock.axis[i] = 0.;
	irec = UN_clfile_current;
	nstep = 0;
	cfl[0] = UN_playparm.cfl[0];
	cfl[1] = UN_playparm.cfl[1];
	cfl[2] = UN_playparm.cfl[2];
	npt = UN_playparm.npt;
	feed = UN_playparm.fedrat[0];
	feed2 = UN_playparm.fedrat[1];
	rpm = UN_playparm.rpm;
	spdir = UN_playparm.spdir;
	ccmode = UN_playparm.ccmode;
	ccdir = UN_playparm.ccdir;
	coolnt = UN_playparm.coolnt;
	tlno = UN_playparm.tlno;
	tlen = UN_playparm.tlen;
	ftyp = UN_playparm.ftyp;
	for (i=0;i<10;i++)
	{
		icyc[i] = UN_playparm.icyc[i];
		rcyc[i] = UN_playparm.rcyc[i];
	}
	kret = UN_playparm.cycret;
	iscirc = 0;
	for (i=0;i<3;i++)
	{
		strncpy(sym[i],UN_playparm.sym[i],MAXSYMLEN-1);
		nc = MAXSYMLEN-1;
		ul_strip_blanks(sym[i],&nc);
	}
	for (i=0;i<6;i++)
	{
		mblock.spt[i] = UN_playparm.spt[i];
		mblock.ept[i] = UN_playparm.spt[i];
	}
	for (i=0;i<20;i++)
	{
		cutr[i] = UN_playparm.cutr[i];
	}
	segfl = 0;
	shadfl = pmod[7];
	shadshk = pmod[8];
	shadhld = pmod[9];
	movfl = pmod[5];
/*	
.....Added option to show source in IPV playback - ASF 12/18/13.
	if (!LW_active) ssrc = pmod[16];
	else ssrc = 0;
*/
	ssrc = pmod[16];
	if (ssrc == 1 && UN_mcd_nlines != 0) ssrc = 2;
	mcdrec = 0;
	UM_init_f77_str(fsym,sym[0],MAXSYMLEN-1);
	UM_init_f77_str(fgsym,gsym,MAXSYMLEN-1);
	ifl35 = 0;
	rstint();
	gtrafl(&trafl);
	for (i=0;i<12;i++)
	{
		tracut[i] = UN_playparm.tracut[i];
	}
	for (i=0;i<4;i++) mblock.axis[i+6] = UN_playparm.axis[i];
	ifl307 = 307;
	val1 = 1;
	getifl(&ifl307,&val307);
	setifl(&ifl307,&val1);
	ifl319 = 319;
	val1 = 0;
	getifl(&ifl319,&val319);
	setifl(&ifl319,&val1);
	irap = UN_playparm.rapid; irapsv = 0;
	UN_playback_active = UU_TRUE;
	isnfl = UU_FALSE;
/*
.....Initialize Scan Type 2 variables
*/
	if (scan == 2)
	{
		for (i=0;i<3;i++)
		{
			bounds[i] = 10000. ; bounds[i+3] = -10000.;
		}
	}
/*
.....Initialize Scan Type 3 variables
*/
	if (scan == 3)
	{
		uu_list_init(clist,sizeof(UN_cutter_list),50,50);
		cdata.ctype[0] = cdata.ctype[1] = cdata.ctype[2] = 0;
		cdata.color[0] = cdata.color[1] = cdata.color[2] = 0;
		cdata.cut_color = -1;
		cdata.used = UU_TRUE;
		ncl_get_tool_symlib(cdata.symlib);
		cdata.symbol[0][0] = cdata.symbol[1][0] = cdata.symbol[2][0] = '\0';
		cdata.toler = LW_default_tool.toler;
		cdata.maxang = LW_default_tool.maxang;
		cdata.trans[0] = cdata.trans[1] = cdata.trans[2] =
			LW_default_tool.translucency;
		cdata.shank_clash = LW_default_tool.shank_clash;
		for (i=0;i<3;i++)
			cdata.parms[i][0] = cdata.parms[i][1] = cdata.parms[i][2] = 
				cdata.parms[i][3] = 0.;
		cdata.tlno = 0; cdata.tlen = 0.; cdata.tlofs = 0.;
		cpt = (UN_cutter_list *) UU_LIST_ARRAY (clist);
		mdid = 1;
		*ncutr = 0;
/*
........Make sure tool from imported session
........Stays in the list
*/
		if (LW_ntool_sess != 0)
		{
			tpt = (UN_cutter_list *) UU_LIST_ARRAY(&LW_tool_sess);
			uu_list_push(clist,&tpt[LW_tool_sess_act]);
			cpt[*ncutr].isn = cpt[*ncutr].clrec = 0.;
			*ncutr = *ncutr + 1;
		}
	}
	else
	{
		cpt = (UN_cutter_list *) UU_LIST_ARRAY (&LW_tool_list);
	}
/*
.....Setup cut colors
*/
	if (LW_active && (scan == 0 || scan == 1))
		ul_ipv_set_colors();
		
/*
.....Set units conversion
*/
	ifl = 264;
	getifl(&ifl,&ival);
	if (ival == 0) cnv = 1.;
	else cnv = 25.4;
/*
..... make sure speed is between 1 and 100 percent
*/
	UN_playback_speed = pmod[1];
	if (UN_playback_speed > 100) UN_playback_speed = 100;
	else if (UN_playback_speed < 1) UN_playback_speed = 1;
/*
.....Step mode
.....Trap REJECT OP
*/
	if (scan == 0)
	{
		if (pmod[2] == 1)
		{
			imark = 1;
			UD_MARK(markval,UU_TRUE);
			if (markval != 0) goto done;
		}
/*
.....Get cutter iterations
*/
		ifl = 129;
		getifl(&ifl,&cutinc);
		incr = 0;
		ifli = 86;
/*
.....Erase motion first
*/
		if (!LW_active)
		{
			if (pmod[4] == 1) motdel();
		}
/*
.....Moving part
.....Create part object for manipulation
*/
		if (LW_active && movfl == 2 && !LW_mach_simul)
		{
			i = LW_vport.xform;
			moving_part = 1;
			UN_playtax[i-1][0] = UN_playtax_save[0];
			UN_playtax[i-1][1] = UN_playtax_save[1];
			UN_playtax[i-1][2] = UN_playtax_save[2];
			givpn3(i,&UN_playvpn[i-1]);
			givup3(i,&UN_playvup[i-1]);
		}
		else if (!LW_active && movfl == 2)
		{
			moving_part = 1;
/*			UV_dynview_active = UU_TRUE;*/
			ncl_redrawws();
			for (i=1;i<=UV_act_screen[0].nvports;i++)
			{
				uv_getvpid(UV_act_screen[0].vports[i-1],&vport);
				uw_glndctodev(&vport.llf,ll);
				uw_glndctodev(&vport.urb,ur);
				if (vport.motion)
				{
					ug_sntran(i);
					ncl_motvwp(&vp,UN_playvpn[i-1],UN_playvup[i-1],&vpsc,&vmode);
					UN_playtax[i-1][0] = UN_playtax_save[0];
					UN_playtax[i-1][1] = UN_playtax_save[1];
					UN_playtax[i-1][2] = UN_playtax_save[2];
					ncl_makeobj(i-1);
/*
.....We need to setup ll and ur values
*/
					ncl_draw_part(i-1,ll,ur);
					ncl_closeobj();
				}
			}
		}
/*
.....Analyzation mode is on
.....Save current motion color and line style
*/
		if (pmod[15] != 0)
		{
			UN_motsav_color = UN_motion_color;
			UN_motsav_line = UN_motion_line;
			UN_rapsav_color = UN_rapid_color;
			UN_rapsav_line = UN_rapid_line;
			ncl_cutter_get_defattr(&cattr);
			UN_cutsav_color = cattr.color[0];
		}
	}
/*
.....Open window for *SHOW/SOURCE
*/
	if (ssrc != 0)
	{
		wargs[1] = 1;
		status = ul_open_window(glines,UL_wincol,wargs);
		if (status != UU_SUCCESS) ssrc = 0;
	}
	for (i=0;i<10;i++) UM_init_f77_str(fbuf[i],fstr[i],80);
/*
.....Get linearization value if dynamic display
.....Bobby  -  1/4/94
*/
	if (movfl != 0)
	{
		mscale = 0.;
		for (i=1;i<=UV_act_screen[0].nvports;i++)
		{
			ug_gksstli.curvwindex = i;
			ncl_motvwp(&vp,vpn,vup,&vscale,&vmode);
			if (vscale > mscale) mscale = vscale;
		}
		if (mscale == 0.) lintol = 10000.;
		else
		{
			lintol = .25 / mscale;
		}
	}
/*
.....Loop to read clfile
*/
	first = UU_TRUE;
	if (scan == 0 && pmod[0] == 0) LW_progress_count = 0;
	jerr = 0;
	do
	{
/*
.....Read a clfile record
*/
		if (UN_clfile_curpt == 0) clread(&UN_clfile,&irec,iclw,rclw,&jerr);
		if (jerr == 1) goto done;
		if (UN_clfile_curpt == 0 && scan == 0) LW_progress_count++;
/*
.....Display source line if
.....MCD file is active
*/
		if (ssrc == 2)
		{
			for (i=mcdrec;i<iclw[0];i++)
			{
				ncl_mcd_read(i,mbuf,&nc);
				ul_win_out(mbuf,0);
			}
			if (iclw[0] > mcdrec) mcdrec = iclw[0];
		}
/*
.....Process clfile record
*/
		switch (iclw[2])
		{
/*
.....ISN record
*/
		case 1000:
			isnfl = UU_TRUE;
			motisn(rclw);
			break;
/*
.....Post-processor command
*/
		case 2000:
/*
........*SHOW/SOURCE in effect
*/
			if (ssrc == 1)
			{
				aspwrd (&iclw[3],rclw,&iclw[4],UM_addr_of_f77_str(fbuf[0]),knc);
				for (i=0;i<4;i++)
				{
					if (knc[i] > 0)
					{
						fstr[i][knc[i]] = '\0';
						ul_win_out(fstr[i],0);
					}
				}
			}
/*
........CYCLE
*/
			if (iclw[3] == 1054 || (iclw[3] == 1036 && pmod[14] == 3))
			{
				if (pmod[2] == 3) goto done;
				irap = 0; irapsv = 0;
				isav = icyc[0];
/*
...........Lathe Cycle
*/
				if (pmod[14] == 3)
				{
					clcycl(&iclw[3],rclw,&iclw[4],&cnv,&idid,icyc,rcyc);
/*
...........Point was furnished with CYCLE command
*/
					if (icyc[6] != 0 || icyc[7] != 0)
					{
						iclw[2] = 5000;
						iclw[3] = 5;
						iclw[4] = 6;
						UN_clfile_curpt = 3;
						if (icyc[6] == 1)
							rclw[3] = rcyc[7];
						else
							rclw[3] = mblock.spt[0] + rcyc[7];
						if (icyc[7] == 1)
							rclw[4] = rcyc[8];
						else
							rclw[4] = mblock.spt[1] + rcyc[8];
						rclw[5] = mblock.spt[2];
					}
				}
/*
...........Mill Cycle
*/
				else
					clcyc(rclw,&iclw[4],&cnv,&idid,icyc,rcyc);
				if (idid == 1)
				{
/*
..... icyc[1] = 8: Helical pocket entry
*/
					if (pmod[14] == 1 && icyc[1] != 2 && icyc[1] != 8)
						icyc[1] = 1;
					if (icyc[0] != 0)
					{
						if (isav == 0) cycfed = feed;
						if (icyc[3] == 0 || rcyc[5] == 0.) rcyc[5] = feed;
					}
					else
					{
						if (isav == 1) feed = cycfed;
					}
					if (icyc[1] == 8 && rcyc[3] == 0.) rcyc[3] = cutr[0];
				}
			}
/*
........FEDRAT
*/
			else if (iclw[3] == 1009)
			{
				irap = 0; irapsv = 0;
				fsav   = feed;
				clfed(rclw,&iclw[4],&ftyp,&feed);
				fdset(&ftyp,&feed);
				feed = feed / cnv;
/*				if (feed != fsav) feed = feed / 25.4;*/
				if (ftyp == 2)
				{
					feed2 = feed;
					if (rpm != 0.) feed  = rpm * feed2;
					else feed = fsav;
				}
			}
/*
........PPRINT
*/
			else if (iclw[3] == 1044)
			{
				if (scan == 0 || scan == 1 || scan == 3 || scan == 4)
				{
					strncpy(lpprint,(char *)rclw,66);
					lpprint[66] = '\0';
					nc = ul_cut_string(lpprint,66);
					while (lpprint[nc-1] == '~')
					{
						lpprint[nc-1] = '\0';
						irecsv = irec;
						clread(&UN_clfile,&irec,iclw,rclw,&jerr);
						if (iclw[2] == 2000 && iclw[3] == 1044)
						{
							strncpy(ltxt,(char *)rclw,66);
							ltxt[66] = '\0';
							nc = ul_cut_string(ltxt,66);
							ltxt[nc] = '\0';
							strcat(lpprint,ltxt);
							nc = strlen(lpprint);
							if (scan == 0) LW_progress_count++;
						}
						else
						{
							irec = irecsv;
							break;
						}
					}
					exefl = LW_active && scan == 0;
					ul_ipv_parse_pprint(iclw[0],lpprint,exefl,iclw,rclw,
						&UN_clfile_curpt,&mdid);
				}
			}
/*
........RAPID
*/
			else if (iclw[3] == 5)
			{
				irap = 1;
			}
/*
........RETRCT
*/
			else if (iclw[3] == 7)
			{
				irap = 0; irapsv = 0;
				clret(rclw,&iclw[4],&kret);
			}
/*
........SPINDL
*/
			else if (iclw[3] == 1031)
			{
				irap = 0; irapsv = 0;
				clspn(rclw,&iclw[4],&rpm,&spdir);
				motspn(&spdir,&rpm);
				if (ftyp == 2 && rpm != 0.)
				{
					feed  = rpm * feed2;
				}
			}
/*
........COOLNT
*/
			else if (iclw[3] == 1030)
			{
				irap = 0; irapsv = 0;
				clcool(rclw,&iclw[4],&coolnt);
				motcln(&coolnt);
			}
/*
........CUTCOM
*/
			else if (iclw[3] == 1007)
			{
				
				irap = 0; irapsv = 0;
				if (scan <= 1)
				{
					clcutc(rclw,&iclw[4],&cctp,&ccm,&ccd);
/*
...........CUTCOM/dir
*/
					if (cctp == 1)
					{
						ccmode = ccm; ccdir = ccd;
						motccm(&ccmode,&ccdir);
					}
/*
...........CUTCOM/ADJUST
*/
					else
						ul_ipv_define_offsets(ccm,ccd,-1,-1);
				}
			}
/*
........LOADTL
*/
			else if (iclw[3] == 1055)
			{
				irap = 0; irapsv = 0;
				clldtl(rclw,&iclw[4],&tlno,&tlen);
				mottln(&tlno,&tlen);
				tlen = tlen / cnv;
				ncl_process_loadtl(scan,clist,mdid,*ncutr,tlno,tlen);
			}
/*
........MODE
*/
			else if (iclw[3] == 1003)
			{
				irap = 0; irapsv = 0;
				clmode(rclw,&iclw[4],&imach);
				if (LW_active && imach != -1 && scan <= 1)
					ul_ipv_set_mach_type(imach);
				if (imach == 0 && pmod[14] == 3) pmod[14] = 1;
				else if (imach == 1 && pmod[14] != 0) pmod[14] = 3;
			}
/*
........TURRET
*/
			else if (iclw[3] == 1033)
			{
				irap = 0; irapsv = 0;
				clturr(rclw,&iclw[4],&tlno,&tlen);
				mottln(&tlno,&tlen);
				tlen = tlen / cnv;
				ncl_process_loadtl(scan,clist,mdid,*ncutr,tlno,tlen);
			}
/*
........MACHIN
*/
			else if (iclw[3] == 1015)
			{
				irap = 0; irapsv = 0;
				jval = 1 - UN_clfile;
				clmach(rclw,&iclw[4],UM_addr_of_f77_str(fbuf[0]),&knc[0],
					UM_addr_of_f77_str(fbuf[1]),&knc[1],UL_pworks_machs,
					&UL_pworks_nmach,&jval);
				strncpy(UL_pworks_mdf,fstr[1],knc[1]);
				if (scan == 5) goto endit;
			}
/*
........TOOLNO
*/
			else if (iclw[3] == 1025)
			{
				
				irap = 0; irapsv = 0;
				if (scan <= 1)
				{
					clcutc(rclw,&iclw[4],&cctp,&ccm,&ccd);
					if (cctp == 2)
						ul_ipv_define_offsets(-1,-1,ccm,ccd);
				}
			}
/*
........TOOLPN
*/
			else if (iclw[3] == 1053)
			{
				if (iclw[4] != 9)
				{
					strcpy(fstr[9],
						"ERROR/Invalid number of parameters in TOOLPN command");
					knc[10] = strlen(fstr[9]);
					aspwrd (&iclw[3],rclw,&iclw[4],UM_addr_of_f77_str(fbuf[0]),knc);
					fstr[0][knc[0]] = '\0';
					if (LW_active && scan == 0)
						ul_ipv_diag_error(iclw[0],fstr[9],knc[10],fstr[0],knc[0]);
					else if (ssrc == 1)
					{
						if (knc[10] > 0) ul_win_out(fstr[9],0);
						if (knc[0] > 0) ul_win_out(fstr[0],0);
					}
				}
				else
				{
					for (i=0;i<3;i++) rclw[i] = rclw[i] / cnv;
					if (scan == 0) ul_ipv_tool_pin("",&rclw[0],UU_TRUE);
				}
			}
/*
........All others
*/
			else
			{
				irap = 0; irapsv = 0;
			}
			rpset(&irap);
			break;
/*
.....Stock/Fixture record
*/
		case 2600:
		case 2601:
			if (LW_active && scan == 0)
			{
				ifxt = iclw[2] - 2600;
				jpt = (UM_int4 *)rclw;
				switch (iclw[3])
				{
/*
........STOCK/BOX
*/
				case BOXV:
					iftyp = jpt[0];
					numid = jpt[1];
					for (i=0;i<6;i++) param[i] = rclw[i+1];
					ulf_verify_box(&ifxt,&iftyp,param,&numid,&kerr);
					break;
/*
........STOCK/[REMOVE],CHIPS
*/
				case CHIPV:
					nidns = iclw[4] / 6;
					ulf_verify_chips(rclw,&nidns);
					break;
/*
........STOCK/CYLNDR
*/
				case CYLV:
					iftyp = jpt[0];
					numid = jpt[1];
					for (i=0;i<8;i++) param[i] = rclw[i+1];
					ulf_verify_cyl(&ifxt,param,&numid,&kerr);
					break;
/*
........STOCK/CONE
*/
				case CONEV:
					iftyp = jpt[0];
					numid = jpt[1];
					for (i=0;i<9;i++) param[i] = rclw[i+1];
					ulf_verify_cone(&ifxt,param,&numid,&kerr);
					break;
/*
........STOCK/SPHERE
*/
				case SPHERV:
					iftyp = jpt[0];
					numid = jpt[1];
					for (i=0;i<4;i++) param[i] = rclw[i+1];
					ulf_verify_sphere(&ifxt,param,&numid,&kerr);
					break;
/*
........STOCK/TORUS
*/
				case TORUSV:
					iftyp = jpt[0];
					numid = jpt[1];
					for (i=0;i<8;i++) param[i] = rclw[i+1];
					ulf_verify_torus(&ifxt,param,&numid,&kerr);
					break;
/*
........STOCK/LOAD
*/
				case LOADV:
					numid = jpt[0];
					nc = jpt[1];
					fpt = (char *)&rclw[1];
					UM_init_f77_str(stkbuf,stkstr,nc);
					strncpy(stkstr,fpt,nc);
					stkstr[nc] = '\0';
					ulf_verify_load(stkstr,&nc,&numid,&kerr);
					break;
/*
........STOCK/STL
*/
				case STLV:
					numid = jpt[0];
					nc = jpt[1];
					iunit = jpt[2];
					fpt = (char *)&rclw[2];
					UM_init_f77_str(stkbuf,stkstr,nc);
					strncpy(stkstr,fpt,nc);
					stkstr[nc] = '\0';
					ulf_verify_stl(&ifxt,stkbuf,&nc,&iunit,&numid,&kerr);
					break;
/*
........STOCK/CLONE
*/
				case CLONEV:
					numid = jpt[0];
               iftyp = jpt[1];
					i = jpt[2];
					ulf_verify_copy(&ifxt,&iftyp,&numid,&i,0,&rclw[1],&kerr);
					break;
/*
........STOCK/MOVE
*/
				case MOVEV:
					nidns = jpt[0];
					fpt = (char *)&rclw[13];
					UM_init_f77_str(stkbuf,stkstr,6);
					strncpy(stkstr,fpt,6);
					stkstr[6] = '\0';
					for (i=0;i<nidns;i++) idns[i] = jpt[i+28];
					nc = strlen(stkbuf);
					ulf_verify_move(&ifxt,idns,&nidns,&rclw[1],stkbuf,&nc,&kerr);
					break;
/*
........STOCK/REMOVE
*/
				case REMOVV:
					nidns = jpt[0];
					for (i=0;i<nidns;i++) idns[i] = jpt[i+2];
					ulf_verify_remove(&ifxt,idns,&nidns,&kerr);
					break;
/*
........STOCK/MODIFY
*/
				case MODV:
					nidns = jpt[0];
					icol = jpt[1];
					ivis = jpt[2];
					itrans = jpt[3];
					iact = jpt[4];
					tol = rclw[3];
					for (i=0;i<nidns;i++) idns[i] = jpt[i+8];
					ulf_verify_modify(&ifxt,idns,&nidns,&icol,&ivis,&tol,&itrans,
						&iact,&kerr);
					break;
				}
			}
			break;
/*
.....Circular record
*/
		case 3000:
			if (pmod[2] == 3) goto done;
			iscirc = 1;
/*
........Process circle if IPV is active
*/
			if (LW_active && scan == 0 && !ncl_cutter_is_blade())
			{
				isrec = UN_clrec;
				status = ncl_pb_circul(&irec,iclw,&rclw[5],&mblock,cnv,trafl,
					tracut,npt,cirrec);
				if (status != 0)
				{
/*
...........Setup motion data
*/
					mdata.isn = iclw[0];
					mdata.seqno = 0;
					mdata.isnptr = motisn_set();
					mdata.clrec[0] = isrec;
					mdata.clrec[1] = UN_clrec;
					mdata.fr_mode = ftyp;
					if (irap == 1) mdata.fr_mode = 0;
					if (ftyp == 1) mdata.fr_val = feed;
					else mdata.fr_val = feed2;
/*
...........Setup motion attributes
*/
					mattr.tlno = LW_act_tool[0];
					mattr.loadtl = tlno;
					mattr.tlen = tlen;
					mattr.sp_mode = spdir;
					mattr.sp_val = rpm;
					mattr.coolnt = coolnt;
					mattr.cc_mode = ccmode;
					mattr.cc_dir = ccdir;
/*
...........Setup motion type
*/
					mblock.type = 1;
					mblock.opskip = 0;
					mblock.time = 0.;
/*
........Set Motion Analyzation colors
*/
					if (pmod[15] != 0)
						ncl_pb_acolor(pmod[15],irap,feed,&cattr,mblock.type);
/*
...........Output circular move
*/
					S_pb_motout(pmod,&mblock,&mdata,&mattr,UU_FALSE,status,cirrec,
						scan);
					for (j=0;j<6;j++) mblock.spt[j] = mblock.ept[j];
					iscirc = 0;
					mdid = 1;
/*
...........Step mode
...........Prompt the user
*/
					if (!ncl_pb_step(pmod,&nstep,vis,scan)) goto done;
				}
			}
			break;
/*
.....Motion record
*/
		case 5000:
		case 5001:
		case 5002:
		case 5200:
			if (pmod[2] == 3) goto done;
			if (iclw[3] != 6 && iscirc == 2) iscirc = 0;
			if (iscirc == 1) iscirc = 2;
			if (iclw[3] == 6 && irapsv != 0)
			{
				irap = irapsv;
				rpset(&irap);
			}
/*
.....Setup motion data
*/
			if (iclw[3] != 6 && !isnfl)
			{
				iary[0] = 1; iary[1] = iclw[0]; motisn(iary);
			}
			mdata.isn = iclw[0];
			mdata.seqno = 0;
			mdata.isnptr = motisn_set();
			mdata.clrec[0] = UN_clrec;
			mdata.clrec[1] = UN_clrec;
			mdata.fr_mode = ftyp;
			if (irap == 1) mdata.fr_mode = 0;
			if (ftyp == 1) mdata.fr_val = feed;
			else mdata.fr_val = feed2;
/*
.....Setup motion attributes
*/
			mattr.tlno = LW_act_tool[0];
			mattr.loadtl = tlno;
			mattr.tlen = tlen;
			mattr.sp_mode = spdir;
			mattr.sp_val = rpm;
			mattr.coolnt = coolnt;
			mattr.cc_mode = ccmode;
			mattr.cc_dir = ccdir;
/*
.....Setup motion type
*/
			if (icyc[0] != 0) mblock.type = 3;
			else if (iclw[3] == 8) mblock.type = 3;
			else if (iscirc != 0) mblock.type = 1;
			else if (iclw[3] == 7) mblock.type = 1;
			else if (irap == 1) mblock.type = 2;
			else mblock.type = 0;
			if (iclw[2] == 5002) mblock.type += 100;
/*
........Set Motion Analyzation colors
*/
			if (pmod[15] != 0)
				ncl_pb_acolor(pmod[15],irap,feed,&cattr,mblock.type);
			if (iclw[2] == 5001 || iclw[2] == 5002)
			{
				if (iclw[2] == 5002)
				{
					for (i=0;i<13;i++)
					{
						if (i==0 || i==1 || i==2 || i >= 6)
							mblock.axis[i] = rclw[i+8] / cnv;
						else
							mblock.axis[i] = rclw[i+8];
					}
				}
				else
				{
					for (i=0;i<6;i++) mblock.axis[i] = rclw[i+8] / cnv;
/*
.....'axis' are the rotary axes during IPV simulation
.....and the Blade axes during NCL playback
.....So don't store 'axis' during NCL playback
*/
					if (LW_active)
					{
						for (i=6;i<10;i++) mblock.axis[i] = rclw[i+8];
					}
				}
				ipt = (UM_int2 *)&rclw[0];
				mdata.seqno = ipt[0];
				mblock.opskip = ipt[1];
				mblock.time = rclw[1];
				iclw[4] = iclw[4] - 12;
				UN_clfile_curpt += 2;
			}
			else
			{
				mblock.opskip = 0;
				mblock.time = 0.;
			}
			nptt = npt;
			if (iclw[2] == 5200) nptt = 21;
			else if (iclw[2] == 5001 || iclw[2] == 5002) nptt = 18;
/*
........Loop through all cl points
*/
			for (ix=UN_clfile_curpt;ix<iclw[4];ix=ix+nptt)
			{
/*
........Check for user interrupt
*/
				UN_clfile_curpt = ix;
				if (scan == 0)
				{
/*					um_reset_pocket_graphics(UM_IPV_WINDOW);*/
					ckintr(&ifl86,&ifl35);
					getifl(&ifli,&ifl86);
					if (ifl86 != 0) goto done;
				}
				if (trafl == 0)
				{
					itra   = 3;
					conent(&rclw[ix],tracut,&itra);
				}
				mblock.ept[0] = rclw[ix] / cnv;
				mblock.ept[1] = rclw[ix+1] / cnv;
				mblock.ept[2] = rclw[ix+2] / cnv;
				if (npt > 3)
				{
					mblock.ept[3] = rclw[ix+3];
					mblock.ept[4] = rclw[ix+4];
					mblock.ept[5] = rclw[ix+5];
					if (trafl == 0)
					{
						itra   = 4;
						conent(&mblock.ept[3],tracut,&itra);
					}
				}
				if (!LW_active && scan != 2)
				{
					mcswcs (&i2v0, mblock.ept);
					if (npt > 3) mcswcs (&i2v1, &mblock.ept[3]);
				}
				if (iclw[2] == 5200)
				{
					mblock.vfwd[0] = rclw[ix+6];
					mblock.vfwd[1] = rclw[ix+7];
					mblock.vfwd[2] = rclw[ix+8];
					if (trafl == 0)
					{
						itra   = 4;
						conent(mblock.vfwd,tracut,&itra);
					}
					mcswcs (&i2v1, mblock.vfwd);
				}
				else if (iclw[2] == 5001)
				{
					mblock.vfwd[0] = rclw[ix+18];
					mblock.vfwd[1] = rclw[ix+19];
					mblock.vfwd[2] = rclw[ix+20];
				}
				else
				{
					um_vcmnvc(mblock.ept, mblock.spt, mblock.vfwd);
					um_unitvc(mblock.vfwd, mblock.vfwd);
				}
				if (mblock.type == 0 && !um_cceqcc(&mblock.spt[3],&mblock.ept[3]))
					mblock.type = 4;
				pb_stat = 0;
				if (scan == 0)
				{
					incr++;
					if (incr >= cutinc || ix >= iclw[4]-nptt || movfl != 0)
					{
						incr = 0;
						idc[0] = cfl[0];
					}
					else
					{
						idc[0] = 0;
					}
/*
........*SHOW/SOURCE in effect
*/
					if (ssrc == 1)
					{
						knpt = npt;
						ifl = 1;
						asgoto(&iclw[3],&rclw[ix],&knpt,&ifl,&iclw[0],
							UM_addr_of_f77_str(fbuf[0]),knc);
						fstr[0][knc[0]] = '\0';
						ul_win_out(fstr[0],0);
						if (knc[1] != 0)
						{
							fstr[1][knc[1]] = '\0';
							ul_win_out(fstr[1],0);
						}
					}
/*
........This is a FROM statement
*/
					fromt = UU_FALSE;
					if (iclw[3] == 3)
					{
						fromt = UU_TRUE;
						for (j=0;j<6;j++) mblock.spt[j] = mblock.ept[j];
					}
/*
........Cycle active
........generate cycle points
*/
					if (icyc[0] != 0 && irap == 0 && pmod[14] != 0 && !fromt)
					{
						if (pmod[14] == 3)
							cylman(mblock.spt,mblock.ept,icyc,rcyc,&kret,cycpt,&kncyc);
/*
...........Helical pocket entry
*/
						else if (icyc[1] == 8)
						{
							int lmdsys = (!LW_active && scan != 2);

							cycmn1(&lmdsys,mblock.spt,mblock.ept,icyc,rcyc,cycpt,&kncyc);
							for (i=0;i<10;i++)
							{
								icyc[i] = UN_playparm.icyc[i];
								rcyc[i] = UN_playparm.rcyc[i];
							}
						}
/*
...........Mill Cycle
*/
						else
							cycman(mblock.spt,mblock.ept,icyc,rcyc,&kret,cycpt,&kncyc);
						numcyc = kncyc;
						idcs = idc[0]; if (movfl == 0) idc[0] = 0;
						for (j=0;j<numcyc;j++)
						{
							mblock.ept[0] = cycpt[j][0];
							mblock.ept[1] = cycpt[j][1];
							mblock.ept[2] = cycpt[j][2];
							mblock.time = 0.;
							if (cycpt[j][3] != 0.)
							{
								irap = 0;
								if (icyc[3] == 2 && rpm != 0.)
								{
									feed2 = cycpt[j][3];
									feed = feed2 * rpm;
								}
								else
								{
									feed = cycpt[j][3];
									if (rpm == 0) feed2 = 0.;
									else feed2 = feed / rpm;
								}
/*
...........Feedrate Analyzation
...........Set proper color for current speed
*/
								if (pmod[15] == 1)
								{
									for (i=0;i<10;i++)
									{
										if (feed <= UN_anlz_feed[i]) break;
									}
									if (i >= 10) i = 9;
									UN_motion_color = UN_anlz_fcolor[i];
									cattr.color[0] = UN_anlz_fcolor[i];
									ncl_cutter_set_attr(&cattr);
								}
							}
							else
							{
								irap = 1;
								if (pmod[15] == 1)
								{
									cattr.color[0] = UN_rapid_color;
									ncl_cutter_set_attr(&cattr);
								}
							}
							rpset(&irap);
							if (j+1 == numcyc) idc[0] = idcs;
/*
...........Setup motion data
*/
							mdata.isn = iclw[0];
							mdata.clrec[0] = UN_clrec;
							mdata.clrec[1] = UN_clrec;
							mdata.fr_val = feed;
							if (irap == 1) mdata.fr_mode = 0;
							else if (icyc[3] == 2)
							{
								mdata.fr_mode = 2;
								mdata.fr_val = feed2;
							}
							else mdata.fr_mode = 1;
							idc[1] = cfl[1]; idc[2] = cfl[2];
							pb_stat = S_pb_motout(pmod,&mblock,&mdata,&mattr,
								UU_FALSE,0,cirrec,scan);
							ncl_pb_delay(UN_playback_speed,vis);
							for (k=0;k<6;k++) mblock.spt[k] = mblock.ept[k];
						}
/*
...........Single shot cycle
*/
						if (icyc[0] == 2)
						{
							icyc[0] = 0;
							feed = cycfed;
						}
					}
/*
........Rapid move
........Modify motion
*/
					if (irap != 0 && pmod[17] != 0 && !fromt)
					{
						rapmod(mblock.spt,mblock.ept,&pmod[17],cycpt,&kncyc);
						numcyc = kncyc;
						idcs = idc[0]; if (movfl == 0) idc[0] = 0;
						for (j=0;j<numcyc;j++)
						{
							mblock.ept[0] = cycpt[j][0];
							mblock.ept[1] = cycpt[j][1];
							mblock.ept[2] = cycpt[j][2];
							mblock.time = 0.;
							irap = 1;
							if (pmod[15] == 1)
							{
								cattr.color[0] = UN_rapid_color;
								ncl_cutter_set_attr(&cattr);
							}
							rpset(&irap);
							if (j+1 == numcyc) idc[0] = idcs;
/*
...........Setup motion data
*/
							mdata.isn = iclw[0];
							mdata.clrec[0] = UN_clrec;
							mdata.clrec[1] = UN_clrec;
							mdata.fr_val = feed;
							mdata.fr_mode = 0;
							idc[1] = cfl[1]; idc[2] = cfl[2];
							pb_stat = S_pb_motout(pmod,&mblock,&mdata,&mattr,
								UU_FALSE,0,cirrec,scan);
							ncl_pb_delay(UN_playback_speed,vis);
							for (k=0;k<6;k++) mblock.spt[k] = mblock.ept[k];
						}
					}
/*
........Output single move
*/
					else
					{
/*
...........Interpolation Analyzation
...........Check for tool axis change
*/
						if (pmod[15] == 2)
						{
							if (fabs(mblock.ept[3]-mblock.spt[3]) > .000001 ||
							    fabs(mblock.ept[4]-mblock.spt[4]) > .000001 ||
							    fabs(mblock.ept[5]-mblock.spt[5]) > .000001)
							{
								ncl_pb_acolor(pmod[15],irap,feed,&cattr,4);
							}
						}
/*
...........Setup motion data
*/
						mdata.isn = iclw[0];
						mdata.clrec[0] = UN_clrec;
						mdata.clrec[1] = UN_clrec;
						mdata.fr_val = feed;
						if (irap == 1) mdata.fr_mode = 0;
						else if (ftyp == 1) mdata.fr_mode = 1;
						else
						{
							mdata.fr_mode = 2;
							mdata.fr_val = feed2;
						}
/*
...........Output move
*/
						idc[1] = cfl[1]; idc[2] = cfl[2];
						pb_stat = S_pb_motout(pmod,&mblock,&mdata,&mattr,fromt,
							0,cirrec,scan);
					}
				} /* if (scan) */
/*
........Save this point and increment
........to next point
*/
				for (j=0;j<6;j++) mblock.spt[j] = mblock.ept[j];
				mdid = 1;
				if (scan == 2 && irap == 0)
				{
					for (j=0;j<3;j++)
					{
						if (mblock.ept[j] < bounds[j]) bounds[j] = mblock.ept[j];
						if (mblock.ept[j] > bounds[j+3]) bounds[j+3] = mblock.ept[j];
					}
				}
				UN_clfile_curpt = ix + nptt;
/*
.....User interrupted motion
*/
				if (pb_stat == -1) goto done;
/*
........Step mode
........Prompt the user
*/
				if (!ncl_pb_step(pmod,&nstep,vis,scan)) goto done;
			}
			UN_clfile_curpt = 0;
			irapsv = irap; irap = 0; rpset(&irap);
			break;
/*
.....Standard cutter statement
*/
		case 6000:
			UN_clfile_curpt = 0;
			if (iclw[3] != 6 && iclw[4] == 7) claptc(rclw,&iclw[4],&ierr);
			for (i=0;i<6;i++) iclwt[i] = iclw[i];
			iclwt[5] = UN_clrec;
			if (LW_active && (scan <= 1 || scan == 4))
			{
				ifnd = ncl_process_ipvcutter(scan,iclwt,cutr,cfl,clist,sym[0],
					sym[1],sym[2],symkey);
				if (ifnd && pmod[2] == 2) goto done;
				if (scan == 4) break;
				if (LW_mach_simul && LW_spindle_num > 1)
					ul_ipv_cutters(mblock.spt,cutr,&mdata,&mattr,scan);
			}
			else
			{
				ifnd = ncl_process_cutter(scan,iclwt,rclw,cnv,clist,&mdid,ncutr,
					cutr,tlno,tlen);
				cutr[13] = cutr[14] = cutr[15] = cutr[16] = 0.;
				if (scan == 3) break;
			}
/*
........*SHOW/SOURCE in effect
*/
			if (ssrc == 1)
			{
				ifl = 0;
				ascutr(rclw,&iclw[4],&ifl,UM_addr_of_f77_str(fbuf[0]),knc);
				fstr[0][knc[0]] = '\0';
				ul_win_out(fstr[0],0);
				if (knc[1] != 0)
				{
					fstr[1][knc[1]] = '\0';
					ul_win_out(fstr[1],0);
				}
			}
			cfl[0] = 1;
			cfl[1] = 0;
			cfl[2] = 0;
			break;
/*
.....TRACUT
*/
		case 7000:
			if (iclw[3] == 1)
			{
				ncl_invert_matrix(rclw,tracut);
			}
			else if (iclw[3] == 2)
			{
				tracut[0] = 1.; tracut[1] = 0.; tracut[2] = 0.; tracut[3] = 0.;
				tracut[4] = 0.; tracut[5] = 1.; tracut[6] = 0.; tracut[7] = 0.;
				tracut[8] = 0.; tracut[9] = 0.; tracut[10] = 1.; tracut[11] = 0.;
			}
			break;
/*
.....Display cutter statement
*/
		case 7100:
			UN_clfile_curpt = 0;
/*
........CUTTER/DISPLY,dia,rad,hgt,...
*/
			if (iclw[3] == 1)
			{
				if (iclw[4] == 7) claptc(rclw,&iclw[4],&ierr);
				for (i=0;i<6;i++) iclwt[i] = iclw[i];
				iclwt[5] = UN_clrec;
				if (LW_active && (scan <= 1 || scan == 4))
				{
					ifnd = ncl_process_ipvcutter(scan,iclwt,cutr,cfl,clist,sym[0],
						sym[1],sym[2],symkey);
					if (ifnd && pmod[2] == 2) goto done;
					if (scan == 4) break;
				}
				else
				{
					ifnd = ncl_process_cutter(scan,iclwt,rclw,cnv,clist,&mdid,ncutr,
						cutr,tlno,tlen);
					if (scan == 3) break;
				}
/*
........*SHOW/SOURCE in effect
*/
				if (ssrc == 1)
				{
					ifl = 1;
					ascutr(rclw,&iclw[4],&ifl,UM_addr_of_f77_str(fbuf[0]),knc);
					fstr[0][knc[0]] = '\0';
					ul_win_out(fstr[0],0);
					if (knc[1] != 0)
					{
						fstr[1][knc[1]] = '\0';
						ul_win_out(fstr[1],0);
					}
				}
				cfl[0] = 1;
			}
/*
........CUTTER/DISPLY,symbol (pre 9.6)
*/
			else if (iclw[3] == 2)
			{
				nc = 20;
				strncpy(sym[0],(char *)&rclw[0],nc);
				sym[0][nc] = '\0';
				strcpy(gsym,sym[0]);
				ul_strip_blanks(sym,&nc);
				ipt = (UM_int2 *)&rclw[3];
				cfl[0] = ipt[3];
				cutr[9] = cutr[10] = cutr[11] = cutr[12] = 0.;
				inc = 9;
				for (i=4;i<iclw[4];i++)
				{
						cutr[inc] = rclw[i] / cnv;
						inc++;
				}
				for (i=0;i<6;i++) iclwt[i] = iclw[i];
				iclwt[5] = UN_clrec;
				if (cfl[0] == 2)
				{
					cutr[18] = rclw[6] / cnv;
					cutr[19] = rclw[7] / cnv;
					if (LW_active && (scan <= 1 || scan == 4))
					{
						ifnd = ncl_process_ipvcutter(scan,iclwt,cutr,cfl,clist,
							sym[0],sym[1],sym[2],symkey);
						if (ifnd && pmod[2] == 2) goto done;
					}
					else
					{
						ifnd = ncl_process_symcutter(scan,iclwt,sym[0],&cutr[18],
							cfl[0],clist,&mdid,ncutr,tlno,tlen);
						cutr[13] = 0;
						cutr[14] = 0;
						cfl[1] = 0;
						cfl[2] = 0;
					}
				}
				else
				{
					cfl[0] = 1;
					cfl[2] = 2;
					strcpy(sym[2],sym[0]);
					sym[0][0] = '\0';
					ncl_process_holder(scan,sym[2],&cutr[9],cfl[2],2,clist,ncutr);
				}
			}
/*
........CUTTER/DISPLY,(PART,ALL)
*/
			else if (iclw[3] == 3)
			{
				ipt = (UM_int2 *)&rclw[0];
				segfl = ipt[3];
			}
/*
........CUTTER/DISPLY,SHANK (pre 9.6)
*/
			else if (iclw[3] == 4)
			{
				cfl[1] = 1;
				dfl = 0;
				inc = 13;
				for (i=0;i<iclw[4];i++)
				{
					if (rclw[i] == 716-10000) dfl = 0;
					else if (rclw[i] == 157-10000) dfl = 1;
					else
					{
						cutr[inc] = rclw[i] / cnv;
						inc++;
					}
				}
				tfl = cfl[1];
				ncl_process_holder(scan,sym[1],&cutr[13],tfl,dfl,clist,ncutr);
			}
/*
........CUTTER/DISPLY,symbol (9.6)
*/
			else if (iclw[3] == 5)
			{
				nc = 20;
				strncpy(sym[0],(char *)&rclw[0],nc);
				sym[0][nc] = '\0';
				strcpy(gsym,sym[0]);
				ul_strip_blanks(sym[0],&nc);
				ipt = (UM_int2 *)&rclw[2];
				cfl[0] = ipt[2];
				cfl[1] = 0;
				cfl[2] = 0;
				cutr[18] = rclw[3] / cnv; cutr[19] = rclw[4] / cnv;
				for (i=0;i<6;i++) iclwt[i] = iclw[i];
				iclwt[5] = UN_clrec;
				if (LW_active && (scan <= 1 || scan == 4))
				{
					ifnd = ncl_process_ipvcutter(scan,iclwt,cutr,cfl,clist,
						sym[0],sym[1],sym[2],symkey);
					if (ifnd && pmod[2] == 2) goto done;
				}
				else
				{
					ifnd = ncl_process_symcutter(scan,iclwt,sym[0],&cutr[18],cfl[0],
						clist,&mdid,ncutr,tlno,tlen);
				}
			}
/*
........CUTTER/DISPLY,SHANK-HOLDER (9.6)
*/
			else if (iclw[3] == 6)
			{
				ipt = (UM_int2 *)&rclw[2];
				if (ipt[3] == 2)
				{
					sympt = sym[2];
					cfl[2] = ipt[2];
					tfl = cfl[2];
					inc = 9;
				}
				else
				{
					sympt = sym[1];
					cfl[1] = ipt[2];
					tfl = cfl[1];
					inc = 13;
				}
				nc = 20;
				strncpy(sympt,(char *)&rclw[0],nc);
				sympt[nc] = '\0';
				strcpy(gsym,sympt);
				ul_strip_blanks(sympt,&nc);
				for (i=0;i<4;i++)
				{
					cutr[inc+i] = rclw[i+3];
					if (cutr[8]>=10)
						lthbit = cutr[8] - 10;
					else
						lthbit = 0;
					if (lthbit!=0)
						cutr[inc+i] = cutr[inc+i] / cnv;
					else if ((cfl[1] != 1 || inc+i != 15) && (cfl[2] != 1 || inc+i != 11))
					{
						cutr[inc+i] = cutr[inc+i] / cnv;
					}
/*
					if ((cfl[1] != 1 || inc+i != 15) && (cfl[2] != 1 || inc+i != 11))
						cutr[inc+i] = cutr[inc+i] / cnv;
*/
				}
				dfl = ipt[3];
				ncl_process_holder(scan,sympt,&cutr[inc],tfl,dfl,clist,ncutr);
			}
/*
........CUTTER/DISPLY,symbol (9.7)
*/
			else if (iclw[3] == 7)
			{
				ipt = (UM_int2 *)&rclw[0];
				nc = ipt[0];
				if (nc != 0) strncpy(sym[0],(char *)&rclw[3],nc);
				sym[0][nc] = '\0';
				strcpy(gsym,sym[0]);
				ul_strip_blanks(sym[0],&nc);
				cfl[0] = ipt[1];
				cfl[1] = 0;
				cfl[2] = 0;
				cutr[18] = rclw[1] / cnv; cutr[19] = rclw[2] / cnv;
				for (i=0;i<6;i++) iclwt[i] = iclw[i];
				iclwt[5] = UN_clrec;
				if (LW_active && (scan <= 1 || scan == 4))
				{
					ifnd = ncl_process_ipvcutter(scan,iclwt,cutr,cfl,clist,
						sym[0],sym[1],sym[2],symkey);
					if (ifnd && pmod[2] == 2) goto done;
				}
				else
				{
					ifnd = ncl_process_symcutter(scan,iclwt,sym[0],&cutr[18],cfl[0],
						clist,&mdid,ncutr,tlno,tlen);
				}
			}
/*
........CUTTER/DISPLY,SHANK-HOLDER (9.7)
*/
			else if (iclw[3] == 8)
			{
				ipt = (UM_int2 *)&rclw[0];
				if (ipt[2] == 2)
				{
					sympt = sym[2];
					cfl[2] = ipt[1];
					tfl = cfl[2];
					inc = 9;
				}
				else
				{
					sympt = sym[1];
					cfl[1] = ipt[1];
					tfl = cfl[1];
					inc = 13;
				}
				nc = ipt[0];
				if (nc != 0) strncpy(sympt,(char *)&rclw[5],nc);
				sympt[nc] = '\0';
				strcpy(gsym,sympt);
				ul_strip_blanks(sympt,&nc);
				for (i=0;i<4;i++)
				{
					cutr[inc+i] = rclw[i+1];
					if (cutr[8]>=10)
						lthbit = cutr[8] - 10;
					else
						lthbit = 0;
					if (lthbit!=0)
						cutr[inc+i] = cutr[inc+i] / cnv;
					else if ((cfl[1] != 1 || inc+i != 15) && (cfl[2] != 1 || inc+i != 11))
					{
						cutr[inc+i] = cutr[inc+i] / cnv;
					}
/*
					if ((cfl[1] != 1 || inc+i != 15) && (cfl[2] != 1 || inc+i != 11))
						cutr[inc+i] = cutr[inc+i] / cnv;
*/
				}
				dfl = ipt[2];
				ncl_process_holder(scan,sympt,&cutr[inc],tfl,dfl,clist,ncutr);
			}
/*
........*SHOW/SOURCE in effect
*/
			if (ssrc == 1 && iclw[3] != 1)
			{
				ifl = 1;
				ascudi(&iclw[3],rclw,&iclw[4],UM_addr_of_f77_str(fgsym),
					UM_addr_of_f77_str(fbuf[0]),knc);
				fstr[0][knc[0]] = '\0';
				ul_win_out(fstr[0],0);
				if (knc[1] != 0)
				{
					fstr[1][knc[1]] = '\0';
					ul_win_out(fstr[1],0);
				}
			}
			break;
/*
.....Cutter Profile
*/
		case 7110:
		case 7120:
			if (iclw[2] == 7110)
			{
				jpt = (UM_int4 *)&rclw[2];
				ninp = jpt[1];
				ipt = (UM_int2 *)&rclw[3];
				ctype = ipt[0];
				nc = 0;
				inc = 4;
			}
			else
			{
				jpt = (UM_int4 *)&rclw[0];
				ninp = jpt[1];
				ipt = (UM_int2 *)&rclw[0];
				ctype = ipt[1];
				nc = ipt[0];
				inc = 1 + (nc+7) / 8;
			}
			strncpy(tsym,(char *)&rclw[1],nc);
			ul_strip_blanks(tsym,&nc);
			pts = (UM_coord *)uu_malloc(sizeof(UM_coord)*ninp);
			vcs = (UM_vector *)uu_malloc(sizeof(UM_vector)*ninp);
			nout = ninp;
			nptt = ninp;
			k = 0;
			do
			{
				ival = iclw[3];
				if (nptt > 104) nptt = 104;
				for (i=0;i<nptt;i++)
				{
					if (ctype == 1)
					{
						pts[i+k][0] = rclw[inc];
						pts[i+k][1] = 0.;
						pts[i+k][2] = rclw[inc+1];
						vcs[i+k][0] = rclw[inc+2];
						vcs[i+k][1] = 0.;
						vcs[i+k][2] = rclw[inc+3];
					}
					else
					{
						pts[i+k][0] = rclw[inc];
						pts[i+k][1] = rclw[inc+1];
						pts[i+k][2] = 0.;
						vcs[i+k][0] = rclw[inc+2];
						vcs[i+k][1] = rclw[inc+3];
						vcs[i+k][2] = 0.;
					}
					inc += 4;
				}
				if (iclw[3] == 2)
				{
					clread(&UN_clfile,&irec,iclw,rclw,&jerr);
					if (scan == 0) LW_progress_count++;
					ninp = ninp - nptt;
					k = k + nptt;
					inc = 0;
				}
				if (jerr == 1) goto done;
			} while (ival == 2);
/*
........Store the tool geometry
*/
			ncl_cutter_store_geo(tsym,pts,vcs,ctype,nout);
			uu_free(pts);
			uu_free(vcs);
			break;
/*
.....SEQUNC
*/
		case 7200:
			if (iclw[3] == 1)
			{
/*
........Current point
........Do not set current point
........unless we are scanning or SEQUNC is first record in playback
........Otherwise cycle simulation playback will not work correctly
*/
				if (scan != 0 || first)
				{
					for (i=0;i<6;i++) mblock.spt[i] = rclw[i+3];
					if (trafl == 0)
					{
						itra   = 3;
						conent(mblock.spt,tracut,&itra);
						itra   = 4;
						conent(&mblock.spt[3],tracut,&itra);
					}
					mblock.spt[0] = mblock.spt[0] / cnv;
					mblock.spt[1] = mblock.spt[1] / cnv;
					mblock.spt[2] = mblock.spt[2] / cnv;
					if (!LW_active && scan != 2)
					{
						mcswcs (&i2v0, mblock.spt);
						mcswcs (&i2v1, &mblock.spt[3]);
					}
				}
/*
........CUTTER
*/
				ipt = (UM_int2 *)&rclw[27];
				if (ipt[0] == 0 || ipt[0] == 2 || ipt[0] == 3)
					ist = 9;
				else
					ist = 15;
				iclwt[0] = iclw[0]; iclwt[5] = UN_clrec;
				iclwt[4] = 1;
				for (j=0;j<6;j++) if (rclw[ist+j] != 0.) iclwt[4] = j + 1;
				segfl = ipt[1];
				if (version < 9.550)
				{
					if (ipt[0] == 0 || ipt[0] == 1)
					{
						cfl[0] = 1;
						cfl[2] = 0;
					}
					else if (ipt[0] == 2)
					{
						cfl[0] = 2;
						cfl[2] = 0;
					}
					else
					{
						cfl[0] = 1;
						cfl[2] = 2;
					}
					nc = 20;
					if (cfl[0] == 2)
					{
						strncpy(sym[0],(char *)&rclw[25],nc);
						ul_strip_blanks(sym[0],&nc);
					}
					else if (cfl[2] == 2)
					{
						strncpy(sym[2],(char *)&rclw[25],nc);
						ul_strip_blanks(sym[2],&nc);
					}
					cfl[1] = 1; if (rclw[62] == 0. || rclw[63] == 0.) cfl[1] = 0;
				}
				else
				{
					ipt1 = (UM_int2 *)&rclw[25];
					sympt = (char *)&rclw[69];
					for (i=0;i<3;i++)
					{
						nc = ipt1[i];
						if (version < 9.650) nc = 20;
						if (nc != 0)
						{
							nc = ipt1[i];
							strncpy(sym[i],sympt,nc);
							ul_strip_blanks(sym[i],&nc);
							sympt += nc;
						}
						else
							sym[i][0] = '\0';
					}
					cfl[0] = ipt[0];
					if (cfl[0] == 0) cfl[0] = 1;
					cfl[1] = ipt[4];
					cfl[2] = ipt[5];
				}
				cutr[9] = rclw[24] / cnv;
				inc = 10;
				for (i=59;i<69;i++)
				{
					cutr[inc] = rclw[i];
					if (cutr[8]>=10)
						lthbit = cutr[8] - 10;
					else
						lthbit = 0;
					if (lthbit!=0)
						cutr[inc] = cutr[inc] / cnv;
					else if ((cfl[1] != 1 || inc != 15) && (cfl[2] != 1 || inc != 11))
					{
						cutr[inc] = cutr[inc] / cnv;
					}
/*
					if ((cfl[1] != 1 || inc != 15) && (cfl[2] != 1 || inc != 11))
						cutr[inc] = cutr[inc] / cnv;
*/
					inc++;
				}
				if (LW_active && (scan <= 1 || scan == 4))
				{
					ifnd = ncl_process_ipvcutter(scan,iclwt,cutr,cfl,clist,sym,
						sym[1],sym[2],symkey);
					if (ifnd && pmod[2] == 2) goto done;
				}
				else
				{
					if (cfl[0] == 1)
						ifnd = ncl_process_cutter(scan,iclwt,&rclw[ist],cnv,clist,
							&mdid,ncutr,cutr,tlno,tlen);
					else
					{
						ifnd = ncl_process_symcutter(scan,iclwt,sym[0],&cutr[18],
							cfl[0],clist,&mdid,ncutr,tlno,tlen);
					}
				}
/*
........SHANK
*/
				if (ifnd || scan == 3 && cfl[1] != 0)
				{
					if (rclw[66] == 0) dfl = 0;
					else dfl = 1;
					ncl_process_holder(scan,sym[1],&cutr[13],cfl[1],dfl,clist,
						ncutr);
				}
/*
........HOLDER
*/
				if (ifnd || scan == 3 && cfl[2] != 0)
				{
					ncl_process_holder(scan,sym[2],&cutr[9],cfl[2],2,clist,
						ncutr);
				}
				if (ifnd && pmod[2] == 2) goto done;
/*
........MULTAX
*/
				ipt = (UM_int2 *)&rclw[30];
				npt = 3; if (ipt[1] == 1) npt = 6;
/*
........RAPID, FEDRAT, SPINDL
*/
				rpset(&ipt[0]);
				feed = rclw[31] / cnv;
				if (iclw[4] >= 59)
				{
					ftyp = ipt[2];
					feed2 = rclw[44] / cnv;
					rpm = rclw[45];
					spdir = ipt[3];
					if (ftyp == 2 && rpm != 0.)
					{
						feed  = rpm * feed2;
					}
				}
				else
				{
					ftyp = 1;
				}
/*
........TRACUT
*/
				ncl_invert_matrix(&rclw[32],tracut);
/*
........CYCLE
*/
				if (iclw[4] >= 59)
				{
					ipt = (UM_int2 *)&rclw[46];
					for (i=0;i<10;i++)
					{
						icyc[i] = ipt[i];
						rcyc[i] = rclw[i+49];
					}
					if (icyc[0] == 1)
					{
						cycfed = feed;
						if (icyc[3] == 0 || rcyc[5] == 0.) rcyc[5] = feed;
					}
				}
				else
				{
					icyc[0] = 0;
				}
			}
			break;
/*
.....Units
*/
		case 7300:
			if (iclw[3] == 1) cnv = 1.0;
			else if (iclw[3] == 2) cnv = 25.4;
			break;
/*
.....Clfile name and Date
*/
		case 7400:
			nc = 6;
			strncpy(stkstr,(char *)&rclw[14],nc);
			ul_strip_blanks(stkstr,&nc);
			ul_to_reals(&version,&i,1,stkstr);
/*
........*SHOW/SOURCE in effect
*/
			if (ssrc == 1)
			{
				asclnm (rclw,UM_addr_of_f77_str(fbuf[0]),knc);
				for (i=0;i<2;i++)
				{
					if (knc[i] > 0)
					{
						fstr[i][knc[i]] = '\0';
						ul_win_out(fstr[i],0);
					}
				}
			}
			break;
/*
.....Error message
*/
		case 8000:
			iclw[3] = 937;
			aspwrd (&iclw[3],rclw,&iclw[4],UM_addr_of_f77_str(fbuf[0]),knc);
			fstr[0][knc[0]] = '\0';
			strcpy(fstr[9],fstr[0]);
			knc[10] = knc[0];
/*
........Check for 2nd error message line
*/
			irecsv = irec;
			clread(&UN_clfile,&irec,iclw,rclw,&jerr);
			if (jerr == 1) goto done;
			if (iclw[2] == 8000)
			{
				iclw[3] = 937;
				aspwrd (&iclw[3],rclw,&iclw[4],UM_addr_of_f77_str(fbuf[0]),knc);
				if (scan == 0) LW_progress_count++;
			}
			else
			{
				irec = irecsv;
				knc[0] = 0;
			}
			fstr[0][knc[0]] = '\0';
/*
........Report error in NCLIPV playback
*/
			if (LW_active && scan == 0)
				ul_ipv_diag_error(iclw[0],fstr[9],knc[10],fstr[0],knc[0]);
/*
........*SHOW/SOURCE in effect
*/
			if (ssrc == 1)
			{
				if (knc[10] > 0) ul_win_out(fstr[9],0);
				if (knc[0] > 0) ul_win_out(fstr[0],0);
			}
			break;
/*
.....Multax
*/
		case 9000:
			npt = 3;
			if (iclw[3] == 0) npt = 6;
/*
........*SHOW/SOURCE in effect
*/
			if (ssrc == 1)
			{
				asmult(&iclw[3],&knpt,UM_addr_of_f77_str(fbuf[0]),knc);
				fstr[0][knc[0]] = '\0';
				ul_win_out(fstr[0],0);
			}
			break;
/*
.....Multax
*/
		case 14000:
			goto done;
		}
/*		UN_clfile_curpt = 0;*/
		first = UU_FALSE;
	} while ((irec != UU_NULL && irec != UN_clfile_end) || UN_clfile_curpt != 0);
	irec = UU_NULL;
/*
.....Save the current clfile position
*/
done:;
/*	if (LW_mach_mode == LW_RAPIDCUT)*/
	{
		ul_ipv_flush();
		um_reset_pocket_graphics(UM_IPV_WINDOW);
	}
 	if (pmod[2] == 0 && UN_playback_speed >= 99) ud_updatews(UG_SUPPRESS);
	ul_close_window();
	setifl(&ifl307,&val307);
	setifl(&ifl319,&val319);
	UN_clfile_current = irec;
	UN_playparm.cfl[0] = cfl[0];
	UN_playparm.cfl[1] = cfl[1];
	UN_playparm.cfl[2] = cfl[2];
	UN_playparm.npt = npt;
	UN_playparm.fedrat[0] = feed;
	UN_playparm.fedrat[1] = feed2;
	UN_playparm.rpm = rpm;
	UN_playparm.spdir = spdir;
	UN_playparm.rapid = irap;
	for (i=0;i<10;i++)
	{
		UN_playparm.icyc[i] = icyc[i];
		UN_playparm.rcyc[i] = rcyc[i];
	}
	UN_playparm.cycret = kret;
	for (i=0;i<3;i++) strcpy(UN_playparm.sym[i],sym[i]);
	for (i=0;i<6;i++)
	{
		UN_playparm.spt[i] = mblock.spt[i];
	}
	for (i=0;i<20;i++)
	{
		UN_playparm.cutr[i] = cutr[i];
	}
	for (i=0;i<12;i++)
	{
		UN_playparm.tracut[i] = tracut[i];
	}
	for (i=0;i<4;i++) UN_playparm.axis[i] = mblock.axis[i+6];
/*
.....Reset viewing transformations
.....after dynamic part play back
*/
	moving_part = 0;
	if (!LW_active)
	{
		if (movfl == 2)
		{
			UV_dynview_active = UU_FALSE;
			for (i=1;i<=UV_act_screen[0].nvports;i++)
			{
				uv_getvpid(UV_act_screen[0].vports[i-1],&vport);
				if (vport.motion)
				{
					ncl_delete_cutseg(i);
					uv_getvid(vport.cur_view,&view);
					givpn3(vport.xform,&pt);
					view.cur_pln_norm[0] = pt.x;
					view.cur_pln_norm[1] = pt.y;
					view.cur_pln_norm[2] = pt.z;
					givup3(vport.xform,&pt);
					view.cur_up_vect[0] = pt.x;
					view.cur_up_vect[1] = pt.y;
					view.cur_up_vect[2] = pt.z;
					givref3(vport.xform,&pt);
					view.cur_ref_pt[0] = pt.x;
					view.cur_ref_pt[1] = pt.y;
					view.cur_ref_pt[2] = pt.z;
					uv_putv(&view);
					uv_updatevp(&vport,UU_FALSE);
				}
			}
		}
	}
/*
.....Analyzation mode is on
.....Reset current motion color and line style
*/
	if (pmod[15] != 0 && scan == 0)
	{
		UN_motion_color = UN_motsav_color;
		UN_motion_line = UN_motsav_line;
		UN_rapid_color = UN_rapsav_color;
		UN_rapid_line = UN_rapsav_line;
		cattr.color[0] = UN_cutsav_color;
		ncl_cutter_set_attr(&cattr);
/*		if (UN_anlz_geo[0] != -1 || UN_anlz_geo[1] != -1)
			ncl_redraw_geo(pmod[4],movfl);*/
	}
/*
.....Tool list scan is active
.....Mark last tool as used
*/
	if (scan == 3 && *ncutr > 0)
	{
		cpt = (UN_cutter_list *) UU_LIST_ARRAY (clist);
		cpt[*ncutr-1].used = 1;
	}
/*
.....Reset REJECT OP jump
*/
endit:;
	UN_playback_active = UU_FALSE;
	NCL_animation = 0;
	if (imark == 1)
	{
		UD_UNMARK(markval);
	}
//need put before call UD_UNMARK
//	NCL_animation = 0;
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_pb_motout(pmod,mblock,mdata,mattr,cfrom,cirfl,cirrec)
**			Controls the output of a motion point during clfile play
**			back, including linearization sequences.
**    PARAMETERS   
**       INPUT  : 
**           pmod    = Array of flags which control the playback.
**
**           mblock  = Contains Point coming from, Point going to, 
**                     Machine axes, Forward vector, Machining time,
**                     Optional skip state, and record type.
**
**           mdata   = Motion data block.
**
**           mattr   = Motion attribute block.
**
**           cfrom   = UU_TRUE = This is a FROM statement.
**
**           cirfl   = 0 = Linear motion, 1 = Horizontal circular motion,
**                     3 = Vertical circular motion.
**
**           cirrec  = [0:2] = Center of circle, [3:5] = Circle axis,
**                     [6] = Circle radius, [7] = Delta angle.
**
**			    scan    = 0 = Perform normal playback.
**			              1 = Performing SEQUNC scanning operation only.
**			              2 = Performing bounding box scanning operation.
**			              3 = Scan for list of cutters used.
**			              4 = Preload lathe cutters onto turret.
**
**       OUTPUT :  
**			    mblock.spt  = Point going to (ept).
**    RETURNS      :
**			-1      = User interrupted playback.
**			0       = Normal completion.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
S_pb_motout(pmod,mblock,mdata,mattr,cfrom,cirfl,cirrec,scan)
int pmod[];
UN_motion_block *mblock;
UN_mot_attr *mattr;
UN_mot_data *mdata;
UU_LOGICAL cfrom;
int cirfl;
UU_REAL cirrec[];
{
	int i,j,ntim,status,sav_colr[3],colr[5],npt,kpt,cvis,isav,nvp,inc;
	int sav_cut[2];
	UU_LOGICAL flush,fromt,cirt,ffl;
	UU_REAL dis,tmp,tvc[3],vdis,evc[3],vvc[3],tdis,tpt[6],opt[5][6];
	UU_KEY_ID symkey[3];
	Gfloat ndc1[3],ndc2[3];
	UM_int2 ifl86,ifl35,ifli,i2v0=0;
	UM_angle um_angle(),ang;
	UM_coord pt1,pt2;
	UV_vport vport;
	Gnrect rect;
	UN_motseg_cutattr cattr;
	static int MXPT[10]={5,10,15,20,25,30,35,40,40,40};
	static UU_REAL MXAN[10]={60.,50.,40.,30.,20.,10.,10.,10.,10.,10.};
/*
.....Initialize routine
*/
	status = 0;
	ifli = 86;
	sav_colr[0] = UN_motion_color;
	sav_colr[1] = UN_rapid_color;
	ncl_cutter_get_defattr(&cattr);
	sav_colr[2] = cattr.color[0];
	sav_cut[0] = idc[0];
	sav_cut[1] = movfl;
	fromt = cfrom;
	cirt = cirfl;
	symkey[0] = symkey[1] = symkey[2] = 0;
/*
.....A bounding region is enabled
.....Clip points to this region
*/
	if (UN_clip_enable == 1)
	{
		ncl_clip_motion(mblock->spt,mblock->ept,opt,colr,&npt);
	}
	else
	{
		for (i=0;i<6;i++) opt[0][i] = mblock->ept[i];
		colr[0] = 1;
		npt = 1;
	}
/*
.....If step mode or delayed speed is in effect
.....then determine if this move is visible in any views
*/
	vis = 1;
	cvis = 0;
/*
..... make sure speed is between 1 and 100 percent
*/
	if (UN_playback_speed > 100) UN_playback_speed = 100;
	else if (UN_playback_speed < 1) UN_playback_speed = 1;

	if (UN_playback_speed != 100 || pmod[2] == 1)
	{
		nvp = UV_act_screen[0].nvports;
		if (LW_active) nvp = 1;
		for (i=1;i<=nvp;i++)
		{
			if (LW_active)
			{
				isav = ug_gksstli.curvwindex;
				if (LW_mach_simul)
				{
					vis = 1;
					break;
				}
				um_vctovc(mblock->spt,pt1);
				um_vctovc(mblock->ept,pt2);
				mcswcs(&i2v0,pt1);
				mcswcs(&i2v0,pt2);
				uv_cctondc(pt1,ndc1,LW_vport.xform);
				uv_cctondc(pt1,ndc2,LW_vport.xform);
				uv_getvpid(LW_vport.key,&vport);
				rect.ll.x = vport.llf[0];
				rect.ll.y = vport.llf[1];
				rect.ur.x = vport.urb[0];
				rect.ur.y = vport.urb[1];
				ug_gksstli.curvwindex = isav;
			}
			else
			{
				uv_cctondc(mblock->spt,ndc1,i);
				uv_cctondc(mblock->ept,ndc2,i);
				rect.ll.x = ug_gksstli.vtran[i].vport.llf.x;
				rect.ll.y = ug_gksstli.vtran[i].vport.llf.y;
				rect.ur.x = ug_gksstli.vtran[i].vport.urb.x;
				rect.ur.y = ug_gksstli.vtran[i].vport.urb.y;
			}
			vis = ug_lineinrect2(ndc1,ndc2,&rect);
			if (vis == 1) break;
		}
	}
/*
.....Calculate cutter shape
*/
	if (LW_active)
	{
		if ((ul_ipv_cutters(mblock->spt,cutr,mdata,mattr,scan) == UU_TRUE &&
			LW_tool_from) || LW_dntcut)
		{
			fromt = UU_TRUE;
			cirt = UU_FALSE;
		}
	}
/*
.....Loop thru output points
*/
	for (kpt=0;kpt<npt;kpt++)
	{
/*
........Reset display colors
*/
		if (colr[kpt] == 1)
		{
			cvis = 1;
			UN_motion_color = sav_colr[0];
			UN_rapid_color = sav_colr[1];
			cattr.color[0] = sav_colr[2];
			ncl_cutter_set_attr(&cattr);
			idc[0] = sav_cut[0];
			movfl = sav_cut[1];
		}
		else
		{
			UN_motion_color = 0;
			UN_rapid_color = 0;
			cattr.color[0] = 0;
			ncl_cutter_set_attr(&cattr);
			idc[0] = 0;
			movfl = 0;
		}
		um_vcmnvc(opt[kpt],mblock->spt,tvc);
		um_unitvc(tvc,tvc);
/*
........IPV is active
........Determine if we are displaying this move
*/
		flush = UU_TRUE;
		if (LW_active)
		{
			LW_step++;
			flush = UU_FALSE;
			if (LW_step >= pmod[3])
			{
				flush = UU_TRUE;
				LW_step = 0;
			}
		}
/*
........Linearize move if too long
........This gives us a smoother dynamic display
........Only linearize with moving cutter/part &
........Speed is not set to 100%
........Bobby  -  1/3/94
*/
		if (movfl != 0 && UN_playback_speed != 100 && vis == 1 &&
			colr[kpt] == 1 && !fromt && !cirt && flush)
		{
/*
...........Determine number of moves to generate
...........based on maximum screen distance allowed
*/
			inc = 10 - ((UN_playback_speed+9) / 10);
			dis = um_dcccc(mblock->spt,opt[kpt]);
			um_vcmnvc(opt[kpt],mblock->spt,tvc);
			um_unitvc(tvc,tvc);
			tmp = dis / lintol;
/*
...........Determine if angular change in tool axis
...........requires more linearization moves
...........based on a maximum of 10 degrees (.175 rads)
*/
			if (tmp > MXPT[inc]) tmp = MXPT[inc];
			ntim = tmp - 1;
			ang = um_angle(&(mblock->spt[3]),&opt[kpt][3]);
			if (ang < 0) ang = ang * -1.;
			tmp = ang / sin(MXAN[inc]);
			if (tmp > MXPT[inc]) tmp = MXPT[inc];
			i = tmp - 1; if (i > ntim) ntim = i;
/*
...........Linearize move
*/
			if (!LW_active && ntim > 0)
			{
/*
.....tmp should = ntim+1 
.....Yurong 8/27/98
*/
/*				tmp = ntim; tdis = dis / tmp;      */
				tmp = ntim+1; tdis = dis / tmp;
				vdis = um_dcccc(&(mblock->spt[3]),&opt[kpt][3]);
				vdis = vdis / tmp;
				evc[0] = mblock->spt[3];
				evc[1] = mblock->spt[4];
				evc[2] = mblock->spt[5];
				um_vcmnvc(&opt[kpt][3],&(mblock->spt[3]),vvc);
				um_unitvc(vvc,vvc);
				for (i=0;i<ntim;i++)
				{
					tpt[0] = mblock->spt[0] + tdis * tvc[0];
					tpt[1] = mblock->spt[1] + tdis * tvc[1];
					tpt[2] = mblock->spt[2] + tdis * tvc[2];
					tpt[3] = evc[0] + vdis * vvc[0];
					tpt[4] = evc[1] + vdis * vvc[1];
					tpt[5] = evc[2] + vdis * vvc[2];
					if (um_mag(&tpt[3]) < UM_DFUZZ)
					{
						for (j=0;j<3;j++)
						{
							tmp = tpt[j+3];
							tpt[j+3] = evc[j];
							evc[j] = tmp;
						}
					}
					else
					{
						evc[0] = tpt[3];
						evc[1] = tpt[4];
						evc[2] = tpt[5];
					}
					um_unitvc(&tpt[3],&tpt[3]);
					if (LW_active)
					{
						um_vctovc(tpt,mblock->ept);
						um_vctovc(evc,&(mblock->ept[3]));
						ul_ipv_pltmot(mblock,mdata,mattr,flush,fromt);
					}
					else
					{
						ncl_cutter_set(cutr,idc,flags,sym[0],sym[1],sym[2],symkey);
						pltmot(mblock->spt,tpt,mblock->vfwd);
					}
					for (j=0;j<6;j++) mblock->spt[j] = tpt[j];
					if (UN_playback_speed != 100)
					{
						ckintr(&ifl86,&ifl35);
						getifl(&ifli,&ifl86);
						if (ifl86 != 0)
						{
							status = -1;
							goto done;
						}
						ncl_pb_delay(UN_playback_speed,1);
					} /* if (UN_playback_speed) */
				} /* for (i<ntim) */
			} /* if (ntim) */
		} /* if (movfl) */
/*
.....Display final point
*/
		if (!LW_active)
		{
			ncl_cutter_set(cutr,idc,flags,sym[0],sym[1],sym[2],symkey);
			pltmot(mblock->spt,opt[kpt],&(mblock->vfwd));
		}
		else
		{
			for (i=0;i<6;i++) mblock->ept[i] = opt[kpt][i];
			if (colr[kpt] == 1 || npt > 1)
			{
				if (colr[kpt] == 1) ffl = fromt;
				else ffl = UU_TRUE;
				ul_ipv_pltmot(mblock,mdata,mattr,flush,ffl,cirt,cirrec);
			}
		}
		for (i=0;i<6;i++) mblock->spt[i] = opt[kpt][i];
	} /* for (kpt) */
/*
.....End of routine
*/
done:;
	if (cvis == 0) vis = 0;
	UN_motion_color = sav_colr[0];
	UN_rapid_color = sav_colr[1];
	cattr.color[0] = sav_colr[2];
	ncl_cutter_set_attr(&cattr);
	idc[0] = sav_cut[0];
	movfl = sav_cut[1];
	LW_dntcut = UU_FALSE;
/*	if (LW_active) um_reset_pocket_graphics(UM_IPV_WINDOW);*/
/*
.....NCLIPV is active
.....Check for clash
*/
	if (LW_active)
	{
		ckintr(&ifl86,&ifl35);
		getifl(&ifli,&ifl86);
		if (ifl86 != 0) status = -1;
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_playback_speed(speed)
**       Sets the motion playback speed.
**    PARAMETERS   
**       INPUT  : 
**          speed   = New motion playback speed setting.
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_playback_speed(speed)
int speed;
{
	UN_playback_speed = speed;
	if (UN_playback_speed < 1) UN_playback_speed = 1;
	if (UN_playback_speed > 100) UN_playback_speed = 100;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_motvwp(vp,vpnorm,vpup,vpscale,vshad)
**       Return the viewing parameters for the view in which motion
**       is being displayed.
**    PARAMETERS   
**       INPUT  : 
**          vp      = View port to return info about.
**       OUTPUT :  
**          vpnorm  = Vector normal to view plane.
**				vpup    = View up vector.
**				vpscale = View scale.
**				vmode   = Display mode.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_motvwp(vp,vpnorm,vpup,vpscale,vmode)
int *vp,*vmode;
UM_vector vpnorm,vpup;
UU_REAL *vpscale;
{
	int status,n;
	Gwpoint3 vpn;
	UU_REAL vs,vw;
	UV_vport vport;
#define SC1 .072727		/* .80 / 11.0 (Assumed 1:1 scale) */
/*
.....Get viewport normal vector
*/
	n = ug_gksstli.curvwindex;
	if (n < 1) n = 1;
	*vp = n - 1;
	givpn3(n, &vpn);
	vpnorm[0] = vpn.x;
	vpnorm[1] = vpn.y;
	vpnorm[2] = vpn.z;
/*
.....Get viewport up vector
*/
	givup3(n,&vpn);
	vpup[0] = vpn.x;
	vpup[1] = vpn.y;
	vpup[2] = vpn.z;
/*
.....Get display mode
*/
	uv_getvpid(UV_act_screen[0].vports[*vp],&vport);
	*vmode = vport.disp_mode;
/*
.....Calculate window to viewport scale factors
........Viewport extents
*/
	vs = ug_gksstli.vtran[n].vport.urb.x - ug_gksstli.vtran[n].vport.llf.x;
/*
........Window extents
*/
	vw = ug_gksstli.vtran[n].window.urb.x - ug_gksstli.vtran[n].window.llf.x;
/*
........Scale factor
*/
	vs = vs / vw;
	*vpscale = vs / SC1;
	status = UU_SUCCESS;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_motion_extrema(vp,rect)
**       Return the bounding box for displayed motion in the requested
**       view port.  Usually used with extrema zoom.
**    PARAMETERS   
**       INPUT  : 
**          vp      = View port to return info about.
**          rect    = Current 3-D rectangle for displayed digs
**			          segments.
**       OUTPUT :  
**          rect    = Bounding 3-D rectangle for motion display.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_motion_extrema(vp,rect)
int vp;
Gwrect3 *rect;
{
	int irtn,i;
	irtn = 0;
	i = vp - 1;
/*
.....Motion is displayed
.....Adjust input bounding box
.....Taking into consideration motion bounding box
*/
	if (mot_vpbox[i].ll[0] <= mot_vpbox[i].ur[0])
	{
		irtn = 1;
		ug_boxexpnt3(rect,1,mot_vpbox[i].ll);
		ug_boxexpnt3(rect,1,mot_vpbox[i].ur);
	}
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : ncl_save_iclw()
**       Saves the clfile record data for when scanning the clfile.
**       So that it can be restored for motion playback.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_save_iclw()
{
	int i;
	if (iclw[0] != -1)
	{
		for (i=0;i<6;i++) Siclw[i] = iclw[i];
		for (i=0;i<iclw[4];i++) Srclw[i] = rclw[i];
	}
}

/*********************************************************************
**    E_FUNCTION     : ncl_reset_iclw()
**       Restores the clfile record data after scanning the clfile.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_reset_iclw()
{
	int i;
	if (iclw[0] != -1)
	{
		for (i=0;i<6;i++) iclw[i] = Siclw[i];
		for (i=0;i<iclw[4];i++) rclw[i] = Srclw[i];
	}
}
