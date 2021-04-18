/*********************************************************************
**    NAME         :  newatmain.c
**       CONTAINS:  Routines for calculating waterline geometry
**
**        void nclf_wat_get_lastfl
**        watrln
**        ncl_waterline
**        ncl_waterline1
**        ncl_waterline2
**        ncl_waterline3
**        ncl_waterline3D
**        ncl_waterline4
**        ncl_waterline_progress
**
**    COPYRIGHT 2001 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       newatmain.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       01/20/17 , 10:47:46
*********************************************************************/
#include "mdattr.h"
#include "mattr.h"
#include "uminmax.h"
#include "mdpick.h"
#include "nclwaterln.h"
#include "nclclip.h"
#include "udforms.h"
#include "class.h"
#include "msol.h"

#define FPRG 5
#define CO05 .99999962

#define STOCK 0
#define PLANE 1
#define DIST 2
#define DEPTH 2
#define ZLEV 3

static NCL_sfs_dispmode *dispmode = UU_NULL;
static int Sfrm=-1;
static UU_LOGICAL Sactive = UU_FALSE;
static UD_LIST stat_list;
static UU_LIST strings;
static int allcv,sfdisp,sfview;

static int NPT = 0;
static int sfnum = 0;
static int asfnum = 0;
static int bsfnum = 0;
static int csfnum = 0;
static int Slayer = 0;
static int Smethod;

static UU_LIST sfky;
static UM_int2 GUI,mm;
static UM_int2 wbatch = 0;
static char wbuf[120];
static UM_f77_str_ptr str77;
static UU_REAL *stk_xmmx = UU_NULL, *stk_ymmx = UU_NULL;
static NCL_waterline_surf *sff = UU_NULL;
static UU_LIST ptsio,nio,overlap;
static NCL_w_geo stkbox;

static int zoning = 0;
static int godeep = 0;

static UU_LOGICAL docheck = UU_TRUE;
static UU_LOGICAL watrevsf = UU_FALSE;

static UU_REAL wtol,wtolsq;
static UU_REAL tol2d,tol2dsq;

static UM_real8 tool[6];

static UU_REAL wlev = 0;
static int ccur = 0;
static ncl_polygon *curpock = UU_NULL;
static int *njcur = UU_NULL;

static int cstk = -1, tstk = -1;
static UU_LIST stklst;
static int stklst_init = 0;

static UU_LIST levlst,shuflst;

static ncl_polygon *dpoks = UU_NULL;
static UU_LIST *njdpoks = UU_NULL;
static NCL_dpnode *dphead = UU_NULL;

static ncl_polygon Spol0,Spol1;

UU_LOGICAL NCL_waterline = UU_FALSE;
extern UU_LOGICAL UN_clip_debug;

static UU_LOGICAL Slastfl = UU_FALSE;

static UM_tessellation Stess,Scheck,Sstock;
static UU_REAL Sbox[6],Sexpand;
static UU_LIST Sbndpts;
static int Suse_box = -1;
//static UU_LOGICAL do_3d = UU_TRUE;//UU_FALSE;
//static int Scalls,Snentry=0;

//void ncl_nvmill_reset_calls()
//{
//	Scalls = 0;
//}

/* #define DBGSEQUNC */

/*********************************************************************
**    I_FUNCTION     :  ncl_vmill_get_tess(tess)
**       Retrieve tesselation of part surfaces.
*********************************************************************/
void ncl_vmill_get_tess(tess,num)
UM_tessellation **tess;
{
	if (num == 0)
		*tess = &Stess;
	else if (num == 1)
		*tess = &Sstock;
	else
		*tess = &Scheck;
}
/*********************************************************************
**    I_FUNCTION     :  ncl_vmill_get_expand(val)
**       Retrieve expansion value for BOUND type stock.
*********************************************************************/
void ncl_vmill_get_expand(use,val)
UU_LOGICAL *use;
UU_REAL *val;
{
	*use = (Suse_box == 2);
	*val = Sexpand;
}
/*********************************************************************
**    I_FUNCTION     :  ncl_vmill_get_tess(tess)
**       Retrieve tesselation of part surfaces.
*********************************************************************/
void ncl_vmill_get_boxfl(use)
int *use;
{
	*use = Suse_box;
}
/*********************************************************************
**    I_FUNCTION     :  ncl_vmill_get_tess(tess)
**       Retrieve tesselation of part surfaces.
*********************************************************************/
void ncl_vmill_get_bndpts(list)
UU_LIST **list;
{
	*list = &Sbndpts;
}
/*********************************************************************
**    I_FUNCTION     :  nclf_wat_get_lastfl()
**       Retrieve last layer flag
*********************************************************************/
void ncl_vmill_get_box(box)
UU_REAL *box;
{
	int i;
	for (i=0;i<6;i++) box[i] = Sbox[i];
}

/*********************************************************************
**    I_FUNCTION     :  nclf_wat_get_lastfl()
**       Retrieve last layer flag
*********************************************************************/
void nclf_wat_get_lastfl(last)
UM_int2 *last;
{
	if (Slastfl) *last = 1;
	else *last = 0;
}

/*********************************************************************
**    I_FUNCTION     :  Sfdisp0()
**       Retrieve and save display data for all surfaces,
**       then erase all surfaces.
*********************************************************************/
static void Sfdisp0()
{
	UU_KEY_ID *sfkey;

	dispmode = (NCL_sfs_dispmode *) uu_malloc (sfnum*sizeof(NCL_sfs_dispmode));
	if (dispmode == UU_NULL) return;
	sfkey = (UU_KEY_ID *) UU_LIST_ARRAY(&sfky);

	ncl_get_dispmodes (sfkey,sfnum,dispmode);
}

/*********************************************************************
**    I_FUNCTION     :  Sfdisp1()
**       Redisplay all surfaces as they were originally.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void Sfdisp1()
{
	UU_KEY_ID *sfkey;

	sfkey = (UU_KEY_ID *) UU_LIST_ARRAY(&sfky);
	ncl_redisp_sfs (sfkey,sfnum,dispmode);
}

/*********************************************************************
**    I_FUNCTION     :  OnSfdisp()
**       Display the layer surfaces in the way selected by user.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnSfdisp()
{
	UU_KEY_ID *sfkey;

	if (sfview == sfdisp) return ((UD_FSTAT)0);
	sfview = sfdisp;
	sfkey = (UU_KEY_ID *) UU_LIST_ARRAY(&sfky);

	ncl_disp_sfs (sfkey,sfnum,sfdisp);

	return ((UD_FSTAT)0);
}

/*********************************************************************
**    I_FUNCTION     :  OnAllcv()
**       Display or erase all surface intersections.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnAllcv()
{
	int i,nseg;
	Gseg *psg;
	Gsegvis vis;

	if (allcv)
		vis = UG_VISIBLE;
	else
		vis = UG_INVISIBLE;

	ncl_get_psegs (&nseg,&psg);
/*
..... first segment is the incomplete contour, second is the closest
..... intersection - the two are always displayed
*/
	for (i = 2; i < nseg; i++)
		gssegvis(psg[i],vis);

	return ((UD_FSTAT)0);
}

/*********************************************************************
**    I_FUNCTION     :  OnFit()
**       Zoom in on the problem area.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnFit()
{
	uz_extrema_zoom();
	return ((UD_FSTAT)0);
}

/*********************************************************************
**    I_FUNCTION     :  OnView()
**       Routine to enter dynamic viewing from the Current Status form.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnView()
{
	ud_dspfrm_invis(Sfrm);
	ud_form_invis();
	uz_dyn_mouse();
	ud_form_vis();
	ud_dspfrm_vis(Sfrm);

	return ((UD_FSTAT)0);
}

/*********************************************************************
**    I_FUNCTION     :  OnClose()
**       Method called when the Current Status form is closed.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose()
{
/*
..... redisplay the layer surfaces
*/
	if (dispmode != UU_NULL) Sfdisp1();
/*
.....Free the lists
*/
	uu_list_free (&sfky);
	UU_FREE (dispmode);
	ncl_free_segs();
	ud_free_flist(&stat_list);
	uu_list_init0 (&strings);
	Sactive = UU_FALSE;
	NCL_waterline = UU_FALSE;
	uz_repaint(0);
	Sfrm = -1;
	return ((UD_FSTAT)0);
}

/*********************************************************************
**    I_FUNCTION     :  S_put_list(list,sbuf)
**       Add a string to a list of strings
*********************************************************************/
static void S_put_list(list,sbuf)
UD_LIST *list;
char *sbuf;
{
	char *buf;
	unsigned int strlen();

	buf = (char *) uu_malloc((strlen(sbuf)+1)*sizeof(char));
	strcpy (buf,sbuf);
	uu_list_push (&strings,&buf);

	list->item = (char **) UU_LIST_ARRAY(&strings);
	list->num_item++;
}

/*********************************************************************
**    I_FUNCTION     :  S_msg_list()
**       Refresh Current Status display
*********************************************************************/
static void S_msg_list()
{
	S_put_list(&stat_list,wbuf);
	strcpy(stat_list.answer,wbuf);
	ud_dispfrm_update_answer(Sfrm,0,(int *)&stat_list);
	ud_update_form(Sfrm);
}

/*********************************************************************
**    I_FUNCTION     : S_overlap_error (sff,numsf,overlap)
**       Output the 'overlap' error message, when the connecting routine found
**       overlapping intersections. The message says which surfaces overlap.
**    PARAMETERS
**       INPUT  :
**          numsf      - number of surfaces
**          sff        - data for each surface
**          overlap    - list of overlapping curve pieces
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_overlap_error (sff,numsf,overlap)
int numsf;
NCL_waterline_surf *sff;
UU_LIST *overlap;
{
	UU_KEY_ID key0,key1,*ky;
	UU_LIST osfky;
	int i,k;
	int *npl;
	char lab0[80],lab1[80];

	npl = (int *) UU_LIST_ARRAY (overlap);
	uu_list_init(&osfky,sizeof(UU_KEY_ID),10,10);
	for (k = 0; k < overlap->cur_cnt; k += 2, npl += 2)
	{
		ncl_find_2keys(sff,numsf,npl[0],npl[1],&key0,&key1);
		if (key0 == NULLKEY || key1 == NULLKEY || key0 == key1) break;
		if (osfky.cur_cnt > 0)
		{
			ky = (UU_KEY_ID *) UU_LIST_ARRAY(&osfky);
			for (i = 0; i < osfky.cur_cnt; i += 2)
			{
				if ((key0 == ky[0] && key1 == ky[1]) ||
					(key0 == ky[1] && key1 == ky[1]))
				{
					key0 = key1 = NULLKEY;
					break;
				}
			}
		}
		if (key0 != NULLKEY && key1 != NULLKEY)
		{
			uu_list_push (&osfky,&key0); uu_list_push (&osfky,&key1);
		}
	}

	if (osfky.cur_cnt > 0)
	{
		ky = (UU_KEY_ID *) UU_LIST_ARRAY(&osfky);
		for (i = 0; i < osfky.cur_cnt; i += 2, ky += 2)
		{
			wbuf[0] = '\0'; S_msg_list ();
			ncl_get_label_with_key(ky[0], lab0);
			ncl_get_label_with_key(ky[1], lab1);
			sprintf(wbuf,"WARNING: Surfaces %s and %s may overlap.",lab0,lab1);
			S_msg_list ();
		}
	}

	uu_list_free (&osfky);
}

/*********************************************************************
**    I_FUNCTION     : S_connect_error (wbase,htop0,sff,sfnum,cgap)
**
**       Output the size of the gap, and between which surfaces and
**       which points it occured.
**    PARAMETERS
**       INPUT  :
**          cgap       - the gap struct
**          htop0      - current level
**          numsf      - number of surfaces
**          sff        - data for each surface
**          wbase      - base surface data
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_connect_error (wbase,htop0,sff,numsf,cgap)
NCL_w_base *wbase;
NCL_w_gap *cgap;
int numsf;
UU_REAL htop0;
NCL_waterline_surf *sff;
{
	UU_KEY_ID key0,key1;
	char lab0[80],lab1[80];
	UM_coord pt0,pt1;
	UU_REAL d;

	if (cgap->lnj <= 0) return;

	ncl_find_2keys(sff,numsf,cgap->lnj,cgap->cnj,&key0,&key1);

	if (key0 != NULLKEY)
		ncl_get_label_with_key(key0, lab0);
	else
		sprintf(lab0,"???");

	if (key1 != NULLKEY)
		ncl_get_label_with_key(key1, lab1);
	else
		sprintf(lab1,"???");

	if (wbatch == 1)
	{
		sprintf(wbuf,"%s %s.",lab0,lab1);
		uerror2(str77);
	}
	else
	{
		um_vctovc (cgap->lpt,pt0);
		um_vctovc (cgap->cpt,pt1);
		ncl_get_gap_data (wbase,pt0,pt1,htop0,&d);

		wbuf[0] = '\0'; S_msg_list ();

		sprintf(wbuf,"***** ERROR *****"); S_msg_list ();

		sprintf(wbuf,"Gap of %g between:",d);
		S_msg_list ();
		sprintf(wbuf,"%s at <%g,%g,%g> and",
				lab0,pt0[0],pt0[1],pt0[2]);
		S_msg_list ();
		sprintf(wbuf,"%s at <%g,%g,%g>.",
				lab1,pt1[0],pt1[1],pt1[2]);
		S_msg_list ();
	}
}

/*********************************************************************
**    E_FUNCTION     : void S_draw_pocket(nloops,ptsio)
**       Create a display segment showing the current pocket geometry.
**    PARAMETERS
**       INPUT  :
**          nloops            number of loops
**          ptsio             a list to use for points storage
**       OUTPUT :
**          psegs             the list of display segments updated
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_draw_pocket(nloops,ptsio)
int nloops;
UU_LIST *ptsio;
{
	int c,i,j,np = 0;
	int icolor = 11;
	UM_2Dcoord *vtx;
	UM_coord pt,*cpts;
	UU_REAL d;
	UM_srf_boundary *bndr;

	if (nloops < 1) return;
	ncl_psegs_init (nloops);

	ncl_init_view_box();

	if (watrevsf)
	{
		ncl_wat_get_boundary (&bndr);
		if (bndr == UU_NULL) return;
		cpts = (UM_coord *) UU_LIST_ARRAY (bndr->cvpts);
		if (cpts == UU_NULL || bndr->nb < 1 || bndr->cvpts->cur_cnt < 3) return;
		for (c = 0; c < bndr->nb; c++)
		{
			np = bndr->np[c];
			ncl_draw_polyline (np,cpts,icolor,1);
			cpts += np;
		}
	}
	else
	{
		pt[2] = wlev;

		for (c = ccur, i = 0; i < nloops; c++, i++)
		{
			if (c == cstk)
			{
				if (stklst_init == 0) return;
				np = stklst.cur_cnt;
				vtx = (UM_2Dcoord *) UU_LIST_ARRAY (&stklst);
			}
			else
			{
				np = abs(curpock->np[c]);
				vtx = (UM_2Dcoord *) UU_LIST_ARRAY (curpock->contour);
				vtx += njcur[c];
			}
			if (np <= 0) continue;
			UU_LIST_EMPTY (ptsio);

			for (j = 0; j < np; j++)
			{
				pt[0] = vtx[j][0]; pt[1] = vtx[j][1];
				uu_list_push (ptsio,pt);
			}
			d = UM_SQDIS_2D (vtx[0],vtx[np-1]);
			if (d > wtolsq)
			{
				np++;
				pt[0] = vtx[0][0]; pt[1] = vtx[0][1];
				uu_list_push (ptsio,pt);
			}
			cpts = (UM_coord *) UU_LIST_ARRAY (ptsio);
			ncl_draw_polyline (np,cpts,icolor,1);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : void ncl_waterline_progress(iprog)
**       Updates the Waterline form Progress Bar.
**    PARAMETERS
**       INPUT  :
**          iprog      Current progress.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_waterline_progress(iprog)
int iprog;
{
	int istat;
	UM_int2 ifl35,ifl86;
/*
.....Update progress field

*/
	/*if (iprog==90)
	{
		iprog = 90;
		
	}*/
	if (Sfrm != -1)
	{
		istat = iprog;
		if (istat > 100) istat = 100;
		ud_dispfrm_update_answer(Sfrm,FPRG,&istat);
		ud_update_form(Sfrm);
		ifl35 = 0;
		ckintr(&ifl86,&ifl35);
	}
}

/*********************************************************************
*********************************************************************/
int ncl_init_wpol (itsk,num)
int itsk,num;
{
	int status = UU_SUCCESS;

	if (itsk == 1)
		status = ncl_init_polygon (&Spol1,num);
	else
		status = ncl_init_polygon (&Spol0,num);
	return (status);
}

/*********************************************************************
*********************************************************************/
void ncl_free_wpols()
{
	ncl_free_polygon (&Spol0); ncl_free_polygon (&Spol1);
}

/*********************************************************************
**    E_FUNCTION     : void watrln(lnum,btkey,btyp,zb,tpkey,ttyp,zt,stktyp,
**                                 stkoff,mxgap,cnpoc,numlvl,lvdep,ier)
**       Fortran interface for the main waterline roughing routine
**    PARAMETERS
**       INPUT  :
**          lnum              layer number
**       OUTPUT :
**          ier               error (zero if none)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void watrln(lnum,btkey,btyp,zb,tpkey,ttyp,zt,stktyp,stkoff,mxgap,ftyp,frkey,
											cnpoc,numlvl,lvdep,method,ier)
UM_int4 *btkey,*tpkey,*frkey;
UM_int2 *btyp,*ttyp,*ftyp,*stktyp,*lnum,*cnpoc,*lvdep,*ier,*method;
UM_real4 *numlvl;
UM_real8 *zb,*zt,*stkoff,*mxgap;
{
	int layer,dum,ietype;
	UM_int2 ffinis;
	UM_int2 idx,ifl86;
	UM_real4 dup,ddn;
	UU_KEY_ID nclkey;
	UU_KEY_ID *keys;
	struct NCL_fixed_databag e;
	UM_sgeo geo;
	int bottype,poctype,frmtype,conpoc;
	UU_REAL mxtol2;
	NCL_w_setup wset;
	NCL_w_param wpar;

	Slayer = *lnum;

	uu_list_init(&sfky,sizeof(UU_KEY_ID),100,100);
/*
..... get the layer surface keys
*/
	vxlfst();
	idx = 35; getifl(&idx,&wbatch);
	while (UU_TRUE)
	{
		if (!ncl_vxlnxt (&nclkey,&ietype)) break;

		if (ietype == NCLI_SURF || ietype == NCLI_SOLID)
		{
			um_get_attrib (&nclkey,&dum,&dum,&dum,&dum,&layer);
			if (layer == Slayer)
			{
				e.key = nclkey;
				if (ncl_retrieve_data_fixed(&e) == UU_SUCCESS)
				{
					geo.key = e.key;
					geo.relnum = e.rel_num;
					nclu_push_sfkey (&sfky,&geo);
				}
			}
		}
		ckintr(&ifl86,&wbatch);
		idx = 86; getifl(&idx,&ifl86);
		if (ifl86 != 0)
		{
			*ier = 36; goto Done;
		}
	}

	if (sfky.cur_cnt < 1)
	{
		*ier = 36; goto Done;
	}
	asfnum = sfnum = sfky.cur_cnt;

	ncl_getnum_listkey (LIST_B,&bsfnum);
	if (bsfnum > 0)
	{
		ncl_get_listkeys (LIST_B,&keys);
		uu_list_push_multiple (&sfky,bsfnum,keys);
		sfnum += bsfnum;
	}

	ncl_getnum_listkey (LIST_C,&csfnum);
	if (csfnum > 0)
	{
		ncl_get_listkeys (LIST_C,&keys);
		uu_list_push_multiple (&sfky,csfnum,keys);
		sfnum += csfnum;
	}

	ncl_wpar_init (&wpar);

	conpoc = *cnpoc;
	godeep = 0;
	if (conpoc > 10)
	{
		conpoc = conpoc%10;
		if (conpoc > 0) godeep = 1;
	}

	bottype = *btyp;
	watrevsf = (bottype >= 100);
	if (watrevsf) bottype -= 100;

	frmtype = *ftyp;
	zoning = frmtype/100;
	frmtype = frmtype - zoning*100;

	poctype = *stktyp;

	mxtol2 = *mxgap;
	mxtol2 = mxtol2*mxtol2;

	wset.steptype = *lvdep;
	wset.zstep = *numlvl;
	wset.bottype = bottype;
	wset.zbot = *zb;
	wset.botkey = *btkey;
	wset.toptype = *ttyp;
	wset.ztop = *zt;
	wset.topkey = *tpkey;
	wset.frmkey = *frkey;
	wset.offdis = *stkoff;

	wpar.frmtype = frmtype;
	wpar.conpoc = conpoc;
	wpar.poctype = poctype;
	wpar.havestk = (conpoc >= 1 && (poctype == STK_CONTOUR || poctype == STK_BOX));

	wpar.finish = 0;
	wpar.dup = wpar.ddn = 0.;
	wgtfinis (&ffinis,&dup,&ddn);
	if (ffinis == 1)
	{
		wpar.finish = 1;
		wpar.dup = dup;
		wpar.ddn = ddn;
	}
	wpar.method = Smethod = *method;

	NCL_waterline = UU_TRUE;
	*ier = ncl_waterline (&wset,&wpar,mxtol2);

Done:
	if (wbatch == 1)
	{
		NCL_waterline = UU_FALSE;
		uu_list_free (&sfky);
	}
}

/*********************************************************************
**    E_FUNCTION     : int ncl_waterline (wset,wpar,mxtol2)
**       Intermediate waterline roughing routine.
**       INPUT  :
**          wset       - waterline settings struct
**          wpar       - waterline parameters struct
**          mxtol2     - squared maximum gap tolerance
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_waterline (wset,wpar,mxtol2)
NCL_w_setup *wset;
NCL_w_param *wpar;
UU_REAL mxtol2;
{
	UM_int2 isub;
	int status = 0;
	static int iprog;
	static char traverse[] = {1,0,0,0,0,0};
	static UD_METHOD methods[] =
	{UU_NULL,OnAllcv,OnSfdisp,OnView,OnFit,UU_NULL,OnClose};
	static char called[] = {6,6,6,6,6,6,6};
	static char display[] = {1,1,1,1,1,0};
	static int *ans[] = {(int *)&stat_list,&allcv,&sfdisp,UU_NULL,UU_NULL,
		&iprog};

	stat_list.num_item = 0;
	stat_list.item = UU_NULL;
	stat_list.answer = UU_NULL;
	uu_list_init0 (&strings);

	if (Sactive)
	{
/*
.....why close the form when ative?
.....my change just make sure there is no memory error
.....but why close the form when form is open?
.....Yurong
*/
/*
		ud_close_dispfrm (Sfrm); return (status);
*/
		if (Sfrm!=-1)
			ud_close_dispfrm (Sfrm);
		Sfrm = -1;
		Sactive = UU_FALSE;
		return (status);
	}

	isub = 264;	getifl(&isub,&mm);
	isub = 343; getifl(&isub,&GUI);

	if (wbatch != 1)
	{
		allcv = sfdisp = sfview = 0;
		Sactive = UU_TRUE;
		uu_list_init (&strings,sizeof(char *),100,100);
		stat_list.item = (char **) UU_LIST_ARRAY(&strings);
		stat_list.answer = (char *) uu_malloc(120*sizeof(char));
		strcpy(stat_list.answer," ");
		traverse[FPRG] = display[FPRG] = Smethod;
		Sfrm = ud_form_display1("watstat.frm", ans, ans, methods, called, display,
			traverse);
	}

	status = ncl_waterline1 (wset,wpar,mxtol2);
	if (Sactive) Sfdisp0();
	watrevsf = UU_FALSE;

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_waterline1 (wset,wpar,mxtol2)
**       Intermediate waterline roughing routine.
**       INPUT  :
**          wset       - waterline settings struct
**          wpar       - waterline parameters struct
**          mxtol2     - squared maximum gap tolerance
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_waterline1 (wset,wpar,mxtol2)
NCL_w_setup *wset;
NCL_w_param *wpar;
UU_REAL mxtol2;
{
	UM_int2 isub;
	UM_real8 thk,tol8,diam;

	UM_vector nvec;
	UM_transf rot;
	int i,status,ntimes;
	UU_REAL toler,zmin,zmax;

	UU_REAL areamin,trad,trad0;

	UU_REAL xmm[2],ymm[2];
	NCL_w_base base;
	struct NCL_fixed_databag bsf;
	struct NCL_nclpl_rec pl;
	UU_KEY_ID key0,*sfkey;
	char errout[80];

	um_identtf (rot);

	docheck = (wpar->poctype != STK_CONTOUR);
	curpock = UU_NULL;
	key0 = NULLKEY;

	getend (tool);

	UM_init_f77_str (str77, wbuf, 80);
	wbuf[0] = '\0';

	pl.key = bsf.key = NULLKEY;
	ncl_wbase_init (&base);

	sfkey = (UU_KEY_ID *) UU_LIST_ARRAY(&sfky);

	if (watrevsf)
	{
		base.srf = &bsf;
		status = ncl_create_wat_botsf(Slayer,wset->botkey,tool,GUI,wbuf,&base,&pl);
		if (status != UU_SUCCESS)
		{
			status = 519;
			goto Err;
		}
		if (wpar->havestk && base.key0 != NULLKEY)
		{
			if (wpar->poctype == STK_CONTOUR) wpar->poctype = STK_BOX;
			if (wset->offdis > 0) wset->offdis = 0;
		}

		key0 = bsf.key;
	}
	else
	{
		status = ncl_create_wat_botpl(&pl);
		if (status != UU_SUCCESS) goto Err;
		key0 = pl.key;
		nvec[0] = tool[3]; nvec[1] = tool[4]; nvec[2] = tool[5];
		if (UM_DOT(nvec,nvec) < UM_FUZZ)
		{
			nvec[0] = tool[3] = 0.; nvec[1] = tool[4] = 0.;
			nvec[2] = tool[5] = 1.;
		}
		ncl_define_rotmx (mm,nvec,rot,&i);
	}

	status = ur_update_displayable (key0, UM_NEVERDISPLAYABLE);
	if (status != UU_SUCCESS) goto Err;

	ncl_init_connect_lst(sfnum);
	uu_list_init (&ptsio, sizeof(UM_coord), 100, 100);
	uu_list_init (&nio, sizeof(int), 10, 10);
	uu_list_init (&overlap,sizeof(int),0,20);
	uu_list_init (&shuflst, sizeof(UM_coord), 0, 100);
	uu_list_init (&levlst, sizeof(UM_coord), 0, 100);

	if (watrevsf)
	{
		status = ncl_init_wbase_lists();
		if (status != UU_SUCCESS) goto Err;
	}

	sff = (NCL_waterline_surf *) uu_malloc (sfnum * sizeof (*sff));
	if (!sff) goto Err;

	NPT = (wpar->conpoc >= 1 && wpar->poctype == STK_CONTOUR)? 1: 0;
	if (watrevsf || wpar->havestk)
	{
		stk_xmmx = xmm; stk_ymmx = ymm;
	}
	else
	{
		stk_xmmx = stk_ymmx = UU_NULL;
	}

/* wtol is current units, wtolsq = wtol*wtol, toler is inches */
	getsct(&tol8);
	wtol = tol8;
	wtolsq = wtol*wtol;
	toler = (mm == 1)? wtol/25.4: wtol;

	isub = 24; getsc(&isub,&thk);
	isub = 28; getsc(&isub,&diam);

	trad0 = trad = 0.5*diam + thk;

	if (watrevsf) ncl_wat_set_trad (&base,tool,&trad);

	wpar->trad = trad;
	wpar->rsq = trad*trad;

	areamin = MAX2 (wpar->rsq,wtolsq);
	areamin = (UM_PI + 2.)*areamin; /* minimum island area */

	if (wbatch != 1)
	{
		sprintf (wbuf,"Preprocessing %d surfaces ...",sfnum);
		S_msg_list ();
	}

	if (watrevsf)
	{
		status = ncl_process_netsf1 (&base,sfnum,sfkey,rot,&zmax,&zmin,sff,wtol,
			toler,&NPT,areamin,stk_xmmx,stk_ymmx,wbuf);
	}
	else
	{
		status = ncl_process_netsf (0,sfnum,sfkey,rot,&zmax,&zmin,sff,wtol,toler,
			&NPT,areamin,stk_xmmx,stk_ymmx,wbuf);
	}

	if (status != UU_SUCCESS || zmax < zmin)
	{
		if (GUI == 1)
		{
			if (wbuf[0] != '\0')
			{
				sprintf(errout,"Bad surface %s.",wbuf);
				ud_wrerr(errout);
			}
			else
				ud_wrerr("Bad part geometry.");
		}
		else
		{
			if (wbuf[0] != '\0') uerror2(str77);
		}
		status = 36; goto Err;
	}

	status = ncl_wat_levels (wset,wpar,&ntimes,zmin,zmax,wtol,&tool[3],diam,GUI,wbuf);
	if (status != UU_SUCCESS)
	{
		status = 521; goto Err;
	}

	if (watrevsf)
	{
		trad = trad0;
		ncl_refine_trad (&base,stk_xmmx,stk_ymmx,&trad,&tol2d,wtol);
		wpar->trad = trad;
		wpar->rsq = trad*trad;
	}
	else
	{
		tol2d = wtol;
	}
	tol2dsq = tol2d*tol2d;
/*
..... get near point or direction vector if used for ordering pockets
*/
	status = ncl_wat_sortpt (&base,wset,wpar,stk_xmmx,stk_ymmx,rot,GUI,wtol);
	if (status != UU_SUCCESS)
	{
		status = 520; goto Err;
	}

	status = ncl_waterline2 (wset,wpar,&base,nvec,&pl,rot,ntimes,toler,mxtol2);
	goto Done;

Err:
	if (status == UU_SUCCESS || status == UU_FAILURE) status = 177;

Done:
	if (sff)
	{
		int isf;
		NCL_waterline_surf *p1;
		p1 = sff;
		for (isf = 0; isf < sfnum && p1->key != NULLKEY; isf++, p1++)
		{
			ncl_free_bound (&p1->bound);
			UU_LIST_FREE (p1->trianlist);
		}
		UU_FREE (sff);
	}
	stk_xmmx = stk_ymmx = UU_NULL;
	ncl_free_uv();
	ncl_free_nios();
	ncl_free_aux_ptlist();
	ncl_free_tripolst();
	ncl_free_connect_lst();
	uu_list_free (&ptsio);
	uu_list_free (&nio);
	uu_list_free (&overlap);
	uu_list_free (&shuflst);
	uu_list_free (&levlst);
	if (key0 > NULLKEY) uc_delete (key0);
	if (watrevsf)
		ncl_free_wbase_lists();

	return (status);
}

#if 0
/*********************************************************************
**    I_FUNCTION     : UU_LOGICAL S_find_interlev0 (h1,h0,tol,hint)
**       Find a cutting level (to finish) between the previous and
**       the current levels.
**    PARAMETERS
**       INPUT  :
**          h1         - previous level
**          h0         - current level
**          tol        - tolerance
**       OUTPUT :
**          hint       - intermediate level
**    RETURNS      : UU_TRUE iff found
**    SIDE EFFECTS : none
*********************************************************************/
static UU_LOGICAL S_find_interlev0 (h1,h0,tol,hint)
UU_REAL h0,h1,tol,*hint;
{
	UU_REAL h;
	int isf;
	NCL_waterline_surf *sfi;
	UU_LOGICAL lfound;

	*hint = h = h0;
	lfound = UU_FALSE;

	for (isf = 0, sfi = sff; isf < sfnum; isf++,sfi++)
	{
		if (sfi->key != NULLKEY && sfi->sf_flag == HORZPL)
		{
			if (sfi->zmax < h1-tol && sfi->zmin > h+tol)
			{
				lfound = UU_TRUE;
				h = sfi->zmax;
			}
		}
	}
	if (lfound) *hint = h;

	return (lfound);
}
#endif

/*********************************************************************
**    I_FUNCTION     : UU_LOGICAL S_find_interlev (h1,h0,dup,ddn,tol,hint,ladj)
**       Find a cutting level (to finish) between the previous and
**       the current levels.
**    PARAMETERS
**       INPUT  :
**          h1         - previous level
**          h0         - current level
**          ladj       - current level may be adjusted if TRUE
**          dup        - current level may be adjusted up by this number
**          ddn        - current level may be adjusted down by this number
**          tol        - tolerance
**       OUTPUT :
**          hint       - intermediate level
**          ladj       - current level will be adjusted if TRUE
**    RETURNS      : UU_TRUE iff found
**    SIDE EFFECTS : none
*********************************************************************/
static UU_LOGICAL S_find_interlev (h1,h0,dup,ddn,tol,hint,ladj)
UU_REAL h0,h1,dup,ddn,tol,*hint;
UU_LOGICAL *ladj;
{
	UU_REAL h,hmax,hmin;
	int isf;
	NCL_waterline_surf *sfi;
	UU_LOGICAL lfound,ladj0;

	*hint = h = h0;
	lfound = UU_FALSE;
	ladj0 = *ladj;

	hmax = h1 - tol;
/*
..... if current level may be adjusted, decrease the range lower bound by ddn
*/
	if (ladj0 && ddn > tol)
		hmin = h0 - ddn - 0.01*tol;
	else
		hmin = h0 + tol;
/*
..... find the highest level surface within the range
*/
	for (isf = 0, sfi = sff; isf < sfnum; isf++,sfi++)
	{
		if (sfi->key != NULLKEY && sfi->sf_flag == HORZPL)
		{
			if (sfi->zmax < hmax && sfi->zmin > hmin)
			{
				if (lfound)
				{
					if (sfi->zmin > h + tol)
						h = sfi->zmax;
				}
				else
				{
					lfound = UU_TRUE;
					h = sfi->zmax;
				}
			}
		}
	}
	if (lfound)
	{
		*hint = h;
/*
..... if the current level may be adjusted, and the found level is within the
..... limits to the current level, the current one will be adjusted
*/
		*ladj = ladj0 && (h-.01*tol <= h0 + dup && h+.01*tol >= h0 - ddn);
	}

	return (lfound);
}

/*********************************************************************
**    I_FUNCTION     : void S_find_nlevs (wpar,w0,nlevs)
**       Find the total number of cutting levels, including intermediate
**       finishing levels.
**    PARAMETERS
**       INPUT  :
**          wpar       - parameters
**          w0         - top level
**          nlevs      - initial number of levels
**       OUTPUT :
**          nlevs      - total number of levels
**    RETURNS      : UU_TRUE iff found
**    SIDE EFFECTS : none
*********************************************************************/
static void S_find_nlevs (wpar,w0,nlevs)
NCL_w_param *wpar;
UU_REAL w0;
int *nlevs;
{
	UU_REAL wprev,wnext,wcur,wi;
	int itim,ntim;
	UU_LOGICAL levbetween = UU_FALSE;
	UU_LOGICAL ladj = UU_FALSE;
	UU_LOGICAL lv98;

	ntim = *nlevs;
	wcur = wnext = w0;
	lv98 = ncl_setver(98);

	for (itim = 0; itim < ntim; itim++)
	{
		wprev = wcur;
		if (!levbetween)
		{
			wnext = wnext - wpar->dz;
		}

		wcur = wnext;

		levbetween = UU_FALSE;
		if (itim > 0 || !lv98)
		{
			ladj = (wpar->finish == 2 && (itim < ntim - 1 || !lv98));

			levbetween = S_find_interlev (wprev,wcur,wpar->dup,wpar->ddn,wtol,&wi,
				&ladj);
			if (levbetween)
			{
				wcur = wi;
				if (ladj) levbetween = UU_FALSE;
			}
			if (levbetween) ntim++;
		}
	}
	*nlevs = ntim;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_waterline2 (wset,wpar,base,rot,ntimes,toler,mxtol2)
**       Intermediate waterline roughing routine.
**       Intermediate waterline roughing routine.
**       INPUT  :
**          wset       - waterline settings struct
**          wpar       - waterline parameters struct
**          wbase      - base surface data
**			nvec	   - unit vector UP
**          pl         - a Unibase plane structure, to be used for intersections
**          rot        - rotation matrix
**          ntimes     - number of horizontal levels to cut
**          toler      - Unibase tolerance
**          mxtol2     - square of the acceptable gap parameter
**    RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_waterline2 (wset,wpar,wbase,nvec,pl,rot,ntimes,toler,mxtol2)
NCL_w_setup *wset;
NCL_w_param *wpar;
NCL_w_base *wbase;
UM_vector nvec;
struct NCL_nclpl_rec *pl;
UM_transf rot;
int ntimes;
UU_REAL toler,mxtol2;
{
	int i,nlevs;
	int status = UU_SUCCESS;
	UU_LOGICAL lv98;

	lv98 = ncl_setver(98);
/*
..... zone ordering
*/
	if (zoning > 0)
	{
		status = ncl_prepare_zones (wbase,&nio,&ptsio,rot,toler);
		if (status != UU_SUCCESS)
		{
			if (GUI == 1)
				ud_wrerr("Zones are not defined.");
			status = 494;
			goto Done;
		}
	}

	if ((ntimes > 1 || !lv98) && wpar->finish > 0)
	{
		if (wpar->ddn > wtol || wpar->dup > wtol)
		wpar->finish = 2;
	}

/*
..... prepare storage for deep cutting
*/
	if (godeep == 1)
	{
		if (ntimes > 1)
		{
			nlevs = ntimes;
			if (wpar->finish > 0)
				S_find_nlevs (wpar,wlev,&nlevs);

			dpoks = (ncl_polygon *) uu_malloc (nlevs*sizeof(ncl_polygon));
			njdpoks = (UU_LIST *) uu_malloc (nlevs*sizeof(UU_LIST));
			if (!dpoks || !njdpoks)
			{
				status = UU_FAILURE; goto Done;
			}
			for (i = 0; i < nlevs; i++)
			{
				ncl_init_polygon (&dpoks[i],0);
				uu_list_init0 (&njdpoks[i]);
			}
		}
		else
			godeep = 0;
	}

	status = ncl_waterline3 (wset,wpar,wbase,nvec,pl,rot,ntimes,mxtol2);

Done:
	if (status == UU_FAILURE) status = 177;
	if (zoning > 0) ncl_free_zones ();
	if (dpoks)
	{
		for (i = 0; i < nlevs; i++) ncl_free_polygon (&dpoks[i]);
		UU_FREE (dpoks);
	}
	if (njdpoks)
	{
		for (i = 0; i < nlevs; i++) uu_list_free (&njdpoks[i]);
		UU_FREE (njdpoks);
	}

	if (bsfnum > 0) ncl_free_keylist(LIST_B);
	if (csfnum > 0) ncl_free_keylist(LIST_C);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_waterline3 (wset,wpar,wbase,pl,rot,ntimes,mxtol2)
**       Intermediate waterline roughing routine.
**       INPUT  :
**          wset       - waterline settings struct
**          wpar       - waterline parameters struct
**          wbase      - base surface data
**          pl         - a Unibase plane structure, to be used for intersections
**          ntimes     - number of horizontal levels to cut
**          mxtol2     - square of the acceptable gap parameter
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_waterline3 (wset,wpar,wbase,nvec,pl,rot,ntimes,mxtol2)
NCL_w_setup *wset;
NCL_w_param *wpar;
NCL_w_base *wbase;
UM_vector nvec;
struct NCL_nclpl_rec *pl;
UM_transf rot;
int ntimes;
UU_REAL mxtol2;
{
	int status = UU_SUCCESS;
	UU_REAL dist;
	UM_2box bxbox;
	UM_coord boxpt[4];
	UU_LIST *plst = UU_NULL;

/*
..... initialize global polygons
*/
	if (NPT < 100) NPT = 100;
	uu_list_init0 (&stklst);

	status = ncl_init_wpol (0,2*NPT);
	if (status == UU_SUCCESS) ncl_init_wpol (1,NPT);

	if (status != UU_SUCCESS) goto Done;

	ncl_init_edgelst (2*NPT);
/*
..... create a stock
*/
	if (wpar->havestk)
	{
/*
..... box stock - even if the contour stock is specified, the box stock
..... is used for all preliminary operations: intersection with the previous
..... level, overhangs, etc. The 'real' contour stock is calculated and
..... processed once, and then used only for actual pocketing
*/
		stkbox.pt = &boxpt[0];
		if (watrevsf)
			status = ncl_create_2box1 (wbase,stk_xmmx,stk_ymmx,wset->offdis,
			&bxbox,wtol);
		else
		{
			if (wpar->poctype == STK_CONTOUR && wpar->method <= 1)
				dist = wset->offdis + wpar->trad;
			else
				dist = wset->offdis;
			ncl_create_2box (stk_xmmx,stk_ymmx,dist,&bxbox);
		}

		if (status == UU_SUCCESS)
		{
			ncl_create_boxgeo (&bxbox,&stkbox);
			if (watrevsf && wpar->poctype == STK_BOX)
			{
				status = ncl_box_stklst (wbase,&stklst,&bxbox,&ptsio,wtol);

				if (stklst.data != UU_NULL)
				{
					stklst_init = 1;
					plst = &stklst;
				}
			}
		}
		if (wpar->poctype == STK_CONTOUR && status == UU_SUCCESS)
		{
/*
..... contour stock - a minimal contour containing horizontal projections of
..... all surfaces - is contained in the global Spol0
*/
			wbuf[0] = '\0';
			if (wbatch != 1)
			{
				sprintf (wbuf,"Creating stock ..."); S_msg_list ();
			}
			status = ncl_create_contour_stock (sfnum,sff,rot,tol2d,tol2dsq);
			if (status == UU_SUCCESS && Spol0.contour->cur_cnt >= 3)
			{
#if 0	
				ncl_debug_contour_stock(Spol0.contour,wlev);
#endif
/*
..... offset the stock by the expansion factor
*/
				if (watrevsf)
				{
					status = ncl_offset_out1 (wbase,&stklst,Spol0.contour,&ptsio,
						wset->offdis,tol2d,wtol);
				}
				else
				{
					ncl_find_circles0 (Spol0.contour,&bxbox,wtol,NPT);
					status = ncl_offset_out (&stklst,Spol0.contour,&ptsio,
						wset->offdis,tol2d,tol2dsq);
				}
				if (stklst.data != UU_NULL)
				{
					stklst_init = 1;
					plst = &stklst;
				}
			}
		}

		if (status != UU_SUCCESS)
		{
			if (GUI == 1)
				ud_wrerr("Could not create stock.");
			if (status == UU_FAILURE) status = 501;
			goto Done;
		}
		if (zoning)
			ncl_stk_zone (plst,&bxbox,zoning);

		if (watrevsf && fabs(wset->offdis) < wtol)
		{
/*
..... create a fake box offset to help polygon operations (there may be
..... problems when holes touch outer contours)
*/
			ncl_create_2box (stk_xmmx,stk_ymmx,0.1,&bxbox);
			ncl_create_boxgeo (&bxbox,&stkbox);
		}
	}
 	if (wpar->method > 1)	// Sasha, Feb.02, 2021
	/*if (wpar->method == 2)*/
		status = ncl_waterline3D (wset,wpar,&bxbox,wbase,nvec,pl,rot,ntimes,
			wset->offdis,mxtol2);
	/*else if (wpar->method == 3)
	{
		;
	}*/
 	else
		status = ncl_waterline4 (wset,wpar,&bxbox,wbase,nvec,pl,rot,ntimes,
			wset->offdis,mxtol2);

Done:
	if (status == UU_FAILURE) status = 177;
	ncl_free_wpols();
	ncl_free_edgelst ();
	uu_list_free (&stklst); stklst_init = 0;

	return (status);
}

/*********************************************************************
*********************************************************************/
static void S_wpock_warn(ier,numwarn)
UM_int2 ier;
int *numwarn;
{
	if (ier == -475)
		sprintf(wbuf,"Entry fixed to avoid gouging at Z = %g.",wlev);
	else if (ier == -511)
		sprintf(wbuf,"Exit fixed to avoid gouging at Z = %g.",wlev);
	else if (ier == -510)
		sprintf(wbuf,"Pocket exit violates geometry at Z = %g.",wlev);
	else
		sprintf(wbuf,"Pocket entry violates geometry at Z = %g.",wlev);
	S_msg_list ();
	*numwarn = -ier;
}

/*********************************************************************
*********************************************************************/
static void S_wpock_err (itry,nloops)
int itry,nloops;
{
	ud_setfrm_traverse_mask(Sfrm,2,UU_TRUE);
	ud_setfrm_traverse_mask(Sfrm,3,UU_TRUE);
	ud_setfrm_traverse_mask(Sfrm,4,UU_TRUE);

	wbuf[0] = '\0';	S_msg_list ();
	sprintf(wbuf,"***** Pocketing ERROR *****"); S_msg_list ();
	if (itry > 0)
	{
		sprintf(wbuf,"Could not pocket contour number %d.",itry);
		S_msg_list ();
	}

	S_draw_pocket (nloops,&ptsio);
}

#if 0
/*********************************************************************
*********************************************************************/
static int S_bndr_pol (bound,pol)
UM_srf_bound *bound;
ncl_polygon *pol;
{
	int nb,ib,iv,nv,c;
	UM_coord *pts;

	nb = bound->nb;
	pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);

	if (!pol->contour)
	{
		pol->contour = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
		if (!pol->contour) return (UU_FAILURE);
		nv = bound->cvpts->cur_cnt;
		if (nv < 100) nv = 100;
		uu_list_init (pol->contour,sizeof(UM_2Dcoord),nv,nv);
	}
	if (pol->contour->data  == UU_NULL) return (UU_FAILURE);
	UU_LIST_EMPTY(pol->contour);

	pol->num_contours = nb;
	UU_FREE (pol->np);
	pol->np = (int *) uu_malloc (nb*sizeof(int));
	if (pol->np == UU_NULL) return (UU_FAILURE);

	for (ib = 0, c = 0; ib < nb; ib++, pts += (nv+1))
	{
		nv = (ib == 0)? bound->npo - 1: abs(bound->np[ib-1]) - 1;
		if (ib > 0 && bound->np[ib-1] < 0)
		{
			pol->num_contours--;
			continue;
		}
		pol->np[c] = (ib == 0)? nv: -nv;
		c++;
		for (iv = 0; iv < nv; iv++)
		uu_list_push (pol->contour,pts[iv]);
	}

	UU_FREE (pol->box);
	pol->box = (UM_2box *) uu_malloc (nb*sizeof(UM_2box));
	if (pol->box == UU_NULL) return (UU_FAILURE);
	ncl_calc_boxes (pol);

	return (UU_SUCCESS);
}
#endif

/*********************************************************************
**    I_FUNCTION     : S_area_test (pol,tolsq)
**       Weed out polygon contours if too small.
**    PARAMETERS
**       INPUT  :
**          pol        - polygon structure
**          tolsq      - tolerance (squared)
**       OUTPUT :
**          pol        - updated polygon structure
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_area_test (pol,areamin,tolsq)
ncl_polygon *pol;
UU_REAL areamin,tolsq;
{
	int nc,c,nv,i,nv0,num;
	UM_2Dcoord *pp;
	UU_REAL a,ai,area = 0.;

	num = nc = pol->num_contours;

	for (c = 0, nv0 = 0; c < nc; c++)
	{
		nv = pol->np[c];
		if (nv < 3)
		{
			num--;
			nv = abs(nv);
			nv0 += nv;
		}
		else
		{
			pp = (UM_2Dcoord *) UU_LIST_ARRAY (pol->contour);
			pp += nv0;

			a = UM_SQDIS_2D(pp[0],pp[1]);
			ai = UM_SQDIS_2D(pp[0],pp[nv-1]);
			if (ai > a) a = ai;
			for (i = 1, area = 0.; i < nv - 1; i++)
			{
				area += um_triangle_signed_area(pp[0],pp[i],pp[i+1]);
				ai = UM_SQDIS_2D(pp[i],pp[i+1]);
				if (ai > a) a = ai;
			}
			if ((fabs(area) < areamin) || (nv < 5 && area*area < a*tolsq))
			{
				num--;
			}
			nv0 += nv;
		}
	}

	return (num);
}

/*********************************************************************
**    S_FUNCTION     : S_shuffle (cvpoint,npts,eps)
**       Make the contour start and end at the middle of the longest segment
**       before offsetting. This way, the endpoints will match after offsetting.
**       
**    PARAMETERS
**       INPUT  : 
**                cvpoint     - the points list
**                npts        - number of points
**                eps         - squared distance parameter
**       OUTPUT :
**                cvpoint     - shuffled points list
**                npts        - new number of points
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_shuffle (cvpoint,npts,eps)
UU_LIST *cvpoint;
int npts;
UU_REAL eps;
{
	int i,imax,j,n0,n1;
	UU_REAL d,dmax;
	UM_coord *pp,*qq;
	UM_coord pti;

	npts = cvpoint->cur_cnt;
	pp = (UM_coord *) UU_LIST_ARRAY (cvpoint);

	imax = -1;
	dmax = eps;
/*
..... shuffle if the longest segment is more than eps
*/
	for (i = 0; i < npts-2; i++)
	{
		d = UM_SQDIS_2D(pp[i],pp[i+1]);
		if (d > dmax)
		{
			dmax = d; imax = i;
		}
	}

	if (imax >= 0)
	{
		n0 = npts - 1; n1 = npts + 1;
		UU_LIST_EMPTY (&shuflst);

		um_middlept (pp[imax],pp[imax+1],pti);

		uu_list_push (&shuflst,pti);
		for (i = 1; i < npts; i++)
		{
			j = (imax + i) % n0;
			uu_list_push (&shuflst,&pp[j]);
		}
		uu_list_push (&shuflst,pti);

		UU_LIST_EMPTY (cvpoint);
		qq = (UM_coord *) UU_LIST_ARRAY (&shuflst);
		uu_list_push_multiple (cvpoint,n1,qq);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_offset_bndr_pol (bound,points,pol,dis)
**       Offset a surface outer boundary and put the result into a polygon.
**    PARAMETERS
**       INPUT  :
**          bound      - boundary structure
**          points     - initialized points list to use
**          dis        - offset distance
**       OUTPUT :
**          pol        - polygon structure
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_offset_bndr_pol (bound,points,pol,dis)
UM_srf_bound *bound;
UU_LIST *points;
ncl_polygon *pol;
UU_REAL dis;
{
	UM_coord *pts;
	int npt,status,lr,n1,i;
	UM_vector dm;

	pts = (UM_coord *) UU_LIST_ARRAY (bound->cvpts);
	npt = bound->npo;

	UU_LIST_EMPTY (points);
	uu_list_push_multiple (points,npt,pts);

	S_shuffle (points,npt,10000*tol2dsq);
	npt = points->cur_cnt;
/*
..... get the direction vector for the first point
*/
	status = ncl_out_dm (points,NULLST,dm,&lr);

	if (status == UU_SUCCESS)
	{
		n1 = ncl_offset_contour0 (tol2d,npt,dm,dis,points);
		if (n1 < 4) status = UU_FAILURE;
	}

	if (status == UU_SUCCESS)
	{
		npt = points->cur_cnt;
		pts = (UM_coord *) UU_LIST_ARRAY (points);

		pol->num_contours = 1;
		pol->np[0] = npt;

		UU_LIST_EMPTY (pol->contour);
		for (i = 0; i < npt; i++)
		{
			uu_list_push (pol->contour,&pts[i]);
		}		
	}

	return (status);
}

/*********************************************************************
**    I_FUNCTION     : S_relevant_pieces (cpol,points,trad,rsq)
**       Collect this level horizontal surfaces, expand them by trad, then
**       intersect with the current level polygon.
**    PARAMETERS
**       INPUT  :
**          cpol       - current level polygon
**          points     - initialized points list to use
**          trad       - pocket offset parameter (trad+thk)
**          rsq        - used to calculate minimal area
**       OUTPUT :
**          cpol       - updated level polygon
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_relevant_pieces (cpol,points,trad,rsq)
ncl_polygon *cpol;
UU_LIST *points;
UU_REAL trad,rsq;
{
	int isf,status,nc;
	int np1;
	NCL_waterline_surf *sfi;
	UU_LOGICAL lfound;
	UU_REAL areamin = (UM_PI + 2.)*rsq; /* minimum island area */

	Spol1.np = &np1;
	lfound = UU_FALSE;
	nc = 0;
/*
..... a surface needs to be expanded because we want to machine it all; and
..... perimeters are offset inside when pocketed.
*/
	for (isf = 0, sfi = sff; isf < sfnum; isf++,sfi++)
	{
		if (sfi->key != NULLKEY && sfi->sf_flag == HORZPL &&
			sfi->zmin > wlev - wtol && sfi->zmax < wlev + wtol)
		{
			if (lfound)
				status = S_offset_bndr_pol (&sfi->bound,points,&Spol1,trad);
			else
			{
				Spol0.np = (int *) uu_malloc (sizeof(int));
				status = S_offset_bndr_pol (&sfi->bound,points,&Spol0,trad);
				if (status != 0) UU_FREE (Spol0.np);
			}

			if (status != 0) continue;

			if (!lfound)
			{
				lfound = UU_TRUE;
			}
			else
			{
				status =
				ncl_polygon_clip (NCL_UNION,&Spol0,&Spol1,&Spol0,tol2d,tol2dsq);
				if (status != UU_SUCCESS) goto Done;
			}
		}
	}
/*
..... intersect the union of expanded boundaries with the current polygon
*/
	if (Spol0.num_contours > 0)
	{
		ncl_calc_boxes (&Spol0);
		status = ncl_polygon_clip (NCL_INTOF,cpol,&Spol0,cpol,tol2d,tol2dsq);
		if (status == 0)
		{
			nc = cpol->num_contours;
			if (nc > 0) nc = S_area_test (cpol,areamin,tol2dsq);
		}
		else
			nc = 0;
	}
		
Done:
	UU_FREE (Spol0.np);
	UU_FREE (Spol0.box);
	Spol1.np = UU_NULL;

	return (nc);
}

/*********************************************************************
**    I_FUNCTION     : ncl_pt_on_bounding_surfaces(pt,tol)
**       Check if the point is on the bounding surfaces or nor.
**    PARAMETERS
**       INPUT  :
**          pt			- the point to check
**          tol		    - tolerance
**       OUTPUT :
**          none   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_pt_on_bounding_surfaces(pt,tol)
UM_coord pt;
UU_REAL tol;
{
	int isf,status;
	NCL_waterline_surf *sfi;

	status = UU_FAILURE;

	for (isf = 0, sfi = sff; isf < sfnum; isf++,sfi++)
	{
		if (sfi->slist != LIST_B)
			continue; 
/*
.....Check if the point is the on bounding surface
*/
		status = ncl_pt_on_surface(pt,sfi,tol);
		if (status == UU_SUCCESS) 
			return UU_SUCCESS;
	}

	return status;
}

/*********************************************************************
**    I_FUNCTION     : ncl_contour_on_bounding_surfaces(cvsi,tol)
**       Check if the contours cvsi is on the bounding surfaces or nor.
**    PARAMETERS
**       INPUT  :
**          cvsi		- the contour to check
**          tol		    - tolerance
**       OUTPUT :
**          none   
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_contour_on_bounding_surfaces(cvsi,tol)
NCL_w_geo *cvsi;
UU_REAL tol;
{
	int i,status;
	NCL_waterline_surf *sfi;

	status = UU_FAILURE;
/*
..... check if the contour is on the bounding surfaces.
*/
	for (i = 0; i < cvsi->np; i++)
	{
		status = ncl_pt_on_bounding_surfaces(cvsi->pt[i],tol);
		if (status != UU_SUCCESS)
			return status;
	}

	return status;
}

/*********************************************************************
**    I_FUNCTION     : UU_LOGICAL S_find_botpl (plkey)
**       Find bottom plane inside part surfaces or not
**    PARAMETERS
**       INPUT  :
**          plkey      - plane key
**       OUTPUT :	none
**    RETURNS      : UU_TRUE iff found
**    SIDE EFFECTS : none
*********************************************************************/
static UU_LOGICAL S_find_botpl (plkey)
UU_KEY_ID plkey;
{
	int isf;
	NCL_waterline_surf *sfi;
	UU_LOGICAL lfound = UU_FALSE;
	for (isf = 0, sfi = sff; isf < sfnum; isf++,sfi++)
	{
		if (sfi->key != NULLKEY && sfi->sf_flag == HORZPL)
		{
			if (sfi->key == plkey)
			{
				lfound = UU_TRUE;
				break;
			}
		}
	}

	return (lfound);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_waterline3D (wset,wpar,mxtol2)
**       Main waterline roughing routine for 3-Axis VoluMill style motion.
**    PARAMETERS
**       INPUT  :
**          wset       - waterline settings struct
**          wpar       - waterline parameters struct
**          mxtol2     - squared maximum gap tolerance
**       OUTPUT : none
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_waterline3D (wset,wpar,bxbox,wbase,nvec,pl,rot,ntimes,
	offdis,mxtol2)
NCL_w_setup *wset;
NCL_w_param *wpar;
UM_2box *bxbox;
NCL_w_base *wbase;
UM_vector nvec;
struct NCL_nclpl_rec *pl;
UM_transf rot;
int ntimes;
UU_REAL offdis,mxtol2;
{
	int isf,i,j,n,status,stat,npts,irot,numwarn;
	struct UC_entitydatabag eptr;
	struct UM_solid_rec *solid;
	NCL_waterline_surf *sfi;
	UM_transf tfmat;
	UM_int4 botkey;
	UM_int2 idx,mm,isub,nlps,offpr,lmotion,ier;
	UU_REAL tol,rval,tsav,dtol,tbox[6],fact;
	UM_real8 tol8,topdis,offdis8;
	UM_plane plane;
	UM_coord pts[3];
	UM_tessellation tess,tesstmp;
	UM_trian *tript;
	UM_srf_boundary *bndr;
	UU_LOGICAL found,init = UU_FALSE;
	UU_LIST dlist,*tri,*uvtri,poly;
	char buf[80];
	ncl_get_rotfl (&irot);
	Sbox[0] = Sbox[1] = Sbox[2] = 1.e12;
	Sbox[3] = Sbox[4] = Sbox[5] = -1.e12;
	Sexpand = 0.;
/*
.....Set up tesselation data.
*/
	gettol(&tol8); tol = tol8;
	idx = 264; getifl(&idx,&mm);
	fact = (mm)? 1./25.4 : 1.;
	um_init_tess(&Stess); um_init_tess(&tesstmp); um_init_tess(&tess);
	uu_list_init(&Stess.vertices,sizeof(UM_coord),10000,2000);
	uu_list_init(&Stess.normals,sizeof(UM_vector),10000,2000);
	uu_list_init(&Stess.tri,sizeof(UM_tript),10000,2000);
	if (bsfnum > 0)
	{
		um_init_tess(&Sstock);
		uu_list_init(&Sstock.vertices,sizeof(UM_coord),10000,2000);
		uu_list_init(&Sstock.normals,sizeof(UM_vector),10000,2000);
		uu_list_init(&Sstock.tri,sizeof(UM_tript),10000,2000);
	}
	if (csfnum > 0)
	{
		um_init_tess(&Scheck);
		uu_list_init(&Scheck.vertices,sizeof(UM_coord),10000,2000);
		uu_list_init(&Scheck.normals,sizeof(UM_vector),10000,2000);
		uu_list_init(&Scheck.tri,sizeof(UM_tript),10000,2000);
	}
	uu_list_init(&tesstmp.vertices,sizeof(UM_coord),10000,2000);
	uu_list_init(&tesstmp.normals,sizeof(UM_vector),10000,2000);
	uu_list_init(&tesstmp.tri,sizeof(UM_tript),10000,2000);
	uu_list_init(&tess.vertices,sizeof(UM_coord),10000,2000);
	uu_list_init(&tess.normals,sizeof(UM_vector),10000,2000);
	uu_list_init(&tess.tri,sizeof(UM_tript),10000,2000);
/*
.....Get tessellation lists
*/
	idx = 175; getsc(&idx,&tsav); rval = tol; setscv(&idx,&rval);
	for (isf = 0, sfi = sff; isf < sfnum; isf++,sfi++)
	{
		eptr.key = sfi->key;
		ncl_retrieve_data_fixed(&eptr);
		uc_retrieve_transf (sfi->key, tfmat);
		if (nclc_tessmgr_check_sfkey(sfi->key) == UU_SUCCESS)
		{
			found = UU_TRUE;
/*
.....Initialize surface boundary
*/	
			if (!init)
			{
				bndr = (UM_srf_boundary *) uu_malloc (sizeof(UM_srf_boundary));
				um_init_boundary (bndr);

				bndr->uvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
				bndr->cvpts = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
				uu_list_init (bndr->uvpts, sizeof(UM_coord), 200, 200);
				uu_list_init (bndr->cvpts, sizeof(UM_coord), 200, 200);
				init = UU_TRUE;
			}
/*
.....Get the trimmed surface boundary with 0.0005 tolerance
*/
			dtol = 0.0005;
        	status = ncl_get_bndry (&eptr,tfmat,bndr,dtol,UU_FALSE);
/*
.....Get tessellation polygon data
*/
			nclc_tessmgr_get_tess_trianlst(sfi->key,&tri,&uvtri);
			uu_list_init (&poly,sizeof(UM_trian),tri->cur_cnt,tri->cur_cnt);
			status = ncl_trim_tess_boundpoly(&eptr,tfmat,rot,irot,tol,
										bndr,tri,uvtri,&poly);
		   npts = poly.cur_cnt;
/*
.....Store tessellation polygon data
*/
			if (status == UU_SUCCESS)
			{
				sfi->trianlist = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
				uu_list_init (sfi->trianlist,sizeof(UM_trian),npts,npts);
				uu_list_push_list (sfi->trianlist, &poly);
			}
			UU_LIST_EMPTY(bndr->cvpts);
			UU_LIST_EMPTY(bndr->uvpts);
		}
		else
		{
/*
........Tessellate solid within tolerance
*/
			found = UU_FALSE;
			if (eptr.rel_num == UM_SOLID_REL)
			{
				solid = (struct UM_solid_rec *)&eptr;
				status = ncl_solid_calc_lists(solid,solid->sdata,solid->no_sdata,
					&dlist,&tesstmp);
				setscv(&idx,&tsav);
				if (status != UU_SUCCESS) continue;
				uu_list_free(&dlist);
			}
/*
.....Tessellate surface within tolerance
*/
			else
			{
				ncl_set_boundary_toler(tol);
				ncl_set_tess_parms(UM_TESS_TOLER,tol,0,0);
				um_init_tess(&tesstmp);
				status = ncl_tessellate_surf(&eptr,&tesstmp);
				if (status != UU_SUCCESS) continue;
			}
			sfi->trianlist = (UU_LIST *)uu_malloc(sizeof(UU_LIST));
			uu_list_init(sfi->trianlist,sizeof(UM_trian),
				tesstmp.ntri,tesstmp.ntri);
			status = ncl_get_tess_triangles (&tesstmp,sfi->trianlist,2,0);
			tript = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
			UU_LIST_EMPTY(&tesstmp.vertices);
			UU_LIST_EMPTY(&tesstmp.tri);
			UU_LIST_EMPTY(&tesstmp.normals);
		}
/*
............Get triangle points
*/
		tript = (UM_trian *)UU_LIST_ARRAY(sfi->trianlist);
		for (j=0;j<sfi->trianlist->cur_cnt;j++)
		{
			um_vctovc(tript[j].p1,pts[0]);
			um_vctovc(tript[j].p2,pts[1]);
			um_vctovc(tript[j].p3,pts[2]);
			if (irot)
			{
				um_cctmtf(pts[0],rot,pts[0]);
				um_cctmtf(pts[1],rot,pts[1]);
				um_cctmtf(pts[2],rot,pts[2]);
			}
/*
.....Convert to standard units.
*/
			if (mm == 1)
			{
				um_vctmsc(pts[0],fact,pts[0]);
				um_vctmsc(pts[1],fact,pts[1]);
				um_vctmsc(pts[2],fact,pts[2]);
			}
			if (sfi->slist == LIST_A)
			{
				ncl_update_box (pts[0], Sbox);
				ncl_update_box (pts[1], Sbox);
				ncl_update_box (pts[2], Sbox);
			}
			ncl_wcstomcs(0, pts[0], pts[0]);
			ncl_wcstomcs(0, pts[1], pts[1]);
			ncl_wcstomcs(0, pts[2], pts[2]);
/*
............Calculate normal vector
*/
			um_plane1(pts[0],pts[1],pts[2],&plane);
			if (asfnum > 0 && sfi->slist == LIST_A)
				ncl_push_triangle(&Stess,&tess,pts,plane.n,UU_FALSE);//UU_TRUE);
			else if (bsfnum > 0 && sfi->slist == LIST_B)
				ncl_push_triangle(&Sstock,&tess,pts,plane.n,UU_FALSE);//UU_TRUE);
			else if (csfnum > 0)
				ncl_push_triangle(&Scheck,&tess,pts,plane.n,UU_FALSE);//UU_TRUE);
		}
	}
/*
.....Use if optimizing tessellation
	uu_list_init(&Stess.normals,sizeof(UM_vector),tess.ntri,2000);
	uu_list_init(&Stess.tri,sizeof(UM_vector),tess.ntri,2000);
	ncl_sort_triangles(&Stess,&tess);
*/
#if 0
	um_debug_tess(&Stess);
#endif
/*
.....Set up pocketing parameters.
*/
	Sbox[0] -= offdis*fact; Sbox[1] -= offdis*fact;
	Sbox[3] += offdis*fact; Sbox[4] += offdis*fact;
	Sexpand = offdis;
	if (wset->bottype == 0)
		Sbox[2] += wset->zbot*fact;
	else
		Sbox[2] = wpar->z0*fact;
	if (wset->toptype == 0)
		Sbox[5] += wset->ztop*fact;
	else
		Sbox[5] = wpar->z1*fact;
/*
.....Leave levels and distances in metric since they will be converted later
*/
	topdis = (Sbox[5]-Sbox[2])/fact; isub = 0;
	nlps = 2; offdis8 = offdis; offpr = 0;
	wlev = Sbox[2];
	ncl_wat_setpln (pl);
	if (wbatch != 1)
	{
		sprintf (wbuf,"Calculating Motion ..."); S_msg_list ();
	}
	Suse_box = -1;
	if (wpar->havestk)
	{
		if (wpar->poctype == STK_CONTOUR) Suse_box = 0;
		else Suse_box = 1;
	}
	else if (wpar->poctype == STK_BOUNDSF)
		Suse_box = 2;
	watrevsf = 0; wlev = Sbox[2];
	botkey = pl->key;
/*
.....Calculate motion.
*/
	wpock3(&botkey,&topdis,&offdis,&isub,&nlps,&offpr,&lmotion,&ier);
	status = UU_SUCCESS;
	if (wbatch != 1)
	{
		if (ier < 0) S_wpock_warn(ier,&numwarn);
		status = ier;
	}
	goto done;
failed:
	status = 466;
done:
	if (status == UU_SUCCESS && wbatch != 1)
	{
		if (Sactive)
		{
			if (Sfrm!=-1)
				ud_close_dispfrm (Sfrm);
			Sfrm = -1;
		}
		else
			OnClose();
	}
	Suse_box = UU_FALSE;
	um_free_tess(&Stess);
	if (bsfnum > 0) um_free_tess(&Sstock);
	if (csfnum > 0) um_free_tess(&Scheck);
	um_free_tess(&tess);
	um_free_tess(&tesstmp);
	if (init) um_free_boundary (bndr);
	ncl_free_circ(0);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_waterline4 (wset,wpar,bxbox,base,ntimes,
**										offdis,mxtol2)
**       Main waterline roughing routine.
**       Cut a collection of surfaces by horizontal plane(s) and pocket the
**       resulting contours.
**    PARAMETERS
**       INPUT  :
**          bxbox      - stock box, used if a calculated stock is specified
**          wbase      - base surface data
**			nvec	   - unit vector UP
**          pl         - a Unibase plane structure, to be used for intersections
**          ntimes     - number of horizontal levels to cut
**			offdis	   - expandsion distance for bounding surfaces
**          mxtol2     - square of the acceptable gap parameter
**
**          wpar       - structure holding various parameters:
**          wpar.frmtype    - style of ordering the pockets on the same level
**          wpar.sortpt     - near point, used iff frmtype is NEARPT
**          wpar.poctype    - 0 when stock included;
**                          - 1 when use contour stock;
**                          - 2 when use XY-box stock;
**                          - 3 when ignore outermost contours
**							- 4 when using bounding surfaces
**          wpar.conpoc     - flag for the output
**       OUTPUT :
**          the pocketing motion if conpoc = 2; intersection polylines if
**          conpoc = 0; pocketing contours if conpoc = 1
**
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_waterline4 (wset,wpar,bxbox,wbase,nvec,pl,rot,ntimes,offdis,mxtol2)
NCL_w_setup *wset;
NCL_w_param *wpar;
UM_2box *bxbox;
NCL_w_base *wbase;
UM_vector nvec;
struct NCL_nclpl_rec *pl;
UM_transf rot;
int ntimes;
UU_REAL offdis,mxtol2;
{
	int i,j,itime,nn,isf,npts,nc,dep,c,itry,status,nnl,numwarn,ncvs,nloops;
	int ideep = 0;
	int *nj,*nnp;
	UU_REAL dbest,hbest,wlev0,pm,cdis,wprev,wi,wnext,wlevdeep,psthk,dist;
	UU_REAL eps = 0.1*tol2d;
	UM_coord *ptio;
	UM_int4 botkey;
	NCL_waterline_surf *sfi;
	UM_int2 isub,ifl86,ier,nlps,offpr;
	UU_LIST curves,njlist,curve1;
	ncl_polygon pock,pock0;
	char errout[80],sbuf[80];
	UM_real8 topdis;
	NCL_w_geo *cvs;
	NCL_w_base cbase;
	NCL_w_gap cgap;
	struct NCL_fixed_databag csf;
	int c0;
	UM_int2 lmotion = 0;
	UU_LOGICAL levbetween = UU_FALSE,prev_levbetween = UU_FALSE;
	UU_LOGICAL ladj = UU_FALSE;
	UU_LOGICAL lwmoved = UU_FALSE;
	UU_LOGICAL lbotpl = UU_FALSE;
	UU_LOGICAL lstkslice = UU_FALSE;
	UU_LOGICAL lprtslice = UU_TRUE;
	UU_LOGICAL llast;
	UU_LOGICAL lv97,lv98;

	status = UU_SUCCESS;

	csf.key = NULLKEY;
	ncl_wbase_init (&cbase);

	nnl = numwarn = 0;
	ier = 0;

	ncl_init_polygon (&pock0,0); ncl_init_polygon (&pock,0);

	uu_list_init (&curves, sizeof(NCL_w_geo), 10, 10);
	uu_list_init (&njlist, sizeof(int), 10, 10);

	if (wpar->conpoc >= 1 && wpar->poctype == STK_BOUNDSF)
		uu_list_init (&curve1, sizeof(NCL_w_geo), 10, 10);

	if (watrevsf)
	{
		cbase.srf = &csf;
		status = ncl_copy_wat_botsf(wbase,&cbase);
		if (status == UU_SUCCESS)
			status = ncl_offset_wat_botsf(&cbase,wlev);
		if (status != UU_SUCCESS) goto Done;
		botkey = csf.key;
	}
	else
		botkey = pl->key;

	topdis = wpar->dz;

	lv97 = ncl_setver(97);
	lv98 = ncl_setver(98);

	if (!lv97 && wset->botkey > 0)
		lbotpl = S_find_botpl(wset->botkey);

	psthk = 0.;
	if (!lv97 && ntimes > 1)
	{
		isub = 23; getsc(&isub,&psthk);
		if (psthk != 0.) wlev = wlev - psthk;
	}	
	wnext = wlev;
/*
<<<<<<<<<<<<<<<<<<  MAIN LOOP >>>>>>>>>>>>>>>>>>>>>
*/
	for (itime = 0; itime < ntimes; itime++)
	{
		Slastfl = UU_FALSE;
		ncl_nvmill_reset_calls();
		ckintr(&ifl86,&wbatch);
		isub = 86; getifl(&isub,&ifl86);
		if (ifl86 != 0)
		{
			ier = 149; goto Done;
		}
		wprev = wlev;
		if (!levbetween)
		{
			wnext = wnext - wpar->dz;
		}

		wlev0 = hbest = wlev = wnext;

		itry = 0; pm = 0.1; dbest = 1000000.;
		overlap.cur_cnt = 0;

		prev_levbetween = levbetween;
		levbetween = lwmoved = UU_FALSE;
		if ((itime > 0 || !lv98) && wpar->finish > 0)
		{
			ladj = (wpar->finish == 2 && (itime < ntimes - 1 || !lv98));

			levbetween = S_find_interlev (wprev,wlev,wpar->dup,wpar->ddn,wtol,&wi,&ladj);
			if (levbetween)
			{
				wlev0 = hbest = wlev = wi + wtol;
				if (ladj) levbetween = UU_FALSE;
				lwmoved = UU_TRUE;
			}
		}

		if (wbatch != 1)
		{
			wbuf[0] = '\0'; S_msg_list ();
			sprintf (wbuf,"Intersecting surfaces at Z = %g ...",wlev);
			S_msg_list ();
		}

		if (wpar->poctype == STK_BOUNDSF)
		{
			lstkslice = UU_TRUE;
			lprtslice = UU_FALSE;
		}

Intof:
		if (itry == 0 && wlev < wpar->z0)
		{
			if (wlev < wpar->z0 - 0.5*wtol) break;
			wlev = wpar->z0;
		}

		status = ncl_wat_setpln (pl);

		if (status != UU_SUCCESS) goto Done;


PrtSlice:
		ptsio.cur_cnt = nio.cur_cnt = 0;
		if (wpar->conpoc >= 1) curves.cur_cnt = 0;
		for (isf = 0, sfi = sff; isf < sfnum; isf++,sfi++)
		{
			sfi->ncvs = 0; sfi->nlist0 = sfi->plist0 = -1;
			if (itime > 0 || prev_levbetween) sfi->size = 1.;
			if (lstkslice && sfi->slist == LIST_A)
				continue;
			if (lprtslice && sfi->slist == LIST_B)
				continue;
			if (sfi->slist == LIST_C || (levbetween && sfi->slist != LIST_A))
				continue;
			if (sfi->key == NULLKEY || sfi->sf_flag == HORZPL) continue;
			if (sfi->zmin > wlev + wtol || sfi->zmax < wlev + 0.1*wtol)
				continue;
			sfi->nlist0 = nio.cur_cnt; sfi->plist0 = ptsio.cur_cnt;
			if (lv97 || watrevsf || sfi->flag1 == VSOLID)	
				nc = ncl_sf_pln_io (sfi,pl,&nio,&ptsio,wlev,tol2d,wtol,0);
			else				
				nc = ncl_sf_pln_io2 (sfi,pl,nvec,rot,&nio,&ptsio,wlev,tol2d,wtol,0);
			if (nc < 0 || (nc > 0 && wpar->conpoc == 0))
				ncl_get_label_with_key(sfi->key, wbuf);
			if (nc < 0)
			{
				if (GUI == 1)
				{
					sprintf(errout,"Could not intersect surface %s.",wbuf);
					ud_wrerr(errout);
				}
				else
				{
					ier = 502;
					uerror2(str77);
				}
				status = UU_FAILURE; goto Done;
			}
			if (nc == 0) continue;
			if (wpar->conpoc == 0)
			{
				int *np;
				np = (int *) UU_LIST_ARRAY (&nio);
				ptio = (UM_coord *) UU_LIST_ARRAY (&ptsio);
				ptio += sfi->plist0;
				for (i = 0; i < nc; i++)
				{
					npts = np[sfi->nlist0 + i];
					status = ncl_waterline_create_geo (wbase,wlev,npts,ptio);
					if (status != UU_SUCCESS) goto Done;
					ptio += npts;
				}
			}
/*
.....Debug slices before connect
*/
#if 0
//			if (sfi->key == 2808 || sfi->key==2843 || sfi->key==2826 ||
//				sfi->key==4509 || sfi->key==2866 || sfi->key==4523 ||
//				sfi->key==17075 || sfi->key==17092 || sfi->key==2030 ||
//				sfi->key==2050 || sfi->key==3870 || sfi->key==17057)
			ncl_debug_slices(sfi->key,wbase,wlev);
#endif
		}

		if (wpar->conpoc >= 1)
		{
			llast = (itry == 19);

			nn = nio.cur_cnt;
			if (nn < 1)
			{
/*
..... no intersections at this level
*/
				if (wpar->havestk)
				{
					ncvs = 1;
					if (levbetween)
					{
						wlev = wi;
						status = ncl_wat_setpln (pl);
					}
					goto Pock;
				}
				if (ntimes == 1)
				{
					if (GUI == 1)
						ud_wrerr("No intersections.");
					else
						ier = 502;
					status = UU_FAILURE; goto Done;
				}
				else
					continue;
			}

			nnp = (int *) UU_LIST_ARRAY (&nio);
			njlist.cur_cnt = 0;
			nnl = 0;
			uu_list_push (&njlist,&nnl);
			for (j = 0; j < nn-1; j++)
			{
				nnl += nnp[j];
				uu_list_push (&njlist,&nnl);
			}
			nnl += nnp[j];
			nj = (int *) UU_LIST_ARRAY (&njlist);

			if (itry == 0 && wbatch != 1)
			{
				sprintf (wbuf,"Connecting %d pieces",nio.cur_cnt);
				S_msg_list ();

				status = ncl_waterline_connect (wbase,wlev,&nio,nj,&ptsio,
					&cgap,mxtol2,&curves,&overlap,wpar->poctype,llast);
				if (overlap.cur_cnt > 0)
					S_overlap_error (sff,sfnum,&overlap);
			}
			else
				status = ncl_waterline_connect (wbase,wlev,&nio,nj,&ptsio,
					&cgap,mxtol2,&curves,UU_NULL,wpar->poctype,llast);

			if (status != UU_SUCCESS)
			{
/*
..... try to connect at a little lower or higher level
*/
				if (!llast)
				{
					itry++;
					if (itry == 19)
						wlev = hbest;
					else
					{
						if (cgap.dsec < dbest)
						{
							dbest = cgap.dsec; hbest = wlev;
						}
						if (wlev0 > wpar->z0)
						{
							pm = -pm;
							wlev = wlev0 + ((itry+1)/2)*pm*wtol;
						}
						else
							wlev = wlev0 + itry*pm*wtol;
					}
#if 0
					sprintf(sbuf,"wlev = %12.4f",wlev);
					NclxDbgPstr(sbuf);
#endif
					goto Intof;
				}
				ier = 503;
				if (wbatch != 1)
				{
					ud_setfrm_traverse_mask(Sfrm,1,UU_TRUE);
					ud_setfrm_traverse_mask(Sfrm,2,UU_TRUE);
					ud_setfrm_traverse_mask(Sfrm,3,UU_TRUE);
					ud_setfrm_traverse_mask(Sfrm,4,UU_TRUE);
					S_connect_error (wbase,wlev0,sff,sfnum,&cgap);
				}

				goto Done;
			}
			if (wbatch != 1) ud_setfrm_traverse_mask(Sfrm,1,UU_FALSE);
/*
.....Slice seperately for BOUND sflist stock
*/
			if (wpar->poctype == STK_BOUNDSF)
			{
				if (lstkslice)
				{
					lstkslice = UU_FALSE;
					lprtslice = UU_TRUE;
					uu_list_push_list(&curve1,&curves);
/*
.....Set up the bounding box so the stock can be found later.  The
.....stock can now be treated as open so we need to know where it
.....is in the contour list - ASF 12/11/13.
*/
					if (wpar->method == 1)
					{
						cvs = (NCL_w_geo *) UU_LIST_ARRAY (&curves);
						for (j=0;j<curves.cur_cnt;j++)
						{
							if (j == 0)
							{
								bxbox->xmin = cvs[j].xrange[0];
								bxbox->xmax = cvs[j].xrange[1];
								bxbox->ymin = cvs[j].yrange[0];
								bxbox->ymax = cvs[j].yrange[1];
							}
							else
							{
								if (cvs[j].xrange[0] < bxbox->xmin)
									bxbox->xmin = cvs[j].xrange[0];
								if (cvs[j].xrange[1] > bxbox->xmax)
									bxbox->xmax = cvs[j].xrange[1];
								if (cvs[j].yrange[0] < bxbox->ymin)
									bxbox->ymin = cvs[j].yrange[0];
								if (cvs[j].xrange[0] < bxbox->ymax)
									bxbox->ymax = cvs[j].yrange[1];
							}
						}
					}
					goto  PrtSlice;
				}
				else if (lprtslice)
				{
					uu_list_push_list(&curves,&curve1);
					curve1.cur_cnt = 0;
					if (status == UU_SUCCESS)
						status = ncl_set_insides (&curves);
				}
			}

			if (lwmoved)
				wlev = wi;
			else
				wlev = wlev0;
			status = ncl_wat_setpln (pl);
			ncvs = curves.cur_cnt;
Pock:
			if (ncvs > 0)
			{
				if (pock.contour == UU_NULL)
				{
					pock.contour = (UU_LIST *) uu_malloc (sizeof(UU_LIST));
					if (nnl < 100) nnl = 100;
					uu_list_init (pock.contour, sizeof(UM_2Dcoord), nnl, nnl);
				}
				else
					pock.contour->cur_cnt = 0;

				dep = (wpar->poctype == STK_IGNORE)? 1: 0;
/*
..... add the box to the list of curves if there is a stock;
..... store depth numbers for all loops in njlist
*/
				ncl_add_stk (wpar->poctype,&curves,&njlist,&stkbox);
/*
..... put all loops in a polygon structure, in order of their depth numbers
*/
				if (wpar->method == 1) dist = offdis;
				else dist = offdis+wpar->trad;
				ncl_loops_to_pol (wbase,dep,&curves,&pock,&njlist,wpar->poctype,
					dist,tol2d,wtol);
#if 0
// GOOD
//				if (wlev < .877)
				{
//					UN_clip_debug = UU_TRUE;
					ncl_debug_polygon(UU_NULL,&pock,4);
				}
#endif
				ncvs = curves.cur_cnt;
				if (wpar->havestk) ncvs--;
				cvs = (NCL_w_geo *) UU_LIST_ARRAY (&curves);
				for (j = 0; j < ncvs; j++,cvs++) UU_FREE(cvs->pt);

				if (levbetween)
				{
#if 0
				ncl_debug_polygon(UU_NULL,&pock,3);
#endif
// PROBLEM AREA
					nc = S_relevant_pieces (&pock,&levlst,wpar->trad,wpar->rsq);
					if (nc < 1) goto Apres;
				}
#if 0
// BAD
				ncl_debug_polygon(UU_NULL,&pock,5);
#endif
				if (pock0.num_contours > 0 && wpar->poctype != STK_BOUNDSF)
				{
					if (wbatch != 1)
					{
						sprintf (wbuf,"Intersecting with the previous level ...");
						S_msg_list ();
					}
#if 0
//					if (wlev < .877)
						ncl_debug_polygon(UU_NULL,&pock0,2);
#endif
					status =
						ncl_polygon_clip (NCL_INTOF,&pock,&pock0,&pock,wtol,wtolsq);
					if (status != UU_SUCCESS) goto Done;
#if 0
					if (wlev < .877) ncl_debug_polygon(UU_NULL,&pock,5);
#endif
				}
				if (itime > 0) docheck = UU_FALSE;
				if (wbatch != 1)
				{
					sprintf (wbuf,"Checking possible overhanging surfaces ...");
					S_msg_list ();
				}
#if 0
// BAD
					ncl_debug_polygon(UU_NULL,&pock,9);
#endif
				status =
				ncl_order_pock_geo (&pock,&njlist,0,0,0,wpar->sortpt,UU_NULL);

				if (status != UU_SUCCESS)
				{
					if (GUI == 1)
						ud_wrerr("Could not order contours.");
					if (status == UU_FAILURE) ier = 520;
					goto Done;
				}
				if (status == UU_SUCCESS)
				{
					njcur = (int *) UU_LIST_ARRAY (&njlist);
#if 0
// BAD
					ncl_debug_polygon(UU_NULL,&pock,10);
#endif
					status = ncl_check_overhangs (sfnum,sff,wbase,rot,wlev,wprev,
						&pock,&nio,&ptsio,wpar->poctype,wpar->rsq,tol2d,tol2dsq,wtol,
					NPT,levbetween);
/*
.....Debug pocket contours after overhang
*/
#if 0
					if (wlev < .877) ncl_debug_polygon(UU_NULL,&pock,11);
#endif
					if (status == UU_SUCCESS && wpar->conpoc == 1)
						status = ncl_waterline_create_geo1 (wbase,wlev,tol2dsq,
						&pock);
					if (status == UU_SUCCESS)
						ncl_prepare_for_pocketing (&pock,tol2d,tol2dsq);
				}
				if (status != UU_SUCCESS || pock.num_contours <= 0) goto Done;

				if (wpar->conpoc == 2)
				{
					curpock = &pock;
					ncvs = curpock->num_contours;
					c0 = cstk = tstk = -1;
					if (wpar->havestk)
						ncl_find_stock(bxbox,&cstk,eps);

					status = ncl_order_pock_geo (curpock,&njlist,zoning,cstk,
												wpar->frmtype,wpar->sortpt,&ncvs);


					if (status == UU_SUCCESS)
					{
						if (wpar->method == 1) tstk = cstk;
						else tstk = 0;
						cstk = -1;
						if (!levbetween && wpar->poctype == STK_CONTOUR)
							status = ncl_find_stock(bxbox,&cstk,eps);
/*
.....Find the stock polygon when stock is generated from slicing
.....boundary surfaces.  Ensures this type of stock can be treated
.....as open in VoluMill - ASF 12/11/13.
*/
//						if (wpar->poctype == STK_BOUNDSF && wpar-> method == 1)
//							ncl_find_bound_stock(bxbox,&tstk,eps);
					}

					if (status != UU_SUCCESS)
					{
						if (GUI == 1)
							ud_wrerr("Could not order contours.");
						if (status == UU_FAILURE) ier = 520;
						goto Done;
					}

					if (godeep == 1 && ncvs > 0)
					{
						status = ncl_copy_polygon(curpock,&dpoks[ideep]);
						if (status == UU_SUCCESS)
						status = ncl_add_node (levbetween,ideep,ncvs,cstk,
							&njlist,dpoks,njdpoks,&dphead,wtol);
						if (status != UU_SUCCESS)
						{
							if (GUI == 1)
								ud_wrerr("Failed to arrange for deep cutting.");
							goto Done;
						}
						if (wbatch != 1)
						{
							nc = curpock->num_contours;
							sprintf (wbuf,
						"Prepared to pocket %d contours at Z = %g ...",nc,wlev);
							S_msg_list ();
						}
						if (ideep == 0)
							wlevdeep = wlev;
						ideep++;
						goto Apres;
					}

					ncl_find_circles (curpock,wtol,NPT,cstk);

					ccur = 0;
					njcur = (int *) UU_LIST_ARRAY (&njlist);
					nc = curpock->num_contours;
					if (wbatch != 1)
					{
						sprintf (wbuf,
							"Pocketing %d contours at Z = %g ...",nc,wlev);
						S_msg_list ();
						nclu_vmill_update_progress(0);
					}
					ier = 0; isub = itime;

					if (watrevsf)
					{
						cdis = wlev - wprev;
						status = ncl_offset_wat_botsf(&cbase,cdis);
					}

					status = ncl_rotate_plane (pl,mm);
					if (status != UU_SUCCESS) goto Done;

					itry = 0;
					if (wpar->havestk)
					{
						if (!levbetween && cstk < 0)
							ncl_find_stock(bxbox,&c0,eps);
						else
							c0 = cstk;
						tstk = c0;
					}

#ifdef DBGSEQUNC
/*
..... insert a 'sequnc/wlev1', 'sequnc/wlev2',... into the CL-file before each
..... level - to be able to see each level cut separately
*/
					wlevseq (&isub);
#endif

					while (ccur < nc && ccur < ncvs)
					{
						nloops = 1;
						for (c = ccur + 1; c < nc && curpock->np[c] < 0; c++)
							nloops++;
						itry++;
						nlps = nloops;
						if ((wpar->havestk && ccur == c0) ||
							(!wpar->havestk && nloops > 1))
							offpr = 1;
						else
							offpr = 0;
#if 0
						ncl_debug_overhang(curpock,&cbase,wlev);
#endif
						if (itime == ntimes-1 && ccur+nloops >= ncvs)
							Slastfl = UU_TRUE;
						if (watrevsf)
						{
							status = ncl_wat_set_pokgeo (&cbase,curpock,ccur,c,
								c0,wtol,tol2dsq);
							if (status != UU_SUCCESS) goto Done;
							wpock1 (&cbase.asw,&topdis,&isub,&nlps,&offpr,
								&tool[3],&lmotion,&ier);
						}
						else if (wpar->method == 1)
						{
							wpock2(&botkey,&topdis,&isub,&nlps,&offpr,
								&lmotion,&ier);
						}
						else
						{
/*							if (!levbetween || S_relevant_piece(nloops,wpar->rsq)) */
							wpock (&botkey,&topdis,&isub,&nlps,&offpr,&lmotion,&ier);
						}
						if (ier < 0 && wbatch != 1) S_wpock_warn(ier,&numwarn);
						if (ier > 0) break;
						ccur += nloops;
					}
					ncl_free_circ (1);
					if (ier > 0)
					{
						if (wbatch != 1)
						{
							S_wpock_err (itry,nloops); status = ier;
						}
						goto Done;
					}
				}
Apres:
#if 0
					if (wlev < .877)
						ncl_debug_polygon(UU_NULL,&pock,6);
#endif
				if (itime < ntimes - 1 && !levbetween)
					status = ncl_copy_polygon(&pock,&pock0);
#if 0
					if (wlev < .877)
						ncl_debug_polygon(UU_NULL,&pock,9);
#endif

				pock.num_contours = 0;
				UU_FREE (pock.np); UU_FREE (pock.box);
				if (status != UU_SUCCESS) goto Done;
			}
		}
		if (levbetween)
		{
			itime--;
		}		
   }

   if (godeep == 1)
   {
		NCL_dpnode *dpcur;
		wlev = wlevdeep;

		for (dpcur = dphead; dpcur != UU_NULL; dphead = dpcur)
		{
			wprev = wlev;
			cstk = dpcur->cstk;
			itime = dpcur->itime;
			wlev = dpcur->htop;
			c = dpcur->cnum;
			levbetween = dpcur->levbetween;

			curpock = &dpoks[itime];
			ccur = c;
			nc = curpock->num_contours;

			njcur = (int *) UU_LIST_ARRAY (&njdpoks[itime]);

			ncl_find_circles1 (curpock,c,njcur[c],wtol,NPT,cstk);

			if (wbatch != 1)
			{
				sprintf (wbuf,"Now pocketing at Z = %g ...",wlev);
				S_msg_list ();
			}
			ier = 0; isub = itime;

			status = ncl_wat_setpln (pl);

			if (watrevsf)
			{
				cdis = wlev - wprev;
				status = ncl_offset_wat_botsf(&cbase,cdis);
			}

			if (status == UU_SUCCESS)
				status = ncl_rotate_plane (pl,mm);
			if (status != UU_SUCCESS) goto Done;

			if (wpar->havestk)
			{
				if (cstk < 0)
					ncl_find_stock(bxbox,&c0,eps);
				else
					c0 = cstk;
			}

			offpr = 0;
			nloops = 1;
			for (c = ccur + 1; c < nc && curpock->np[c] < 0; c++)
				nloops++;

#if 0
					ncl_debug_overhang(curpock,&cbase,wlev);
#endif

			nlps = nloops;
			if (watrevsf)
			{
				status = ncl_wat_set_pokgeo (&cbase,curpock,ccur,c,
					c0,wtol,tol2dsq);
				if (status != UU_SUCCESS) goto Done;
				wpock1 (&cbase.asw,&topdis,&isub,&nlps,&offpr,
							&tool[3],&lmotion,&ier);
			}
			else if (wpar->method == 1)
			{
				wpock2(&botkey,&topdis,&isub,&nlps,&offpr,
					&lmotion,&ier);
			}
			else
			{
/*				if (!levbetween || S_relevant_piece(nloops,wpar->rsq)) */
				wpock (&botkey,&topdis,&isub,&nlps,&offpr,&lmotion,&ier);
			}

			if (ier < 0 && wbatch != 1) S_wpock_warn(ier,&numwarn);
			ncl_free_circ (1);
			if (ier > 0)
			{
				if (wbatch != 1)
				{
					S_wpock_err (-1,nloops); status = ier;
				}
				goto Done;
			}
			dpcur = dphead->next;
			UU_FREE (dphead);
		}
   }

Done:

#ifdef DBGSEQUNC
/*
..... insert a 'sequnc/end' into the CL-file after all levels are done
*/
	isub = -1;
	wlevseq (&isub);
#endif

	if (status == UU_SUCCESS && numwarn == 0 && wbatch != 1)
	{
		if (Sactive)
		{
			if (Sfrm!=-1)
				ud_close_dispfrm (Sfrm);
			Sfrm = -1;
		}
		else
			OnClose();
	}
	ncl_free_polygon (&pock0); ncl_free_polygon (&pock);
	uu_list_free (&curves);
	uu_list_free (&njlist);
	ncl_free_circ (0); ncl_free_circ (1);
	ncl_free_nodes (&dphead);
	if (wpar->conpoc == 2 && lmotion == 0 && ier == 0)
		ier = 530;
	if (ier > 0)
		status = ier;
	else if (status != UU_SUCCESS)
		status = 177;
	else if (numwarn > 0)
		status = -numwarn;

	if (csf.key > NULLKEY) uc_delete (csf.key);
	if (watrevsf) ncl_wat_set_boundary (UU_NULL);
	if (watrevsf) ncl_wat_reset_plane();

	return (status);
}

/*********************************************************************
*********************************************************************/
void ncl_set_wlev (z)
UU_REAL z;
{
	wlev = z;
}

/*********************************************************************
*********************************************************************/
void ncl_get_wlev (z)
UU_REAL *z;
{
	*z = wlev;
}

/*********************************************************************
*********************************************************************/
void ncl_get_wtol (t)
UU_REAL *t;
{
	*t = wtol;
}

/*********************************************************************
*********************************************************************/
void ncl_get_wtol2d (t)
UU_REAL *t;
{
	*t = tol2d;
}

/*********************************************************************
*********************************************************************/
void ncl_get_wtols (t,tsq)
UU_REAL *t,*tsq;
{
	*t = wtol; *tsq = wtolsq;
}

/*********************************************************************
*********************************************************************/
void ncl_get_wpol (itsk,p)
int itsk;
ncl_polygon **p;
{
	if (itsk == 0)
		*p = &Spol0;
	else
		*p = &Spol1;
}

/*********************************************************************
*********************************************************************/
void ncl_get_current_contour (c)
int *c;
{
	*c = ccur;
}

/*********************************************************************
*********************************************************************/
void ncl_get_current_pol (p)
ncl_polygon **p;
{
	*p = curpock;
}

/*********************************************************************
*********************************************************************/
void ncl_get_current_pol1 (p,nj)
ncl_polygon **p;
int **nj;
{
	*p = curpock;
	*nj = njcur;
}

/*********************************************************************
*********************************************************************/
UU_LOGICAL ncl_is_watrev ()
{
	return (watrevsf == 1);
}

/*********************************************************************
*********************************************************************/
UU_LOGICAL ncl_batch_on()
{
	return (wbatch == 1);
}

/*********************************************************************
*********************************************************************/
UU_LOGICAL ncl_need_test()
{
	return (docheck);
}

/*********************************************************************
*********************************************************************/
void ncl_get_cstk (c)
int *c;
{
	*c = cstk;
}

/*********************************************************************
.....Added for VoluMill open stock boundary.
*********************************************************************/
void ncl_get_tstk (c)
int *c;
{
	*c = tstk;
}

/*********************************************************************
*********************************************************************/
void ncl_get_stklst (lst)
UU_LIST **lst;
{
	if (stklst_init == 1)
		*lst = &stklst;
	else
		*lst = NULLST;
}

/*********************************************************************
*********************************************************************/
void ncl_wat_get_uvlim (wmin,wmax)
UU_REAL wmin[],wmax[];
{
	if (stk_xmmx == UU_NULL)
	{
		wmin[0] = 0; wmax[0] = 1;
	}
	else
	{
		wmin[0] = stk_xmmx[0]; wmax[0] = stk_xmmx[1];
	}
	if (stk_ymmx == UU_NULL)
	{
		wmin[1] = 0; wmax[1] = 1;
	}
	else
	{
		wmin[1] = stk_ymmx[0]; wmax[1] = stk_ymmx[1];
	}
}

/*********************************************************************
*********************************************************************/
void ncl_wat_get_sfnums (numa,numb,numc)
int *numa,*numb,*numc;
{
	*numa = asfnum;
	*numb = bsfnum;
	*numc = csfnum;
}

